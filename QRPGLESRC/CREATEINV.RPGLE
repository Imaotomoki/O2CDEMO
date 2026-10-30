     H DATEDIT(*YMD) DECEDIT('0.') COPYRIGHT('DEMOAPP')
      *=====================================================================
      * プログラム名: CREATEINV
      * 説明: 請求発行プログラム（締め条件対応）
      * ライブラリ: DEMOAPP
      * 変更履歴:
      *   修正#4 @PRCSHP - 単価のハードコード廃止 SORDERLP参照
      *   修正#12 *ENTRY PLIST 化 INVNUM自動生成
      *   修正#R1 @GENINVID - SQLシーケンス採番に変更
      *   修正#R2 @UPDSOH - CHAIN(E)でロック取得しLost Update防止
      *   修正#R7 @PRCSHP - 税額に%INT切り捨て端数処理を明示
      *   修正#R12 @WRTINVH/@UPDSOH - 監査USERをUSER変数に変更
      *   修正#R13 @END - COMMIT/ROLBK追加（INVOICEHP/INVOICELP/SORDERHP）
      *   修正#DUP @VLDSOH追加 - 受注ステータスがSHIPPED以外は請求しない
      *            (請求済受注への二重請求防止)
      *            @END - エラー時はCPF9898エスケープで呼び出し元へ通知
      *            @PRCSHP のRETURNをLEAVESRに変更(@END経由で終了)
      *   修正#NODSP DSPLYを削除(画面から呼ばれると応答待ちで止まるため)
      *   修正#SEQ @GENINVID - シーケンス名の修飾を外しSQLCODEを確認
      *   修正#MULTI @PRCSHP - 受注の未請求の出荷を全件請求(分納対応)
      *            請求した出荷はSTATUS=INVOICEDにし二重請求を防止
      *   修正#TAX2 消費税は税率ごとに合計してから1回だけ切り捨て
      *            (インボイス制度。従来は明細ごとに切り捨て)
      *   修正#DUE 支払期限は受注の支払条件、通貨は受注の通貨
      *   修正#PART 一部出荷(PARTIAL)の受注も出荷済分を請求できる
      *            全量出荷済(SHIPPED)を請求した時だけ受注をINVOICED
      *   修正#PRT 請求書を確定したらINVPRINTで印刷する
      *   修正#SCN 取り消した出荷(CANCELLED)は請求しない
      *=====================================================================

      *---------------------------------------------------------------
      * ファイル定義
      *---------------------------------------------------------------
     FSORDERHP  UF   E           K DISK    COMMIT
      * [修正#4] SORDERLP: 受注明細から単価・税率を取得するために追加
     FSORDERLP  IF   E           K DISK
     FSHIPMENTHPIF   E           K DISK
      * [修正#MULTI] 請求済の出荷にINVOICEDを付けるため更新可能にする
     FSHIPMENHL1UF   E           K DISK    PREFIX(SL1_) COMMIT
     F                                     RENAME(SHPREC:SL1REC)
     FSHIPMENTLPIF   E           K DISK
      * [修正#R13] COMMIT制御対象ファイルにキーワード追加
     FINVOICEHP O    E             DISK    COMMIT
     FINVOICELP O    E             DISK    COMMIT
     FCUSTOMERP IF   E           K DISK    PREFIX(C_)
      * [修正#9] ITEMP: 請求明細DESCフィールド設定のために追加
     FITEMP     IF   E           K DISK    PREFIX(ITM_)

      *---------------------------------------------------------------
      * 変数定義
      *---------------------------------------------------------------
     D WTIMESTAMP      S               Z
     D WDATE8          S              8S 0
     D WTIME6          S              6S 0
     D W#CNTR          S              9S 0
     D W#INVID         S             18P 0
     D W#SHPID         S             18P 0
     D W#SOID          S             18P 0
     D W#CUSTID        S              9P 0
     D W#LINENO        S              9P 0
     D W#FOUND         S               N
     D W#ERROR         S               N
     D W#ERRMSG        S             52A
     D W#EOF           S               N
      * [修正#MULTI] 出荷ヘッダの読込終了/請求した出荷件数
     D W#SHPEOF        S               N
     D W#SHPCNT        S             10I 0
      * [修正#R12] 実行ユーザー取得用
     D W#USER          S             10A
      * 請求金額計算用
     D W#SUBTOTAL      S             15P 2
     D W#TAXAMT        S             15P 2
     D W#TOTAMT        S             15P 2
     D W#LINEAMT       S             15P 2
      * 明細用変数
     D W#ITEMID        S              9P 0
     D W#QTY           S             15P 3
     D W#UNITPRC       S             15P 4
     D W#TAXRATE       S              5P 3
     D W#SOLINENO      S              9P 0
      * [修正#TAX2] 税率別の税抜合計(1請求書内の税率は10種まで)
     D W#TXRT          S              5P 3 DIM(10)
     D W#TXBASE        S             15P 2 DIM(10)
     D W#TXN           S             10I 0
     D W#K             S             10I 0
      * [修正#DUE/#PART] 受注の通貨・支払条件・状態
     D W#CURRCD        S              3A
     D W#PAYTERM       S             20A
     D W#SOSTAT        S             12A
     D W#DUEDAYS       S             10I 0
      * [修正#PRT] 請求書印刷
     D INVPRINT        PR                  EXTPGM('INVPRINT')
     D   pInvNum                     20A   CONST
      * [修正#DUP] エスケープメッセージ送信用
     D W#ESCMSG        S             52A
     D W#MSGKEY        S              4A
     D W#APIERR        DS
     D   W#APIPRV                    10I 0 INZ(0)
     D   W#APIAVL                    10I 0 INZ(0)
     D QMHSNDPM        PR                  EXTPGM('QMHSNDPM')
     D   MsgId                        7A   CONST
     D   MsgFile                     20A   CONST
     D   MsgDta                      52A   CONST
     D   MsgDtaLen                   10I 0 CONST
     D   MsgType                     10A   CONST
     D   CallStk                     10A   CONST
     D   CallStkCtr                  10I 0 CONST
     D   MsgKey                       4A
     D   ErrCode                      8A

      *---------------------------------------------------------------
      * キーリスト: 受注明細複合キー（SOID + LINENO）
      * [修正#4] 受注明細から単価を取得するためのキー定義
      *---------------------------------------------------------------
     C     SOLKEY        KLIST
     C                   KFLD                    W#SOID
     C                   KFLD                    W#SOLINENO

      *=====================================================================
      * エントリパラメータ
      * [修正#13] 第1パラメータを W#SHPID -> W#SOID に変更
      *           SHIPMENTHP は @PRCSHP 内で SOID から READE して取得
      *=====================================================================
     C     *ENTRY        PLIST
     C                   PARM                    W#SOID
     C                   PARM                    W#CUSTID

      *=====================================================================
      * メインルーチン
      *=====================================================================
     C                   EXSR      @INZ
     C                   EXSR      @VLDCUST
     C                   IF        W#ERROR
     C                   EXSR      @END
     C                   ENDIF
      * [修正#DUP] 受注ステータスチェック(二重請求防止)
     C                   EXSR      @VLDSOH
     C                   IF        W#ERROR
     C                   EXSR      @END
     C                   ENDIF
     C                   EXSR      @GENINVID
     C                   IF        W#ERROR
     C                   EXSR      @END
     C                   ENDIF
     C                   EXSR      @PRCSHP
     C                   IF        W#ERROR
     C                   EXSR      @END
     C                   ENDIF
     C                   EXSR      @WRTINVH
     C                   EXSR      @UPDSOH
     C                   EXSR      @END

      *=====================================================================
      * サブルーチン: @INZ - 初期処理
      *=====================================================================
     C     @INZ          BEGSR
     C                   TIME                    WTIMESTAMP
     C                   EVAL      WDATE8 = %DEC(%DATE(WTIMESTAMP):*ISO)
     C                   EVAL      WTIME6 = %DEC(%TIME(WTIMESTAMP):*HMS)
     C                   EVAL      W#CNTR = *ZERO
     C                   EVAL      W#ERROR = *OFF
     C                   EVAL      W#LINENO = *ZERO
     C                   EVAL      W#SUBTOTAL = *ZERO
     C                   EVAL      W#TAXAMT = *ZERO
     C                   EVAL      W#TOTAMT = *ZERO
     C                   EVAL      W#TAXRATE = 0.100
     C                   EVAL      W#TXN = 0
      * [修正#R12] 実行ユーザーをSQLで取得
     C/EXEC SQL
     C+   VALUES USER INTO :W#USER
     C/END-EXEC
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @VLDCUST - 顧客チェック
      *=====================================================================
     C     @VLDCUST      BEGSR
     C     W#CUSTID      CHAIN     CUSTOMERP
     C                   EVAL      W#FOUND = %FOUND(CUSTOMERP)
     C                   IF        NOT W#FOUND
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '顧客が見つかりません'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @VLDSOH - 受注ステータスチェック
      * [修正#DUP] 出荷済(SHIPPED)の受注のみ請求対象とする
      *            INVOICED等の受注に対する二重請求を防止
      *=====================================================================
     C     @VLDSOH       BEGSR
     C     W#SOID        CHAIN(N)  SORDERHP
     C                   IF        NOT %FOUND(SORDERHP)
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '受注が見つかりません'
     C                   LEAVESR
     C                   ENDIF
      * [修正#PART] 一部出荷(PARTIAL)も出荷済分を請求できる
     C                   IF        STATUS <> 'SHIPPED' AND
     C                             STATUS <> 'PARTIAL'
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '未出荷の受注は請求できません'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#SOSTAT = STATUS
     C                   EVAL      W#CURRCD = CURRCD
     C                   EVAL      W#PAYTERM = PAYTERM
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @GENINVID - 請求ID採番
      *=====================================================================
     C     @GENINVID     BEGSR
      * [修正#R1] SQLシーケンス採番（重複リスク解消）
      * 修正#SEQ: 'LIB/SEQ'の'/'は除算と解釈され採番に失敗
      *           していた(0のまま登録)。修飾なしで*LIBLから解決する
     C/EXEC SQL
     C+   VALUES NEXT VALUE FOR O2C_INVID_SEQ
     C+          INTO :W#INVID
     C/END-EXEC
     C                   IF        SQLCODE <> 0 OR W#INVID <= 0
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '請求ID採番エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @PRCSHP - 出荷データ処理
      * [修正#4] W#UNITPRC のハードコード(1000.0000)を廃止
      *=====================================================================
     C     @PRCSHP       BEGSR
      * [修正#MULTI] 受注の出荷を全件読み、未請求(SHIPPED)のものを請求する
      *              (従来は最初の出荷1件のみ。分納の2回目以降が漏れる)
     C                   EVAL      W#SHPCNT = 0
     C                   EVAL      W#SHPEOF = *OFF
     C     W#SOID        SETLL     SHIPMENHL1
     C                   DOW       NOT W#SHPEOF
     C     W#SOID        READE(E)  SHIPMENHL1
     C                   IF        %ERROR
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '出荷データの読込に失敗しました'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#SHPEOF = %EOF(SHIPMENHL1)
     C                   IF        W#SHPEOF
     C                   LEAVE
     C                   ENDIF
      * 請求済の出荷は対象外
      * 修正#SCN: 取り消した出荷(CANCELLED)も対象外
     C                   IF        SL1_STATUS = 'INVOICED' OR
     C                             SL1_STATUS = 'CANCELLED'
     C                   ITER
     C                   ENDIF
     C                   EVAL      W#SHPID = SL1_SHPID
     C                   EXSR      @PRCSHPL
     C                   IF        W#ERROR
     C                   LEAVESR
     C                   ENDIF
      * 請求済にする(COMMITは請求書と一緒に@ENDで行う)
     C                   EVAL      SL1_STATUS = 'INVOICED'
     C                   EVAL      SL1_UPDUSER = W#USER
     C                   EVAL      SL1_UPDDATE = WDATE8
     C                   EVAL      SL1_UPDTIM = WTIME6
     C                   UPDATE(E) SL1REC
     C                   IF        %ERROR
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '出荷データの更新に失敗しました'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#SHPCNT = W#SHPCNT + 1
     C                   ENDDO
     C                   IF        W#SHPCNT = 0
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '請求対象の出荷がありません'
     C                   LEAVESR
     C                   ENDIF
      * [修正#TAX2] 税率ごとに1回だけ切り捨てて税額を求める
     C                   EVAL      W#TAXAMT = 0
     C                   EVAL      W#K = 1
     C                   DOW       W#K <= W#TXN
     C                   EVAL      W#TAXAMT = W#TAXAMT +
     C                             %INT(W#TXBASE(W#K) * W#TXRT(W#K))
     C                   EVAL      W#K = W#K + 1
     C                   ENDDO
     C                   EVAL      W#TOTAMT = W#SUBTOTAL + W#TAXAMT
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @ADDTAX - 税率別の税抜金額に加算
      * [修正#TAX2] W#TAXRATE/W#LINEAMT がセットされていること
      *=====================================================================
     C     @ADDTAX       BEGSR
     C                   EVAL      W#K = 1
     C                   DOW       W#K <= W#TXN
     C                   IF        W#TXRT(W#K) = W#TAXRATE
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#K = W#K + 1
     C                   ENDDO
     C                   IF        W#K > W#TXN
     C                   IF        W#TXN >= 10
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '税率の種類が多すぎます'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#TXN = W#TXN + 1
     C                   EVAL      W#K = W#TXN
     C                   EVAL      W#TXRT(W#K) = W#TAXRATE
     C                   EVAL      W#TXBASE(W#K) = 0
     C                   ENDIF
     C                   EVAL      W#TXBASE(W#K) = W#TXBASE(W#K) + W#LINEAMT
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @PRCSHPL - 出荷1件分の明細を請求明細にする
      * [修正#MULTI] @PRCSHPから分離。W#SHPIDがセットされていること
      *=====================================================================
     C     @PRCSHPL      BEGSR
      * W#SOID は SHIPMENHL1 の SL1_SOID と同じだが念のため確認済み
      * 出荷明細処理
     C     W#SHPID       SETLL     SHIPMENTLP
     C                   EVAL      W#EOF = *OFF
     C                   DOW       NOT W#EOF
     C     W#SHPID       READE     SHIPMENTLP                             99
     C                   EVAL      W#EOF = *IN99
     C                   IF        W#EOF
     C                   LEAVE
     C                   ENDIF
      * 出荷明細データ取得
     C                   EVAL      W#SOLINENO = SOLNO
     C                   EVAL      W#ITEMID = ITEMID
     C                   EVAL      W#QTY = SHPQTY
      * [修正#4] 受注明細（SORDERLP）から実際の単価・税率を取得
     C     SOLKEY        CHAIN     SORDERLP                           99
     C                   IF        NOT *IN99
      * 受注明細から単価と税率を取得
     C                   EVAL      W#UNITPRC = UNITPRC
     C                   EVAL      W#TAXRATE = TAXRATE
     C                   ELSE
      * 受注明細が見つからない場合はエラーとして処理
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '受注明細が見つかりません'
     C                   LEAVESR
     C                   ENDIF
      * [修正#9] ITEMPから商品説明を取得
     C     W#ITEMID      CHAIN     ITEMP
      * 金額計算（税別単価 × 出荷数量）
     C                   EVAL      W#LINEAMT = W#QTY * W#UNITPRC
      * 請求明細書き込み
     C                   EXSR      @WRTINVL
      * 金額集計
     C                   EVAL      W#SUBTOTAL = W#SUBTOTAL + W#LINEAMT
      * [修正#R7] %INTで切り捨て端数処理を明示
      * [修正#TAX2] 税額は明細では計算せず税率別に税抜金額を集計
     C                   EXSR      @ADDTAX
     C                   ENDDO
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @WRTINVH - 請求ヘッダ書き込み
      *=====================================================================
     C     @WRTINVH      BEGSR
     C                   EVAL      INVID = W#INVID
      * [修正#12] INVNUM を W#INVID から自動生成（'INV-' + 採番ID）
     C                   EVAL      INVNUM = 'INV-' + %CHAR(W#INVID)
     C                   EVAL      CUSTID = W#CUSTID
     C                   EVAL      SOID = W#SOID
     C                   EVAL      INVDATE = WDATE8
      * [修正#DUE] 支払期限は支払条件から(従来は一律30日)
     C                   SELECT
     C                   WHEN      %TRIM(W#PAYTERM) = 'NET60'
     C                   EVAL      W#DUEDAYS = 60
     C                   WHEN      %TRIM(W#PAYTERM) = 'COD' OR
     C                             %TRIM(W#PAYTERM) = 'PREPAID'
     C                   EVAL      W#DUEDAYS = 0
     C                   OTHER
     C                   EVAL      W#DUEDAYS = 30
     C                   ENDSL
     C                   EVAL      DUEDATE = %DEC(%DATE(WDATE8:*ISO)
     C                               + %DAYS(W#DUEDAYS):*ISO)
      * [修正#DUE] 通貨は受注の通貨(従来はJPY固定)
     C                   IF        W#CURRCD = *BLANKS
     C                   EVAL      CURRCD = 'JPY'
     C                   ELSE
     C                   EVAL      CURRCD = W#CURRCD
     C                   ENDIF
     C                   EVAL      SUBTOT = W#SUBTOTAL
     C                   EVAL      TAXAMT = W#TAXAMT
     C                   EVAL      TOTAMT = W#TOTAMT
     C                   EVAL      PAIDAMT = *ZERO
     C                   EVAL      BALANCE = W#TOTAMT
     C                   EVAL      STATUS = 'OPEN'
      * [修正#R12] 監査ユーザーを実行ユーザーに変更
     C                   EVAL      CRTUSER = W#USER
     C                   EVAL      CRTDATE = WDATE8
     C                   EVAL      CRTTIM = WTIME6
     C                   EVAL      UPDUSER = W#USER
     C                   EVAL      UPDDATE = WDATE8
     C                   EVAL      UPDTIM = WTIME6
     C                   WRITE     INVREC
     C                   EVAL      W#CNTR = W#CNTR + 1
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @WRTINVL - 請求明細書き込み
      *=====================================================================
     C     @WRTINVL      BEGSR
     C                   EVAL      W#LINENO = W#LINENO + 1
     C                   EVAL      INVID = W#INVID
     C                   EVAL      LINENO = W#LINENO
     C                   EVAL      SOID = W#SOID
     C                   EVAL      SHPID = W#SHPID
     C                   EVAL      ITEMID = W#ITEMID
      * [修正#9] 商品説明をDESCにセット（ITEMP未ヒット時は空白）
     C                   IF        %FOUND(ITEMP)
     C                   EVAL      DESC = %SUBST(ITM_ITEMDSC:1:
     C                               %MIN(%LEN(%TRIMR(ITM_ITEMDSC)):240))
     C                   ELSE
     C                   EVAL      DESC = *BLANKS
     C                   ENDIF
     C                   EVAL      QTY = W#QTY
     C                   EVAL      UNITPRC = W#UNITPRC
     C                   EVAL      TAXRATE = W#TAXRATE
     C                   EVAL      LINEAMT = W#LINEAMT
     C                   WRITE     INVLREC
     C                   EVAL      W#CNTR = W#CNTR + 1
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @UPDSOH - 受注ヘッダ更新
      * [修正#R2] CHAIN(E)でロック取得しLost Update防止
      *=====================================================================
     C     @UPDSOH       BEGSR
     C     W#SOID        CHAIN(E)  SORDERHP
     C                   IF        NOT %ERROR AND %FOUND(SORDERHP)
      * [修正#PART] 全量出荷済(SHIPPED)の時だけ請求済にする
     C                   IF        STATUS = 'SHIPPED'
     C                   EVAL      STATUS = 'INVOICED'
     C                   ENDIF
      * [修正#R12] 監査ユーザーを実行ユーザーに変更
     C                   EVAL      UPDUSER = W#USER
     C                   EVAL      UPDDATE = WDATE8
     C                   EVAL      UPDTIM = WTIME6
     C                   UPDATE    SOHREC
     C                   ELSE
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '受注ヘッダ更新失敗'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @END - 終了処理
      * [修正#R13] 成功時COMMIT、失敗時ROLBKを追加
      *=====================================================================
     C     @END          BEGSR
     C                   IF        NOT W#ERROR
      * 成功: 全DB操作をコミット
     C                   COMMIT
      * [修正#PRT] 確定した請求書を印刷(失敗しても請求書は取り消さない)
     C                   MONITOR
     C                   CALLP     INVPRINT(INVNUM)
     C                   ON-ERROR
     C                   ENDMON
     C                   EVAL      W#ERRMSG = '請求書を発行しました'
     C                   ELSE
      * 失敗: 全DB操作をロールバック
     C                   ROLBK
     C                   EVAL      W#ESCMSG = W#ERRMSG
     C                   EVAL      W#ERRMSG = '請求書の発行に失敗しました'
      * [修正#DUP] 呼び出し元(CRTINVCL)へエスケープで失敗を通知
     C                   EVAL      *INLR = *ON
     C                   CALLP     QMHSNDPM('CPF9898':'QCPFMSG   *LIBL':
     C                             W#ESCMSG:%LEN(%TRIMR(W#ESCMSG)):
     C                             '*ESCAPE':'*PGMBDY':1:W#MSGKEY:W#APIERR)
     C                   ENDIF
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDSR
