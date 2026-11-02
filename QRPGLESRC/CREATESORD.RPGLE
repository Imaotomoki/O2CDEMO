     H DATEDIT(*YMD) DECEDIT('0.') COPYRIGHT('DEMOAPP')
      *=====================================================================
      * プログラム名: CREATESORD
      * 説明: 受注登録プログラム（ヘッダ+明細の一括登録）
      * ライブラリ: DEMOAPP
      * 変更履歴:
      *   修正#3 @ALCINV - INVENTORYP のCHAINキーを W#WHID 単体から
      *          複合キー INVKEY(W#WHID+W#ITEMID) に変更
      *   修正#R1 @GENSOID - タイムスタンプ採番を SQL シーケンスに変更
      *           (重複リスク解消)
      *   修正#R5 *ENTRY PLIST 追加 - CUSTID/WHID/TAXRATE をパラメータ化
      *           @INZ のハードコード廃止
      *   修正#R12 @PRCHDR - 監査ユーザーを USER 特殊レジスタに変更
      *   修正#R14 @PRCLN1-3廃止 - 明細配列をパラメータで受取
      *            ITEMID/ORDQTY/UNITPRC 各20要素配列 + 件数を追加
      *   修正#SEQ @GENSOID - シーケンス名の修飾を外しSQLCODEを確認
      *   修正#CMT 受注/明細/在庫をコミットメント制御で一括確定
      *            エラー時はROLBKしCPF9898エスケープで呼び出し元へ通知
      *   修正#HDR 明細を先に処理し合計金額(TOTAMT)を設定してヘッダ書込
      *            希望納期は日付演算(従来はYYYYMMDD+10で無効日付)
      *            明細金額は税抜(ORDERENTRY/請求書と統一)
      *   修正#NODSP DSPLYを廃止(バッチ実行で応答待ちになるため)
      *   修正#RULE 受注入力(ORDERENTRY)と同じ業務ルールを確認する
      *          取引停止の顧客・販売停止の商品は受注できない
      *          与信限度額を超える受注はできない(限度額0は無制限)
      *          通貨は顧客の通貨(JPY固定だった)
      *=====================================================================

      *---------------------------------------------------------------
      * ファイル定義
      *---------------------------------------------------------------
      * 修正#CMT: コミットメント制御下で更新(STRCMTCTL後に開くためUSROPN)
     FSORDERHP  O    E             DISK    COMMIT USROPN
     FSORDERLP  O    E             DISK    COMMIT USROPN
     FINVENTORYPUF   E           K DISK    COMMIT USROPN
     FITEMP     IF   E           K DISK    PREFIX(ITM_)
     FCUSTOMERP IF   E           K DISK    PREFIX(CUST_)

      *---------------------------------------------------------------
      * 変数定義
      *---------------------------------------------------------------
     D WTIMESTAMP      S               Z
     D WDATE8          S              8S 0
     D WTIME6          S              6S 0
     D W#CNTR          S              9S 0
     D W#SOID          S             18P 0
     D W#LINENO        S              9P 0
     D W#TOTAMT        S             15P 2
      * 修正#RULE 与信チェック用
     D W#AR            S             15P 2
     D W#UNSHIP        S             15P 2
     D W#UNBILL        S             15P 2
     D W#NEWAMT        S             15P 2
     D W#LINEAMT       S             15P 2
     D W#FOUND         S               N
     D W#ERROR         S               N
     D W#ERRMSG        S             52A
      * [修正#R5] エントリパラメータ受取用変数
     D W#CUSTID        S              9P 0
     D W#WHID          S              4P 0
     D W#TAXRATE       S              5P 3
      * 明細処理用変数
     D W#ITEMID        S              9P 0
     D W#ORDQTY        S             15P 3
     D W#UNITPRC       S             15P 4
      * [修正#R12] 実行ユーザー取得用
     D W#USER          S             10A
      * [修正#R14] 明細配列パラメータ（最大20行）
     D W#LINECOUNT     S             10I 0
     D W#P_ITEMID      S              9P 0 DIM(20)
     D W#P_ORDQTY      S             15P 3 DIM(20)
     D W#P_UNITPRC     S             15P 4 DIM(20)
      * ループカウンタ
     D W#IDX           S             10I 0
      * 修正#CMT: コミットメント制御開始/エスケープメッセージ送信用
     D W#CMD           S            200A
     D QCMDEXC         PR                  EXTPGM('QCMDEXC')
     D   Cmd                        200A   CONST OPTIONS(*VARSIZE)
     D   CmdLen                      15P 5 CONST
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
      * キーリスト: 在庫複合キー（WHID + ITEMID）
      * [修正#3] @ALCINV で使用する複合キーを定義
      *---------------------------------------------------------------
     C     INVKEY        KLIST
     C                   KFLD                    W#WHID
     C                   KFLD                    W#ITEMID

      *=====================================================================
      * エントリパラメータ
      * [修正#R5] CUSTID / WHID / TAXRATE をパラメータとして受け取る
      * [修正#R14] 明細件数・ITEMID・ORDQTY・UNITPRC 配列を追加
      *=====================================================================
     C     *ENTRY        PLIST
     C                   PARM                    W#CUSTID
     C                   PARM                    W#WHID
     C                   PARM                    W#TAXRATE
     C                   PARM                    W#LINECOUNT
     C                   PARM                    W#P_ITEMID
     C                   PARM                    W#P_ORDQTY
     C                   PARM                    W#P_UNITPRC

      *=====================================================================
      * メインルーチン
      *=====================================================================
     C                   EXSR      @INZ
     C                   EXSR      @VLDCUST
     C                   IF        W#ERROR
     C                   EXSR      @END
     C                   ENDIF
     C                   EXSR      @GENSOID
     C                   IF        W#ERROR
     C                   EXSR      @END
     C                   ENDIF
      * 修正#HDR: 明細(引当)を先に処理して合計を求めてからヘッダを書く
     C                   EXSR      @PRCLINES
     C                   IF        W#ERROR
     C                   EXSR      @END
     C                   ENDIF
      * 修正#RULE 与信チェック(明細の合計が出てから)
     C                   EXSR      @CHKCRD
     C                   IF        W#ERROR
     C                   EXSR      @END
     C                   ENDIF
     C                   EXSR      @PRCHDR
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
     C                   EVAL      W#TOTAMT = *ZERO
      * [修正#R12] 実行ユーザーをSQLで取得
     C/EXEC SQL
     C+   VALUES USER INTO :W#USER
     C/END-EXEC
      * [修正#R5] CUSTID/WHID/TAXRATE は *ENTRY PLIST で受け取る
      * 修正#CMT: コミットメント制御を開始してから更新ファイルを開く
     C                   EVAL      W#CMD = 'STRCMTCTL LCKLVL(*CHG) ' +
     C                             'CMTSCOPE(*ACTGRP)'
     C                   MONITOR
     C                   CALLP     QCMDEXC(W#CMD:%LEN(%TRIMR(W#CMD)))
     C                   ON-ERROR
      * 既に開始済み(CPF8351)ならそのまま使う
     C                   ENDMON
     C                   OPEN      SORDERHP
     C                   OPEN      SORDERLP
     C                   OPEN      INVENTORYP
      * 修正#CMT: 明細件数の範囲チェック(配列の範囲外参照を防止)
     C                   IF        W#LINECOUNT < 1 OR W#LINECOUNT > 20
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '明細件数が不正です(1-20)'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @VLDCUST - 顧客存在チェック
      *=====================================================================
     C     @VLDCUST      BEGSR
     C     W#CUSTID      CHAIN     CUSTOMERP
     C                   EVAL      W#FOUND = %FOUND(CUSTOMERP)
     C                   IF        NOT W#FOUND
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '顧客が見つかりません'
     C                   LEAVESR
     C                   ENDIF
      * 修正#RULE 取引停止の顧客は受注できない
     C                   IF        CUST_STATUS <> 'ACTIVE'
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '取引停止の顧客です'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @GENSOID - 受注ID採番
      *=====================================================================
     C     @GENSOID      BEGSR
      * [修正#R1] SQLシーケンスによる採番（重複リスク解消）
      * 修正#SEQ: 'LIB/SEQ'の'/'は除算と解釈され採番に失敗
      *           していた(0のまま登録)。修飾なしで*LIBLから解決する
     C/EXEC SQL
     C+   VALUES NEXT VALUE FOR O2C_SOID_SEQ INTO :W#SOID
     C/END-EXEC
     C                   IF        SQLCODE <> 0 OR W#SOID <= 0
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '受注ID採番エラー'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @PRCHDR - ヘッダ処理
      *=====================================================================
     C     @PRCHDR       BEGSR
     C                   EVAL      SOID = W#SOID
      * [修正#R5] 受注番号をシーケンス番号から生成
     C                   EVAL      SONUM = 'SO-' + %CHAR(W#SOID)
     C                   EVAL      CUSTID = W#CUSTID
     C                   EVAL      ORDDATE = WDATE8
      * 修正#HDR: 日付として10日後(従来は数値+10で無効日付になった)
     C                   EVAL      REQDATE = %DEC(%DATE(WDATE8:*ISO) +
     C                             %DAYS(10):*ISO)
      * 修正#RULE 通貨は顧客の通貨
     C                   EVAL      CURRCD = CUST_CURRCD
     C                   IF        CURRCD = *BLANKS
     C                   EVAL      CURRCD = 'JPY'
     C                   ENDIF
     C                   EVAL      PAYTERM = 'NET30'
     C                   EVAL      WHID = W#WHID
     C                   EVAL      STATUS = 'ALLOCATED'
      * 修正#HDR: 明細合計(税抜)
     C                   EVAL      TOTAMT = W#TOTAMT
      * [修正#R12] 監査ユーザーを実行ユーザーに変更
     C                   EVAL      CRTUSER = W#USER
     C                   EVAL      CRTDATE = WDATE8
     C                   EVAL      CRTTIM = WTIME6
     C                   EVAL      UPDUSER = W#USER
     C                   EVAL      UPDDATE = WDATE8
     C                   EVAL      UPDTIM = WTIME6
     C                   WRITE(E)  SOHREC
     C                   IF        %ERROR
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '受注ヘッダの登録に失敗'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#CNTR = W#CNTR + 1
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @PRCLINES - 全明細処理ループ
      * [修正#R14] @PRCLN1-3廃止 - パラメータ配列をループで処理
      *=====================================================================
     C     @PRCLINES     BEGSR
     C                   EVAL      W#IDX = 1
     C                   DOW       W#IDX <= W#LINECOUNT AND NOT W#ERROR
     C                   EVAL      W#ITEMID = W#P_ITEMID(W#IDX)
     C                   EVAL      W#ORDQTY = W#P_ORDQTY(W#IDX)
     C                   EVAL      W#UNITPRC = W#P_UNITPRC(W#IDX)
     C                   EXSR      @PRCLINE
     C                   EVAL      W#IDX = W#IDX + 1
     C                   ENDDO
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @PRCLINE - 明細共通処理
      *   W#ITEMID / W#ORDQTY / W#UNITPRC がセットされていること
      *=====================================================================
     C     @PRCLINE      BEGSR
      * 修正#CMT: RETURNは@END(ROLBK)を通らずに終わるためLEAVESR
     C                   EXSR      @VLDITEM
     C                   IF        W#ERROR
     C                   LEAVESR
     C                   ENDIF
     C                   EXSR      @ALCINV
     C                   IF        W#ERROR
     C                   LEAVESR
     C                   ENDIF
     C                   EXSR      @WRTLINE
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @VLDITEM - 商品存在チェック
      *=====================================================================
     C     @VLDITEM      BEGSR
     C     W#ITEMID      CHAIN     ITEMP
     C                   EVAL      W#FOUND = %FOUND(ITEMP)
     C                   IF        NOT W#FOUND
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '商品が見つかりません'
     C                   LEAVESR
     C                   ENDIF
      * 修正#RULE 販売停止の商品は受注できない
     C                   IF        ITM_STATUS <> 'ACTIVE'
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '販売停止の商品です'
     C                   LEAVESR
     C                   ENDIF
      * 修正#CMT: 数量は正の値のみ
     C                   IF        W#ORDQTY <= 0 OR W#UNITPRC < 0
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '数量または単価が不正です'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @ALCINV - 在庫引当処理
      * [修正#3] W#WHID 単体のCHAINから INVKEY(W#WHID+W#ITEMID) に変更
      *=====================================================================
     C     @ALCINV       BEGSR
     C     INVKEY        CHAIN     INVENTORYP                         99
     C                   IF        *IN99
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '在庫が見つかりません'
     C                   LEAVESR
     C                   ENDIF
      * 引当可能数量チェック
     C                   IF        QTYAVL < W#ORDQTY
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '在庫数量が不足しています'
     C                   LEAVESR
     C                   ENDIF
      * 引当数量更新
     C                   EVAL      QTYALC = QTYALC + W#ORDQTY
     C                   EVAL      QTYAVL = QTYOH - QTYALC
     C                   EVAL      LSTUPD = WTIMESTAMP
     C                   UPDATE(E) INVREC
     C                   IF        %ERROR
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '在庫の更新に失敗しました'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @WRTLINE - 明細書き込み
      *=====================================================================
     C     @WRTLINE      BEGSR
     C                   EVAL      W#LINENO = W#LINENO + 1
     C                   EVAL      SOID = W#SOID
     C                   EVAL      LINENO = W#LINENO
     C                   EVAL      ITEMID = W#ITEMID
     C                   EVAL      ORDQTY = W#ORDQTY
     C                   EVAL      UNITPRC = W#UNITPRC
     C                   EVAL      TAXRATE = W#TAXRATE
      * 明細金額計算
      * 修正#HDR: 税抜(税額は請求書発行時に税率から計算する)
     C                   EVAL(H)   W#LINEAMT = W#ORDQTY * W#UNITPRC
     C                   EVAL      LINEAMT = W#LINEAMT
     C                   EVAL      ALCQTY = W#ORDQTY
     C                   EVAL      SHPQTY = *ZERO
     C                   EVAL      STATUS = 'ALLOCATED'
     C                   WRITE(E)  SOLREC
     C                   IF        %ERROR
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '受注明細の登録に失敗'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#CNTR = W#CNTR + 1
      * 合計金額に加算
     C                   EVAL      W#TOTAMT = W#TOTAMT + W#LINEAMT
     C                   ENDSR

      *=====================================================================
      * 修正#RULE @CHKCRD - 与信チェック(ORDERENTRYと同じ式)
      *   未収残高+未出荷の受注額+出荷済未請求額+今回の受注額
      *   (すべて税込)が与信限度額を超えたらエラー。限度額0は無制限
      *=====================================================================
     C     @CHKCRD       BEGSR
     C                   IF        CUST_CREDLMT <= 0
     C                   LEAVESR
     C                   ENDIF
     C/EXEC SQL
     C+   SELECT COALESCE(SUM(BALANCE), 0) INTO :W#AR
     C+     FROM INVOICEHP
     C+    WHERE CUSTID = :W#CUSTID AND BALANCE > 0
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '与信情報の取得に失敗しました'
     C                   LEAVESR
     C                   ENDIF
     C/EXEC SQL
     C+   SELECT COALESCE(SUM((L.ORDQTY - L.SHPQTY) * L.UNITPRC
     C+          * (1 + L.TAXRATE)), 0) INTO :W#UNSHIP
     C+     FROM SORDERLP L JOIN SORDERHP H ON H.SOID = L.SOID
     C+    WHERE H.CUSTID = :W#CUSTID
     C+      AND H.STATUS IN ('OPEN', 'ALLOCATED', 'PARTIAL')
     C+      AND L.ORDQTY > L.SHPQTY
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '与信情報の取得に失敗しました'
     C                   LEAVESR
     C                   ENDIF
     C/EXEC SQL
     C+   SELECT COALESCE(SUM(D.SHPQTY * L.UNITPRC
     C+          * (1 + L.TAXRATE)), 0) INTO :W#UNBILL
     C+     FROM SHIPMENTHP S
     C+     JOIN SHIPMENTLP D ON D.SHPID = S.SHPID
     C+     JOIN SORDERLP L ON L.SOID = S.SOID AND L.LINENO = D.SOLNO
     C+     JOIN SORDERHP H ON H.SOID = S.SOID
     C+    WHERE H.CUSTID = :W#CUSTID AND S.STATUS = 'SHIPPED'
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '与信情報の取得に失敗しました'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL(H)   W#NEWAMT = W#TOTAMT * (1 + W#TAXRATE)
     C                   IF        W#NEWAMT > CUST_CREDLMT - W#AR - W#UNSHIP
     C                             - W#UNBILL
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '与信限度額を超えています'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @END - 終了処理
      *=====================================================================
     C     @END          BEGSR
      * 修正#CMT: 成功はCOMMIT、失敗はROLBKしてエスケープで通知
     C                   IF        NOT W#ERROR
     C                   COMMIT
     C                   ELSE
     C                   IF        %OPEN(SORDERHP)
     C                   ROLBK
     C                   ENDIF
     C                   EVAL      *INLR = *ON
     C                   CALLP     QMHSNDPM('CPF9898':'QCPFMSG   *LIBL':
     C                             W#ERRMSG:%LEN(%TRIMR(W#ERRMSG)):
     C                             '*ESCAPE':'*PGMBDY':1:W#MSGKEY:W#APIERR)
     C                   ENDIF
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDSR
