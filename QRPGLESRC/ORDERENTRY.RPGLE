     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *===============================================================
      * プログラム名: ORDERENTRY
      * 機能: 受注入力画面プログラム
      * 作成日: 2025-12-18
      * 説明: 5250画面を使用した対話型受注入力処理
      * 変更履歴:
      *   修正#1  - TOTAMT/ERRMSG初期化をワーク変数(W#TOTAMT)に統一
      *   修正#2  - REQDATE計算 %DATE+%DAYS / UNITPRC 9S4->13S4
      *   修正#3  - ITEML1作成 / RENAME CUSTREC->CSTREC,ITEMREC->ITMREC
      *   修正#4  - CHAIN前にWHID(4S0)->W#WHID(4P0)変換
      *   修正#5  - DIM(20) / F6=確認 / 合計リアルタイム更新
      *   修正#6  - @INPDETAIL再帰廃止 / DOWループ継続
      *   修正#9  - ORDDATE/REQDATE 8S0->8A
      *  修正#13  - OQTY_O/UPRC_O(表示用)セット処理追加
      *  修正#15  - L_ORDQTY: 15P3->15P0(整数)
      *  修正#16  - ORDQTY/UNITPRCをB型に統合
      *  修正#17  - L_UNITPRC/L_LINEAMT 整数化
      *  修正#PRC - 単価を文字で入力(外貨は小数2桁まで。円は整数)
      *  修正#19  - WDATE8 YYYYMMDD format fix
      *  修正#F4  - F4: Customer/Warehouse/Item(WH) search windows
      *  修正#KEY - WRITE DETAILKEY をEXFMT直前に移動(ASSUME削除)
      *  修正#TAX - 受注明細TAXRATEを顧客TAXCATから設定(未設定で0%)
      *  修正#A - 受注日/納期の日付妥当性チェック(RNX0105防止)
      *  修正#B - 名称の切り詰めを全角対応(DbcsTrunc)
      *  修正#C - 明細エラー時に入力行を保持(SFLNXTCHG)
      *  修正#D - 在庫チェックを同一商品の合計数量で判定
      *  修正#E - @REDRAWSFLでRRN設定(5=削除/F12でRNQ1255)
      *  修正#G - 確認画面F12の2回目で登録される不具合
      *  修正#H/J/K/L - 金額桁あふれ/倉庫在庫/採番時刻/ロック
      *  修正#M - 入力用空行をW#LASTRRNで管理(空行増殖/二重WRITE)
      *  修正#CMT - 在庫引当/受注/明細をコミットメント制御で一括確定
      *  修正#CRD - 与信チェック(未収+未出荷+未請求+今回 > 限度額)
      *            取引停止の顧客・販売停止の商品はエラー
      *===============================================================

      *---------------------------------------------------------------
      * ファイル定義
      *---------------------------------------------------------------
     FORDENTRYD CF   E             WORKSTN SFILE(DETAIL:SFLRRN)
     F                                     SFILE(CUSTSFL:CSFRRN)
     F                                     SFILE(WHSFL:WSFRRN)
     F                                     SFILE(ITMSFL:ISFRRN)
     FCUSTOMERL1IF   E           K DISK    PREFIX(C_)
     F                                     RENAME(CUSTREC:CSTREC)
     FITEML1    IF   E           K DISK    PREFIX(I_)
     F                                     RENAME(ITEMREC:ITMREC)
     FWAREHOUSEPIF   E           K DISK    PREFIX(W_)
      * 修正#CMT: 受注・明細・在庫はコミットメント制御下で更新する
      *           (STRCMTCTL後に開くためUSROPN)
     FSORDERHP  O    E             DISK    PREFIX(SO_) COMMIT USROPN
     FSORDERLP  O    E             DISK    PREFIX(SL_) COMMIT USROPN
     FINVENTORYPUF   E           K DISK    COMMIT USROPN

      *---------------------------------------------------------------
      * グローバル変数定義
      *---------------------------------------------------------------
     D WDATE8          S              8S 0
     D WTIME6          S              6S 0

     D W#SOID          S             18P 0
     D W#LINENO        S              9P 0
     D W#IDX           S             10I 0
     D W#ERROR         S               N
     D W#LINE_COUNT    S             10I 0
     D SFLRRN          S              4S 0

      * 修正#9: 8A型
     D W#ORDDATE       S              8A
     D W#REQDATE       S              8A
     D W#CURRCD        S              3A
     D W#PAYTERM       S             20A
     D W#WHID          S              4P 0
      * [修正#R10] 15P0->15P2 (DSPF TOTAMT_D=15Y2O に合わせる)
     D W#TOTAMT        S             15P 2
     D W#AMT           S             15P 2
      * 修正#TAX: 顧客の税区分から決定した税率
     D W#TAXRATE       S              5P 3
      * 修正#CRD: 与信チェック用
     D W#CREDLMT       S             15P 2
     D W#AR            S             15P 2
     D W#UNSHIP        S             15P 2
     D W#UNBILL        S             15P 2
     D W#NEWAMT        S             15P 2
     D W#CRDREM        S             15P 2

     D W#ORDDT         S               D
     D W#REQDT         S               D

     D W#CUSTCD        S             20A
     D W#ITEMCD        S             12A
     D W#CUSTID        S              9P 0
     D W#ITEMID        S              9P 0

      *---------------------------------------------------------------
      * 明細行配列（最大20行）
      *---------------------------------------------------------------
     D L_ITEMCD        S             12A   DIM(20)
     D L_ITEMID        S              9P 0 DIM(20)
     D L_ITEMNM        S             22A   DIM(20)
      * [修正#R10] 小数桁を正しく定義
     D L_ORDQTY        S             15P 3 DIM(20)
     D L_UNITPRC       S             15P 4 DIM(20)
     D L_LINEAMT       S             15P 2 DIM(20)
      * 修正#20: 在庫引当用作業変数
     D W#ALCERRFLG     S               N
     D W#ALCERRMSG     S             52A
      * 修正#F4: 検索ウィンドウ作業変数
     D W#SFLEOF        S               N
     D W#SFLRRN        S              4S 0
     D W#ITEMSEL       S               N
     D W#ITEMNM        S             22A
     D W#UNITPRC       S             15P 4
      * 修正#F12: 確認->F12 再入フラグ (ON=SFL再描画のみ)
     D W#REDTL         S               N
      * 修正#G: 画面遷移状態 D=明細 C=確認 H=ヘッダへ X=終了
     D W#STEP          S              1A
      * 修正#C: 入力エラー行の保持(エラー時に入力内容を消さない)
     D W#KEEPLN        S               N
      * 修正#M: 最後にWRITEしたSFLのRRN(入力用空行の管理)
     D W#LASTRRN       S              4S 0
     D W#K_ITEMCD      S             12A
     D W#K_QTY         S              7S 0
     D W#K_PRC         S             15P 4
      * 修正#PRC: 画面の単価(文字)を数値にした値と入力エラー
     D UNITPRC         S             15P 4
     D W#PRCERR        S               N
     D W#PRCMSG        S             50A
     D W#PRCS          S             20A   VARYING
      * 修正#PRC: エラー行は入力した文字のまま再表示する
     D W#K_PRCA        S             14A
     D W#K_IN43        S               N
     D W#RAWPRC        S               N
     D W#K_IN41        S               N
      * 修正#D: 同一商品の合計数量チェック用
     D W#SUMQTY        S             15P 3
     D W#J             S             10I 0
      * 修正#H: 明細金額の桁あふれチェック用
     D W#CHKAMT        S             31P 4
      * 修正#B: 全角(DBCS)を壊さずに切り詰める
     D W#SO            C                   X'0E'
      * 修正#CMT: コミットメント制御開始用
     D QCMDEXC         PR                  EXTPGM('QCMDEXC')
     D   Cmd                        200A   CONST OPTIONS(*VARSIZE)
     D   CmdLen                      15P 5 CONST
     D W#CMD           S            200A
      * 修正#F4P: 共通の検索ウィンドウ(F4)と入力チェック用
     D O2CPRMPT        PR                  EXTPGM('O2CPRMPT')
     D   pType                       10A   CONST
     D   pCdType                     30A   CONST
     D   pValue                      30A
     D W#PVAL          S             30A
     D W#CDCNT         S             10I 0
      * 修正#CMT: 作成者=実行ユーザー(従来は'DSPFUSER'固定)
     D PSDS           SDS
     D   P#USER              254    263A
     D W#SI            C                   X'0F'
     D DbcsTrunc       PR           256A   VARYING
     D   pStr                       256A   VARYING CONST
     D   pMax                        10I 0 CONST

      *---------------------------------------------------------------
      * 修正#20: 在庫引当 複合キー (WHID + ITEMID)
      *---------------------------------------------------------------
     C     INVKEY        KLIST
     C                   KFLD                    W#WHID
     C                   KFLD                    W#ITEMID

      *===============================================================
      * メイン処理
      *===============================================================
      * 修正#CRD: 与信チェックの集計SQLはロックを取らない(*NONE)
     C/EXEC SQL
     C+   SET OPTION COMMIT = *NONE, CLOSQLCSR = *ENDMOD
     C/END-EXEC
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

     C                   DOW       NOT *IN03
     C                   EXSR      @INIT
     C                   EXSR      @INPUT_HEADER

     C                   IF        *IN12
     C                   EVAL      *IN12 = *OFF
     C                   ITER
     C                   ENDIF

     C                   IF        *IN03
     C                   LEAVE
     C                   ENDIF

      * 修正#G: 明細(D)/確認(C)を状態で制御
      *   確認->F12 は何度でも明細へ戻る(従来は2回目のF12で登録)
      *   登録エラー(在庫不足等)は入力を保持して明細へ戻る
     C                   EVAL      W#REDTL = *OFF
     C                   EVAL      W#STEP = 'D'
     C                   DOW       W#STEP = 'D' OR W#STEP = 'C'
     C                   IF        W#STEP = 'D'
     C                   EXSR      @INPDETAIL
     C                   EVAL      W#REDTL = *OFF
     C                   SELECT
     C                   WHEN      *IN03
     C                   EVAL      W#STEP = 'X'
     C                   WHEN      *IN12
     C                   EVAL      *IN12 = *OFF
     C                   EVAL      W#STEP = 'H'
     C                   OTHER
     C                   EVAL      W#STEP = 'C'
     C                   ENDSL
     C                   ELSE
     C                   EXSR      @CONFIRM
     C                   SELECT
     C                   WHEN      *IN03
     C                   EVAL      W#STEP = 'X'
     C                   WHEN      *IN12
     C                   EVAL      *IN12 = *OFF
     C                   EVAL      W#REDTL = *ON
     C                   EVAL      W#STEP = 'D'
     C                   OTHER
     C                   EXSR      @SAVEORD
     C                   IF        W#ERROR
     C                   EVAL      W#REDTL = *ON
     C                   EVAL      W#STEP = 'D'
     C                   ELSE
     C                   EXSR      @SHOWCOMP
     C                   EVAL      W#STEP = 'H'
     C                   ENDIF
     C                   ENDSL
     C                   ENDIF
     C                   ENDDO

     C                   IF        W#STEP = 'X'
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO

     C                   EVAL      *INLR = *ON
     C                   RETURN

     C*===============================================================
     C* @INIT - 初期化
     C*===============================================================
     C     @INIT         BEGSR

     C                   TIME                    WTIME6
      * 修正#19: *DATE は MMDDYY形式になる場合があるため
      *          W#ORDDT(Date型)からYYYYMMDD形式の8桁数値を取得
      * 修正#SYSDT: ジョブ日付はサインオン日のままなのでシステム日付
     C                   EVAL      W#ORDDT = %DATE()
     C                   EVAL      WDATE8 = %INT(%CHAR(W#ORDDT:*ISO0))

     C                   EVAL      W#ERROR = *OFF
     C                   EVAL      W#LINE_COUNT = 0
     C                   EVAL      W#TOTAMT = 0
     C                   EVAL      W#AMT = 0
     C                   EVAL      ERRMSG = *BLANK
     C                   EVAL      ERRMSG_D = *BLANK
     C                   EVAL      W#REDTL = *OFF

     C                   EVAL      *IN03 = *OFF
     C                   EVAL      *IN06 = *OFF
     C                   EVAL      *IN12 = *OFF
     C                   EVAL      *IN31 = *OFF
     C                   EVAL      *IN32 = *OFF
     C                   EVAL      *IN33 = *OFF
     C                   EVAL      *IN34 = *OFF
     C                   EVAL      *IN35 = *OFF

     C                   EVAL      W#REQDT = W#ORDDT + %DAYS(10)
     C                   EVAL      ORDDATE = %CHAR(W#ORDDT:*ISO0)
     C                   EVAL      REQDATE = %CHAR(W#REQDT:*ISO0)
     C                   EVAL      CURRCD = 'JPY'
     C                   EVAL      PAYTERM = 'NET30'
     C                   EVAL      CUSTCD = *BLANK
     C                   EVAL      CUSTNM = *BLANK
     C                   EVAL      WHID = 0
     C                   EVAL      WHNM = *BLANK

     C                   ENDSR

     C*===============================================================
     C* @INPUT_HEADER - ヘッダー入力
     C*===============================================================
     C     @INPUT_HEADER BEGSR

     C                   EVAL      *IN31 = *OFF
     C                   EVAL      *IN32 = *OFF
     C                   EVAL      *IN33 = *OFF
     C                   EVAL      *IN34 = *OFF
     C                   EVAL      *IN35 = *OFF
     C                   EVAL      ERRMSG = *BLANK

     C                   EXFMT     HEADER

     C                   DOW       NOT *IN03 AND NOT *IN12

      * 修正#F4: F4キー検出 - 顧客または倉庫検索
      * 修正#F4P: カーソルのある項目で検索先を決める(通貨・支払条件を追加)
      *   それ以外の項目では従来どおり(顧客未入力なら顧客、入力済みなら倉庫)
     C                   IF        *IN04
     C                   EVAL      *IN04 = *OFF
     C                   SELECT
     C                   WHEN      CSRFLD = 'CURRCD'
     C                   EVAL      W#PVAL = CURRCD
     C                   CALLP     O2CPRMPT('CODE':'CURRENCY':W#PVAL)
     C                   EVAL      CURRCD = W#PVAL
     C                   WHEN      CSRFLD = 'PAYTERM'
     C                   EVAL      W#PVAL = PAYTERM
     C                   CALLP     O2CPRMPT('CODE':'PAYMENT_TERM':W#PVAL)
     C                   EVAL      PAYTERM = W#PVAL
     C                   WHEN      CSRFLD = 'CUSTCD'
     C                   EXSR      @SRCCUST
     C                   WHEN      CSRFLD = 'WHID'
     C                   EXSR      @SRCWH
     C                   WHEN      CUSTCD = *BLANK OR W#CUSTID = *ZERO
     C                   EXSR      @SRCCUST
     C                   OTHER
     C                   EXSR      @SRCWH
     C                   ENDSL
     C                   EXFMT     HEADER
     C                   ITER
     C                   ENDIF

     C                   EXSR      @VALIDHDR

     C                   IF        NOT W#ERROR
     C                   LEAVE
     C                   ENDIF

     C                   EXFMT     HEADER
     C                   ENDDO

     C                   ENDSR

     C*===============================================================
     C* @VALIDHDR - ヘッダーバリデーション
     C*===============================================================
     C     @VALIDHDR     BEGSR

     C                   EVAL      W#ERROR = *OFF
     C                   EVAL      ERRMSG = *BLANK

     C     CUSTCD        CHAIN     CSTREC                             99
     C                   IF        *IN99
     C                   EVAL      *IN31 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '顧客が見つかりません'
     C                   LEAVESR
     C                   ELSE
     C                   EVAL      *IN31 = *OFF
     C                   EVAL      CUSTNM = DbcsTrunc(C_CUSTNM:40)
     C                   EVAL      W#CUSTID = C_CUSTID
      * 修正#CRD: 取引停止の顧客は受注できない
     C                   IF        C_STATUS <> 'ACTIVE'
     C                   EVAL      *IN31 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '取引停止中の顧客です'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#CREDLMT = C_CREDLMT
      * 修正#TAX: 税区分(CODEMSTRP TAX_CAT)から税率を決定
     C                   SELECT
     C                   WHEN      C_TAXCAT = 'REDUCED'
     C                   EVAL      W#TAXRATE = 0.080
     C                   WHEN      C_TAXCAT = 'EXEMPT' OR
     C                             C_TAXCAT = 'EXPORT'
     C                   EVAL      W#TAXRATE = 0
     C                   OTHER
     C                   EVAL      W#TAXRATE = 0.100
     C                   ENDSL
     C                   ENDIF

     C                   IF        ORDDATE = *BLANK
     C                   EVAL      *IN32 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '受注日が無効です'
     C                   LEAVESR
     C                   ELSE
     C                   EVAL      *IN32 = *OFF
     C                   ENDIF

      * 修正#A: 日付の妥当性チェック(YYYYMMDD)
     C     *ISO0         TEST(DE)                ORDDATE
     C                   IF        %ERROR
     C                   EVAL      *IN32 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '受注日が無効です(YYYYMMDD)'
     C                   LEAVESR
     C                   ENDIF
     C     *ISO0         TEST(DE)                REQDATE
     C                   IF        %ERROR
     C                   EVAL      *IN33 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '納期が無効です(YYYYMMDD)'
     C                   LEAVESR
     C                   ENDIF

     C                   IF        REQDATE < ORDDATE
     C                   EVAL      *IN33 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '納期が無効です'
     C                   LEAVESR
     C                   ELSE
     C                   EVAL      *IN33 = *OFF
     C                   ENDIF

      * 修正#F4P: 通貨・支払条件はコードマスターにあるものだけ
     C                   EVAL      *IN34 = *OFF
     C                   EVAL      *IN36 = *OFF
     C/EXEC SQL
     C+   SELECT COUNT(*) INTO :W#CDCNT FROM CODEMSTRP
     C+    WHERE CODETYPE = 'CURRENCY' AND CODEVAL = :CURRCD
     C+      AND ISACTIVE = 'Y'
     C/END-EXEC
     C                   IF        W#CDCNT = 0
     C                   EVAL      *IN34 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '通貨が登録されていません' +
     C                             '(F4で選択)'
     C                   LEAVESR
     C                   ENDIF
     C/EXEC SQL
     C+   SELECT COUNT(*) INTO :W#CDCNT FROM CODEMSTRP
     C+    WHERE CODETYPE = 'PAYMENT_TERM' AND CODEVAL = :PAYTERM
     C+      AND ISACTIVE = 'Y'
     C/END-EXEC
     C                   IF        W#CDCNT = 0
     C                   EVAL      *IN36 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '支払条件が登録されていません' +
     C                             '(F4で選択)'
     C                   LEAVESR
     C                   ENDIF

     C                   EVAL      W#WHID = WHID
     C     W#WHID        CHAIN     WAREHOUSEP                         99
     C                   IF        *IN99
     C                   EVAL      *IN35 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '倉庫が見つかりません'
     C                   LEAVESR
     C                   ELSE
     C                   EVAL      *IN35 = *OFF
     C                   EVAL      WHNM = DbcsTrunc(W_WHNM:35)
     C                   ENDIF

     C                   ENDSR

     C*===============================================================
     C* @INPDETAIL - 明細入力
     C*===============================================================
     C     @INPDETAIL    BEGSR

     C                   EVAL      CUSTCD_D = CUSTCD
     C                   EVAL      CUSTNM_D = CUSTNM
     C                   EVAL      ORDDATE_D = ORDDATE
     C                   EVAL      REQDATE_D = REQDATE
     C                   EVAL      CURRCD_D = CURRCD
     C                   EVAL      PAYTERM_D = PAYTERM
     C                   EVAL      WHID_D = WHID
     C                   EVAL      WHNM_D = WHNM

      * 修正#F12: 通常初回=SFLクリア / 確認->F12=配列から再描画
     C                   EVAL      *IN93 = *ON
     C                   WRITE     DETAILCTL
     C                   EVAL      *IN93 = *OFF
     C                   IF        NOT W#REDTL
      * 通常初回: カウント・合計をゼロクリア
     C                   EVAL      W#LINE_COUNT = 0
     C                   EVAL      SFLRRN = 0
     C                   EVAL      W#LASTRRN = 0
     C                   EVAL      W#TOTAMT = 0
     C                   ELSE
      * 確認->F12: 既存の配列からSFLを再構築
     C                   EVAL      SFLRRN = 0
     C                   EXSR      @REDRAWSFL
     C                   ENDIF
     C                   EVAL      W#ERROR = *OFF
     C                   EVAL      ERRMSG_D = *BLANK
      * 修正#G: 登録エラーで戻った場合はメッセージを残す
     C                   IF        NOT W#REDTL
     C                   EVAL      ERRMSG = *BLANK
     C                   ENDIF
     C                   EVAL      *IN06 = *OFF
     C                   EVAL      W#ITEMSEL = *OFF
     C                   EVAL      W#KEEPLN = *OFF

     C                   DOW       NOT *IN03 AND NOT *IN12
     C                             AND NOT *IN06

     C                   EVAL      *IN06 = *OFF

      * 修正#M: 未確定の入力行がない時だけ空行を追加する
      *   (Enter/F4取消のたびに空行が増え、行番号と配列がずれていた)
      *   (READCでSFLRRNが変わるため既存行への二重WRITEも起きていた)
     C                   IF        W#LASTRRN <= W#LINE_COUNT
      * 入力用空行を追加
     C                   IF        W#LINE_COUNT < 20
     C                   EVAL      SFLRRN = W#LASTRRN + 1
     C                   EVAL      LINENO = SFLRRN
     C                   EVAL      DLTOPT = ' '
     C                   EVAL      ITEMCD = *BLANK
     C                   EVAL      ITEMNM = *BLANK
     C                   EVAL      ORDQTY = 0
     C                   EVAL      UNITPRC = 0
      * 修正#C: エラーになった入力行は内容を残して再表示
      *         (SFLNXTCHGで次回READCの対象にする)
     C                   IF        W#KEEPLN
     C                   EVAL      ITEMCD = W#K_ITEMCD
     C                   EVAL      ORDQTY = W#K_QTY
     C                   EVAL      UNITPRC = W#K_PRC
     C                   EVAL      W#RAWPRC = *ON
     C                   EVAL      *IN41 = W#K_IN41
     C                   EVAL      *IN43 = W#K_IN43
     C                   EVAL      *IN44 = *ON
     C                   ENDIF
     C                   EXSR      @PUTPRC
     C                   WRITE     DETAIL
     C                   EVAL      W#LASTRRN = SFLRRN
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN43 = *OFF
     C                   EVAL      *IN44 = *OFF
     C                   EVAL      W#KEEPLN = *OFF
     C                   ENDIF
     C                   ENDIF
     C                   EVAL      W#ITEMSEL = *OFF

     C                   EVAL      TOTAMT_D = W#TOTAMT
      * 修正#CUR: 外貨は小数2桁と通貨コードで表示
     C                   EVAL      *IN46 = (CURRCD <> 'JPY')
     C                   EVALR     TOTAMT_DF = %EDITC(%DEC(W#TOTAMT:13:2):'1')
     C                   EVAL      CUR_T = CURRCD

     C                   EVAL      *IN91 = *ON
     C                   EVAL      *IN92 = *ON
     C                   EVAL      ERRMSG_D = ERRMSG
      * 修正#KEY: WRITE DETAILKEY をEXFMT直前に毎回実行
      *          SFLCLRでクリアされた後に再描画するため消えない
     C                   WRITE     DETAILKEY
     C                   EXFMT     DETAILCTL
     C                   EVAL      ERRMSG = *BLANK
     C                   EVAL      ERRMSG_D = *BLANK

     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF

      * 修正#F4: 明細画面でのF4 - 商品検索(倉庫絞込)
      * 選択後は現在行(SFLRRN)をUPDATEしてW#ITEMSELをONにする
     C                   IF        *IN04
     C                   EVAL      *IN04 = *OFF
      * 修正#I: 前回READCの値が残らないようクリア(F12取消時の誤設定防止)
     C                   EVAL      ITEMCD = *BLANK
     C                   EVAL      UNITPRC = 0
     C                   EXSR      @SRCITEM
     C                   EVAL      ERRMSG = *BLANK
      * @SRCITEMで選択された場合は現在行をUPDATE
      * SFLのUPDATEには事前にCHAIN(読取)が必要
      * CHAINでフィールドが上書きされるため事前退避
      * 修正#M: 更新先は未確定の入力行(W#LASTRRN)
     C                   IF        ITEMCD <> *BLANK AND
     C                             W#LASTRRN > W#LINE_COUNT
     C                   EVAL      SFLRRN = W#LASTRRN
     C                   EVAL      W#ITEMCD = ITEMCD
     C                   EVAL      W#ITEMNM = %SUBST(ITEMNM:1:22)
     C                   EVAL      W#UNITPRC = UNITPRC
     C     SFLRRN        CHAIN(E)  DETAIL
     C                   IF        %FOUND(ORDENTRYD)
     C                   EVAL      LINENO = SFLRRN
     C                   EVAL      ITEMCD = W#ITEMCD
     C                   EVAL      ITEMNM = W#ITEMNM
     C                   EVAL      UNITPRC = W#UNITPRC
      * 修正#C: 未入力のまま実行されても検証されるようSFLNXTCHG
     C                   EVAL      *IN44 = *ON
     C                   EXSR      @PUTPRC
     C                   UPDATE    DETAIL
     C                   EVAL      *IN44 = *OFF
     C                   EVAL      W#ITEMSEL = *ON
     C                   ENDIF
     C                   ENDIF
     C                   ITER
     C                   ENDIF

     C                   IF        *IN06
     C                   IF        W#LINE_COUNT = 0
     C                   EVAL      ERRMSG = '商品を1件以上入力してください'
     C                   EVAL      *IN06 = *OFF
     C                   EVAL      *IN93 = *ON
     C                   WRITE     DETAILCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      SFLRRN = 0
     C                   EXSR      @REDRAWSFL
     C                   ENDIF
     C                   ITER
     C                   ENDIF

     C                   READC     DETAIL                                 99

     C                   IF        *IN99
     C                   ITER
     C                   ENDIF
      * 修正#PRC: 画面の単価(文字)を数値にする
     C                   EXSR      @GETPRC

      * 5=行削除: 配列から削除して再描画
     C                   IF        DLTOPT = '5'
      * 削除対象の行番号(=配列インデックス)をSFLRRNから取得
     C                   EVAL      W#IDX = SFLRRN
      * 配列未登録行(入力中の未確定行)への5=
      * DLTOPTをクリアしてSFLをUPDATE、空行追加なしでスキップ
     C                   IF        W#IDX > W#LINE_COUNT
      * 未確定行 - 全フィールドをクリアしてSFLをUPDATE
     C                   EVAL      DLTOPT = ' '
     C                   EVAL      ITEMCD = *BLANK
     C                   EVAL      ITEMNM = *BLANK
     C                   EVAL      ORDQTY = 0
     C                   EVAL      UNITPRC = 0
     C                   EXSR      @PUTPRC
     C                   UPDATE    DETAIL
     C                   EVAL      W#ITEMSEL = *ON
     C                   ITER
     C                   ENDIF
      * 配列を前に詰める
     C                   EVAL      W#ERROR = *OFF
     C                   DOW       W#IDX < W#LINE_COUNT
     C                   EVAL      W#LINENO = W#IDX + 1
     C                   EVAL      L_ITEMCD(W#IDX) = L_ITEMCD(W#LINENO)
     C                   EVAL      L_ITEMID(W#IDX) = L_ITEMID(W#LINENO)
     C                   EVAL      L_ITEMNM(W#IDX) = L_ITEMNM(W#LINENO)
     C                   EVAL      L_ORDQTY(W#IDX) = L_ORDQTY(W#LINENO)
     C                   EVAL      L_UNITPRC(W#IDX)= L_UNITPRC(W#LINENO)
     C                   EVAL      L_LINEAMT(W#IDX)= L_LINEAMT(W#LINENO)
     C                   EVAL      W#IDX = W#IDX + 1
     C                   ENDDO
     C                   EVAL      W#LINE_COUNT = W#LINE_COUNT - 1
      * 合計金額を再計算
     C                   EVAL      W#TOTAMT = 0
     C                   EVAL      W#IDX = 1
     C                   DOW       W#IDX <= W#LINE_COUNT
     C                   EVAL      W#AMT = L_LINEAMT(W#IDX)
     C                   EVAL      W#TOTAMT = W#TOTAMT + W#AMT
     C                   EVAL      W#IDX = W#IDX + 1
     C                   ENDDO
      * SFLをクリアして配列から再描画後、新しい空行入力欄を表示
     C                   EVAL      *IN93 = *ON
     C                   WRITE     DETAILCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      SFLRRN = 0
     C                   EXSR      @REDRAWSFL
     C                   ITER
     C                   ENDIF

     C                   IF        ITEMCD = *BLANK
     C                   ITER
     C                   ENDIF

     C                   EXSR      @VALIDDET

     C                   IF        W#ERROR
      * 修正#C: 新規入力行のエラーは入力内容を退避して再表示する
     C                   IF        SFLRRN > W#LINE_COUNT
     C                   EVAL      W#KEEPLN = *ON
     C                   EVAL      W#K_ITEMCD = ITEMCD
     C                   EVAL      W#K_QTY = ORDQTY
     C                   EVAL      W#K_PRC = UNITPRC
     C                   EVAL      W#K_PRCA = UPRC_A
     C                   EVAL      W#K_IN41 = *IN41
     C                   EVAL      W#K_IN43 = *IN43
     C                   ENDIF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN43 = *OFF
     C                   EVAL      *IN93 = *ON
     C                   WRITE     DETAILCTL
     C                   EVAL      *IN93 = *OFF
     C                   EXSR      @REDRAWSFL
     C                   ITER
     C                   ENDIF

      * 修正#B: SFLRRNが既確定行(<=W#LINE_COUNT)なら配列の上書き更新
      *         SFLRRNが未確定行(>W#LINE_COUNT) なら新規行として追加
     C                   IF        SFLRRN <= W#LINE_COUNT
      * 既確定行の上書き: 旧行金額を差し引いてから新金額を加算
     C                   EVAL      W#IDX = SFLRRN
     C                   EVAL      W#TOTAMT = W#TOTAMT - L_LINEAMT(W#IDX)
     C                   EVAL      L_ITEMCD(W#IDX) = ITEMCD
     C                   EVAL      L_ITEMID(W#IDX) = W#ITEMID
     C                   EVAL      L_ITEMNM(W#IDX) = ITEMNM
     C                   EVAL      L_ORDQTY(W#IDX) = ORDQTY
     C                   EVAL      L_UNITPRC(W#IDX) = UNITPRC
     C                   EVAL      L_LINEAMT(W#IDX) = ORDQTY * UNITPRC
     C                   EVAL      W#AMT = L_LINEAMT(W#IDX)
     C                   EVAL      W#TOTAMT = W#TOTAMT + W#AMT
     C                   EXSR      @PUTPRC
     C                   UPDATE    DETAIL
     C                   ELSE
      * 新規行として追加
     C                   EVAL      W#LINE_COUNT = W#LINE_COUNT + 1
     C                   EVAL      W#IDX = W#LINE_COUNT
     C                   EVAL      L_ITEMCD(W#IDX) = ITEMCD
     C                   EVAL      L_ITEMID(W#IDX) = W#ITEMID
     C                   EVAL      L_ITEMNM(W#IDX) = ITEMNM
     C                   EVAL      L_ORDQTY(W#IDX) = ORDQTY
     C                   EVAL      L_UNITPRC(W#IDX) = UNITPRC
     C                   EVAL      L_LINEAMT(W#IDX) = ORDQTY * UNITPRC
     C                   EVAL      W#AMT = L_LINEAMT(W#IDX)
     C                   EVAL      W#TOTAMT = W#TOTAMT + W#AMT
     C                   EXSR      @PUTPRC
     C                   UPDATE    DETAIL
     C                   ENDIF

     C                   ENDDO

     C                   IF        W#LINE_COUNT = 0
     C                             AND NOT *IN03 AND NOT *IN12
     C                             AND NOT *IN06
     C                   EVAL      ERRMSG = '商品を1件以上入力してください'
     C                   EVAL      W#ERROR = *ON
     C                   ENDIF

     C                   ENDSR

     C*===============================================================
     C* @REDRAWSFL - サブファイル再描画
     C*===============================================================
     C     @REDRAWSFL    BEGSR

     C                   EVAL      W#IDX = 1
     C                   DOW       W#IDX <= W#LINE_COUNT
      * 修正#E: 行ごとにRRNを設定(未設定だとRRN=0/重複でRNQ1255)
     C                   EVAL      SFLRRN = W#IDX
     C                   EVAL      LINENO = W#IDX
     C                   EVAL      DLTOPT = ' '
     C                   EVAL      ITEMCD = L_ITEMCD(W#IDX)
     C                   EVAL      ITEMNM = L_ITEMNM(W#IDX)
     C                   EVAL      ORDQTY = L_ORDQTY(W#IDX)
     C                   EVAL      UNITPRC = L_UNITPRC(W#IDX)
     C                   EXSR      @PUTPRC
     C                   WRITE     DETAIL
     C                   EVAL      W#IDX = W#IDX + 1
     C                   ENDDO
     C                   EVAL      SFLRRN = W#LINE_COUNT
     C                   EVAL      W#LASTRRN = W#LINE_COUNT

     C                   ENDSR

     C*===============================================================
     C* @VALIDDET - 明細バリデーション
     C*===============================================================
     C     @VALIDDET     BEGSR

     C                   EVAL      W#ERROR = *OFF
     C                   EVAL      ERRMSG = *BLANK

      * 修正#PRC: 単価の入力エラー
     C                   IF        W#PRCERR
     C                   EVAL      *IN43 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = W#PRCMSG
     C                   LEAVESR
     C                   ENDIF

     C     ITEMCD        CHAIN     ITMREC                             99
     C                   IF        *IN99
     C                   EVAL      *IN41 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '商品が見つかりません'
     C                   LEAVESR
     C                   ELSE
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      ITEMNM = DbcsTrunc(I_ITEMDSC:22)
     C                   EVAL      W#ITEMID = I_ITEMID
      * 修正#CRD: 販売停止の商品は受注できない
     C                   IF        I_STATUS <> 'ACTIVE'
     C                   EVAL      *IN41 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '販売停止中の商品です'
     C                   LEAVESR
     C                   ENDIF
     C                   IF        UNITPRC = 0
     C                   EVAL      UNITPRC = I_UNITPRC
     C                   ENDIF
     C                   ENDIF

      * 修正#J: 選択倉庫に在庫レコードがない商品は入力時点でエラー
     C     INVKEY        CHAIN(N)  INVENTORYP
     C                   IF        NOT %FOUND(INVENTORYP)
     C                   EVAL      *IN41 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = 'この倉庫に在庫がない商品です'
     C                   LEAVESR
     C                   ENDIF

     C                   IF        ORDQTY <= 0
     C                   EVAL      *IN42 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '数量が無効です'
     C                   LEAVESR
     C                   ELSE
     C                   EVAL      *IN42 = *OFF
     C                   ENDIF

     C                   IF        UNITPRC < 0
     C                   EVAL      *IN43 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '単価が無効です'
     C                   LEAVESR
     C                   ELSE
     C                   EVAL      *IN43 = *OFF
     C                   ENDIF

      * 修正#H: 明細金額の桁あふれ防止(20行合計でも15P2に収まる上限)
     C                   EVAL      W#CHKAMT = ORDQTY * UNITPRC
     C                   IF        W#CHKAMT > 99999999999
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '明細金額が上限を超えています'
     C                   LEAVESR
     C                   ENDIF

     C                   ENDSR

     C*===============================================================
     C* @CONFIRM - 確認画面
     C*===============================================================
     C     @CONFIRM      BEGSR

     C                   EVAL      CUSTCD_C = CUSTCD
     C                   EVAL      CUSTNM_C = CUSTNM
     C                   EVAL      ORDDATE_C = ORDDATE
     C                   EVAL      REQDATE_C = REQDATE
     C                   EVAL      CURRCD_C = CURRCD
     C                   EVAL      PAYTERM_C = PAYTERM
     C                   EVAL      WHID_C = WHID
     C                   EVAL      WHNM_C = WHNM
     C                   EVAL      TOTAMT = W#TOTAMT
     C                   EVAL      *IN46 = (CURRCD <> 'JPY')
     C                   EVALR     TOTAMT_F = %EDITC(%DEC(W#TOTAMT:13:2):'1')
     C                   EVAL      CUR_T = CURRCD

     C                   EVAL      *IN91 = *ON
     C                   EVAL      *IN92 = *ON

     C                   EXFMT     CONFIRM

     C                   ENDSR

     C*===============================================================
     C* @SAVEORD - 受注保存
     C*===============================================================
     C     @SAVEORD      BEGSR

      * 修正#K: 採番時刻を登録時点で取り直す(ヘッダ入力開始時刻では古い)
     C                   TIME                    WTIME6
     C                   EVAL      W#SOID = (WDATE8 * 1000000) + WTIME6
     C                   EVAL      W#ORDDATE = ORDDATE
     C                   EVAL      W#REQDATE = REQDATE
     C                   EVAL      W#CURRCD = CURRCD
     C                   EVAL      W#PAYTERM = PAYTERM
     C                   EVAL      W#WHID = WHID

     C                   CLEAR                   SOHREC
     C                   EVAL      SO_SOID = W#SOID
      * 修正#19: WDATE8はYYYYMMDD(8桁)になったので受注番号は正しい形式
     C                   EVAL      SO_SONUM = 'SO-' + %CHAR(WDATE8) + '-' +
     C                             %EDITC(WTIME6:'X')
     C                   EVAL      SO_CUSTID = W#CUSTID
     C                   EVAL      SO_ORDDATE = %INT(W#ORDDATE)
     C                   EVAL      SO_REQDATE = %INT(W#REQDATE)
     C                   EVAL      SO_CURRCD = W#CURRCD
     C                   EVAL      SO_PAYTERM = W#PAYTERM
     C                   EVAL      SO_WHID = W#WHID
      * 修正#D: 2パス方式 - Phase1=在庫チェックのみ(UPDATE無し)
      *         Phase2=全行チェック後にUPDATEを実行
      *         エラー後再登録時の二重引当を防止する
      * -------------------------------------------------------
      * 修正#CRD: 与信チェック(在庫を更新する前に行う)
     C                   EXSR      @CHKCRD
     C                   IF        W#ERROR
     C                   LEAVESR
     C                   ENDIF
      * Phase1: 全行の在庫チェック (UPDATE は行わない)
     C                   EVAL      W#ALCERRFLG = *OFF
     C                   EVAL      W#LINENO = 1
     C                   DOW       W#LINENO <= W#LINE_COUNT
     C                   EVAL      W#IDX = W#LINENO
     C                   EVAL      W#ITEMID = L_ITEMID(W#IDX)
      * 修正#D: チェックのみなのでロックしない(N)
     C     INVKEY        CHAIN(EN) INVENTORYP
     C                   IF        NOT %FOUND(INVENTORYP)
     C                   EVAL      W#ALCERRFLG = *ON
     C                   EVAL      W#ALCERRMSG = '在庫情報が見つかりません'
     C                   LEAVE
     C                   ENDIF
      * 修正#D: 同一商品が複数行ある場合は合計数量で判定
     C                   EVAL      W#SUMQTY = 0
     C                   EVAL      W#J = 1
     C                   DOW       W#J <= W#LINE_COUNT
     C                   IF        L_ITEMID(W#J) = W#ITEMID
     C                   EVAL      W#SUMQTY = W#SUMQTY + L_ORDQTY(W#J)
     C                   ENDIF
     C                   EVAL      W#J = W#J + 1
     C                   ENDDO
     C                   IF        QTYAVL < W#SUMQTY
     C                   EVAL      W#ALCERRFLG = *ON
     C                   EVAL      W#ALCERRMSG = %TRIM(L_ITEMCD(W#IDX)) +
     C                             ' の在庫が不足しています'
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#LINENO = W#LINENO + 1
     C                   ENDDO
      * Phase1でエラーが発生した場合はUPDATEせずに終了
     C                   IF        W#ALCERRFLG
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = W#ALCERRMSG
     C                   LEAVESR
     C                   ENDIF
      * Phase2: 全行チェック通過 - 実際に在庫引当UPDATEを実行
     C                   EVAL      W#LINENO = 1
     C                   DOW       W#LINENO <= W#LINE_COUNT
     C                   EVAL      W#IDX = W#LINENO
     C                   EVAL      W#ITEMID = L_ITEMID(W#IDX)
     C     INVKEY        CHAIN(E)  INVENTORYP
      * 修正#CMT: ロック取得後に再チェック(他の利用者の引当と競合した場合)
     C                   IF        %ERROR OR NOT %FOUND(INVENTORYP)
     C                   EVAL      ERRMSG = '在庫の更新に失敗しました' +
     C                             '(他で使用中)'
     C                   EXSR      @SAVEERR
     C                   LEAVESR
     C                   ENDIF
     C                   IF        QTYAVL < L_ORDQTY(W#IDX)
     C                   EVAL      ERRMSG = %TRIM(L_ITEMCD(W#IDX)) +
     C                             ' の在庫が不足しています'
     C                   EXSR      @SAVEERR
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      QTYALC = QTYALC + L_ORDQTY(W#IDX)
     C                   EVAL      QTYAVL = QTYOH - QTYALC
     C                   UPDATE(E) INVREC
     C                   IF        %ERROR
     C                   EVAL      ERRMSG = '在庫の更新に失敗しました'
     C                   EXSR      @SAVEERR
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#LINENO = W#LINENO + 1
     C                   ENDDO
      * 在庫引当成功 - 受注ヘッダを ALLOCATED で保存
     C                   EVAL      SO_STATUS = 'ALLOCATED'
     C                   EVAL      SO_TOTAMT = W#TOTAMT
     C                   EVAL      SO_CRTUSER = P#USER
     C                   EVAL      SO_UPDUSER = P#USER
     C                   EVAL      SO_CRTDATE = WDATE8
     C                   EVAL      SO_UPDDATE = WDATE8
     C                   EVAL      SO_CRTTIM = WTIME6
     C                   EVAL      SO_UPDTIM = WTIME6

      * 修正#CMT: 受注番号の重複(同じ秒)等は取消して再登録を促す
     C                   WRITE(E)  SOHREC
     C                   IF        %ERROR
     C                   EVAL      ERRMSG = '受注の登録に失敗しました。' +
     C                             '再度F6で登録してください'
     C                   EXSR      @SAVEERR
     C                   LEAVESR
     C                   ENDIF

      * 受注明細を ALLOCATED で保存
     C                   EVAL      W#LINENO = 1
     C                   DOW       W#LINENO <= W#LINE_COUNT
     C                   EVAL      W#IDX = W#LINENO
     C                   CLEAR                   SOLREC
     C                   EVAL      SL_SOID = W#SOID
     C                   EVAL      SL_LINENO = W#LINENO
     C                   EVAL      SL_ITEMID = L_ITEMID(W#IDX)
     C                   EVAL      SL_ORDQTY = L_ORDQTY(W#IDX)
     C                   EVAL      SL_UNITPRC = L_UNITPRC(W#IDX)
     C                   EVAL      SL_TAXRATE = W#TAXRATE
     C                   EVAL      SL_LINEAMT = L_LINEAMT(W#IDX)
     C                   EVAL      SL_ALCQTY = L_ORDQTY(W#IDX)
     C                   EVAL      SL_SHPQTY = 0
     C                   EVAL      SL_STATUS = 'ALLOCATED'

     C                   WRITE(E)  SOLREC
     C                   IF        %ERROR
     C                   EVAL      ERRMSG = '受注明細の登録に失敗しました'
     C                   EXSR      @SAVEERR
     C                   LEAVESR
     C                   ENDIF

     C                   EVAL      W#LINENO = W#LINENO + 1
     C                   ENDDO

      * 修正#CMT: 在庫引当・受注・明細をまとめて確定
     C                   COMMIT
     C                   ENDSR

     C*===============================================================
     C* 修正#CMT @SAVEERR - 登録途中のエラーは全変更を取消
     C*===============================================================
     C     @SAVEERR      BEGSR
     C                   ROLBK
     C                   EVAL      W#ERROR = *ON
     C                   ENDSR

     C*===============================================================
     C* 修正#CRD @CHKCRD - 与信チェック
     C*   与信使用額 = 未収残高 + 未出荷の受注額 + 出荷済未請求額
     C*   (未出荷・未請求は税込)。これに今回の受注額(税込)を足して
     C*   与信限度額を超える場合はエラー。限度額0は無制限とみなす
     C*===============================================================
     C     @CHKCRD       BEGSR
     C                   IF        W#CREDLMT <= 0
     C                   LEAVESR
     C                   ENDIF
     C/EXEC SQL
     C+   SELECT COALESCE(SUM(BALANCE), 0) INTO :W#AR
     C+     FROM INVOICEHP
     C+    WHERE CUSTID = :W#CUSTID AND BALANCE > 0
     C/END-EXEC
     C/EXEC SQL
     C+   SELECT COALESCE(SUM((L.ORDQTY - L.SHPQTY) * L.UNITPRC
     C+          * (1 + L.TAXRATE)), 0) INTO :W#UNSHIP
     C+     FROM SORDERLP L JOIN SORDERHP H ON H.SOID = L.SOID
     C+    WHERE H.CUSTID = :W#CUSTID
     C+      AND H.STATUS IN ('OPEN', 'ALLOCATED', 'PARTIAL')
     C+      AND L.ORDQTY > L.SHPQTY
     C/END-EXEC
     C/EXEC SQL
     C+   SELECT COALESCE(SUM(D.SHPQTY * L.UNITPRC
     C+          * (1 + L.TAXRATE)), 0) INTO :W#UNBILL
     C+     FROM SHIPMENTHP S
     C+     JOIN SHIPMENTLP D ON D.SHPID = S.SHPID
     C+     JOIN SORDERLP L ON L.SOID = S.SOID AND L.LINENO = D.SOLNO
     C+     JOIN SORDERHP H ON H.SOID = S.SOID
     C+    WHERE H.CUSTID = :W#CUSTID AND S.STATUS = 'SHIPPED'
     C/END-EXEC
     C                   EVAL(H)   W#NEWAMT = W#TOTAMT * (1 + W#TAXRATE)
     C                   EVAL      W#CRDREM = W#CREDLMT - W#AR - W#UNSHIP
     C                             - W#UNBILL
     C                   IF        W#NEWAMT > W#CRDREM
     C                   EVAL      W#ERROR = *ON
     C                   IF        W#CRDREM < 0
     C                   EVAL      W#CRDREM = 0
     C                   ENDIF
     C                   EVAL      ERRMSG = '与信限度額超過(残り与信 ' +
     C                             %CHAR(%DEC(W#CRDREM:15:0)) + '円)'
     C                   ENDIF
     C                   ENDSR

     C*===============================================================
     C* @SHOWCOMP - 完了画面
     C*===============================================================
     C     @SHOWCOMP     BEGSR

     C                   EVAL      *IN91 = *ON
     C                   EVAL      *IN92 = *ON
     C                   EVAL      SONUM = SO_SONUM

     C                   EXFMT     COMPLETE

     C                   ENDSR
     C*==============================================================
     C* 修正#F4 @SRCCUST - 顧客検索ウィンドウ
     C* CUSTOMERL1を全スキャンして全顧客を表示
     C* 選択時: CUSTCD/CUSTNM/W#CUSTID を画面バッファにセット
     C*==============================================================
     C     @SRCCUST      BEGSR
      * Step1: SFLCLR
     C                   EVAL      *IN71 = *OFF
     C                   EVAL      *IN72 = *OFF
     C                   EVAL      *IN73 = *ON
     C                   WRITE     CUSTSFLCTL
     C                   EVAL      *IN73 = *OFF
      * Step2: SFLロード
     C                   EVAL      W#SFLEOF = *OFF
     C                   EVAL      CSFRRN = 0
     C     *LOVAL        SETLL     CSTREC
     C                   DOW       NOT W#SFLEOF AND CSFRRN < 20
     C                   READ(E)   CSTREC
     C                   EVAL      W#SFLEOF = %EOF(CUSTOMERL1)
     C                   IF        W#SFLEOF
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      CSFRRN = CSFRRN + 1
     C                   EVAL      CSFOPT = ' '
     C                   EVAL      CSFCD = C_CUSTCD
     C                   EVAL      CSFNM = DbcsTrunc(C_CUSTNM:24)
     C                   WRITE     CUSTSFL
     C                   ENDDO
      * Step3: 0件時はメッセージ表示して終了
     C                   EVAL      W#SFLRRN = CSFRRN
     C                   IF        W#SFLRRN = *ZERO
     C                   EVAL      ERRMSG = '顧客が登録されていません'
     C                   LEAVESR
     C                   ENDIF
      * Step3: EXFMT
     C                   EVAL      *IN71 = *ON
     C                   EVAL      *IN72 = *ON
     C                   EXFMT     CUSTSFLCTL
      * F12=戻る
     C                   IF        *IN12
     C                   EVAL      *IN12 = *OFF
     C                   LEAVESR
     C                   ENDIF
      * Step4: 1=選択をスキャン
     C                   EVAL      CSFRRN = 0
     C                   DOW       CSFRRN < W#SFLRRN
     C                   EVAL      CSFRRN = CSFRRN + 1
     C     CSFRRN        CHAIN(E)  CUSTSFL
     C                   IF        NOT %FOUND(ORDENTRYD)
     C                   LEAVE
     C                   ENDIF
     C                   IF        CSFOPT = '1'
     C                   EVAL      CUSTCD = CSFCD
     C                   EVAL      CUSTNM = %SUBST(CSFNM:1:24)
     C     CUSTCD        CHAIN     CSTREC                             99
     C                   IF        NOT *IN99
     C                   EVAL      W#CUSTID = C_CUSTID
     C                   EVAL      CUSTNM = DbcsTrunc(C_CUSTNM:40)
     C                   ENDIF
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR

     C*==============================================================
     C* 修正#F4 @SRCWH - 倉庫検索ウィンドウ
     C* WAREHOUSEPを全スキャンして全倉庫を表示
     C* 選択時: WHID/WHNM/W#WHID をセット
     C*==============================================================
     C     @SRCWH        BEGSR
      * Step1: SFLCLR
     C                   EVAL      *IN74 = *OFF
     C                   EVAL      *IN75 = *OFF
     C                   EVAL      *IN76 = *ON
     C                   WRITE     WHSFLCTL
     C                   EVAL      *IN76 = *OFF
      * Step2: ロード
     C                   EVAL      W#SFLEOF = *OFF
     C                   EVAL      WSFRRN = 0
     C     *LOVAL        SETLL     WAREHOUSEP
     C                   DOW       NOT W#SFLEOF AND WSFRRN < 20
     C                   READ      WAREHOUSEP                             99
     C                   EVAL      W#SFLEOF = *IN99
     C                   IF        W#SFLEOF
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      WSFRRN = WSFRRN + 1
     C                   EVAL      WSFOPT = ' '
     C                   EVAL      WSFID = %CHAR(W_WHID)
     C                   EVAL      WSFCD = W_WHCD
     C                   EVAL      WSFNM = DbcsTrunc(W_WHNM:35)
     C                   WRITE     WHSFL
     C                   ENDDO
      * Step3: 表示または戻る
     C                   EVAL      W#SFLRRN = WSFRRN
     C                   IF        W#SFLRRN = *ZERO
     C                   EVAL      ERRMSG = '倉庫が登録されていません'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      *IN74 = *ON
     C                   EVAL      *IN75 = *ON
     C                   EXFMT     WHSFLCTL
     C                   IF        *IN12
     C                   EVAL      *IN12 = *OFF
     C                   LEAVESR
     C                   ENDIF
      * Step4: オプション1をスキャン
     C                   EVAL      WSFRRN = 0
     C                   DOW       WSFRRN < W#SFLRRN
     C                   EVAL      WSFRRN = WSFRRN + 1
     C     WSFRRN        CHAIN(E)  WHSFL
     C                   IF        NOT %FOUND(ORDENTRYD)
     C                   LEAVE
     C                   ENDIF
     C                   IF        WSFOPT = '1'
     C                   EVAL      WHID = %INT(WSFID)
     C                   EVAL      W#WHID = %INT(WSFID)
     C                   EVAL      WHNM = %SUBST(WSFNM:1:35)
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR

     C*==============================================================
     C* 修正#F4 @SRCITEM - 商品検索ウィンドウ (W#WHIDで在庫絞込)
     C* W#WHIDの在庫レコードが存在する商品のみ表示
     C* 選択時: ITEMCD/ITEMNM/W#ITEMID/UNITPRC をセット
     C*==============================================================
     C     @SRCITEM      BEGSR
      * Step1: SFLCLR
     C                   EVAL      *IN77 = *OFF
     C                   EVAL      *IN78 = *OFF
     C                   EVAL      *IN79 = *ON
     C                   WRITE     ITMSFLCTL
     C                   EVAL      *IN79 = *OFF
      * Step2: SFLロード
     C                   EVAL      W#SFLEOF = *OFF
     C                   EVAL      ISFRRN = 0
     C     *LOVAL        SETLL     ITMREC
     C                   DOW       NOT W#SFLEOF AND ISFRRN < 20
     C                   READ      ITMREC                                 99
     C                   EVAL      W#SFLEOF = *IN99
     C                   IF        W#SFLEOF
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#ITEMID = I_ITEMID
      * 修正#L: 一覧表示のみなのでロックしない(N)
     C     INVKEY        CHAIN(N)  INVENTORYP                         99
     C                   IF        *IN99
     C                   ITER
     C                   ENDIF
     C                   EVAL      ISFRRN = ISFRRN + 1
     C                   EVAL      ISFOPT = ' '
     C                   EVAL      ISFCD = I_ITEMCD
     C                   EVAL      ISFNM = DbcsTrunc(I_ITEMDSC:20)
      * 修正#L: 受注で引当できる数量(引当可能数)を表示
     C                   EVAL      ISFQTY = QTYAVL
     C                   WRITE     ITMSFL
     C                   ENDDO
      * Step3: 0件時はメッセージ表示して終了
     C                   EVAL      W#SFLRRN = ISFRRN
     C                   IF        W#SFLRRN = *ZERO
     C                   EVAL      ERRMSG = 'この倉庫に在庫商品がありません'
     C                   LEAVESR
     C                   ENDIF
      * Step3: EXFMT
     C                   EVAL      *IN77 = *ON
     C                   EVAL      *IN78 = *ON
     C                   EXFMT     ITMSFLCTL
      * F12=戻る
     C                   IF        *IN12
     C                   EVAL      *IN12 = *OFF
     C                   LEAVESR
     C                   ENDIF
      * Step4: 1=選択をスキャン
     C                   EVAL      ISFRRN = 0
     C                   DOW       ISFRRN < W#SFLRRN
     C                   EVAL      ISFRRN = ISFRRN + 1
     C     ISFRRN        CHAIN(E)  ITMSFL
     C                   IF        NOT %FOUND(ORDENTRYD)
     C                   LEAVE
     C                   ENDIF
     C                   IF        ISFOPT = '1'
     C                   EVAL      ITEMCD = ISFCD
     C                   EVAL      ITEMNM = ISFNM
     C     ITEMCD        CHAIN     ITMREC                             99
     C                   IF        NOT *IN99
     C                   EVAL      W#ITEMID = I_ITEMID
     C                   EVAL      ITEMNM = DbcsTrunc(I_ITEMDSC:22)
     C                   IF        UNITPRC = 0
     C                   EVAL      UNITPRC = I_UNITPRC
     C                   ENDIF
     C                   ENDIF
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR

      *===============================================================
      * 修正#B DbcsTrunc - 全角(SO/SI)を壊さずにpMaxバイトへ切り詰め
      *   %SUBSTのバイト切りでは全角文字の途中で切れて文字化けする
      *===============================================================
     C*===============================================================
     C* 修正#PRC @PUTPRC - 単価を画面の文字にする
     C*   円は整数・外貨は小数2桁。0は空白(空白なら商品の標準単価)
     C*===============================================================
     C     @PUTPRC       BEGSR
     C                   IF        W#RAWPRC
     C                   EVAL      UPRC_A = W#K_PRCA
     C                   EVAL      W#RAWPRC = *OFF
     C                   LEAVESR
     C                   ENDIF
     C                   SELECT
     C                   WHEN      UNITPRC = 0
     C                   EVAL      UPRC_A = *BLANKS
     C                   WHEN      CURRCD = 'JPY' OR CURRCD = *BLANKS
     C                   EVALR     UPRC_A = %EDITC(%DECH(UNITPRC:10:0):'J')
     C                   OTHER
     C                   EVALR     UPRC_A = %EDITC(%DECH(UNITPRC:10:2):'J')
     C                   ENDSL
     C                   ENDSR

     C*===============================================================
     C* 修正#PRC @GETPRC - 画面の単価(文字)を数値にする
     C*   カンマは無視。円は整数、外貨は小数2桁まで
     C*===============================================================
     C     @GETPRC       BEGSR
     C                   EVAL      W#PRCERR = *OFF
     C                   EVAL      W#PRCS = %TRIM(%SCANRPL(',':'':UPRC_A))
     C                   IF        %LEN(W#PRCS) = 0
     C                   EVAL      UNITPRC = 0
     C                   LEAVESR
     C                   ENDIF
     C                   MONITOR
     C                   EVAL      UNITPRC = %DEC(W#PRCS:15:4)
     C                   ON-ERROR
     C                   EVAL      W#PRCERR = *ON
     C                   EVAL      W#PRCMSG = '単価は数字で入力してください'
     C                   EVAL      UNITPRC = 0
     C                   LEAVESR
     C                   ENDMON
     C                   SELECT
     C                   WHEN      UNITPRC > 99999999.99
     C                   EVAL      W#PRCERR = *ON
     C                   EVAL      W#PRCMSG = '単価が大きすぎます'
     C                   WHEN      (CURRCD = 'JPY' OR CURRCD = *BLANKS) AND
     C                             UNITPRC <> %INT(UNITPRC)
     C                   EVAL      W#PRCERR = *ON
     C                   EVAL      W#PRCMSG = '円の単価は整数です'
     C                   WHEN      UNITPRC * 100 <> %INT(UNITPRC * 100)
     C                   EVAL      W#PRCERR = *ON
     C                   EVAL      W#PRCMSG = '単価は小数2桁までです'
     C                   ENDSL
     C                   ENDSR

     P DbcsTrunc       B
     D DbcsTrunc       PI           256A   VARYING
     D   pStr                       256A   VARYING CONST
     D   pMax                        10I 0 CONST
     D wOut            S            256A   VARYING
     D wCh             S              1A
     D wI              S             10I 0
     D wLen            S             10I 0
     D wDbcs           S               N
     C                   EVAL      wOut = ''
     C                   EVAL      wDbcs = *OFF
     C                   EVAL      wLen = %LEN(%TRIMR(pStr))
     C                   EVAL      wI = 1
     C                   DOW       wI <= wLen
     C                   EVAL      wCh = %SUBST(pStr:wI:1)
     C                   SELECT
      * SO: SO+全角1文字+SIが入る余地がなければ終了
     C                   WHEN      wCh = W#SO
     C                   IF        %LEN(wOut) + 3 > pMax
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      wOut = wOut + wCh
     C                   EVAL      wDbcs = *ON
     C                   EVAL      wI = wI + 1
     C                   WHEN      wCh = W#SI
     C                   EVAL      wOut = wOut + wCh
     C                   EVAL      wDbcs = *OFF
     C                   EVAL      wI = wI + 1
      * 全角: 2バイト単位。閉じるSIの1バイトを常に確保
     C                   WHEN      wDbcs
     C                   IF        %LEN(wOut) + 3 > pMax OR wI + 1 > wLen
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      wOut = wOut + %SUBST(pStr:wI:2)
     C                   EVAL      wI = wI + 2
     C                   OTHER
     C                   IF        %LEN(wOut) + 1 > pMax
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      wOut = wOut + wCh
     C                   EVAL      wI = wI + 1
     C                   ENDSL
     C                   ENDDO
     C                   IF        wDbcs
     C                   EVAL      wOut = wOut + W#SI
     C                   ENDIF
     C                   RETURN    wOut
     P DbcsTrunc       E
