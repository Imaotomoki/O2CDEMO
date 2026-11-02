     H DFTACTGRP(*NO) ACTGRP(*NEW)
      *===============================================================
      * プログラム名: PAYENTRY
      * 機能: 入金処理画面プログラム
      * 作成日: 2025-06-09
      * 説明: 5250画面を使用した対話型入金処理
      *        1. ヘッダー入力 (顧客コード/入金日/入金方法/通貨/参照
      *        2. 対象請求書サブファイル表示・確認
      *        3. 入金登録・請求書への消込更新
      * 変更履歴:
      *   修正#P1 PAYENTRYD 顧客検索WINDOWが画面外(CPF500B)で異常終了
      *   修正#P2 @SRCCUST F12でRETURN(プログラム終了)→LEAVESR
      *   修正#P3 @SRCCUST 前方一致検索(READEは完全一致だった)
      *   修正#P4 入金額の数値変換エラー(RNX0105)をMONITOR
      *   修正#P5 入金日の妥当性チェック/入力した入金日で登録
      *   修正#P6 為替レート未登録時は1で換算せずエラー(直近日付を使用)
      *   修正#P7 未収残高合計を超える入金/対象請求書0件はエラー
      *   修正#P8 コミットメント制御(COMMIT/ROLLBACK)+SQLCODEチェック
      *   修正#PG 確認F12の2回目で登録される不具合(状態で画面制御)
      *   修正#PN 入金番号を入金IDから採番(秒単位の重複防止)
      *   修正#SEQ シーケンス名の修飾を外す('/'が除算と解釈され採番失敗)
      *   修正#SEL 請求一覧で1=選択した請求書だけに消込(未選択は古い順)
      *   修正#CUR 消込は請求書の通貨で行う。入金と同じ通貨の請求書だけを
      *            対象にし、入金額(換算前)で消し込む(円換算額は記録用)
      *===============================================================

      *---------------------------------------------------------------
      * ファイル定義
      *---------------------------------------------------------------
     FPAYENTRYD CF   E             WORKSTN SFILE(INVSFL:SFLRRN)
     F                                     SFILE(CUSTSFL:CSFRRN)
     FCUSTOMERL1IF   E           K DISK    PREFIX(C_)
     F                                     RENAME(CUSTREC:CSTREC)

      *---------------------------------------------------------------
      * グローバル変数定義
      *---------------------------------------------------------------
     D WDATE8          S              8P 0
     D WTIME6          S              6P 0
     D W#USER          S             10A

     D W#PAYID         S             18P 0
     D W#ERROR         S               N
     D SFLRRN          S              4S 0
     D W#SFLCNT        S             10I 0

      * 入力作業変数
     D W#PAYDT         S               D
     D W#CUSTCD        S             20A
     D W#CUSTID        S              9P 0
     D W#PAYDATE       S              8A
     D W#PAYMETH       S             20A
      * 修正#F4P: 共通の検索ウィンドウ(F4)と入力チェック用
     D O2CPRMPT        PR                  EXTPGM('O2CPRMPT')
     D   pType                       10A   CONST
     D   pCdType                     30A   CONST
     D   pValue                      30A
     D W#PVAL          S             30A
      * 修正#CUR: 一覧の金額を編集(円は小数なし、外貨は小数2桁)
     D FmtAmt          PR            15A   VARYING
     D   pAmt                        15P 2 CONST
     D W#CDCNT         S             10I 0
     D W#CURRCD        S              3A
     D W#REFNO         S             30A
     D W#AMOUNT        S             15P 2
     D W#AMTSTR        S             15A

      * 為替換算作業変数
     D W#FXRATE        S             10P 6
     D W#BASEAMT       S             15P 2
     D W#REMAMT        S             15P 2

      * 入金番号生成用
     D W#PAYNUM        S             20A
     D W#DATESTR       S              8A
     D W#TIMESTR       S              6A

      * 基準通貨
     D W#BASECUR       S              3A   INZ('JPY')

      * SQL用ホスト変数 (請求書情報)
     D H#INVID         S             18P 0
     D H#INVNUM        S             20A
     D H#INVDATE       S              8P 0
     D H#TOTAMT        S             15P 2
     D H#PAIDAMT       S             15P 2
     D H#BALANCE       S             15P 2
     D H#STATUS        S             12A
     D H#CUSTID        S              9P 0
     D H#APPLYID       S             18P 0
     D H#APPLYAMT      S             15P 2
      * Mod#F4: 検索ウィンドウ作業変数
     D W#SFLEOF        S               N
     D W#SFRRN         S              4S 0
      * 修正#PG: 画面状態 I=初期 H=ヘッダ L=一覧 C=確認 X=終了
     D W#STEP          S              1A
     D W#ERRTXT        S             50A
      * 修正#P5: 入金日(数値)
     D W#PAYD8         S              8P 0
      * 修正#P7: 未収残高合計
     D W#OPENBAL       S             15P 2
      * 修正#P3: 前方一致検索
     D W#SRCLEN        S             10I 0
     D W#SELECTED      S               N
      * 修正#SEL: 消込先として選択した請求書
     D W#SELID         S             18P 0 DIM(20)
     D W#SELCNT        S             10I 0
     D W#SELBAL        S             15P 2
     D W#SI2           S             10I 0
      * 修正#B: 全角(DBCS)を壊さずに切り詰める
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
     D DbcsTrunc       PR           256A   VARYING
     D   pStr                       256A   VARYING CONST
     D   pMax                        10I 0 CONST

      * 修正#P8: SQLの更新はコミットメント制御下で行う
     C/EXEC SQL
     C+   SET OPTION COMMIT = *CHG, CLOSQLCSR = *ENDMOD
     C/END-EXEC

      *===============================================================
      * メイン処理
      *===============================================================
      * 修正#PG: 状態で画面を制御
      *   請求一覧/確認からのF12はひとつ前の画面へ(入力内容は保持)
      *   登録エラー時は請求一覧へ戻りメッセージを表示
     C                   EVAL      W#STEP = 'I'
     C                   DOW       W#STEP <> 'X'
     C                   SELECT
     C                   WHEN      W#STEP = 'I'
     C                   EXSR      @INIT
     C                   EVAL      W#STEP = 'H'
     C                   WHEN      W#STEP = 'H'
     C                   EXSR      @INPUT_HEADER
     C                   SELECT
     C                   WHEN      *IN03
     C                   EVAL      W#STEP = 'X'
     C                   WHEN      *IN12
     C                   EVAL      *IN12 = *OFF
     C                   EVAL      W#STEP = 'I'
     C                   OTHER
     C                   EXSR      @LOAD_INV
     C                   EVAL      W#STEP = 'L'
     C                   ENDSL
     C                   WHEN      W#STEP = 'L'
     C                   EXSR      @SHOW_INV
     C                   SELECT
     C                   WHEN      *IN03
     C                   EVAL      W#STEP = 'X'
     C                   WHEN      *IN12
     C                   EVAL      *IN12 = *OFF
     C                   EVAL      W#STEP = 'H'
     C                   OTHER
     C                   EVAL      W#STEP = 'C'
     C                   ENDSL
     C                   WHEN      W#STEP = 'C'
     C                   EXSR      @CONFIRM
     C                   SELECT
     C                   WHEN      *IN03
     C                   EVAL      W#STEP = 'X'
     C                   WHEN      *IN12
     C                   EVAL      *IN12 = *OFF
     C                   EVAL      W#STEP = 'L'
     C                   OTHER
     C                   EXSR      @SAVEPAY
     C                   IF        W#ERROR
     C                   EVAL      W#ERRTXT = ERRMSG
     C                   EXSR      @LOAD_INV
     C                   EVAL      W#STEP = 'L'
     C                   ELSE
     C                   EXSR      @SHOWCOMP
     C                   IF        *IN03
     C                   EVAL      W#STEP = 'X'
     C                   ELSE
     C                   EVAL      W#STEP = 'I'
     C                   ENDIF
     C                   ENDIF
     C                   ENDSL
     C                   ENDSL
     C                   ENDDO

     C                   EVAL      *INLR = *ON
     C                   RETURN

      *===============================================================
      * @INIT - 初期化
      *===============================================================
     C     @INIT         BEGSR

     C                   TIME                    WTIME6
      * 修正#SYSDT: ジョブ日付はサインオン日のままなのでシステム日付
     C                   EVAL      W#PAYDT = %DATE()
     C                   EVAL      WDATE8 = %INT(%CHAR(W#PAYDT:*ISO0))

     C                   EVAL      W#ERROR = *OFF
     C                   EVAL      W#SFLCNT = 0
     C                   EVAL      ERRMSG = *BLANK
     C                   EVAL      ERRMSG_D = *BLANK

     C                   EVAL      *IN03 = *OFF
     C                   EVAL      *IN06 = *OFF
     C                   EVAL      *IN12 = *OFF
     C                   EVAL      *IN31 = *OFF
     C                   EVAL      *IN32 = *OFF
     C                   EVAL      *IN33 = *OFF
     C                   EVAL      *IN34 = *OFF
     C                   EVAL      *IN35 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN91 = *OFF
     C                   EVAL      *IN92 = *OFF
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN94 = *OFF

     C                   EVAL      W#PAYDATE = %CHAR(W#PAYDT:*ISO0)
     C                   EVAL      W#CURRCD = 'JPY'
     C                   EVAL      W#PAYMETH = *BLANK
     C                   EVAL      W#REFNO = *BLANK
     C                   EVAL      W#AMOUNT = 0

     C                   EVAL      CUSTCD = *BLANK
     C                   EVAL      CUSTNM = *BLANK
     C                   EVAL      PAYDATE = W#PAYDATE
     C                   EVAL      PAYMETH = *BLANK
     C                   EVAL      CURRCD = W#CURRCD
     C                   EVAL      REFNO = *BLANK
     C                   EVAL      AMOUNT = *BLANK

     C                   ENDSR

      *===============================================================
      * @INPUT_HEADER - ヘッダー入力
      *===============================================================
     C     @INPUT_HEADER BEGSR

     C                   EVAL      *IN31 = *OFF
     C                   EVAL      *IN32 = *OFF
     C                   EVAL      *IN33 = *OFF
     C                   EVAL      *IN34 = *OFF
     C                   EVAL      *IN35 = *OFF
     C                   EVAL      ERRMSG = *BLANK

     C                   EXFMT     HEADER

     C                   DOW       NOT *IN03 AND NOT *IN12

      * Mod#F4: F4キー - 顧客検索
      * 修正#F4P: カーソルのある項目で検索先を決める(入金方法・通貨を追加)
     C                   IF        *IN04
     C                   EVAL      *IN04 = *OFF
     C                   SELECT
     C                   WHEN      CSRFLD = 'PAYMETH'
     C                   EVAL      W#PVAL = PAYMETH
     C                   CALLP     O2CPRMPT('CODE':'PAY_METHOD':W#PVAL)
     C                   EVAL      PAYMETH = W#PVAL
     C                   WHEN      CSRFLD = 'CURRCD'
     C                   EVAL      W#PVAL = CURRCD
     C                   CALLP     O2CPRMPT('CODE':'CURRENCY':W#PVAL)
     C                   EVAL      CURRCD = W#PVAL
     C                   OTHER
     C                   EXSR      @SRCCUST
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

      *===============================================================
      * @VALIDHDR - ヘッダーバリデーション
      *===============================================================
     C     @VALIDHDR     BEGSR

     C                   EVAL      W#ERROR = *OFF
     C                   EVAL      ERRMSG = *BLANK

      * 顧客コード検証
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
     C                   EVAL      W#CUSTCD = CUSTCD
     C                   ENDIF

      * 入金日検証
     C                   IF        PAYDATE = *BLANK
     C                   EVAL      *IN32 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '入金日が無効です'
     C                   LEAVESR
     C                   ENDIF
      * 修正#P5: 日付の妥当性チェック(YYYYMMDD)・未来日は不可
     C     *ISO0         TEST(DE)                PAYDATE
     C                   IF        %ERROR
     C                   EVAL      *IN32 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '入金日が無効です(YYYYMMDD)'
     C                   LEAVESR
     C                   ENDIF
     C                   IF        %DATE(PAYDATE:*ISO0) > %DATE()
     C                   EVAL      *IN32 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '入金日に未来日は指定できません'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      *IN32 = *OFF
     C                   EVAL      W#PAYDATE = PAYDATE
     C                   EVAL      W#PAYD8 = %DEC(PAYDATE:8:0)

      * 入金方法検証
     C                   IF        PAYMETH = *BLANK
     C                   EVAL      *IN33 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '入金方法を入力してください'
     C                   LEAVESR
     C                   ELSE
     C                   EVAL      *IN33 = *OFF
     C                   EVAL      W#PAYMETH = PAYMETH
     C                   ENDIF
      * 修正#F4P: 入金方法はコードマスター(PAY_METHOD)にあるものだけ
     C/EXEC SQL
     C+   SELECT COUNT(*) INTO :W#CDCNT FROM CODEMSTRP
     C+    WHERE CODETYPE = 'PAY_METHOD' AND CODEVAL = :PAYMETH
     C+      AND ISACTIVE = 'Y'
     C/END-EXEC
     C                   IF        SQLCODE <> 0 OR W#CDCNT = 0
     C                   EVAL      *IN33 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '入金方法が登録されていません' +
     C                             '(F4で選択)'
     C                   LEAVESR
     C                   ENDIF

      * 通貨コード検証
     C                   IF        CURRCD = *BLANK
     C                   EVAL      *IN34 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '通貨コードを入力してください'
     C                   LEAVESR
     C                   ELSE
     C                   EVAL      *IN34 = *OFF
     C                   EVAL      W#CURRCD = CURRCD
     C                   ENDIF
      * 修正#F4P: 通貨はコードマスター(CURRENCY)にあるものだけ
     C/EXEC SQL
     C+   SELECT COUNT(*) INTO :W#CDCNT FROM CODEMSTRP
     C+    WHERE CODETYPE = 'CURRENCY' AND CODEVAL = :CURRCD
     C+      AND ISACTIVE = 'Y'
     C/END-EXEC
     C                   IF        SQLCODE <> 0 OR W#CDCNT = 0
     C                   EVAL      *IN34 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '通貨が登録されていません' +
     C                             '(F4で選択)'
     C                   LEAVESR
     C                   ENDIF

      * 入金額検証 (文字列->数値変換)
     C                   EVAL      W#AMTSTR = %TRIM(AMOUNT)
     C                   IF        W#AMTSTR = *BLANK
     C                   EVAL      *IN35 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '入金額を入力してください'
     C                   LEAVESR
     C                   ELSE
     C                   EVAL      *IN35 = *OFF
      * 修正#P4: 数値以外はエラー(カンマ区切りは許可)
     C                   EVAL      W#AMTSTR = %SCANRPL(',':'':W#AMTSTR)
     C                   MONITOR
     C                   EVAL      W#AMOUNT = %DEC(W#AMTSTR:15:2)
     C                   ON-ERROR
     C                   EVAL      *IN35 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '入金額は数値で入力してください'
     C                   LEAVESR
     C                   ENDMON
     C                   IF        W#AMOUNT <= 0
     C                   EVAL      *IN35 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '入金額は正の値を入力してください'
     C                   LEAVESR
     C                   ENDIF
     C                   ENDIF

     C                   EVAL      W#REFNO = REFNO

      * 修正#P6: 為替換算(入金日以前の直近レート)。レートなしはエラー
     C                   EVAL      W#FXRATE = 1
     C                   EVAL      W#BASEAMT = W#AMOUNT
     C                   IF        W#CURRCD <> W#BASECUR
     C                   EVAL      W#FXRATE = 0
     C/EXEC SQL
     C+   SELECT RATE INTO :W#FXRATE FROM FXRATEP
     C+    WHERE FROMCUR = :W#CURRCD AND TOCUR = :W#BASECUR
     C+      AND RATEDATE <= :W#PAYD8
     C+    ORDER BY RATEDATE DESC FETCH FIRST 1 ROW ONLY
     C/END-EXEC
     C                   IF        SQLCODE <> 0 OR W#FXRATE <= 0
     C                   EVAL      *IN34 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = %TRIM(W#CURRCD) +
     C                             'の為替レートが登録されていません'
     C                   LEAVESR
     C                   ENDIF
     C                   MONITOR
     C                   EVAL(H)   W#BASEAMT = W#AMOUNT * W#FXRATE
     C                   ON-ERROR
     C                   EVAL      *IN35 = *ON
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '換算後の金額が大きすぎます'
     C                   LEAVESR
     C                   ENDMON
     C                   ENDIF

     C                   ENDSR

      *===============================================================
      * @LOAD_INV - 対象請求書をSFLにロード (SQL使用)
      *===============================================================
     C     @LOAD_INV     BEGSR

     C                   EVAL      *IN93 = *ON
     C                   WRITE     INVSFLCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      SFLRRN = 0
     C                   EVAL      W#SFLCNT = 0
     C                   EVAL      W#SELCNT = 0
     C                   EVAL      *IN41 = *OFF

      * SQLカーソルで顧客の未収請求書を取得
     C/EXEC SQL
     C+ DECLARE INVCSR CURSOR FOR
     C+   SELECT INVID, INVNUM, INVDATE, TOTAMT, PAIDAMT, BALANCE
     C+   FROM INVOICEHP
     C+   WHERE CUSTID = :W#CUSTID AND BALANCE > 0
     C+     AND CURRCD = :W#CURRCD
     C+   ORDER BY INVDATE
     C/END-EXEC

     C/EXEC SQL
     C+   OPEN INVCSR
     C/END-EXEC

     C                   DOW       SQLCODE = 0 AND W#SFLCNT < 20

     C/EXEC SQL
     C+   FETCH INVCSR
     C+   INTO :H#INVID, :H#INVNUM, :H#INVDATE,
     C+        :H#TOTAMT, :H#PAIDAMT, :H#BALANCE
     C/END-EXEC

     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF

     C                   EVAL      SFLINVNO = H#INVNUM
     C                   EVAL      SFLINVDT = H#INVDATE
     C                   EVALR     SFLTOTAM = FmtAmt(H#TOTAMT)
     C                   EVALR     SFLPAIDM = FmtAmt(H#PAIDAMT)
     C                   EVALR     SFLBALM = FmtAmt(H#BALANCE)
     C                   EVAL      SFLINVID = H#INVID
     C                   EVAL      SFLBALH = H#BALANCE
     C                   EVAL      SFLOPT = ' '

     C                   IF        H#BALANCE >= W#AMOUNT
     C                   EVAL      *IN41 = *ON
     C                   ELSE
     C                   EVAL      *IN41 = *OFF
     C                   ENDIF

     C                   EVAL      SFLRRN = SFLRRN + 1
     C                   WRITE     INVSFL
     C                   EVAL      W#SFLCNT = W#SFLCNT + 1

     C                   ENDDO

     C/EXEC SQL
     C+   CLOSE INVCSR
     C/END-EXEC

      * 修正#P7: 一覧は20件まで。消込は全件対象なので合計を取得
     C                   EVAL      W#OPENBAL = 0
     C/EXEC SQL
     C+   SELECT COALESCE(SUM(BALANCE), 0) INTO :W#OPENBAL
     C+     FROM INVOICEHP
     C+    WHERE CUSTID = :W#CUSTID AND BALANCE > 0
     C+      AND CURRCD = :W#CURRCD
     C/END-EXEC

     C                   ENDSR

      *===============================================================
      * @SHOW_INV - 請求書サブファイル画面表示
      *===============================================================
     C     @SHOW_INV     BEGSR

     C                   EVAL      CUSTCD_D = W#CUSTCD
     C                   EVAL      CUSTNM_D = DbcsTrunc(CUSTNM:30)
     C                   EVAL      PAYDATE_D = W#PAYDATE
     C                   EVAL      PAYMETH_D = W#PAYMETH
     C                   EVAL      CURRCD_D = W#CURRCD
     C                   EVAL      REFNO_D = W#REFNO
     C                   EVAL      AMOUNT_D = W#AMOUNT
      * 修正#YEN: 円は小数なしで表示(外貨は小数2桁)
     C                   EVAL      *IN45 = (W#CURRCD = W#BASECUR)
     C                   EVAL      AMOUNT_DJ = W#AMOUNT

     C                   EVAL      W#ERROR = *OFF
     C                   EVAL      ERRMSG_D = W#ERRTXT
     C                   EVAL      W#ERRTXT = *BLANK

     C                   IF        W#SFLCNT > 0
     C                   EVAL      *IN91 = *ON
     C                   EVAL      *IN92 = *ON
     C                   ELSE
     C                   EVAL      *IN91 = *OFF
     C                   EVAL      *IN92 = *ON
      * 修正#CUR: 入金の通貨の請求書だけが対象
     C                   EVAL      ERRMSG_D = %TRIM(W#CURRCD) +
     C                             'の未収請求書はありません'
     C                   ENDIF

      * 修正#P9: SFLEND(*MORE)はON=終わり/OFF=続く(従来は逆だった)
     C                   IF        W#SFLCNT < 20
     C                   EVAL      *IN94 = *ON
     C                   ELSE
     C                   EVAL      *IN94 = *OFF
     C                   ENDIF

      * 修正#P7: 消込できない入金は確認画面へ進ませない
     C                   DOW       *ON
     C                   EXFMT     INVSFLCTL
     C                   EVAL      *IN06 = *OFF
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
     C                   IF        W#SFLCNT = 0
     C                   EVAL      ERRMSG_D = %TRIM(W#CURRCD) +
     C                             'の未収請求書はありません(F12=戻る)'
     C                   ITER
     C                   ENDIF
      * 修正#SEL: 1=選択した請求書を集計
     C                   EVAL      W#SELCNT = 0
     C                   EVAL      W#SELBAL = 0
     C                   EVAL      W#SI2 = 1
     C                   DOW       W#SI2 <= W#SFLCNT
     C                   EVAL      SFLRRN = W#SI2
     C     SFLRRN        CHAIN     INVSFL
     C                   IF        %FOUND(PAYENTRYD) AND SFLOPT = '1'
     C                   EVAL      W#SELCNT = W#SELCNT + 1
     C                   EVAL      W#SELID(W#SELCNT) = SFLINVID
     C                   EVAL      W#SELBAL = W#SELBAL + SFLBALH
     C                   ENDIF
     C                   EVAL      W#SI2 = W#SI2 + 1
     C                   ENDDO
     C                   IF        W#SELCNT > 0 AND W#AMOUNT > W#SELBAL
     C                   EVAL      ERRMSG_D = '入金額が選択した請求書の' +
     C                             '残高合計を超えています'
     C                   ITER
     C                   ENDIF
     C                   IF        W#AMOUNT > W#OPENBAL
     C                   EVAL      ERRMSG_D = '入金額が未収残高合計 ' +
     C                             %CHAR(W#OPENBAL) + ' を超えています'
     C                   ITER
     C                   ENDIF
     C                   LEAVE
     C                   ENDDO

     C                   ENDSR

      *===============================================================
      * @CONFIRM - 確認画面
      *===============================================================
     C     @CONFIRM      BEGSR

     C                   EVAL      W#ERROR = *OFF
     C                   EVAL      CUSTCD_C = W#CUSTCD
     C                   EVAL      CUSTNM_C = CUSTNM
     C                   EVAL      PAYDAT_C = W#PAYDATE
     C                   EVAL      PAYMTH_C = W#PAYMETH
     C                   EVAL      CURRCD_C = W#CURRCD
     C                   EVAL      REFNO_C = W#REFNO
     C                   EVAL      AMOUNT_C = W#AMOUNT
     C                   EVAL      *IN45 = (W#CURRCD = W#BASECUR)
     C                   EVAL      AMOUNT_CJ = W#AMOUNT

     C                   EXFMT     CONFIRM

     C                   ENDSR

      *===============================================================
      * @SAVEPAY - 入金登録・請求書消込処理 (SQL使用)
      *===============================================================
     C     @SAVEPAY      BEGSR

     C                   EVAL      W#ERROR = *OFF
     C                   TIME                    WTIME6

      * [修正#R3] PAYIDをSQLシーケンスで採番（競合リスク解消）
     C/EXEC SQL
     C+   VALUES NEXT VALUE FOR O2C_PAYID_SEQ
     C+          INTO :W#PAYID
     C/END-EXEC

     C                   IF        SQLCODE < 0
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      ERRMSG = '入金ID採番エラー'
     C                   LEAVESR
     C                   ENDIF

      * 修正#PN: 入金番号は入金IDから生成(秒単位の重複を防止)
     C                   EVAL      W#PAYNUM = 'PAY-' + %CHAR(W#PAYID)

      * [修正#R8] 為替換算はヘッダー検証(@VALIDHDR)で確定済み
      *           W#BASEAMT = 基準通貨(JPY)換算後の消込用金額

      * PAYMENTPへの入金レコード書込 (SQL INSERT)
      * 修正#P5: 入金日は画面で入力した日付(W#PAYD8)
     C/EXEC SQL
     C+   INSERT INTO PAYMENTP
     C+     (PAYID, PAYNUM, CUSTID, INVID, PAYDATE, PAYMETH,
     C+      CURRCD, AMOUNT, FXRATE, BASEAMT, REFNO, STATUS,
     C+      CRTUSER, CRTDATE, CRTTIM, UPDUSER, UPDDATE, UPDTIM)
     C+   VALUES
     C+     (:W#PAYID, :W#PAYNUM, :W#CUSTID, 0, :W#PAYD8, :W#PAYMETH,
     C+      :W#CURRCD, :W#AMOUNT, :W#FXRATE, :W#BASEAMT, :W#REFNO,
     C+      'APPLIED',
     C+      USER, :WDATE8, :WTIME6, USER, :WDATE8, :WTIME6)
     C/END-EXEC
     C                   IF        SQLCODE < 0
     C                   EVAL      ERRMSG = '入金登録でエラーが発生しました'
     C                   EXSR      @SAVEERR
     C                   LEAVESR
     C                   ENDIF

      * 修正#CUR: 消込は請求書の通貨(=入金の通貨)の金額で行う
      *           (円換算額 W#BASEAMT は入金の記録用)
     C                   EVAL      W#REMAMT = W#AMOUNT

     C/EXEC SQL
     C+   DECLARE UPDCSR CURSOR FOR
     C+   SELECT INVID, BALANCE, PAIDAMT, STATUS
     C+   FROM INVOICEHP
     C+   WHERE CUSTID = :W#CUSTID AND BALANCE > 0
     C+     AND CURRCD = :W#CURRCD
     C+   ORDER BY INVDATE
     C+   FOR UPDATE OF PAIDAMT, BALANCE, STATUS,
     C+                 UPDUSER, UPDDATE, UPDTIM
     C/END-EXEC

     C/EXEC SQL
     C+   OPEN UPDCSR
     C/END-EXEC
     C                   IF        SQLCODE < 0
     C                   EVAL      ERRMSG = '請求書の読込エラー'
     C                   EXSR      @SAVEERR
     C                   LEAVESR
     C                   ENDIF

     C                   DOW       SQLCODE = 0 AND W#REMAMT > 0

     C/EXEC SQL
     C+   FETCH UPDCSR
     C+   INTO :H#INVID, :H#BALANCE, :H#PAIDAMT, :H#STATUS
     C/END-EXEC
      * 修正#SEL: 選択がある場合は選択した請求書だけに消込
     C                   IF        SQLCODE = 0 AND W#SELCNT > 0
     C                   IF        %LOOKUP(H#INVID:W#SELID:1:W#SELCNT) = 0
     C                   ITER
     C                   ENDIF
     C                   ENDIF

     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF

     C                   IF        W#REMAMT >= H#BALANCE
     C                   EVAL      H#APPLYAMT = H#BALANCE
     C                   EVAL      H#PAIDAMT = H#PAIDAMT + H#BALANCE
     C                   EVAL      W#REMAMT = W#REMAMT - H#BALANCE
     C                   EVAL      H#BALANCE = 0
     C                   EVAL      H#STATUS = 'PAID'
     C                   ELSE
     C                   EVAL      H#APPLYAMT = W#REMAMT
     C                   EVAL      H#PAIDAMT = H#PAIDAMT + W#REMAMT
     C                   EVAL      H#BALANCE = H#BALANCE - W#REMAMT
     C                   EVAL      W#REMAMT = 0
     C                   EVAL      H#STATUS = 'PARTIAL'
     C                   ENDIF

     C/EXEC SQL
     C+   UPDATE INVOICEHP
     C+   SET PAIDAMT = :H#PAIDAMT, BALANCE = :H#BALANCE,
     C+       STATUS = :H#STATUS,
     C+       UPDUSER = USER, UPDDATE = :WDATE8, UPDTIM = :WTIME6
     C+   WHERE CURRENT OF UPDCSR
     C/END-EXEC
     C                   IF        SQLCODE < 0
     C                   EVAL      ERRMSG = '請求書の消込エラー'
     C                   EXSR      @SAVEERR
     C                   LEAVESR
     C                   ENDIF

      * [修正#R3] APPLYIDをSQLシーケンスで採番（競合リスク解消）
     C/EXEC SQL
     C+   VALUES NEXT VALUE FOR O2C_APPLYID_SEQ
     C+          INTO :H#APPLYID
     C/END-EXEC

     C/EXEC SQL
     C+   INSERT INTO PAYAPPLYP
     C+     (APPLYID, PAYID, INVID, APPLYAMT, APPLYDATE,
     C+      CRTUSER, CRTDATE, CRTTIM)
     C+   VALUES
     C+     (:H#APPLYID, :W#PAYID, :H#INVID, :H#APPLYAMT, :W#PAYD8,
     C+      USER, :WDATE8, :WTIME6)
     C/END-EXEC
     C                   IF        SQLCODE < 0
     C                   EVAL      ERRMSG = '充当明細の登録エラー'
     C                   EXSR      @SAVEERR
     C                   LEAVESR
     C                   ENDIF

     C                   ENDDO

     C                   IF        SQLCODE < 0
     C                   EVAL      ERRMSG = '請求書の読込エラー'
     C                   EXSR      @SAVEERR
     C                   LEAVESR
     C                   ENDIF

     C/EXEC SQL
     C+   CLOSE UPDCSR
     C/END-EXEC

      * 修正#P7: 消込しきれない残額がある(他で入金された等)場合は取消
     C                   IF        W#REMAMT > 0
     C                   EVAL      ERRMSG = '未収残高が変更されています' +
     C                             '。再度確認してください'
     C                   EXSR      @SAVEERR
     C                   LEAVESR
     C                   ENDIF

      * 修正#P8: 入金・消込・充当明細をまとめて確定
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   IF        SQLCODE < 0
     C                   EVAL      ERRMSG = '確定(COMMIT)エラー'
     C                   EXSR      @SAVEERR
     C                   ENDIF

     C                   ENDSR

      *===============================================================
      * 修正#P8 @SAVEERR - 登録エラー時の取消(ROLLBACK)
      *===============================================================
     C     @SAVEERR      BEGSR
     C                   EVAL      W#ERROR = *ON
     C/EXEC SQL
     C+   CLOSE UPDCSR
     C/END-EXEC
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   ENDSR

      *===============================================================
      * @SHOWCOMP - 完了画面表示
      *===============================================================
     C     @SHOWCOMP     BEGSR

     C                   EVAL      PAYNUM = W#PAYNUM

     C                   EXFMT     COMPLETE

     C                   ENDSR

      *=============================================================
      * Mod#F4 @SRCCUST - 顧客検索ウィンドウ
      * CUSTCD CSFSRCH プレフィックス一致で CUSTOMERL1 を検索
      * 選択時: CUSTCD/CUSTNM/W#CUSTID を画面バッファにセット
      *=============================================================
     C     @SRCCUST      BEGSR
     C                   EVAL      CSFSRCH = CUSTCD
     C                   EVAL      W#SELECTED = *OFF
      * 修正#P3: 条件を変えてEnterで再検索(1=選択/F12で終了)
     C                   DOW       NOT W#SELECTED
     C                   EVAL      W#SFLEOF = *OFF
     C                   EVAL      *IN71 = *OFF
     C                   EVAL      *IN73 = *ON
     C                   EVAL      *IN72 = *OFF
     C                   WRITE     CUSTSFLCTL
     C                   EVAL      *IN73 = *OFF
     C                   EVAL      W#SFRRN = 0
      * 修正#P3: 前方一致(READEはキー完全一致のため一致しなかった)
     C                   EVAL      W#SRCLEN = %LEN(%TRIM(CSFSRCH))
     C     CSFSRCH       SETLL     CSTREC
     C                   DOW       NOT W#SFLEOF AND W#SFRRN < 20
     C                   READ(E)   CSTREC
     C                   EVAL      W#SFLEOF = %EOF(CUSTOMERL1)
     C                   IF        W#SFLEOF
     C                   LEAVE
     C                   ENDIF
     C                   IF        W#SRCLEN > 0
     C                   IF        %SUBST(C_CUSTCD:1:W#SRCLEN) <>
     C                             %TRIM(CSFSRCH)
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   EVAL      W#SFRRN = W#SFRRN + 1
     C                   EVAL      CSFRRN = W#SFRRN
     C                   EVAL      CSFOPT = ' '
     C                   EVAL      CSFCD = C_CUSTCD
     C                   EVAL      CSFNM = DbcsTrunc(C_CUSTNM:36)
     C                   WRITE     CUSTSFL
     C                   ENDDO
     C                   IF        W#SFRRN > 0
     C                   EVAL      *IN71 = *ON
     C                   ENDIF
     C                   EVAL      *IN72 = *ON
     C                   EXFMT     CUSTSFLCTL
      * F12=キャンセル: 戻る
      * 修正#P2: RETURNはプログラム自体を終了させるためLEAVESR
     C                   IF        *IN12
     C                   EVAL      *IN12 = *OFF
     C                   LEAVESR
     C                   ENDIF
      * オプション1をスキャン
     C                   EVAL      CSFRRN = 0
     C                   DOW       CSFRRN < W#SFRRN
     C                   EVAL      CSFRRN = CSFRRN + 1
     C     CSFRRN        CHAIN(E)  CUSTSFL
     C                   IF        %ERROR OR NOT %FOUND(PAYENTRYD)
     C                   LEAVE
     C                   ENDIF
     C                   IF        CSFOPT = '1'
     C                   EVAL      CUSTCD = CSFCD
      * CUSTID/CUSTNM を取得するためバリデーション実行
     C     CUSTCD        CHAIN     CSTREC                             99
     C                   IF        NOT *IN99
     C                   EVAL      W#CUSTID = C_CUSTID
     C                   EVAL      CUSTNM = DbcsTrunc(C_CUSTNM:40)
     C                   ENDIF
     C                   EVAL      W#SELECTED = *ON
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDDO
     C                   ENDSR

      *===============================================================
      * 修正#B DbcsTrunc - 全角(SO/SI)を壊さずにpMaxバイトへ切り詰め
      *   %SUBSTのバイト切りでは全角文字の途中で切れて文字化けする
      *===============================================================
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

      *===============================================================
      * FmtAmt - 金額を編集する(入金の通貨が円なら小数なし)
      *===============================================================
     P FmtAmt          B
     D FmtAmt          PI            15A   VARYING
     D   pAmt                        15P 2 CONST
     D W#AMTED         S             20A   VARYING
     C                   IF        W#CURRCD = W#BASECUR
     C                   RETURN    %TRIM(%EDITC(%DECH(pAmt:12:0):'1'))
     C                   ENDIF
      * 修正#OVF 桁に収まらない金額・数量は*で表示(異常終了していた)
     C                   MONITOR
     C                   EVAL      W#AMTED = %TRIM(%EDITC(%DEC(pAmt:11:2):'1'))
     C                   ON-ERROR
     C                   EVAL      W#AMTED = *ALL'*'
     C                   ENDMON
      * 0は '.00' になるので '0.00' にする
     C                   IF        %SUBST(W#AMTED:1:1) = '.'
     C                   EVAL      W#AMTED = '0' + W#AMTED
     C                   ENDIF
     C                   RETURN    W#AMTED
     P FmtAmt          E
