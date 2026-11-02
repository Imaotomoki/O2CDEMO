     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: GLEXPORT
      * 説明: 会計仕訳データの出力(CSV)。メニューの24番から呼ぶ
      * 作成: 修正#GL 新規作成
      *   対象期間の売上(請求書・赤伝)と入金を仕訳にして、IFS に
      *   CSV(UTF-8、CRLF)で出す。ファイル名 GL_開始日_終了日.csv
      *   売上: 借方 売掛金(税込)/貸方 売上高(税抜)・仮受消費税
      *   入金: 借方 現金預金/貸方 売掛金(入金時の円換算額)
      *   外貨の請求書は請求日以前で直近の為替レートで円換算
      *   (なければ最も古いレート)。外貨の金額も別の列に出す
      *   取消済の請求書・入金、赤伝の相殺(CREDITNOTE)は出さない
      *   科目名はコードマスター GL_ACCOUNT(AR/SALES/TAX/CASH)
      *   修正#BNK 振込手数料(PAYMETH BANKFEE)は 借方 支払手数料
      *     (科目名は GL_ACCOUNT の FEE)/貸方 売掛金
      *=====================================================================
     FGLEXPD    CF   E             WORKSTN

     D W#ERR           S               N
     D W#DT            S               D
     D W#FILE          S            128A   VARYING
     D W#LINE          S           1024A   VARYING
     D W#SCNT          S              7P 0
     D W#PCNT          S              7P 0
     D W#USER          S             10A   INZ(*USER)
     D W#SQLCD         S             10I 0
      * 科目名
     D W#ACAR          S             60A
     D W#ACSAL         S             60A
     D W#ACTAX         S             60A
     D W#ACCSH         S             60A
     D W#ACFEE         S             60A
     D W#ACDR          S             60A
     D W#DESC          S            120A
      * 読み込み
     D S#KIND          S              1A
     D S#DATE          S              8P 0
     D S#DOC           S             20A
     D S#CUR           S              3A
     D S#TOT           S             15P 2
     D S#SUB           S             15P 2
     D S#TAX           S             15P 2
     D S#FX            S             10P 6
     D S#BASE          S             15P 2
     D S#CD            S             20A
     D S#NM            S            120A
     D W#JTOT          S             15P 0
     D W#JSUB          S             15P 0
     D W#JTAX          S             15P 0
     D W#D10           S             10A
     D W#TXT           S            160A   VARYING
     D NUMTXT          PR            24A   VARYING
     D   PAMT                        15P 2 CONST
     D CSV             PR           200A   VARYING
     D   PSTR                       200A   VARYING CONST
     D WRITELN         PR
     D   PLINE                     1024A   VARYING CONST
     D   PNEW                          N   CONST

     C/EXEC SQL
     C+   SET OPTION COMMIT = *NONE, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE GLCSR CURSOR FOR
     C+   SELECT 'S', H.INVDATE, H.INVNUM, H.CURRCD, H.TOTAMT, H.SUBTOT,
     C+          H.TAXAMT,
     C+          CASE WHEN H.CURRCD IN ('JPY', ' ') THEN 1 ELSE COALESCE(
     C+           (SELECT F.RATE FROM FXRATEP F
     C+             WHERE F.FROMCUR = H.CURRCD AND F.TOCUR = 'JPY'
     C+               AND F.RATEDATE <= H.INVDATE
     C+             ORDER BY F.RATEDATE DESC FETCH FIRST 1 ROW ONLY),
     C+           (SELECT F.RATE FROM FXRATEP F
     C+             WHERE F.FROMCUR = H.CURRCD AND F.TOCUR = 'JPY'
     C+             ORDER BY F.RATEDATE FETCH FIRST 1 ROW ONLY), 0) END,
     C+          0, C.CUSTCD, CAST(C.CUSTNM AS VARCHAR(120) FOR BIT DATA)
     C+     FROM INVOICEHP H JOIN CUSTOMERP C ON C.CUSTID = H.CUSTID
     C+    WHERE H.STATUS <> 'CANCELLED'
     C+      AND H.INVDATE BETWEEN :DFROM AND :DTO
     C+   UNION ALL
     C+   SELECT CASE WHEN P.PAYMETH = 'BANKFEE' THEN 'F' ELSE 'P' END,
     C+          P.PAYDATE, P.PAYNUM, P.CURRCD, P.AMOUNT, 0, 0, P.FXRATE,
     C+          P.BASEAMT, C.CUSTCD,
     C+          CAST(C.CUSTNM AS VARCHAR(120) FOR BIT DATA)
     C+     FROM PAYMENTP P JOIN CUSTOMERP C ON C.CUSTID = P.CUSTID
     C+    WHERE P.STATUS <> 'CANCELLED' AND P.PAYMETH <> 'CREDITNOTE'
     C+      AND P.PAYDATE BETWEEN :DFROM AND :DTO
     C+    ORDER BY 2, 1 DESC, 3
     C+    FOR READ ONLY
     C/END-EXEC

      * 初期値: 今月の1日から今日まで、出力先は自分のホーム
     C                   EVAL      W#DT = %DATE()
     C                   EVAL      DTO = %DEC(W#DT:*ISO)
     C                   EVAL      DFROM = %DEC(W#DT - %DAYS(%SUBDT(W#DT:*D)
     C                             - 1):*ISO)
     C                   EVAL      DPATH = '/home/' + %TRIM(W#USER) + '/gl'
     C                   DOW       *ON
     C                   EXFMT     GL
     C                   EVAL      DMSG = *BLANKS
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
     C                   EXSR      @CHECK
     C                   IF        W#ERR
     C                   ITER
     C                   ENDIF
     C                   EXSR      @EXPORT
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C                   RETURN

      * @CHECK - 入力チェック
     C     @CHECK        BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      *IN31 = *OFF
     C                   EVAL      *IN32 = *OFF
     C                   EVAL      *IN33 = *OFF
     C     *ISO          TEST(DE)                DFROM
     C                   IF        %ERROR
     C                   EVAL      *IN31 = *ON
     C                   EVAL      DMSG = '開始日が無効です(YYYYMMDD)'
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C     *ISO          TEST(DE)                DTO
     C                   IF        %ERROR OR DTO < DFROM
     C                   EVAL      *IN32 = *ON
     C                   EVAL      DMSG = '終了日が無効か開始日より前です'
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   IF        %SUBST(DPATH:1:1) <> '/'
     C                   EVAL      *IN33 = *ON
     C                   EVAL      DMSG = '出力先は / で始めます'
     C                   EVAL      W#ERR = *ON
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @EXPORT - CSVを作る
      *=====================================================================
     C     @EXPORT       BEGSR
     C                   EXSR      @ACCOUNT
     C                   EVAL      W#FILE = %TRIMR(DPATH)
     C                   IF        %SUBST(W#FILE:%LEN(W#FILE):1) <> '/'
     C                   EVAL      W#FILE = W#FILE + '/'
     C                   ENDIF
     C                   EVAL      W#FILE = W#FILE + 'GL_' + %CHAR(DFROM) +
     C                             '_' + %CHAR(DTO) + '.csv'
     C                   EVAL      W#SCNT = 0
     C                   EVAL      W#PCNT = 0
     C                   EVAL      W#ERR = *OFF
      * 見出し行(ファイルを作り直す)
     C                   CALLP     WRITELN('伝票日付,伝票番号,借方科目,' +
     C                             '借方金額,貸方科目,貸方金額,通貨,' +
     C                             '外貨金額,取引先コード,取引先名,摘要'
     C                             :*ON)
     C                   IF        W#ERR
     C                   LEAVESR
     C                   ENDIF
     C/EXEC SQL
     C+   OPEN GLCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0
     C/EXEC SQL
     C+   FETCH GLCSR INTO :S#KIND, :S#DATE, :S#DOC, :S#CUR, :S#TOT, :S#SUB,
     C+     :S#TAX, :S#FX, :S#BASE, :S#CD, :S#NM
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#D10 = %SUBST(%EDITC(S#DATE:'X'):1:4) +
     C                             '/' + %SUBST(%EDITC(S#DATE:'X'):5:2) +
     C                             '/' + %SUBST(%EDITC(S#DATE:'X'):7:2)
     C                   IF        S#KIND = 'S'
     C                   EXSR      @SALES
     C                   ELSE
     C                   EXSR      @PAY
     C                   ENDIF
     C                   IF        W#ERR
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   EVAL      W#SQLCD = SQLCODE
     C/EXEC SQL
     C+   CLOSE GLCSR
     C/END-EXEC
     C                   EVAL      DSCNT = W#SCNT
     C                   EVAL      DPCNT = W#PCNT
     C                   EVAL      DFILE = W#FILE
     C                   IF        W#ERR
     C                   LEAVESR
     C                   ENDIF
     C                   IF        W#SQLCD < 0
     C                   EVAL      DMSG = '読み込みエラー SQLCODE=' +
     C                             %CHAR(W#SQLCD)
     C                   ELSE
     C                   EVAL      DMSG = '出力しました(売上 ' +
     C                             %CHAR(W#SCNT) + ' 件 入金 ' +
     C                             %CHAR(W#PCNT) + ' 件)'
     C                   ENDIF
     C                   ENDSR

      * @SALES - 売上の仕訳(2行: 売掛金/売上高、空欄/仮受消費税)
     C     @SALES        BEGSR
      * 円換算(円は換算しない)
     C                   IF        S#CUR = 'JPY' OR S#CUR = *BLANKS
     C                   EVAL(H)   W#JTOT = S#TOT
     C                   EVAL(H)   W#JSUB = S#SUB
     C                   EVAL(H)   W#JTAX = S#TAX
     C                   ELSE
     C                   EVAL(H)   W#JSUB = S#SUB * S#FX
     C                   EVAL(H)   W#JTAX = S#TAX * S#FX
     C                   EVAL      W#JTOT = W#JSUB + W#JTAX
     C                   ENDIF
     C                   EVAL      W#TXT = ''
     C                   IF        S#TOT < 0
     C                   EVAL      W#TXT = '赤伝'
     C                   ENDIF
     C                   EVAL      W#LINE = W#D10 + ',' + CSV(S#DOC) + ',' +
     C                             CSV(W#ACAR) + ',' + NUMTXT(W#JTOT) + ',' +
     C                             CSV(W#ACSAL) + ',' + NUMTXT(W#JSUB) + ',' +
     C                             %TRIM(S#CUR) + ',' + NUMTXT(S#SUB) + ',' +
     C                             CSV(S#CD) + ',' + CSV(S#NM) + ',' +
     C                             CSV(W#TXT)
     C                   CALLP     WRITELN(W#LINE:*OFF)
     C                   IF        W#ERR
     C                   LEAVESR
     C                   ENDIF
     C                   IF        S#TAX <> 0
     C                   EVAL      W#LINE = W#D10 + ',' + CSV(S#DOC) + ',,,' +
     C                             CSV(W#ACTAX) + ',' + NUMTXT(W#JTAX) + ',' +
     C                             %TRIM(S#CUR) + ',' + NUMTXT(S#TAX) + ',' +
     C                             CSV(S#CD) + ',' + CSV(S#NM) + ',' +
     C                             CSV(W#TXT)
     C                   CALLP     WRITELN(W#LINE:*OFF)
     C                   ENDIF
     C                   EVAL      W#SCNT = W#SCNT + 1
     C                   ENDSR

      * @PAY - 入金の仕訳(現金預金/売掛金、円換算額)
     C     @PAY          BEGSR
     C                   EVAL(H)   W#JTOT = S#BASE
     C                   IF        S#KIND = 'F'
     C                   EVAL      W#ACDR = W#ACFEE
     C                   ELSE
     C                   EVAL      W#ACDR = W#ACCSH
     C                   ENDIF
     C                   EVAL      W#LINE = W#D10 + ',' + CSV(S#DOC) + ',' +
     C                             CSV(W#ACDR) + ',' + NUMTXT(W#JTOT) + ',' +
     C                             CSV(W#ACAR) + ',' + NUMTXT(W#JTOT) + ',' +
     C                             %TRIM(S#CUR) + ',' + NUMTXT(S#TOT) + ',' +
     C                             CSV(S#CD) + ',' + CSV(S#NM) + ','
     C                   CALLP     WRITELN(W#LINE:*OFF)
     C                   EVAL      W#PCNT = W#PCNT + 1
     C                   ENDSR

      * @ACCOUNT - 科目名(コードマスター GL_ACCOUNT。なければ標準)
     C     @ACCOUNT      BEGSR
     C                   EVAL      W#ACAR = '売掛金'
     C                   EVAL      W#ACSAL = '売上高'
     C                   EVAL      W#ACTAX = '仮受消費税'
     C                   EVAL      W#ACCSH = '現金預金'
     C                   EVAL      W#ACFEE = '支払手数料'
     C                   EVAL      W#DESC = *BLANKS
     C/EXEC SQL
     C+   SELECT CAST(DESC AS VARCHAR(120) FOR BIT DATA) INTO :W#DESC
     C+     FROM CODEMSTRP WHERE CODETYPE = 'GL_ACCOUNT' AND CODEVAL = 'AR'
     C/END-EXEC
     C                   IF        SQLCODE = 0 AND W#DESC <> *BLANKS
     C                   EVAL      W#ACAR = W#DESC
     C                   ENDIF
     C/EXEC SQL
     C+   SELECT CAST(DESC AS VARCHAR(120) FOR BIT DATA) INTO :W#DESC
     C+     FROM CODEMSTRP WHERE CODETYPE = 'GL_ACCOUNT' AND CODEVAL = 'SALES'
     C/END-EXEC
     C                   IF        SQLCODE = 0 AND W#DESC <> *BLANKS
     C                   EVAL      W#ACSAL = W#DESC
     C                   ENDIF
     C/EXEC SQL
     C+   SELECT CAST(DESC AS VARCHAR(120) FOR BIT DATA) INTO :W#DESC
     C+     FROM CODEMSTRP WHERE CODETYPE = 'GL_ACCOUNT' AND CODEVAL = 'TAX'
     C/END-EXEC
     C                   IF        SQLCODE = 0 AND W#DESC <> *BLANKS
     C                   EVAL      W#ACTAX = W#DESC
     C                   ENDIF
     C/EXEC SQL
     C+   SELECT CAST(DESC AS VARCHAR(120) FOR BIT DATA) INTO :W#DESC
     C+     FROM CODEMSTRP WHERE CODETYPE = 'GL_ACCOUNT' AND CODEVAL = 'CASH'
     C/END-EXEC
     C                   IF        SQLCODE = 0 AND W#DESC <> *BLANKS
     C                   EVAL      W#ACCSH = W#DESC
     C                   ENDIF
     C/EXEC SQL
     C+   SELECT CAST(DESC AS VARCHAR(120) FOR BIT DATA) INTO :W#DESC
     C+     FROM CODEMSTRP WHERE CODETYPE = 'GL_ACCOUNT' AND CODEVAL = 'FEE'
     C/END-EXEC
     C                   IF        SQLCODE = 0 AND W#DESC <> *BLANKS
     C                   EVAL      W#ACFEE = W#DESC
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * WRITELN - CSVに1行書く(PNEW=*ON はファイルを作り直す)
      *=====================================================================
     P WRITELN         B
     D WRITELN         PI
     D   PLINE                     1024A   VARYING CONST
     D   PNEW                          N   CONST
     D WMODE           S              8A
     D WLINE           S           1024A   VARYING
     C                   IF        PNEW
     C                   EVAL      WMODE = 'REPLACE'
     C                   ELSE
     C                   EVAL      WMODE = 'APPEND'
     C                   ENDIF
     C                   EVAL      WLINE = PLINE
     C/EXEC SQL
     C+   CALL QSYS2.IFS_WRITE_UTF8(PATH_NAME => :W#FILE, LINE => :WLINE,
     C+        OVERWRITE => :WMODE, END_OF_LINE => 'CRLF')
     C/END-EXEC
     C                   IF        SQLCODE < 0
     C                   EVAL      DMSG = 'ファイルに書けません SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EVAL      W#ERR = *ON
     C                   ENDIF
     P WRITELN         E

      *=====================================================================
      * NUMTXT - 金額を文字に(カンマなし、小数は必要なときだけ)
      *=====================================================================
     P NUMTXT          B
     D NUMTXT          PI            24A   VARYING
     D   PAMT                        15P 2 CONST
     D WTXT            S             24A   VARYING
     C                   IF        PAMT = %INT(PAMT)
     C                   RETURN    %CHAR(%INT(PAMT))
     C                   ENDIF
     C                   EVAL      WTXT = %CHAR(PAMT)
     C                   IF        %SUBST(WTXT:1:1) = '.'
     C                   EVAL      WTXT = '0' + WTXT
     C                   ENDIF
     C                   IF        %LEN(WTXT) > 1 AND
     C                             %SUBST(WTXT:1:2) = '-.'
     C                   EVAL      WTXT = '-0' + %SUBST(WTXT:2)
     C                   ENDIF
     C                   RETURN    WTXT
     P NUMTXT          E

      *=====================================================================
      * CSV - 文字の項目を "..." で囲む(" は "" にする)
      *=====================================================================
     P CSV             B
     D CSV             PI           200A   VARYING
     D   PSTR                       200A   VARYING CONST
     C                   IF        %TRIM(PSTR) = ''
     C                   RETURN    ''
     C                   ENDIF
     C                   RETURN    '"' + %SCANRPL('"':'""':%TRIM(PSTR)) + '"'
     P CSV             E
