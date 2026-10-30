     H DFTACTGRP(*NO) ACTGRP(*NEW)
      *=====================================================================
      * プログラム名: ARAGING
      * 説明: 売掛金年齢表(照会・印刷)
      * 作成: 修正#AGE 新規作成
      *   顧客ごとの未収残高を、基準日時点の支払期限からの経過日数で
      *   期限内 / 1-30日 / 31-60日 / 61-90日 / 91日超 に区分する
      *   5=請求書明細  F6=印刷(スプール ARAGPRTF)
      *   支払期限が0の請求書は請求日を期限とみなす
      *   修正#CUR 外貨は基準日以前の直近の為替レートで円に換算
      *=====================================================================
     FARAGINGD  CF   E             WORKSTN SFILE(CUSSFL:W#RRN)
     F                                     SFILE(DTLSFL:W#DRRN)
     FARAGPRTF  O    E             PRINTER OFLIND(*IN90) USROPN

     D W#RRN           S              4S 0
     D W#DRRN          S              4S 0
     D W#MSG           S             75A
     D W#F6            S               N
     D W#EXIT          S               N
     D W#I             S             10I 0
     D W#BASEISO       S             10A
     D W#BASE8         S              8P 0
     D W#NOFX          S             10I 0
     D W#BASEDSP       S             10A
     D W#LIKE          S             21A   VARYING
     D W#CUSTID        S              9P 0
     D W#D8            S              8A
     D W#CNT           S             10I 0
      * 修正#F4P: 共通の検索ウィンドウ(F4)
     D O2CPRMPT        PR                  EXTPGM('O2CPRMPT')
     D   pType                       10A   CONST
     D   pCdType                     30A   CONST
     D   pValue                      30A
     D W#PVAL          S             30A
      * 区分ごとの合計(1=未収合計 2=期限内 3=1-30日 4=31-60日
      *                 5=61-90日 6=91日超)
     D W#TOT           S             15P 2 DIM(6)
      * 顧客別集計の取り出し先
     D S#ROW           DS
     D S#CUSTID                       9P 0
     D S#CUSTCD                      20A
     D S#CUSTNM                      60A
     D S#A1                          15P 2
     D S#A2                          15P 2
     D S#A3                          15P 2
     D S#A4                          15P 2
     D S#A5                          15P 2
     D S#A6                          15P 2
      * 区分ごとの金額(S#A1-S#A6を配列で扱う)
     D S#AMT           S             15P 2 DIM(6)
      * 請求書明細の取り出し先
     D V#ROW           DS
     D V#INVNUM                      20A
     D V#INVDATE                      8P 0
     D V#DUEDATE                      8P 0
     D V#BAL                         15P 2
     D V#DAYS                        10I 0
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
     D DbcsTrunc       PR           256A   VARYING
     D   pStr                       256A   VARYING CONST
     D   pMax                        10I 0 CONST
      * 画面の金額欄(9桁)に収める
     D Fit9            PR             9P 0
     D   pAmt                        15P 2 CONST
     D FmtDate         PR            10A
     D   pDate                        8P 0 CONST

     C/EXEC SQL
     C+   SET OPTION COMMIT = *NONE, CLOSQLCSR = *ENDMOD
     C/END-EXEC

      * 顧客別の年齢区分集計
     C/EXEC SQL
     C+   DECLARE SUMCSR CURSOR FOR
     C+   WITH A AS (
      * 修正#CUR: 外貨の請求書は基準日の為替レートで円に換算して集計
     C+     SELECT H.CUSTID, H.BALANCE * (
     C+       CASE WHEN H.CURRCD IN ('JPY', ' ') THEN 1 ELSE COALESCE(
     C+        (SELECT F.RATE FROM FXRATEP F
     C+          WHERE F.FROMCUR = H.CURRCD AND F.TOCUR = 'JPY'
     C+            AND F.RATEDATE <= :W#BASE8
     C+          ORDER BY F.RATEDATE DESC FETCH FIRST 1 ROW ONLY),
     C+        (SELECT F.RATE FROM FXRATEP F
     C+          WHERE F.FROMCUR = H.CURRCD AND F.TOCUR = 'JPY'
     C+          ORDER BY F.RATEDATE FETCH FIRST 1 ROW ONLY), 0) END
     C+       ) AS BAL,
     C+            DAYS(DATE(:W#BASEISO)) - DAYS(DATE(TO_DATE(DIGITS(
     C+            CASE WHEN H.DUEDATE > 0 THEN H.DUEDATE
     C+                 ELSE H.INVDATE END), 'YYYYMMDD'))) AS D
     C+       FROM INVOICEHP H
     C+      WHERE H.BALANCE > 0)
     C+   SELECT C.CUSTID, C.CUSTCD, C.CUSTNM, SUM(A.BAL),
     C+          SUM(CASE WHEN A.D <= 0 THEN A.BAL ELSE 0 END),
     C+          SUM(CASE WHEN A.D BETWEEN 1 AND 30 THEN A.BAL ELSE 0 END),
     C+          SUM(CASE WHEN A.D BETWEEN 31 AND 60 THEN A.BAL ELSE 0 END),
     C+          SUM(CASE WHEN A.D BETWEEN 61 AND 90 THEN A.BAL ELSE 0 END),
     C+          SUM(CASE WHEN A.D > 90 THEN A.BAL ELSE 0 END)
     C+     FROM A JOIN CUSTOMERP C ON C.CUSTID = A.CUSTID
     C+    WHERE C.CUSTCD LIKE :W#LIKE
     C+    GROUP BY C.CUSTID, C.CUSTCD, C.CUSTNM
     C+    ORDER BY C.CUSTCD
     C/END-EXEC

      * 1顧客の未収請求書(支払期限の古い順)
     C/EXEC SQL
     C+   DECLARE DTLCSR CURSOR FOR
     C+   SELECT INVNUM, INVDATE, DUEDATE, BALANCE * (
     C+     CASE WHEN I.CURRCD IN ('JPY', ' ') THEN 1 ELSE COALESCE(
     C+      (SELECT F.RATE FROM FXRATEP F
     C+        WHERE F.FROMCUR = I.CURRCD AND F.TOCUR = 'JPY'
     C+          AND F.RATEDATE <= :W#BASE8
     C+        ORDER BY F.RATEDATE DESC FETCH FIRST 1 ROW ONLY),
     C+      (SELECT F.RATE FROM FXRATEP F
     C+        WHERE F.FROMCUR = I.CURRCD AND F.TOCUR = 'JPY'
     C+        ORDER BY F.RATEDATE FETCH FIRST 1 ROW ONLY), 0) END
     C+          ),
     C+          DAYS(DATE(:W#BASEISO)) - DAYS(DATE(TO_DATE(DIGITS(
     C+          CASE WHEN DUEDATE > 0 THEN DUEDATE
     C+               ELSE INVDATE END), 'YYYYMMDD')))
     C+     FROM INVOICEHP I
     C+    WHERE CUSTID = :W#CUSTID AND BALANCE > 0
     C+    ORDER BY DUEDATE, INVNUM
     C/END-EXEC

     C                   EVAL      BASEDT = %DEC(%DATE():*ISO)
     C                   EVAL      FILTER = *BLANKS
     C                   EVAL      W#MSG = *BLANKS
     C                   EXSR      @LOAD
     C                   DOW       NOT W#EXIT
     C                   EVAL      CMSG = W#MSG
     C                   EVAL      W#MSG = *BLANKS
     C                   EVAL      *IN92 = *ON
     C                   EVAL      *IN91 = (W#RRN > 0)
     C                   EVAL      *IN94 = *ON
     C                   EXFMT     CUSCTL
      * サブファイルのCHAINで*IN06が変わらないよう退避
     C                   EVAL      W#F6 = *IN06
     C                   EVAL      *IN06 = *OFF
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
      * 修正#F4P: F4=顧客の検索(選んだ顧客で絞り込む)
     C                   IF        *IN04
     C                   EVAL      W#PVAL = FILTER
     C                   CALLP     O2CPRMPT('CUST':' ':W#PVAL)
     C                   EVAL      FILTER = W#PVAL
     C                   EVAL      W#F6 = *OFF
     C                   ENDIF
     C                   EVAL      *IN31 = *OFF
     C     *ISO          TEST(DE)                BASEDT
     C                   IF        %ERROR
     C                   EVAL      *IN31 = *ON
     C                   EVAL      W#MSG = '基準日が無効です(YYYYMMDD)'
     C                   ITER
     C                   ENDIF
      * 明細・印刷も入力した基準日で行う
     C                   EXSR      @SETCOND
      * 5=明細を先に処理(一覧を作り直す前に読む)
     C                   IF        W#RRN > 0
     C                   READC     CUSSFL
     C                   DOW       NOT %EOF(ARAGINGD) AND NOT W#EXIT
     C                   IF        COPT = '5'
     C                   EXSR      @DETAIL
     C                   ENDIF
     C                   READC     CUSSFL
     C                   ENDDO
     C                   ENDIF
     C                   IF        W#EXIT
     C                   LEAVE
     C                   ENDIF
     C                   EXSR      @LOAD
     C                   IF        W#F6
     C                   EXSR      @PRINT
     C                   ENDIF
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C                   RETURN

      *=====================================================================
      * @SETCOND - 基準日・顧客コードの条件をSQL用に整える
      *=====================================================================
     C     @SETCOND      BEGSR
     C                   EVAL      W#BASEISO = %CHAR(%DATE(BASEDT:*ISO):*ISO)
     C                   EVAL      W#BASE8 = BASEDT
     C                   EVAL      W#BASEDSP = FmtDate(BASEDT)
     C                   EVAL      W#LIKE = %TRIM(FILTER) + '%'
     C                   ENDSR

      *=====================================================================
      * @TOARR - 取り出した区分ごとの金額を配列に移す
      *=====================================================================
     C     @TOARR        BEGSR
     C                   EVAL      S#AMT(1) = S#A1
     C                   EVAL      S#AMT(2) = S#A2
     C                   EVAL      S#AMT(3) = S#A3
     C                   EVAL      S#AMT(4) = S#A4
     C                   EVAL      S#AMT(5) = S#A5
     C                   EVAL      S#AMT(6) = S#A6
     C                   ENDSR

      *=====================================================================
      * @LOAD - 顧客別の年齢区分をサブファイルに読み込む
      *=====================================================================
     C     @LOAD         BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     CUSCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#RRN = 0
     C                   EVAL      W#TOT = 0
     C                   EXSR      @SETCOND
     C/EXEC SQL
     C+   OPEN SUMCSR
     C/END-EXEC
     C                   IF        SQLCODE < 0
     C                   EVAL      W#MSG = '集計に失敗しました SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   LEAVESR
     C                   ENDIF
     C                   DOW       W#RRN < 999
     C/EXEC SQL
     C+   FETCH SUMCSR INTO :S#ROW
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EXSR      @TOARR
     C                   EVAL      W#RRN = W#RRN + 1
     C                   EVAL      COPT = ' '
     C                   EVAL      CCUSTID = S#CUSTID
     C                   EVAL      CCUSTCD = S#CUSTCD
     C                   EVAL      CCUSTNM = DbcsTrunc(S#CUSTNM:40)
     C                   EVAL      CAMT0 = Fit9(S#AMT(1))
     C                   EVAL      CAMT1 = Fit9(S#AMT(2))
     C                   EVAL      CAMT2 = Fit9(S#AMT(3))
     C                   EVAL      CAMT3 = Fit9(S#AMT(4))
     C                   EVAL      CAMT4 = Fit9(S#AMT(5))
     C                   EVAL      CAMT5 = Fit9(S#AMT(6))
     C                   WRITE     CUSSFL
     C                   FOR       W#I = 1 TO 6
     C                   EVAL      W#TOT(W#I) = W#TOT(W#I) + S#AMT(W#I)
     C                   ENDFOR
     C                   ENDDO
     C                   IF        SQLCODE < 0
     C                   EVAL      W#MSG = '集計に失敗しました SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   ENDIF
     C/EXEC SQL
     C+   CLOSE SUMCSR
     C/END-EXEC
     C                   EVAL      TAMT0 = Fit9(W#TOT(1))
     C                   EVAL      TAMT1 = Fit9(W#TOT(2))
     C                   EVAL      TAMT2 = Fit9(W#TOT(3))
     C                   EVAL      TAMT3 = Fit9(W#TOT(4))
     C                   EVAL      TAMT4 = Fit9(W#TOT(5))
     C                   EVAL      TAMT5 = Fit9(W#TOT(6))
     C                   IF        W#RRN = 0 AND W#MSG = *BLANKS
     C                   EVAL      W#MSG = '未収残高のある顧客はありません'
     C                   ENDIF
      * 修正#CUR: 為替レートがない外貨の請求書は0円で集計される
     C/EXEC SQL
     C+   SELECT COUNT(*) INTO :W#NOFX FROM INVOICEHP H
     C+    WHERE H.BALANCE > 0 AND H.CURRCD NOT IN ('JPY', ' ')
     C+      AND NOT EXISTS (SELECT 1 FROM FXRATEP F
     C+           WHERE F.FROMCUR = H.CURRCD AND F.TOCUR = 'JPY')
     C/END-EXEC
     C                   IF        W#NOFX > 0 AND W#MSG = *BLANKS
     C                   EVAL      W#MSG = %CHAR(W#NOFX) + '件の外貨' +
     C                             'の請求書はレートがなく0円で集計'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @DETAIL - 選択した顧客の未収請求書を表示
      *=====================================================================
     C     @DETAIL       BEGSR
     C                   EVAL      W#CUSTID = CCUSTID
     C                   EVAL      DCUST = %TRIM(CCUSTCD) + '  ' + CCUSTNM
     C                   EVAL      DBASE = W#BASEDSP
     C                   EVAL      *IN83 = *ON
     C                   WRITE     DTLCTL
     C                   EVAL      *IN83 = *OFF
     C                   EVAL      W#DRRN = 0
     C/EXEC SQL
     C+   OPEN DTLCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#DRRN < 999
     C/EXEC SQL
     C+   FETCH DTLCSR INTO :V#ROW
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#DRRN = W#DRRN + 1
     C                   EVAL      DINVNUM = V#INVNUM
     C                   EVAL      DINVDT = FmtDate(V#INVDATE)
     C                   EVAL      DDUEDT = FmtDate(V#DUEDATE)
     C                   EVAL      DBAL = Fit9(V#BAL)
     C                   SELECT
     C                   WHEN      V#DAYS <= 0
     C                   EVAL      DBKT = '期限内'
     C                   WHEN      V#DAYS <= 30
     C                   EVAL      DBKT = '1-30日'
     C                   WHEN      V#DAYS <= 60
     C                   EVAL      DBKT = '31-60日'
     C                   WHEN      V#DAYS <= 90
     C                   EVAL      DBKT = '61-90日'
     C                   OTHER
     C                   EVAL      DBKT = '91日超'
     C                   ENDSL
     C                   IF        V#DAYS > 99999
     C                   EVAL      DDAYS = 99999
     C                   ELSEIF    V#DAYS > 0
     C                   EVAL      DDAYS = V#DAYS
     C                   ELSE
     C                   EVAL      DDAYS = 0
     C                   ENDIF
     C                   WRITE     DTLSFL
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE DTLCSR
     C/END-EXEC
     C                   EVAL      *IN82 = *ON
     C                   EVAL      *IN81 = (W#DRRN > 0)
     C                   EVAL      *IN84 = *ON
     C                   EXFMT     DTLCTL
     C                   IF        *IN03
     C                   EVAL      W#EXIT = *ON
     C                   ENDIF
     C                   EVAL      *IN03 = *OFF
     C                   EVAL      *IN12 = *OFF
     C                   ENDSR

      *=====================================================================
      * @PRINT - 年齢表を印刷(画面と同じ条件)
      *=====================================================================
     C     @PRINT        BEGSR
     C                   EXSR      @SETCOND
     C                   OPEN      ARAGPRTF
     C                   EVAL      PBASE = W#BASEDSP
     C                   WRITE     PHEAD
     C                   EVAL      *IN90 = *OFF
     C                   EVAL      W#TOT = 0
     C                   EVAL      W#CNT = 0
     C/EXEC SQL
     C+   OPEN SUMCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0
     C/EXEC SQL
     C+   FETCH SUMCSR INTO :S#ROW
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EXSR      @TOARR
     C                   IF        *IN90
     C                   WRITE     PHEAD
     C                   EVAL      *IN90 = *OFF
     C                   ENDIF
     C                   EVAL      W#CNT = W#CNT + 1
     C                   EVAL      PCUSTCD = S#CUSTCD
     C                   EVAL      PCUSTNM = DbcsTrunc(S#CUSTNM:22)
     C                   EVAL(H)   PAMT0 = S#AMT(1)
     C                   EVAL(H)   PAMT1 = S#AMT(2)
     C                   EVAL(H)   PAMT2 = S#AMT(3)
     C                   EVAL(H)   PAMT3 = S#AMT(4)
     C                   EVAL(H)   PAMT4 = S#AMT(5)
     C                   EVAL(H)   PAMT5 = S#AMT(6)
     C                   WRITE     PDETAIL
     C                   FOR       W#I = 1 TO 6
     C                   EVAL      W#TOT(W#I) = W#TOT(W#I) + S#AMT(W#I)
     C                   ENDFOR
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE SUMCSR
     C/END-EXEC
     C                   EVAL(H)   PTOT0 = W#TOT(1)
     C                   EVAL(H)   PTOT1 = W#TOT(2)
     C                   EVAL(H)   PTOT2 = W#TOT(3)
     C                   EVAL(H)   PTOT3 = W#TOT(4)
     C                   EVAL(H)   PTOT4 = W#TOT(5)
     C                   EVAL(H)   PTOT5 = W#TOT(6)
      * 構成比(未収合計に対する割合)
     C                   IF        W#TOT(1) > 0
     C                   EVAL      PPCT0 = 100
     C                   EVAL(H)   PPCT1 = W#TOT(2) * 100 / W#TOT(1)
     C                   EVAL(H)   PPCT2 = W#TOT(3) * 100 / W#TOT(1)
     C                   EVAL(H)   PPCT3 = W#TOT(4) * 100 / W#TOT(1)
     C                   EVAL(H)   PPCT4 = W#TOT(5) * 100 / W#TOT(1)
     C                   EVAL(H)   PPCT5 = W#TOT(6) * 100 / W#TOT(1)
     C                   ELSE
     C                   EVAL      PPCT0 = 0
     C                   EVAL      PPCT1 = 0
     C                   EVAL      PPCT2 = 0
     C                   EVAL      PPCT3 = 0
     C                   EVAL      PPCT4 = 0
     C                   EVAL      PPCT5 = 0
     C                   ENDIF
     C                   WRITE     PTOTAL
     C                   CLOSE     ARAGPRTF
     C                   EVAL      W#MSG = '売掛金年齢表を印刷しました(' +
     C                             %CHAR(W#CNT) + '社  スプール ARAGPRTF)'
     C                   ENDSR

      *=====================================================================
      * Fit9 - 9桁(999,999,999)に収めて四捨五入
      *=====================================================================
     P Fit9            B
     D Fit9            PI             9P 0
     D   pAmt                        15P 2 CONST
     D wAmt            S             15P 0
     C                   EVAL(H)   wAmt = pAmt
     C                   IF        wAmt > 999999999
     C                   RETURN    999999999
     C                   ENDIF
     C                   IF        wAmt < -99999999
     C                   RETURN    -99999999
     C                   ENDIF
     C                   RETURN    wAmt
     P Fit9            E

      *=====================================================================
      * FmtDate - YYYYMMDD を YYYY/MM/DD に
      *=====================================================================
     P FmtDate         B
     D FmtDate         PI            10A
     D   pDate                        8P 0 CONST
     D wD8             S              8A
     C                   IF        pDate = 0
     C                   RETURN    *BLANKS
     C                   ENDIF
     C                   EVAL      wD8 = %EDITC(pDate:'X')
     C                   RETURN    %SUBST(wD8:1:4) + '/' + %SUBST(wD8:5:2) +
     C                             '/' + %SUBST(wD8:7:2)
     P FmtDate         E

      *=====================================================================
      * DbcsTrunc - 全角(SO/SI)を壊さずに指定バイト数以内に切り詰める
      *=====================================================================
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
