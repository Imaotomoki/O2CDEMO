     H DFTACTGRP(*NO) ACTGRP(*NEW)
      *=====================================================================
      * プログラム名: SALESUM
      * 説明: 売上・入金の月次集計(12か月)と顧客別の内訳
      * 作成: 修正#SUM 新規作成。メニューの13番から呼ぶ
      *   請求書(取消済を除く)の件数・売上(税抜)・消費税・請求合計と
      *   入金(取消済を除く)を月ごとに集計する
      *   外貨は請求日の為替レートで円に換算。入金は入金時の円換算額
      *   5=顧客別の内訳(その月)
      *=====================================================================
     FSALESUMD  CF   E             WORKSTN SFILE(MSFL:W#RRN)
     F                                     SFILE(CSFL:W#CRRN)

     D W#RRN           S              4S 0
     D W#CRRN          S              4S 0
     D W#CCNT          S              4S 0
     D W#DONE          S               N
     D W#END           S              6P 0
     D W#Y             S              4P 0
     D W#M             S              2P 0
     D W#I             S             10I 0
     D W#YM            S              6P 0
     D W#F             S              8P 0
     D W#T             S              8P 0
     D W#SELYM         S              6P 0
     D S#CNT           S              9P 0
     D S#SALES         S             17P 2
     D S#TAX           S             17P 2
     D S#PAY           S             17P 2
     D W#TCNT          S              9P 0
     D W#TSAL          S             17P 2
     D W#TTAX          S             17P 2
     D W#TPAY          S             17P 2
     D C#CUST          S             20A
     D C#NAME          S             60A
     D FmtYen          PR            14A
     D   pAmt                        17P 2 CONST
     D   pLen                        10I 0 CONST
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
     D DbcsTrunc       PR           256A   VARYING
     D   pStr                       256A   VARYING CONST
     D   pMax                        10I 0 CONST
     D Fit9            PR             9P 0
     D   pAmt                        15P 2 CONST
     D FmtDate         PR            10A
     D   pDate                        8P 0 CONST

     C/EXEC SQL
     C+   SET OPTION COMMIT = *NONE, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE CUSCSR CURSOR FOR
     C+   SELECT C.CUSTCD, C.CUSTNM, COALESCE(I.N, 0), COALESCE(I.S, 0),
     C+          COALESCE(I.T, 0), COALESCE(P.A, 0)
     C+     FROM CUSTOMERP C
     C+     LEFT JOIN (SELECT X.CUSTID, COUNT(*) AS N, SUM(X.S) AS S,
     C+                       SUM(X.T) AS T
     C+        FROM (SELECT H.CUSTID, H.SUBTOT * FX.R AS S,
     C+                     H.TAXAMT * FX.R AS T
     C+                FROM INVOICEHP H, LATERAL (SELECT
     C+           CASE WHEN H.CURRCD IN ('JPY', ' ') THEN 1 ELSE COALESCE(
     C+            (SELECT F.RATE FROM FXRATEP F
     C+              WHERE F.FROMCUR = H.CURRCD AND F.TOCUR = 'JPY'
     C+                AND F.RATEDATE <= H.INVDATE
     C+              ORDER BY F.RATEDATE DESC FETCH FIRST 1 ROW ONLY),
     C+            (SELECT F.RATE FROM FXRATEP F
     C+              WHERE F.FROMCUR = H.CURRCD AND F.TOCUR = 'JPY'
     C+              ORDER BY F.RATEDATE FETCH FIRST 1 ROW ONLY), 0) END
     C+                     AS R FROM SYSIBM.SYSDUMMY1) FX
     C+               WHERE H.STATUS <> 'CANCELLED'
     C+                 AND H.INVDATE BETWEEN :W#F AND :W#T) X
     C+       GROUP BY X.CUSTID) I ON I.CUSTID = C.CUSTID
     C+     LEFT JOIN (SELECT CUSTID, SUM(BASEAMT) AS A FROM PAYMENTP
     C+                 WHERE STATUS <> 'CANCELLED'
     C+                   AND PAYDATE BETWEEN :W#F AND :W#T
     C+                 GROUP BY CUSTID) P ON P.CUSTID = C.CUSTID
     C+    WHERE I.N IS NOT NULL OR P.A IS NOT NULL
     C+    ORDER BY C.CUSTCD
     C+    FOR READ ONLY
     C/END-EXEC

     C                   EVAL      MEND = %SUBST(%CHAR(%DATE():*ISO0):1:6)
     C                   EVAL      MMSG = *BLANKS
     C                   EXSR      @LOAD
     C                   DOW       NOT W#DONE
     C                   EVAL      *IN92 = *ON
     C                   EVAL      *IN91 = *ON
     C                   WRITE     MFOOT
     C                   EXFMT     MCTL
     C                   EVAL      MMSG = *BLANKS
     C                   EVAL      *IN31 = *OFF
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
      * 5=顧客別の内訳(最初の1件)
     C                   EVAL      W#SELYM = 0
     C                   READC     MSFL
     C                   DOW       NOT %EOF(SALESUMD)
     C                   IF        MOPT = '5' AND W#SELYM = 0
     C                   EVAL      W#SELYM = MYMH
     C                   ENDIF
     C                   READC     MSFL
     C                   ENDDO
     C                   IF        W#SELYM > 0
     C                   EXSR      @CUST
     C                   ENDIF
     C                   EXSR      @LOAD
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C                   RETURN

      *=====================================================================
      * @LOAD - 終了年月までの12か月を集計する
      *=====================================================================
     C     @LOAD         BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     MCTL
     C                   EVAL      *IN93 = *OFF
     C                   MONITOR
     C                   EVAL      W#END = %DEC(MEND:6:0)
     C                   ON-ERROR
     C                   EVAL      W#END = 0
     C                   ENDMON
     C                   EVAL      W#Y = %DIV(W#END:100)
     C                   EVAL      W#M = %REM(W#END:100)
     C                   IF        W#Y < 1900 OR W#M < 1 OR W#M > 12
     C                   EVAL      *IN31 = *ON
     C                   EVAL      MMSG = '年月はYYYYMMで入力'
     C                   EVAL      MEND = %SUBST(%CHAR(%DATE():*ISO0):1:6)
     C                   EVAL      W#END = %DEC(MEND:6:0)
     C                   EVAL      W#Y = %DIV(W#END:100)
     C                   EVAL      W#M = %REM(W#END:100)
     C                   ENDIF
      * 11か月前から始める
     C                   EVAL      W#M = W#M - 11
     C                   IF        W#M <= 0
     C                   EVAL      W#M = W#M + 12
     C                   EVAL      W#Y = W#Y - 1
     C                   ENDIF
     C                   EVAL      W#TCNT = 0
     C                   EVAL      W#TSAL = 0
     C                   EVAL      W#TTAX = 0
     C                   EVAL      W#TPAY = 0
     C                   FOR       W#I = 1 TO 12
     C                   EVAL      W#YM = W#Y * 100 + W#M
     C                   EVAL      W#F = W#YM * 100 + 1
     C                   EVAL      W#T = W#YM * 100 + 31
     C/EXEC SQL
     C+   SELECT COUNT(*), COALESCE(SUM(X.S), 0), COALESCE(SUM(X.T), 0)
     C+     INTO :S#CNT, :S#SALES, :S#TAX
     C+     FROM (SELECT H.SUBTOT * FX.R AS S, H.TAXAMT * FX.R AS T
     C+             FROM INVOICEHP H, LATERAL (SELECT
     C+           CASE WHEN H.CURRCD IN ('JPY', ' ') THEN 1 ELSE COALESCE(
     C+            (SELECT F.RATE FROM FXRATEP F
     C+              WHERE F.FROMCUR = H.CURRCD AND F.TOCUR = 'JPY'
     C+                AND F.RATEDATE <= H.INVDATE
     C+              ORDER BY F.RATEDATE DESC FETCH FIRST 1 ROW ONLY),
     C+            (SELECT F.RATE FROM FXRATEP F
     C+              WHERE F.FROMCUR = H.CURRCD AND F.TOCUR = 'JPY'
     C+              ORDER BY F.RATEDATE FETCH FIRST 1 ROW ONLY), 0) END
     C+                  AS R FROM SYSIBM.SYSDUMMY1) FX
     C+            WHERE H.STATUS <> 'CANCELLED'
     C+              AND H.INVDATE BETWEEN :W#F AND :W#T) X
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      S#CNT = 0
     C                   EVAL      S#SALES = 0
     C                   EVAL      S#TAX = 0
     C                   EVAL      MMSG = '集計エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   ENDIF
     C/EXEC SQL
     C+   SELECT COALESCE(SUM(BASEAMT), 0) INTO :S#PAY FROM PAYMENTP
     C+    WHERE STATUS <> 'CANCELLED' AND PAYDATE BETWEEN :W#F AND :W#T
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      S#PAY = 0
     C                   ENDIF
     C                   EVAL      MOPT = ' '
     C                   EVAL      MYMH = W#YM
     C                   EVAL      MYM = %CHAR(W#Y) + '/' +
     C                             %SUBST(%EDITC(W#M:'X'):1:2)
     C                   EVALR     MCNT = %CHAR(S#CNT)
     C                   EVAL      MSALES = FmtYen(S#SALES:14)
     C                   EVAL      MTAX = FmtYen(S#TAX:12)
     C                   EVAL      MTOT = FmtYen(S#SALES + S#TAX:14)
     C                   EVAL      MPAY = FmtYen(S#PAY:14)
     C                   EVAL      W#RRN = W#I
     C                   WRITE     MSFL
     C                   EVAL      W#TCNT = W#TCNT + S#CNT
     C                   EVAL      W#TSAL = W#TSAL + S#SALES
     C                   EVAL      W#TTAX = W#TTAX + S#TAX
     C                   EVAL      W#TPAY = W#TPAY + S#PAY
     C                   EVAL      W#M = W#M + 1
     C                   IF        W#M > 12
     C                   EVAL      W#M = 1
     C                   EVAL      W#Y = W#Y + 1
     C                   ENDIF
     C                   ENDFOR
     C                   EVALR     TCNT = %CHAR(W#TCNT)
     C                   EVAL      TSALES = FmtYen(W#TSAL:14)
     C                   EVAL      TTAX = FmtYen(W#TTAX:12)
     C                   EVAL      TTOT = FmtYen(W#TSAL + W#TTAX:14)
     C                   EVAL      TPAY = FmtYen(W#TPAY:14)
     C                   ENDSR

      *=====================================================================
      * @CUST - 選んだ月の顧客別の内訳(F12で戻る)
      *=====================================================================
     C     @CUST         BEGSR
     C                   EVAL      W#F = W#SELYM * 100 + 1
     C                   EVAL      W#T = W#SELYM * 100 + 31
     C                   EVAL      CYM = %SUBST(%EDITC(W#SELYM:'X'):1:4) + '/' +
     C                             %SUBST(%EDITC(W#SELYM:'X'):5:2)
     C                   EVAL      *IN83 = *ON
     C                   WRITE     CCTL
     C                   EVAL      *IN83 = *OFF
     C                   EVAL      W#CCNT = 0
     C/EXEC SQL
     C+   OPEN CUSCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#CCNT < 999
     C/EXEC SQL
     C+   FETCH CUSCSR INTO :C#CUST, :C#NAME, :S#CNT, :S#SALES, :S#TAX,
     C+     :S#PAY
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      CCUST = C#CUST
     C                   EVAL      CNAME = DbcsTrunc(%TRIMR(C#NAME):16)
     C                   EVALR     CCNT = %CHAR(S#CNT)
     C                   EVAL      CSALES = FmtYen(S#SALES:12)
     C                   EVAL      CTAX = FmtYen(S#TAX:10)
     C                   EVAL      CTOT = FmtYen(S#SALES + S#TAX:12)
     C                   EVAL      CPAY = FmtYen(S#PAY:9)
     C                   EVAL      W#CCNT = W#CCNT + 1
     C                   EVAL      W#CRRN = W#CCNT
     C                   WRITE     CSFL
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE CUSCSR
     C/END-EXEC
     C                   IF        W#CCNT = 0
     C                   EVAL      CMSG = 'この月の請求・入金はありません'
     C                   ELSE
     C                   EVAL      CMSG = *BLANKS
     C                   ENDIF
     C                   EVAL      *IN82 = *ON
     C                   EVAL      *IN81 = (W#CCNT > 0)
     C                   EVAL      *IN84 = *ON
     C                   WRITE     CFOOT
     C                   EXFMT     CCTL
     C                   IF        *IN03
     C                   EVAL      W#DONE = *ON
     C                   ENDIF
     C                   ENDSR

      * FmtYen - 円の金額を右寄せの文字に(小数なし。pLen桁に収める)
     P FmtYen          B
     D FmtYen          PI            14A
     D   pAmt                        17P 2 CONST
     D   pLen                        10I 0 CONST
     D wTxt            S             20A   VARYING
     D wOut            S             14A
     C                   EVAL      wTxt = %TRIM(%EDITC(%DECH(pAmt:15:0):'J'))
     C                   IF        %LEN(wTxt) > pLen
     C                   EVAL      wTxt = %SUBST('*************':1:pLen)
     C                   ENDIF
     C                   EVAL      wOut = *BLANKS
     C                   EVAL      %SUBST(wOut:pLen - %LEN(wTxt) + 1:
     C                             %LEN(wTxt)) = wTxt
     C                   RETURN    wOut
     P FmtYen          E

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
