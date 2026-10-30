     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: CUSTINQ
      * 説明: 顧客の与信・残高照会
      * 作成: 修正#CIQ 新規作成。メニューの14番から呼ぶ
      *   与信使用額 = 未収残高 + 未出荷の受注額 + 出荷済未請求額
      *   (受注入力の与信チェックと同じ式。未出荷・未請求は税込)
      *   残り与信 = 与信限度額 - 与信使用額(限度額0は無制限)
      *   金額は顧客の通貨(円は小数なし、外貨は詳細だけ小数2桁)
      *   5=詳細(内訳・未収件数・最も長い期限超過日数・最後の入金日)
      *=====================================================================
     FCUSTINQD  CF   E             WORKSTN SFILE(LSFL:W#RRN)

     D W#RRN           S              4S 0
     D W#CNT           S              4S 0
     D W#DONE          S               N
     D W#PVAL          S             30A
     D W#LIKE          S             21A   VARYING
     D W#SEL           S              9P 0
     D W#CUR           S              3A
     D S#ID            S              9P 0
     D S#CD            S             20A
     D S#NM            S             60A
     D S#CUR           S              3A
     D S#LMT           S             17P 2
     D S#STS           S             10A
     D S#AR            S             17P 2
     D S#UNSHP         S             17P 2
     D S#UNBIL         S             17P 2
     D S#CNT           S              9P 0
     D S#OVD           S              9P 0
     D S#LPAY          S              8P 0
     D W#USED          S             17P 2
     D W#REM           S             17P 2
     D O2CPRMPT        PR                  EXTPGM('O2CPRMPT')
     D   pType                       10A   CONST
     D   pCdType                     30A   CONST
     D   pValue                      30A
     D FmtCur          PR            17A
     D   pAmt                        17P 2 CONST
     D   pLen                        10I 0 CONST
     D   pDec                        10I 0 CONST OPTIONS(*NOPASS)
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
     C+   DECLARE LSTCSR CURSOR FOR
     C+   SELECT C.CUSTID, C.CUSTCD, C.CUSTNM, C.CURRCD, C.CREDLMT, C.STATUS,
     C+     COALESCE((SELECT SUM(I.BALANCE) FROM INVOICEHP I
     C+        WHERE I.CUSTID = C.CUSTID AND I.BALANCE > 0), 0),
     C+     COALESCE((SELECT SUM((L.ORDQTY - L.SHPQTY) * L.UNITPRC
     C+               * (1 + L.TAXRATE))
     C+        FROM SORDERLP L JOIN SORDERHP H ON H.SOID = L.SOID
     C+       WHERE H.CUSTID = C.CUSTID
     C+         AND H.STATUS IN ('OPEN', 'ALLOCATED', 'PARTIAL')
     C+         AND L.ORDQTY > L.SHPQTY), 0),
     C+     COALESCE((SELECT SUM(D.SHPQTY * L.UNITPRC * (1 + L.TAXRATE))
     C+        FROM SHIPMENTHP S JOIN SHIPMENTLP D ON D.SHPID = S.SHPID
     C+        JOIN SORDERLP L ON L.SOID = S.SOID AND L.LINENO = D.SOLNO
     C+        JOIN SORDERHP H ON H.SOID = S.SOID
     C+       WHERE H.CUSTID = C.CUSTID AND S.STATUS = 'SHIPPED'), 0),
     C+     COALESCE((SELECT COUNT(*) FROM INVOICEHP I
     C+        WHERE I.CUSTID = C.CUSTID AND I.BALANCE > 0), 0),
     C+     COALESCE((SELECT MAX(DAYS(CURRENT DATE) - DAYS(DATE(
     C+               TO_DATE(DIGITS(I.DUEDATE), 'YYYYMMDD'))))
     C+        FROM INVOICEHP I WHERE I.CUSTID = C.CUSTID
     C+         AND I.BALANCE > 0 AND I.DUEDATE > 0), 0),
     C+     COALESCE((SELECT MAX(P.PAYDATE) FROM PAYMENTP P
     C+        WHERE P.CUSTID = C.CUSTID AND P.STATUS <> 'CANCELLED'), 0)
     C+     FROM CUSTOMERP C WHERE C.CUSTCD LIKE :W#LIKE
     C+    ORDER BY C.CUSTCD FOR READ ONLY
     C/END-EXEC

     C                   EVAL      LFCUST = *BLANKS
     C                   EVAL      LMSG = *BLANKS
     C                   EXSR      @LOAD
     C                   DOW       NOT W#DONE
     C                   EVAL      *IN92 = *ON
     C                   EVAL      *IN91 = (W#CNT > 0)
     C                   EVAL      *IN94 = *ON
     C                   WRITE     LFOOT
     C                   EXFMT     LCTL
     C                   EVAL      LMSG = *BLANKS
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN04
     C                   EVAL      W#PVAL = LFCUST
     C                   CALLP     O2CPRMPT('CUST':' ':W#PVAL)
     C                   EVAL      LFCUST = W#PVAL
     C                   EXSR      @LOAD
     C                   ITER
     C                   ENDIF
     C                   EVAL      W#SEL = 0
     C                   IF        W#CNT > 0
     C                   READC     LSFL
     C                   DOW       NOT %EOF(CUSTINQD)
     C                   IF        LOPT = '5' AND W#SEL = 0
     C                   EVAL      W#SEL = LCUSTID
     C                   ENDIF
     C                   READC     LSFL
     C                   ENDDO
     C                   ENDIF
     C                   IF        W#SEL > 0
     C                   EXSR      @DETAIL
     C                   ENDIF
     C                   EXSR      @LOAD
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C                   RETURN

      *=====================================================================
      * @LOAD - 顧客の一覧を読み込む
      *=====================================================================
     C     @LOAD         BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     LCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#CNT = 0
     C                   EVAL      LFCUST = %XLATE('abcdefghijklmnopqrstuvwxyz':
     C                             'ABCDEFGHIJKLMNOPQRSTUVWXYZ':LFCUST)
     C                   EVAL      W#LIKE = %TRIM(LFCUST) + '%'
     C/EXEC SQL
     C+   OPEN LSTCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#CNT < 999
     C/EXEC SQL
     C+   FETCH LSTCSR
     C+     INTO :S#ID, :S#CD, :S#NM, :S#CUR, :S#LMT, :S#STS, :S#AR,
     C+          :S#UNSHP, :S#UNBIL, :S#CNT, :S#OVD, :S#LPAY
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EXSR      @CALC
     C                   EVAL      LOPT = ' '
     C                   EVAL      LCUSTID = S#ID
     C                   EVAL      LCUST = S#CD
     C                   EVAL      LNAME = DbcsTrunc(%TRIMR(S#NM):10)
     C                   EVAL      LCUR = S#CUR
     C                   IF        S#LMT <= 0
     C                   EVALR     LLMT = '無制限'
     C                   EVAL      LREM = *BLANKS
     C                   ELSE
     C                   EVAL      LLMT = FmtCur(S#LMT:14:0)
     C                   EVAL      LREM = FmtCur(W#REM:14:0)
     C                   ENDIF
     C                   EVAL      LUSED = FmtCur(W#USED:14:0)
     C                   IF        S#OVD > 0
     C                   EVALR     LOVD = %CHAR(S#OVD)
     C                   ELSE
     C                   EVAL      LOVD = *BLANKS
     C                   ENDIF
     C                   EVAL      *IN61 = (S#LMT > 0 AND W#REM < 0)
     C                   EVAL      *IN62 = (S#OVD > 0)
     C                   EVAL      W#CNT = W#CNT + 1
     C                   EVAL      W#RRN = W#CNT
     C                   WRITE     LSFL
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE LSTCSR
     C/END-EXEC
     C                   EVAL      *IN61 = *OFF
     C                   EVAL      *IN62 = *OFF
     C                   IF        W#CNT = 0 AND LMSG = *BLANKS
     C                   EVAL      LMSG = '該当する顧客がありません'
     C                   ENDIF
     C                   ENDSR

      * @CALC - 与信使用額と残り与信
     C     @CALC         BEGSR
     C                   EVAL      W#CUR = S#CUR
     C                   EVAL      W#USED = S#AR + S#UNSHP + S#UNBIL
     C                   EVAL      W#REM = S#LMT - W#USED
     C                   ENDSR

      *=====================================================================
      * @DETAIL - 顧客の詳細(F12で一覧へ戻る)
      *=====================================================================
     C     @DETAIL       BEGSR
     C/EXEC SQL
     C+   SELECT C.CUSTID, C.CUSTCD, C.CUSTNM, C.CURRCD, C.CREDLMT, C.STATUS,
     C+     COALESCE((SELECT SUM(I.BALANCE) FROM INVOICEHP I
     C+        WHERE I.CUSTID = C.CUSTID AND I.BALANCE > 0), 0),
     C+     COALESCE((SELECT SUM((L.ORDQTY - L.SHPQTY) * L.UNITPRC
     C+               * (1 + L.TAXRATE))
     C+        FROM SORDERLP L JOIN SORDERHP H ON H.SOID = L.SOID
     C+       WHERE H.CUSTID = C.CUSTID
     C+         AND H.STATUS IN ('OPEN', 'ALLOCATED', 'PARTIAL')
     C+         AND L.ORDQTY > L.SHPQTY), 0),
     C+     COALESCE((SELECT SUM(D.SHPQTY * L.UNITPRC * (1 + L.TAXRATE))
     C+        FROM SHIPMENTHP S JOIN SHIPMENTLP D ON D.SHPID = S.SHPID
     C+        JOIN SORDERLP L ON L.SOID = S.SOID AND L.LINENO = D.SOLNO
     C+        JOIN SORDERHP H ON H.SOID = S.SOID
     C+       WHERE H.CUSTID = C.CUSTID AND S.STATUS = 'SHIPPED'), 0),
     C+     COALESCE((SELECT COUNT(*) FROM INVOICEHP I
     C+        WHERE I.CUSTID = C.CUSTID AND I.BALANCE > 0), 0),
     C+     COALESCE((SELECT MAX(DAYS(CURRENT DATE) - DAYS(DATE(
     C+               TO_DATE(DIGITS(I.DUEDATE), 'YYYYMMDD'))))
     C+        FROM INVOICEHP I WHERE I.CUSTID = C.CUSTID
     C+         AND I.BALANCE > 0 AND I.DUEDATE > 0), 0),
     C+     COALESCE((SELECT MAX(P.PAYDATE) FROM PAYMENTP P
     C+        WHERE P.CUSTID = C.CUSTID AND P.STATUS <> 'CANCELLED'), 0)
     C+     INTO :S#ID, :S#CD, :S#NM, :S#CUR, :S#LMT, :S#STS, :S#AR,
     C+          :S#UNSHP, :S#UNBIL, :S#CNT, :S#OVD, :S#LPAY
     C+     FROM CUSTOMERP C WHERE C.CUSTID = :W#SEL
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      LMSG = '顧客が見つかりません'
     C                   LEAVESR
     C                   ENDIF
     C                   EXSR      @CALC
     C                   EVAL      DCUST = S#CD
     C                   EVAL      DNAME = DbcsTrunc(%TRIMR(S#NM):40)
     C                   EVAL      DCUR = S#CUR
     C                   IF        S#STS = 'ACTIVE'
     C                   EVAL      DSTS = '取引中'
     C                   ELSE
     C                   EVAL      DSTS = '取引停止'
     C                   ENDIF
     C                   IF        S#LMT <= 0
     C                   EVALR     DLMT = '無制限'
     C                   EVAL      DREM = *BLANKS
     C                   ELSE
     C                   EVAL      DLMT = FmtCur(S#LMT:17)
     C                   EVAL      DREM = FmtCur(W#REM:17)
     C                   ENDIF
     C                   EVAL      DAR = FmtCur(S#AR:17)
     C                   EVAL      DUNSHP = FmtCur(S#UNSHP:17)
     C                   EVAL      DUNBIL = FmtCur(S#UNBIL:17)
     C                   EVAL      DUSED = FmtCur(W#USED:17)
     C                   EVALR     DCNT = %CHAR(S#CNT)
     C                   IF        S#OVD > 0
     C                   EVALR     DOVD = %CHAR(S#OVD)
     C                   ELSE
     C                   EVALR     DOVD = '0'
     C                   ENDIF
     C                   EVAL      DLPAY = FmtDate(S#LPAY)
     C                   IF        S#LPAY = 0
     C                   EVAL      DLPAY = 'なし'
     C                   ENDIF
     C                   EVAL      *IN61 = (S#LMT > 0 AND W#REM < 0)
     C                   EVAL      *IN62 = (S#OVD > 0)
     C                   IF        *IN61
     C                   EVAL      DMSG = '与信限度額を超えています'
     C                   ELSE
     C                   EVAL      DMSG = *BLANKS
     C                   ENDIF
     C                   EXFMT     DTL
     C                   IF        *IN03
     C                   EVAL      W#DONE = *ON
     C                   ENDIF
     C                   ENDSR

      * FmtCur - 金額を右寄せの文字に(円は小数なし、外貨は小数2桁)
     P FmtCur          B
     D FmtCur          PI            17A
     D   pAmt                        17P 2 CONST
     D   pLen                        10I 0 CONST
     D   pDec                        10I 0 CONST OPTIONS(*NOPASS)
     D wTxt            S             24A   VARYING
     D wOut            S             17A
      * 円と、一覧(pDec=0)は小数なし
     C                   IF        W#CUR = 'JPY' OR W#CUR = *BLANKS OR
     C                             %PARMS >= 3
     C                   EVAL      wTxt = %TRIM(%EDITC(%DECH(pAmt:15:0):'J'))
     C                   ELSE
     C                   EVAL      wTxt = %TRIM(%EDITC(pAmt:'J'))
     C                   ENDIF
     C                   IF        %LEN(wTxt) > pLen
     C                   EVAL      wTxt = %SUBST('*****************':1:pLen)
     C                   ENDIF
     C                   EVAL      wOut = *BLANKS
     C                   EVAL      %SUBST(wOut:pLen - %LEN(wTxt) + 1:
     C                             %LEN(wTxt)) = wTxt
     C                   RETURN    wOut
     P FmtCur          E

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
