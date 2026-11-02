     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: INVPAYH
      * 説明: 請求書の入金履歴(どの入金でいくら消し込んだか)
      * 作成: 修正#PYH 新規作成。請求書照会(INVINQ)のF7から呼ぶ
      *   パラメーター: 請求番号
      *   入金取消で戻した消込はマイナスの金額・区分「取消」で出る
      *   金額は円なら小数なし、外貨なら小数2桁
      *=====================================================================
     FINVPAYHD  CF   E             WORKSTN SFILE(HSFL:W#RRN)

     D P#INVNUM        S             20A
     D W#INVNUM        S             20A
     D W#RRN           S              4S 0
     D W#CNT           S              4S 0
     D W#INVID         S             18P 0
     D W#CUR           S              3A
     D W#TOT           S             15P 2
     D W#PAID          S             15P 2
     D W#BAL           S             15P 2
     D S#APDT          S              8P 0
     D S#PAYNUM        S             20A
     D S#PAYDT         S              8P 0
     D S#METH          S             20A
     D S#AMT           S             15P 2
     D S#STS           S             12A
     D FmtAmt          PR            15A
     D   pAmt                        15P 2 CONST
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
     D DbcsTrunc       PR           256A   VARYING
     D   pStr                       256A   VARYING CONST
     D   pMax                        10I 0 CONST
     D Fit9            PR             9P 0
     D   pAmt                        15P 2 CONST
     D FmtDate         PR            10A
     D   pDate                        8P 0 CONST

     C     *ENTRY        PLIST
     C                   PARM                    P#INVNUM

     C/EXEC SQL
     C+   SET OPTION COMMIT = *NONE, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE HCSR CURSOR FOR
     C+   SELECT A.APPLYDATE, P.PAYNUM, P.PAYDATE, P.PAYMETH, A.APPLYAMT,
     C+          P.STATUS
     C+     FROM PAYAPPLYP A JOIN PAYMENTP P ON P.PAYID = A.PAYID
     C+    WHERE A.INVID = :W#INVID
     C+    ORDER BY A.APPLYID
     C+    FOR READ ONLY
     C/END-EXEC

     C                   EVAL      W#INVNUM = P#INVNUM
     C/EXEC SQL
     C+   SELECT INVID, CURRCD, TOTAMT, PAIDAMT, BALANCE
     C+     INTO :W#INVID, :W#CUR, :W#TOT, :W#PAID, :W#BAL
     C+     FROM INVOICEHP WHERE INVNUM = :W#INVNUM
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF
     C                   EVAL      HINV = W#INVNUM
     C                   EVAL      HCUR = W#CUR
     C                   EVAL      HTOT = FmtAmt(W#TOT)
     C                   EVAL      HPAID = FmtAmt(W#PAID)
     C                   EVAL      HBAL = FmtAmt(W#BAL)

     C                   EVAL      *IN93 = *ON
     C                   WRITE     HCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#CNT = 0
     C/EXEC SQL
     C+   OPEN HCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#CNT < 999
     C/EXEC SQL
     C+   FETCH HCSR INTO :S#APDT, :S#PAYNUM, :S#PAYDT, :S#METH,
     C+     :S#AMT, :S#STS
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      HAPDT = FmtDate(S#APDT)
     C                   EVAL      HPAYNUM = S#PAYNUM
     C                   EVAL      HPAYDT = FmtDate(S#PAYDT)
     C                   EVAL      HMETH = S#METH
     C                   EVAL      HAMT = FmtAmt(S#AMT)
      * マイナスの消込は入金取消で戻したもの
     C                   IF        S#AMT < 0
     C                   EVAL      HKIND = '取消'
     C                   ELSE
     C                   EVAL      HKIND = '消込'
     C                   ENDIF
     C                   EVAL      W#CNT = W#CNT + 1
     C                   EVAL      W#RRN = W#CNT
     C                   WRITE     HSFL
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE HCSR
     C/END-EXEC
     C                   IF        W#CNT = 0
     C                   EVAL      HMSG = '入金の消込はまだありません'
     C                   ELSE
     C                   EVAL      HMSG = *BLANKS
     C                   ENDIF
     C                   EVAL      *IN92 = *ON
     C                   EVAL      *IN91 = (W#CNT > 0)
     C                   EVAL      *IN94 = *ON
     C                   WRITE     HFOOT
     C                   EXFMT     HCTL
     C                   EVAL      *INLR = *ON
     C                   RETURN

      * FmtAmt - 金額を右寄せの文字に(円は小数なし、外貨は小数2桁)
     P FmtAmt          B
     D FmtAmt          PI            15A
     D   pAmt                        15P 2 CONST
     D wOut            S             15A
     C                   IF        W#CUR = 'JPY' OR W#CUR = *BLANKS
      * 修正#OVF 桁に収まらない金額・数量は*で表示(異常終了していた)
     C                   MONITOR
     C                   EVALR     wOut = %EDITC(%DECH(pAmt:11:0):'J')
     C                   ON-ERROR
     C                   EVAL      wOut = *ALL'*'
     C                   ENDMON
     C                   ELSE
     C                   MONITOR
     C                   EVALR     wOut = %EDITC(%DECH(pAmt:11:2):'J')
     C                   ON-ERROR
     C                   EVAL      wOut = *ALL'*'
     C                   ENDMON
     C                   ENDIF
     C                   RETURN    wOut
     P FmtAmt          E

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
