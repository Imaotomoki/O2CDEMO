     H DFTACTGRP(*NO) ACTGRP(*NEW)
      *=====================================================================
      * プログラム名: SHPDUE
      * 説明: 出荷予定(未出荷の受注明細を希望納期の早い順に表示)
      * 作成: 修正#DUE 新規作成。メニューの16番から呼ぶ
      *   対象: OPEN/ALLOCATED/PARTIAL の受注で残数>0の明細
      *   納期が過ぎている行は希望納期と遅れ日を赤、
      *   倉庫の現在庫が残数より少ない行は現在庫を赤で表示
      *=====================================================================
     FSHPDUED   CF   E             WORKSTN SFILE(DSFL:W#RRN)

     D W#RRN           S              4S 0
     D W#CNT           S              9P 0
     D W#LATE          S              9P 0
     D W#SHORT         S              9P 0
     D W#SQLCD         S             10I 0
     D W#CNTC          S             10A
     D W#PVAL          S             30A
     D W#FWH           S             10A
     D W#TODAY         S               D
     D W#DAYS          S             10I 0
     D S#REQ           S              8P 0
     D S#SONUM         S             20A
     D S#CUST          S             20A
     D S#ITEM          S             12A
     D S#REM           S             15P 3
     D S#OH            S             15P 3
     D O2CPRMPT        PR                  EXTPGM('O2CPRMPT')
     D   pType                       10A   CONST
     D   pCdType                     30A   CONST
     D   pValue                      30A
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
     D DbcsTrunc       PR           256A   VARYING
     D   pStr                       256A   VARYING CONST
     D   pMax                        10I 0 CONST
     D Fit9            PR             9P 0
     D   pAmt                        15P 2 CONST
     D FmtQ8           PR             8A
     D   pQty                        15P 3 CONST
     D FmtDate         PR            10A
     D   pDate                        8P 0 CONST

     C/EXEC SQL
     C+   SET OPTION COMMIT = *NONE, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE DUECSR CURSOR FOR
     C+   SELECT H.REQDATE, H.SONUM, C.CUSTCD, I.ITEMCD,
     C+          L.ORDQTY - L.SHPQTY, COALESCE(V.QTYOH, 0)
     C+     FROM SORDERLP L JOIN SORDERHP H ON H.SOID = L.SOID
     C+     JOIN CUSTOMERP C ON C.CUSTID = H.CUSTID
     C+     JOIN ITEMP I ON I.ITEMID = L.ITEMID
     C+     JOIN WAREHOUSEP W ON W.WHID = H.WHID
     C+     LEFT JOIN INVENTORYP V ON V.WHID = H.WHID
     C+          AND V.ITEMID = L.ITEMID
     C+    WHERE H.STATUS IN ('OPEN', 'ALLOCATED', 'PARTIAL')
     C+      AND L.ORDQTY > L.SHPQTY
     C+      AND (:W#FWH = ' ' OR W.WHCD = :W#FWH)
     C+    ORDER BY H.REQDATE, H.SONUM, L.LINENO
     C+    FOR READ ONLY
     C/END-EXEC

     C                   EVAL      DFWH = *BLANKS
     C                   EXSR      @LOAD
     C                   DOW       *ON
     C                   EVAL      *IN92 = *ON
     C                   EVAL      *IN91 = (W#CNT > 0)
     C                   EVAL      *IN94 = *ON
     C                   WRITE     DFOOT
     C                   EXFMT     DCTL
     C                   EVAL      DMSG = *BLANKS
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN04
     C                   EVAL      W#PVAL = DFWH
     C                   CALLP     O2CPRMPT('WH':' ':W#PVAL)
     C                   EVAL      DFWH = W#PVAL
     C                   ENDIF
     C                   EXSR      @LOAD
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C                   RETURN

      * @LOAD - 未出荷の明細を読み込む
     C     @LOAD         BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     DCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#CNT = 0
     C                   EVAL      W#LATE = 0
     C                   EVAL      W#SHORT = 0
     C                   EVAL      W#TODAY = %DATE()
     C                   EVAL      DFWH = %XLATE('abcdefghijklmnopqrstuvwxyz':
     C                             'ABCDEFGHIJKLMNOPQRSTUVWXYZ':DFWH)
     C                   EVAL      W#FWH = DFWH
     C/EXEC SQL
     C+   OPEN DUECSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#CNT < 999
     C/EXEC SQL
     C+   FETCH DUECSR INTO :S#REQ, :S#SONUM, :S#CUST, :S#ITEM, :S#REM,
     C+     :S#OH
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      DREQ = FmtDate(S#REQ)
     C                   EVAL      DSONUM = S#SONUM
     C                   EVAL      DCUST = S#CUST
     C                   EVAL      DITEM = S#ITEM
     C                   EVAL      DREM = FmtQ8(S#REM)
     C                   EVAL      DOH = FmtQ8(S#OH)
      * 遅れ日(希望納期を過ぎた日数)
     C                   EVAL      W#DAYS = 0
     C                   MONITOR
     C                   EVAL      W#DAYS = %DIFF(W#TODAY:%DATE(S#REQ:*ISO):
     C                             *DAYS)
     C                   ON-ERROR
     C                   EVAL      W#DAYS = 0
     C                   ENDMON
     C                   IF        W#DAYS > 0
     C                   EVALR     DLATE = %CHAR(W#DAYS)
     C                   EVAL      *IN61 = *ON
     C                   EVAL      W#LATE = W#LATE + 1
     C                   ELSE
     C                   EVAL      DLATE = *BLANKS
     C                   EVAL      *IN61 = *OFF
     C                   ENDIF
     C                   EVAL      *IN62 = (S#OH < S#REM)
     C                   IF        *IN62
     C                   EVAL      W#SHORT = W#SHORT + 1
     C                   ENDIF
     C                   EVAL      W#CNT = W#CNT + 1
     C                   EVAL      W#RRN = W#CNT
     C                   WRITE     DSFL
     C                   ENDDO
      * 修正#SUM 999行で打ち切ったら件数に+、SQLエラーは表示
     C                   EVAL      W#SQLCD = SQLCODE
     C/EXEC SQL
     C+   CLOSE DUECSR
     C/END-EXEC
     C                   EVAL      *IN61 = *OFF
     C                   EVAL      *IN62 = *OFF
     C                   EVAL      W#CNTC = %CHAR(W#CNT)
     C                   IF        W#SQLCD = 0
     C                   EVAL      W#CNTC = %TRIM(W#CNTC) + '+'
     C                   ENDIF
     C                   EVAL      DSUM = '未出荷 ' + %TRIM(W#CNTC) +
     C                             ' 行  納期遅れ ' + %CHAR(W#LATE) +
     C                             ' 行・在庫不足 ' + %CHAR(W#SHORT) + ' 行'
     C                   IF        W#SQLCD < 0
     C                   EVAL      DMSG = '読み込みエラー SQLCODE=' +
     C                             %CHAR(W#SQLCD)
     C                   ELSEIF    W#CNT = 0
     C                   EVAL      DMSG = '未出荷の受注はありません'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * FmtQ8 - 数量を右寄せ8桁に(修正#Q8 100万以上は左が欠けていた)
      *   7桁まではカンマ付き(100万以上はカンマなし)、超えたら*
      *=====================================================================
     P FmtQ8           B
     D FmtQ8           PI             8A
     D   pQty                        15P 3 CONST
     D wOut            S              8A
     D wQty            S             15P 0
     C                   EVAL(H)   wQty = pQty
     C                   IF        wQty > 9999999 OR wQty < -999999
     C                   RETURN    *ALL'*'
     C                   ENDIF
     C                   IF        wQty > 999999 OR wQty < -99999
     C                   EVALR     wOut = %EDITC(%DEC(wQty:7:0):'3')
     C                   ELSE
     C                   EVALR     wOut = %EDITC(%DEC(wQty:7:0):'1')
     C                   ENDIF
     C                   RETURN    wOut
     P FmtQ8           E

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
