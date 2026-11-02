     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: SHPPRINT
      * 説明: ピッキングリスト・納品書の印刷
      * 作成: 修正#SHP 新規作成。出荷確定(CNFSHPENT)から呼ぶ
      *   CALL SHPPRINT PARM('P' 受注ID) ピッキングリスト(未出荷の明細)
      *   CALL SHPPRINT PARM('D' 受注ID) 納品書(その受注の最後の出荷)
      *   ピッキングリストは商品コード順(倉庫で拾いやすい順)
      *=====================================================================
     FSHPPRTF   O    E             PRINTER OFLIND(*IN90)

     D P#TYPE          S              1A
     D P#SOID          S             18P 0
     D W#P2            S             13P 2
     D W#P0            S             13P 0
     D W#TOT           S             15P 2
     D W#TQTY          S             15P 3
     D W#DESC          S            120A
      * SQL 用
     D H#SONUM         S             20A
     D H#REQ           S              8P 0
     D H#WHID          S              4P 0
     D H#WHNM          S             60A
     D H#CUSTNM        S             60A
     D H#ADR           S             60A
     D H#CURR          S              3A
     D H#SHPID         S             18P 0
     D H#SHPNUM        S             20A
     D H#SHPDT         S              8P 0
     D H#LINE          S              9P 0
     D H#ITEM          S             12A
     D H#IDESC         S             40A
     D H#QTY           S             15P 3
     D H#OH            S             15P 3
     D H#PRC           S             15P 4
     D H#RATE          S              5P 3
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
     D DBCSTRUNC       PR           256A   VARYING
     D   PSTR                       256A   VARYING CONST
     D   PMAX                        10I 0 CONST

     C     *ENTRY        PLIST
     C                   PARM                    P#TYPE
     C                   PARM                    P#SOID

     C/EXEC SQL
     C+   SET OPTION COMMIT = *NONE, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE PCKCSR CURSOR FOR
     C+   SELECT L.LINENO, I.ITEMCD, I.ITEMDSC, L.ORDQTY - L.SHPQTY,
     C+          COALESCE(V.QTYOH, 0)
     C+     FROM SORDERLP L
     C+     JOIN ITEMP I ON I.ITEMID = L.ITEMID
     C+     LEFT JOIN INVENTORYP V ON V.WHID = :H#WHID
     C+                           AND V.ITEMID = L.ITEMID
     C+    WHERE L.SOID = :P#SOID AND L.ORDQTY > L.SHPQTY
     C+    ORDER BY I.ITEMCD, L.LINENO
     C+    FOR READ ONLY
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE DLVCSR CURSOR FOR
     C+   SELECT D.LINENO, I.ITEMCD, I.ITEMDSC, D.SHPQTY, L.UNITPRC,
     C+          L.TAXRATE
     C+     FROM SHIPMENTLP D
     C+     JOIN SORDERLP L ON L.SOID = :P#SOID AND L.LINENO = D.SOLNO
     C+     JOIN ITEMP I ON I.ITEMID = D.ITEMID
     C+    WHERE D.SHPID = :H#SHPID
     C+    ORDER BY D.LINENO
     C+    FOR READ ONLY
     C/END-EXEC

     C                   IF        P#TYPE = 'P'
     C                   EXSR      @PICK
     C                   ELSE
     C                   EXSR      @DELIV
     C                   ENDIF
     C                   EVAL      *INLR = *ON
     C                   RETURN

      *=====================================================================
      * @PICK - ピッキングリスト
      *=====================================================================
     C     @PICK         BEGSR
     C/EXEC SQL
     C+   SELECT H.SONUM, H.REQDATE, H.WHID, COALESCE(W.WHNM, ' '),
     C+          C.CUSTNM
     C+     INTO :H#SONUM, :H#REQ, :H#WHID, :H#WHNM, :H#CUSTNM
     C+     FROM SORDERHP H
     C+     JOIN CUSTOMERP C ON C.CUSTID = H.CUSTID
     C+     LEFT JOIN WAREHOUSEP W ON W.WHID = H.WHID
     C+    WHERE H.SOID = :P#SOID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      KPRTDT = %CHAR(%DATE():*ISO)
     C                   EVAL      KSONUM = H#SONUM
     C                   MONITOR
     C                   EVAL      KREQDT = %CHAR(%DATE(H#REQ:*ISO):*ISO)
     C                   ON-ERROR
     C                   EVAL      KREQDT = *BLANKS
     C                   ENDMON
     C                   EVAL      KWHNM = DBCSTRUNC(%TRIMR(H#WHNM):40)
     C                   EVAL      KCUSTNM = H#CUSTNM
     C                   WRITE     KHEAD
     C                   EVAL      W#TQTY = 0
     C/EXEC SQL
     C+   OPEN PCKCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0
     C/EXEC SQL
     C+   FETCH PCKCSR INTO :H#LINE, :H#ITEM, :H#IDESC, :H#QTY, :H#OH
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN90
     C                   WRITE     KHEAD
     C                   EVAL      *IN90 = *OFF
     C                   ENDIF
     C                   EVAL      KLINE = H#LINE
     C                   EVAL      KITEM = H#ITEM
     C                   EVAL      KDESC = H#IDESC
     C                   EVAL(H)   KQTY = H#QTY
     C                   EVAL(H)   KOH = H#OH
     C                   EVAL      W#TQTY = W#TQTY + H#QTY
     C                   WRITE     KDETAIL
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE PCKCSR
     C/END-EXEC
     C                   EVAL(H)   KTQTY = W#TQTY
     C                   WRITE     KTOTAL
     C                   ENDSR

      *=====================================================================
      * @DELIV - 納品書(受注の最後の出荷)
      *=====================================================================
     C     @DELIV        BEGSR
     C/EXEC SQL
     C+   SELECT S.SHPID, S.SHPNUM, S.SHPDATE, H.SONUM, H.CURRCD,
     C+          C.CUSTNM, COALESCE(NULLIF(C.SHIPADR, ' '), C.BILLADR)
     C+     INTO :H#SHPID, :H#SHPNUM, :H#SHPDT, :H#SONUM, :H#CURR,
     C+          :H#CUSTNM, :H#ADR
     C+     FROM SHIPMENTHP S
     C+     JOIN SORDERHP H ON H.SOID = S.SOID
     C+     JOIN CUSTOMERP C ON C.CUSTID = H.CUSTID
     C+    WHERE S.SOID = :P#SOID AND S.STATUS <> 'CANCELLED'
     C+    ORDER BY S.SHPID DESC FETCH FIRST 1 ROW ONLY
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#DESC = *BLANKS
     C/EXEC SQL
     C+   SELECT CAST(DESC AS VARCHAR(120) FOR BIT DATA) INTO :W#DESC
     C+     FROM CODEMSTRP WHERE CODETYPE = 'COMPANY' AND CODEVAL = 'NAME'
     C/END-EXEC
     C                   EVAL      DCOMPNM = DBCSTRUNC(%TRIMR(W#DESC):40)
     C                   EVAL      DCUSTNM = H#CUSTNM
     C                   EVAL      DSHPADR = H#ADR
     C                   EVAL      DSHPNUM = H#SHPNUM
     C                   EVAL      DSONUM = H#SONUM
     C                   EVAL      DSHPDT = %CHAR(%DATE(H#SHPDT:*ISO):*ISO)
     C                   EVAL      DCURR = H#CURR
     C                   WRITE     DHEAD
     C                   EVAL      W#TOT = 0
     C/EXEC SQL
     C+   OPEN DLVCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0
     C/EXEC SQL
     C+   FETCH DLVCSR INTO :H#LINE, :H#ITEM, :H#IDESC, :H#QTY, :H#PRC,
     C+                     :H#RATE
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN90
     C                   WRITE     DHEAD
     C                   EVAL      *IN90 = *OFF
     C                   ENDIF
     C                   EVAL      DLINE = H#LINE
     C                   EVAL      DITEM = H#ITEM
     C                   EVAL      DDESC = H#IDESC
     C                   EVAL(H)   DQTY = H#QTY
     C                   EVAL(H)   W#P2 = H#QTY * H#PRC
     C                   EVAL      W#TOT = W#TOT + W#P2
     C                   EVAL      *IN51 = (H#RATE = 0.080)
     C                   IF        H#CURR = 'JPY'
     C                   EVAL(H)   W#P0 = H#PRC
     C                   EVALR     DPRC = %TRIM(%EDITC(W#P0:'J'))
     C                   EVAL(H)   W#P0 = W#P2
     C                   EVALR     DAMT = %TRIM(%EDITC(W#P0:'J'))
     C                   ELSE
     C                   EVAL(H)   W#P2 = H#PRC
     C                   EVALR     DPRC = %TRIM(%EDITC(W#P2:'J'))
     C                   EVAL(H)   W#P2 = H#QTY * H#PRC
     C                   EVALR     DAMT = %TRIM(%EDITC(W#P2:'J'))
     C                   ENDIF
     C                   WRITE     DDETAIL
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE DLVCSR
     C/END-EXEC
     C                   EVAL      *IN51 = *OFF
     C                   IF        H#CURR = 'JPY'
     C                   EVAL(H)   W#P0 = W#TOT
     C                   EVALR     DTOT = %TRIM(%EDITC(W#P0:'J'))
     C                   ELSE
     C                   EVAL      W#P2 = W#TOT
     C                   EVALR     DTOT = %TRIM(%EDITC(W#P2:'J'))
     C                   ENDIF
     C                   WRITE     DTOTAL
     C                   ENDSR

      *=====================================================================
      * DBCSTRUNC - 全角(SO/SI)を壊さずに指定バイト数以内に切り詰める
      *=====================================================================
     P DBCSTRUNC       B
     D DBCSTRUNC       PI           256A   VARYING
     D   PSTR                       256A   VARYING CONST
     D   PMAX                        10I 0 CONST
     D WOUT            S            256A   VARYING
     D WCH             S              1A
     D WI              S             10I 0
     D WLEN            S             10I 0
     D WDBCS           S               N
     C                   EVAL      WOUT = ''
     C                   EVAL      WDBCS = *OFF
     C                   EVAL      WLEN = %LEN(%TRIMR(PSTR))
     C                   EVAL      WI = 1
     C                   DOW       WI <= WLEN
     C                   EVAL      WCH = %SUBST(PSTR:WI:1)
     C                   SELECT
     C                   WHEN      WCH = W#SO
     C                   IF        %LEN(WOUT) + 3 > PMAX
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      WOUT = WOUT + WCH
     C                   EVAL      WDBCS = *ON
     C                   EVAL      WI = WI + 1
     C                   WHEN      WCH = W#SI
     C                   EVAL      WOUT = WOUT + WCH
     C                   EVAL      WDBCS = *OFF
     C                   EVAL      WI = WI + 1
     C                   WHEN      WDBCS
     C                   IF        %LEN(WOUT) + 3 > PMAX OR WI + 1 > WLEN
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      WOUT = WOUT + %SUBST(PSTR:WI:2)
     C                   EVAL      WI = WI + 2
     C                   OTHER
     C                   IF        %LEN(WOUT) + 1 > PMAX
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      WOUT = WOUT + WCH
     C                   EVAL      WI = WI + 1
     C                   ENDSL
     C                   ENDDO
     C                   IF        WDBCS
     C                   EVAL      WOUT = WOUT + W#SI
     C                   ENDIF
     C                   RETURN    WOUT
     P DBCSTRUNC       E
