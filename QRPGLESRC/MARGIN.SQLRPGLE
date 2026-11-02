     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: MARGIN
      * 説明: 粗利分析(顧客別・商品別)
      * 作成: 修正#MRG 新規作成。販売管理メニューの23から呼ぶ
      *   期間内(請求日)の請求書の明細を集計する。取消済は除く
      *   売上 = 明細金額(税抜)。外貨は請求日以前で直近の為替レートで
      *          円換算(なければ最も古いレート。GLEXPORT と同じ)
      *   原価 = 数量 × 商品の標準原価(STDCOST)。赤伝の返品は
      *          数量がマイナスなので原価も減る(値引きは原価なし)
      *   粗利がマイナスの行は赤で表示。粗利の大きい順
      *=====================================================================
     FMARGIND   CF   E             WORKSTN SFILE(LSFL:W#RRN)

     D W#RRN           S              4S 0
     D W#CNT           S              4S 0
     D W#DT            S               D
     D W#P0            S             13P 0
     D W#RT            S              7P 1
     D W#TS            S             15P 2
     D W#TC            S             15P 2
     D H#KIND          S              1A
     D H#CODE          S             20A
     D H#NM            S            120A
     D H#SALES         S             15P 2
     D H#COST          S             15P 2
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
     D DBCSTRUNC       PR           256A   VARYING
     D   PSTR                       256A   VARYING CONST
     D   PMAX                        10I 0 CONST

     C/EXEC SQL
     C+   SET OPTION COMMIT = *NONE, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE MRGCSR CURSOR FOR
     C+   SELECT K, MAX(NM), SUM(SALES), SUM(COST) FROM (
     C+     SELECT CASE WHEN :H#KIND = 'C' THEN C.CUSTCD
     C+                 ELSE COALESCE(I.ITEMCD, ' ') END AS K,
     C+            CASE WHEN :H#KIND = 'C'
     C+                 THEN CAST(C.CUSTNM AS VARCHAR(120) FOR BIT DATA)
     C+                 ELSE CAST(COALESCE(I.ITEMDSC, ' ')
     C+                      AS VARCHAR(120) FOR BIT DATA) END AS NM,
     C+            L.LINEAMT *
     C+            CASE WHEN H.CURRCD IN ('JPY', ' ') THEN 1 ELSE COALESCE(
     C+             (SELECT F.RATE FROM FXRATEP F
     C+               WHERE F.FROMCUR = H.CURRCD AND F.TOCUR = 'JPY'
     C+                 AND F.RATEDATE <= H.INVDATE
     C+               ORDER BY F.RATEDATE DESC FETCH FIRST 1 ROW ONLY),
     C+             (SELECT F.RATE FROM FXRATEP F
     C+               WHERE F.FROMCUR = H.CURRCD AND F.TOCUR = 'JPY'
     C+               ORDER BY F.RATEDATE FETCH FIRST 1 ROW ONLY), 0) END
     C+            AS SALES,
     C+            L.QTY * COALESCE(I.STDCOST, 0) AS COST
     C+       FROM INVOICELP L
     C+       JOIN INVOICEHP H ON H.INVID = L.INVID
     C+       JOIN CUSTOMERP C ON C.CUSTID = H.CUSTID
     C+       LEFT JOIN ITEMP I ON I.ITEMID = L.ITEMID
     C+      WHERE H.STATUS <> 'CANCELLED'
     C+        AND H.INVDATE BETWEEN :LFROM AND :LTO) X
     C+    GROUP BY K
     C+    ORDER BY SUM(SALES) - SUM(COST) DESC
     C+    FOR READ ONLY
     C/END-EXEC

      * 初期値: 今月の1日から今日まで、顧客別
     C                   EVAL      W#DT = %DATE()
     C                   EVAL      LTO = %DEC(W#DT:*ISO)
     C                   EVAL      LFROM = %DEC(W#DT - %DAYS(%SUBDT(W#DT:*D)
     C                             - 1):*ISO)
     C                   EVAL      LKIND = 'C'
     C                   EXSR      @LOAD
     C                   DOW       *ON
     C                   EVAL      *IN91 = (W#CNT > 0)
     C                   EVAL      *IN94 = *ON
     C                   WRITE     LFOOT
     C                   EXFMT     LCTL
     C                   EVAL      LMSG = *BLANKS
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
     C                   EXSR      @LOAD
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C                   RETURN

      * @LOAD - 集計
     C     @LOAD         BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     LCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#CNT = 0
     C                   EVAL      W#TS = 0
     C                   EVAL      W#TC = 0
     C                   EVAL      *IN31 = *OFF
     C                   EVAL      *IN32 = *OFF
     C                   IF        LKIND = 'C'
     C                   EVAL      LH1 = '顧客'
     C                   ELSE
     C                   EVAL      LH1 = '商品'
     C                   ENDIF
     C     *ISO          TEST(DE)                LFROM
     C                   IF        %ERROR
     C                   EVAL      *IN31 = *ON
     C                   EVAL      LMSG = '期間の開始日が正しくありません'
     C                   EXSR      @TOTAL
     C                   LEAVESR
     C                   ENDIF
     C     *ISO          TEST(DE)                LTO
     C                   IF        %ERROR OR LTO < LFROM
     C                   EVAL      *IN32 = *ON
     C                   EVAL      LMSG = '期間の終了日が正しくありません'
     C                   EXSR      @TOTAL
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      H#KIND = LKIND
     C/EXEC SQL
     C+   OPEN MRGCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#CNT < 999
     C/EXEC SQL
     C+   FETCH MRGCSR INTO :H#CODE, :H#NM, :H#SALES, :H#COST
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      LCODE = H#CODE
     C                   EVAL      LNAME = DBCSTRUNC(%TRIMR(H#NM):20)
     C                   EVAL(H)   W#P0 = H#SALES
     C                   EVALR     LSALES = %TRIM(%EDITC(W#P0:'J'))
     C                   EVAL(H)   W#P0 = H#COST
     C                   EVALR     LCOST = %TRIM(%EDITC(W#P0:'J'))
     C                   EVAL(H)   W#P0 = H#SALES - H#COST
     C                   EVALR     LMRG = %TRIM(%EDITC(W#P0:'J'))
     C                   IF        H#SALES <> 0
     C                   EVAL(H)   W#RT = (H#SALES - H#COST) / H#SALES * 100
     C                   EVALR     LRATE = %TRIM(%EDITC(W#RT:'J'))
     C                   ELSE
     C                   EVAL      LRATE = *BLANKS
     C                   ENDIF
     C                   EVAL      *IN61 = (H#SALES - H#COST < 0)
     C                   EVAL      W#TS = W#TS + H#SALES
     C                   EVAL      W#TC = W#TC + H#COST
     C                   EVAL      W#CNT = W#CNT + 1
     C                   EVAL      W#RRN = W#CNT
     C                   WRITE     LSFL
     C                   ENDDO
     C                   IF        SQLCODE < 0
     C                   EVAL      LMSG = '集計エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   ENDIF
     C/EXEC SQL
     C+   CLOSE MRGCSR
     C/END-EXEC
     C                   EVAL      *IN61 = *OFF
     C                   IF        W#CNT = 0 AND LMSG = *BLANKS
     C                   EVAL      LMSG = 'この期間の請求書はありません'
     C                   ENDIF
     C                   EXSR      @TOTAL
     C                   ENDSR

      * @TOTAL - 合計
     C     @TOTAL        BEGSR
     C                   EVAL(H)   W#P0 = W#TS
     C                   EVALR     TSALES = %TRIM(%EDITC(W#P0:'J'))
     C                   EVAL(H)   W#P0 = W#TC
     C                   EVALR     TCOST = %TRIM(%EDITC(W#P0:'J'))
     C                   EVAL(H)   W#P0 = W#TS - W#TC
     C                   EVALR     TMRG = %TRIM(%EDITC(W#P0:'J'))
     C                   IF        W#TS <> 0
     C                   EVAL(H)   W#RT = (W#TS - W#TC) / W#TS * 100
     C                   EVALR     TRATE = %TRIM(%EDITC(W#RT:'J'))
     C                   ELSE
     C                   EVAL      TRATE = *BLANKS
     C                   ENDIF
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
