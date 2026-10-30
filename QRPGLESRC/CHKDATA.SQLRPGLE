     H DFTACTGRP(*NO) ACTGRP(*NEW)
      *=====================================================================
      * プログラム名: CHKDATA
      * 説明: データ整合性チェック(データは変更しない)
      * 作成: 修正#CHK 新規作成。メニューの15番から呼ぶ
      *    1 請求:入金済と消込の合計
      *    2 請求:残高と合計-入金済
      *    3 請求:取消済なのに残高
      *    4 請求:合計と小計+税
      *    5 請求:状態と入金・残高
      *    6 受注:出荷済数と出荷合計
      *    7 在庫:有効在庫の計算
      *    8 在庫:引当済と受注残
      *    9 入金:入金額と消込合計
      *   10 受注:合計と明細合計
      *   値=いまの値、あるべき値=ほかのファイルから計算した値
      *=====================================================================
     FCHKDATAD  CF   E             WORKSTN SFILE(RSFL:W#RRN)

     D W#RRN           S              4S 0
     D W#CNT           S              9P 0
     D S#N             S             10I 0
     D S#K             S             30A
     D S#A             S             20A
     D S#B             S             20A
     D W#NAMES         S             28A   DIM(10) CTDATA PERRCD(1)

     C/EXEC SQL
     C+   SET OPTION COMMIT = *NONE, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE CHKCSR CURSOR FOR
     C+   SELECT N, VARCHAR(K, 30), VARCHAR(A, 20), VARCHAR(B, 20) FROM (
     C+   SELECT 1 AS N, H.INVNUM AS K, CHAR(H.PAIDAMT) AS A,
     C+          CHAR(COALESCE(SUM(P.APPLYAMT), 0)) AS B
     C+     FROM INVOICEHP H LEFT JOIN PAYAPPLYP P ON P.INVID = H.INVID
     C+    GROUP BY H.INVNUM, H.PAIDAMT
     C+   HAVING H.PAIDAMT <> COALESCE(SUM(P.APPLYAMT), 0)
     C+   UNION ALL
     C+   SELECT 2, INVNUM, CHAR(BALANCE), CHAR(TOTAMT - PAIDAMT)
     C+     FROM INVOICEHP
     C+    WHERE STATUS <> 'CANCELLED' AND BALANCE <> TOTAMT - PAIDAMT
     C+   UNION ALL
     C+   SELECT 3, INVNUM, CHAR(BALANCE), '0' FROM INVOICEHP
     C+    WHERE STATUS = 'CANCELLED' AND BALANCE <> 0
     C+   UNION ALL
     C+   SELECT 4, INVNUM, CHAR(TOTAMT), CHAR(SUBTOT + TAXAMT)
     C+     FROM INVOICEHP WHERE TOTAMT <> SUBTOT + TAXAMT
     C+   UNION ALL
     C+   SELECT 5, INVNUM, STATUS, CHAR(BALANCE) FROM INVOICEHP
     C+    WHERE (STATUS = 'PAID' AND BALANCE <> 0)
     C+       OR (STATUS = 'OPEN' AND PAIDAMT <> 0)
     C+       OR (STATUS = 'PARTIAL' AND (PAIDAMT = 0 OR BALANCE = 0))
     C+   UNION ALL
     C+   SELECT 6, TRIM(H.SONUM) || '/' || TRIM(CHAR(L.LINENO)),
     C+          CHAR(L.SHPQTY), CHAR(
     C+   COALESCE((SELECT SUM(D.SHPQTY) FROM SHIPMENTLP D
     C+     JOIN SHIPMENTHP S ON S.SHPID = D.SHPID
     C+     WHERE S.SOID = L.SOID AND D.SOLNO = L.LINENO
     C+       AND S.STATUS <> 'CANCELLED'), 0)
     C+   )
     C+     FROM SORDERLP L JOIN SORDERHP H ON H.SOID = L.SOID
     C+    WHERE L.SHPQTY <> 
     C+   COALESCE((SELECT SUM(D.SHPQTY) FROM SHIPMENTLP D
     C+     JOIN SHIPMENTHP S ON S.SHPID = D.SHPID
     C+     WHERE S.SOID = L.SOID AND D.SOLNO = L.LINENO
     C+       AND S.STATUS <> 'CANCELLED'), 0)
     C+   UNION ALL
     C+   SELECT 7, TRIM(W.WHCD) || '/' || TRIM(I.ITEMCD),
     C+          CHAR(V.QTYAVL), CHAR(V.QTYOH - V.QTYALC)
     C+     FROM INVENTORYP V JOIN WAREHOUSEP W ON W.WHID = V.WHID
     C+     JOIN ITEMP I ON I.ITEMID = V.ITEMID
     C+    WHERE V.QTYAVL <> V.QTYOH - V.QTYALC
     C+   UNION ALL
     C+   SELECT 8, TRIM(W.WHCD) || '/' || TRIM(I.ITEMCD),
     C+          CHAR(V.QTYALC), CHAR(
     C+   COALESCE((SELECT SUM(L.ORDQTY - L.SHPQTY) FROM SORDERLP L
     C+     JOIN SORDERHP H ON H.SOID = L.SOID
     C+     WHERE H.WHID = V.WHID AND L.ITEMID = V.ITEMID
     C+       AND H.STATUS <> 'CANCELLED'), 0)
     C+   )
     C+     FROM INVENTORYP V JOIN WAREHOUSEP W ON W.WHID = V.WHID
     C+     JOIN ITEMP I ON I.ITEMID = V.ITEMID
     C+    WHERE V.QTYALC <> 
     C+   COALESCE((SELECT SUM(L.ORDQTY - L.SHPQTY) FROM SORDERLP L
     C+     JOIN SORDERHP H ON H.SOID = L.SOID
     C+     WHERE H.WHID = V.WHID AND L.ITEMID = V.ITEMID
     C+       AND H.STATUS <> 'CANCELLED'), 0)
     C+   UNION ALL
     C+   SELECT 9, P.PAYNUM, CHAR(P.AMOUNT),
     C+          CHAR(COALESCE(SUM(A.APPLYAMT), 0))
     C+     FROM PAYMENTP P LEFT JOIN PAYAPPLYP A ON A.PAYID = P.PAYID
     C+    WHERE P.STATUS <> 'CANCELLED'
     C+    GROUP BY P.PAYNUM, P.AMOUNT
     C+   HAVING P.AMOUNT <> COALESCE(SUM(A.APPLYAMT), 0)
     C+   UNION ALL
     C+   SELECT 10, H.SONUM, CHAR(H.TOTAMT), CHAR(SUM(L.LINEAMT))
     C+     FROM SORDERHP H JOIN SORDERLP L ON L.SOID = H.SOID
     C+    GROUP BY H.SONUM, H.TOTAMT
     C+   HAVING H.TOTAMT <> SUM(L.LINEAMT)
     C+   ) X ORDER BY N, 2
     C+    FOR READ ONLY
     C/END-EXEC

     C                   EXSR      @RUN
     C                   DOW       *ON
     C                   EVAL      *IN92 = *ON
     C                   EVAL      *IN91 = (W#CNT > 0)
     C                   EVAL      *IN94 = *ON
     C                   WRITE     RFOOT
     C                   EXFMT     RCTL
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN05
     C                   EXSR      @RUN
     C                   ENDIF
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C                   RETURN

      * @RUN - チェックを実行して一覧に出す
     C     @RUN          BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     RCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#CNT = 0
     C/EXEC SQL
     C+   OPEN CHKCSR
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      RMSG = 'チェックのSQLエラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   LEAVESR
     C                   ENDIF
     C                   DOW       W#CNT < 999
     C/EXEC SQL
     C+   FETCH CHKCSR INTO :S#N, :S#K, :S#A, :S#B
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#CNT = W#CNT + 1
     C                   IF        S#N >= 1 AND S#N <= 10
     C                   EVAL      RNAME = W#NAMES(S#N)
     C                   ELSE
     C                   EVAL      RNAME = %CHAR(S#N)
     C                   ENDIF
     C                   EVAL      RKEY = S#K
     C                   EVALR     RVAL = %TRIM(S#A)
     C                   EVALR     RCOR = %TRIM(S#B)
     C                   EVAL      W#RRN = W#CNT
     C                   WRITE     RSFL
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE CHKCSR
     C/END-EXEC
     C                   EVALR     RCNT = %CHAR(W#CNT)
     C                   IF        W#CNT = 0
     C                   EVAL      RMSG = '不一致はありません'
     C                   ELSE
     C                   EVAL      RMSG = '不一致があります。原因を確認'
     C                   ENDIF
     C                   ENDSR
**CTDATA W#NAMES
請求:入金済と消込の合計
請求:残高と合計-入金済
請求:取消済なのに残高
請求:合計と小計+税
請求:状態と入金・残高
受注:出荷済数と出荷合計
在庫:有効在庫の計算
在庫:引当済と受注残
入金:入金額と消込合計
受注:合計と明細合計
