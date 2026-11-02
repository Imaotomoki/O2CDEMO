     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: NIGHTBAT
      * 説明: 夜間バッチの点検レポート(データは変更しない)
      * 作成: 修正#BAT 新規作成。NIGHTCL から呼ぶ(画面なし)
      *   次の件数・金額を数えて QSYSPRT に印刷する
      *     支払期限を過ぎた請求書(円の残高)・発注点を下回る商品
      *     入荷予定を過ぎた発注・有効期限を過ぎた見積(未受注)
      *     前受金(未充当)・未処理の入金データ
      *     整合性(請求の残高、入金と消込)の不一致
      *   注意が必要な項目がある行には「要確認」を付ける
      *=====================================================================
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D W#TODAY         S              8P 0
     D W#N             S             10I 0
     D W#AMT           S             15P 2
     D P#DATE          S             10A
     D P#TIME          S              8A
     D P#ITEM          S             40A
     D P#CNT           S             10A
     D P#AMT           S             17A
     D P#MARK          S             10A
     D W#P0            S             13P 0

     C/EXEC SQL
     C+   SET OPTION COMMIT = *NONE, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C                   EVAL      W#TODAY = %DEC(%DATE():*ISO)
     C                   EVAL      P#DATE = %CHAR(%DATE():*ISO)
     C                   EVAL      P#TIME = %CHAR(%TIME():*HMS)
     C                   EXCEPT    HEAD
      * 支払期限を過ぎた請求書
     C/EXEC SQL
     C+   SELECT COUNT(*), COALESCE(SUM(CASE WHEN CURRCD = 'JPY'
     C+          THEN BALANCE ELSE 0 END), 0) INTO :W#N, :W#AMT
     C+     FROM INVOICEHP
     C+    WHERE BALANCE > 0 AND STATUS <> 'CANCELLED'
     C+      AND DUEDATE < :W#TODAY
     C/END-EXEC
     C                   EVAL      P#ITEM = '期限超過の請求書(円の残高)'
     C                   EXSR      @LINE
      * 発注点を下回る商品・倉庫
     C/EXEC SQL
     C+   SELECT COUNT(*) INTO :W#N
     C+     FROM ITEMP I JOIN INVENTORYP V ON V.ITEMID = I.ITEMID
     C+    WHERE I.STATUS = 'ACTIVE' AND I.REORDPT > 0
     C+      AND V.QTYAVL + COALESCE((SELECT SUM(L.ORDQTY - L.RCVQTY)
     C+            FROM POLINEP L JOIN POHDRP H ON H.POID = L.POID
     C+           WHERE H.STATUS IN ('OPEN', 'PARTIAL')
     C+             AND H.WHID = V.WHID AND L.ITEMID = I.ITEMID), 0)
     C+          < I.REORDPT
     C/END-EXEC
     C                   EVAL      W#AMT = 0
     C                   EVAL      P#ITEM = '発注点を下回る商品・倉庫'
     C                   EXSR      @LINE
      * 入荷予定を過ぎた発注
     C/EXEC SQL
     C+   SELECT COUNT(*), COALESCE(SUM(TOTAMT), 0) INTO :W#N, :W#AMT
     C+     FROM POHDRP
     C+    WHERE STATUS IN ('OPEN', 'PARTIAL') AND DUEDATE < :W#TODAY
     C/END-EXEC
     C                   EVAL      P#ITEM = '入荷予定を過ぎた発注'
     C                   EXSR      @LINE
      * 有効期限を過ぎた見積(未受注)
     C/EXEC SQL
     C+   SELECT COUNT(*), COALESCE(SUM(TOTAMT), 0) INTO :W#N, :W#AMT
     C+     FROM QUOTEHP
     C+    WHERE STATUS = 'OPEN' AND VALIDTO < :W#TODAY
     C/END-EXEC
     C                   EVAL      P#ITEM = '有効期限を過ぎた見積(未受注)'
     C                   EXSR      @LINE
      * 前受金(未充当)
     C/EXEC SQL
     C+   SELECT COUNT(*), COALESCE(SUM(P.AMOUNT -
     C+          COALESCE((SELECT SUM(A.APPLYAMT) FROM PAYAPPLYP A
     C+                     WHERE A.PAYID = P.PAYID), 0)), 0)
     C+     INTO :W#N, :W#AMT
     C+     FROM PAYMENTP P WHERE P.STATUS = 'UNAPPLIED'
     C/END-EXEC
     C                   EVAL      P#ITEM = '前受金(未充当の入金)'
     C                   EXSR      @LINE
      * 未処理の入金データ
     C/EXEC SQL
     C+   SELECT COUNT(*), COALESCE(SUM(AMOUNT), 0) INTO :W#N, :W#AMT
     C+     FROM BANKINP WHERE STATUS IN ('NEW', 'MATCHED')
     C/END-EXEC
     C                   EVAL      P#ITEM = '未処理の入金データ'
     C                   EXSR      @LINE
      * 整合性: 請求の残高 = 合計 - 入金済
     C/EXEC SQL
     C+   SELECT COUNT(*) INTO :W#N FROM INVOICEHP
     C+    WHERE STATUS <> 'CANCELLED' AND BALANCE <> TOTAMT - PAIDAMT
     C/END-EXEC
     C                   EVAL      W#AMT = 0
     C                   EVAL      P#ITEM = '整合性: 請求の残高の不一致'
     C                   EXSR      @LINE
      * 整合性: 入金額と消込の合計(前受金は消込が入金額以下ならよい)
     C/EXEC SQL
     C+   SELECT COUNT(*) INTO :W#N FROM (
     C+     SELECT P.PAYID FROM PAYMENTP P
     C+       LEFT JOIN PAYAPPLYP A ON A.PAYID = P.PAYID
     C+      WHERE P.STATUS <> 'CANCELLED'
     C+      GROUP BY P.PAYID, P.AMOUNT, P.STATUS
     C+     HAVING (P.STATUS <> 'UNAPPLIED' AND
     C+             P.AMOUNT <> COALESCE(SUM(A.APPLYAMT), 0))
     C+         OR COALESCE(SUM(A.APPLYAMT), 0) > P.AMOUNT) X
     C/END-EXEC
     C                   EVAL      W#AMT = 0
     C                   EVAL      P#ITEM = '整合性: 入金と消込の不一致'
     C                   EXSR      @LINE
     C                   EXCEPT    FOOT
     C                   EVAL      *INLR = *ON
     C                   RETURN

      * @LINE - 1行印刷(件数が0でなければ「要確認」)
     C     @LINE         BEGSR
     C                   IF        SQLCODE <> 0
     C                   EVAL      P#CNT = 'エラー'
     C                   EVAL      P#AMT = 'SQLCODE=' + %CHAR(SQLCODE)
     C                   EVAL      P#MARK = *BLANKS
     C                   ELSE
     C                   EVALR     P#CNT = %TRIM(%EDITC(W#N:'3'))
     C                   IF        W#AMT <> 0
     C                   EVAL(H)   W#P0 = W#AMT
     C                   EVALR     P#AMT = %TRIM(%EDITC(W#P0:'J'))
     C                   ELSE
     C                   EVAL      P#AMT = *BLANKS
     C                   ENDIF
     C                   IF        W#N > 0
     C                   EVAL      P#MARK = '要確認'
     C                   ELSE
     C                   EVAL      P#MARK = *BLANKS
     C                   ENDIF
     C                   ENDIF
     C                   IF        *INOF
     C                   EXCEPT    HEAD
     C                   EVAL      *INOF = *OFF
     C                   ENDIF
     C                   EXCEPT    DETAIL
     C                   ENDSR

     OQSYSPRT   E            HEAD           2 03
     O                                           40 '夜間点検レポート'
     O                       P#DATE              64
     O                       P#TIME              74
     O          E            HEAD           2
     O                                           10 '点検項目'
     O                                           58 '件数'
     O                                           74 '金額(円)'
     O                                           84 '判定'
     O          E            DETAIL         1
     O                       P#ITEM              44
     O                       P#CNT               58
     O                       P#AMT               76
     O                       P#MARK              88
     O          E            FOOT        2
     O                                           40 '(データ変更なし)'
