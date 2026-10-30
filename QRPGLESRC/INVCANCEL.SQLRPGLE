     H DFTACTGRP(*NO) ACTGRP(*NEW)
      *=====================================================================
      * プログラム名: INVCANCEL
      * 説明: 請求書の取消(画面なし。請求書照会 INVINQ から呼ぶ)
      * 作成: 修正#ICN 新規作成
      *   パラメーター
      *     P#ACT    : C=取消  L=受注の最新の有効な請求書を探す
      *     P#INVNUM : 請求番号
      *     P#MSG    : 結果のメッセージ(空白なら成功)
      *     P#NEWINV : L のとき見つかった請求番号
      *   取消できるのは、入金済が0で、締め済みの期間でない請求書
      *   取消の内容(1つのトランザクション)
      *     - 請求書の状態を CANCELLED、残高を0にする
      *       (売掛金年齢表・督促・入金・締め請求の対象から外れる)
      *     - 請求した出荷を SHIPPED に戻す(もう一度請求できる)
      *     - 受注が INVOICED なら SHIPPED に戻す
      *   再発行は INVINQ が CRTINVCL(=CREATEINV)を呼んで行う
      *   ACTGRP(*NEW): コミット定義をこのプログラムの中で閉じる
      *=====================================================================

     D P#ACT           S              1A
     D P#INVNUM        S             20A
     D P#MSG           S             78A
     D P#NEWINV        S             20A
     D W#INVNUM        S             20A
     D W#INVID         S             18P 0
     D W#SOID          S             18P 0
     D W#CUSTID        S              9P 0
     D W#CUR           S              3A
     D W#INVDT         S              8P 0
     D W#PAID          S             15P 2
     D W#STS           S             12A
     D W#LCLS          S              8P 0
     D W#D8            S              8P 0
     D W#T6            S              6P 0
     D W#CNT           S             10I 0

     C     *ENTRY        PLIST
     C                   PARM                    P#ACT
     C                   PARM                    P#INVNUM
     C                   PARM                    P#MSG
     C                   PARM                    P#NEWINV

     C/EXEC SQL
     C+   SET OPTION COMMIT = *CHG, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C                   EVAL      P#MSG = *BLANKS
     C                   EVAL      P#NEWINV = *BLANKS
     C                   EVAL      W#INVNUM = P#INVNUM
     C/EXEC SQL
     C+   SELECT INVID, SOID, CUSTID, CURRCD, INVDATE, PAIDAMT, STATUS
     C+     INTO :W#INVID, :W#SOID, :W#CUSTID, :W#CUR, :W#INVDT,
     C+          :W#PAID, :W#STS
     C+     FROM INVOICEHP WHERE INVNUM = :W#INVNUM
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      P#MSG = '請求書が見つかりません'
     C                   RETURN
     C                   ENDIF

      * L=同じ受注の最新の有効な請求書(再発行の後に表示するため)
     C                   IF        P#ACT = 'L'
     C/EXEC SQL
     C+   SELECT INVNUM INTO :W#INVNUM FROM INVOICEHP
     C+    WHERE SOID = :W#SOID AND STATUS <> 'CANCELLED'
     C+    ORDER BY INVID DESC FETCH FIRST 1 ROW ONLY
     C/END-EXEC
     C                   IF        SQLCODE = 0
     C                   EVAL      P#NEWINV = W#INVNUM
     C                   ELSE
     C                   EVAL      P#MSG = '再発行した請求書が見つかりません'
     C                   ENDIF
     C                   RETURN
     C                   ENDIF

      * C=取消。取り消せるかを確かめる
     C                   IF        W#STS = 'CANCELLED'
     C                   EVAL      P#MSG = 'すでに取り消し済みです'
     C                   RETURN
     C                   ENDIF
     C                   IF        W#PAID <> 0
     C                   EVAL      P#MSG = '先に入金を取り消してください'
     C                   RETURN
     C                   ENDIF
     C/EXEC SQL
     C+   SELECT COALESCE(MAX(CLSDATE), 0) INTO :W#LCLS FROM BILLSTMP
     C+    WHERE CUSTID = :W#CUSTID AND CURRCD = :W#CUR
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#LCLS = 0
     C                   ENDIF
     C                   IF        W#LCLS > 0 AND W#INVDT <= W#LCLS
     C                   EVAL      P#MSG = '締め済みのため取り消せません'
     C                   RETURN
     C                   ENDIF

     C                   EVAL      W#D8 = %DEC(%DATE():*ISO)
     C                   EVAL      W#T6 = %DEC(%TIME():*HMS)
      * 先に請求書を更新してロック(条件が変わっていたら取り消さない)
     C/EXEC SQL
     C+   UPDATE INVOICEHP SET STATUS = 'CANCELLED', BALANCE = 0,
     C+          UPDUSER = USER, UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHERE INVID = :W#INVID AND STATUS <> 'CANCELLED'
     C+      AND PAIDAMT = 0
     C/END-EXEC
     C                   IF        SQLCODE < 0
     C                   EVAL      P#MSG = '請求書の更新エラー'
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   RETURN
     C                   ENDIF
     C                   IF        SQLER3 = 0
     C                   EVAL      P#MSG = '他で更新されました'
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   RETURN
     C                   ENDIF
      * 請求した出荷を未請求(SHIPPED)に戻す
     C/EXEC SQL
     C+   UPDATE SHIPMENTHP SET STATUS = 'SHIPPED',
     C+          UPDUSER = USER, UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHERE STATUS = 'INVOICED'
     C+      AND SHPID IN (SELECT SHPID FROM INVOICELP
     C+                     WHERE INVID = :W#INVID)
     C/END-EXEC
     C                   IF        SQLCODE < 0
     C                   EVAL      P#MSG = '出荷の更新エラー'
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   RETURN
     C                   ENDIF
      * 受注が請求済なら出荷済に戻す
     C/EXEC SQL
     C+   UPDATE SORDERHP SET STATUS = 'SHIPPED',
     C+          UPDUSER = USER, UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHERE STATUS = 'INVOICED'
     C+      AND SOID IN (SELECT SOID FROM INVOICELP
     C+                    WHERE INVID = :W#INVID)
     C/END-EXEC
     C                   IF        SQLCODE < 0
     C                   EVAL      P#MSG = '受注の更新エラー'
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   RETURN
     C                   ENDIF
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      P#MSG = '確定(COMMIT)エラー'
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   RETURN
     C                   ENDIF
     C                   EVAL      *INLR = *ON
     C                   RETURN
