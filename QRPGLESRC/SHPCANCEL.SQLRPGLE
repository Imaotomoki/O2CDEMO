     H DFTACTGRP(*NO) ACTGRP(*NEW)
      *=====================================================================
      * プログラム名: SHPCANCEL
      * 説明: 出荷取消(未請求の出荷を取り消し、在庫と受注を戻す)
      * 作成: 修正#SCN 新規作成。メニューの11番から呼ぶ
      *   一覧(出荷日の新しい順)で4=取消→確認画面→F6で実行
      *   取り消せるのは未請求(SHIPPED)の出荷だけ
      *     (請求済は先に請求書照会のF10で請求書を取り消す)
      *   取消の内容(1つのトランザクション。失敗したらROLLBACK)
      *     - 出荷の状態を CANCELLED にする
      *     - 在庫の現在庫と引当済を出荷数だけ戻す(引当し直す)
      *     - 受注明細の出荷済数を減らす(0なら ALLOCATED、残れば PARTIAL)
      *     - 受注の状態を ALLOCATED / PARTIAL に戻す
      *     - 在庫移動履歴(STKMOVEP)に SHPCANCEL を記録
      *   ACTGRP(*NEW): SQLのコミット定義を終了時に閉じる
      *=====================================================================
     FSHPCNLD   CF   E             WORKSTN SFILE(LSFL:W#RRN)

     D W#RRN           S              4S 0
     D W#CNT           S              4S 0
     D W#DONE          S               N
     D W#ERR           S               N
     D W#MSG           S             78A
     D W#PVAL          S             30A
     D W#FCUST         S             20A
     D W#SEL           S             18P 0
     D W#SHPID         S             18P 0
     D W#D8            S              8P 0
     D W#T6            S              6P 0
     D W#K             S             10I 0
     D W#NM24          S             24A
      * 出荷の読み込み用
     D S#SHPID         S             18P 0
     D S#SHPNUM        S             20A
     D S#SHPDT         S              8P 0
     D S#SOID          S             18P 0
     D S#SONUM         S             20A
     D S#CUSTCD        S             20A
     D S#CUSTNM        S             60A
     D S#WHID          S              4P 0
     D S#WHCD          S             10A
     D S#WHNM          S             60A
     D S#STS           S             12A
      * 出荷明細(受注明細の出荷済数と一緒に読む)
     D A#SOLNO         S              9P 0
     D A#ITEMID        S              9P 0
     D A#ITEMCD        S             12A
     D A#ITEMNM        S             60A
     D A#QTY           S             15P 3
     D A#SOSHP         S             15P 3
     D A#BEF           S             15P 3
     D A#AFT           S             15P 3
     D W#AN            S             10I 0
     D W#LSQL          S             10I 0
     D W#ASOL          S              9P 0 DIM(100)
     D W#AITM          S              9P 0 DIM(100)
     D W#ACD           S             12A   DIM(100)
     D W#ANM           S             60A   DIM(100)
     D W#AQTY          S             15P 3 DIM(100)
     D W#ASOS          S             15P 3 DIM(100)
     D W#LN            S             76A   DIM(10)
     D O2CPRMPT        PR                  EXTPGM('O2CPRMPT')
     D   pType                       10A   CONST
     D   pCdType                     30A   CONST
     D   pValue                      30A
     D FmtQty          PR            10A
     D   pQty                        15P 3 CONST
     D StsName         PR            10A
     D   pSts                        12A   CONST
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
     C+   SET OPTION COMMIT = *CHG, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE LSTCSR CURSOR FOR
     C+   SELECT S.SHPID, S.SHPNUM, S.SHPDATE, H.SONUM, C.CUSTCD, S.STATUS
     C+     FROM SHIPMENTHP S JOIN SORDERHP H ON H.SOID = S.SOID
     C+     JOIN CUSTOMERP C ON C.CUSTID = H.CUSTID
     C+    WHERE (:W#FCUST = ' ' OR C.CUSTCD = :W#FCUST)
     C+    ORDER BY S.SHPDATE DESC, S.SHPID DESC
     C+    FOR READ ONLY
     C/END-EXEC
     C/EXEC SQL
     C+   DECLARE LINCSR CURSOR FOR
     C+   SELECT D.SOLNO, D.ITEMID, COALESCE(I.ITEMCD, ' '),
     C+          COALESCE(I.ITEMDSC, ' '), D.SHPQTY, L.SHPQTY
     C+     FROM SHIPMENTLP D
     C+     JOIN SHIPMENTHP S ON S.SHPID = D.SHPID
     C+     JOIN SORDERLP L ON L.SOID = S.SOID AND L.LINENO = D.SOLNO
     C+     LEFT JOIN ITEMP I ON I.ITEMID = D.ITEMID
     C+    WHERE D.SHPID = :W#SHPID
     C+    ORDER BY D.LINENO
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
     C                   EVAL      *IN31 = *OFF
     C                   EVAL      LMSG = *BLANKS
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
      * F4=顧客の検索
     C                   IF        *IN04
     C                   EVAL      W#PVAL = LFCUST
     C                   CALLP     O2CPRMPT('CUST':' ':W#PVAL)
     C                   EVAL      LFCUST = W#PVAL
     C                   EXSR      @LOAD
     C                   ITER
     C                   ENDIF
      * 4=取消 が付いた最初の出荷を確認画面へ
     C                   EVAL      W#SEL = 0
     C                   IF        W#CNT > 0
     C                   READC     LSFL
     C                   DOW       NOT %EOF(SHPCNLD)
     C                   IF        LOPT = '4' AND W#SEL = 0
     C                   EVAL      W#SEL = LSHPID
     C                   ENDIF
     C                   READC     LSFL
     C                   ENDDO
     C                   ENDIF
     C                   IF        W#SEL > 0
     C                   EVAL      W#SHPID = W#SEL
     C                   EXSR      @CONFIRM
     C                   ENDIF
     C                   EXSR      @LOAD
     C                   ENDDO
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   EVAL      *INLR = *ON
     C                   RETURN

      *=====================================================================
      * @LOAD - 出荷の一覧を読み込む(顧客コードで絞り込み)
      *=====================================================================
     C     @LOAD         BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     LCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#CNT = 0
     C                   EVAL      W#FCUST = LFCUST
     C/EXEC SQL
     C+   OPEN LSTCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#CNT < 999
     C/EXEC SQL
     C+   FETCH LSTCSR INTO :S#SHPID, :S#SHPNUM, :S#SHPDT, :S#SONUM,
     C+     :S#CUSTCD, :S#STS
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      LOPT = ' '
     C                   EVAL      LSHPID = S#SHPID
     C                   EVAL      LSHPNUM = S#SHPNUM
     C                   EVAL      LSHPDT = FmtDate(S#SHPDT)
     C                   EVAL      LSONUM = S#SONUM
     C                   EVAL      LCUSTCD = S#CUSTCD
     C                   EVAL      LSTS = StsName(S#STS)
     C                   EVAL      W#CNT = W#CNT + 1
     C                   EVAL      W#RRN = W#CNT
     C                   WRITE     LSFL
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE LSTCSR
     C/END-EXEC
     C                   IF        W#CNT = 0 AND LMSG = *BLANKS
     C                   EVAL      LMSG = '該当する出荷がありません'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @CONFIRM - 確認画面(F6=取消実行 F12=戻る)
      *=====================================================================
     C     @CONFIRM      BEGSR
     C                   EXSR      @GETSHP
     C                   IF        W#ERR
     C                   EVAL      LMSG = W#MSG
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      CMSG = *BLANKS
     C                   DOW       *ON
     C                   EXFMT     CNF
     C                   EVAL      CMSG = *BLANKS
     C                   IF        *IN03
     C                   EVAL      W#DONE = *ON
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN12
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN06
     C                   EXSR      @CANCEL
     C                   IF        NOT W#ERR
     C                   EVAL      LMSG = '出荷 ' + %TRIM(CSHPNUM) +
     C                             ' を取り消しました'
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      CMSG = W#MSG
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR

      *=====================================================================
      * @GETSHP - 出荷と明細を読んで確認画面の項目を作る
      *=====================================================================
     C     @GETSHP       BEGSR
     C                   EVAL      W#ERR = *OFF
     C/EXEC SQL
     C+   SELECT S.SHPNUM, S.SHPDATE, S.SOID, H.SONUM, C.CUSTCD,
     C+          C.CUSTNM, S.WHID, COALESCE(W.WHCD, ' '),
     C+          COALESCE(W.WHNM, ' '), S.STATUS
     C+     INTO :S#SHPNUM, :S#SHPDT, :S#SOID, :S#SONUM, :S#CUSTCD,
     C+          :S#CUSTNM, :S#WHID, :S#WHCD, :S#WHNM, :S#STS
     C+     FROM SHIPMENTHP S JOIN SORDERHP H ON H.SOID = S.SOID
     C+     JOIN CUSTOMERP C ON C.CUSTID = H.CUSTID
     C+     LEFT JOIN WAREHOUSEP W ON W.WHID = S.WHID
     C+    WHERE S.SHPID = :W#SHPID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#MSG = '出荷が見つかりません'
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   IF        S#STS = 'CANCELLED'
     C                   EVAL      W#MSG = 'すでに取り消し済みです'
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   IF        S#STS = 'INVOICED'
     C                   EVAL      W#MSG = '先に請求書を取り消してください'
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   IF        S#STS <> 'SHIPPED'
     C                   EVAL      W#MSG = 'この出荷は取り消せません'
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      CSHPNUM = S#SHPNUM
     C                   EVAL      CSTS = StsName(S#STS)
     C                   EVAL      CSONUM = S#SONUM
     C                   EVAL      CCUSTCD = S#CUSTCD
     C                   EVAL      CCUSTNM = DbcsTrunc(%TRIMR(S#CUSTNM):38)
     C                   EVAL      CSHPDT = FmtDate(S#SHPDT)
     C                   EVAL      CWH = DbcsTrunc(%TRIM(S#WHCD) + ' ' +
     C                             %TRIMR(S#WHNM):40)
     C                   EXSR      @LOADLIN
      * 戻す商品(10行まで。多いときは最後の行を「ほか n 件」)
     C                   EVAL      W#LN = *BLANKS
     C                   IF        W#AN = 0
     C                   EVAL      W#LN(1) = '  (明細なし)'
     C                   ENDIF
     C                   EVAL      W#K = 1
     C                   DOW       W#K <= W#AN AND W#K <= 10
     C                   IF        W#K = 10 AND W#AN > 10
     C                   EVAL      W#LN(W#K) = '  ほか ' +
     C                             %CHAR(W#AN - 9) + ' 件'
     C                   ELSE
     C                   EVAL      W#NM24 = DbcsTrunc(%TRIMR(W#ANM(W#K)):24)
     C                   EVAL      W#LN(W#K) = '  ' + W#ACD(W#K) + ' ' +
     C                             W#NM24 +
     C                             FmtQty(W#AQTY(W#K)) + '  ' +
     C                             FmtQty(W#ASOS(W#K) - W#AQTY(W#K))
     C                   ENDIF
     C                   EVAL      W#K = W#K + 1
     C                   ENDDO
     C                   EVAL      CAL01 = W#LN(1)
     C                   EVAL      CAL02 = W#LN(2)
     C                   EVAL      CAL03 = W#LN(3)
     C                   EVAL      CAL04 = W#LN(4)
     C                   EVAL      CAL05 = W#LN(5)
     C                   EVAL      CAL06 = W#LN(6)
     C                   EVAL      CAL07 = W#LN(7)
     C                   EVAL      CAL08 = W#LN(8)
     C                   EVAL      CAL09 = W#LN(9)
     C                   EVAL      CAL10 = W#LN(10)
     C                   ENDSR

      *=====================================================================
      * @LOADLIN - 出荷明細を配列に読む
      *=====================================================================
     C     @LOADLIN      BEGSR
     C                   EVAL      W#AN = 0
     C/EXEC SQL
     C+   OPEN LINCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#AN < 100
     C/EXEC SQL
     C+   FETCH LINCSR INTO :A#SOLNO, :A#ITEMID, :A#ITEMCD, :A#ITEMNM,
     C+     :A#QTY, :A#SOSHP
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#AN = W#AN + 1
     C                   EVAL      W#ASOL(W#AN) = A#SOLNO
     C                   EVAL      W#AITM(W#AN) = A#ITEMID
     C                   EVAL      W#ACD(W#AN) = A#ITEMCD
     C                   EVAL      W#ANM(W#AN) = A#ITEMNM
     C                   EVAL      W#AQTY(W#AN) = A#QTY
     C                   EVAL      W#ASOS(W#AN) = A#SOSHP
     C                   ENDDO
     C                   EVAL      W#LSQL = SQLCODE
     C/EXEC SQL
     C+   CLOSE LINCSR
     C/END-EXEC
     C                   ENDSR

      *=====================================================================
      * @CANCEL - 取消の実行(1つのトランザクション)
      *=====================================================================
     C     @CANCEL       BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      W#D8 = %DEC(%DATE():*ISO)
     C                   EVAL      W#T6 = %DEC(%TIME():*HMS)
      * 先に出荷を更新してロックする(請求・取消と同時に動かないように)
     C/EXEC SQL
     C+   UPDATE SHIPMENTHP SET STATUS = 'CANCELLED',
     C+          UPDUSER = USER, UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHERE SHPID = :W#SHPID AND STATUS = 'SHIPPED'
     C/END-EXEC
     C                   IF        SQLCODE < 0
     C                   EVAL      W#MSG = '出荷の更新エラー'
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
     C                   IF        SQLER3 = 0
     C                   EVAL      W#MSG = '他で請求・取消されました'
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
     C                   EXSR      @LOADLIN
      * 修正#LIN 明細を読めない・多すぎるときは取消さない(在庫と
      *   受注明細を戻さないまま出荷だけ取消済になっていた)
     C                   IF        W#LSQL < 0 OR W#AN = 0 OR
     C                             (W#LSQL = 0 AND W#AN >= 100)
     C                   EVAL      W#MSG = '出荷明細の読み込みエラー'
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#K = 1
     C                   DOW       W#K <= W#AN
     C                   EVAL      A#SOLNO = W#ASOL(W#K)
     C                   EVAL      A#ITEMID = W#AITM(W#K)
     C                   EVAL      A#QTY = W#AQTY(W#K)
      * 受注明細の出荷済数を戻す
     C/EXEC SQL
     C+   UPDATE SORDERLP
     C+      SET SHPQTY = SHPQTY - :A#QTY,
     C+          STATUS = CASE WHEN SHPQTY - :A#QTY <= 0
     C+                        THEN 'ALLOCATED' ELSE 'PARTIAL' END
     C+    WHERE SOID = :S#SOID AND LINENO = :A#SOLNO
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#MSG = '受注明細の更新エラー'
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
      * 在庫の現在庫と引当済を戻す(引当し直すので有効在庫は変わらない)
      * 修正#BEF 先に更新(ロック)してから読み、変更前は逆算する
     C/EXEC SQL
     C+   UPDATE INVENTORYP
     C+      SET QTYOH = QTYOH + :A#QTY, QTYALC = QTYALC + :A#QTY,
     C+          QTYAVL = QTYOH - QTYALC, LSTUPD = CURRENT TIMESTAMP
     C+    WHERE WHID = :S#WHID AND ITEMID = :A#ITEMID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#MSG = '在庫の更新エラー'
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
     C/EXEC SQL
     C+   SELECT QTYOH INTO :A#AFT FROM INVENTORYP
     C+    WHERE WHID = :S#WHID AND ITEMID = :A#ITEMID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#MSG = '在庫が見つかりません'
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      A#BEF = A#AFT - A#QTY
     C/EXEC SQL
     C+   INSERT INTO STKMOVEP
     C+     (MOVID, WHID, ITEMID, MOVTYPE, MOVQTY, BEFQTY, AFTQTY,
     C+      REFNO, MOVDATE, CRTUSER, CRTDATE, CRTTIM)
     C+   VALUES
     C+     (NEXT VALUE FOR O2C_MOVID_SEQ, :S#WHID, :A#ITEMID,
     C+      'SHPCANCEL', :A#QTY, :A#BEF, :A#AFT, :S#SHPNUM,
     C+      :W#D8, USER, :W#D8, :W#T6)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#MSG = '在庫移動履歴の登録エラー'
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#K = W#K + 1
     C                   ENDDO
      * 受注の状態: 出荷済の明細が残れば PARTIAL、なければ ALLOCATED
     C/EXEC SQL
     C+   UPDATE SORDERHP
     C+      SET STATUS = CASE WHEN (SELECT COUNT(*) FROM SORDERLP L
     C+                          WHERE L.SOID = :S#SOID AND L.SHPQTY > 0) > 0
     C+                        THEN 'PARTIAL' ELSE 'ALLOCATED' END,
     C+          UPDUSER = USER, UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHERE SOID = :S#SOID AND STATUS IN ('SHIPPED', 'PARTIAL')
     C/END-EXEC
     C                   IF        SQLCODE < 0
     C                   EVAL      W#MSG = '受注の更新エラー'
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#MSG = '確定(COMMIT)エラー'
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
     C                   ENDSR

      * @ROLLBK - エラーのときは取消をすべて元に戻す
     C     @ROLLBK       BEGSR
     C                   EVAL      W#ERR = *ON
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   ENDSR

      * FmtQty - 数量を右寄せ10桁の文字に(小数なし)
     P FmtQty          B
     D FmtQty          PI            10A
     D   pQty                        15P 3 CONST
     D wOut            S             10A
     C                   EVALR     wOut = %EDITC(%DECH(pQty:9:0):'J')
     C                   RETURN    wOut
     P FmtQty          E

      * StsName - 出荷の状態を日本語に
     P StsName         B
     D StsName         PI            10A
     D   pSts                        12A   CONST
     C                   SELECT
     C                   WHEN      pSts = 'SHIPPED'
     C                   RETURN    '未請求'
     C                   WHEN      pSts = 'INVOICED'
     C                   RETURN    '請求済'
     C                   WHEN      pSts = 'CANCELLED'
     C                   RETURN    '取消済'
     C                   ENDSL
     C                   RETURN    pSts
     P StsName         E

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
