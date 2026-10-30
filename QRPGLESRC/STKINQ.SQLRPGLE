     H DFTACTGRP(*NO) ACTGRP(*NEW)
      *=====================================================================
      * プログラム名: STKINQ
      * 説明: 在庫照会(倉庫・商品ごとの在庫数と入出庫の履歴)
      * 作成: 修正#SIQ 新規作成。メニューの12番から呼ぶ
      *   倉庫コード・商品コード(前方一致)で絞り込み。F4で検索
      *   5=入出庫の履歴: 在庫移動履歴(入庫・棚卸・出荷取消)と
      *     出荷明細(出荷)を日付の新しい順に表示する
      *   有効在庫が0以下の行は赤で表示
      *=====================================================================
     FSTKINQD   CF   E             WORKSTN SFILE(LSFL:W#RRN)
     F                                     SFILE(HSFL:W#HRRN)

     D W#RRN           S              4S 0
     D W#HRRN          S              4S 0
     D W#CNT           S              4S 0
     D W#HCNT          S              4S 0
     D W#DONE          S               N
     D W#PVAL          S             30A
     D W#FWH           S             10A
      * LIKEの条件は可変長(固定長だと後ろの空白まで比べてしまう)
     D W#FIT           S             13A   VARYING
     D W#WHID          S              4P 0
     D W#ITEMID        S              9P 0
     D W#SELWH         S              4P 0
     D W#SELIT         S              9P 0
      * 一覧の読み込み用
     D S#WHID          S              4P 0
     D S#WHCD          S             10A
     D S#WHNM          S             60A
     D S#ITEMID        S              9P 0
     D S#ITEMCD        S             12A
     D S#ITEMNM        S             40A
     D S#OH            S             15P 3
     D S#ALC           S             15P 3
     D S#AVL           S             15P 3
      * 履歴の読み込み用
     D H#DT            S              8P 0
     D H#TM            S              6P 0
     D H#KIND          S             10A
     D H#QTY           S             15P 3
     D H#AFT           S             15P 3
     D H#REF           S             30A
     D H#HAS           S              1P 0
     D O2CPRMPT        PR                  EXTPGM('O2CPRMPT')
     D   pType                       10A   CONST
     D   pCdType                     30A   CONST
     D   pValue                      30A
     D FmtQ10          PR            10A
     D   pQty                        15P 3 CONST
     D FmtQ12          PR            12A
     D   pQty                        15P 3 CONST
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
     C+   SELECT V.WHID, W.WHCD, V.ITEMID, I.ITEMCD, I.ITEMDSC,
     C+          V.QTYOH, V.QTYALC, V.QTYAVL
     C+     FROM INVENTORYP V JOIN WAREHOUSEP W ON W.WHID = V.WHID
     C+     JOIN ITEMP I ON I.ITEMID = V.ITEMID
     C+    WHERE (:W#FWH = ' ' OR W.WHCD = :W#FWH)
     C+      AND I.ITEMCD LIKE :W#FIT
     C+    ORDER BY W.WHCD, I.ITEMCD
     C+    FOR READ ONLY
     C/END-EXEC
     C/EXEC SQL
     C+   DECLARE HISCSR CURSOR FOR
     C+   SELECT DT, TM, KIND, QTY, AFT, HAS, REF FROM (
     C+     SELECT M.MOVDATE AS DT, M.CRTTIM AS TM,
     C+            CAST(M.MOVTYPE AS CHAR(10)) AS KIND,
     C+            M.MOVQTY AS QTY, M.AFTQTY AS AFT, 1 AS HAS,
     C+            CAST(M.REFNO AS CHAR(30)) AS REF
     C+       FROM STKMOVEP M
     C+      WHERE M.WHID = :W#WHID AND M.ITEMID = :W#ITEMID
     C+     UNION ALL
     C+     SELECT S.SHPDATE, S.CRTTIM,
     C+            CASE WHEN S.STATUS = 'CANCELLED' THEN 'SHIPX'
     C+                 ELSE 'SHIP' END,
     C+            0 - D.SHPQTY, 0, 0, CAST(S.SHPNUM AS CHAR(30))
     C+       FROM SHIPMENTLP D JOIN SHIPMENTHP S ON S.SHPID = D.SHPID
     C+      WHERE S.WHID = :W#WHID AND D.ITEMID = :W#ITEMID
     C+   ) X
     C+    ORDER BY DT DESC, TM DESC
     C+    FOR READ ONLY
     C/END-EXEC

     C                   EVAL      LFWH = *BLANKS
     C                   EVAL      LFITEM = *BLANKS
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
      * F4=カーソルのある欄で倉庫か商品を検索
     C                   IF        *IN04
     C                   IF        CSRFLD = 'LFITEM'
     C                   EVAL      W#PVAL = LFITEM
     C                   CALLP     O2CPRMPT('ITEM':' ':W#PVAL)
     C                   EVAL      LFITEM = W#PVAL
     C                   ELSE
     C                   EVAL      W#PVAL = LFWH
     C                   CALLP     O2CPRMPT('WH':' ':W#PVAL)
     C                   EVAL      LFWH = W#PVAL
     C                   ENDIF
     C                   EXSR      @LOAD
     C                   ITER
     C                   ENDIF
      * 5=入出庫の履歴(最初の1件)
     C                   EVAL      W#SELIT = 0
     C                   IF        W#CNT > 0
     C                   READC     LSFL
     C                   DOW       NOT %EOF(STKINQD)
     C                   IF        LOPT = '5' AND W#SELIT = 0
     C                   EVAL      W#SELWH = LWHID
     C                   EVAL      W#SELIT = LITEMID
     C                   ENDIF
     C                   READC     LSFL
     C                   ENDDO
     C                   ENDIF
     C                   IF        W#SELIT > 0
     C                   EVAL      W#WHID = W#SELWH
     C                   EVAL      W#ITEMID = W#SELIT
     C                   EXSR      @HIST
     C                   ENDIF
     C                   EXSR      @LOAD
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C                   RETURN

      *=====================================================================
      * @LOAD - 在庫の一覧を読み込む
      *=====================================================================
     C     @LOAD         BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     LCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#CNT = 0
     C                   EVAL      LFWH = %XLATE('abcdefghijklmnopqrstuvwxyz':
     C                             'ABCDEFGHIJKLMNOPQRSTUVWXYZ':LFWH)
     C                   EVAL      LFITEM = %XLATE('abcdefghijklmnopqrstuvwxyz':
     C                             'ABCDEFGHIJKLMNOPQRSTUVWXYZ':LFITEM)
     C                   EVAL      W#FWH = LFWH
     C                   EVAL      W#FIT = %TRIM(LFITEM) + '%'
     C/EXEC SQL
     C+   OPEN LSTCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#CNT < 999
     C/EXEC SQL
     C+   FETCH LSTCSR INTO :S#WHID, :S#WHCD, :S#ITEMID, :S#ITEMCD,
     C+     :S#ITEMNM, :S#OH, :S#ALC, :S#AVL
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      LOPT = ' '
     C                   EVAL      LWHID = S#WHID
     C                   EVAL      LITEMID = S#ITEMID
     C                   EVAL      LWH = S#WHCD
     C                   EVAL      LITEM = S#ITEMCD
     C                   EVAL      LNAME = DbcsTrunc(%TRIMR(S#ITEMNM):20)
     C                   EVAL      LOH = FmtQ10(S#OH)
     C                   EVAL      LALC = FmtQ10(S#ALC)
     C                   EVAL      LAVL = FmtQ10(S#AVL)
     C                   EVAL      *IN61 = (S#AVL <= 0)
     C                   EVAL      W#CNT = W#CNT + 1
     C                   EVAL      W#RRN = W#CNT
     C                   WRITE     LSFL
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE LSTCSR
     C/END-EXEC
     C                   EVAL      *IN61 = *OFF
     C                   IF        W#CNT = 0 AND LMSG = *BLANKS
     C                   EVAL      LMSG = '該当する在庫がありません'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @HIST - 入出庫の履歴(F12で一覧へ戻る)
      *=====================================================================
     C     @HIST         BEGSR
     C/EXEC SQL
     C+   SELECT W.WHCD, W.WHNM, I.ITEMCD, I.ITEMDSC,
     C+          V.QTYOH, V.QTYALC, V.QTYAVL
     C+     INTO :S#WHCD, :S#WHNM, :S#ITEMCD, :S#ITEMNM,
     C+          :S#OH, :S#ALC, :S#AVL
     C+     FROM INVENTORYP V JOIN WAREHOUSEP W ON W.WHID = V.WHID
     C+     JOIN ITEMP I ON I.ITEMID = V.ITEMID
     C+    WHERE V.WHID = :W#WHID AND V.ITEMID = :W#ITEMID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      LMSG = '在庫が見つかりません'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      HWH = DbcsTrunc(%TRIM(S#WHCD) + ' ' +
     C                             %TRIMR(S#WHNM):40)
     C                   EVAL      HITEM = DbcsTrunc(%TRIM(S#ITEMCD) + ' ' +
     C                             %TRIMR(S#ITEMNM):40)
     C                   EVAL      HOH = FmtQ10(S#OH)
     C                   EVAL      HALC = FmtQ10(S#ALC)
     C                   EVAL      HAVL = FmtQ10(S#AVL)
     C                   EVAL      *IN83 = *ON
     C                   WRITE     HCTL
     C                   EVAL      *IN83 = *OFF
     C                   EVAL      W#HCNT = 0
     C/EXEC SQL
     C+   OPEN HISCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#HCNT < 999
     C/EXEC SQL
     C+   FETCH HISCSR INTO :H#DT, :H#TM, :H#KIND, :H#QTY, :H#AFT,
     C+     :H#HAS, :H#REF
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      HDT = FmtDate(H#DT)
     C                   SELECT
     C                   WHEN      H#KIND = 'RECEIPT'
     C                   EVAL      HKIND = '入庫'
     C                   WHEN      H#KIND = 'COUNT'
     C                   EVAL      HKIND = '棚卸'
     C                   WHEN      H#KIND = 'SHPCANCEL'
     C                   EVAL      HKIND = '出荷取消'
     C                   WHEN      H#KIND = 'SHIP'
     C                   EVAL      HKIND = '出荷'
     C                   WHEN      H#KIND = 'SHIPX'
     C                   EVAL      HKIND = '出荷*'
     C                   OTHER
     C                   EVAL      HKIND = H#KIND
     C                   ENDSL
     C                   EVAL      HQTY = FmtQ12(H#QTY)
     C                   IF        H#HAS = 1
     C                   EVAL      HAFT = FmtQ12(H#AFT)
     C                   ELSE
     C                   EVAL      HAFT = *BLANKS
     C                   ENDIF
     C                   EVAL      HREF = H#REF
     C                   EVAL      W#HCNT = W#HCNT + 1
     C                   EVAL      W#HRRN = W#HCNT
     C                   WRITE     HSFL
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE HISCSR
     C/END-EXEC
     C                   IF        W#HCNT = 0
     C                   EVAL      HMSG = '入出庫の履歴はありません'
     C                   ELSE
     C                   EVAL      HMSG = '出荷*=取り消した出荷'
     C                   ENDIF
     C                   EVAL      *IN82 = *ON
     C                   EVAL      *IN81 = (W#HCNT > 0)
     C                   EVAL      *IN84 = *ON
     C                   WRITE     HFOOT
     C                   EXFMT     HCTL
     C                   IF        *IN03
     C                   EVAL      W#DONE = *ON
     C                   ENDIF
     C                   ENDSR

      * FmtQ10 - 数量を右寄せ10桁の文字に(小数なし)
     P FmtQ10          B
     D FmtQ10          PI            10A
     D   pQty                        15P 3 CONST
     D wOut            S             10A
     C                   EVALR     wOut = %EDITC(%DECH(pQty:7:0):'J')
     C                   RETURN    wOut
     P FmtQ10          E

      * FmtQ12 - 数量を右寄せ12桁の文字に(小数なし)
     P FmtQ12          B
     D FmtQ12          PI            12A
     D   pQty                        15P 3 CONST
     D wOut            S             12A
     C                   EVALR     wOut = %EDITC(%DECH(pQty:9:0):'J')
     C                   RETURN    wOut
     P FmtQ12          E

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
