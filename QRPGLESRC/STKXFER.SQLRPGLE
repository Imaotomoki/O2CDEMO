     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: STKXFER
      * 説明: 倉庫間の在庫移動
      * 作成: 修正#XFR 新規作成。販売管理メニューの22から呼ぶ
      *   移動元の有効在庫(現在庫-引当済)の範囲で、移動先へ在庫を移す
      *   在庫移動(STKMOVEP)に XFER-OUT(移動元、マイナス)と
      *   XFER-IN(移動先)を残す。参照番号は入力した番号
      *   Enter で確認、F6 で登録
      *=====================================================================
     FSTKXFERD  CF   E             WORKSTN

     D W#ERR           S               N
     D W#PVAL          S             30A
     D W#USER          S             10A   INZ(*USER)
     D W#D8            S              8P 0
     D W#T6            S              6P 0
     D W#P0            S             11P 0
     D W#CHK           S             40A
     D W#KEY           S             40A
     D H#ITEMCD        S             12A
     D H#ITEMID        S              9P 0
     D H#NM            S             60A
     D H#STS           S             10A
     D H#WHID          S              4P 0
     D H#WHCD          S             10A
     D H#AVL           S             15P 3
     D H#OH            S             15P 3
     D H#QTY           S             15P 3
     D H#BEF           S             15P 3
     D H#AFT           S             15P 3
     D H#REF           S             30A
     D H#TYPE          S             10A
     D W#FAVL          S             15P 3
     D W#FSTS          S             10A
     D W#TSTS          S             10A
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
     D O2CPRMPT        PR                  EXTPGM('O2CPRMPT')
     D   PTYPE                       10A   CONST
     D   PCDTYPE                     30A   CONST
     D   PVALUE                      30A
     D DBCSTRUNC       PR           256A   VARYING
     D   PSTR                       256A   VARYING CONST
     D   PMAX                        10I 0 CONST

     C/EXEC SQL
     C+   SET OPTION COMMIT = *CHG, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C                   EVAL      XMSG = *BLANKS
     C                   DOW       *ON
     C                   EXFMT     XFER
     C                   EVAL      XMSG = *BLANKS
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN04
     C                   EXSR      @PROMPT
     C                   EXSR      @NAMES
     C                   ITER
     C                   ENDIF
     C                   EXSR      @CHECK
     C                   IF        W#ERR
     C                   ITER
     C                   ENDIF
      * 入力を変えずに F6 を押したときだけ登録する
     C                   EVAL      W#KEY = XITEM + %CHAR(XFROM) + '/' +
     C                             %CHAR(XTO) + '/' + %CHAR(XQTY)
     C                   IF        NOT *IN06 OR W#KEY <> W#CHK
     C                   EVAL      W#CHK = W#KEY
     C                   EVAL      XMSG = '入力に誤りはありません。F6 で' +
     C                             '移動を登録します'
     C                   ITER
     C                   ENDIF
     C                   EXSR      @MOVE
     C                   ENDDO
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   EVAL      *INLR = *ON
     C                   RETURN

      * @PROMPT - F4(商品・倉庫)。倉庫は倉庫コードから倉庫IDにする
     C     @PROMPT       BEGSR
     C                   SELECT
     C                   WHEN      CSRFLD = 'XITEM'
     C                   EVAL      W#PVAL = XITEM
     C                   CALLP     O2CPRMPT('ITEM':' ':W#PVAL)
     C                   EVAL      XITEM = W#PVAL
     C                   WHEN      CSRFLD = 'XFROM' OR CSRFLD = 'XTO'
     C                   EVAL      W#PVAL = *BLANKS
     C                   CALLP     O2CPRMPT('WH':' ':W#PVAL)
     C                   IF        W#PVAL <> *BLANKS
     C                   EVAL      H#WHCD = W#PVAL
     C/EXEC SQL
     C+   SELECT WHID INTO :H#WHID FROM WAREHOUSEP WHERE WHCD = :H#WHCD
     C/END-EXEC
     C                   IF        SQLCODE = 0 AND CSRFLD = 'XFROM'
     C                   EVAL      XFROM = H#WHID
     C                   ELSEIF    SQLCODE = 0
     C                   EVAL      XTO = H#WHID
     C                   ENDIF
     C                   ENDIF
     C                   OTHER
     C                   EVAL      XMSG = '商品か倉庫にカーソルを置いてF4'
     C                   ENDSL
     C                   ENDSR

      * @NAMES - 商品名・倉庫名・在庫
     C     @NAMES        BEGSR
     C                   EVAL      XITEMNM = *BLANKS
     C                   EVAL      XFROMNM = *BLANKS
     C                   EVAL      XTONM = *BLANKS
     C                   EVAL      XFAVL = *BLANKS
     C                   EVAL      XTOH = *BLANKS
     C                   EVAL      H#ITEMID = 0
     C                   EVAL      H#STS = *BLANKS
     C                   EVAL      W#FAVL = 0
     C                   EVAL      W#FSTS = *BLANKS
     C                   EVAL      W#TSTS = *BLANKS
     C                   IF        XITEM <> *BLANKS
     C                   EVAL      H#ITEMCD = XITEM
     C/EXEC SQL
     C+   SELECT ITEMID, ITEMDSC, STATUS INTO :H#ITEMID, :H#NM, :H#STS
     C+     FROM ITEMP WHERE ITEMCD = :H#ITEMCD
     C/END-EXEC
     C                   IF        SQLCODE = 0
     C                   EVAL      XITEMNM = DBCSTRUNC(%TRIMR(H#NM):40)
     C                   ELSE
     C                   EVAL      H#ITEMID = 0
     C                   EVAL      XITEMNM = '(商品なし)'
     C                   ENDIF
     C                   ENDIF
     C                   IF        XFROM > 0
     C                   EVAL      H#WHID = XFROM
     C/EXEC SQL
     C+   SELECT WHNM, STATUS INTO :H#NM, :W#FSTS
     C+     FROM WAREHOUSEP WHERE WHID = :H#WHID
     C/END-EXEC
     C                   IF        SQLCODE = 0
     C                   EVAL      XFROMNM = DBCSTRUNC(%TRIMR(H#NM):20)
     C                   ELSE
     C                   EVAL      W#FSTS = *BLANKS
     C                   ENDIF
     C                   IF        H#ITEMID > 0
     C/EXEC SQL
     C+   SELECT QTYAVL INTO :W#FAVL FROM INVENTORYP
     C+    WHERE WHID = :H#WHID AND ITEMID = :H#ITEMID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#FAVL = 0
     C                   ENDIF
     C                   EVAL(H)   W#P0 = W#FAVL
     C                   EVALR     XFAVL = %TRIM(%EDITC(W#P0:'J'))
     C                   ENDIF
     C                   ENDIF
     C                   IF        XTO > 0
     C                   EVAL      H#WHID = XTO
     C/EXEC SQL
     C+   SELECT WHNM, STATUS INTO :H#NM, :W#TSTS
     C+     FROM WAREHOUSEP WHERE WHID = :H#WHID
     C/END-EXEC
     C                   IF        SQLCODE = 0
     C                   EVAL      XTONM = DBCSTRUNC(%TRIMR(H#NM):20)
     C                   ELSE
     C                   EVAL      W#TSTS = *BLANKS
     C                   ENDIF
     C                   IF        H#ITEMID > 0
     C                   EVAL      H#OH = 0
     C/EXEC SQL
     C+   SELECT QTYOH INTO :H#OH FROM INVENTORYP
     C+    WHERE WHID = :H#WHID AND ITEMID = :H#ITEMID
     C/END-EXEC
     C                   EVAL(H)   W#P0 = H#OH
     C                   EVALR     XTOH = %TRIM(%EDITC(W#P0:'J'))
     C                   ENDIF
     C                   ENDIF
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   ENDSR

      * @CHECK - 入力チェック
     C     @CHECK        BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      *IN31 = *OFF
     C                   EVAL      *IN32 = *OFF
     C                   EVAL      *IN33 = *OFF
     C                   EVAL      *IN34 = *OFF
     C                   EXSR      @NAMES
     C                   SELECT
     C                   WHEN      H#ITEMID = 0
     C                   EVAL      *IN31 = *ON
     C                   EVAL      XMSG = '商品を入力してください(F4=検索)'
     C                   WHEN      W#FSTS <> 'ACTIVE'
     C                   EVAL      *IN32 = *ON
     C                   EVAL      XMSG = '移動元の倉庫がないか停止中です'
     C                   WHEN      W#TSTS <> 'ACTIVE'
     C                   EVAL      *IN33 = *ON
     C                   EVAL      XMSG = '移動先の倉庫がないか停止中です'
     C                   WHEN      XFROM = XTO
     C                   EVAL      *IN33 = *ON
     C                   EVAL      XMSG = '移動元と移動先が同じです'
     C                   WHEN      XQTY <= 0
     C                   EVAL      *IN34 = *ON
     C                   EVAL      XMSG = '移動数を入力してください'
     C                   WHEN      XQTY > W#FAVL
     C                   EVAL      *IN34 = *ON
     C                   EVAL      XMSG = '移動元の有効在庫を超えています'
     C                   ENDSL
     C                   IF        XMSG <> *BLANKS
     C                   EVAL      W#ERR = *ON
     C                   EVAL      W#CHK = *BLANKS
     C                   ENDIF
     C                   ENDSR

      * @MOVE - 移動の登録(移動元を減らし、移動先を増やす)
     C     @MOVE         BEGSR
     C                   EVAL      W#D8 = %DEC(%DATE():*ISO)
     C                   EVAL      W#T6 = %DEC(%TIME():*HMS)
     C                   EVAL      H#QTY = XQTY
     C                   EVAL      H#REF = XREF
     C                   IF        H#REF = *BLANKS
     C                   EVAL      H#REF = 'XFER ' + %CHAR(W#D8)
     C                   ENDIF
      * 移動元(ほかで引き当てられていたら更新しない)
     C                   EVAL      H#WHID = XFROM
     C/EXEC SQL
     C+   UPDATE INVENTORYP
     C+      SET QTYOH = QTYOH - :H#QTY, QTYAVL = QTYAVL - :H#QTY,
     C+          LSTUPD = CURRENT TIMESTAMP
     C+    WHERE WHID = :H#WHID AND ITEMID = :H#ITEMID
     C+      AND QTYAVL >= :H#QTY
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      XMSG = '移動元の在庫を更新できません(' +
     C                             '在庫が変わった可能性) SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EXSR      @FAIL
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      H#TYPE = 'XFER-OUT'
     C                   EXSR      @MOVLOG
     C                   IF        W#ERR
     C                   LEAVESR
     C                   ENDIF
      * 移動先
     C                   EVAL      H#WHID = XTO
     C/EXEC SQL
     C+   UPDATE INVENTORYP
     C+      SET QTYOH = QTYOH + :H#QTY, QTYAVL = QTYAVL + :H#QTY,
     C+          LSTUPD = CURRENT TIMESTAMP
     C+    WHERE WHID = :H#WHID AND ITEMID = :H#ITEMID
     C/END-EXEC
     C                   IF        SQLCODE = 100
     C/EXEC SQL
     C+   INSERT INTO INVENTORYP (WHID, ITEMID, QTYOH, QTYALC, QTYAVL,
     C+          LSTUPD)
     C+   VALUES (:H#WHID, :H#ITEMID, :H#QTY, 0, :H#QTY, CURRENT TIMESTAMP)
     C/END-EXEC
     C                   ENDIF
     C                   IF        SQLCODE <> 0
     C                   EVAL      XMSG = '移動先の在庫エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EXSR      @FAIL
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      H#TYPE = 'XFER-IN'
     C                   EXSR      @MOVLOG
     C                   IF        W#ERR
     C                   LEAVESR
     C                   ENDIF
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   EXSR      @NAMES
     C                   EVAL      XMSG = %TRIM(XITEM) + ' を ' +
     C                             %CHAR(XQTY) +
     C                             ' 移動しました'
     C                   EVAL      XQTY = 0
     C                   EVAL      W#CHK = *BLANKS
     C                   ENDSR

      * @MOVLOG - 在庫移動の記録(H#WHID, H#TYPE)
     C     @MOVLOG       BEGSR
     C                   EVAL      W#ERR = *OFF
     C/EXEC SQL
     C+   SELECT QTYOH INTO :H#AFT FROM INVENTORYP
     C+    WHERE WHID = :H#WHID AND ITEMID = :H#ITEMID
     C/END-EXEC
     C                   IF        H#TYPE = 'XFER-OUT'
     C                   EVAL      H#BEF = H#AFT + H#QTY
     C                   EVAL      H#OH = 0 - H#QTY
     C                   ELSE
     C                   EVAL      H#BEF = H#AFT - H#QTY
     C                   EVAL      H#OH = H#QTY
     C                   ENDIF
     C/EXEC SQL
     C+   INSERT INTO STKMOVEP (MOVID, WHID, ITEMID, MOVTYPE, MOVQTY,
     C+          BEFQTY, AFTQTY, REFNO, MOVDATE, CRTUSER, CRTDATE, CRTTIM)
     C+   VALUES (NEXT VALUE FOR O2C_MOVID_SEQ, :H#WHID, :H#ITEMID,
     C+          :H#TYPE, :H#OH, :H#BEF, :H#AFT, :H#REF, :W#D8,
     C+          :W#USER, :W#D8, :W#T6)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      XMSG = '在庫移動の記録エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EXSR      @FAIL
     C                   ENDIF
     C                   ENDSR

      * @FAIL - 取消
     C     @FAIL         BEGSR
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   EVAL      W#ERR = *ON
     C                   EVAL      W#CHK = *BLANKS
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
