     H DFTACTGRP(*NO) ACTGRP(*NEW)
      *=====================================================================
      * プログラム名: STKMAINT
      * 説明: 在庫の入庫・棚卸
      * 作成: 修正#STK 新規作成
      *   1=入庫  商品と数量を入力し、現在庫・引当可能数を増やす
      *           倉庫に在庫レコードがない商品は新しく作る
      *   2=棚卸  倉庫の在庫を一覧し、実棚数を入力して差異を反映する
      *           引当済数より少なくはできない。表示後に他の処理で
      *           在庫が変わった場合は反映しない(再表示を求める)
      *   増減は在庫移動履歴 STKMOVEP に記録する
      *   更新はコミットメント制御で一括確定(失敗時は全て取消)
      *=====================================================================
     FSTKMNTD   CF   E             WORKSTN SFILE(CNTSFL:W#RRN)

     D W#RRN           S              4S 0
     D W#CNT           S              4S 0
     D W#I             S              4S 0
     D W#EXIT          S               N
     D W#F6            S               N
     D W#ERR           S               N
     D W#CHG           S             10I 0
     D W#FIRST         S              4S 0
     D W#WHID          S              4P 0
     D W#WHNM          S             60A
     D W#WHDSP         S             60A
     D W#ITEMID        S              9P 0
     D W#ITEMNM        S             40A
     D W#UOM           S             10A
     D W#OH            S             15P 3
     D W#ALC           S             15P 3
     D W#AVL           S             15P 3
     D W#QTY           S             15P 3
     D W#BEF           S             15P 3
     D W#AFT           S             15P 3
     D W#TYPE          S             10A
     D W#REF           S             30A
      * 入庫: 最後に確認した商品コード(変えた直後のF6は確認だけ)
     D W#CHKCD         S             12A
     D W#USER          S             10A
     D W#TODAY         S              8P 0
     D W#NOW           S              6P 0
     D W#MSG           S             75A
      * 修正#F4P: 共通の検索ウィンドウ(F4)
     D O2CPRMPT        PR                  EXTPGM('O2CPRMPT')
     D   pType                       10A   CONST
     D   pCdType                     30A   CONST
     D   pValue                      30A
     D W#PVAL          S             30A
      * 棚卸一覧の取り出し先
     D C#ROW           DS
     D C#ITEMID                       9P 0
     D C#ITEMCD                      12A
     D C#ITEMNM                      40A
     D C#OH                          15P 3
     D C#ALC                         15P 3
     D PSDS           SDS
     D   P#USER              254    263A
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
     D Join            PR           256A   VARYING
     D   pStr                       256A   VARYING CONST
     D DbcsTrunc       PR           256A   VARYING
     D   pStr                       256A   VARYING CONST
     D   pMax                        10I 0 CONST

     C/EXEC SQL
     C+   SET OPTION COMMIT = *CHG, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE CNTCSR CURSOR FOR
     C+   SELECT I.ITEMID, I.ITEMCD, I.ITEMDSC, V.QTYOH, V.QTYALC
     C+     FROM INVENTORYP V JOIN ITEMP I ON I.ITEMID = V.ITEMID
     C+    WHERE V.WHID = :W#WHID
     C+    ORDER BY I.ITEMCD
     C+    FOR READ ONLY
     C/END-EXEC

     C                   EVAL      W#USER = P#USER
     C                   EVAL      MMSG = *BLANKS
     C                   DOW       NOT W#EXIT
     C                   EXFMT     STKMAIN
     C                   EVAL      MMSG = *BLANKS
     C                   EVAL      *IN31 = *OFF
     C                   EVAL      *IN32 = *OFF
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
      * 修正#F4P: F4=倉庫の検索
     C                   IF        *IN04
     C                   EVAL      W#PVAL = MWHCD
     C                   CALLP     O2CPRMPT('WH':' ':W#PVAL)
     C                   EVAL      MWHCD = W#PVAL
     C                   ITER
     C                   ENDIF
      * 倉庫コードのチェック
     C/EXEC SQL
     C+   SELECT WHID, WHNM INTO :W#WHID, :W#WHNM FROM WAREHOUSEP
     C+    WHERE WHCD = :MWHCD
     C/END-EXEC
     C                   IF        SQLCODE <> 0 OR MWHCD = *BLANKS
     C                   EVAL      *IN31 = *ON
     C                   EVAL      MWHNM = *BLANKS
     C                   EVAL      MMSG = '倉庫コードが見つかりません'
     C                   ITER
     C                   ENDIF
     C                   EVAL      MWHNM = DbcsTrunc(W#WHNM:40)
     C                   EVAL      W#WHDSP = %TRIM(MWHCD) + '  ' +
     C                             DbcsTrunc(W#WHNM:40)
     C                   SELECT
     C                   WHEN      MMODE = '1'
     C                   EXSR      @RECEIPT
     C                   WHEN      MMODE = '2'
     C                   EXSR      @COUNT
     C                   OTHER
     C                   EVAL      *IN32 = *ON
     C                   EVAL      MMSG = Join(
     C                             '処理は1(入庫)か2(棚卸)' +
     C                             'を入力してください')
     C                   ENDSL
     C                   ENDDO
      * 確定していない更新は残さない
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   EVAL      *INLR = *ON
     C                   RETURN

      *=====================================================================
      * @RECEIPT - 入庫
      *=====================================================================
     C     @RECEIPT      BEGSR
     C                   EVAL      RWH = W#WHDSP
     C                   EVAL      RITEMCD = *BLANKS
     C                   EVAL      RQTY = 0
     C                   EVAL      RREF = *BLANKS
     C                   EXSR      @RCLEAR
     C                   EVAL      RMSG = *BLANKS
     C                   EVAL      W#CHKCD = *BLANKS
     C                   DOW       *ON
     C                   EXFMT     RCPT
     C                   EVAL      W#F6 = *IN06
     C                   EVAL      *IN06 = *OFF
     C                   EVAL      RMSG = *BLANKS
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN42 = *OFF
     C                   IF        *IN03
     C                   EVAL      W#EXIT = *ON
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN12
     C                   LEAVE
     C                   ENDIF
      * 修正#F4P: F4=商品の検索(選んだ商品の在庫をすぐ表示する)
     C                   IF        *IN04
     C                   EVAL      W#PVAL = RITEMCD
     C                   CALLP     O2CPRMPT('ITEM':' ':W#PVAL)
     C                   EVAL      RITEMCD = W#PVAL
     C                   EVAL      W#F6 = *OFF
     C                   ENDIF
      * 商品のチェックと現在の在庫の表示
     C/EXEC SQL
     C+   SELECT ITEMID, ITEMDSC, UOM INTO :W#ITEMID, :W#ITEMNM, :W#UOM
     C+     FROM ITEMP WHERE ITEMCD = :RITEMCD
     C/END-EXEC
     C                   IF        SQLCODE <> 0 OR RITEMCD = *BLANKS
     C                   EXSR      @RCLEAR
     C                   EVAL      *IN41 = *ON
     C                   EVAL      RMSG = '商品コードが見つかりません'
     C                   ITER
     C                   ENDIF
     C                   EVAL      RITEMNM = DbcsTrunc(W#ITEMNM:40)
     C                   EVAL      RUOM = W#UOM
     C                   EXSR      @GETSTK
     C                   IF        RQTY <= 0
     C                   EVAL      *IN42 = *ON
     C                   EVAL      RMSG = '入庫数を入力してください'
     C                   ITER
     C                   ENDIF
     C                   IF        NOT W#F6 OR RITEMCD <> W#CHKCD
     C                   EVAL      W#CHKCD = RITEMCD
     C                   EVAL      RMSG = Join('入力に誤りはありません。F6' +
     C                             'で入庫を登録します')
     C                   ITER
     C                   ENDIF
      * 入庫の登録
     C                   EVAL      W#QTY = RQTY
     C/EXEC SQL
     C+   UPDATE INVENTORYP
     C+      SET QTYOH = QTYOH + :W#QTY, QTYAVL = QTYAVL + :W#QTY,
     C+          LSTUPD = CURRENT TIMESTAMP
     C+    WHERE WHID = :W#WHID AND ITEMID = :W#ITEMID
     C/END-EXEC
     C                   IF        SQLCODE = 100
     C/EXEC SQL
     C+   INSERT INTO INVENTORYP (WHID, ITEMID, QTYOH, QTYALC, QTYAVL,
     C+          LSTUPD)
     C+   VALUES (:W#WHID, :W#ITEMID, :W#QTY, 0, :W#QTY, CURRENT TIMESTAMP)
     C/END-EXEC
     C                   ENDIF
     C                   IF        SQLCODE <> 0
     C                   EXSR      @FAIL
     C                   EVAL      RMSG = W#MSG
     C                   ITER
     C                   ENDIF
     C/EXEC SQL
     C+   SELECT QTYOH INTO :W#AFT FROM INVENTORYP
     C+    WHERE WHID = :W#WHID AND ITEMID = :W#ITEMID
     C/END-EXEC
     C                   EVAL      W#BEF = W#AFT - W#QTY
     C                   EVAL      W#TYPE = 'RECEIPT'
     C                   EVAL      W#REF = RREF
     C                   EXSR      @WRTMOV
     C                   IF        W#ERR
     C                   EVAL      RMSG = W#MSG
     C                   ITER
     C                   ENDIF
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   EXSR      @GETSTK
     C                   EVAL      RMSG = Join(%TRIM(RITEMCD) + ' を ' +
     C                             %CHAR(RQTY) + ' 入庫しました(現在庫 ' +
     C                             %CHAR(ROH) + ')')
     C                   EVAL      RQTY = 0
     C                   EVAL      RREF = *BLANKS
     C                   ENDDO
     C                   ENDSR

      * 画面の商品情報を消す
     C     @RCLEAR       BEGSR
     C                   EVAL      RITEMNM = *BLANKS
     C                   EVAL      RUOM = *BLANKS
     C                   EVAL      ROH = 0
     C                   EVAL      RALC = 0
     C                   EVAL      RAVL = 0
     C                   ENDSR

      * 現在の在庫を画面に出す(在庫レコードがなければ0)
     C     @GETSTK       BEGSR
     C/EXEC SQL
     C+   SELECT QTYOH, QTYALC, QTYAVL INTO :W#OH, :W#ALC, :W#AVL
     C+     FROM INVENTORYP WHERE WHID = :W#WHID AND ITEMID = :W#ITEMID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#OH = 0
     C                   EVAL      W#ALC = 0
     C                   EVAL      W#AVL = 0
     C                   ENDIF
     C                   EVAL(H)   ROH = W#OH
     C                   EVAL(H)   RALC = W#ALC
     C                   EVAL(H)   RAVL = W#AVL
     C                   ENDSR

      *=====================================================================
      * @COUNT - 棚卸
      *=====================================================================
     C     @COUNT        BEGSR
     C                   EVAL      CWH = W#WHDSP
     C                   EXSR      @LOADCNT
     C                   DOW       *ON
     C                   EVAL      *IN92 = *ON
     C                   EVAL      *IN91 = (W#CNT > 0)
     C                   EVAL      *IN94 = *ON
     C                   IF        CRRN < 1
     C                   EVAL      CRRN = 1
     C                   ENDIF
     C                   EXFMT     CNTCTL
     C                   EVAL      W#F6 = *IN06
     C                   EVAL      *IN06 = *OFF
     C                   EVAL      CMSG = *BLANKS
     C                   IF        *IN03
     C                   EVAL      W#EXIT = *ON
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN12
     C                   LEAVE
     C                   ENDIF
     C                   IF        W#CNT = 0
     C                   ITER
     C                   ENDIF
     C                   EXSR      @CHKCNT
     C                   IF        W#ERR
     C                   ITER
     C                   ENDIF
     C                   IF        W#CHG = 0
     C                   EVAL      CMSG = Join(
     C                             '帳簿在庫と差異のある商品はあ' +
     C                             'りません')
     C                   ITER
     C                   ENDIF
     C                   IF        NOT W#F6
     C                   EVAL      CMSG = Join('差異のある商品 ' +
     C                             %CHAR(W#CHG) +
     C                             ' 件。F6で在庫に反映します')
     C                   ITER
     C                   ENDIF
     C                   EXSR      @APPLYCNT
     C                   IF        NOT W#ERR
     C                   EXSR      @LOADCNT
     C                   EVAL      CMSG = Join('棚卸を反映しました(' +
     C                             %CHAR(W#CHG) + ' 件)')
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR

      * 倉庫の在庫を一覧に読み込む(実棚数の初期値は帳簿在庫)
     C     @LOADCNT      BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     CNTCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN44 = *OFF
     C                   EVAL      *IN45 = *OFF
     C                   EVAL      W#CNT = 0
     C                   EVAL      CRRN = 1
     C/EXEC SQL
     C+   OPEN CNTCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#CNT < 999
     C/EXEC SQL
     C+   FETCH CNTCSR INTO :C#ROW
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#CNT = W#CNT + 1
     C                   EVAL      CITEMID = C#ITEMID
     C                   EVAL      CITEMCD = C#ITEMCD
     C                   EVAL      CITEMNM = DbcsTrunc(C#ITEMNM:20)
     C                   EVAL(H)   CBOOK = C#OH
     C                   EVAL(H)   CALC = C#ALC
     C                   EVAL      CACT = CBOOK
     C                   EVAL      CDIFF = 0
     C                   EVAL      CORGOH = C#OH
     C                   EVAL      CORGALC = C#ALC
     C                   EVAL      W#RRN = W#CNT
     C                   WRITE     CNTSFL
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE CNTCSR
     C/END-EXEC
     C                   IF        W#CNT = 0
     C                   EVAL      CMSG = 'この倉庫には在庫がありません'
     C                   ENDIF
     C                   ENDSR

      * 実棚数のチェックと差異の計算(全行)
     C     @CHKCNT       BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      W#CHG = 0
     C                   EVAL      W#FIRST = 0
     C                   FOR       W#I = 1 TO W#CNT
     C                   EVAL      W#RRN = W#I
     C     W#RRN         CHAIN     CNTSFL
     C                   IF        NOT %FOUND(STKMNTD)
     C                   ITER
     C                   ENDIF
     C                   EVAL      *IN45 = *OFF
     C                   IF        CACT < 0
     C                   EVAL      *IN45 = *ON
     C                   IF        W#FIRST = 0
     C                   EVAL      CMSG = Join(%TRIM(CITEMCD) +
     C                             ' 実棚数にマイナスは入力でき' + 'ません')
     C                   ENDIF
     C                   ELSEIF    CACT < CORGALC
     C                   EVAL      *IN45 = *ON
     C                   IF        W#FIRST = 0
     C                   EVAL      CMSG = Join(%TRIM(CITEMCD) + ' 引当済数(' +
     C                             %CHAR(CALC) + ')より少なくはできません')
     C                   ENDIF
     C                   ENDIF
     C                   IF        *IN45
     C                   EVAL      W#ERR = *ON
     C                   IF        W#FIRST = 0
     C                   EVAL      W#FIRST = W#I
     C                   ENDIF
     C                   ENDIF
     C                   EVAL      CDIFF = CACT - CBOOK
     C                   IF        CACT <> CORGOH
     C                   EVAL      W#CHG = W#CHG + 1
     C                   ENDIF
      * 入力した行は次回もREADCで読めるように変更済みにする
     C                   EVAL      *IN44 = *ON
     C                   UPDATE    CNTSFL
     C                   EVAL      *IN44 = *OFF
     C                   ENDFOR
     C                   EVAL      *IN45 = *OFF
     C                   IF        W#FIRST > 0
     C                   EVAL      CRRN = W#FIRST
     C                   ENDIF
     C                   ENDSR

      * 差異を在庫に反映(全件成功したら確定)
     C     @APPLYCNT     BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   FOR       W#I = 1 TO W#CNT
     C                   EVAL      W#RRN = W#I
     C     W#RRN         CHAIN     CNTSFL
     C                   IF        NOT %FOUND(STKMNTD) OR CACT = CORGOH
     C                   ITER
     C                   ENDIF
     C                   EVAL      W#ITEMID = CITEMID
     C                   EVAL      W#AFT = CACT
     C                   EVAL      W#BEF = CORGOH
      * 表示した時から在庫・引当が変わっていなければ更新する
     C/EXEC SQL
     C+   UPDATE INVENTORYP
     C+      SET QTYOH = :W#AFT, QTYAVL = :W#AFT - QTYALC,
     C+          LSTUPD = CURRENT TIMESTAMP
     C+    WHERE WHID = :W#WHID AND ITEMID = :W#ITEMID
     C+      AND QTYOH = :CORGOH AND QTYALC = :CORGALC
     C/END-EXEC
     C                   IF        SQLCODE = 100
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   EVAL      W#ERR = *ON
      * 再表示で商品コードが変わる前にメッセージを作る
     C                   EVAL      W#MSG = Join(%TRIM(CITEMCD) +
     C                             ' は他の処理で在庫が変わった' +
     C                             'ため再表示しました')
     C                   EXSR      @LOADCNT
     C                   EVAL      CMSG = W#MSG
     C                   LEAVESR
     C                   ENDIF
     C                   IF        SQLCODE <> 0
     C                   EXSR      @FAIL
     C                   EVAL      CMSG = W#MSG
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#QTY = W#AFT - W#BEF
     C                   EVAL      W#TYPE = 'COUNT'
      * 参照番号の列は半角のみ(CCSID 5123)
     C                   EVAL      W#REF = 'STOCKTAKE'
     C                   EXSR      @WRTMOV
     C                   IF        W#ERR
     C                   EVAL      CMSG = W#MSG
     C                   LEAVESR
     C                   ENDIF
     C                   ENDFOR
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   ENDSR

      *=====================================================================
      * @WRTMOV - 在庫移動履歴を書く(失敗時は取り消してW#ERR)
      *=====================================================================
     C     @WRTMOV       BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      W#TODAY = %DEC(%DATE():*ISO)
     C                   EVAL      W#NOW = %DEC(%TIME():*HMS)
     C/EXEC SQL
     C+   INSERT INTO STKMOVEP (MOVID, WHID, ITEMID, MOVTYPE, MOVQTY,
     C+          BEFQTY, AFTQTY, REFNO, MOVDATE, CRTUSER, CRTDATE, CRTTIM)
     C+   VALUES (NEXT VALUE FOR O2C_MOVID_SEQ, :W#WHID, :W#ITEMID,
     C+          :W#TYPE, :W#QTY, :W#BEF, :W#AFT, :W#REF, :W#TODAY,
     C+          :W#USER, :W#TODAY, :W#NOW)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EXSR      @FAIL
     C                   ENDIF
     C                   ENDSR

      * 更新の失敗: 取り消してメッセージを作る
     C     @FAIL         BEGSR
     C                   EVAL      W#MSG = '更新に失敗しました(SQLCODE=' +
     C                             %CHAR(SQLCODE) + ')。取り消しました'
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   EVAL      W#ERR = *ON
     C                   ENDSR

      * Join - 連結した全角文字列の SI+SO を取り除く
     P Join            B
     D Join            PI           256A   VARYING
     D   pStr                       256A   VARYING CONST
     C                   RETURN    %SCANRPL(W#SI + W#SO:'':pStr)
     P Join            E

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
