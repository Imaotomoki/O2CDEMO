     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: POENTRY
      * 説明: 発注点アラート・発注・入荷
      * 作成: 修正#PO 新規作成。販売管理メニューの21から呼ぶ
      *   一覧: 4=取消(入荷前のみ) 5=表示 6=入荷 F6=新規 F7=発注点
      *   発注点: 有効在庫+発注残が商品の発注点(REORDPT)より少ない
      *     商品・倉庫を出す。1=で発注数(既定は商品の発注数量)の発注を
      *     仕入先(商品の SUPPCD)・倉庫ごとにまとめて作る
      *   入荷: 明細ごとに今回の入荷数を入れて F6。在庫(INVENTORYP)を
      *     増やし、在庫移動(STKMOVEP RECEIPT 参照=発注番号)を残す
      *     全部入荷したら RECEIVED、一部なら PARTIAL
      *   仕入単価0は商品の標準原価(STDCOST)
      *   発注IDは O2C_POID_SEQ で採番。発注番号は PO-発注ID
      *=====================================================================
     FPOENTRYD  CF   E             WORKSTN SFILE(LSFL:W#RRN)
     F                                     SFILE(PSFL:W#PRRN)
     F                                     SFILE(DSFL:W#DRRN)

     D W#RRN           S              4S 0
     D W#PRRN          S              4S 0
     D W#DRRN          S              4S 0
     D W#CNT           S              4S 0
     D W#PCNT          S              4S 0
     D W#SQLCD         S             10I 0
     D W#DONE          S               N
     D W#ERR           S               N
     D W#MODE          S              1A
     D W#F4            S               N
     D W#F6            S               N
     D W#SEL           S             18P 0
     D W#OPT           S              1A
     D W#N             S             10I 0
     D W#I             S             10I 0
     D W#J             S             10I 0
     D W#K             S             10I 0
     D W#PVAL          S             30A
     D W#USER          S             10A   INZ(*USER)
     D W#D8            S              8P 0
     D W#T6            S              6P 0
     D W#P2            S             13P 2
     D W#P0            S             11P 0
     D W#TOT           S             15P 2
     D W#WSTS          S             10A
     D W#ISTS          S             10A
     D W#STD           S             15P 4
     D W#ALL           S               N
     D W#ANY           S               N
     D W#DESC          S             60A
      * 明細(10行)
     D L_ITEM          S             12A   DIM(10)
     D L_QTY           S              9P 0 DIM(10)
     D L_COST          S             11P 2 DIM(10)
     D L_ITEMID        S              9P 0 DIM(10)
     D L_NAME          S             16A   DIM(10)
     D L_LINE          S              9P 0 DIM(10)
     D L_RCV           S              9P 0 DIM(10)
     D L_NOW           S              9P 0 DIM(10)
     D L_ERR           S              1A   DIM(10)
      * 発注の提案で選んだ行
     D W#SN            S             10I 0
     D S_ITEMID        S              9P 0 DIM(999)
     D S_WHID          S              4P 0 DIM(999)
     D S_SUPP          S             20A   DIM(999)
     D S_QTY           S              9P 0 DIM(999)
     D S_DONE          S               N   DIM(999)
      * SQL 用
     D H#ID            S             18P 0
     D H#NUM           S             20A
     D H#SUPP          S             20A
     D H#PODT          S              8P 0
     D H#DUE           S              8P 0
     D H#WHID          S              4P 0
     D H#STS           S             12A
     D H#TOT           S             15P 2
     D H#NM            S             60A
     D H#LINE          S              9P 0
     D H#ITEMID        S              9P 0
     D H#ITEMCD        S             12A
     D H#QTY           S             15P 3
     D H#RCV           S             15P 3
     D H#COST          S             15P 4
     D H#AMT           S             15P 2
     D H#WHCD          S             10A
     D H#AVL           S             15P 3
     D H#OPEN          S             15P 3
     D H#RPT           S             15P 3
     D H#RQTY          S             15P 3
     D H#F             S              1A
     D H#BEF           S             15P 3
     D H#AFT           S             15P 3
     D O2CPRMPT        PR                  EXTPGM('O2CPRMPT')
     D   PTYPE                       10A   CONST
     D   PCDTYPE                     30A   CONST
     D   PVALUE                      30A
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
     D DBCSTRUNC       PR           256A   VARYING
     D   PSTR                       256A   VARYING CONST
     D   PMAX                        10I 0 CONST

     C/EXEC SQL
     C+   SET OPTION COMMIT = *CHG, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE LSTCSR CURSOR FOR
     C+   SELECT POID, PONUM, SUPPCD, PODATE, DUEDATE, TOTAMT, STATUS
     C+     FROM POHDRP
     C+    WHERE :H#F = 'A' OR STATUS IN ('OPEN', 'PARTIAL')
     C+    ORDER BY POID DESC
     C+    FOR READ ONLY
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE LINCSR CURSOR FOR
     C+   SELECT L.LINENO, L.ITEMID, COALESCE(I.ITEMCD, ' '), L.ORDQTY,
     C+          L.RCVQTY, L.UNITCOST
     C+     FROM POLINEP L
     C+     LEFT JOIN ITEMP I ON I.ITEMID = L.ITEMID
     C+    WHERE L.POID = :H#ID
     C+    ORDER BY L.LINENO
     C+    FOR READ ONLY
     C/END-EXEC

      * 発注点を下回る商品・倉庫(在庫のレコードがある倉庫だけ)
     C/EXEC SQL
     C+   DECLARE RPTCSR CURSOR FOR
     C+   SELECT I.ITEMID, I.ITEMCD, I.ITEMDSC, W.WHID, W.WHCD,
     C+          V.QTYAVL, COALESCE((SELECT SUM(L.ORDQTY - L.RCVQTY)
     C+            FROM POLINEP L JOIN POHDRP H ON H.POID = L.POID
     C+           WHERE H.STATUS IN ('OPEN', 'PARTIAL')
     C+             AND H.WHID = W.WHID AND L.ITEMID = I.ITEMID), 0),
     C+          I.REORDPT, I.REORDQTY, I.SUPPCD
     C+     FROM ITEMP I
     C+     JOIN INVENTORYP V ON V.ITEMID = I.ITEMID
     C+     JOIN WAREHOUSEP W ON W.WHID = V.WHID
     C+    WHERE I.STATUS = 'ACTIVE' AND W.STATUS = 'ACTIVE'
     C+      AND I.REORDPT > 0
     C+      AND V.QTYAVL + COALESCE((SELECT SUM(L.ORDQTY - L.RCVQTY)
     C+            FROM POLINEP L JOIN POHDRP H ON H.POID = L.POID
     C+           WHERE H.STATUS IN ('OPEN', 'PARTIAL')
     C+             AND H.WHID = W.WHID AND L.ITEMID = I.ITEMID), 0)
     C+          < I.REORDPT
     C+    ORDER BY I.SUPPCD, W.WHID, I.ITEMCD
     C+    FOR READ ONLY
     C/END-EXEC

     C                   EVAL      LMSG = *BLANKS
     C                   EVAL      LFSTS = ' '
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
     C                   IF        *IN06
     C                   EVAL      W#MODE = 'A'
     C                   EVAL      W#SEL = 0
     C                   EXSR      @EDIT
     C                   EXSR      @LOAD
     C                   ITER
     C                   ENDIF
     C                   IF        *IN07
     C                   EXSR      @PROPOSE
     C                   EXSR      @LOAD
     C                   ITER
     C                   ENDIF
     C                   EVAL      W#SEL = 0
     C                   IF        W#CNT > 0
     C                   READC     LSFL
     C                   DOW       NOT %EOF(POENTRYD)
     C                   IF        LOPT <> ' ' AND W#SEL = 0
     C                   EVAL      W#SEL = LID
     C                   EVAL      W#OPT = LOPT
     C                   ENDIF
     C                   READC     LSFL
     C                   ENDDO
     C                   ENDIF
     C                   IF        W#SEL > 0
     C                   SELECT
     C                   WHEN      W#OPT = '4'
     C                   EXSR      @CANCEL
     C                   WHEN      W#OPT = '5'
     C                   EVAL      W#MODE = 'V'
     C                   EXSR      @EDIT
     C                   WHEN      W#OPT = '6'
     C                   EVAL      W#MODE = 'R'
     C                   EXSR      @EDIT
     C                   ENDSL
     C                   ENDIF
     C                   EXSR      @LOAD
     C                   ENDDO
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   EVAL      *INLR = *ON
     C                   RETURN

      * @LOAD - 発注の一覧と発注点アラート
     C     @LOAD         BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     LCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#CNT = 0
     C                   EVAL      H#F = LFSTS
     C/EXEC SQL
     C+   OPEN LSTCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#CNT < 999
     C/EXEC SQL
     C+   FETCH LSTCSR INTO :H#ID, :H#NUM, :H#SUPP, :H#PODT, :H#DUE,
     C+                     :H#TOT, :H#STS
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      LOPT = ' '
     C                   EVAL      LID = H#ID
     C                   EVAL      LPONUM = H#NUM
     C                   EVAL      LSUPP = H#SUPP
     C                   EVAL      LPODT = %CHAR(%DATE(H#PODT:*ISO):*ISO)
     C                   EVAL      LDUE = %CHAR(%DATE(H#DUE:*ISO):*ISO)
     C                   EVAL      W#P2 = H#TOT
     C                   EVALR     LAMT = %TRIM(%EDITC(W#P2:'J'))
     C                   EVAL      *IN61 = *OFF
     C                   EXSR      @STSNAME
     C                   EVAL      LSTS = DSTS
     C                   IF        (H#STS = 'OPEN' OR H#STS = 'PARTIAL') AND
     C                             H#DUE < %DEC(%DATE():*ISO)
     C                   EVAL      LSTS = '入荷遅れ'
     C                   EVAL      *IN61 = *ON
     C                   ENDIF
     C                   EVAL      W#CNT = W#CNT + 1
     C                   EVAL      W#RRN = W#CNT
     C                   WRITE     LSFL
     C                   ENDDO
     C                   EVAL      W#SQLCD = SQLCODE
     C/EXEC SQL
     C+   CLOSE LSTCSR
     C/END-EXEC
     C                   EVAL      *IN61 = *OFF
     C                   IF        W#SQLCD < 0
     C                   EVAL      LMSG = '読み込みエラー SQLCODE=' +
     C                             %CHAR(W#SQLCD)
     C                   ELSEIF    W#CNT = 0 AND LMSG = *BLANKS
     C                   EVAL      LMSG = '未入荷の発注はありません'
     C                   ENDIF
      * 発注点アラート(件数)
     C                   EVAL      W#N = 0
     C/EXEC SQL
     C+   OPEN RPTCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0
     C/EXEC SQL
     C+   FETCH RPTCSR INTO :H#ITEMID, :H#ITEMCD, :H#NM, :H#WHID, :H#WHCD,
     C+                     :H#AVL, :H#OPEN, :H#RPT, :H#RQTY, :H#SUPP
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#N = W#N + 1
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE RPTCSR
     C/END-EXEC
     C                   IF        W#N > 0
     C                   EVAL      LALERT = '発注点を下回る商品 ' +
     C                             %CHAR(W#N) + ' 件(F7)'
     C                   ELSE
     C                   EVAL      LALERT = *BLANKS
     C                   ENDIF
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   ENDSR

      * @STSNAME - 状態の名前(H#STS → DSTS)
     C     @STSNAME      BEGSR
     C                   SELECT
     C                   WHEN      H#STS = 'OPEN'
     C                   EVAL      DSTS = '発注済'
     C                   WHEN      H#STS = 'PARTIAL'
     C                   EVAL      DSTS = '一部入荷'
     C                   WHEN      H#STS = 'RECEIVED'
     C                   EVAL      DSTS = '入荷済'
     C                   WHEN      H#STS = 'CANCELLED'
     C                   EVAL      DSTS = '取消'
     C                   OTHER
     C                   EVAL      DSTS = H#STS
     C                   ENDSL
     C                   ENDSR

      * @HEADER - 発注のヘッダ(W#SEL)
     C     @HEADER       BEGSR
     C                   EVAL      W#ERR = *OFF
     C/EXEC SQL
     C+   SELECT PONUM, SUPPCD, PODATE, DUEDATE, WHID, STATUS, TOTAMT
     C+     INTO :H#NUM, :H#SUPP, :H#PODT, :H#DUE, :H#WHID, :H#STS, :H#TOT
     C+     FROM POHDRP WHERE POID = :W#SEL
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      LMSG = '発注が見つかりません'
     C                   EVAL      W#ERR = *ON
     C                   ENDIF
     C                   ENDSR

      * @CANCEL - 取消(入荷前の発注だけ)
     C     @CANCEL       BEGSR
     C                   EXSR      @HEADER
     C                   IF        W#ERR
     C                   LEAVESR
     C                   ENDIF
     C                   IF        H#STS <> 'OPEN'
     C                   EVAL      LMSG = '入荷前の発注だけ取り消せます'
     C                   LEAVESR
     C                   ENDIF
     C                   EXSR      @STAMP
     C/EXEC SQL
     C+   UPDATE POHDRP SET STATUS = 'CANCELLED', UPDUSER = :W#USER,
     C+          UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHERE POID = :W#SEL AND STATUS = 'OPEN'
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      LMSG = '取消エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   ELSE
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   EVAL      LMSG = %TRIM(H#NUM) + ' を取り消しました'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @PROPOSE - 発注点を下回る商品(F7)と発注の作成
      *=====================================================================
     C     @PROPOSE      BEGSR
     C                   EVAL      *IN83 = *ON
     C                   WRITE     PCTL
     C                   EVAL      *IN83 = *OFF
     C                   EVAL      W#PCNT = 0
     C                   EVAL      PMSG = *BLANKS
     C/EXEC SQL
     C+   OPEN RPTCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#PCNT < 999
     C/EXEC SQL
     C+   FETCH RPTCSR INTO :H#ITEMID, :H#ITEMCD, :H#NM, :H#WHID, :H#WHCD,
     C+                     :H#AVL, :H#OPEN, :H#RPT, :H#RQTY, :H#SUPP
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      POPT = ' '
     C                   EVAL      PITEM = H#ITEMCD
     C                   EVAL      PNAME = DBCSTRUNC(%TRIMR(H#NM):16)
     C                   EVAL      PWH = H#WHCD
     C                   EVAL(H)   W#P0 = H#AVL
     C                   EVALR     PAVL = %TRIM(%EDITC(W#P0:'J'))
     C                   EVAL(H)   W#P0 = H#OPEN
     C                   EVALR     PONORD = %TRIM(%EDITC(W#P0:'J'))
     C                   EVAL(H)   W#P0 = H#RPT
     C                   EVALR     PRPT = %TRIM(%EDITC(W#P0:'J'))
      * 発注数: 商品の発注数量(なければ発注点に届く数)
     C                   IF        H#RQTY > 0
     C                   EVAL(H)   PQTY = H#RQTY
     C                   ELSE
     C                   EVAL(H)   PQTY = H#RPT - H#AVL - H#OPEN
     C                   ENDIF
     C                   EVAL      PITEMID = H#ITEMID
     C                   EVAL      PWHID = H#WHID
     C                   EVAL      PSUPP = H#SUPP
     C                   EVAL      *IN42 = *OFF
     C                   EVAL      W#PCNT = W#PCNT + 1
     C                   EVAL      W#PRRN = W#PCNT
     C                   WRITE     PSFL
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE RPTCSR
     C/END-EXEC
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   IF        W#PCNT = 0
     C                   EVAL      LMSG = '発注点を下回る商品はありません'
     C                   LEAVESR
     C                   ENDIF
     C                   DOW       *ON
     C                   EVAL      *IN81 = *ON
     C                   EVAL      *IN84 = *ON
     C                   WRITE     PFOOT
     C                   EXFMT     PCTL
     C                   EVAL      PMSG = *BLANKS
     C                   IF        *IN03
     C                   EVAL      W#DONE = *ON
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN12
     C                   LEAVE
     C                   ENDIF
      * 選んだ行を集める(仕入先のない商品は発注できない)
     C                   EVAL      W#SN = 0
     C                   EVAL      W#ERR = *OFF
     C                   FOR       W#I = 1 TO W#PCNT
     C     W#I           CHAIN     PSFL
     C                   IF        NOT %FOUND(POENTRYD)
     C                   ITER
     C                   ENDIF
     C                   EVAL      *IN42 = *OFF
     C                   IF        POPT = '1'
     C                   IF        PQTY <= 0 OR PSUPP = *BLANKS
     C                   EVAL      *IN42 = *ON
     C                   EVAL      W#ERR = *ON
     C                   IF        PSUPP = *BLANKS
     C                   EVAL      PMSG = %TRIM(PITEM) + ': 商品に仕入先が' +
     C                             'ありません(商品マスターで設定)'
     C                   ELSE
     C                   EVAL      PMSG = %TRIM(PITEM) + ': 発注数を入力' +
     C                             'してください'
     C                   ENDIF
     C                   ELSE
     C                   EVAL      W#SN = W#SN + 1
     C                   EVAL      S_ITEMID(W#SN) = PITEMID
     C                   EVAL      S_WHID(W#SN) = PWHID
     C                   EVAL      S_SUPP(W#SN) = PSUPP
     C                   EVAL      S_QTY(W#SN) = PQTY
     C                   EVAL      S_DONE(W#SN) = *OFF
     C                   ENDIF
     C                   ENDIF
     C                   UPDATE    PSFL
     C                   ENDFOR
     C                   IF        W#ERR
     C                   ITER
     C                   ENDIF
     C                   IF        W#SN = 0
     C                   EVAL      PMSG = '発注する商品に1を入れてください'
     C                   ITER
     C                   ENDIF
     C                   EXSR      @AUTOPO
     C                   IF        NOT W#ERR
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR

      * @AUTOPO - 選んだ商品を仕入先・倉庫ごとの発注にする
     C     @AUTOPO       BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      W#N = 0
     C                   EXSR      @STAMP
     C                   FOR       W#I = 1 TO W#SN
     C                   IF        S_DONE(W#I)
     C                   ITER
     C                   ENDIF
     C                   EVAL      H#SUPP = S_SUPP(W#I)
     C                   EVAL      H#WHID = S_WHID(W#I)
     C                   EVAL      H#PODT = W#D8
     C                   EVAL      H#DUE = %DEC(%DATE() + %DAYS(7):*ISO)
     C/EXEC SQL
     C+   VALUES NEXT VALUE FOR O2C_POID_SEQ INTO :H#ID
     C/END-EXEC
     C                   EVAL      H#NUM = 'PO-' + %CHAR(H#ID)
     C                   EVAL      H#TOT = 0
     C/EXEC SQL
     C+   INSERT INTO POHDRP (POID, PONUM, SUPPCD, PODATE, DUEDATE, WHID,
     C+          STATUS, TOTAMT, CRTUSER, CRTDATE, CRTTIM, UPDUSER,
     C+          UPDDATE, UPDTIM)
     C+   VALUES (:H#ID, :H#NUM, :H#SUPP, :H#PODT, :H#DUE, :H#WHID,
     C+          'OPEN', 0, :W#USER, :W#D8, :W#T6, :W#USER, :W#D8, :W#T6)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EXSR      @POFAIL
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      H#LINE = 0
     C                   FOR       W#J = W#I TO W#SN
     C                   IF        S_DONE(W#J) OR S_SUPP(W#J) <> H#SUPP OR
     C                             S_WHID(W#J) <> H#WHID
     C                   ITER
     C                   ENDIF
     C                   EVAL      S_DONE(W#J) = *ON
     C                   EVAL      H#LINE = H#LINE + 1
     C                   EVAL      H#ITEMID = S_ITEMID(W#J)
     C                   EVAL      H#QTY = S_QTY(W#J)
     C                   EVAL      H#COST = 0
     C/EXEC SQL
     C+   SELECT STDCOST INTO :H#COST FROM ITEMP WHERE ITEMID = :H#ITEMID
     C/END-EXEC
     C                   EVAL(H)   H#AMT = H#QTY * H#COST
     C                   EVAL      H#TOT = H#TOT + H#AMT
     C/EXEC SQL
     C+   INSERT INTO POLINEP (POID, LINENO, ITEMID, ORDQTY, RCVQTY,
     C+          UNITCOST, LINEAMT)
     C+   VALUES (:H#ID, :H#LINE, :H#ITEMID, :H#QTY, 0, :H#COST, :H#AMT)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EXSR      @POFAIL
     C                   LEAVESR
     C                   ENDIF
     C                   ENDFOR
     C/EXEC SQL
     C+   UPDATE POHDRP SET TOTAMT = :H#TOT WHERE POID = :H#ID
     C/END-EXEC
     C                   EVAL      W#N = W#N + 1
     C                   ENDFOR
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   EVAL      LMSG = %CHAR(W#N) + ' 件の発注を作成' +
     C                             '(入荷予定は7日後)'
     C                   ENDSR

      * @POFAIL - 発注の作成エラー
     C     @POFAIL       BEGSR
     C                   EVAL      PMSG = '発注の登録エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   EVAL      W#ERR = *ON
     C                   ENDSR

      *=====================================================================
      * @EDIT - 新規(A)・表示(V)・入荷(R)
      *=====================================================================
     C     @EDIT         BEGSR
     C                   EVAL      DMSG = *BLANKS
     C                   EVAL      L_ITEM = *BLANKS
     C                   EVAL      L_QTY = 0
     C                   EVAL      L_COST = 0
     C                   EVAL      L_RCV = 0
     C                   EVAL      L_NOW = 0
     C                   EVAL      L_LINE = 0
     C                   EVAL      L_ERR = *BLANKS
     C                   IF        W#MODE = 'A'
     C                   EVAL      DMODE = '発注の登録'
     C                   EVAL      DPONUM = '(登録時に採番)'
     C                   EVAL      DPODT = %DEC(%DATE():*ISO)
     C                   EVAL      DDUE = %DEC(%DATE() + %DAYS(7):*ISO)
     C                   EVAL      DSUPP = *BLANKS
     C                   EVAL      DWHID = 1
     C                   EVAL      H#STS = 'OPEN'
     C                   EXSR      @STSNAME
     C                   ELSE
     C                   EXSR      @HEADER
     C                   IF        W#ERR
     C                   LEAVESR
     C                   ENDIF
     C                   IF        W#MODE = 'R' AND H#STS <> 'OPEN' AND
     C                             H#STS <> 'PARTIAL'
     C                   EVAL      LMSG = '入荷済・取消の発注は入荷できません'
     C                   LEAVESR
     C                   ENDIF
     C                   IF        W#MODE = 'R'
     C                   EVAL      DMODE = '入荷の登録'
     C                   ELSE
     C                   EVAL      DMODE = '発注の表示'
     C                   ENDIF
     C                   EVAL      DPONUM = H#NUM
     C                   EVAL      DPODT = H#PODT
     C                   EVAL      DDUE = H#DUE
     C                   EVAL      DSUPP = H#SUPP
     C                   EVAL      DWHID = H#WHID
     C                   EXSR      @STSNAME
     C                   EVAL      W#I = 0
     C                   EVAL      H#ID = W#SEL
     C/EXEC SQL
     C+   OPEN LINCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#I < 10
     C/EXEC SQL
     C+   FETCH LINCSR INTO :H#LINE, :H#ITEMID, :H#ITEMCD, :H#QTY, :H#RCV,
     C+                     :H#COST
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#I = W#I + 1
     C                   EVAL      L_LINE(W#I) = H#LINE
     C                   EVAL      L_ITEM(W#I) = H#ITEMCD
     C                   EVAL(H)   L_QTY(W#I) = H#QTY
     C                   EVAL(H)   L_RCV(W#I) = H#RCV
     C                   EVAL(H)   L_COST(W#I) = H#COST
     C                   EVAL      L_NOW(W#I) = L_QTY(W#I) - L_RCV(W#I)
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE LINCSR
     C/END-EXEC
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   ENDIF
     C                   EVAL      *IN50 = (W#MODE <> 'A')
     C                   EVAL      *IN53 = (W#MODE <> 'A')
     C                   EVAL      *IN52 = (W#MODE = 'R')
     C                   EXSR      @NAMES
     C                   EXSR      @CALC
     C                   DOW       *ON
     C                   EXSR      @WRTDTL
     C                   WRITE     DFOOT
     C                   EXFMT     DCTL
      * サブファイルのCHAINで標識が上書きされるので先に覚えておく
     C                   EVAL      W#F4 = *IN04
     C                   EVAL      W#F6 = *IN06
     C                   EVAL      DMSG = *BLANKS
     C                   IF        *IN03
     C                   EVAL      W#DONE = *ON
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN12 OR W#MODE = 'V'
     C                   LEAVE
     C                   ENDIF
     C                   EXSR      @RDDTL
     C                   IF        W#MODE = 'R'
     C                   EXSR      @CHKRCV
     C                   IF        W#ERR OR NOT W#F6
     C                   IF        NOT W#ERR
     C                   EVAL      DMSG = '入荷数を確認して F6 で登録します'
     C                   ENDIF
     C                   ITER
     C                   ENDIF
     C                   EXSR      @RECEIVE
     C                   IF        NOT W#ERR
     C                   LEAVE
     C                   ENDIF
     C                   ITER
     C                   ENDIF
     C                   IF        W#F4
     C                   EXSR      @PROMPT
     C                   EXSR      @NAMES
     C                   ITER
     C                   ENDIF
     C                   EXSR      @CHECK
     C                   IF        W#ERR
     C                   ITER
     C                   ENDIF
     C                   EXSR      @CALC
     C                   IF        NOT W#F6
     C                   EVAL      DMSG = '計算しました。F6で登録します'
     C                   ITER
     C                   ENDIF
     C                   EXSR      @SAVE
     C                   IF        NOT W#ERR
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   EVAL      *IN50 = *OFF
     C                   EVAL      *IN52 = *OFF
     C                   EVAL      *IN53 = *OFF
     C                   ENDSR

      * @WRTDTL - 明細の配列を画面へ
     C     @WRTDTL       BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     DCTL
     C                   EVAL      *IN93 = *OFF
     C                   FOR       W#I = 1 TO 10
     C                   EVAL      SNO = W#I
     C                   EVAL      SITEM = L_ITEM(W#I)
     C                   EVAL      SINAME = L_NAME(W#I)
     C                   EVAL      SQTY = L_QTY(W#I)
     C                   EVAL      SCOST = L_COST(W#I)
     C                   IF        L_ITEM(W#I) <> *BLANKS AND W#MODE <> 'A'
     C                   EVAL      W#P0 = L_RCV(W#I)
     C                   EVALR     SRCV = %TRIM(%EDITC(W#P0:'J'))
     C                   ELSE
     C                   EVAL      SRCV = *BLANKS
     C                   ENDIF
     C                   EVAL      SNOW = L_NOW(W#I)
     C                   EVAL      *IN41 = (L_ERR(W#I) = 'I')
     C                   EVAL      *IN42 = (L_ERR(W#I) = 'Q')
     C                   EVAL      *IN43 = (L_ERR(W#I) = 'P')
     C                   EVAL      *IN44 = (L_ERR(W#I) = 'R')
     C                   EVAL      W#DRRN = W#I
     C                   WRITE     DSFL
     C                   ENDFOR
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN42 = *OFF
     C                   EVAL      *IN43 = *OFF
     C                   EVAL      *IN44 = *OFF
     C                   ENDSR

      * @RDDTL - 画面の明細を配列へ
     C     @RDDTL        BEGSR
     C                   FOR       W#I = 1 TO 10
     C     W#I           CHAIN     DSFL
     C                   IF        %FOUND(POENTRYD)
     C                   IF        W#MODE = 'A'
     C                   EVAL      L_ITEM(W#I) = SITEM
     C                   EVAL      L_QTY(W#I) = SQTY
     C                   EVAL      L_COST(W#I) = SCOST
     C                   ELSE
     C                   EVAL      L_NOW(W#I) = SNOW
     C                   ENDIF
     C                   ENDIF
     C                   ENDFOR
     C                   ENDSR

      * @PROMPT - F4(仕入先・商品)
     C     @PROMPT       BEGSR
     C                   SELECT
     C                   WHEN      CSRFLD = 'DSUPP'
     C                   EVAL      W#PVAL = DSUPP
     C                   CALLP     O2CPRMPT('CODE':'SUPPLIER':W#PVAL)
     C                   EVAL      DSUPP = W#PVAL
     C                   WHEN      CSRFLD = 'SITEM' AND CSRRRN >= 1 AND
     C                             CSRRRN <= 10
     C                   EVAL      W#PVAL = L_ITEM(CSRRRN)
     C                   CALLP     O2CPRMPT('ITEM':' ':W#PVAL)
     C                   EVAL      L_ITEM(CSRRRN) = W#PVAL
     C                   OTHER
     C                   EVAL      DMSG = '仕入先か商品コードにカーソルを' +
     C                             '置いてF4'
     C                   ENDSL
     C                   ENDSR

      * @NAMES - 仕入先名・倉庫名・商品名
     C     @NAMES        BEGSR
     C                   EVAL      DSUPNM = *BLANKS
     C                   EVAL      DWHNM = *BLANKS
     C                   EVAL      W#DESC = *BLANKS
     C                   IF        DSUPP <> *BLANKS
     C                   EVAL      H#SUPP = DSUPP
     C/EXEC SQL
     C+   SELECT CAST(DESC AS VARCHAR(60) FOR BIT DATA) INTO :W#DESC
     C+     FROM CODEMSTRP WHERE CODETYPE = 'SUPPLIER' AND CODEVAL = :H#SUPP
     C/END-EXEC
     C                   IF        SQLCODE = 0
     C                   EVAL      DSUPNM = DBCSTRUNC(%TRIMR(W#DESC):30)
     C                   ELSE
     C                   EVAL      DSUPNM = '(仕入先なし)'
     C                   ENDIF
     C                   ENDIF
     C                   EVAL      W#WSTS = *BLANKS
     C                   EVAL      H#WHID = DWHID
     C/EXEC SQL
     C+   SELECT WHNM, STATUS INTO :H#NM, :W#WSTS
     C+     FROM WAREHOUSEP WHERE WHID = :H#WHID
     C/END-EXEC
     C                   IF        SQLCODE = 0
     C                   EVAL      DWHNM = DBCSTRUNC(%TRIMR(H#NM):16)
     C                   ELSE
     C                   EVAL      W#WSTS = *BLANKS
     C                   ENDIF
     C                   FOR       W#I = 1 TO 10
     C                   EVAL      L_NAME(W#I) = *BLANKS
     C                   EVAL      L_ITEMID(W#I) = 0
     C                   IF        L_ITEM(W#I) <> *BLANKS
     C                   EVAL      H#ITEMCD = L_ITEM(W#I)
     C/EXEC SQL
     C+   SELECT ITEMID, ITEMDSC INTO :H#ITEMID, :H#NM
     C+     FROM ITEMP WHERE ITEMCD = :H#ITEMCD
     C/END-EXEC
     C                   IF        SQLCODE = 0
     C                   EVAL      L_ITEMID(W#I) = H#ITEMID
     C                   EVAL      L_NAME(W#I) = DBCSTRUNC(%TRIMR(H#NM):16)
     C                   ENDIF
     C                   ENDIF
     C                   ENDFOR
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   ENDSR

      * @CHECK - 新規の入力チェック(単価0は標準原価)
     C     @CHECK        BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      *IN31 = *OFF
     C                   EVAL      *IN32 = *OFF
     C                   EVAL      *IN33 = *OFF
     C                   EVAL      *IN34 = *OFF
     C                   EVAL      L_ERR = *BLANKS
     C                   EXSR      @NAMES
     C     *ISO          TEST(DE)                DPODT
     C                   IF        %ERROR
     C                   EVAL      *IN32 = *ON
     C                   EVAL      DMSG = '発注日が正しくありません'
     C                   ENDIF
     C                   IF        DMSG = *BLANKS
     C     *ISO          TEST(DE)                DDUE
     C                   IF        %ERROR
     C                   EVAL      *IN33 = *ON
     C                   EVAL      DMSG = '入荷予定日が正しくありません'
     C                   ENDIF
     C                   ENDIF
     C                   IF        DMSG = *BLANKS
     C                   SELECT
     C                   WHEN      DSUPP = *BLANKS OR W#DESC = *BLANKS
     C                   EVAL      *IN31 = *ON
     C                   EVAL      DMSG = '仕入先を入力してください'
     C                   WHEN      W#WSTS <> 'ACTIVE'
     C                   EVAL      *IN34 = *ON
     C                   EVAL      DMSG = '倉庫が見つからないか停止中です'
     C                   WHEN      DDUE < DPODT
     C                   EVAL      *IN33 = *ON
     C                   EVAL      DMSG = '入荷予定日は発注日以降にして' +
     C                             'ください'
     C                   ENDSL
     C                   ENDIF
     C                   IF        DMSG <> *BLANKS
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#N = 0
     C                   FOR       W#I = 1 TO 10
     C                   IF        L_ITEM(W#I) = *BLANKS
     C                   IF        L_QTY(W#I) <> 0 OR L_COST(W#I) <> 0
     C                   EVAL      L_ERR(W#I) = 'I'
     C                   EVAL      DMSG = %CHAR(W#I) + '行目: 商品コード' +
     C                             'を入力してください'
     C                   LEAVE
     C                   ENDIF
     C                   ITER
     C                   ENDIF
     C                   IF        L_ITEMID(W#I) = 0
     C                   EVAL      L_ERR(W#I) = 'I'
     C                   EVAL      DMSG = %CHAR(W#I) + '行目: 商品が' +
     C                             '見つかりません'
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      H#ITEMID = L_ITEMID(W#I)
     C/EXEC SQL
     C+   SELECT STATUS, STDCOST INTO :W#ISTS, :W#STD
     C+     FROM ITEMP WHERE ITEMID = :H#ITEMID
     C/END-EXEC
     C                   IF        W#ISTS <> 'ACTIVE'
     C                   EVAL      L_ERR(W#I) = 'I'
     C                   EVAL      DMSG = %CHAR(W#I) + '行目: 販売停止の' +
     C                             '商品です'
     C                   LEAVE
     C                   ENDIF
     C                   IF        L_QTY(W#I) <= 0
     C                   EVAL      L_ERR(W#I) = 'Q'
     C                   EVAL      DMSG = %CHAR(W#I) + '行目: 発注数を' +
     C                             '入力してください'
     C                   LEAVE
     C                   ENDIF
     C                   IF        L_COST(W#I) < 0
     C                   EVAL      L_ERR(W#I) = 'P'
     C                   EVAL      DMSG = %CHAR(W#I) + '行目: 仕入単価が' +
     C                             '正しくありません'
     C                   LEAVE
     C                   ENDIF
     C                   IF        L_COST(W#I) = 0
     C                   EVAL(H)   L_COST(W#I) = W#STD
     C                   ENDIF
     C                   EVAL      W#N = W#N + 1
     C                   ENDFOR
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   IF        DMSG = *BLANKS AND W#N = 0
     C                   EVAL      L_ERR(1) = 'I'
     C                   EVAL      DMSG = '明細を1行以上入力してください'
     C                   ENDIF
     C                   IF        DMSG <> *BLANKS
     C                   EVAL      W#ERR = *ON
     C                   ENDIF
     C                   ENDSR

      * @CALC - 合計
     C     @CALC         BEGSR
     C                   EVAL      W#TOT = 0
     C                   FOR       W#I = 1 TO 10
     C                   IF        L_ITEM(W#I) <> *BLANKS
     C                   EVAL(H)   W#TOT = W#TOT + L_QTY(W#I) * L_COST(W#I)
     C                   ENDIF
     C                   ENDFOR
     C                   EVAL      W#P2 = W#TOT
     C                   EVALR     DTOT = %TRIM(%EDITC(W#P2:'J'))
     C                   ENDSR

      * @SAVE - 新規の発注を登録
     C     @SAVE         BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EXSR      @STAMP
     C/EXEC SQL
     C+   VALUES NEXT VALUE FOR O2C_POID_SEQ INTO :H#ID
     C/END-EXEC
     C                   EVAL      H#NUM = 'PO-' + %CHAR(H#ID)
     C                   EVAL      H#SUPP = DSUPP
     C                   EVAL      H#PODT = DPODT
     C                   EVAL      H#DUE = DDUE
     C                   EVAL      H#WHID = DWHID
     C                   EVAL      H#TOT = W#TOT
     C/EXEC SQL
     C+   INSERT INTO POHDRP (POID, PONUM, SUPPCD, PODATE, DUEDATE, WHID,
     C+          STATUS, TOTAMT, CRTUSER, CRTDATE, CRTTIM, UPDUSER,
     C+          UPDDATE, UPDTIM)
     C+   VALUES (:H#ID, :H#NUM, :H#SUPP, :H#PODT, :H#DUE, :H#WHID,
     C+          'OPEN', :H#TOT, :W#USER, :W#D8, :W#T6, :W#USER, :W#D8,
     C+          :W#T6)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EXSR      @SAVEERR
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      H#LINE = 0
     C                   FOR       W#I = 1 TO 10
     C                   IF        L_ITEM(W#I) = *BLANKS
     C                   ITER
     C                   ENDIF
     C                   EVAL      H#LINE = H#LINE + 1
     C                   EVAL      H#ITEMID = L_ITEMID(W#I)
     C                   EVAL      H#QTY = L_QTY(W#I)
     C                   EVAL      H#COST = L_COST(W#I)
     C                   EVAL(H)   H#AMT = H#QTY * H#COST
     C/EXEC SQL
     C+   INSERT INTO POLINEP (POID, LINENO, ITEMID, ORDQTY, RCVQTY,
     C+          UNITCOST, LINEAMT)
     C+   VALUES (:H#ID, :H#LINE, :H#ITEMID, :H#QTY, 0, :H#COST, :H#AMT)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EXSR      @SAVEERR
     C                   LEAVESR
     C                   ENDIF
     C                   ENDFOR
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   EVAL      LMSG = %TRIM(H#NUM) + ' を登録しました'
     C                   ENDSR

      * @SAVEERR - 登録エラー
     C     @SAVEERR      BEGSR
     C                   EVAL      DMSG = '登録エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   EVAL      W#ERR = *ON
     C                   ENDSR

      * @CHKRCV - 今回入荷数のチェック(0～発注残)
     C     @CHKRCV       BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      W#ANY = *OFF
     C                   EVAL      L_ERR = *BLANKS
     C                   FOR       W#I = 1 TO 10
     C                   IF        L_ITEM(W#I) = *BLANKS
     C                   ITER
     C                   ENDIF
     C                   IF        L_NOW(W#I) < 0 OR
     C                             L_NOW(W#I) > L_QTY(W#I) - L_RCV(W#I)
     C                   EVAL      L_ERR(W#I) = 'R'
     C                   EVAL      DMSG = %CHAR(W#I) + '行目: 今回入荷は' +
     C                             '発注残以下にしてください'
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   IF        L_NOW(W#I) > 0
     C                   EVAL      W#ANY = *ON
     C                   ENDIF
     C                   ENDFOR
     C                   IF        NOT W#ANY
     C                   EVAL      DMSG = '今回入荷数を入力してください'
     C                   EVAL      W#ERR = *ON
     C                   ENDIF
     C                   ENDSR

      * @RECEIVE - 入荷の登録(発注明細・在庫・在庫移動・発注の状態)
     C     @RECEIVE      BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EXSR      @STAMP
     C                   EVAL      H#ID = W#SEL
     C                   FOR       W#I = 1 TO 10
     C                   IF        L_ITEM(W#I) = *BLANKS OR L_NOW(W#I) = 0
     C                   ITER
     C                   ENDIF
     C                   EVAL      H#LINE = L_LINE(W#I)
     C                   EVAL      H#QTY = L_NOW(W#I)
     C                   EVAL      H#ITEMID = L_ITEMID(W#I)
      * 同時に入荷された場合に備えて発注残を条件にする
     C/EXEC SQL
     C+   UPDATE POLINEP SET RCVQTY = RCVQTY + :H#QTY
     C+    WHERE POID = :H#ID AND LINENO = :H#LINE
     C+      AND RCVQTY + :H#QTY <= ORDQTY
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      DMSG = %CHAR(W#I) + '行目: 発注明細の' +
     C                             '更新エラー SQLCODE=' + %CHAR(SQLCODE)
     C                   EXSR      @RCVERR
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      H#WHID = DWHID
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
     C                   EVAL      DMSG = '在庫の更新エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EXSR      @RCVERR
     C                   LEAVESR
     C                   ENDIF
     C/EXEC SQL
     C+   SELECT QTYOH INTO :H#AFT FROM INVENTORYP
     C+    WHERE WHID = :H#WHID AND ITEMID = :H#ITEMID
     C/END-EXEC
     C                   EVAL      H#BEF = H#AFT - H#QTY
     C                   EVAL      H#NUM = DPONUM
     C/EXEC SQL
     C+   INSERT INTO STKMOVEP (MOVID, WHID, ITEMID, MOVTYPE, MOVQTY,
     C+          BEFQTY, AFTQTY, REFNO, MOVDATE, CRTUSER, CRTDATE, CRTTIM)
     C+   VALUES (NEXT VALUE FOR O2C_MOVID_SEQ, :H#WHID, :H#ITEMID,
     C+          'RECEIPT', :H#QTY, :H#BEF, :H#AFT, :H#NUM, :W#D8,
     C+          :W#USER, :W#D8, :W#T6)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      DMSG = '在庫移動の登録エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EXSR      @RCVERR
     C                   LEAVESR
     C                   ENDIF
     C                   ENDFOR
      * 発注の状態
     C                   EVAL      W#N = 0
     C/EXEC SQL
     C+   SELECT COUNT(*) INTO :W#N FROM POLINEP
     C+    WHERE POID = :H#ID AND RCVQTY < ORDQTY
     C/END-EXEC
     C                   IF        W#N = 0
     C                   EVAL      H#STS = 'RECEIVED'
     C                   ELSE
     C                   EVAL      H#STS = 'PARTIAL'
     C                   ENDIF
     C/EXEC SQL
     C+   UPDATE POHDRP SET STATUS = :H#STS, UPDUSER = :W#USER,
     C+          UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHERE POID = :H#ID
     C/END-EXEC
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   EXSR      @STSNAME
     C                   EVAL      LMSG = %TRIM(DPONUM) + ' の入荷を登録' +
     C                             'しました(' + %TRIM(DSTS) + ')'
     C                   ENDSR

      * @RCVERR - 入荷の登録エラー
     C     @RCVERR       BEGSR
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   EVAL      W#ERR = *ON
     C                   ENDSR

      * @STAMP - 更新日時
     C     @STAMP        BEGSR
     C                   EVAL      W#D8 = %DEC(%DATE():*ISO)
     C                   EVAL      W#T6 = %DEC(%TIME():*HMS)
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
