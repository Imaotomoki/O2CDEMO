     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: ITEMMNT
      * 説明: 商品マスターの保守(登録・変更・停止/再開)
      * 作成: 修正#MST 新規作成。マスター保守メニューから呼ぶ
      *   一覧: 商品コードの前方一致。2=変更 4=停止・再開 F6=新規
      *   登録: 商品コードは重複不可(変更では変えられない)
      *         税区分は TAX_CAT(空白=顧客の税区分)
      *         仕入先は SUPPLIER のコード(空白可)
      *         発注点・発注数量は発注点アラートで使う(0=発注しない)
      *   商品IDは既存の最大値+1
      *=====================================================================
     FITEMMNTD  CF   E             WORKSTN SFILE(LSFL:W#RRN)

     D W#RRN           S              4S 0
     D W#CNT           S              4S 0
     D W#SQLCD         S             10I 0
     D W#DONE          S               N
     D W#ERR           S               N
     D W#ADD           S               N
     D W#PVAL          S             30A
     D W#LIKE          S             13A   VARYING
     D W#SEL           S              9P 0
     D W#OPT           S              1A
     D W#N             S             10I 0
     D W#ID            S              9P 0
     D W#USER          S             10A   INZ(*USER)
     D W#D8            S              8P 0
     D W#T6            S              6P 0
     D W#STS           S             10A
     D W#DESC          S            120A
      * 一覧の読み込み
     D S#ID            S              9P 0
     D S#CD            S             12A
     D S#NM            S             40A
     D S#PRC           S             15P 4
     D S#TAX           S             10A
     D S#STS           S             10A
      * 登録・変更の値
     D H#CD            S             12A
     D H#NM            S             40A
     D H#UOM           S             10A
     D H#CAT           S             30A
     D H#PRC           S             15P 4
     D H#COST          S             15P 4
     D H#TAX           S             10A
     D H#ROP           S             15P 3
     D H#ROQ           S             15P 3
     D H#SUPP          S             20A
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
     C+   SET OPTION COMMIT = *NONE, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE LSTCSR CURSOR FOR
     C+   SELECT ITEMID, ITEMCD, ITEMDSC, UNITPRC, TAXCAT, STATUS
     C+     FROM ITEMP WHERE ITEMCD LIKE :W#LIKE
     C+    ORDER BY ITEMCD FOR READ ONLY
     C/END-EXEC

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
     C                   IF        *IN04
     C                   EVAL      W#PVAL = LFITEM
     C                   CALLP     O2CPRMPT('ITEM':' ':W#PVAL)
     C                   EVAL      LFITEM = W#PVAL
     C                   EXSR      @LOAD
     C                   ITER
     C                   ENDIF
     C                   IF        *IN06
     C                   EVAL      W#ADD = *ON
     C                   EVAL      W#SEL = 0
     C                   EXSR      @EDIT
     C                   EXSR      @LOAD
     C                   ITER
     C                   ENDIF
     C                   EVAL      W#SEL = 0
     C                   IF        W#CNT > 0
     C                   READC     LSFL
     C                   DOW       NOT %EOF(ITEMMNTD)
     C                   IF        LOPT <> ' ' AND W#SEL = 0
     C                   EVAL      W#SEL = LITEMID
     C                   EVAL      W#OPT = LOPT
     C                   ENDIF
     C                   READC     LSFL
     C                   ENDDO
     C                   ENDIF
     C                   IF        W#SEL > 0
     C                   IF        W#OPT = '2'
     C                   EVAL      W#ADD = *OFF
     C                   EXSR      @EDIT
     C                   ELSE
     C                   EXSR      @TOGGLE
     C                   ENDIF
     C                   ENDIF
     C                   EXSR      @LOAD
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C                   RETURN

      *=====================================================================
      * @LOAD - 商品の一覧を読み込む
      *=====================================================================
     C     @LOAD         BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     LCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#CNT = 0
     C                   EVAL      W#LIKE = %TRIM(LFITEM) + '%'
     C/EXEC SQL
     C+   OPEN LSTCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#CNT < 999
     C/EXEC SQL
     C+   FETCH LSTCSR INTO :S#ID, :S#CD, :S#NM, :S#PRC, :S#TAX, :S#STS
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      LOPT = ' '
     C                   EVAL      LITEMID = S#ID
     C                   EVAL      LITEM = S#CD
     C                   EVAL      LNAME = DBCSTRUNC(%TRIMR(S#NM):28)
     C                   MONITOR
     C                   EVALR     LPRC = %EDITC(%DEC(S#PRC:13:2):'J')
     C                   ON-ERROR
     C                   EVAL      LPRC = *ALL'*'
     C                   ENDMON
     C                   EVAL      LTAX = S#TAX
     C                   IF        S#STS = 'ACTIVE'
     C                   EVAL      LSTS = '販売中'
     C                   EVAL      *IN61 = *OFF
     C                   ELSE
     C                   EVAL      LSTS = '停止'
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
     C                   EVAL      LMSG = '該当する商品がありません'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @TOGGLE - 停止・再開(販売中 <-> 停止)
      *=====================================================================
     C     @TOGGLE       BEGSR
     C                   EXSR      @STAMP
     C/EXEC SQL
     C+   UPDATE ITEMP
     C+      SET STATUS = CASE WHEN STATUS = 'ACTIVE' THEN 'INACTIVE'
     C+                        ELSE 'ACTIVE' END,
     C+          UPDUSER = :W#USER, UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHERE ITEMID = :W#SEL
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      LMSG = '更新エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   ELSE
     C                   EVAL      LMSG = '商品の状態を切り替えました'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @EDIT - 登録(W#ADD=*ON)・変更
      *=====================================================================
     C     @EDIT         BEGSR
     C                   EVAL      *IN50 = NOT W#ADD
     C                   EVAL      DMSG = *BLANKS
     C                   IF        W#ADD
     C                   EVAL      DMODE = '商品の登録'
     C                   EVAL      DITEMCD = *BLANKS
     C                   EVAL      DITEMNM = *BLANKS
     C                   EVAL      DUOM = 'EA'
     C                   EVAL      DCAT = *BLANKS
     C                   EVAL      DPRC = 0
     C                   EVAL      DCOST = 0
     C                   EVAL      DTAXCAT = *BLANKS
     C                   EVAL      DROP = 0
     C                   EVAL      DROQ = 0
     C                   EVAL      DSUPP = *BLANKS
     C                   EVAL      DSTS = '販売中'
     C                   ELSE
     C                   EVAL      DMODE = '商品の変更'
     C/EXEC SQL
     C+   SELECT ITEMCD, ITEMDSC, UOM, CATEGORY, UNITPRC, STDCOST, TAXCAT,
     C+          REORDPT, REORDQTY, SUPPCD, STATUS
     C+     INTO :H#CD, :H#NM, :H#UOM, :H#CAT, :H#PRC, :H#COST, :H#TAX,
     C+          :H#ROP, :H#ROQ, :H#SUPP, :W#STS
     C+     FROM ITEMP WHERE ITEMID = :W#SEL
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      LMSG = '商品が見つかりません'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      DITEMCD = H#CD
     C                   EVAL      DITEMNM = H#NM
     C                   EVAL      DUOM = H#UOM
     C                   EVAL      DCAT = H#CAT
     C                   EVAL(H)   DPRC = H#PRC
     C                   EVAL(H)   DCOST = H#COST
     C                   EVAL      DTAXCAT = H#TAX
     C                   EVAL(H)   DROP = H#ROP
     C                   EVAL(H)   DROQ = H#ROQ
     C                   EVAL      DSUPP = H#SUPP
     C                   IF        W#STS = 'ACTIVE'
     C                   EVAL      DSTS = '販売中'
     C                   ELSE
     C                   EVAL      DSTS = '停止'
     C                   ENDIF
     C                   ENDIF
     C                   EXSR      @NAMES
     C                   DOW       *ON
     C                   EXFMT     DTL
     C                   EVAL      DMSG = *BLANKS
     C                   IF        *IN03
     C                   EVAL      W#DONE = *ON
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN12
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN04
     C                   EXSR      @PROMPT
     C                   ITER
     C                   ENDIF
     C                   EXSR      @CHECK
     C                   IF        W#ERR
     C                   ITER
     C                   ENDIF
     C                   EXSR      @SAVE
     C                   IF        NOT W#ERR
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   EVAL      *IN50 = *OFF
     C                   ENDSR

      * @PROMPT - F4(カーソル位置の項目を検索)
     C     @PROMPT       BEGSR
     C                   SELECT
     C                   WHEN      CSRFLD = 'DTAXCAT'
     C                   EVAL      W#PVAL = DTAXCAT
     C                   CALLP     O2CPRMPT('CODE':'TAX_CAT':W#PVAL)
     C                   EVAL      DTAXCAT = W#PVAL
     C                   WHEN      CSRFLD = 'DSUPP'
     C                   EVAL      W#PVAL = DSUPP
     C                   CALLP     O2CPRMPT('CODE':'SUPPLIER':W#PVAL)
     C                   EVAL      DSUPP = W#PVAL
     C                   OTHER
     C                   EVAL      DMSG = '税区分か仕入先にカーソルを置いてF4'
     C                   ENDSL
     C                   EXSR      @NAMES
     C                   ENDSR

      * @NAMES - 税区分・仕入先の名称
     C     @NAMES        BEGSR
     C                   EVAL      DTAXNM = *BLANKS
     C                   EVAL      DSUPNM = *BLANKS
     C                   IF        DTAXCAT <> *BLANKS
     C/EXEC SQL
     C+   SELECT DESC INTO :W#DESC FROM CODEMSTRP
     C+    WHERE CODETYPE = 'TAX_CAT' AND CODEVAL = :DTAXCAT
     C/END-EXEC
     C                   IF        SQLCODE = 0
     C                   EVAL      DTAXNM = DBCSTRUNC(%TRIMR(W#DESC):30)
     C                   ENDIF
     C                   ENDIF
     C                   IF        DSUPP <> *BLANKS
     C/EXEC SQL
     C+   SELECT DESC INTO :W#DESC FROM CODEMSTRP
     C+    WHERE CODETYPE = 'SUPPLIER' AND CODEVAL = :DSUPP
     C/END-EXEC
     C                   IF        SQLCODE = 0
     C                   EVAL      DSUPNM = DBCSTRUNC(%TRIMR(W#DESC):30)
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @CHECK - 入力チェック
      *=====================================================================
     C     @CHECK        BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      *IN31 = *OFF
     C                   EVAL      *IN32 = *OFF
     C                   EVAL      *IN33 = *OFF
     C                   EVAL      *IN34 = *OFF
     C                   EVAL      *IN35 = *OFF
     C                   EVAL      *IN36 = *OFF
     C                   EVAL      *IN37 = *OFF
     C                   EVAL      *IN38 = *OFF
     C                   EXSR      @NAMES
     C                   SELECT
     C                   WHEN      DITEMCD = *BLANKS
     C                   EVAL      *IN31 = *ON
     C                   EVAL      DMSG = '商品コードを入力してください'
     C                   WHEN      %SCAN(' ':%TRIM(DITEMCD)) > 0
     C                   EVAL      *IN31 = *ON
     C                   EVAL      DMSG = '商品コードに空白は使えません'
     C                   WHEN      DITEMNM = *BLANKS
     C                   EVAL      *IN32 = *ON
     C                   EVAL      DMSG = '商品名を入力してください'
     C                   WHEN      DPRC < 0
     C                   EVAL      *IN33 = *ON
     C                   EVAL      DMSG = '標準単価にマイナスは使えません'
     C                   WHEN      DCOST < 0
     C                   EVAL      *IN34 = *ON
     C                   EVAL      DMSG = '標準原価にマイナスは使えません'
     C                   WHEN      DTAXCAT <> *BLANKS AND DTAXNM = *BLANKS
     C                   EVAL      *IN35 = *ON
     C                   EVAL      DMSG = '税区分が登録されていません(F4)'
     C                   WHEN      DROP < 0
     C                   EVAL      *IN36 = *ON
     C                   EVAL      DMSG = '発注点にマイナスは使えません'
     C                   WHEN      DROQ < 0 OR (DROP > 0 AND DROQ = 0)
     C                   EVAL      *IN37 = *ON
     C                   EVAL      DMSG = '発注点を入れたら発注数量も入力'
     C                   WHEN      DSUPP <> *BLANKS AND DSUPNM = *BLANKS
     C                   EVAL      *IN38 = *ON
     C                   EVAL      DMSG = '仕入先が登録されていません(F4)'
     C                   ENDSL
     C                   IF        DMSG <> *BLANKS
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   IF        W#ADD
     C/EXEC SQL
     C+   SELECT COUNT(*) INTO :W#N FROM ITEMP WHERE ITEMCD = :DITEMCD
     C/END-EXEC
     C                   IF        SQLCODE <> 0 OR W#N > 0
     C                   EVAL      *IN31 = *ON
     C                   EVAL      DMSG = 'この商品コードは登録済みです'
     C                   EVAL      W#ERR = *ON
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @SAVE - 登録・更新
      *=====================================================================
     C     @SAVE         BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EXSR      @STAMP
     C                   EVAL      H#CD = DITEMCD
     C                   EVAL      H#NM = DITEMNM
     C                   EVAL      H#UOM = DUOM
     C                   EVAL      H#CAT = DCAT
     C                   EVAL      H#PRC = DPRC
     C                   EVAL      H#COST = DCOST
     C                   EVAL      H#TAX = DTAXCAT
     C                   EVAL      H#ROP = DROP
     C                   EVAL      H#ROQ = DROQ
     C                   EVAL      H#SUPP = DSUPP
     C                   IF        W#ADD
     C/EXEC SQL
     C+   SELECT COALESCE(MAX(ITEMID), 0) + 1 INTO :W#ID FROM ITEMP
     C/END-EXEC
     C/EXEC SQL
     C+   INSERT INTO ITEMP (ITEMID, ITEMCD, ITEMDSC, UOM, UNITPRC, CATEGORY,
     C+          STATUS, CRTUSER, CRTDATE, CRTTIM, UPDUSER, UPDDATE, UPDTIM,
     C+          TAXCAT, STDCOST, REORDPT, REORDQTY, SUPPCD)
     C+   VALUES (:W#ID, :H#CD, :H#NM, :H#UOM, :H#PRC, :H#CAT, 'ACTIVE',
     C+          :W#USER, :W#D8, :W#T6, :W#USER, :W#D8, :W#T6, :H#TAX,
     C+          :H#COST, :H#ROP, :H#ROQ, :H#SUPP)
     C/END-EXEC
     C                   ELSE
     C/EXEC SQL
     C+   UPDATE ITEMP
     C+      SET ITEMDSC = :H#NM, UOM = :H#UOM, CATEGORY = :H#CAT,
     C+          UNITPRC = :H#PRC, STDCOST = :H#COST, TAXCAT = :H#TAX,
     C+          REORDPT = :H#ROP, REORDQTY = :H#ROQ, SUPPCD = :H#SUPP,
     C+          UPDUSER = :W#USER, UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHERE ITEMID = :W#SEL
     C/END-EXEC
     C                   ENDIF
     C                   IF        SQLCODE <> 0
     C                   EVAL      DMSG = '登録エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   IF        W#ADD
     C                   EVAL      LMSG = %TRIM(H#CD) + ' を登録しました'
     C                   ELSE
     C                   EVAL      LMSG = %TRIM(H#CD) + ' を変更しました'
     C                   ENDIF
     C                   EVAL      LFITEM = H#CD
     C                   ENDSR

      * @STAMP - 更新日時
     C     @STAMP        BEGSR
     C                   EVAL      W#D8 = %DEC(%DATE():*ISO)
     C                   EVAL      W#T6 = %DEC(%TIME():*HMS)
     C                   ENDSR

      *=====================================================================
      * DbcsTrunc - 全角(SO/SI)を壊さずに指定バイト数以内に切り詰める
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
