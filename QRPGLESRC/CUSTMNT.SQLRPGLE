     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: CUSTMNT
      * 説明: 顧客マスターの保守(登録・変更・停止/再開)
      * 作成: 修正#MST 新規作成。マスター保守メニューから呼ぶ
      *   一覧: 顧客コードの前方一致。2=変更 4=停止・再開 F6=新規
      *   登録: 顧客コードは重複不可(変更では変えられない)
      *         税区分は TAX_CAT、通貨は CURRENCY のコードだけ
      *         締日は CUSTCLSP に保存(0=末日)
      *   顧客IDは既存の最大値+1
      *=====================================================================
     FCUSTMNTD  CF   E             WORKSTN SFILE(LSFL:W#RRN)

     D W#RRN           S              4S 0
     D W#CNT           S              4S 0
     D W#SQLCD         S             10I 0
     D W#DONE          S               N
     D W#ERR           S               N
     D W#ADD           S               N
     D W#PVAL          S             30A
     D W#LIKE          S             21A   VARYING
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
     D S#CD            S             20A
     D S#NM            S             60A
     D S#CUR           S              3A
     D S#TAX           S             10A
     D S#STS           S             10A
      * 登録・変更の値
     D H#CD            S             20A
     D H#NM            S             60A
     D H#BILL          S             60A
     D H#SHIP          S             60A
     D H#KANA          S             40A
     D H#TAX           S             10A
     D H#CUR           S              3A
     D H#CRED          S             15P 2
     D H#CLS           S              2P 0
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
     C+   SELECT CUSTID, CUSTCD, CUSTNM, CURRCD, TAXCAT, STATUS
     C+     FROM CUSTOMERP WHERE CUSTCD LIKE :W#LIKE
     C+    ORDER BY CUSTCD FOR READ ONLY
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
     C                   EVAL      LMSG = *BLANKS
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN04
     C                   EVAL      W#PVAL = LFCUST
     C                   CALLP     O2CPRMPT('CUST':' ':W#PVAL)
     C                   EVAL      LFCUST = W#PVAL
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
      * 選択(最初の1件だけ処理する)
     C                   EVAL      W#SEL = 0
     C                   IF        W#CNT > 0
     C                   READC     LSFL
     C                   DOW       NOT %EOF(CUSTMNTD)
     C                   IF        LOPT <> ' ' AND W#SEL = 0
     C                   EVAL      W#SEL = LCUSTID
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
      * @LOAD - 顧客の一覧を読み込む
      *=====================================================================
     C     @LOAD         BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     LCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#CNT = 0
     C                   EVAL      W#LIKE = %TRIM(LFCUST) + '%'
     C/EXEC SQL
     C+   OPEN LSTCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#CNT < 999
     C/EXEC SQL
     C+   FETCH LSTCSR INTO :S#ID, :S#CD, :S#NM, :S#CUR, :S#TAX, :S#STS
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      LOPT = ' '
     C                   EVAL      LCUSTID = S#ID
     C                   EVAL      LCUST = S#CD
     C                   EVAL      LNAME = DBCSTRUNC(%TRIMR(S#NM):30)
     C                   EVAL      LCUR = S#CUR
     C                   EVAL      LTAX = S#TAX
     C                   IF        S#STS = 'ACTIVE'
     C                   EVAL      LSTS = '取引中'
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
     C                   EVAL      LMSG = '該当する顧客がありません'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @TOGGLE - 停止・再開(取引中 <-> 停止)
      *=====================================================================
     C     @TOGGLE       BEGSR
     C                   EXSR      @STAMP
     C/EXEC SQL
     C+   UPDATE CUSTOMERP
     C+      SET STATUS = CASE WHEN STATUS = 'ACTIVE' THEN 'INACTIVE'
     C+                        ELSE 'ACTIVE' END,
     C+          UPDUSER = :W#USER, UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHERE CUSTID = :W#SEL
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      LMSG = '更新エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   ELSE
     C                   EVAL      LMSG = '顧客の状態を切り替えました'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @EDIT - 登録(W#ADD=*ON)・変更
      *=====================================================================
     C     @EDIT         BEGSR
     C                   EVAL      *IN50 = NOT W#ADD
     C                   EVAL      DMSG = *BLANKS
     C                   IF        W#ADD
     C                   EVAL      DMODE = '顧客の登録'
     C                   EVAL      DCUSTCD = *BLANKS
     C                   EVAL      DCUSTNM = *BLANKS
     C                   EVAL      DBILL = *BLANKS
     C                   EVAL      DSHIP = *BLANKS
     C                   EVAL      DKANA = *BLANKS
     C                   EVAL      DTAXCAT = 'STANDARD'
     C                   EVAL      DCURR = 'JPY'
     C                   EVAL      DCRED = 0
     C                   EVAL      DCLSDAY = 0
     C                   EVAL      DSTS = '取引中'
     C                   ELSE
     C                   EVAL      DMODE = '顧客の変更'
     C/EXEC SQL
     C+   SELECT C.CUSTCD, C.CUSTNM, C.BILLADR, C.SHIPADR, C.CUSTKANA,
     C+          C.TAXCAT, C.CURRCD, C.CREDLMT, C.STATUS,
     C+          COALESCE(K.CLOSEDAY, 0)
     C+     INTO :H#CD, :H#NM, :H#BILL, :H#SHIP, :H#KANA, :H#TAX, :H#CUR,
     C+          :H#CRED, :W#STS, :H#CLS
     C+     FROM CUSTOMERP C LEFT JOIN CUSTCLSP K ON K.CUSTID = C.CUSTID
     C+    WHERE C.CUSTID = :W#SEL
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      LMSG = '顧客が見つかりません'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      DCUSTCD = H#CD
     C                   EVAL      DCUSTNM = H#NM
     C                   EVAL      DBILL = H#BILL
     C                   EVAL      DSHIP = H#SHIP
     C                   EVAL      DKANA = H#KANA
     C                   EVAL      DTAXCAT = H#TAX
     C                   EVAL      DCURR = H#CUR
     C                   EVAL      DCRED = H#CRED
     C                   EVAL      DCLSDAY = H#CLS
     C                   IF        W#STS = 'ACTIVE'
     C                   EVAL      DSTS = '取引中'
     C                   ELSE
     C                   EVAL      DSTS = '停止'
     C                   ENDIF
     C                   ENDIF
     C                   EXSR      @TAXNM
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
     C                   EXSR      @TAXNM
     C                   WHEN      CSRFLD = 'DCURR'
     C                   EVAL      W#PVAL = DCURR
     C                   CALLP     O2CPRMPT('CODE':'CURRENCY':W#PVAL)
     C                   EVAL      DCURR = W#PVAL
     C                   OTHER
     C                   EVAL      DMSG = '税区分か通貨にカーソルを置いてF4'
     C                   ENDSL
     C                   ENDSR

      * @TAXNM - 税区分の名称
     C     @TAXNM        BEGSR
     C                   EVAL      DTAXNM = *BLANKS
     C/EXEC SQL
     C+   SELECT DESC INTO :W#DESC FROM CODEMSTRP
     C+    WHERE CODETYPE = 'TAX_CAT' AND CODEVAL = :DTAXCAT
     C/END-EXEC
     C                   IF        SQLCODE = 0
     C                   EVAL      DTAXNM = DBCSTRUNC(%TRIMR(W#DESC):30)
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
     C                   EXSR      @TAXNM
     C                   SELECT
     C                   WHEN      DCUSTCD = *BLANKS
     C                   EVAL      *IN31 = *ON
     C                   EVAL      DMSG = '顧客コードを入力してください'
     C                   WHEN      %SCAN(' ':%TRIM(DCUSTCD)) > 0
     C                   EVAL      *IN31 = *ON
     C                   EVAL      DMSG = '顧客コードに空白は使えません'
     C                   WHEN      DCUSTNM = *BLANKS
     C                   EVAL      *IN32 = *ON
     C                   EVAL      DMSG = '顧客名を入力してください'
     C                   WHEN      DTAXNM = *BLANKS
     C                   EVAL      *IN33 = *ON
     C                   EVAL      DMSG = '税区分が登録されていません(F4)'
     C                   WHEN      DCRED < 0
     C                   EVAL      *IN35 = *ON
     C                   EVAL      DMSG = '与信限度額にマイナスは使えません'
     C                   WHEN      DCLSDAY > 31
     C                   EVAL      *IN36 = *ON
     C                   EVAL      DMSG = '締日は1から31、末日は0です'
     C                   ENDSL
     C                   IF        DMSG <> *BLANKS
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
      * 通貨はコードマスター(CURRENCY)にあるものだけ
     C/EXEC SQL
     C+   SELECT COUNT(*) INTO :W#N FROM CODEMSTRP
     C+    WHERE CODETYPE = 'CURRENCY' AND CODEVAL = :DCURR
     C+      AND ISACTIVE = 'Y'
     C/END-EXEC
     C                   IF        SQLCODE <> 0 OR W#N = 0
     C                   EVAL      *IN34 = *ON
     C                   EVAL      DMSG = '通貨が登録されていません(F4)'
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
      * 新規は顧客コードの重複チェック
     C                   IF        W#ADD
     C/EXEC SQL
     C+   SELECT COUNT(*) INTO :W#N FROM CUSTOMERP WHERE CUSTCD = :DCUSTCD
     C/END-EXEC
     C                   IF        SQLCODE <> 0 OR W#N > 0
     C                   EVAL      *IN31 = *ON
     C                   EVAL      DMSG = 'この顧客コードは登録済みです'
     C                   EVAL      W#ERR = *ON
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @SAVE - 登録・更新(締日は CUSTCLSP)
      *=====================================================================
     C     @SAVE         BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EXSR      @STAMP
     C                   EVAL      H#CD = DCUSTCD
     C                   EVAL      H#NM = DCUSTNM
     C                   EVAL      H#BILL = DBILL
     C                   EVAL      H#SHIP = DSHIP
     C                   EVAL      H#KANA = DKANA
     C                   EVAL      H#TAX = DTAXCAT
     C                   EVAL      H#CUR = DCURR
     C                   EVAL      H#CRED = DCRED
     C                   EVAL      H#CLS = DCLSDAY
     C                   IF        W#ADD
     C/EXEC SQL
     C+   SELECT COALESCE(MAX(CUSTID), 0) + 1 INTO :W#ID FROM CUSTOMERP
     C/END-EXEC
     C/EXEC SQL
     C+   INSERT INTO CUSTOMERP (CUSTID, CUSTCD, CUSTNM, BILLADR, SHIPADR,
     C+          TAXCAT, CURRCD, CREDLMT, STATUS, CRTUSER, CRTDATE, CRTTIM,
     C+          UPDUSER, UPDDATE, UPDTIM, CUSTKANA)
     C+   VALUES (:W#ID, :H#CD, :H#NM, :H#BILL, :H#SHIP, :H#TAX, :H#CUR,
     C+          :H#CRED, 'ACTIVE', :W#USER, :W#D8, :W#T6, :W#USER, :W#D8,
     C+          :W#T6, :H#KANA)
     C/END-EXEC
     C                   ELSE
     C                   EVAL      W#ID = W#SEL
     C/EXEC SQL
     C+   UPDATE CUSTOMERP
     C+      SET CUSTNM = :H#NM, BILLADR = :H#BILL, SHIPADR = :H#SHIP,
     C+          CUSTKANA = :H#KANA, TAXCAT = :H#TAX, CURRCD = :H#CUR,
     C+          CREDLMT = :H#CRED, UPDUSER = :W#USER, UPDDATE = :W#D8,
     C+          UPDTIM = :W#T6
     C+    WHERE CUSTID = :W#ID
     C/END-EXEC
     C                   ENDIF
     C                   IF        SQLCODE <> 0
     C                   EVAL      DMSG = '登録エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
      * 締日(未登録なら作る)
     C/EXEC SQL
     C+   MERGE INTO CUSTCLSP T
     C+   USING (VALUES (:W#ID, :H#CLS)) AS N (CUSTID, CLOSEDAY)
     C+      ON T.CUSTID = N.CUSTID
     C+    WHEN MATCHED THEN UPDATE SET CLOSEDAY = N.CLOSEDAY,
     C+         UPDUSER = :W#USER, UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHEN NOT MATCHED THEN INSERT (CUSTID, CLOSEDAY, UPDUSER,
     C+         UPDDATE, UPDTIM)
     C+         VALUES (N.CUSTID, N.CLOSEDAY, :W#USER, :W#D8, :W#T6)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      DMSG = '締日の保存エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   IF        W#ADD
     C                   EVAL      LMSG = %TRIM(H#CD) + ' を登録しました'
     C                   ELSE
     C                   EVAL      LMSG = %TRIM(H#CD) + ' を変更しました'
     C                   ENDIF
     C                   EVAL      LFCUST = H#CD
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
