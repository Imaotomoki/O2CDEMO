     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: WHMNT
      * 説明: 倉庫マスターの保守(登録・変更・停止/再開)
      * 作成: 修正#MST 新規作成。マスター保守メニューから呼ぶ
      *   2=変更 4=停止・再開 F6=新規。倉庫コードは重複不可
      *   倉庫IDは既存の最大値+1
      *=====================================================================
     FWHMNTD    CF   E             WORKSTN SFILE(LSFL:W#RRN)

     D W#RRN           S              4S 0
     D W#CNT           S              4S 0
     D W#SQLCD         S             10I 0
     D W#DONE          S               N
     D W#ERR           S               N
     D W#ADD           S               N
     D W#SEL           S              4P 0
     D W#OPT           S              1A
     D W#N             S             10I 0
     D W#ID            S              4P 0
     D W#USER          S             10A   INZ(*USER)
     D W#D8            S              8P 0
     D W#T6            S              6P 0
     D W#STS           S             10A
     D S#ID            S              4P 0
     D S#CD            S             10A
     D S#NM            S             60A
     D S#MGR           S             60A
     D S#STS           S             10A
     D H#CD            S             10A
     D H#NM            S             60A
     D H#ADR           S             60A
     D H#MGR           S             60A
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
     C+   SELECT WHID, WHCD, WHNM, MGRNM, STATUS
     C+     FROM WAREHOUSEP ORDER BY WHCD FOR READ ONLY
     C/END-EXEC

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
     C                   DOW       NOT %EOF(WHMNTD)
     C                   IF        LOPT <> ' ' AND W#SEL = 0
     C                   EVAL      W#SEL = LWHID
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

      * @LOAD - 倉庫の一覧
     C     @LOAD         BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     LCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#CNT = 0
     C/EXEC SQL
     C+   OPEN LSTCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#CNT < 999
     C/EXEC SQL
     C+   FETCH LSTCSR INTO :S#ID, :S#CD, :S#NM, :S#MGR, :S#STS
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      LOPT = ' '
     C                   EVAL      LWHID = S#ID
     C                   EVAL      LWH = S#CD
     C                   EVAL      LNAME = DBCSTRUNC(%TRIMR(S#NM):30)
     C                   EVAL      LMGR = DBCSTRUNC(%TRIMR(S#MGR):20)
     C                   IF        S#STS = 'ACTIVE'
     C                   EVAL      LSTS = '使用中'
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
     C                   EVAL      LMSG = '倉庫が登録されていません'
     C                   ENDIF
     C                   ENDSR

      * @TOGGLE - 停止・再開(在庫がある倉庫も停止はできる)
     C     @TOGGLE       BEGSR
     C                   EXSR      @STAMP
     C/EXEC SQL
     C+   UPDATE WAREHOUSEP
     C+      SET STATUS = CASE WHEN STATUS = 'ACTIVE' THEN 'INACTIVE'
     C+                        ELSE 'ACTIVE' END,
     C+          UPDUSER = :W#USER, UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHERE WHID = :W#SEL
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      LMSG = '更新エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   ELSE
     C                   EVAL      LMSG = '倉庫の状態を切り替えました'
     C                   ENDIF
     C                   ENDSR

      * @EDIT - 登録・変更
     C     @EDIT         BEGSR
     C                   EVAL      *IN50 = NOT W#ADD
     C                   EVAL      DMSG = *BLANKS
     C                   IF        W#ADD
     C                   EVAL      DMODE = '倉庫の登録'
     C                   EVAL      DWHCD = *BLANKS
     C                   EVAL      DWHNM = *BLANKS
     C                   EVAL      DWHADR = *BLANKS
     C                   EVAL      DMGR = *BLANKS
     C                   EVAL      DSTS = '使用中'
     C                   ELSE
     C                   EVAL      DMODE = '倉庫の変更'
     C/EXEC SQL
     C+   SELECT WHCD, WHNM, WHADR, MGRNM, STATUS
     C+     INTO :H#CD, :H#NM, :H#ADR, :H#MGR, :W#STS
     C+     FROM WAREHOUSEP WHERE WHID = :W#SEL
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      LMSG = '倉庫が見つかりません'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      DWHCD = H#CD
     C                   EVAL      DWHNM = H#NM
     C                   EVAL      DWHADR = H#ADR
     C                   EVAL      DMGR = H#MGR
     C                   IF        W#STS = 'ACTIVE'
     C                   EVAL      DSTS = '使用中'
     C                   ELSE
     C                   EVAL      DSTS = '停止'
     C                   ENDIF
     C                   ENDIF
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

      * @CHECK - 入力チェック
     C     @CHECK        BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      *IN31 = *OFF
     C                   EVAL      *IN32 = *OFF
     C                   SELECT
     C                   WHEN      DWHCD = *BLANKS
     C                   EVAL      *IN31 = *ON
     C                   EVAL      DMSG = '倉庫コードを入力してください'
     C                   WHEN      %SCAN(' ':%TRIM(DWHCD)) > 0
     C                   EVAL      *IN31 = *ON
     C                   EVAL      DMSG = '倉庫コードに空白は使えません'
     C                   WHEN      DWHNM = *BLANKS
     C                   EVAL      *IN32 = *ON
     C                   EVAL      DMSG = '倉庫名を入力してください'
     C                   ENDSL
     C                   IF        DMSG <> *BLANKS
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   IF        W#ADD
     C/EXEC SQL
     C+   SELECT COUNT(*) INTO :W#N FROM WAREHOUSEP WHERE WHCD = :DWHCD
     C/END-EXEC
     C                   IF        SQLCODE <> 0 OR W#N > 0
     C                   EVAL      *IN31 = *ON
     C                   EVAL      DMSG = 'この倉庫コードは登録済みです'
     C                   EVAL      W#ERR = *ON
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR

      * @SAVE - 登録・更新
     C     @SAVE         BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EXSR      @STAMP
     C                   EVAL      H#CD = DWHCD
     C                   EVAL      H#NM = DWHNM
     C                   EVAL      H#ADR = DWHADR
     C                   EVAL      H#MGR = DMGR
     C                   IF        W#ADD
     C/EXEC SQL
     C+   SELECT COALESCE(MAX(WHID), 0) + 1 INTO :W#ID FROM WAREHOUSEP
     C/END-EXEC
     C/EXEC SQL
     C+   INSERT INTO WAREHOUSEP (WHID, WHCD, WHNM, WHADR, MGRNM, STATUS,
     C+          CRTUSER, CRTDATE, CRTTIM, UPDUSER, UPDDATE, UPDTIM)
     C+   VALUES (:W#ID, :H#CD, :H#NM, :H#ADR, :H#MGR, 'ACTIVE', :W#USER,
     C+          :W#D8, :W#T6, :W#USER, :W#D8, :W#T6)
     C/END-EXEC
     C                   ELSE
     C/EXEC SQL
     C+   UPDATE WAREHOUSEP
     C+      SET WHNM = :H#NM, WHADR = :H#ADR, MGRNM = :H#MGR,
     C+          UPDUSER = :W#USER, UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHERE WHID = :W#SEL
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
