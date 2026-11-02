     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: FXMNT
      * 説明: 為替レートの保守(外貨 → JPY)
      * 作成: 修正#MST 新規作成。マスター保守メニューから呼ぶ
      *   一覧は適用日の新しい順。2=変更(レートだけ) 4=削除 F6=新規
      *   通貨は CURRENCY のコード(JPY以外)。同じ日・通貨は1件だけ
      *   年齢表・督促・月次集計は、基準日以前で直近のレートを使う
      *=====================================================================
     FFXMNTD    CF   E             WORKSTN SFILE(LSFL:W#RRN)

     D W#RRN           S              4S 0
     D W#CNT           S              4S 0
     D W#SQLCD         S             10I 0
     D W#DONE          S               N
     D W#ERR           S               N
     D W#ADD           S               N
     D W#PVAL          S             30A
     D W#FCUR          S              3A
     D W#SELD          S              8P 0
     D W#SELC          S              3A
     D W#OPT           S              1A
     D W#N             S             10I 0
     D W#D8            S              8P 0
     D W#T6            S              6P 0
     D W#DT            S               D
     D S#DATE          S              8P 0
     D S#FROM          S              3A
     D S#TO            S              3A
     D S#RATE          S             10P 6
     D H#DATE          S              8P 0
     D H#FROM          S              3A
     D H#RATE          S             10P 6
     D O2CPRMPT        PR                  EXTPGM('O2CPRMPT')
     D   PTYPE                       10A   CONST
     D   PCDTYPE                     30A   CONST
     D   PVALUE                      30A
     D FMTDATE         PR            10A
     D   PDATE                        8P 0 CONST

     C/EXEC SQL
     C+   SET OPTION COMMIT = *NONE, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE LSTCSR CURSOR FOR
     C+   SELECT RATEDATE, FROMCUR, TOCUR, RATE FROM FXRATEP
     C+    WHERE (:W#FCUR = ' ' OR FROMCUR = :W#FCUR) AND TOCUR = 'JPY'
     C+    ORDER BY RATEDATE DESC, FROMCUR FOR READ ONLY
     C/END-EXEC

     C                   EVAL      LFCUR = *BLANKS
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
     C                   EVAL      W#PVAL = LFCUR
     C                   CALLP     O2CPRMPT('CODE':'CURRENCY':W#PVAL)
     C                   EVAL      LFCUR = W#PVAL
     C                   EXSR      @LOAD
     C                   ITER
     C                   ENDIF
     C                   IF        *IN06
     C                   EVAL      W#ADD = *ON
     C                   EXSR      @EDIT
     C                   EXSR      @LOAD
     C                   ITER
     C                   ENDIF
     C                   EVAL      W#SELD = 0
     C                   IF        W#CNT > 0
     C                   READC     LSFL
     C                   DOW       NOT %EOF(FXMNTD)
     C                   IF        LOPT <> ' ' AND W#SELD = 0
     C                   EVAL      W#SELD = LDATE8
     C                   EVAL      W#SELC = LFROM
     C                   EVAL      W#OPT = LOPT
     C                   ENDIF
     C                   READC     LSFL
     C                   ENDDO
     C                   ENDIF
     C                   IF        W#SELD > 0
     C                   IF        W#OPT = '2'
     C                   EVAL      W#ADD = *OFF
     C                   EXSR      @EDIT
     C                   ELSE
     C                   EXSR      @DELETE
     C                   ENDIF
     C                   ENDIF
     C                   EXSR      @LOAD
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C                   RETURN

      * @LOAD - レートの一覧
     C     @LOAD         BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     LCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#CNT = 0
     C                   EVAL      W#FCUR = LFCUR
     C/EXEC SQL
     C+   OPEN LSTCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#CNT < 999
     C/EXEC SQL
     C+   FETCH LSTCSR INTO :S#DATE, :S#FROM, :S#TO, :S#RATE
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      LOPT = ' '
     C                   EVAL      LDATE8 = S#DATE
     C                   EVAL      LDATE = FMTDATE(S#DATE)
     C                   EVAL      LFROM = S#FROM
     C                   EVAL      LTO = S#TO
     C                   EVALR     LRATE = %EDITC(S#RATE:'J')
     C                   EVAL      W#CNT = W#CNT + 1
     C                   EVAL      W#RRN = W#CNT
     C                   WRITE     LSFL
     C                   ENDDO
     C                   EVAL      W#SQLCD = SQLCODE
     C/EXEC SQL
     C+   CLOSE LSTCSR
     C/END-EXEC
     C                   IF        W#SQLCD < 0
     C                   EVAL      LMSG = '読み込みエラー SQLCODE=' +
     C                             %CHAR(W#SQLCD)
     C                   ELSEIF    W#CNT = 0 AND LMSG = *BLANKS
     C                   EVAL      LMSG = '為替レートが登録されていません'
     C                   ENDIF
     C                   ENDSR

      * @DELETE - レートの削除
     C     @DELETE       BEGSR
     C/EXEC SQL
     C+   DELETE FROM FXRATEP
     C+    WHERE RATEDATE = :W#SELD AND FROMCUR = :W#SELC AND TOCUR = 'JPY'
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      LMSG = '削除エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   ELSE
     C                   EVAL      LMSG = %TRIM(W#SELC) + ' ' +
     C                             FMTDATE(W#SELD) + ' のレートを削除しました'
     C                   ENDIF
     C                   ENDSR

      * @EDIT - 登録・変更
     C     @EDIT         BEGSR
     C                   EVAL      *IN50 = NOT W#ADD
     C                   EVAL      DMSG = *BLANKS
     C                   IF        W#ADD
     C                   EVAL      DMODE = '為替レートの登録'
     C                   EVAL      DDATE = %DEC(%DATE():*ISO)
     C                   EVAL      DFROM = LFCUR
     C                   EVAL      DRATE = 0
     C                   ELSE
     C                   EVAL      DMODE = '為替レートの変更'
     C/EXEC SQL
     C+   SELECT RATE INTO :H#RATE FROM FXRATEP
     C+    WHERE RATEDATE = :W#SELD AND FROMCUR = :W#SELC AND TOCUR = 'JPY'
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      LMSG = 'レートが見つかりません'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      DDATE = W#SELD
     C                   EVAL      DFROM = W#SELC
     C                   EVAL      DRATE = H#RATE
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
     C                   IF        *IN04 AND W#ADD
     C                   EVAL      W#PVAL = DFROM
     C                   CALLP     O2CPRMPT('CODE':'CURRENCY':W#PVAL)
     C                   EVAL      DFROM = W#PVAL
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

      * @CHECK - 入力チェック
     C     @CHECK        BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      *IN31 = *OFF
     C                   EVAL      *IN32 = *OFF
     C                   EVAL      *IN33 = *OFF
     C                   MONITOR
     C                   EVAL      W#DT = %DATE(DDATE:*ISO)
     C                   ON-ERROR
     C                   EVAL      *IN31 = *ON
     C                   EVAL      DMSG = '適用日が無効です(YYYYMMDD)'
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDMON
     C                   IF        DFROM = 'JPY' OR DFROM = *BLANKS
     C                   EVAL      *IN32 = *ON
     C                   EVAL      DMSG = 'JPY以外の通貨を入力してください'
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C/EXEC SQL
     C+   SELECT COUNT(*) INTO :W#N FROM CODEMSTRP
     C+    WHERE CODETYPE = 'CURRENCY' AND CODEVAL = :DFROM
     C/END-EXEC
     C                   IF        SQLCODE <> 0 OR W#N = 0
     C                   EVAL      *IN32 = *ON
     C                   EVAL      DMSG = '通貨が登録されていません(F4)'
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   IF        DRATE <= 0
     C                   EVAL      *IN33 = *ON
     C                   EVAL      DMSG = 'レートは0より大きい値を入力'
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   IF        W#ADD
     C                   EVAL      H#DATE = DDATE
     C                   EVAL      H#FROM = DFROM
     C/EXEC SQL
     C+   SELECT COUNT(*) INTO :W#N FROM FXRATEP
     C+    WHERE RATEDATE = :H#DATE AND FROMCUR = :H#FROM AND TOCUR = 'JPY'
     C/END-EXEC
     C                   IF        SQLCODE <> 0 OR W#N > 0
     C                   EVAL      *IN31 = *ON
     C                   EVAL      DMSG = 'この日・通貨のレートは登録済み'
     C                   EVAL      W#ERR = *ON
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR

      * @SAVE - 登録・更新
     C     @SAVE         BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      W#D8 = %DEC(%DATE():*ISO)
     C                   EVAL      W#T6 = %DEC(%TIME():*HMS)
     C                   EVAL      H#DATE = DDATE
     C                   EVAL      H#FROM = DFROM
     C                   EVAL      H#RATE = DRATE
     C                   IF        W#ADD
     C/EXEC SQL
     C+   INSERT INTO FXRATEP (RATEDATE, FROMCUR, TOCUR, RATE, CRTDATE,
     C+          CRTTIM)
     C+   VALUES (:H#DATE, :H#FROM, 'JPY', :H#RATE, :W#D8, :W#T6)
     C/END-EXEC
     C                   ELSE
     C/EXEC SQL
     C+   UPDATE FXRATEP SET RATE = :H#RATE
     C+    WHERE RATEDATE = :H#DATE AND FROMCUR = :H#FROM AND TOCUR = 'JPY'
     C/END-EXEC
     C                   ENDIF
     C                   IF        SQLCODE <> 0
     C                   EVAL      DMSG = '登録エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      LMSG = %TRIM(H#FROM) + ' ' + FMTDATE(H#DATE)
     C                             + ' のレートを保存しました'
     C                   ENDSR

      * FmtDate - YYYYMMDD を YYYY/MM/DD に
     P FMTDATE         B
     D FMTDATE         PI            10A
     D   PDATE                        8P 0 CONST
     D WD8             S              8A
     C                   IF        PDATE = 0
     C                   RETURN    *BLANKS
     C                   ENDIF
     C                   EVAL      WD8 = %EDITC(PDATE:'X')
     C                   RETURN    %SUBST(WD8:1:4) + '/' + %SUBST(WD8:5:2) +
     C                             '/' + %SUBST(WD8:7:2)
     P FMTDATE         E
