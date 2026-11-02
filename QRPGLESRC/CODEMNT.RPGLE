     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: CODEMNT
      * 説明: コードマスターの保守(登録・変更・有効/無効)
      * 作成: 修正#MST 新規作成。マスター保守メニューから呼ぶ
      *   一覧: コード種別で絞り込み。2=変更 4=有効・無効 F6=新規
      *   説明の列は'A'型で全角を入れているため、SQLではなく
      *   レコード単位で読み書きする(SQLだと変換エラー -330)
      *   使い方の例: COMPANY/REGNO に適格請求書の登録番号
      *=====================================================================
     FCODEMNTD  CF   E             WORKSTN SFILE(LSFL:W#RRN)
     FCODEMSTRP UF A E           K DISK

     D W#RRN           S              4S 0
     D W#CNT           S              4S 0
     D W#DONE          S               N
     D W#ERR           S               N
     D W#ADD           S               N
     D W#OPT           S              1A
     D W#SELT          S             30A
     D W#SELV          S             30A
     D W#TYPE          S             30A
     D W#VAL           S             30A
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
     D DBCSTRUNC       PR           256A   VARYING
     D   PSTR                       256A   VARYING CONST
     D   PMAX                        10I 0 CONST

     C     CDKEY         KLIST
     C                   KFLD                    W#TYPE
     C                   KFLD                    W#VAL

     C                   EVAL      LFTYPE = *BLANKS
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
     C                   EXSR      @EDIT
     C                   EXSR      @LOAD
     C                   ITER
     C                   ENDIF
     C                   EVAL      W#SELT = *BLANKS
     C                   IF        W#CNT > 0
     C                   READC     LSFL
     C                   DOW       NOT %EOF(CODEMNTD)
     C                   IF        LOPT <> ' ' AND W#SELT = *BLANKS
     C                   EVAL      W#SELT = LTYPEH
     C                   EVAL      W#SELV = LVALH
     C                   EVAL      W#OPT = LOPT
     C                   ENDIF
     C                   READC     LSFL
     C                   ENDDO
     C                   ENDIF
     C                   IF        W#SELT <> *BLANKS
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

      * @LOAD - コードの一覧(種別・コード順)
     C     @LOAD         BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     LCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#CNT = 0
     C                   EVAL      W#TYPE = LFTYPE
     C                   EVAL      W#VAL = *LOVAL
     C     CDKEY         SETLL     CODEREC
     C                   DOW       W#CNT < 999
     C                   READ(N)   CODEREC
     C                   IF        %EOF(CODEMSTRP)
     C                   LEAVE
     C                   ENDIF
     C                   IF        LFTYPE <> *BLANKS AND CODETYPE <> LFTYPE
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      LOPT = ' '
     C                   EVAL      LTYPEH = CODETYPE
     C                   EVAL      LVALH = CODEVAL
     C                   EVAL      LTYPE = CODETYPE
     C                   EVAL      LVAL = CODEVAL
     C                   EVAL      LDESC = DBCSTRUNC(%TRIMR(DESC):30)
     C                   EVALR     LORD = %CHAR(DISPORD)
     C                   IF        ISACTIVE = 'Y'
     C                   EVAL      LACT = '有効'
     C                   EVAL      *IN61 = *OFF
     C                   ELSE
     C                   EVAL      LACT = '無効'
     C                   EVAL      *IN61 = *ON
     C                   ENDIF
     C                   EVAL      W#CNT = W#CNT + 1
     C                   EVAL      W#RRN = W#CNT
     C                   WRITE     LSFL
     C                   ENDDO
     C                   EVAL      *IN61 = *OFF
     C                   IF        W#CNT = 0 AND LMSG = *BLANKS
     C                   EVAL      LMSG = '該当するコードがありません'
     C                   ENDIF
     C                   ENDSR

      * @TOGGLE - 有効・無効の切り替え
     C     @TOGGLE       BEGSR
     C                   EVAL      W#TYPE = W#SELT
     C                   EVAL      W#VAL = W#SELV
     C     CDKEY         CHAIN(E)  CODEREC
     C                   IF        %ERROR OR NOT %FOUND(CODEMSTRP)
     C                   EVAL      LMSG = 'コードが見つからないか使用中です'
     C                   LEAVESR
     C                   ENDIF
     C                   IF        ISACTIVE = 'Y'
     C                   EVAL      ISACTIVE = 'N'
     C                   ELSE
     C                   EVAL      ISACTIVE = 'Y'
     C                   ENDIF
     C                   UPDATE(E) CODEREC
     C                   IF        %ERROR
     C                   EVAL      LMSG = '更新エラー'
     C                   ELSE
     C                   EVAL      LMSG = %TRIM(W#SELV) + ' を切り替えました'
     C                   ENDIF
     C                   ENDSR

      * @EDIT - 登録・変更
     C     @EDIT         BEGSR
     C                   EVAL      *IN50 = NOT W#ADD
     C                   EVAL      DMSG = *BLANKS
     C                   IF        W#ADD
     C                   EVAL      DMODE = 'コードの登録'
     C                   EVAL      DTYPE = LFTYPE
     C                   EVAL      DVAL = *BLANKS
     C                   EVAL      DDESC = *BLANKS
     C                   EVAL      DORD = 0
     C                   EVAL      DACT = 'Y'
     C                   ELSE
     C                   EVAL      DMODE = 'コードの変更'
     C                   EVAL      W#TYPE = W#SELT
     C                   EVAL      W#VAL = W#SELV
     C     CDKEY         CHAIN(N)  CODEREC
     C                   IF        NOT %FOUND(CODEMSTRP)
     C                   EVAL      LMSG = 'コードが見つかりません'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      DTYPE = CODETYPE
     C                   EVAL      DVAL = CODEVAL
     C                   EVAL      DDESC = DESC
     C                   EVAL      DORD = DISPORD
     C                   EVAL      DACT = ISACTIVE
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
     C                   EVAL      *IN33 = *OFF
     C                   SELECT
     C                   WHEN      DTYPE = *BLANKS
     C                   EVAL      *IN31 = *ON
     C                   EVAL      DMSG = 'コード種別を入力してください'
     C                   WHEN      DVAL = *BLANKS
     C                   EVAL      *IN32 = *ON
     C                   EVAL      DMSG = 'コードを入力してください'
     C                   WHEN      DDESC = *BLANKS
     C                   EVAL      *IN33 = *ON
     C                   EVAL      DMSG = '説明を入力してください'
     C                   ENDSL
     C                   IF        DMSG <> *BLANKS
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   IF        W#ADD
     C                   EVAL      W#TYPE = DTYPE
     C                   EVAL      W#VAL = DVAL
     C     CDKEY         SETLL     CODEREC
     C                   IF        %EQUAL(CODEMSTRP)
     C                   EVAL      *IN32 = *ON
     C                   EVAL      DMSG = 'このコードは登録済みです'
     C                   EVAL      W#ERR = *ON
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR

      * @SAVE - 登録・更新
     C     @SAVE         BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      W#TYPE = DTYPE
     C                   EVAL      W#VAL = DVAL
     C                   IF        W#ADD
     C                   CLEAR                   CODEREC
     C                   EVAL      CODETYPE = DTYPE
     C                   EVAL      CODEVAL = DVAL
     C                   EVAL      CRTDATE = %DATE()
     C                   EVAL      CRTTIM = %TIME()
     C                   ELSE
     C     CDKEY         CHAIN(E)  CODEREC
     C                   IF        %ERROR OR NOT %FOUND(CODEMSTRP)
     C                   EVAL      DMSG = 'コードが見つからないか使用中です'
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   ENDIF
     C                   EVAL      DESC = DDESC
     C                   EVAL      DISPORD = DORD
     C                   EVAL      ISACTIVE = DACT
     C                   IF        W#ADD
     C                   WRITE(E)  CODEREC
     C                   ELSE
     C                   UPDATE(E) CODEREC
     C                   ENDIF
     C                   IF        %ERROR
     C                   EVAL      DMSG = '登録エラー'
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      LMSG = %TRIM(DVAL) + ' を保存しました'
     C                   EVAL      LFTYPE = DTYPE
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
