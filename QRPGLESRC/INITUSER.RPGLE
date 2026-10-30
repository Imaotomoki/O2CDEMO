     H DATEDIT(*YMD) DECEDIT('0.') COPYRIGHT('DEMOAPP')
      *=====================================================================
      * プログラム名: INITUSER
      * 説明: ユーザーの初期データ投入
      * 修正#ROLE: ユーザーロール(USERROLEP)は不要のため削除
      *=====================================================================
     FAPPUSERP  O    E             DISK
     D WTIMESTAMP      S               Z
     D WDATE8          S              8S 0
     D WTIME6          S              6S 0
     D W#UCNT          S              9S 0
     D W#NXID          S              9S 0
     C                   EXSR      @INZ
     C                   EXSR      @USER
     C                   EXSR      @END
     C     @INZ          BEGSR
     C                   TIME                    WTIMESTAMP
     C                   EVAL      WDATE8 = %DEC(%DATE(WTIMESTAMP):*ISO)
     C                   EVAL      WTIME6 = %DEC(%TIME(WTIMESTAMP):*HMS)
     C                   EVAL      W#UCNT = *ZERO
     C                   EVAL      W#NXID = 1
     C                   ENDSR
     C     @USER         BEGSR
     C                   EVAL      USERID = W#NXID
     C                   EVAL      USERNAME = 'admin'
     C                   EVAL      FULLNAME = '管理者'
     C                   EVAL      EMAIL = 'admin@demoapp.com'
     C                   EVAL      DEPT = 'IT'
     C                   EVAL      STATUS = 'ACTIVE'
     C                   EVAL      CRTDATE = WDATE8
     C                   EVAL      CRTTIM = WTIME6
     C                   EVAL      UPDDATE = WDATE8
     C                   EVAL      UPDTIM = WTIME6
     C                   WRITE     USERREC
     C                   EVAL      W#UCNT = W#UCNT + 1
     C                   EVAL      W#NXID = W#NXID + 1
     C                   EVAL      USERID = W#NXID
     C                   EVAL      USERNAME = 'yamada.t'
     C                   EVAL      FULLNAME = '山田太郎'
     C                   EVAL      EMAIL = 'yamada.t@demoapp.com'
     C                   EVAL      DEPT = 'SALES'
     C                   EVAL      STATUS = 'ACTIVE'
     C                   EVAL      CRTDATE = WDATE8
     C                   EVAL      CRTTIM = WTIME6
     C                   EVAL      UPDDATE = WDATE8
     C                   EVAL      UPDTIM = WTIME6
     C                   WRITE     USERREC
     C                   EVAL      W#UCNT = W#UCNT + 1
     C                   EVAL      W#NXID = W#NXID + 1
     C                   EVAL      USERID = W#NXID
     C                   EVAL      USERNAME = 'tanaka.h'
     C                   EVAL      FULLNAME = '田中花子'
     C                   EVAL      EMAIL = 'tanaka.h@demoapp.com'
     C                   EVAL      DEPT = 'SALES'
     C                   EVAL      STATUS = 'ACTIVE'
     C                   EVAL      CRTDATE = WDATE8
     C                   EVAL      CRTTIM = WTIME6
     C                   EVAL      UPDDATE = WDATE8
     C                   EVAL      UPDTIM = WTIME6
     C                   WRITE     USERREC
     C                   EVAL      W#UCNT = W#UCNT + 1
     C                   EVAL      W#NXID = W#NXID + 1
     C                   EVAL      USERID = W#NXID
     C                   EVAL      USERNAME = 'suzuki.i'
     C                   EVAL      FULLNAME = '鈴木一郎'
     C                   EVAL      EMAIL = 'suzuki.i@demoapp.com'
     C                   EVAL      DEPT = 'SALES'
     C                   EVAL      STATUS = 'ACTIVE'
     C                   EVAL      CRTDATE = WDATE8
     C                   EVAL      CRTTIM = WTIME6
     C                   EVAL      UPDDATE = WDATE8
     C                   EVAL      UPDTIM = WTIME6
     C                   WRITE     USERREC
     C                   EVAL      W#UCNT = W#UCNT + 1
     C                   EVAL      W#NXID = W#NXID + 1
     C                   EVAL      USERID = W#NXID
     C                   EVAL      USERNAME = 'sato.j'
     C                   EVAL      FULLNAME = '佐藤次郎'
     C                   EVAL      EMAIL = 'sato.j@demoapp.com'
     C                   EVAL      DEPT = 'WAREHOUSE'
     C                   EVAL      STATUS = 'ACTIVE'
     C                   EVAL      CRTDATE = WDATE8
     C                   EVAL      CRTTIM = WTIME6
     C                   EVAL      UPDDATE = WDATE8
     C                   EVAL      UPDTIM = WTIME6
     C                   WRITE     USERREC
     C                   EVAL      W#UCNT = W#UCNT + 1
     C                   EVAL      W#NXID = W#NXID + 1
     C                   EVAL      USERID = W#NXID
     C                   EVAL      USERNAME = 'takahashi.s'
     C                   EVAL      FULLNAME = '高橋三郎'
     C                   EVAL      EMAIL = 'takahashi.s@demoapp.com'
     C                   EVAL      DEPT = 'WAREHOUSE'
     C                   EVAL      STATUS = 'ACTIVE'
     C                   EVAL      CRTDATE = WDATE8
     C                   EVAL      CRTTIM = WTIME6
     C                   EVAL      UPDDATE = WDATE8
     C                   EVAL      UPDTIM = WTIME6
     C                   WRITE     USERREC
     C                   EVAL      W#UCNT = W#UCNT + 1
     C                   EVAL      W#NXID = W#NXID + 1
     C                   EVAL      USERID = W#NXID
     C                   EVAL      USERNAME = 'watanabe.y'
     C                   EVAL      FULLNAME = '渡辺四郎'
     C                   EVAL      EMAIL = 'watanabe.y@demoapp.com'
     C                   EVAL      DEPT = 'WAREHOUSE'
     C                   EVAL      STATUS = 'ACTIVE'
     C                   EVAL      CRTDATE = WDATE8
     C                   EVAL      CRTTIM = WTIME6
     C                   EVAL      UPDDATE = WDATE8
     C                   EVAL      UPDTIM = WTIME6
     C                   WRITE     USERREC
     C                   EVAL      W#UCNT = W#UCNT + 1
     C                   EVAL      W#NXID = W#NXID + 1
     C                   EVAL      USERID = W#NXID
     C                   EVAL      USERNAME = 'ito.g'
     C                   EVAL      FULLNAME = '伊藤五郎'
     C                   EVAL      EMAIL = 'ito.g@demoapp.com'
     C                   EVAL      DEPT = 'ACCOUNTING'
     C                   EVAL      STATUS = 'ACTIVE'
     C                   EVAL      CRTDATE = WDATE8
     C                   EVAL      CRTTIM = WTIME6
     C                   EVAL      UPDDATE = WDATE8
     C                   EVAL      UPDTIM = WTIME6
     C                   WRITE     USERREC
     C                   EVAL      W#UCNT = W#UCNT + 1
     C                   EVAL      W#NXID = W#NXID + 1
     C                   EVAL      USERID = W#NXID
     C                   EVAL      USERNAME = 'yamamoto.r'
     C                   EVAL      FULLNAME = '山本六郎'
     C                   EVAL      EMAIL = 'yamamoto.r@demoapp.com'
     C                   EVAL      DEPT = 'ACCOUNTING'
     C                   EVAL      STATUS = 'ACTIVE'
     C                   EVAL      CRTDATE = WDATE8
     C                   EVAL      CRTTIM = WTIME6
     C                   EVAL      UPDDATE = WDATE8
     C                   EVAL      UPDTIM = WTIME6
     C                   WRITE     USERREC
     C                   EVAL      W#UCNT = W#UCNT + 1
     C                   EVAL      W#NXID = W#NXID + 1
     C                   EVAL      USERID = W#NXID
     C                   EVAL      USERNAME = 'nakamura.n'
     C                   EVAL      FULLNAME = '中村七郎'
     C                   EVAL      EMAIL = 'nakamura.n@demoapp.com'
     C                   EVAL      DEPT = 'SALES'
     C                   EVAL      STATUS = 'ACTIVE'
     C                   EVAL      CRTDATE = WDATE8
     C                   EVAL      CRTTIM = WTIME6
     C                   EVAL      UPDDATE = WDATE8
     C                   EVAL      UPDTIM = WTIME6
     C                   WRITE     USERREC
     C                   EVAL      W#UCNT = W#UCNT + 1
     C                   EVAL      W#NXID = W#NXID + 1
     C                   EVAL      USERID = W#NXID
     C                   EVAL      USERNAME = 'kobayashi.h'
     C                   EVAL      FULLNAME = '小林八郎'
     C                   EVAL      EMAIL = 'kobayashi.h@demoapp.com'
     C                   EVAL      DEPT = 'ACCOUNTING'
     C                   EVAL      STATUS = 'ACTIVE'
     C                   EVAL      CRTDATE = WDATE8
     C                   EVAL      CRTTIM = WTIME6
     C                   EVAL      UPDDATE = WDATE8
     C                   EVAL      UPDTIM = WTIME6
     C                   WRITE     USERREC
     C                   EVAL      W#UCNT = W#UCNT + 1
     C                   EVAL      W#NXID = W#NXID + 1
     C                   EVAL      USERID = W#NXID
     C                   EVAL      USERNAME = 'test.user'
     C                   EVAL      FULLNAME = 'テストユーザー'
     C                   EVAL      EMAIL = 'test@demoapp.com'
     C                   EVAL      DEPT = 'IT'
     C                   EVAL      STATUS = 'INACTIVE'
     C                   EVAL      CRTDATE = WDATE8
     C                   EVAL      CRTTIM = WTIME6
     C                   EVAL      UPDDATE = WDATE8
     C                   EVAL      UPDTIM = WTIME6
     C                   WRITE     USERREC
     C                   EVAL      W#UCNT = W#UCNT + 1
     C                   EVAL      W#NXID = W#NXID + 1
     C                   ENDSR
     C     @END          BEGSR
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDSR
