     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: QUOTENT
      * 説明: 見積入力・受注への変換
      * 作成: 修正#QT 新規作成。販売管理メニューの18から呼ぶ
      *   一覧: 1=受注へ変換 2=変更 4=取消 5=表示 F6=新規
      *   明細は10行まで。単価0は顧客別単価(PRICEP)か標準単価を入れる
      *   税率は受注入力と同じ(顧客が非課税・輸出は0、商品の税区分)
      *   受注への変換は CREATESORD を呼ぶ(在庫引当・与信チェックも
      *   CREATESORD が行う)。有効期限を過ぎた見積は変換できない
      *   見積IDは O2C_QTID_SEQ で採番。見積番号は QT-見積ID
      *=====================================================================
     FQUOTENTD  CF   E             WORKSTN SFILE(LSFL:W#RRN)
     F                                     SFILE(DSFL:W#DRRN)

     D PSDS           SDS
     D  PS_MSGID              40     46
     D  PS_MSGDTA             91    170

     D W#RRN           S              4S 0
     D W#DRRN          S              4S 0
     D W#CNT           S              4S 0
     D W#SQLCD         S             10I 0
     D W#DONE          S               N
     D W#ERR           S               N
     D W#ADD           S               N
     D W#VIEW          S               N
     D W#F4            S               N
     D W#F6            S               N
     D W#SEL           S             18P 0
     D W#OPT           S              1A
     D W#N             S             10I 0
     D W#I             S             10I 0
     D W#PVAL          S             30A
     D W#USER          S             10A   INZ(*USER)
     D W#D8            S              8P 0
     D W#T6            S              6P 0
     D W#TODAY         S              8P 0
     D W#P2            S             13P 2
     D W#TOT           S             15P 2
     D W#TAX           S             15P 2
     D W#CTAX          S              5P 3
     D W#CCAT          S             10A
     D W#CSTS          S             10A
     D W#WSTS          S             10A
     D W#ITAX          S             10A
     D W#ISTS          S             10A
     D W#STDPRC        S             15P 4
     D W#SONUM         S             20A
     D W#MSG           S             78A
      * 明細(10行)
     D L_ITEM          S             12A   DIM(10)
     D L_QTY           S              9P 0 DIM(10)
     D L_PRC           S             11P 2 DIM(10)
     D L_ITEMID        S              9P 0 DIM(10)
     D L_NAME          S             20A   DIM(10)
     D L_RATE          S              5P 3 DIM(10)
     D L_AMT           S             15P 2 DIM(10)
     D L_ERR           S              1A   DIM(10)
      * CREATESORD のパラメーター
     D C_CUSTID        S              9P 0
     D C_WHID          S              4P 0
     D C_TAXRATE       S              5P 3
     D C_COUNT         S             10I 0
     D C_ITEMID        S              9P 0 DIM(20)
     D C_QTY           S             15P 3 DIM(20)
     D C_PRC           S             15P 4 DIM(20)
      * SQL 用
     D H#ID            S             18P 0
     D H#NUM           S             20A
     D H#CUSTID        S              9P 0
     D H#CUSTCD        S             20A
     D H#QDATE         S              8P 0
     D H#VALID         S              8P 0
     D H#CURR          S              3A
     D H#WHID          S              4P 0
     D H#STS           S             12A
     D H#TOT           S             15P 2
     D H#TAX           S             15P 2
     D H#SONUM         S             20A
     D H#NOTE          S             40A
     D H#NM            S             60A
     D H#LINE          S              9P 0
     D H#ITEMID        S              9P 0
     D H#ITEMCD        S             12A
     D H#QTY           S             15P 3
     D H#PRC           S             15P 4
     D H#RATE          S              5P 3
     D H#AMT           S             15P 2
     D H#F             S              1A
     D CREATESORD      PR                  EXTPGM('CREATESORD')
     D   PCUSTID                      9P 0
     D   PWHID                        4P 0
     D   PTAXRATE                     5P 3
     D   PCOUNT                      10I 0
     D   PITEMID                      9P 0 DIM(20)
     D   PQTY                        15P 3 DIM(20)
     D   PPRC                        15P 4 DIM(20)
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
     C+   SELECT Q.QTID, Q.QTNUM, COALESCE(C.CUSTCD, ' '), Q.QTDATE,
     C+          Q.VALIDTO, Q.TOTAMT, Q.STATUS
     C+     FROM QUOTEHP Q
     C+     LEFT JOIN CUSTOMERP C ON C.CUSTID = Q.CUSTID
     C+    WHERE :H#F = ' ' OR Q.STATUS = 'OPEN'
     C+    ORDER BY Q.QTID DESC
     C+    FOR READ ONLY
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE LINCSR CURSOR FOR
     C+   SELECT L.LINENO, L.ITEMID, COALESCE(I.ITEMCD, ' '), L.QTQTY,
     C+          L.UNITPRC, L.TAXRATE
     C+     FROM QUOTELP L
     C+     LEFT JOIN ITEMP I ON I.ITEMID = L.ITEMID
     C+    WHERE L.QTID = :H#ID
     C+    ORDER BY L.LINENO
     C+    FOR READ ONLY
     C/END-EXEC

     C                   EVAL      LMSG = *BLANKS
     C                   EVAL      LFSTS = 'O'
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
     C                   EVAL      W#VIEW = *OFF
     C                   EVAL      W#SEL = 0
     C                   EXSR      @EDIT
     C                   EXSR      @LOAD
     C                   ITER
     C                   ENDIF
     C                   EVAL      W#SEL = 0
     C                   IF        W#CNT > 0
     C                   READC     LSFL
     C                   DOW       NOT %EOF(QUOTENTD)
     C                   IF        LOPT <> ' ' AND W#SEL = 0
     C                   EVAL      W#SEL = LID
     C                   EVAL      W#OPT = LOPT
     C                   ENDIF
     C                   READC     LSFL
     C                   ENDDO
     C                   ENDIF
     C                   IF        W#SEL > 0
     C                   SELECT
     C                   WHEN      W#OPT = '1'
     C                   EXSR      @CONVERT
     C                   WHEN      W#OPT = '2'
     C                   EVAL      W#ADD = *OFF
     C                   EVAL      W#VIEW = *OFF
     C                   EXSR      @EDIT
     C                   WHEN      W#OPT = '4'
     C                   EXSR      @CANCEL
     C                   WHEN      W#OPT = '5'
     C                   EVAL      W#ADD = *OFF
     C                   EVAL      W#VIEW = *ON
     C                   EXSR      @EDIT
     C                   ENDSL
     C                   ENDIF
     C                   EXSR      @LOAD
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C                   RETURN

      * @LOAD - 見積の一覧
     C     @LOAD         BEGSR
     C                   EVAL      W#TODAY = %DEC(%DATE():*ISO)
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
     C+   FETCH LSTCSR INTO :H#ID, :H#NUM, :H#CUSTCD, :H#QDATE, :H#VALID,
     C+                     :H#TOT, :H#STS
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      LOPT = ' '
     C                   EVAL      LID = H#ID
     C                   EVAL      LQTNUM = H#NUM
     C                   EVAL      LCUST = H#CUSTCD
     C                   EVAL      LQDATE = %CHAR(%DATE(H#QDATE:*ISO):*ISO)
     C                   EVAL      LVALID = %CHAR(%DATE(H#VALID:*ISO):*ISO)
     C                   EVAL      W#P2 = H#TOT
     C                   EVALR     LAMT = %TRIM(%EDITC(W#P2:'J'))
     C                   EVAL      *IN61 = *OFF
     C                   SELECT
     C                   WHEN      H#STS = 'ORDERED'
     C                   EVAL      LSTS = '受注済'
     C                   WHEN      H#STS = 'CANCELLED'
     C                   EVAL      LSTS = '取消'
     C                   WHEN      H#VALID < W#TODAY
     C                   EVAL      LSTS = '期限切れ'
     C                   EVAL      *IN61 = *ON
     C                   OTHER
     C                   EVAL      LSTS = '未受注'
     C                   ENDSL
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
     C                   EVAL      LMSG = '見積がありません(F6=新規)'
     C                   ENDIF
     C                   ENDSR

      * @HEADER - 見積のヘッダを読む(H#xx)
     C     @HEADER       BEGSR
     C                   EVAL      W#ERR = *OFF
     C/EXEC SQL
     C+   SELECT Q.QTNUM, Q.CUSTID, COALESCE(C.CUSTCD, ' '), Q.QTDATE,
     C+          Q.VALIDTO, Q.WHID, Q.STATUS, Q.SONUM, Q.QTNOTE
     C+     INTO :H#NUM, :H#CUSTID, :H#CUSTCD, :H#QDATE, :H#VALID,
     C+          :H#WHID, :H#STS, :H#SONUM, :H#NOTE
     C+     FROM QUOTEHP Q
     C+     LEFT JOIN CUSTOMERP C ON C.CUSTID = Q.CUSTID
     C+    WHERE Q.QTID = :W#SEL
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      LMSG = '見積が見つかりません'
     C                   EVAL      W#ERR = *ON
     C                   ENDIF
     C                   ENDSR

      * @CANCEL - 取消(未受注のみ)
     C     @CANCEL       BEGSR
     C                   EXSR      @HEADER
     C                   IF        W#ERR
     C                   LEAVESR
     C                   ENDIF
     C                   IF        H#STS <> 'OPEN'
     C                   EVAL      LMSG = '未受注の見積だけ取り消せます'
     C                   LEAVESR
     C                   ENDIF
     C                   EXSR      @STAMP
     C/EXEC SQL
     C+   UPDATE QUOTEHP SET STATUS = 'CANCELLED', UPDUSER = :W#USER,
     C+          UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHERE QTID = :W#SEL AND STATUS = 'OPEN'
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      LMSG = '取消エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   ELSE
     C                   EVAL      LMSG = %TRIM(H#NUM) + ' を取り消しました'
     C                   ENDIF
     C                   ENDSR

      * @CONVERT - 受注へ変換(CREATESORD)
     C     @CONVERT      BEGSR
     C                   EXSR      @HEADER
     C                   IF        W#ERR
     C                   LEAVESR
     C                   ENDIF
     C                   SELECT
     C                   WHEN      H#STS = 'ORDERED'
     C                   EVAL      LMSG = %TRIM(H#NUM) + ' は受注済です(' +
     C                             %TRIM(H#SONUM) + ')'
     C                   LEAVESR
     C                   WHEN      H#STS <> 'OPEN'
     C                   EVAL      LMSG = '取消済の見積は変換できません'
     C                   LEAVESR
     C                   WHEN      H#VALID < %DEC(%DATE():*ISO)
     C                   EVAL      LMSG = '期限切れです。2=変更で期限を' +
     C                             '延ばしてから変換してください'
     C                   LEAVESR
     C                   ENDSL
      * 明細をパラメーターに詰める
     C                   EVAL      C_COUNT = 0
     C                   EVAL      H#ID = W#SEL
     C/EXEC SQL
     C+   OPEN LINCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND C_COUNT < 20
     C/EXEC SQL
     C+   FETCH LINCSR INTO :H#LINE, :H#ITEMID, :H#ITEMCD, :H#QTY, :H#PRC,
     C+                     :H#RATE
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      C_COUNT = C_COUNT + 1
     C                   EVAL      C_ITEMID(C_COUNT) = H#ITEMID
     C                   EVAL      C_QTY(C_COUNT) = H#QTY
     C                   EVAL      C_PRC(C_COUNT) = H#PRC
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE LINCSR
     C/END-EXEC
     C                   IF        C_COUNT = 0
     C                   EVAL      LMSG = '明細のない見積は変換できません'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      C_CUSTID = H#CUSTID
     C                   EVAL      C_WHID = H#WHID
     C                   EXSR      @CUSTINFO
     C                   EVAL      C_TAXRATE = W#CTAX
      * 作られた受注を見分けるため、呼ぶ前の日時を覚えておく
      *   (CREATESORD の受注IDは受注入力の受注IDより小さいことがある)
     C                   EXSR      @STAMP
     C                   MONITOR
     C                   CALLP     CREATESORD(C_CUSTID:C_WHID:C_TAXRATE:
     C                             C_COUNT:C_ITEMID:C_QTY:C_PRC)
     C                   ON-ERROR
     C                   EVAL      LMSG = '受注にできません: ' +
     C                             %TRIMR(PS_MSGDTA)
     C                   LEAVESR
     C                   ENDMON
     C                   EVAL      W#SONUM = *BLANKS
     C/EXEC SQL
     C+   SELECT SONUM INTO :W#SONUM FROM SORDERHP
     C+    WHERE CUSTID = :C_CUSTID AND CRTUSER = USER
     C+      AND (CRTDATE > :W#D8 OR (CRTDATE = :W#D8 AND CRTTIM >= :W#T6))
     C+    ORDER BY CRTDATE DESC, CRTTIM DESC, SOID DESC
     C+    FETCH FIRST 1 ROW ONLY
     C/END-EXEC
     C                   EXSR      @STAMP
     C/EXEC SQL
     C+   UPDATE QUOTEHP SET STATUS = 'ORDERED', SONUM = :W#SONUM,
     C+          UPDUSER = :W#USER, UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHERE QTID = :W#SEL
     C/END-EXEC
     C                   EVAL      LMSG = %TRIM(H#NUM) + ' を受注 ' +
     C                             %TRIM(W#SONUM) + ' にしました'
     C                   ENDSR

      * @CUSTINFO - 顧客の税区分・既定税率・状態(H#CUSTID)
     C     @CUSTINFO     BEGSR
     C                   EVAL      W#CCAT = *BLANKS
     C                   EVAL      W#CSTS = *BLANKS
     C/EXEC SQL
     C+   SELECT TAXCAT, STATUS, CURRCD INTO :W#CCAT, :W#CSTS, :H#CURR
     C+     FROM CUSTOMERP WHERE CUSTID = :H#CUSTID
     C/END-EXEC
     C                   SELECT
     C                   WHEN      W#CCAT = 'REDUCED'
     C                   EVAL      W#CTAX = 0.080
     C                   WHEN      W#CCAT = 'EXEMPT' OR W#CCAT = 'EXPORT'
     C                   EVAL      W#CTAX = 0
     C                   OTHER
     C                   EVAL      W#CTAX = 0.100
     C                   ENDSL
     C                   IF        H#CURR = *BLANKS
     C                   EVAL      H#CURR = 'JPY'
     C                   ENDIF
     C                   ENDSR

      * @EDIT - 登録・変更・表示
     C     @EDIT         BEGSR
     C                   EVAL      DMSG = *BLANKS
     C                   EVAL      L_ITEM = *BLANKS
     C                   EVAL      L_QTY = 0
     C                   EVAL      L_PRC = 0
     C                   EVAL      L_RATE = 0
     C                   EVAL      L_ERR = *BLANKS
     C                   IF        W#ADD
     C                   EVAL      DMODE = '見積の登録'
     C                   EVAL      DQTNUM = '(保存時に採番)'
     C                   EVAL      DQDATE = %DEC(%DATE():*ISO)
     C                   EVAL      DVALID = %DEC(%DATE() + %DAYS(30):*ISO)
     C                   EVAL      DCUST = *BLANKS
     C                   EVAL      DWHID = 1
     C                   EVAL      DNOTE = *BLANKS
     C                   EVAL      DSTS = '未受注'
     C                   EVAL      DSONUM = *BLANKS
     C                   ELSE
     C                   EXSR      @HEADER
     C                   IF        W#ERR
     C                   LEAVESR
     C                   ENDIF
     C                   IF        NOT W#VIEW AND H#STS <> 'OPEN'
     C                   EVAL      LMSG = '未受注の見積だけ変更できます' +
     C                             '(5=表示で見られます)'
     C                   LEAVESR
     C                   ENDIF
     C                   IF        W#VIEW
     C                   EVAL      DMODE = '見積の表示'
     C                   ELSE
     C                   EVAL      DMODE = '見積の変更'
     C                   ENDIF
     C                   EVAL      DQTNUM = H#NUM
     C                   EVAL      DQDATE = H#QDATE
     C                   EVAL      DVALID = H#VALID
     C                   EVAL      DCUST = H#CUSTCD
     C                   EVAL      DWHID = H#WHID
     C                   EVAL      DNOTE = H#NOTE
     C                   SELECT
     C                   WHEN      H#STS = 'ORDERED'
     C                   EVAL      DSTS = '受注済'
     C                   WHEN      H#STS = 'CANCELLED'
     C                   EVAL      DSTS = '取消'
     C                   OTHER
     C                   EVAL      DSTS = '未受注'
     C                   ENDSL
     C                   EVAL      DSONUM = H#SONUM
     C                   EVAL      W#I = 0
     C                   EVAL      H#ID = W#SEL
     C/EXEC SQL
     C+   OPEN LINCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#I < 10
     C/EXEC SQL
     C+   FETCH LINCSR INTO :H#LINE, :H#ITEMID, :H#ITEMCD, :H#QTY, :H#PRC,
     C+                     :H#RATE
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#I = W#I + 1
     C                   EVAL      L_ITEM(W#I) = H#ITEMCD
     C                   EVAL(H)   L_QTY(W#I) = H#QTY
     C                   EVAL(H)   L_PRC(W#I) = H#PRC
     C                   EVAL      L_RATE(W#I) = H#RATE
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE LINCSR
     C/END-EXEC
     C                   ENDIF
     C                   EVAL      *IN50 = W#VIEW
     C                   EXSR      @NAMES
     C                   EXSR      @CALC
     C                   EVAL      DMSG = *BLANKS
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
     C                   IF        *IN12 OR W#VIEW
     C                   LEAVE
     C                   ENDIF
     C                   EXSR      @RDDTL
     C                   IF        W#F4
     C                   EXSR      @PROMPT
     C                   EXSR      @NAMES
     C                   EXSR      @CALC
     C                   ITER
     C                   ENDIF
     C                   EXSR      @CHECK
     C                   IF        W#ERR
     C                   ITER
     C                   ENDIF
     C                   EXSR      @CALC
     C                   IF        NOT W#F6
     C                   EVAL      DMSG = '計算しました。F6で保存します'
     C                   ITER
     C                   ENDIF
     C                   EXSR      @SAVE
     C                   IF        NOT W#ERR
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   EVAL      *IN50 = *OFF
     C                   ENDSR

      * @WRTDTL - 明細の配列を画面のサブファイルへ
     C     @WRTDTL       BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     DCTL
     C                   EVAL      *IN93 = *OFF
     C                   FOR       W#I = 1 TO 10
     C                   EVAL      SNO = W#I
     C                   EVAL      SITEM = L_ITEM(W#I)
     C                   EVAL      SINAME = L_NAME(W#I)
     C                   EVAL      SQTY = L_QTY(W#I)
     C                   EVAL      SPRC = L_PRC(W#I)
     C                   IF        L_AMT(W#I) <> 0
     C                   EVAL      W#P2 = L_AMT(W#I)
     C                   EVALR     SAMT = %TRIM(%EDITC(W#P2:'J'))
     C                   ELSE
     C                   EVAL      SAMT = *BLANKS
     C                   ENDIF
     C                   EVAL      *IN41 = (L_ERR(W#I) = 'I')
     C                   EVAL      *IN42 = (L_ERR(W#I) = 'Q')
     C                   EVAL      *IN43 = (L_ERR(W#I) = 'P')
     C                   EVAL      W#DRRN = W#I
     C                   WRITE     DSFL
     C                   ENDFOR
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN42 = *OFF
     C                   EVAL      *IN43 = *OFF
     C                   ENDSR

      * @RDDTL - 画面のサブファイルを明細の配列へ
     C     @RDDTL        BEGSR
     C                   FOR       W#I = 1 TO 10
     C     W#I           CHAIN     DSFL
     C                   IF        %FOUND(QUOTENTD)
     C                   EVAL      L_ITEM(W#I) = SITEM
     C                   EVAL      L_QTY(W#I) = SQTY
     C                   EVAL      L_PRC(W#I) = SPRC
     C                   ENDIF
     C                   ENDFOR
     C                   ENDSR

      * @PROMPT - F4(カーソル位置の項目を検索)
     C     @PROMPT       BEGSR
     C                   SELECT
     C                   WHEN      CSRFLD = 'DCUST'
     C                   EVAL      W#PVAL = DCUST
     C                   CALLP     O2CPRMPT('CUST':' ':W#PVAL)
     C                   EVAL      DCUST = W#PVAL
     C                   WHEN      CSRFLD = 'SITEM' AND CSRRRN >= 1 AND
     C                             CSRRRN <= 10
     C                   EVAL      W#PVAL = L_ITEM(CSRRRN)
     C                   CALLP     O2CPRMPT('ITEM':' ':W#PVAL)
     C                   EVAL      L_ITEM(CSRRRN) = W#PVAL
     C                   OTHER
     C                   EVAL      DMSG = '顧客か商品コードにカーソルを' +
     C                             '置いてF4'
     C                   ENDSL
     C                   ENDSR

      * @NAMES - 顧客名・倉庫名・商品名
     C     @NAMES        BEGSR
     C                   EVAL      DCUSTNM = *BLANKS
     C                   EVAL      DWHNM = *BLANKS
     C                   EVAL      H#CUSTID = -1
     C                   IF        DCUST <> *BLANKS
     C/EXEC SQL
     C+   SELECT CUSTID, CUSTNM INTO :H#CUSTID, :H#NM
     C+     FROM CUSTOMERP WHERE CUSTCD = :DCUST
     C/END-EXEC
     C                   IF        SQLCODE = 0
     C                   EVAL      DCUSTNM = DBCSTRUNC(%TRIMR(H#NM):30)
     C                   ELSE
     C                   EVAL      H#CUSTID = -1
     C                   EVAL      DCUSTNM = '(顧客なし)'
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
     C                   EVAL      L_NAME(W#I) = DBCSTRUNC(%TRIMR(H#NM):20)
     C                   ENDIF
     C                   ENDIF
     C                   ENDFOR
     C                   ENDSR

      * @CHECK - 入力チェック(単価0は単価を決めて入れる)
     C     @CHECK        BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      *IN31 = *OFF
     C                   EVAL      *IN32 = *OFF
     C                   EVAL      *IN33 = *OFF
     C                   EVAL      *IN34 = *OFF
     C                   EVAL      L_ERR = *BLANKS
     C                   EXSR      @NAMES
     C                   IF        H#CUSTID > 0
     C                   EXSR      @CUSTINFO
     C                   ENDIF
     C     *ISO          TEST(DE)                DQDATE
     C                   IF        %ERROR
     C                   EVAL      *IN32 = *ON
     C                   EVAL      DMSG = '見積日が正しくありません'
     C                   ENDIF
     C                   IF        DMSG = *BLANKS
     C     *ISO          TEST(DE)                DVALID
     C                   IF        %ERROR
     C                   EVAL      *IN33 = *ON
     C                   EVAL      DMSG = '有効期限が正しくありません'
     C                   ENDIF
     C                   ENDIF
     C                   IF        DMSG = *BLANKS
     C                   SELECT
     C                   WHEN      DCUST = *BLANKS OR H#CUSTID < 0
     C                   EVAL      *IN31 = *ON
     C                   EVAL      DMSG = '顧客を入力してください(F4=検索)'
     C                   WHEN      W#CSTS <> 'ACTIVE'
     C                   EVAL      *IN31 = *ON
     C                   EVAL      DMSG = '取引停止の顧客です'
     C                   WHEN      W#WSTS <> 'ACTIVE'
     C                   EVAL      *IN34 = *ON
     C                   EVAL      DMSG = '倉庫が見つからないか停止中です'
     C                   WHEN      DVALID < DQDATE
     C                   EVAL      *IN33 = *ON
     C                   EVAL      DMSG = '有効期限は見積日以降にしてください'
     C                   ENDSL
     C                   ENDIF
     C                   IF        DMSG <> *BLANKS
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
      * 明細
     C                   EVAL      W#N = 0
     C                   FOR       W#I = 1 TO 10
     C                   IF        L_ITEM(W#I) = *BLANKS
     C                   IF        L_QTY(W#I) <> 0 OR L_PRC(W#I) <> 0
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
     C+   SELECT TAXCAT, STATUS, UNITPRC INTO :W#ITAX, :W#ISTS, :W#STDPRC
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
     C                   EVAL      DMSG = %CHAR(W#I) + '行目: 数量を' +
     C                             '入力してください'
     C                   LEAVE
     C                   ENDIF
     C                   IF        L_PRC(W#I) < 0
     C                   EVAL      L_ERR(W#I) = 'P'
     C                   EVAL      DMSG = %CHAR(W#I) + '行目: 単価が' +
     C                             '正しくありません'
     C                   LEAVE
     C                   ENDIF
     C                   IF        L_PRC(W#I) = 0
     C                   EXSR      @PRICE
     C                   ENDIF
      * 税率(受注入力と同じ決め方)
     C                   SELECT
     C                   WHEN      W#CCAT = 'EXEMPT' OR W#CCAT = 'EXPORT'
     C                   EVAL      L_RATE(W#I) = 0
     C                   WHEN      W#ITAX = 'REDUCED'
     C                   EVAL      L_RATE(W#I) = 0.080
     C                   WHEN      W#ITAX = 'EXEMPT' OR W#ITAX = 'EXPORT'
     C                   EVAL      L_RATE(W#I) = 0
     C                   WHEN      W#ITAX = 'STANDARD'
     C                   EVAL      L_RATE(W#I) = 0.100
     C                   OTHER
     C                   EVAL      L_RATE(W#I) = W#CTAX
     C                   ENDSL
     C                   EVAL      W#N = W#N + 1
     C                   ENDFOR
     C                   IF        DMSG = *BLANKS AND W#N = 0
     C                   EVAL      L_ERR(1) = 'I'
     C                   EVAL      DMSG = '明細を1行以上入力してください'
     C                   ENDIF
     C                   IF        DMSG <> *BLANKS
     C                   EVAL      W#ERR = *ON
     C                   ENDIF
     C                   ENDSR

      * @PRICE - 単価0の行に単価を入れる(受注入力の @PRICE と同じ順)
     C     @PRICE        BEGSR
     C                   EVAL      H#PRC = W#STDPRC
     C                   EVAL      H#QTY = L_QTY(W#I)
     C                   EVAL      H#QDATE = DQDATE
     C/EXEC SQL
     C+   SELECT P.UNITPRC INTO :H#PRC FROM PRICEP P
     C+    WHERE P.ITEMID = :H#ITEMID AND P.CUSTID IN (:H#CUSTID, 0)
     C+      AND P.MINQTY <= :H#QTY AND P.FROMDATE <= :H#QDATE
     C+      AND (P.TODATE = 0 OR P.TODATE >= :H#QDATE)
     C+    ORDER BY CASE WHEN P.CUSTID = 0 THEN 1 ELSE 0 END,
     C+          P.MINQTY DESC, P.FROMDATE DESC
     C+    FETCH FIRST 1 ROW ONLY
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      H#PRC = W#STDPRC
     C                   ENDIF
     C                   EVAL(H)   L_PRC(W#I) = H#PRC
     C                   ENDSR

      * @CALC - 金額・合計・消費税(税は税率ごとに合計して端数切り捨て)
     C     @CALC         BEGSR
     C                   EVAL      W#TOT = 0
     C                   EVAL      W#TAX = 0
     C                   FOR       W#I = 1 TO 10
     C                   EVAL(H)   L_AMT(W#I) = L_QTY(W#I) * L_PRC(W#I)
     C                   IF        L_ITEM(W#I) = *BLANKS
     C                   EVAL      L_AMT(W#I) = 0
     C                   ENDIF
     C                   EVAL      W#TOT = W#TOT + L_AMT(W#I)
     C                   ENDFOR
     C                   EVAL      W#P2 = 0
     C                   FOR       W#I = 1 TO 10
     C                   IF        L_RATE(W#I) = 0.080
     C                   EVAL      W#P2 = W#P2 + L_AMT(W#I)
     C                   ENDIF
     C                   ENDFOR
     C                   EVAL      W#TAX = %INT(W#P2 * 0.08)
     C                   EVAL      W#P2 = 0
     C                   FOR       W#I = 1 TO 10
     C                   IF        L_RATE(W#I) = 0.100
     C                   EVAL      W#P2 = W#P2 + L_AMT(W#I)
     C                   ENDIF
     C                   ENDFOR
     C                   EVAL      W#TAX = W#TAX + %INT(W#P2 * 0.10)
     C                   EVAL      W#P2 = W#TOT
     C                   EVALR     DTOT = %TRIM(%EDITC(W#P2:'J'))
     C                   EVAL      W#P2 = W#TAX
     C                   EVALR     DTAX = %TRIM(%EDITC(W#P2:'J'))
     C                   EVAL      W#P2 = W#TOT + W#TAX
     C                   EVALR     DGRAND = %TRIM(%EDITC(W#P2:'J'))
     C                   ENDSR

      * @SAVE - 登録・更新(明細は消して入れ直す)
     C     @SAVE         BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EXSR      @STAMP
     C                   EVAL      H#QDATE = DQDATE
     C                   EVAL      H#VALID = DVALID
     C                   EVAL      H#WHID = DWHID
     C                   EVAL      H#TOT = W#TOT
     C                   EVAL      H#TAX = W#TAX
     C                   EVAL      H#NOTE = DNOTE
     C                   IF        W#ADD
     C/EXEC SQL
     C+   VALUES NEXT VALUE FOR O2C_QTID_SEQ INTO :H#ID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      DMSG = '見積ID採番エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      H#NUM = 'QT-' + %CHAR(H#ID)
     C/EXEC SQL
     C+   INSERT INTO QUOTEHP (QTID, QTNUM, CUSTID, QTDATE, VALIDTO,
     C+          CURRCD, WHID, STATUS, TOTAMT, TAXAMT, SONUM, QTNOTE,
     C+          CRTUSER, CRTDATE, CRTTIM, UPDUSER, UPDDATE, UPDTIM)
     C+   VALUES (:H#ID, :H#NUM, :H#CUSTID, :H#QDATE, :H#VALID, :H#CURR,
     C+          :H#WHID, 'OPEN', :H#TOT, :H#TAX, ' ', :H#NOTE,
     C+          :W#USER, :W#D8, :W#T6, :W#USER, :W#D8, :W#T6)
     C/END-EXEC
     C                   ELSE
     C                   EVAL      H#ID = W#SEL
     C/EXEC SQL
     C+   UPDATE QUOTEHP
     C+      SET CUSTID = :H#CUSTID, QTDATE = :H#QDATE,
     C+          VALIDTO = :H#VALID, CURRCD = :H#CURR, WHID = :H#WHID,
     C+          TOTAMT = :H#TOT, TAXAMT = :H#TAX, QTNOTE = :H#NOTE,
     C+          UPDUSER = :W#USER, UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHERE QTID = :H#ID AND STATUS = 'OPEN'
     C/END-EXEC
     C                   ENDIF
     C                   IF        SQLCODE <> 0
     C                   EVAL      DMSG = '登録エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C/EXEC SQL
     C+   DELETE FROM QUOTELP WHERE QTID = :H#ID
     C/END-EXEC
     C                   EVAL      H#LINE = 0
     C                   FOR       W#I = 1 TO 10
     C                   IF        L_ITEM(W#I) = *BLANKS
     C                   ITER
     C                   ENDIF
     C                   EVAL      H#LINE = H#LINE + 1
     C                   EVAL      H#ITEMID = L_ITEMID(W#I)
     C                   EVAL      H#QTY = L_QTY(W#I)
     C                   EVAL      H#PRC = L_PRC(W#I)
     C                   EVAL      H#RATE = L_RATE(W#I)
     C                   EVAL      H#AMT = L_AMT(W#I)
     C/EXEC SQL
     C+   INSERT INTO QUOTELP (QTID, LINENO, ITEMID, QTQTY, UNITPRC,
     C+          TAXRATE, LINEAMT)
     C+   VALUES (:H#ID, :H#LINE, :H#ITEMID, :H#QTY, :H#PRC, :H#RATE,
     C+          :H#AMT)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      DMSG = '明細の登録エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   ENDFOR
     C                   IF        W#ADD
     C                   EVAL      LMSG = %TRIM(H#NUM) + ' を登録しました'
     C                   ELSE
     C                   EVAL      LMSG = %TRIM(H#NUM) + ' を変更しました'
     C                   ENDIF
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
