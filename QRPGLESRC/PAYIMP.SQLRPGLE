     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: PAYIMP
      * 説明: 入金データの取込・自動消込・前受金の充当
      * 作成: 修正#BNK 新規作成。販売管理メニューの20から呼ぶ
      *   F6  銀行の入金データ(CSV UTF-8: 入金日,金額,振込依頼人)を
      *       BANKINP に取り込む。半角カナは全角にする
      *   推定 振込依頼人を顧客のカナ名(CUSTKANA)と比べて顧客を決め
      *       (空白・カ)などの法人の略称・小書きの違いは無視)
      *       請求書の残高と比べて消込のしかたを決める
      *         EXACT 残高と一致する請求書  FEE 差額が手数料の範囲内
      *         TOTAL 未収合計と一致(手数料の範囲内を含む)
      *         FIFO  古い請求書から  ADVANCE 消し込めない分は前受金
      *       手数料の範囲はコードマスター BANK/FEETOL(既定 880円)
      *   1   入金を登録して消し込む(PAYMETH BANK)。手数料は別の入金
      *       (PAYMETH BANKFEE)として同じ請求書に消し込む
      *       残りがあれば入金の状態を UNAPPLIED(前受金)にする
      *   2   顧客・請求書を指定する(顧客のカナ名の登録もできる)
      *   4   対象外にする(もう一度4で戻す)
      *   F10 推定済の入金データをまとめて登録
      *   F8  前受金(UNAPPLIED)を古い請求書から充当する
      *   円の請求書だけを消し込む
      *=====================================================================
     FPAYIMPD   CF   E             WORKSTN SFILE(LSFL:W#RRN)
      * 顧客のカナ名(A型の項目)はSQLでは全角を書けないのでレコードで更新
     FCUSTOMERP UF   E           K DISK    PREFIX(C_) USROPN

     D W#RRN           S              4S 0
     D W#CNT           S              4S 0
     D W#SQLCD         S             10I 0
     D W#ERR           S               N
     D W#I             S             10I 0
     D W#J             S             10I 0
     D W#N             S             10I 0
     D W#OPT           S              1A
     D W#PVAL          S             30A
     D W#USER          S             10A   INZ(*USER)
     D W#D8            S              8P 0
     D W#T6            S              6P 0
     D W#TOL           S             15P 2
     D W#TXT           S             20A
     D W#P0            S             11P 0
     D W#MSG           S             78A
      * 選んだ行
     D W#SELN          S             10I 0
     D W#SELID         S             18P 0 DIM(999)
     D W#SELOP         S              1A   DIM(999)
      * 取込
     D W#LINE          S           1000C   VARYING
     D W#FLD           S            200C   VARYING DIM(5)
     D W#NF            S             10I 0
     D W#INQ           S               N
     D W#U             S            200C   VARYING
     D W#K             S            200C   VARYING
     D W#C             S             60A
     D W#IMP           S             10I 0
     D W#DUP           S             10I 0
     D W#BAD           S             10I 0
      * UCS-2 の1文字と文字コード
     D W#CC            S             10I 0
     D W#HFROM         S             63C   VARYING
     D W#HTO           S             63C   VARYING
     D W#PC            S             10I 0
     D W#UC            DS
     D  W#UCC                         1C
     D  W#UCN                         5U 0 OVERLAY(W#UC)
      * 顧客のカナ名(比較用)
     D W#CUSTN         S             10I 0
     D W#CID           S              9P 0 DIM(999)
     D W#CKEY          S             80C   VARYING DIM(999)
      * 消込
     D W#REM           S             15P 2
     D W#APPLY         S             15P 2
     D W#PAYID         S             18P 0
     D W#PAYNUM        S             20A
     D W#PSTS          S             12A
     D W#METH          S             20A
     D W#ADVN          S             10I 0
     D W#ADVAMT        S             15P 2
     D W#AID           S             18P 0 DIM(500)
     D W#AREM          S             15P 2 DIM(500)
     D W#ACUST         S              9P 0 DIM(500)
     D W#ACUR          S              3A   DIM(500)
      * SQL 用
     D H#PATH          S            128A   VARYING
     D H#F             S              1A
     D H#ID            S             18P 0
     D H#PDATE         S              8P 0
     D H#AMT           S             15P 2
     D H#PAYER         S             48A
     D H#CUSTID        S              9P 0
     D H#CUSTCD        S             20A
     D H#INVID         S             18P 0
     D H#INVNUM        S             20A
     D H#FEE           S             15P 2
     D H#TYPE          S             10A
     D H#STS           S             10A
     D H#KANA          S             40A
     D H#NM            S             60A
     D H#CUR           S              3A
     D H#BAL           S             15P 2
     D H#PAID          S             15P 2
     D H#ISTS          S             12A
     D H#TOT           S             15P 2
     D H#APPID         S             18P 0
     D H#AID           S             18P 0
     D H#REF           S             60A
     D O2CPRMPT        PR                  EXTPGM('O2CPRMPT')
     D   PTYPE                       10A   CONST
     D   PCDTYPE                     30A   CONST
     D   PVALUE                      30A
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
     D DBCSTRUNC       PR           256A   VARYING
     D   PSTR                       256A   VARYING CONST
     D   PMAX                        10I 0 CONST
     D NORMKEY         PR           200C   VARYING
     D   PSTR                       200C   VARYING CONST

     C/EXEC SQL
     C+   SET OPTION COMMIT = *CHG, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE LSTCSR CURSOR FOR
     C+   SELECT B.BKID, B.PAYDATE, B.AMOUNT, B.PAYERNM,
     C+          COALESCE(C.CUSTCD, ' '), B.MATCHTYP, B.STATUS
     C+     FROM BANKINP B
     C+     LEFT JOIN CUSTOMERP C ON C.CUSTID = B.CUSTID
     C+    WHERE :H#F = 'A' OR B.STATUS IN ('NEW', 'MATCHED')
     C+    ORDER BY B.BKID
     C+    FOR READ ONLY
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE IMPCSR CURSOR FOR
     C+   SELECT CAST(LINE AS VARGRAPHIC(1000) CCSID 13488)
     C+     FROM TABLE(QSYS2.IFS_READ_UTF8(PATH_NAME => :H#PATH)) X
     C+    FOR READ ONLY
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE CUSCSR CURSOR FOR
     C+   SELECT CUSTID, CAST(CUSTKANA AS CHAR(40) FOR BIT DATA)
     C+     FROM CUSTOMERP
     C+    WHERE CUSTKANA <> ' ' AND STATUS = 'ACTIVE'
     C+    ORDER BY CUSTID
     C+    FOR READ ONLY
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE ADVCSR CURSOR FOR
     C+   SELECT P.PAYID, P.CUSTID, P.CURRCD, P.AMOUNT -
     C+          COALESCE((SELECT SUM(A.APPLYAMT) FROM PAYAPPLYP A
     C+                     WHERE A.PAYID = P.PAYID), 0)
     C+     FROM PAYMENTP P
     C+    WHERE P.STATUS = 'UNAPPLIED'
     C+    ORDER BY P.PAYDATE, P.PAYID
     C+    FOR READ ONLY
     C/END-EXEC

     C                   EVAL      LPATH = '/home/' + %TRIM(W#USER) +
     C                             '/bank/bank.csv'
     C                   EVAL      LFSTS = ' '
     C                   EVAL      LMSG = *BLANKS
     C                   EXSR      @SETUP
     C                   EXSR      @LOAD
     C                   DOW       *ON
     C                   EVAL      *IN92 = *ON
     C                   EVAL      *IN91 = (W#CNT > 0)
     C                   EVAL      *IN94 = *ON
     C                   WRITE     LFOOT
     C                   EXFMT     LCTL
     C                   EVAL      LMSG = *BLANKS
     C                   SELECT
     C                   WHEN      *IN03 OR *IN12
     C                   LEAVE
     C                   WHEN      *IN06
     C                   EXSR      @IMPORT
     C                   WHEN      *IN05
     C                   EXSR      @REMATCH
     C                   WHEN      *IN08
     C                   EXSR      @ADVAPPLY
     C                   WHEN      *IN10
     C                   EXSR      @APPLYALL
     C                   OTHER
     C                   EXSR      @OPTIONS
     C                   ENDSL
     C                   EXSR      @LOAD
     C                   ENDDO
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   EVAL      *INLR = *ON
     C                   RETURN

      * @SETUP - 手数料の範囲・顧客のカナ名
     C     @SETUP        BEGSR
     C                   EVAL      W#TOL = 880
     C                   EVAL      W#TXT = *BLANKS
     C/EXEC SQL
     C+   SELECT CAST(DESC AS VARCHAR(20) FOR BIT DATA) INTO :W#TXT
     C+     FROM CODEMSTRP WHERE CODETYPE = 'BANK' AND CODEVAL = 'FEETOL'
     C/END-EXEC
     C                   IF        SQLCODE = 0 AND W#TXT <> *BLANKS
     C                   MONITOR
     C                   EVAL      W#TOL = %DEC(%TRIM(W#TXT):15:2)
     C                   ON-ERROR
     C                   EVAL      W#TOL = 880
     C                   ENDMON
     C                   ENDIF
     C                   EXSR      @LOADKANA
     C                   ENDSR

      * @LOADKANA - 顧客のカナ名を比較用の形にして覚える
     C     @LOADKANA     BEGSR
     C                   EVAL      W#CUSTN = 0
     C/EXEC SQL
     C+   OPEN CUSCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#CUSTN < 999
     C/EXEC SQL
     C+   FETCH CUSCSR INTO :H#CUSTID, :H#KANA
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#CUSTN = W#CUSTN + 1
     C                   EVAL      W#CID(W#CUSTN) = H#CUSTID
     C                   EVAL      W#CKEY(W#CUSTN) =
     C                             NORMKEY(%UCS2(%TRIMR(H#KANA)))
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE CUSCSR
     C/END-EXEC
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   ENDSR

      * @LOAD - 入金データの一覧と前受金の合計
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
     C+   FETCH LSTCSR INTO :H#ID, :H#PDATE, :H#AMT, :H#PAYER,
     C+                     :H#CUSTCD, :H#TYPE, :H#STS
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      LOPT = ' '
     C                   EVAL      LID = H#ID
     C                   EVAL      LPDATE = %CHAR(%DATE(H#PDATE:*ISO):*ISO)
     C                   EVAL(H)   W#P0 = H#AMT
     C                   EVALR     LAMT = %TRIM(%EDITC(W#P0:'J'))
     C                   EVAL      LPAYER = DBCSTRUNC(%TRIMR(H#PAYER):20)
     C                   EVAL      LCUST = H#CUSTCD
     C                   SELECT
     C                   WHEN      H#TYPE = 'EXACT'
     C                   EVAL      LTYPE = '一致'
     C                   WHEN      H#TYPE = 'FEE'
     C                   EVAL      LTYPE = '手数料'
     C                   WHEN      H#TYPE = 'TOTAL'
     C                   EVAL      LTYPE = '合計'
     C                   WHEN      H#TYPE = 'FIFO'
     C                   EVAL      LTYPE = '古い順'
     C                   WHEN      H#TYPE = 'ADVANCE'
     C                   EVAL      LTYPE = '前受金'
     C                   OTHER
     C                   EVAL      LTYPE = *BLANKS
     C                   ENDSL
     C                   EVAL      *IN61 = *OFF
     C                   SELECT
     C                   WHEN      H#STS = 'NEW'
     C                   EVAL      LSTS = '不明'
     C                   EVAL      *IN61 = *ON
     C                   WHEN      H#STS = 'MATCHED'
     C                   EVAL      LSTS = '推定済'
     C                   WHEN      H#STS = 'APPLIED'
     C                   EVAL      LSTS = '登録済'
     C                   OTHER
     C                   EVAL      LSTS = '対象外'
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
     C                   EVAL      LMSG = '未処理の入金データはありません' +
     C                             '(F6=取込)'
     C                   ENDIF
     C                   EVAL      H#TOT = 0
     C/EXEC SQL
     C+   SELECT COALESCE(SUM(P.AMOUNT -
     C+          COALESCE((SELECT SUM(A.APPLYAMT) FROM PAYAPPLYP A
     C+                     WHERE A.PAYID = P.PAYID), 0)), 0)
     C+     INTO :H#TOT
     C+     FROM PAYMENTP P WHERE P.STATUS = 'UNAPPLIED'
     C/END-EXEC
     C                   EVAL(H)   W#P0 = H#TOT
     C                   EVALR     LADV = %TRIM(%EDITC(W#P0:'J'))
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   ENDSR

      * @OPTIONS - 一覧で選んだ行(先に集めてから処理する)
     C     @OPTIONS      BEGSR
     C                   EVAL      W#SELN = 0
     C                   IF        W#CNT = 0
     C                   LEAVESR
     C                   ENDIF
     C                   READC     LSFL
     C                   DOW       NOT %EOF(PAYIMPD)
     C                   IF        LOPT <> ' ' AND W#SELN < 999
     C                   EVAL      W#SELN = W#SELN + 1
     C                   EVAL      W#SELID(W#SELN) = LID
     C                   EVAL      W#SELOP(W#SELN) = LOPT
     C                   ENDIF
     C                   READC     LSFL
     C                   ENDDO
     C                   FOR       W#I = 1 TO W#SELN
     C                   EVAL      H#ID = W#SELID(W#I)
     C                   SELECT
     C                   WHEN      W#SELOP(W#I) = '1'
     C                   EXSR      @APPLY
     C                   WHEN      W#SELOP(W#I) = '2'
     C                   EXSR      @ASSIGN
     C                   WHEN      W#SELOP(W#I) = '4'
     C                   EXSR      @IGNORE
     C                   ENDSL
     C                   ENDFOR
     C                   ENDSR

      *=====================================================================
      * @IMPORT - CSV を取り込む(1行目が見出しなら読み飛ばす)
      *=====================================================================
     C     @IMPORT       BEGSR
     C                   EVAL      W#IMP = 0
     C                   EVAL      W#DUP = 0
     C                   EVAL      W#BAD = 0
     C                   EVAL      H#PATH = %TRIM(LPATH)
     C/EXEC SQL
     C+   OPEN IMPCSR
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      LMSG = 'ファイルを読めません SQLCODE=' +
     C                             %CHAR(SQLCODE) + ' ' + %TRIM(LPATH)
     C                   LEAVESR
     C                   ENDIF
     C                   DOW       *ON
     C/EXEC SQL
     C+   FETCH IMPCSR INTO :W#LINE
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EXSR      @SPLIT
     C                   IF        W#NF < 3
     C                   IF        %LEN(%TRIM(W#LINE)) > 0
     C                   EVAL      W#BAD = W#BAD + 1
     C                   ENDIF
     C                   ITER
     C                   ENDIF
      * 入金日(YYYYMMDD / YYYY-MM-DD / YYYY/MM/DD)
     C                   EVAL      W#C = %CHAR(W#FLD(1))
     C                   EVAL      W#C = %SCANRPL('/':'':W#C)
     C                   EVAL      W#C = %SCANRPL('-':'':W#C)
     C                   MONITOR
     C                   EVAL      H#PDATE = %DEC(%TRIM(W#C):8:0)
     C     *ISO          TEST(DE)                H#PDATE
     C                   IF        %ERROR
     C                   EVAL      W#BAD = W#BAD + 1
     C                   ITER
     C                   ENDIF
     C                   EVAL      W#C = %CHAR(W#FLD(2))
     C                   EVAL      W#C = %SCANRPL(',':'':W#C)
     C                   EVAL      H#AMT = %DEC(%TRIM(W#C):15:2)
     C                   ON-ERROR
      * 見出しの行など
     C                   EVAL      W#BAD = W#BAD + 1
     C                   ITER
     C                   ENDMON
     C                   IF        H#AMT <= 0
     C                   EVAL      W#BAD = W#BAD + 1
     C                   ITER
     C                   ENDIF
     C                   EXSR      @TOFULL
     C                   EVAL      H#PAYER = DBCSTRUNC(%CHAR(W#U):48)
      * 同じ入金日・金額・振込依頼人のデータは取り込まない
     C/EXEC SQL
     C+   SELECT COUNT(*) INTO :W#N FROM BANKINP
     C+    WHERE PAYDATE = :H#PDATE AND AMOUNT = :H#AMT
     C+      AND PAYERNM = :H#PAYER AND STATUS <> 'IGNORED'
     C/END-EXEC
     C                   IF        W#N > 0
     C                   EVAL      W#DUP = W#DUP + 1
     C                   ITER
     C                   ENDIF
     C                   EXSR      @STAMP
     C/EXEC SQL
     C+   VALUES NEXT VALUE FOR O2C_BKID_SEQ INTO :H#ID
     C/END-EXEC
     C/EXEC SQL
     C+   INSERT INTO BANKINP (BKID, IMPDATE, PAYDATE, AMOUNT, PAYERNM,
     C+          CUSTID, INVID, FEEAMT, MATCHTYP, STATUS, PAYID,
     C+          CRTUSER, CRTDATE, CRTTIM)
     C+   VALUES (:H#ID, :W#D8, :H#PDATE, :H#AMT, :H#PAYER, 0, 0, 0,
     C+          ' ', 'NEW', 0, :W#USER, :W#D8, :W#T6)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      LMSG = '取込エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#IMP = W#IMP + 1
     C                   EXSR      @MATCH
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE IMPCSR
     C/END-EXEC
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   IF        LMSG = *BLANKS
     C                   EVAL      LMSG = %CHAR(W#IMP) + ' 件取り込みました' +
     C                             '(重複 ' + %CHAR(W#DUP) + ' 件、' +
     C                             '読めない行 ' + %CHAR(W#BAD) + ' 件)'
     C                   ENDIF
     C                   ENDSR

      * @SPLIT - W#LINE をカンマで分ける(" で囲んだ中のカンマは無視)
     C     @SPLIT        BEGSR
     C                   EVAL      W#NF = 1
     C                   EVAL      W#FLD = %UCS2('')
     C                   EVAL      W#INQ = *OFF
     C                   FOR       W#J = 1 TO %LEN(W#LINE)
     C                   EVAL      W#UCC = %SUBST(W#LINE:W#J:1)
     C                   SELECT
     C                   WHEN      W#UCN = 34
     C                   EVAL      W#INQ = NOT W#INQ
     C                   WHEN      W#UCN = 44 AND NOT W#INQ
     C                   IF        W#NF >= 5
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#NF = W#NF + 1
     C                   WHEN      W#UCN = 13 OR W#UCN = 10 OR
     C                             W#UCN = 65279
     C                   OTHER
     C                   IF        %LEN(W#FLD(W#NF)) < 200
     C                   EVAL      W#FLD(W#NF) = W#FLD(W#NF) + W#UCC
     C                   ENDIF
     C                   ENDSL
     C                   ENDFOR
     C                   FOR       W#J = 1 TO W#NF
     C                   EVAL      W#FLD(W#J) = %TRIM(W#FLD(W#J))
     C                   ENDFOR
     C                   ENDSR

      * @TOFULL - 振込依頼人(W#FLD(3))の半角カナを全角にする → W#U
      *   濁点・半濁点は前の文字と合わせる(ｶﾞ→ガ ﾊﾟ→パ)
     C     @TOFULL       BEGSR
     C                   EVAL      W#K = W#FLD(3)
     C                   IF        %LEN(W#HFROM) = 0
     C                   EXSR      @KANATBL
     C                   ENDIF
     C                   EVAL      W#K = %XLATE(W#HFROM:W#HTO:W#K)
     C                   EVAL      W#U = %UCS2('')
     C                   FOR       W#J = 1 TO %LEN(W#K)
     C                   EVAL      W#UCC = %SUBST(W#K:W#J:1)
     C                   EVAL      W#CC = W#UCN
     C                   EVAL      W#PC = -1
     C                   IF        (W#CC = 12443 OR W#CC = 12444) AND
     C                             %LEN(W#U) > 0
     C                   EVAL      W#UCC = %SUBST(W#U:%LEN(W#U):1)
     C                   EVAL      W#PC = W#UCN
     C                   SELECT
      * ウ+゛→ヴ
     C                   WHEN      W#CC = 12443 AND W#PC = 12454
     C                   EVAL      W#UCN = 12532
      * カ～ト+゛
     C                   WHEN      W#CC = 12443 AND
     C                             ((W#PC >= 12459 AND W#PC <= 12481 AND
     C                             %REM(W#PC:2) = 1) OR W#PC = 12484 OR
     C                             W#PC = 12486 OR W#PC = 12488)
     C                   EVAL      W#UCN = W#PC + 1
      * ハ行+゛゜
     C                   WHEN      W#PC >= 12495 AND W#PC <= 12507 AND
     C                             %REM(W#PC - 12495:3) = 0
     C                   IF        W#CC = 12443
     C                   EVAL      W#UCN = W#PC + 1
     C                   ELSE
     C                   EVAL      W#UCN = W#PC + 2
     C                   ENDIF
     C                   OTHER
     C                   EVAL      W#PC = -1
     C                   ENDSL
     C                   ENDIF
     C                   IF        W#PC >= 0
     C                   EVAL      %SUBST(W#U:%LEN(W#U):1) = W#UCC
     C                   ELSE
     C                   EVAL      W#UCN = W#CC
     C                   EVAL      W#U = W#U + W#UCC
     C                   ENDIF
     C                   ENDFOR
     C                   IF        %LEN(W#U) > 100
     C                   EVAL      W#U = %SUBST(W#U:1:100)
     C                   ENDIF
     C                   ENDSR

      * @KANATBL - 半角カナ(U+FF61～FF9F)と全角の対応表
     C     @KANATBL      BEGSR
     C                   EVAL      W#HFROM = %UCS2('')
     C                   FOR       W#J = 65377 TO 65439
     C                   EVAL      W#UCN = W#J
     C                   EVAL      W#HFROM = W#HFROM + W#UCC
     C                   ENDFOR
     C                   EVAL      W#HTO = U'3002300C300D300130FB30F230A1' +
     C                             U'30A330A530A730A930E330E530E7' +
     C                             U'30C330FC30A230A430A630A830AA' +
     C                             U'30AB30AD30AF30B130B330B530B7' +
     C                             U'30B930BB30BD30BF30C130C430C6' +
     C                             U'30C830CA30CB30CC30CD30CE30CF' +
     C                             U'30D230D530D830DB30DE30DF30E0' +
     C                             U'30E130E230E430E630E830E930EA' +
     C                             U'30EB30EC30ED30EF30F3309B309C'
     C                   ENDSR

      *=====================================================================
      * @MATCH - 入金データ(H#ID)の顧客・請求書を推定する
      *=====================================================================
     C     @MATCH        BEGSR
     C/EXEC SQL
     C+   SELECT AMOUNT, PAYERNM, CUSTID INTO :H#AMT, :H#PAYER, :H#CUSTID
     C+     FROM BANKINP WHERE BKID = :H#ID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVESR
     C                   ENDIF
      * 顧客: カナ名が1件だけ一致したとき
     C                   IF        H#CUSTID = 0
     C                   EVAL      W#K = NORMKEY(%UCS2(%TRIMR(H#PAYER)))
     C                   EVAL      W#N = 0
     C                   FOR       W#J = 1 TO W#CUSTN
     C                   IF        %LEN(W#K) > 0 AND W#CKEY(W#J) = W#K
     C                   EVAL      W#N = W#N + 1
     C                   EVAL      H#CUSTID = W#CID(W#J)
     C                   ENDIF
     C                   ENDFOR
     C                   IF        W#N <> 1
     C                   EVAL      H#CUSTID = 0
     C                   ENDIF
     C                   ENDIF
     C                   IF        H#CUSTID = 0
     C/EXEC SQL
     C+   UPDATE BANKINP SET CUSTID = 0, INVID = 0, FEEAMT = 0,
     C+          MATCHTYP = ' ', STATUS = 'NEW'
     C+    WHERE BKID = :H#ID
     C/END-EXEC
     C                   LEAVESR
     C                   ENDIF
     C                   EXSR      @MATCHINV
     C/EXEC SQL
     C+   UPDATE BANKINP SET CUSTID = :H#CUSTID, INVID = :H#INVID,
     C+          FEEAMT = :H#FEE, MATCHTYP = :H#TYPE, STATUS = 'MATCHED'
     C+    WHERE BKID = :H#ID
     C/END-EXEC
     C                   ENDSR

      * @MATCHINV - 顧客(H#CUSTID)・金額(H#AMT)から消込のしかたを決める
      *   → H#INVID, H#FEE, H#TYPE
     C     @MATCHINV     BEGSR
     C                   EVAL      H#INVID = 0
     C                   EVAL      H#FEE = 0
      * 残高と一致する請求書
     C/EXEC SQL
     C+   SELECT INVID INTO :H#INVID FROM INVOICEHP
     C+    WHERE CUSTID = :H#CUSTID AND BALANCE > 0 AND CURRCD = 'JPY'
     C+      AND BALANCE = :H#AMT
     C+    ORDER BY INVDATE, INVID FETCH FIRST 1 ROW ONLY
     C/END-EXEC
     C                   IF        SQLCODE = 0
     C                   EVAL      H#TYPE = 'EXACT'
     C                   LEAVESR
     C                   ENDIF
      * 差額が手数料の範囲内の請求書
     C/EXEC SQL
     C+   SELECT INVID, BALANCE - :H#AMT INTO :H#INVID, :H#FEE
     C+     FROM INVOICEHP
     C+    WHERE CUSTID = :H#CUSTID AND BALANCE > 0 AND CURRCD = 'JPY'
     C+      AND BALANCE - :H#AMT > 0 AND BALANCE - :H#AMT <= :W#TOL
     C+    ORDER BY BALANCE - :H#AMT, INVDATE FETCH FIRST 1 ROW ONLY
     C/END-EXEC
     C                   IF        SQLCODE = 0
     C                   EVAL      H#TYPE = 'FEE'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      H#INVID = 0
     C                   EVAL      H#FEE = 0
     C                   EVAL      H#TOT = 0
     C/EXEC SQL
     C+   SELECT COALESCE(SUM(BALANCE), 0) INTO :H#TOT FROM INVOICEHP
     C+    WHERE CUSTID = :H#CUSTID AND BALANCE > 0 AND CURRCD = 'JPY'
     C/END-EXEC
     C                   SELECT
     C                   WHEN      H#TOT > 0 AND H#TOT = H#AMT
     C                   EVAL      H#TYPE = 'TOTAL'
     C                   WHEN      H#TOT > H#AMT AND H#TOT - H#AMT <= W#TOL
     C                   EVAL      H#TYPE = 'TOTAL'
     C                   EVAL      H#FEE = H#TOT - H#AMT
     C                   WHEN      H#AMT > H#TOT
     C                   EVAL      H#TYPE = 'ADVANCE'
     C                   OTHER
     C                   EVAL      H#TYPE = 'FIFO'
     C                   ENDSL
     C                   ENDSR

      * @REMATCH - 未処理の入金データをもう一度推定する(F5)
     C     @REMATCH      BEGSR
     C                   EXSR      @LOADKANA
     C                   EVAL      W#SELN = 0
     C/EXEC SQL
     C+   DECLARE RMCSR CURSOR FOR
     C+   SELECT BKID FROM BANKINP WHERE STATUS IN ('NEW', 'MATCHED')
     C+    ORDER BY BKID FOR READ ONLY
     C/END-EXEC
     C/EXEC SQL
     C+   OPEN RMCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#SELN < 999
     C/EXEC SQL
     C+   FETCH RMCSR INTO :H#ID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#SELN = W#SELN + 1
     C                   EVAL      W#SELID(W#SELN) = H#ID
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE RMCSR
     C/END-EXEC
     C                   FOR       W#I = 1 TO W#SELN
     C                   EVAL      H#ID = W#SELID(W#I)
      * 顧客を手で指定したものはそのまま、請求書だけ推定し直す
     C/EXEC SQL
     C+   UPDATE BANKINP SET CUSTID = 0
     C+    WHERE BKID = :H#ID AND STATUS = 'NEW'
     C/END-EXEC
     C                   EXSR      @MATCH
     C                   ENDFOR
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   EVAL      LMSG = %CHAR(W#SELN) + ' 件を推定しました'
     C                   ENDSR

      * @IGNORE - 対象外にする/戻す
     C     @IGNORE       BEGSR
     C/EXEC SQL
     C+   UPDATE BANKINP
     C+      SET STATUS = CASE WHEN STATUS = 'IGNORED' THEN 'NEW'
     C+                        ELSE 'IGNORED' END,
     C+          CUSTID = CASE WHEN STATUS = 'IGNORED' THEN CUSTID
     C+                        ELSE 0 END
     C+    WHERE BKID = :H#ID AND STATUS <> 'APPLIED'
     C/END-EXEC
     C                   IF        SQLCODE = 100
     C                   EVAL      LMSG = '登録済の入金データは変更できません'
     C                   ELSE
     C                   EXSR      @MATCHIF
     C                   EVAL      LMSG = '対象外を切り替えました'
     C                   ENDIF
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   ENDSR

      * @MATCHIF - 対象外から戻したものは推定し直す
     C     @MATCHIF      BEGSR
     C/EXEC SQL
     C+   SELECT STATUS INTO :H#STS FROM BANKINP WHERE BKID = :H#ID
     C/END-EXEC
     C                   IF        SQLCODE = 0 AND H#STS = 'NEW'
     C                   EXSR      @MATCH
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @ASSIGN - 顧客・請求書を指定する
      *=====================================================================
     C     @ASSIGN       BEGSR
     C/EXEC SQL
     C+   SELECT B.PAYDATE, B.AMOUNT, B.PAYERNM, B.CUSTID, B.STATUS,
     C+          COALESCE(C.CUSTCD, ' '), COALESCE(I.INVNUM, ' ')
     C+     INTO :H#PDATE, :H#AMT, :H#PAYER, :H#CUSTID, :H#STS,
     C+          :H#CUSTCD, :H#INVNUM
     C+     FROM BANKINP B
     C+     LEFT JOIN CUSTOMERP C ON C.CUSTID = B.CUSTID
     C+     LEFT JOIN INVOICEHP I ON I.INVID = B.INVID
     C+    WHERE B.BKID = :H#ID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVESR
     C                   ENDIF
     C                   IF        H#STS = 'APPLIED' OR H#STS = 'IGNORED'
     C                   EVAL      LMSG = '登録済・対象外の入金データは' +
     C                             '指定できません'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      APDATE = %CHAR(%DATE(H#PDATE:*ISO):*ISO)
     C                   EVAL(H)   W#P0 = H#AMT
     C                   EVALR     AAMT = %TRIM(%EDITC(W#P0:'J'))
     C                   EVAL      APAYER = H#PAYER
     C                   EVAL      ACUST = H#CUSTCD
     C                   EVAL      AINVNUM = H#INVNUM
     C                   EVAL      AKANA = ' '
     C                   EVAL      AMSG = *BLANKS
     C                   EXSR      @ACUSTNM
     C                   DOW       *ON
     C                   EXFMT     ASGN
     C                   EVAL      AMSG = *BLANKS
     C                   EVAL      *IN31 = *OFF
     C                   EVAL      *IN32 = *OFF
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN04
     C                   EVAL      W#PVAL = ACUST
     C                   CALLP     O2CPRMPT('CUST':' ':W#PVAL)
     C                   EVAL      ACUST = W#PVAL
     C                   EXSR      @ACUSTNM
     C                   ITER
     C                   ENDIF
     C                   EXSR      @ACUSTNM
     C                   IF        H#CUSTID <= 0
     C                   EVAL      *IN31 = *ON
     C                   EVAL      AMSG = '顧客が見つかりません(F4=検索)'
     C                   ITER
     C                   ENDIF
     C                   EVAL      H#INVID = 0
     C                   IF        AINVNUM <> *BLANKS
     C/EXEC SQL
     C+   SELECT INVID, BALANCE INTO :H#INVID, :H#BAL FROM INVOICEHP
     C+    WHERE INVNUM = :AINVNUM AND CUSTID = :H#CUSTID
     C+      AND BALANCE > 0 AND CURRCD = 'JPY'
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      *IN32 = *ON
     C                   EVAL      AMSG = 'この顧客の未収の請求書(円)' +
     C                             'ではありません'
     C                   ITER
     C                   ENDIF
     C                   ENDIF
      * 消込のしかた
     C                   IF        H#INVID > 0
     C                   EVAL      H#FEE = 0
     C                   SELECT
     C                   WHEN      H#AMT = H#BAL
     C                   EVAL      H#TYPE = 'EXACT'
     C                   WHEN      H#AMT < H#BAL AND H#BAL - H#AMT <= W#TOL
     C                   EVAL      H#TYPE = 'FEE'
     C                   EVAL      H#FEE = H#BAL - H#AMT
     C                   WHEN      H#AMT < H#BAL
     C                   EVAL      H#TYPE = 'FIFO'
     C                   OTHER
     C                   EVAL      H#TYPE = 'ADVANCE'
     C                   ENDSL
     C                   ELSE
     C                   EXSR      @MATCHINV
     C                   ENDIF
     C/EXEC SQL
     C+   UPDATE BANKINP SET CUSTID = :H#CUSTID, INVID = :H#INVID,
     C+          FEEAMT = :H#FEE, MATCHTYP = :H#TYPE, STATUS = 'MATCHED'
     C+    WHERE BKID = :H#ID
     C/END-EXEC
      * 振込依頼人を顧客のカナ名にする
     C                   IF        AKANA = 'Y'
     C                   EVAL      H#KANA = DBCSTRUNC(%TRIMR(H#PAYER):40)
     C                   IF        NOT %OPEN(CUSTOMERP)
     C                   OPEN      CUSTOMERP
     C                   ENDIF
     C     H#CUSTID      CHAIN     CUSTREC
     C                   IF        %FOUND(CUSTOMERP)
     C                   EVAL      C_CUSTKANA = H#KANA
     C                   UPDATE    CUSTREC
     C                   ENDIF
     C                   ENDIF
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   IF        AKANA = 'Y'
     C                   EXSR      @LOADKANA
     C                   ENDIF
     C                   EVAL      LMSG = '顧客を指定しました(1=登録で' +
     C                             '入金にします)'
     C                   LEAVE
     C                   ENDDO
     C                   ENDSR

      * @ACUSTNM - 指定した顧客の名前(H#CUSTID も決める)
     C     @ACUSTNM      BEGSR
     C                   EVAL      ACUSTNM = *BLANKS
     C                   EVAL      H#CUSTID = 0
     C                   IF        ACUST <> *BLANKS
     C/EXEC SQL
     C+   SELECT CUSTID, CUSTNM INTO :H#CUSTID, :H#NM
     C+     FROM CUSTOMERP WHERE CUSTCD = :ACUST
     C/END-EXEC
     C                   IF        SQLCODE = 0
     C                   EVAL      ACUSTNM = DBCSTRUNC(%TRIMR(H#NM):40)
     C                   ELSE
     C                   EVAL      H#CUSTID = 0
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @APPLYALL - 推定済の入金データをまとめて登録(F10)
      *=====================================================================
     C     @APPLYALL     BEGSR
     C                   EVAL      W#SELN = 0
     C/EXEC SQL
     C+   DECLARE ALLCSR CURSOR FOR
     C+   SELECT BKID FROM BANKINP WHERE STATUS = 'MATCHED'
     C+    ORDER BY BKID FOR READ ONLY
     C/END-EXEC
     C/EXEC SQL
     C+   OPEN ALLCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#SELN < 999
     C/EXEC SQL
     C+   FETCH ALLCSR INTO :H#ID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#SELN = W#SELN + 1
     C                   EVAL      W#SELID(W#SELN) = H#ID
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE ALLCSR
     C/END-EXEC
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   EVAL      W#N = 0
     C                   FOR       W#I = 1 TO W#SELN
     C                   EVAL      H#ID = W#SELID(W#I)
     C                   EXSR      @APPLY
     C                   IF        NOT W#ERR
     C                   EVAL      W#N = W#N + 1
     C                   ENDIF
     C                   ENDFOR
     C                   IF        W#N = W#SELN
     C                   EVAL      LMSG = %CHAR(W#N) + ' 件を入金にしました'
     C                   ELSE
     C                   EVAL      LMSG = %CHAR(W#N) + ' 件を入金にしました' +
     C                             '。エラー ' + %CHAR(W#SELN - W#N) +
     C                             ' 件: ' + %TRIM(W#MSG)
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @APPLY - 入金データ(H#ID)を入金にして消し込む
      *=====================================================================
     C     @APPLY        BEGSR
     C                   EVAL      W#ERR = *OFF
     C/EXEC SQL
     C+   SELECT PAYDATE, AMOUNT, CUSTID, INVID, FEEAMT, STATUS
     C+     INTO :H#PDATE, :H#AMT, :H#CUSTID, :H#INVID, :H#FEE, :H#STS
     C+     FROM BANKINP WHERE BKID = :H#ID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   IF        H#STS <> 'MATCHED' OR H#CUSTID = 0
     C                   EVAL      W#MSG = '推定済の入金データだけ登録' +
     C                             'できます(顧客不明は2で指定)'
     C                   EVAL      LMSG = W#MSG
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   EXSR      @STAMP
      * 入金(振込額)
     C                   EVAL      W#METH = 'BANK'
     C                   EVAL      W#REM = H#AMT
     C                   EXSR      @NEWPAY
     C                   IF        W#ERR
     C                   EXSR      @ROLLBACK
     C                   LEAVESR
     C                   ENDIF
     C                   EXSR      @APPLYPAY
     C                   IF        W#ERR
     C                   EXSR      @ROLLBACK
     C                   LEAVESR
     C                   ENDIF
     C                   EXSR      @PAYSTS
     C                   EVAL      H#APPID = W#PAYID
      * 振込手数料(差額)を別の入金として同じ請求書へ
     C                   IF        H#FEE > 0
     C                   EVAL      W#METH = 'BANKFEE'
     C                   EVAL      W#REM = H#FEE
     C                   EXSR      @NEWPAY
     C                   IF        NOT W#ERR
     C                   EXSR      @APPLYPAY
     C                   ENDIF
     C                   IF        W#ERR
     C                   EXSR      @ROLLBACK
     C                   LEAVESR
     C                   ENDIF
     C                   EXSR      @PAYSTS
     C                   ENDIF
     C/EXEC SQL
     C+   UPDATE BANKINP SET STATUS = 'APPLIED', PAYID = :H#APPID
     C+    WHERE BKID = :H#ID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#MSG = '入金データの更新エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EXSR      @ROLLBACK
     C                   LEAVESR
     C                   ENDIF
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   EVAL      LMSG = %TRIM(W#PAYNUM) + ' を登録しました'
     C                   ENDSR

      * @NEWPAY - 入金(PAYMENTP)を作る。W#METH, W#REM(金額)
     C     @NEWPAY       BEGSR
     C/EXEC SQL
     C+   VALUES NEXT VALUE FOR O2C_PAYID_SEQ INTO :W#PAYID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#MSG = '入金ID採番エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#PAYNUM = 'PAY-' + %CHAR(W#PAYID)
     C                   EVAL      H#REF = 'BANKIMP BK-' + %CHAR(H#ID)
     C/EXEC SQL
     C+   INSERT INTO PAYMENTP
     C+     (PAYID, PAYNUM, CUSTID, INVID, PAYDATE, PAYMETH,
     C+      CURRCD, AMOUNT, FXRATE, BASEAMT, REFNO, STATUS,
     C+      CRTUSER, CRTDATE, CRTTIM, UPDUSER, UPDDATE, UPDTIM)
     C+   VALUES
     C+     (:W#PAYID, :W#PAYNUM, :H#CUSTID, :H#INVID, :H#PDATE, :W#METH,
     C+      'JPY', :W#REM, 1, :W#REM, :H#REF, 'APPLIED',
     C+      :W#USER, :W#D8, :W#T6, :W#USER, :W#D8, :W#T6)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#MSG = '入金の登録エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EVAL      W#ERR = *ON
     C                   ENDIF
     C                   ENDSR

      * @APPLYPAY - 入金 W#PAYID の W#REM を消し込む
      *   H#INVID > 0 はその請求書だけ、0 は古い請求書から
     C     @APPLYPAY     BEGSR
     C                   DOW       W#REM > 0
     C                   IF        H#INVID > 0
     C/EXEC SQL
     C+   SELECT INVID, BALANCE, PAIDAMT INTO :H#APPID, :H#BAL, :H#PAID
     C+     FROM INVOICEHP
     C+    WHERE INVID = :H#INVID AND BALANCE > 0
     C/END-EXEC
     C                   ELSE
     C/EXEC SQL
     C+   SELECT INVID, BALANCE, PAIDAMT INTO :H#APPID, :H#BAL, :H#PAID
     C+     FROM INVOICEHP
     C+    WHERE CUSTID = :H#CUSTID AND BALANCE > 0 AND CURRCD = 'JPY'
     C+    ORDER BY INVDATE, INVID FETCH FIRST 1 ROW ONLY
     C/END-EXEC
     C                   ENDIF
     C                   IF        SQLCODE = 100
      * 消し込む請求書がない(残りは前受金)
     C                   LEAVE
     C                   ENDIF
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#MSG = '請求書の読込エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   EXSR      @APPLYONE
     C                   IF        W#ERR
     C                   LEAVESR
     C                   ENDIF
     C                   IF        H#INVID > 0
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR

      * @APPLYONE - 請求書(H#APPID, 残高 H#BAL)に W#REM を消し込む
     C     @APPLYONE     BEGSR
     C                   IF        W#REM >= H#BAL
     C                   EVAL      W#APPLY = H#BAL
     C                   EVAL      H#ISTS = 'PAID'
     C                   ELSE
     C                   EVAL      W#APPLY = W#REM
     C                   EVAL      H#ISTS = 'PARTIAL'
     C                   ENDIF
     C                   EVAL      H#BAL = H#BAL - W#APPLY
     C                   EVAL      H#PAID = H#PAID + W#APPLY
     C                   EVAL      W#REM = W#REM - W#APPLY
     C/EXEC SQL
     C+   UPDATE INVOICEHP
     C+      SET PAIDAMT = :H#PAID, BALANCE = :H#BAL, STATUS = :H#ISTS,
     C+          UPDUSER = :W#USER, UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHERE INVID = :H#APPID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#MSG = '請求書の消込エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#T6 = %DEC(%TIME():*HMS)
     C/EXEC SQL
     C+   VALUES NEXT VALUE FOR O2C_APPLYID_SEQ INTO :H#AID
     C/END-EXEC
     C/EXEC SQL
     C+   INSERT INTO PAYAPPLYP (APPLYID, PAYID, INVID, APPLYAMT,
     C+          APPLYDATE, CRTUSER, CRTDATE, CRTTIM)
     C+   VALUES (:H#AID, :W#PAYID, :H#APPID, :W#APPLY, :W#D8,
     C+          :W#USER, :W#D8, :W#T6)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#MSG = '消込明細の登録エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EVAL      W#ERR = *ON
     C                   ENDIF
     C                   ENDSR

      * @PAYSTS - 消し込めない残りがあれば前受金(UNAPPLIED)にする
     C     @PAYSTS       BEGSR
     C                   IF        W#REM > 0
     C/EXEC SQL
     C+   UPDATE PAYMENTP SET STATUS = 'UNAPPLIED' WHERE PAYID = :W#PAYID
     C/END-EXEC
     C                   ENDIF
     C                   ENDSR

      * @ROLLBACK - 登録の取消
     C     @ROLLBACK     BEGSR
     C                   EVAL      W#ERR = *ON
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   EVAL      LMSG = W#MSG
     C                   ENDSR

      *=====================================================================
      * @ADVAPPLY - 前受金(UNAPPLIED)を古い請求書から充当する(F8)
      *=====================================================================
     C     @ADVAPPLY     BEGSR
     C                   EVAL      W#ADVN = 0
     C/EXEC SQL
     C+   OPEN ADVCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#ADVN < 500
     C/EXEC SQL
     C+   FETCH ADVCSR INTO :H#APPID, :H#CUSTID, :H#CUR, :H#TOT
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#ADVN = W#ADVN + 1
     C                   EVAL      W#AID(W#ADVN) = H#APPID
     C                   EVAL      W#ACUST(W#ADVN) = H#CUSTID
     C                   EVAL      W#ACUR(W#ADVN) = H#CUR
     C                   EVAL      W#AREM(W#ADVN) = H#TOT
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE ADVCSR
     C/END-EXEC
     C                   EVAL      W#N = 0
     C                   EVAL      W#ADVAMT = 0
     C                   EXSR      @STAMP
     C                   FOR       W#I = 1 TO W#ADVN
     C                   EVAL      W#PAYID = W#AID(W#I)
     C                   EVAL      H#CUSTID = W#ACUST(W#I)
     C                   EVAL      W#REM = W#AREM(W#I)
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      H#CUR = W#ACUR(W#I)
     C                   DOW       W#REM > 0
     C/EXEC SQL
     C+   SELECT INVID, BALANCE, PAIDAMT INTO :H#APPID, :H#BAL, :H#PAID
     C+     FROM INVOICEHP
     C+    WHERE CUSTID = :H#CUSTID AND BALANCE > 0
     C+      AND CURRCD = :H#CUR
     C+    ORDER BY INVDATE, INVID FETCH FIRST 1 ROW ONLY
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EXSR      @APPLYONE
     C                   IF        W#ERR
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   IF        W#ERR
     C                   EXSR      @ROLLBACK
     C                   LEAVESR
     C                   ENDIF
     C                   IF        W#REM < W#AREM(W#I)
     C                   EVAL      W#N = W#N + 1
     C                   EVAL      W#ADVAMT = W#ADVAMT + W#AREM(W#I) - W#REM
     C                   ENDIF
     C                   IF        W#REM = 0
     C/EXEC SQL
     C+   UPDATE PAYMENTP SET STATUS = 'APPLIED', UPDUSER = :W#USER,
     C+          UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHERE PAYID = :W#PAYID
     C/END-EXEC
     C                   ENDIF
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   ENDFOR
     C                   EVAL(H)   W#P0 = W#ADVAMT
     C                   EVAL      LMSG = %CHAR(W#N) + ' 件の前受金を充当し' +
     C                             'ました(' + %TRIM(%EDITC(W#P0:'J')) + ')'
     C                   ENDSR

      * @STAMP - 更新日時
     C     @STAMP        BEGSR
     C                   EVAL      W#D8 = %DEC(%DATE():*ISO)
     C                   EVAL      W#T6 = %DEC(%TIME():*HMS)
     C                   ENDSR

      *=====================================================================
      * NORMKEY - 振込依頼人・カナ名を比べるための形にする
      *   空白・記号を除き、英字は大文字、全角英数は半角、小書きは大書き
      *   カ)・(カ などの法人の略称と「カブシキガイシヤ」等を除く
      *=====================================================================
     P NORMKEY         B
     D NORMKEY         PI           200C   VARYING
     D   PSTR                       200C   VARYING CONST
     D WOUT            S            200C   VARYING
     D WI              S             10I 0
     D WX              S             10I 0
     D WC              DS
     D  WCC                           1C
     D  WCN                           5U 0 OVERLAY(WC)
     D WMARK           S              1C   DIM(4)
     D WPAT            S             10C   VARYING
     D WLP             C                   U'0028'
     D WRP             C                   U'0029'
     C                   EVAL      WOUT = %UCS2('')
     C                   FOR       WI = 1 TO %LEN(PSTR)
     C                   EVAL      WCC = %SUBST(PSTR:WI:1)
      * 全角英数記号 → 半角
     C                   IF        WCN >= 65281 AND WCN <= 65374
     C                   EVAL      WCN = WCN - 65248
     C                   ENDIF
      * 英小文字 → 大文字
     C                   IF        WCN >= 97 AND WCN <= 122
     C                   EVAL      WCN = WCN - 32
     C                   ENDIF
      * 空白・全角空白・中点・ピリオド・カンマ・ハイフンは除く
     C                   IF        WCN = 32 OR WCN = 12288 OR
     C                             WCN = 12539 OR WCN = 46 OR WCN = 44 OR
     C                             WCN = 45 OR WCN = 12290 OR WCN = 12289
     C                   ITER
     C                   ENDIF
      * 小書き → 大書き(ァィゥェォッャュョヮ)
     C                   IF        WCN = 12449 OR WCN = 12451 OR
     C                             WCN = 12453 OR WCN = 12455 OR
     C                             WCN = 12457 OR WCN = 12483 OR
     C                             WCN = 12515 OR WCN = 12517 OR
     C                             WCN = 12519 OR WCN = 12526
     C                   EVAL      WCN = WCN + 1
     C                   ENDIF
     C                   EVAL      WOUT = WOUT + WCC
     C                   ENDFOR
      * 法人の略称の括弧つき: カ(株) ユ(有) ド(合同) シ(社団)
     C                   EVAL      WMARK(1) = U'30AB'
     C                   EVAL      WMARK(2) = U'30E6'
     C                   EVAL      WMARK(3) = U'30C9'
     C                   EVAL      WMARK(4) = U'30B7'
     C                   FOR       WX = 1 TO 4
     C                   EVAL      WPAT = WLP + WMARK(WX) + WRP
     C                   EVAL      WOUT = %SCANRPL(WPAT:%UCS2(''):WOUT)
     C                   EVAL      WPAT = WMARK(WX) + WRP
     C                   EVAL      WOUT = %SCANRPL(WPAT:%UCS2(''):WOUT)
     C                   EVAL      WPAT = WLP + WMARK(WX)
     C                   EVAL      WOUT = %SCANRPL(WPAT:%UCS2(''):WOUT)
     C                   ENDFOR
     C                   EVAL      WOUT = %SCANRPL(WLP:%UCS2(''):WOUT)
     C                   EVAL      WOUT = %SCANRPL(WRP:%UCS2(''):WOUT)
      * カブシキガイシヤ・カブシキカイシヤ・ユウゲンガイシヤ
     C                   EVAL      WOUT = %SCANRPL(
     C                             U'30AB30D630B730AD30AC30A430B730E4':
     C                             %UCS2(''):WOUT)
     C                   EVAL      WOUT = %SCANRPL(
     C                             U'30AB30D630B730AD30AB30A430B730E4':
     C                             %UCS2(''):WOUT)
     C                   EVAL      WOUT = %SCANRPL(
     C                             U'30E630A630B230F330AC30A430B730E4':
     C                             %UCS2(''):WOUT)
     C                   RETURN    WOUT
     P NORMKEY         E

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
