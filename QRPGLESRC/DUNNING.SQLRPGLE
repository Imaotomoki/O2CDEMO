     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: DUNNING
      * 説明: 督促状の発行
      * 作成: 修正#DUN 新規作成
      *   基準日の時点で支払期限を指定日数以上過ぎた未収がある顧客を一覧
      *   1=督促状発行(スプール DUNPRTF)。督促履歴 DUNNINGP に記録し、
      *   回数に応じて文面を変える(1回目/再通知/最終通知)
      *   同じ基準日に同じ顧客へ再度発行した場合は再発行として扱い、
      *   履歴は追加しない(回数は増えない)
      *   修正#CUR 一覧と履歴の金額は円換算、督促状は請求書の通貨
      *=====================================================================
     FDUNNINGD  CF   E             WORKSTN SFILE(DUNSFL:W#RRN)
     FDUNPRTF   O    E             PRINTER OFLIND(*IN90) USROPN

     D W#RRN           S              4S 0
     D W#MSG           S             75A
     D W#EXIT          S               N
     D W#BASEISO       S             10A
     D W#BASE8         S              8P 0
     D W#NOFX          S             10I 0
      * 修正#CUR: 督促状の通貨ごとの合計
     D W#TCUR          S              3A   DIM(10)
     D W#TAMT          S             15P 2 DIM(10)
     D W#TN            S             10I 0
     D W#K             S             10I 0
     D W#MIN           S             10I 0
     D W#CUSTID        S              9P 0
     D W#LEVEL         S              3P 0
     D W#REISSUE       S               N
     D W#NEW           S             10I 0
     D W#RE            S             10I 0
     D W#TOT           S             15P 2
     D W#CNT           S              5P 0
     D W#MAXD          S              5P 0
     D W#CNM           S             60A
     D W#CADR          S             60A
     D W#USER          S             10A
     D W#TODAY         S              8P 0
     D W#NOW           S              6P 0
      * 期限超過の顧客別集計
     D S#ROW           DS
     D S#CUSTID                       9P 0
     D S#CUSTCD                      20A
     D S#CUSTNM                      60A
     D S#AMT                         15P 2
     D S#CNT                         10I 0
     D S#DAYS                        10I 0
     D S#LAST                         8P 0
     D S#TIMES                       10I 0
      * 督促状に載せる請求書
     D V#ROW           DS
     D V#INVNUM                      20A
     D V#INVDATE                      8P 0
     D V#DUEDATE                      8P 0
     D V#BAL                         15P 2
     D V#DAYS                        10I 0
     D V#CUR                          3A
     D V#FX                          15P 6
     D PSDS           SDS
     D   P#USER              254    263A
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
     D DbcsTrunc       PR           256A   VARYING
     D   pStr                       256A   VARYING CONST
     D   pMax                        10I 0 CONST
     D Fit9            PR             9P 0
     D   pAmt                        15P 2 CONST
     D FmtDate         PR            10A
     D   pDate                        8P 0 CONST
      * 全角の文字列を連結したときの SI+SO を取り除く
     D Join            PR           256A   VARYING
     D   pStr                       256A   VARYING CONST

     C/EXEC SQL
     C+   SET OPTION COMMIT = *NONE, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE SUMCSR CURSOR FOR
     C+   WITH A AS (
      * 修正#CUR: 一覧の金額は基準日の為替レートで円に換算
     C+     SELECT H.CUSTID, H.BALANCE * (
     C+       CASE WHEN H.CURRCD IN ('JPY', ' ') THEN 1 ELSE COALESCE(
     C+        (SELECT F.RATE FROM FXRATEP F
     C+          WHERE F.FROMCUR = H.CURRCD AND F.TOCUR = 'JPY'
     C+            AND F.RATEDATE <= :W#BASE8
     C+          ORDER BY F.RATEDATE DESC FETCH FIRST 1 ROW ONLY),
     C+        (SELECT F.RATE FROM FXRATEP F
     C+          WHERE F.FROMCUR = H.CURRCD AND F.TOCUR = 'JPY'
     C+          ORDER BY F.RATEDATE FETCH FIRST 1 ROW ONLY), 0) END
     C+       ) AS BAL,
     C+            DAYS(DATE(:W#BASEISO)) - DAYS(DATE(TO_DATE(DIGITS(
     C+            CASE WHEN H.DUEDATE > 0 THEN H.DUEDATE
     C+                 ELSE H.INVDATE END), 'YYYYMMDD'))) AS D
     C+       FROM INVOICEHP H
     C+      WHERE H.BALANCE > 0)
     C+   SELECT C.CUSTID, C.CUSTCD, C.CUSTNM, SUM(A.BAL), COUNT(*),
     C+          MAX(A.D),
     C+          COALESCE((SELECT MAX(U.DUNDATE) FROM DUNNINGP U
     C+                     WHERE U.CUSTID = C.CUSTID), 0),
     C+          (SELECT COUNT(*) FROM DUNNINGP U
     C+            WHERE U.CUSTID = C.CUSTID)
     C+     FROM A JOIN CUSTOMERP C ON C.CUSTID = A.CUSTID
     C+    WHERE A.D >= :W#MIN
     C+    GROUP BY C.CUSTID, C.CUSTCD, C.CUSTNM
     C+    ORDER BY 6 DESC, 2
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE DTLCSR CURSOR FOR
      * 修正#CUR: 督促状は請求書の通貨で出す(円換算レートは履歴用)
     C+   SELECT INVNUM, INVDATE, DUEDATE, BALANCE, D, CURRCD, FX FROM (
     C+     SELECT INVNUM, INVDATE, DUEDATE, BALANCE, H.CURRCD, (
     C+       CASE WHEN H.CURRCD IN ('JPY', ' ') THEN 1 ELSE COALESCE(
     C+        (SELECT F.RATE FROM FXRATEP F
     C+          WHERE F.FROMCUR = H.CURRCD AND F.TOCUR = 'JPY'
     C+            AND F.RATEDATE <= :W#BASE8
     C+          ORDER BY F.RATEDATE DESC FETCH FIRST 1 ROW ONLY),
     C+        (SELECT F.RATE FROM FXRATEP F
     C+          WHERE F.FROMCUR = H.CURRCD AND F.TOCUR = 'JPY'
     C+          ORDER BY F.RATEDATE FETCH FIRST 1 ROW ONLY), 0) END
     C+       ) AS FX,
     C+            DAYS(DATE(:W#BASEISO)) - DAYS(DATE(TO_DATE(DIGITS(
     C+            CASE WHEN DUEDATE > 0 THEN DUEDATE
     C+                 ELSE INVDATE END), 'YYYYMMDD'))) AS D
     C+       FROM INVOICEHP H
     C+      WHERE CUSTID = :W#CUSTID AND BALANCE > 0) X
     C+    WHERE D >= :W#MIN
     C+    ORDER BY CURRCD, DUEDATE, INVNUM
     C/END-EXEC

     C                   EVAL      BASEDT = %DEC(%DATE():*ISO)
     C                   EVAL      MINDAYS = 1
     C                   EVAL      W#MSG = *BLANKS
     C                   EXSR      @SETCOND
     C                   EXSR      @LOAD
     C                   DOW       NOT W#EXIT
     C                   EVAL      DMSG = W#MSG
     C                   EVAL      W#MSG = *BLANKS
     C                   EVAL      *IN92 = *ON
     C                   EVAL      *IN91 = (W#RRN > 0)
     C                   EVAL      *IN94 = *ON
     C                   EXFMT     DUNCTL
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      *IN31 = *OFF
     C                   EVAL      *IN32 = *OFF
     C     *ISO          TEST(DE)                BASEDT
     C                   IF        %ERROR
     C                   EVAL      *IN31 = *ON
     C                   EVAL      W#MSG = '基準日が無効です(YYYYMMDD)'
     C                   ITER
     C                   ENDIF
     C                   IF        MINDAYS < 1
     C                   EVAL      *IN32 = *ON
     C                   EVAL      W#MSG = '日数は1以上を入力してください'
     C                   ITER
     C                   ENDIF
     C                   EXSR      @SETCOND
      * 1=督促状発行 (選択した顧客をまとめて1つのスプールに出す)
     C                   EVAL      W#NEW = 0
     C                   EVAL      W#RE = 0
     C                   IF        W#RRN > 0
     C                   READC     DUNSFL
     C                   DOW       NOT %EOF(DUNNINGD)
     C                   IF        SOPT = '1'
     C                   EXSR      @ISSUE
     C                   ENDIF
     C                   READC     DUNSFL
     C                   ENDDO
     C                   ENDIF
     C                   IF        %OPEN(DUNPRTF)
     C                   CLOSE     DUNPRTF
     C                   EVAL      W#MSG = Join('督促状を発行しました' +
     C                             '(新規 ' + %CHAR(W#NEW) + '社' +
     C                             '  再発行 ' + %CHAR(W#RE) + '社' +
     C                             '  スプール DUNPRTF)')
     C                   ENDIF
     C                   EXSR      @LOAD
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C                   RETURN

      *=====================================================================
      * @SETCOND - 画面の条件をSQL用に整える
      *=====================================================================
     C     @SETCOND      BEGSR
     C                   EVAL      W#BASEISO = %CHAR(%DATE(BASEDT:*ISO):*ISO)
     C                   EVAL      W#BASE8 = BASEDT
     C                   EVAL      W#MIN = MINDAYS
     C                   ENDSR

      *=====================================================================
      * @LOAD - 期限超過の顧客をサブファイルに読み込む
      *=====================================================================
     C     @LOAD         BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     DUNCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#RRN = 0
     C/EXEC SQL
     C+   OPEN SUMCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#RRN < 999
     C/EXEC SQL
     C+   FETCH SUMCSR INTO :S#ROW
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#RRN = W#RRN + 1
     C                   EVAL      SOPT = ' '
     C                   EVAL      SCUSTID = S#CUSTID
     C                   EVAL      SCUSTCD = S#CUSTCD
     C                   EVAL      SCUSTNM = DbcsTrunc(S#CUSTNM:18)
     C                   EVAL      SAMT = Fit9(S#AMT)
     C                   EVAL      SCNT = %MIN(S#CNT:999)
     C                   EVAL      SDAYS = %MIN(S#DAYS:99999)
     C                   EVAL      SLASTDT = S#LAST
     C                   EVAL      SLAST = FmtDate(S#LAST)
     C                   EVAL      STIMESH = %MIN(S#TIMES:999)
     C                   EVAL      STIMES = STIMESH
     C                   WRITE     DUNSFL
     C                   ENDDO
     C                   IF        SQLCODE < 0
     C                   EVAL      W#MSG = '集計に失敗しました SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   ENDIF
     C/EXEC SQL
     C+   CLOSE SUMCSR
     C/END-EXEC
     C                   IF        W#RRN = 0 AND W#MSG = *BLANKS
     C                   EVAL      W#MSG = '督促の対象となる顧客はありません'
     C                   ENDIF
      * 修正#CUR: 為替レートがない外貨の請求書は0円で集計される
     C/EXEC SQL
     C+   SELECT COUNT(*) INTO :W#NOFX FROM INVOICEHP H
     C+    WHERE H.BALANCE > 0 AND H.CURRCD NOT IN ('JPY', ' ')
     C+      AND NOT EXISTS (SELECT 1 FROM FXRATEP F
     C+           WHERE F.FROMCUR = H.CURRCD AND F.TOCUR = 'JPY')
     C/END-EXEC
     C                   IF        W#NOFX > 0 AND W#MSG = *BLANKS
     C                   EVAL      W#MSG = %CHAR(W#NOFX) + '件の外貨' +
     C                             'の請求書はレートがなく0円で集計'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @ISSUE - 1社分の督促状を印刷し、履歴に記録する
      *=====================================================================
     C     @ISSUE        BEGSR
     C                   EVAL      W#CUSTID = SCUSTID
      * 同じ基準日に発行済みなら再発行(回数はそのまま)
     C                   IF        STIMESH > 0 AND SLASTDT = BASEDT
     C                   EVAL      W#REISSUE = *ON
     C                   EVAL      W#LEVEL = STIMESH
     C                   ELSE
     C                   EVAL      W#REISSUE = *OFF
     C                   EVAL      W#LEVEL = STIMESH + 1
     C                   ENDIF
     C/EXEC SQL
     C+   SELECT CUSTNM, BILLADR INTO :W#CNM, :W#CADR
     C+     FROM CUSTOMERP WHERE CUSTID = :W#CUSTID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#CNM = SCUSTNM
     C                   EVAL      W#CADR = *BLANKS
     C                   ENDIF
     C                   IF        NOT %OPEN(DUNPRTF)
     C                   OPEN      DUNPRTF
     C                   ENDIF
     C                   EVAL      PISSUE = FmtDate(BASEDT)
     C                   EVAL      PCUSTNM = W#CNM
     C                   EVAL      PBILLADR = W#CADR
     C                   SELECT
     C                   WHEN      W#LEVEL <= 1
     C                   EVAL      PTITLE = 'お支払いのお願い'
     C                   EVAL      PMSG3 = Join('が、本日現在ご入金の確認' +
     C                             'ができておりません。')
     C                   WHEN      W#LEVEL = 2
     C                   EVAL      PTITLE = Join('お支払いのお願い（再通知' +
     C                             '）')
     C                   EVAL      PMSG3 = Join('が、先日のご案内後もご入' +
     C                             '金の確認ができておりません。')
     C                   OTHER
     C                   EVAL      PTITLE = Join('お支払いのお願い（最終通' +
     C                             '知）')
     C                   EVAL      PMSG3 = Join('が、再三のご案内にもかか' +
     C                             'わらずご入金の確認ができておりま' +
     C                             'せん。')
     C                   ENDSL
     C                   EVAL      PMSG1 = Join('いつも格別のお引き立てを' +
     C                             '賜り、厚く御礼申し上げます。')
     C                   EVAL      PMSG2 = Join('さて、下記のご請求につき' +
     C                             'まして、お支払期限を過ぎておりま' +
     C                             'す')
     C                   EVAL      PMSG4 = Join('お手数ですが、至急お支払' +
     C                             'いくださいますようお願い申し上げ' +
     C                             'ます。')
     C                   WRITE     PHEAD
     C                   EVAL      *IN90 = *OFF
     C                   EVAL      W#TOT = 0
     C                   EVAL      W#CNT = 0
     C                   EVAL      W#MAXD = 0
     C                   EVAL      W#TN = 0
     C/EXEC SQL
     C+   OPEN DTLCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0
     C/EXEC SQL
     C+   FETCH DTLCSR INTO :V#ROW
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN90
     C                   WRITE     PHEAD
     C                   EVAL      *IN90 = *OFF
     C                   ENDIF
     C                   EVAL      PINVNUM = V#INVNUM
     C                   EVAL      PINVDT = FmtDate(V#INVDATE)
     C                   EVAL      PDUEDT = FmtDate(V#DUEDATE)
     C                   EVAL      PDAYS = %MIN(V#DAYS:99999)
      * 50=外貨(小数2桁と通貨コード)
     C                   EVAL      *IN50 = (V#CUR <> 'JPY' AND V#CUR <> *BLANKS)
     C                   EVAL(H)   PBAL = V#BAL
     C                   EVALR     PBALF = %EDITC(%DEC(V#BAL:11:2):'1')
     C                   EVAL      PCUR = V#CUR
     C                   WRITE     PDETAIL
      * 通貨ごとの合計(督促状用)と円換算の合計(履歴用)
     C                   EVAL      W#K = 0
     C                   IF        W#TN > 0
     C                   EVAL      W#K = %LOOKUP(V#CUR:W#TCUR:1:W#TN)
     C                   ENDIF
     C                   IF        W#K = 0 AND W#TN < 10
     C                   EVAL      W#TN = W#TN + 1
     C                   EVAL      W#K = W#TN
     C                   EVAL      W#TCUR(W#K) = V#CUR
     C                   EVAL      W#TAMT(W#K) = 0
     C                   ENDIF
     C                   IF        W#K > 0
     C                   EVAL      W#TAMT(W#K) = W#TAMT(W#K) + V#BAL
     C                   ENDIF
     C                   EVAL(H)   W#TOT = W#TOT + V#BAL * V#FX
     C                   EVAL      W#CNT = W#CNT + 1
     C                   IF        PDAYS > W#MAXD
     C                   EVAL      W#MAXD = PDAYS
     C                   ENDIF
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE DTLCSR
     C/END-EXEC
     C                   FOR       W#K = 1 TO W#TN
     C                   EVAL      *IN50 = (W#TCUR(W#K) <> 'JPY' AND
     C                             W#TCUR(W#K) <> *BLANKS)
     C                   EVAL(H)   PTOTBAL = W#TAMT(W#K)
     C                   EVALR     PTOTF = %EDITC(%DEC(W#TAMT(W#K):11:2):'1')
     C                   EVAL      PTCUR = W#TCUR(W#K)
     C                   WRITE     PTOTAL
     C                   ENDFOR
     C                   EVAL      *IN50 = *OFF
     C                   EVAL      PMSG5 = Join('なお、本状と行き違いにお' +
     C                             '支払い済みの場合は、ご容赦くださ' +
     C                             'い。')
     C                   EVAL      PMSG6 = Join('ご不明な点がございました' +
     C                             'ら、担当までご連絡ください。')
     C                   WRITE     PEND
      * 履歴に記録(再発行は記録しない)
     C                   IF        W#REISSUE
     C                   EVAL      W#RE = W#RE + 1
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#USER = P#USER
     C                   EVAL      W#TODAY = %DEC(%DATE():*ISO)
     C                   EVAL      W#NOW = %DEC(%TIME():*HMS)
     C/EXEC SQL
     C+   INSERT INTO DUNNINGP (DUNID, CUSTID, DUNDATE, DUNLEVEL, INVCNT,
     C+          DUNAMT, MAXDAYS, CRTUSER, CRTDATE, CRTTIM)
     C+   VALUES (NEXT VALUE FOR O2C_DUNID_SEQ, :W#CUSTID, :BASEDT,
     C+          :W#LEVEL, :W#CNT, :W#TOT, :W#MAXD, :W#USER, :W#TODAY,
     C+          :W#NOW)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#MSG = Join('督促履歴の記録に' +
     C                             '失敗しました') + ' SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   ELSE
     C                   EVAL      W#NEW = W#NEW + 1
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * Join - 連結した全角文字列の SI+SO を取り除く
      *=====================================================================
     P Join            B
     D Join            PI           256A   VARYING
     D   pStr                       256A   VARYING CONST
     C                   RETURN    %SCANRPL(W#SI + W#SO:'':pStr)
     P Join            E

      *=====================================================================
      * Fit9 - 9桁(999,999,999)に収めて四捨五入
      *=====================================================================
     P Fit9            B
     D Fit9            PI             9P 0
     D   pAmt                        15P 2 CONST
     D wAmt            S             15P 0
     C                   EVAL(H)   wAmt = pAmt
     C                   IF        wAmt > 999999999
     C                   RETURN    999999999
     C                   ENDIF
     C                   IF        wAmt < -99999999
     C                   RETURN    -99999999
     C                   ENDIF
     C                   RETURN    wAmt
     P Fit9            E

      *=====================================================================
      * FmtDate - YYYYMMDD を YYYY/MM/DD に
      *=====================================================================
     P FmtDate         B
     D FmtDate         PI            10A
     D   pDate                        8P 0 CONST
     D wD8             S              8A
     C                   IF        pDate = 0
     C                   RETURN    *BLANKS
     C                   ENDIF
     C                   EVAL      wD8 = %EDITC(pDate:'X')
     C                   RETURN    %SUBST(wD8:1:4) + '/' + %SUBST(wD8:5:2) +
     C                             '/' + %SUBST(wD8:7:2)
     P FmtDate         E

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
