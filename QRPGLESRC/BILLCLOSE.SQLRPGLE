     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: BILLCLOSE
      * 説明: 請求締め(合計請求書の発行)
      * 作成: 修正#CLS 新規作成
      *   締め日を入力すると、その日が締日の顧客を「対象」として一覧する
      *   (締日は顧客締日 CUSTCLSP。未設定・0は末日締め)
      *   1=締め実行  前回の締めから締め日までの請求書と入金を集計し、
      *               請求締め履歴 BILLSTMP に記録して合計請求書を印刷
      *               締め済みの顧客は記録済みの内容で再発行する
      *   今回御請求額 = 前回御請求額 - 御入金額
      *                + 今回お買上額 + 消費税
      *   締日の列を書き換えてEnterで顧客の締日を保存する
      *   修正#CUR 合計請求書は顧客の通貨。その通貨の請求書と入金を集計
      *=====================================================================
     FBILLCLSD  CF   E             WORKSTN SFILE(CLSSFL:W#RRN)
     FCLSPRTF   O    E             PRINTER OFLIND(*IN90) USROPN

     D W#RRN           S              4S 0
     D W#CNT           S              4S 0
     D W#I             S              4S 0
     D W#PCNT          S             10I 0
     D W#ERR           S               N
     D W#CLS           S              8P 0
     D W#LOADED        S              8P 0
     D W#TODAY         S              8P 0
     D W#NOW           S              6P 0
     D W#USER          S             10A
     D W#MSG           S             75A
     D W#DT            S               D
     D W#EOM           S               D
     D W#DAY           S              2P 0
     D W#ISEOM         S               N
     D W#MATCH         S               N
     D W#NEW           S             10I 0
     D W#RE            S             10I 0
     D W#NG            S             10I 0
     D W#DAYCHG        S             10I 0
     D W#LATER         S             10I 0
      * 1顧客分の締めの内容
     D W#CUSTID        S              9P 0
      * 修正#CUR: 合計請求書の通貨(顧客の通貨)
     D W#CUR           S              3A
     D W#CLOSEDAY      S              2P 0
     D W#LASTCLS       S              8P 0
     D W#DONE          S             10I 0
     D W#PREV          S             15P 2
     D W#PAY           S             15P 2
     D W#CARRY         S             15P 2
     D W#SALES         S             15P 2
     D W#TAX           S             15P 2
     D W#TOTAL         S             15P 2
     D W#ICNT          S              5P 0
     D W#CNM           S             60A
     D W#CADR          S             60A
     D W#CDESC         S            120A
      * 顧客一覧の取り出し先
     D C#ROW           DS
     D C#CUSTID                       9P 0
     D C#CUSTCD                      20A
     D C#CUSTNM                      60A
     D C#CUR                          3A
     D C#DAY                          2P 0
     D C#LAST                         8P 0
     D C#DONE                        10I 0
      * 合計請求書の明細(期間内の請求書)
     D V#ROW           DS
     D V#INVNUM                      20A
     D V#INVDATE                      8P 0
     D V#SUB                         15P 2
     D V#TAX                         15P 2
     D V#TOT                         15P 2
     D PSDS           SDS
     D   P#USER              254    263A
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
     D Join            PR           256A   VARYING
     D   pStr                       256A   VARYING CONST
     D DbcsTrunc       PR           256A   VARYING
     D   pStr                       256A   VARYING CONST
     D   pMax                        10I 0 CONST
     D Fit9            PR             9P 0
     D   pAmt                        15P 2 CONST
      * 画面の金額欄(8桁)に収める
     D Fit8            PR             8P 0
     D   pAmt                        15P 2 CONST
     D FmtDate         PR            10A
     D   pDate                        8P 0 CONST

     C/EXEC SQL
     C+   SET OPTION COMMIT = *CHG, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE CUSCSR CURSOR FOR
     C+   SELECT C.CUSTID, C.CUSTCD, C.CUSTNM, C.CURRCD,
     C+          COALESCE(D.CLOSEDAY, 0),
     C+          COALESCE((SELECT MAX(S.CLSDATE) FROM BILLSTMP S
     C+                     WHERE S.CUSTID = C.CUSTID
     C+                       AND S.CLSDATE < :W#CLS), 0),
     C+          (SELECT COUNT(*) FROM BILLSTMP S
     C+            WHERE S.CUSTID = C.CUSTID AND S.CLSDATE = :W#CLS)
     C+     FROM CUSTOMERP C LEFT JOIN CUSTCLSP D ON D.CUSTID = C.CUSTID
     C+    ORDER BY C.CUSTCD
     C+    FOR READ ONLY
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE INVCSR CURSOR FOR
     C+   SELECT INVNUM, INVDATE, SUBTOT, TAXAMT, TOTAMT
     C+     FROM INVOICEHP
     C+    WHERE CUSTID = :W#CUSTID AND CURRCD = :W#CUR
     C+      AND INVDATE > :W#LASTCLS AND INVDATE <= :W#CLS
     C+      AND STATUS NOT IN ('CANCELLED', 'VOID')
     C+    ORDER BY INVDATE, INVNUM
     C+    FOR READ ONLY
     C/END-EXEC

     C                   EVAL      W#USER = P#USER
      * 締め日の初期値は前月末
     C                   EVAL      W#DT = %DATE() - %DAYS(%SUBDT(%DATE():*D))
     C                   EVAL      CLSDT = %DEC(W#DT:*ISO)
     C                   EVAL      W#MSG = *BLANKS
     C                   EXSR      @LOAD
     C                   DOW       *ON
     C                   EVAL      CMSG = W#MSG
     C                   EVAL      W#MSG = *BLANKS
     C                   EVAL      *IN92 = *ON
     C                   EVAL      *IN91 = (W#CNT > 0)
     C                   EVAL      *IN94 = *ON
     C                   EXFMT     CLSCTL
     C                   EVAL      *IN31 = *OFF
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
     C     *ISO          TEST(DE)                CLSDT
     C                   IF        %ERROR
     C                   EVAL      *IN31 = *ON
     C                   EVAL      W#MSG = '締め日が無効です(YYYYMMDD)'
     C                   ITER
     C                   ENDIF
     C                   EVAL      W#TODAY = %DEC(%DATE():*ISO)
     C                   IF        CLSDT > W#TODAY
     C                   EVAL      *IN31 = *ON
     C                   EVAL      W#MSG = Join(
     C                             '締め日に未来の日付は指定でき' +
     C                             'ません')
     C                   ITER
     C                   ENDIF
      * 締め日を変えたときは一覧を作り直すだけ(選択は無視)
     C                   IF        CLSDT <> W#LOADED
     C                   EXSR      @LOAD
     C                   ITER
     C                   ENDIF
     C                   EXSR      @SAVEDAY
     C                   IF        W#ERR
     C                   ITER
     C                   ENDIF
      * 修正#DAY 締日を保存した回は一覧を作り直すだけ(古い判定の
      *   まま締めていた)。選択は作り直した一覧でやり直す
     C                   IF        W#DAYCHG > 0
     C                   EVAL      W#MSG = Join(%TRIMR(W#MSG) +
     C                             '。締めは再度1で')
     C                   EXSR      @LOAD
     C                   ITER
     C                   ENDIF
     C                   EXSR      @OPTIONS
     C                   EXSR      @LOAD
     C                   ENDDO
      * 確定していない更新は残さない
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   EVAL      *INLR = *ON
     C                   RETURN

      *=====================================================================
      * @LOAD - 顧客一覧を作る(締め日時点の締めの見込み/結果)
      *=====================================================================
     C     @LOAD         BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     CLSCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN46 = *OFF
     C                   EVAL      W#CNT = 0
     C                   EVAL      W#CLS = CLSDT
     C                   EVAL      W#LOADED = CLSDT
     C                   EXSR      @SETEOM
     C/EXEC SQL
     C+   OPEN CUSCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#CNT < 999
     C/EXEC SQL
     C+   FETCH CUSCSR INTO :C#ROW
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#CNT = W#CNT + 1
     C                   EVAL      W#CUSTID = C#CUSTID
     C                   EVAL      W#CLOSEDAY = C#DAY
     C                   EVAL      W#LASTCLS = C#LAST
     C                   EVAL      SOPT = ' '
     C                   EVAL      SCUSTID = C#CUSTID
     C                   EVAL      SCUSTCD = C#CUSTCD
     C                   EVAL      SCUSTNM = DbcsTrunc(C#CUSTNM:10)
     C                   EVAL      W#CUR = C#CUR
     C                   EVAL      SCUR = C#CUR
     C                   EVAL      SDAY = C#DAY
     C                   EVAL      SDAYORG = C#DAY
     C                   EVAL      SLAST = FmtDate(C#LAST)
     C                   IF        C#DONE > 0
     C                   EXSR      @READSTM
     C                   EVAL      SSTAT = '締め済'
     C                   EVAL      SSTATCD = 'D'
     C                   ELSE
     C                   EXSR      @CALC
     C                   EXSR      @CHKDAY
     C                   IF        W#MATCH
     C                   EVAL      SSTAT = '対象'
     C                   EVAL      SSTATCD = 'T'
     C                   ELSE
     C                   EVAL      SSTAT = '対象外'
     C                   EVAL      SSTATCD = 'N'
     C                   ENDIF
     C                   ENDIF
     C                   EVAL      SCARRY = Fit8(W#CARRY)
     C                   EVAL      STOTAL = Fit8(W#TOTAL)
     C                   EVAL      W#RRN = W#CNT
     C                   WRITE     CLSSFL
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE CUSCSR
     C/END-EXEC
      * 修正#999 上限で打ち切ったことを知らせる
     C                   IF        W#CNT >= 999 AND W#MSG = *BLANKS
     C                   EVAL      W#MSG = Join(
     C                             '顧客が多いため先頭の999社だけ' +
     C                             'を表示しています')
     C                   ENDIF
     C                   IF        W#CNT = 0 AND W#MSG = *BLANKS
     C                   EVAL      W#MSG = '顧客が登録されていません'
     C                   ENDIF
     C                   ENDSR

      * 締め日が月末かどうか(W#ISEOM)と日(W#DAY)
     C     @SETEOM       BEGSR
     C                   EVAL      W#DT = %DATE(W#CLS:*ISO)
     C                   EVAL      W#DAY = %SUBDT(W#DT:*D)
     C                   EVAL      W#EOM = W#DT - %DAYS(W#DAY - 1) + %MONTHS(1)
     C                   EVAL      W#EOM = W#EOM - %DAYS(1)
     C                   EVAL      W#ISEOM = (W#DT = W#EOM)
     C                   ENDSR

      * 顧客の締日(W#CLOSEDAY)が締め日に合うか
      *   締日=日 または 月末で締日が0か月の日数より大きい
     C     @CHKDAY       BEGSR
     C                   EVAL      W#MATCH = (W#CLOSEDAY = W#DAY) OR
     C                             (W#ISEOM AND
     C                              (W#CLOSEDAY = 0 OR W#CLOSEDAY > W#DAY))
     C                   ENDSR

      *=====================================================================
      * @CALC - 前回の締め(W#LASTCLS)から締め日までを集計
      *=====================================================================
     C     @CALC         BEGSR
     C                   EVAL      W#PREV = 0
     C                   IF        W#LASTCLS > 0
     C/EXEC SQL
     C+   SELECT TOTAL INTO :W#PREV FROM BILLSTMP
     C+    WHERE CUSTID = :W#CUSTID AND CLSDATE = :W#LASTCLS
     C+    FETCH FIRST 1 ROW ONLY
     C/END-EXEC
     C                   ENDIF
      * 修正#CUR: 入金も請求書も顧客の通貨のもの(入金は換算前の金額)
     C/EXEC SQL
     C+   SELECT COALESCE(SUM(AMOUNT), 0) INTO :W#PAY FROM PAYMENTP
     C+    WHERE CUSTID = :W#CUSTID AND CURRCD = :W#CUR
     C+      AND PAYDATE > :W#LASTCLS AND PAYDATE <= :W#CLS
     C+      AND STATUS NOT IN ('CANCELLED', 'VOID')
     C/END-EXEC
     C/EXEC SQL
     C+   SELECT COALESCE(SUM(SUBTOT), 0), COALESCE(SUM(TAXAMT), 0),
     C+          COUNT(*)
     C+     INTO :W#SALES, :W#TAX, :W#ICNT
     C+     FROM INVOICEHP
     C+    WHERE CUSTID = :W#CUSTID AND CURRCD = :W#CUR
     C+      AND INVDATE > :W#LASTCLS AND INVDATE <= :W#CLS
     C+      AND STATUS NOT IN ('CANCELLED', 'VOID')
     C/END-EXEC
     C                   EVAL      W#CARRY = W#PREV - W#PAY
     C                   EVAL      W#TOTAL = W#CARRY + W#SALES + W#TAX
     C                   ENDSR

      * 記録済みの締めの内容を読む(再発行・一覧表示用)
     C     @READSTM      BEGSR
     C/EXEC SQL
     C+   SELECT PREVCLS, PREVBAL, PAYAMT, CARRY, SALES, TAX, TOTAL, INVCNT,
     C+          CURRCD
     C+     INTO :W#LASTCLS, :W#PREV, :W#PAY, :W#CARRY, :W#SALES, :W#TAX,
     C+          :W#TOTAL, :W#ICNT, :W#CUR
     C+     FROM BILLSTMP
     C+    WHERE CUSTID = :W#CUSTID AND CLSDATE = :W#CLS
     C+    FETCH FIRST 1 ROW ONLY
     C/END-EXEC
     C                   ENDSR

      *=====================================================================
      * @SAVEDAY - 書き換えた締日を保存
      *=====================================================================
     C     @SAVEDAY      BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      W#DAYCHG = 0
     C                   FOR       W#I = 1 TO W#CNT
     C                   EVAL      W#RRN = W#I
     C     W#RRN         CHAIN     CLSSFL
     C                   IF        NOT %FOUND(BILLCLSD) OR SDAY = SDAYORG
     C                   ITER
     C                   ENDIF
     C                   IF        SDAY > 31
     C                   EVAL      *IN46 = *ON
     C                   UPDATE    CLSSFL
     C                   EVAL      *IN46 = *OFF
     C                   EVAL      W#ERR = *ON
     C                   IF        W#MSG = *BLANKS
     C                   EVAL      W#MSG = Join(
     C                             '締日は1から31、末日締め' +
     C                             'は0を入力してください')
     C                   ENDIF
     C                   ITER
     C                   ENDIF
     C                   EVAL      W#CUSTID = SCUSTID
     C                   EVAL      W#CLOSEDAY = SDAY
     C                   EVAL      W#TODAY = %DEC(%DATE():*ISO)
     C                   EVAL      W#NOW = %DEC(%TIME():*HMS)
     C/EXEC SQL
     C+   MERGE INTO CUSTCLSP T
     C+   USING (VALUES (:W#CUSTID, :W#CLOSEDAY)) AS N (CUSTID, CLOSEDAY)
     C+      ON T.CUSTID = N.CUSTID
     C+    WHEN MATCHED THEN UPDATE SET CLOSEDAY = N.CLOSEDAY,
     C+         UPDUSER = :W#USER, UPDDATE = :W#TODAY, UPDTIM = :W#NOW
     C+    WHEN NOT MATCHED THEN INSERT (CUSTID, CLOSEDAY, UPDUSER,
     C+         UPDDATE, UPDTIM)
     C+         VALUES (N.CUSTID, N.CLOSEDAY, :W#USER, :W#TODAY, :W#NOW)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#ERR = *ON
     C                   EVAL      W#MSG = Join(
     C                             '締日の保存に失敗しました SQL' + 'CODE=' +
     C                             %CHAR(SQLCODE))
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#DAYCHG = W#DAYCHG + 1
     C                   ENDFOR
     C                   IF        W#ERR
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   LEAVESR
     C                   ENDIF
     C                   IF        W#DAYCHG > 0
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   EVAL      W#MSG = Join('締日を保存しました(' +
     C                             %CHAR(W#DAYCHG) + ' 件)')
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @OPTIONS - 1=締め実行(締め済みは再発行)
      *=====================================================================
     C     @OPTIONS      BEGSR
     C                   EVAL      W#NEW = 0
     C                   EVAL      W#RE = 0
     C                   EVAL      W#NG = 0
     C                   FOR       W#I = 1 TO W#CNT
     C                   EVAL      W#RRN = W#I
     C     W#RRN         CHAIN     CLSSFL
     C                   IF        NOT %FOUND(BILLCLSD) OR SOPT <> '1'
     C                   ITER
     C                   ENDIF
     C                   EVAL      W#CUSTID = SCUSTID
     C                   EVAL      W#CUR = SCUR
     C                   SELECT
     C                   WHEN      SSTATCD = 'D'
     C                   EXSR      @READSTM
     C                   EXSR      @PRINT
     C                   EVAL      W#RE = W#RE + 1
     C                   WHEN      SSTATCD = 'T'
     C                   EXSR      @CLOSE
     C                   OTHER
     C                   EVAL      W#NG = W#NG + 1
     C                   ENDSL
     C                   ENDFOR
     C                   IF        %OPEN(CLSPRTF)
     C                   CLOSE     CLSPRTF
     C                   ENDIF
     C                   IF        W#NEW + W#RE + W#NG = 0
     C                   LEAVESR
     C                   ENDIF
      * 修正#MSG @CLOSEでエラーの理由を設定したらそれを表示する
     C                   IF        W#MSG <> *BLANKS
     C                   LEAVESR
     C                   ENDIF
     C                   IF        W#NG > 0 AND W#NEW + W#RE = 0
     C                   EVAL      W#MSG = Join(
     C                             '締日が締め日に合わない顧客は' +
     C                             '締められません')
     C                   LEAVESR
     C                   ENDIF
      * 75バイトに収まる長さにする(全角の途中で切れると化けるため)
     C                   EVAL      W#MSG = Join('合計請求書を発行しました(' +
     C                             '締め' + %CHAR(W#NEW) + '社 再発行' +
     C                             %CHAR(W#RE) + '社 対象外' + %CHAR(W#NG) +
     C                             '社)')
     C                   ENDSR

      *=====================================================================
      * @CLOSE - 1社の締め(記録して印刷)
      *=====================================================================
     C     @CLOSE        BEGSR
      * 同じ締め日以降の締めがあれば締めない(二重締め・期間の重なり)
     C/EXEC SQL
     C+   SELECT COUNT(*) INTO :W#LATER FROM BILLSTMP
     C+    WHERE CUSTID = :W#CUSTID AND CLSDATE >= :W#CLS
     C/END-EXEC
     C                   IF        W#LATER > 0
      * 修正#MSG 理由を表示する(締日が合わないと表示していた)
     C                   EVAL      W#MSG = Join(%TRIM(SCUSTCD) +
     C                             ' は締め日以降に締め済みです')
     C                   EVAL      W#NG = W#NG + 1
     C                   LEAVESR
     C                   ENDIF
     C/EXEC SQL
     C+   SELECT COALESCE(MAX(CLSDATE), 0) INTO :W#LASTCLS FROM BILLSTMP
     C+    WHERE CUSTID = :W#CUSTID AND CLSDATE < :W#CLS
     C/END-EXEC
     C                   EXSR      @CALC
     C                   EVAL      W#TODAY = %DEC(%DATE():*ISO)
     C                   EVAL      W#NOW = %DEC(%TIME():*HMS)
     C/EXEC SQL
     C+   INSERT INTO BILLSTMP (STMID, CUSTID, CLSDATE, PREVCLS, CURRCD,
     C+          PREVBAL, PAYAMT, CARRY, SALES, TAX, TOTAL, INVCNT, CRTUSER,
     C+          CRTDATE, CRTTIM)
     C+   VALUES (NEXT VALUE FOR O2C_STMID_SEQ, :W#CUSTID, :W#CLS,
     C+          :W#LASTCLS, :W#CUR, :W#PREV, :W#PAY, :W#CARRY, :W#SALES,
     C+          :W#TAX, :W#TOTAL, :W#ICNT, :W#USER, :W#TODAY, :W#NOW)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   EVAL      W#MSG = Join(
     C                             '締めの記録に失敗しました SQL' + 'CODE=' +
     C                             %CHAR(SQLCODE))
     C                   EVAL      W#NG = W#NG + 1
     C                   LEAVESR
     C                   ENDIF
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   EXSR      @PRINT
     C                   EVAL      W#NEW = W#NEW + 1
     C                   ENDSR

      *=====================================================================
      * @PRINT - 合計請求書を印刷(W#PREV など集計済みの値を使う)
      *=====================================================================
     C     @PRINT        BEGSR
     C/EXEC SQL
     C+   SELECT CUSTNM, BILLADR INTO :W#CNM, :W#CADR
     C+     FROM CUSTOMERP WHERE CUSTID = :W#CUSTID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#CNM = *BLANKS
     C                   EVAL      W#CADR = *BLANKS
     C                   ENDIF
     C                   IF        NOT %OPEN(CLSPRTF)
     C                   OPEN      CLSPRTF
     C                   ENDIF
     C                   EVAL      PCLSDT = FmtDate(W#CLS)
      * 50=外貨(小数2桁)
     C                   EVAL      *IN50 = (W#CUR <> 'JPY' AND W#CUR <> *BLANKS)
     C                   EVAL      PCURH = W#CUR
     C                   EVAL      PCUSTNM = W#CNM
     C                   EVAL      PBILLADR = W#CADR
      * 修正#INV2 適格請求書: 発行者(社名)と登録番号(COMPANY)
     C                   EVAL      W#CDESC = *BLANKS
     C/EXEC SQL
     C+   SELECT CAST(DESC AS VARCHAR(120) FOR BIT DATA) INTO :W#CDESC
     C+     FROM CODEMSTRP WHERE CODETYPE = 'COMPANY' AND CODEVAL = 'NAME'
     C/END-EXEC
     C                   EVAL      PCOMPNM = DbcsTrunc(%TRIMR(W#CDESC):40)
     C                   EVAL      W#CDESC = *BLANKS
     C/EXEC SQL
     C+   SELECT CAST(DESC AS VARCHAR(120) FOR BIT DATA) INTO :W#CDESC
     C+     FROM CODEMSTRP WHERE CODETYPE = 'COMPANY' AND CODEVAL = 'REGNO'
     C/END-EXEC
     C                   EVAL      PREGNO = W#CDESC
     C                   IF        PREGNO = *BLANKS
     C                   EVAL      PREGNO = '(未登録)'
     C                   ENDIF
     C                   IF        W#LASTCLS > 0
     C                   EVAL      W#DT = %DATE(W#LASTCLS:*ISO) + %DAYS(1)
     C                   EVAL      PFROM = FmtDate(%DEC(W#DT:*ISO))
     C                   ELSE
     C                   EVAL      PFROM = *BLANKS
     C                   ENDIF
     C                   EVAL      PTO = FmtDate(W#CLS)
     C                   EVAL(H)   PPREV = W#PREV
     C                   EVAL(H)   PPAY = W#PAY
     C                   EVAL(H)   PCARRY = W#CARRY
     C                   EVAL(H)   PSALES = W#SALES
     C                   EVAL(H)   PTAX = W#TAX
     C                   EVAL(H)   PTOTAL = W#TOTAL
      * 修正#OVF 桁に収まらない金額・数量は*で表示(異常終了していた)
     C                   MONITOR
     C                   EVALR     PPREVF = %EDITC(%DEC(W#PREV:11:2):'N')
     C                   ON-ERROR
     C                   EVAL      PPREVF = *ALL'*'
     C                   ENDMON
     C                   MONITOR
     C                   EVALR     PPAYF = %EDITC(%DEC(W#PAY:11:2):'N')
     C                   ON-ERROR
     C                   EVAL      PPAYF = *ALL'*'
     C                   ENDMON
     C                   MONITOR
     C                   EVALR     PCARRYF = %EDITC(%DEC(W#CARRY:11:2):'N')
     C                   ON-ERROR
     C                   EVAL      PCARRYF = *ALL'*'
     C                   ENDMON
     C                   MONITOR
     C                   EVALR     PSALESF = %EDITC(%DEC(W#SALES:11:2):'N')
     C                   ON-ERROR
     C                   EVAL      PSALESF = *ALL'*'
     C                   ENDMON
     C                   MONITOR
     C                   EVALR     PTAXF = %EDITC(%DEC(W#TAX:11:2):'N')
     C                   ON-ERROR
     C                   EVAL      PTAXF = *ALL'*'
     C                   ENDMON
     C                   MONITOR
     C                   EVALR     PTOTALF = %EDITC(%DEC(W#TOTAL:11:2):'N')
     C                   ON-ERROR
     C                   EVAL      PTOTALF = *ALL'*'
     C                   ENDMON
     C                   WRITE     PHEAD
     C                   EVAL      *IN90 = *OFF
     C/EXEC SQL
     C+   OPEN INVCSR
     C/END-EXEC
      * 明細の件数(W#I は呼び出し元のループで使うので別の変数)
     C                   EVAL      W#PCNT = 0
     C                   DOW       SQLCODE = 0
     C/EXEC SQL
     C+   FETCH INVCSR INTO :V#ROW
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
     C                   EVAL(H)   PISUB = V#SUB
     C                   EVAL(H)   PITAX = V#TAX
     C                   EVAL(H)   PITOT = V#TOT
     C                   MONITOR
     C                   EVALR     PISUBF = %EDITC(%DEC(V#SUB:11:2):'N')
     C                   ON-ERROR
     C                   EVAL      PISUBF = *ALL'*'
     C                   ENDMON
     C                   MONITOR
     C                   EVALR     PITAXF = %EDITC(%DEC(V#TAX:11:2):'N')
     C                   ON-ERROR
     C                   EVAL      PITAXF = *ALL'*'
     C                   ENDMON
     C                   MONITOR
     C                   EVALR     PITOTF = %EDITC(%DEC(V#TOT:11:2):'N')
     C                   ON-ERROR
     C                   EVAL      PITOTF = *ALL'*'
     C                   ENDMON
     C                   WRITE     PDETAIL
     C                   EVAL      W#PCNT = W#PCNT + 1
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE INVCSR
     C/END-EXEC
     C                   IF        W#PCNT = 0
     C                   WRITE     PNONE
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * Fit8 - 8桁(99,999,999)に収めて四捨五入
      *=====================================================================
     P Fit8            B
     D Fit8            PI             8P 0
     D   pAmt                        15P 2 CONST
     D wAmt            S             15P 0
     C                   EVAL(H)   wAmt = pAmt
     C                   IF        wAmt > 99999999
     C                   RETURN    99999999
     C                   ENDIF
     C                   IF        wAmt < -99999999
     C                   RETURN    -99999999
     C                   ENDIF
     C                   RETURN    wAmt
     P Fit8            E

      * Join - 連結した全角文字列の SI+SO を取り除く
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
