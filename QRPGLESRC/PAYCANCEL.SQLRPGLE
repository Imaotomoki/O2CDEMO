     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: PAYCANCEL
      * 説明: 入金取消(登録済みの入金を取り消し、消込を戻す)
      * 作成: 修正#PCN 新規作成。メニューの10番から呼ぶ
      *   一覧で 4=取消 → 確認画面(消込を戻す請求書) → F6 で実行
      *   - 入金の状態を CANCELLED にする(締め請求の入金合計から外れる)
      *   - 消し込んだ請求書の入金済を減らし、残高を戻す
      *     (入金済が0になれば OPEN、残れば PARTIAL)
      *   - 消込明細(PAYAPPLYP)は消さず、マイナスの明細を追加して履歴を残す
      *   - 締め済みの期間(入金日 <= 最終締め日)の入金は取り消せない
      *   - すべて1つのトランザクションで行い、失敗したらROLLBACK
      *   ACTGRP(*NEW): SQLのコミット定義を終了時に閉じる
      *     (*CALLERだと既定の活動化グループに残り、出荷確定の
      *      STRCMTCTL が CPF8351 で失敗する)
      *=====================================================================
     FPAYCNLD   CF   E             WORKSTN SFILE(LSFL:W#RRN)

     D W#RRN           S              4S 0
     D W#CNT           S              4S 0
     D W#DONE          S               N
     D W#ERR           S               N
     D W#MSG           S             78A
     D W#PVAL          S             30A
     D W#FCUST         S             20A
     D W#SEL           S             18P 0
     D W#PAYID         S             18P 0
     D W#D8            S              8P 0
     D W#T6            S              6P 0
     D W#LCLS          S              8P 0
     D W#K             S             10I 0
      * 入金の読み込み用
     D S#PAYID         S             18P 0
     D S#PAYNUM        S             20A
     D S#PAYDT         S              8P 0
     D S#CUSTID        S              9P 0
     D S#CUSTCD        S             20A
     D S#CUSTNM        S             60A
     D S#CUR           S              3A
     D S#AMT           S             15P 2
     D S#METH          S             20A
     D S#STS           S             12A
     D S#REF           S             60A
      * 消込先の請求書(請求書ごとの消込額の合計)
     D A#INVID         S             18P 0
     D A#INVNUM        S             20A
     D A#INVDT         S              8P 0
     D A#AMT           S             15P 2
     D A#BAL           S             15P 2
     D A#NEG           S             15P 2
     D W#AN            S             10I 0
     D W#AINV          S             18P 0 DIM(100)
     D W#ANUM          S             20A   DIM(100)
     D W#ADT           S              8P 0 DIM(100)
     D W#AAMT          S             15P 2 DIM(100)
     D W#ABAL          S             15P 2 DIM(100)
     D W#LN            S             76A   DIM(10)
     D O2CPRMPT        PR                  EXTPGM('O2CPRMPT')
     D   pType                       10A   CONST
     D   pCdType                     30A   CONST
     D   pValue                      30A
     D FmtAmt          PR            15A
     D   pAmt                        15P 2 CONST
     D   pCur                         3A   CONST
     D StsName         PR            10A
     D   pSts                        12A   CONST
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
     D DbcsTrunc       PR           256A   VARYING
     D   pStr                       256A   VARYING CONST
     D   pMax                        10I 0 CONST
     D Fit9            PR             9P 0
     D   pAmt                        15P 2 CONST
     D FmtDate         PR            10A
     D   pDate                        8P 0 CONST

     C/EXEC SQL
     C+   SET OPTION COMMIT = *CHG, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE LSTCSR CURSOR FOR
     C+   SELECT P.PAYID, P.PAYNUM, P.PAYDATE, C.CUSTCD, P.CURRCD,
     C+          P.AMOUNT, P.PAYMETH, P.STATUS
     C+     FROM PAYMENTP P JOIN CUSTOMERP C ON C.CUSTID = P.CUSTID
     C+    WHERE (:W#FCUST = ' ' OR C.CUSTCD = :W#FCUST)
     C+    ORDER BY P.PAYID DESC
     C+    FOR READ ONLY
     C/END-EXEC
     C/EXEC SQL
     C+   DECLARE APPCSR CURSOR FOR
     C+   SELECT A.INVID, H.INVNUM, H.INVDATE, SUM(A.APPLYAMT), H.BALANCE
     C+     FROM PAYAPPLYP A JOIN INVOICEHP H ON H.INVID = A.INVID
     C+    WHERE A.PAYID = :W#PAYID
     C+    GROUP BY A.INVID, H.INVNUM, H.INVDATE, H.BALANCE
     C+    HAVING SUM(A.APPLYAMT) > 0
     C+    ORDER BY H.INVDATE, A.INVID
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
     C                   EVAL      *IN31 = *OFF
     C                   EVAL      LMSG = *BLANKS
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
      * F4=顧客の検索
     C                   IF        *IN04
     C                   EVAL      W#PVAL = LFCUST
     C                   CALLP     O2CPRMPT('CUST':' ':W#PVAL)
     C                   EVAL      LFCUST = W#PVAL
     C                   EXSR      @LOAD
     C                   ITER
     C                   ENDIF
      * 4=取消 が付いた最初の入金を確認画面へ
     C                   EVAL      W#SEL = 0
     C                   IF        W#CNT > 0
     C                   READC     LSFL
     C                   DOW       NOT %EOF(PAYCNLD)
     C                   IF        LOPT = '4' AND W#SEL = 0
     C                   EVAL      W#SEL = LPAYID
     C                   ENDIF
     C                   READC     LSFL
     C                   ENDDO
     C                   ENDIF
     C                   IF        W#SEL > 0
     C                   EVAL      W#PAYID = W#SEL
     C                   EXSR      @CONFIRM
     C                   ENDIF
     C                   EXSR      @LOAD
     C                   ENDDO
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   EVAL      *INLR = *ON
     C                   RETURN

      *=====================================================================
      * @LOAD - 入金の一覧を読み込む(顧客コードで絞り込み)
      *=====================================================================
     C     @LOAD         BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     LCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#CNT = 0
     C                   EVAL      W#FCUST = LFCUST
     C/EXEC SQL
     C+   OPEN LSTCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#CNT < 999
     C/EXEC SQL
     C+   FETCH LSTCSR INTO :S#PAYID, :S#PAYNUM, :S#PAYDT, :S#CUSTCD,
     C+     :S#CUR, :S#AMT, :S#METH, :S#STS
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      LOPT = ' '
     C                   EVAL      LPAYID = S#PAYID
     C                   EVAL      LPAYNUM = S#PAYNUM
     C                   EVAL      LPAYDT = FmtDate(S#PAYDT)
     C                   EVAL      LCUSTCD = S#CUSTCD
     C                   EVAL      LCUR = S#CUR
     C                   EVAL      LAMT = FmtAmt(S#AMT:S#CUR)
     C                   EVAL      LMETH = S#METH
     C                   EVAL      LSTS = StsName(S#STS)
     C                   EVAL      W#CNT = W#CNT + 1
     C                   EVAL      W#RRN = W#CNT
     C                   WRITE     LSFL
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE LSTCSR
     C/END-EXEC
     C                   IF        W#CNT = 0 AND LMSG = *BLANKS
     C                   EVAL      LMSG = '該当する入金がありません'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @CONFIRM - 確認画面(F6=取消実行 F12=戻る)
      *=====================================================================
     C     @CONFIRM      BEGSR
     C                   EXSR      @GETPAY
     C                   IF        W#ERR
     C                   EVAL      LMSG = W#MSG
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      CMSG = *BLANKS
     C                   DOW       *ON
     C                   EXFMT     CNF
     C                   EVAL      CMSG = *BLANKS
     C                   IF        *IN03
     C                   EVAL      W#DONE = *ON
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN12
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN06
     C                   EXSR      @CANCEL
     C                   IF        NOT W#ERR
     C                   EVAL      LMSG = '入金 ' + %TRIM(CPAYNUM) +
     C                             ' を取り消しました'
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      CMSG = W#MSG
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR

      *=====================================================================
      * @GETPAY - 入金と消込先を読んで確認画面の項目を作る
      *=====================================================================
     C     @GETPAY       BEGSR
     C                   EVAL      W#ERR = *OFF
     C/EXEC SQL
     C+   SELECT P.PAYNUM, P.PAYDATE, P.CUSTID, C.CUSTCD, C.CUSTNM,
     C+          P.CURRCD, P.AMOUNT, P.PAYMETH, P.STATUS, P.REFNO
     C+     INTO :S#PAYNUM, :S#PAYDT, :S#CUSTID, :S#CUSTCD, :S#CUSTNM,
     C+          :S#CUR, :S#AMT, :S#METH, :S#STS, :S#REF
     C+     FROM PAYMENTP P JOIN CUSTOMERP C ON C.CUSTID = P.CUSTID
     C+    WHERE P.PAYID = :W#PAYID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#MSG = '入金が見つかりません'
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   IF        S#STS = 'CANCELLED'
     C                   EVAL      W#MSG = 'すでに取り消し済みです'
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
      * 修正#RET 赤伝の相殺(区分 CREDITNOTE)は入金取消の対象外
     C                   IF        S#METH = 'CREDITNOTE'
     C                   EVAL      W#MSG = '赤伝の相殺は取り消せません'
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   EXSR      @CHKCLS
     C                   IF        W#ERR
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      CPAYNUM = S#PAYNUM
     C                   EVAL      CSTS = StsName(S#STS)
     C                   EVAL      CCUSTCD = S#CUSTCD
     C                   EVAL      CCUSTNM = DbcsTrunc(%TRIMR(S#CUSTNM):38)
     C                   EVAL      CPAYDT = FmtDate(S#PAYDT)
     C                   EVAL      CMETH = S#METH
     C                   EVAL      CCUR = S#CUR
     C                   EVAL      CAMT = FmtAmt(S#AMT:S#CUR)
     C                   EVAL      CREF = DbcsTrunc(%TRIMR(S#REF):30)
     C                   EXSR      @LOADAPP
      * 消込先の請求書(10行まで。多いときは最後の行を「ほか n 件」)
     C                   EVAL      W#LN = *BLANKS
     C                   IF        W#AN = 0
     C                   EVAL      W#LN(1) = '  (消込なし)'
     C                   ENDIF
     C                   EVAL      W#K = 1
     C                   DOW       W#K <= W#AN AND W#K <= 10
     C                   IF        W#K = 10 AND W#AN > 10
     C                   EVAL      W#LN(W#K) = '  ほか ' +
     C                             %CHAR(W#AN - 9) + ' 件'
     C                   ELSE
     C                   EVAL      W#LN(W#K) = '  ' + W#ANUM(W#K) + ' ' +
     C                             FmtDate(W#ADT(W#K)) + '   ' +
     C                             FmtAmt(W#AAMT(W#K):S#CUR) + '   ' +
     C                             FmtAmt(W#ABAL(W#K) + W#AAMT(W#K):S#CUR)
     C                   ENDIF
     C                   EVAL      W#K = W#K + 1
     C                   ENDDO
     C                   EVAL      CAL01 = W#LN(1)
     C                   EVAL      CAL02 = W#LN(2)
     C                   EVAL      CAL03 = W#LN(3)
     C                   EVAL      CAL04 = W#LN(4)
     C                   EVAL      CAL05 = W#LN(5)
     C                   EVAL      CAL06 = W#LN(6)
     C                   EVAL      CAL07 = W#LN(7)
     C                   EVAL      CAL08 = W#LN(8)
     C                   EVAL      CAL09 = W#LN(9)
     C                   EVAL      CAL10 = W#LN(10)
     C                   ENDSR

      *=====================================================================
      * @CHKCLS - 締め済みの期間の入金は取り消せない
      *   (合計請求書の「御入金額」が変わってしまうため)
      *=====================================================================
     C     @CHKCLS       BEGSR
     C/EXEC SQL
     C+   SELECT COALESCE(MAX(CLSDATE), 0) INTO :W#LCLS FROM BILLSTMP
     C+    WHERE CUSTID = :S#CUSTID AND CURRCD = :S#CUR
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#LCLS = 0
     C                   ENDIF
     C                   IF        W#LCLS > 0 AND S#PAYDT <= W#LCLS
     C                   EVAL      W#MSG = '締め済み(' + FmtDate(W#LCLS) +
     C                             ')の入金は取り消せません'
     C                   EVAL      W#ERR = *ON
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @LOADAPP - 消込先の請求書を配列に読む
      *=====================================================================
     C     @LOADAPP      BEGSR
     C                   EVAL      W#AN = 0
     C/EXEC SQL
     C+   OPEN APPCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#AN < 100
     C/EXEC SQL
     C+   FETCH APPCSR INTO :A#INVID, :A#INVNUM, :A#INVDT, :A#AMT, :A#BAL
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#AN = W#AN + 1
     C                   EVAL      W#AINV(W#AN) = A#INVID
     C                   EVAL      W#ANUM(W#AN) = A#INVNUM
     C                   EVAL      W#ADT(W#AN) = A#INVDT
     C                   EVAL      W#AAMT(W#AN) = A#AMT
     C                   EVAL      W#ABAL(W#AN) = A#BAL
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE APPCSR
     C/END-EXEC
     C                   ENDSR

      *=====================================================================
      * @CANCEL - 取消の実行(1つのトランザクション)
      *=====================================================================
     C     @CANCEL       BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      W#D8 = %DEC(%DATE():*ISO)
     C                   EVAL      W#T6 = %DEC(%TIME():*HMS)
     C                   EXSR      @CHKCLS
     C                   IF        W#ERR
     C                   LEAVESR
     C                   ENDIF
      * 先に入金を更新して行をロックする(同時に取り消されるのを防ぐ)
     C/EXEC SQL
     C+   UPDATE PAYMENTP SET STATUS = 'CANCELLED',
     C+          UPDUSER = USER, UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHERE PAYID = :W#PAYID AND STATUS <> 'CANCELLED'
     C/END-EXEC
     C                   IF        SQLCODE < 0
     C                   EVAL      W#MSG = '入金の更新エラー'
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
     C                   IF        SQLER3 = 0
     C                   EVAL      W#MSG = 'すでに取り消し済みです'
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
      * ロックした後の消込を読み直して、請求書ごとに戻す
      * 修正#APN カーソルを直接読む(配列は100件までで、超えた分の
      *   請求書が戻らないまま確定していた。読込エラーも取消す)
     C/EXEC SQL
     C+   OPEN APPCSR
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#MSG = '消込の読み込みエラー'
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
     C                   DOW       *ON
     C/EXEC SQL
     C+   FETCH APPCSR INTO :A#INVID, :A#INVNUM, :A#INVDT, :A#AMT, :A#BAL
     C/END-EXEC
     C                   IF        SQLCODE = 100
     C                   LEAVE
     C                   ENDIF
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#MSG = '消込の読み込みエラー'
     C                   EXSR      @CLSAPP
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      A#NEG = - A#AMT
     C/EXEC SQL
     C+   UPDATE INVOICEHP
     C+      SET PAIDAMT = PAIDAMT - :A#AMT,
     C+          BALANCE = BALANCE + :A#AMT,
     C+          STATUS = CASE WHEN PAIDAMT - :A#AMT <= 0
     C+                        THEN 'OPEN' ELSE 'PARTIAL' END,
     C+          UPDUSER = USER, UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHERE INVID = :A#INVID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#MSG = '請求書の更新エラー'
     C                   EXSR      @CLSAPP
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
      * 消込明細はマイナスの明細を追加して打ち消す(履歴を残す)
     C/EXEC SQL
     C+   INSERT INTO PAYAPPLYP
     C+     (APPLYID, PAYID, INVID, APPLYAMT, APPLYDATE,
     C+      CRTUSER, CRTDATE, CRTTIM)
     C+   VALUES
     C+     (NEXT VALUE FOR O2C_APPLYID_SEQ, :W#PAYID, :A#INVID, :A#NEG,
     C+      :W#D8, USER, :W#D8, :W#T6)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#MSG = '消込明細の登録エラー'
     C                   EXSR      @CLSAPP
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
     C                   ENDDO
     C                   EXSR      @CLSAPP
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#MSG = '確定(COMMIT)エラー'
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
     C                   ENDSR

      * @CLSAPP - 消込のカーソルを閉じる
     C     @CLSAPP       BEGSR
     C/EXEC SQL
     C+   CLOSE APPCSR
     C/END-EXEC
     C                   ENDSR

      * @ROLLBK - エラーのときは取消をすべて元に戻す
     C     @ROLLBK       BEGSR
     C                   EVAL      W#ERR = *ON
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   ENDSR

      * FmtAmt - 金額を右寄せの文字に(円は小数なし、外貨は小数2桁)
     P FmtAmt          B
     D FmtAmt          PI            15A
     D   pAmt                        15P 2 CONST
     D   pCur                         3A   CONST
     D wOut            S             15A
     C                   IF        pCur = 'JPY' OR pCur = *BLANKS
      * 修正#OVF 桁に収まらない金額・数量は*で表示(異常終了していた)
     C                   MONITOR
     C                   EVALR     wOut = %EDITC(%DECH(pAmt:11:0):'J')
     C                   ON-ERROR
     C                   EVAL      wOut = *ALL'*'
     C                   ENDMON
     C                   ELSE
     C                   MONITOR
     C                   EVALR     wOut = %EDITC(%DECH(pAmt:11:2):'J')
     C                   ON-ERROR
     C                   EVAL      wOut = *ALL'*'
     C                   ENDMON
     C                   ENDIF
     C                   RETURN    wOut
     P FmtAmt          E

      * StsName - 入金の状態を日本語に
     P StsName         B
     D StsName         PI            10A
     D   pSts                        12A   CONST
     C                   SELECT
     C                   WHEN      pSts = 'APPLIED'
     C                   RETURN    '消込済'
     C                   WHEN      pSts = 'CANCELLED'
     C                   RETURN    '取消済'
     C                   ENDSL
     C                   RETURN    pSts
     P StsName         E

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
