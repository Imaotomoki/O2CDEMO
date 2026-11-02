     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: RETENTRY
      * 説明: 返品・値引き(赤伝)。メニューの19番から呼ぶ
      * 作成: 修正#RET 新規作成
      *   請求書を指定し、明細ごとに返品数(区分1)か値引額(区分2)
      *   Enter=計算、F6=赤伝(マイナスの請求書 CN-n)を作成して印刷
      *   返品は在庫に戻す(戻す倉庫。STKMOVEP に RETURN を記録)
      *   赤伝は元の請求書の残高と相殺し、残りは同じ顧客・通貨の
      *   未収の請求書と古い順に相殺する(区分 CREDITNOTE の入金として
      *   消込明細に記録。入金額は0)。相殺しきれない分は赤伝の残高に
      *   マイナスで残る
      *   税は税率ごとに1回切り捨て(円は整数、外貨は小数2桁)
      *   全体を1つのトランザクションで行い、失敗したら元に戻す
      *=====================================================================
     FRETENTD   CF   E             WORKSTN SFILE(RSFL:W#RRN)

     D W#RRN           S              4S 0
     D W#CNT           S              4S 0
     D W#I             S             10I 0
     D W#K             S             10I 0
     D W#ERR           S               N
     D W#CALCOK        S               N
     D W#F6            S               N
     D W#LOADED        S             20A
     D W#LASTTOT       S             15P 2
     D W#PVAL          S             30A
     D W#USER          S             10A   INZ(*USER)
     D W#D8            S              8P 0
     D W#T6            S              6P 0
     D W#FRN           S               N
     D W#SQLCD         S             10I 0
      * 元の請求書
     D H#INVID         S             18P 0
     D H#CUSTID        S              9P 0
     D H#SOID          S             18P 0
     D H#CUR           S              3A
     D H#BAL           S             15P 2
     D H#TOT           S             15P 2
     D H#STS           S             12A
     D H#CD            S             20A
     D H#NM            S            120A
     D H#WHCD          S             10A
     D W#WHID          S              4P 0
      * 明細の読み込み
     D S#LINE          S              9P 0
     D S#ITEMID        S              9P 0
     D S#ITEMCD        S             12A
     D S#DESC          S            240A
     D S#QTY           S             15P 3
     D S#PRC           S             15P 4
     D S#TAXR          S              5P 3
     D S#AMT           S             15P 2
     D S#RET           S             15P 3
      * 計算(税率別)
     D W#TXRT          S              5P 3 DIM(10)
     D W#TXBASE        S             15P 2 DIM(10)
     D W#TXN           S             10I 0
     D W#LNAMT         S             15P 2
     D W#SUB           S             15P 2
     D W#TAX           S             15P 2
     D W#TX1           S             15P 2
     D W#TOT           S             15P 2
      * 作成
     D W#CNID          S             18P 0
     D W#CNNUM         S             20A
     D W#NEWLN         S              9P 0
     D W#RETID         S             18P 0
     D W#Q             S             15P 3
     D W#NEGQ          S             15P 3
     D W#NEGA          S             15P 2
     D W#PRCU          S             15P 4
     D W#BEF           S             15P 3
     D W#AFT           S             15P 3
     D W#REASON        S             60A
     D W#TYPE          S              1A
      * 相殺
     D W#OINV          S             18P 0 DIM(50)
     D W#OBAL          S             15P 2 DIM(50)
     D W#ON            S             10I 0
     D W#REM           S             15P 2
     D W#APP           S             15P 2
     D W#APPTOT        S             15P 2
     D W#PAYID         S             18P 0
     D W#PAYNUM        S             20A
     D W#A1            S             15P 2
     D W#A2            S             15P 2
     D W#IID           S             18P 0
     D O2CPRMPT        PR                  EXTPGM('O2CPRMPT')
     D   PTYPE                       10A   CONST
     D   PCDTYPE                     30A   CONST
     D   PVALUE                      30A
     D INVPRINT        PR                  EXTPGM('INVPRINT')
     D   PINVNUM                     20A
     D FMTAMT          PR            15A
     D   PAMT                        15P 2 CONST
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
     D DBCSTRUNC       PR           256A   VARYING
     D   PSTR                       256A   VARYING CONST
     D   PMAX                        10I 0 CONST

     C/EXEC SQL
     C+   SET OPTION COMMIT = *CHG, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE LINCSR CURSOR FOR
     C+   SELECT L.LINENO, L.ITEMID, COALESCE(I.ITEMCD, ' '),
     C+          CAST(L.DESC AS VARCHAR(240) FOR BIT DATA), L.QTY,
     C+          L.UNITPRC, L.TAXRATE, L.LINEAMT,
     C+          COALESCE((SELECT SUM(R.RETQTY) FROM RETURNP R
     C+            WHERE R.ORGINVID = L.INVID AND R.ORGLINE = L.LINENO
     C+              AND R.RETTYPE = '1'), 0)
     C+     FROM INVOICELP L LEFT JOIN ITEMP I ON I.ITEMID = L.ITEMID
     C+    WHERE L.INVID = :H#INVID
     C+    ORDER BY L.LINENO FOR READ ONLY
     C/END-EXEC

     C                   EVAL      DINV = *BLANKS
     C                   EVAL      DTYPE = '1'
     C                   EXSR      @CLEAR
     C                   DOW       *ON
     C                   EVAL      *IN92 = *ON
     C                   EVAL      *IN91 = (W#CNT > 0)
     C                   EVAL      *IN94 = *ON
     C                   WRITE     RFOOT
     C                   EXFMT     RCTL
      * F6はサブファイルを読むと消えるので先に保存する
     C                   EVAL      W#F6 = *IN06
     C                   EVAL      DMSG = *BLANKS
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN04
     C                   EXSR      @PROMPT
     C                   ITER
     C                   ENDIF
      * 請求番号を変えたら明細を読み直す
     C                   IF        DINV <> W#LOADED
     C                   EXSR      @LOAD
     C                   ITER
     C                   ENDIF
     C                   IF        W#CNT = 0
     C                   ITER
     C                   ENDIF
     C                   EXSR      @CALC
     C                   IF        W#ERR
     C                   EVAL      W#CALCOK = *OFF
     C                   ITER
     C                   ENDIF
      * F6: 直前の計算と同じ内容なら作成(違えば計算結果の確認だけ)
     C                   IF        W#F6
     C                   IF        W#CALCOK AND W#TOT = W#LASTTOT
     C                   EXSR      @CREATE
     C                   ELSE
     C                   EVAL      DMSG = '金額を確認して、もう一度F6で作成'
     C                   ENDIF
     C                   ELSE
     C                   EVAL      DMSG = '確認してF6で赤伝を作成します'
     C                   ENDIF
     C                   EVAL      W#CALCOK = *ON
     C                   EVAL      W#LASTTOT = W#TOT
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C                   RETURN

      *=====================================================================
      * @CLEAR - 画面の初期化
      *=====================================================================
     C     @CLEAR        BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     RCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#CNT = 0
     C                   EVAL      W#LOADED = *BLANKS
     C                   EVAL      W#CALCOK = *OFF
     C                   EVAL      DCUST = *BLANKS
     C                   EVAL      DCUR = *BLANKS
     C                   EVAL      DBAL = *BLANKS
     C                   EVAL      DREASON = *BLANKS
     C                   EVAL      DWHCD = *BLANKS
     C                   EVAL      DSUB = *BLANKS
     C                   EVAL      DTAX = *BLANKS
     C                   EVAL      DTOT = *BLANKS
     C                   ENDSR

      * @PROMPT - F4(請求番号・倉庫)
     C     @PROMPT       BEGSR
     C                   SELECT
     C                   WHEN      CSRFLD = 'DWHCD'
     C                   EVAL      W#PVAL = DWHCD
     C                   CALLP     O2CPRMPT('WH':' ':W#PVAL)
     C                   EVAL      DWHCD = W#PVAL
     C                   OTHER
     C                   EVAL      W#PVAL = DINV
     C                   CALLP     O2CPRMPT('INV':' ':W#PVAL)
     C                   EVAL      DINV = W#PVAL
     C                   ENDSL
     C                   ENDSR

      *=====================================================================
      * @LOAD - 請求書と明細を読む
      *=====================================================================
     C     @LOAD         BEGSR
     C                   EXSR      @CLEAR
     C                   EVAL      W#LOADED = DINV
     C                   EVAL      *IN31 = *OFF
     C                   IF        DINV = *BLANKS
     C                   LEAVESR
     C                   ENDIF
     C/EXEC SQL
     C+   SELECT H.INVID, H.CUSTID, H.SOID, H.CURRCD, H.BALANCE, H.TOTAMT,
     C+          H.STATUS, C.CUSTCD,
     C+          CAST(C.CUSTNM AS VARCHAR(120) FOR BIT DATA),
     C+          COALESCE(W.WHCD, ' ')
     C+     INTO :H#INVID, :H#CUSTID, :H#SOID, :H#CUR, :H#BAL, :H#TOT,
     C+          :H#STS, :H#CD, :H#NM, :H#WHCD
     C+     FROM INVOICEHP H JOIN CUSTOMERP C ON C.CUSTID = H.CUSTID
     C+     LEFT JOIN SORDERHP S ON S.SOID = H.SOID
     C+     LEFT JOIN WAREHOUSEP W ON W.WHID = S.WHID
     C+    WHERE H.INVNUM = :DINV
     C/END-EXEC
     C                   SELECT
     C                   WHEN      SQLCODE <> 0
     C                   EVAL      DMSG = '請求書が見つかりません'
     C                   WHEN      H#STS = 'CANCELLED'
     C                   EVAL      DMSG = '取消済の請求書は対象外です'
     C                   WHEN      H#TOT < 0
     C                   EVAL      DMSG = '赤伝は対象外です'
     C                   ENDSL
     C                   IF        DMSG <> *BLANKS
     C                   EVAL      *IN31 = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#FRN = (H#CUR <> 'JPY' AND H#CUR <> *BLANKS)
     C                   EVAL      DCUST = DBCSTRUNC(%TRIM(H#CD) + ' ' +
     C                             %TRIMR(H#NM):36)
     C                   EVAL      DCUR = H#CUR
     C                   EVAL      DBAL = FMTAMT(H#BAL)
     C                   EVAL      DWHCD = H#WHCD
     C/EXEC SQL
     C+   OPEN LINCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#CNT < 99
     C/EXEC SQL
     C+   FETCH LINCSR INTO :S#LINE, :S#ITEMID, :S#ITEMCD, :S#DESC, :S#QTY,
     C+     :S#PRC, :S#TAXR, :S#AMT, :S#RET
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#CNT = W#CNT + 1
     C                   EVAL      LLINE = %CHAR(S#LINE)
     C                   EVAL      LITEM = S#ITEMCD
     C                   EVAL      LNAME = DBCSTRUNC(%TRIMR(S#DESC):16)
     C                   EVALR     LQTY = %CHAR(%DECH(S#QTY:9:0))
     C                   EVALR     LRET = %CHAR(%DECH(S#RET:9:0))
     C                   EVAL      LPRC = %SUBST(FMTAMT(S#PRC):6:10)
     C                   EVAL      RQTY = 0
     C                   EVAL      RAMT = 0
     C                   EVAL      LORGLN = S#LINE
     C                   EVAL      LITEMID = S#ITEMID
     C                   EVAL(H)   LQTYV = S#QTY
     C                   EVAL(H)   LRETV = S#RET
     C                   EVAL      LPRCV = S#PRC
     C                   EVAL      LAMTV = S#AMT
     C                   EVAL      LTAXR = S#TAXR
     C                   EVAL      W#RRN = W#CNT
     C                   WRITE     RSFL
     C                   ENDDO
     C                   EVAL      W#SQLCD = SQLCODE
     C/EXEC SQL
     C+   CLOSE LINCSR
     C/END-EXEC
     C                   IF        W#SQLCD < 0
     C                   EVAL      DMSG = '明細の読み込みエラー SQLCODE=' +
     C                             %CHAR(W#SQLCD)
     C                   ELSEIF    W#CNT = 0
     C                   EVAL      DMSG = '明細がありません'
     C                   ELSE
     C                   EVAL      DMSG = '返品数か値引額を入れてEnter'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @CALC - 入力チェックと赤伝の金額(税率別に1回切り捨て)
      *=====================================================================
     C     @CALC         BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      *IN32 = *OFF
     C                   EVAL      W#TXN = 0
     C                   EVAL      W#SUB = 0
     C                   FOR       W#I = 1 TO W#CNT
     C                   EVAL      W#RRN = W#I
     C     W#RRN         CHAIN     RSFL
     C                   IF        NOT %FOUND(RETENTD)
     C                   ITER
     C                   ENDIF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN42 = *OFF
     C                   EVAL      W#LNAMT = 0
     C                   IF        DTYPE = '1'
     C                   IF        RAMT <> 0
     C                   EVAL      *IN42 = *ON
     C                   EVAL      DMSG = '返品では値引額は入力しません'
     C                   ELSEIF    RQTY > LQTYV - LRETV
     C                   EVAL      *IN41 = *ON
     C                   EVAL      DMSG = '返品数が返品できる数を超えています'
     C                   ELSE
     C                   EVAL(H)   W#LNAMT = RQTY * LPRCV
     C                   ENDIF
     C                   ELSE
     C                   IF        RQTY <> 0
     C                   EVAL      *IN41 = *ON
     C                   EVAL      DMSG = '値引きでは返品数は入力しません'
     C                   ELSEIF    RAMT > LAMTV
     C                   EVAL      *IN42 = *ON
     C                   EVAL      DMSG = '値引額が明細の金額を超えています'
     C                   ELSEIF    NOT W#FRN AND RAMT <> %INT(RAMT)
     C                   EVAL      *IN42 = *ON
     C                   EVAL      DMSG = '円の値引額は整数で入力します'
     C                   ELSE
     C                   EVAL      W#LNAMT = RAMT
     C                   ENDIF
     C                   ENDIF
     C                   IF        *IN41 OR *IN42
     C                   EVAL      W#ERR = *ON
     C                   ENDIF
      * 入力した行は次回も読めるように変更済みにする
     C                   UPDATE    RSFL
      * 税率別に集計
     C                   IF        W#LNAMT > 0
     C                   EVAL      W#SUB = W#SUB + W#LNAMT
     C                   EVAL      W#K = 0
     C                   IF        W#TXN > 0
     C                   EVAL      W#K = %LOOKUP(LTAXR:W#TXRT:1:W#TXN)
     C                   ENDIF
     C                   IF        W#K = 0 AND W#TXN < 10
     C                   EVAL      W#TXN = W#TXN + 1
     C                   EVAL      W#K = W#TXN
     C                   EVAL      W#TXRT(W#K) = LTAXR
     C                   EVAL      W#TXBASE(W#K) = 0
     C                   ENDIF
     C                   IF        W#K > 0
     C                   EVAL      W#TXBASE(W#K) = W#TXBASE(W#K) + W#LNAMT
     C                   ENDIF
     C                   ENDIF
     C                   ENDFOR
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN42 = *OFF
     C                   IF        W#ERR
     C                   LEAVESR
     C                   ENDIF
     C                   IF        W#SUB = 0
     C                   EVAL      DMSG = '返品数か値引額を入れてください'
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
      * 返品は戻す倉庫が必要
     C                   IF        DTYPE = '1'
     C/EXEC SQL
     C+   SELECT WHID INTO :W#WHID FROM WAREHOUSEP WHERE WHCD = :DWHCD
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      *IN32 = *ON
     C                   EVAL      DMSG = '戻す倉庫が登録されていません(F4)'
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   ENDIF
     C                   EVAL      W#TAX = 0
     C                   FOR       W#K = 1 TO W#TXN
     C                   IF        W#FRN
     C                   EVAL      W#TX1 = W#TXBASE(W#K) * W#TXRT(W#K)
     C                   ELSE
     C                   EVAL      W#TX1 = %INT(W#TXBASE(W#K) * W#TXRT(W#K))
     C                   ENDIF
     C                   EVAL      W#TAX = W#TAX + W#TX1
     C                   ENDFOR
     C                   EVAL      W#TOT = W#SUB + W#TAX
     C                   EVAL      DSUB = FMTAMT(- W#SUB)
     C                   EVAL      DTAX = FMTAMT(- W#TAX)
     C                   EVAL      DTOT = FMTAMT(- W#TOT)
     C                   ENDSR

      *=====================================================================
      * @CREATE - 赤伝の作成(1つのトランザクション)
      *=====================================================================
     C     @CREATE       BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      W#D8 = %DEC(%DATE():*ISO)
     C                   EVAL      W#T6 = %DEC(%TIME():*HMS)
     C                   EVAL      W#REASON = DREASON
     C                   EVAL      W#TYPE = DTYPE
     C/EXEC SQL
     C+   VALUES NEXT VALUE FOR O2C_INVID_SEQ INTO :W#CNID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      DMSG = '赤伝の採番エラー'
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#CNNUM = 'CN-' + %CHAR(W#CNID)
     C                   EVAL      W#A1 = - W#SUB
     C                   EVAL      W#A2 = - W#TAX
     C                   EVAL      W#NEGA = - W#TOT
     C/EXEC SQL
     C+   INSERT INTO INVOICEHP (INVID, INVNUM, CUSTID, SOID, INVDATE,
     C+          DUEDATE, CURRCD, SUBTOT, TAXAMT, TOTAMT, PAIDAMT, BALANCE,
     C+          STATUS, CRTUSER, CRTDATE, CRTTIM, UPDUSER, UPDDATE, UPDTIM)
     C+   VALUES (:W#CNID, :W#CNNUM, :H#CUSTID, :H#SOID, :W#D8, :W#D8,
     C+          :H#CUR, :W#A1, :W#A2, :W#NEGA, 0, :W#NEGA, 'OPEN',
     C+          :W#USER, :W#D8, :W#T6, :W#USER, :W#D8, :W#T6)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      DMSG = '赤伝の登録エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
      * 明細(元の明細の品名を引き継ぐ)
     C                   EVAL      W#NEWLN = 0
     C                   FOR       W#I = 1 TO W#CNT
     C                   EVAL      W#RRN = W#I
     C     W#RRN         CHAIN     RSFL
     C                   IF        NOT %FOUND(RETENTD)
     C                   ITER
     C                   ENDIF
     C                   IF        DTYPE = '1'
     C                   IF        RQTY = 0
     C                   ITER
     C                   ENDIF
     C                   EVAL      W#Q = RQTY
     C                   EVAL(H)   W#LNAMT = RQTY * LPRCV
     C                   EVAL      W#PRCU = LPRCV
     C                   ELSE
     C                   IF        RAMT = 0
     C                   ITER
     C                   ENDIF
     C                   EVAL      W#Q = 0
     C                   EVAL      W#LNAMT = RAMT
     C                   EVAL      W#PRCU = 0
     C                   ENDIF
     C                   EVAL      W#NEWLN = W#NEWLN + 1
     C                   EVAL      W#NEGQ = - W#Q
     C                   EVAL      W#NEGA = - W#LNAMT
     C                   EVAL      S#LINE = LORGLN
     C                   EVAL      S#ITEMID = LITEMID
     C                   EVAL      S#TAXR = LTAXR
     C/EXEC SQL
     C+   INSERT INTO INVOICELP (INVID, LINENO, SOID, SHPID, ITEMID, DESC,
     C+          QTY, UNITPRC, TAXRATE, LINEAMT)
     C+   SELECT :W#CNID, :W#NEWLN, L.SOID, L.SHPID, L.ITEMID, L.DESC,
     C+          :W#NEGQ, :W#PRCU, L.TAXRATE, :W#NEGA
     C+     FROM INVOICELP L WHERE L.INVID = :H#INVID AND L.LINENO = :S#LINE
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      DMSG = '赤伝明細の登録エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
     C/EXEC SQL
     C+   VALUES NEXT VALUE FOR O2C_RETID_SEQ INTO :W#RETID
     C/END-EXEC
     C                   IF        DTYPE <> '1'
     C                   EVAL      W#WHID = 0
     C                   ENDIF
     C/EXEC SQL
     C+   INSERT INTO RETURNP (RETID, CNINVID, ORGINVID, ORGLINE, ITEMID,
     C+          RETTYPE, RETQTY, RETAMT, TAXRATE, WHID, RETDATE, REASON,
     C+          CRTUSER, CRTDATE, CRTTIM)
     C+   VALUES (:W#RETID, :W#CNID, :H#INVID, :S#LINE, :S#ITEMID, :W#TYPE,
     C+          :W#Q, :W#LNAMT, :S#TAXR, :W#WHID, :W#D8, :W#REASON,
     C+          :W#USER, :W#D8, :W#T6)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      DMSG = '返品明細の登録エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
      * 返品は在庫に戻す
     C                   IF        DTYPE = '1'
     C                   EXSR      @RESTOCK
     C                   IF        W#ERR
     C                   LEAVESR
     C                   ENDIF
     C                   ENDIF
     C                   ENDFOR
     C                   EXSR      @OFFSET
     C                   IF        W#ERR
     C                   LEAVESR
     C                   ENDIF
     C/EXEC SQL
     C+   COMMIT
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      DMSG = '確定(COMMIT)エラー'
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
      * 赤伝を印刷(失敗しても赤伝は作成済み)
     C                   MONITOR
     C                   CALLP     INVPRINT(W#CNNUM)
     C                   ON-ERROR
     C                   ENDMON
     C                   EVAL      DINV = *BLANKS
     C                   EXSR      @CLEAR
     C                   EVAL      DMSG = %TRIM(W#CNNUM) + ' を作成しました' +
     C                             '(相殺 ' + %TRIM(FMTAMT(W#APPTOT)) + ')'
     C                   ENDSR

      * @RESTOCK - 返品の在庫を戻し、在庫移動履歴に記録する
     C     @RESTOCK      BEGSR
     C/EXEC SQL
     C+   UPDATE INVENTORYP
     C+      SET QTYOH = QTYOH + :W#Q, QTYAVL = QTYAVL + :W#Q,
     C+          LSTUPD = CURRENT TIMESTAMP
     C+    WHERE WHID = :W#WHID AND ITEMID = :S#ITEMID
     C/END-EXEC
     C                   IF        SQLCODE = 100
     C/EXEC SQL
     C+   INSERT INTO INVENTORYP (WHID, ITEMID, QTYOH, QTYALC, QTYAVL, LSTUPD)
     C+   VALUES (:W#WHID, :S#ITEMID, :W#Q, 0, :W#Q, CURRENT TIMESTAMP)
     C/END-EXEC
     C                   ENDIF
     C                   IF        SQLCODE <> 0
     C                   EVAL      DMSG = '在庫の更新エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
     C/EXEC SQL
     C+   SELECT QTYOH INTO :W#AFT FROM INVENTORYP
     C+    WHERE WHID = :W#WHID AND ITEMID = :S#ITEMID
     C/END-EXEC
     C                   EVAL      W#BEF = W#AFT - W#Q
     C/EXEC SQL
     C+   INSERT INTO STKMOVEP (MOVID, WHID, ITEMID, MOVTYPE, MOVQTY, BEFQTY,
     C+          AFTQTY, REFNO, MOVDATE, CRTUSER, CRTDATE, CRTTIM)
     C+   VALUES (NEXT VALUE FOR O2C_MOVID_SEQ, :W#WHID, :S#ITEMID,
     C+          'RETURN', :W#Q, :W#BEF, :W#AFT, :W#CNNUM, :W#D8, :W#USER,
     C+          :W#D8, :W#T6)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      DMSG = '在庫移動履歴の登録エラー'
     C                   EXSR      @ROLLBK
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @OFFSET - 赤伝を未収の請求書と相殺(元の請求書から、古い順)
      *=====================================================================
     C     @OFFSET       BEGSR
     C                   EVAL      W#APPTOT = 0
     C                   EVAL      W#ON = 0
     C/EXEC SQL
     C+   DECLARE OFSCSR CURSOR FOR
     C+   SELECT INVID, BALANCE FROM INVOICEHP
     C+    WHERE CUSTID = :H#CUSTID AND CURRCD = :H#CUR AND BALANCE > 0
     C+      AND STATUS <> 'CANCELLED'
     C+    ORDER BY CASE WHEN INVID = :H#INVID THEN 0 ELSE 1 END,
     C+          INVDATE, INVID
     C+    FOR READ ONLY
     C/END-EXEC
     C/EXEC SQL
     C+   OPEN OFSCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#ON < 50
     C/EXEC SQL
     C+   FETCH OFSCSR INTO :W#IID, :H#BAL
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#ON = W#ON + 1
     C                   EVAL      W#OINV(W#ON) = W#IID
     C                   EVAL      W#OBAL(W#ON) = H#BAL
     C                   ENDDO
     C                   EVAL      W#SQLCD = SQLCODE
     C/EXEC SQL
     C+   CLOSE OFSCSR
     C/END-EXEC
     C                   IF        W#SQLCD < 0
     C                   EVAL      DMSG = '相殺先の読み込みエラー'
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
     C                   IF        W#ON = 0
     C                   LEAVESR
     C                   ENDIF
      * 相殺の記録用の入金(区分 CREDITNOTE、入金額0)
     C/EXEC SQL
     C+   VALUES NEXT VALUE FOR O2C_PAYID_SEQ INTO :W#PAYID
     C/END-EXEC
     C                   EVAL      W#PAYNUM = 'PAY-' + %CHAR(W#PAYID)
     C/EXEC SQL
     C+   INSERT INTO PAYMENTP (PAYID, PAYNUM, CUSTID, INVID, PAYDATE,
     C+          PAYMETH, CURRCD, AMOUNT, FXRATE, BASEAMT, REFNO, STATUS,
     C+          CRTUSER, CRTDATE, CRTTIM, UPDUSER, UPDDATE, UPDTIM)
     C+   VALUES (:W#PAYID, :W#PAYNUM, :H#CUSTID, :H#INVID, :W#D8,
     C+          'CREDITNOTE', :H#CUR, 0, 1, 0, :W#CNNUM, 'APPLIED',
     C+          :W#USER, :W#D8, :W#T6, :W#USER, :W#D8, :W#T6)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      DMSG = '相殺の記録エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#REM = W#TOT
     C                   FOR       W#K = 1 TO W#ON
     C                   IF        W#REM <= 0
     C                   LEAVE
     C                   ENDIF
     C                   IF        W#OBAL(W#K) < W#REM
     C                   EVAL      W#APP = W#OBAL(W#K)
     C                   ELSE
     C                   EVAL      W#APP = W#REM
     C                   ENDIF
     C                   EVAL      W#IID = W#OINV(W#K)
     C                   EXSR      @APPLY
     C                   IF        W#ERR
     C                   LEAVESR
     C                   ENDIF
     C/EXEC SQL
     C+   UPDATE INVOICEHP
     C+      SET PAIDAMT = PAIDAMT + :W#APP, BALANCE = BALANCE - :W#APP,
     C+          STATUS = CASE WHEN BALANCE - :W#APP <= 0 THEN 'PAID'
     C+                        ELSE 'PARTIAL' END,
     C+          UPDUSER = :W#USER, UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHERE INVID = :W#IID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      DMSG = '請求書の更新エラー'
     C                   EXSR      @ROLLBK
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#REM = W#REM - W#APP
     C                   EVAL      W#APPTOT = W#APPTOT + W#APP
     C                   ENDFOR
      * 赤伝側(マイナスの消込)
     C                   EVAL      W#APP = - W#APPTOT
     C                   EVAL      W#IID = W#CNID
     C                   EXSR      @APPLY
     C                   IF        W#ERR
     C                   LEAVESR
     C                   ENDIF
     C/EXEC SQL
     C+   UPDATE INVOICEHP
     C+      SET PAIDAMT = :W#APP, BALANCE = TOTAMT - :W#APP,
     C+          STATUS = CASE WHEN TOTAMT - :W#APP = 0 THEN 'PAID'
     C+                        ELSE 'PARTIAL' END
     C+    WHERE INVID = :W#CNID
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      DMSG = '赤伝の更新エラー'
     C                   EXSR      @ROLLBK
     C                   ENDIF
     C                   ENDSR

      * @APPLY - 消込明細(W#IID=請求ID、W#APP=金額)
     C     @APPLY        BEGSR
     C/EXEC SQL
     C+   INSERT INTO PAYAPPLYP (APPLYID, PAYID, INVID, APPLYAMT, APPLYDATE,
     C+          CRTUSER, CRTDATE, CRTTIM)
     C+   VALUES (NEXT VALUE FOR O2C_APPLYID_SEQ, :W#PAYID, :W#IID, :W#APP,
     C+          :W#D8, :W#USER, :W#D8, :W#T6)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      DMSG = '消込明細の登録エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EXSR      @ROLLBK
     C                   ENDIF
     C                   ENDSR

      * @ROLLBK - 失敗したら全て元に戻す
     C     @ROLLBK       BEGSR
     C                   EVAL      W#ERR = *ON
     C/EXEC SQL
     C+   ROLLBACK
     C/END-EXEC
     C                   ENDSR

      *=====================================================================
      * FMTAMT - 金額を右寄せの文字に(円は小数なし、外貨は小数2桁)
      *=====================================================================
     P FMTAMT          B
     D FMTAMT          PI            15A
     D   PAMT                        15P 2 CONST
     D WOUT            S             15A
     C                   MONITOR
     C                   IF        W#FRN
     C                   EVALR     WOUT = %EDITC(%DEC(PAMT:13:2):'J')
     C                   ELSE
     C                   EVALR     WOUT = %EDITC(%DECH(PAMT:13:0):'J')
     C                   ENDIF
     C                   ON-ERROR
     C                   EVAL      WOUT = *ALL'*'
     C                   ENDMON
     C                   RETURN    WOUT
     P FMTAMT          E

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
