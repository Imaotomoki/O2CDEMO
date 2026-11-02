     H DFTACTGRP(*NO) ACTGRP(*CALLER) DECEDIT('0.')
      *=====================================================================
      * プログラム名: INVINQ
      * 説明: 請求書照会(見出し・明細・税率別の消費税・入金済・残高)
      * 作成: 修正#INQ 新規作成。メニューの5番から呼ぶ
      *   パラメーター: 請求番号(空白なら入力画面から始める)
      *   F4=請求書の一覧(O2CPRMPT)  F6=印刷(INVPRINT)  F12=戻る
      *   金額は円なら小数なし、外貨なら小数2桁で表示する
      *   税率別の消費税は INVPRINT と同じ計算(円は税率ごとに切り捨て)
      *   修正#ICN F10=請求書の取消(2回押して実行。INVCANCEL)
      *            F11=取消済の請求書を出荷から作り直す(CRTINVCL)
      *   修正#PYH F7=入金履歴(INVPAYH)
      *=====================================================================
     FINVINQD   CF   E             WORKSTN SFILE(DSFL:W#RRN)
     FINVOICEHL1IF   E           K DISK
     FINVOICELP IF   E           K DISK    PREFIX(L_)
     FCUSTOMERP IF   E           K DISK    PREFIX(C_)
     FITEMP     IF   E           K DISK    PREFIX(I_)

     D P#INVNUM        S             20A
     D W#RRN           S              4S 0
     D W#CNT           S              4S 0
     D W#DONE          S               N
     D W#FRN           S               N
     D W#D8            S              8A
     D W#DAYS          S             10I 0
     D W#TODAY         S               D
     D W#TXRT          S              5P 3 DIM(10)
     D W#TXBASE        S             15P 2 DIM(10)
     D W#TXN           S             10I 0
     D W#K             S             10I 0
     D W#TXAMT         S             15P 2
     D W#TXT           S             76A
     D W#PVAL          S             30A
     D W#ARM           S               N
     D W#CMSG          S             78A
     D W#NEWINV        S             20A
     D W#SOID          S             18P 0
     D W#CUSTID        S              9P 0
     D INVCANCEL       PR                  EXTPGM('INVCANCEL')
     D   pAct                         1A   CONST
     D   pInvNum                     20A   CONST
     D   pMsg                        78A
     D   pNewInv                     20A
     D INVPAYH         PR                  EXTPGM('INVPAYH')
     D   pInvNum                     20A   CONST
     D CRTINVCL        PR                  EXTPGM('CRTINVCL')
     D   pSoid                       18P 0
     D   pCustId                      9P 0
     D O2CPRMPT        PR                  EXTPGM('O2CPRMPT')
     D   pType                       10A   CONST
     D   pCdType                     30A   CONST
     D   pValue                      30A
     D INVPRINT        PR                  EXTPGM('INVPRINT')
     D   pInvNum                     20A
     D FmtAmt          PR            15A
     D   pAmt                        15P 2 CONST
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
     D DbcsTrunc       PR           256A   VARYING
     D   pStr                       256A   VARYING CONST
     D   pMax                        10I 0 CONST

     C     *ENTRY        PLIST
     C                   PARM                    P#INVNUM

     C                   EVAL      QSINV = P#INVNUM
     C                   EVAL      QSMSG = *BLANKS
      * 請求番号を渡されたらすぐに照会画面を出す
     C                   IF        QSINV <> *BLANKS
     C                   EXSR      @SHOW
     C                   ENDIF
     C                   DOW       NOT W#DONE
     C                   EXFMT     SEL
     C                   EVAL      *IN31 = *OFF
     C                   EVAL      QSMSG = *BLANKS
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
      * F4=請求書の一覧(新しい順)から選ぶ
     C                   IF        *IN04
     C                   EVAL      W#PVAL = QSINV
     C                   CALLP     O2CPRMPT('INV':' ':W#PVAL)
     C                   IF        W#PVAL <> QSINV AND W#PVAL <> *BLANKS
     C                   EVAL      QSINV = W#PVAL
      * 一覧で選んだらそのまま照会画面を出す
     C                   EXSR      @SHOW
     C                   ENDIF
     C                   ITER
     C                   ENDIF
     C                   IF        QSINV = *BLANKS
     C                   EVAL      *IN31 = *ON
     C                   EVAL      QSMSG = '請求番号を入力してください'
     C                   ITER
     C                   ENDIF
     C                   EXSR      @SHOW
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C                   RETURN

      *=====================================================================
      * @SHOW - 請求書を読んで照会画面を表示する(F12で入力画面へ戻る)
      *=====================================================================
     C     @SHOW         BEGSR
     C                   EVAL      QSINV = %XLATE('abcdefghijklmnopqrstuvwxyz':
     C                             'ABCDEFGHIJKLMNOPQRSTUVWXYZ':QSINV)
     C     QSINV         CHAIN     INVOICEHL1
     C                   IF        NOT %FOUND(INVOICEHL1)
     C                   EVAL      *IN31 = *ON
     C                   EVAL      QSMSG = '請求書が見つかりません'
     C                   LEAVESR
     C                   ENDIF
     C                   EXSR      @FILL
     C                   EVAL      QDMSG = *BLANKS
     C                   EVAL      W#ARM = *OFF
     C                   DOW       *ON
     C                   WRITE     DFOOT
     C                   EVAL      *IN92 = *ON
     C                   EVAL      *IN91 = (W#CNT > 0)
     C                   EVAL      *IN94 = *ON
     C                   EXFMT     DCTL
     C                   EVAL      QDMSG = *BLANKS
      * F10 以外のキーを押したら取消の確認を解除
     C                   IF        NOT *IN10
     C                   EVAL      W#ARM = *OFF
     C                   ENDIF
     C                   IF        *IN03
     C                   EVAL      W#DONE = *ON
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN12
     C                   LEAVE
     C                   ENDIF
      * F6=請求書を印刷(スプール INVPRTF)
     C                   IF        *IN06 AND STATUS = 'CANCELLED'
     C                   EVAL      QDMSG = '取消済の請求書は印刷できません'
     C                   ITER
     C                   ENDIF
     C                   IF        *IN06
     C                   MONITOR
     C                   CALLP     INVPRINT(INVNUM)
     C                   EVAL      QDMSG = '印刷しました(INVPRTF)'
     C                   ON-ERROR
     C                   EVAL      QDMSG = '印刷できませんでした'
     C                   ENDMON
     C                   ENDIF
      * F7=入金履歴
     C                   IF        *IN07
     C                   CALLP     INVPAYH(INVNUM)
     C                   ENDIF
     C                   IF        *IN10
     C                   EXSR      @CANCEL
     C                   ENDIF
     C                   IF        *IN11
     C                   EXSR      @REISSUE
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR

      * @FILL - 読んだ請求書で画面の項目を作る
     C     @FILL         BEGSR
     C                   EVAL      W#FRN = (CURRCD <> 'JPY' AND
     C                             CURRCD <> *BLANKS)
     C                   EXSR      @HEAD
     C                   EXSR      @LINES
     C                   EXSR      @FOOT
     C                   ENDSR

      *=====================================================================
      * @CANCEL - F10=請求書の取消(1回目は確認のメッセージだけ)
      *=====================================================================
     C     @CANCEL       BEGSR
     C                   IF        STATUS = 'CANCELLED'
     C                   EVAL      QDMSG = 'すでに取り消し済みです'
     C                   LEAVESR
     C                   ENDIF
     C                   IF        NOT W#ARM
     C                   EVAL      W#ARM = *ON
     C                   EVAL      QDMSG = 'もう一度F10で取消' +
     C                             '(出荷は未請求に戻ります)'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#ARM = *OFF
     C                   CALLP     INVCANCEL('C':INVNUM:W#CMSG:W#NEWINV)
     C                   IF        W#CMSG <> *BLANKS
     C                   EVAL      QDMSG = W#CMSG
     C                   LEAVESR
     C                   ENDIF
     C     QSINV         CHAIN     INVOICEHL1
     C                   EXSR      @FILL
     C                   EVAL      QDMSG = '取り消しました。' +
     C                             'F11で作り直せます'
     C                   ENDSR

      *=====================================================================
      * @REISSUE - F11=取消済の請求書を出荷から作り直す(印刷もされる)
      *=====================================================================
     C     @REISSUE      BEGSR
     C                   IF        STATUS <> 'CANCELLED'
     C                   EVAL      QDMSG = '取消済の請求書だけ作り直せます'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#SOID = SOID
     C                   EVAL      W#CUSTID = CUSTID
     C                   MONITOR
     C                   CALLP     CRTINVCL(W#SOID:W#CUSTID)
     C                   ON-ERROR
     C                   EVAL      QDMSG = '作り直せませんでした' +
     C                             '(請求する出荷がない等)'
     C                   LEAVESR
     C                   ENDMON
     C                   CALLP     INVCANCEL('L':INVNUM:W#CMSG:W#NEWINV)
     C                   IF        W#CMSG <> *BLANKS
     C                   EVAL      QDMSG = W#CMSG
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      QSINV = W#NEWINV
     C     QSINV         CHAIN     INVOICEHL1
     C                   EXSR      @FILL
     C                   EVAL      QDMSG = '作り直して印刷しました: ' +
     C                             %TRIM(QSINV)
     C                   ENDSR

      *=====================================================================
      * @HEAD - 見出し(請求番号・状態・顧客・日付・通貨・期限超過)
      *=====================================================================
     C     @HEAD         BEGSR
     C                   EVAL      QINV = INVNUM
     C                   SELECT
     C                   WHEN      STATUS = 'OPEN'
     C                   EVAL      QSTS = '未入金'
     C                   WHEN      STATUS = 'PARTIAL'
     C                   EVAL      QSTS = '一部入金'
     C                   WHEN      STATUS = 'PAID'
     C                   EVAL      QSTS = '入金済'
     C                   WHEN      STATUS = 'CANCELLED'
     C                   EVAL      QSTS = '取消済'
     C                   OTHER
     C                   EVAL      QSTS = STATUS
     C                   ENDSL
     C     CUSTID        CHAIN     CUSTOMERP
     C                   IF        %FOUND(CUSTOMERP)
     C                   EVAL      QCUSTCD = C_CUSTCD
     C                   EVAL      QCUSTNM = DbcsTrunc(%TRIMR(C_CUSTNM):38)
     C                   ELSE
     C                   EVAL      QCUSTCD = *BLANKS
     C                   EVAL      QCUSTNM = *BLANKS
     C                   ENDIF
     C                   EVAL      W#D8 = %EDITC(INVDATE:'X')
     C                   EVAL      QINVDT = %SUBST(W#D8:1:4) + '/' +
     C                             %SUBST(W#D8:5:2) + '/' + %SUBST(W#D8:7:2)
     C                   EVAL      W#D8 = %EDITC(DUEDATE:'X')
     C                   EVAL      QDUEDT = %SUBST(W#D8:1:4) + '/' +
     C                             %SUBST(W#D8:5:2) + '/' + %SUBST(W#D8:7:2)
     C                   EVAL      QCUR = CURRCD
      * 未収があって支払期限を過ぎていれば日数を表示
     C                   EVAL      QOVER = *BLANKS
     C                   EVAL      W#TODAY = %DATE()
     C                   IF        BALANCE > 0 AND DUEDATE > 0
     C     *ISO0         TEST(DE)                W#D8
     C                   IF        NOT %ERROR
     C                   EVAL      W#DAYS = %DIFF(W#TODAY:%DATE(W#D8:*ISO0):
     C                             *DAYS)
     C                   IF        W#DAYS > 0
     C                   EVAL      QOVER = '支払期限を' + %CHAR(W#DAYS) +
     C                             '日過ぎています'
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @LINES - 明細をサブファイルに読み込み、税率別に集計する
      *=====================================================================
     C     @LINES        BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     DCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#CNT = 0
     C                   EVAL      W#TXN = 0
     C     INVID         SETLL     INVLREC
     C                   DOW       *ON
     C     INVID         READE     INVLREC
     C                   IF        %EOF(INVOICELP)
     C                   LEAVE
     C                   ENDIF
     C                   EVALR     QLNO = %CHAR(L_LINENO)
     C     L_ITEMID      CHAIN     ITEMP
     C                   IF        %FOUND(ITEMP)
     C                   EVAL      QITEM = I_ITEMCD
     C                   ELSE
     C                   EVAL      QITEM = *BLANKS
     C                   ENDIF
     C                   EVAL      QDESC = DbcsTrunc(%TRIMR(L_DESC):19)
      * 修正#OVF 桁に収まらない金額・数量は*で表示(異常終了していた)
     C                   MONITOR
     C                   EVALR     QQTY = %EDITC(%DECH(L_QTY:6:0):'1')
     C                   ON-ERROR
     C                   EVAL      QQTY = *ALL'*'
     C                   ENDMON
     C                   IF        W#FRN
     C                   MONITOR
     C                   EVALR     QUPRC = %EDITC(%DECH(L_UNITPRC:9:2):'1')
     C                   ON-ERROR
     C                   EVAL      QUPRC = *ALL'*'
     C                   ENDMON
     C                   MONITOR
     C                   EVALR     QLAMT = %EDITC(%DECH(L_LINEAMT:11:2):'1')
     C                   ON-ERROR
     C                   EVAL      QLAMT = *ALL'*'
     C                   ENDMON
     C                   ELSE
     C                   MONITOR
     C                   EVALR     QUPRC = %EDITC(%DECH(L_UNITPRC:9:0):'1')
     C                   ON-ERROR
     C                   EVAL      QUPRC = *ALL'*'
     C                   ENDMON
     C                   MONITOR
     C                   EVALR     QLAMT = %EDITC(%DECH(L_LINEAMT:11:0):'1')
     C                   ON-ERROR
     C                   EVAL      QLAMT = *ALL'*'
     C                   ENDMON
     C                   ENDIF
     C                   EVALR     QLTAX = %CHAR(%DECH(L_TAXRATE * 100:3:0))
     C                             + '%'
     C                   EVAL      W#CNT = W#CNT + 1
     C                   EVAL      W#RRN = W#CNT
     C                   WRITE     DSFL
      * 税率別に税抜金額を集計
     C                   EVAL      W#K = 1
     C                   DOW       W#K <= W#TXN
     C                   IF        W#TXRT(W#K) = L_TAXRATE
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#K = W#K + 1
     C                   ENDDO
     C                   IF        W#K > W#TXN AND W#TXN < 10
     C                   EVAL      W#TXN = W#TXN + 1
     C                   EVAL      W#K = W#TXN
     C                   EVAL      W#TXRT(W#K) = L_TAXRATE
     C                   EVAL      W#TXBASE(W#K) = 0
     C                   ENDIF
     C                   IF        W#K <= W#TXN
     C                   EVAL      W#TXBASE(W#K) = W#TXBASE(W#K) + L_LINEAMT
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR

      *=====================================================================
      * @FOOT - 税率別の対象額・消費税額(2税率まで)と合計・入金・残高
      *=====================================================================
     C     @FOOT         BEGSR
     C                   EVAL      QTX1 = *BLANKS
     C                   EVAL      QTX2 = *BLANKS
     C                   EVAL      W#K = 1
     C                   DOW       W#K <= W#TXN AND W#K <= 2
     C                   IF        W#FRN
     C                   EVAL      W#TXAMT = W#TXBASE(W#K) * W#TXRT(W#K)
     C                   ELSE
     C                   EVAL      W#TXAMT = %INT(W#TXBASE(W#K) * W#TXRT(W#K))
     C                   ENDIF
     C                   EVAL      W#TXT = '税率' +
     C                             %CHAR(%DECH(W#TXRT(W#K) * 100:3:0)) + '%' +
     C                             ' 対象額' + FmtAmt(W#TXBASE(W#K)) +
     C                             '   消費税額' + FmtAmt(W#TXAMT)
     C                   IF        W#K = 1
     C                   EVAL      QTX1 = W#TXT
     C                   ELSE
     C                   EVAL      QTX2 = W#TXT
     C                   ENDIF
     C                   EVAL      W#K = W#K + 1
     C                   ENDDO
     C                   EVAL      QSUB = FmtAmt(SUBTOT)
     C                   EVAL      QTAX = FmtAmt(TAXAMT)
     C                   EVAL      QTOT = FmtAmt(TOTAMT)
     C                   EVAL      QPAID = FmtAmt(PAIDAMT)
     C                   EVAL      QBAL = FmtAmt(BALANCE)
     C                   EVAL      *IN61 = (BALANCE > 0)
     C                   ENDSR

      * FmtAmt - 金額を右寄せの文字に(円は小数なし、外貨は小数2桁)
     P FmtAmt          B
     D FmtAmt          PI            15A
     D   pAmt                        15P 2 CONST
     D wOut            S             15A
     C                   IF        W#FRN
     C                   MONITOR
     C                   EVALR     wOut = %EDITC(%DECH(pAmt:11:2):'J')
     C                   ON-ERROR
     C                   EVAL      wOut = *ALL'*'
     C                   ENDMON
     C                   ELSE
     C                   MONITOR
     C                   EVALR     wOut = %EDITC(%DECH(pAmt:11:0):'J')
     C                   ON-ERROR
     C                   EVAL      wOut = *ALL'*'
     C                   ENDMON
     C                   ENDIF
     C                   RETURN    wOut
     P FmtAmt          E

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
