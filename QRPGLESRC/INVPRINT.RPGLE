     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: INVPRINT
      * 説明: 請求書印刷(請求番号を指定して1枚出力)
      * 作成: 修正#PRT 新規作成
      *   - CREATEINVから請求書発行時に自動で呼ばれる
      *   - 再印刷: CALL INVPRINT PARM('INV-23')
      *   - 税率別の対象額と消費税額を出力(インボイス制度)
      *   修正#CUR 外貨の請求書は金額を小数2桁で印字(50=外貨)
      *   修正#INV2 適格請求書: 発行者と登録番号(COMPANY)を印字。
      *   修正#RET 赤伝(マイナス)は表題を変え、金額にマイナスを付ける
      *            軽減税率(8%)の明細に※を付ける
      *            税率0%も0と印字する
      *=====================================================================
     FINVPRTF   O    E             PRINTER OFLIND(*IN90)
     FINVOICEHL1IF   E           K DISK
     FINVOICELP IF   E           K DISK    PREFIX(L_)
     FCUSTOMERP IF   E           K DISK    PREFIX(C_)
     FITEMP     IF   E           K DISK    PREFIX(I_)
      * 修正#INV2 自社情報(COMPANY: NAME=社名 REGNO=登録番号)
     FCODEMSTRP IF   E           K DISK    PREFIX(CD_)

     D P#INVNUM        S             20A
     D W#INVNUM        S             20A
     D W#TXRT          S              5P 3 DIM(10)
     D W#TXBASE        S             15P 2 DIM(10)
     D W#TXN           S             10I 0
     D W#K             S             10I 0
     D W#D8            S              8A
      * 修正#CUR: 税率別の消費税額(外貨は小数2桁で切り捨て)
     D W#TXAMT         S             15P 2
     D W#CDTYPE        S             30A
     D W#CDVAL         S             30A
     D W#ESCMSG        S             52A
     D W#MSGKEY        S              4A
     D W#APIERR        DS
     D   W#APIPRV                    10I 0 INZ(0)
     D   W#APIAVL                    10I 0 INZ(0)
     D QMHSNDPM        PR                  EXTPGM('QMHSNDPM')
     D   MsgId                        7A   CONST
     D   MsgFile                     20A   CONST
     D   MsgDta                      52A   CONST
     D   MsgDtaLen                   10I 0 CONST
     D   MsgType                     10A   CONST
     D   CallStk                     10A   CONST
     D   CallStkCtr                  10I 0 CONST
     D   MsgKey                       4A
     D   ErrCode                      8A
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
     D DbcsTrunc       PR           256A   VARYING
     D   pStr                       256A   VARYING CONST
     D   pMax                        10I 0 CONST

     C     CDKEY         KLIST
     C                   KFLD                    W#CDTYPE
     C                   KFLD                    W#CDVAL
     C     *ENTRY        PLIST
     C                   PARM                    P#INVNUM

     C                   EVAL      W#INVNUM = P#INVNUM
     C     W#INVNUM      CHAIN     INVOICEHL1
     C                   IF        NOT %FOUND(INVOICEHL1)
     C                   EVAL      W#ESCMSG = '請求書が見つかりません: ' +
     C                             %TRIM(W#INVNUM)
     C                   EVAL      *INLR = *ON
     C                   CALLP     QMHSNDPM('CPF9898':'QCPFMSG   *LIBL':
     C                             W#ESCMSG:%LEN(%TRIMR(W#ESCMSG)):
     C                             '*ESCAPE':'*PGMBDY':1:W#MSGKEY:W#APIERR)
     C                   ENDIF
      * 修正#CNL 取消済の請求書は印刷しない(再印刷のCALLで出ていた)
     C                   IF        STATUS = 'CANCELLED'
     C                   EVAL      W#ESCMSG = '取消済の請求書です: ' +
     C                             %TRIM(W#INVNUM)
     C                   EVAL      *INLR = *ON
     C                   CALLP     QMHSNDPM('CPF9898':'QCPFMSG   *LIBL':
     C                             W#ESCMSG:%LEN(%TRIMR(W#ESCMSG)):
     C                             '*ESCAPE':'*PGMBDY':1:W#MSGKEY:W#APIERR)
     C                   ENDIF

      * 修正#CUR: 外貨は金額を小数2桁の文字で印字する
     C                   EVAL      *IN50 = (CURRCD <> 'JPY' AND
     C                             CURRCD <> *BLANKS)
      * 修正#RET 赤伝(合計がマイナス)は表題を変える
     C                   EVAL      *IN52 = (TOTAMT < 0)
      * 見出し(顧客・請求番号・日付)
     C     CUSTID        CHAIN     CUSTOMERP
     C                   IF        %FOUND(CUSTOMERP)
     C                   EVAL      PCUSTNM = C_CUSTNM
     C                   EVAL      PBILLADR = C_BILLADR
     C                   ELSE
     C                   EVAL      PCUSTNM = *BLANKS
     C                   EVAL      PBILLADR = *BLANKS
     C                   ENDIF
     C                   EVAL      PINVNUM = INVNUM
     C                   EVAL      W#D8 = %EDITC(INVDATE:'X')
     C                   EVAL      PINVDT = %SUBST(W#D8:1:4) + '/' +
     C                             %SUBST(W#D8:5:2) + '/' + %SUBST(W#D8:7:2)
     C                   EVAL      W#D8 = %EDITC(DUEDATE:'X')
     C                   EVAL      PDUEDT = %SUBST(W#D8:1:4) + '/' +
     C                             %SUBST(W#D8:5:2) + '/' + %SUBST(W#D8:7:2)
     C                   EVAL      PCURRCD = CURRCD
      * 修正#INV2 適格請求書: 発行者(社名)と登録番号
     C                   EVAL      W#CDTYPE = 'COMPANY'
     C                   EVAL      W#CDVAL = 'NAME'
     C     CDKEY         CHAIN     CODEREC
     C                   IF        %FOUND(CODEMSTRP)
     C                   EVAL      PCOMPNM = DbcsTrunc(CD_DESC:40)
     C                   ELSE
     C                   EVAL      PCOMPNM = *BLANKS
     C                   ENDIF
     C                   EVAL      W#CDVAL = 'REGNO'
     C     CDKEY         CHAIN     CODEREC
     C                   IF        %FOUND(CODEMSTRP)
     C                   EVAL      PREGNO = CD_DESC
     C                   ELSE
     C                   EVAL      PREGNO = '(未登録)'
     C                   ENDIF
     C                   WRITE     PHEAD

      * 明細
     C                   EVAL      W#TXN = 0
     C     INVID         SETLL     INVLREC
     C                   DOW       *ON
     C     INVID         READE     INVLREC
     C                   IF        %EOF(INVOICELP)
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN90
     C                   WRITE     PHEAD
     C                   EVAL      *IN90 = *OFF
     C                   ENDIF
     C                   EVAL      PLINENO = L_LINENO
     C     L_ITEMID      CHAIN     ITEMP
     C                   IF        %FOUND(ITEMP)
     C                   EVAL      PITEMCD = I_ITEMCD
     C                   ELSE
     C                   EVAL      PITEMCD = *BLANKS
     C                   ENDIF
     C                   EVAL      PDESC = DbcsTrunc(L_DESC:40)
     C                   EVAL(H)   PQTY = L_QTY
     C                   EVAL(H)   PUNITPRC = L_UNITPRC
     C                   EVAL(H)   PLINEAMT = L_LINEAMT
      * 修正#OVF 桁に収まらない金額・数量は*で表示(異常終了していた)
     C                   MONITOR
     C                   EVALR     PUNITPRCF = %EDITC(%DEC(L_UNITPRC:11:2):'J')
     C                   ON-ERROR
     C                   EVAL      PUNITPRCF = *ALL'*'
     C                   ENDMON
     C                   MONITOR
     C                   EVALR     PLINEAMTF = %EDITC(%DEC(L_LINEAMT:13:2):'J')
     C                   ON-ERROR
     C                   EVAL      PLINEAMTF = *ALL'*'
     C                   ENDMON
     C                   EVAL(H)   PTAXPCT = L_TAXRATE * 100
      * 修正#INV2 軽減税率(8%)の明細に※
     C                   EVAL      *IN51 = (L_TAXRATE = 0.080)
     C                   WRITE     PDETAIL
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

      * 税率別の対象額・消費税額(税率ごとに1回切り捨て)
     C                   EVAL      W#K = 1
     C                   DOW       W#K <= W#TXN
     C                   EVAL(H)   PTXPCT = W#TXRT(W#K) * 100
     C                   EVAL(H)   PTXBASE = W#TXBASE(W#K)
     C                   IF        *IN50
     C                   EVAL      W#TXAMT = W#TXBASE(W#K) * W#TXRT(W#K)
     C                   ELSE
     C                   EVAL      W#TXAMT = %INT(W#TXBASE(W#K) * W#TXRT(W#K))
     C                   ENDIF
     C                   EVAL      PTXAMT = W#TXAMT
     C                   EVALR     PTXBASEF = %EDITC(%DEC(W#TXBASE(W#K):
     C                             13:2):'J')
     C                   MONITOR
     C                   EVALR     PTXAMTF = %EDITC(%DEC(W#TXAMT:13:2):'J')
     C                   ON-ERROR
     C                   EVAL      PTXAMTF = *ALL'*'
     C                   ENDMON
     C                   WRITE     PTAXL
     C                   EVAL      W#K = W#K + 1
     C                   ENDDO

      * 合計(請求ヘッダの金額)
     C                   EVAL(H)   PSUBTOT = SUBTOT
     C                   EVAL(H)   PTAXAMT = TAXAMT
     C                   EVAL(H)   PTOTAMT = TOTAMT
     C                   MONITOR
     C                   EVALR     PSUBTOTF = %EDITC(%DEC(SUBTOT:13:2):'J')
     C                   ON-ERROR
     C                   EVAL      PSUBTOTF = *ALL'*'
     C                   ENDMON
     C                   MONITOR
     C                   EVALR     PTAXAMTF = %EDITC(%DEC(TAXAMT:13:2):'J')
     C                   ON-ERROR
     C                   EVAL      PTAXAMTF = *ALL'*'
     C                   ENDMON
     C                   MONITOR
     C                   EVALR     PTOTAMTF = %EDITC(%DEC(TOTAMT:13:2):'J')
     C                   ON-ERROR
     C                   EVAL      PTOTAMTF = *ALL'*'
     C                   ENDMON
     C                   WRITE     PTOTAL

     C                   EVAL      *INLR = *ON
     C                   RETURN

      *===============================================================
      * 修正#B DbcsTrunc - 全角(SO/SI)を壊さずにpMaxバイトへ切り詰め
      *   %SUBSTのバイト切りでは全角文字の途中で切れて文字化けする
      *===============================================================
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
