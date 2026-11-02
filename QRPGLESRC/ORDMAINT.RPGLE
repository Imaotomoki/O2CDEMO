     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: ORDMAINT
      * 説明: 受注の照会・変更・取消
      * 作成: 修正#MNT 新規作成
      *   5=照会  明細を表示
      *   2=変更  明細の受注数量を変更(増加は在庫を追加引当、減少は戻す)
      *           出荷済数量より少なくはできない。合計金額を再計算
      *   4=取消  未出荷の受注を取消し、在庫の引当を全て戻す
      *   変更・取消はコミットメント制御で一括確定(失敗時は全て取消)
      * 修正#PRC 金額・単価を外貨は小数2桁で表示(円は小数なし)
      * 修正#UPC 変更モードで単価も変更できる(外貨は小数2桁まで)
      *          請求済の出荷がある受注は単価を変えられない
      *          (請求書照会のF10で取り消してから直し、F11で作り直す)
      * 修正#OFL 一覧を顧客・状態で絞り込み(F4=顧客検索)。新しい順に表示
      *=====================================================================
     FORDMNTD   CF   E             WORKSTN SFILE(LSTSFL:W#RRN)
     F                                     SFILE(DTLSFL:W#DRRN)
     FSORDERHP  UF   E           K DISK    COMMIT USROPN
     FSORDERLP  UF   E           K DISK    COMMIT USROPN PREFIX(L_)
     F                                     RENAME(SOLREC:LREC)
     FINVENTORYPUF   E           K DISK    COMMIT USROPN PREFIX(V_)
     F                                     RENAME(INVREC:VREC)
     FSHIPMENHL1IF   E           K DISK    PREFIX(S_)
     F                                     RENAME(SHPREC:SREC)
     FCUSTOMERP IF   E           K DISK    PREFIX(C_)
     F                                     RENAME(CUSTREC:CREC)
     FITEMP     IF   E           K DISK    PREFIX(I_)
     FCODEMSTRP IF   E           K DISK    PREFIX(CD_)

     D W#RRN           S              4S 0
     D W#DRRN          S              4S 0
     D W#DCNT          S             10I 0
     D W#I             S             10I 0
     D W#SOID          S             18P 0
     D W#WHID          S              4P 0
     D W#ITEMID        S              9P 0
     D W#LINE          S              9P 0
     D W#MODE          S              1A
     D W#STAT          S             12A
     D W#MSG           S             75A
     D W#ERR           S               N
     D W#DONE          S               N
      * F6の押下状態(サブファイルのCHAINで*IN06が上書きされるため退避)
     D W#F6            S               N
     D W#DELTA         S             15P 3
      * 修正#UPC: 単価の変更
     D W#NEWP          S             15P 4
     D W#PCHG          S               N
     D W#PERR          S               N
     D W#PMSG          S             50A
     D W#PRCS          S             20A   VARYING
     D W#INVD          S               N
     D W#TOT           S             15P 2
     D W#ALLDONE       S               N
     D W#ANYSHP        S               N
     D W#UNBILL        S               N
     D W#CDTYPE        S             30A
     D W#CDVAL         S             30A
     D W#CMD           S            200A
     D W#PVAL          S             30A
     D W#FLEN          S             10I 0
     D O2CPRMPT        PR                  EXTPGM('O2CPRMPT')
     D   pType                       10A   CONST
     D   pCdType                     30A   CONST
     D   pValue                      30A
     D QCMDEXC         PR                  EXTPGM('QCMDEXC')
     D   Cmd                        200A   CONST OPTIONS(*VARSIZE)
     D   CmdLen                      15P 5 CONST
     D PSDS           SDS
     D   P#USER              254    263A
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
     D DbcsTrunc       PR           256A   VARYING
     D   pStr                       256A   VARYING CONST
     D   pMax                        10I 0 CONST

     C     LKEY          KLIST
     C                   KFLD                    W#SOID
     C                   KFLD                    W#LINE
     C     VKEY          KLIST
     C                   KFLD                    W#WHID
     C                   KFLD                    W#ITEMID
     C     CDKEY         KLIST
     C                   KFLD                    W#CDTYPE
     C                   KFLD                    W#CDVAL

      * コミットメント制御を開始してから更新ファイルを開く
     C                   EVAL      W#CMD = 'STRCMTCTL LCKLVL(*CHG) ' +
     C                             'CMTSCOPE(*ACTGRP)'
     C                   MONITOR
     C                   CALLP     QCMDEXC(W#CMD:%LEN(%TRIMR(W#CMD)))
     C                   ON-ERROR
     C                   ENDMON
     C                   OPEN      SORDERHP
     C                   OPEN      SORDERLP
     C                   OPEN      INVENTORYP

     C                   EVAL      W#MSG = *BLANKS
     C                   EXSR      @LOADLST
     C                   DOW       *ON
     C                   EVAL      LMSG = W#MSG
     C                   EVAL      W#MSG = *BLANKS
     C                   EVAL      *IN92 = *ON
     C                   EVAL      *IN91 = (W#RRN > 0)
     C                   EVAL      *IN94 = *ON
     C                   EXFMT     LSTCTL
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN05
     C                   EXSR      @LOADLST
     C                   ITER
     C                   ENDIF
      * 修正#OFL: F4=顧客の検索
     C                   IF        *IN04
     C                   EVAL      W#PVAL = LFCUST
     C                   CALLP     O2CPRMPT('CUST':' ':W#PVAL)
     C                   EVAL      LFCUST = W#PVAL
     C                   EXSR      @LOADLST
     C                   ITER
     C                   ENDIF
      * 選択された受注を1件ずつ処理
     C                   EVAL      W#DONE = *OFF
     C                   IF        W#RRN > 0
     C                   READC     LSTSFL
     C                   DOW       NOT %EOF(ORDMNTD)
     C                   IF        LOPT <> ' '
     C                   EVAL      W#DONE = *ON
     C                   EVAL      W#SOID = LSOID
     C                   EVAL      W#STAT = LSTSCD
     C                   SELECT
     C                   WHEN      LOPT = '5'
     C                   EVAL      W#MODE = 'V'
     C                   EXSR      @DETAIL
     C                   WHEN      LOPT = '2'
      * 修正#UPC: 出荷済(請求書を取り消した受注)も変更できる
     C                   IF        W#STAT = 'OPEN' OR W#STAT = 'ALLOCATED'
     C                             OR W#STAT = 'PARTIAL' OR W#STAT = 'SHIPPED'
     C                   EVAL      W#MODE = 'C'
     C                   EXSR      @DETAIL
     C                   ELSE
     C                   EVAL      W#MSG = %TRIM(LSONUM) +
     C                             ' はこの状態では変更できません'
     C                   ENDIF
     C                   WHEN      LOPT = '4'
     C                   IF        W#STAT = 'OPEN' OR W#STAT = 'ALLOCATED'
     C                   EVAL      W#MODE = 'X'
     C                   EXSR      @DETAIL
     C                   ELSE
     C                   EVAL      W#MSG = %TRIM(LSONUM) +
     C                             ' は出荷済のため取消できません'
     C                   ENDIF
     C                   ENDSL
     C                   ENDIF
     C                   READC     LSTSFL
     C                   ENDDO
     C                   ENDIF
      * 修正#OFL: 選択がなくても絞込条件で読み直す(Enter=検索)
     C                   EXSR      @LOADLST
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C                   RETURN

      *=====================================================================
      * @LOADLST - 受注一覧の読込(新しい順。顧客・状態で絞り込み)
      *=====================================================================
     C     @LOADLST      BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     LSTCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#RRN = 0
     C                   EVAL      LFCUST = %XLATE('abcdefghijklmnopqrstuvwxyz':
     C                             'ABCDEFGHIJKLMNOPQRSTUVWXYZ':LFCUST)
     C                   EVAL      W#FLEN = %LEN(%TRIM(LFCUST))
     C     *HIVAL        SETGT     SOHREC
     C                   DOW       W#RRN < 9999
     C                   READP(N)  SOHREC
     C                   IF        %EOF(SORDERHP)
     C                   LEAVE
     C                   ENDIF
      * 状態で絞り込み(1=未完了 2=完了)
     C                   IF        LFSTS = '1' AND
     C                             (STATUS = 'INVOICED' OR STATUS = 'CANCELLED')
     C                   ITER
     C                   ENDIF
     C                   IF        LFSTS = '2' AND
     C                             STATUS <> 'INVOICED' AND
     C                             STATUS <> 'CANCELLED'
     C                   ITER
     C                   ENDIF
     C     CUSTID        CHAIN     CUSTOMERP
      * 顧客コードの前方一致で絞り込み
     C                   IF        W#FLEN > 0
     C                   IF        NOT %FOUND(CUSTOMERP)
     C                   ITER
     C                   ENDIF
     C                   IF        %SUBST(C_CUSTCD:1:W#FLEN) <> %TRIM(LFCUST)
     C                   ITER
     C                   ENDIF
     C                   ENDIF
     C                   EVAL      W#RRN = W#RRN + 1
     C                   EVAL      LOPT = ' '
     C                   EVAL      LSONUM = SONUM
     C                   EVAL      LODATE = ORDDATE
     C     CUSTID        CHAIN     CUSTOMERP
     C                   IF        %FOUND(CUSTOMERP)
     C                   EVAL      LCUSTNM = DbcsTrunc(C_CUSTNM:16)
     C                   ELSE
     C                   EVAL      LCUSTNM = *BLANKS
     C                   ENDIF
      * 修正#PRC: 円は小数なし、外貨は小数2桁
     C                   IF        CURRCD = 'JPY' OR CURRCD = *BLANKS
      * 修正#OVF 桁に収まらない金額・数量は*で表示(異常終了していた)
     C                   MONITOR
     C                   EVALR     LTOTAMT = %EDITC(%DECH(TOTAMT:11:0):'1')
     C                   ON-ERROR
     C                   EVAL      LTOTAMT = *ALL'*'
     C                   ENDMON
     C                   ELSE
     C                   MONITOR
     C                   EVALR     LTOTAMT = %EDITC(%DECH(TOTAMT:11:2):'1')
     C                   ON-ERROR
     C                   EVAL      LTOTAMT = *ALL'*'
     C                   ENDMON
     C                   ENDIF
     C                   EVAL      W#CDVAL = STATUS
     C                   EXSR      @STATNM
     C                   EVAL      LSTAT = W#CDVAL
     C                   EVAL      LSOID = SOID
     C                   EVAL      LSTSCD = STATUS
     C                   WRITE     LSTSFL
     C                   ENDDO
     C                   IF        W#RRN = 0 AND W#MSG = *BLANKS
     C                   EVAL      W#MSG = '該当する受注がありません'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @STATNM - 状態コード(W#CDVAL)を日本語名称に置き換える
      *=====================================================================
     C     @STATNM       BEGSR
     C                   EVAL      W#CDTYPE = 'SO_STATUS'
     C     CDKEY         CHAIN     CODEMSTRP
     C                   IF        %FOUND(CODEMSTRP)
     C                   EVAL      W#CDVAL = DbcsTrunc(CD_DESC:12)
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @DETAIL - 受注明細の照会(V)/変更(C)/取消確認(X)
      *=====================================================================
     C     @DETAIL       BEGSR
     C     W#SOID        CHAIN(N)  SOHREC
     C                   IF        NOT %FOUND(SORDERHP)
     C                   EVAL      W#MSG = '受注が見つかりません'
     C                   LEAVESR
     C                   ENDIF
      * サブファイル行の属性は書込時の標識で決まるため、先にモードを設定
     C                   SELECT
     C                   WHEN      W#MODE = 'V'
     C                   EVAL      DTITLE = '受注照会'
     C                   EVAL      DKEYS = 'F12=戻る'
     C                   EVAL      *IN87 = *OFF
     C                   WHEN      W#MODE = 'C'
     C                   EVAL      DTITLE = '受注変更'
     C                   EVAL      DKEYS = '数量・単価を変更しF6=確定 ' +
     C                             'Enter=チェック  F12=戻る'
     C                   EVAL      *IN87 = *ON
     C                   OTHER
     C                   EVAL      DTITLE = '受注取消の確認'
     C                   EVAL      DKEYS = 'この受注を取り消します。' +
     C                             'F6=取消実行  F12=戻る'
     C                   EVAL      *IN87 = *OFF
     C                   ENDSL
     C                   EXSR      @CHKINVD
     C                   EXSR      @LOADDTL
     C                   EVAL      DMSG = *BLANKS
     C                   DOW       *ON
     C                   EVAL      *IN82 = *ON
     C                   EVAL      *IN81 = (W#DCNT > 0)
     C                   EVAL      *IN84 = *ON
     C                   EXFMT     DTLCTL
     C                   EVAL      W#F6 = *IN06
     C                   EVAL      DMSG = *BLANKS
     C                   IF        *IN12 OR *IN03
     C                   EVAL      *IN12 = *OFF
     C                   EVAL      *IN03 = *OFF
     C                   LEAVE
     C                   ENDIF
     C                   IF        W#MODE = 'V'
     C                   LEAVE
     C                   ENDIF
     C                   IF        W#MODE = 'X'
     C                   IF        W#F6
     C                   EXSR      @CANCEL
     C                   LEAVE
     C                   ENDIF
     C                   ITER
     C                   ENDIF
      * 変更モード: 入力チェック、F6なら確定
     C                   EXSR      @CHKCHG
     C                   IF        W#ERR
     C                   ITER
     C                   ENDIF
     C                   IF        W#F6
     C                   EXSR      @CHKINVD
     C                   EXSR      @APPLYCHG
     C                   IF        W#ERR
     C                   ITER
     C                   ENDIF
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      DMSG = '入力に誤りはありません。' +
     C                             'F6で確定してください'
     C                   ENDDO
     C                   EVAL      *IN06 = *OFF
     C                   ENDSR

      *=====================================================================
      * @LOADDTL - 明細をサブファイルに読み込む
      *=====================================================================
     C     @LOADDTL      BEGSR
     C                   EVAL      *IN83 = *ON
     C                   WRITE     DTLCTL
     C                   EVAL      *IN83 = *OFF
     C                   EVAL      W#DRRN = 0
     C                   EVAL      DSONUM = SONUM
     C                   EVAL      W#CDVAL = STATUS
     C                   EXSR      @STATNM
     C                   EVAL      DSTAT = W#CDVAL
     C                   IF        CURRCD = 'JPY' OR CURRCD = *BLANKS
     C                   MONITOR
     C                   EVALR     DTOTAMT = %EDITC(%DECH(TOTAMT:13:0):'1')
     C                   ON-ERROR
     C                   EVAL      DTOTAMT = *ALL'*'
     C                   ENDMON
     C                   ELSE
     C                   MONITOR
     C                   EVALR     DTOTAMT = %EDITC(%DECH(TOTAMT:13:2):'1')
     C                   ON-ERROR
     C                   EVAL      DTOTAMT = *ALL'*'
     C                   ENDMON
     C                   ENDIF
     C                   EVAL      DCUR = CURRCD
     C                   EVAL      W#WHID = WHID
     C     CUSTID        CHAIN     CUSTOMERP
     C                   IF        %FOUND(CUSTOMERP)
     C                   EVAL      DCUSTNM = DbcsTrunc(C_CUSTNM:40)
     C                   ELSE
     C                   EVAL      DCUSTNM = *BLANKS
     C                   ENDIF
     C     W#SOID        SETLL     LREC
     C                   DOW       W#DRRN < 99
     C     W#SOID        READE(N)  LREC
     C                   IF        %EOF(SORDERLP)
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#DRRN = W#DRRN + 1
     C                   EVAL      DLINE = L_LINENO
     C     L_ITEMID      CHAIN     ITEMP
     C                   IF        %FOUND(ITEMP)
     C                   EVAL      DITEM = I_ITEMCD
     C                   EVAL      DNAME = DbcsTrunc(I_ITEMDSC:18)
     C                   ELSE
     C                   EVAL      DITEM = *BLANKS
     C                   EVAL      DNAME = *BLANKS
     C                   ENDIF
     C                   EVAL(H)   DORD = L_ORDQTY
     C                   EVAL(H)   DOLDQ = L_ORDQTY
     C                   EVAL      DOLDP = L_UNITPRC
     C                   EVAL(H)   DSHP = L_SHPQTY
     C                   IF        CURRCD = 'JPY' OR CURRCD = *BLANKS
     C                   MONITOR
     C                   EVALR     DPRC = %EDITC(%DECH(L_UNITPRC:9:0):'1')
     C                   ON-ERROR
     C                   EVAL      DPRC = *ALL'*'
     C                   ENDMON
     C                   ELSE
     C                   MONITOR
     C                   EVALR     DPRC = %EDITC(%DECH(L_UNITPRC:8:2):'1')
     C                   ON-ERROR
     C                   EVAL      DPRC = *ALL'*'
     C                   ENDMON
     C                   ENDIF
     C                   IF        CURRCD = 'JPY' OR CURRCD = *BLANKS
     C                   MONITOR
     C                   EVALR     DAMT = %EDITC(%DECH(L_LINEAMT:11:0):'1')
     C                   ON-ERROR
     C                   EVAL      DAMT = *ALL'*'
     C                   ENDMON
     C                   ELSE
     C                   MONITOR
     C                   EVALR     DAMT = %EDITC(%DECH(L_LINEAMT:11:2):'1')
     C                   ON-ERROR
     C                   EVAL      DAMT = *ALL'*'
     C                   ENDMON
     C                   ENDIF
     C                   EVAL      *IN85 = *OFF
     C                   WRITE     DTLSFL
     C                   ENDDO
     C                   EVAL      W#DCNT = W#DRRN
     C                   ENDSR

      *=====================================================================
      * @CHKCHG - 変更内容のチェック(出荷済以上・在庫)
      *=====================================================================
     C     @CHKCHG       BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      W#I = 1
     C                   DOW       W#I <= W#DCNT
     C                   EVAL      W#DRRN = W#I
     C     W#DRRN        CHAIN     DTLSFL
     C                   EVAL      *IN85 = *OFF
     C                   IF        DORD <= 0
     C                   EVAL      *IN85 = *ON
     C                   EVAL      DMSG = '受注数は1以上を入力してください'
     C                   ELSEIF    DORD < DSHP
     C                   EVAL      *IN85 = *ON
     C                   EVAL      DMSG = '出荷済数量より少なくはできません'
     C                   ELSEIF    DORD > DOLDQ
      * 増加分は在庫の引当可能数を確認
     C                   EVAL      W#LINE = DLINE
     C     LKEY          CHAIN(N)  LREC
     C                   EVAL      W#ITEMID = L_ITEMID
     C     VKEY          CHAIN(N)  VREC
     C                   IF        NOT %FOUND(INVENTORYP) OR
     C                             V_QTYAVL < DORD - DOLDQ
     C                   EVAL      *IN85 = *ON
     C                   EVAL      DMSG = %TRIM(DITEM) + ' 在庫不足'
     C                   ENDIF
     C                   ENDIF
      * 修正#UPC: 単価のチェック
     C                   EVAL      *IN86 = *OFF
     C                   IF        NOT *IN85
     C                   EXSR      @GETPRC
     C                   IF        W#PERR
     C                   EVAL      *IN86 = *ON
     C                   EVAL      DMSG = W#PMSG
     C                   ELSEIF    W#NEWP <> DOLDP AND W#INVD
     C                   EVAL      *IN86 = *ON
     C                   EVAL      DMSG = '請求済のため単価は変更不可'
     C                   ENDIF
     C                   ENDIF
     C                   IF        *IN85 OR *IN86
     C                   EVAL      W#ERR = *ON
     C                   ENDIF
     C                   UPDATE    DTLSFL
     C                   EVAL      W#I = W#I + 1
     C                   ENDDO
     C                   ENDSR

      *=====================================================================
      * @APPLYCHG - 変更を確定(明細・在庫引当・合計・状態)
      *=====================================================================
     C     @APPLYCHG     BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      W#I = 1
     C                   DOW       W#I <= W#DCNT AND NOT W#ERR
     C                   EVAL      W#DRRN = W#I
     C     W#DRRN        CHAIN     DTLSFL
     C                   EVAL      W#DELTA = DORD - DOLDQ
     C                   EXSR      @GETPRC
     C                   EVAL      W#PCHG = (NOT W#PERR AND W#NEWP <> DOLDP)
     C                   IF        W#PCHG AND W#INVD
     C                   EVAL      W#ERR = *ON
     C                   EVAL      DMSG = '請求済のため単価は変更不可'
     C                   LEAVE
     C                   ENDIF
     C                   IF        W#DELTA <> 0 OR W#PCHG
     C                   EVAL      W#LINE = DLINE
     C     LKEY          CHAIN(E)  LREC
     C                   IF        %ERROR OR NOT %FOUND(SORDERLP)
     C                   EVAL      W#ERR = *ON
     C                   EVAL      DMSG = '明細が他で使用中です'
     C                   LEAVE
     C                   ENDIF
      * 画面表示後に他で出荷された場合
     C                   IF        DORD < L_SHPQTY
     C                   EVAL      W#ERR = *ON
     C                   EVAL      DMSG = '出荷済数量が変わりました。' +
     C                             'F12で戻ってやり直してください'
     C                   LEAVE
     C                   ENDIF
      * 修正#CONC 画面表示後に他で取消・数量変更された場合(表示時の
      *   数量との差で引当を増減するため、引当がずれていた)
     C                   IF        L_STATUS = 'CANCELLED' OR
     C                             %DECH(L_ORDQTY:9:0) <> DOLDQ
     C                   EVAL      W#ERR = *ON
     C                   EVAL      DMSG = '他で変更されました。' +
     C                             'F12で戻ってやり直してください'
     C                   LEAVE
     C                   ENDIF
      * 在庫の引当は受注数が変わったときだけ
     C                   IF        W#DELTA <> 0
     C                   EVAL      W#ITEMID = L_ITEMID
     C     VKEY          CHAIN(E)  VREC
     C                   IF        %ERROR OR NOT %FOUND(INVENTORYP)
     C                   EVAL      W#ERR = *ON
     C                   EVAL      DMSG = '在庫が他で使用中です'
     C                   LEAVE
     C                   ENDIF
     C                   IF        W#DELTA > V_QTYAVL
     C                   EVAL      W#ERR = *ON
     C                   EVAL      DMSG = %TRIM(DITEM) + ' 在庫不足'
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      V_QTYALC = V_QTYALC + W#DELTA
     C                   EVAL      V_QTYAVL = V_QTYOH - V_QTYALC
     C                   UPDATE(E) VREC
     C                   IF        %ERROR
     C                   EVAL      W#ERR = *ON
     C                   EVAL      DMSG = '在庫の更新に失敗しました'
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   IF        W#PCHG
     C                   EVAL      L_UNITPRC = W#NEWP
     C                   ENDIF
     C                   EVAL      L_ORDQTY = DORD
     C                   EVAL      L_ALCQTY = L_ALCQTY + W#DELTA
     C                   EVAL(H)   L_LINEAMT = L_ORDQTY * L_UNITPRC
     C                   IF        L_SHPQTY >= L_ORDQTY
     C                   EVAL      L_STATUS = 'COMPLETED'
     C                   ELSEIF    L_SHPQTY > 0
     C                   EVAL      L_STATUS = 'PARTIAL'
     C                   ELSE
     C                   EVAL      L_STATUS = 'ALLOCATED'
     C                   ENDIF
     C                   UPDATE(E) LREC
     C                   IF        %ERROR
     C                   EVAL      W#ERR = *ON
     C                   EVAL      DMSG = '明細の更新に失敗しました'
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   EVAL      W#I = W#I + 1
     C                   ENDDO
     C                   IF        NOT W#ERR
     C                   EXSR      @UPDHDR
     C                   ENDIF
     C                   IF        W#ERR
     C                   ROLBK
     C                   ELSE
     C                   COMMIT
     C                   EVAL      W#MSG = %TRIM(DSONUM) + ' を変更しました'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @UPDHDR - 合計金額と状態を明細から再計算してヘッダを更新
      *=====================================================================
     C     @UPDHDR       BEGSR
     C                   EVAL      W#TOT = 0
     C                   EVAL      W#ALLDONE = *ON
     C                   EVAL      W#ANYSHP = *OFF
     C     W#SOID        SETLL     LREC
     C                   DOW       *ON
     C     W#SOID        READE(N)  LREC
     C                   IF        %EOF(SORDERLP)
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#TOT = W#TOT + L_LINEAMT
     C                   IF        L_SHPQTY < L_ORDQTY
     C                   EVAL      W#ALLDONE = *OFF
     C                   ENDIF
     C                   IF        L_SHPQTY > 0
     C                   EVAL      W#ANYSHP = *ON
     C                   ENDIF
     C                   ENDDO
     C     W#SOID        CHAIN(E)  SOHREC
     C                   IF        %ERROR OR NOT %FOUND(SORDERHP)
     C                   EVAL      W#ERR = *ON
     C                   EVAL      DMSG = '受注ヘッダが他で使用中です'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      TOTAMT = W#TOT
      * 修正#UPC: 出荷済の受注で数量を増やし未出荷が出たら一部出荷に戻す
     C                   IF        STATUS = 'SHIPPED' AND NOT W#ALLDONE
     C                   EVAL      STATUS = 'PARTIAL'
     C                   ENDIF
      * 一部出荷の受注が残数の減少で全量出荷済になった場合
     C                   IF        STATUS = 'PARTIAL' AND W#ALLDONE
     C                   EXSR      @CHKUNBILL
     C                   IF        W#UNBILL
     C                   EVAL      STATUS = 'SHIPPED'
     C                   ELSE
     C                   EVAL      STATUS = 'INVOICED'
     C                   ENDIF
     C                   ENDIF
     C                   EVAL      UPDUSER = P#USER
     C                   EVAL      UPDDATE = %DEC(%DATE():*ISO)
     C                   EVAL      UPDTIM = %DEC(%TIME():*HMS)
     C                   UPDATE(E) SOHREC
     C                   IF        %ERROR
     C                   EVAL      W#ERR = *ON
     C                   EVAL      DMSG = '受注ヘッダの更新に失敗しました'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * 修正#UPC @CHKINVD - 請求済(INVOICED)の出荷があるか
      *=====================================================================
     C     @CHKINVD      BEGSR
     C                   EVAL      W#INVD = *OFF
     C     W#SOID        SETLL     SREC
     C                   DOW       *ON
     C     W#SOID        READE     SREC
     C                   IF        %EOF(SHIPMENHL1)
     C                   LEAVE
     C                   ENDIF
     C                   IF        S_STATUS = 'INVOICED'
     C                   EVAL      W#INVD = *ON
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR

      *=====================================================================
      * 修正#UPC @GETPRC - 画面の単価(文字)を数値にする
      *   カンマは無視。円は整数、外貨は小数2桁まで。空白は元の単価
      *=====================================================================
     C     @GETPRC       BEGSR
     C                   EVAL      W#PERR = *OFF
     C                   EVAL      W#NEWP = DOLDP
     C                   EVAL      W#PRCS = %TRIM(%SCANRPL(',':'':DPRC))
     C                   IF        %LEN(W#PRCS) = 0
     C                   LEAVESR
     C                   ENDIF
     C                   MONITOR
     C                   EVAL      W#NEWP = %DEC(W#PRCS:15:4)
     C                   ON-ERROR
     C                   EVAL      W#PERR = *ON
     C                   EVAL      W#PMSG = '単価は数字で入力してください'
     C                   EVAL      W#NEWP = DOLDP
     C                   LEAVESR
     C                   ENDMON
     C                   SELECT
     C                   WHEN      W#NEWP < 0 OR W#NEWP > 99999999.99
     C                   EVAL      W#PERR = *ON
     C                   EVAL      W#PMSG = '単価が無効です'
     C                   WHEN      (CURRCD = 'JPY' OR CURRCD = *BLANKS) AND
     C                             W#NEWP <> %INT(W#NEWP)
     C                   EVAL      W#PERR = *ON
     C                   EVAL      W#PMSG = '円の単価は整数です'
     C                   WHEN      W#NEWP * 100 <> %INT(W#NEWP * 100)
     C                   EVAL      W#PERR = *ON
     C                   EVAL      W#PMSG = '単価は小数2桁までです'
     C                   ENDSL
     C                   IF        W#PERR
     C                   EVAL      W#NEWP = DOLDP
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @CHKUNBILL - 未請求(SHIPPED)の出荷があるか
      *=====================================================================
     C     @CHKUNBILL    BEGSR
     C                   EVAL      W#UNBILL = *OFF
     C     W#SOID        SETLL     SREC
     C                   DOW       *ON
     C     W#SOID        READE     SREC
     C                   IF        %EOF(SHIPMENHL1)
     C                   LEAVE
     C                   ENDIF
     C                   IF        S_STATUS = 'SHIPPED'
     C                   EVAL      W#UNBILL = *ON
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR

      *=====================================================================
      * @CANCEL - 受注取消(未出荷のみ)。在庫の引当を全て戻す
      *=====================================================================
     C     @CANCEL       BEGSR
     C                   EVAL      W#ERR = *OFF
     C     W#SOID        CHAIN(E)  SOHREC
     C                   IF        %ERROR OR NOT %FOUND(SORDERHP)
     C                   EVAL      W#MSG = '受注が他で使用中のため取消不可'
     C                   LEAVESR
     C                   ENDIF
      * 画面表示後に出荷された場合は取消しない
     C                   IF        STATUS <> 'OPEN' AND STATUS <> 'ALLOCATED'
     C                   ROLBK
     C                   EVAL      W#MSG = %TRIM(SONUM) +
     C                             ' は出荷済のため取消できません'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#WHID = WHID
     C     W#SOID        SETLL     LREC
     C                   DOW       NOT W#ERR
     C     W#SOID        READE(E)  LREC
     C                   IF        %ERROR
     C                   EVAL      W#ERR = *ON
     C                   LEAVE
     C                   ENDIF
     C                   IF        %EOF(SORDERLP)
     C                   LEAVE
     C                   ENDIF
     C                   IF        L_ALCQTY > 0
     C                   EVAL      W#ITEMID = L_ITEMID
     C     VKEY          CHAIN(E)  VREC
     C                   IF        %ERROR OR NOT %FOUND(INVENTORYP)
     C                   EVAL      W#ERR = *ON
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      V_QTYALC = V_QTYALC - L_ALCQTY
     C                   EVAL      V_QTYAVL = V_QTYOH - V_QTYALC
     C                   UPDATE(E) VREC
     C                   IF        %ERROR
     C                   EVAL      W#ERR = *ON
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   EVAL      L_ALCQTY = 0
     C                   EVAL      L_STATUS = 'CANCELLED'
     C                   UPDATE(E) LREC
     C                   IF        %ERROR
     C                   EVAL      W#ERR = *ON
     C                   ENDIF
     C                   ENDDO
     C                   IF        NOT W#ERR
     C                   EVAL      STATUS = 'CANCELLED'
     C                   EVAL      UPDUSER = P#USER
     C                   EVAL      UPDDATE = %DEC(%DATE():*ISO)
     C                   EVAL      UPDTIM = %DEC(%TIME():*HMS)
     C                   UPDATE(E) SOHREC
     C                   IF        %ERROR
     C                   EVAL      W#ERR = *ON
     C                   ENDIF
     C                   ENDIF
     C                   IF        W#ERR
     C                   ROLBK
     C                   EVAL      W#MSG = '取消に失敗しました(他で使用中)'
     C                   ELSE
     C                   COMMIT
     C                   EVAL      W#MSG = %TRIM(SONUM) + ' を取り消しました'
     C                   ENDIF
     C                   ENDSR

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
