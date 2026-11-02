     H DFTACTGRP(*NO) ACTGRP(*NEW)
      *=====================================================================
      * プログラム名: CNFSHPENT
      * 説明: 出荷確認 - 受注選択画面
      *            ALLOCATEDおよびOPENステータスの受注をサブファイルに表示
      *            オプション1 = 選択したSOID+WHIDでCONFIRMSHPを呼び出し
      *                         その後CRTINVCLを呼び出して請求書を作成
      *   修正#1  @PROCESS - CNFSHPCL後、SORDERHPをCHAINしてCUSTIDを取
      *                     その後CRTINVCL(W#SOID + W#CUSTID)を呼び出す
      *   修正#R4 @PROCESS - CRTINVCL失敗時にエラーメッセージをFOOTER表示
      *   修正#R9 @PROCESS - DSPLYをERRMSGフィールド設定に変更
      *   修正#FLT @LOADSFL - ALLOCATED/OPEN以外の受注は表示しない
      *                     (出荷済・請求済の受注の再選択による二重請求防止)
      *   修正#MSG @DISPLAY - エラー/完了/0件メッセージを表示
      *   修正#MUL @PROCESS - 1=選択の全行を処理
      *   修正#B   顧客名の切り詰めを全角対応(DbcsTrunc)
      *   修正#INV 出荷済(未請求)も表示し1=で請求書発行のみ行う
      *   修正#JP  状態をコードマスターの日本語名称で表示
      *   修正#PART 2=分納出荷: 明細別に今回出荷数量を入力して出荷
      *            一部出荷(PARTIAL)の受注も一覧に表示
      *   修正#SHP 3=ピッキングリストを印刷。出荷確定したら納品書を印刷
      *            (SHPPRINT。印刷に失敗しても出荷・請求は続ける)
      *=====================================================================

      *---------------------------------------------------------------------
      * ファイル定義
      *---------------------------------------------------------------------
      * 画面ファイル
     FCNFSHPD   CF   E             WORKSTN SFILE(ORDSFL:W#RRN)
     F                                     SFILE(LINSFL:W#LRRN)
      * 受注ヘッダー（読み取り専用、到着順）
     FSORDERHP  IF   E           K DISK    BLOCK(*NO)
      * 顧客マスター（顧客名参照用）
     FCUSTOMERP IF   E           K DISK    PREFIX(C_)
     F                                     RENAME(CUSTREC:CSTREC)
      * [修正#JP] コードマスター(状態の日本語名称)
     FCODEMSTRP IF   E           K DISK    PREFIX(CD_)
      * [修正#PART] 分納出荷ウィンドウ用(受注明細・商品名)
     FSORDERLP  IF   E           K DISK    PREFIX(SL_)
     F                                     RENAME(SOLREC:SLREC)
     FITEMP     IF   E           K DISK    PREFIX(IT_)

      *---------------------------------------------------------------------
      * 変数定義
      *---------------------------------------------------------------------
     D W#RRN           S              4S 0
     D W#EOF           S               N
     D W#SOID          S             18P 0
     D W#WHID          S              4P 0
      * [修正#1] CRTINVCL呼び出し用顧客ID
     D W#CUSTID        S              9P 0
     D W#FOUND         S               N
     D W#ERRMSG        S             75A
      * [修正#MUL] 複数選択の処理件数
     D W#OKCNT         S             10I 0
     D W#NGCNT         S             10I 0
     D W#SONUM         S             20A
      * [修正#JP] 状態コード/名称取得用
     D W#STATUS        S             12A
     D W#CDTYPE        S             30A
     D W#CDVAL         S             30A
      * [修正#PART] 出荷モード/明細別出荷数量(CNFSHPCLへ渡す)
     D W#MODE          S              1A
     D W#QTYDS         DS
     D   W#QTYS                      15P 3 DIM(20)
     D W#LRRN          S              4S 0
     D W#LCNT          S             10I 0
     D W#LIDX          S             10I 0
     D W#ANY           S               N
     D W#QERR          S               N
     D W#REM           S             15P 3
      * [修正#B] 全角(DBCS)を壊さずに切り詰める
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
      * [修正#SHP] ピッキングリスト・納品書
     D SHPPRINT        PR                  EXTPGM('SHPPRINT')
     D   PTYPE                        1A   CONST
     D   PSOID                       18P 0
     D W#PCKCNT        S             10I 0
     D DbcsTrunc       PR           256A   VARYING
     D   pStr                       256A   VARYING CONST
     D   pMax                        10I 0 CONST

      *=====================================================================
      * メインルーチン
      *=====================================================================
     C                   EXSR      @LOADSFL
     C                   DOW       NOT *IN03
     C                   EXSR      @DISPLAY
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
     C                   EXSR      @PROCESS
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C                   RETURN

      * [修正#JP] コードマスターのキー(コード種別+コード値)
     C     CDKEY         KLIST
     C                   KFLD                    W#CDTYPE
     C                   KFLD                    W#CDVAL

      * [修正#1] CRTINVCLの呼び出しPLIST（SOID 18P0 + CUSTID 9P0）
     C     INVPLIST      PLIST
     C                   PARM                    W#SOID
     C                   PARM                    W#CUSTID

      *=====================================================================
      * @LOADSFL - SORDERHPからサブファイルをロード
      *=====================================================================
     C     @LOADSFL      BEGSR
      * サブファイルのクリア
     C                   EVAL      *IN33 = *ON
     C                   EVAL      *IN31 = *OFF
     C                   EVAL      *IN32 = *OFF
     C                   WRITE     ORDCTL
     C                   EVAL      *IN33 = *OFF
     C                   EVAL      W#RRN = *ZERO
     C                   EVAL      W#EOF = *OFF
      * 受注ヘッダーから全受注を読み込む
     C     *LOVAL        SETLL     SORDERHP
      * 修正#9999 サブファイルの上限(9999件)で止める
     C                   DOW       NOT W#EOF AND W#RRN < 9999
     C                   READ      SORDERHP                               99
     C                   EVAL      W#EOF = *IN99
     C                   IF        W#EOF
     C                   LEAVE
     C                   ENDIF
      * [修正#FLT] 出荷確定対象(ALLOCATED/OPEN)のみ表示
      * [修正#INV] 出荷済で未請求(SHIPPED)も表示し請求書発行だけ行う
     C                   IF        STATUS <> 'ALLOCATED' AND
     C                             STATUS <> 'OPEN' AND
     C                             STATUS <> 'PARTIAL' AND
     C                             STATUS <> 'SHIPPED'
     C                   ITER
     C                   ENDIF
      * 顧客名を取得
     C     CUSTID        CHAIN     CUSTOMERP
     C                   IF        %FOUND(CUSTOMERP)
     C                   EVAL      SFCUSTNM = DbcsTrunc(C_CUSTNM:20)
     C                   ELSE
     C                   EVAL      SFCUSTNM = '(Unknown)'
     C                   ENDIF
      * サブファイルフィールドをセット
     C                   EVAL      SFLOPT = ' '
     C                   EVAL      SFSONUM = SONUM
     C                   EVAL      SFSODT = ORDDATE
      * [修正#JP] 状態は日本語名称で表示(判定用コードは隠しフィールド)
     C                   EVAL      SFSTSCD = STATUS
     C                   EVAL      W#CDTYPE = 'SO_STATUS'
     C                   EVAL      W#CDVAL = STATUS
     C     CDKEY         CHAIN     CODEMSTRP
     C                   IF        %FOUND(CODEMSTRP)
     C                   EVAL      SFSTATUS = DbcsTrunc(CD_DESC:10)
     C                   ELSE
     C                   EVAL      SFSTATUS = STATUS
     C                   ENDIF
     C                   EVAL      SFSOID = SOID
     C                   EVAL      SFWHID = WHID
      * サブファイルレコードを書き込む
     C                   EVAL      W#RRN = W#RRN + 1
     C                   WRITE     ORDSFL
     C                   ENDDO
      * 1件以上あればサブファイルを表示
     C                   IF        W#RRN > 0
     C                   EVAL      *IN31 = *ON
      * [修正#MSG] 0件の場合はその旨を表示(エラー表示中は上書きしない)
     C                   ELSEIF    W#ERRMSG = *BLANKS
     C                   EVAL      W#ERRMSG = '出荷確定の対象となる' +
     C                             '受注はありません'
     C                   ENDIF
     C                   EVAL      *IN32 = *ON
     C                   EVAL      *IN34 = *ON
     C                   ENDSR

      *=====================================================================
      * @DISPLAY - 画面を表示してユーザー入力を待つ
      * [修正#MSG] W#ERRMSGを画面のERRMSGへ転記して表示
      *           (従来は表示直前に*IN41をOFFにしており一度も出なかった)
      *=====================================================================
     C     @DISPLAY      BEGSR
     C                   EVAL      ERRMSG = W#ERRMSG
     C                   EVAL      *IN41 = (W#ERRMSG <> *BLANKS)
      * [修正#CLR] 前回の一覧が残らないよう画面を消してから表示
     C                   WRITE     CLRSCR
     C                   WRITE     FOOTER
     C                   EXFMT     ORDCTL
     C                   EVAL      W#ERRMSG = *BLANKS
     C                   ENDSR

      *=====================================================================
      * @PROCESS - ユーザー選択の処理
      * [修正#R4] CRTINVCL失敗時にエラーをERRMSGフィールドに設定し表示
      * [Mod#R9] DSPLY を ERRMSG フィールド設定に変更
      * [修正#MUL] READCで1=選択の全行を処理(従来は先頭1件のみ)
      *=====================================================================
     C     CNFPLIST      PLIST
     C                   PARM                    W#SOID
     C                   PARM                    W#WHID
      * [修正#PART] 出荷モードと明細別出荷数量
     C                   PARM                    W#MODE
     C                   PARM                    W#QTYDS

     C     @PROCESS      BEGSR
     C                   EVAL      W#ERRMSG = *BLANKS
     C                   EVAL      W#OKCNT = 0
     C                   EVAL      W#NGCNT = 0
     C                   EVAL      W#PCKCNT = 0
     C                   READC     ORDSFL                                 99
     C                   DOW       NOT *IN99
      * [修正#SHP] 3=ピッキングリスト
     C                   IF        SFLOPT = '3'
     C                   EVAL      W#SOID = SFSOID
     C                   CALLP(E)  SHPPRINT('P':W#SOID)
     C                   IF        %ERROR
     C                   EVAL      W#ERRMSG = 'ピッキングリスト印刷エラー:' +
     C                             %TRIM(SFSONUM)
     C                   ELSE
     C                   EVAL      W#PCKCNT = W#PCKCNT + 1
     C                   ENDIF
     C                   ENDIF
     C                   IF        SFLOPT = '1' OR SFLOPT = '2'
     C                   EVAL      W#SOID = SFSOID
     C                   EVAL      W#WHID = SFWHID
     C                   EVAL      W#SONUM = SFSONUM
     C                   EVAL      W#STATUS = SFSTSCD
     C                   CLEAR                   W#QTYDS
     C                   IF        SFLOPT = '1'
     C                   EVAL      W#MODE = 'A'
     C                   EXSR      @SHIPONE
     C                   ELSE
      * [修正#PART] 2=分納出荷
     C                   EXSR      @PARTIAL
     C                   ENDIF
     C                   ENDIF
     C                   READC     ORDSFL                                 99
     C                   ENDDO
      * ステータス更新を反映するためサブファイルを再ロード
     C                   IF        W#PCKCNT > 0 AND W#OKCNT + W#NGCNT = 0
     C                             AND W#ERRMSG = *BLANKS
     C                   EVAL      W#ERRMSG = %CHAR(W#PCKCNT) +
     C                             '件のピッキングリストを印刷しました'
     C                   EXSR      @LOADSFL
     C                   ENDIF
     C                   IF        W#OKCNT + W#NGCNT > 0
     C                   EXSR      @LOADSFL
     C                   IF        W#NGCNT = 0
     C                   EVAL      W#ERRMSG = %CHAR(W#OKCNT) +
     C                             '件の出荷確定・請求書発行が完了しました'
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * @SHIPONE - 1受注の出荷確定と請求書発行
      * [修正#1] CNFSHPCL失敗時(エスケープ)は請求書を発行しない
      *=====================================================================
     C     @SHIPONE      BEGSR
      * [修正#INV] 出荷済(未請求)は出荷確定を行わず請求書発行のみ
     C                   IF        W#STATUS <> 'SHIPPED'
     C                   CALL(E)   'CNFSHPCL'    CNFPLIST
     C                   IF        %ERROR
     C                   EVAL      W#NGCNT = W#NGCNT + 1
     C                   EVAL      W#ERRMSG = '出荷確定エラー: ' +
     C                             %TRIM(W#SONUM) + ' (ジョブログ参照)'
     C                   LEAVESR
     C                   ENDIF
      * [修正#SHP] 納品書
     C                   CALLP(E)  SHPPRINT('D':W#SOID)
     C                   ENDIF
      * SORDERHPからCUSTIDを取得
     C     W#SOID        CHAIN     SORDERHP
     C                   IF        NOT %FOUND(SORDERHP)
     C                   EVAL      W#NGCNT = W#NGCNT + 1
     C                   EVAL      W#ERRMSG = '受注ヘッダーなし: ' +
     C                             %TRIM(W#SONUM)
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#CUSTID = CUSTID
     C                   CALL(E)   'CRTINVCL'    INVPLIST
     C                   IF        %ERROR
     C                   EVAL      W#NGCNT = W#NGCNT + 1
     C                   EVAL      W#ERRMSG = '請求書発行エラー: ' +
     C                             %TRIM(W#SONUM) + ' (ジョブログ参照)'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#OKCNT = W#OKCNT + 1
     C                   ENDSR

      *=====================================================================
      * @PARTIAL - 分納出荷(明細別に今回出荷数量を入力)
      * [修正#PART] 入力した数量だけ出荷し、出荷分の請求書を発行する
      *=====================================================================
     C     @PARTIAL      BEGSR
     C                   IF        W#STATUS = 'SHIPPED'
     C                   EVAL      W#NGCNT = W#NGCNT + 1
     C                   EVAL      W#ERRMSG = '出荷済のため分納不可: ' +
     C                             %TRIM(W#SONUM)
     C                   LEAVESR
     C                   ENDIF
      * 残数のある明細をウィンドウに表示
     C                   EVAL      *IN83 = *ON
     C                   WRITE     LINCTL
     C                   EVAL      *IN83 = *OFF
     C                   EVAL      W#LRRN = 0
     C     W#SOID        SETLL     SLREC
     C                   DOW       W#LRRN < 20
     C     W#SOID        READE     SLREC
     C                   IF        %EOF(SORDERLP)
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#REM = SL_ORDQTY - SL_SHPQTY
     C                   IF        W#REM <= 0
     C                   ITER
     C                   ENDIF
     C                   EVAL      W#LRRN = W#LRRN + 1
     C                   EVAL      LSLINE = SL_LINENO
     C     SL_ITEMID     CHAIN     ITEMP
     C                   IF        %FOUND(ITEMP)
     C                   EVAL      LSITEM = IT_ITEMCD
     C                   EVAL      LSNAME = DbcsTrunc(IT_ITEMDSC:20)
     C                   ELSE
     C                   EVAL      LSITEM = *BLANKS
     C                   EVAL      LSNAME = *BLANKS
     C                   ENDIF
     C                   EVAL      LSORD = SL_ORDQTY
     C                   EVAL      LSSHP = SL_SHPQTY
     C                   EVAL      LSREM = W#REM
     C                   EVAL      LSQTY = 0
     C                   EVAL      *IN85 = *OFF
     C                   WRITE     LINSFL
     C                   ENDDO
     C                   IF        W#LRRN = 0
     C                   EVAL      W#NGCNT = W#NGCNT + 1
     C                   EVAL      W#ERRMSG = '出荷できる明細がありません: ' +
     C                             %TRIM(W#SONUM)
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      W#LCNT = W#LRRN
     C                   EVAL      LSSONUM = W#SONUM
     C                   EVAL      LSMSG = *BLANKS
      * 入力チェック(0以上・残数以下・1行以上)が通るまで繰り返す
     C                   DOW       *ON
     C                   EVAL      *IN81 = *ON
     C                   EVAL      *IN82 = *ON
     C                   EXFMT     LINCTL
     C                   IF        *IN12
     C                   EVAL      *IN12 = *OFF
     C                   EVAL      W#ERRMSG = '分納出荷を取り消しました: ' +
     C                             %TRIM(W#SONUM)
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      *IN03 = *OFF
     C                   CLEAR                   W#QTYDS
     C                   EVAL      W#ANY = *OFF
     C                   EVAL      W#QERR = *OFF
     C                   EVAL      LSMSG = *BLANKS
     C                   EVAL      W#LIDX = 1
     C                   DOW       W#LIDX <= W#LCNT
     C                   EVAL      W#LRRN = W#LIDX
     C     W#LRRN        CHAIN     LINSFL
     C                   EVAL      *IN85 = *OFF
     C                   IF        LSQTY < 0 OR LSQTY > LSREM
     C                   EVAL      *IN85 = *ON
     C                   EVAL      W#QERR = *ON
     C                   ELSEIF    LSQTY > 0
     C                   EVAL      W#ANY = *ON
     C                   IF        LSLINE <= 20
     C                   EVAL      W#QTYS(LSLINE) = LSQTY
     C                   ENDIF
     C                   ENDIF
     C                   UPDATE    LINSFL
     C                   EVAL      W#LIDX = W#LIDX + 1
     C                   ENDDO
     C                   IF        W#QERR
     C                   EVAL      LSMSG = '今回出荷数量が残数を超えています'
     C                   ITER
     C                   ENDIF
     C                   IF        NOT W#ANY
     C                   EVAL      LSMSG = '今回出荷数量を入力してください'
     C                   ITER
     C                   ENDIF
     C                   LEAVE
     C                   ENDDO
     C                   EVAL      W#MODE = 'P'
     C                   EXSR      @SHIPONE
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
