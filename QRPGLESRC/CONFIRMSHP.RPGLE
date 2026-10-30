     H DATEDIT(*YMD) DECEDIT('0.') COPYRIGHT('DEMOAPP')
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      *=====================================================================
      * プログラム名: CONFIRMSHP
      * 説明: 出荷確認プログラム（在庫引当処理）
      * ライブラリ: DEMOAPP
      * 変更履歴:
      *   修正#2  @UPDSOL - SORDERLPのCHAINキーを複合キー
      *          SOLKEY(W#SOID+W#SOLINENO)に変更
      *   修正#5  @REDINV - 在庫チェック順序を修正
      *          QTYALCをQTYOH前にチェック
      *   修正#6  @REDINV - CHAINキーをINVKEY(W#WHID+W#ITEMID)に変更
      *   修正#7  @UPDSOH - @VLDSOH後の冗長なCHAINを削除
      *   修正#8  @COMMIT - コミット制御を追加
      *          正常時COMMIT、エラー時ROLLBACK
      *   修正#9  @UPDSOL - 冗長CHAINを削除（CHAINのみのループを使用）
      *   修正#10 @PRCSOL - LINENO CHAINのみでループ
      *   修正#11 @WRTSHPH - SHPNUM/CARRIER/TRACKNO/USERのハードコード廃止
      *          SHPNUMはW#SHPIDから生成、ユーザーはPSDSから取得
      *   修正#ESC @END - エラー時はCPF9898エスケープで呼び出し元へ通知
      *          @VLDSOH/@REDINV のRETURNをLEAVESRに変更(@END経由で終了)
      *   修正#PART 分納出荷: 第3/4パラメータ(モード/明細別出荷数量)
      *          A=残数を全量出荷 P=指定数量のみ出荷(0は出荷しない)
      *          全明細出荷済ならSHIPPED、残りがあればPARTIAL
      *   修正#SHPID 出荷IDをシーケンスで採番(同じ秒の出荷で重複)
      *   修正#HDR @UPDSOHで受注ヘッダを読み直す(出荷ヘッダの作成日等で
      *          受注の作成日が上書きされていた)
      *   修正#NODSP DSPLYを削除(画面から呼ばれると応答待ちで止まるため)
      *          エラー内容はエスケープメッセージで呼び出し元へ通知
      *=====================================================================

      *---------------------------------------------------------------
      * ファイル定義
      *---------------------------------------------------------------
     FSORDERHP  UF   E           K DISK    COMMIT
     FSORDERLP  UF   E           K DISK    COMMIT
     FSHIPMENTHPO    E             DISK    COMMIT
     FSHIPMENTLPO    E             DISK    COMMIT
     FINVENTORYPUF   E           K DISK    COMMIT


      *---------------------------------------------------------------
      * 変数定義
      *---------------------------------------------------------------
     D WTIMESTAMP      S               Z
     D WDATE8          S              8S 0
     D WTIME6          S              6S 0
     D W#CNTR          S              9S 0
     D W#SHPID         S             18P 0
     D W#SHPNUM        S             20A
     D W#SOID          S             18P 0
     D W#WHID          S              4P 0
     D W#LINENO        S              9P 0
     D W#FOUND         S               N
     D W#ERROR         S               N
     D W#ERRMSG        S             52A
     D W#EOF           S               N
      * 出荷明細変数
     D W#ITEMID        S              9P 0
     D W#SHPQTY        S             15P 3
     D W#UNITPRC       S             15P 4
     D W#LINEAMT       S             15P 2
     D W#SOLINENO      S              9P 0
     D W#TIM6C         S              6A
     D W#USER          S             30A
      * [修正#PART] 出荷モード/明細別出荷数量(受注明細行番号順)
     D W#MODE          S              1A
     D W#P_SHPQTY      S             15P 3 DIM(20)
     D W#REMQTY        S             15P 3
     D W#SHIPLN        S             10I 0
     D W#ALLDONE       S               N
      * [修正#ESC] エスケープメッセージ送信用
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

      *=====================================================================
      * エントリパラメータ
      *=====================================================================
     C     *ENTRY        PLIST
     C                   PARM                    W#SOID
     C                   PARM                    W#WHID
      * [修正#PART] 省略時は全量出荷
     C                   PARM                    W#MODE
     C                   PARM                    W#P_SHPQTY

      * [修正#SHPID] シーケンスはSQL命名で修飾して解決
     C/EXEC SQL
     C+   SET OPTION COMMIT = *NONE, NAMING = *SQL
     C/END-EXEC

      *=====================================================================
      * メインルーチン
      *=====================================================================
     C                   EXSR      @INZ
     C                   EXSR      @VLDSOH
     C                   IF        W#ERROR
     C                   EXSR      @END
     C                   ENDIF
     C                   EXSR      @GENSHPID
     C                   IF        W#ERROR
     C                   EXSR      @END
     C                   ENDIF
     C                   EXSR      @PRCSOL
     C                   IF        W#ERROR
     C                   EXSR      @END
     C                   ENDIF
     C                   EXSR      @WRTSHPH
     C                   EXSR      @UPDSOH
     C                   EXSR      @END

      *=====================================================================
      * サブルーチン: @INZ - 初期処理
      *=====================================================================
     C     @INZ          BEGSR
     C                   TIME                    WTIMESTAMP
     C                   EVAL      WDATE8 = %DEC(%DATE(WTIMESTAMP):*ISO)
     C                   EVAL      WTIME6 = %DEC(%TIME(WTIMESTAMP):*HMS)
     C                   EVAL      W#CNTR = *ZERO
     C                   EVAL      W#ERROR = *OFF
     C                   EVAL      W#LINENO = *ZERO
      * W#SOID / W#WHID は *ENTRY PLIST で受け取る
     C                   IF        %PARMS < 4
     C                   EVAL      W#MODE = 'A'
     C                   ENDIF
     C                   IF        W#MODE <> 'P'
     C                   EVAL      W#MODE = 'A'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @VLDSOH - 受注ヘッダーバリデーション
      *=====================================================================
     C     @VLDSOH       BEGSR
     C     W#SOID        CHAIN     SORDERHP
     C                   EVAL      W#FOUND = %FOUND(SORDERHP)
     C                   IF        NOT W#FOUND
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '受注が見つかりません'
     C                   LEAVESR
     C                   ENDIF
      * 監査フィールド用に受注ヘッダーから作成ユーザーを取得
     C                   EVAL      W#USER = CRTUSER
      * ステータスチェック
      * [修正#PART] 一部出荷(PARTIAL)の残りも出荷できる
     C                   IF        STATUS <> 'ALLOCATED' AND
     C                             STATUS <> 'OPEN' AND
     C                             STATUS <> 'PARTIAL'
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '受注ステータスが無効です'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @GENSHPID - 出荷ID・出荷番号の生成
      * [Mod#R6] MOVEL+%TRIMRを廃止し %EDITC でゼロ埋め6桁を保証
      *=====================================================================
     C     @GENSHPID     BEGSR
      * [修正#SHPID] シーケンスで採番(時刻では同じ秒の出荷が重複した)
     C/EXEC SQL
     C+   VALUES NEXT VALUE FOR O2C_SHPID_SEQ INTO :W#SHPID
     C/END-EXEC
     C                   IF        SQLCODE <> 0 OR W#SHPID <= 0
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '出荷ID採番エラー'
     C                   LEAVESR
     C                   ENDIF
      * 出荷番号: 'SHP-' + 出荷ID
     C                   EVAL      W#SHPNUM = 'SHP-' + %CHAR(W#SHPID)
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @PRCSOL - 受注明細処理
      * [修正#10] LINENO CHAINのみでループ - READE+CHAINのカーソル問題を回
      *=====================================================================
     C     SOLKEY        KLIST
     C                   KFLD                    W#SOID
     C                   KFLD                    W#SOLINENO

     C     @PRCSOL       BEGSR
     C                   EVAL      W#SOLINENO = *ZERO
     C                   EVAL      W#EOF = *OFF
     C                   EVAL      W#SHIPLN = 0
     C                   EVAL      W#ALLDONE = *ON
     C                   DOW       NOT W#EOF AND NOT W#ERROR
      * 行番号をインクリメントしてCHAIN（ロック取得）
     C                   EVAL      W#SOLINENO = W#SOLINENO + 1
     C     SOLKEY        CHAIN     SORDERLP                           99
     C                   IF        *IN99
      * この受注の明細なし - ループの正常終了
     C                   EVAL      W#EOF = *ON
     C                   LEAVE
     C                   ENDIF
      * 明細データの取得
     C                   EVAL      W#ITEMID = ITEMID
      * [修正#PART] 残数(出荷済は対象外)と今回の出荷数量
     C                   EVAL      W#REMQTY = ORDQTY - SHPQTY
     C                   IF        W#REMQTY <= 0
     C                   ITER
     C                   ENDIF
     C                   IF        W#MODE = 'P'
     C                   IF        W#SOLINENO <= 20
     C                   EVAL      W#SHPQTY = W#P_SHPQTY(W#SOLINENO)
     C                   ELSE
     C                   EVAL      W#SHPQTY = 0
     C                   ENDIF
     C                   ELSE
     C                   EVAL      W#SHPQTY = W#REMQTY
     C                   ENDIF
     C                   IF        W#SHPQTY < 0 OR W#SHPQTY > W#REMQTY
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '出荷数量が残数超過(' +
     C                             %CHAR(W#SOLINENO) + '行目)'
     C                   LEAVE
     C                   ENDIF
     C                   IF        W#SHPQTY < W#REMQTY
     C                   EVAL      W#ALLDONE = *OFF
     C                   ENDIF
     C                   IF        W#SHPQTY = 0
     C                   ITER
     C                   ENDIF
     C                   EVAL      W#SHIPLN = W#SHIPLN + 1
     C                   EVAL      W#UNITPRC = UNITPRC
     C                   EVAL      W#LINEAMT = W#SHPQTY * W#UNITPRC
      * 在庫減算
     C                   EXSR      @REDINV
     C                   IF        W#ERROR
      * [修正#8] 在庫エラー時にロールバック
     C                   EXSR      @RLBK
     C                   LEAVE
     C                   ENDIF
      * 受注明細の更新
     C                   EXSR      @UPDSOL
     C                   IF        W#ERROR
      * [修正#8] 受注明細エラー時にロールバック
     C                   EXSR      @RLBK
     C                   LEAVE
     C                   ENDIF
      * 出荷明細の書き込み
     C                   EXSR      @WRTSHPL
     C                   ENDDO
      * [修正#PART] 1行も出荷しない場合はエラー
     C                   IF        NOT W#ERROR AND W#SHIPLN = 0
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '出荷する明細がありません'
     C                   ENDIF
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @REDINV - 在庫減算
      * [修正#5] 在庫チェック順序を修正
      * [修正#6] CHAINキーをINVKEY(W#WHID+W#ITEMID)に変更
      *=====================================================================
     C     INVKEY        KLIST
     C                   KFLD                    W#WHID
     C                   KFLD                    W#ITEMID

     C     @REDINV       BEGSR
     C     INVKEY        CHAIN     INVENTORYP                         99
     C                   IF        *IN99
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '在庫が見つかりません'
     C                   LEAVESR
     C                   ENDIF
      * [修正#5] 先に引当数量をチェック
     C                   IF        QTYALC < W#SHPQTY
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '引当数量が不足しています'
     C                   LEAVESR
     C                   ENDIF
      * 在庫数量の二重チェック
     C                   IF        QTYOH < W#SHPQTY
     C                   EVAL      W#ERROR = *ON
     C                   EVAL      W#ERRMSG = '在庫数量が不足しています'
     C                   LEAVESR
     C                   ENDIF
      * 在庫減算 quantities
     C                   EVAL      QTYOH = QTYOH - W#SHPQTY
     C                   EVAL      QTYALC = QTYALC - W#SHPQTY
     C                   EVAL      QTYAVL = QTYOH - QTYALC
     C                   EVAL      LSTUPD = WTIMESTAMP
     C                   UPDATE    INVREC
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @UPDSOL - 受注明細の更新
      * [修正#9] 冗長CHAINを削除 - READEでロック済みレコードを直接使用
      *=====================================================================
     C     @UPDSOL       BEGSR
      * 出荷済数量の更新
     C                   EVAL      SHPQTY = SHPQTY + W#SHPQTY
      * ステータスの更新
     C                   IF        SHPQTY >= ORDQTY
     C                   EVAL      STATUS = 'COMPLETED'
     C                   ELSE
     C                   EVAL      STATUS = 'PARTIAL'
     C                   ENDIF
     C                   UPDATE    SOLREC
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @WRTSHPH - 出荷ヘッダーの書き込み
      *=====================================================================
     C     @WRTSHPH      BEGSR
     C                   EVAL      SHPID = W#SHPID
     C                   EVAL      SHPNUM = W#SHPNUM
     C                   EVAL      SOID = W#SOID
     C                   EVAL      SHPDATE = WDATE8
     C                   EVAL      WHID = W#WHID
     C                   EVAL      CARRIER = *BLANKS
     C                   EVAL      TRACKNO = *BLANKS
     C                   EVAL      STATUS = 'SHIPPED'
     C                   EVAL      CRTUSER = W#USER
     C                   EVAL      CRTDATE = WDATE8
     C                   EVAL      CRTTIM = WTIME6
     C                   EVAL      UPDUSER = W#USER
     C                   EVAL      UPDDATE = WDATE8
     C                   EVAL      UPDTIM = WTIME6
     C                   WRITE     SHPREC
     C                   EVAL      W#CNTR = W#CNTR + 1
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @WRTSHPL - 出荷明細の書き込み
      *=====================================================================
     C     @WRTSHPL      BEGSR
     C                   EVAL      W#LINENO = W#LINENO + 1
     C                   EVAL      SHPID = W#SHPID
     C                   EVAL      LINENO = W#LINENO
     C                   EVAL      SOLNO = W#SOLINENO
     C                   EVAL      ITEMID = W#ITEMID
     C                   EVAL      SHPQTY = W#SHPQTY
     C                   EVAL      LOTNO = ''
     C                   WRITE     SHPLREC
     C                   EVAL      W#CNTR = W#CNTR + 1
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @UPDSOH - 受注ヘッダーの更新
      * [修正#7] @VLDSOH後の冗長CHAINを削除
      *=====================================================================
     C     @UPDSOH       BEGSR
      * [修正#HDR] 同名項目で上書きされているため受注ヘッダを読み直す
     C     W#SOID        CHAIN     SORDERHP
      * [修正#PART] 全明細出荷済ならSHIPPED、残りがあればPARTIAL
     C                   IF        W#ALLDONE
     C                   EVAL      STATUS = 'SHIPPED'
     C                   ELSE
     C                   EVAL      STATUS = 'PARTIAL'
     C                   ENDIF
     C                   EVAL      UPDUSER = W#USER
     C                   EVAL      UPDDATE = WDATE8
     C                   EVAL      UPDTIM = WTIME6
     C                   UPDATE    SOHREC
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @END - 終了処理
      * [修正#8] 成功時COMMIT、エラー時ROLLBACK
      *=====================================================================
     C     @END          BEGSR
     C                   IF        NOT W#ERROR
      * [修正#8] 全変更をコミット
     C                   COMMIT
     C                   EVAL      W#ERRMSG = '出荷確認が完了しました'
     C                   ELSE
      * [修正#8] 全変更をロールバック
     C                   EXSR      @RLBK
     C                   EVAL      W#ESCMSG = W#ERRMSG
     C                   EVAL      W#ERRMSG = '出荷確認に失敗しました'
      * [修正#ESC] 呼び出し元(CNFSHPCL)へエスケープで失敗を通知
      *            これにより CNFSHPENT は請求書発行(CRTINVCL)を行わない
     C                   EVAL      *INLR = *ON
     C                   CALLP     QMHSNDPM('CPF9898':'QCPFMSG   *LIBL':
     C                             W#ESCMSG:%LEN(%TRIMR(W#ESCMSG)):
     C                             '*ESCAPE':'*PGMBDY':1:W#MSGKEY:W#APIERR)
     C                   ENDIF
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDSR

      *=====================================================================
      * サブルーチン: @RLBK - 全変更のロールバック
      * [修正#8] エラー発生時に全DB更新を取り消すために呼び出す
      *=====================================================================
     C     @RLBK         BEGSR
     C                   ROLBK
     C                   ENDSR

