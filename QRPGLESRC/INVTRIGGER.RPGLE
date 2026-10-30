     H DATEDIT(*YMD) DECEDIT('0.') COPYRIGHT('DEMOAPP')
      * プログラム名: INVTRIGGER
      * 説明: 在庫更新トリガープログラム(INVENTORYP)
      * ライブラリ: DEMOAPP
      * 変更履歴:
      *   修正#8 - *ENTRY PLISTでトリガーバッファ受取
      *   修正#R11 @CHKCONS - 整合性エラー時に異常終了
      *   修正#R12 @CHKCONS - 整合性エラー時は異常終了せずログのみ
      *            トリガーが例外で終了すると UPDATE 自体が MCH1202 を
      *            引き起こすため、@CHKCONS は RETURN せずに処理を継続
      *   修正#BIN TRGHEADER - 4B 0(2バイト)を10I 0(4バイト)に変更
      *            オフセットずれによるRNQ0907を解消
      *   修正#LOGID LOGIDをシーケンス(O2C_LOGID_SEQ)で採番
      *            (時刻+件数では同じ秒の更新でキー重複し在庫更新が失敗)
      *            WRITE(E)でログ失敗時も在庫更新は失敗させない
      *   修正#NODSP 在庫マイナス警告のDSPLYを廃止しログ(ALERT)に記録
      *            (バッチで更新すると応答待ちで止まるため)
     FCHANGELOGPO    E             DISK
     D TRGBUF          S          32767A
     D TRGBUFLEN       S             10I 0
     D pTRGBUF         S               *
     D TRGHEADER       DS                  BASED(pTRGBUF)
     D   TH_PFNAME                   10A
     D   TH_PFLIB                    10A
     D   TH_PFMBR                    10A
     D   TH_EVENT                     1A
     D   TH_TIME                      1A
     D   TH_CMTLCK                    1A
     D   TH_RSRV1                     3A
     D   TH_CCSID                    10I 0
     D   TH_RSRV2                     8A
     D   TH_OLDOFS                   10I 0
     D   TH_OLDLEN                   10I 0
     D   TH_OLDNBOFS                 10I 0
     D   TH_OLDNBLEN                 10I 0
     D   TH_NEWOFS                   10I 0
     D   TH_NEWLEN                   10I 0
     D   TH_NEWNBOFS                 10I 0
     D   TH_NEWNBLEN                 10I 0
     D   TH_RRNO                     10I 0
     D   TH_RSRV3                     8A
     D pINVREC         S               *
     D INVREC_DS       DS                  BASED(pINVREC)
     D   IR_WHID                      4P 0
     D   IR_ITEMID                    9P 0
     D   IR_QTYOH                    15P 3
     D   IR_QTYALC                   15P 3
     D   IR_QTYAVL                   15P 3
     D   IR_LSTUPD                     Z
     D W#WHID          S              4P 0
     D W#ITEMID        S              9P 0
     D W#QTYOH_OLD     S             15P 3
     D W#QTYOH_NEW     S             15P 3
     D W#QTYALC_OLD    S             15P 3
     D W#QTYALC_NEW    S             15P 3
     D W#QTYAVL_OLD    S             15P 3
     D W#QTYAVL_NEW    S             15P 3
     D WTIMESTAMP      S               Z
     D W#CNTR          S              9S 0
     D W#LOGID         S             18P 0
     D W#EXPECTED      S             15P 3
     D W#RECKEY        S             30A
     D W#ALERT         S             52A
     D W#OLDVAL        S           1024A
      * 修正#LOGID: ログの操作区分
     D W#ACTION        S             10A
     D W#NEWVAL        S           1024A
      * 修正#LOGID: どのジョブ(ライブラリーリスト)から呼ばれても
      *            シーケンスを解決できるようSQL命名で修飾する
     C/EXEC SQL
     C+   SET OPTION COMMIT = *NONE, NAMING = *SQL
     C/END-EXEC
     C     *ENTRY        PLIST
     C                   PARM                    TRGBUF
     C                   PARM                    TRGBUFLEN
     C                   EXSR      @INZ
     C                   EXSR      @CHKQOH
     C                   EXSR      @CHKALC
     C                   EXSR      @CHKAVL
     C                   EXSR      @CHKCONS
     C                   EXSR      @END
     C     @INZ          BEGSR
     C                   TIME                    WTIMESTAMP
     C                   EVAL      W#CNTR = *ZERO
     C                   EVAL      pTRGBUF = %ADDR(TRGBUF)
     C                   EVAL      pINVREC = %ADDR(TRGBUF) +
     C                               TH_OLDOFS
     C                   EVAL      W#WHID = IR_WHID
     C                   EVAL      W#ITEMID = IR_ITEMID
     C                   EVAL      W#QTYOH_OLD = IR_QTYOH
     C                   EVAL      W#QTYALC_OLD = IR_QTYALC
     C                   EVAL      W#QTYAVL_OLD = IR_QTYAVL
     C                   EVAL      pINVREC = %ADDR(TRGBUF) +
     C                               TH_NEWOFS
     C                   EVAL      W#QTYOH_NEW = IR_QTYOH
     C                   EVAL      W#QTYALC_NEW = IR_QTYALC
     C                   EVAL      W#QTYAVL_NEW = IR_QTYAVL
     C                   EVAL      W#RECKEY = 'WH=' + %CHAR(W#WHID) +
     C                               ',IT=' + %CHAR(W#ITEMID)
     C                   ENDSR
     C     @CHKQOH       BEGSR
     C                   IF        W#QTYOH_OLD <> W#QTYOH_NEW
     C                   EVAL      W#OLDVAL = 'QTYOH='+%CHAR(W#QTYOH_OLD)
     C                   EVAL      W#NEWVAL = 'QTYOH='+%CHAR(W#QTYOH_NEW)
     C                   EXSR      @LOGCHG
     C                   IF        W#QTYOH_NEW < 0
      * 在庫数量がマイナスになった場合の警告(修正#NODSP: ログに記録)
     C                   EVAL      W#ALERT = '在庫数量がマイナスです'
     C                   EVAL      W#OLDVAL = W#ALERT
     C                   EVAL      W#NEWVAL = 'QTYOH='+%CHAR(W#QTYOH_NEW)
     C                   EVAL      W#ACTION = 'ALERT'
     C                   EXSR      @WRTLOG
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C     @CHKALC       BEGSR
     C                   IF        W#QTYALC_OLD <> W#QTYALC_NEW
     C                   EVAL      W#OLDVAL = 'QTYALC='+%CHAR(W#QTYALC_OLD)
     C                   EVAL      W#NEWVAL = 'QTYALC='+%CHAR(W#QTYALC_NEW)
     C                   EXSR      @LOGCHG
     C                   ENDIF
     C                   ENDSR
     C     @CHKAVL       BEGSR
     C                   IF        W#QTYAVL_OLD <> W#QTYAVL_NEW
     C                   EVAL      W#OLDVAL = 'QTYAVL='+%CHAR(W#QTYAVL_OLD)
     C                   EVAL      W#NEWVAL = 'QTYAVL='+%CHAR(W#QTYAVL_NEW)
     C                   EXSR      @LOGCHG
     C                   ENDIF
     C                   ENDSR
      * [修正#R12] 整合性エラー時はログのみ - 異常終了しない
      *            トリガーを *INLR=*ON+RETURN で終了させると
      *            DB2 が MCH1202 を呼び出し元の UPDATE 命令に返すため
      *            ORDERENTRY が RNX1024 で異常終了する
      *            整合性違反の検出はログに記録するにとどめる
     C     @CHKCONS      BEGSR
     C                   EVAL      W#EXPECTED=W#QTYOH_NEW-W#QTYALC_NEW
     C                   IF        W#QTYAVL_NEW <> W#EXPECTED
     C                   EVAL      W#OLDVAL='Expected='
     C                              +%CHAR(W#EXPECTED)
     C                   EVAL      W#NEWVAL='Actual='
     C                              +%CHAR(W#QTYAVL_NEW)
     C                   EXSR      @LOGERR
      * 修正#R12: 異常終了せずに処理を続行（ログのみ）
     C                   ENDIF
     C                   IF        W#QTYALC_NEW > W#QTYOH_NEW
     C                   EVAL      W#OLDVAL='QTYALC='
     C                              +%CHAR(W#QTYALC_NEW)
     C                   EVAL      W#NEWVAL='QTYOH='
     C                              +%CHAR(W#QTYOH_NEW)
     C                   EXSR      @LOGERR
      * 修正#R12: 異常終了せずに処理を続行（ログのみ）
     C                   ENDIF
     C                   ENDSR
     C     @LOGCHG       BEGSR
     C                   EVAL      W#ACTION = 'UPDATE'
     C                   EXSR      @WRTLOG
     C                   ENDSR
     C     @LOGERR       BEGSR
     C                   EVAL      W#ACTION = 'ERROR'
     C                   EXSR      @WRTLOG
     C                   ENDSR
      * 修正#LOGID @WRTLOG - 変更ログ1件の書き込み
     C     @WRTLOG       BEGSR
     C/EXEC SQL
     C+   VALUES NEXT VALUE FOR O2C_LOGID_SEQ INTO :W#LOGID
     C/END-EXEC
      * 採番できない場合は時刻(マイクロ秒)を使い、在庫更新は止めない
     C                   IF        SQLCODE <> 0
     C                   EVAL      W#CNTR = W#CNTR + 1
     C                   EVAL      W#LOGID = %DIFF(WTIMESTAMP:
     C                             Z'2000-01-01-00.00.00.000000':*MS)
     C                             + W#CNTR
     C                   ENDIF
     C                   EVAL      LOGID = W#LOGID
     C                   EVAL      TBLNAME = 'INVENTORYP'
     C                   EVAL      RECID = W#RECKEY
     C                   EVAL      ACTION = W#ACTION
     C                   EVAL      CHGBY = 'TRIGGER'
     C                   EVAL      CHGAT = WTIMESTAMP
     C                   EVAL      OLDVAL = W#OLDVAL
     C                   EVAL      NEWVAL = W#NEWVAL
     C                   WRITE(E)  LOGREC
     C                   ENDSR
     C     @END          BEGSR
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDSR

