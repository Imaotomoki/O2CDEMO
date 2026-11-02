     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: CHGLOGINQ
      * 説明: 変更履歴の照会
      * 作成: 修正#LOG 新規作成。マスター保守メニューの8から呼ぶ
      *   CHANGELOGP(トリガー O2CTRIG と在庫トリガー INVTRIGGER が書く)
      *   を新しい順に表示する。ファイル名と番号(前方一致)で絞り込み
      *   5=内容(変更前・変更後の値)を表示
      *=====================================================================
     FCHGLOGD   CF   E             WORKSTN SFILE(LSFL:W#RRN)

     D W#RRN           S              4S 0
     D W#CNT           S              4S 0
     D W#SEL           S             18P 0
     D H#TBL           S             10A
     D H#KEY           S             21A
     D H#ID            S             18P 0
     D H#TNM           S             30A
     D H#RID           S             30A
     D H#ACT           S             10A
     D H#BY            S             30A
     D H#AT            S               Z
     D H#OLD           S           1024A
     D H#NEW           S           1024A
     D W#TS            S             26A

     C/EXEC SQL
     C+   SET OPTION COMMIT = *NONE, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE LOGCSR CURSOR FOR
     C+   SELECT LOGID, CAST(TBLNAME AS CHAR(30) FOR BIT DATA), RECID,
     C+          ACTION, CAST(CHGBY AS CHAR(30) FOR BIT DATA), CHGAT
     C+     FROM CHANGELOGP
     C+    WHERE (:H#TBL = ' '
     C+           OR CAST(TBLNAME AS CHAR(30) FOR BIT DATA) = :H#TBL)
     C+      AND (:H#KEY = ' ' OR RECID LIKE TRIM(:H#KEY) CONCAT '%')
     C+    ORDER BY CHGAT DESC, LOGID DESC
     C+    FETCH FIRST 999 ROWS ONLY
     C+    FOR READ ONLY
     C/END-EXEC

     C                   EXSR      @LOAD
     C                   DOW       *ON
     C                   EVAL      *IN91 = (W#CNT > 0)
     C                   EVAL      *IN94 = *ON
     C                   WRITE     LFOOT
     C                   EXFMT     LCTL
     C                   EVAL      LMSG = *BLANKS
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      W#SEL = 0
     C                   IF        W#CNT > 0
     C                   READC     LSFL
     C                   DOW       NOT %EOF(CHGLOGD)
     C                   IF        LOPT = '5' AND W#SEL = 0
     C                   EVAL      W#SEL = LID
     C                   ENDIF
     C                   READC     LSFL
     C                   ENDDO
     C                   ENDIF
     C                   IF        W#SEL > 0
     C                   EXSR      @DETAIL
     C                   ENDIF
     C                   EXSR      @LOAD
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C                   RETURN

      * @LOAD - 一覧
     C     @LOAD         BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     LCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#CNT = 0
     C                   EVAL      H#TBL = LFTBL
     C                   EVAL      H#KEY = LFKEY
     C/EXEC SQL
     C+   OPEN LOGCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#CNT < 999
     C/EXEC SQL
     C+   FETCH LOGCSR INTO :H#ID, :H#TNM, :H#RID, :H#ACT, :H#BY, :H#AT
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      LOPT = ' '
     C                   EVAL      LID = H#ID
     C                   EVAL      W#TS = %CHAR(H#AT)
     C                   EVAL      LAT = %SUBST(W#TS:1:10) + ' ' +
     C                             %SUBST(W#TS:12:2) + ':' +
     C                             %SUBST(W#TS:15:2)
     C                   EVAL      LTBL = H#TNM
     C                   EVAL      LKEY = H#RID
     C                   EVAL      LACT = H#ACT
     C                   EVAL      LUSR = H#BY
     C                   EVAL      W#CNT = W#CNT + 1
     C                   EVAL      W#RRN = W#CNT
     C                   WRITE     LSFL
     C                   ENDDO
     C                   IF        SQLCODE < 0
     C                   EVAL      LMSG = '読み込みエラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   ENDIF
     C/EXEC SQL
     C+   CLOSE LOGCSR
     C/END-EXEC
     C                   IF        W#CNT = 0 AND LMSG = *BLANKS
     C                   EVAL      LMSG = '変更履歴はありません'
     C                   ENDIF
     C                   ENDSR

      * @DETAIL - 内容の表示
     C     @DETAIL       BEGSR
     C/EXEC SQL
     C+   SELECT CAST(TBLNAME AS CHAR(30) FOR BIT DATA), RECID, ACTION,
     C+          CAST(CHGBY AS CHAR(30) FOR BIT DATA), CHGAT,
     C+          OLDVAL, NEWVAL
     C+     INTO :H#TNM, :H#RID, :H#ACT, :H#BY, :H#AT, :H#OLD, :H#NEW
     C+     FROM CHANGELOGP WHERE LOGID = :W#SEL
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      LMSG = '変更履歴が見つかりません'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      DAT = %CHAR(H#AT)
     C                   EVAL      DTBL = H#TNM
     C                   EVAL      DKEY = H#RID
     C                   EVAL      DACT = H#ACT
     C                   EVAL      DUSR = H#BY
     C                   EVAL      DOLD1 = %SUBST(H#OLD:1:78)
     C                   EVAL      DOLD2 = %SUBST(H#OLD:79:78)
     C                   EVAL      DNEW1 = %SUBST(H#NEW:1:78)
     C                   EVAL      DNEW2 = %SUBST(H#NEW:79:78)
     C                   EXFMT     DTL
     C                   ENDSR
