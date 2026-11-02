     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      *=====================================================================
      * プログラム名: CHKAUTH
      * 説明: メニューの権限の確認
      * 作成: 修正#AUT 新規作成。O2CMENU・MSTMENU から呼ぶ
      *   CALL CHKAUTH PARM(プログラム名 結果) Y=使える N=使えない
      *   MENUAUTP にそのユーザーの行が1件もなければ Y(制限なし)
      *=====================================================================
     D P#PGM           S             10A
     D P#OK            S              1A
     D W#N             S             10I 0
     D W#ALLOW         S              1A

     C     *ENTRY        PLIST
     C                   PARM                    P#PGM
     C                   PARM                    P#OK

     C/EXEC SQL
     C+   SET OPTION COMMIT = *NONE, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C                   EVAL      P#OK = 'Y'
     C                   EVAL      W#N = 0
     C/EXEC SQL
     C+   SELECT COUNT(*) INTO :W#N FROM MENUAUTP WHERE USRPRF = USER
     C/END-EXEC
     C                   IF        SQLCODE = 0 AND W#N > 0
     C                   EVAL      W#ALLOW = 'N'
     C/EXEC SQL
     C+   SELECT ALLOW INTO :W#ALLOW FROM MENUAUTP
     C+    WHERE USRPRF = USER AND PGMNAME = :P#PGM
     C/END-EXEC
     C                   IF        W#ALLOW <> 'Y'
     C                   EVAL      P#OK = 'N'
     C                   ENDIF
     C                   ENDIF
     C                   RETURN
