     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: USERMNT
      * 説明: ユーザーとメニュー権限の保守
      * 作成: 修正#AUT 新規作成。マスター保守メニューの7から呼ぶ
      *   ユーザープロファイルごとに、メニューの機能(プログラム)を
      *   使える(Y)・使えない(N)を決める。MENUAUTP に保存する
      *   F11 でそのユーザーの行を消すと制限なし(全部使える)
      *   メニュー(O2CMENU・MSTMENU)は CHKAUTH で確認してから呼ぶ
      *=====================================================================
     FUSERMNTD  CF   E             WORKSTN SFILE(LSFL:W#RRN)

     D W#RRN           S              4S 0
     D W#I             S             10I 0
     D W#N             S             10I 0
     D W#USER          S             10A   INZ(*USER)
     D W#LOADED        S             10A
     D W#D8            S              8P 0
     D H#USR           S             10A
     D H#PGM           S             10A
     D H#ALLOW         S              1A
      * メニューの機能(番号・プログラム・名前)
     D W#TBL           S             50A   DIM(32) CTDATA PERRCD(1)
     D W#MENU          S              4A
     D W#PGM           S             10A
     D W#NAME          S             32A

     C/EXEC SQL
     C+   SET OPTION COMMIT = *NONE, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C                   EVAL      LUSER = *BLANKS
     C                   EVAL      W#LOADED = *BLANKS
     C                   EVAL      LMSG = 'ユーザーを入れて Enter' +
     C                             'を押してください'
     C                   EXSR      @CLEAR
     C                   DOW       *ON
     C                   EVAL      *IN91 = (W#LOADED <> *BLANKS)
     C                   EVAL      *IN94 = *ON
     C                   WRITE     LFOOT
     C                   EXFMT     LCTL
     C                   EVAL      LMSG = *BLANKS
     C                   EVAL      *IN31 = *OFF
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
     C                   IF        LUSER = *BLANKS
     C                   EVAL      *IN31 = *ON
     C                   EVAL      LMSG = 'ユーザーを入れてください'
     C                   ITER
     C                   ENDIF
     C                   EVAL      H#USR = LUSER
      * F11 制限を外す
     C                   IF        *IN11
     C/EXEC SQL
     C+   DELETE FROM MENUAUTP WHERE USRPRF = :H#USR
     C/END-EXEC
     C                   EXSR      @LOAD
     C                   EVAL      LMSG = %TRIM(LUSER) + ' の制限を外しました'
     C                   ITER
     C                   ENDIF
      * ユーザーを変えたら表示、同じなら保存
     C                   IF        LUSER <> W#LOADED
     C                   EXSR      @LOAD
     C                   ITER
     C                   ENDIF
     C                   EXSR      @SAVE
     C                   EXSR      @LOAD
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C                   RETURN

      * @CLEAR - 一覧を消す
     C     @CLEAR        BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     LCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      LSTATE = *BLANKS
     C                   ENDSR

      * @LOAD - ユーザーの権限を表示(行がない機能は Y)
     C     @LOAD         BEGSR
     C                   EXSR      @CLEAR
     C                   EVAL      W#N = 0
     C/EXEC SQL
     C+   SELECT COUNT(*) INTO :W#N FROM MENUAUTP WHERE USRPRF = :H#USR
     C/END-EXEC
     C                   IF        W#N = 0
     C                   EVAL      LSTATE = '制限なし(全部使える)'
     C                   ELSE
     C                   EVAL      LSTATE = '制限あり'
     C                   ENDIF
     C                   FOR       W#I = 1 TO 32
     C                   EXSR      @SPLIT
     C                   EVAL      LMENU = W#MENU
     C                   EVAL      LPGM = W#PGM
     C                   EVAL      LNAME = W#NAME
     C                   EVAL      H#PGM = W#PGM
     C                   EVAL      H#ALLOW = 'Y'
     C/EXEC SQL
     C+   SELECT ALLOW INTO :H#ALLOW FROM MENUAUTP
     C+    WHERE USRPRF = :H#USR AND PGMNAME = :H#PGM
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   IF        W#N = 0
     C                   EVAL      H#ALLOW = 'Y'
     C                   ELSE
     C                   EVAL      H#ALLOW = 'N'
     C                   ENDIF
     C                   ENDIF
     C                   EVAL      LALLOW = H#ALLOW
     C                   EVAL      *IN61 = (H#ALLOW = 'N')
     C                   EVAL      W#RRN = W#I
     C                   WRITE     LSFL
     C                   ENDFOR
     C                   EVAL      *IN61 = *OFF
     C                   EVAL      W#LOADED = LUSER
     C                   ENDSR

      * @SAVE - 表示中の権限を保存(全部 Y なら制限なしにする)
     C     @SAVE         BEGSR
     C                   EVAL      W#D8 = %DEC(%DATE():*ISO)
     C                   EVAL      W#N = 0
     C                   FOR       W#I = 1 TO 32
     C     W#I           CHAIN     LSFL
     C                   IF        %FOUND(USERMNTD) AND LALLOW = 'N'
     C                   EVAL      W#N = W#N + 1
     C                   ENDIF
     C                   ENDFOR
     C/EXEC SQL
     C+   DELETE FROM MENUAUTP WHERE USRPRF = :H#USR
     C/END-EXEC
     C                   IF        W#N = 0
     C                   EVAL      LMSG = %TRIM(LUSER) +
     C                             ' を制限なしにしました'
     C                   LEAVESR
     C                   ENDIF
     C                   FOR       W#I = 1 TO 32
     C     W#I           CHAIN     LSFL
     C                   IF        NOT %FOUND(USERMNTD)
     C                   ITER
     C                   ENDIF
     C                   EVAL      H#PGM = LPGM
     C                   EVAL      H#ALLOW = LALLOW
     C/EXEC SQL
     C+   INSERT INTO MENUAUTP (USRPRF, PGMNAME, ALLOW, UPDUSER, UPDDATE)
     C+   VALUES (:H#USR, :H#PGM, :H#ALLOW, :W#USER, :W#D8)
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      LMSG = '保存エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   LEAVESR
     C                   ENDIF
     C                   ENDFOR
     C                   EVAL      LMSG = %TRIM(LUSER) +
     C                             ' の権限を保存しました(' +
     C                             '使えない機能 ' + %CHAR(W#N) + ' 件)'
     C                   ENDSR

      * @SPLIT - 表の1行(番号 4桁・プログラム 10桁・名前)を分ける
     C     @SPLIT        BEGSR
     C                   EVAL      W#MENU = %SUBST(W#TBL(W#I):1:4)
     C                   EVAL      W#PGM = %SUBST(W#TBL(W#I):5:10)
     C                   EVAL      W#NAME = %SUBST(W#TBL(W#I):16:32)
     C                   ENDSR
** CTDATA W#TBL
1   ORDERENTRY 受注入力
2   ORDMAINT   受注照会・変更・取消
3   CNFSHPENT  出荷確定
4   STKMAINT   在庫入庫・棚卸
5   INVINQ     請求書の照会・印刷・取消
6   BILLCLOSE  請求締め
7   PAYENTRY   入金処理
8   ARAGING    売掛金年齢表
9   DUNNING    督促状発行
10  PAYCANCEL  入金取消
11  SHPCANCEL  出荷取消
12  STKINQ     在庫照会
13  SALESUM    売上・入金の月次集計
14  CUSTINQ    顧客の与信・残高照会
15  CHKDATA    データ整合性チェック
16  SHPDUE     出荷予定
17  MSTMENU    マスター保守
18  QUOTENT    見積入力・受注への変換
19  RETENTRY   返品・値引き(赤伝)
20  PAYIMP     入金データの取込・消込
21  POENTRY    発注点・発注・入荷
22  STKXFER    倉庫間の在庫移動
23  MARGIN     粗利分析
24  GLEXPORT   会計仕訳データの出力
M1  CUSTMNT    顧客マスター
M2  ITEMMNT    商品マスター
M3  WHMNT      倉庫マスター
M4  FXMNT      為替レート
M5  CODEMNT    コードマスター
M6  PRICEMNT   顧客別単価
M7  USERMNT    ユーザーとメニュー権限
M8  CHGLOGINQ  変更履歴の照会
