     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      *=====================================================================
      * プログラム名: O2CPRMPT
      * 説明: 共通の検索ウィンドウ(各画面のF4から呼ぶ)
      * 作成: 修正#F4P 新規作成
      *   パラメーター
      *     P#TYPE   : CUST=顧客 WH=倉庫 ITEM=商品 INV=請求書
      *                CODE=コードマスター(P#CDTYPEの種別)
      *     P#CDTYPE : コード種別(CODEのとき。例 CURRENCY)
      *     P#VALUE  : 選択したコードを返す(取消のときは変えない)
      *   検索欄: コードの前方一致、または名称の一部
      *   請求書は請求日の新しい順、同じ日は請求IDの新しい順
      *   修正#FLT 請求書は絞込(空白=取消以外 1=未収 2=期限超過
      *            9=全部)と状態の表示、顧客コードでも検索できる
      *   名称は変換せずにバイトのまま読む(コードマスターの説明は
      *   'A'型の列に全角が入っているため、SQLの変換で化けないように)
      *=====================================================================
     FO2CPRMD   CF   E             WORKSTN SFILE(PSFL:W#RRN)

     D W#RRN           S              4S 0
     D W#CNT           S              4S 0
     D W#TYPE          S             10A
     D W#CDTYPE        S             30A
     D W#SRCH          S             40A   VARYING
     D W#NM            S            120A   VARYING
     D W#DONE          S               N
     D W#PFX           S             30A   VARYING
     D W#LO            C                   'abcdefghijklmnopqrstuvwxyz'
     D W#UP            C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D C#ROW           DS
     D C#KEY                         30A
     D C#NAME                       120A
     D C#BAL                         15P 2
     D C#DUE                          8P 0
     D C#STS                         12A
     D C#CC                          20A
     D W#FLT           S              1A
     D W#TODAY         S              8P 0
     D W#LBL           S             10A   VARYING
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
     D DbcsTrunc       PR           256A   VARYING
     D   pStr                       256A   VARYING CONST
     D   pMax                        10I 0 CONST
     D NoShift         PR           256A   VARYING
     D   pStr                       256A   VARYING CONST

     C     *ENTRY        PLIST
     C                   PARM                    P#TYPE           10
     C                   PARM                    P#CDTYPE         30
     C                   PARM                    P#VALUE          30

     C/EXEC SQL
     C+   SET OPTION COMMIT = *NONE, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE LSTCSR CURSOR FOR
     C+   SELECT K, N, B, D, ST, CC FROM (
     C+     SELECT 'CUST' AS T, CAST(CUSTCD AS VARCHAR(30)) AS K,
     C+            CAST(CUSTNM AS VARCHAR(120) FOR BIT DATA) AS N,
     C+            CAST(CUSTCD AS VARCHAR(30)) AS S,
     C+            DEC(0, 15, 2) AS B, DEC(0, 8, 0) AS D,
     C+            CAST(' ' AS CHAR(12)) AS ST, CAST(' ' AS CHAR(20)) AS CC
     C+       FROM CUSTOMERP WHERE STATUS = 'ACTIVE'
     C+     UNION ALL
     C+     SELECT 'WH', CAST(WHCD AS VARCHAR(30)),
     C+            CAST(WHNM AS VARCHAR(120) FOR BIT DATA),
     C+            CAST(WHCD AS VARCHAR(30)), 0, 0, ' ', ' '
     C+       FROM WAREHOUSEP
     C+     UNION ALL
     C+     SELECT 'ITEM', CAST(ITEMCD AS VARCHAR(30)),
     C+            CAST(ITEMDSC AS VARCHAR(120) FOR BIT DATA),
     C+            CAST(ITEMCD AS VARCHAR(30)), 0, 0, ' ', ' '
     C+       FROM ITEMP WHERE STATUS = 'ACTIVE'
     C+     UNION ALL
     C+     SELECT 'INV', CAST(I.INVNUM AS VARCHAR(30)),
     C+            CAST(SUBSTR(DIGITS(I.INVDATE), 1, 4) CONCAT '/' CONCAT
     C+                 SUBSTR(DIGITS(I.INVDATE), 5, 2) CONCAT '/' CONCAT
     C+                 SUBSTR(DIGITS(I.INVDATE), 7, 2) CONCAT ' ' CONCAT
     C+                 C.CUSTNM AS VARCHAR(120) FOR BIT DATA),
     C+            CAST(DIGITS(DEC(99999999 - I.INVDATE, 8, 0)) CONCAT
     C+                 DIGITS(DEC(999999999999999999 - I.INVID, 18, 0))
     C+                 AS VARCHAR(30)),
     C+            I.BALANCE, I.DUEDATE, I.STATUS, C.CUSTCD
     C+       FROM INVOICEHP I JOIN CUSTOMERP C ON C.CUSTID = I.CUSTID
     C+     UNION ALL
     C+     SELECT 'CODE', CAST(CODEVAL AS VARCHAR(30)),
     C+            CAST(DESC AS VARCHAR(120) FOR BIT DATA),
     C+            CAST(DIGITS(DISPORD) AS VARCHAR(30)), 0, 0, ' ', ' '
     C+       FROM CODEMSTRP
     C+      WHERE CODETYPE = :W#CDTYPE AND ISACTIVE = 'Y'
     C+   ) X
     C+    WHERE T = :W#TYPE
     C+    ORDER BY S
     C+    FOR READ ONLY
     C/END-EXEC

     C                   EVAL      W#TYPE = P#TYPE
     C                   EVAL      W#CDTYPE = P#CDTYPE
     C                   SELECT
     C                   WHEN      W#TYPE = 'CUST'
     C                   EVAL      PTITLE = '顧客の検索'
     C                   WHEN      W#TYPE = 'WH'
     C                   EVAL      PTITLE = '倉庫の検索'
     C                   WHEN      W#TYPE = 'ITEM'
     C                   EVAL      PTITLE = '商品の検索'
     C                   WHEN      W#TYPE = 'INV'
     C                   EVAL      PTITLE = '請求書の検索(新しい順)'
     C                   WHEN      W#CDTYPE = 'CURRENCY'
     C                   EVAL      PTITLE = '通貨の検索'
     C                   WHEN      W#CDTYPE = 'PAYMENT_TERM'
     C                   EVAL      PTITLE = '支払条件の検索'
     C                   WHEN      W#CDTYPE = 'PAY_METHOD'
     C                   EVAL      PTITLE = '入金方法の検索'
     C                   OTHER
     C                   EVAL      PTITLE = 'コードの検索'
     C                   ENDSL
     C                   EVAL      PSRCH = *BLANKS
     C                   EVAL      PFLT = ' '
     C                   EVAL      *IN50 = (W#TYPE = 'INV')
     C                   EVAL      W#TODAY = %DEC(%DATE():*ISO)
     C                   EXSR      @LOAD
     C                   DOW       NOT W#DONE
     C                   EVAL      *IN92 = *ON
     C                   EVAL      *IN91 = (W#CNT > 0)
     C                   EVAL      *IN94 = *ON
     C                   EXFMT     PCTL
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
      * 1=選択があればそのコードを返す。なければ検索し直す
     C                   IF        W#CNT > 0
     C                   READC     PSFL
     C                   DOW       NOT %EOF(O2CPRMD)
     C                   IF        POPT = '1'
     C                   EVAL      P#VALUE = PKEYH
     C                   EVAL      W#DONE = *ON
     C                   LEAVE
     C                   ENDIF
     C                   READC     PSFL
     C                   ENDDO
     C                   ENDIF
     C                   IF        NOT W#DONE
     C                   EXSR      @LOAD
     C                   ENDIF
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C                   RETURN

      *=====================================================================
      * @LOAD - 一覧を読み込む(検索欄で絞り込む)
      *=====================================================================
     C     @LOAD         BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     PCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#CNT = 0
     C                   EVAL      PMSG = *BLANKS
      * 全角の検索文字はSO/SIを除いて名称と比べる
     C                   EVAL      W#SRCH = NoShift(%TRIM(PSRCH))
     C                   EVAL      W#FLT = PFLT
     C/EXEC SQL
     C+   OPEN LSTCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#CNT < 999
     C/EXEC SQL
     C+   FETCH LSTCSR INTO :C#ROW
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
      * 修正#FLT: 請求書の絞込
     C                   IF        W#TYPE = 'INV'
     C                   SELECT
     C                   WHEN      W#FLT = ' ' AND C#STS = 'CANCELLED'
     C                   ITER
     C                   WHEN      W#FLT = '1' AND C#BAL <= 0
     C                   ITER
     C                   WHEN      W#FLT = '2' AND
     C                             (C#BAL <= 0 OR C#DUE >= W#TODAY)
     C                   ITER
     C                   ENDSL
     C                   ENDIF
     C                   IF        %LEN(W#SRCH) > 0
     C                   EVAL      W#NM = NoShift(%TRIMR(C#NAME))
      * コードは大文字小文字を区別しない前方一致
     C                   EVAL      W#PFX = %SUBST(C#KEY:1:%MIN(%LEN(W#SRCH):30))
     C                   IF        W#PFX <> %XLATE(W#LO:W#UP:W#SRCH)
     C                             AND %SCAN(W#SRCH:W#NM) = 0
     C                             AND (W#TYPE <> 'INV' OR
     C                             %SUBST(C#CC:1:%MIN(%LEN(W#SRCH):20)) <>
     C                             %XLATE(W#LO:W#UP:W#SRCH))
     C                   ITER
     C                   ENDIF
     C                   ENDIF
     C                   EVAL      W#CNT = W#CNT + 1
     C                   EVAL      POPT = ' '
     C                   EVAL      PKEYH = C#KEY
     C                   EVAL      PKEY = C#KEY
     C                   IF        W#TYPE = 'INV'
      * 請求日の後ろに状態(取消/入金済/期限超過/未収)を付ける
     C                   SELECT
     C                   WHEN      C#STS = 'CANCELLED'
     C                   EVAL      W#LBL = '取消'
     C                   WHEN      C#BAL <= 0
     C                   EVAL      W#LBL = '入金済'
     C                   WHEN      C#DUE < W#TODAY
     C                   EVAL      W#LBL = '期限超過'
     C                   OTHER
     C                   EVAL      W#LBL = '未収'
     C                   ENDSL
     C                   EVAL      PNAME = DbcsTrunc(%SUBST(C#NAME:1:11) +
     C                             W#LBL + ' ' + %TRIMR(%SUBST(C#NAME:12)):38)
     C                   ELSE
     C                   EVAL      PNAME = DbcsTrunc(%TRIMR(C#NAME):38)
     C                   ENDIF
     C                   EVAL      W#RRN = W#CNT
     C                   WRITE     PSFL
     C                   ENDDO
     C/EXEC SQL
     C+   CLOSE LSTCSR
     C/END-EXEC
     C                   IF        W#CNT = 0
     C                   EVAL      PMSG = '該当するものがありません'
     C                   ENDIF
     C                   ENDSR

      * NoShift - SO/SIを取り除く(全角どうしの部分一致用)
     P NoShift         B
     D NoShift         PI           256A   VARYING
     D   pStr                       256A   VARYING CONST
     C                   RETURN    %SCANRPL(W#SI:'':%SCANRPL(W#SO:'':pStr))
     P NoShift         E

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
