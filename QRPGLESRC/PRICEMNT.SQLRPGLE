     H DFTACTGRP(*NO) ACTGRP(*NEW) DECEDIT('0.')
      *=====================================================================
      * プログラム名: PRICEMNT
      * 説明: 顧客別単価・数量割引の保守(登録・変更・削除)
      * 作成: 修正#PRC2 新規作成。マスター保守メニューから呼ぶ
      *   2=変更 4=削除 F6=新規。顧客が空白なら全顧客向け(CUSTID 0)
      *   受注入力で単価を空白にすると、顧客別>全顧客、最低数量の
      *   大きいもの、開始日の新しいものの順で単価を決める
      *   単価IDは O2C_PRICEID_SEQ で採番する
      *=====================================================================
     FPRICEMNTD CF   E             WORKSTN SFILE(LSFL:W#RRN)

     D W#RRN           S              4S 0
     D W#CNT           S              4S 0
     D W#SQLCD         S             10I 0
     D W#DONE          S               N
     D W#ERR           S               N
     D W#ADD           S               N
     D W#SEL           S             18P 0
     D W#OPT           S              1A
     D W#N             S             10I 0
     D W#PVAL          S             30A
     D W#USER          S             10A   INZ(*USER)
     D W#D8            S              8P 0
     D W#T6            S              6P 0
     D W#P2            S             11P 2
     D H#F             S             20A
     D H#CUSTID        S              9P 0
     D H#ITEMID        S              9P 0
     D H#MINQ          S             15P 3
     D H#FROM          S              8P 0
     D H#TO            S              8P 0
     D H#PRC           S             15P 4
     D H#CUSTCD        S             20A
     D H#ITEMCD        S             12A
     D H#NM            S             60A
     D S#ID            S             18P 0
     D S#CUST          S             20A
     D S#CUSTID        S              9P 0
     D S#ITEM          S             12A
     D S#MINQ          S             15P 3
     D S#FROM          S              8P 0
     D S#TO            S              8P 0
     D S#PRC           S             15P 4
     D O2CPRMPT        PR                  EXTPGM('O2CPRMPT')
     D   PTYPE                       10A   CONST
     D   PCDTYPE                     30A   CONST
     D   PVALUE                      30A
     D W#SO            C                   X'0E'
     D W#SI            C                   X'0F'
     D DBCSTRUNC       PR           256A   VARYING
     D   PSTR                       256A   VARYING CONST
     D   PMAX                        10I 0 CONST

     C/EXEC SQL
     C+   SET OPTION COMMIT = *NONE, CLOSQLCSR = *ENDMOD
     C/END-EXEC

     C/EXEC SQL
     C+   DECLARE LSTCSR CURSOR FOR
     C+   SELECT P.PRICEID, COALESCE(C.CUSTCD, ' '), P.CUSTID,
     C+          COALESCE(I.ITEMCD, ' '), P.MINQTY, P.FROMDATE,
     C+          P.TODATE, P.UNITPRC
     C+     FROM PRICEP P
     C+     LEFT JOIN CUSTOMERP C ON C.CUSTID = P.CUSTID
     C+     LEFT JOIN ITEMP I ON I.ITEMID = P.ITEMID
     C+    WHERE :H#F = ' '
     C+       OR (:H#F = '*' AND P.CUSTID = 0)
     C+       OR C.CUSTCD = :H#F
     C+    ORDER BY I.ITEMCD, P.CUSTID, P.MINQTY, P.FROMDATE
     C+    FOR READ ONLY
     C/END-EXEC

     C                   EVAL      LMSG = *BLANKS
     C                   EXSR      @LOAD
     C                   DOW       NOT W#DONE
     C                   EVAL      *IN92 = *ON
     C                   EVAL      *IN91 = (W#CNT > 0)
     C                   EVAL      *IN94 = *ON
     C                   WRITE     LFOOT
     C                   EXFMT     LCTL
     C                   EVAL      LMSG = *BLANKS
     C                   IF        *IN03 OR *IN12
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN04
     C                   EVAL      W#PVAL = LFCUST
     C                   CALLP     O2CPRMPT('CUST':' ':W#PVAL)
     C                   EVAL      LFCUST = W#PVAL
     C                   EXSR      @LOAD
     C                   ITER
     C                   ENDIF
     C                   IF        *IN06
     C                   EVAL      W#ADD = *ON
     C                   EVAL      W#SEL = 0
     C                   EXSR      @EDIT
     C                   EXSR      @LOAD
     C                   ITER
     C                   ENDIF
     C                   EVAL      W#SEL = 0
     C                   IF        W#CNT > 0
     C                   READC     LSFL
     C                   DOW       NOT %EOF(PRICEMNTD)
     C                   IF        LOPT <> ' ' AND W#SEL = 0
     C                   EVAL      W#SEL = LID
     C                   EVAL      W#OPT = LOPT
     C                   ENDIF
     C                   READC     LSFL
     C                   ENDDO
     C                   ENDIF
     C                   IF        W#SEL > 0
     C                   IF        W#OPT = '2'
     C                   EVAL      W#ADD = *OFF
     C                   EXSR      @EDIT
     C                   ELSE
     C                   EXSR      @DELETE
     C                   ENDIF
     C                   ENDIF
     C                   EXSR      @LOAD
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C                   RETURN

      * @LOAD - 単価の一覧
     C     @LOAD         BEGSR
     C                   EVAL      *IN93 = *ON
     C                   WRITE     LCTL
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      W#CNT = 0
     C                   EVAL      H#F = %TRIM(LFCUST)
     C/EXEC SQL
     C+   OPEN LSTCSR
     C/END-EXEC
     C                   DOW       SQLCODE = 0 AND W#CNT < 999
     C/EXEC SQL
     C+   FETCH LSTCSR INTO :S#ID, :S#CUST, :S#CUSTID, :S#ITEM, :S#MINQ,
     C+                     :S#FROM, :S#TO, :S#PRC
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      LOPT = ' '
     C                   EVAL      LID = S#ID
     C                   IF        S#CUSTID = 0
     C                   EVAL      LCUST = '*全顧客'
     C                   ELSE
     C                   EVAL      LCUST = S#CUST
     C                   ENDIF
     C                   EVAL      LITEM = S#ITEM
     C                   EVALR     LMINQ = %TRIM(%EDITC(%INT(S#MINQ):'3'))
     C                   EVAL      LFROM = %CHAR(%DATE(S#FROM:*ISO):*ISO)
     C                   IF        S#TO = 0
     C                   EVAL      LTO = '期限なし'
     C                   ELSE
     C                   EVAL      LTO = %CHAR(%DATE(S#TO:*ISO):*ISO)
     C                   ENDIF
     C                   EVAL(H)   W#P2 = S#PRC
     C                   EVALR     LPRC = %TRIM(%EDITC(W#P2:'J'))
     C                   EVAL      W#CNT = W#CNT + 1
     C                   EVAL      W#RRN = W#CNT
     C                   WRITE     LSFL
     C                   ENDDO
     C                   EVAL      W#SQLCD = SQLCODE
     C/EXEC SQL
     C+   CLOSE LSTCSR
     C/END-EXEC
     C                   IF        W#SQLCD < 0
     C                   EVAL      LMSG = '読み込みエラー SQLCODE=' +
     C                             %CHAR(W#SQLCD)
     C                   ELSEIF    W#CNT = 0 AND LMSG = *BLANKS
     C                   EVAL      LMSG = '単価が登録されていません'
     C                   ENDIF
     C                   ENDSR

      * @DELETE - 削除(受注の単価は受注明細に残っているので影響しない)
     C     @DELETE       BEGSR
     C/EXEC SQL
     C+   DELETE FROM PRICEP WHERE PRICEID = :W#SEL
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      LMSG = '削除エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   ELSE
     C                   EVAL      LMSG = '単価を削除しました'
     C                   ENDIF
     C                   ENDSR

      * @EDIT - 登録・変更
     C     @EDIT         BEGSR
     C                   EVAL      DMSG = *BLANKS
     C                   IF        W#ADD
     C                   EVAL      DMODE = '単価の登録'
     C                   EVAL      DCUST = %TRIM(LFCUST)
     C                   IF        DCUST = '*'
     C                   EVAL      DCUST = *BLANKS
     C                   ENDIF
     C                   EVAL      DITEM = *BLANKS
     C                   EVAL      DMINQ = 0
     C                   EVAL      DFROM = %DEC(%DATE():*ISO)
     C                   EVAL      DTO = 0
     C                   EVAL      DPRC = 0
     C                   ELSE
     C                   EVAL      DMODE = '単価の変更'
     C/EXEC SQL
     C+   SELECT COALESCE(C.CUSTCD, ' '), COALESCE(I.ITEMCD, ' '),
     C+          P.MINQTY, P.FROMDATE, P.TODATE, P.UNITPRC
     C+     INTO :H#CUSTCD, :H#ITEMCD, :H#MINQ, :H#FROM, :H#TO, :H#PRC
     C+     FROM PRICEP P
     C+     LEFT JOIN CUSTOMERP C ON C.CUSTID = P.CUSTID
     C+     LEFT JOIN ITEMP I ON I.ITEMID = P.ITEMID
     C+    WHERE P.PRICEID = :W#SEL
     C/END-EXEC
     C                   IF        SQLCODE <> 0
     C                   EVAL      LMSG = '単価が見つかりません'
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      DCUST = H#CUSTCD
     C                   EVAL      DITEM = H#ITEMCD
     C                   EVAL(H)   DMINQ = H#MINQ
     C                   EVAL      DFROM = H#FROM
     C                   EVAL      DTO = H#TO
     C                   EVAL(H)   DPRC = H#PRC
     C                   ENDIF
     C                   EXSR      @NAMES
     C                   DOW       *ON
     C                   EXFMT     DTL
     C                   EVAL      DMSG = *BLANKS
     C                   IF        *IN03
     C                   EVAL      W#DONE = *ON
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN12
     C                   LEAVE
     C                   ENDIF
     C                   IF        *IN04
     C                   EXSR      @PROMPT
     C                   ITER
     C                   ENDIF
     C                   EXSR      @CHECK
     C                   IF        W#ERR
     C                   ITER
     C                   ENDIF
     C                   EXSR      @SAVE
     C                   IF        NOT W#ERR
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR

      * @PROMPT - F4(カーソル位置の項目を検索)
     C     @PROMPT       BEGSR
     C                   SELECT
     C                   WHEN      CSRFLD = 'DCUST'
     C                   EVAL      W#PVAL = DCUST
     C                   CALLP     O2CPRMPT('CUST':' ':W#PVAL)
     C                   EVAL      DCUST = W#PVAL
     C                   WHEN      CSRFLD = 'DITEM'
     C                   EVAL      W#PVAL = DITEM
     C                   CALLP     O2CPRMPT('ITEM':' ':W#PVAL)
     C                   EVAL      DITEM = W#PVAL
     C                   OTHER
     C                   EVAL      DMSG = '顧客か商品にカーソルを置いてF4'
     C                   ENDSL
     C                   EXSR      @NAMES
     C                   ENDSR

      * @NAMES - 顧客名・商品名(IDも取っておく)
     C     @NAMES        BEGSR
     C                   EVAL      DCUSTNM = *BLANKS
     C                   EVAL      DITEMNM = *BLANKS
     C                   EVAL      H#CUSTID = -1
     C                   EVAL      H#ITEMID = -1
     C                   IF        DCUST = *BLANKS
     C                   EVAL      H#CUSTID = 0
     C                   EVAL      DCUSTNM = '(全顧客向け)'
     C                   ELSE
     C/EXEC SQL
     C+   SELECT CUSTID, CUSTNM INTO :H#CUSTID, :H#NM
     C+     FROM CUSTOMERP WHERE CUSTCD = :DCUST
     C/END-EXEC
     C                   IF        SQLCODE = 0
     C                   EVAL      DCUSTNM = DBCSTRUNC(%TRIMR(H#NM):40)
     C                   ELSE
     C                   EVAL      H#CUSTID = -1
     C                   ENDIF
     C                   ENDIF
     C                   IF        DITEM <> *BLANKS
     C/EXEC SQL
     C+   SELECT ITEMID, ITEMDSC INTO :H#ITEMID, :H#NM
     C+     FROM ITEMP WHERE ITEMCD = :DITEM
     C/END-EXEC
     C                   IF        SQLCODE = 0
     C                   EVAL      DITEMNM = DBCSTRUNC(%TRIMR(H#NM):40)
     C                   ELSE
     C                   EVAL      H#ITEMID = -1
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR

      * @CHECK - 入力チェック
     C     @CHECK        BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EVAL      *IN31 = *OFF
     C                   EVAL      *IN32 = *OFF
     C                   EVAL      *IN33 = *OFF
     C                   EVAL      *IN34 = *OFF
     C                   EVAL      *IN35 = *OFF
     C                   EVAL      *IN36 = *OFF
     C                   EXSR      @NAMES
     C     *ISO          TEST(DE)                DFROM
     C                   IF        %ERROR
     C                   EVAL      *IN34 = *ON
     C                   EVAL      DMSG = '開始日が正しくありません'
     C                   ENDIF
     C                   IF        DMSG = *BLANKS AND DTO <> 0
     C     *ISO          TEST(DE)                DTO
     C                   IF        %ERROR
     C                   EVAL      *IN35 = *ON
     C                   EVAL      DMSG = '終了日が正しくありません'
     C                   ENDIF
     C                   ENDIF
     C                   IF        DMSG = *BLANKS
     C                   SELECT
     C                   WHEN      H#CUSTID < 0
     C                   EVAL      *IN31 = *ON
     C                   EVAL      DMSG = '顧客が見つかりません'
     C                   WHEN      DITEM = *BLANKS
     C                   EVAL      *IN32 = *ON
     C                   EVAL      DMSG = '商品コードを入力してください'
     C                   WHEN      H#ITEMID < 0
     C                   EVAL      *IN32 = *ON
     C                   EVAL      DMSG = '商品が見つかりません'
     C                   WHEN      DMINQ < 0
     C                   EVAL      *IN33 = *ON
     C                   EVAL      DMSG = '最低数量は0以上にしてください'
     C                   WHEN      DTO <> 0 AND DTO < DFROM
     C                   EVAL      *IN35 = *ON
     C                   EVAL      DMSG = '終了日は開始日以降にしてください'
     C                   WHEN      DPRC <= 0
     C                   EVAL      *IN36 = *ON
     C                   EVAL      DMSG = '単価を入力してください'
     C                   ENDSL
     C                   ENDIF
     C                   IF        DMSG <> *BLANKS
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
      * 同じ顧客・商品・最低数量・開始日の単価は1件だけ
     C                   EVAL      H#MINQ = DMINQ
     C                   EVAL      H#FROM = DFROM
     C/EXEC SQL
     C+   SELECT COUNT(*) INTO :W#N FROM PRICEP
     C+    WHERE CUSTID = :H#CUSTID AND ITEMID = :H#ITEMID
     C+      AND MINQTY = :H#MINQ AND FROMDATE = :H#FROM
     C+      AND PRICEID <> :W#SEL
     C/END-EXEC
     C                   IF        SQLCODE <> 0 OR W#N > 0
     C                   EVAL      *IN34 = *ON
     C                   EVAL      DMSG = '同じ条件の単価が登録済みです'
     C                   EVAL      W#ERR = *ON
     C                   ENDIF
     C                   ENDSR

      * @SAVE - 登録・更新
     C     @SAVE         BEGSR
     C                   EVAL      W#ERR = *OFF
     C                   EXSR      @STAMP
     C                   EVAL      H#MINQ = DMINQ
     C                   EVAL      H#FROM = DFROM
     C                   EVAL      H#TO = DTO
     C                   EVAL      H#PRC = DPRC
     C                   IF        W#ADD
     C/EXEC SQL
     C+   INSERT INTO PRICEP (PRICEID, CUSTID, ITEMID, MINQTY, FROMDATE,
     C+          TODATE, UNITPRC, CRTUSER, CRTDATE, CRTTIM, UPDUSER,
     C+          UPDDATE, UPDTIM)
     C+   VALUES (NEXT VALUE FOR O2C_PRICEID_SEQ, :H#CUSTID, :H#ITEMID,
     C+          :H#MINQ, :H#FROM, :H#TO, :H#PRC, :W#USER, :W#D8, :W#T6,
     C+          :W#USER, :W#D8, :W#T6)
     C/END-EXEC
     C                   ELSE
     C/EXEC SQL
     C+   UPDATE PRICEP
     C+      SET CUSTID = :H#CUSTID, ITEMID = :H#ITEMID,
     C+          MINQTY = :H#MINQ, FROMDATE = :H#FROM, TODATE = :H#TO,
     C+          UNITPRC = :H#PRC,
     C+          UPDUSER = :W#USER, UPDDATE = :W#D8, UPDTIM = :W#T6
     C+    WHERE PRICEID = :W#SEL
     C/END-EXEC
     C                   ENDIF
     C                   IF        SQLCODE <> 0
     C                   EVAL      DMSG = '登録エラー SQLCODE=' +
     C                             %CHAR(SQLCODE)
     C                   EVAL      W#ERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   IF        W#ADD
     C                   EVAL      LMSG = %TRIM(DITEM) + ' を登録しました'
     C                   ELSE
     C                   EVAL      LMSG = %TRIM(DITEM) + ' を変更しました'
     C                   ENDIF
     C                   ENDSR

      * @STAMP - 更新日時
     C     @STAMP        BEGSR
     C                   EVAL      W#D8 = %DEC(%DATE():*ISO)
     C                   EVAL      W#T6 = %DEC(%TIME():*HMS)
     C                   ENDSR

      *=====================================================================
      * DBCSTRUNC - 全角(SO/SI)を壊さずに指定バイト数以内に切り詰める
      *=====================================================================
     P DBCSTRUNC       B
     D DBCSTRUNC       PI           256A   VARYING
     D   PSTR                       256A   VARYING CONST
     D   PMAX                        10I 0 CONST
     D WOUT            S            256A   VARYING
     D WCH             S              1A
     D WI              S             10I 0
     D WLEN            S             10I 0
     D WDBCS           S               N
     C                   EVAL      WOUT = ''
     C                   EVAL      WDBCS = *OFF
     C                   EVAL      WLEN = %LEN(%TRIMR(PSTR))
     C                   EVAL      WI = 1
     C                   DOW       WI <= WLEN
     C                   EVAL      WCH = %SUBST(PSTR:WI:1)
     C                   SELECT
     C                   WHEN      WCH = W#SO
     C                   IF        %LEN(WOUT) + 3 > PMAX
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      WOUT = WOUT + WCH
     C                   EVAL      WDBCS = *ON
     C                   EVAL      WI = WI + 1
     C                   WHEN      WCH = W#SI
     C                   EVAL      WOUT = WOUT + WCH
     C                   EVAL      WDBCS = *OFF
     C                   EVAL      WI = WI + 1
     C                   WHEN      WDBCS
     C                   IF        %LEN(WOUT) + 3 > PMAX OR WI + 1 > WLEN
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      WOUT = WOUT + %SUBST(PSTR:WI:2)
     C                   EVAL      WI = WI + 2
     C                   OTHER
     C                   IF        %LEN(WOUT) + 1 > PMAX
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      WOUT = WOUT + WCH
     C                   EVAL      WI = WI + 1
     C                   ENDSL
     C                   ENDDO
     C                   IF        WDBCS
     C                   EVAL      WOUT = WOUT + W#SI
     C                   ENDIF
     C                   RETURN    WOUT
     P DBCSTRUNC       E
