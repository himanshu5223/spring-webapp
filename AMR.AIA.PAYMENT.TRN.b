SUBROUTINE AMR.AIA.PAYMENT.TRN
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Code Review_Amendment Date :   24-12-2020
* Kiran HS      04-Jan-2021     Review
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.AMR.OFS.PARAMETER
    $INSERT I_F.AMR.AIA.PARAM
    $INSERT I_F.CUSTOMER
    $INSERT I_F.DATES
    $INSERT I_F.USER
* INCLUDE JBC.h
    
    GOSUB INITALIZE
    GOSUB PROCESS
RETURN

****************
INITALIZE:
****************

    FN.FT = 'F.FUNDS.TRANSFER'
    FL.FT = ''
    CALL OPF(FN.FT, FL.FT)

    FN.DATES = 'F.DATES'
    FL.DATES = ''
    CALL OPF(FN.DATES, FL.DATES)

    FN.AMR.PARA = 'F.AMR.OFS.PARAMETER'
    FV.AMR.PARA = ''
    CALL OPF(FN.AMR.PARA,FV.AMR.PARA)
    
    FN.AIA.PARAM = 'F.AMR.AIA.PARAM'
    F.AIA.PARAM = ''
    CALL OPF(FN.AIA.PARAM,F.AIA.PARAM)

    FN.ACC = 'F.ACCOUNT'
    FL.ACC = ''
    CALL OPF(FN.ACC, FL.ACC)

    FN.CUST = 'F.CUSTOMER'
    FL.CUST = ''
    CALL OPF(FN.CUST, FL.CUST)
*INC0018016 - Start
    FN.USER = 'F.USER'
    F.USER = ''
    CALL OPF(FN.USER,F.USER)
*INC0018016 - End
* OPEN FILE F.DATE
*FN.DATE='F.DATES'
*FL.DATE=''
*CALL OPF(FN.DATE, FL.DATE)

*Y.LCCY = "KHR"
    Y.LCCY = LCCY
    Y.PARAM.ID = 'SYSTEM'
*    CALL F.READ(FN.AIA.PARAM,Y.PARAM.ID,R.PARAM,F.AIA.PARAM,PARAM.ERR)
    CALL CACHE.READ(FN.AIA.PARAM,Y.PARAM.ID,R.PARAM,PARAM.ERR)  ;*Code Review.3 Changed F.READ

 
    Y.DATE.ID=R.PARAM<AMR.AIA.INS.COMPANY>
    DATE.REC=''
    Y.ER=''
    CALL F.READ(FN.DATES, Y.DATE.ID, DATE.REC, FL.DATES,Y.ER)

    Y.TODAY.DATE=DATE.REC<EB.DAT.LAST.WORKING.DAY>
    Y.FILE.DATE = OCONV(DATE(),'DY'):FMT(OCONV(DATE(),'DM'),'R%2'):FMT(OCONV(DATE(),'DD'),'R%2')
**    Y.REPORTDATE=Y.TODAY.DATE
    Y.LWD = Y.TODAY.DATE

    Y.TODAY.DATE=DATE.REC<EB.DAT.TODAY>
    Y.REPORTDATE = Y.TODAY.DATE

*CALL GET.LOC.REF("CUSTOMER","AMR.CO.PN",AMR.CUST.CO.PN.POS)
*CALL GET.LOC.REF("CUSTOMER","AMR.POLICY.NO",AMR.CUST.POLICY.NO.POS)

    Y.APPLICATION = 'CUSTOMER'
    Y.FLD = 'AMR.CO.PN':VM:'AMR.POLICY.NO'
    Y.POS = ''

    CALL MULTI.GET.LOC.REF(Y.APPLICATION,Y.FLD,Y.POS)
    AMR.CUST.CO.PN.POS = Y.POS<1,1>
    AMR.CUST.POLICY.NO.POS = Y.POS<1,2>

    Y.COMPANY = R.PARAM<AMR.AIA.INS.COMPANY>
    AIA.DIR = R.PARAM<AMR.AIA.INS.AIA.FILE.PATH>
    AIA.FILE.NAME = R.PARAM<AMR.AIA.INS.FILE.NAME>
    REP.FILE.PATH = AIA.FILE.NAME : 'REP_':Y.TODAY.DATE:'.txt'
    RES.DIR = R.PARAM<AMR.AIA.INS.RESP.FILE.PATH>
    
*OPENSEQ AIA.DIR,REP.FILE.PATH TO REP.FILE ELSE
    OPENSEQ RES.DIR,REP.FILE.PATH TO REP.FILE ELSE
        
        CREATE REP.FILE ELSE
            RETURN
        END
    END


****************
RETURN
****************
****************
PROCESS:
**************** 
*READ AMR.OFS.REC FROM FV.AMR.PARA, 'SYSTEM' ELSE AMR.OFS.REC = ''     
*    CALL F.READ(FN.AMR.PARA,Y.PARAM.ID,AMR.OFS.REC,FV.AMR.PARA,PARA.ERR)
    CALL CACHE.READ(FN.AMR.PARA,Y.PARAM.ID,AMR.OFS.REC,PARA.ERR)  ;*Code Review.3 Changed F.READ
    IF AMR.OFS.REC THEN
        OFS.USERNAME.ID = AMR.OFS.REC<AMR.OFS.USER>
*INC0018016 - Start
        CALL F.READ(FN.USER,OFS.USERNAME.ID,USR.REC,F.USER,USR.ERR)
        OFS.USERNAME = USR.REC<EB.USE.SIGN.ON.NAME>
*INC0018016 - End 
        V.OFS.P = AMR.OFS.REC<AMR.OFS.PASSWORD>
*INC0018016 - Start
        Y.PASS.ENC = '123456789123456789A'  
        OFS.PASSWORD = DECRYPT(V.OFS.P,Y.PASS.ENC,JBASE_CRYPT_BLOWFISH_BASE64) 
*OFS.PASSWORD = DECRYPT(V.OFS.P,'AMRET',JBASE_CRYPT_BLOWFISH_BASE64)
*INC0018016 - End 
        OFS.SOURCE.ID = AMR.OFS.REC<AMR.OFS.SOURCE>
    END ELSE
        RETURN
*PRINT 'CAN NOT FIND OFS!'
    END



    CALL F.READ(FN.DATES,Y.COMPANY,DATES.REC,FL.DATES,DATES.ERR)

    IF DATES.REC EQ '' THEN
        RETURN
    END

    T.DATE = DATES.REC<EB.DAT.TODAY>

    OPENSEQ AIA.DIR,REP.FILE.PATH TO AIA.F THEN
        GOSUB GET.READ.FILE.INFO  ;*Code Review.4 - Loop Moved to GOSUB
       
    END
*DELETESEQ AIA.DIR,AIA.FILE.NAME:'.TXT' ELSE NULL

****************
RETURN
****************
*********************
GET.READ.FILE.INFO:
*********************
*Code Review.4 - Loop Moved to GOSUB
 V.SPACE = 0
  
        LOOP
            V.LINE = ''
*            READSEQ V.LINE FROM AIA.F ELSE BREAK
            READSEQ V.LINE FROM AIA.F ELSE        ;*Code Review.1 - THEN/ELSE is on same line, break replaced with return.
               RETURN
             END
            IF V.LINE NE '' THEN

                AIA.SEQ.NO = FIELD(V.LINE,'#',1)
                AIA.TRN.REF.NO = FIELD(V.LINE,'#',2)
                AIA.TRN.TYPE = FIELD(V.LINE,'#',3)
                AIA.TRN.METHOD = FIELD(V.LINE,'#',4)
                AIA.TRN.CCY = FIELD(V.LINE,'#',5)
                CUST.ACCT.NO = FIELD(V.LINE,'#',6)
                AIA.TRN.AMOUNT = FIELD(V.LINE,'#',7)
                AIA.TRN.HASH.AMOUNT = FIELD(V.LINE,'#',8)
                AIA.CUSTOMER.NAME = FIELD(V.LINE,'#',9)
                AIA.POLICY.NUMBER = FIELD(V.LINE,'#',10)
                AIA.BANK.CODE = FIELD(V.LINE,'#',11)
                AIA.TRAN.STATUS = FIELD(V.LINE,'#',12)
                AIA.REMARK = FIELD(V.LINE,'#',13)
                AIA.HASH.COMPARE.STATUS = FIELD(V.LINE,'#',14)
                AIA.ACC.NO = FIELD(V.LINE,'#',15)
                AIA.PAYMENT.DATE = FIELD(V.LINE,'#',16)
                AIA.CREATE.DATE = FIELD(V.LINE,'#',17)
                GOSUB CHECK.IF.STATEMENTS  ;*Code Review.2 if Statements moved to GOSUB to have CONTINUE Keyword

              IF Y.CONTINUE.FLAG NE 'END.ITTERATION' THEN ;*Code Review.2 Added IF Statement, to handle Flag Logic on Replacement of CONTINUE keyword  
                IS.BOOKING.OFS = 'FALSE'
                GOSUB PROC.FUNDS.TRANSFER
                GOSUB POST.OFS.PROC.RESPONE ;*Code Review Added GOSUB to split IF Statement. 
                
              END          ;*Code Review.2 Added 'END', to handle Flag Logic on Replacement of CONTINUE keyword  
          END
        REPEAT

RETURN

*********************
POST.OFS.PROC.RESPONE:
*********************
	IF IS.BOOKING.OFS EQ 'FALSE' THEN
             AIA.TRAN.STATUS = 'F'
             AIA.REMARK = '04'
             GOSUB PROC.RESPONE
        END ELSE
             AIA.TRAN.STATUS = 'S'
             GOSUB PROC.RESPONE
        END

RETURN
*********************
CHECK.IF.STATEMENTS:
*********************
*Code Review.2 if Statements moved to GOSUB to have CONTINUE Keyword 
Y.CONTINUE.FLAG = ''  ;*Code Review.2 Flag Variable initialised
* //-- check Hash compate status
                IF TRIM(LEFT(AIA.HASH.COMPARE.STATUS,1)) EQ 'F' THEN
                    AIA.TRAN.STATUS = 'F'
                    AIA.REMARK = '05'
                    GOSUB PROC.RESPONE
*                    CONTINUE
                     Y.CONTINUE.FLAG = 'END.ITTERATION' ;*Code Review.2 Changed CONTINUE keyword to Flag Logic
                     RETURN ;*Code Review.2 Return Keyword is used to exit from current GOSUB
                END
* //-- end

* //-- check Account AIA in T24
                ACC.REC = ''
                CALL F.READ(FN.ACC,AIA.ACC.NO,ACC.REC,FL.ACC,ACC.ERR)
                IF ACC.REC EQ '' THEN
                    AIA.TRAN.STATUS = 'F'
                    AIA.REMARK = '03'
                    GOSUB PROC.RESPONE
*                    CONTINUE
                     Y.CONTINUE.FLAG = 'END.ITTERATION' ;*Code Review.2 Changed CONTINUE keyword to Flag Logic
                     RETURN ;*Code Review.2 Return Keyword is used to exit from current GOSUB
                END

                AIA.ACC.ACTUAL.BAL = ACC.REC<AC.ONLINE.ACTUAL.BAL>

                IF AIA.ACC.ACTUAL.BAL EQ '' THEN      ;* check AIA Amount
                    AIA.TRAN.STATUS = 'F'
                    AIA.REMARK = '03'
                    GOSUB PROC.RESPONE
 *                    CONTINUE
                     Y.CONTINUE.FLAG = 'END.ITTERATION' ;*Code Review.2 Changed CONTINUE keyword to Flag Logic
                     RETURN ;*Code Review.2 Return Keyword is used to exit from current GOSUB
                END

                IF AIA.TRN.AMOUNT GT AIA.ACC.ACTUAL.BAL THEN    ;* check AIA Amount and TRAN amount
                    AIA.TRAN.STATUS = 'F'
                    AIA.REMARK = '04'
                    GOSUB PROC.RESPONE
*                    CONTINUE
                     Y.CONTINUE.FLAG = 'END.ITTERATION' ;*Code Review.2 Changed CONTINUE keyword to Flag Logic
                     RETURN ;*Code Review.2 Return Keyword is used to exit from current GOSUB
                END
* // -- end

* //-- check account customer in T24 system
                ACC.REC = ''
                CALL F.READ(FN.ACC,CUST.ACCT.NO,ACC.REC,FL.ACC,ACC.ERR)
                IF ACC.REC EQ '' THEN
                    AIA.TRAN.STATUS = 'F'
                    AIA.REMARK = '01'
                    GOSUB PROC.RESPONE
*                    CONTINUE
                     Y.CONTINUE.FLAG = 'END.ITTERATION' ;*Code Review.2 Changed CONTINUE keyword to Flag Logic
                     RETURN ;*Code Review.2 Return Keyword is used to exit from current GOSUB
                END
* //-- end

* //-- check customer amret is real cooperate with AIA

                CUST.ID = TRIM(ACC.REC<AC.CUSTOMER>)
                IF CUST.ID EQ '' THEN
                    AIA.TRAN.STATUS = 'F'
                    AIA.REMARK = '01'
                    GOSUB PROC.RESPONE
*                    CONTINUE
                     Y.CONTINUE.FLAG = 'END.ITTERATION' ;*Code Review.2 Changed CONTINUE keyword to Flag Logic
                     RETURN ;*Code Review.2 Return Keyword is used to exit from current GOSUB
                END

                CALL F.READ(FN.CUST,CUST.ID,CUST.REC,FL.CUST,CUST.ERR)
                IF CUST.REC EQ '' THEN
                    AIA.TRAN.STATUS = 'F'
                    AIA.REMARK = '01'
                    GOSUB PROC.RESPONE
*                    CONTINUE
                     Y.CONTINUE.FLAG = 'END.ITTERATION' ;*Code Review.2 Changed CONTINUE keyword to Flag Logic
                     RETURN ;*Code Review.2 Return Keyword is used to exit from current GOSUB
                END

                AMR.CUST.CO.POLICY = CUST.REC<EB.CUS.LOCAL.REF><1,AMR.CUST.CO.PN.POS>
                AMR.CUST.POLICY.PN = CUST.REC<EB.CUS.LOCAL.REF><1,AMR.CUST.POLICY.NO.POS>

* IF AMR.CUST.CO.POLICY EQ '' THEN
*     AIA.TRAN.STATUS = 'F'
*     AIA.REMARK = '01'
*     GOSUB PROC.RESPONE
*     CONTINUE
* END

* IS.CUST.AIA = 'FALSE'

* FOR I = 1 TO DCOUNT(AMR.CUST.CO.POLICY,SM)
*     IS.CUST.AIA = 'FALSE'
*     IF TRIM(AMR.CUST.CO.POLICY<1,1,I>) EQ 'AIA' THEN
*         POLICY.NUM = AMR.CUST.POLICY.PN<1,1,I>
*         IF POLICY.NUM EQ TRIM(AIA.POLICY.NUMBER) THEN
*             IS.CUST.AIA = 'TRUE'
*             BREAK
*         END

*     END

* NEXT I

* IF IS.CUST.AIA EQ 'FALSE' THEN
*     AIA.TRAN.STATUS = 'F'
*     AIA.REMARK = '01'
*     GOSUB PROC.RESPONE
*     CONTINUE
* END


* //-- end


* //-- check account status
                IF ACC.REC<AC.CLOSURE.DATE> NE '' THEN
                    AIA.TRAN.STATUS = 'F'
                    AIA.REMARK = '02'
                    GOSUB PROC.RESPONE
*                    CONTINUE
                     Y.CONTINUE.FLAG = 'END.ITTERATION' ;*Code Review.2 Changed CONTINUE keyword to Flag Logic
                     RETURN ;*Code Review.2 Return Keyword is used to exit from current GOSUB
                END
* //-- end

* //-- check account currency , allow only USD
                IF ACC.REC<AC.CURRENCY> NE 'USD' THEN
                    AIA.TRAN.STATUS = 'F'
                    AIA.REMARK = '02'
                    GOSUB PROC.RESPONE
*                    CONTINUE
                     Y.CONTINUE.FLAG = 'END.ITTERATION' ;*Code Review.2 Changed CONTINUE keyword to Flag Logic
                     RETURN ;*Code Review.2 Return Keyword is used to exit from current GOSUB
                END
* //--

* //-- check transaction type
                IF TRIM(AIA.TRN.TYPE) NE 'P1' THEN
                    AIA.TRAN.STATUS = 'F'
                    AIA.REMARK = '03'
                    GOSUB PROC.RESPONE
*                    CONTINUE
                     Y.CONTINUE.FLAG = 'END.ITTERATION' ;*Code Review.2 Changed CONTINUE keyword to Flag Logic
                     RETURN ;*Code Review.2 Return Keyword is used to exit from current GOSUB
                END
* //-- end

* //-- check transaction method
                IF TRIM(AIA.TRN.METHOD) NE 'TF' THEN
                    AIA.TRAN.STATUS = 'F'
                    AIA.REMARK = '03'
                    GOSUB PROC.RESPONE
*                    CONTINUE
                     Y.CONTINUE.FLAG = 'END.ITTERATION' ;*Code Review.2 Changed CONTINUE keyword to Flag Logic
                     RETURN ;*Code Review.2 Return Keyword is used to exit from current GOSUB
                END
* //-- end



RETURN
*********************
PROC.FUNDS.TRANSFER:
*********************

    APPLICATION.ID = 'FUNDS.TRANSFER'
    OFS.VERSION = 'AMR.AIA'

    OFS.MSG = APPLICATION.ID:',':OFS.VERSION:'/I/PROCESS,':OFS.USERNAME:'/':OFS.PASSWORD:'/':Y.COMPANY
    OFS.MSG := ',,DEBIT.ACCT.NO::=':AIA.ACC.NO:',DEBIT.CURRENCY::=':AIA.TRN.CCY
    OFS.MSG := ',DEBIT.AMOUNT::=':AIA.TRN.AMOUNT:',DEBIT.VALUE.DATE::=':AIA.PAYMENT.DATE
    OFS.MSG := ',CREDIT.ACCT.NO::=':CUST.ACCT.NO:',CREDIT.CURRENCY::=':AIA.TRN.CCY
    OFS.MSG := ',TRANSACTION.TYPE::=AC'

    OFS.DIR = R.PARAM<AMR.AIA.INS.OFS.FILE.PATH>
    OFS.FILE = 'AIA.PAYMENT.OFS_'
    OPENSEQ OFS.DIR,OFS.FILE: T.DATE :'.txt' TO T.FILE ELSE
        CREATE T.FILE ELSE END
    END


*WRITESEQ OFS.MSG APPEND TO T.FILE ELSE END
    
*CALL OFS.GLOBUS.MANAGER(OFS.SOURCE.ID,OFS.MSG)
    
    CALL OFS.CALL.BULK.MANAGER(OFS.SOURCE.ID, OFS.MSG, Y.OFS.RESPONSE, Y.TXN.COMITTED)
    WRITESEQ OFS.MSG : '-----':CUST.ACCT.NO APPEND TO T.FILE ELSE END
    WRITESEQ '-----------------------------' APPEND TO T.FILE ELSE END
*CLOSESEQ T.FILE
 

*    T.STATUS = FIELD(OFS.MSG,'/',3)
    T.STATUS = FIELD(Y.OFS.RESPONSE,'/',3) ;*INC0018016
    IF T.STATUS EQ '-1' THEN
        IS.BOOKING.OFS = 'FALSE'
        RETURN
    END


    IS.BOOKING.OFS = 'TRUE'

*********************
RETURN
*********************

*********************
PROC.RESPONE:
*********************

    V.TEXT = AIA.SEQ.NO:'#':AIA.TRN.REF.NO:'#':AIA.TRN.TYPE:'#':AIA.TRN.METHOD:'#':AIA.TRN.CCY:'#':CUST.ACCT.NO:'#':AIA.TRN.AMOUNT:'#':AIA.TRN.HASH.AMOUNT:'#':AIA.CUSTOMER.NAME:'#':AIA.POLICY.NUMBER:'#':AIA.BANK.CODE:'#'
    V.TEXT := AIA.TRAN.STATUS:'#':AIA.REMARK:'#':AIA.HASH.COMPARE.STATUS:'#':AIA.ACC.NO:'#':AIA.PAYMENT.DATE:'#':AIA.CREATE.DATE:'#':Y.FILE.DATE

    WRITESEQ V.TEXT APPEND TO REP.FILE ELSE END
    OFS.DIR = R.PARAM<AMR.AIA.INS.OFS.FILE.PATH>
    OFS.FILE = 'AIA.PAYMENT.OFS_'
    OPENSEQ OFS.DIR,OFS.FILE: T.DATE :'.txt' TO T.FILE ELSE
        CREATE T.FILE ELSE END
    END
*WRITESEQ V.TEXT APPEND TO T.FILE ELSE END
*CLOSESEQ T.FILE


*********************
RETURN
*********************

END
