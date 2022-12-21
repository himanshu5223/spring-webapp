*-----------------------------------------------------------------------------
SUBROUTINE  AMR.V.COMM.ACCT
*-------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Code Review_Amendment Date :   29-12-2020
* Kiran HS      04-Jan-2021     Review
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER
    $INSERT I_F.STANDING.ORDER
    $INSERT I_F.AMR.AIA.PARAM
    $INSERT I_F.ACCOUNT

    GOSUB INITIALISE
    GOSUB PROCESS
RETURN
*--------------- 
INITIALISE:
*---------------
    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''
    CALL OPF(FN.ACC,F.ACC)

    FN.PARAM = 'F.AMR.AIA.PARAM'
    F.PARAM = ''
    CALL OPF(FN.PARAM,F.PARAM)

   Y.APPLICATION = "TELLER"
    Y.FIELD = "AMR.MT.CHRG.AMT":VM:"AMR.NET.AMOUNT"
    Y.POS.REF = ""
    CALL MULTI.GET.LOC.REF(Y.APPLICATION,Y.FIELD,Y.POS.REF)

    Y.CHRG.AMT.POS = Y.POS.REF<1,1> 
    Y.NET.AMT.POS = Y.POS.REF<1,2> 


RETURN
*---------------
PROCESS:
*---------------

    BEGIN CASE

        CASE APPLICATION EQ 'FUNDS.TRANSFER'
                
*            Y.CREDIT.CCY = R.NEW(FT.DEBIT.CURRENCY)
            GOSUB CREDIT.ACCT
            
            IF Y.CREDIT.ACCT NE '' THEN
                R.NEW(FT.CREDIT.ACCT.NO) = Y.CREDIT.ACCT

            END
    
        CASE APPLICATION EQ 'TELLER'
*            Y.CREDIT.CCY = R.NEW(TT.TE.CURRENCY.1)
            GOSUB CREDIT.ACCT
           
            IF Y.CREDIT.ACCT NE '' THEN
                R.NEW(TT.TE.ACCOUNT.2) = Y.CREDIT.ACCT
		R.NEW(TT.TE.CURRENCY.2) = Y.CREDIT.CCY
            END
            Y.FCY.AMT = R.NEW(TT.TE.AMOUNT.FCY.1) 
            Y.LCY.AMT = R.NEW(TT.TE.AMOUNT.LOCAL.1)
            Y.CHG.AMT = '0'
            IF Y.FCY.AMT THEN
                Y.CHG.AMT = R.NEW(TT.TE.LOCAL.REF)<1,Y.CHRG.AMT.POS>
                Y.TOY.NET.AMT = Y.FCY.AMT - Y.CHG.AMT
                R.NEW(TT.TE.LOCAL.REF)<1,Y.NET.AMT.POS> = Y.TOY.NET.AMT  
                
            END ELSE
               Y.CHG.AMT = R.NEW(TT.TE.LOCAL.REF)<1,Y.CHRG.AMT.POS>
               GOSUB GET.EXCHG.RATE 
                Y.TOY.NET.AMT = Y.LCY.AMT.RATE - Y.CHG.AMT
                R.NEW(TT.TE.LOCAL.REF)<1,Y.NET.AMT.POS> = Y.TOY.NET.AMT

            END
      
    
        CASE APPLICATION EQ 'STANDING.ORDER'
    
*            Y.CREDIT.CCY = R.NEW(STO.CURRENCY)
            GOSUB CREDIT.ACCT
            
            IF Y.CREDIT.ACCT NE '' THEN
                R.NEW(STO.CPTY.ACCT.NO) = Y.CREDIT.ACCT
            END
           

    END CASE


RETURN
*---------------            
CREDIT.ACCT:
*---------------
    Y.CREDIT.CCY = 'USD'	
    Y.PARAM.ID = 'SYSTEM'
*CALL F.READ(FN.PARAM,Y.PARAM.ID,R.PARAM,F.PARAM,PARAM.ERR)
    CALL CACHE.READ(FN.PARAM,Y.PARAM.ID,R.PARAM,PARAM.ERR) ;*Code Review.1 Changed F.READ TO CACHE.READ For parameter table.


    Y.PARAM.CCY = R.PARAM<AMR.AIA.CREDIT.CURRENCY>

    LOCATE Y.CREDIT.CCY IN Y.PARAM.CCY<1,1> SETTING Y.CCY.POS THEN
        
        Y.CREDIT.ACCT = R.PARAM<AMR.AIA.CREDIT.ACCOUNT,Y.CCY.POS>
        
    END
RETURN
*-------------
GET.EXCHG.RATE:
*-------------
    Y.SELL.AMT = ''
    Y.BASE.CCY = ''
    Y.EXCH.RATE = ''
    Y.DIFF = ''
    Y.AMT.LCY = ''
    Y.RET.CODE = ''
    Y.CUR.CR.CCY = 'KHR'
    CALL EXCHRATE('1',Y.CUR.CR.CCY,Y.LCY.AMT,Y.CREDIT.CCY,Y.SELL.AMT,Y.BASE.CCY,Y.EXCH.RATE,Y.DIFF,Y.AMT.LCY,Y.RET.CODE)
    
    Y.LCY.AMT.RATE = Y.SELL.AMT

RETURN
END