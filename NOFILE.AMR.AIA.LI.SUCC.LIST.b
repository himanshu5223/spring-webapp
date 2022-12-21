SUBROUTINE NOFILE.AMR.AIA.LI.SUCC.LIST(Y.ARRAY)
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Code Review_Amendment Date :   29-12-2020
* Kiran HS      04-Jan-2021     Review
*-----------------------------------------------------------------------------
* DATE MODIFIED       : 13/04/2022
* MODIFIED BY         : Chea Oengviseth
* MODIFICATION DETAIL : CHG0034900
* DESCRIPTION         : Reversed in XML file (batch run) that send to AIA
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER
	
*CHG0034900 - START	
	
    GOSUB INIT
    GOSUB PROCESS


RETURN
*****
INIT:
*****
    
    FN.FT = 'F.FUNDS.TRANSFER'
    F.FT = ''
    CALL OPF(FN.FT,F.FT)

    FN.TT = 'F.TELLER'
    F.TT = ''
    CALL OPF(FN.TT,F.TT)
	
	
	LOCATE '@ID' IN D.FIELDS<1> SETTING Y.POS THEN 
      Y.TRANSACTION = D.RANGE.AND.VALUE<Y.POS>
    END 
	
RETURN
*****
PROCESS:
*****
 
  Y.FIELD = SUBSTRINGS(Y.TRANSACTION,0,2)
  IF Y.FIELD EQ 'FT' THEN
	GOSUB GET.POLICY.FT  
  END ELSE
	GOSUB GET.POLICY.TT  
  END
     
RETURN

*****
GET.POLICY.FT:
*************
	
	CALL F.READ(FN.FT,Y.TRANSACTION,R.FT,F.FT,FT.ERR)
		
	IF FT.ERR NE '' THEN
		*Y.ARRAY<-1> = "Record already reversed"
		ENQ.ERROR = "Record already reversed"
		RETURN
	END
	
	Y.TXN.ID = Y.TRANSACTION
	Y.TXN.DATE = R.FT<FT.PROCESSING.DATE>
	Y.TXN.AMOUNT = R.FT<FT.DEBIT.AMOUNT>
	Y.DEBIT.ACCOUNT = R.FT<FT.DEBIT.ACCT.NO>
	Y.DEBIT.CURRENCY = R.FT<FT.DEBIT.CURRENCY>
	Y.CREDIT.ACCOUNT = R.FT<FT.CREDIT.ACCT.NO>
	Y.ARRAY<-1> = Y.TXN.ID:"*":Y.TXN.DATE:"*":Y.TXN.AMOUNT:"*":Y.DEBIT.ACCOUNT:"*":Y.DEBIT.CURRENCY:"*":Y.CREDIT.ACCOUNT
	
RETURN	
*************
GET.POLICY.TT:
*************
	
	CALL F.READ(FN.TT,Y.TRANSACTION,R.TT,F.TT,TT.ERR)
	
	IF TT.ERR NE '' THEN 
		ENQ.ERROR = "Record already reversed"
		RETURN
	END
	
	Y.TXN.ID = Y.TRANSACTION
	Y.TXN.DATE = R.TT<TT.TE.VALUE.DATE.1>
	Y.TXN.AMOUNT = R.TT<TT.TE.AMOUNT.LOCAL.2>
	Y.DEBIT.CURRENCY = R.TT<TT.TE.CURRENCY.2>
	Y.CREDIT.ACCOUNT = R.TT<TT.TE.ACCOUNT.2>
	Y.DEBIT.ACCOUNT = R.TT<TT.TE.ACCOUNT.1>

	Y.ARRAY<-1> = Y.TXN.ID:"*":Y.TXN.DATE:"*":Y.TXN.AMOUNT:"*":Y.DEBIT.ACCOUNT:"*":Y.DEBIT.CURRENCY:"*":Y.CREDIT.ACCOUNT
		
RETURN
END
*CHG0034900 - END