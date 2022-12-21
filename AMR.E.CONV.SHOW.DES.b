***********************************************************************************
* DATE CREATED        : 01/APR/2022
* CREATED BY          : CHOU RAKSA
* DESCRIPTION         : CHG0034775
*                     : BOD-Request to display the LO number after Txn description in AIA
* ATTACHED TO         : ENQUIRY>ENQ.AMR.STMT.ENT.BOOK
* ATTACHED AS         : CONVERSION
* IN/OUT ARGUMENTS    : N/A
***********************************************************************************
* DATE CREATED        : 2022-08-08
* CREATED BY          : SEIT LYHEANG
* DESCRIPTION         : CHG0033965
*                     : change & add transaction ID for Fast Transaction
* ATTACHED TO         : ENQUIRY>AMR.EM.STMT.ENT.BOOK.CUI
* ATTACHED AS         : CONVERSION
* IN/OUT ARGUMENTS    : N/A
***********************************************************************************
	SUBROUTINE AMR.E.CONV.SHOW.DES
	
	$INSERT I_EQUATE
    $INSERT I_COMMON
    $INSERT I_ENQUIRY.COMMON
	$INSERT I_F.FUNDS.TRANSFER
	$INSERT I_F.ACCOUNT
	$INSERT I_AA.APP.COMMON
    $INSERT I_AA.LOCAL.COMMON
    $INSERT I_F.AA.CUSTOMER
	$INSERT I_F.AA.ARRANGEMENT.ACTIVITY
	$INSERT I_F.AA.CUSTOMER
	$INSERT I_F.STMT.ENTRY

	GOSUB INIT
	GOSUB PROCESS
	RETURN

*--------
PROCESS:
*--------

	*R.RECORD is the keyword to select the record from the table that enquiry call
	*EX: if the enquiry call from stmt.entry, it will read record from that stmt.entry
	*O.DATA is a keyword for incoming and outgoing to the specific field we assign
	
	VAR.TRAN.REF = R.RECORD<AC.STE.OUR.REFERENCE>
	FT.ERR = ''
	FT.REC = ''
	CALL F.READ(FN.FT,VAR.TRAN.REF,FT.REC,F.FT,FT.ERR)
	IF FT.REC EQ '' THEN
		VAR.TRAN.REF = VAR.TRAN.REF:';1'
		CALL F.READ(FN.FT.HIS,VAR.TRAN.REF,FT.REC,F.FT.HIS,FT.ERR)
	END
		*START-CHG0033965
		VAR.TXN.TYPE = FT.REC<FT.TRANSACTION.TYPE>
		VAR.FT.REF.ID = FT.REC<FT.LOCAL.REF><1,AMR.REF.ID.POS>
		VAR.AT.UNIQUE.ID = FT.REC<FT.LOCAL.REF><1,AT.UNIQUE.ID.POS>
		ENQ.NAME =  FIELD(ENQ.SELECTION,@FM,1)
		*END-CHG0033965
	IF VAR.TXN.TYPE EQ 'ACAI' THEN
	
		CALL F.READ(FN.FT,VAR.FT.REF.ID,FT.REC,F.FT,FT.ERR)
		IF FT.REC EQ '' THEN
			VAR.TRAN.REF.ID = VAR.FT.REF.ID:';1'
			CALL F.READ(FN.FT.HIS,VAR.TRAN.REF.ID,FT.REC,F.FT.HIS,FT.ERR)
		END
		
		VAR.DEBIT.ACC = FT.REC<FT.DEBIT.ACCT.NO>
		ACC.ERR = ''
		ACC.REC = ''
		CALL F.READ(FN.ACC,VAR.DEBIT.ACC,ACC.REC,F.ACC,ACC.ERR)
		VAR.ARRANGEMENT.ID = ACC.REC<AC.ARRANGEMENT.ID>
		
		CALL AA.GET.ARRANGEMENT.CONDITIONS(VAR.ARRANGEMENT.ID, 'CUSTOMER', 'CUSTOMER', effectiveDate, returnIds, returnConditions, returnError)
			Y.RET.TM.REC = RAISE(returnConditions)
			VAR.LOAN.ID = Y.RET.TM.REC<AA.CUS.LOCAL.REF><1,LOAN.APPL.ID.POS>	
			O.DATA = TRIM(O.DATA):" - ":VAR.LOAN.ID
	END	

	*START-CHG0033965
	IF ENQ.NAME EQ "AMR.EM.STMT.ENT.BOOK.CUI" THEN
	
		IF VAR.TXN.TYPE EQ "ACOB" THEN
			O.DATA = ''
			O.DATA = VAR.TRAN.REF[4,9]
		END
		ELSE IF VAR.FT.REF.ID NE '' THEN 
			O.DATA = VAR.FT.REF.ID
		END 
		ELSE IF VAR.TXN.TYPE EQ "ACFS" THEN
			O.DATA = VAR.TRAN.REF[4,9]
		END
		
	END
	*END-CHG0033965
*--------	
RETURN
*--------	
*--------
INIT:
*--------

	FN.FT = 'F.FUNDS.TRANSFER'
	F.FT = ''
	CALL OPF(FN.FT,F.FT)
	
	FN.FT.HIS = 'F.FUNDS.TRANSFER$HIS'
	F.FT.HIS = ''
	CALL OPF(FN.FT.HIS,F.FT.HIS)
	
	FN.ACC = 'F.ACCOUNT'
	F.ACC = ''
	CALL OPF(FN.ACC,F.ACC)
	
	FN.STMT.ENTRY= "F.STMT.ENTRY"
    F.STMT.ENTRY= ""
    CALL OPF(FN.STMT.ENTRY,F.STMT.ENTRY)
	
	VAR.RESULT = ''
	
	Y.APPLICATION = "AA.ARR.CUSTOMER":FM:"FUNDS.TRANSFER"
	Y.FIELD.NAME  = "LOAN.APPL.ID":FM:"L.AMR.REF.ID":VM:"AT.UNIQUE.ID"
	Y.POSITION    = ""
	CALL MULTI.GET.LOC.REF(Y.APPLICATION,Y.FIELD.NAME,Y.POSITION)
	
	LOAN.APPL.ID.POS = Y.POSITION<1,1>
	AMR.REF.ID.POS = Y.POSITION<2,1>
	AT.UNIQUE.ID.POS = Y.POSITION<2,2>
	
*--------	
RETURN
*--------
END