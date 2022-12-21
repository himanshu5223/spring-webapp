*-----------------------------------------------------------------------------
SUBROUTINE AMR.NOFILE.AIA.POLICY.NO(Y.ARR)
*-------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_ENQUIRY.COMMON

    GOSUB INITIALISE
    GOSUB PROCESS
RETURN
*--------------- 
INITIALISE:
*---------------
    APPL.ARR = "ACCOUNT"
    Y.LOC.REF = "AMR.POLICY.NO"
    Y.LOC.REF.POS = ""
    CALL MULTI.GET.LOC.REF(APPL.ARR,Y.LOC.REF,Y.LOC.REF.POS)
    Y.POLICY.NO.POS = Y.LOC.REF.POS<1,1>
        
    LOCATE 'ACCOUNT.NO' IN D.FIELDS<1> SETTING ACCT.POS THEN
        ACCT.NO = D.RANGE.AND.VALUE<ACCT.POS>
    END ELSE
        ACCT.NO = ''
    END

    FN.ACCOUNT = "F.ACCOUNT"
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    
    FN.CUSTOMER = "F.CUSTOMER"
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)
    
RETURN         

*--------------- 
PROCESS:
*---------------

   CALL F.READ(FN.ACCOUNT,ACCT.NO,R.ACC.REC,F.ACCOUNT,ACC.ERR)
	Y.POLICY.NO = R.ACC.REC<AC.LOCAL.REF><1,Y.POLICY.NO.POS>
   Y.ARR<-1> = Y.POLICY.NO

RETURN
END