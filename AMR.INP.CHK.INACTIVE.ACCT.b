*-----------------------------------------------------------------------------
SUBROUTINE  AMR.INP.CHK.INACTIVE.ACCT
*-------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER
    $INSERT I_F.STANDING.ORDER
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


RETURN

*---------------
PROCESS:
*---------------

  BEGIN CASE

        CASE APPLICATION EQ 'FUNDS.TRANSFER'
                
            Y.ACCT = R.NEW(FT.DEBIT.ACCT.NO)
            AF = FT.DEBIT.ACCT.NO
            GOSUB INACTIVE.ERR.MSG
            Y.ACCT = R.NEW(FT.CREDIT.ACCT.NO)
            AF = FT.CREDIT.ACCT.NO
            GOSUB INACTIVE.ERR.MSG
            
    
        CASE APPLICATION EQ 'TELLER'
            
            Y.ACCT = R.NEW(TT.TE.ACCOUNT.1)
            AF = TT.TE.ACCOUNT.1
            GOSUB INACTIVE.ERR.MSG
            Y.ACCT = R.NEW(TT.TE.ACCOUNT.2)
            AF = TT.TE.ACCOUNT.2
	    GOSUB INACTIVE.ERR.MSG
         
        CASE APPLICATION EQ 'STANDING.ORDER'

            Y.DR.AC.NO= ID.NEW
            Y.ACCT = FIELD(Y.DR.AC.NO,'.',1)
            AF = @ID
	    GOSUB INACTIVE.ERR.MSG
	    Y.ACCT = R.NEW(STO.CPTY.ACCT.NO)
            AF = STO.CPTY.ACCT.NO
            GOSUB INACTIVE.ERR.MSG
            
    END CASE


RETURN


*---------------
INACTIVE.ERR.MSG:
*---------------
    Y.INACTIVE.MARKER = ''
     CALL F.READ(FN.ACC,Y.ACCT,R.ACC,F.ACC,ERR.ACC)
          Y.INACTIVE.MARKER = R.ACC<AC.INACTIV.MARKER>
     IF Y.INACTIVE.MARKER EQ 'Y' THEN
  	 ETEXT = 'EB-INACTIVE.ERROR':FM:Y.ACCT
  	 CALL STORE.END.ERROR
     END
RETURN
END