*-----------------------------------------------------------------------------
SUBROUTINE AMR.AIA.DEBIT.AC.NO
*-------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
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

    FN.STANDING.ORDER = 'F.STANDING.ORDER'
    F.STANDING.ORDER = ''
    CALL OPF(FN.STANDING.ORDER,F.STANDING.ORDER)

	
    Y.APP.NAME = 'STANDING.ORDER'
    Y.FLD.NAME = 'AMR.DEBIT.AC.NO'
    Y.FLD.POS = ''
    CALL MULTI.GET.LOC.REF(Y.APP.NAME,Y.FLD.NAME,Y.FLD.POS)
    Y.DB.AC.NO.POS = Y.FLD.POS<1,1>
   



RETURN
*---------------
PROCESS:
*---------------
  Y.STO.DEBIT.NO = ID.NEW

  Y.DB.AC.NO = FIELD(Y.STO.DEBIT.NO,".",1)

  R.NEW(STO.LOCAL.REF)<1,Y.DB.AC.NO.POS> = Y.DB.AC.NO

RETURN
END