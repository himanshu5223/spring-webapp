*-----------------------------------------------------------------------------
SUBROUTINE AMR.V.INS.TYPE.VAL
*-------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EM.LO.APPLICATION
   
    GOSUB INITIALISE
    GOSUB PROCESS
RETURN
*--------------- 
INITIALISE:
*---------------

    FN.EM.LO.APPLICATION = 'F.EM.LO.APPLICATION'
    F.EM.LO.APPLICATION = ''
    CALL OPF(FN.EM.LO.APPLICATION,F.EM.LO.APPLICATION)

    Y.APP.NAME = 'EM.LO.APPLICATION'
    Y.FLD.NAME = 'AMR.INS.TYPE':VM:'AMR.INS.CMPY'
    Y.FLD.POS = ''
    CALL MULTI.GET.LOC.REF(Y.APP.NAME,Y.FLD.NAME,Y.FLD.POS)
    Y.INS.TYPE.POS = Y.FLD.POS<1,1>
    Y.INS.CMPY.POS = Y.FLD.POS<1,2>


RETURN
*---------------
PROCESS:
*---------------
   Y.INS.TYPE = COMI
    
   IF Y.INS.TYPE EQ 'Loan Repayment Insurance' THEN
      R.NEW(ELOA.LOCAL.REF)<1,Y.INS.CMPY.POS> = 'AIA'
   END	
     
   IF Y.INS.TYPE EQ 'Fire Insurance' THEN
      R.NEW(ELOA.LOCAL.REF)<1,Y.INS.CMPY.POS> = 'INFINITY'
   END

   IF Y.INS.TYPE EQ 'PPI' THEN
      R.NEW(ELOA.LOCAL.REF)<1,Y.INS.CMPY.POS> = 'PEOPLE&PARTNERS'
   END
	
RETURN
END