* ----------------------------------------------------------------------------
* DATE CREATED        : 10/MAY/2022
* CREATED BY          : CHOU RAKSA
* DESCRIPTION         : INC0025295-AIA LRI Wrong Amount Paid
* ATTACHED TO         : VERSION>AMR.AIA.LO.PRM.AMT,AMR.AIA.PRM.AMT.RESP
* ATTACHED AS         : Input Routine
*-----------------------------------------------------------------------------
	SUBROUTINE AMR.AIA.I.PRM.AMT.CHECK

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EM.LO.APPLICATION
    $INSERT I_F.AMR.AIA.LO.PRM.AMT

    GOSUB INIT
    GOSUB PROCESS
	
*-----------------------------------------------------------------------------	
RETURN
*-----------------------------------------------------------------------------
INIT:
*-----------------------------------------------------------------------------   
    FN.PRM.AMT = 'F.AMR.AIA.LO.PRM.AMT'
    F.PRM.AMT = ''
    CALL OPF(FN.PRM.AMT,F.PRM.AMT)
	
	FN.EM.LO.APPLICATION = 'F.EM.LO.APPLICATION'
    F.EM.LO.APPLICATION = ''
    CALL OPF(FN.EM.LO.APPLICATION,F.EM.LO.APPLICATION)
    
    Y.APPLICATION = 'EM.LO.APPLICATION'
    Y.FLD = 'AMR.PREMIUM.AMT':VM:'L.AMR.INS.REQ':VM:'AMR.INS.CMPY' ;*INC0017410
    Y.POS = ''
    CALL MULTI.GET.LOC.REF(Y.APPLICATION,Y.FLD,Y.POS)
    
    Y.PRM.AMT.POS = Y.POS<1,1>
    Y.INS.REQ.POS = Y.POS<1,2>
    Y.INS.CMPY.POS = Y.POS<1,3> ;*INC0017410
	
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------
	CALL F.READ(FN.PRM.AMT,ID.NEW,R.PRM.AMT,F.PRM.AMT,PRM.ERR)
	Y.PREMIUM.AMT = R.PRM.AMT<AIA.PRM.PREMIUM.AMOUNT>
	*START - INC0025295
	P.VERSION = APPLICATION:PGM.VERSION
	IF P.VERSION EQ 'AMR.AIA.LO.PRM.AMT,AMR.AIA.PRM.AMT.RESP' THEN
	
		AMR.PRE.AMOUNT.NEW = R.NEW(AIA.PRM.PREMIUM.AMOUNT)
		
		IF Y.PREMIUM.AMT AND (Y.PREMIUM.AMT NE AMR.PRE.AMOUNT.NEW) THEN
			CALL F.READ(FN.EM.LO.APPLICATION,ID.NEW,R.EM.LO.APPLICATION,F.EM.LO.APPLICATION,E.EM.LO.APPLICATION)
			R.EM.LO.APPLICATION<ELOA.LOCAL.REF,Y.PRM.AMT.POS> = AMR.PRE.AMOUNT.NEW
			WRITE R.EM.LO.APPLICATION TO F.EM.LO.APPLICATION,ID.NEW
		END
	*END - INC0025295
	END ELSE
		Y.INS.REQ = R.NEW(ELOA.LOCAL.REF)<1,Y.INS.REQ.POS>
		Y.INS.CMPY = R.NEW(ELOA.LOCAL.REF)<1,Y.INS.CMPY.POS> ;*INC0017410
     
		IF Y.INS.REQ EQ 'YES' AND Y.INS.CMPY EQ 'AIA' THEN ;*INC0017410
			IF Y.PREMIUM.AMT EQ ''  THEN
				AF = ELOA.LOCAL.REF
				AV = Y.PRM.AMT.POS
				ETEXT = 'EM-INS.NO.PREMIUM.AMT'
				CALL STORE.END.ERROR
			END ELSE
				R.NEW(ELOA.LOCAL.REF)<1,Y.PRM.AMT.POS> = Y.PREMIUM.AMT   
			END
		END
	END
    
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END
