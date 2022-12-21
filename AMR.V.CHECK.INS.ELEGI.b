SUBROUTINE AMR.V.CHECK.INS.ELEGI
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Code Review_Amendment Date :   24-12-2020
* Kiran HS      04-Jan-2021     Review
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AMR.AIA.PARAM
    $INSERT I_F.EM.LO.APPLICATION

    GOSUB INITIALISE
    GOSUB PROCESS
RETURN

INITIALISE:

    FN.AIA.PARAM = 'F.AMR.AIA.PARAM'
    F.AIA.PARAM = ''
    CALL OPF(FN.AIA.PARAM,F.AIA.PARAM)
    
    Y.INS.VALUE = COMI
    Y.PAR.ID = 'SYSTEM'
*    CALL F.READ(FN.AIA.PARAM,'SYSTEM',R.AIA,F.AIA.PARAM,AIA.ERR)
    CALL CACHE.READ(FN.AIA.PARAM,Y.PAR.ID,R.AIA,AIA.ERR)  ;*Code Review.3 Changed F.READ
    Y.USD.MIN.AMT = R.AIA<AMR.AIA.USD.MIN.AMOUNT>
    Y.USD.MAX.AMT = R.AIA<AMR.AIA.USD.MAX.AMOUNT>

    Y.KHR.MIN.AMT = R.AIA<AMR.AIA.KHR.MIN.AMOUNT>
    Y.KHR.MAX.AMT = R.AIA<AMR.AIA.KHR.MAX.AMOUNT>

    Y.THB.MIN.AMT = R.AIA<AMR.AIA.THB.MIN.AMOUNT>
    Y.THB.MAX.AMT = R.AIA<AMR.AIA.THB.MAX.AMOUNT>

RETURN

PROCESS:

    IF Y.INS.VALUE EQ 'YES' THEN
        Y.CURRENCY= R.NEW(ELOA.CURRENCY)
        Y.AMOUNT = R.NEW(ELOA.AMOUNT)

        BEGIN CASE

            CASE Y.CURRENCY EQ 'USD'
          
                IF (Y.AMOUNT LT Y.USD.MIN.AMT) OR (Y.AMOUNT GT Y.USD.MAX.AMT) THEN
                    AF= ELOA.AMOUNT
                    ETEXT = 'EM-INS.RANGE.ERR'
                    CALL STORE.END.ERROR
                END

            CASE Y.CURRENCY EQ 'KHR'
                IF (Y.AMOUNT LT Y.KHR.MIN.AMT) OR (Y.AMOUNT GT Y.KHR.MAX.AMT) THEN
                    AF= ELOA.AMOUNT
                    ETEXT = 'EM-INS.RANGE.ERR'
                    CALL STORE.END.ERROR
                END
            CASE Y.CURRENCY EQ 'THB'
                IF (Y.AMOUNT LT Y.THB.MIN.AMT) OR (Y.AMOUNT GT Y.THB.MAX.AMT) THEN
                    AF= ELOA.AMOUNT
                    ETEXT = 'EM-INS.RANGE.ERR'
                    CALL STORE.END.ERROR
                END
        END CASE
    END
RETURN

END
           
            
       

