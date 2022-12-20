SUBROUTINE AMR.AIA.CHECK.PN
*-----------------------------------------------------------------------------
* Modification History :
*
* Kiran HS      05-Jan-2021     Reviewed
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* Code Review_Amendment Date :   24-12-2020
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
	$INSERT I_F.CUSTOMER
    $INSERT I_F.TELLER
	$INSERT I_F.FUNDS.TRANSFER
	$INSERT I_F.STANDING.ORDER
	$INSERT I_F.EM.LO.APPLICATION
   

    Y.AIA.POLICY.NO = COMI
    Y.APPLICATION = APPLICATION

    Y.APPL = 'EM.LO.APPLICATION'
    Y.FLD =  'L.AMR.INS.REQ':VM:'L.AMR.POLICY.NO'            ;*Code Review.1 - all MULTI.GET.LOC.REF moved at one place
    Y.POS = ''

    CALL MULTI.GET.LOC.REF(Y.APPL,Y.FLD,Y.POS)

    Y.INS.REQ.POS = Y.POS<1,1>
    Y.AMR.POLICY.POS = Y.POS<1,2>           ;*Code Review.1 - all MULTI.GET.LOC.REF moved at one place(Added this Line)
    Y.INS.VALUE = R.NEW(ELOA.LOCAL.REF)<1,Y.INS.REQ.POS>
    
    IF Y.INS.VALUE EQ 'YES' THEN
        
        IF (Y.AIA.POLICY.NO EQ '') OR (Y.APPLICATION NE "EM.LO.APPLICATION") THEN

*            Y.APP = 'EM.LO.APPLICATION'      
*            Y.FIELD = 'L.AMR.POLICY.NO'
*            CALL MULTI.GET.LOC.REF(Y.APP,Y.FIELD,Y.POS)  ;*Code Review.1 - Dupliacte MULTI.GET.LOC.REF so removed
            AF = ELOA.LOCAL.REF
            AV = Y.AMR.POLICY.POS
            ETEXT = 'EM-INS.POLICY.NO.NOT.RECV'
            CALL STORE.END.ERROR

            RETURN
        END
    
        GOSUB INIT
        GOSUB PROCESS
    END
RETURN

INIT:

    BEGIN CASE
   
        CASE Y.APPLICATION EQ "CUSTOMER"
            Y.APP = 'CUSTOMER'
            AF = EB.CUS.LOCAL.REF
   
        CASE Y.APPLICATION EQ "TELLER"
            Y.APP = 'TELLER'
            AF = TT.TE.LOCAL.REF
	
        CASE Y.APPLICATION EQ "FUNDS.TRANSFER"
            Y.APP = 'FUNDS.TRANSFER'
            AF = FT.LOCAL.REF
	
        CASE Y.APPLICATION EQ "STANDING.ORDER"
            Y.APP = 'STANDING.ORDER'
            AF = STO.LOCAL.REF
	
        CASE Y.APPLICATION EQ "EM.LO.APPLICATION"
            Y.APP = 'EM.LO.APPLICATION'
            AF = ELOA.LOCAL.REF
	
    END CASE
      
*	Y.FIELD = 'L.AMR.POLICY.NO'
*	CALL MULTI.GET.LOC.REF(Y.APP,Y.FIELD,Y.POS)  ;*Code Review.1 - Dupliacte MULTI.GET.LOC.REF so removed


RETURN
PROCESS:

   
    IF Y.AIA.POLICY.NO[8,1] EQ 'A' THEN
        IF Y.AIA.POLICY.NO[1,7] GE '1945151' AND Y.AIA.POLICY.NO[1,7] LE '1988150' THEN
            Y.STEP = 'A'
            GOSUB STEP1
            GOSUB STEP2
        END
        IF Y.AIA.POLICY.NO[1,7] GE '6390529' AND Y.AIA.POLICY.NO[1,7] LE '6392528' THEN
            Y.STEP = 'A'
            GOSUB STEP1
            GOSUB STEP2
        END
        IF Y.STEP NE 'A' THEN
            Y.STEP = 'B'
            GOSUB STEP1
            GOSUB STEP2
        END
    END


    Y.CHK.DIGIT.ORIG = Y.AIA.POLICY.NO[9,2]
    IF Y.CHK.DIGIT EQ '' OR Y.CHK.DIGIT.ORIG EQ '' THEN
* AV = Y.POS          
        AV = Y.AMR.POLICY.POS            ;*Code Review.1 - Dupliacte MULTI.GET.LOC.REF so Y.POS Variable Changed
        ETEXT = 'EM-INS.POLICY.NO.WRONG'
        CALL STORE.END.ERROR
        RETURN
    END
	IF Y.CHK.DIGIT NE Y.CHK.DIGIT.ORIG THEN
* AV = Y.POS 	 
        AV = Y.AMR.POLICY.POS            ;*Code Review.1 - Dupliacte MULTI.GET.LOC.REF so Y.POS Variable Changed
        ETEXT = 'EM-INS.POLICY.NO.WRONG'
        CALL STORE.END.ERROR
        RETURN
	END
RETURN

STEP1:
    Y.ARR.1A = '9':VM:'8':VM:'7':VM:'6':VM:'5':VM:'4':VM:'3':VM:'2'
    Y.ARR.1B = '3':VM:'2':VM:'7':VM:'6':VM:'5':VM:'4':VM:'3':VM:'2'

    IF Y.STEP EQ 'A' THEN
        Y.ARR = Y.ARR.1A
    END ELSE
        Y.ARR = Y.ARR.1B
    END

    Y.CNT = LEN(Y.AIA.POLICY.NO)
    I = 1
    LOOP
    WHILE I LE Y.CNT
        Y.CURR.CHAR = Y.AIA.POLICY.NO[I,1]
        IF ISDIGIT(Y.CURR.CHAR) THEN
            Y.VAR1 = Y.CURR.CHAR * Y.ARR<1,I>
        END ELSE
            GOSUB CHAR.EQU.DIGIT
            Y.VAR1 = Y.CURR.CHAR * Y.ARR<1,I>
        END
        Y.VAR1.TOTAL = Y.VAR1.TOTAL + Y.VAR1
        I = I + 1
    REPEAT
RETURN

CHAR.EQU.DIGIT:

    BEGIN CASE
        CASE Y.CURR.CHAR EQ 'A' OR Y.CURR.CHAR EQ 'K' OR Y.CURR.CHAR EQ 'U'
            Y.CURR.CHAR = '0'

        CASE Y.CURR.CHAR EQ 'B' OR Y.CURR.CHAR EQ 'L' OR Y.CURR.CHAR EQ 'V'
            Y.CURR.CHAR = '1'

        CASE Y.CURR.CHAR EQ 'C' OR Y.CURR.CHAR EQ 'M' OR Y.CURR.CHAR EQ 'W'
            Y.CURR.CHAR = '2'

        CASE Y.CURR.CHAR EQ 'D' OR Y.CURR.CHAR EQ 'N' OR Y.CURR.CHAR EQ 'X'
            Y.CURR.CHAR = '3'

        CASE Y.CURR.CHAR EQ 'E' OR Y.CURR.CHAR EQ 'O' OR Y.CURR.CHAR EQ 'Y'
            Y.CURR.CHAR = '4'

        CASE Y.CURR.CHAR EQ 'F' OR Y.CURR.CHAR EQ 'P' OR Y.CURR.CHAR EQ 'Z'
            Y.CURR.CHAR = '5'

        CASE Y.CURR.CHAR EQ 'G' OR Y.CURR.CHAR EQ 'Q'
            Y.CURR.CHAR = '6'

        CASE Y.CURR.CHAR EQ 'H' OR Y.CURR.CHAR EQ 'R'
            Y.CURR.CHAR = '7'

        CASE Y.CURR.CHAR EQ 'I' OR Y.CURR.CHAR EQ 'S'
            Y.CURR.CHAR = '8'

        CASE Y.CURR.CHAR EQ 'J' OR Y.CURR.CHAR EQ 'T'
            Y.CURR.CHAR = '9'

    END CASE

RETURN

STEP2:

    Y.I = Y.VAR1.TOTAL
    Y.I = Y.I - 11
    LOOP
    WHILE Y.I GT '10'
        Y.I = Y.I - 11
    REPEAT

    IF Y.I EQ '0' THEN
        Y.CHK.DIGIT = 00
    END
    IF Y.I GT '0' THEN
        Y.CHK.DIGIT = 11 - Y.I
    END
RETURN

END

