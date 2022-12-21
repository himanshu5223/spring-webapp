SUBROUTINE AMR.DEF.PARTN.ACC.NAME
*-----------------------------------------------------------------------------
* Description Name        : To default Partner account name when the partner account no is entered
* Developer Name          : M MANOJ
* Company Name            : Techmill Technologies pvt ltd
* ODR Number              : N/A
* Subroutine Type         : S
* Attached to             : VERSION>ACCOUNT,AMR.COA
*                         : VERSION>ACCOUNT,AMR.CA
* Incoming Parameters     : N/A
* Outgoing Parameters     : N/A
* Development Date        : 02/06/2020
* Development name        : ACCOUNT
* Code Review_Amendment Date :   24-12-2020
*-------------------------------------------------------------------------*
* Kiran HS      04-Jan-2021     Review
*-----------------------------------------------------------------------------
* VERSION             : 1.0
* DATE CREATED        : 14/OCT/2021
* CREATED BY          : Seit Lyheang
* DESCRIPTION         : CHG0033874
*                       Add new Field on COA, CA, WA, GA 
* ATTACHED TO         : ACCOUNT,AMR.COA
*					  : ACCOUNT,AMR.CA
* ATTACHED AS         : Input Routine
* IN/OUT ARGUMENTS    : N/A
*-----------------------------------------------------------------------------
* VERSION             : 1.0
* DATE CREATED        : 16/FEB/2021
* CREATED BY          : Seit Lyheang
* DESCRIPTION         : INC0026678
*                     : Allow MSO to create without validate field in this ticket CHG0033874
* ATTACHED TO         : ACCOUNT,AMR.COA
*					  : ACCOUNT,AMR.CA
* ATTACHED AS         : Input Routine
* IN/OUT ARGUMENTS    : N/A
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER
	 
	 
    GOSUB INIT
    GOSUB PROCESS
    GOSUB CHECK.OTHER.SERV.VAL
	
RETURN
****	 
INIT:
****
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT =  ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER =  ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    Y.APP = 'ACCOUNT'
    Y.FIELD = 'AMR.INST.NAME':VM:'AMR.OTHER.SERV':VM:'AMR.NOTE.OTH.SE':VM:'AMR.COM.ACC.NO':VM:'AMR.POLICY.NO':VM:'SOURCE.FUNDS':VM:'OTHER.FUNDS':VM:'SAVE.PURPOSES':VM:'PURPOSE'
    Y.POS = ''
    CALL MULTI.GET.LOC.REF(Y.APP,Y.FIELD,Y.POS)
	
RETURN
********	
PROCESS:
********

    AMR.ACC.NAME.POS = Y.POS<1,1>
    AMR.OTHER.SERV.POS = Y.POS<1,2>
    NOTE.OTHER.SERV.POS = Y.POS<1,3>
    AMR.COM.ACC.NO.POS = Y.POS<1,4>
    Y.AMR.POLICY.NO.POS = Y.POS<1,5>
	Y.SOURCE.FUNDS.POS = Y.POS<1,6>
	Y.OTHER.FUNDS.POS = Y.POS<1,7>
	Y.SAVE.PURPOSES.POS = Y.POS<1,8>
	Y.PURPOSE.POS = Y.POS<1,9>
	Y.VERSION = APPLICATION:PGM.VERSION
*****CHG0033874-START*****

		Y.SAVE.PURPOSE = R.NEW(AC.LOCAL.REF)<1,Y.SAVE.PURPOSES.POS>
		Y.AMR.PURPOSE = R.NEW(AC.LOCAL.REF)<1,Y.PURPOSE.POS>
		Y.SOURCE.FUNDS = R.NEW(AC.LOCAL.REF)<1,Y.SOURCE.FUNDS.POS>
		Y.OTHER.FUNDS = R.NEW(AC.LOCAL.REF)<1,Y.OTHER.FUNDS.POS>
	
	*IF Y.VERSION NE "ACCOUNT,AMR.COA.AMEND" AND Y.VERSION NE "ACCOUNT,AMR.CA.AMEND" THEN *;INC0026678
	IF Y.VERSION EQ "ACCOUNT,AMR.COA" OR Y.VERSION EQ "ACCOUNT,AMR.CA" THEN
	
		IF Y.SAVE.PURPOSE EQ '' THEN
				AF = AC.LOCAL.REF
				AV = Y.SAVE.PURPOSES.POS
				ETEXT = 'CHG0033874: Saving Purpose field cannot be blank'
				CALL STORE.END.ERROR
				RETURN
		END
		ELSE IF Y.SAVE.PURPOSE NE '999' THEN 
			IF Y.AMR.PURPOSE NE '' THEN 
				AF = AC.LOCAL.REF
				AV = Y.PURPOSE.POS
				ETEXT = 'CHG0033874: Other Purpose does not allow to fill information when choose option above'
				CALL STORE.END.ERROR
				RETURN
			END
		END
		ELSE IF Y.SAVE.PURPOSE EQ '999' THEN 
			IF Y.AMR.PURPOSE EQ '' THEN 
				AF = AC.LOCAL.REF
				AV = Y.PURPOSE.POS
				ETEXT = 'CHG0033874: Other Purpose field need to fill the information when choose option 999'
				CALL STORE.END.ERROR
				RETURN
			END 
		END
			
		IF Y.SOURCE.FUNDS EQ '' THEN
			AF = AC.LOCAL.REF
			AV = Y.SOURCE.FUNDS.POS
			ETEXT = 'CHG0033874: Source of Funds field cannot be blank'
			CALL STORE.END.ERROR
			RETURN
		END
		ELSE IF Y.SOURCE.FUNDS NE '999' THEN
				IF Y.OTHER.FUNDS NE '' THEN 
					AF = AC.LOCAL.REF
					AV = Y.OTHER.FUNDS.POS
					ETEXT = 'CHG0033874: Other Source of Funds does not allow to fill information when choose option above'
					CALL STORE.END.ERROR
					RETURN
				END
		END
		ELSE IF Y.SOURCE.FUNDS EQ '999' THEN
			IF Y.OTHER.FUNDS EQ '' THEN 
				AF = AC.LOCAL.REF
				AV = Y.OTHER.FUNDS.POS
				ETEXT = 'CHG0033874: Other Source of Funds field need to fill the information when choose option 999'
				CALL STORE.END.ERROR
				RETURN
			END
		END
	END
*****CHG0033874-END*****

    ACC.NO = R.NEW(AC.LOCAL.REF)<1,AMR.COM.ACC.NO.POS>
    IF ACC.NO EQ "" THEN
        RETURN
    END

    ACC.OTHER.SERV.LIST = R.NEW(AC.LOCAL.REF)<1,AMR.OTHER.SERV.POS>
    ACC.OTHER.SERV.COUNT = DCOUNT(ACC.OTHER.SERV.LIST,SM)

    ACC.LIST = R.NEW(AC.LOCAL.REF)<1,AMR.COM.ACC.NO.POS>

    R.NEW(AC.LOCAL.REF)<1,AMR.ACC.NAME.POS> = ''
    ACC.REC=""
	I = 1
	LOOP
	WHILE I LE ACC.OTHER.SERV.COUNT
        ACC.NO = FIELD(ACC.LIST,SM,I)

        CALL F.READ(FN.ACCOUNT,ACC.NO,ACC.REC,F.ACCOUNT,ACC.ERR)

        IF ACC.NO NE '' AND ACC.REC EQ "" THEN
            ACC.NAME = ''
            AF = AC.LOCAL.REF
            AV = AMR.COM.ACC.NO.POS
            AS = I
            ETEXT = 'EB-ACC.NO.NOT.FOUND':FM:ACC.NO
            CALL STORE.END.ERROR
            RETURN
        END
        ACC.NAME = ACC.REC<AC.ACCOUNT.TITLE.1,1> ;*INC0024956 prevent account over space by user
        R.NEW(AC.LOCAL.REF)<1,AMR.ACC.NAME.POS,I> = ACC.NAME
        I = I + 1
    REPEAT
	
RETURN
*******************
CHECK.OTHER.SERV.VAL:
*******************
    Y.AMR.OTHER.SERV = R.NEW(AC.LOCAL.REF)<1,AMR.OTHER.SERV.POS>
    Y.AMR.OTHER.SERV.DUP = R.NEW(AC.LOCAL.REF)<1,AMR.OTHER.SERV.POS>
    CHANGE SM TO VM IN Y.AMR.OTHER.SERV.DUP
    Y.NOTE.OTHER.SERV = R.NEW(AC.LOCAL.REF)<1,NOTE.OTHER.SERV.POS>
    AMR.OTHER.SERV.CNT = DCOUNT(Y.AMR.OTHER.SERV,SM)
    Y.AMR.POLICY.NO = R.NEW(AC.LOCAL.REF)<1,Y.AMR.POLICY.NO.POS>
	
    IF AMR.OTHER.SERV.CNT EQ 1 THEN
          GOSUB AMR.OTHER.SERV.CASE.CHK  ;*Code Review.1 - Code Statements moved to GOSUB
    END
    GOSUB GET.AMR.OTHER.SERV	 ;*Code Review.1 - Code Statements moved to GOSUB
    
     
RETURN
*******************
AMR.OTHER.SERV.CASE.CHK:
*******************
	BEGIN CASE
    
            CASE Y.AMR.OTHER.SERV<1,1,1> EQ '' AND Y.NOTE.OTHER.SERV<1,1,1> NE ''

                AF = AC.LOCAL.REF
                AV = AMR.OTHER.SERV.POS
                AS = 1
                ETEXT = 'EB-MUST.INP.SERV'
                CALL STORE.END.ERROR
                RETURN
        
            CASE UPCASE(Y.AMR.OTHER.SERV<1,1,1>) EQ 'AIA INSURANCE-LI' AND  Y.AMR.POLICY.NO<1,1,1> EQ ''
                AF = AC.LOCAL.REF
                AV = Y.AMR.POLICY.NO.POS
                AS = 1
                ETEXT = 'EB-AIA.POLICY.NO.INPUT'
                CALL STORE.END.ERROR
                RETURN
*INC0013324-S          
*            CASE Y.AMR.POLICY.NO<1,1,1> NE '' AND UPCASE(Y.AMR.OTHER.SERV<1,1,1>) NE 'AIA INSURANCE-LI'
*                AF = AC.LOCAL.REF
*                AV = AMR.OTHER.SERV.POS
*                AS =  1
*                ETEXT = 'EB-SERVICE.TYPE.MUST.BE.LI'
*                CALL STORE.END.ERROR
*INC0013324-E                
            CASE Y.AMR.POLICY.NO<1,1,1> NE '' AND UPCASE(Y.AMR.OTHER.SERV<1,1,1>) EQ 'AIA INSURANCE-LI'
                Y.POLICY.NO = Y.AMR.POLICY.NO<1,1,1>
                Y.RESULT = ''
                CALL AMR.AIA.POLICY.NO(Y.POLICY.NO,Y.RESULT)
                IF Y.RESULT NE '1' THEN
                    AF = AC.LOCAL.REF
                    AV = Y.AMR.POLICY.NO.POS
                    AS = 1
                    ETEXT = 'EB-INVALID.AIA.POLICY.NO'
                    CALL STORE.END.ERROR
                    RETURN
                END
                RETURN
     END CASE
RETURN
*******************
GET.AMR.OTHER.SERV:
*******************
II = 1
    LOOP
    WHILE II LE AMR.OTHER.SERV.CNT
 	
        Y.AMR.OTHER.SERV<1,1,II> = UPCASE(Y.AMR.OTHER.SERV<1,1,II>)
        Y.POLICY.CHK = Y.AMR.POLICY.NO<1,1,II>
        Y.CNT.POLICY.NO = DCOUNT(Y.AMR.POLICY.NO,Y.POLICY.CHK)
        Y.SERVICE.TYPE  = Y.AMR.OTHER.SERV<1,1,II>
        Y.CNT.SERV = DCOUNT(Y.AMR.OTHER.SERV,Y.SERVICE.TYPE)
        GOSUB OTHER.SERV.CNT.CHK  ;*Code Review.1 - Code Statements moved to GOSUB

        IF AMR.OTHER.SERV.CNT GT 1 THEN
           GOSUB OTHER.SERV.ERR.CHK ;*Code Review.1 - Code Statements moved to GOSUB
        END
        II = II + 1
    REPEAT
RETURN
*******************
OTHER.SERV.CNT.CHK:
*******************
*INC0013324-S
       IF Y.SERVICE.TYPE NE 'AIA INSURANCE-LI' AND Y.SERVICE.TYPE NE 'AIA INSURANCE-LRI' THEN
*INC0013324-E
        IF Y.CNT.SERV GE 3 THEN
            AF = AC.LOCAL.REF
            AV = AMR.OTHER.SERV.POS
            AS = II
            ETEXT = 'EB-CANNOT.INPUT.DUP.SERVICE.TYPE'
            CALL STORE.END.ERROR
            RETURN
        END
       END
        
        IF Y.AMR.OTHER.SERV<1,1,II> EQ 'OTHER' AND Y.NOTE.OTHER.SERV<1,1,II> EQ '' THEN
            AF = AC.LOCAL.REF
            AV = NOTE.OTHER.SERV.POS
            AS = II
            ETEXT = 'EB-SERV.EQ.OTH.MUST.INP.SERVICE':FM:II:VM:II
            CALL STORE.END.ERROR
            RETURN
        END

RETURN
*******************
OTHER.SERV.ERR.CHK:
*******************
	   IF Y.AMR.OTHER.SERV<1,1,II> EQ '' THEN
                AF = AC.LOCAL.REF
                AV = AMR.OTHER.SERV.POS
                AS = II
                ETEXT = 'EB-SERV.COULD.NOT.BE.NULL':FM:II
                CALL STORE.END.ERROR
                RETURN
            END

            IF Y.NOTE.OTHER.SERV<1,1,II> EQ '' THEN
                AF = AC.LOCAL.REF
                AV = NOTE.OTHER.SERV.POS
                AS = II
                ETEXT = 'EB-OTHER.SERV.COULD.NOT.BE.NULL':FM:II
                CALL STORE.END.ERROR
                RETURN
            END
            IF UPCASE(Y.AMR.OTHER.SERV<1,1,II>) EQ 'AIA INSURANCE-LI' AND  Y.AMR.POLICY.NO<1,1,II> EQ '' THEN
                AF = AC.LOCAL.REF
                AV = Y.AMR.POLICY.NO.POS
                AS = II
                ETEXT = 'EB-AIA.POLICY.NO.INPUT'
                CALL STORE.END.ERROR
                RETURN
            END
*INC0013324-S
*            IF Y.AMR.POLICY.NO<1,1,1> NE '' AND UPCASE(Y.AMR.OTHER.SERV<1,1,1>) NE 'AIA INSURANCE-LI' THEN
*                AF = AC.LOCAL.REF
*                AV = AMR.OTHER.SERV.POS
*                AS = II
*                ETEXT = 'EB-SERVICE.TYPE.MUST.BE.LI'
*                CALL STORE.END.ERROR
*                RETURN
*            END
*INC0013324-E
            IF UPCASE(Y.AMR.OTHER.SERV<1,1,II>) EQ 'AIA INSURANCE-LI' AND  Y.AMR.POLICY.NO<1,1,II> NE '' THEN
                Y.POLICY.NO = Y.AMR.POLICY.NO<1,1,II>
                Y.RESULT = ''
                CALL AMR.AIA.POLICY.NO(Y.POLICY.NO,Y.RESULT)
                IF Y.RESULT NE '1' THEN
                    AF = AC.LOCAL.REF
                    AV = Y.AMR.POLICY.NO.POS
                    AS = II
                    ETEXT = 'EB-INVALID.AIA.POLICY.NO'
                    CALL STORE.END.ERROR
                    RETURN
                END
            END
*INC0013324-S
           IF Y.CNT.POLICY.NO GT 2 THEN
		 AF = AC.LOCAL.REF
                 AV = Y.AMR.POLICY.NO.POS
           	 AS = II
           	 ETEXT = 'EB-DUPLICATE.POLICY.NO'
          	 CALL STORE.END.ERROR
          	 RETURN


	   END	
*INC0013324-E
 RETURN
END