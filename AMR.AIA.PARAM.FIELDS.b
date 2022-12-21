*-----------------------------------------------------------------------------
* <Rating>-11</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE AMR.AIA.PARAM.FIELDS
*-----------------------------------------------------------------------------
*<doc>
* Template for field definitions routine YOURAPPLICATION.FIELDS
*
* @author tcoleman@temenos.com
* @stereotype fields template
* @uses Table
* @public Table Creation
* @package infra.eb
* </doc>
*-----------------------------------------------------------------------------
* Modification History :
*
* 19/10/07 - EN_10003543
*            New Template changes
*
* 14/11/07 - BG_100015736
*            Exclude routines that are not released
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
*** </region>
*-----------------------------------------------------------------------------
*   CALL Table.defineId("TABLE.NAME.ID", T24_String) ;* Define Table id
    ID.F = 'AMR.AIA.PARAM'
    ID.N = '6'
    ID.T = ''
    ID.T<2> = "SYSTEM"
    

*-----------------------------------------------------------------------------
*    CALL Table.addField(fieldName, fieldType, args, neighbour) ;* Add a new fields
*

    CALL Table.addFieldDefinition('AIA.COMMERCE.ACCOUNT',35,'A','')
    CALL Table.addFieldDefinition('RECIEVER.ACCOUNT',35,'A','')
    CALL Table.addFieldDefinition('SENDER.ACCOUNT',35,'A','')
    CALL Table.addFieldDefinition('SENDER.PERCENTAGE',4,'A','')
    CALL Table.addFieldDefinition('RECIEVER.PERCENTAGE',4,'A','')
    CALL Table.addFieldDefinition('XX.NOFEE.BRANCH',10,'A','')
    CALL Field.setCheckFile("COMPANY")
	CALL Table.addFieldDefinition('USD.MIN.AMOUNT',15,'A','')
	CALL Table.addFieldDefinition('USD.MAX.AMOUNT',15,'A','')
	CALL Table.addFieldDefinition('KHR.MIN.AMOUNT',15,'A','')
	CALL Table.addFieldDefinition('KHR.MAX.AMOUNT',15,'A','')
	CALL Table.addFieldDefinition('THB.MIN.AMOUNT',15,'A','')
	CALL Table.addFieldDefinition('THB.MAX.AMOUNT',15,'A','')
    CALL Table.addFieldDefinition('XX<CREDIT.CURRENCY',3,'A','')
	CALL Field.setCheckFile("CURRENCY")
    CALL Table.addFieldDefinition('XX>CREDIT.ACCOUNT',20,'A','')
    CALL Table.addFieldDefinition('INS.COMPANY',15,'A','')
    CALL Field.setCheckFile("COMPANY")
    CALL Table.addFieldDefinition('INS.FILE.NAME',35,'A','')
	CALL Table.addFieldDefinition('INS.AIA.FILE.PATH',60,'A','')
    CALL Table.addFieldDefinition('INS.RESP.FILE.PATH',60,'A','')
	CALL Table.addFieldDefinition('INS.OFS.FILE.PATH',60,'A','')
    
    CALL Table.addLocalReferenceField("")
    CALL Table.addReservedField("Reserved5")
    CALL Table.addReservedField("Reserved4")
    CALL Table.addReservedField("Reserved3")
    CALL Table.addReservedField("Reserved2")
    CALL Table.addReservedField("Reserved1")
    CALL Table.addOverrideField
*    CALL Table.addFieldWithEbLookup(fieldName,virtualTableName,neighbour) ;* Specify Lookup values
*    CALL Field.setDefault(defaultValue) ;* Assign default value
*-----------------------------------------------------------------------------
    CALL Table.setAuditPosition         ;* Populate audit information
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END



