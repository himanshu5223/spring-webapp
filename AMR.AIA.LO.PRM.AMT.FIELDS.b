SUBROUTINE AMR.AIA.LO.PRM.AMT.FIELDS
*-----------------------------------------------------------------------------
*<doc>
* Template for field definitions routine <<PREFIX>>.SDFG.FIELDS
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
    CALL Table.defineId("ID", T24_String) ;* Define Table id
*-----------------------------------------------------------------------------
*CALL Table.addField(fieldName, fieldType, args, neighbour) ;* Add a new fields
*CALL Field.setCheckFile(fileName)        ;* Use DEFAULT.ENRICH from SS or just field 1
    
    CALL Table.addFieldDefinition('DATE.OF.BIRTH', '13', 'A', '') ;* Add a new field
    CALL Table.addFieldDefinition('GENDER', '10', 'A', '')
    CALL Table.addFieldDefinition('INDUSTRY', '8', 'A', '')
    CALL Table.addFieldDefinition('APPROVED.AMOUNT', '35', 'A', '')
    CALL Table.addFieldDefinition('GRACE.PERIOD', '15', 'A', '')
    CALL Table.addFieldDefinition('LOAN.TERM', '6', 'A', '')
    CALL Table.addFieldDefinition('PREMIUM.AMOUNT', '25', 'A', '')
    CALL Table.addFieldDefinition('LOAN.APPLICATION.ID', '25', 'A', '')
    CALL Table.addFieldDefinition('POLICY.NUMBER', '25', 'A', '')
    CALL Table.addFieldDefinition('OCCUPATION', '35', 'A', '')
    CALL Table.addFieldDefinition('LOAN.INTEREST.RATE', '10', 'R', '')
      
    
*CALL Table.addFieldWithEbLookup(fieldName,virtualTableName,neighbour) ;* Specify Lookup values
*CALL Field.setDefault(defaultValue) ;* Assign default value
*-----------------------------------------------------------------------------
    CALL Table.setAuditPosition ;* Poputale audit information
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END
