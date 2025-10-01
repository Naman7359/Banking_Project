BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE v-file-name     AS CHARACTER NO-UNDO INITIAL "C:\Users\Suraj.chauhan\Downloads\DummyLocationData-final1.txt".
DEFINE VARIABLE v-line          AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-field-count   AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-record-count  AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE v-skip-header   AS LOGICAL   NO-UNDO INITIAL TRUE.
DEFINE VARIABLE v-country-count AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE v-state-count   AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE v-city-count    AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE v-postal-count  AS INTEGER   NO-UNDO INITIAL 0.
 
/* Field variables for parsing */
DEFINE VARIABLE v-country-name  AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-country-code  AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-state-name    AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-state-code    AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-city-name     AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-city-code     AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-zip-code      AS CHARACTER NO-UNDO.
 
/* ********************  Preprocessor Definitions  ******************** */
 
/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE populateAccountType:
    /*------------------------------------------------------------------------------
     Purpose: Populate AccountType temp-table from CSV stored in LONGCHAR
     Notes : Creates 6 entries
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lcCSV AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cLine AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLine AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iNum  AS INTEGER   NO-UNDO.

    DO ON ERROR UNDO, THROW:

        /* CSV data in LONGCHAR */
        lcCSV = 
            "1,Saving,FD"                + CHR(10) +
            "2,Saving,RD"                + CHR(10) +
            "3,Loan,Personal Loan"       + CHR(10) +
            "4,Loan,Home Loan"           + CHR(10) +
            "5,Loan,Car Loan"            + CHR(10) +
            "6,Loan,Education Loan".

        /* Count number of lines */
        iNum = NUM-ENTRIES(lcCSV, CHR(10)).

        MESSAGE "Starting AccountType population. Total rows: " iNum
            VIEW-AS ALERT-BOX INFO.

        DO iLine = 1 TO iNum:
            cLine = ENTRY(iLine, lcCSV, CHR(10)).

            IF cLine = "" THEN NEXT. /* Skip blank lines */

            CREATE AccountType.
            ASSIGN
                AccountType.AccountTypeID = INTEGER(ENTRY(1, cLine, ","))
                AccountType.AccountType   = TRIM(ENTRY(2, cLine, ","))
                AccountType.AccSubType    = TRIM(ENTRY(3, cLine, ",")).

        END.

        MESSAGE "AccountType population completed successfully."
            VIEW-AS ALERT-BOX INFO.
    END.
    CATCH e AS Progress.Lang.Error:
        MESSAGE "Error populating AccountType: " e:GetMessage(1)
            VIEW-AS ALERT-BOX ERROR.
    END CATCH.

END PROCEDURE.


MESSAGE "Starting data loading process..." VIEW-AS ALERT-BOX INFORMATION.
 
/* Delete existing records in reverse order to maintain referential integrity */
MESSAGE "Deleting existing records..." VIEW-AS ALERT-BOX.
 
FOR EACH BankDB.PostalCode:
    DELETE BankDB.PostalCode.
END.
 
FOR EACH BankDB.City:
    DELETE BankDB.City.
END.
 
FOR EACH BankDB.State:
    DELETE BankDB.State.
END.
 
FOR EACH BankDB.Country:
    DELETE BankDB.Country.
END.
 
MESSAGE "Existing records deleted successfully." VIEW-AS ALERT-BOX.
 
/* Open and read the input file */
RUN populateAccountType.

DO ON ERROR UNDO, THROW:
    INPUT FROM VALUE(v-file-name) NO-ECHO.
 
    REPEAT ON ERROR UNDO, LEAVE:
        IMPORT UNFORMATTED v-line.
        /* Handle end of file */
        IF v-line = ? OR v-line = "" THEN LEAVE.
        /* Skip header row */
        IF v-skip-header THEN 
        DO:
            v-skip-header = FALSE.
            NEXT.
        END.
        /* Parse tab-delimited line */
        ASSIGN
            v-field-count  = NUM-ENTRIES(v-line, CHR(9))
            v-country-name = ENTRY(1, v-line, CHR(9))
            v-country-code = ENTRY(2, v-line, CHR(9))
            v-state-name   = ENTRY(3, v-line, CHR(9))
            v-state-code   = ENTRY(4, v-line, CHR(9))
            v-city-name    = ENTRY(5, v-line, CHR(9))
            v-city-code    = ENTRY(6, v-line, CHR(9))
            v-zip-code     = ENTRY(7, v-line, CHR(9)).
        /* Validate that we have all required fields */
        IF v-field-count >= 7 THEN 
        DO:
            /* Insert Country if not exists */
            FIND FIRST BankDB.Country WHERE 
                BankDB.Country.CountryCode = TRIM(v-country-code) NO-ERROR.
            IF NOT AVAILABLE BankDB.Country THEN 
            DO:
                CREATE BankDB.Country.
                ASSIGN
                    BankDB.Country.CountryCode = TRIM(v-country-code)
                    BankDB.Country.CountryName = TRIM(v-country-name).
                v-country-count = v-country-count + 1.
            END.
            /* Insert State if not exists */
            FIND FIRST BankDB.State WHERE 
                BankDB.State.StateCode = TRIM(v-state-code) AND
                BankDB.State.CountryCode = TRIM(v-country-code) NO-ERROR.
            IF NOT AVAILABLE BankDB.State THEN 
            DO:
                CREATE BankDB.State.
                ASSIGN
                    BankDB.State.StateCode   = TRIM(v-state-code)
                    BankDB.State.StateName   = TRIM(v-state-name)
                    BankDB.State.CountryCode = TRIM(v-country-code).
                v-state-count = v-state-count + 1.
            END.
            /* Insert City if not exists */
            FIND FIRST BankDB.City WHERE 
                BankDB.City.CityCode = TRIM(v-city-code) AND
                BankDB.City.StateCode = TRIM(v-state-code) NO-ERROR.
            IF NOT AVAILABLE BankDB.City THEN 
            DO:
                CREATE BankDB.City.
                ASSIGN
                    BankDB.City.CityCode  = TRIM(v-city-code)
                    BankDB.City.CityName  = TRIM(v-city-name)
                    BankDB.City.StateCode = TRIM(v-state-code).
                v-city-count = v-city-count + 1.
            END.
            /* Insert PostalCode - these might have duplicates in your data */
            FIND FIRST BankDB.PostalCode WHERE 
                BankDB.PostalCode.ZIPCode = TRIM(v-zip-code) NO-ERROR.
            IF NOT AVAILABLE BankDB.PostalCode THEN 
            DO:
                CREATE BankDB.PostalCode.
                ASSIGN
                    BankDB.PostalCode.ZIPCode  = TRIM(v-zip-code)
                    BankDB.PostalCode.CityCode = TRIM(v-city-code).
                v-postal-count = v-postal-count + 1.
            END.
            v-record-count = v-record-count + 1.
            /* Display progress every 50 records */
            IF v-record-count MODULO 50 = 0 THEN 
            DO:
                MESSAGE "Processed" v-record-count "records..." VIEW-AS ALERT-BOX.
            END.
        END.
        ELSE 
        DO:
            MESSAGE "Invalid line format - skipping:" v-line VIEW-AS ALERT-BOX.
        END.
    END. /* REPEAT */
 
    INPUT CLOSE.
END. /* DO ON ERROR */
 
/* Display completion message */
MESSAGE "Data loading completed!" SKIP
    "Total records processed:" v-record-count SKIP
    "Countries inserted:" v-country-count SKIP
    "States inserted:" v-state-count SKIP
    "Cities inserted:" v-city-count SKIP
    "PostalCodes inserted:" v-postal-count SKIP
    "Data loaded successfully into normalized tables."
    VIEW-AS ALERT-BOX INFORMATION.
 
 
MESSAGE "Program execution completed successfully!" VIEW-AS ALERT-BOX INFORMATION.