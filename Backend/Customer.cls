
/*------------------------------------------------------------------------
    File        : Controller.cls
    Purpose     : Backend controller for Customer operations
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.

CLASS Backend.Customer:
    
    {Backend/Utility/temp_table.i}
        
    CONSTRUCTOR PUBLIC Customer():
        SUPER().
    END CONSTRUCTOR.
    
    METHOD PUBLIC LOGICAL GetCustomerDetails(INPUT piCustId AS INTEGER,
        OUTPUT TABLE ttCustomerDetails):
        EMPTY TEMP-TABLE ttCustomerDetails.
        
        FIND FIRST CustomerDetails WHERE CustomerDetails.CustID = piCustId NO-LOCK NO-ERROR.
        IF AVAILABLE CustomerDetails THEN 
        DO:
            CREATE ttCustomerDetails.
            ASSIGN 
                ttCustomerDetails.CustID         = CustomerDetails.CustID
                ttCustomerDetails.FirstName      = CustomerDetails.FirstName
                ttCustomerDetails.LastName       = CustomerDetails.LastName
                ttCustomerDetails.date_of_birth  = CustomerDetails.DOB
                ttCustomerDetails.marital_status = CustomerDetails.MaritalStatus
                ttCustomerDetails.address        = CustomerDetails.Address1
                ttCustomerDetails.address_2      = CustomerDetails.Address2
                ttCustomerDetails.City           = CustomerDetails.City
                ttCustomerDetails.State          = CustomerDetails.State
                ttCustomerDetails.postal_code    = CustomerDetails.ZipCode
                ttCustomerDetails.Country        = CustomerDetails.Country
                ttCustomerDetails.email          = CustomerDetails.EmailId
                ttCustomerDetails.mobile_num     = CustomerDetails.Phone.
            RETURN TRUE.
        END.
        RETURN FALSE.
    END METHOD.
        
        
    METHOD PUBLIC LOGICAL CustomerDetailsExists(INPUT piCustId AS INTEGER):
        FIND FIRST CustomerDetails WHERE CustomerDetails.CustID = piCustId NO-LOCK NO-ERROR.
        RETURN AVAILABLE CustomerDetails.
    END METHOD.
    
    
    METHOD PUBLIC LOGICAL AddCustomerDetails(INPUT TABLE ttCustomerDetails):
        DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO INITIAL FALSE.
        
        DO TRANSACTION ON ERROR UNDO, THROW:
            FOR EACH ttCustomerDetails:
                CREATE CustomerDetails.
                ASSIGN 
                    CustomerDetails.CustID        = NEXT-VALUE(AccountIDSeq)
                    CustomerDetails.FirstName     = ttCustomerDetails.FirstName
                    CustomerDetails.LastName      = ttCustomerDetails.LastName
                    CustomerDetails.DOB           = ttCustomerDetails.date_of_birth
                    CustomerDetails.MaritalStatus = ttCustomerDetails.marital_status
                    CustomerDetails.Address1      = ttCustomerDetails.address
                    CustomerDetails.Address2      = ttCustomerDetails.address_2
                    CustomerDetails.City          = ttCustomerDetails.City
                    CustomerDetails.State         = ttCustomerDetails.State
                    CustomerDetails.ZipCode       = ttCustomerDetails.postal_code
                    CustomerDetails.Country       = ttCustomerDetails.Country
                    CustomerDetails.EmailId       = ttCustomerDetails.email
                    CustomerDetails.Phone         = ttCustomerDetails.mobile_num.
                
                ttCustomerDetails.CustID = CustomerDetails.CustID.
                lSuccess = TRUE.
            END.
        END.
        
        RETURN lSuccess.
    END METHOD.
    
    
    
    METHOD PUBLIC LOGICAL UpdateCustomerDetails(INPUT TABLE ttCustomerDetails):
        DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO INITIAL FALSE.
        DO ON ERROR UNDO, THROW:
            
            FIND FIRST ttCustomerDetails NO-LOCK NO-ERROR.
            IF AVAILABLE ttCustomerDetails THEN 
            DO:
                                    
                DO TRANSACTION ON ERROR UNDO,THROW:
                    FIND FIRST CustomerDetails WHERE CustomerDetails.CustID = ttCustomerDetails.CustID EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAILABLE CustomerDetails THEN 
              
                    DO:
                        MESSAGE "in transaction".
                        ASSIGN 
                            CustomerDetails.FirstName     = ttCustomerDetails.FirstName
                            CustomerDetails.LastName      = ttCustomerDetails.LastName
                            CustomerDetails.DOB           = ttCustomerDetails.date_of_birth
                            CustomerDetails.MaritalStatus = ttCustomerDetails.marital_status
                            CustomerDetails.Address1      = ttCustomerDetails.address
                            CustomerDetails.Address2      = ttCustomerDetails.address_2
                            CustomerDetails.City          = ttCustomerDetails.City
                            CustomerDetails.State         = ttCustomerDetails.State
                            CustomerDetails.ZipCode       = ttCustomerDetails.postal_code
                            CustomerDetails.Country       = ttCustomerDetails.Country
                            CustomerDetails.EmailId       = ttCustomerDetails.email
                            CustomerDetails.Phone         = ttCustomerDetails.mobile_num
                            lSuccess                      = TRUE.
                            
                            
                        MESSAGE "UPDATE SUCCESSFUL!" SKIP(2)
                            "Updated Customer Details:" SKIP
                            "================================" SKIP
                            "Customer ID:" CustomerDetails.CustID SKIP
                            "Name:" CustomerDetails.FirstName CustomerDetails.LastName SKIP
                            "Date of Birth:" CustomerDetails.DOB SKIP
                            "Marital Status:" CustomerDetails.MaritalStatus SKIP
                            "Address:" CustomerDetails.Address1 SKIP
                            "Address 2:" CustomerDetails.Address2 SKIP
                            "City:" CustomerDetails.City SKIP
                            "State:" CustomerDetails.State SKIP
                            "Postal Code:" CustomerDetails.ZipCode SKIP
                            "Country:" CustomerDetails.Country SKIP
                            "Email:" CustomerDetails.EmailId SKIP
                            "Phone:" CustomerDetails.Phone
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    END.
                END.
            END.
        
            RETURN lSuccess.
        END.
        CATCH e AS Progress.Lang.Error :
            MESSAGE e:GetMessage(1).
        END CATCH.
    END METHOD.
    
    
    
    METHOD PUBLIC LOGICAL DeleteCustomerDetails(INPUT piCustId AS INTEGER):
        DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO INITIAL FALSE.
        
        DO TRANSACTION ON ERROR UNDO, THROW:
            
            FIND FIRST CustomerDetails WHERE CustomerDetails.CustID = piCustId EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE CustomerDetails THEN 
            DO:
                DELETE CustomerDetails.
                lSuccess = TRUE.
            END.
        END.
        
        RETURN lSuccess.
    END METHOD.

    /* Method to search CustomerDetailss by name */
    METHOD PUBLIC LOGICAL SearchCustomerDetailss(INPUT pcSearchText AS CHARACTER,
        OUTPUT TABLE ttCustomerDetails):
        EMPTY TEMP-TABLE ttCustomerDetails.
        
        FOR EACH CustomerDetails WHERE 
            CustomerDetails.FirstName MATCHES ("*" + pcSearchText + "*") OR
            CustomerDetails.LastName MATCHES ("*" + pcSearchText + "*") NO-LOCK:
            
            CREATE ttCustomerDetails.
            ASSIGN 
                ttCustomerDetails.CustID         = CustomerDetails.CustID
                ttCustomerDetails.FirstName      = CustomerDetails.FirstName
                ttCustomerDetails.LastName       = CustomerDetails.LastName
                ttCustomerDetails.date_of_birth  = CustomerDetails.DOB
                ttCustomerDetails.marital_status = CustomerDetails.MaritalStatus
                ttCustomerDetails.address        = CustomerDetails.Address1
                ttCustomerDetails.address_2      = CustomerDetails.Address2
                ttCustomerDetails.City           = CustomerDetails.City
                ttCustomerDetails.State          = CustomerDetails.State
                ttCustomerDetails.postal_code    = CustomerDetails.ZipCode
                ttCustomerDetails.Country        = CustomerDetails.Country
                ttCustomerDetails.email          = CustomerDetails.email
                ttCustomerDetails.mobile_num     = CustomerDetails.Phone.
        END.
        
        FIND FIRST ttCustomerDetails NO-ERROR.
        RETURN AVAILABLE ttCustomerDetails.
    END METHOD.

    /* Method to get CustomerDetails count */
    METHOD PUBLIC INTEGER GetCustomerDetailsCount():
        DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
        
        FOR EACH CustomerDetails NO-LOCK:
            iCount = iCount + 1.
        END.
        
        RETURN iCount.
    END METHOD.
      
        
    METHOD PUBLIC LONGCHAR searchCustomers(
        INPUT cFirstName AS CHARACTER,
        INPUT cLastName  AS CHARACTER,
        INPUT cPhone     AS CHARACTER,
        INPUT cEmail     AS CHARACTER
        ):
        DEFINE VARIABLE oResponse AS JsonArray  NO-UNDO.
        DEFINE VARIABLE oCust     AS JsonObject NO-UNDO.
        DEFINE VARIABLE lcResult  AS LONGCHAR   NO-UNDO.

        oResponse = NEW JsonArray().

        FOR EACH CustomerDetails NO-LOCK
            WHERE (cFirstName = "" OR CustomerDetails.FirstName BEGINS cFirstName)
            AND (cLastName  = "" OR CustomerDetails.LastName  BEGINS cLastName)
            AND (cPhone     = "" OR CustomerDetails.Phone     BEGINS cPhone)
            AND (cEmail     = "" OR CustomerDetails.Email     BEGINS cEmail):

            oCust = NEW JsonObject().
            oCust:Add("CustID",    CustomerDetails.CustID).
            oCust:Add("FirstName", CustomerDetails.FirstName).
            oCust:Add("LastName",  CustomerDetails.LastName).
            oCust:Add("Phone",     CustomerDetails.Phone).
            oCust:Add("Email",     CustomerDetails.Email).

            oResponse:Add(oCust).
        END.

        /* Serialize JSON array to string */
        oResponse:Write(lcResult).

        RETURN lcResult.
    END METHOD.
    
    
    
    METHOD PUBLIC LOGICAL DeleteCustomer(INPUT piCustomerId AS INTEGER):
        
        DEFINE VARIABLE lConfirmDelete AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lSuccess       AS LOGICAL   NO-UNDO INITIAL FALSE.
        DEFINE VARIABLE cCustomerName  AS CHARACTER NO-UNDO.
        
        /* Validate customer ID */
        IF piCustomerId <= 0 THEN 
        DO:
            MESSAGE "Invalid Customer ID provided!" 
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN FALSE.
        END.
        
        /* Check if customer exists and get customer name for confirmation */
        FIND FIRST CustomerDetails WHERE CustomerDetails.CustID = piCustomerId NO-LOCK NO-ERROR.
        
        IF NOT AVAILABLE CustomerDetails THEN 
        DO:
            MESSAGE "Customer with ID" piCustomerId "not found!" 
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN FALSE.
        END.
        
        /* Build customer display name for confirmation */
        cCustomerName = TRIM(CustomerDetails.FirstName) + " " + TRIM(CustomerDetails.LastName).
        
        /* Show confirmation dialog */
        MESSAGE "Are you sure you want to delete the customer?" SKIP(1)
            "Customer ID: " + STRING(piCustomerId) SKIP
            "Customer Name: " + cCustomerName SKIP(1)
            "This action cannot be undone!"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
            UPDATE lConfirmDelete.
        
        /* If user clicked NO, exit without deleting */
        IF NOT lConfirmDelete THEN 
        DO:
            MESSAGE "Delete operation cancelled by user." 
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN FALSE.
        END.
        
        /* Proceed with deletion */
        DO TRANSACTION ON ERROR UNDO, THROW:
            
            /* Check for related records that might prevent deletion */
            FIND FIRST CustomerAccounts WHERE CustomerAccounts.CustID = piCustomerId NO-LOCK NO-ERROR.
            IF AVAILABLE CustomerAccounts THEN 
            DO:
                MESSAGE "Cannot delete customer. Customer has active accounts!" SKIP
                    "Please close all accounts before deleting customer."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                RETURN FALSE.
            END.
            
            /* Find customer record for deletion with EXCLUSIVE-LOCK */
            FIND FIRST CustomerDetails WHERE CustomerDetails.CustID = piCustomerId EXCLUSIVE-LOCK NO-ERROR.
            
            IF NOT AVAILABLE CustomerDetails THEN 
            DO:
                MESSAGE "Customer record not found or locked by another user!" 
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                RETURN FALSE.
            END.
            
            /* Delete the customer record */
            DELETE CustomerDetails.
            
            lSuccess = TRUE.
            
        END. /* DO TRANSACTION */
        
        /* Show success message */
        IF lSuccess THEN 
        DO:
            MESSAGE "Customer" cCustomerName "deleted successfully!" 
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        
        RETURN lSuccess.
        
        CATCH eError AS Progress.Lang.Error:
            MESSAGE "Error deleting customer: " + eError:GetMessage(1) 
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN FALSE.
        END CATCH.
        
    END METHOD.
    
    /* Overloaded method that takes customer name for lookup */
    METHOD PUBLIC LOGICAL DeleteCustomerByName(INPUT pcFirstName AS CHARACTER,
        INPUT pcLastName AS CHARACTER):
        
        DEFINE VARIABLE iCustomerId AS INTEGER NO-UNDO.
        
        /* Find customer by name */
        FIND FIRST CustomerDetails WHERE 
            CustomerDetails.FirstName = pcFirstName AND 
            CustomerDetails.LastName = pcLastName NO-LOCK NO-ERROR.
            
        IF NOT AVAILABLE CustomerDetails THEN 
        DO:
            MESSAGE "Customer" pcFirstName pcLastName "not found!" 
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN FALSE.
        END.
        
        iCustomerId = CustomerDetails.CustID.
        
        /* Call the main delete method */
        RETURN THIS-OBJECT:DeleteCustomer(iCustomerId).
        
    END METHOD.
    
    /* Method to check if customer can be deleted (has no dependencies) */
    METHOD PUBLIC LOGICAL CanDeleteCustomer(INPUT piCustomerId AS INTEGER):
        
        /* Check if customer exists */
        FIND FIRST CustomerDetails WHERE CustomerDetails.CustID = piCustomerId NO-LOCK NO-ERROR.
        
        IF NOT AVAILABLE CustomerDetails THEN
            RETURN FALSE.
        
        /* Check for related accounts */
        FIND FIRST CustomerAccounts WHERE CustomerAccounts.CustID = piCustomerId NO-LOCK NO-ERROR.
        
        IF AVAILABLE CustomerAccounts THEN
            RETURN FALSE.
            
        /* Add other dependency checks here as needed */
        /* Example: Check for transactions, loans, etc. */
        
        RETURN TRUE.
        
    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR getCustomerWithAccounts(INPUT iCustID AS INTEGER):

        DEFINE VARIABLE oResponse AS JsonObject NO-UNDO.
        DEFINE VARIABLE oAccounts AS JsonArray  NO-UNDO.
        DEFINE VARIABLE oAccount  AS JsonObject NO-UNDO.
        DEFINE VARIABLE lcData    AS LONGCHAR   NO-UNDO.
        DEFINE VARIABLE lcResult  AS LONGCHAR   NO-UNDO.

        DO ON ERROR UNDO, LEAVE:

            /* Initialize JSON objects */
            oResponse = NEW JsonObject().
            oAccounts = NEW JsonArray().

            /* Find customer */
            FIND FIRST CustomerDetails 
                WHERE CustomerDetails.CustID = iCustID NO-LOCK NO-ERROR.

            IF AVAILABLE CustomerDetails THEN 
            DO:

                /* Add customer details */
                oResponse:Add("CustID",        CustomerDetails.CustID).
                oResponse:Add("FirstName",     CustomerDetails.FirstName).
                oResponse:Add("LastName",      CustomerDetails.LastName).
                oResponse:Add("Address2",      CustomerDetails.Address2).
                oResponse:Add("Address",       CustomerDetails.Address1).
                oResponse:Add("City",          CustomerDetails.City).
                oResponse:Add("State",         CustomerDetails.State).
                oResponse:Add("Country",       CustomerDetails.Country).
                oResponse:Add("PostalCode",    CustomerDetails.ZipCode).
                oResponse:Add("MaritalStatus", CustomerDetails.MaritalStatus).

                /* Collect related accounts */
                /* Always initialize accounts array */
                oAccounts = NEW JsonArray().

                /* Collect related accounts */
                FOR EACH CustomerAccounts 
                    WHERE CustomerAccounts.CustID = CustomerDetails.CustID NO-LOCK:

                    oAccount = NEW JsonObject().

                    /* Lookup account type and subtype */
                    FIND FIRST AccountType 
                        WHERE AccountType.AccountTypeID = CustomerAccounts.AccountTypeId NO-LOCK NO-ERROR.

                    IF AVAILABLE AccountType THEN 
                    DO:
                        oAccount:Add("AcctNum", CustomerAccounts.AcctNum).
                        oAccount:Add("AccountTypeId", CustomerAccounts.AccountTypeId).
                        oAccount:Add("AccountType", AccountType.AccountType).
                        oAccount:Add("AccountSubType", AccountType.AccSubType).
                    END.
                    ELSE 
                    DO:
                        /* fallback if account type not found */
                        oAccount:Add("AcctNum", CustomerAccounts.AcctNum).
                        oAccount:Add("AccountTypeId", CustomerAccounts.AccountTypeId).
                    END.

                    oAccounts:Add(oAccount).
                END.

                /* Attach accounts array � even if empty */
                oResponse:Add("Accounts", oAccounts).

                /* Serialize the core data (customer + accounts) */
                oResponse:Write(lcData).

                /* Wrap in standardized response */
                RETURN Backend.Utility.Mini_Utility:BuildResponse(TRUE, 200, lcData).

            END.
            ELSE 
            DO:
                /* Customer not found → return 404 */
                RETURN Backend.Utility.Mini_Utility:BuildResponse(FALSE, 404, "Customer not found").
            END.

        END.  /* END BLOCK-LEVEL */

        CATCH e AS Progress.Lang.Error :
            RETURN Backend.Utility.Mini_Utility:BuildResponse(FALSE, 500, "GetCustomer Error: " + e:GetMessage(1)).
        
        END CATCH.
        FINALLY:
            IF VALID-OBJECT(oResponse) THEN DELETE OBJECT oResponse.
            IF VALID-OBJECT(oAccounts) THEN DELETE OBJECT oAccounts.
            IF VALID-OBJECT(oAccount)  THEN DELETE OBJECT oAccount.
        END FINALLY.

    END METHOD.

    
    
    METHOD PUBLIC LOGICAL GetCitiesByPostalCode(INPUT piPostalCode AS CHARACTER,
        OUTPUT TABLE ttCities):
        /*------------------------------------------------------------------------
            Purpose     : Get cities for a postal code - simplified version
            Parameters  : INPUT postal code, OUTPUT temp-table with city results
            Returns     : TRUE if cities found, FALSE otherwise
            Notes       : Matches City table structure from image
          ----------------------------------------------------------------------*/
    
        DEFINE VARIABLE cFoundCityCode AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lSuccess       AS LOGICAL   NO-UNDO INITIAL FALSE.
    
        /* Clear the output temp-table */
        EMPTY TEMP-TABLE ttCities.
    
        /* Validate input parameter */
        IF piPostalCode = "" OR piPostalCode = ? THEN 
        DO:
            RETURN FALSE.
        END.
    
        DO ON ERROR UNDO, THROW:
        
            /* Find the postal code in PostalCode table */
            FIND FIRST PostalCode WHERE PostalCode.ZIPCode = piPostalCode NO-LOCK NO-ERROR.
        
            IF NOT AVAILABLE PostalCode THEN 
            DO:
                RETURN FALSE.
            END.
        
            /* Get the city code from PostalCode table */
            cFoundCityCode = PostalCode.CityCode.
        
            /* Build city records in temp-table - matching your City table structure */
            FOR EACH City WHERE City.CityCode = cFoundCityCode NO-LOCK:
            
                CREATE ttCities.
                ASSIGN 
                    ttCities.CityCode  = City.CityCode
                    ttCities.CityName  = City.CityName
                    ttCities.StateCode = City.StateCode.
            
                lSuccess = TRUE.
            END.
            RETURN lSuccess.
        END.
    
        CATCH eError AS Progress.Lang.Error:
            /* Clear temp-table on error */
            EMPTY TEMP-TABLE ttCities.
            RETURN FALSE.
        END CATCH.
        
    END METHOD.
    
    
    METHOD PUBLIC LOGICAL GetStatesByCityName(INPUT pcCityName AS CHARACTER,
        OUTPUT TABLE ttStateDetails):
        /*------------------------------------------------------------------------
            Purpose     : Get states for a given city name using temp-table
            Parameters  : INPUT city name, OUTPUT temp-table with state results
            Returns     : TRUE if states found, FALSE otherwise
            Notes       : Returns state information based on city name lookup
          ----------------------------------------------------------------------*/
    
        DEFINE VARIABLE cFoundStateCode AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lSuccess        AS LOGICAL   NO-UNDO INITIAL FALSE.
    
        /* Clear the output temp-table */
        EMPTY TEMP-TABLE ttStateDetails.
    
        /* Validate input parameter */
        IF pcCityName = "" OR pcCityName = ? THEN 
        DO:
            RETURN FALSE.
        END.
    
        DO ON ERROR UNDO, THROW:
        
            /* Find the city to get the state code */
            FIND FIRST City WHERE City.CityName = pcCityName NO-LOCK NO-ERROR.
        
            IF NOT AVAILABLE City THEN 
            DO:
                RETURN FALSE.
            END.
        
            /* Get the state code from City table */
            cFoundStateCode = City.StateCode.
        
            /* Build state records in temp-table using the state code */
            FOR EACH State WHERE State.StateCode = cFoundStateCode NO-LOCK:
            
                CREATE ttStateDetails.
                ASSIGN 
                    ttStateDetails.StateCode   = State.StateCode
                    ttStateDetails.StateName   = State.StateName
                    ttStateDetails.CountryCode = State.CountryCode.
            
                lSuccess = TRUE.
            END.
            RETURN lSuccess.
        END.
        CATCH eError AS Progress.Lang.Error:
            /* Clear temp-table on error */
            EMPTY TEMP-TABLE ttStateDetails.
            RETURN FALSE.
        END CATCH.
    
    
    END METHOD.


    METHOD PUBLIC LOGICAL GetCountriesByStateName(INPUT pcStateName AS CHARACTER,
        OUTPUT TABLE ttCountryDetails):
        /*------------------------------------------------------------------------
            Purpose     : Get countries for a given state name using temp-table
            Parameters  : INPUT state name, OUTPUT temp-table with country results
            Returns     : TRUE if countries found, FALSE otherwise
            Notes       : Returns country information based on state name lookup
          ----------------------------------------------------------------------*/
    
        DEFINE VARIABLE cFoundCountryCode AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lSuccess          AS LOGICAL   NO-UNDO INITIAL FALSE.
    
        /* Clear the output temp-table */
        EMPTY TEMP-TABLE ttCountryDetails.
    
        /* Validate input parameter */
        IF pcStateName = "" OR pcStateName = ? THEN 
        DO:
            RETURN FALSE.
        END.
    
        DO ON ERROR UNDO, THROW:
        
            /* Find the state to get the country code */
            FIND FIRST State WHERE State.StateName = pcStateName NO-LOCK NO-ERROR.
        
            IF NOT AVAILABLE State THEN 
            DO:
                RETURN FALSE.
            END.
        
            /* Get the country code from State table */
            cFoundCountryCode = State.CountryCode.
        
            /* Build country records in temp-table using the country code */
            FOR EACH Country WHERE Country.CountryCode = cFoundCountryCode NO-LOCK:
            
                CREATE ttCountryDetails.
                ASSIGN 
                    ttCountryDetails.CountryCode = Country.CountryCode
                    ttCountryDetails.CountryName = Country.CountryName.
            
                lSuccess = TRUE.
            END.
            RETURN lSuccess.
        END.
        CATCH eError AS Progress.Lang.Error:
            /* Clear temp-table on error */
            EMPTY TEMP-TABLE ttCountryDetails.
            RETURN FALSE.
        END CATCH.
    
    
    END METHOD.
    
    
    METHOD PUBLIC LOGICAL GetAllCountries(OUTPUT TABLE ttCountryDetails):
        /*------------------------------------------------------------------------
            Purpose     : Get all countries sorted alphabetically
            Parameters  : OUTPUT temp-table with all country results
            Returns     : TRUE if countries found, FALSE otherwise
            Notes       : Returns all countries from database sorted by name
          ----------------------------------------------------------------------*/
    
        DEFINE VARIABLE iCountryCount AS INTEGER NO-UNDO INITIAL 0.
        DEFINE VARIABLE lSuccess      AS LOGICAL NO-UNDO INITIAL FALSE.
    
        /* Clear the output temp-table */
        EMPTY TEMP-TABLE ttCountryDetails.
    
        DO ON ERROR UNDO, THROW:
        
            /* Build complete country list from Country table, sorted by name */
            FOR EACH Country NO-LOCK BY Country.CountryName:
            
                CREATE ttCountryDetails.
                ASSIGN 
                    ttCountryDetails.CountryCode = Country.CountryCode
                    ttCountryDetails.CountryName = Country.CountryName.
            
                iCountryCount = iCountryCount + 1.
            END.
        
            /* Set success flag if countries were found */
            IF iCountryCount > 0 THEN
                lSuccess = TRUE.
            RETURN lSuccess.
        END.
        CATCH eError AS Progress.Lang.Error:
            /* Clear temp-table on error */
            EMPTY TEMP-TABLE ttCountryDetails.
            RETURN FALSE.
        END CATCH.
    
    
    END METHOD.


    METHOD PUBLIC LOGICAL GetAllStates(OUTPUT TABLE ttStateDetails):
        /*------------------------------------------------------------------------
            Purpose     : Get all states sorted alphabetically
            Parameters  : OUTPUT temp-table with all state results
            Returns     : TRUE if states found, FALSE otherwise
            Notes       : Returns all states from database sorted by name
          ----------------------------------------------------------------------*/
    
        DEFINE VARIABLE iStateCount AS INTEGER NO-UNDO INITIAL 0.
        DEFINE VARIABLE lSuccess    AS LOGICAL NO-UNDO INITIAL FALSE.
    
        /* Clear the output temp-table */
        EMPTY TEMP-TABLE ttStateDetails.
    
        DO ON ERROR UNDO, THROW:
        
            /* Build complete state list from State table, sorted by name */
            FOR EACH State NO-LOCK BY State.StateName:
            
                CREATE ttStateDetails.
                ASSIGN 
                    ttStateDetails.StateCode   = State.StateCode
                    ttStateDetails.StateName   = State.StateName
                    ttStateDetails.CountryCode = State.CountryCode.
            
                iStateCount = iStateCount + 1.
            END.
        
            /* Set success flag if states were found */
            IF iStateCount > 0 THEN
                lSuccess = TRUE.
            RETURN lSuccess.
        END.
        CATCH eError AS Progress.Lang.Error:
            /* Clear temp-table on error */
            EMPTY TEMP-TABLE ttStateDetails.
            RETURN FALSE.
        END CATCH.
    
    
    END METHOD.


    METHOD PUBLIC LOGICAL GetStatesByCountryName(INPUT pcCountryName AS CHARACTER,
        OUTPUT TABLE ttStateDetails):
        /*------------------------------------------------------------------------
            Purpose     : Get states for a given country name using temp-table
            Parameters  : INPUT country name, OUTPUT temp-table with state results
            Returns     : TRUE if states found, FALSE otherwise
            Notes       : Returns states filtered by country name, sorted by state name
          ----------------------------------------------------------------------*/
    
        DEFINE VARIABLE cFoundCountryCode AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iStateCount       AS INTEGER   NO-UNDO INITIAL 0.
        DEFINE VARIABLE lSuccess          AS LOGICAL   NO-UNDO INITIAL FALSE.
    
        /* Clear the output temp-table */
        EMPTY TEMP-TABLE ttStateDetails.
    
        /* Validate input parameter */
        IF pcCountryName = "" OR pcCountryName = ? THEN 
        DO:
            RETURN FALSE.
        END.
    
        DO ON ERROR UNDO, THROW:
        
            /* Find the country to get the country code */
            FIND FIRST Country WHERE Country.CountryName = pcCountryName NO-LOCK NO-ERROR.
        
            IF NOT AVAILABLE Country THEN 
            DO:
                RETURN FALSE.
            END.
        
            /* Get the country code from Country table */
            cFoundCountryCode = Country.CountryCode.
        
            /* Build state records in temp-table filtered by country code */
            FOR EACH State WHERE State.CountryCode = cFoundCountryCode NO-LOCK BY State.StateName:
            
                CREATE ttStateDetails.
                ASSIGN 
                    ttStateDetails.StateCode   = State.StateCode
                    ttStateDetails.StateName   = State.StateName
                    ttStateDetails.CountryCode = State.CountryCode.
            
                iStateCount = iStateCount + 1.
                lSuccess = TRUE.
                RETURN lSuccess.
            END.
        END.
    
        CATCH eError AS Progress.Lang.Error:
            /* Clear temp-table on error */
            EMPTY TEMP-TABLE ttStateDetails.
            RETURN FALSE.
        END CATCH.
    
    END METHOD.



    METHOD PUBLIC LOGICAL GetAllCities(OUTPUT TABLE ttCities):
        /*------------------------------------------------------------------------
            Purpose     : Get all cities sorted alphabetically
            Parameters  : OUTPUT temp-table with all city results
            Returns     : TRUE if cities found, FALSE otherwise
            Notes       : Returns all cities from database sorted by name
          ----------------------------------------------------------------------*/
    
        DEFINE VARIABLE iCityCount AS INTEGER NO-UNDO INITIAL 0.
        DEFINE VARIABLE lSuccess   AS LOGICAL NO-UNDO INITIAL FALSE.
    
        /* Clear the output temp-table */
        EMPTY TEMP-TABLE ttCities.
    
        DO ON ERROR UNDO, THROW:
        
            /* Build complete city list from City table, sorted by name */
            FOR EACH City NO-LOCK BY City.CityName:
            
                CREATE ttCities.
                ASSIGN 
                    ttCities.CityCode  = City.CityCode
                    ttCities.CityName  = City.CityName
                    ttCities.StateCode = City.StateCode.
            
                iCityCount = iCityCount + 1.
            END.
        
            /* Set success flag if cities were found */
            IF iCityCount > 0 THEN
                lSuccess = TRUE.
            RETURN lSuccess.
        END.    
        CATCH eError AS Progress.Lang.Error:
            /* Clear temp-table on error */
            EMPTY TEMP-TABLE ttCities.
            RETURN FALSE.
        END CATCH.
    
    
    END METHOD.
    
    
    
    METHOD PUBLIC LOGICAL GetCustomerForForm(INPUT piCustomerId AS INTEGER,
        OUTPUT TABLE ttCustomerDetails):
        /*------------------------------------------------------------------------
            Purpose     : Get customer details for form population
            Parameters  : INPUT customer ID, OUTPUT temp-table with customer data
            Returns     : TRUE if customer found, FALSE otherwise
            Notes       : Returns customer data formatted for UI display
          ----------------------------------------------------------------------*/
    
        DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO INITIAL FALSE.
    
        /* Clear the output temp-table */
        EMPTY TEMP-TABLE ttCustomerDetails.
    
        /* Validate input parameter */
        IF piCustomerId <= 0 THEN 
        DO:
            RETURN FALSE.
        END.
    
        DO ON ERROR UNDO, THROW:
        
            /* Find the customer record */
            FIND FIRST CustomerDetails WHERE CustomerDetails.CustID = piCustomerId NO-LOCK NO-ERROR.
        
            IF AVAILABLE CustomerDetails THEN 
            DO:
                /* Create temp-table record with customer data */
                CREATE ttCustomerDetails.
                ASSIGN 
                    ttCustomerDetails.CustID         = CustomerDetails.CustID
                    ttCustomerDetails.FirstName      = CustomerDetails.FirstName
                    ttCustomerDetails.LastName       = CustomerDetails.LastName
                    ttCustomerDetails.date_of_birth  = CustomerDetails.DOB
                    ttCustomerDetails.marital_status = CustomerDetails.MaritalStatus
                    ttCustomerDetails.address        = CustomerDetails.Address1
                    ttCustomerDetails.address_2      = CustomerDetails.Address2
                    ttCustomerDetails.City           = CustomerDetails.City
                    ttCustomerDetails.State          = CustomerDetails.State
                    ttCustomerDetails.postal_code    = CustomerDetails.ZipCode
                    ttCustomerDetails.Country        = CustomerDetails.Country
                    ttCustomerDetails.email          = CustomerDetails.EmailId
                    ttCustomerDetails.mobile_num     = CustomerDetails.Phone.
            
                lSuccess = TRUE.
            END.
            RETURN lSuccess.
        END.    
        CATCH eError AS Progress.Lang.Error:
            /* Clear temp-table on error */
            EMPTY TEMP-TABLE ttCustomerDetails.
            RETURN FALSE.
        END CATCH.
    
    END METHOD.

    METHOD PUBLIC LOGICAL GetCitiesByStateName(INPUT pcStateName AS CHARACTER,
        OUTPUT TABLE ttCities):
        /*------------------------------------------------------------------------
            Purpose     : Get cities for a given state name using temp-table
            Parameters  : INPUT state name, OUTPUT temp-table with city results
            Returns     : TRUE if cities found, FALSE otherwise
            Notes       : Returns cities filtered by state name, sorted by city name
          ----------------------------------------------------------------------*/
    
        DEFINE VARIABLE cFoundStateCode AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iCityCount      AS INTEGER   NO-UNDO INITIAL 0.
        DEFINE VARIABLE lSuccess        AS LOGICAL   NO-UNDO INITIAL FALSE.
    
        /* Clear the output temp-table */
        EMPTY TEMP-TABLE ttCities.
    
        /* Validate input parameter */
        IF pcStateName = "" OR pcStateName = ? THEN 
        DO:
            RETURN FALSE.
        END.
    
        DO ON ERROR UNDO, THROW:
        
            /* Find the state to get state code */
            FIND FIRST State WHERE State.StateName = pcStateName NO-LOCK NO-ERROR.
        
            IF NOT AVAILABLE State THEN 
            DO:
                RETURN FALSE.
            END.
        
            /* Get state code */
            cFoundStateCode = State.StateCode.
        
            /* Build city records filtered by state code */
            FOR EACH City WHERE City.StateCode = cFoundStateCode NO-LOCK BY City.CityName:
            
                CREATE ttCities.
                ASSIGN 
                    ttCities.CityCode  = City.CityCode
                    ttCities.CityName  = City.CityName
                    ttCities.StateCode = City.StateCode.
            
                iCityCount = iCityCount + 1.
                lSuccess = TRUE.
            END.
            RETURN lSuccess.
        END.
        CATCH eError AS Progress.Lang.Error:
            /* Clear temp-table on error */
            EMPTY TEMP-TABLE ttCities.
            RETURN FALSE.
        END CATCH.
    
    
    END METHOD.


    DESTRUCTOR PUBLIC Customer():
        EMPTY TEMP-TABLE ttCustomerDetails.
    END DESTRUCTOR.


END CLASS.