
/*------------------------------------------------------------------------
    File        : Controller.cls
    Purpose     : Backend controller for Customer operations
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.

CLASS Backend.Customer:
    
    
    DEFINE PRIVATE TEMP-TABLE ttCustomerDetails NO-UNDO
        FIELD CustID         AS INTEGER
        FIELD FirstName      AS CHARACTER
        FIELD LastName       AS CHARACTER
        FIELD date_of_birth  AS DATE
        FIELD marital_status AS CHARACTER
        FIELD address        AS CHARACTER
        FIELD address_2      AS CHARACTER
        FIELD City           AS CHARACTER
        FIELD State          AS CHARACTER
        FIELD postal_code    AS INTEGER
        FIELD Country        AS CHARACTER
        FIELD email          AS CHARACTER
        FIELD mobile_num     AS CHARACTER.
        
    
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
            /*                ttCustomerDetails.date_created = CustomerDetails*/
            /*                ttCustomerDetails.date_modified = CustomerDetails.date_modified.*/
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
                /*                    CustomerDetails.date_created = TODAY*/
                /*                    CustomerDetails.date_modified = TODAY.*/
                
                /* Update temp-table with generated CustomerDetails ID */
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
            /* Check if CustomerDetails has accounts */
            FIND FIRST CustomerAccounts WHERE CustomerAccounts.CustID = piCustId NO-LOCK NO-ERROR.
            IF AVAILABLE account THEN 
            DO:
                /* CustomerDetails has accounts, cannot delete */
                RETURN FALSE.
            END.
            
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




    DESTRUCTOR PUBLIC Customer():
        EMPTY TEMP-TABLE ttCustomerDetails.
    END DESTRUCTOR.


END CLASS.