 
/*------------------------------------------------------------------------
   File        : Customer
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Naman.kaushik
   Created     : Thu Sep 25 16:22:47 IST 2025
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

USING Progress.Json.ObjectModel.*.


BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Backend.Customer: 

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    CONSTRUCTOR PUBLIC Customer (  ):
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR getCustomerWithAccounts(INPUT iCustID AS INTEGER):

        DEFINE VARIABLE oResponse    AS JsonObject  NO-UNDO.
        DEFINE VARIABLE oAccounts    AS JsonArray   NO-UNDO.
        DEFINE VARIABLE oAccount     AS JsonObject  NO-UNDO.
        DEFINE VARIABLE lcData       AS LONGCHAR    NO-UNDO.
        DEFINE VARIABLE lcResult     AS LONGCHAR    NO-UNDO.

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
                FOR EACH CustomerAccounts 
                    WHERE CustomerAccounts.CustID = CustomerDetails.CustID NO-LOCK:

                    /* Lookup account type and subtype */
                    FIND FIRST AccountType 
                        WHERE AccountType.AccountTypeID = CustomerAccounts.AccountTypeId NO-LOCK NO-ERROR.


                    IF AVAILABLE AccountType THEN 
                    DO:
                    oAccount = NEW JsonObject().
                    oAccount:Add("AcctNum", CustomerAccounts.AcctNum).
                    oAccount:Add("AccountTypeId", CustomerAccounts.AccountTypeId).
                        oAccount:Add("AccountType", AccountType.AccountType).
                        oAccount:Add("AccountSubType", AccountType.AccSubType).  /* if SubType exists */
                    END.


                    oAccounts:Add(oAccount).
                END.

                /* Attach accounts array */
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


    DESTRUCTOR PUBLIC Customer ( ):

    END DESTRUCTOR.

END CLASS.