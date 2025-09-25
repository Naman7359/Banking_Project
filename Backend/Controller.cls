/*------------------------------------------------------------------------
    File        : Controller.cls
    Purpose     : Backend controller for Customer operations
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.

CLASS Backend.Controller:

    METHOD PUBLIC LONGCHAR searchCustomers(
        INPUT cFirstName AS CHARACTER,
        INPUT cLastName  AS CHARACTER,
        INPUT CustID AS CHARACTER
        ):
        DEFINE VARIABLE oResponse AS JsonArray  NO-UNDO.
        DEFINE VARIABLE oCust     AS JsonObject NO-UNDO.
        DEFINE VARIABLE lcResult  AS LONGCHAR   NO-UNDO.

        oResponse = NEW JsonArray().

        FOR EACH CustomerDetails NO-LOCK
            WHERE (cFirstName = "" OR CustomerDetails.FirstName BEGINS cFirstName)
            AND (cLastName  = "" OR CustomerDetails.LastName  BEGINS cLastName)
            AND (CustID = "" OR CustomerDetails.CustID BEGINS CustID).

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

END CLASS.
