CLASS Backend.Customer_Filler_Search: 

    CONSTRUCTOR PUBLIC Customer_Filler_Search():
        SUPER().
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------ 
       Method: searchCustomers
       Purpose: Accepts search parameters and returns JSON result string
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR searchCustomers(
        INPUT cFirstName AS CHARACTER,
        INPUT cLastName  AS CHARACTER,
        INPUT cPhone     AS CHARACTER,
        INPUT cEmail     AS CHARACTER):

        DEFINE VARIABLE oController AS Backend.Controller NO-UNDO.
        DEFINE VARIABLE lcResult    AS LONGCHAR NO-UNDO.

        /* Create Controller object and call existing search logic */
        oController = NEW Backend.Controller().
        lcResult = oController:searchCustomers(
                        INPUT cFirstName,
                        INPUT cLastName,
                        INPUT cPhone,
                        INPUT cEmail).

        RETURN lcResult.

    END METHOD.

    DESTRUCTOR PUBLIC Customer_Filler_Search():
    END DESTRUCTOR.

END CLASS.
