CLASS Backend.Customer_Filler_Search: 

    CONSTRUCTOR PUBLIC Customer_Filler_Search():
        SUPER().
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------ 
       Method: searchCustomers
       Purpose: Accepts search parameters and returns JSON result string
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR searchCustomers(
        INPUT iCustId AS INteger):

FIND FIRST CustomerDetails WHERE CustomerDetails.CustID = iCustId NO-LOCK NO-ERROR.
IF AVAILABLE CustomerDetails THEN DO:
    END. 


        RETURN lcResult.

    END METHOD.

    DESTRUCTOR PUBLIC Customer_Filler_Search():
    END DESTRUCTOR.

END CLASS.
