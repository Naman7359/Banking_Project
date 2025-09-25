
 
 /*------------------------------------------------------------------------
    File        : Mini_Utility
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Naman.kaushik
    Created     : Thu Sep 25 16:14:56 IST 2025
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Backend.Utility.Mini_Utility: 

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    CONSTRUCTOR PUBLIC Mini_Utility (  ):
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC STATIC LONGCHAR BuildResponse(
        INPUT plSuccess AS LOGICAL, 
        INPUT piStatus  AS INTEGER, 
        INPUT plcData   AS LONGCHAR
        ):
        DEFINE VARIABLE oResponse AS JsonObject NO-UNDO.
        DEFINE VARIABLE lcResult  AS LONGCHAR   NO-UNDO.
 
        /* Create the response object */
        oResponse = NEW JsonObject().
        oResponse:Add("success", plSuccess).
        oResponse:Add("status", piStatus).
 
        /* If success, add "data" else add "error" */
        IF plSuccess THEN
            oResponse:Add("data", plcData).
        ELSE
            oResponse:Add("error", plcData).
 
        oResponse:Write(lcResult, TRUE).
 
        RETURN lcResult.
 
        FINALLY:
            IF VALID-OBJECT(oResponse) THEN DELETE OBJECT oResponse.
        END FINALLY.
    END METHOD.

    DESTRUCTOR PUBLIC Mini_Utility ( ):

    END DESTRUCTOR.

END CLASS.