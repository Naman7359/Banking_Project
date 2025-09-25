 
 /*------------------------------------------------------------------------
    File        : Utility
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Suraj.chauhan
    Created     : Tue Sep 23 18:31:56 IST 2025
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS frontend.Utility.Utility: 
    
    METHOD PUBLIC STATIC LONGCHAR ReadResponse(
        INPUT plcResponse AS LONGCHAR
        ):
        DEFINE VARIABLE oParser   AS ObjectModelParser NO-UNDO.
        DEFINE VARIABLE oResponse AS JsonObject        NO-UNDO.
        DEFINE VARIABLE lcResult  AS LONGCHAR          NO-UNDO.
        DEFINE VARIABLE lSuccess  AS LOGICAL           NO-UNDO.
        DEFINE VARIABLE iStatus   AS INTEGER           NO-UNDO.
        /* Parse the response */
        oParser   = NEW ObjectModelParser().
 
        oResponse = CAST(oParser:Parse(plcResponse), JsonObject) NO-ERROR.
        IF ERROR-STATUS:NUM-MESSAGES > 0 OR NOT VALID-OBJECT(oResponse) THEN 
        DO:
            UNDO, THROW NEW Progress.Lang.AppError("Error while Casting from LongChar to JSON",0).
        END.
        /* Read common fields */
        lSuccess = oResponse:GetLogical("success").
        iStatus  = oResponse:GetInteger("status").
 
        IF lSuccess THEN 
        DO:
            /* Return the data as LONGCHAR */
            lcResult = oResponse:GetLongchar("data" , "UTF-8").
            RETURN lcResult.
        END.
        ELSE 
        DO:
            DEFINE VARIABLE cError AS CHARACTER NO-UNDO.
            cError = oResponse:GetCharacter("error").
            UNDO, THROW NEW Progress.Lang.AppError("Status " + STRING(iStatus) + ": " + cError, 0).
        END.
        FINALLY:
            IF VALID-OBJECT(oResponse) THEN DELETE OBJECT oResponse.
            IF VALID-OBJECT(oParser) THEN DELETE OBJECT oParser.
        END FINALLY.
    END METHOD.
 

END CLASS.