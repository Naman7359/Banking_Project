/*------------------------------------------------------------------------
    File        : UtilityManager.cls
    Purpose     : Utility methods for dropdown lists and validation
    Syntax      : 
    Description : Manages combo box data and common utility functions
    Author(s)   : 
    Created     : 
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS UtilityManager:

    /* Constructor */
    CONSTRUCTOR PUBLIC UtilityManager():
        SUPER().
    END CONSTRUCTOR.

    /* Destructor */
    DESTRUCTOR PUBLIC UtilityManager():
    END DESTRUCTOR.

    /* Method to get marital status list for combo box */
    METHOD PUBLIC CHARACTER GetMaritalStatusList():
        RETURN "Single,Married,Divorced,Widowed,Separated".
    END METHOD.

    /* Method to get city list from database */
    METHOD PUBLIC CHARACTER GetCityList():
        DEFINE VARIABLE cCityList AS CHARACTER NO-UNDO INITIAL "".
        
        FOR EACH City NO-LOCK:
            cCityList = cCityList + (IF cCityList = "" THEN "" ELSE ",") + City.CityName.
        END.
        
        /* If no cities in database, provide default list */
        IF cCityList = "" THEN
            cCityList = "Delhi,Mumbai,Bangalore,Chennai,Kolkata,Hyderabad,Pune,Noida,Gurgaon".
            
        RETURN cCityList.
    END METHOD.

    /* Method to get state list from database */
    METHOD PUBLIC CHARACTER GetStateList():
        DEFINE VARIABLE cStateList AS CHARACTER NO-UNDO INITIAL "".
        
        FOR EACH State NO-LOCK:
            cStateList = cStateList + (IF cStateList = "" THEN "" ELSE ",") + State.StateName.
        END.
        
        /* If no states in database, provide default list */
        IF cStateList = "" THEN
            cStateList = "Delhi,Maharashtra,Karnataka,Tamil Nadu,West Bengal,Telangana,Gujarat,Rajasthan,Uttar Pradesh".
            
        RETURN cStateList.
    END METHOD.

    /* Method to get country list from database */
    METHOD PUBLIC CHARACTER GetCountryList():
        DEFINE VARIABLE cCountryList AS CHARACTER NO-UNDO INITIAL "".
        
        FOR EACH Country NO-LOCK:
            cCountryList = cCountryList + (IF cCountryList = "" THEN "" ELSE ",") + Country.CountryName.
        END.
        
        /* If no countries in database, provide default list */
        IF cCountryList = "" THEN
            cCountryList = "India,USA,UK,Canada,Australia,Germany,France,Japan".
            
        RETURN cCountryList.
    END METHOD.

    /* Method to get postal code list from database */
    METHOD PUBLIC CHARACTER GetPostalCodeList():
        DEFINE VARIABLE cPostalList AS CHARACTER NO-UNDO INITIAL "".
        
        FOR EACH PostalCode NO-LOCK:
            cPostalList = cPostalList + (IF cPostalList = "" THEN "" ELSE ",") + PostalCode.ZIPCode.
        END.
        
        /* If no postal codes in database, provide default list */
        IF cPostalList = "" THEN
            cPostalList = "110001,400001,560001,600001,700001,500001,201301,122001".
            
        RETURN cPostalList.
    END METHOD.

    /* Method to validate email format */
    METHOD PUBLIC LOGICAL ValidateEmail(INPUT pcEmail AS CHARACTER):
        DEFINE VARIABLE lValid AS LOGICAL NO-UNDO INITIAL FALSE.
        
        IF pcEmail <> "" THEN DO:
            /* Basic email validation - contains @ and . */
            IF INDEX(pcEmail, "@") > 0 AND INDEX(pcEmail, ".") > 0 THEN
                lValid = TRUE.
        END.
        
        RETURN lValid.
    END METHOD.

    /* Method to validate mobile number */
    METHOD PUBLIC LOGICAL ValidateMobile(INPUT pcMobile AS CHARACTER):
        DEFINE VARIABLE lValid AS LOGICAL NO-UNDO INITIAL FALSE.
        DEFINE VARIABLE iLength AS INTEGER NO-UNDO.
        
        IF pcMobile <> "" THEN DO:
            iLength = LENGTH(TRIM(pcMobile)).
            /* Check if mobile number is 10 digits */
            IF iLength = 10 AND CAN-DO("0,1,2,3,4,5,6,7,8,9", SUBSTRING(pcMobile,1,1)) THEN
                lValid = TRUE.
        END.
        
        RETURN lValid.
    END METHOD.

END CLASS.
