DEFINE TEMP-TABLE ttCities NO-UNDO
    FIELD CityCode  AS CHARACTER
    FIELD CityName  AS CHARACTER  
    FIELD StateCode AS CHARACTER
    INDEX idx-city CityCode.


/* Country temp-table */
DEFINE TEMP-TABLE ttCountryDetails NO-UNDO
    FIELD CountryCode AS CHARACTER
    FIELD CountryName AS CHARACTER
    INDEX idx-country CountryCode.


    
DEFINE TEMP-TABLE ttStateDetails NO-UNDO
    FIELD StateCode   AS CHARACTER
    FIELD StateName   AS CHARACTER  
    FIELD CountryCode AS CHARACTER
    INDEX idx-state   StateCode
    INDEX idx-country CountryCode.
    

DEFINE TEMP-TABLE ttCustomerDetails NO-UNDO
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