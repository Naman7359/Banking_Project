&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/
USING Backend.Utiity.*.
/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---         
                                  */
DEFINE INPUT PARAMETER piCustomerId AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER cAction AS CHARACTER NO-UNDO.           

/* Local Variable Definitions ---                                       */
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-9 RECT-11 FLN-FirstName FLN-LastName ~
FLN-DateOfBirth CMB-MaritalStatus FLN-Address FLN-Address_2 CMB-State ~
FLN-PostalCode CMB-Country CMB-City BTN-Save BTN-Cancel 
&Scoped-Define DISPLAYED-OBJECTS FLN-FirstName FLN-LastName FLN-DateOfBirth ~
CMB-MaritalStatus FLN-Address FLN-Address_2 CMB-State FLN-PostalCode ~
CMB-Country CMB-City 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-Cancel AUTO-END-KEY 
    LABEL "Cancel" 
    SIZE 10 BY 1.14
    BGCOLOR 8 .

DEFINE BUTTON BTN-Save AUTO-GO 
    LABEL "Save" 
    SIZE 10 BY 1.14
    BGCOLOR 8 .

DEFINE VARIABLE CMB-City          AS CHARACTER FORMAT "X(256)":U 
    LABEL "City" 
    VIEW-AS COMBO-BOX INNER-LINES 5
    LIST-ITEMS "Item 1" 
    DROP-DOWN-LIST
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE CMB-Country       AS CHARACTER FORMAT "X(256)":U 
    LABEL "Country" 
    VIEW-AS COMBO-BOX INNER-LINES 5
    LIST-ITEMS "Item 1" 
    DROP-DOWN-LIST
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE CMB-MaritalStatus AS CHARACTER FORMAT "X(256)":U 
    LABEL "Marital Status" 
    VIEW-AS COMBO-BOX INNER-LINES 5
    LIST-ITEMS "Item 1" 
    DROP-DOWN-LIST
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE CMB-State         AS CHARACTER FORMAT "X(256)":U 
    LABEL "State" 
    VIEW-AS COMBO-BOX INNER-LINES 5
    LIST-ITEMS "Item 1" 
    DROP-DOWN-LIST
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-Address       AS CHARACTER FORMAT "X(256)":U 
    LABEL "Address" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-Address_2     AS CHARACTER FORMAT "X(256)":U 
    LABEL "Address 2" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-DateOfBirth   AS DATE      FORMAT "99/99/99":U 
    LABEL "Date of Birth" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-FirstName     AS CHARACTER FORMAT "X(256)":U 
    LABEL "First Name" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-LastName      AS CHARACTER FORMAT "X(256)":U 
    LABEL "Last Name" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-PostalCode    AS CHARACTER FORMAT "X(256)":U 
    LABEL "Postal Code" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-11
    EDGE-PIXELS 10  NO-FILL   
    SIZE 71 BY 7.52.

DEFINE RECTANGLE RECT-9
    EDGE-PIXELS 10  NO-FILL   
    SIZE 71 BY 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    FLN-FirstName AT ROW 3.76 COL 19 COLON-ALIGNED WIDGET-ID 2
    FLN-LastName AT ROW 3.76 COL 53 COLON-ALIGNED WIDGET-ID 4
    FLN-DateOfBirth AT ROW 5.52 COL 19 COLON-ALIGNED WIDGET-ID 8
    CMB-MaritalStatus AT ROW 5.52 COL 53 COLON-ALIGNED WIDGET-ID 12
    FLN-Address AT ROW 8 COL 19 COLON-ALIGNED WIDGET-ID 6
    FLN-Address_2 AT ROW 8 COL 53 COLON-ALIGNED WIDGET-ID 16
    CMB-State AT ROW 9.52 COL 53 COLON-ALIGNED WIDGET-ID 20
    FLN-PostalCode AT ROW 9.57 COL 19 COLON-ALIGNED WIDGET-ID 30
    CMB-Country AT ROW 11 COL 53 COLON-ALIGNED WIDGET-ID 24
    CMB-City AT ROW 11.24 COL 19 COLON-ALIGNED WIDGET-ID 18
    BTN-Save AT ROW 13 COL 28
    BTN-Cancel AT ROW 13 COL 45
    "ADD/UPDATE CUSTOMER" VIEW-AS TEXT
    SIZE 31 BY 1.52 AT ROW 1.24 COL 27 WIDGET-ID 10
    FONT 1
    RECT-9 AT ROW 3.24 COL 6 WIDGET-ID 14
    RECT-11 AT ROW 7.52 COL 6 WIDGET-ID 28
    SPACE(4.87) SKIP(0.67)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    TITLE "<insert dialog title>"
    DEFAULT-BUTTON BTN-Save CANCEL-BUTTON BTN-Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
    FRAME Dialog-Frame:SCROLLABLE = FALSE
    FRAME Dialog-Frame:HIDDEN     = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* <insert dialog title> */
    DO:
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Save Dialog-Frame
ON CHOOSE OF BTN-Save IN FRAME Dialog-Frame /* Save */
    DO:
        
        IF piCustomerId > 0 THEN 
            RUN update-customer.
        ELSE 
            RUN create-customer.
        /* If customer ID exists, UPDATE. Otherwise, CREATE */
        /*        IF piCustomerId > 0 THEN                                                                            */
        /*        DO:                                                                                                 */
        /*            /* UPDATE - Find and overwrite */                                                               */
        /*            FIND FIRST CustomerDetails WHERE CustomerDetails.CustID = piCustomerId EXCLUSIVE-LOCK NO-ERROR. */
        /*            IF AVAILABLE CustomerDetails THEN                                                               */
        /*            DO:                                                                                             */
        /*                /* Brutally overwrite everything */                                                         */
        /*                ASSIGN                                                                                      */
        /*                    CustomerDetails.FirstName     = FLN-FirstName:SCREEN-VALUE IN FRAME Dialog-Frame        */
        /*                    CustomerDetails.LastName      = FLN-LastName:SCREEN-VALUE IN FRAME Dialog-Frame         */
        /*                    CustomerDetails.DOB           = DATE(FLN-DateOfBirth:SCREEN-VALUE IN FRAME Dialog-Frame)*/
        /*                    CustomerDetails.MaritalStatus = CMB-MaritalStatus:SCREEN-VALUE IN FRAME Dialog-Frame    */
        /*                    CustomerDetails.Address1      = FLN-Address:SCREEN-VALUE IN FRAME Dialog-Frame          */
        /*                    CustomerDetails.Address2      = FLN-Address_2:SCREEN-VALUE IN FRAME Dialog-Frame        */
        /*                    CustomerDetails.Country       = CMB-Country:SCREEN-VALUE IN FRAME Dialog-Frame          */
        /*                    CustomerDetails.State         = CMB-State:SCREEN-VALUE IN FRAME Dialog-Frame            */
        /*                    CustomerDetails.City          = CMB-City:SCREEN-VALUE IN FRAME Dialog-Frame             */
        /*                    CustomerDetails.ZipCode       = FLN-PostalCode:SCREEN-VALUE IN FRAME Dialog-Frame.      */
        /*                                                                                                            */
        /*                MESSAGE "Customer updated!" VIEW-AS ALERT-BOX.                                              */
        /*            END.                                                                                            */
        /*        END.                                                                                                */
        /*        ELSE                                                                                                */
        /*        DO:                                                                                                 */
        /*            /* CREATE - New customer */                                                                     */
        /*            CREATE CustomerDetails.                                                                         */
        /*            ASSIGN                                                                                          */
        /*                CustomerDetails.FirstName     = FLN-FirstName:SCREEN-VALUE IN FRAME Dialog-Frame            */
        /*                CustomerDetails.LastName      = FLN-LastName:SCREEN-VALUE IN FRAME Dialog-Frame             */
        /*                CustomerDetails.DOB           = DATE(FLN-DateOfBirth:SCREEN-VALUE IN FRAME Dialog-Frame)    */
        /*                CustomerDetails.MaritalStatus = CMB-MaritalStatus:SCREEN-VALUE IN FRAME Dialog-Frame        */
        /*                CustomerDetails.Address1      = FLN-Address:SCREEN-VALUE IN FRAME Dialog-Frame              */
        /*                CustomerDetails.Address2      = FLN-Address_2:SCREEN-VALUE IN FRAME Dialog-Frame            */
        /*                CustomerDetails.Country       = CMB-Country:SCREEN-VALUE IN FRAME Dialog-Frame              */
        /*                CustomerDetails.State         = CMB-State:SCREEN-VALUE IN FRAME Dialog-Frame                */
        /*                CustomerDetails.City          = CMB-City:SCREEN-VALUE IN FRAME Dialog-Frame                 */
        /*                CustomerDetails.ZipCode       = FLN-Postalcode:SCREEN-VALUE IN FRAME Dialog-Frame.          */
        /*                                                                                                            */
        /*            MESSAGE "Customer created!" VIEW-AS ALERT-BOX.                                                  */
        /*        END.                                                                                                */
    
        APPLY "WINDOW-CLOSE" TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB-City
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB-City Dialog-Frame
ON VALUE-CHANGED OF CMB-City IN FRAME Dialog-Frame /* City */
    DO:
        /* City is the lowest level - no further cascading needed */
        /* Could add additional logic here like populating postal codes */
    
        /* Optional: Show selected city information */
        IF CMB-City:SCREEN-VALUE IN FRAME Dialog-Frame <> "" THEN
            MESSAGE "Selected city:" CMB-City:SCREEN-VALUE IN FRAME Dialog-Frame 
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB-Country
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB-Country Dialog-Frame
ON VALUE-CHANGED OF CMB-Country IN FRAME Dialog-Frame /* Country */
    DO:
        /* Clear dependent combo boxes */
        ASSIGN
            CMB-State:LIST-ITEMS IN FRAME Dialog-Frame   = ""
            CMB-State:SCREEN-VALUE IN FRAME Dialog-Frame = ""
            CMB-City:LIST-ITEMS IN FRAME Dialog-Frame    = ""
            CMB-City:SCREEN-VALUE IN FRAME Dialog-Frame  = "".
    
        /* Populate states for the selected country */
        RUN populate-states-by-country.  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB-MaritalStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB-MaritalStatus Dialog-Frame


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME CMB-State
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB-State Dialog-Frame
ON VALUE-CHANGED OF CMB-State IN FRAME Dialog-Frame /* State */
    DO:
        /* Clear dependent combo boxes */
        ASSIGN
            CMB-City:LIST-ITEMS IN FRAME Dialog-Frame   = ""
            CMB-City:SCREEN-VALUE IN FRAME Dialog-Frame = "".
    
        /* Populate cities for the selected state */
        RUN populate-cities-by-state.  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FLN-PostalCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FLN-PostalCode Dialog-Frame
ON VALUE-CHANGED OF FLN-PostalCode IN FRAME Dialog-Frame /* Postal Code */
    DO:
        RUN populate-city-combo.
        RUN populate-state-combo.     
        RUN populate-country-combo.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.
    
    /* Load customer data if ID provided */
    IF piCustomerId > 0 and lc(cAction  ) = "update" THEN 
    DO:
        FIND FIRST CustomerDetails WHERE CustomerDetails.CustID = piCustomerId NO-LOCK NO-ERROR.
        IF AVAILABLE CustomerDetails THEN 
        DO:
            /* Just brutally overwrite all the screen values */
            FLN-FirstName:SCREEN-VALUE IN FRAME Dialog-Frame = CustomerDetails.FirstName.
            FLN-LastName:SCREEN-VALUE IN FRAME Dialog-Frame = CustomerDetails.LastName.
            FLN-DateOfBirth:SCREEN-VALUE IN FRAME Dialog-Frame = STRING(CustomerDetails.DOB, "99/99/99").
            FLN-Address:SCREEN-VALUE IN FRAME Dialog-Frame = CustomerDetails.Address1.
            FLN-Address_2:SCREEN-VALUE IN FRAME Dialog-Frame = CustomerDetails.Address2.
            FLN-Postalcode:SCREEN-VALUE IN FRAME Dialog-Frame = STRING(CustomerDetails.ZipCode).
            
            /* Set combo box values directly */
            CMB-MaritalStatus:SCREEN-VALUE IN FRAME Dialog-Frame = CustomerDetails.MaritalStatus.
            CMB-Country:SCREEN-VALUE IN FRAME Dialog-Frame = CustomerDetails.Country.
            CMB-State:SCREEN-VALUE IN FRAME Dialog-Frame = CustomerDetails.State.
            CMB-City:SCREEN-VALUE IN FRAME Dialog-Frame = CustomerDetails.City.
        END.
    END.
    ELSE 
    DO:
        /* New customer - populate combo boxes */
        RUN populate-all-countries.
        RUN populate-all-states.
        RUN populate-all-cities.
        RUN populate-marital-status.
        
    END.
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.



PROCEDURE populate-city-combo:
    DEFINE VARIABLE entered-postal  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE found-city-code AS CHARACTER NO-UNDO.
    DEFINE VARIABLE city-list       AS CHARACTER NO-UNDO.
    
    /* Get the postal code entered by user */
    entered-postal = TRIM(FLN-Postalcode:SCREEN-VALUE IN FRAME Dialog-Frame).
    
    /* Clear the combo box first */
    CMB-City:LIST-ITEMS IN FRAME Dialog-Frame = "".
    
    /* Check if postal code is entered */
    IF entered-postal = "" THEN RETURN.
    
    /* Find the postal code in PostalCode table */
    FIND FIRST PostalCode WHERE PostalCode.ZIPCode = entered-postal NO-LOCK NO-ERROR.
    
    IF AVAILABLE PostalCode THEN 
    DO:
        /* Get the city code from PostalCode table */
        found-city-code = PostalCode.CItyCode.
        
        /* Build city list from City table based on city code */
        FOR EACH City WHERE City.CityCode = found-city-code NO-LOCK:
            IF city-list = "" THEN
                city-list = City.CityName.
            ELSE
                city-list = city-list + "," + City.CityName.
        END.
        
        /* Populate the CMB-City combo box */
        IF city-list <> "" THEN 
        DO:
            CMB-City:LIST-ITEMS IN FRAME Dialog-Frame = city-list.
            /* Optionally select the first city */
            CMB-City:SCREEN-VALUE IN FRAME Dialog-Frame = ENTRY(1, city-list).
        END.
        ELSE 
        DO:
            MESSAGE "No cities found for postal code" entered-postal VIEW-AS ALERT-BOX.
        END.
    END.
    ELSE 
    DO:
        /* Postal code not found */
        MESSAGE "Postal code" entered-postal "not found in database" VIEW-AS ALERT-BOX.
        CMB-City:LIST-ITEMS IN FRAME Dialog-Frame = "".
    END.
END PROCEDURE.


PROCEDURE populate-state-combo:
    
    DEFINE VARIABLE cStateCode AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStateList AS CHARACTER NO-UNDO.
    
    /* Clear the state combo box first */
    CMB-State:LIST-ITEMS IN FRAME Dialog-Frame = "".
    
    FIND FIRST City WHERE City.CityName = CMB-City:SCREEN-VALUE IN FRAME Dialog-Frame NO-LOCK NO-ERROR.
    
    IF AVAILABLE City THEN 
    DO:
        cStateCode = City.StateCode.
        
        /* Build state list using the state code from City table */
        FOR EACH State WHERE State.StateCode = cStateCode NO-LOCK:
            IF cStateList = "" THEN
                cStateList = State.StateName.
            ELSE
                cStateList = cStateList + "," + State.StateName.
        END.
        
        /* Populate the state combo box */
        IF cStateList <> "" THEN 
        DO:
            CMB-State:LIST-ITEMS IN FRAME Dialog-Frame = cStateList.
            /* Optionally select the first state */
            CMB-State:SCREEN-VALUE IN FRAME Dialog-Frame = ENTRY(1, cStateList).
        END.
        ELSE 
        DO:
            MESSAGE "No states found for state code:" cStateCode VIEW-AS ALERT-BOX.
        END.
    END.
    ELSE 
    DO:
        MESSAGE "City not found:" CMB-City:SCREEN-VALUE IN FRAME Dialog-Frame VIEW-AS ALERT-BOX.
    END.
    
END PROCEDURE.


PROCEDURE populate-country-combo:
    
    DEFINE VARIABLE cCountryCode AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCountryList AS CHARACTER NO-UNDO.
    
    /* Clear the country combo box first */
    CMB-Country:LIST-ITEMS IN FRAME Dialog-Frame = "".
    
    FIND FIRST State WHERE State.StateName = CMB-State:SCREEN-VALUE IN FRAME Dialog-Frame NO-LOCK NO-ERROR.
    
    IF AVAILABLE State THEN 
    DO:
        cCountryCode = State.CountryCode.
        
        /* Build country list using the country code from State table */
        FOR EACH Country WHERE Country.CountryCode = cCountryCode NO-LOCK:
            IF cCountryList = "" THEN
                cCountryList = Country.CountryName.
            ELSE
                cCountryList = cCountryList + "," + Country.CountryName.
        END.
        
        /* Populate the country combo box */
        IF cCountryList <> "" THEN 
        DO:
            CMB-Country:LIST-ITEMS IN FRAME Dialog-Frame = cCountryList.
            /* Optionally select the first country */
            CMB-Country:SCREEN-VALUE IN FRAME Dialog-Frame = ENTRY(1, cCountryList).
        END.
        ELSE 
        DO:
            MESSAGE "No countries found for country code:" cCountryCode VIEW-AS ALERT-BOX.
        END.
    END.
    ELSE 
    DO:
        MESSAGE "State not found:" CMB-State:SCREEN-VALUE IN FRAME Dialog-Frame VIEW-AS ALERT-BOX.
    END.
    
END PROCEDURE.

/*populate ALL countries*/
PROCEDURE populate-all-countries:
    
    DEFINE VARIABLE cCountryList AS CHARACTER NO-UNDO.
    
    /* Clear the country combo box first */
    CMB-Country:LIST-ITEMS IN FRAME Dialog-Frame = "".
    
    /* Build complete country list from Country table */
    FOR EACH Country NO-LOCK BY Country.CountryName:
        IF cCountryList = "" THEN
            cCountryList = Country.CountryName.
        ELSE
            cCountryList = cCountryList + "," + Country.CountryName.
    END.
    
    /* Populate the country combo box with all countries */
    IF cCountryList <> "" THEN 
    DO:
        CMB-Country:LIST-ITEMS IN FRAME Dialog-Frame = cCountryList.
    /*        MESSAGE "Loaded" NUM-ENTRIES(cCountryList) "countries" VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    END.
    ELSE 
    DO:
        MESSAGE "No countries found in database" VIEW-AS ALERT-BOX WARNING BUTTONS OK.
    END.
    
END PROCEDURE.


/*populate ALL states*/
PROCEDURE populate-all-states:
    
    DEFINE VARIABLE cStateList AS CHARACTER NO-UNDO.
    
    /* Clear the state combo box first */
    CMB-State:LIST-ITEMS IN FRAME Dialog-Frame = "".
    
    /* Build complete state list from State table */
    FOR EACH State NO-LOCK BY State.StateName:
        IF cStateList = "" THEN
            cStateList = State.StateName.
        ELSE
            cStateList = cStateList + "," + State.StateName.
    END.
    
    /* Populate the state combo box with all states */
    IF cStateList <> "" THEN 
    DO:
        CMB-State:LIST-ITEMS IN FRAME Dialog-Frame = cStateList.
    /*        MESSAGE "Loaded" NUM-ENTRIES(cStateList) "states" VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    END.
    ELSE 
    DO:
        MESSAGE "No states found in database" VIEW-AS ALERT-BOX WARNING BUTTONS OK.
    END.
    
END PROCEDURE.


/*populate ALL cities*/
PROCEDURE populate-all-cities:
    
    DEFINE VARIABLE cCityList AS CHARACTER NO-UNDO.
    
    /* Clear the city combo box first */
    CMB-City:LIST-ITEMS IN FRAME Dialog-Frame = "".
    
    /* Build complete city list from City table */
    FOR EACH City NO-LOCK BY City.CityName:
        IF cCityList = "" THEN
            cCityList = City.CityName.
        ELSE
            cCityList = cCityList + "," + City.CityName.
    END.
    
    /* Populate the city combo box with all cities */
    IF cCityList <> "" THEN 
    DO:
        CMB-City:LIST-ITEMS IN FRAME Dialog-Frame = cCityList.
    /*        MESSAGE "Loaded" NUM-ENTRIES(cCityList) "cities" VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    END.
    ELSE 
    DO:
        MESSAGE "No cities found in database" VIEW-AS ALERT-BOX WARNING BUTTONS OK.
    END.
    
END PROCEDURE.


PROCEDURE populate-states-by-country:
    
    DEFINE VARIABLE selectedCountry AS CHARACTER NO-UNDO.
    DEFINE VARIABLE countryCode     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE stateList       AS CHARACTER NO-UNDO.
    
    /* Get selected country name */
    selectedCountry = CMB-Country:SCREEN-VALUE IN FRAME Dialog-Frame.
    
    /* Clear states and cities combo boxes */
    CMB-State:LIST-ITEMS IN FRAME Dialog-Frame = "".
    CMB-City:LIST-ITEMS IN FRAME Dialog-Frame = "".
    
    IF selectedCountry = "" OR selectedCountry = ? THEN RETURN.
    
    /* Find country record to get country code */
    FIND FIRST Country WHERE Country.CountryName = selectedCountry NO-LOCK NO-ERROR.
    
    IF AVAILABLE Country THEN 
    DO:
        countryCode = Country.CountryCode.
        
        /* Build state list for the selected country */
        FOR EACH State WHERE State.CountryCode = countryCode NO-LOCK BY State.StateName:
            IF stateList = "" THEN
                stateList = State.StateName.
            ELSE
                stateList = stateList + "," + State.StateName.
        END.
        
        /* Populate states combo box */
        IF stateList <> "" THEN 
        DO:
            CMB-State:LIST-ITEMS IN FRAME Dialog-Frame = stateList.
            CMB-State:SENSITIVE IN FRAME Dialog-Frame = TRUE.
        END.
        ELSE 
        DO:
            MESSAGE "No states found for country:" selectedCountry VIEW-AS ALERT-BOX INFO BUTTONS OK.
            CMB-State:SENSITIVE IN FRAME Dialog-Frame = FALSE.
        END.
    END.
    ELSE 
    DO:
        MESSAGE "Country not found:" selectedCountry VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        CMB-State:SENSITIVE IN FRAME Dialog-Frame = FALSE.
    END.
    
END PROCEDURE.



PROCEDURE populate-cities-by-state:
    
    DEFINE VARIABLE selectedState AS CHARACTER NO-UNDO.
    DEFINE VARIABLE stateCode     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cityList      AS CHARACTER NO-UNDO.
    
    /* Get selected state name */
    selectedState = CMB-State:SCREEN-VALUE IN FRAME Dialog-Frame.
    
    /* Clear cities combo box */
    CMB-City:LIST-ITEMS IN FRAME Dialog-Frame = "".
    
    IF selectedState = "" OR selectedState = ? THEN RETURN.
    
    /* Find state record to get state code */
    FIND FIRST State WHERE State.StateName = selectedState NO-LOCK NO-ERROR.
    
    IF AVAILABLE State THEN 
    DO:
        stateCode = State.StateCode.
        
        /* Build city list for the selected state */
        FOR EACH City WHERE City.StateCode = stateCode NO-LOCK BY City.CityName:
            IF cityList = "" THEN
                cityList = City.CityName.
            ELSE
                cityList = cityList + "," + City.CityName.
        END.
        
        /* Populate cities combo box */
        IF cityList <> "" THEN 
        DO:
            CMB-City:LIST-ITEMS IN FRAME Dialog-Frame = cityList.
            CMB-City:SENSITIVE IN FRAME Dialog-Frame = TRUE.
        END.
        ELSE 
        DO:
            MESSAGE "No cities found for state:" selectedState VIEW-AS ALERT-BOX INFO BUTTONS OK.
            CMB-City:SENSITIVE IN FRAME Dialog-Frame = FALSE.
        END.
    END.
    ELSE 
    DO:
        MESSAGE "State not found:" selectedState VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        CMB-City:SENSITIVE IN FRAME Dialog-Frame = FALSE.
    END.
    
END PROCEDURE.


PROCEDURE update-customer :
    /*------------------------------------------------------------------------------
      Purpose:     Update existing customer using Backend.Customer class
    ------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE oCustomer AS Backend.Customer NO-UNDO.
    DEFINE VARIABLE lSuccess  AS LOGICAL          NO-UNDO.

    /* Clear and create temp-table record */
    EMPTY TEMP-TABLE ttCustomerDetails.
    CREATE ttCustomerDetails.
    
    ASSIGN 
        ttCustomerDetails.CustID         = piCustomerId
        ttCustomerDetails.FirstName      = FLN-FirstName:SCREEN-VALUE IN FRAME Dialog-Frame
        ttCustomerDetails.LastName       = FLN-LastName:SCREEN-VALUE IN FRAME Dialog-Frame
        ttCustomerDetails.date_of_birth  = DATE(FLN-DateOfBirth:SCREEN-VALUE IN FRAME Dialog-Frame)
        ttCustomerDetails.marital_status = CMB-MaritalStatus:SCREEN-VALUE IN FRAME Dialog-Frame
        ttCustomerDetails.address        = FLN-Address:SCREEN-VALUE IN FRAME Dialog-Frame
        ttCustomerDetails.address_2      = FLN-Address_2:SCREEN-VALUE IN FRAME Dialog-Frame
        ttCustomerDetails.City           = CMB-City:SCREEN-VALUE IN FRAME Dialog-Frame
        ttCustomerDetails.State          = CMB-State:SCREEN-VALUE IN FRAME Dialog-Frame
        ttCustomerDetails.postal_code    = INTEGER(FLN-PostalCode:SCREEN-VALUE IN FRAME Dialog-Frame)
        ttCustomerDetails.Country        = CMB-Country:SCREEN-VALUE IN FRAME Dialog-Frame
        ttCustomerDetails.email          = ""
        ttCustomerDetails.mobile_num     = "".

    /* Create controller and call update method */
    oCustomer = NEW Backend.Customer().
    lSuccess = oCustomer:UpdateCustomerDetails(INPUT TABLE ttCustomerDetails).
    
    /* Show result */
    IF lSuccess THEN
        MESSAGE "Customer updated successfully!" VIEW-AS ALERT-BOX INFO.
    ELSE
        MESSAGE "Failed to update customer!" VIEW-AS ALERT-BOX ERROR.

    /* Cleanup */
    DELETE OBJECT oCustomer.

END PROCEDURE.

PROCEDURE populate-marital-status :
    /*------------------------------------------------------------------------------
      Purpose:     Populate marital status combo box using Utility.UtilityManager
      Parameters:  <none>
      Notes:       Called from main block to load marital status options
    ------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE oUtility           AS UtilityManager NO-UNDO.
    DEFINE VARIABLE cMaritalStatusList AS CHARACTER      NO-UNDO.
    
    /* Clear the combo box first */
    CMB-MaritalStatus:LIST-ITEMS IN FRAME Dialog-Frame = "".
    
    /* Create utility manager and get marital status list */
    oUtility = NEW UtilityManager().
    cMaritalStatusList = oUtility:GetMaritalStatusList().
    
    /* Populate the combo box */
    IF cMaritalStatusList <> "" THEN 
    DO:
        CMB-MaritalStatus:LIST-ITEMS IN FRAME Dialog-Frame = cMaritalStatusList.
    END.
    ELSE 
    DO:
        MESSAGE "No marital status options available" VIEW-AS ALERT-BOX WARNING.
    END.
    
    /* Clean up */
    DELETE OBJECT oUtility.
    
    CATCH eError AS Progress.Lang.Error:
        MESSAGE "Error loading marital status options:" eError:GetMessage(1) VIEW-AS ALERT-BOX ERROR.
        IF VALID-OBJECT(oUtility) THEN
            DELETE OBJECT oUtility.
    END CATCH.

END PROCEDURE.



PROCEDURE create-customer :
    /*------------------------------------------------------------------------------
      Purpose:     Create new customer using Backend.Customer class
    ------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE oCustomer AS Backend.Customer NO-UNDO.
    DEFINE VARIABLE lSuccess  AS LOGICAL          NO-UNDO.

    /* Clear and create temp-table record */
    EMPTY TEMP-TABLE ttCustomerDetails.
    CREATE ttCustomerDetails.
    
    ASSIGN 
        ttCustomerDetails.FirstName      = FLN-FirstName:SCREEN-VALUE IN FRAME Dialog-Frame
        ttCustomerDetails.LastName       = FLN-LastName:SCREEN-VALUE IN FRAME Dialog-Frame
        ttCustomerDetails.date_of_birth  = DATE(FLN-DateOfBirth:SCREEN-VALUE IN FRAME Dialog-Frame)
        ttCustomerDetails.marital_status = CMB-MaritalStatus:SCREEN-VALUE IN FRAME Dialog-Frame
        ttCustomerDetails.address        = FLN-Address:SCREEN-VALUE IN FRAME Dialog-Frame
        ttCustomerDetails.address_2      = FLN-Address_2:SCREEN-VALUE IN FRAME Dialog-Frame
        ttCustomerDetails.City           = CMB-City:SCREEN-VALUE IN FRAME Dialog-Frame
        ttCustomerDetails.State          = CMB-State:SCREEN-VALUE IN FRAME Dialog-Frame
        ttCustomerDetails.postal_code    = INTEGER(FLN-PostalCode:SCREEN-VALUE IN FRAME Dialog-Frame)
        ttCustomerDetails.Country        = CMB-Country:SCREEN-VALUE IN FRAME Dialog-Frame
        ttCustomerDetails.email          = ""
        ttCustomerDetails.mobile_num     = "".

    /* Create controller and call create method */
    oCustomer = NEW Backend.Customer().
    lSuccess = oCustomer:AddCustomerDetails(INPUT TABLE ttCustomerDetails).
    
    /* Show result */
    IF lSuccess THEN
        MESSAGE "Customer created successfully!" VIEW-AS ALERT-BOX INFO.
    ELSE
        MESSAGE "Failed to create customer!" VIEW-AS ALERT-BOX ERROR.

    /* Cleanup */
    DELETE OBJECT oCustomer.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
    /*------------------------------------------------------------------------------
      Purpose:     DISABLE the User Interface
      Parameters:  <none>
      Notes:       Here we clean-up the user-interface by deleting
                   dynamic widgets we have created and/or hide 
                   frames.  This procedure is usually called when
                   we are ready to "clean-up" after running.
    ------------------------------------------------------------------------------*/
    /* Hide all frames. */
    HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
    /*------------------------------------------------------------------------------
      Purpose:     ENABLE the User Interface
      Parameters:  <none>
      Notes:       Here we display/view/enable the widgets in the
                   user-interface.  In addition, OPEN all queries
                   associated with each FRAME and BROWSE.
                   These statements here are based on the "Other 
                   Settings" section of the widget Property Sheets.
    ------------------------------------------------------------------------------*/
    DISPLAY FLN-FirstName FLN-LastName FLN-DateOfBirth CMB-MaritalStatus 
        FLN-Address FLN-Address_2 CMB-State FLN-PostalCode CMB-Country 
        CMB-City 
        WITH FRAME Dialog-Frame.
    ENABLE RECT-9 RECT-11 FLN-FirstName FLN-LastName FLN-DateOfBirth 
        CMB-MaritalStatus FLN-Address FLN-Address_2 CMB-State FLN-PostalCode 
        CMB-Country CMB-City BTN-Save BTN-Cancel 
        WITH FRAME Dialog-Frame.
    VIEW FRAME Dialog-Frame.
    {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

