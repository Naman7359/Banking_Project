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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS Procedure

USING Backend.Customer FROM PROPATH.
{Backend/Utility/temp_table.i}

&ANALYZE-RESUME
/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---         
                                  */
DEFINE INPUT PARAMETER piCustomerId AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER cAction AS CHARACTER NO-UNDO.           

/* Local Variable Definitions ---                                       */

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
        
        IF LC(cAction) = "update" THEN 
            RUN update-customer.
        ELSE 
            RUN create-customer.
    
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
    RUN InitialSetup.
     
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitialSetup Dialog-Frame
PROCEDURE InitialSetup :
    /*-----------------------------------------------------------
       Purpose : One-time form setup + load customer (update mode)
    -----------------------------------------------------------*/

    /* 1.  Populate the static lists */
    RUN populate-all-countries.
    RUN populate-all-states.
    RUN populate-all-cities.
    RUN populate-marital-status.

    /* 2.  Only continue if we are in UPDATE mode */
    IF piCustomerId > 0                                             
        AND CAN-DO("update", STRING(lc(cAction))) THEN 
    DO:              

        DEFINE VARIABLE oCtr   AS Backend.Customer NO-UNDO.
        DEFINE VARIABLE lFound AS LOGICAL          NO-UNDO.

        /* Clear temp-table that will receive the data */
        EMPTY TEMP-TABLE ttCustomerDetails.

        /* --- call the class method --- */
        oCtr  = NEW Backend.Customer().
        lFound = oCtr:GetCustomerForForm( INPUT  piCustomerId
            , OUTPUT TABLE ttCustomerDetails).

        IF lFound THEN 
        DO:
            FIND FIRST ttCustomerDetails NO-ERROR.
            IF AVAILABLE ttCustomerDetails THEN
                ASSIGN
                    /* fill-ins */
                    FLN-FirstName:SCREEN-VALUE IN FRAME Dialog-Frame     = ttCustomerDetails.FirstName
                    FLN-LastName:SCREEN-VALUE  IN FRAME Dialog-Frame     = ttCustomerDetails.LastName
                    FLN-DateOfBirth:SCREEN-VALUE IN FRAME Dialog-Frame   = (IF ttCustomerDetails.date_of_birth <> ?
                          THEN STRING(ttCustomerDetails.date_of_birth,"99/99/9999") ELSE "")
                    FLN-Address:SCREEN-VALUE  IN FRAME Dialog-Frame      = ttCustomerDetails.address
                    FLN-Address_2:SCREEN-VALUE IN FRAME Dialog-Frame     = ttCustomerDetails.address_2
                    FLN-Postalcode:SCREEN-VALUE IN FRAME Dialog-Frame    = (IF ttCustomerDetails.postal_code <> ?
                          THEN STRING(ttCustomerDetails.postal_code) ELSE "")
                    /* combo-boxes */
                    CMB-MaritalStatus:SCREEN-VALUE IN FRAME Dialog-Frame = ttCustomerDetails.marital_status
                    CMB-Country:SCREEN-VALUE IN FRAME Dialog-Frame       = ttCustomerDetails.Country
                    CMB-State:SCREEN-VALUE   IN FRAME Dialog-Frame       = ttCustomerDetails.State
                    CMB-City:SCREEN-VALUE    IN FRAME Dialog-Frame       = ttCustomerDetails.City.
        END.
        ELSE
            MESSAGE "Customer ID" piCustomerId "not found." VIEW-AS ALERT-BOX WARNING.

        /* tidy up */
        IF VALID-OBJECT(oCtr) THEN DELETE OBJECT oCtr NO-ERROR.
    END. /* update mode */

END PROCEDURE.




/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



PROCEDURE populate-city-combo:
    /*------------------------------------------------------------------------
        Purpose: Populate city combo box based on postal code
        Notes:   Uses Backend.Customer class method
      ----------------------------------------------------------------------*/
    
    DEFINE VARIABLE entered-postal AS CHARACTER NO-UNDO.
    DEFINE VARIABLE city-list      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE oController    AS Customer  NO-UNDO.
    DEFINE VARIABLE iCityCount     AS INTEGER   NO-UNDO INITIAL 0.

    /* Get the postal code entered by user */
    entered-postal = TRIM(FLN-Postalcode:SCREEN-VALUE IN FRAME Dialog-Frame).

    /* Clear the combo box first */
    CMB-City:LIST-ITEMS IN FRAME Dialog-Frame = "".
    CMB-City:SCREEN-VALUE IN FRAME Dialog-Frame = "".

    /* Check if postal code is entered */
    IF entered-postal = "" OR entered-postal = ? THEN 
    DO:
        MESSAGE "Please enter a postal code." VIEW-AS ALERT-BOX INFO.
        RETURN.
    END.

    /* Clear previous temp-table data */
    EMPTY TEMP-TABLE ttCities.

    /* Create controller instance and call method */
    DO ON ERROR UNDO, THROW:
        
        oController = NEW Backend.Customer().
        lFound = oController:GetCitiesByPostalCode(INPUT entered-postal, OUTPUT TABLE ttCities).

        IF lFound THEN 
        DO:
            /* Build city list from temp-table */
            FOR EACH ttCities:
                IF city-list = "" THEN
                    city-list = ttCities.CityName.
                ELSE
                    city-list = city-list + "," + ttCities.CityName.
                    
                iCityCount = iCityCount + 1.
            END.

            /* Populate the CMB-City combo box */
            IF city-list <> "" THEN 
            DO:
                CMB-City:LIST-ITEMS IN FRAME Dialog-Frame = city-list.
                /* Optionally select the first city */
                CMB-City:SCREEN-VALUE IN FRAME Dialog-Frame = ENTRY(1, city-list).
                
                MESSAGE "Found" iCityCount "city(ies) for postal code" entered-postal
                    VIEW-AS ALERT-BOX INFO.
            END.
        END.
        ELSE 
        DO:
            MESSAGE "No cities found for postal code" entered-postal 
                VIEW-AS ALERT-BOX WARNING.
        END.
    END.
    CATCH eError AS Progress.Lang.Error:
        MESSAGE "Error retrieving cities: " + eError:GetMessage(1)
            VIEW-AS ALERT-BOX ERROR.
    END CATCH.
    
    FINALLY:
        /* Clean up controller object */
        IF VALID-OBJECT(oController) THEN
            DELETE OBJECT oController NO-ERROR.
    END FINALLY.

END PROCEDURE.


PROCEDURE populate-state-combo:
    /*------------------------------------------------------------------------
        Purpose: Populate state combo box using class method
        Notes:   Uses Backend.Customer class method with ttStateDetails temp-table
      ----------------------------------------------------------------------*/
    
    DEFINE VARIABLE oController   AS Backend.Customer NO-UNDO.
    DEFINE VARIABLE cSelectedCity AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cStateList    AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE lFound        AS LOGICAL          NO-UNDO.

    /* Get selected city from combo box */
    cSelectedCity = TRIM(CMB-City:SCREEN-VALUE IN FRAME Dialog-Frame).

    /* Clear the state combo box */
    CMB-State:LIST-ITEMS IN FRAME Dialog-Frame = "".
    CMB-State:SCREEN-VALUE IN FRAME Dialog-Frame = "".

    /* Validate city selection */
    IF cSelectedCity = "" OR cSelectedCity = ? THEN 
    DO:
        RETURN.
    END.

    /* Clear previous temp-table data */
    EMPTY TEMP-TABLE ttStateDetails.

    /* Use class method to get states */
    DO ON ERROR UNDO, THROW:
        
        oController = NEW Backend.Customer().
        lFound = oController:GetStatesByCityName(INPUT cSelectedCity, 
            OUTPUT TABLE ttStateDetails).

        IF lFound THEN 
        DO:
            /* Build state list from temp-table */
            FOR EACH ttStateDetails:
                IF cStateList = "" THEN
                    cStateList = ttStateDetails.StateName.
                ELSE
                    cStateList = cStateList + "," + ttStateDetails.StateName.
            END.

            /* Populate the state combo box */
            IF cStateList <> "" THEN 
            DO:
                CMB-State:LIST-ITEMS IN FRAME Dialog-Frame = cStateList.
                /* Optionally select the first state */
                CMB-State:SCREEN-VALUE IN FRAME Dialog-Frame = ENTRY(1, cStateList).
                
                MESSAGE "Found states for city:" cSelectedCity VIEW-AS ALERT-BOX INFO.
            END.
        END.
        ELSE 
        DO:
            MESSAGE "No states found for city:" cSelectedCity VIEW-AS ALERT-BOX INFO.
        END.
    END.

    CATCH eError AS Progress.Lang.Error:
        MESSAGE "Error retrieving states: " + eError:GetMessage(1)
            VIEW-AS ALERT-BOX ERROR.
    END CATCH.
    
    FINALLY:
        /* Clean up */
        IF VALID-OBJECT(oController) THEN
            DELETE OBJECT oController NO-ERROR.
    END FINALLY.

END PROCEDURE.

PROCEDURE populate-country-combo:
    /*------------------------------------------------------------------------
        Purpose: Populate country combo box using class method
        Notes:   Uses Backend.Customer class method with ttCountryDetails temp-table
      ----------------------------------------------------------------------*/
    
    DEFINE VARIABLE oController    AS Backend.Customer NO-UNDO.
    DEFINE VARIABLE cSelectedState AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cCountryList   AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE lFound         AS LOGICAL          NO-UNDO.

    /* Get selected state from combo box */
    cSelectedState = TRIM(CMB-State:SCREEN-VALUE IN FRAME Dialog-Frame).

    /* Clear the country combo box */
    CMB-Country:LIST-ITEMS IN FRAME Dialog-Frame = "".
    CMB-Country:SCREEN-VALUE IN FRAME Dialog-Frame = "".

    /* Validate state selection */
    IF cSelectedState = "" OR cSelectedState = ? THEN 
    DO:
        RETURN.
    END.

    /* Clear previous temp-table data */
    EMPTY TEMP-TABLE ttCountryDetails.

    /* Use class method to get countries */
    DO ON ERROR UNDO, THROW:
        
        oController = NEW Backend.Customer().
        lFound = oController:GetCountriesByStateName(INPUT cSelectedState, 
            OUTPUT TABLE ttCountryDetails).

        IF lFound THEN 
        DO:
            /* Build country list from temp-table */
            FOR EACH ttCountryDetails:
                IF cCountryList = "" THEN
                    cCountryList = ttCountryDetails.CountryName.
                ELSE
                    cCountryList = cCountryList + "," + ttCountryDetails.CountryName.
            END.

            /* Populate the country combo box */
            IF cCountryList <> "" THEN 
            DO:
                CMB-Country:LIST-ITEMS IN FRAME Dialog-Frame = cCountryList.
                /* Optionally select the first country */
                CMB-Country:SCREEN-VALUE IN FRAME Dialog-Frame = ENTRY(1, cCountryList).
                
                MESSAGE "Found countries for state:" cSelectedState VIEW-AS ALERT-BOX INFO.
            END.
        END.
        ELSE 
        DO:
            MESSAGE "No countries found for state:" cSelectedState VIEW-AS ALERT-BOX INFO.
        END.
    END.
    CATCH eError AS Progress.Lang.Error:
        MESSAGE "Error retrieving countries: " + eError:GetMessage(1)
            VIEW-AS ALERT-BOX ERROR.
    END CATCH.
    
    FINALLY:
        /* Clean up */
        IF VALID-OBJECT(oController) THEN
            DELETE OBJECT oController NO-ERROR.
    END FINALLY.

END PROCEDURE.


/*populate ALL countries*/
PROCEDURE populate-all-countries:
    /*------------------------------------------------------------------------
        Purpose: Populate country combo box with all countries using class method
        Notes:   Uses Backend.Customer class method with ttCountryDetails temp-table
      ----------------------------------------------------------------------*/
    
    DEFINE VARIABLE oController   AS Backend.Customer NO-UNDO.
    DEFINE VARIABLE cCountryList  AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE lFound        AS LOGICAL          NO-UNDO.
    DEFINE VARIABLE iCountryCount AS INTEGER          NO-UNDO INITIAL 0.

    /* Clear the country combo box */
    CMB-Country:LIST-ITEMS IN FRAME Dialog-Frame = "".
    CMB-Country:SCREEN-VALUE IN FRAME Dialog-Frame = "".

    /* Clear previous temp-table data */
    EMPTY TEMP-TABLE ttCountryDetails.

    /* Use class method to get all countries */
    DO ON ERROR UNDO, THROW:
        
        oController = NEW Backend.Customer().
        lFound = oController:GetAllCountries(OUTPUT TABLE ttCountryDetails).

        IF lFound THEN 
        DO:
            /* Build country list from temp-table (already sorted by name) */
            FOR EACH ttCountryDetails:
                IF cCountryList = "" THEN
                    cCountryList = ttCountryDetails.CountryName.
                ELSE
                    cCountryList = cCountryList + "," + ttCountryDetails.CountryName.
                    
                iCountryCount = iCountryCount + 1.
            END.

            /* Populate the country combo box */
            IF cCountryList <> "" THEN 
            DO:
                CMB-Country:LIST-ITEMS IN FRAME Dialog-Frame = cCountryList.
                
            /* Optional: Show count message */
            /* MESSAGE "Loaded" iCountryCount "countries" VIEW-AS ALERT-BOX INFO BUTTONS OK. */
            END.
        END.
        ELSE 
        DO:
            MESSAGE "No countries found in database" VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        END.

        CATCH eError AS Progress.Lang.Error:
            MESSAGE "Error retrieving countries: " + eError:GetMessage(1)
                VIEW-AS ALERT-BOX ERROR.
        END CATCH.
    END.
    FINALLY:
        /* Clean up */
        IF VALID-OBJECT(oController) THEN
            DELETE OBJECT oController NO-ERROR.
    END FINALLY.

END PROCEDURE.



/*populate ALL states*/
PROCEDURE populate-all-states:
    /*------------------------------------------------------------------------
        Purpose: Populate state combo box with all states using class method
        Notes:   Uses Backend.Customer class method with ttStateDetails temp-table
      ----------------------------------------------------------------------*/
    
    DEFINE VARIABLE oController AS Backend.Customer NO-UNDO.
    DEFINE VARIABLE cStateList  AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE lFound      AS LOGICAL          NO-UNDO.
    DEFINE VARIABLE iStateCount AS INTEGER          NO-UNDO INITIAL 0.

    /* Clear the state combo box */
    CMB-State:LIST-ITEMS IN FRAME Dialog-Frame = "".
    CMB-State:SCREEN-VALUE IN FRAME Dialog-Frame = "".

    /* Clear previous temp-table data */
    EMPTY TEMP-TABLE ttStateDetails.

    /* Use class method to get all states */
    DO ON ERROR UNDO, THROW:
        
        oController = NEW Backend.Customer().
        lFound = oController:GetAllStates(OUTPUT TABLE ttStateDetails).

        IF lFound THEN 
        DO:
            /* Build state list from temp-table (already sorted by name) */
            FOR EACH ttStateDetails:
                IF cStateList = "" THEN
                    cStateList = ttStateDetails.StateName.
                ELSE
                    cStateList = cStateList + "," + ttStateDetails.StateName.
                    
                iStateCount = iStateCount + 1.
            END.

            /* Populate the state combo box */
            IF cStateList <> "" THEN 
            DO:
                CMB-State:LIST-ITEMS IN FRAME Dialog-Frame = cStateList.
                
            /* Optional: Show count message */
            /* MESSAGE "Loaded" iStateCount "states" VIEW-AS ALERT-BOX INFO BUTTONS OK. */
            END.
        END.
        ELSE 
        DO:
            MESSAGE "No states found in database" VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        END.

        CATCH eError AS Progress.Lang.Error:
            MESSAGE "Error retrieving states: " + eError:GetMessage(1)
                VIEW-AS ALERT-BOX ERROR.
        END CATCH.
    END.    
    FINALLY:
        /* Clean up */
        IF VALID-OBJECT(oController) THEN
            DELETE OBJECT oController NO-ERROR.
    END FINALLY.

END PROCEDURE.



PROCEDURE populate-all-cities:
    /*------------------------------------------------------------------------
        Purpose: Populate city combo box with all cities using class method
        Notes:   Uses Backend.Customer class method with ttCities temp-table
      ----------------------------------------------------------------------*/
    
    DEFINE VARIABLE oController AS Backend.Customer NO-UNDO.
    DEFINE VARIABLE cCityList   AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE lFound      AS LOGICAL          NO-UNDO.
    DEFINE VARIABLE iCityCount  AS INTEGER          NO-UNDO INITIAL 0.

    /* Clear the city combo box */
    CMB-City:LIST-ITEMS IN FRAME Dialog-Frame = "".
    CMB-City:SCREEN-VALUE IN FRAME Dialog-Frame = "".

    /* Clear previous temp-table data */
    EMPTY TEMP-TABLE ttCities.

    /* Use class method to get all cities */
    DO ON ERROR UNDO, THROW:
        
        oController = NEW Backend.Customer().
        lFound = oController:GetAllCities(OUTPUT TABLE ttCities).

        IF lFound THEN 
        DO:
            /* Build city list from temp-table (already sorted by name) */
            FOR EACH ttCities:
                IF cCityList = "" THEN
                    cCityList = ttCities.CityName.
                ELSE
                    cCityList = cCityList + "," + ttCities.CityName.
                    
                iCityCount = iCityCount + 1.
            END.

            /* Populate the city combo box */
            IF cCityList <> "" THEN 
            DO:
                CMB-City:LIST-ITEMS IN FRAME Dialog-Frame = cCityList.
                
            /* Optional: Show count message */
            /* MESSAGE "Loaded" iCityCount "cities" VIEW-AS ALERT-BOX INFO BUTTONS OK. */
            END.
        END.
        ELSE 
        DO:
            MESSAGE "No cities found in database" VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        END.

        CATCH eError AS Progress.Lang.Error:
            MESSAGE "Error retrieving cities: " + eError:GetMessage(1)
                VIEW-AS ALERT-BOX ERROR.
        END CATCH.
    END.    
    FINALLY:
        /* Clean up */
        IF VALID-OBJECT(oController) THEN
            DELETE OBJECT oController NO-ERROR.
    END FINALLY.

END PROCEDURE.


PROCEDURE populate-states-by-country:
    /*------------------------------------------------------------------------
        Purpose: Populate state combo box filtered by selected country using class method
        Notes:   Uses Backend.Customer class method with ttStateDetails temp-table
      ----------------------------------------------------------------------*/
    
    DEFINE VARIABLE oController     AS Backend.Customer NO-UNDO.
    DEFINE VARIABLE selectedCountry AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE stateList       AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE lFound          AS LOGICAL          NO-UNDO.
    DEFINE VARIABLE iStateCount     AS INTEGER          NO-UNDO INITIAL 0.

    /* Get selected country name */
    selectedCountry = TRIM(CMB-Country:SCREEN-VALUE IN FRAME Dialog-Frame).
    
    /* Clear states and cities combo boxes */
    CMB-State:LIST-ITEMS IN FRAME Dialog-Frame = "".
    CMB-City:LIST-ITEMS IN FRAME Dialog-Frame = "".
    CMB-State:SCREEN-VALUE IN FRAME Dialog-Frame = "".
    CMB-City:SCREEN-VALUE IN FRAME Dialog-Frame = "".
    
    /* Validate country selection */
    IF selectedCountry = "" OR selectedCountry = ? THEN 
    DO:
        CMB-State:SENSITIVE IN FRAME Dialog-Frame = FALSE.
        RETURN.
    END.

    /* Clear previous temp-table data */
    EMPTY TEMP-TABLE ttStateDetails.

    /* Use class method to get states for selected country */
    DO ON ERROR UNDO, THROW:
        
        oController = NEW Backend.Customer().
        lFound = oController:GetStatesByCountryName(INPUT selectedCountry, 
            OUTPUT TABLE ttStateDetails).

        IF lFound THEN 
        DO:
            /* Build state list from temp-table (already sorted by name) */
            FOR EACH ttStateDetails:
                IF stateList = "" THEN
                    stateList = ttStateDetails.StateName.
                ELSE
                    stateList = stateList + "," + ttStateDetails.StateName.
                    
                iStateCount = iStateCount + 1.
            END.

            /* Populate states combo box */
            IF stateList <> "" THEN 
            DO:
                CMB-State:LIST-ITEMS IN FRAME Dialog-Frame = stateList.
                CMB-State:SENSITIVE IN FRAME Dialog-Frame = TRUE.
            END.
            ELSE 
            DO:
                MESSAGE "No states found for country:" selectedCountry 
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                CMB-State:SENSITIVE IN FRAME Dialog-Frame = FALSE.
            END.
        END.
        ELSE 
        DO:
            MESSAGE "Country not found:" selectedCountry 
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            CMB-State:SENSITIVE IN FRAME Dialog-Frame = FALSE.
        END.

        CATCH eError AS Progress.Lang.Error:
            MESSAGE "Error retrieving states: " + eError:GetMessage(1)
                VIEW-AS ALERT-BOX ERROR.
            CMB-State:SENSITIVE IN FRAME Dialog-Frame = FALSE.
        END CATCH.
    END.    
    FINALLY:
        /* Clean up */
        IF VALID-OBJECT(oController) THEN
            DELETE OBJECT oController NO-ERROR.
    END FINALLY.

END PROCEDURE.


PROCEDURE populate-cities-by-state:
    /*------------------------------------------------------------------------
        Purpose: Populate city combo box filtered by selected state using class method
        Notes:   Uses Backend.Customer class method with ttCities temp-table
      ----------------------------------------------------------------------*/
    
    DEFINE VARIABLE oController   AS Backend.Customer NO-UNDO.
    DEFINE VARIABLE selectedState AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cityList      AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE lFound        AS LOGICAL          NO-UNDO.
    DEFINE VARIABLE iCityCount    AS INTEGER          NO-UNDO INITIAL 0.

    /* Get selected state name */
    selectedState = TRIM(CMB-State:SCREEN-VALUE IN FRAME Dialog-Frame).
    
    /* Clear cities combo box */
    CMB-City:LIST-ITEMS IN FRAME Dialog-Frame = "".
    CMB-City:SCREEN-VALUE IN FRAME Dialog-Frame = "".
    
    /* Validate state selection */
    IF selectedState = "" OR selectedState = ? THEN 
    DO:
        CMB-City:SENSITIVE IN FRAME Dialog-Frame = FALSE.
        RETURN.
    END.

    /* Clear previous temp-table data */
    EMPTY TEMP-TABLE ttCities.

    /* Use class method to get cities for selected state */
    DO ON ERROR UNDO, THROW:
        
        oController = NEW Backend.Customer().
        lFound = oController:GetCitiesByStateName(INPUT selectedState, 
            OUTPUT TABLE ttCities).

        IF lFound THEN 
        DO:
            /* Build city list from temp-table (already sorted by name) */
            FOR EACH ttCities:
                IF cityList = "" THEN
                    cityList = ttCities.CityName.
                ELSE
                    cityList = cityList + "," + ttCities.CityName.
                    
                iCityCount = iCityCount + 1.
            END.

            /* Populate cities combo box */
            IF cityList <> "" THEN 
            DO:
                CMB-City:LIST-ITEMS IN FRAME Dialog-Frame = cityList.
                CMB-City:SENSITIVE IN FRAME Dialog-Frame = TRUE.
            END.
            ELSE 
            DO:
                MESSAGE "No cities found for state:" selectedState 
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                CMB-City:SENSITIVE IN FRAME Dialog-Frame = FALSE.
            END.
        END.
        ELSE 
        DO:
            MESSAGE "State not found:" selectedState 
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            CMB-City:SENSITIVE IN FRAME Dialog-Frame = FALSE.
        END.

        CATCH eError AS Progress.Lang.Error:
            MESSAGE "Error retrieving cities: " + eError:GetMessage(1)
                VIEW-AS ALERT-BOX ERROR.
            CMB-City:SENSITIVE IN FRAME Dialog-Frame = FALSE.
        END CATCH.
    END.    
    FINALLY:
        /* Clean up */
        IF VALID-OBJECT(oController) THEN
            DELETE OBJECT oController NO-ERROR.
    END FINALLY.

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
    MESSAGE "1".
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
    
    MESSAGE "2".
    /* Create controller and call update method */
    oCustomer = NEW Backend.Customer().
    lSuccess = oCustomer:UpdateCustomerDetails(INPUT TABLE ttCustomerDetails).
    MESSAGE lSuccess.
    
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

