&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

USING Progress.Json.ObjectModel.*.
CREATE WIDGET-POOL.
/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE TEMP-TABLE ttCustomer NO-UNDO
    FIELD CustID        AS INTEGER
    FIELD FirstName     AS CHARACTER
    FIELD LastName      AS CHARACTER
    FIELD Mobile        AS CHARACTER
    FIELD Email         AS CHARACTER
    FIELD Address       AS CHARACTER
    FIELD Address2      AS CHARACTER
    FIELD City          AS CHARACTER
    FIELD State         AS CHARACTER
    FIELD Country       AS CHARACTER
    FIELD PostalCode    AS CHARACTER
    FIELD MaritalStatus AS CHARACTER
    INDEX pkCustID IS PRIMARY UNIQUE CustID.
    
    
DEFINE TEMP-TABLE ttAccount NO-UNDO 
    FIELD AcctNum        AS INTEGER 
    FIELD AccountTypeId  AS INTEGER 
    FIELD AccountType    AS CHARACTER 
    FIELD AccountSubType AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW-AccountInformation

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttAccount

/* Definitions for BROWSE BRW-AccountInformation                        */
&Scoped-define FIELDS-IN-QUERY-BRW-AccountInformation ttAccount   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW-AccountInformation   
&Scoped-define SELF-NAME BRW-AccountInformation
&Scoped-define QUERY-STRING-BRW-AccountInformation FOR EACH ttAccount
&Scoped-define OPEN-QUERY-BRW-AccountInformation OPEN QUERY {&SELF-NAME} FOR EACH ttAccount.
&Scoped-define TABLES-IN-QUERY-BRW-AccountInformation ttAccount
&Scoped-define FIRST-TABLE-IN-QUERY-BRW-AccountInformation ttAccount


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW-AccountInformation}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 RECT-4 RECT-5 RECT-6 RECT-7 ~
FLN-CustID BTN-Search BTN-AdvanceSearch BTN-Add BTN-Update BTN-Delete ~
TGL-SelectAll TGL-DeselectAll BTN-AddAccount BRW-AccountInformation ~
BTN-UpdateAccount BTN-DeleteAccount 
&Scoped-Define DISPLAYED-OBJECTS FLN-CustID FLN-FirstName FLN-LastName ~
CMB-MaritalStatus FLN-Address FLN-Address-2 FLN-City FLN-State FLN-Country ~
FLN-PostalCode TGL-SelectAll TGL-DeselectAll 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-Add 
    LABEL "Add" 
    SIZE 10 BY 1.13.

DEFINE BUTTON BTN-AddAccount 
    LABEL "Add Account" 
    SIZE 16 BY 1.13.

DEFINE BUTTON BTN-AdvanceSearch 
    LABEL "Advance Search" 
    SIZE 16 BY 1.

DEFINE BUTTON BTN-Delete 
    LABEL "Delete" 
    SIZE 10 BY 1.13.

DEFINE BUTTON BTN-DeleteAccount 
    LABEL "Delete Account" 
    SIZE 16 BY 1.13.

DEFINE BUTTON BTN-Search 
    LABEL "Search" 
    SIZE 14 BY 1.

DEFINE BUTTON BTN-Update 
    LABEL "Update" 
    SIZE 10 BY 1.25.

DEFINE BUTTON BTN-UpdateAccount 
    LABEL "Update Account" 
    SIZE 16 BY 1.13.

DEFINE VARIABLE CMB-MaritalStatus AS CHARACTER FORMAT "X(256)":U 
    LABEL "Marital Status" 
    VIEW-AS COMBO-BOX INNER-LINES 5
    LIST-ITEMS "Single","Married" 
    DROP-DOWN-LIST
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-Address       AS CHARACTER FORMAT "X(256)":U 
    LABEL "Address" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-Address-2     AS CHARACTER FORMAT "X(256)":U 
    LABEL "Address 2" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-City          AS CHARACTER FORMAT "X(256)":U 
    LABEL "City" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-Country       AS CHARACTER FORMAT "X(256)":U 
    LABEL "Country" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-CustID        AS INTEGER   FORMAT "->,>>>,>>9":U INITIAL 0 
    LABEL "Customer ID" 
    VIEW-AS FILL-IN 
    SIZE 30 BY 1 NO-UNDO.

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

DEFINE VARIABLE FLN-State         AS CHARACTER FORMAT "X(256)":U 
    LABEL "State" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-3
    EDGE-PIXELS 10  NO-FILL   
    SIZE 104 BY 2.

DEFINE RECTANGLE RECT-4
    EDGE-PIXELS 10  NO-FILL   
    SIZE 104 BY 9.53.

DEFINE RECTANGLE RECT-5
    EDGE-PIXELS 10  NO-FILL   
    SIZE 13 BY 6.53.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 10  NO-FILL   
    SIZE 104 BY 6.53.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 10  NO-FILL   
    SIZE 18.75 BY 4.75.

DEFINE VARIABLE TGL-DeselectAll AS LOGICAL INITIAL no 
    LABEL "Deselect All" 
    VIEW-AS TOGGLE-BOX
    SIZE 14 BY .75 NO-UNDO.

DEFINE VARIABLE TGL-SelectAll   AS LOGICAL INITIAL no 
    LABEL "Select All" 
    VIEW-AS TOGGLE-BOX
    SIZE 11.75 BY .75 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW-AccountInformation FOR 
    ttAccount SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW-AccountInformation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW-AccountInformation C-Win _FREEFORM
    QUERY BRW-AccountInformation DISPLAY
    ttAccount
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 74 BY 3.75 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
    FLN-CustID AT ROW 4 COL 29 COLON-ALIGNED WIDGET-ID 26
    BTN-Search AT ROW 4 COL 77 WIDGET-ID 28
    BTN-AdvanceSearch AT ROW 4 COL 95 WIDGET-ID 30
    FLN-FirstName AT ROW 8.25 COL 23 COLON-ALIGNED WIDGET-ID 34
    FLN-LastName AT ROW 8.25 COL 53 COLON-ALIGNED WIDGET-ID 42
    CMB-MaritalStatus AT ROW 8.25 COL 83 COLON-ALIGNED WIDGET-ID 50
    BTN-Add AT ROW 9.47 COL 101 WIDGET-ID 52
    FLN-Address AT ROW 10.25 COL 23 COLON-ALIGNED WIDGET-ID 36
    FLN-Address-2 AT ROW 10.25 COL 53 COLON-ALIGNED WIDGET-ID 44
    BTN-Update AT ROW 11.47 COL 101 WIDGET-ID 54
    FLN-City AT ROW 12.25 COL 23 COLON-ALIGNED WIDGET-ID 38
    FLN-State AT ROW 12.25 COL 53 COLON-ALIGNED WIDGET-ID 46
    BTN-Delete AT ROW 13.72 COL 101 WIDGET-ID 56
    FLN-Country AT ROW 14.25 COL 23 COLON-ALIGNED WIDGET-ID 40
    FLN-PostalCode AT ROW 14.25 COL 53 COLON-ALIGNED WIDGET-ID 48
    TGL-SelectAll AT ROW 18.75 COL 20 WIDGET-ID 70
    TGL-DeselectAll AT ROW 18.75 COL 34 WIDGET-ID 72
    BTN-AddAccount AT ROW 19.06 COL 96.38 WIDGET-ID 74
    BRW-AccountInformation AT ROW 20 COL 16.75 WIDGET-ID 200
    BTN-UpdateAccount AT ROW 20.56 COL 96.25 WIDGET-ID 76
    BTN-DeleteAccount AT ROW 22.06 COL 96.25 WIDGET-ID 78
    "CUSTOMER INFORMATION" VIEW-AS TEXT
    SIZE-PIXELS 264 BY 56 AT Y 8 X 382 WIDGET-ID 24
    FONT 1
    "Customer Details" VIEW-AS TEXT
    SIZE 19 BY 1 AT ROW 5.72 COL 56 WIDGET-ID 62
    FONT 5
    "Account Information" VIEW-AS TEXT
    SIZE 23 BY 1 AT ROW 16.75 COL 54 WIDGET-ID 66
    FONT 5
    RECT-3 AT ROW 3.53 COL 13 WIDGET-ID 32
    RECT-4 AT ROW 7.25 COL 13 WIDGET-ID 58
    RECT-5 AT ROW 8.94 COL 100 WIDGET-ID 60
    RECT-6 AT ROW 18 COL 13 WIDGET-ID 68
    RECT-7 AT ROW 18.81 COL 95.25 WIDGET-ID 80
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COLUMN 1 ROW 1
    SIZE 129.13 BY 24 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW C-Win ASSIGN
        HIDDEN             = YES
        TITLE              = "<insert window title>"
        HEIGHT             = 24
        WIDTH              = 129.25
        MAX-HEIGHT         = 24.56
        MAX-WIDTH          = 155.75
        VIRTUAL-HEIGHT     = 24.56
        VIRTUAL-WIDTH      = 155.75
        RESIZE             = yes
        SCROLL-BARS        = no
        STATUS-AREA        = no
        BGCOLOR            = ?
        FGCOLOR            = ?
        KEEP-FRAME-Z-ORDER = yes
        THREE-D            = yes
        MESSAGE-AREA       = no
        SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW-AccountInformation BTN-AddAccount DEFAULT-FRAME */
/* SETTINGS FOR COMBO-BOX CMB-MaritalStatus IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FLN-Address IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FLN-Address-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FLN-City IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FLN-Country IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FLN-FirstName IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FLN-LastName IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FLN-PostalCode IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FLN-State IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW-AccountInformation
/* Query rebuild information for BROWSE BRW-AccountInformation
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttAccount.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BRW-AccountInformation */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Add C-Win
ON CHOOSE OF BTN-Add IN FRAME DEFAULT-FRAME /* Add */
    DO:
        RUN Add_Update_Customer.w (INPUT 0, INPUT "Add").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-AddAccount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-AddAccount C-Win
ON CHOOSE OF BTN-AddAccount IN FRAME DEFAULT-FRAME /* Add Account */
    DO:
        DEFINE VARIABLE iCustID    AS INTEGER NO-UNDO.
        DEFINE VARIABLE iAccountID AS INTEGER NO-UNDO.

        /* Get selected customer */
        iCustID = INTEGER(FLN-CustID:SCREEN-VALUE).

        IF iCustID = 0 THEN 
        DO:
            MESSAGE "Please select a customer before adding account." VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.

        /* Run dialog */
        RUN Add_Account.w(
            INPUT iCustID,    
            OUTPUT iAccountID,    /* customer ID */
            INPUT "ADD"     /* newly created account ID */
            ).

        /* Refresh Accounts browse if an account was created */
        IF iAccountID > 0 THEN 
        DO:
            BROWSE BRW-AccountInformation:REFRESH().
        END.  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-AdvanceSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-AdvanceSearch C-Win
ON CHOOSE OF BTN-AdvanceSearch IN FRAME DEFAULT-FRAME /* Advance Search */
    DO:
        DEFINE VARIABLE iCustID AS INTEGER NO-UNDO.

        /* Open Customer Filter dialog and get selected CustID */
        RUN Customer_Filler.w (OUTPUT iCustID).  

        /* If a customer was selected, populate fields and trigger Search */
        IF iCustID <> 0 THEN 
        DO:
            FLN-CustID:SCREEN-VALUE = STRING(iCustID).

            /* Trigger Search button logic to fill main form fields */
            APPLY "CHOOSE" TO BTN-Search IN FRAME DEFAULT-FRAME.

        /* Load Orders and Order Lines for the selected customer */
        /*        RUN Load_Orders(iCustID).*/
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Delete C-Win
ON CHOOSE OF BTN-Delete IN FRAME DEFAULT-FRAME /* Delete */
    DO:
        DEFINE VARIABLE iSelectedCustomerId AS INTEGER NO-UNDO.
        DEFINE VARIABLE lDeleteResult       AS LOGICAL NO-UNDO.

        /* Get selected customer ID from UI */
        iSelectedCustomerId = INTEGER(FLN-CustID:SCREEN-VALUE IN FRAME DEFAULT-FRAME) NO-ERROR.
    
        /* Validate customer ID */
        IF iSelectedCustomerId <= 0 THEN 
        DO:
            MESSAGE "Please select a valid customer to delete!"
                VIEW-AS ALERT-BOX WARNING BUTTONS OK.
            RETURN.
        END.

        /* Call deletion procedure with UI refresh */
        RUN DeleteCustomerWithRefresh(INPUT iSelectedCustomerId, 
            OUTPUT lDeleteResult).
    
        /* Optional: Additional processing based on result */
        IF NOT lDeleteResult THEN 
        DO:
            /* Handle deletion failure if needed */
            MESSAGE "Customer deletion was not completed."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END. 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-Search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Search C-Win
ON CHOOSE OF BTN-Search IN FRAME DEFAULT-FRAME /* Search */
    DO:
        RUN Search_Customer.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-Update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Update C-Win
ON CHOOSE OF BTN-Update IN FRAME DEFAULT-FRAME /* Update */
    DO:
        RUN Add_Update_Customer.w (INPUT 0, INPUT "Update").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-UpdateAccount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-UpdateAccount C-Win
ON CHOOSE OF BTN-UpdateAccount IN FRAME DEFAULT-FRAME /* Update Account */
    DO:
        DEFINE VARIABLE iSelectedAccountID AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cAction            AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iAccountId         AS INTEGER   NO-UNDO.
        /* Make sure a row is selected in the browse (ttAccount is query buffer) */
        IF AVAILABLE ttAccount THEN 
        DO:
            /* Get selected AccountID */
            iSelectedAccountID = ttAccount.AcctNum.

            /* Set action to UPDATE */
            cAction = "UPDATE".

            /* Open Add_Account dialog in UPDATE mode, passing AccountID and cAction */
            RUN Add_Account.w (
                INPUT iSelectedAccountID,
                OUTPUT iAccountId,
                INPUT cAction
                ).
        END.
        ELSE 
        DO:
            MESSAGE "Please select an account to update." VIEW-AS ALERT-BOX INFO.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW-AccountInformation
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
    RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.




PROCEDURE DeleteCustomerById:
    /*------------------------------------------------------------------------
        Purpose     : Delete a customer by ID
        Parameters  : INPUT customer ID, OUTPUT success indicator  
        Notes       : Pure deletion logic without UI dependencies
      ----------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER piCustomerId    AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER plDeleteSuccess AS LOGICAL NO-UNDO INITIAL FALSE.
    
    DEFINE VARIABLE oController AS backend.Customer NO-UNDO.
    
    /* Validate input parameter */
    IF piCustomerId <= 0 THEN 
    DO:
        MESSAGE "Invalid Customer ID: " + STRING(piCustomerId)
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN.
    END.
    
    /* Main deletion logic */
    DO ON ERROR UNDO, THROW:
        
        oController = NEW backend.Customer().
        plDeleteSuccess = oController:DeleteCustomer(piCustomerId).
    END.
    CATCH eError AS Progress.Lang.Error:
        MESSAGE "Error deleting customer: " + eError:GetMessage(1)
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        plDeleteSuccess = FALSE.
    END CATCH.
    
    FINALLY:
        IF VALID-OBJECT(oController) THEN
            DELETE OBJECT oController NO-ERROR.
    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
    /*------------------------------------------------------------------------------
      Purpose:     DISABLE the User Interface
      Parameters:  <none>
      Notes:       Here we clean-up the user-interface by deleting
                   dynamic widgets we have created and/or hide 
                   frames.  This procedure is usually called when
                   we are ready to "clean-up" after running.
    ------------------------------------------------------------------------------*/
    /* Delete the WINDOW we created */
    IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
        THEN DELETE WIDGET C-Win.
    IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
    DISPLAY FLN-CustID FLN-FirstName FLN-LastName CMB-MaritalStatus FLN-Address 
        FLN-Address-2 FLN-City FLN-State FLN-Country FLN-PostalCode 
        TGL-SelectAll TGL-DeselectAll 
        WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
    ENABLE RECT-3 RECT-4 RECT-5 RECT-6 RECT-7 FLN-CustID BTN-Search 
        BTN-AdvanceSearch BTN-Add BTN-Update BTN-Delete TGL-SelectAll 
        TGL-DeselectAll BTN-AddAccount BRW-AccountInformation 
        BTN-UpdateAccount BTN-DeleteAccount 
        WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillCustomerData C-Win 
PROCEDURE FillCustomerData :
    /*------------------------------------------------------------------------------
             Purpose:
             Notes:
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER lcInputData AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE oParser     AS ObjectModelParser NO-UNDO.
    DEFINE VARIABLE oJson       AS JsonObject        NO-UNDO.
    DEFINE VARIABLE oAcctsArray AS JsonArray         NO-UNDO.
    DEFINE VARIABLE i           AS INTEGER           NO-UNDO.
    DEFINE VARIABLE oAccount    AS JsonObject        NO-UNDO.

    DO ON ERROR UNDO, THROW :
        
        /* 1. Parse the Input Data (Customer Details and Accounts) */
        oParser = NEW ObjectModelParser().
        oJson = CAST(oParser:Parse(lcInputData), JsonObject) NO-ERROR.
        
        IF ERROR-STATUS:NUM-MESSAGES > 0 OR NOT VALID-OBJECT(oJson) THEN
        DO:
            UNDO, THROW NEW Progress.Lang.AppError("Failed to parse customer data response into JSON object.", 0).
        END.
        
        /* 2. Populate Customer Fill-in Widgets (Customer Details) */
        Assign
            FLN-CustID:SCREEN-VALUE IN FRAME DEFAULT-FRAME      = STRING(oJson:GetInteger("CustID")) 
            FLN-FirstName:SCREEN-VALUE                          = oJson:GetCharacter("FirstName") 
            FLN-LastName:SCREEN-VALUE                           = oJson:GetCharacter("LastName") 
            FLN-Address:SCREEN-VALUE                            = oJson:GetCharacter("Address") 
            FLN-Address-2:SCREEN-VALUE                          = oJson:GetCharacter("Address2")
            FLN-City:SCREEN-VALUE                               = oJson:GetCharacter("City") 
            FLN-State:SCREEN-VALUE                              = oJson:GetCharacter("State") 
            FLN-Country:SCREEN-VALUE                            = oJson:GetCharacter("Country") 
            FLN-PostalCode:SCREEN-VALUE                         = STRING(oJson:GetInteger("PostalCode")) 
            CMB-MaritalStatus:LIST-ITEMS in FRAME DEFAULT-FRAME = oJson:GetCharacter("MaritalStatus")
            CMB-MaritalStatus:SCREEN-VALUE                      = oJson:GetCharacter("MaritalStatus") .

        /* 3. Load Accounts into Temp-Table (ttAccount) */
        
        /* Clear previous data */
        EMPTY TEMP-TABLE ttAccount.
        QUERY BRW-AccountInformation:QUERY-CLOSE ().
        /* Get the Accounts array */
        oAcctsArray = CAST(oJson:GetJsonArray("Accounts"), JsonArray) NO-ERROR.
        /*        MESSAGE STRING (oAcctsArray) .*/

        /* Iterate through the array and create records in ttAccount */
        IF oAcctsArray:Length > 0 THEN 
        DO:
            DO i = 1 TO oAcctsArray:Length:
                oAccount = CAST(oAcctsArray:GetJsonObject(i), JsonObject).

                CREATE ttAccount.
                ASSIGN
                    ttAccount.AcctNum       = oAccount:GetInteger("AcctNum")
                    ttAccount.AccountTypeId = oAccount:GetInteger("AccountTypeId")
                    ttAccount.AccountType   = oAccount:GetCharacter("AccountType") NO-ERROR.
                ttAccount.AccountSubType = oAccount:GetCharacter("AccountSubType") NO-ERROR.

                DELETE OBJECT oAccount NO-ERROR. /* Cleanup object inside loop */
            END.
            
            QUERY BRW-AccountInformation:QUERY-PREPARE("FOR EACH ttAccount NO-LOCK").
            QUERY BRW-AccountInformation:QUERY-OPEN().
            
        END.
        ElSE 
        DO :
            MESSAGE "No Accounts Available." VIEW-AS ALERT-BOX INFORMATION.
        END.
        
    END.
    
    CATCH e AS Progress.Lang.Error:
        MESSAGE "Error populating customer data: " + e:GetMessage(1) SKIP 
            "Stack: " + e:CallStack
            VIEW-AS ALERT-BOX ERROR TITLE "Data Loading Failure".
    END CATCH.

    FINALLY:
        DELETE OBJECT oJson NO-ERROR.
        DELETE OBJECT oParser NO-ERROR.
            
    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Load_Orders C-Win 
PROCEDURE Load_Orders :
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Search_Customer C-Win 
PROCEDURE Search_Customer :
    /*------------------------------------------------------------------------------
             Purpose:
             Notes:
            ------------------------------------------------------------------------------*/

    DEFINE VARIABLE oCustomer AS Backend.Customer NO-UNDO.
    DEFINE VARIABLE lcRes     AS LONGCHAR         NO-UNDO.
    DEFINE VARIABLE lcData    AS LONGCHAR         NO-UNDO.
    DO ON ERROR UNDO, THROW :
        If(
            FLN-CustID:SCREEN-VALUE IN FRAME DEFAULT-FRAME <> "0" AND 
            FLN-CustID:SCREEN-VALUE IN FRAME DEFAULT-FRAME <> "" AND 
            FLN-CustID:SCREEN-VALUE IN FRAME DEFAULT-FRAME <> ?
            ) THEN 
        DO:
            
            oCustomer    = NEW Backend.Customer().
            lcRes = oCustomer:getCustomerWithAccounts( INPUT INTEGER(FLN-CustID:SCREEN-VALUE IN FRAME DEFAULT-FRAME)).
            lcData = Frontend.Utility.Utility:ReadResponse(lcRes).
            RUN FillCustomerData ( INPUT lcData).
        /*            MESSAGE STRING(lcData).*/
        end. 
        ELSE 
        DO:
            MESSAGE "Enter Customer ID " VIEW-AS ALERT-BOX INFORMATION.
        END.
    END. 
    CATCH e AS Progress.Lang.Error:
        Message "Error while Search Customer: " + e:GetMessage(1) View-as ALERT-BOX ERROR.
    END CATCH.  
    FINALLY:
        DELETE OBJECT oCustomer NO-ERROR. 
    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

