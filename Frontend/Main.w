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
    FIELD CreatedDate   AS DATE
    FIELD ModifiedDate  AS DATE
    INDEX pkCustID IS PRIMARY UNIQUE CustID.
    
    
DEFINE TEMP-TABLE ttAccount NO-UNDO
    FIELD AccountID   AS INTEGER
    FIELD CustID      AS INTEGER
    FIELD AccountType AS CHARACTER  /* Saving / Loan */
    FIELD Balance     AS DECIMAL
    FIELD CreatedDate AS DATE
    FIELD cStatus     AS CHARACTER  /* Active / Closed */
    INDEX pkAccountID IS PRIMARY UNIQUE AccountID
    INDEX ixCustID                      CustID.

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
&Scoped-define INTERNAL-TABLES ttCustomer

/* Definitions for BROWSE BRW-AccountInformation                        */
&Scoped-define FIELDS-IN-QUERY-BRW-AccountInformation   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW-AccountInformation   
&Scoped-define SELF-NAME BRW-AccountInformation
&Scoped-define QUERY-STRING-BRW-AccountInformation FOR EACH ttCustomer
&Scoped-define OPEN-QUERY-BRW-AccountInformation OPEN QUERY {&SELF-NAME} FOR EACH ttCustomer.
&Scoped-define TABLES-IN-QUERY-BRW-AccountInformation ttCustomer
&Scoped-define FIRST-TABLE-IN-QUERY-BRW-AccountInformation ttCustomer


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW-AccountInformation}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 RECT-4 RECT-5 RECT-6 RECT-7 ~
FLN-CustID BTN-Search BTN-AdvanceSearch FLN-FirstName FLN-LastName ~
CMB-MaritalStatus BTN-Add FLN-Address FLN-Address-2 BTN-Update FLN-City ~
FLN-State BTN-Delete FLN-Country FLN-PostalCode TGL-SelectAll ~
TGL-DeselectAll BTN-AddAccount BRW-AccountInformation BTN-UpdateAccount ~
BTN-DeleteAccount 
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

DEFINE VARIABLE FLN-Address AS CHARACTER FORMAT "X(256)":U 
     LABEL "Address" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-Address-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Address 2" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-City AS CHARACTER FORMAT "X(256)":U 
     LABEL "City" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-Country AS CHARACTER FORMAT "X(256)":U 
     LABEL "Country" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-CustID AS CHARACTER FORMAT "X(256)":U 
     LABEL "Customer ID" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-FirstName AS CHARACTER FORMAT "X(256)":U 
     LABEL "First Name" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-LastName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Last Name" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-PostalCode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Postal Code" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-State AS CHARACTER FORMAT "X(256)":U 
     LABEL "State" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 10  NO-FILL   
     SIZE 104 BY 2.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 10  NO-FILL   
     SIZE 104 BY 9.5.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 10  NO-FILL   
     SIZE 13 BY 6.5.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 10  NO-FILL   
     SIZE 104 BY 6.5.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 10  NO-FILL   
     SIZE 18.75 BY 4.75.

DEFINE VARIABLE TGL-DeselectAll AS LOGICAL INITIAL no 
     LABEL "Deselect All" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .75 NO-UNDO.

DEFINE VARIABLE TGL-SelectAll AS LOGICAL INITIAL no 
     LABEL "Select All" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.88 BY .75 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW-AccountInformation FOR 
      ttCustomer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW-AccountInformation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW-AccountInformation C-Win _FREEFORM
  QUERY BRW-AccountInformation DISPLAY
      
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 74 BY 3.75 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FLN-CustID AT ROW 4 COL 29 COLON-ALIGNED WIDGET-ID 26
     BTN-Search AT ROW 4 COL 77 WIDGET-ID 28
     BTN-AdvanceSearch AT ROW 4 COL 95 WIDGET-ID 30
     FLN-FirstName AT ROW 8.22 COL 23 COLON-ALIGNED WIDGET-ID 34
     FLN-LastName AT ROW 8.22 COL 53 COLON-ALIGNED WIDGET-ID 42
     CMB-MaritalStatus AT ROW 8.22 COL 83 COLON-ALIGNED WIDGET-ID 50
     BTN-Add AT ROW 9.47 COL 101 WIDGET-ID 52
     FLN-Address AT ROW 10.22 COL 23 COLON-ALIGNED WIDGET-ID 36
     FLN-Address-2 AT ROW 10.25 COL 53 COLON-ALIGNED WIDGET-ID 44
     BTN-Update AT ROW 11.47 COL 101 WIDGET-ID 54
     FLN-City AT ROW 12.22 COL 23 COLON-ALIGNED WIDGET-ID 38
     FLN-State AT ROW 12.22 COL 53 COLON-ALIGNED WIDGET-ID 46
     BTN-Delete AT ROW 13.72 COL 101 WIDGET-ID 56
     FLN-Country AT ROW 14.22 COL 23 COLON-ALIGNED WIDGET-ID 40
     FLN-PostalCode AT ROW 14.22 COL 53 COLON-ALIGNED WIDGET-ID 48
     TGL-SelectAll AT ROW 18.75 COL 20 WIDGET-ID 70
     TGL-DeselectAll AT ROW 18.75 COL 34 WIDGET-ID 72
     BTN-AddAccount AT ROW 19.03 COL 96.38 WIDGET-ID 74
     BRW-AccountInformation AT ROW 20 COL 16.88 WIDGET-ID 200
     BTN-UpdateAccount AT ROW 20.56 COL 96.13 WIDGET-ID 76
     BTN-DeleteAccount AT ROW 22.03 COL 96.13 WIDGET-ID 78
     "Account Information" VIEW-AS TEXT
          SIZE 23 BY 1 AT ROW 16.75 COL 54 WIDGET-ID 66
          FONT 5
     "CUSTOMER INFORMATION" VIEW-AS TEXT
          SIZE-PIXELS 264 BY 56 AT Y 8 X 386 WIDGET-ID 24
          FONT 9
     "Customer Details" VIEW-AS TEXT
          SIZE 19 BY 1 AT ROW 5.72 COL 56 WIDGET-ID 62
          FONT 5
     RECT-3 AT ROW 3.5 COL 13 WIDGET-ID 32
     RECT-4 AT ROW 7 COL 12 WIDGET-ID 58
     RECT-5 AT ROW 8.97 COL 100 WIDGET-ID 60
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
         WIDTH              = 129.13
         MAX-HEIGHT         = 24.59
         MAX-WIDTH          = 155.75
         VIRTUAL-HEIGHT     = 24.59
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW-AccountInformation
/* Query rebuild information for BROWSE BRW-AccountInformation
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttCustomer.
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


&Scoped-define SELF-NAME BTN-AddAccount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-AddAccount C-Win
ON CHOOSE OF BTN-AddAccount IN FRAME DEFAULT-FRAME /* Add Account */
DO:
DEFINE VARIABLE iCustID AS INTEGER NO-UNDO.
    DEFINE VARIABLE iAccountID AS INTEGER NO-UNDO.

    /* Get selected customer */
    iCustID = INTEGER(FLN-CustID:SCREEN-VALUE).

    IF iCustID = 0 THEN DO:
        MESSAGE "Please select a customer before adding account." VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    /* Run dialog */
    RUN Add_Account.w (
        INPUT iCustID,        /* customer ID */
        OUTPUT iAccountID     /* newly created account ID */
    ).

    /* Refresh Accounts browse if an account was created */
    IF iAccountID > 0 THEN DO:
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

    /* Run Customer Filler dialog for search */
    RUN Customer_Filler.w (
        OUTPUT iCustID
    ).

    /* If a customer was selected, fill the Customer ID and trigger search */
    IF iCustID > 0 THEN DO:
        FLN-CustID:SCREEN-VALUE = STRING(iCustID).
        APPLY "CHOOSE" TO BTN-Search.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-Search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Search C-Win
ON CHOOSE OF BTN-Search IN FRAME DEFAULT-FRAME /* Search */
DO:
        EMPTY TEMP-TABLE ttCustomer.
        EMPTY TEMP-TABLE ttAccount.

        /* Lookup customer by CustID */
        IF AVAILABLE ttCustomer THEN 
        DO:
            /* Populate UI fields */
            FLN-FirstName:SCREEN-VALUE       = ttCustomer.FirstName.
            FLN-LastName:SCREEN-VALUE        = ttCustomer.LastName.
            FLN-Address-2:SCREEN-VALUE       = ttCustomer.Address2.   /* <─ New */
            FLN-Address:SCREEN-VALUE         = ttCustomer.Address.
            FLN-City:SCREEN-VALUE            = ttCustomer.City.
            FLN-State:SCREEN-VALUE           = ttCustomer.State.
            FLN-Country:SCREEN-VALUE         = ttCustomer.Country.
            FLN-PostalCode:SCREEN-VALUE      = ttCustomer.PostalCode.
            CMB-MaritalStatus:SCREEN-VALUE   = ttCustomer.MaritalStatus.

            /* Load accounts */
            RUN Load_Accounts.p (INPUT ttCustomer.CustID, OUTPUT TABLE ttAccount).

            BROWSE BRW-AccountInformation:REFRESH().
        END.
        ELSE 
        DO:
            MESSAGE "Customer not found" VIEW-AS ALERT-BOX INFO.
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
         BTN-AdvanceSearch FLN-FirstName FLN-LastName CMB-MaritalStatus BTN-Add 
         FLN-Address FLN-Address-2 BTN-Update FLN-City FLN-State BTN-Delete 
         FLN-Country FLN-PostalCode TGL-SelectAll TGL-DeselectAll 
         BTN-AddAccount BRW-AccountInformation BTN-UpdateAccount 
         BTN-DeleteAccount 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

