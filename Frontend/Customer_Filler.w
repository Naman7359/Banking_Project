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

/* ***************************  Definitions  ************************** */
USING Progress.Json.ObjectModel.*.
USING Backend.Customer.

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER iCustID AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE TEMP-TABLE ttCustomer NO-UNDO
    FIELD CustID        AS INTEGER
    FIELD FirstName     AS CHARACTER
    FIELD LastName      AS CHARACTER
    FIELD Phone         AS CHARACTER
    FIELD Email         AS CHARACTER
    FIELD Address       AS CHARACTER
    FIELD City          AS CHARACTER
    FIELD State         AS CHARACTER
    FIELD Country       AS CHARACTER
    FIELD PostalCode    AS CHARACTER
    FIELD MaritalStatus AS CHARACTER
    FIELD CreatedDate   AS DATE
    FIELD ModifiedDate  AS DATE
    INDEX pkCustID IS PRIMARY UNIQUE CustID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW-CustomerDetails

/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 FLN-FirstName FLN-LastName ~
FLN-Phone FLN-Email BTN-Search BRW-CustomerDetails BTN-Okay BTN-Cancel 
&Scoped-Define DISPLAYED-OBJECTS FLN-FirstName FLN-LastName FLN-Phone ~
FLN-Email 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-Cancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BTN-Okay 
     LABEL "Okay" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BTN-Search 
     LABEL "Search" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE FLN-Email AS CHARACTER FORMAT "X(256)":U 
     LABEL "Email" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-FirstName AS CHARACTER FORMAT "X(256)":U 
     LABEL "First Name" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-LastName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Last Name" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-Phone AS CHARACTER FORMAT "X(256)":U 
     LABEL "Phone No," 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 10  NO-FILL   
     SIZE 59 BY 6.5.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 10  NO-FILL   
     SIZE 58 BY 5.75.


/* Browse definitions                                                   */
DEFINE BROWSE BRW-CustomerDetails
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW-CustomerDetails Dialog-Frame _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 52 BY 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FLN-FirstName AT ROW 4 COL 13 COLON-ALIGNED WIDGET-ID 4
     FLN-LastName AT ROW 4 COL 40 COLON-ALIGNED WIDGET-ID 6
     FLN-Phone AT ROW 6 COL 13 COLON-ALIGNED WIDGET-ID 8
     FLN-Email AT ROW 6 COL 40 COLON-ALIGNED WIDGET-ID 10
     BTN-Search AT ROW 7.5 COL 24.5 WIDGET-ID 14
     BRW-CustomerDetails AT ROW 11.75 COL 6 WIDGET-ID 200
     BTN-Okay AT ROW 17.25 COL 11 WIDGET-ID 22
     BTN-Cancel AT ROW 17.25 COL 37 WIDGET-ID 24
     "Customer Details" VIEW-AS TEXT
          SIZE 19 BY 1 AT ROW 9.75 COL 23 WIDGET-ID 16
          FONT 5
     "CUSTOMER FILTER" VIEW-AS TEXT
          SIZE 24 BY 1 AT ROW 1.5 COL 20 WIDGET-ID 2
          FONT 1
     RECT-1 AT ROW 3 COL 3 WIDGET-ID 12
     RECT-2 AT ROW 11 COL 3 WIDGET-ID 20
     SPACE(2.74) SKIP(2.12)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>" WIDGET-ID 100.


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
/* BROWSE-TAB BRW-CustomerDetails BTN-Search Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

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


&Scoped-define SELF-NAME BTN-Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Cancel Dialog-Frame
ON CHOOSE OF BTN-Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-Okay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Okay Dialog-Frame
ON CHOOSE OF BTN-Okay IN FRAME Dialog-Frame /* Okay */
DO:
      IF AVAILABLE ttCustomer THEN
        iCustID = ttCustomer.CustID.
    ELSE
        iCustID = 0.

    APPLY "CLOSE" TO THIS-PROCEDURE. /* return to Main.w */
END.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-Search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Search Dialog-Frame
ON CHOOSE OF BTN-Search IN FRAME Dialog-Frame /* Search */
DO:
    DEFINE VARIABLE oController AS Backend.Customer NO-UNDO.
    DEFINE VARIABLE lcResult    AS LONGCHAR          NO-UNDO.
    DEFINE VARIABLE oParser     AS ObjectModelParser NO-UNDO.
    DEFINE VARIABLE oArray      AS JsonArray         NO-UNDO.
    DEFINE VARIABLE oCust       AS JsonObject        NO-UNDO.
    DEFINE VARIABLE i           AS INTEGER           NO-UNDO.

    /* Clear previous browse records */
    EMPTY TEMP-TABLE ttCustomer.

    /* Collect input values from UI fields */
    DEFINE VARIABLE cFirstName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLastName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPhone     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEmail     AS CHARACTER NO-UNDO.

    ASSIGN
        cFirstName = FLN-FirstName:SCREEN-VALUE
        cLastName  = FLN-LastName:SCREEN-VALUE
        cPhone     = FLN-Phone:SCREEN-VALUE
        cEmail     = FLN-Email:SCREEN-VALUE.

    /* Call backend search method */
    oController = NEW Backend.Customer().
    lcResult    = oController:searchCustomers(
                        INPUT cFirstName,
                        INPUT cLastName,
                        INPUT cPhone,
                        INPUT cEmail).

    /* Parse JSON result into temp-table */
    oParser = NEW ObjectModelParser().
    oArray  = CAST(oParser:Parse(lcResult), JsonArray).

    DO i = 1 TO oArray:Length:
        oCust = oArray:GetJsonObject(i).

        CREATE ttCustomer.
        ASSIGN
            ttCustomer.CustID    = INTEGER(oCust:GetInteger("CustID"))
            ttCustomer.FirstName = oCust:GetCharacter("FirstName")
            ttCustomer.LastName  = oCust:GetCharacter("LastName")
            ttCustomer.Phone     = oCust:GetCharacter("Phone")
            ttCustomer.Email     = oCust:GetCharacter("Email").
    END.

    /* Refresh browse so user sees updated records */
    BROWSE BRW-CustomerDetails:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW-CustomerDetails
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

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
  DISPLAY FLN-FirstName FLN-LastName FLN-Phone FLN-Email 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 RECT-2 FLN-FirstName FLN-LastName FLN-Phone FLN-Email 
         BTN-Search BTN-Okay BTN-Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

