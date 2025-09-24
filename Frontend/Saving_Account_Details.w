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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE TEMP-TABLE ttSavingAccount NO-UNDO
    FIELD AccountID     AS INTEGER
    FIELD MinBalance    AS DECIMAL
    INDEX pkSavingAcct  IS PRIMARY UNIQUE AccountID.
    
DEFINE TEMP-TABLE ttTransaction NO-UNDO
    FIELD TxnID         AS INTEGER
    FIELD AccountID     AS INTEGER
    FIELD TxnType       AS CHARACTER   /* Deposit / Withdraw / EMI */
    FIELD Amount        AS DECIMAL
    FIELD TxnDate       AS DATE
    FIELD Remarks       AS CHARACTER
    INDEX pkTxnID       IS PRIMARY UNIQUE TxnID
    INDEX ixAccountID   AccountID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW-SavingsAccountDetails

/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-14 FLN-Account FLN-Balance ~
FLN-TransferLimit FLN-IFSC-Code BTN-Deposit BTN-Withdraw BTN-Report ~
CMB-Filter BRW-SavingsAccountDetails 
&Scoped-Define DISPLAYED-OBJECTS FLN-Account FLN-Balance FLN-TransferLimit ~
FLN-IFSC-Code CMB-AccountType CMB-Filter 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-Deposit AUTO-GO 
     LABEL "Deposit" 
     SIZE 10 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON BTN-Report 
     LABEL "Report" 
     SIZE 10 BY 1.13.

DEFINE BUTTON BTN-Withdraw AUTO-END-KEY 
     LABEL "Withdraw" 
     SIZE 10 BY 1.13
     BGCOLOR 8 .

DEFINE VARIABLE CMB-AccountType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Account Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Savings" 
     DROP-DOWN-LIST
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE CMB-Filter AS CHARACTER FORMAT "X(256)":U 
     LABEL "Filter" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-Account AS CHARACTER FORMAT "X(256)":U 
     LABEL "Account" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-Balance AS CHARACTER FORMAT "X(256)":U 
     LABEL "Balance" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-IFSC-Code AS CHARACTER FORMAT "X(256)":U 
     LABEL "IFSC Code" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-TransferLimit AS CHARACTER FORMAT "X(256)":U 
     LABEL "Transfer Limit" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 10  NO-FILL   
     SIZE 69 BY 8.75.


/* Browse definitions                                                   */
DEFINE BROWSE BRW-SavingsAccountDetails
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW-SavingsAccountDetails Dialog-Frame _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 68 BY 6 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FLN-Account AT ROW 4 COL 17 COLON-ALIGNED WIDGET-ID 4
     FLN-Balance AT ROW 4 COL 45 COLON-ALIGNED WIDGET-ID 6
     FLN-TransferLimit AT ROW 6 COL 17 COLON-ALIGNED WIDGET-ID 8
     FLN-IFSC-Code AT ROW 6 COL 45 COLON-ALIGNED WIDGET-ID 10
     CMB-AccountType AT ROW 8 COL 18 COLON-ALIGNED WIDGET-ID 12
     BTN-Deposit AT ROW 10 COL 23
     BTN-Withdraw AT ROW 10 COL 43
     BTN-Report AT ROW 12.25 COL 61.63 WIDGET-ID 18
     CMB-Filter AT ROW 12.5 COL 7 COLON-ALIGNED WIDGET-ID 16
     BRW-SavingsAccountDetails AT ROW 14 COL 4 WIDGET-ID 200
     "SAVING ACCOUNT DETAILS" VIEW-AS TEXT
          SIZE 33 BY 1.75 AT ROW 1.25 COL 23 WIDGET-ID 2
          FONT 1
     RECT-14 AT ROW 3 COL 3 WIDGET-ID 14
     SPACE(2.49) SKIP(8.68)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>"
         DEFAULT-BUTTON BTN-Deposit CANCEL-BUTTON BTN-Withdraw WIDGET-ID 100.


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
/* BROWSE-TAB BRW-SavingsAccountDetails CMB-Filter Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB-AccountType IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
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


&Scoped-define BROWSE-NAME BRW-SavingsAccountDetails
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
  DISPLAY FLN-Account FLN-Balance FLN-TransferLimit FLN-IFSC-Code 
          CMB-AccountType CMB-Filter 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-14 FLN-Account FLN-Balance FLN-TransferLimit FLN-IFSC-Code 
         BTN-Deposit BTN-Withdraw BTN-Report CMB-Filter 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

