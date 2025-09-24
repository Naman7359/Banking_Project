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
DEFINE TEMP-TABLE ttAccount NO-UNDO
    FIELD AccountID     AS INTEGER
    FIELD CustID        AS INTEGER
    FIELD AccountType   AS CHARACTER  /* Saving / Loan */
    FIELD Balance       AS DECIMAL FORMAT "->,>>>,>>9.99" INITIAL 0
    FIELD CreatedDate   AS DATE
    FIELD cStatus       AS CHARACTER  /* Active / Closed */
    INDEX pkAccountID   IS PRIMARY UNIQUE AccountID
    INDEX ixCustID      CustID.
    
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

&Scoped-define WIDGETID-FILE-NAME 

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-15 FLN-WithdrawDate FLN-Amount ~
FLN-ToAccount CMB-Source FLN-TransactionId BTN-Okay BTN-Cancel 
&Scoped-Define DISPLAYED-OBJECTS FLN-WithdrawDate FLN-Amount FLN-ToAccount ~
CMB-Source FLN-TransactionId 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON BTN-Okay AUTO-GO 
     LABEL "Okay" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE VARIABLE CMB-Source AS CHARACTER FORMAT "X(256)":U 
     LABEL "Source" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "TO BANK" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-Amount AS CHARACTER FORMAT "X(256)":U 
     LABEL "Amount" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-ToAccount AS CHARACTER FORMAT "X(256)":U 
     LABEL "To Account" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-TransactionId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Transaction ID" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-WithdrawDate AS CHARACTER FORMAT "X(256)":U 
     LABEL "Withdraw Date" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 10  NO-FILL   
     SIZE 40 BY 8.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FLN-WithdrawDate AT ROW 3.5 COL 25 COLON-ALIGNED WIDGET-ID 4
     FLN-Amount AT ROW 5 COL 25 COLON-ALIGNED WIDGET-ID 6
     FLN-ToAccount AT ROW 6.5 COL 25 COLON-ALIGNED WIDGET-ID 8
     CMB-Source AT ROW 8 COL 23 COLON-ALIGNED WIDGET-ID 12
     FLN-TransactionId AT ROW 9.5 COL 25 COLON-ALIGNED WIDGET-ID 10
     BTN-Okay AT ROW 12 COL 10 WIDGET-ID 18
     BTN-Cancel AT ROW 12 COL 34 WIDGET-ID 16
     "WITHDRAW SCREEN" VIEW-AS TEXT
          SIZE 25 BY 1.5 AT ROW 1.25 COL 16 WIDGET-ID 2
          FONT 9
     RECT-15 AT ROW 3 COL 9 WIDGET-ID 14
     SPACE(9.37) SKIP(2.71)
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
                                                                        */
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
  DISPLAY FLN-WithdrawDate FLN-Amount FLN-ToAccount CMB-Source FLN-TransactionId 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-15 FLN-WithdrawDate FLN-Amount FLN-ToAccount CMB-Source 
         FLN-TransactionId BTN-Okay BTN-Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

