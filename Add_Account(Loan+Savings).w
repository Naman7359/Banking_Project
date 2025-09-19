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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

&Scoped-define WIDGETID-FILE-NAME 

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-Cancel-Loan 
     LABEL "Cancel" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BTN-Create-Loan 
     LABEL "Create" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE CMB-AccountSubType-Loan AS CHARACTER FORMAT "X(256)":U 
     LABEL "Account Sub Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE CMB-AccountType-Loan AS CHARACTER FORMAT "X(256)":U 
     LABEL "Account Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Loan" 
     DROP-DOWN-LIST
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE CMB-LoanDuration AS CHARACTER FORMAT "X(256)":U 
     LABEL "Loan Duration (yrs)" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE FLN-IFSC-Code-Loan AS CHARACTER FORMAT "X(256)":U 
     LABEL "IFSC Code" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-RateOfIntrest AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rate of Intrest (%)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-TotalLoanAmmount AS CHARACTER FORMAT "X(256)":U 
     LABEL "Total Loan" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 10  NO-FILL   
     SIZE 75 BY 6.5.

DEFINE BUTTON BTN-Cancel-Savings 
     LABEL "Cancel" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BTN-Create-Savings 
     LABEL "Create" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE CMB-AccountSubType-Savings AS CHARACTER FORMAT "X(256)":U 
     LABEL "Account Sub Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE CMB-AccountType-Savings AS CHARACTER FORMAT "X(256)":U 
     LABEL "Account Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Savings" 
     DROP-DOWN-LIST
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE FLN-IFSC-Code-Savings AS CHARACTER FORMAT "X(256)":U 
     LABEL "IFSC Code" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-TransferLimit AS CHARACTER FORMAT "X(256)":U 
     LABEL "Transfer Limit" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 10  NO-FILL   
     SIZE 74 BY 5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     SPACE(88.50) SKIP(20.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>" WIDGET-ID 100.

DEFINE FRAME FMB-LoanAccount
     CMB-AccountType-Loan AT ROW 2 COL 17.25 COLON-ALIGNED WIDGET-ID 16
     CMB-AccountSubType-Loan AT ROW 2 COL 55.25 COLON-ALIGNED WIDGET-ID 18
     FLN-IFSC-Code-Loan AT ROW 4 COL 17.25 COLON-ALIGNED WIDGET-ID 20
     CMB-LoanDuration AT ROW 4 COL 55.25 COLON-ALIGNED WIDGET-ID 22
     FLN-TotalLoanAmmount AT ROW 6 COL 17 COLON-ALIGNED WIDGET-ID 24
     FLN-RateOfIntrest AT ROW 6 COL 57.25 COLON-ALIGNED WIDGET-ID 26
     BTN-Create-Loan AT ROW 8.25 COL 19 WIDGET-ID 30
     BTN-Cancel-Loan AT ROW 8.25 COL 46 WIDGET-ID 32
     RECT-13 AT ROW 1.5 COL 3 WIDGET-ID 28
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COLUMN 5 ROW 11
         SIZE 80 BY 9.5
         TITLE "Add Loan Account" WIDGET-ID 300.

DEFINE FRAME FRM-SavingAccount
     CMB-AccountType-Savings AT ROW 2 COL 17 COLON-ALIGNED WIDGET-ID 2
     CMB-AccountSubType-Savings AT ROW 2 COL 55 COLON-ALIGNED WIDGET-ID 4
     FLN-IFSC-Code-Savings AT ROW 4 COL 17 COLON-ALIGNED WIDGET-ID 6
     FLN-TransferLimit AT ROW 4 COL 55 COLON-ALIGNED WIDGET-ID 8
     BTN-Create-Savings AT ROW 7 COL 20 WIDGET-ID 12
     BTN-Cancel-Savings AT ROW 7 COL 46 WIDGET-ID 14
     RECT-12 AT ROW 1.5 COL 4 WIDGET-ID 10
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COLUMN 5 ROW 2
         SIZE 80 BY 8.25
         TITLE "Add Savings Account" WIDGET-ID 200.


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
/* REPARENT FRAME */
ASSIGN FRAME FMB-LoanAccount:FRAME = FRAME Dialog-Frame:HANDLE
       FRAME FRM-SavingAccount:FRAME = FRAME Dialog-Frame:HANDLE.

/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRM-SavingAccount:MOVE-BEFORE-TAB-ITEM (FRAME FMB-LoanAccount:HANDLE)
/* END-ASSIGN-TABS */.

ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FMB-LoanAccount
                                                                        */
/* SETTINGS FOR COMBO-BOX CMB-AccountType-Loan IN FRAME FMB-LoanAccount
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRM-SavingAccount
                                                                        */
/* SETTINGS FOR COMBO-BOX CMB-AccountType-Savings IN FRAME FRM-SavingAccount
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
  HIDE FRAME FMB-LoanAccount.
  HIDE FRAME FRM-SavingAccount.
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
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
  DISPLAY CMB-AccountType-Savings CMB-AccountSubType-Savings 
          FLN-IFSC-Code-Savings FLN-TransferLimit 
      WITH FRAME FRM-SavingAccount.
  ENABLE RECT-12 CMB-AccountSubType-Savings FLN-IFSC-Code-Savings 
         FLN-TransferLimit BTN-Create-Savings BTN-Cancel-Savings 
      WITH FRAME FRM-SavingAccount.
  {&OPEN-BROWSERS-IN-QUERY-FRM-SavingAccount}
  DISPLAY CMB-AccountType-Loan CMB-AccountSubType-Loan FLN-IFSC-Code-Loan 
          CMB-LoanDuration FLN-TotalLoanAmmount FLN-RateOfIntrest 
      WITH FRAME FMB-LoanAccount.
  ENABLE RECT-13 CMB-AccountSubType-Loan FLN-IFSC-Code-Loan CMB-LoanDuration 
         FLN-TotalLoanAmmount FLN-RateOfIntrest BTN-Create-Loan BTN-Cancel-Loan 
      WITH FRAME FMB-LoanAccount.
  {&OPEN-BROWSERS-IN-QUERY-FMB-LoanAccount}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

