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
DEFINE INPUT  PARAMETER iCustID    AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER iAccountID AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE TEMP-TABLE ttAccount NO-UNDO
    FIELD AccountID   AS INTEGER
    FIELD CustID      AS INTEGER
    FIELD AccountType AS CHARACTER  /* Saving / Loan */
    FIELD Balance     AS DECIMAL
    FIELD CreatedDate AS DATE
    FIELD cStatus     AS CHARACTER  /* Active / Closed */
    INDEX pkAccountID IS PRIMARY UNIQUE AccountID
    INDEX ixCustID                      CustID.
    
DEFINE TEMP-TABLE ttSavingAccount NO-UNDO
    FIELD AccountID  AS INTEGER
    FIELD MinBalance AS DECIMAL
    INDEX pkSavingAcct IS PRIMARY UNIQUE AccountID.
    
    
DEFINE TEMP-TABLE ttLoanAccount NO-UNDO
    FIELD AccountID    AS INTEGER
    FIELD LoanAmount   AS DECIMAL
    FIELD EMIAmount    AS DECIMAL
    FIELD InterestRate AS DECIMAL
    FIELD TenureMonths AS INTEGER
    FIELD StartDate    AS DATE
    INDEX pkLoanAcct IS PRIMARY UNIQUE AccountID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB-SelectAccountType 
&Scoped-Define DISPLAYED-OBJECTS CMB-SelectAccountType 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE VARIABLE CMB-SelectAccountType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Select Account Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Savings","Loan" 
     DROP-DOWN-LIST
     SIZE 12 BY 1 NO-UNDO.

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
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE CMB-AccountType-Loan AS CHARACTER FORMAT "X(256)":U 
     LABEL "Account Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Loan" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE CMB-LoanDuration AS CHARACTER FORMAT "X(256)":U 
     LABEL "Loan Duration (yrs)" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1","2","3","4","5","6","7" 
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
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE CMB-AccountType-Savings AS CHARACTER FORMAT "X(256)":U 
     LABEL "Account Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Savings" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

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
     CMB-SelectAccountType AT ROW 2 COL 46 COLON-ALIGNED WIDGET-ID 4
     SPACE(26.99) SKIP(20.36)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>" WIDGET-ID 100.

DEFINE FRAME FRM-LoanAccount
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
         AT COLUMN 4 ROW 13
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
         AT COLUMN 4 ROW 4
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
ASSIGN FRAME FRM-LoanAccount:FRAME = FRAME Dialog-Frame:HANDLE
       FRAME FRM-SavingAccount:FRAME = FRAME Dialog-Frame:HANDLE.

/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRM-SavingAccount:MOVE-AFTER-TAB-ITEM (CMB-SelectAccountType:HANDLE IN FRAME Dialog-Frame)
       XXTABVALXX = FRAME FRM-SavingAccount:MOVE-BEFORE-TAB-ITEM (FRAME FRM-LoanAccount:HANDLE)
/* END-ASSIGN-TABS */.

ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRM-LoanAccount
                                                                        */
/* SETTINGS FOR COMBO-BOX CMB-AccountType-Loan IN FRAME FRM-LoanAccount
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


&Scoped-define FRAME-NAME FRM-LoanAccount
&Scoped-define SELF-NAME BTN-Create-Loan
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Create-Loan Dialog-Frame
ON CHOOSE OF BTN-Create-Loan IN FRAME FRM-LoanAccount /* Create */
DO:
        RUN CreateLoanAccountInternal.  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRM-SavingAccount
&Scoped-define SELF-NAME BTN-Create-Savings
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Create-Savings Dialog-Frame
ON CHOOSE OF BTN-Create-Savings IN FRAME FRM-SavingAccount /* Create */
DO:
        RUN CreateSavingsAccountInternal.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB-AccountType-Savings
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB-AccountType-Savings Dialog-Frame
ON VALUE-CHANGED OF CMB-AccountType-Savings IN FRAME FRM-SavingAccount /* Account Type */
DO:

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define SELF-NAME CMB-SelectAccountType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB-SelectAccountType Dialog-Frame
ON VALUE-CHANGED OF CMB-SelectAccountType IN FRAME Dialog-Frame /* Select Account Type */
DO:
        CASE SELF:SCREEN-VALUE:
            WHEN "SAVINGS" THEN 
                DO:
                    ENABLE ALL WITH FRAME FRM-SavingAccount.
                    DISABLE ALL WITH FRAME FRM-LoanAccount.
                END.
            WHEN "LOAN" THEN 
                DO:
                    ENABLE ALL WITH FRAME FRM-LoanAccount.
                    DISABLE ALL WITH FRAME FRM-SavingAccount.
                END.
        END CASE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateLoanAccountInternal Dialog-Frame 
PROCEDURE CreateLoanAccountInternal :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iNextAccID AS INTEGER NO-UNDO.
    DEFINE VARIABLE oAccounts  AS CLASS Accounts NO-UNDO.

    /* create object */
    oAccounts = NEW Accounts().

    /* call method */
    oAccounts:CreateLoanAccount(
        iCustID,
        FLN-IFSC-Code-Loan:SCREEN-VALUE IN FRAME FRM-LoanAccount,
        INTEGER(FLN-TotalLoanAmmount:SCREEN-VALUE),
        INTEGER(FLN-RateOfIntrest:SCREEN-VALUE),
        INTEGER(CMB-LoanDuration:SCREEN-VALUE),
        OUTPUT iNextAccID
    ).

    MESSAGE "Loan Account created successfully, ID: " iNextAccID VIEW-AS ALERT-BOX INFO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateSavingsAccountInternal Dialog-Frame 
PROCEDURE CreateSavingsAccountInternal :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iNextAccountID AS INTEGER  NO-UNDO.
    DEFINE VARIABLE oAccounts      AS Accounts NO-UNDO. 
    oAccounts = NEW Accounts().
  
    /* Call backend class method */
    oAccounts:CreateSavingsAccount(
        INPUT iCustID,
        INPUT FLN-IFSC-Code-Savings:SCREEN-VALUE in FRAme FRM-SavingAccount,
        INPUT INTEGER(FLN-TransferLimit:SCREEN-VALUE),
        output iNextAccountID).

    MESSAGE "Savings Account created successfully, ID: " iNextAccountID VIEW-AS ALERT-BOX INFO.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  HIDE FRAME FRM-LoanAccount.
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
  DISPLAY CMB-SelectAccountType 
      WITH FRAME Dialog-Frame.
  ENABLE CMB-SelectAccountType 
      WITH FRAME Dialog-Frame.
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
      WITH FRAME FRM-LoanAccount.
  ENABLE RECT-13 CMB-AccountSubType-Loan FLN-IFSC-Code-Loan CMB-LoanDuration 
         FLN-TotalLoanAmmount FLN-RateOfIntrest BTN-Create-Loan BTN-Cancel-Loan 
      WITH FRAME FRM-LoanAccount.
  {&OPEN-BROWSERS-IN-QUERY-FRM-LoanAccount}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

