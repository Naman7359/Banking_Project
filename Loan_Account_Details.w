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
&Scoped-define BROWSE-NAME BROWSE-9

/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-16 RECT-17 FLN-LoanAccount ~
FLN-TotalAmount FLN-PrincipalAmount FLN-RateOfIntrest FLN-AcountSubType ~
FLN-Duration BTN-PayEMI BROWSE-9 
&Scoped-Define DISPLAYED-OBJECTS FLN-LoanAccount FLN-TotalAmount ~
FLN-PrincipalAmount FLN-RateOfIntrest FLN-AcountSubType FLN-Duration 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-PayEMI 
     LABEL "Pay EMI" 
     SIZE 10 BY 1.13.

DEFINE VARIABLE FLN-AcountSubType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Acount Sub Type" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-Duration AS CHARACTER FORMAT "X(256)":U 
     LABEL "Duration (Mth)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-LoanAccount AS CHARACTER FORMAT "X(256)":U 
     LABEL "Loan Account" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-PrincipalAmount AS CHARACTER FORMAT "X(256)":U 
     LABEL "Principal Amount" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-RateOfIntrest AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rate of Intrest" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FLN-TotalAmount AS CHARACTER FORMAT "X(256)":U 
     LABEL "Total Amount" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 10  NO-FILL   
     SIZE 68 BY 7.75.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 10  NO-FILL   
     SIZE 68 BY 5.75.


/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-9 Dialog-Frame _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 61 BY 4.5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FLN-LoanAccount AT ROW 4 COL 28 COLON-ALIGNED WIDGET-ID 4
     FLN-TotalAmount AT ROW 4 COL 58 COLON-ALIGNED WIDGET-ID 10
     FLN-PrincipalAmount AT ROW 5.75 COL 28 COLON-ALIGNED WIDGET-ID 6
     FLN-RateOfIntrest AT ROW 5.75 COL 58 COLON-ALIGNED WIDGET-ID 12
     FLN-AcountSubType AT ROW 7.5 COL 28 COLON-ALIGNED WIDGET-ID 8
     FLN-Duration AT ROW 7.5 COL 58 COLON-ALIGNED WIDGET-ID 14
     BTN-PayEMI AT ROW 9.25 COL 41 WIDGET-ID 22
     BROWSE-9 AT ROW 13 COL 14 WIDGET-ID 200
     "EMI Details" VIEW-AS TEXT
          SIZE 13 BY 1 AT ROW 11.25 COL 40 WIDGET-ID 26
          FONT 5
     "LOAN ACCOUNT DETAILS" VIEW-AS TEXT
          SIZE 31 BY 1.5 AT ROW 1.5 COL 31 WIDGET-ID 2
          FONT 9
     RECT-16 AT ROW 3.25 COL 11 WIDGET-ID 24
     RECT-17 AT ROW 12.5 COL 11 WIDGET-ID 28
     SPACE(11.37) SKIP(0.74)
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
/* BROWSE-TAB BROWSE-9 BTN-PayEMI Dialog-Frame */
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


&Scoped-define BROWSE-NAME BROWSE-9
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
  DISPLAY FLN-LoanAccount FLN-TotalAmount FLN-PrincipalAmount FLN-RateOfIntrest 
          FLN-AcountSubType FLN-Duration 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-16 RECT-17 FLN-LoanAccount FLN-TotalAmount FLN-PrincipalAmount 
         FLN-RateOfIntrest FLN-AcountSubType FLN-Duration BTN-PayEMI 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

