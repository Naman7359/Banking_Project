 
/*------------------------------------------------------------------------
   File        : Accounts
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Naman.kaushik
   Created     : Wed Sep 24 22:28:38 IST 2025
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Accounts: 
   


    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    CONSTRUCTOR PUBLIC Accounts (  ):
        SUPER ().
        
    END CONSTRUCTOR.


    /* Method to create a Savings account */
    METHOD PUBLIC VOID CreateSavingsAccount(
        INPUT  iCustID       AS INTEGER,
        INPUT  cBranchCode   AS CHARACTER,
        INPUT  iTransferLimit AS INTEGER,
        OUTPUT iAccountID    AS INTEGER):

        DEFINE VARIABLE iNextAccID AS INTEGER NO-UNDO.

        /* Get next account number */
        iNextAccID = NEXT-VALUE(AccountIDSeq).

        /* Insert into CustomerAccounts */
        CREATE CustomerAccounts.
        ASSIGN
            CustomerAccounts.AcctNum       = iNextAccID
            CustomerAccounts.CustID        = iCustID
            CustomerAccounts.AccountTypeId = 1.  /* 1 = Savings */

        /* Insert into SavingAccountDetail */
        CREATE SavingAccountDetail.
        ASSIGN
            SavingAccountDetail.AcctNum        = iNextAccID
            SavingAccountDetail.BranchCode     = cBranchCode
            SavingAccountDetail.Transfer_limit = iTransferLimit.

        /* Return AccountID */
        iAccountID = iNextAccID.

    END METHOD.

    /* Method to create a Loan account */
    METHOD PUBLIC VOID CreateLoanAccount(
        INPUT  iCustID      AS INTEGER,
        INPUT  cBranchCode  AS CHARACTER,
        INPUT  iTotalLoan   AS INTEGER,
        INPUT  iRateOfInt   AS INTEGER,
        INPUT  iLoanTenure  AS INTEGER,
        OUTPUT iAccountID   AS INTEGER):

        DEFINE VARIABLE iNextAccID AS INTEGER NO-UNDO.

        /* Get next account number */
        iNextAccID = NEXT-VALUE(AccountIDSeq).

        /* Insert into CustomerAccounts */
        CREATE CustomerAccounts.
        ASSIGN
            CustomerAccounts.AcctNum       = iNextAccID
            CustomerAccounts.CustID        = iCustID
            CustomerAccounts.AccountTypeId = 2.  /* 2 = Loan */

        /* Insert into LoanAccountDetail */
        CREATE LoanAccountDetail.
        ASSIGN
            LoanAccountDetail.AccNum          = iNextAccID
            LoanAccountDetail.BranchCode      = INTEGER(cBranchCode)
            LoanAccountDetail.TotalLoanAmount = iTotalLoan
            LoanAccountDetail.RateOfIntrest   = iRateOfInt
            LoanAccountDetail.LoanTenure      = iLoanTenure.

        /* Return AccountID */
        iAccountID = iNextAccID.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC Accounts ( ):

    END DESTRUCTOR.

END CLASS.
