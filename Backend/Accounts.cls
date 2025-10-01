 
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
USING Progress.Json.ObjectModel.*.


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
    
    
    METHOD PUBLIC VOID UpdateSavingsAccount(
        INPUT iAccountID   AS INTEGER,
        INPUT cBranchCode  AS CHARACTER,
        INPUT iTransferLimit AS INTEGER):

        /* Find the record first */
        FIND SavingAccountDetail WHERE SavingAccountDetail.AcctNum = iAccountID NO-LOCK NO-ERROR.
    
        IF AVAILABLE SavingAccountDetail THEN 
        DO:
            ASSIGN
                SavingAccountDetail.BranchCode     = cBranchCode
                SavingAccountDetail.Transfer_limit = iTransferLimit.
        END.

    END METHOD.



    METHOD PUBLIC VOID UpdateLoanAccount(
        INPUT iAccountID   AS INTEGER,
        INPUT cBranchCode  AS INTEGER,
        INPUT iTotalLoan   AS INTEGER,
        INPUT iRateOfInt   AS INTEGER,
        INPUT iLoanTenure  AS INTEGER):

        /* Find the record first */
        FIND LoanAccountDetail WHERE LoanAccountDetail.AccNum = iAccountID NO-LOCK NO-ERROR.
    
        IF AVAILABLE LoanAccountDetail THEN 
        DO:
            ASSIGN
                LoanAccountDetail.BranchCode      = cBranchCode
                LoanAccountDetail.TotalLoanAmount = iTotalLoan
                LoanAccountDetail.RateOfIntrest   = iRateOfInt
                LoanAccountDetail.LoanTenure      = iLoanTenure.
        END.

    END METHOD.
        
        
        
        
    METHOD PUBLIC LONGCHAR GetAccountDetails (
        INPUT iAccountID AS INTEGER):

        DEFINE VARIABLE oJsonObj  AS JsonObject NO-UNDO.
        DEFINE VARIABLE iAcctType AS INTEGER    NO-UNDO.
        DEFINE VARIABLE lcData    AS LONGCHAR   NO-UNDO.

        DO ON ERROR UNDO, THROW:
            
            FIND FIRST CustomerAccounts WHERE CustomerAccounts.AcctNum = iAccountID NO-LOCK NO-ERROR.
            
            IF NOT AVAILABLE CustomerAccounts THEN 
                RETURN Backend.Utility.Mini_Utility:BuildResponse(FALSE, 404, "Account data not Found").                
            FIND FIRST AccountType WHERE AccountType.AccountTypeID = CustomerAccounts.AccountTypeId NO-LOCK NO-ERROR.
            IF NOT AVAILABLE AccountType THEN 
                RETURN Backend.Utility.Mini_Utility:BuildResponse(FALSE, 404, "Account Type not Found").                
            
            oJsonObj:set("Account Num",iAccountID).
            oJsonObj:set("AccountType", AccountType.AccountType).
            oJsonObj:set("AccountSubType", AccountType.AccSubType).

            IF AccountType.AccountType = "Saving" THEN 
            DO:
                FIND FIRST SavingAccountDetail WHERE SavingAccountDetail.AcctNum = iAccountID NO-LOCK NO-ERROR.
                IF NOT AVAILABLE SavingAccountDetail THEN 
                    RETURN Backend.Utility.Mini_Utility:BuildResponse(FALSE, 404, "Savings Account not Found").                
                
                oJsonObj:set("IFSC_Code", SavingAccountDetail.BranchCode).
                oJsonObj:set("Transfer_Limit",SavingAccountDetail.Transfer_limit).
            END.
            ELSE IF AccountType.AccountType = "Loan" THEN 
                DO:
                    FIND FIRST LoanAccountDetail WHERE LoanAccountDetail.AccNum = iAccountID NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE LoanAccountDetail THEN 
                        RETURN Backend.Utility.Mini_Utility:BuildResponse(FALSE, 404, "Loan Account not Found").                
                    oJsonObj:set("IFSC_Code", LoanAccountDetail.BranchCode).
                    oJsonObj:set("Loan_Duration", LoanAccountDetail.LoanTenure).
                    oJsonObj:set("Total_Loan", LoanAccountDetail.TotalLoanAmount).
                    oJsonObj:set("ROI", LoanAccountDetail.RateOfIntrest).
                END.
            oJsonObj:Write(lcData,TRUE,"UTF-8").
            
            RETURN Backend.Utility.Mini_Utility:BuildResponse(TRUE,200, lcData).
             
        END.
        CATCH  e AS Progress.Lang.Error :
            RETURN Backend.Utility.Mini_Utility:BuildResponse(FALSE,500, "Get Account Detail by ID: Internal Server Error.").
        END CATCH.
        FINALLY:
            DELETE OBJECT oJsonObj NO-ERROR.
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC Accounts ( ):

    END DESTRUCTOR.

END CLASS.
