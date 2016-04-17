/*
 *  MATRIX FACTORIZATION MODULE
 *
 *  Author:                     Advising Professor:
 *      Kenneth S. Kundert          Alberto Sangiovanni-Vincentelli
 *      UC Berkeley
 */
/*! \file
 *  This file contains the routines to factor the matrix into LU form.
 *
 *  Objects that begin with the \a spc prefix are considered private
 *  and should not be used.
 *
 *  \author
 *  Kenneth S. Kundert <kundert@users.sourceforge.net>
 */
/*  >>> User accessible functions contained in this file:
 *  spOrderAndFactor
 *  spFactor
 *  spPartition
 *
 *  >>> Other functions contained in this file:
 *  FactorComplexMatrix         spcCreateInternalVectors
 *  CountMarkowitz              MarkowitzProducts
 *  SearchForPivot              SearchForSingleton
 *  QuicklySearchDiagonal       SearchDiagonal
 *  SearchEntireMatrix          FindLargestInCol
 *  FindBiggestInColExclude     ExchangeRowsAndCols
 *  spcRowExchange              spcColExchange
 *  ExchangeColElements         ExchangeRowElements
 *  RealRowColElimination       ComplexRowColElimination
 *  UpdateMarkowitzNumbers      MatrixIsSingular
 *  ZeroPivot                        WriteStatus
 */


/*
 *  Revision and copyright information.
 *
 *  Copyright (c) 1985-2004
 *  by Kenneth S. Kundert
 */


/*
 *  IMPORTS
 *
 *  >>> Import descriptions:
 *  spConfig.h
 *    Macros that customize the sparse matrix routines.
 *  spMatrix.h
 *    Macros and declarations to be imported by the user.
 *  spDefs.h
 *    Matrix type and macro definitions for the sparse matrix routines.
 */

#define spINSIDE_SPARSE
#include <stdio.h>
#include "spConfig.h"
#include "spMatrix.h"
#include "spDefs.h"





/*
 * Function declarations
 */
#if spCOMPLEX
static int  FactorComplexMatrix( MatrixPtr );
static void CreateInternalVectors( MatrixPtr );
#endif
static void CountMarkowitz( MatrixPtr, RealVector, int );
static void MarkowitzProducts( MatrixPtr, int );
static ElementPtr SearchForPivot( MatrixPtr, int, int );
static ElementPtr SearchForSingleton( MatrixPtr, int );
static ElementPtr SearchDiagonal( MatrixPtr, int );
static ElementPtr QuicklySearchDiagonal( MatrixPtr, int );
static ElementPtr SearchEntireMatrix( MatrixPtr, int );
static RealNumber FindLargestInCol( ElementPtr );
static RealNumber FindBiggestInColExclude( MatrixPtr, ElementPtr, int );
static void ExchangeRowsAndCols( MatrixPtr, ElementPtr, int );
static void ExchangeColElements( MatrixPtr, int, ElementPtr, int,
                                          ElementPtr, int );
static void ExchangeRowElements( MatrixPtr, int, ElementPtr, int,
                                          ElementPtr, int );
static void RealRowColElimination( MatrixPtr, ElementPtr );
#if spCOMPLEX
static void ComplexRowColElimination( MatrixPtr, ElementPtr );
#endif
static void UpdateMarkowitzNumbers( MatrixPtr, ElementPtr );
static int  MatrixIsSingular( MatrixPtr, int );
static int  ZeroPivot( MatrixPtr, int );
#if (ANNOTATE == FULL)
static void WriteStatus( MatrixPtr, int );
#endif




/*!
 *  This routine chooses a pivot order for the matrix and factors it
 *  into \a LU form.  It handles both the initial factorization and subsequent
 *  factorizations when a reordering is desired.  This is handled in a manner
 *  that is transparent to the user.  The routine uses a variation of
 *  Gauss's method where the pivots are associated with \a L and the
 *  diagonal terms of \a U are one.
 *
 *  \return
 *  The error code is returned.  Possible errors are \a spNO_MEMORY,
 *  \a spSINGULAR and \a spSMALL_PIVOT.
 *  Error is cleared upon entering this function.
 *
 *  \param eMatrix
 *      Pointer to the matrix.
 *  \param RHS
 *      Representative right-hand side vector that is used to determine
 *      pivoting order when the right hand side vector is sparse.  If
 *      RHS is a NULL pointer then the RHS vector is assumed to
 *      be full and it is not used when determining the pivoting
 *      order.
 *  \param RelThreshold
 *      This number determines what the pivot relative threshold will
 *      be.  It should be between zero and one.  If it is one then the
 *      pivoting method becomes complete pivoting, which is very slow
 *      and tends to fill up the matrix.  If it is set close to zero
 *      the pivoting method becomes strict Markowitz with no
 *      threshold.  The pivot threshold is used to eliminate pivot
 *      candidates that would cause excessive element growth if they
 *      were used.  Element growth is the cause of roundoff error.
 *      Element growth occurs even in well-conditioned matrices.
 *      Setting the \a RelThreshold large will reduce element growth and
 *      roundoff error, but setting it too large will cause execution
 *      time to be excessive and will result in a large number of
 *      fill-ins.  If this occurs, accuracy can actually be degraded
 *      because of the large number of operations required on the
 *      matrix due to the large number of fill-ins.  A good value seems
 *      to be 0.001.  The default is chosen by giving a value larger
 *      than one or less than or equal to zero.  This value should be
 *      increased and the matrix resolved if growth is found to be
 *      excessive.  Changing the pivot threshold does not improve
 *      performance on matrices where growth is low, as is often the
 *      case with ill-conditioned matrices.  Once a valid threshold is
 *      given, it becomes the new default.  The default value of
 *      \a RelThreshold was choosen for use with nearly diagonally
 *      dominant matrices such as node- and modified-node admittance
 *      matrices.  For these matrices it is usually best to use
 *      diagonal pivoting.  For matrices without a strong diagonal, it
 *      is usually best to use a larger threshold, such as 0.01 or
 *      0.1.
 *  \param AbsThreshold
 *      The absolute magnitude an element must have to be considered
 *      as a pivot candidate, except as a last resort.  This number
 *      should be set significantly smaller than the smallest diagonal
 *      element that is is expected to be placed in the matrix.  If
 *      there is no reasonable prediction for the lower bound on these
 *      elements, then \a AbsThreshold should be set to zero.
 *      \a AbsThreshold is used to reduce the possibility of choosing as a
 *      pivot an element that has suffered heavy cancellation and as a
 *      result mainly consists of roundoff error.  Once a valid
 *      threshold is given, it becomes the new default.
 *  \param DiagPivoting
 *      A flag indicating that pivot selection should be confined to the
 *      diagonal if possible.  If \a DiagPivoting is nonzero and if
 *      \a DIAGONAL_PIVOTING is enabled pivots will be chosen only from
 *      the diagonal unless there are no diagonal elements that satisfy
 *      the threshold criteria.  Otherwise, the entire reduced
 *      submatrix is searched when looking for a pivot.  The diagonal
 *      pivoting in Sparse is efficient and well refined, while the
 *      off-diagonal pivoting is not.  For symmetric and near symmetric
 *      matrices, it is best to use diagonal pivoting because it
 *      results in the best performance when reordering the matrix and
 *      when factoring the matrix without ordering.  If there is a
 *      considerable amount of nonsymmetry in the matrix, then
 *      off-diagonal pivoting may result in a better equation ordering
 *      simply because there are more pivot candidates to choose from.
 *      A better ordering results in faster subsequent factorizations.
 *      However, the initial pivot selection process takes considerably
 *      longer for off-diagonal pivoting.
 *
 *  \see spFactor()
 */
/*  >>> Local variables:
 *  pPivot  (ElementPtr)
 *      Pointer to the element being used as a pivot.
 *
 */

spError
spOrderAndFactor(
    spMatrix eMatrix,
    spREAL RHS[],
    spREAL RelThreshold,
    spREAL AbsThreshold,
    int DiagPivoting
)
{
MatrixPtr  Matrix = (MatrixPtr)eMatrix;
ElementPtr  pPivot;
int  Step, Size;
RealNumber LargestInCol;

/* Begin `spOrderAndFactor'. */
    ASSERT_IS_SPARSE( Matrix );
    ASSERT_NO_ERRORS( Matrix );
    ASSERT_IS_NOT_FACTORED( Matrix );

    Matrix->Error = spOKAY;
    Size = Matrix->Size;
    if (RelThreshold <= 0.0) RelThreshold = Matrix->RelThreshold;
    if (RelThreshold > 1.0) RelThreshold = Matrix->RelThreshold;
    Matrix->RelThreshold = RelThreshold;
    if (AbsThreshold < 0.0) AbsThreshold = Matrix->AbsThreshold;
    Matrix->AbsThreshold = AbsThreshold;

    if (NOT Matrix->NeedsOrdering)
    {
/* Matrix has been factored before and reordering is not required. */
        for (Step = 1; Step <= Size; Step++)
        {   pPivot = Matrix->Diag[Step];
            LargestInCol = FindLargestInCol(pPivot->NextInCol);
            if ((LargestInCol * RelThreshold < ELEMENT_MAG(pPivot)))
            {
#if spCOMPLEX
              if (Matrix->Complex)
                ComplexRowColElimination( Matrix, pPivot );
              else
                RealRowColElimination( Matrix, pPivot );
#else
              RealRowColElimination( Matrix, pPivot );
#endif
            }
            else
            {   Matrix->NeedsOrdering = YES;
                break; /* for loop */
            }
        }
        if (NOT Matrix->NeedsOrdering)
            goto Done;
        else
        {
/*
 * A pivot was not large enough to maintain accuracy,
 * so a partial reordering is required.
 */

#if (ANNOTATE >= ON_STRANGE_BEHAVIOR)
            printf("Reordering,  Step = %1d\n", Step);
#endif
        }
    } /* End of if(NOT Matrix->NeedsOrdering) */
    else
    {
/*
 * This is the first time the matrix has been factored.  These few statements
 * indicate to the rest of the code that a full reodering is required rather
 * than a partial reordering, which occurs during a failure of a fast
 * factorization.
 */
        Step = 1;
        if (NOT Matrix->RowsLinked)
            spcLinkRows( Matrix );
        if (NOT Matrix->InternalVectorsAllocated)
            spcCreateInternalVectors( Matrix );
        if (Matrix->Error >= spFATAL)
            return Matrix->Error;
    }

/* Form initial Markowitz products. */
    CountMarkowitz( Matrix, RHS, Step );
    MarkowitzProducts( Matrix, Step );
    Matrix->MaxRowCountInLowerTri = -1;

/* Perform reordering and factorization. */
    for (; Step <= Size; Step++)
    {   pPivot = SearchForPivot( Matrix, Step, DiagPivoting );
        if (pPivot == NULL) return MatrixIsSingular( Matrix, Step );
        ExchangeRowsAndCols( Matrix, pPivot, Step );
#if spCOMPLEX
        if (Matrix->Complex)
            ComplexRowColElimination( Matrix, pPivot );
        else
            RealRowColElimination( Matrix, pPivot );
#else
            RealRowColElimination( Matrix, pPivot );
#endif
        if(Matrix->Error>spFATAL)
          Size = 0;

        if (Matrix->Error >= spFATAL) return Matrix->Error;
        UpdateMarkowitzNumbers( Matrix, pPivot );

#if (ANNOTATE == FULL)
        WriteStatus( Matrix, Step );
#endif
    }

Done:
    Matrix->NeedsOrdering = NO;
    Matrix->Reordered = YES;
    Matrix->Factored = YES;

    return Matrix->Error;
}







/*!
 *  This routine is the companion routine to spOrderAndFactor().
 *  Unlike spOrderAndFactor(), spFactor() cannot change the ordering.
 *  It is also faster than spOrderAndFactor().  The standard way of
 *  using these two routines is to first use spOrderAndFactor() for the
 *  initial factorization.  For subsequent factorizations, spFactor()
 *  is used if there is some assurance that little growth will occur
 *  (say for example, that the matrix is diagonally dominant).  If
 *  spFactor() is called for the initial factorization of the matrix,
 *  then spOrderAndFactor() is automatically called with the default
 *  threshold.  This routine uses "row at a time" \a LU factorization.
 *  Pivots are associated with the lower triangular matrix and the
 *  diagonals of the upper triangular matrix are ones.
 *
 *  \return
 *  The error code is returned.  Possible errors are
 *  \a spNO_MEMORY, \a spSINGULAR, \a spZERO_DIAG and \a spSMALL_PIVOT.
 *  Error is cleared upon entering this function.
 *
 *  \param eMatrix
 *      Pointer to matrix.
 *  \see spOrderAndFactor()
 */

spError
spFactor( spMatrix eMatrix )
{
 MatrixPtr  Matrix = (MatrixPtr)eMatrix;
register  ElementPtr  pElement;
register  ElementPtr  pColumn;
register  int  Step, Size;
RealNumber Mult;

/* Begin `spFactor'. */
    ASSERT_IS_SPARSE( Matrix );
    ASSERT_NO_ERRORS( Matrix );
    ASSERT_IS_NOT_FACTORED( Matrix );

    if (Matrix->NeedsOrdering)
    {   return spOrderAndFactor( eMatrix, (RealVector)NULL,
                                 0.0, 0.0, DIAG_PIVOTING_AS_DEFAULT );
    }
    if (NOT Matrix->Partitioned) spPartition( eMatrix, spDEFAULT_PARTITION );
#if spCOMPLEX
    if (Matrix->Complex) return FactorComplexMatrix( Matrix );
#endif

#if REAL
    Size = Matrix->Size;

    if (Matrix->Diag[1]->Real == 0.0) return ZeroPivot( Matrix, 1 );
    Matrix->Diag[1]->Real = 1.0 / Matrix->Diag[1]->Real;

/* Start factorization. */
    for (Step = 2; Step <= Size; Step++)
    {   if (Matrix->DoRealDirect[Step])
        {   /* Update column using direct addressing scatter-gather. */
            register RealNumber *Dest = (RealNumber *)Matrix->Intermediate;

/* Scatter. */
            pElement = Matrix->FirstInCol[Step];
            while (pElement != NULL)
            {   Dest[pElement->Row] = pElement->Real;
                pElement = pElement->NextInCol;
            }

/* Update column. */
            pColumn = Matrix->FirstInCol[Step];
            while (pColumn->Row < Step)
            {   pElement = Matrix->Diag[pColumn->Row];
                pColumn->Real = Dest[pColumn->Row] * pElement->Real;
                while ((pElement = pElement->NextInCol) != NULL)
                    Dest[pElement->Row] -= pColumn->Real * pElement->Real;
                pColumn = pColumn->NextInCol;
            }

/* Gather. */
            pElement = Matrix->Diag[Step]->NextInCol;
            while (pElement != NULL)
            {   pElement->Real = Dest[pElement->Row];
                pElement = pElement->NextInCol;
            }

/* Check for singular matrix. */
            if (Dest[Step] == 0.0) return ZeroPivot( Matrix, Step );
            Matrix->Diag[Step]->Real = 1.0 / Dest[Step];
        }
        else
        {   /* Update column using indirect addressing scatter-gather. */
            register RealNumber **pDest = (RealNumber **)Matrix->Intermediate;

/* Scatter. */
            pElement = Matrix->FirstInCol[Step];
            while (pElement != NULL)
            {   pDest[pElement->Row] = &pElement->Real;
                pElement = pElement->NextInCol;
            }

/* Update column. */
            pColumn = Matrix->FirstInCol[Step];
            while (pColumn->Row < Step)
            {   pElement = Matrix->Diag[pColumn->Row];
                Mult = (*pDest[pColumn->Row] *= pElement->Real);
                while ((pElement = pElement->NextInCol) != NULL)
                    *pDest[pElement->Row] -= Mult * pElement->Real;
                pColumn = pColumn->NextInCol;
            }

/* Check for singular matrix. */
            if (Matrix->Diag[Step]->Real == 0.0)
                return ZeroPivot( Matrix, Step );
            Matrix->Diag[Step]->Real = 1.0 / Matrix->Diag[Step]->Real;
        }
    }

    Matrix->Factored = YES;
    return (Matrix->Error = spOKAY);
#endif /* REAL */
}






#if spCOMPLEX
/*
 *  FACTOR COMPLEX MATRIX
 *
 *  This routine is the companion routine to spFactor(), it
 *  handles complex matrices.  It is otherwise identical.
 *
 *  >>> Returned:
 *  The error code is returned.  Possible errors are listed below.
 *
 *  >>> Arguments:
 *  Matrix  <input>  (char *)
 *      Pointer to matrix.
 *
 *  >>> Possible errors:
 *  spSINGULAR
 *  Error is cleared in this function.
 */

static int
FactorComplexMatrix( MatrixPtr  Matrix )
{
register  ElementPtr  pElement;
register  ElementPtr  pColumn;
register  int  Step, Size;
ComplexNumber Mult, Pivot;

/* Begin `FactorComplexMatrix'. */
    ASSERT(Matrix->Complex);

    Size = Matrix->Size;
    pElement = Matrix->Diag[1];
    if (ELEMENT_MAG(pElement) == 0.0) return ZeroPivot( Matrix, 1 );
/* Cmplx expr: *pPivot = 1.0 / *pPivot. */
    CMPLX_RECIPROCAL( *pElement, *pElement );

/* Start factorization. */
    for (Step = 2; Step <= Size; Step++)
    {   if (Matrix->DoCmplxDirect[Step])
        {   /* Update column using direct addressing scatter-gather. */
            register  ComplexNumber  *Dest;
            Dest = (ComplexNumber *)Matrix->Intermediate;

/* Scatter. */
            pElement = Matrix->FirstInCol[Step];
            while (pElement != NULL)
            {   Dest[pElement->Row] = *(ComplexNumber *)pElement;
                pElement = pElement->NextInCol;
            }

/* Update column. */
            pColumn = Matrix->FirstInCol[Step];
            while (pColumn->Row < Step)
            {   pElement = Matrix->Diag[pColumn->Row];
                /* Cmplx expr: Mult = Dest[pColumn->Row] * (1.0 / *pPivot). */
                CMPLX_MULT(Mult, Dest[pColumn->Row], *pElement);
                CMPLX_ASSIGN(*pColumn, Mult);
                while ((pElement = pElement->NextInCol) != NULL)
                {   /* Cmplx expr: Dest[pElement->Row] -= Mult * pElement */
                    CMPLX_MULT_SUBT_ASSIGN(Dest[pElement->Row],Mult,*pElement);
                }
                pColumn = pColumn->NextInCol;
            }

/* Gather. */
            pElement = Matrix->Diag[Step]->NextInCol;
            while (pElement != NULL)
            {   *(ComplexNumber *)pElement = Dest[pElement->Row];
                pElement = pElement->NextInCol;
            }

/* Check for singular matrix. */
            Pivot = Dest[Step];
            if (CMPLX_1_NORM(Pivot) == 0.0) return ZeroPivot( Matrix, Step );
            CMPLX_RECIPROCAL( *Matrix->Diag[Step], Pivot );
        }
        else
        {   /* Update column using direct addressing scatter-gather. */
            register  ComplexNumber  **pDest;
            pDest = (ComplexNumber **)Matrix->Intermediate;

/* Scatter. */
            pElement = Matrix->FirstInCol[Step];
            while (pElement != NULL)
            {   pDest[pElement->Row] = (ComplexNumber *)pElement;
                pElement = pElement->NextInCol;
            }

/* Update column. */
            pColumn = Matrix->FirstInCol[Step];
            while (pColumn->Row < Step)
            {   pElement = Matrix->Diag[pColumn->Row];
                /* Cmplx expr: Mult = *pDest[pColumn->Row] * (1.0 / *pPivot). */
                CMPLX_MULT(Mult, *pDest[pColumn->Row], *pElement);
                CMPLX_ASSIGN(*pDest[pColumn->Row], Mult);
                while ((pElement = pElement->NextInCol) != NULL)
                {  /* Cmplx expr: *pDest[pElement->Row] -= Mult * pElement */
                   CMPLX_MULT_SUBT_ASSIGN(*pDest[pElement->Row],Mult,*pElement);
                }
                pColumn = pColumn->NextInCol;
            }

/* Check for singular matrix. */
            pElement = Matrix->Diag[Step];
            if (ELEMENT_MAG(pElement) == 0.0) return ZeroPivot( Matrix, Step );
            CMPLX_RECIPROCAL( *pElement, *pElement );
        }
    }

    Matrix->Factored = YES;
    return (Matrix->Error = spOKAY);
}
#endif /* spCOMPLEX */






/*!
 *  This routine determines the cost to factor each row using both
 *  direct and indirect addressing and decides, on a row-by-row basis,
 *  which addressing mode is fastest.  This information is used in
 *  spFactor() to speed the factorization.
 *
 *  When factoring a previously ordered matrix using spFactor(), Sparse
 *  operates on a row-at-a-time basis.  For speed, on each step, the
 *  row being updated is copied into a full vector and the operations
 *  are performed on that vector.  This can be done one of two ways,
 *  either using direct addressing or indirect addressing.  Direct
 *  addressing is fastest when the matrix is relatively dense and
 *  indirect addressing is best when the matrix is quite sparse.  The
 *  user selects the type of partition used with \a Mode.  If \a Mode is set
 *  to \a spDIRECT_PARTITION, then the all rows are placed in the direct
 *  addressing partition.  Similarly, if \a Mode is set to
 *  \a spINDIRECT_PARTITION, then the all rows are placed in the indirect
 *  addressing partition.  By setting \a Mode to \a spAUTO_PARTITION, the
 *  user allows Sparse to select the partition for each row
 *  individually.  spFactor() generally runs faster if Sparse is
 *  allowed to choose its own partitioning, however choosing a
 *  partition is expensive.  The time required to choose a partition is
 *  of the same order of the cost to factor the matrix.  If you plan to
 *  factor a large number of matrices with the same structure, it is
 *  best to let Sparse choose the partition.  Otherwise, you should
 *  choose the partition based on the predicted density of the matrix.
 *
 *  \param eMatrix
 *      Pointer to matrix.
 *  \param Mode
 *      Mode must be one of three special codes: \a spDIRECT_PARTITION,
 *      \a spINDIRECT_PARTITION, or \a spAUTO_PARTITION.
 */

void
spPartition(
    spMatrix eMatrix,
    int Mode
)
{
MatrixPtr  Matrix = (MatrixPtr)eMatrix;
register  ElementPtr  pElement, pColumn;
register  int  Step, Size;
register  int  *Nc, *No;
register  long  *Nm;
BOOLEAN *DoRealDirect, *DoCmplxDirect;

/* Begin `spPartition'. */
    ASSERT_IS_SPARSE( Matrix );

    if (Matrix->Partitioned) return;
    Size = Matrix->Size;
    DoRealDirect = Matrix->DoRealDirect;
//    DoCmplxDirect = Matrix->DoCmplxDirect;
    Matrix->Partitioned = YES;

/* If partition is specified by the user, this is easy. */
    if (Mode == spDEFAULT_PARTITION) Mode = DEFAULT_PARTITION;
    if (Mode == spDIRECT_PARTITION)
    {   for (Step = 1; Step <= Size; Step++)
#if REAL
            DoRealDirect[Step] = YES;
#endif
#if spCOMPLEX
            DoCmplxDirect[Step] = YES;
#endif
        return;
    }
    else if (Mode == spINDIRECT_PARTITION)
    {   for (Step = 1; Step <= Size; Step++)
#if REAL
            DoRealDirect[Step] = NO;
#endif
#if spCOMPLEX
            DoCmplxDirect[Step] = NO;
#endif
        return;
    }
    else vASSERT( Mode == spAUTO_PARTITION, "Invalid partition code" );;

/* Otherwise, count all operations needed in when factoring matrix. */
    Nc = (int *)Matrix->MarkowitzRow;
    No = (int *)Matrix->MarkowitzCol;
    Nm = (long *)Matrix->MarkowitzProd;

/* Start mock-factorization. */
    for (Step = 1; Step <= Size; Step++)
    {   Nc[Step] = No[Step] = Nm[Step] = 0;

        pElement = Matrix->FirstInCol[Step];
        while (pElement != NULL)
        {   Nc[Step]++;
            pElement = pElement->NextInCol;
        }

        pColumn = Matrix->FirstInCol[Step];
        while (pColumn->Row < Step)
        {   pElement = Matrix->Diag[pColumn->Row];
            Nm[Step]++;
            while ((pElement = pElement->NextInCol) != NULL)
                No[Step]++;
            pColumn = pColumn->NextInCol;
        }
    }

    for (Step = 1; Step <= Size; Step++)
    {
/*
 * The following are just estimates based on a count on the number of
 * machine instructions used on each machine to perform the various
 * tasks.  It was assumed that each machine instruction required the
 * same amount of time (I don't believe this is true for the VAX, and
 * have no idea if this is true for the 68000 family).  For optimum
 * performance, these numbers should be tuned to the machine.
 *   Nc is the number of nonzero elements in the column.
 *   Nm is the number of multipliers in the column.
 *   No is the number of operations in the inner loop.
 */

#define generic
#ifdef hp9000s300
#if REAL
        DoRealDirect[Step] = (Nm[Step] + No[Step] > 3*Nc[Step] - 2*Nm[Step]);
#endif
#if spCOMPLEX
        /* On the hp350, it is never profitable to use direct for complex. */
        DoCmplxDirect[Step] = NO;
#endif
#undef generic
#endif

#ifdef vax
#if REAL
        DoRealDirect[Step] = (Nm[Step] + No[Step] > 3*Nc[Step] - 2*Nm[Step]);
#endif
#if spCOMPLEX
        DoCmplxDirect[Step] = (Nm[Step] + No[Step] > 7*Nc[Step] - 4*Nm[Step]);
#endif
#undef generic
#endif

#ifdef generic
#if REAL
        DoRealDirect[Step] = (Nm[Step] + No[Step] > 3*Nc[Step] - 2*Nm[Step]);
#endif
#if spCOMPLEX
        DoCmplxDirect[Step] = (Nm[Step] + No[Step] > 7*Nc[Step] - 4*Nm[Step]);
#endif
#undef generic
#endif
    }

#if (ANNOTATE == FULL)
    {   int Ops = 0;
        for (Step = 1; Step <= Size; Step++)
            Ops += No[Step];
        printf("Operation count for inner loop of factorization = %d.\n", Ops);
    }
#endif
    return;
}







/*
 *  CREATE INTERNAL VECTORS
 *
 *  Creates the Markowitz and Intermediate vectors.
 *
 *  >>> Arguments:
 *  Matrix  <input>  (MatrixPtr)
 *      Pointer to matrix.
 *
 *  >>> Possible errors:
 *  spNO_MEMORY
 */

void
spcCreateInternalVectors( MatrixPtr Matrix )
{
int  Size;

/* Begin `spcCreateInternalVectors'. */
/* Create Markowitz arrays. */
    Size= Matrix->Size;

    if (Matrix->MarkowitzRow == NULL)
    {   if (( Matrix->MarkowitzRow = ALLOC(int, Size+1)) == NULL)
            Matrix->Error = spNO_MEMORY;
    }
    if (Matrix->MarkowitzCol == NULL)
    {   if (( Matrix->MarkowitzCol = ALLOC(int, Size+1)) == NULL)
            Matrix->Error = spNO_MEMORY;
    }
    if (Matrix->MarkowitzProd == NULL)
    {   if (( Matrix->MarkowitzProd = ALLOC(long, Size+2)) == NULL)
            Matrix->Error = spNO_MEMORY;
    }

/* Create DoDirect vectors for use in spFactor(). */
#if REAL
    if (Matrix->DoRealDirect == NULL)
    {   if (( Matrix->DoRealDirect = ALLOC(BOOLEAN, Size+1)) == NULL)
            Matrix->Error = spNO_MEMORY;
    }
#endif
#if spCOMPLEX
    if (Matrix->DoCmplxDirect == NULL)
    {   if (( Matrix->DoCmplxDirect = ALLOC(BOOLEAN, Size+1)) == NULL)
            Matrix->Error = spNO_MEMORY;
    }
#endif

/* Create Intermediate vectors for use in MatrixSolve. */
#if spCOMPLEX
    if (Matrix->Intermediate == NULL)
    {   if ((Matrix->Intermediate = ALLOC(RealNumber,2*(Size+1))) == NULL)
            Matrix->Error = spNO_MEMORY;
    }
#else
    if (Matrix->Intermediate == NULL)
    {   if ((Matrix->Intermediate = ALLOC(RealNumber, Size+1)) == NULL)
            Matrix->Error = spNO_MEMORY;
    }
#endif

    if (Matrix->Error != spNO_MEMORY)
        Matrix->InternalVectorsAllocated = YES;
    return;
}







/*
 *  COUNT MARKOWITZ
 *
 *  Scans Matrix to determine the Markowitz counts for each row and column.
 *
 *  >>> Arguments:
 *  Matrix  <input>  (MatrixPtr)
 *      Pointer to matrix.
 *  RHS  <input>  (RealVector)
 *      Representative right-hand side vector that is used to determine
 *      pivoting order when the right hand side vector is sparse.  If
 *      RHS is a NULL pointer then the RHS vector is assumed to be full
 *      and it is not used when determining the pivoting order.
 *  Step  <input>  (int)
 *     Index of the diagonal currently being eliminated.
 *
 *  >>> Local variables:
 *  Count  (int)
 *     Temporary counting variable.
 *  ExtRow  (int)
 *     The external row number that corresponds to I.
 *  pElement  (ElementPtr)
 *     Pointer to matrix elements.
 *  Size  (int)
 *     The size of the matrix.
 */

static void
CountMarkowitz(
    MatrixPtr Matrix,
    register RealVector  RHS,
    int Step
)
{
register int  Count, I, Size = Matrix->Size;
register ElementPtr  pElement;
int  ExtRow;

/* Begin `CountMarkowitz'. */

/* Correct array pointer for ARRAY_OFFSET. */
#if NOT ARRAY_OFFSET
#if spSEPARATED_COMPLEX_VECTORS OR NOT spCOMPLEX
        if (RHS != NULL) --RHS;
#else
        if (RHS != NULL)
        {   if (Matrix->Complex) RHS -= 2;
            else --RHS;
        }
#endif
#endif

/* Generate MarkowitzRow Count for each row. */
    for (I = Step; I <= Size; I++)
    {
/* Set Count to -1 initially to remove count due to pivot element. */
        Count = -1;
        pElement = Matrix->FirstInRow[I];
        while (pElement != NULL AND pElement->Col < Step)
            pElement = pElement->NextInRow;
        while (pElement != NULL)
        {   Count++;
            pElement = pElement->NextInRow;
        }

/* Include nonzero elements in the RHS vector. */
        ExtRow = Matrix->IntToExtRowMap[I];

#if spSEPARATED_COMPLEX_VECTORS OR NOT spCOMPLEX
        if (RHS != NULL)
            if (RHS[ExtRow] != 0.0)  Count++;
#else
        if (RHS != NULL)
        {   if (Matrix->Complex)
            {   if ((RHS[2*ExtRow] != 0.0) OR (RHS[2*ExtRow+1] != 0.0))
                    Count++;
            }
            else if (RHS[I] != 0.0) Count++;
        }
#endif
        Matrix->MarkowitzRow[I] = Count;
    }

/* Generate the MarkowitzCol count for each column. */
    for (I = Step; I <= Size; I++)
    {
/* Set Count to -1 initially to remove count due to pivot element. */
        Count = -1;
        pElement = Matrix->FirstInCol[I];
        while (pElement != NULL AND pElement->Row < Step)
            pElement = pElement->NextInCol;
        while (pElement != NULL)
        {   Count++;
            pElement = pElement->NextInCol;
        }
        Matrix->MarkowitzCol[I] = Count;
    }
    return;
}










/*
 *  MARKOWITZ PRODUCTS
 *
 *  Calculates MarkowitzProduct for each diagonal element from the Markowitz
 *  counts.
 *
 *  >>> Arguments:
 *  Matrix  <input>  (MatrixPtr)
 *      Pointer to matrix.
 *  Step  <input>  (int)
 *      Index of the diagonal currently being eliminated.
 *
 *  >>> Local Variables:
 *  pMarkowitzProduct  (long *)
 *      Pointer that points into MarkowitzProduct array. Is used to
 *      sequentially access entries quickly.
 *  pMarkowitzRow  (int *)
 *      Pointer that points into MarkowitzRow array. Is used to sequentially
 *      access entries quickly.
 *  pMarkowitzCol  (int *)
 *      Pointer that points into MarkowitzCol array. Is used to sequentially
 *      access entries quickly.
 *  Product  (long)
 *      Temporary storage for Markowitz product./
 *  Size  (int)
 *      The size of the matrix.
 */

static void
MarkowitzProducts(
    MatrixPtr Matrix,
    int Step
)
{
register  int  I, *pMarkowitzRow, *pMarkowitzCol;
register  long  Product, *pMarkowitzProduct;
register  int  Size = Matrix->Size;
double fProduct;

/* Begin `MarkowitzProducts'. */
    Matrix->Singletons = 0;

    pMarkowitzProduct = &(Matrix->MarkowitzProd[Step]);
    pMarkowitzRow = &(Matrix->MarkowitzRow[Step]);
    pMarkowitzCol = &(Matrix->MarkowitzCol[Step]);

    for (I = Step; I <= Size; I++)
    {
/* If chance of overflow, use real numbers. */
        if ((*pMarkowitzRow > LARGEST_SHORT_INTEGER AND *pMarkowitzCol != 0) OR
            (*pMarkowitzCol > LARGEST_SHORT_INTEGER AND *pMarkowitzRow != 0))
        {   fProduct = (double)(*pMarkowitzRow++) * (double)(*pMarkowitzCol++);
            if (fProduct >= LARGEST_LONG_INTEGER)
                *pMarkowitzProduct++ = LARGEST_LONG_INTEGER;
            else
                *pMarkowitzProduct++ = (long)fProduct;
        }
        else
        {   Product = *pMarkowitzRow++ * *pMarkowitzCol++;
            if ((*pMarkowitzProduct++ = Product) == 0)
                Matrix->Singletons++;
        }
    }
    return;
}


/*
 *  SEARCH FOR BEST PIVOT
 *
 *  Performs a search to determine the element with the lowest Markowitz
 *  Product that is also acceptable.  An acceptable element is one that is
 *  larger than the AbsThreshold and at least as large as RelThreshold times
 *  the largest element in the same column.  The first step is to look for
 *  singletons if any exist.  If none are found, then all the diagonals are
 *  searched. The diagonal is searched once quickly using the assumption that
 *  elements on the diagonal are large compared to other elements in their
 *  column, and so the pivot can be chosen only on the basis of the Markowitz
 *  criterion.  After a element has been chosen to be pivot on the basis of
 *  its Markowitz product, it is checked to see if it is large enough.
 *  Waiting to the end of the Markowitz search to check the size of a pivot
 *  candidate saves considerable time, but is not guaranteed to find an
 *  acceptable pivot.  Thus if unsuccessful a second pass of the diagonal is
 *  made.  This second pass checks to see if an element is large enough during
 *  the search, not after it.  If still no acceptable pivot candidate has
 *  been found, the search expands to cover the entire matrix.
 *
 *  >>> Returned:
 *  A pointer to the element chosen to be pivot.  If every element in the
 *  matrix is zero, then NULL is returned.
 *
 *  >>> Arguments:
 *  Matrix  <input>  (MatrixPtr)
 *      Pointer to matrix.
 *  Step  <input>  (int)
 *      The row and column number of the beginning of the reduced submatrix.
 *
 *  >>> Local variables:
 *  ChosenPivot  (ElementPtr)
 *      Pointer to element that has been chosen to be the pivot.
 *
 *  >>> Possible errors:
 *  spSINGULAR
 *  spSMALL_PIVOT
 */

static ElementPtr
SearchForPivot(
    MatrixPtr Matrix,
    int Step,
    BOOLEAN DiagPivoting
)
{
register ElementPtr  ChosenPivot;

/* Begin `SearchForPivot'. */

/* If singletons exist, look for an acceptable one to use as pivot. */
    if (Matrix->Singletons)
    {   ChosenPivot = SearchForSingleton( Matrix, Step );
        if (ChosenPivot != NULL)
        {   Matrix->PivotSelectionMethod = 's';
            return ChosenPivot;
        }
    }

#if DIAGONAL_PIVOTING
    if (DiagPivoting)
    {
/*
 * Either no singletons exist or they weren't acceptable.  Take quick first
 * pass at searching diagonal.  First search for element on diagonal of
 * remaining submatrix with smallest Markowitz product, then check to see
 * if it okay numerically.  If not, QuicklySearchDiagonal fails.
 */
        ChosenPivot = QuicklySearchDiagonal( Matrix, Step );
        if (ChosenPivot != NULL)
        {   Matrix->PivotSelectionMethod = 'q';
            return ChosenPivot;
        }

/*
 * Quick search of diagonal failed, carefully search diagonal and check each
 * pivot candidate numerically before even tentatively accepting it.
 */
        ChosenPivot = SearchDiagonal( Matrix, Step );
        if (ChosenPivot != NULL)
        {   Matrix->PivotSelectionMethod = 'd';
            return ChosenPivot;
        }
    }
#endif /* DIAGONAL_PIVOTING */

/* No acceptable pivot found yet, search entire matrix. */
    ChosenPivot = SearchEntireMatrix( Matrix, Step );
    Matrix->PivotSelectionMethod = 'e';

    return ChosenPivot;
}









/*
 *  SEARCH FOR SINGLETON TO USE AS PIVOT
 *
 *  Performs a search to find a singleton to use as the pivot.  The
 *  first acceptable singleton is used.  A singleton is acceptable if
 *  it is larger in magnitude than the AbsThreshold and larger
 *  than RelThreshold times the largest of any other elements in the same
 *  column.  It may seem that a singleton need not satisfy the
 *  relative threshold criterion, however it is necessary to prevent
 *  excessive growth in the RHS from resulting in overflow during the
 *  forward and backward substitution.  A singleton does not need to
 *  be on the diagonal to be selected.
 *
 *  >>> Returned:
 *  A pointer to the singleton chosen to be pivot.  In no singleton is
 *  acceptable, return NULL.
 *
 *  >>> Arguments:
 *  Matrix  <input>  (MatrixPtr)
 *      Pointer to matrix.
 *  Step  <input>  (int)
 *      Index of the diagonal currently being eliminated.
 *
 *  >>> Local variables:
 *  ChosenPivot  (ElementPtr)
 *      Pointer to element that has been chosen to be the pivot.
 *  PivotMag  (RealNumber)
 *      Magnitude of ChosenPivot.
 *  Singletons  (int)
 *      The count of the number of singletons that can be used as pivots.
 *      A local version of Matrix->Singletons.
 *  pMarkowitzProduct  (long *)
 *      Pointer that points into MarkowitzProduct array. It is used to quickly
 *      access successive Markowitz products.
 */

static ElementPtr
SearchForSingleton(
    MatrixPtr Matrix,
    int Step
)
{
register  ElementPtr  ChosenPivot;
register  int  I;
register  long  *pMarkowitzProduct;
int  Singletons;
RealNumber  PivotMag;

/* Begin `SearchForSingleton'. */
/* Initialize pointer that is to scan through MarkowitzProduct vector. */
    pMarkowitzProduct = &(Matrix->MarkowitzProd[Matrix->Size+1]);
    Matrix->MarkowitzProd[Matrix->Size+1] = Matrix->MarkowitzProd[Step];

/* Decrement the count of available singletons, on the assumption that an
 * acceptable one will be found. */
    Singletons = Matrix->Singletons--;

/*
 * Assure that following while loop will always terminate, this is just
 * preventive medicine, if things are working right this should never
 * be needed.
 */
    Matrix->MarkowitzProd[Step-1] = 0;

    while (Singletons-- > 0)
    {
/* Singletons exist, find them. */

/*
 * This is tricky.  Am using a pointer to sequentially step through the
 * MarkowitzProduct array.  Search terminates when singleton (Product = 0)
 * is found.  Note that the conditional in the while statement
 * ( *pMarkowitzProduct ) is true as long as the MarkowitzProduct is not
 * equal to zero.  The row (and column) index on the diagonal is then
 * calculated by subtracting the pointer to the Markowitz product of
 * the first diagonal from the pointer to the Markowitz product of the
 * desired element, the singleton.
 *
 * Search proceeds from the end (high row and column numbers) to the
 * beginning (low row and column numbers) so that rows and columns with
 * large Markowitz products will tend to be move to the bottom of the
 * matrix.  However, choosing Diag[Step] is desirable because it would
 * require no row and column interchanges, so inspect it first by
 * putting its Markowitz product at the end of the MarkowitzProd
 * vector.
 */

        while ( *pMarkowitzProduct-- )
        {   /*
             * N bottles of beer on the wall;
             * N bottles of beer.
             * you take one down and pass it around;
             * N-1 bottles of beer on the wall.
             */
        }
        I = (int)(pMarkowitzProduct - Matrix->MarkowitzProd + 1);
        // No point leaving in ptrdiff_t since, matrix::size is int

/* Assure that I is valid. */
        if (I < Step) break;  /* while (Singletons-- > 0) */
        if (I > Matrix->Size) I = Step;

/* Singleton has been found in either/both row or/and column I. */
        if ((ChosenPivot = Matrix->Diag[I]) != NULL)
        {
/* Singleton lies on the diagonal. */
            PivotMag = ELEMENT_MAG(ChosenPivot);
            if
            (    PivotMag > Matrix->AbsThreshold AND
                 PivotMag > Matrix->RelThreshold *
                            FindBiggestInColExclude( Matrix, ChosenPivot, Step )
            ) return ChosenPivot;
        }
        else
        {
/* Singleton does not lie on diagonal, find it. */
            if (Matrix->MarkowitzCol[I] == 0)
            {   ChosenPivot = Matrix->FirstInCol[I];
                while ((ChosenPivot != NULL) AND (ChosenPivot->Row < Step))
                    ChosenPivot = ChosenPivot->NextInCol;
                if (ChosenPivot == NULL)
                {   /* Reduced column has no elements, matrix is singular. */
                    break;
                }
                PivotMag = ELEMENT_MAG( ChosenPivot );
                if
                (    PivotMag > Matrix->AbsThreshold AND
                     PivotMag > Matrix->RelThreshold *
                                FindBiggestInColExclude( Matrix, ChosenPivot,
                                                         Step )
                ) return ChosenPivot;
                else
                {   if (Matrix->MarkowitzRow[I] == 0)
                    {   ChosenPivot = Matrix->FirstInRow[I];
                        while((ChosenPivot != NULL) AND (ChosenPivot->Col<Step))
                            ChosenPivot = ChosenPivot->NextInRow;
                        if (ChosenPivot == NULL)
                        {/* Reduced row has no elements, matrix is singular. */
                            break;
                        }
                        PivotMag = ELEMENT_MAG(ChosenPivot);
                        if
                        (    PivotMag > Matrix->AbsThreshold AND
                             PivotMag > Matrix->RelThreshold *
                                        FindBiggestInColExclude( Matrix,
                                                                 ChosenPivot,
                                                                 Step )
                        ) return ChosenPivot;
                    }
                }
            }
            else
            {   ChosenPivot = Matrix->FirstInRow[I];
                while ((ChosenPivot != NULL) AND (ChosenPivot->Col < Step))
                    ChosenPivot = ChosenPivot->NextInRow;
                if (ChosenPivot == NULL)
                {   /* Reduced row has no elements, matrix is singular. */
                    break;
                }
                PivotMag = ELEMENT_MAG(ChosenPivot);
                if
                (    PivotMag > Matrix->AbsThreshold AND
                     PivotMag > Matrix->RelThreshold *
                                FindBiggestInColExclude( Matrix, ChosenPivot,
                                                         Step )
                ) return ChosenPivot;
            }
        }
/* Singleton not acceptable (too small), try another. */
    } /* end of while(lSingletons>0) */

/*
 * All singletons were unacceptable.  Restore Matrix->Singletons count.
 * Initial assumption that an acceptable singleton would be found was wrong.
 */
    Matrix->Singletons++;
    return NULL;
}












#if DIAGONAL_PIVOTING
#if MODIFIED_MARKOWITZ
/*
 *  QUICK SEARCH OF DIAGONAL FOR PIVOT WITH MODIFIED MARKOWITZ CRITERION
 *
 *  Searches the diagonal looking for the best pivot.  For a pivot to be
 *  acceptable it must be larger than the pivot RelThreshold times the largest
 *  element in its reduced column.  Among the acceptable diagonals, the
 *  one with the smallest MarkowitzProduct is sought.  Search terminates
 *  early if a diagonal is found with a MarkowitzProduct of one and its
 *  magnitude is larger than the other elements in its row and column.
 *  Since its MarkowitzProduct is one, there is only one other element in
 *  both its row and column, and, as a condition for early termination,
 *  these elements must be located symmetricly in the matrix.  If a tie
 *  occurs between elements of equal MarkowitzProduct, then the element with
 *  the largest ratio between its magnitude and the largest element in its
 *  column is used.  The search will be terminated after a given number of
 *  ties have occurred and the best (largest ratio) of the tied element will
 *  be used as the pivot.  The number of ties that will trigger an early
 *  termination is MinMarkowitzProduct * TIES_MULTIPLIER.
 *
 *  >>> Returned:
 *  A pointer to the diagonal element chosen to be pivot.  If no diagonal is
 *  acceptable, a NULL is returned.
 *
 *  >>> Arguments:
 *  Step  <input>  (int)
 *      Index of the diagonal currently being eliminated.
 *
 *  >>> Local variables:
 *  ChosenPivot  (ElementPtr)
 *      Pointer to the element that has been chosen to be the pivot.
 *  LargestOffDiagonal  (RealNumber)
 *      Magnitude of the largest of the off-diagonal terms associated with
 *      a diagonal with MarkowitzProduct equal to one.
 *  Magnitude  (RealNumber)
 *      Absolute value of diagonal element.
 *  MaxRatio  (RealNumber)
 *      Among the elements tied with the smallest Markowitz product, MaxRatio
 *      is the best (smallest) ratio of LargestInCol to the diagonal Magnitude
 *      found so far.  The smaller the ratio, the better numerically the
 *      element will be as pivot.
 *  MinMarkowitzProduct  (long)
 *      Smallest Markowitz product found of pivot candidates that lie along
 *      diagonal.
 *  NumberOfTies  (int)
 *      A count of the number of Markowitz ties that have occurred at current
 *      MarkowitzProduct.
 *  pDiag  (ElementPtr)
 *      Pointer to current diagonal element.
 *  pMarkowitzProduct  (long *)
 *      Pointer that points into MarkowitzProduct array. It is used to quickly
 *      access successive Markowitz products.
 *  Ratio  (RealNumber)
 *      For the current pivot candidate, Ratio is the ratio of the largest
 *      element in its column (excluding itself) to its magnitude.
 *  TiedElements  (ElementPtr[])
 *      Array of pointers to the elements with the minimum Markowitz
 *      product.
 *  pOtherInCol  (ElementPtr)
 *      When there is only one other element in a column other than the
 *      diagonal, pOtherInCol is used to point to it.  Used when Markowitz
 *      product is to determine if off diagonals are placed symmetricly.
 *  pOtherInRow  (ElementPtr)
 *      When there is only one other element in a row other than the diagonal,
 *      pOtherInRow is used to point to it.  Used when Markowitz product is
 *      to determine if off diagonals are placed symmetricly.
 */

static ElementPtr
QuicklySearchDiagonal(
    MatrixPtr Matrix,
    int Step
)
{
register long  MinMarkowitzProduct, *pMarkowitzProduct;
register  ElementPtr  pDiag, pOtherInRow, pOtherInCol;
int  I, NumberOfTies;
ElementPtr  ChosenPivot, TiedElements[MAX_MARKOWITZ_TIES + 1];
RealNumber  Magnitude, LargestInCol, Ratio, MaxRatio;
RealNumber  LargestOffDiagonal;
RealNumber  FindBiggestInColExclude();

/* Begin `QuicklySearchDiagonal'. */
    NumberOfTies = -1;
    MinMarkowitzProduct = LARGEST_LONG_INTEGER;
    pMarkowitzProduct = &(Matrix->MarkowitzProd[Matrix->Size+2]);
    Matrix->MarkowitzProd[Matrix->Size+1] = Matrix->MarkowitzProd[Step];

/* Assure that following while loop will always terminate. */
    Matrix->MarkowitzProd[Step-1] = -1;

/*
 * This is tricky.  Am using a pointer in the inner while loop to
 * sequentially step through the MarkowitzProduct array.  Search
 * terminates when the Markowitz product of zero placed at location
 * Step-1 is found.  The row (and column) index on the diagonal is then
 * calculated by subtracting the pointer to the Markowitz product of
 * the first diagonal from the pointer to the Markowitz product of the
 * desired element. The outer for loop is infinite, broken by using
 * break.
 *
 * Search proceeds from the end (high row and column numbers) to the
 * beginning (low row and column numbers) so that rows and columns with
 * large Markowitz products will tend to be move to the bottom of the
 * matrix.  However, choosing Diag[Step] is desirable because it would
 * require no row and column interchanges, so inspect it first by
 * putting its Markowitz product at the end of the MarkowitzProd
 * vector.
 */

    for(;;)  /* Endless for loop. */
    {   while (MinMarkowitzProduct < *(--pMarkowitzProduct))
        {   /*
             * N bottles of beer on the wall;
             * N bottles of beer.
             * You take one down and pass it around;
             * N-1 bottles of beer on the wall.
             */
        }

        I = pMarkowitzProduct - Matrix->MarkowitzProd;

/* Assure that I is valid; if I < Step, terminate search. */
        if (I < Step) break; /* Endless for loop */
        if (I > Matrix->Size) I = Step;

        if ((pDiag = Matrix->Diag[I]) == NULL)
            continue; /* Endless for loop */
        if ((Magnitude = ELEMENT_MAG(pDiag)) <= Matrix->AbsThreshold)
            continue; /* Endless for loop */

        if (*pMarkowitzProduct == 1)
        {
/* Case where only one element exists in row and column other than diagonal. */

/* Find off diagonal elements. */
            pOtherInRow = pDiag->NextInRow;
            pOtherInCol = pDiag->NextInCol;
            if (pOtherInRow == NULL AND pOtherInCol == NULL)
            {    pOtherInRow = Matrix->FirstInRow[I];
                 while(pOtherInRow != NULL)
                 {   if (pOtherInRow->Col >= Step AND pOtherInRow->Col != I)
                         break;
                     pOtherInRow = pOtherInRow->NextInRow;
                 }
                 pOtherInCol = Matrix->FirstInCol[I];
                 while(pOtherInCol != NULL)
                 {   if (pOtherInCol->Row >= Step AND pOtherInCol->Row != I)
                         break;
                     pOtherInCol = pOtherInCol->NextInCol;
                 }
            }

/* Accept diagonal as pivot if diagonal is larger than off diagonals and the
 * off diagonals are placed symmetricly. */
            if (pOtherInRow != NULL  AND  pOtherInCol != NULL)
            {   if (pOtherInRow->Col == pOtherInCol->Row)
                {   LargestOffDiagonal = MAX(ELEMENT_MAG(pOtherInRow),
                                                      ELEMENT_MAG(pOtherInCol));
                    if (Magnitude >= LargestOffDiagonal)
                    {
/* Accept pivot, it is unlikely to contribute excess error. */
                        return pDiag;
                    }
                }
            }
        }

        if (*pMarkowitzProduct < MinMarkowitzProduct)
        {
/* Notice strict inequality in test. This is a new smallest MarkowitzProduct. */
            TiedElements[0] = pDiag;
            MinMarkowitzProduct = *pMarkowitzProduct;
            NumberOfTies = 0;
        }
        else
        {
/* This case handles Markowitz ties. */
            if (NumberOfTies < MAX_MARKOWITZ_TIES)
            {   TiedElements[++NumberOfTies] = pDiag;
                if (NumberOfTies >= MinMarkowitzProduct * TIES_MULTIPLIER)
                    break; /* Endless for loop */
            }
        }
    } /* End of endless for loop. */

/* Test to see if any element was chosen as a pivot candidate. */
    if (NumberOfTies < 0)
        return NULL;

/* Determine which of tied elements is best numerically. */
    ChosenPivot = NULL;
    MaxRatio = 1.0 / Matrix->RelThreshold;

    for (I = 0; I <= NumberOfTies; I++)
    {   pDiag = TiedElements[I];
        Magnitude = ELEMENT_MAG(pDiag);
        LargestInCol = FindBiggestInColExclude( Matrix, pDiag, Step );
        Ratio = LargestInCol / Magnitude;
        if (Ratio < MaxRatio)
        {   ChosenPivot = pDiag;
            MaxRatio = Ratio;
        }
    }
    return ChosenPivot;
}










#else /* Not MODIFIED_MARKOWITZ */
/*
 *  QUICK SEARCH OF DIAGONAL FOR PIVOT WITH CONVENTIONAL MARKOWITZ
 *  CRITERION
 *
 *  Searches the diagonal looking for the best pivot.  For a pivot to be
 *  acceptable it must be larger than the pivot RelThreshold times the largest
 *  element in its reduced column.  Among the acceptable diagonals, the
 *  one with the smallest MarkowitzProduct is sought.  Search terminates
 *  early if a diagonal is found with a MarkowitzProduct of one and its
 *  magnitude is larger than the other elements in its row and column.
 *  Since its MarkowitzProduct is one, there is only one other element in
 *  both its row and column, and, as a condition for early termination,
 *  these elements must be located symmetricly in the matrix.
 *
 *  >>> Returned:
 *  A pointer to the diagonal element chosen to be pivot.  If no diagonal is
 *  acceptable, a NULL is returned.
 *
 *  >>> Arguments:
 *  Matrix  <input>  (MatrixPtr)
 *      Pointer to matrix.
 *  Step  <input>  (int)
 *      Index of the diagonal currently being eliminated.
 *
 *  >>> Local variables:
 *  ChosenPivot  (ElementPtr)
 *      Pointer to the element that has been chosen to be the pivot.
 *  LargestOffDiagonal  (RealNumber)
 *      Magnitude of the largest of the off-diagonal terms associated with
 *      a diagonal with MarkowitzProduct equal to one.
 *  Magnitude  (RealNumber)
 *      Absolute value of diagonal element.
 *  MinMarkowitzProduct  (long)
 *      Smallest Markowitz product found of pivot candidates which are
 *      acceptable.
 *  pDiag  (ElementPtr)
 *      Pointer to current diagonal element.
 *  pMarkowitzProduct  (long *)
 *      Pointer that points into MarkowitzProduct array. It is used to quickly
 *      access successive Markowitz products.
 *  pOtherInCol  (ElementPtr)
 *      When there is only one other element in a column other than the
 *      diagonal, pOtherInCol is used to point to it.  Used when Markowitz
 *      product is to determine if off diagonals are placed symmetricly.
 *  pOtherInRow  (ElementPtr)
 *      When there is only one other element in a row other than the diagonal,
 *      pOtherInRow is used to point to it.  Used when Markowitz product is
 *      to determine if off diagonals are placed symmetricly.
 */

static ElementPtr
QuicklySearchDiagonal(
    MatrixPtr Matrix,
    int Step
)
{
register long  MinMarkowitzProduct, *pMarkowitzProduct;
register  ElementPtr  pDiag;
int  I;
ElementPtr  ChosenPivot, pOtherInRow, pOtherInCol;
RealNumber  Magnitude, LargestInCol, LargestOffDiagonal;
RealNumber  FindBiggestInColExclude();

/* Begin `QuicklySearchDiagonal'. */
    ChosenPivot = NULL;
    MinMarkowitzProduct = LARGEST_LONG_INTEGER;
    pMarkowitzProduct = &(Matrix->MarkowitzProd[Matrix->Size+2]);
    Matrix->MarkowitzProd[Matrix->Size+1] = Matrix->MarkowitzProd[Step];

/* Assure that following while loop will always terminate. */
    Matrix->MarkowitzProd[Step-1] = -1;

/*
 * This is tricky.  Am using a pointer in the inner while loop to
 * sequentially step through the MarkowitzProduct array.  Search
 * terminates when the Markowitz product of zero placed at location
 * Step-1 is found.  The row (and column) index on the diagonal is then
 * calculated by subtracting the pointer to the Markowitz product of
 * the first diagonal from the pointer to the Markowitz product of the
 * desired element. The outer for loop is infinite, broken by using
 * break.
 *
 * Search proceeds from the end (high row and column numbers) to the
 * beginning (low row and column numbers) so that rows and columns with
 * large Markowitz products will tend to be move to the bottom of the
 * matrix.  However, choosing Diag[Step] is desirable because it would
 * require no row and column interchanges, so inspect it first by
 * putting its Markowitz product at the end of the MarkowitzProd
 * vector.
 */

    for (;;)  /* Endless for loop. */
    {   while (*(--pMarkowitzProduct) >= MinMarkowitzProduct)
        {   /* Just passing through. */
        }

        I = (int)(pMarkowitzProduct - Matrix->MarkowitzProd);

/* Assure that I is valid; if I < Step, terminate search. */
        if (I < Step) break; /* Endless for loop */
        if (I > Matrix->Size) I = Step;

        if ((pDiag = Matrix->Diag[I]) == NULL)
            continue; /* Endless for loop */
        if ((Magnitude = ELEMENT_MAG(pDiag)) <= Matrix->AbsThreshold)
            continue; /* Endless for loop */

        if (*pMarkowitzProduct == 1)
        {
/* Case where only one element exists in row and column other than diagonal. */

/* Find off-diagonal elements. */
            pOtherInRow = pDiag->NextInRow;
            pOtherInCol = pDiag->NextInCol;
            if (pOtherInRow == NULL AND pOtherInCol == NULL)
            {    pOtherInRow = Matrix->FirstInRow[I];
                 while(pOtherInRow != NULL)
                 {   if (pOtherInRow->Col >= Step AND pOtherInRow->Col != I)
                         break;
                     pOtherInRow = pOtherInRow->NextInRow;
                 }
                 pOtherInCol = Matrix->FirstInCol[I];
                 while(pOtherInCol != NULL)
                 {   if (pOtherInCol->Row >= Step AND pOtherInCol->Row != I)
                         break;
                     pOtherInCol = pOtherInCol->NextInCol;
                 }
            }

/* Accept diagonal as pivot if diagonal is larger than off-diagonals and the
 * off-diagonals are placed symmetricly. */
            if (pOtherInRow != NULL  AND  pOtherInCol != NULL)
            {   if (pOtherInRow->Col == pOtherInCol->Row)
                {   LargestOffDiagonal = MAX(ELEMENT_MAG(pOtherInRow),
                                                      ELEMENT_MAG(pOtherInCol));
                    if (Magnitude >= LargestOffDiagonal)
                    {
/* Accept pivot, it is unlikely to contribute excess error. */
                        return pDiag;
                    }
                }
            }
        }

        MinMarkowitzProduct = *pMarkowitzProduct;
        ChosenPivot = pDiag;
    }  /* End of endless for loop. */

    if (ChosenPivot != NULL)
    {   LargestInCol = FindBiggestInColExclude( Matrix, ChosenPivot, Step );
        if( ELEMENT_MAG(ChosenPivot) <= Matrix->RelThreshold * LargestInCol )
            ChosenPivot = NULL;
    }
    return ChosenPivot;
}
#endif /* Not MODIFIED_MARKOWITZ */









/*
 *  SEARCH DIAGONAL FOR PIVOT WITH MODIFIED MARKOWITZ CRITERION
 *
 *  Searches the diagonal looking for the best pivot.  For a pivot to be
 *  acceptable it must be larger than the pivot RelThreshold times the largest
 *  element in its reduced column.  Among the acceptable diagonals, the
 *  one with the smallest MarkowitzProduct is sought.  If a tie occurs
 *  between elements of equal MarkowitzProduct, then the element with
 *  the largest ratio between its magnitude and the largest element in its
 *  column is used.  The search will be terminated after a given number of
 *  ties have occurred and the best (smallest ratio) of the tied element will
 *  be used as the pivot.  The number of ties that will trigger an early
 *  termination is MinMarkowitzProduct * TIES_MULTIPLIER.
 *
 *  >>> Returned:
 *  A pointer to the diagonal element chosen to be pivot.  If no diagonal is
 *  acceptable, a NULL is returned.
 *
 *  >>> Arguments:
 *  Matrix  <input>  (MatrixPtr)
 *      Pointer to matrix.
 *  Step  <input>  (int)
 *      Index of the diagonal currently being eliminated.
 *
 *  >>> Local variables:
 *  ChosenPivot  (ElementPtr)
 *      Pointer to the element that has been chosen to be the pivot.
 *  Size  (int)
 *      Local version of size which is placed in a register to increase speed.
 *  Magnitude  (RealNumber)
 *      Absolute value of diagonal element.
 *  MinMarkowitzProduct  (long)
 *      Smallest Markowitz product found of those pivot candidates which are
 *      acceptable.
 *  NumberOfTies  (int)
 *      A count of the number of Markowitz ties that have occurred at current
 *      MarkowitzProduct.
 *  pDiag  (ElementPtr)
 *      Pointer to current diagonal element.
 *  pMarkowitzProduct  (long*)
 *      Pointer that points into MarkowitzProduct array. It is used to quickly
 *      access successive Markowitz products.
 *  Ratio  (RealNumber)
 *      For the current pivot candidate, Ratio is the
 *      Ratio of the largest element in its column to its magnitude.
 *  RatioOfAccepted  (RealNumber)
 *      For the best pivot candidate found so far, RatioOfAccepted is the
 *      Ratio of the largest element in its column to its magnitude.
 */

static ElementPtr
SearchDiagonal(
    MatrixPtr Matrix,
    register int Step
)
{
register  int  J;
register long  MinMarkowitzProduct, *pMarkowitzProduct;
register  int  I;
register  ElementPtr  pDiag;
int  NumberOfTies=0, Size = Matrix->Size;
ElementPtr  ChosenPivot=0;
RealNumber  Magnitude=0, Ratio=0, RatioOfAccepted =0, LargestInCol=0;
RealNumber  FindBiggestInColExclude();

/* Begin `SearchDiagonal'. */
    ChosenPivot = NULL;
    MinMarkowitzProduct = LARGEST_LONG_INTEGER;
    pMarkowitzProduct = &(Matrix->MarkowitzProd[Size+2]);
    Matrix->MarkowitzProd[Size+1] = Matrix->MarkowitzProd[Step];

/* Start search of diagonal. */
    for (J = Size+1; J > Step; J--)
    {
        if (*(--pMarkowitzProduct) > MinMarkowitzProduct)
            continue; /* for loop */
        if (J > Matrix->Size)
            I = Step;
        else
            I = J;
        if ((pDiag = Matrix->Diag[I]) == NULL)
            continue; /* for loop */
        if ((Magnitude = ELEMENT_MAG(pDiag)) <= Matrix->AbsThreshold)
            continue; /* for loop */

/* Test to see if diagonal's magnitude is acceptable. */
        LargestInCol = FindBiggestInColExclude( Matrix, pDiag, Step );
        if (Magnitude <= Matrix->RelThreshold * LargestInCol)
            continue; /* for loop */

        if (*pMarkowitzProduct < MinMarkowitzProduct)
        {
/* Notice strict inequality in test. This is a new smallest MarkowitzProduct. */
            ChosenPivot = pDiag;
            MinMarkowitzProduct = *pMarkowitzProduct;
            RatioOfAccepted = LargestInCol / Magnitude;
            NumberOfTies = 0;
        }
        else
        {
/* This case handles Markowitz ties. */
            NumberOfTies++;
            Ratio = LargestInCol / Magnitude;
            if (Ratio < RatioOfAccepted)
            {   ChosenPivot = pDiag;
                RatioOfAccepted = Ratio;
            }
            if (NumberOfTies >= MinMarkowitzProduct * TIES_MULTIPLIER)
                return ChosenPivot;
        }
    } /* End of for(Step) */
    return ChosenPivot;
}
#endif /* DIAGONAL_PIVOTING */










/*
 *  SEARCH ENTIRE MATRIX FOR BEST PIVOT
 *
 *  Performs a search over the entire matrix looking for the acceptable
 *  element with the lowest MarkowitzProduct.  If there are several that
 *  are tied for the smallest MarkowitzProduct, the tie is broken by using
 *  the ratio of the magnitude of the element being considered to the largest
 *  element in the same column.  If no element is acceptable then the largest
 *  element in the reduced submatrix is used as the pivot and the
 *  matrix is declared to be spSMALL_PIVOT.  If the largest element is
 *  zero, the matrix is declared to be spSINGULAR.
 *
 *  >>> Returned:
 *  A pointer to the diagonal element chosen to be pivot.  If no element is
 *  found, then NULL is returned and the matrix is spSINGULAR.
 *
 *  >>> Arguments:
 *  Matrix  <input>  (MatrixPtr)
 *      Pointer to matrix.
 *  Step  <input>  (int)
 *      Index of the diagonal currently being eliminated.
 *
 *  >>> Local variables:
 *  ChosenPivot  (ElementPtr)
 *      Pointer to the element that has been chosen to be the pivot.
 *  LargestElementMag  (RealNumber)
 *      Magnitude of the largest element yet found in the reduced submatrix.
 *  Size  (int)
 *      Local version of Size; placed in a register for speed.
 *  Magnitude  (RealNumber)
 *      Absolute value of diagonal element.
 *  MinMarkowitzProduct  (long)
 *      Smallest Markowitz product found of pivot candidates which are
 *      acceptable.
 *  NumberOfTies  (int)
 *      A count of the number of Markowitz ties that have occurred at current
 *      MarkowitzProduct.
 *  pElement  (ElementPtr)
 *      Pointer to current element.
 *  pLargestElement  (ElementPtr)
 *      Pointer to the largest element yet found in the reduced submatrix.
 *  Product  (long)
 *      Markowitz product for the current row and column.
 *  Ratio  (RealNumber)
 *      For the current pivot candidate, Ratio is the
 *      Ratio of the largest element in its column to its magnitude.
 *  RatioOfAccepted  (RealNumber)
 *      For the best pivot candidate found so far, RatioOfAccepted is the
 *      Ratio of the largest element in its column to its magnitude.
 *
 *  >>> Possible errors:
 *  spSINGULAR
 *  spSMALL_PIVOT
 */

static ElementPtr
SearchEntireMatrix(
    MatrixPtr Matrix,
    int Step
)
{
register  int  I, Size = Matrix->Size;
register  ElementPtr  pElement;
int  NumberOfTies=0;
long  Product=0, MinMarkowitzProduct=0;
ElementPtr  ChosenPivot=0, pLargestElement=0;
RealNumber  Magnitude=0, LargestElementMag=0,
 Ratio=0, RatioOfAccepted=0, LargestInCol=0;
RealNumber  FindLargestInCol();

/* Begin `SearchEntireMatrix'. */
    ChosenPivot = NULL;
    LargestElementMag = 0.0;
    MinMarkowitzProduct = LARGEST_LONG_INTEGER;

/* Start search of matrix on column by column basis. */
    for (I = Step; I <= Size; I++)
    {   pElement = Matrix->FirstInCol[I];

        while (pElement != NULL AND pElement->Row < Step)
            pElement = pElement->NextInCol;

        if((LargestInCol = FindLargestInCol(pElement)) == 0.0)
            continue; /* for loop */

        while (pElement != NULL)
        {
/* Check to see if element is the largest encountered so far.  If so, record
   its magnitude and address. */
            if ((Magnitude = ELEMENT_MAG(pElement)) > LargestElementMag)
            {   LargestElementMag = Magnitude;
                pLargestElement = pElement;
            }
/* Calculate element's MarkowitzProduct. */
            spcMarkoProd( Product, Matrix->MarkowitzRow[pElement->Row],
                          Matrix->MarkowitzCol[pElement->Col] );

/* Test to see if element is acceptable as a pivot candidate. */
            if ((Product <= MinMarkowitzProduct) AND
                (Magnitude > Matrix->RelThreshold * LargestInCol) AND
                (Magnitude > Matrix->AbsThreshold))
            {
/* Test to see if element has lowest MarkowitzProduct yet found, or whether it
   is tied with an element found earlier. */
                if (Product < MinMarkowitzProduct)
                {
/* Notice strict inequality in test. This is a new smallest MarkowitzProduct. */
                    ChosenPivot = pElement;
                    MinMarkowitzProduct = Product;
                    RatioOfAccepted = LargestInCol / Magnitude;
                    NumberOfTies = 0;
                }
                else
                {
/* This case handles Markowitz ties. */
                    NumberOfTies++;
                    Ratio = LargestInCol / Magnitude;
                    if (Ratio < RatioOfAccepted)
                    {   ChosenPivot = pElement;
                        RatioOfAccepted = Ratio;
                    }
                    if (NumberOfTies >= MinMarkowitzProduct * TIES_MULTIPLIER)
                        return ChosenPivot;
                }
            }
            pElement = pElement->NextInCol;
        }  /* End of while(pElement != NULL) */
    } /* End of for(Step) */

    if (ChosenPivot != NULL) return ChosenPivot;

    if (LargestElementMag == 0.0)
    {   Matrix->Error = spSINGULAR;
        return NULL;
    }

    Matrix->Error = spSMALL_PIVOT;
    return pLargestElement;
}












/*
 *  DETERMINE THE MAGNITUDE OF THE LARGEST ELEMENT IN A COLUMN
 *
 *  This routine searches a column and returns the magnitude of the largest
 *  element.  This routine begins the search at the element pointed to by
 *  pElement, the parameter.
 *
 *  The search is conducted by starting at the element specified by a pointer,
 *  which should be one below the diagonal, and moving down the column.  On
 *  the way down the column, the magnitudes of the elements are tested to see
 *  if they are the largest yet found.
 *
 *  >>> Returned:
 *  The magnitude of the largest element in the column below and including
 *  the one pointed to by the input parameter.
 *
 *  >>> Arguments:
 *  pElement  <input>  (ElementPtr)
 *      The pointer to the first element to be tested.  Also, used by the
 *      routine to access all lower elements in the column.
 *
 *  >>> Local variables:
 *  Largest  (RealNumber)
 *      The magnitude of the largest element.
 *  Magnitude  (RealNumber)
 *      The magnitude of the currently active element.
 */

static RealNumber
FindLargestInCol( register ElementPtr pElement )
{
RealNumber  Magnitude, Largest = 0.0;

/* Begin `FindLargestInCol'. */
/* Search column for largest element beginning at Element. */
    while (pElement != NULL)
    {   if ((Magnitude = ELEMENT_MAG(pElement)) > Largest)
            Largest = Magnitude;
        pElement = pElement->NextInCol;
    }

    return Largest;
}










/*
 *  DETERMINE THE MAGNITUDE OF THE LARGEST ELEMENT IN A COLUMN
 *  EXCLUDING AN ELEMENT
 *
 *  This routine searches a column and returns the magnitude of the largest
 *  element.  One given element is specifically excluded from the search.
 *
 *  The search is conducted by starting at the first element in the column
 *  and moving down the column until the active part of the matrix is entered,
 *  i.e. the reduced submatrix.  The rest of the column is then traversed
 *  looking for the largest element.
 *
 *  >>> Returned:
 *  The magnitude of the largest element in the active portion of the column,
 *  excluding the specified element, is returned.
 *
 *  >>> Arguments:
 *  Matrix  <input>    (MatrixPtr)
 *      Pointer to the matrix.
 *  pElement  <input>  (ElementPtr)
 *      The pointer to the element that is to be excluded from search. Column
 *      to be searched is one that contains this element.  Also used to
 *      access the elements in the column.
 *  Step  <input>  (int)
 *      Index of the diagonal currently being eliminated.  Indicates where
 *      the active part of the matrix begins.
 *
 *  >>> Local variables:
 *  Col  (int)
 *      The number of the column to be searched.  Also the column number of
 *      the element to be avoided in the search.
 *  Largest  (RealNumber)
 *      The magnitude of the largest element.
 *  Magnitude  (RealNumber)
 *      The magnitude of the currently active element.
 *  Row  (int)
 *      The row number of element to be excluded from the search.
 */

static RealNumber
FindBiggestInColExclude(
    MatrixPtr Matrix,
    register ElementPtr pElement,
    register int Step
)
{
register  int  Row;
int  Col;
RealNumber  Largest, Magnitude;

/* Begin `FindBiggestInColExclude'. */
    Row = pElement->Row;
    Col = pElement->Col;
    pElement = Matrix->FirstInCol[Col];

/* Travel down column until reduced submatrix is entered. */
    while ((pElement != NULL) AND (pElement->Row < Step))
        pElement = pElement->NextInCol;

/* Initialize the variable Largest. */
    if (pElement->Row != Row)
        Largest = ELEMENT_MAG(pElement);
    else
        Largest = 0.0;

/* Search rest of column for largest element, avoiding excluded element. */
    while ((pElement = pElement->NextInCol) != NULL)
    {   if ((Magnitude = ELEMENT_MAG(pElement)) > Largest)
        {   if (pElement->Row != Row)
                Largest = Magnitude;
        }
    }

    return Largest;
}









/*
 *  EXCHANGE ROWS AND COLUMNS
 *
 *  Exchanges two rows and two columns so that the selected pivot is moved to
 *  the upper left corner of the remaining submatrix.
 *
 *  >>> Arguments:
 *  Matrix  <input>  (MatrixPtr)
 *      Pointer to the matrix.
 *  pPivot  <input>  (ElementPtr)
 *      Pointer to the current pivot.
 *  Step  <input>  (int)
 *      Index of the diagonal currently being eliminated.
 *
 *  >>> Local variables:
 *  Col  (int)
 *      Column where the pivot was found.
 *  Row  (int)
 *      Row where the pivot was found.
 *  OldMarkowitzProd_Col  (long)
 *      Markowitz product associated with the diagonal element in the row
 *      the pivot was found in.
 *  OldMarkowitzProd_Row  (long)
 *      Markowitz product associated with the diagonal element in the column
 *      the pivot was found in.
 *  OldMarkowitzProd_Step  (long)
 *      Markowitz product associated with the diagonal element that is being
 *      moved so that the pivot can be placed in the upper left-hand corner
 *      of the reduced submatrix.
 */

static void
ExchangeRowsAndCols(
    MatrixPtr Matrix,
    ElementPtr pPivot,
    register int Step
)
{
register  int   Row, Col;
long  OldMarkowitzProd_Step, OldMarkowitzProd_Row, OldMarkowitzProd_Col;

/* Begin `ExchangeRowsAndCols'. */
    Row = pPivot->Row;
    Col = pPivot->Col;
    Matrix->PivotsOriginalRow = Row;
    Matrix->PivotsOriginalCol = Col;

    if ((Row == Step) AND (Col == Step)) return;

/* Exchange rows and columns. */
    if (Row == Col)
    {   spcRowExchange( Matrix, Step, Row );
        spcColExchange( Matrix, Step, Col );
        SWAP( long, Matrix->MarkowitzProd[Step], Matrix->MarkowitzProd[Row] );
        SWAP( ElementPtr, Matrix->Diag[Row], Matrix->Diag[Step] );
    }
    else
    {

/* Initialize variables that hold old Markowitz products. */
        OldMarkowitzProd_Step = Matrix->MarkowitzProd[Step];
        OldMarkowitzProd_Row = Matrix->MarkowitzProd[Row];
        OldMarkowitzProd_Col = Matrix->MarkowitzProd[Col];

/* Exchange rows. */
        if (Row != Step)
        {   spcRowExchange( Matrix, Step, Row );
            Matrix->NumberOfInterchangesIsOdd =
                                       NOT Matrix->NumberOfInterchangesIsOdd;
            spcMarkoProd( Matrix->MarkowitzProd[Row],
                          Matrix->MarkowitzRow[Row],
                          Matrix->MarkowitzCol[Row] );

/* Update singleton count. */
            if ((Matrix->MarkowitzProd[Row]==0) != (OldMarkowitzProd_Row==0))
            {   if (OldMarkowitzProd_Row == 0)
                    Matrix->Singletons--;
                else
                    Matrix->Singletons++;
            }
        }

/* Exchange columns. */
        if (Col != Step)
        {   spcColExchange( Matrix, Step, Col );
            Matrix->NumberOfInterchangesIsOdd =
                                       NOT Matrix->NumberOfInterchangesIsOdd;
            spcMarkoProd( Matrix->MarkowitzProd[Col],
                              Matrix->MarkowitzCol[Col],
                          Matrix->MarkowitzRow[Col] );

/* Update singleton count. */
            if ((Matrix->MarkowitzProd[Col]==0) != (OldMarkowitzProd_Col==0))
            {   if (OldMarkowitzProd_Col == 0)
                    Matrix->Singletons--;
                else
                    Matrix->Singletons++;
            }

            Matrix->Diag[Col] = spcFindDiag( Matrix, Col );
        }
        if (Row != Step)
            Matrix->Diag[Row] = spcFindDiag( Matrix, Row );
        Matrix->Diag[Step] = spcFindDiag( Matrix, Step );

/* Update singleton count. */
        Matrix->MarkowitzProd[Step] = Matrix->MarkowitzCol[Step] *
                                                    Matrix->MarkowitzRow[Step];
        if ((Matrix->MarkowitzProd[Step]==0) != (OldMarkowitzProd_Step==0))
        {   if (OldMarkowitzProd_Step == 0)
                Matrix->Singletons--;
            else
                Matrix->Singletons++;
        }
    }
    return;
}









/*
 *  EXCHANGE ROWS
 *
 *  Performs all required operations to exchange two rows. Those operations
 *  include: swap FirstInRow pointers, fixing up the NextInCol pointers,
 *  swapping row indexes in MatrixElements, and swapping Markowitz row
 *  counts.
 *
 *  >>> Arguments:
 *  Matrix  <input>  (MatrixPtr)
 *      Pointer to the matrix.
 *  Row1  <input>  (int)
 *      Row index of one of the rows, becomes the smallest index.
 *  Row2  <input>  (int)
 *      Row index of the other row, becomes the largest index.
 *
 *  Local variables:
 *  Column  (int)
 *      Column in which row elements are currently being exchanged.
 *  Row1Ptr  (ElementPtr)
 *      Pointer to an element in Row1.
 *  Row2Ptr  (ElementPtr)
 *      Pointer to an element in Row2.
 *  Element1  (ElementPtr)
 *      Pointer to the element in Row1 to be exchanged.
 *  Element2  (ElementPtr)
 *      Pointer to the element in Row2 to be exchanged.
 */

void
spcRowExchange(
    MatrixPtr Matrix,
    int Row1,
    int Row2
)
{
register  ElementPtr  Row1Ptr, Row2Ptr;
int  Column;
ElementPtr  Element1, Element2;

/* Begin `spcRowExchange'. */
    if (Row1 > Row2)  SWAP(int, Row1, Row2);

    Row1Ptr = Matrix->FirstInRow[Row1];
    Row2Ptr = Matrix->FirstInRow[Row2];
    while (Row1Ptr != NULL OR Row2Ptr != NULL)
    {
/* Exchange elements in rows while traveling from left to right. */
        if (Row1Ptr == NULL)
        {   Column = Row2Ptr->Col;
            Element1 = NULL;
            Element2 = Row2Ptr;
            Row2Ptr = Row2Ptr->NextInRow;
        }
        else if (Row2Ptr == NULL)
        {   Column = Row1Ptr->Col;
            Element1 = Row1Ptr;
            Element2 = NULL;
            Row1Ptr = Row1Ptr->NextInRow;
        }
        else if (Row1Ptr->Col < Row2Ptr->Col)
        {   Column = Row1Ptr->Col;
            Element1 = Row1Ptr;
            Element2 = NULL;
            Row1Ptr = Row1Ptr->NextInRow;
        }
        else if (Row1Ptr->Col > Row2Ptr->Col)
        {   Column = Row2Ptr->Col;
            Element1 = NULL;
            Element2 = Row2Ptr;
            Row2Ptr = Row2Ptr->NextInRow;
        }
        else   /* Row1Ptr->Col == Row2Ptr->Col */
        {   Column = Row1Ptr->Col;
            Element1 = Row1Ptr;
            Element2 = Row2Ptr;
            Row1Ptr = Row1Ptr->NextInRow;
            Row2Ptr = Row2Ptr->NextInRow;
        }

        ExchangeColElements( Matrix, Row1, Element1, Row2, Element2, Column);
    }  /* end of while(Row1Ptr != NULL OR Row2Ptr != NULL) */

    if (Matrix->InternalVectorsAllocated)
        SWAP( int, Matrix->MarkowitzRow[Row1], Matrix->MarkowitzRow[Row2]);
    SWAP( ElementPtr, Matrix->FirstInRow[Row1], Matrix->FirstInRow[Row2]);
    SWAP( int, Matrix->IntToExtRowMap[Row1], Matrix->IntToExtRowMap[Row2]);
#if TRANSLATE
    Matrix->ExtToIntRowMap[ Matrix->IntToExtRowMap[Row1] ] = Row1;
    Matrix->ExtToIntRowMap[ Matrix->IntToExtRowMap[Row2] ] = Row2;
#endif

    return;
}









/*
 *  EXCHANGE COLUMNS
 *
 *  Performs all required operations to exchange two columns. Those operations
 *  include: swap FirstInCol pointers, fixing up the NextInRow pointers,
 *  swapping column indexes in MatrixElements, and swapping Markowitz
 *  column counts.
 *
 *  >>> Arguments:
 *  Matrix  <input>  (MatrixPtr)
 *      Pointer to the matrix.
 *  Col1  <input>  (int)
 *      Column index of one of the columns, becomes the smallest index.
 *  Col2  <input>  (int)
 *      Column index of the other column, becomes the largest index
 *
 *  Local variables:
 *  Row  (int)
 *      Row in which column elements are currently being exchanged.
 *  Col1Ptr  (ElementPtr)
 *      Pointer to an element in Col1.
 *  Col2Ptr  (ElementPtr)
 *      Pointer to an element in Col2.
 *  Element1  (ElementPtr)
 *      Pointer to the element in Col1 to be exchanged.
 *  Element2  (ElementPtr)
 *      Pointer to the element in Col2 to be exchanged.
 */

void
spcColExchange(
    MatrixPtr Matrix,
    int Col1,
    int Col2
)
{
register  ElementPtr  Col1Ptr, Col2Ptr;
int  Row;
ElementPtr  Element1, Element2;

/* Begin `spcColExchange'. */
    if (Col1 > Col2)  SWAP(int, Col1, Col2);

    Col1Ptr = Matrix->FirstInCol[Col1];
    Col2Ptr = Matrix->FirstInCol[Col2];
    while (Col1Ptr != NULL OR Col2Ptr != NULL)
    {
/* Exchange elements in rows while traveling from top to bottom. */
        if (Col1Ptr == NULL)
        {   Row = Col2Ptr->Row;
            Element1 = NULL;
            Element2 = Col2Ptr;
            Col2Ptr = Col2Ptr->NextInCol;
        }
        else if (Col2Ptr == NULL)
        {   Row = Col1Ptr->Row;
            Element1 = Col1Ptr;
            Element2 = NULL;
            Col1Ptr = Col1Ptr->NextInCol;
        }
        else if (Col1Ptr->Row < Col2Ptr->Row)
        {   Row = Col1Ptr->Row;
            Element1 = Col1Ptr;
            Element2 = NULL;
            Col1Ptr = Col1Ptr->NextInCol;
        }
        else if (Col1Ptr->Row > Col2Ptr->Row)
        {   Row = Col2Ptr->Row;
            Element1 = NULL;
            Element2 = Col2Ptr;
            Col2Ptr = Col2Ptr->NextInCol;
        }
        else   /* Col1Ptr->Row == Col2Ptr->Row */
        {   Row = Col1Ptr->Row;
            Element1 = Col1Ptr;
            Element2 = Col2Ptr;
            Col1Ptr = Col1Ptr->NextInCol;
            Col2Ptr = Col2Ptr->NextInCol;
        }

        ExchangeRowElements( Matrix, Col1, Element1, Col2, Element2, Row);
    }  /* end of while(Col1Ptr != NULL OR Col2Ptr != NULL) */

    if (Matrix->InternalVectorsAllocated)
        SWAP( int, Matrix->MarkowitzCol[Col1], Matrix->MarkowitzCol[Col2]);
    SWAP( ElementPtr, Matrix->FirstInCol[Col1], Matrix->FirstInCol[Col2]);
    SWAP( int, Matrix->IntToExtColMap[Col1], Matrix->IntToExtColMap[Col2]);
#if TRANSLATE
    Matrix->ExtToIntColMap[ Matrix->IntToExtColMap[Col1] ] = Col1;
    Matrix->ExtToIntColMap[ Matrix->IntToExtColMap[Col2] ] = Col2;
#endif

    return;
}







/*
 *  EXCHANGE TWO ELEMENTS IN A COLUMN
 *
 *  Performs all required operations to exchange two elements in a column.
 *  Those operations are: restring NextInCol pointers and swapping row indexes
 *  in the MatrixElements.
 *
 *  >>> Arguments:
 *  Matrix  <input>  (MatrixPtr)
 *      Pointer to the matrix.
 *  Row1  <input>  (int)
 *      Row of top element to be exchanged.
 *  Element1  <input>  (ElementPtr)
 *      Pointer to top element to be exchanged.
 *  Row2  <input>  (int)
 *      Row of bottom element to be exchanged.
 *  Element2  <input>  (ElementPtr)
 *      Pointer to bottom element to be exchanged.
 *  Column <input>  (int)
 *      Column that exchange is to take place in.
 *
 *  >>> Local variables:
 *  ElementAboveRow1  (ElementPtr *)
 *      Location of pointer which points to the element above Element1. This
 *      pointer is modified so that it points to correct element on exit.
 *  ElementAboveRow2  (ElementPtr *)
 *      Location of pointer which points to the element above Element2. This
 *      pointer is modified so that it points to correct element on exit.
 *  ElementBelowRow1  (ElementPtr)
 *      Pointer to element below Element1.
 *  ElementBelowRow2  (ElementPtr)
 *      Pointer to element below Element2.
 *  pElement  (ElementPtr)
 *      Pointer used to traverse the column.
 */

static void
ExchangeColElements(
    MatrixPtr Matrix,
    int  Row1,
    register ElementPtr Element1,
    int  Row2,
    register ElementPtr Element2,
    int  Column
)
{
ElementPtr  *ElementAboveRow1, *ElementAboveRow2;
ElementPtr  ElementBelowRow1, ElementBelowRow2;
register  ElementPtr  pElement;

/* Begin `ExchangeColElements'. */
/* Search to find the ElementAboveRow1. */
    ElementAboveRow1 = &(Matrix->FirstInCol[Column]);
    pElement = *ElementAboveRow1;
    while (pElement->Row < Row1)
    {   ElementAboveRow1 = &(pElement->NextInCol);
        pElement = *ElementAboveRow1;
    }
    if (Element1 != NULL)
    {   ElementBelowRow1 = Element1->NextInCol;
        if (Element2 == NULL)
        {
/* Element2 does not exist, move Element1 down to Row2. */
            if ( ElementBelowRow1 != NULL AND ElementBelowRow1->Row < Row2 )
            {
/* Element1 must be removed from linked list and moved. */
                *ElementAboveRow1 = ElementBelowRow1;

/* Search column for Row2. */
                pElement = ElementBelowRow1;
                do
                {   ElementAboveRow2 = &(pElement->NextInCol);
                    pElement = *ElementAboveRow2;
                }   while (pElement != NULL AND pElement->Row < Row2);

/* Place Element1 in Row2. */
                *ElementAboveRow2 = Element1;
                Element1->NextInCol = pElement;
                *ElementAboveRow1 =ElementBelowRow1;
            }
            Element1->Row = Row2;
        }
        else
        {
/* Element2 does exist, and the two elements must be exchanged. */
            if ( ElementBelowRow1->Row == Row2)
            {
/* Element2 is just below Element1, exchange them. */
                Element1->NextInCol = Element2->NextInCol;
                Element2->NextInCol = Element1;
                *ElementAboveRow1 = Element2;
            }
            else
            {
/* Element2 is not just below Element1 and must be searched for. */
                pElement = ElementBelowRow1;
                do
                {   ElementAboveRow2 = &(pElement->NextInCol);
                    pElement = *ElementAboveRow2;
                }   while (pElement->Row < Row2);

                ElementBelowRow2 = Element2->NextInCol;

/* Switch Element1 and Element2. */
                *ElementAboveRow1 = Element2;
                Element2->NextInCol = ElementBelowRow1;
                *ElementAboveRow2 = Element1;
                Element1->NextInCol = ElementBelowRow2;
            }
            Element1->Row = Row2;
            Element2->Row = Row1;
        }
    }
    else
    {
/* Element1 does not exist. */
        ElementBelowRow1 = pElement;

/* Find Element2. */
        if (ElementBelowRow1->Row != Row2)
        {   do
            {   ElementAboveRow2 = &(pElement->NextInCol);
                pElement = *ElementAboveRow2;
            }   while (pElement->Row < Row2);

//        ElementBelowRow2 = Element2->NextInCol;

/* Move Element2 to Row1. */
            *ElementAboveRow2 = Element2->NextInCol;
            *ElementAboveRow1 = Element2;
            Element2->NextInCol = ElementBelowRow1;
        }
        Element2->Row = Row1;
    }
    return;
}







/*
 *  EXCHANGE TWO ELEMENTS IN A ROW
 *
 *  Performs all required operations to exchange two elements in a row.
 *  Those operations are: restring NextInRow pointers and swapping column
 *  indexes in the MatrixElements.
 *
 *  >>> Arguments:
 *  Matrix  <input>  (MatrixPtr)
 *      Pointer to the matrix.
 *  Col1  <input>  (int)
 *      Col of left-most element to be exchanged.
 *  Element1  <input>  (ElementPtr)
 *      Pointer to left-most element to be exchanged.
 *  Col2  <input>  (int)
 *      Col of right-most element to be exchanged.
 *  Element2  <input>  (ElementPtr)
 *      Pointer to right-most element to be exchanged.
 *  Row <input>  (int)
 *      Row that exchange is to take place in.
 *
 *  >>> Local variables:
 *  ElementLeftOfCol1  (ElementPtr *)
 *      Location of pointer which points to the element to the left of
 *      Element1. This pointer is modified so that it points to correct
 *      element on exit.
 *  ElementLeftOfCol2  (ElementPtr *)
 *      Location of pointer which points to the element to the left of
 *      Element2. This pointer is modified so that it points to correct
 *      element on exit.
 *  ElementRightOfCol1  (ElementPtr)
 *      Pointer to element right of Element1.
 *  ElementRightOfCol2  (ElementPtr)
 *      Pointer to element right of Element2.
 *  pElement  (ElementPtr)
 *      Pointer used to traverse the row.
 */

static void
ExchangeRowElements(
    MatrixPtr Matrix,
    int  Col1,
    register ElementPtr Element1,
    int  Col2,
    register ElementPtr Element2,
    int  Row
)
{
ElementPtr  *ElementLeftOfCol1, *ElementLeftOfCol2;
ElementPtr  ElementRightOfCol1, ElementRightOfCol2;
register   ElementPtr  pElement;

/* Begin `ExchangeRowElements'. */
/* Search to find the ElementLeftOfCol1. */
    ElementLeftOfCol1 = &(Matrix->FirstInRow[Row]);
    pElement = *ElementLeftOfCol1;
    while (pElement->Col < Col1)
    {   ElementLeftOfCol1 = &(pElement->NextInRow);
        pElement = *ElementLeftOfCol1;
    }
    if (Element1 != NULL)
    {   ElementRightOfCol1 = Element1->NextInRow;
        if (Element2 == NULL)
        {
/* Element2 does not exist, move Element1 to right to Col2. */
            if ( ElementRightOfCol1 != NULL AND ElementRightOfCol1->Col < Col2 )
            {
/* Element1 must be removed from linked list and moved. */
                *ElementLeftOfCol1 = ElementRightOfCol1;

/* Search Row for Col2. */
                pElement = ElementRightOfCol1;
                do
                {   ElementLeftOfCol2 = &(pElement->NextInRow);
                    pElement = *ElementLeftOfCol2;
                }   while (pElement != NULL AND pElement->Col < Col2);

/* Place Element1 in Col2. */
                *ElementLeftOfCol2 = Element1;
                Element1->NextInRow = pElement;
                *ElementLeftOfCol1 =ElementRightOfCol1;
            }
            Element1->Col = Col2;
        }
        else
        {
/* Element2 does exist, and the two elements must be exchanged. */
            if ( ElementRightOfCol1->Col == Col2)
            {
/* Element2 is just right of Element1, exchange them. */
                Element1->NextInRow = Element2->NextInRow;
                Element2->NextInRow = Element1;
                *ElementLeftOfCol1 = Element2;
            }
            else
            {
/* Element2 is not just right of Element1 and must be searched for. */
                pElement = ElementRightOfCol1;
                do
                {   ElementLeftOfCol2 = &(pElement->NextInRow);
                    pElement = *ElementLeftOfCol2;
                }   while (pElement->Col < Col2);

                ElementRightOfCol2 = Element2->NextInRow;

/* Switch Element1 and Element2. */
                *ElementLeftOfCol1 = Element2;
                Element2->NextInRow = ElementRightOfCol1;
                *ElementLeftOfCol2 = Element1;
                Element1->NextInRow = ElementRightOfCol2;
            }
            Element1->Col = Col2;
            Element2->Col = Col1;
        }
    }
    else
    {
/* Element1 does not exist. */
        ElementRightOfCol1 = pElement;

/* Find Element2. */
        if (ElementRightOfCol1->Col != Col2)
        {   do
            {   ElementLeftOfCol2 = &(pElement->NextInRow);
                pElement = *ElementLeftOfCol2;
            }   while (pElement->Col < Col2);

//            ElementRightOfCol3 = Element2->NextInRow;

/* Move Element2 to Col1. */
            *ElementLeftOfCol2 = Element2->NextInRow;
            *ElementLeftOfCol1 = Element2;
            Element2->NextInRow = ElementRightOfCol1;
        }
        Element2->Col = Col1;
    }
    return;
}











/*
 *  PERFORM ROW AND COLUMN ELIMINATION ON REAL MATRIX
 *
 *  Eliminates a single row and column of the matrix and leaves single row of
 *  the upper triangular matrix and a single column of the lower triangular
 *  matrix in its wake.  Uses Gauss's method.
 *
 *  >>> Argument:
 *  Matrix  <input>  (MatrixPtr)
 *      Pointer to the matrix.
 *  pPivot  <input>  (ElementPtr)
 *      Pointer to the current pivot.
 *
 *  >>> Local variables:
 *  pLower  (ElementPtr)
 *      Points to matrix element in lower triangular column.
 *  pSub (ElementPtr)
 *      Points to elements in the reduced submatrix.
 *  Row  (int)
 *      Row index.
 *  pUpper  (ElementPtr)
 *      Points to matrix element in upper triangular row.
 *
 *  >>> Possible errors:
 *  spNO_MEMORY
 */

static void
RealRowColElimination(
    MatrixPtr Matrix,
    register ElementPtr pPivot
)
{
#if REAL
register  ElementPtr  pSub, *ppAbove;
register  int  Row;
register  ElementPtr  pLower, pUpper;

/* Begin `RealRowColElimination'. */

/* Test for zero pivot. */
    if (ABS(pPivot->Real) == 0.0)
    {   (void)MatrixIsSingular( Matrix, pPivot->Row );
        return;
    }
    pPivot->Real = 1.0 / pPivot->Real;

    pUpper = pPivot->NextInRow;
    while (pUpper != NULL)
    {
/* Calculate upper triangular element. */
        pUpper->Real *= pPivot->Real;

        pSub = pUpper->NextInCol;
        pLower = pPivot->NextInCol;
        ppAbove = &pUpper->NextInCol;
        while (pLower != NULL)
        {   Row = pLower->Row;

/* Find element in row that lines up with current lower triangular element. */
            while (pSub != NULL AND pSub->Row < Row)
            {   ppAbove = &pSub->NextInCol;
                pSub = pSub->NextInCol;
            }

/* Test to see if desired element was not found, if not, create fill-in. */
            if (pSub == NULL OR pSub->Row > Row)
            {   pSub = spcCreateElement( Matrix, Row, pUpper->Col,
                                         &pLower->NextInRow, ppAbove, YES );
                if (pSub == NULL)
                {   Matrix->Error = spNO_MEMORY;
                    return;
                }
            }
            pSub->Real -= pUpper->Real * pLower->Real;
            pSub = pSub->NextInCol;
            pLower = pLower->NextInCol;
        }
        pUpper = pUpper->NextInRow;
    }
    return;
#endif /* REAL */
}









/*
 *  PERFORM ROW AND COLUMN ELIMINATION ON COMPLEX MATRIX
 *
 *  Eliminates a single row and column of the matrix and leaves single row of
 *  the upper triangular matrix and a single column of the lower triangular
 *  matrix in its wake.  Uses Gauss's method.
 *
 *  >>> Argument:
 *  Matrix  <input>  (MatrixPtr)
 *      Pointer to the matrix.
 *  pPivot  <input>  (ElementPtr)
 *      Pointer to the current pivot.
 *
 *  >>> Local variables:
 *  pLower  (ElementPtr)
 *      Points to matrix element in lower triangular column.
 *  pSub (ElementPtr)
 *      Points to elements in the reduced submatrix.
 *  Row  (int)
 *      Row index.
 *  pUpper  (ElementPtr)
 *      Points to matrix element in upper triangular row.
 *
 *  Possible errors:
 *  spNO_MEMORY
 */
#if spCOMPLEX
static void
ComplexRowColElimination(
    MatrixPtr Matrix,
    register ElementPtr pPivot
)
{
register  ElementPtr  pSub, *ppAbove;
register  int  Row;
register  ElementPtr  pLower, pUpper;

/* Begin `ComplexRowColElimination'. */

/* Test for zero pivot. */
    if (ELEMENT_MAG(pPivot) == 0.0)
    {   (void)MatrixIsSingular( Matrix, pPivot->Row );
        return;
    }
    CMPLX_RECIPROCAL(*pPivot, *pPivot);

    pUpper = pPivot->NextInRow;
    while (pUpper != NULL)
    {
/* Calculate upper triangular element. */
/* Cmplx expr: *pUpper = *pUpper * (1.0 / *pPivot). */
        CMPLX_MULT_ASSIGN(*pUpper, *pPivot);

        pSub = pUpper->NextInCol;
        pLower = pPivot->NextInCol;
        ppAbove = &pUpper->NextInCol;
        while (pLower != NULL)
        {   Row = pLower->Row;

/* Find element in row that lines up with current lower triangular element. */
            while (pSub != NULL AND pSub->Row < Row)
            {   ppAbove = &pSub->NextInCol;
                pSub = pSub->NextInCol;
            }

/* Test to see if desired element was not found, if not, create fill-in. */
            if (pSub == NULL OR pSub->Row > Row)
            {   pSub = spcCreateElement( Matrix, Row, pUpper->Col,
                                         &pLower->NextInRow, ppAbove, YES );
                if (pSub == NULL)
                {   Matrix->Error = spNO_MEMORY;
                    return;
                }
            }

/* Cmplx expr: pElement -= *pUpper * pLower. */
            CMPLX_MULT_SUBT_ASSIGN(*pSub, *pUpper, *pLower);
            pSub = pSub->NextInCol;
            pLower = pLower->NextInCol;
        }
        pUpper = pUpper->NextInRow;
    }
    return;
}
#endif /* spCOMPLEX */




/*
 *  UPDATE MARKOWITZ NUMBERS
 *
 *  Updates the Markowitz numbers after a row and column have been eliminated.
 *  Also updates singleton count.
 *
 *  >>> Argument:
 *  Matrix  <input>  (MatrixPtr)
 *      Pointer to the matrix.
 *  pPivot  <input>  (ElementPtr)
 *      Pointer to the current pivot.
 *
 *  >>> Local variables:
 *  Row  (int)
 *      Row index.
 *  Col  (int)
 *      Column index.
 *  ColPtr  (ElementPtr)
 *      Points to matrix element in upper triangular column.
 *  RowPtr  (ElementPtr)
 *      Points to matrix element in lower triangular row.
 */

static void
UpdateMarkowitzNumbers(
    MatrixPtr Matrix,
    ElementPtr pPivot
)
{
register  int  Row, Col;
register  ElementPtr  ColPtr, RowPtr;
register  int *MarkoRow = Matrix->MarkowitzRow, *MarkoCol = Matrix->MarkowitzCol;
double Product;

/* Begin `UpdateMarkowitzNumbers'. */

/* Update Markowitz numbers. */
    for (ColPtr = pPivot->NextInCol; ColPtr != NULL; ColPtr = ColPtr->NextInCol)
    {   Row = ColPtr->Row;
        --MarkoRow[Row];

/* Form Markowitz product while being cautious of overflows. */
        if ((MarkoRow[Row] > LARGEST_SHORT_INTEGER AND MarkoCol[Row] != 0) OR
            (MarkoCol[Row] > LARGEST_SHORT_INTEGER AND MarkoRow[Row] != 0))
        {   Product = (double)MarkoCol[Row] * (double)MarkoRow[Row];
            if (Product >= LARGEST_LONG_INTEGER)
                Matrix->MarkowitzProd[Row] = LARGEST_LONG_INTEGER;
            else
                Matrix->MarkowitzProd[Row] = (long)Product;
        }
        else Matrix->MarkowitzProd[Row] = MarkoRow[Row] * MarkoCol[Row];
        if (MarkoRow[Row] == 0)
            Matrix->Singletons++;
    }

    for (RowPtr = pPivot->NextInRow; RowPtr != NULL; RowPtr = RowPtr->NextInRow)
    {   Col = RowPtr->Col;
        --MarkoCol[Col];

/* Form Markowitz product while being cautious of overflows. */
        if ((MarkoRow[Col] > LARGEST_SHORT_INTEGER AND MarkoCol[Col] != 0) OR
            (MarkoCol[Col] > LARGEST_SHORT_INTEGER AND MarkoRow[Col] != 0))
        {   Product = (double)MarkoCol[Col] * (double)MarkoRow[Col];
            if (Product >= LARGEST_LONG_INTEGER)
                Matrix->MarkowitzProd[Col] = LARGEST_LONG_INTEGER;
            else
                Matrix->MarkowitzProd[Col] = (long)Product;
        }
        else Matrix->MarkowitzProd[Col] = MarkoRow[Col] * MarkoCol[Col];
        if ((MarkoCol[Col] == 0) AND (MarkoRow[Col] != 0))
            Matrix->Singletons++;
    }
    return;
}






/*
 *  ZERO PIVOT ENCOUNTERED
 *
 *  This routine is called when a singular matrix is found.  It then
 *  records the current row and column and exits.
 *
 *  >>> Returned:
 *  The error code spSINGULAR or spZERO_DIAG is returned.
 *
 *  >>> Arguments:
 *  Matrix  <input>  (MatrixPtr)
 *      Pointer to matrix.
 *  Step  <input>  (int)
 *      Index of diagonal that is zero.
 */

static int
MatrixIsSingular(
    MatrixPtr Matrix,
    int Step
)
{
/* Begin `MatrixIsSingular'. */

    Matrix->SingularRow = Matrix->IntToExtRowMap[ Step ];
    Matrix->SingularCol = Matrix->IntToExtColMap[ Step ];
    return (Matrix->Error = spSINGULAR);
}


static int
ZeroPivot(
    MatrixPtr Matrix,
    int Step
)
{
/* Begin `ZeroPivot'. */

    Matrix->SingularRow = Matrix->IntToExtRowMap[ Step ];
    Matrix->SingularCol = Matrix->IntToExtColMap[ Step ];
    return (Matrix->Error = spZERO_DIAG);
}






#if (ANNOTATE == FULL)

/*
 *
 *  WRITE STATUS
 *
 *  Write a summary of important variables to standard output.
 */

static void
WriteStatus(
    MatrixPtr Matrix,
    int Step
)
{
int  I;

/* Begin `WriteStatus'. */

    printf("Step = %1d   ", Step);
    printf("Pivot found at %1d,%1d using ", Matrix->PivotsOriginalRow,
                                            Matrix->PivotsOriginalCol);
    switch(Matrix->PivotSelectionMethod)
    {   case 's': printf("SearchForSingleton\n");  break;
        case 'q': printf("QuicklySearchDiagonal\n");  break;
        case 'd': printf("SearchDiagonal\n");  break;
        case 'e': printf("SearchEntireMatrix\n");  break;
    }

    printf("MarkowitzRow     = ");
    for (I = 1; I <= Matrix->Size; I++)
        printf("%2d  ", Matrix->MarkowitzRow[I]);
    printf("\n");

    printf("MarkowitzCol     = ");
    for (I = 1; I <= Matrix->Size; I++)
        printf("%2d  ", Matrix->MarkowitzCol[I]);
    printf("\n");

    printf("MarkowitzProduct = ");
    for (I = 1; I <= Matrix->Size; I++)
        printf("%2d  ", Matrix->MarkowitzProd[I]);
    printf("\n");

    printf("Singletons = %2d\n", Matrix->Singletons);

    printf("IntToExtRowMap     = ");
    for (I = 1; I <= Matrix->Size; I++)
        printf("%2d  ", Matrix->IntToExtRowMap[I]);
    printf("\n");

    printf("IntToExtColMap     = ");
    for (I = 1; I <= Matrix->Size; I++)
        printf("%2d  ", Matrix->IntToExtColMap[I]);
    printf("\n");

    printf("ExtToIntRowMap     = ");
    for (I = 1; I <= Matrix->ExtSize; I++)
        printf("%2d  ", Matrix->ExtToIntRowMap[I]);
    printf("\n");

    printf("ExtToIntColMap     = ");
    for (I = 1; I <= Matrix->ExtSize; I++)
        printf("%2d  ", Matrix->ExtToIntColMap[I]);
    printf("\n\n");

/*  spPrint((char *)Matrix, NO, YES);  */

    return;

}
#endif /* ANNOTATE == FULL */
