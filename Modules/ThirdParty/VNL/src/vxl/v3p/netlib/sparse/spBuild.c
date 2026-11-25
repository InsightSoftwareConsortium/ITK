/*
 *  MATRIX BUILD MODULE
 *
 *  Author:                     Advising professor:
 *     Kenneth S. Kundert           Alberto Sangiovanni-Vincentelli
 *     UC Berkeley
 */
/*!\file
 *  This file contains the routines associated with clearing, loading and
 *  preprocessing the matrix.
 *
 *  Objects that begin with the \a spc prefix are considered private
 *  and should not be used.
 *
 *  \author
 *  Kenneth S. Kundert <kundert@users.sourceforge.net>
 */
/*  >>> User accessible functions contained in this file:
 *  spClear
 *  spFindElement
 *  spGetElement
 *  spGetAdmittance
 *  spGetQuad
 *  spGetOnes
 *  spInstallInitInfo
 *  spGetInitInfo
 *  spInitialize
 *
 *  >>> Other functions contained in this file:
 *  Translate
 *  spcFindDiag
 *  spcCreateElement
 *  spcLinkRows
 *  EnlargeMatrix
 *  ExpandTranslationArrays
 */


/*
 *  Revision and copyright information.
 *
 *  Copyright (c) 1985-2003 by Kenneth S. Kundert
 */


/*
 *  IMPORTS
 *
 *  >>> Import descriptions:
 *  spConfig.h
 *     Macros that customize the sparse matrix routines.
 *  spMatrix.h
 *     Macros and declarations to be imported by the user.
 *  spDefs.h
 *     Matrix type and macro definitions for the sparse matrix routines.
 */

#define spINSIDE_SPARSE
#include <stdio.h>
#include "spConfig.h"
#include "spMatrix.h"
#include "spDefs.h"





/*
 *  Function declarations
 */

static void Translate( MatrixPtr, int*, int* );
static void EnlargeMatrix( MatrixPtr, int );
static void ExpandTranslationArrays( MatrixPtr, int );






/*!
 *  Sets every element of the matrix to zero and clears the error flag.
 *
 *  \param eMatrix
 *     Pointer to matrix that is to be cleared.
 */
/*  >>> Local variables:
 *  pElement  (ElementPtr)
 *     A pointer to the element being cleared.
 */

void
spClear( spMatrix eMatrix )
{
MatrixPtr  Matrix = (MatrixPtr)eMatrix;
register  ElementPtr  pElement;
register  int  I;

/* Begin `spClear'. */
    ASSERT_IS_SPARSE( Matrix );

/* Clear matrix. */
#if spCOMPLEX
    if (Matrix->PreviousMatrixWasComplex OR Matrix->Complex)
    {   for (I = Matrix->Size; I > 0; I--)
        {   pElement = Matrix->FirstInCol[I];
            while (pElement != NULL)
            {   pElement->Real = 0.0;
                pElement->Imag = 0.0;
                pElement = pElement->NextInCol;
            }
        }
    }
    else
#endif
    {   for (I = Matrix->Size; I > 0; I--)
        {   pElement = Matrix->FirstInCol[I];
            while (pElement != NULL)
            {   pElement->Real = 0.0;
                pElement = pElement->NextInCol;
            }
        }
    }

/* Empty the trash. */
    Matrix->TrashCan.Real = 0.0;
#if spCOMPLEX
    Matrix->TrashCan.Imag = 0.0;
#endif

    Matrix->Error = spOKAY;
    Matrix->Factored = NO;
    Matrix->SingularCol = 0;
    Matrix->SingularRow = 0;
    Matrix->PreviousMatrixWasComplex = Matrix->Complex;
    return;
}










/*!
 *  This routine is used to find an element given its indices.  It will not
 *  create it if it does not exist.
 *
 *  \return
 *  A pointer to the desired element, or \a NULL if it does not exist.
 *
 *  \param eMatrix
 *        Pointer to matrix.
 *  \param Row
 *      Row index for element.
 *  \param Col
 *      Column index for element.
 *
 *  \see spGetElement()
 */
/*  >>> Local variables:
 *  pElement  (ElementPtr)
 *      Pointer to an element in the matrix.
 */

spElement *
spFindElement(
    spMatrix eMatrix,
    int Row,
    int Col
)
{
MatrixPtr  Matrix = (MatrixPtr)eMatrix;
register ElementPtr  pElement;
int StartAt=0, Min = LARGEST_INTEGER;
#define BorderRight 0        /* Start at left border, move right. */
#define BorderDown  1        /* Start at top border, move down. */
#define DiagRight   2        /* Start at diagonal, move right. */
#define DiagDown    3        /* Start at diagonal, move down. */

/* Begin `spFindElement'. */
    if (Row == Col) return &Matrix->Diag[Row]->Real;

/* Determine where to start the search. */
    if (Matrix->RowsLinked)
    {   if ((Col >= Row) AND Matrix->Diag[Row])
        {   Min = Col - Row;
            StartAt = DiagRight;
        }
        else
        {   Min = Col;
            StartAt = BorderRight;
        }
    }
    if ((Row >= Col) AND Matrix->Diag[Col])
    {   if (Row - Col < Min)
            StartAt = DiagDown;
    }
    else if (Row < Min)
        StartAt = BorderDown;

/* Search column for element. */
    if ((StartAt == BorderDown) OR (StartAt == DiagDown))
    {   if (StartAt == BorderDown)
            pElement = Matrix->FirstInCol[Col];
        else
            pElement = Matrix->Diag[Col];

        while ((pElement != NULL) AND (pElement->Row < Row))
            pElement = pElement->NextInCol;
        if (pElement AND (pElement->Row == Row))
            return &pElement->Real;
        else
            return NULL;
    }

/* Search row for element. */
    if (StartAt == BorderRight)
        pElement = Matrix->FirstInRow[Row];
    else
        pElement = Matrix->Diag[Row];

    while ((pElement != NULL) AND (pElement->Col < Col))
        pElement = pElement->NextInRow;
    if (pElement AND (pElement->Col == Col))
        return &pElement->Real;
    else
        return NULL;
}









/*!
 *  Finds element [Row,Col] and returns a pointer to it.  If element is
 *  not found then it is created and spliced into matrix.  This routine
 *  is only to be used after spCreate() and before spMNA_Preorder(),
 *  spFactor() or spOrderAndFactor().  Returns a pointer to the
 *  real portion of an \a spElement.  This pointer is later used by
 *  \a spADD_xxx_ELEMENT to directly access element.
 *
 *  \return
 *  Returns a pointer to the element.  This pointer is then used to directly
 *  access the element during successive builds.
 *
 *  \param eMatrix
 *     Pointer to the matrix that the element is to be added to.
 *  \param Row
 *     Row index for element.  Must be in the range of [0..Size] unless
 *     the options \a EXPANDABLE or \a TRANSLATE are used. Elements placed in
 *     row zero are discarded.  In no case may \a Row be less than zero.
 *  \param Col
 *     Column index for element.  Must be in the range of [0..Size] unless
 *     the options \a EXPANDABLE or \a TRANSLATE are used. Elements placed in
 *     column zero are discarded.  In no case may \a Col be less than zero.

 *  \see spFindElement()
 */
/*  >>> Local variables:
 *  pElement  (RealNumber *)
 *     Pointer to the element.
 *
 *  >>> Possible errors:
 *  spNO_MEMORY
 *  Error is not cleared in this routine.
 */

spElement *
spGetElement(
    spMatrix eMatrix,
    int Row,
    int Col
)
{
MatrixPtr Matrix = (MatrixPtr)eMatrix;
ElementPtr pElement;

/* Begin `spGetElement'. */
    ASSERT_IS_SPARSE( Matrix );
    vASSERT( Row >= 0 AND Col >= 0, "Negative row or column number" );

    if ((Row == 0) OR (Col == 0))
        return &Matrix->TrashCan.Real;

#if NOT TRANSLATE
    vASSERT( NOT Matrix->Reordered,
             "Set TRANSLATE to add elements to a reordered matrix" );
#endif

#if TRANSLATE
    Translate( Matrix, &Row, &Col );
    if (Matrix->Error == spNO_MEMORY) return NULL;
#endif

#if NOT TRANSLATE
#if NOT EXPANDABLE
    vASSERT( (Row <= Matrix->Size) AND (Col <= Matrix->Size),
             "Row or column number too large" );
#endif

#if EXPANDABLE
/* Re-size Matrix if necessary. */
    if ((Row > Matrix->Size) OR (Col > Matrix->Size))
        EnlargeMatrix( Matrix, MAX(Row, Col) );
    if (Matrix->Error == spNO_MEMORY) return NULL;
#endif
#endif

    if ((Row != Col) OR ((pElement = Matrix->Diag[Row]) == NULL))
    {   /*
         * Element does not exist or does not reside along diagonal.  Search
         * for element and if it does not exist, create it.
         */
        pElement = spcCreateElement( Matrix, Row, Col,
                                     &(Matrix->FirstInRow[Row]),
                                     &(Matrix->FirstInCol[Col]), NO );
    }
/*
 * Cast pointer into a pointer to a RealNumber.  This requires that Real
 * be the first record in the MatrixElement structure.
 */
    return &pElement->Real;
}







#if TRANSLATE

/*
 *  TRANSLATE EXTERNAL INDICES TO INTERNAL
 *
 *  Convert internal row and column numbers to internal row and column numbers.
 *  Also updates Ext/Int maps.
 *
 *
 *  >>> Arguments:
 *  Matrix  <input>    (MatrixPtr)
 *      Pointer to the matrix.
 *  Row  <input/output>  (int *)
 *     Upon entry Row is either a external row number of an external node
 *     number.  Upon entry, the internal equivalent is supplied.
 *  Col  <input/output>  (int *)
 *     Upon entry Column is either a external column number of an external node
 *     number.  Upon entry, the internal equivalent is supplied.
 *
 *  >>> Local variables:
 *  ExtCol  (int)
 *     Temporary variable used to hold the external column or node number
 *     during the external to internal column number translation.
 *  ExtRow  (int)
 *     Temporary variable used to hold the external row or node number during
 *     the external to internal row number translation.
 *  IntCol  (int)
 *     Temporary variable used to hold the internal column or node number
 *     during the external to internal column number translation.
 *  IntRow  (int)
 *     Temporary variable used to hold the internal row or node number during
 *     the external to internal row number translation.
 */

static void
Translate(
    MatrixPtr Matrix,
    int *Row,
    int *Col
)
{
register int IntRow, IntCol, ExtRow, ExtCol;

/* Begin `Translate'. */
    ExtRow = *Row;
    ExtCol = *Col;

/* Expand translation arrays if necessary. */
    if ((ExtRow > Matrix->AllocatedExtSize) OR
        (ExtCol > Matrix->AllocatedExtSize))
    {
        ExpandTranslationArrays( Matrix, MAX(ExtRow, ExtCol) );
        if (Matrix->Error == spNO_MEMORY) return;
    }

/* Set ExtSize if necessary. */
    if ((ExtRow > Matrix->ExtSize) OR (ExtCol > Matrix->ExtSize))
        Matrix->ExtSize = MAX(ExtRow, ExtCol);

/* Translate external row or node number to internal row or node number. */
    if ((IntRow = Matrix->ExtToIntRowMap[ExtRow]) == -1)
    {   Matrix->ExtToIntRowMap[ExtRow] = ++Matrix->CurrentSize;
        Matrix->ExtToIntColMap[ExtRow] = Matrix->CurrentSize;
        IntRow = Matrix->CurrentSize;

#if NOT EXPANDABLE
        vASSERT( IntRow <= Matrix->Size, "Matrix size fixed" );
#endif

#if EXPANDABLE
/* Re-size Matrix if necessary. */
        if (IntRow > Matrix->Size)
            EnlargeMatrix( Matrix, IntRow );
        if (Matrix->Error == spNO_MEMORY) return;
#endif

        Matrix->IntToExtRowMap[IntRow] = ExtRow;
        Matrix->IntToExtColMap[IntRow] = ExtRow;
    }

/* Translate external column or node number to internal column or node number.*/
    if ((IntCol = Matrix->ExtToIntColMap[ExtCol]) == -1)
    {   Matrix->ExtToIntRowMap[ExtCol] = ++Matrix->CurrentSize;
        Matrix->ExtToIntColMap[ExtCol] = Matrix->CurrentSize;
        IntCol = Matrix->CurrentSize;

#if NOT EXPANDABLE
        vASSERT( IntCol <= Matrix->Size, "Matrix size fixed" );
#endif

#if EXPANDABLE
/* Re-size Matrix if necessary. */
        if (IntCol > Matrix->Size)
            EnlargeMatrix( Matrix, IntCol );
        if (Matrix->Error == spNO_MEMORY) return;
#endif

        Matrix->IntToExtRowMap[IntCol] = ExtCol;
        Matrix->IntToExtColMap[IntCol] = ExtCol;
    }

    *Row = IntRow;
    *Col = IntCol;
    return;
}
#endif






#if QUAD_ELEMENT
/*!
 *  Performs same function as spGetElement() except rather than one
 *  element, all four matrix elements for a floating two terminal
 *  admittance component are added. This routine also works if component
 *  is grounded.  Positive elements are placed at [Node1,Node2] and
 *  [Node2,Node1].  This routine is only to be used after spCreate()
 *  and before spMNA_Preorder(), spFactor() or spOrderAndFactor().
 *
 *  \return
 *  Error code. Possible errors include \a spNO_MEMORY.
 *  Error is not cleared in this routine.
 *
 *  \param Matrix
 *     Pointer to the matrix that component is to be entered in.
 *  \param Node1
 *     Row and column indices for elements. Must be in the range of [0..Size]
 *     unless the options \a EXPANDABLE or \a TRANSLATE are used. Node zero is the
 *     ground node.  In no case may \a Node1 be less than zero.
 *  \param Node2
 *     Row and column indices for elements. Must be in the range of [0..Size]
 *     unless the options \a EXPANDABLE or \a TRANSLATE are used. Node zero is the
 *     ground node.  In no case may \a Node2 be less than zero.
 *  \param Template
 *     Collection of pointers to four elements that are later used to directly
 *     address elements.  User must supply the template, this routine will
 *     fill it.
 */

spError
spGetAdmittance(
    spMatrix Matrix,
    int Node1,
    int Node2,
    struct spTemplate *Template
)
{

/* Begin `spGetAdmittance'. */
    Template->Element1 = spGetElement(Matrix, Node1, Node1 );
    Template->Element2 = spGetElement(Matrix, Node2, Node2 );
    Template->Element3Negated = spGetElement( Matrix, Node2, Node1 );
    Template->Element4Negated = spGetElement( Matrix, Node1, Node2 );
    if
    (   (Template->Element1 == NULL)
        OR (Template->Element2 == NULL)
        OR (Template->Element3Negated == NULL)
        OR (Template->Element4Negated == NULL)
    )   return spNO_MEMORY;

    if (Node1 == 0)
        SWAP( RealNumber*, Template->Element1, Template->Element2 );

    return spOKAY;
}
#endif /* QUAD_ELEMENT */









#if QUAD_ELEMENT
/*!
 *  Similar to spGetAdmittance(), except that spGetAdmittance() only
 *  handles 2-terminal components, whereas spGetQuad() handles simple
 *  4-terminals as well.  These 4-terminals are simply generalized
 *  2-terminals with the option of having the sense terminals different
 *  from the source and sink terminals.  spGetQuad() adds four
 *  elements to the matrix.  Positive elements occur at [Row1,Col1]
 *  [Row2,Col2] while negative elements occur at [Row1,Col2] and [Row2,Col1].
 *  The routine works fine if any of the rows and columns are zero.
 *  This routine is only to be used after spCreate() and before
 *  spMNA_Preorder(), spFactor() or spOrderAndFactor()
 *  unless \a TRANSLATE is set true.
 *
 *  \return
 *  Error code. Possible errors include \a spNO_MEMORY.
 *  Error is not cleared in this routine.
 *
 *  \param Matrix
 *     Pointer to the matrix that component is to be entered in.
 *  \param Row1
 *     First row index for elements. Must be in the range of [0..Size]
 *     unless the options \a EXPANDABLE or \a TRANSLATE are used. Zero is the
 *     ground row.  In no case may Row1 be less than zero.
 *  \param Row2
 *     Second row index for elements. Must be in the range of [0..Size]
 *     unless the options \a EXPANDABLE or \a TRANSLATE are used. Zero is the
 *     ground row.  In no case may Row2 be less than zero.
 *  \param Col1
 *     First column index for elements. Must be in the range of [0..Size]
 *     unless the options \a EXPANDABLE or \a TRANSLATE are used. Zero is the
 *     ground column.  In no case may Col1 be less than zero.
 *  \param Col2
 *     Second column index for elements. Must be in the range of [0..Size]
 *     unless the options \a EXPANDABLE or \a TRANSLATE are used. Zero is the
 *     ground column.  In no case may Col2 be less than zero.
 *  \param Template
 *     Collection of pointers to four elements that are later used to directly
 *     address elements.  User must supply the template, this routine will
 *     fill it.
 */

spError
spGetQuad(
    spMatrix Matrix,
    int Row1,
    int Row2,
    int Col1,
    int Col2,
    struct  spTemplate  *Template
)
{
/* Begin `spGetQuad'. */
    Template->Element1 = spGetElement( Matrix, Row1, Col1);
    Template->Element2 = spGetElement( Matrix, Row2, Col2 );
    Template->Element3Negated = spGetElement( Matrix, Row2, Col1 );
    Template->Element4Negated = spGetElement( Matrix, Row1, Col2 );
    if
    (   (Template->Element1 == NULL)
        OR (Template->Element2 == NULL)
        OR (Template->Element3Negated == NULL)
        OR (Template->Element4Negated == NULL)
    )   return spNO_MEMORY;

    if (Template->Element1 == &((MatrixPtr)Matrix)->TrashCan.Real)
        SWAP( RealNumber *, Template->Element1, Template->Element2 );

    return spOKAY;
}
#endif /* QUAD_ELEMENT */









#if QUAD_ELEMENT
/*!
 *  Addition of four structural ones to matrix by index.
 *  Performs similar function to spGetQuad() except this routine is
 *  meant for components that do not have an admittance representation.
 *
 *  The following stamp is used: \code
 *         Pos  Neg  Eqn
 *  Pos  [  .    .    1  ]
 *  Neg  [  .    .   -1  ]
 *  Eqn  [  1   -1    .  ]
 *  \endcode
 *
 *  \return
 *  Error code. Possible errors include \a spNO_MEMORY.
 *  Error is not cleared in this routine.
 *
 *  \param Matrix
 *     Pointer to the matrix that component is to be entered in.
 *  \param Pos
 *     See stamp above. Must be in the range of [0..Size]
 *     unless the options \a EXPANDABLE or \a TRANSLATE are used. Zero is the
 *     ground row.  In no case may \a Pos be less than zero.
 *  \param Neg
 *     See stamp above. Must be in the range of [0..Size]
 *     unless the options \a EXPANDABLE or \a TRANSLATE are used. Zero is the
 *     ground row.  In no case may \a Neg be less than zero.
 *  \param Eqn
 *     See stamp above. Must be in the range of [0..Size]
 *     unless the options \a EXPANDABLE or \a TRANSLATE are used. Zero is the
 *     ground row.  In no case may \a Eqn be less than zero.
 *  \param Template
 *     Collection of pointers to four elements that are later used to directly
 *     address elements.  User must supply the template, this routine will
 *     fill it.
 */

spError
spGetOnes(
    spMatrix Matrix,
    int Pos,
    int Neg,
    int Eqn,
    struct spTemplate *Template
)
{
/* Begin `spGetOnes'. */
    Template->Element4Negated = spGetElement( Matrix, Neg, Eqn );
    Template->Element3Negated = spGetElement( Matrix, Eqn, Neg );
    Template->Element2 = spGetElement( Matrix, Pos, Eqn );
    Template->Element1 = spGetElement( Matrix, Eqn, Pos );
    if
    (   (Template->Element1 == NULL)
        OR (Template->Element2 == NULL)
        OR (Template->Element3Negated == NULL)
        OR (Template->Element4Negated == NULL)
    )   return spNO_MEMORY;

    spADD_REAL_QUAD( *Template, 1.0 );
    return spOKAY;
}
#endif /* QUAD_ELEMENT */







/*
 *  FIND DIAGONAL
 *
 *  This routine is used to find a diagonal element.  It will not
 *  create it if it does not exist.
 *
 *  >>> Returned:
 *  A pointer to the desired element, or NULL if it does not exist.
 *
 *  >>> Arguments:
 *  Matrix  <input>  (MatrixPtr)
 *        Pointer to matrix.
 *  Index  <input>  (int)
 *      Row, Col index for diagonal element.
 *
 *  >>> Local variables:
 *  pElement  (ElementPtr)
 *      Pointer to an element in the matrix.
 */

ElementPtr
spcFindDiag(
    MatrixPtr Matrix,
    register int Index
)
{
register ElementPtr  pElement;

/* Begin `spcFindDiag'. */
    pElement = Matrix->FirstInCol[Index];

/* Search column for element. */
    while ((pElement != NULL) AND (pElement->Row < Index))
        pElement = pElement->NextInCol;
    if (pElement AND (pElement->Row == Index))
        return pElement;
    else
        return NULL;
}








/*
 *  CREATE AND SPLICE ELEMENT INTO MATRIX
 *
 *  This routine is used to create new matrix elements and splice them into the
 *  matrix.
 *
 *  >>> Returned:
 *  A pointer to the element that was created is returned.
 *
 *  >>> Arguments:
 *  Matrix  <input>  (MatrixPtr)
 *        Pointer to matrix.
 *  Row  <input>  (int)
 *      Row index for element.
 *  Col  <input>  (int)
 *      Column index for element.
 *  ppToLeft  <input-output>  (ElementPtr *)
 *      This contains the address of the pointer to an element to the left
 *        of the one being created.  It is used to speed the search and if it
 *        is immediately to the left, it is updated with address of the
 *        created element.
 *  ppAbove  <input-output> (ElementPtr *)
 *      This contains the address of the pointer to an element above the
 *      one being created.  It is used to speed the search and it if it
 *        is immediatley above, it is updated with address of the created
 *        element.
 *  Fillin  <input>  (BOOLEAN)
 *      Flag that indicates if created element is to be a fill-in.
 *
 *  >>> Local variables:
 *  pElement  (ElementPtr)
 *      Pointer to an element in the matrix.
 *  pCreatedElement  (ElementPtr)
 *      Pointer to the desired element, the one that was just created.
 *
 *  >>> Possible errors:
 *  spNO_MEMORY
 */

ElementPtr
spcCreateElement(
    MatrixPtr Matrix,
    int Row,
    register int Col,
    register ElementPtr *ppToLeft,
    register ElementPtr *ppAbove,
    BOOLEAN Fillin
)
{
register ElementPtr  pElement, pCreatedElement;

/* Begin `spcCreateElement'. */

/* Find element immediately above the desired element. */
    pElement = *ppAbove;
    while ((pElement != NULL) AND (pElement->Row < Row))
    {   ppAbove = &pElement->NextInCol;
        pElement = *ppAbove;
    }
    if ((pElement != NULL) AND (pElement->Row == Row))
        return pElement;

/* The desired element does not exist, create it. */
    if (Fillin)
    {   pCreatedElement = spcGetFillin( Matrix );
        Matrix->Fillins++;

/* Update Markowitz counts and products. */
        ++Matrix->MarkowitzRow[Row];
        spcMarkoProd( Matrix->MarkowitzProd[Row],
                      Matrix->MarkowitzRow[Row],
                      Matrix->MarkowitzCol[Row] );
        if ((Matrix->MarkowitzRow[Row] == 1) AND
            (Matrix->MarkowitzCol[Row] != 0))
        {
            Matrix->Singletons--;
        }
        ++Matrix->MarkowitzCol[Col];
        spcMarkoProd( Matrix->MarkowitzProd[Col],
                      Matrix->MarkowitzCol[Col],
                      Matrix->MarkowitzRow[Col] );
        if ((Matrix->MarkowitzRow[Col] != 0) AND
            (Matrix->MarkowitzCol[Col] == 1))
        {
            Matrix->Singletons--;
        }
    }
    else
    {   pCreatedElement = spcGetElement( Matrix );
        Matrix->NeedsOrdering = YES;
    }
    if (pCreatedElement == NULL) return NULL;
    Matrix->Elements++;

/* Initialize Element. */
    pCreatedElement->Row = Row;
    pCreatedElement->Col = Col;
    pCreatedElement->Real = 0.0;
#if spCOMPLEX
    pCreatedElement->Imag = 0.0;
#endif
#if INITIALIZE
    pCreatedElement->pInitInfo = NULL;
#endif

/* If element is on diagonal, store pointer in Diag. */
    if (Row == Col) Matrix->Diag[Row] = pCreatedElement;

/* Splice element into column. */
    pCreatedElement->NextInCol = *ppAbove;
    *ppAbove = pCreatedElement;

/* Find Element immediately to the left of the fill-in. */
    if (Matrix->RowsLinked)
    {   pElement = *ppToLeft;
        while (pElement != NULL)
        {   if (pElement->Col < Col)
            {   ppToLeft = &pElement->NextInRow;
                pElement = *ppToLeft;
            }
            else break; /* while loop */
        }

/* Splice element into row. */
        pCreatedElement->NextInRow = *ppToLeft;
        *ppToLeft = pCreatedElement;
    }
    return pCreatedElement;
}








/*
 *
 *  LINK ROWS
 *
 *  This routine is used to generate the row links.  The spGetElement()
 *  routines do not create row links, which are needed by the spFactor()
 *  routines.
 *
 *  >>>  Arguments:
 *  Matrix  <input>  (MatrixPtr)
 *      Pointer to the matrix.
 *
 *  >>> Local variables:
 *  pElement  (ElementPtr)
 *      Pointer to an element in the matrix.
 *  FirstInRowEntry  (ElementPtr *)
 *      A pointer into the FirstInRow array.  Points to the FirstInRow entry
 *      currently being operated upon.
 *  FirstInRowArray  (ArrayOfElementPtrs)
 *      A pointer to the FirstInRow array.  Same as Matrix->FirstInRow but
 *      resides in a register and requires less indirection so is faster to
 *      use.
 *  Col  (int)
 *      Column currently being operated upon.
 */

void
spcLinkRows( MatrixPtr Matrix )
{
register  ElementPtr  pElement, *FirstInRowEntry;
register  ArrayOfElementPtrs  FirstInRowArray;
register  int  Col;

/* Begin `spcLinkRows'. */
    FirstInRowArray = Matrix->FirstInRow;
    for (Col = Matrix->Size; Col >= 1; Col--)
        FirstInRowArray[Col] = NULL;

    for (Col = Matrix->Size; Col >= 1; Col--)
    {
/* Generate row links for the elements in the Col'th column. */
        pElement = Matrix->FirstInCol[Col];

        while (pElement != NULL)
        {   pElement->Col = Col;
            FirstInRowEntry = &FirstInRowArray[pElement->Row];
            pElement->NextInRow = *FirstInRowEntry;
            *FirstInRowEntry = pElement;
            pElement = pElement->NextInCol;
        }
    }
    Matrix->RowsLinked = YES;
    return;
}








/*
 *  ENLARGE MATRIX
 *
 *  Increases the size of the matrix.
 *
 *  >>> Arguments:
 *  Matrix  <input>    (MatrixPtr)
 *      Pointer to the matrix.
 *  NewSize  <input>  (int)
 *     The new size of the matrix.
 *
 *  >>> Local variables:
 *  OldAllocatedSize  (int)
 *     The allocated size of the matrix before it is expanded.
 */

static void
EnlargeMatrix(
    MatrixPtr Matrix,
    register int NewSize
)
{
register int I, OldAllocatedSize = Matrix->AllocatedSize;

/* Begin `EnlargeMatrix'. */
    Matrix->Size = NewSize;

    if (NewSize <= OldAllocatedSize)
        return;

/* Expand the matrix frame. */
    NewSize = MAX( NewSize, (int)(EXPANSION_FACTOR * OldAllocatedSize) );
    Matrix->AllocatedSize = NewSize;

    if (( REALLOC(Matrix->IntToExtColMap, int, NewSize+1)) == NULL)
    {   Matrix->Error = spNO_MEMORY;
        return;
    }
    if (( REALLOC(Matrix->IntToExtRowMap, int, NewSize+1)) == NULL)
    {   Matrix->Error = spNO_MEMORY;
        return;
    }
    if (( REALLOC(Matrix->Diag, ElementPtr, NewSize+1)) == NULL)
    {   Matrix->Error = spNO_MEMORY;
        return;
    }
    if (( REALLOC(Matrix->FirstInCol, ElementPtr, NewSize+1)) == NULL)
    {   Matrix->Error = spNO_MEMORY;
        return;
    }
    if (( REALLOC(Matrix->FirstInRow, ElementPtr, NewSize+1)) == NULL)
    {   Matrix->Error = spNO_MEMORY;
        return;
    }

/*
 * Destroy the Markowitz and Intermediate vectors, they will be recreated
 * in spOrderAndFactor().
 */
    FREE( Matrix->MarkowitzRow );
    FREE( Matrix->MarkowitzCol );
    FREE( Matrix->MarkowitzProd );
    FREE( Matrix->DoRealDirect );
    FREE( Matrix->DoCmplxDirect );
    FREE( Matrix->Intermediate );
    Matrix->InternalVectorsAllocated = NO;

/* Initialize the new portion of the vectors. */
    for (I = OldAllocatedSize+1; I <= NewSize; I++)
    {   Matrix->IntToExtColMap[I] = I;
        Matrix->IntToExtRowMap[I] = I;
        Matrix->Diag[I] = NULL;
        Matrix->FirstInRow[I] = NULL;
        Matrix->FirstInCol[I] = NULL;
    }

    return;
}








#if TRANSLATE

/*
 *  EXPAND TRANSLATION ARRAYS
 *
 *  Increases the size arrays that are used to translate external to internal
 *  row and column numbers.
 *
 *  >>> Arguments:
 *  Matrix  <input>    (MatrixPtr)
 *      Pointer to the matrix.
 *  NewSize  <input>  (int)
 *     The new size of the translation arrays.
 *
 *  >>> Local variables:
 *  OldAllocatedSize  (int)
 *     The allocated size of the translation arrays before being expanded.
 */

static void
ExpandTranslationArrays(
    MatrixPtr Matrix,
    register int NewSize
)
{
register int I, OldAllocatedSize = Matrix->AllocatedExtSize;

/* Begin `ExpandTranslationArrays'. */
    Matrix->ExtSize = NewSize;

    if (NewSize <= OldAllocatedSize)
        return;

/* Expand the translation arrays ExtToIntRowMap and ExtToIntColMap. */
    NewSize = MAX( NewSize, (int)(EXPANSION_FACTOR * OldAllocatedSize) );
    Matrix->AllocatedExtSize = NewSize;

    if (( REALLOC(Matrix->ExtToIntRowMap, int, NewSize+1)) == NULL)
    {   Matrix->Error = spNO_MEMORY;
        return;
    }
    if (( REALLOC(Matrix->ExtToIntColMap, int, NewSize+1)) == NULL)
    {   Matrix->Error = spNO_MEMORY;
        return;
    }

/* Initialize the new portion of the vectors. */
    for (I = OldAllocatedSize+1; I <= NewSize; I++)
    {   Matrix->ExtToIntRowMap[I] = -1;
        Matrix->ExtToIntColMap[I] = -1;
    }

    return;
}
#endif









#if INITIALIZE
/*!
 *   Initialize the matrix.
 *
 *   With the \a INITIALIZE compiler option (see spConfig.h) set true,
 *   Sparse allows the user to keep initialization information with each
 *   structurally nonzero matrix element.  Each element has a pointer
 *   that is set and used by the user.  The user can set this pointer
 *   using spInstallInitInfo() and may be read using spGetInitInfo().  Both
 *   may be used only after the element exists.  The function
 *   spInitialize() is a user customizable way to initialize the matrix.
 *   Passed to this routine is a function pointer.  spInitialize() sweeps
 *   through every element in the matrix and checks the \a pInitInfo
 *   pointer (the user supplied pointer).  If the \a pInitInfo is \a NULL,
 *   which is true unless the user changes it (almost always true for
 *   fill-ins), then the element is zeroed.  Otherwise, the function
 *   pointer is called and passed the \a pInitInfo pointer as well as the
 *   element pointer and the external row and column numbers.  If the
 *   user sets the value of each element, then spInitialize() replaces
 *   spClear().
 *
 *   The user function is expected to return a nonzero integer if there
 *   is a fatal error and zero otherwise.  Upon encountering a nonzero
 *   return code, spInitialize() terminates, sets the error state of
 *   the matrix to be \a spMANGLED, and returns the error code.
 *
 *   \return
 *        Returns the return value of the \a pInit() function.
 *   \param eMatrix
 *      Pointer to matrix.
 *   \param pInit
 *      Pointer to a function that initializes an element.

 *   \see spClear()
 */

int
spInitialize(
    spMatrix eMatrix,
    int (*pInit)(
        spElement *pElement,
        spGenericPtr pInitInfo,
        int Row,
        int Col
    )
)
{
MatrixPtr Matrix = (MatrixPtr)eMatrix;
register ElementPtr pElement;
int J, Error, Col;

/* Begin `spInitialize'. */
    ASSERT_IS_SPARSE( Matrix );

#if spCOMPLEX
/* Clear imaginary part of matrix if matrix is real but was complex. */
    if (Matrix->PreviousMatrixWasComplex AND NOT Matrix->Complex)
    {   for (J = Matrix->Size; J > 0; J--)
        {   pElement = Matrix->FirstInCol[J];
            while (pElement != NULL)
            {   pElement->Imag = 0.0;
                pElement = pElement->NextInCol;
            }
        }
    }
#endif /* spCOMPLEX */

/* Initialize the matrix. */
    for (J = Matrix->Size; J > 0; J--)
    {   pElement = Matrix->FirstInCol[J];
        Col = Matrix->IntToExtColMap[J];
        while (pElement != NULL)
        {   if (pElement->pInitInfo == NULL)
            {   pElement->Real = 0.0;
#               if spCOMPLEX
                    pElement->Imag = 0.0;
#               endif
            }
            else
            {   Error = (*pInit)((RealNumber *)pElement, pElement->pInitInfo,
                                 Matrix->IntToExtRowMap[pElement->Row], Col);
                if (Error)
                {   Matrix->Error = spMANGLED;
                    return Error;
                }

            }
            pElement = pElement->NextInCol;
        }
    }

/* Empty the trash. */
    Matrix->TrashCan.Real = 0.0;
#if spCOMPLEX
    Matrix->TrashCan.Imag = 0.0;
#endif

    Matrix->Error = spOKAY;
    Matrix->Factored = NO;
    Matrix->SingularCol = 0;
    Matrix->SingularRow = 0;
    Matrix->PreviousMatrixWasComplex = Matrix->Complex;
    return 0;
}




/*!
 *   This function installs a pointer to a data structure that is used
 *   to contain initialization information to a matrix element. It is
 *   is then used by spInitialize() to initialize the matrix.
 *
 *   \param pElement
 *       Pointer to matrix element.
 *   \param pInitInfo
 *       Pointer to the data structure that will contain initialiation
 *       information.
 *   \see spInitialize()
 */

void
spInstallInitInfo(
    spElement *pElement,
    spGenericPtr pInitInfo
)
{
/* Begin `spInstallInitInfo'. */
    vASSERT( pElement != NULL, "Invalid element pointer" );

    ((ElementPtr)pElement)->pInitInfo = pInitInfo;
}


/*!
 *   This function returns a pointer to a data structure that is used
 *   to contain initialization information to a matrix element.
 *
 *   \return
 *       The pointer to the initialiation information data structure
 *       that is associated with a particular matrix element.
 *
 *   \param pElement
 *       Pointer to the matrix element.
 *
 *   \see spInitialize()
 */
spGenericPtr
spGetInitInfo(
    spElement *pElement
)
{
/* Begin `spGetInitInfo'. */
    vASSERT( pElement != NULL, "Invalid element pointer" );

    return (spGenericPtr)((ElementPtr)pElement)->pInitInfo;
}
#endif /* INITIALIZE */
