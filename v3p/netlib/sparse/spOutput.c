/*
 *  MATRIX OUTPUT MODULE
 *
 *  Author:                     Advisor:
 *      Kenneth S. Kundert          Alberto Sangiovanni-Vincentelli
 *      UC Berkeley
 */
/*! \file
 *
 *  This file contains the output-to-file and output-to-screen routines for
 *  the matrix package.
 *
 *  Objects that begin with the \a spc prefix are considered private
 *  and should not be used.
 *
 *  \author
 *  Kenneth S. Kundert <kundert@users.sourceforge.net>
 */
/*  >>> User accessible functions contained in this file:
 *  spPrint
 *  spFileMatrix
 *  spFileVector
 *  spFileStats
 *
 *  >>> Other functions contained in this file:
 */


/*
 *  Revision and copyright information.
 *
 *  Copyright (c) 1985-2003
 *  by Kenneth S. Kundert
 */
/*
Removed File IO routines to get rid of fopen warnings - JLM
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


#if DOCUMENTATION
/*!
 *  Formats and send the matrix to standard output.  Some elementary
 *  statistics are also output.  The matrix is output in a format that is
 *  readable by people.
 *
 *  \param eMatrix
 *      Pointer to matrix.
 *  \param PrintReordered
 *      Indicates whether the matrix should be printed out in its original
 *      form, as input by the user, or whether it should be printed in its
 *      reordered form, as used by the matrix routines.  A zero indicates that
 *      the matrix should be printed as inputed, a one indicates that it
 *      should be printed reordered.
 *  \param Data
 *      Boolean flag that when false indicates that output should be
 *      compressed such that only the existence of an element should be
 *      indicated rather than giving the actual value.  Thus 11 times as
 *      many can be printed on a row.  A zero signifies that the matrix
 *      should be printed compressed. A one indicates that the matrix
 *      should be printed in all its glory.
 *  \param Header
 *      Flag indicating that extra information should be given, such as row
 *      and column numbers.
 */
/*  >>> Local variables:
 *  Col  (int)
 *      Column being printed.
 *  ElementCount  (int)
 *      Variable used to count the number of nonzero elements in the matrix.
 *  LargestElement  (RealNumber)
 *      The magnitude of the largest element in the matrix.
 *  LargestDiag  (RealNumber)
 *      The magnitude of the largest diagonal in the matrix.
 *  Magnitude  (RealNumber)
 *      The absolute value of the matrix element being printed.
 *  PrintOrdToIntColMap  (int [])
 *      A translation array that maps the order that columns will be
 *      printed in (if not PrintReordered) to the internal column numbers.
 *  PrintOrdToIntRowMap  (int [])
 *      A translation array that maps the order that rows will be
 *      printed in (if not PrintReordered) to the internal row numbers.
 *  pElement  (ElementPtr)
 *      Pointer to the element in the matrix that is to be printed.
 *  pImagElements  (ElementPtr [ ])
 *      Array of pointers to elements in the matrix.  These pointers point
 *      to the elements whose real values have just been printed.  They are
 *      used to quickly access those same elements so their imaginary values
 *      can be printed.
 *  Row  (int)
 *      Row being printed.
 *  Size  (int)
 *      The size of the matrix.
 *  SmallestDiag  (RealNumber)
 *      The magnitude of the smallest diagonal in the matrix.
 *  SmallestElement  (RealNumber)
 *      The magnitude of the smallest element in the matrix excluding zero
 *      elements.
 *  StartCol  (int)
 *      The column number of the first column to be printed in the group of
 *      columns currently being printed.
 *  StopCol  (int)
 *      The column number of the last column to be printed in the group of
 *      columns currently being printed.
 *  Top  (int)
 *      The largest expected external row or column number.
 */

void
spPrint(
    spMatrix eMatrix,
    int PrintReordered,
    int Data,
    int Header
)
{
MatrixPtr  Matrix = (MatrixPtr)eMatrix;
register  int  J = 0;
int I, Row, Col, Size, Top, StartCol = 1, StopCol, Columns, ElementCount = 0;
double  Magnitude, SmallestDiag=LARGEST_REAL, SmallestElement=LARGEST_REAL;
double  LargestElement = 0.0, LargestDiag = 0.0;
ElementPtr  pElement, pImagElements[PRINTER_WIDTH/10+1];
int  *PrintOrdToIntRowMap, *PrintOrdToIntColMap;

/* Begin `spPrint'. */
    ASSERT_IS_SPARSE( Matrix );
    Size = Matrix->Size;

/* Create a packed external to internal row and column translation array. */
# if TRANSLATE
    Top = Matrix->AllocatedExtSize;
#else
    Top = Matrix->AllocatedSize;
#endif
    CALLOC( PrintOrdToIntRowMap, int, Top + 1 );
    CALLOC( PrintOrdToIntColMap, int, Top + 1 );
    if ( PrintOrdToIntRowMap == NULL OR PrintOrdToIntColMap == NULL)
    {   Matrix->Error = spNO_MEMORY;
        return;
    }
    for (I = 1; I <= Size; I++)
    {   PrintOrdToIntRowMap[ Matrix->IntToExtRowMap[I] ] = I;
        PrintOrdToIntColMap[ Matrix->IntToExtColMap[I] ] = I;
    }

/* Pack the arrays. */
    for (J = 1, I = 1; I <= Top; I++)
    {   if (PrintOrdToIntRowMap[I] != 0)
            PrintOrdToIntRowMap[ J++ ] = PrintOrdToIntRowMap[ I ];
    }
    for (J = 1, I = 1; I <= Top; I++)
    {   if (PrintOrdToIntColMap[I] != 0)
            PrintOrdToIntColMap[ J++ ] = PrintOrdToIntColMap[ I ];
    }

/* Print header. */
    if (Header)
    {   printf("MATRIX SUMMARY\n\n");
        printf("Size of matrix = %1d x %1d.\n", Size, Size);
        if ( Matrix->Reordered AND PrintReordered )
            printf("Matrix has been reordered.\n");
        putchar('\n');

        if ( Matrix->Factored )
            printf("Matrix after factorization:\n");
        else
            printf("Matrix before factorization:\n");
    }
    if (Size == 0) return;

/* Determine how many columns to use. */
    Columns = PRINTER_WIDTH;
    if (Header) Columns -= 5;
    if (Data) Columns = (Columns+1) / 10;

/*
 * Print matrix by printing groups of complete columns until all the columns
 * are printed.
 */
    J = 0;
    while ( J <= Size )

/* Calculate index of last column to printed in this group. */
    {   StopCol = StartCol + Columns - 1;
        if (StopCol > Size)
            StopCol = Size;

/* Label the columns. */
        if (Header)
        {   if (Data)
            {   printf("    ");
                for (I = StartCol; I <= StopCol; I++)
                {   if (PrintReordered)
                        Col = I;
                    else
                        Col = PrintOrdToIntColMap[I];
                    printf(" %9d", Matrix->IntToExtColMap[ Col ]);
                }
                printf("\n\n");
            }
            else
            {   if (PrintReordered)
                    printf("Columns %1d to %1d.\n",StartCol,StopCol);
                else
                {   printf("Columns %1d to %1d.\n",
                        Matrix->IntToExtColMap[ PrintOrdToIntColMap[StartCol] ],
                        Matrix->IntToExtColMap[ PrintOrdToIntColMap[StopCol] ]);
                }
            }
        }

/* Print every row ...  */
        for (I = 1; I <= Size; I++)
        {   if (PrintReordered)
                Row = I;
            else
                Row = PrintOrdToIntRowMap[I];

            if (Header)
            {   if (PrintReordered AND NOT Data)
                    printf("%4d", I);
                else
                    printf("%4d", Matrix->IntToExtRowMap[ Row ]);
                if (NOT Data) putchar(' ');
            }

/* ... in each column of the group. */
            for (J = StartCol; J <= StopCol; J++)
            {   if (PrintReordered)
                    Col = J;
                else
                    Col = PrintOrdToIntColMap[J];

                pElement = Matrix->FirstInCol[Col];
                while (pElement != NULL AND pElement->Row != Row)
                    pElement = pElement->NextInCol;

                if (Data)
                    pImagElements[J - StartCol] = pElement;

                if (pElement != NULL)

/* Case where element exists */
                {   if (Data)
                        printf(" %9.3g", (double)pElement->Real);
                    else
                        putchar('x');

/* Update status variables */
                    if ( (Magnitude = ELEMENT_MAG(pElement)) > LargestElement )
                        LargestElement = Magnitude;
                    if ((Magnitude < SmallestElement) AND (Magnitude != 0.0))
                        SmallestElement = Magnitude;
                    ElementCount++;
                }

/* Case where element is structurally zero */
                else
                {   if (Data)
                        printf("       ...");
                    else
                        putchar('.');
                }
            }
            putchar('\n');

#if spCOMPLEX
            if (Matrix->Complex AND Data)
            {   if (Header)
                    printf("    ");
                for (J = StartCol; J <= StopCol; J++)
                {   if (pImagElements[J - StartCol] != NULL)
                    {   printf(" %8.2gj",
                               (double)pImagElements[J-StartCol]->Imag);
                    }
                    else printf("          ");
                }
                putchar('\n');
            }
#endif /* spCOMPLEX */
        }

/* Calculate index of first column in next group. */
        StartCol = StopCol;
        StartCol++;
        putchar('\n');
    }
    if (Header)
    {   printf("\nLargest element in matrix = %-1.4g.\n", LargestElement);
        printf("Smallest element in matrix = %-1.4g.\n", SmallestElement);

/* Search for largest and smallest diagonal values */
        for (I = 1; I <= Size; I++)
        {   if (Matrix->Diag[I] != NULL)
            {   Magnitude = ELEMENT_MAG( Matrix->Diag[I] );
                if ( Magnitude > LargestDiag ) LargestDiag = Magnitude;
                if ( Magnitude < SmallestDiag ) SmallestDiag = Magnitude;
            }
        }

    /* Print the largest and smallest diagonal values */
        if ( Matrix->Factored )
        {   printf("\nLargest diagonal element = %-1.4g.\n", LargestDiag);
            printf("Smallest diagonal element = %-1.4g.\n", SmallestDiag);
        }
        else
        {   printf("\nLargest pivot element = %-1.4g.\n", LargestDiag);
            printf("Smallest pivot element = %-1.4g.\n", SmallestDiag);
        }

    /* Calculate and print sparsity and number of fill-ins created. */
        printf("\nDensity = %2.2f%%.\n", ((double)ElementCount * 100.0)
                                         / (((double)Size * (double)Size)));
        if (NOT Matrix->NeedsOrdering)
            printf("Number of fill-ins = %1d.\n", Matrix->Fillins);
    }
    putchar('\n');
    (void)fflush(stdout);

    FREE(PrintOrdToIntColMap);
    FREE(PrintOrdToIntRowMap);
    return;
}

#endif /* DOCUMENTATION */

/* Added to export the row and column maps to convert the
   internal matrix to an external form - JLM */
void
spRowColOrder(
    spMatrix eMatrix,
    int* OrdToIntRowMap,
    int* OrdToIntColMap
)
{
  MatrixPtr  Matrix = (MatrixPtr)eMatrix;

  int I, Size;
  ASSERT_IS_SPARSE( Matrix );
  Size = Matrix->Size;
  if ( OrdToIntRowMap == NULL OR OrdToIntColMap == NULL)
    {   Matrix->Error = spNO_MEMORY;
        return;
    }
  for (I = 1; I <= Size; I++)
    {   OrdToIntRowMap[ Matrix->IntToExtRowMap[I] ] = I;
        OrdToIntColMap[ Matrix->IntToExtColMap[I] ] = I;
    }
}
