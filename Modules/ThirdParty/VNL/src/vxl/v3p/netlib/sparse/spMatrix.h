/*  EXPORTS for sparse matrix routines. */
/*!
 *  \file
 *
 *  This file contains definitions that are useful to the calling
 *  program.  In particular, this file contains error keyword
 *  definitions, some macro functions that are used to quickly enter
 *  data into the matrix and the type definition of a data structure
 *  that acts as a template for entering admittances into the matrix.
 *  Also included is the type definitions for the various functions
 *  available to the user.
 *
 *  Objects that begin with the \a spc prefix are considered private
 *  and should not be used.
 *
 *  \author
 *  Kenneth S. Kundert <kundert@users.sourceforge.net>
 */


/*
 *  Revision and copyright information.
 *
 *  Copyright (c) 1985-2003 by Kenneth S. Kundert
 *
 */


#ifndef  spOKAY

/*
 *  IMPORTS
 *
 *  >>> Import descriptions:
 *  spConfig.h
 *      Macros that customize the sparse matrix routines.
 */

#include "spConfig.h"

/*
 *  ERROR KEYWORDS
 *
 *  The actual numbers used in the error codes are not sacred, they can be
 *  changed under the condition that the codes for the nonfatal errors are
 *  less than the code for spFATAL and similarly the codes for the fatal
 *  errors are greater than that for spFATAL.
 */

/* Begin error macros. */
#define  spOKAY                0  /*!<
                            * Error code that indicates that no error has
                            * occurred.
                            */
#define  spSMALL_PIVOT        1  /*!<
                            * Non-fatal error code that indicates that, when
                            * reordering the matrix, no element was found that
                            * satisfies the absolute threshold criteria. The
                            * largest element in the matrix was chosen as pivot.
                            */
#define  spZERO_DIAG        2  /*!<
                            * Fatal error code that indicates that, a zero was
                            * encountered on the diagonal the matrix. This does
                            * not necessarily imply that the matrix is singular.
                            * When this error occurs, the matrix should be
                            * reconstructed and factored using
                            * spOrderAndFactor().
                            */
#define  spSINGULAR        3  /*!<
                            * Fatal error code that indicates that, matrix is
                            * singular, so no unique solution exists.
                            */
#define  spMANGLED        4  /*!<
                            * Fatal error code that indicates that, matrix has
                            * been mangled, results of requested operation are
                            * garbage.
                            */
#define  spNO_MEMORY        5  /*!<
                            * Fatal error code that indicates that not enough
                            * memory is available.
                            */
#define  spPANIC        6  /*!<
                            * Fatal error code that indicates that the routines
                            * are not prepared to handle the matrix that has
                            * been requested.  This may occur when the matrix
                            * is specified to be real and the routines are not
                            * compiled for real matrices, or when the matrix is
                            * specified to be complex and the routines are not
                            * compiled to handle complex matrices.
                            */
#define  spFATAL        2  /*!<
                            * Error code that is not an error flag, but rather
                            * the dividing line between fatal errors and
                            * warnings.
                            */


/*
 *  KEYWORD DEFINITIONS
 */

#define  spREAL double  /*!<
                         * Defines the precision of the arithmetic used by
                         * \a Sparse will use.  Double precision is suggested
                         * as being most appropriate for circuit simulation
                         * and for C.  However, it is possible to change spREAL
                         * to a float for single precision arithmetic.  Note
                         * that in C, single precision arithmetic is often
                         * slower than double precision.  Sparse
                         * internally refers to spREALs as RealNumbers.
                         */


/*
 *  PARTITION TYPES
 *
 *  When factoring a previously ordered matrix using spFactor(), Sparse
 *  operates on a row-at-a-time basis.  For speed, on each step, the row
 *  being updated is copied into a full vector and the operations are
 *  performed on that vector.  This can be done one of two ways, either
 *  using direct addressing or indirect addressing.  Direct addressing
 *  is fastest when the matrix is relatively dense and indirect addressing
 *  is quite sparse.  The user can select which partitioning mode is used.
 *  The following keywords are passed to spPartition() and indicate that
 *  Sparse should use only direct addressing, only indirect addressing, or
 *  that it should choose the best mode on a row-by-row basis.  The time
 *  required to choose a partition is of the same order of the cost to factor
 *  the matrix.
 *
 *  If you plan to factor a large number of matrices with the same structure,
 *  it is best to let Sparse choose the partition.  Otherwise, you should
 *  choose the partition based on the predicted density of the matrix.
 */

/* Begin partition keywords. */

#define spDEFAULT_PARTITION        0 /*!<
                                   * Partition code for spPartition().
                                   * Indicates that the default partitioning
                                   * mode should be used.
                                   * \see spPartition()
                                   */
#define spDIRECT_PARTITION        1 /*!<
                                   * Partition code for spPartition().
                                   * Indicates that all rows should be placed
                                   * in the direct addressing partition.
                                   * \see spPartition()
                                   */
#define spINDIRECT_PARTITION        2 /*!<
                                   * Partition code for spPartition().
                                   * Indicates that all rows should be placed
                                   * in the indirect addressing partition.
                                   * \see spPartition()
                                   */
#define spAUTO_PARTITION        3 /*!<
                                   * Partition code for spPartition().
                                   * Indicates that \a Sparse should chose
                                   * the best partition for each row based
                                   * on some simple rules. This is generally
                                   * preferred.
                                   * \see spPartition()
                                   */

/*
 *  MACRO FUNCTION DEFINITIONS
 */

/* Begin Macros. */
/*!
 * Macro function that adds data to a real element in the matrix by a pointer.
 */
#define  spADD_REAL_ELEMENT(element,real)       *(element) += real

/*!
 * Macro function that adds data to a imaginary element in the matrix by
 * a pointer.
 */
#define  spADD_IMAG_ELEMENT(element,imag)       *(element+1) += imag

/*!
 * Macro function that adds data to a complex element in the matrix by
 * a pointer.
 */
#define  spADD_COMPLEX_ELEMENT(element,real,imag)       \
{   *(element) += real;                                 \
    *(element+1) += imag;                               \
}

/*!
 * Macro function that adds data to each of the four real matrix elements
 * specified by the given template.
 */
#define  spADD_REAL_QUAD(template,real)         \
{   *((template).Element1) += real;             \
    *((template).Element2) += real;             \
    *((template).Element3Negated) -= real;      \
    *((template).Element4Negated) -= real;      \
}

/*!
 * Macro function that adds data to each of the four imaginary matrix
 * elements specified by the given template.
 */
#define  spADD_IMAG_QUAD(template,imag)         \
{   *((template).Element1+1) += imag;           \
    *((template).Element2+1) += imag;           \
    *((template).Element3Negated+1) -= imag;    \
    *((template).Element4Negated+1) -= imag;    \
}

/*!
 * Macro function that adds data to each of the four complex matrix
 * elements specified by the given template.
 */
#define  spADD_COMPLEX_QUAD(template,real,imag) \
{   *((template).Element1) += real;             \
    *((template).Element2) += real;             \
    *((template).Element3Negated) -= real;      \
    *((template).Element4Negated) -= real;      \
    *((template).Element1+1) += imag;           \
    *((template).Element2+1) += imag;           \
    *((template).Element3Negated+1) -= imag;    \
    *((template).Element4Negated+1) -= imag;    \
}

/*
 *   TYPE DEFINITION FOR EXTERNAL MATRIX ELEMENT REFERENCES
 *
 *   External type definitions for Sparse data objects.
 */

/*! Declares the type of the a pointer to a matrix. */
typedef spGenericPtr spMatrix;

/*! Declares the type of the a pointer to a matrix element. */
typedef spREAL spElement;

/*! Declares the type of the Sparse error codes. */
typedef int spError;

/* TYPE DEFINITION FOR COMPONENT TEMPLATE */
/*!
 *   This data structure is used to hold pointers to four related elements in
 *   matrix.  It is used in conjunction with the routines spGetAdmittance(),
 *   spGetQuad(), and spGetOnes().  These routines stuff the structure which
 *   is later used by the \a spADD_QUAD macro functions above.  It is also
 *   possible for the user to collect four pointers returned by spGetElement()
 *   and stuff them into the template.  The \a spADD_QUAD routines stuff data
 *   into the matrix in locations specified by \a Element1 and \a Element2
 *   without changing the data.  The data is negated before being placed in
 *   \a Element3 and \a Element4.
 */

/* Begin `spTemplate'. */
struct  spTemplate
{   spElement        *Element1;
    spElement        *Element2;
    spElement        *Element3Negated;
    spElement        *Element4Negated;
};


/*
 *   FUNCTION TYPE DEFINITIONS
 *
 *   The type of every user accessible function is declared here.
 */

/* Begin function declarations. */

spcEXTERN  void       spClear( spMatrix );
spcEXTERN  spREAL     spCondition( spMatrix, spREAL, int* );
spcEXTERN  spMatrix   spCreate( int, int, spError* );
spcEXTERN  void       spDeleteRowAndCol( spMatrix, int, int );
spcEXTERN  void       spDestroy( spMatrix );
spcEXTERN  int        spElementCount( spMatrix );
spcEXTERN  spError    spErrorState( spMatrix );

spcEXTERN  spError    spFactor( spMatrix );
/*  Remove file IO due to archaic fopen method which gives warnings in windoz
spcEXTERN  int        spFileMatrix( spMatrix, char*, char*, int, int, int );
spcEXTERN  int        spFileStats( spMatrix, char*, char* );
*/
spcEXTERN  int        spFillinCount( spMatrix );
spcEXTERN  spElement *spFindElement( spMatrix, int, int );
spcEXTERN  spError    spGetAdmittance( spMatrix, int, int,
                                struct spTemplate* );
spcEXTERN  spElement *spGetElement( spMatrix, int, int );
spcEXTERN  spGenericPtr spGetInitInfo( spElement* );
spcEXTERN  spError    spGetOnes( spMatrix, int, int, int,
                                struct spTemplate* );
spcEXTERN  spError    spGetQuad( spMatrix, int, int, int, int,
                                struct spTemplate* );
spcEXTERN  int        spGetSize( spMatrix, int );
spcEXTERN  int        spInitialize( spMatrix, int (*pInit)(spElement *, spGenericPtr, int, int) );
spcEXTERN  void       spInstallInitInfo( spElement*, spGenericPtr );
spcEXTERN  spREAL     spLargestElement( spMatrix );
spcEXTERN  void       spMNA_Preorder( spMatrix );
spcEXTERN  spREAL     spNorm( spMatrix );
spcEXTERN  spError    spOrderAndFactor( spMatrix, spREAL[], spREAL,
                                spREAL, int );
spcEXTERN  void       spPartition( spMatrix, int );
spcEXTERN  void       spPrint( spMatrix, int, int, int );
spcEXTERN  void       spRowColOrder( spMatrix, int*, int*);
spcEXTERN  spREAL     spPseudoCondition( spMatrix );
spcEXTERN  spREAL     spRoundoff( spMatrix, spREAL );
spcEXTERN  void       spScale( spMatrix, spREAL[], spREAL[] );
spcEXTERN  void       spSetComplex( spMatrix );
spcEXTERN  void       spSetReal( spMatrix );
spcEXTERN  void       spStripFills( spMatrix );
spcEXTERN  void       spWhereSingular( spMatrix, int*, int* );

/* Functions with argument lists that are dependent on options. */

#if spCOMPLEX
spcEXTERN  void       spDeterminant( spMatrix, int*, spREAL*, spREAL* );
#else /* NOT spCOMPLEX */
spcEXTERN  void       spDeterminant( spMatrix, int*, spREAL* );
#endif /* NOT spCOMPLEX */
#if spCOMPLEX && spSEPARATED_COMPLEX_VECTORS
/* Get rid of arcaic file IO
spcEXTERN  int        spFileVector( spMatrix, char* ,
                                spREAL[], spREAL[]);
*/
spcEXTERN  void       spMultiply( spMatrix, spREAL[], spREAL[],
                                spREAL[], spREAL[] );
spcEXTERN  void       spMultTransposed( spMatrix, spREAL[], spREAL[],
                                spREAL[], spREAL[] );
spcEXTERN  void       spSolve( spMatrix, spREAL[], spREAL[], spREAL[],
                                spREAL[] );
spcEXTERN  void       spSolveTransposed( spMatrix, spREAL[], spREAL[],
                                spREAL[], spREAL[] );
#else /* NOT  (spCOMPLEX && spSEPARATED_COMPLEX_VECTORS) */
/* Get rid of arcaic file IO
spcEXTERN  int        spFileVector( spMatrix, char* , spREAL[] );
*/
spcEXTERN  void       spMultiply( spMatrix, spREAL[], spREAL[] );
spcEXTERN  void       spMultTransposed( spMatrix,
                                spREAL[], spREAL[] );
spcEXTERN  void       spSolve( spMatrix, spREAL[], spREAL[] );
spcEXTERN  void       spSolveTransposed( spMatrix,
                                spREAL[], spREAL[] );
#endif /* NOT  (spCOMPLEX && spSEPARATED_COMPLEX_VECTORS) */
#endif  /* spOKAY */
