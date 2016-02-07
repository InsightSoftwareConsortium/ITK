/* CONFIGURATION MACRO DEFINITIONS for sparse matrix routines */
/*!
 *  \file
 *
 *  This file contains macros for the sparse matrix routines that are used
 *  to define the personality of the routines.  The user is expected to
 *  modify this file to maximize the performance of the routines with
 *  his/her matrices.
 *
 *  Macros are distinguished by using solely capital letters in their
 *  identifiers.  This contrasts with C defined identifiers which are
 *  strictly lower case, and program variable and procedure names which use
 *  both upper and lower case.
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


#ifndef spCONFIG_DEFS
#define spCONFIG_DEFS


#ifdef spINSIDE_SPARSE
/*
 *  OPTIONS
 *
 *  These are compiler options.  Set each option to one to compile that
 *  section of the code.  If a feature is not desired, set the macro
 *  to NO.
 */

/* Begin options. */

/* Arithmetic Precision
 *
 * The precision of the arithmetic used by Sparse can be set by
 * changing changing the spREAL macro.  This macro is
 * contained in the file spMatrix.h.  It is strongly suggested to
 * used double precision with circuit simulators.  Note that
 * because C always performs arithmetic operations in double
 * precision, the only benefit to using single precision is that
 * less storage is required.  There is often a noticeable speed
 * penalty when using single precision.  Sparse internally refers
 * to a spREAL as a RealNumber.
 */

/*!
 * This specifies that the routines are expected to handle real
 * systems of equations.  The routines can be compiled to handle
 * both real and complex systems at the same time, but there is a
 * slight speed and memory advantage if the routines are complied
 * to handle only real systems of equations.
 */
#define  REAL                           YES

/*!
 * Setting this compiler flag true (1) makes the matrix
 * expandable before it has been factored.  If the matrix is
 * expandable, then if an element is added that would be
 * considered out of bounds in the current matrix, the size of
 * the matrix is increased to hold that element.  As a result,
 * the size of the matrix need not be known before the matrix is
 * built.  The matrix can be allocated with size zero and expanded.
 */
#define  EXPANDABLE                     YES

/*!
 * This option allows the set of external row and column numbers
 * to be non-packed.  In other words, the row and column numbers
 * do not have to be contiguous.  The priced paid for this
 * flexibility is that when \a TRANSLATE is set true, the time
 * required to initially build the matrix will be greater because
 * the external row and column number must be translated into
 * internal equivalents.  This translation brings about other
 * benefits though.  First, the spGetElement() and
 * spGetAdmittance() routines may be used after the matrix has
 * been factored.  Further, elements, and even rows and columns,
 * may be added to the matrix, and row and columns may be deleted
 * from the matrix, after it has been factored.  Note that when
 * the set of row and column number is not a packed set, neither
 * are the \a RHS and \a Solution vectors.  Thus the size of these
 * vectors must be at least as large as the external size, which
 * is the value of the largest given row or column numbers.
 */
#define  TRANSLATE                      YES

/*!
 * Causes the spInitialize(), spGetInitInfo(), and
 * spInstallInitInfo() routines to be compiled.  These routines
 * allow the user to store and read one pointer in each nonzero
 * element in the matrix.  spInitialize() then calls a user
 * specified function for each structural nonzero in the matrix,
 * and includes this pointer as well as the external row and
 * column numbers as arguments.  This allows the user to write
 * custom matrix initialization routines.
 */
#define  INITIALIZE                     YES

/*!
 * Many matrices, and in particular node- and modified-node
 * admittance matrices, tend to be nearly symmetric and nearly
 * diagonally dominant.  For these matrices, it is a good idea to
 * select pivots from the diagonal.  With this option enabled,
 * this is exactly what happens, though if no satisfactory pivot
 * can be found on the diagonal, an off-diagonal pivot will be
 * used.  If this option is disabled, Sparse does not
 * preferentially search the diagonal.  Because of this, Sparse
 * has a wider variety of pivot candidates available, and so
 * presumably fewer fill-ins will be created.  However, the
 * initial pivot selection process will take considerably longer.
 * If working with node admittance matrices, or other matrices
 * with a strong diagonal, it is probably best to use
 * \a DIAGONAL_PIVOTING for two reasons.  First, accuracy will be
 * better because pivots will be chosen from the large diagonal
 * elements, thus reducing the chance of growth.  Second, a near
 * optimal ordering will be chosen quickly.  If the class of
 * matrices you are working with does not have a strong diagonal,
 * do not use \a DIAGONAL_PIVOTING, but consider using a larger
 * threshold.  When \a DIAGONAL_PIVOTING is turned off, the following
 * options and constants are not used: \a MODIFIED_MARKOWITZ,
 * \a MAX_MARKOWITZ_TIES, and \a TIES_MULTIPLIER.
 */
#define  DIAGONAL_PIVOTING              YES

/*!
 * This determines whether arrays start at an index of zero or one.
 * This option is necessitated by the fact that standard C
 * convention dictates that arrays begin with an index of zero but
 * the standard mathematic convention states that arrays begin with
 * an index of one.  So if you prefer to start your arrays with
 * zero, or your calling Sparse from FORTRAN, set ARRAY_OFFSET to
 * NO or 0.  Otherwise, set ARRAY_OFFSET to YES or 1.  Note that if
 * you use an offset of one, the arrays that you pass to Sparse
 * must have an allocated length of one plus the size of the
 * matrix.  ARRAY_OFFSET must be either 0 or 1, no other offsets
 * are valid.
 */
#define  ARRAY_OFFSET                   NOT FORTRAN

/*!
 * This specifies that the modified Markowitz method of pivot
 * selection is to be used.  The modified Markowitz method differs
 * from standard Markowitz in two ways.  First, under modified
 * Markowitz, the search for a pivot can be terminated early if a
 * adequate (in terms of sparsity) pivot candidate is found.
 * Thus, when using modified Markowitz, the initial factorization
 * can be faster, but at the expense of a suboptimal pivoting
 * order that may slow subsequent factorizations.  The second
 * difference is in the way modified Markowitz breaks Markowitz
 * ties.  When two or more elements are pivot candidates and they
 * all have the same Markowitz product, then the tie is broken by
 * choosing the element that is best numerically.  The numerically
 * best element is the one with the largest ratio of its magnitude
 * to the magnitude of the largest element in the same column,
 * excluding itself.  The modified Markowitz method results in
 * marginally better accuracy.  This option is most appropriate
 * for use when working with very large matrices where the initial
 * factor time represents an unacceptable burden. \a NO is recommended.
 */
#define  MODIFIED_MARKOWITZ             NO

/*!
 * This specifies that the spDeleteRowAndCol() routine
 * should be compiled.  Note that for this routine to be
 * compiled, both \a DELETE and \a TRANSLATE should be set true.
 */
#define  DELETE                         YES

/*!
 * This specifies that the spStripFills() routine should be compiled.
 */
#define  STRIP                          YES

/*!
 * This specifies that the routine that preorders modified node
 * admittance matrices should be compiled.  This routine results
 * in greater speed and accuracy if used with this type of
 * matrix.
 */
#define  MODIFIED_NODAL                 YES

/*!
 * This specifies that the routines that allow four related
 * elements to be entered into the matrix at once should be
 * compiled.  These elements are usually related to an
 * admittance.  The routines affected by \a QUAD_ELEMENT are the
 * spGetAdmittance(), spGetQuad() and spGetOnes() routines.
 */
#define  QUAD_ELEMENT                   YES

/*!
 * This specifies that the routines that solve the matrix as if
 * it was transposed should be compiled.  These routines are
 * useful when performing sensitivity analysis using the adjoint
 * method.
 */
#define  TRANSPOSE                      YES

/*!
 * This specifies that the routine that performs scaling on the
 * matrix should be complied.  Scaling is not strongly
 * supported.  The routine to scale the matrix is provided, but
 * no routines are provided to scale and descale the RHS and
 * Solution vectors.  It is suggested that if scaling is desired,
 * it only be preformed when the pivot order is being chosen [in
 * spOrderAndFactor()].  This is the only time scaling has
 * an effect.  The scaling may then either be removed from the
 * solution by the user or the scaled factors may simply be
 * thrown away. \a NO is recommended.
 */
#define  SCALING                        YES

/*!
 * This specifies that routines that are used to document the
 * matrix, such as spPrint() and spFileMatrix(), should be
 * compiled.
 */
#define  DOCUMENTATION                  YES

/*!
 * This specifies that routines that are used to multily the
 * matrix by a vector, such as spMultiply() and spMultTransposed(), should be
 * compiled.
 */
#define  MULTIPLICATION                 YES

/*!
 * This specifies that the routine spDeterminant() should be complied.
 */
#define  DETERMINANT                    YES

/*!
 * This specifies that spLargestElement() and spRoundoff() should
 * be compiled.  These routines are used to check the stability (and
 * hence the quality of the pivoting) of the factorization by
 * computing a bound on the size of the element is the matrix
 * \f$ E = A - LU \f$.  If this bound is very high after applying
 * spOrderAndFactor(), then the pivot threshold should be raised.
 * If the bound increases greatly after using spFactor(), then the
 * matrix should probably be reordered. Recomend \a NO.
 */
#define  STABILITY                      YES

/*!
 * This specifies that spCondition() and spNorm(), the code that
 * computes a good estimate of the condition number of the matrix,
 * should be compiled. Recomend \a NO.
 */
#define  CONDITION                      YES

/*!
 * This specifies that spPseudoCondition(), the code that computes
 * a crude and easily fooled indicator of ill-conditioning in the
 * matrix, should be compiled. Recomend \a NO.
 */
#define  PSEUDOCONDITION                YES

/*!
 * This specifies that the \a FORTRAN interface routines should be
 * compiled.  When interfacing to \a FORTRAN programs, the \a ARRAY_OFFSET
 * options should be set to NO.
 */
#define  FORTRAN                        NO

/*!
 * This specifies that additional error checking will be compiled.
 * The type of error checked are those that are common when the
 * matrix routines are first integrated into a user's program.  Once
 * the routines have been integrated in and are running smoothly, this
 * option should be turned off. \a YES is recommended.
 */
#define  DEBUG                          YES

#endif /* spINSIDE_SPARSE */

/*
 *  The following options affect Sparse exports and so are exported as a
 *  side effect.  For this reason they use the `sp' prefix.  The boolean
 *  constants YES an NO are not defined in spMatrix.h to avoid conflicts
 *  with user code, so use 0 for NO and 1 for YES.
 */

/*!
 * This specifies that the routines will be complied to handle
 * complex systems of equations.
 */
 #define  spCOMPLEX                      0

/*!
 * This specifies the format for complex vectors.  If this is set
 * false then a complex vector is made up of one double sized
 * array of RealNumber's in which the real and imaginary numbers
 * are placed alternately in the array.  In other
 * words, the first entry would be Complex[1].Real, then comes
 * Complex[1].Imag, then Complex[2].Real, etc.  If
 * \a spSEPARATED_COMPLEX_VECTORS is set true, then each complex
 * vector is represented by two arrays of \a spREALs, one with
 * the real terms, the other with the imaginary. \a NO is recommended.
 */
#define  spSEPARATED_COMPLEX_VECTORS    0

#ifdef spINSIDE_SPARSE

/*
 *  MATRIX CONSTANTS
 *
 *  These constants are used throughout the sparse matrix routines.  They
 *  should be set to suit the type of matrix being solved.
 */

/* Begin constants. */

/*!
 * The relative threshold used if the user enters an invalid
 * threshold.  Also the threshold used by spFactor() when
 * calling spOrderAndFactor().  The default threshold should
 * not be less than or equal to zero nor larger than one.
 * 0.001 is recommended.
 */
#define  DEFAULT_THRESHOLD              1.0e-3

/*!
 * This indicates whether spOrderAndFactor() should use diagonal
 * pivoting as default.  This issue only arises when
 * spOrderAndFactor() is called from spFactor(). \a YES is recommended.
 */
#define  DIAG_PIVOTING_AS_DEFAULT       YES

/*!
 * This number multiplied by the size of the matrix equals the number
 * of elements for which memory is initially allocated in spCreate().
 * 6 is recommended.
 */
#define  SPACE_FOR_ELEMENTS             6

/*!
 * This number multiplied by the size of the matrix equals the number
 * of elements for which memory is initially allocated and specifically
 * reserved for fill-ins in spCreate(). 4 is recommended.
 */
#define  SPACE_FOR_FILL_INS             4

/*!
 * The number of matrix elements requested from the malloc utility on
 * each call to it.  Setting this value greater than 1 reduces the
 * amount of overhead spent in this system call. On a virtual memory
 * machine, its good to allocate slightly less than a page worth of
 * elements at a time (or some multiple thereof).
 * 31 is recommended.
 */
#define  ELEMENTS_PER_ALLOCATION        31

/*!
 * The minimum allocated size of a matrix.  Note that this does not
 * limit the minimum size of a matrix.  This just prevents having to
 * resize a matrix many times if the matrix is expandable, large and
 * allocated with an estimated size of zero.  This number should not
 * be less than one.
 */
#define  MINIMUM_ALLOCATED_SIZE         6

/*!
 * The amount the allocated size of the matrix is increased when it
 * is expanded.
 */
#define  EXPANSION_FACTOR               1.5

/*!
 * Some terminology should be defined.  The Markowitz row count is the number
 * of non-zero elements in a row excluding the one being considered as pivot.
 * There is one Markowitz row count for every row.  The Markowitz column
 * is defined similarly for columns.  The Markowitz product for an element
 * is the product of its row and column counts. It is a measure of how much
 * work would be required on the next step of the factorization if that
 * element were chosen to be pivot.  A small Markowitz product is desirable.
 *
 * This number is used for two slightly different things, both of which
 * relate to the search for the best pivot.  First, it is the maximum
 * number of elements that are Markowitz tied that will be sifted
 * through when trying to find the one that is numerically the best.
 * Second, it creates an upper bound on how large a Markowitz product
 * can be before it eliminates the possibility of early termination
 * of the pivot search.  In other words, if the product of the smallest
 * Markowitz product yet found and \a TIES_MULTIPLIER is greater than
 * \a MAX_MARKOWITZ_TIES, then no early termination takes place.
 * Set \a MAX_MARKOWITZ_TIES to some small value if no early termination of
 * the pivot search is desired. An array of RealNumbers is allocated
 * of size \a MAX_MARKOWITZ_TIES so it must be positive and shouldn't
 * be too large.  Active when MODIFIED_MARKOWITZ is 1 (YES).
 * 100 is recommended.
 * \see TIES_MULTIPLIER
 */
#define  MAX_MARKOWITZ_TIES             100

/*!
 * Specifies the number of Markowitz ties that are allowed to occur
 * before the search for the pivot is terminated early.  Set to some
 * large value if no early termination of the pivot search is desired.
 * This number is multiplied times the Markowitz product to determine
 * how many ties are required for early termination.  This means that
 * more elements will be searched before early termination if a large
 * number of fill-ins could be created by accepting what is currently
 * considered the best choice for the pivot.  Active when
 * \a MODIFIED_MARKOWITZ is 1 (YES).  Setting this number to zero
 * effectively eliminates all pivoting, which should be avoided.
 * This number must be positive.  \a TIES_MULTIPLIER is also used when
 * diagonal pivoting breaks down. 5 is recommended.
 * \see MAX_MARKOWITZ_TIES
 */
#define  TIES_MULTIPLIER                5

/*!
 * Which partition mode is used by spPartition() as default.
 * Possibilities include \a spDIRECT_PARTITION (each row used direct
 * addressing, best for a few relatively dense matrices),
 * \a spINDIRECT_PARTITION (each row used indirect addressing, best
 * for a few very sparse matrices), and \a spAUTO_PARTITION (direct or
 * indirect addressing is chosen on a row-by-row basis, carries a large
 * overhead, but speeds up both dense and sparse matrices, best if there
 * is a large number of matrices that can use the same ordering.
 */
#define  DEFAULT_PARTITION              spAUTO_PARTITION

/*!
 * The number of characters per page width.  Set to 80 for terminal,
 * 132 for line printer. Controls how many columns printed by
 * spPrint() per page width.
 */
#define  PRINTER_WIDTH  80

#endif /* spINSIDE_SPARSE */
/*
 * PORTABILITY MACROS
 */

#ifdef __STDC__
#   define spcCONCAT(prefix,suffix)        prefix ## suffix
#   define spcQUOTE(x)                     # x
#   define spcFUNC_NEEDS_FILE(func,file)   \
                func ## _requires_ ## file ## _to_be_included_
#else
#   define spcCONCAT(prefix,suffix)        prefix/**/suffix
#   define spcQUOTE(x)                     "x"
#   define spcFUNC_NEEDS_FILE(func,file)   \
                func/**/_requires_/**/file/**/_to_be_included_
#endif

#if defined(__cplusplus) || defined(c_plusplus)
    /*
     * Definitions for C++
     */
#   define spcEXTERN                extern "C"
#   define spcNO_ARGS
#   define spcCONST                const
    typedef void *spGenericPtr;
#else
#ifdef __STDC__
    /*
     * Definitions for ANSI C
     */
#   define spcEXTERN                extern
#   define spcNO_ARGS                void
#   define spcCONST                const
    typedef void *spGenericPtr;
#   else
    /*
     * Definitions for K&R C -- ignore function prototypes
     */
#   define spcEXTERN                extern
#   define spcNO_ARGS
#   define spcCONST
    typedef char *spGenericPtr;
#endif
#endif

#ifdef spINSIDE_SPARSE

/*
 *  MACHINE CONSTANTS
 *
 *  These numbers must be updated when the program is ported to a new machine.
 */

/* Begin machine constants. */
#include <limits.h>
#include <float.h>

/*! The resolution of spREAL. */
#define  MACHINE_RESOLUTION      DBL_EPSILON

/*! The largest possible value of spREAL. */
#define  LARGEST_REAL            DBL_MAX

/*! The smalles possible positive value of spREAL. */
#define  SMALLEST_REAL           DBL_MIN

/*! The largest possible value of shorts. */
#define  LARGEST_SHORT_INTEGER   SHRT_MAX

/*! The largest possible value of longs. */
#define  LARGEST_LONG_INTEGER    LONG_MAX

/*! The largest possible value of ints. */
#define  LARGEST_INTEGER    INT_MAX

/* ANNOTATION */
/*!
 * This macro changes the amount of annotation produced by the matrix
 * routines.  The annotation is used as a debugging aid.  Change the number
 * associated with \a ANNOTATE to change the amount of annotation produced by
 * the program. Possible values include \a NONE, \a ON_STRANGE_BEHAVIOR, and
 * \a FULL. \a NONE is recommended.
 */
#define  ANNOTATE               NONE

/*!
 * A possible value for \a ANNOTATE. Disables all annotation.
 */
#define  NONE                   0

/*!
 * A possible value for \a ANNOTATE. Causes annotation to be produce
 * upon unusual occurrences only.
 */
#define  ON_STRANGE_BEHAVIOR    1

/*!
 * A possible value for \a ANNOTATE. Enables full annotation.
 */
#define  FULL                   2

#endif /* spINSIDE_SPARSE */
#endif /* spCONFIG_DEFS */
