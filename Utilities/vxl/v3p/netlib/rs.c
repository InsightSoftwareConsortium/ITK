#include "f2c.h"
#include "netlib.h"

/* Subroutine */ void rs_(nm, n, a, w, matz, z, fv1, fv2, ierr)
const integer *nm, *n;
doublereal *a, *w;
const integer *matz;
doublereal *z, *fv1, *fv2;
integer *ierr;
{
/*     this subroutine calls the recommended sequence of                */
/*     subroutines from the eigensystem subroutine package (eispack)    */
/*     to find the eigenvalues and eigenvectors (if desired)            */
/*     of a real symmetric matrix.                                      */
/*                                                                      */
/*     on input                                                         */
/*                                                                      */
/*        nm  must be set to the row dimension of the two-dimensional   */
/*        array parameters as declared in the calling program           */
/*        dimension statement.                                          */
/*                                                                      */
/*        n  is the order of the matrix  a.                             */
/*                                                                      */
/*        a  contains the real symmetric matrix.                        */
/*                                                                      */
/*        matz  is an integer variable set equal to zero if             */
/*        only eigenvalues are desired.  otherwise it is set to         */
/*        any non-zero integer for both eigenvalues and eigenvectors.   */
/*                                                                      */
/*     on output                                                        */
/*                                                                      */
/*        w  contains the eigenvalues in ascending order.               */
/*                                                                      */
/*        z  contains the eigenvectors if matz is not zero.             */
/*                                                                      */
/*        ierr  is an integer output variable set equal to an error     */
/*           completion code described in the documentation for tqlrat  */
/*           and tql2.  the normal completion code is zero.             */
/*                                                                      */
/*        fv1  and  fv2  are temporary storage arrays.                  */
/*                                                                      */
/*     questions and comments should be directed to burton s. garbow,   */
/*     mathematics and computer science div, argonne national laboratory*/
/*                                                                      */
/*     this version dated august 1983.                                  */
/*                                                                      */
/*   ------------------------------------------------------------------ */

    if (*n > *nm) {
        *ierr = *n * 10;
        return;
    }
    if (*matz == 0) {
/*     .......... find eigenvalues only .......... */
        tred1_(nm, n, a, w, fv1, fv2);
/*  tqlrat encounters catastrophic underflow on the Vax */
        tql1_(n, w, fv1, ierr);
        return;
    }
/*     .......... find both eigenvalues and eigenvectors .......... */
    tred2_(nm, n, a, w, fv1, z);
    tql2_(nm, n, w, fv1, z, ierr);
    return;
} /* rs_ */

