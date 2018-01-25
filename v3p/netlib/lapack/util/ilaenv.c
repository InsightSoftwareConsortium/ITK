/* lapack/util/ilaenv.f -- translated by f2c (version 20050501).
   You must link the resulting object file with libf2c:
        on Microsoft Windows system, link with libf2c.lib;
        on Linux or Unix systems, link with .../path/to/libf2c.a -lm
        or, if you install libf2c.a in a standard place, with -lf2c -lm
        -- in that order, at the end of the command line, as in
                cc *.o -lf2c -lm
        Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

                http://www.netlib.org/f2c/libf2c.zip
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "v3p_netlib.h"

/* Table of constant values */

static integer c__0 = 0;
static real c_b162 = (float)0.;
static real c_b163 = (float)1.;
static integer c__1 = 1;

/*<    >*/
integer ilaenv_(integer *ispec, char *name__, char *opts, integer *n1,
        integer *n2, integer *n3, integer *n4, ftnlen name_len, ftnlen
        opts_len)
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer i__;
    char c1[1], c2[2], c3[3], c4[2];
    integer ic, nb, iz, nx;
    logical cname, sname;
    integer nbmin;
    extern integer ieeeck_(integer *, real *, real *);
    char subnam[6];
    (void)opts;
    (void)n3;
    (void)opts_len;

/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER*( * )    NAME, OPTS >*/
/*<       INTEGER            ISPEC, N1, N2, N3, N4 >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ILAENV is called from the LAPACK routines to choose problem-dependent */
/*  parameters for the local environment.  See ISPEC for a description of */
/*  the parameters. */

/*  This version provides a set of parameters which should give good, */
/*  but not optimal, performance on many of the currently available */
/*  computers.  Users are encouraged to modify this subroutine to set */
/*  the tuning parameters for their particular machine using the option */
/*  and problem size information in the arguments. */

/*  This routine will not function correctly if it is converted to all */
/*  lower case.  Converting it to all upper case is allowed. */

/*  Arguments */
/*  ========= */

/*  ISPEC   (input) INTEGER */
/*          Specifies the parameter to be returned as the value of */
/*          ILAENV. */
/*          = 1: the optimal blocksize; if this value is 1, an unblocked */
/*               algorithm will give the best performance. */
/*          = 2: the minimum block size for which the block routine */
/*               should be used; if the usable block size is less than */
/*               this value, an unblocked routine should be used. */
/*          = 3: the crossover point (in a block routine, for N less */
/*               than this value, an unblocked routine should be used) */
/*          = 4: the number of shifts, used in the nonsymmetric */
/*               eigenvalue routines */
/*          = 5: the minimum column dimension for blocking to be used; */
/*               rectangular blocks must have dimension at least k by m, */
/*               where k is given by ILAENV(2,...) and m by ILAENV(5,...) */
/*          = 6: the crossover point for the SVD (when reducing an m by n */
/*               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds */
/*               this value, a QR factorization is used first to reduce */
/*               the matrix to a triangular form.) */
/*          = 7: the number of processors */
/*          = 8: the crossover point for the multishift QR and QZ methods */
/*               for nonsymmetric eigenvalue problems. */
/*          = 9: maximum size of the subproblems at the bottom of the */
/*               computation tree in the divide-and-conquer algorithm */
/*               (used by xGELSD and xGESDD) */
/*          =10: ieee NaN arithmetic can be trusted not to trap */
/*          =11: infinity arithmetic can be trusted not to trap */

/*  NAME    (input) CHARACTER*(*) */
/*          The name of the calling subroutine, in either upper case or */
/*          lower case. */

/*  OPTS    (input) CHARACTER*(*) */
/*          The character options to the subroutine NAME, concatenated */
/*          into a single character string.  For example, UPLO = 'U', */
/*          TRANS = 'T', and DIAG = 'N' for a triangular routine would */
/*          be specified as OPTS = 'UTN'. */

/*  N1      (input) INTEGER */
/*  N2      (input) INTEGER */
/*  N3      (input) INTEGER */
/*  N4      (input) INTEGER */
/*          Problem dimensions for the subroutine NAME; these may not all */
/*          be required. */

/* (ILAENV) (output) INTEGER */
/*          >= 0: the value of the parameter specified by ISPEC */
/*          < 0:  if ILAENV = -k, the k-th argument had an illegal value. */

/*  Further Details */
/*  =============== */

/*  The following conventions have been used when calling ILAENV from the */
/*  LAPACK routines: */
/*  1)  OPTS is a concatenation of all of the character options to */
/*      subroutine NAME, in the same order that they appear in the */
/*      argument list for NAME, even if they are not used in determining */
/*      the value of the parameter specified by ISPEC. */
/*  2)  The problem dimensions N1, N2, N3, N4 are specified in the order */
/*      that they appear in the argument list for NAME.  N1 is used */
/*      first, N2 second, and so on, and unused problem dimensions are */
/*      passed a value of -1. */
/*  3)  The parameter value returned by ILAENV is checked for validity in */
/*      the calling subroutine.  For example, ILAENV is used to retrieve */
/*      the optimal blocksize for STRTRI as follows: */

/*      NB = ILAENV( 1, 'STRTRI', UPLO // DIAG, N, -1, -1, -1 ) */
/*      IF( NB.LE.1 ) NB = MAX( 1, N ) */

/*  ===================================================================== */

/*     .. Local Scalars .. */
/*<       LOGICAL            CNAME, SNAME >*/
/*<       CHARACTER*1        C1 >*/
/*<       CHARACTER*2        C2, C4 >*/
/*<       CHARACTER*3        C3 >*/
/*<       CHARACTER*6        SUBNAM >*/
/*<       INTEGER            I, IC, IZ, NB, NBMIN, NX >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          CHAR, ICHAR, INT, MIN, REAL >*/
/*     .. */
/*     .. External Functions .. */
/*<       INTEGER            IEEECK >*/
/*<       EXTERNAL           IEEECK >*/
/*     .. */
/*     .. Executable Statements .. */

/*<    >*/
    switch (*ispec) {
        case 1:  goto L100;
        case 2:  goto L100;
        case 3:  goto L100;
        case 4:  goto L400;
        case 5:  goto L500;
        case 6:  goto L600;
        case 7:  goto L700;
        case 8:  goto L800;
        case 9:  goto L900;
        case 10:  goto L1000;
        case 11:  goto L1100;
    }

/*     Invalid value for ISPEC */

/*<       ILAENV = -1 >*/
    ret_val = -1;
/*<       RETURN >*/
    return ret_val;

/*<   100 CONTINUE >*/
L100:

/*     Convert NAME to upper case if the first character is lower case. */

/*<       ILAENV = 1 >*/
    ret_val = 1;
/*<       SUBNAM = NAME >*/
    s_copy(subnam, name__, (ftnlen)6, name_len);
/*<       IC = ICHAR( SUBNAM( 1:1 ) ) >*/
    ic = *(unsigned char *)subnam;
/*<       IZ = ICHAR( 'Z' ) >*/
    iz = 'Z';
/*<       IF( IZ.EQ.90 .OR. IZ.EQ.122 ) THEN >*/
    if (iz == 90 || iz == 122) {

/*        ASCII character set */

/*<          IF( IC.GE.97 .AND. IC.LE.122 ) THEN >*/
        if (ic >= 97 && ic <= 122) {
/*<             SUBNAM( 1:1 ) = CHAR( IC-32 ) >*/
            *(unsigned char *)subnam = (unsigned char) (ic - 32);
/*<             DO 10 I = 2, 6 >*/
            for (i__ = 2; i__ <= 6; ++i__) {
/*<                IC = ICHAR( SUBNAM( I:I ) ) >*/
                ic = *(unsigned char *)&subnam[i__ - 1];
/*<    >*/
                if (ic >= 97 && ic <= 122) {
                    *(unsigned char *)&subnam[i__ - 1] = (unsigned char) (ic - 32);
                }
/*<    10       CONTINUE >*/
/* L10: */
            }
/*<          END IF >*/
        }

/*<       ELSE IF( IZ.EQ.233 .OR. IZ.EQ.169 ) THEN >*/
    } else if (iz == 233 || iz == 169) {

/*        EBCDIC character set */

/*<    >*/
        if ((ic >= 129 && ic <= 137) || (ic >= 145 && ic <= 153) || (ic >= 162 &&
                ic <= 169)) {
/*<             SUBNAM( 1:1 ) = CHAR( IC+64 ) >*/
            *(unsigned char *)subnam = (unsigned char) (ic + 64);
/*<             DO 20 I = 2, 6 >*/
            for (i__ = 2; i__ <= 6; ++i__) {
/*<                IC = ICHAR( SUBNAM( I:I ) ) >*/
                ic = *(unsigned char *)&subnam[i__ - 1];
/*<    >*/
                if ((ic >= 129 && ic <= 137) || (ic >= 145 && ic <= 153) || (ic >=
                        162 && ic <= 169)) {
                    *(unsigned char *)&subnam[i__ - 1] = (unsigned char) (ic + 64);
                }
/*<    20       CONTINUE >*/
/* L20: */
            }
/*<          END IF >*/
        }

/*<       ELSE IF( IZ.EQ.218 .OR. IZ.EQ.250 ) THEN >*/
    } else if (iz == 218 || iz == 250) {

/*        Prime machines:  ASCII+128 */

/*<          IF( IC.GE.225 .AND. IC.LE.250 ) THEN >*/
        if (ic >= 225 && ic <= 250) {
/*<             SUBNAM( 1:1 ) = CHAR( IC-32 ) >*/
            *(unsigned char *)subnam = (unsigned char) (ic - 32);
/*<             DO 30 I = 2, 6 >*/
            for (i__ = 2; i__ <= 6; ++i__) {
/*<                IC = ICHAR( SUBNAM( I:I ) ) >*/
                ic = *(unsigned char *)&subnam[i__ - 1];
/*<    >*/
                if (ic >= 225 && ic <= 250) {
                    *(unsigned char *)&subnam[i__ - 1] = (unsigned char) (ic - 32);
                }
/*<    30       CONTINUE >*/
/* L30: */
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<       C1 = SUBNAM( 1:1 ) >*/
    *(unsigned char *)c1 = *(unsigned char *)subnam;
/*<       SNAME = C1.EQ.'S' .OR. C1.EQ.'D' >*/
    sname = *(unsigned char *)c1 == 'S' || *(unsigned char *)c1 == 'D';
/*<       CNAME = C1.EQ.'C' .OR. C1.EQ.'Z' >*/
    cname = *(unsigned char *)c1 == 'C' || *(unsigned char *)c1 == 'Z';
/*<    >*/
    if (! (cname || sname)) {
        return ret_val;
    }
/*<       C2 = SUBNAM( 2:3 ) >*/
    s_copy(c2, subnam + 1, (ftnlen)2, (ftnlen)2);
/*<       C3 = SUBNAM( 4:6 ) >*/
    s_copy(c3, subnam + 3, (ftnlen)3, (ftnlen)3);
/*<       C4 = C3( 2:3 ) >*/
    s_copy(c4, c3 + 1, (ftnlen)2, (ftnlen)2);

/*<       GO TO ( 110, 200, 300 ) ISPEC >*/
    switch (*ispec) {
        case 1:  goto L110;
        case 2:  goto L200;
        case 3:  goto L300;
    }

/*<   110 CONTINUE >*/
L110:

/*     ISPEC = 1:  block size */

/*     In these examples, separate code is provided for setting NB for */
/*     real and complex.  We assume that NB will take the same value in */
/*     single or double precision. */

/*<       NB = 1 >*/
    nb = 1;

/*<       IF( C2.EQ.'GE' ) THEN >*/
    if (s_cmp(c2, "GE", (ftnlen)2, (ftnlen)2) == 0) {
/*<          IF( C3.EQ.'TRF' ) THEN >*/
        if (s_cmp(c3, "TRF", (ftnlen)3, (ftnlen)3) == 0) {
/*<             IF( SNAME ) THEN >*/
            if (sname) {
/*<                NB = 64 >*/
                nb = 64;
/*<             ELSE >*/
            } else {
/*<                NB = 64 >*/
                nb = 64;
/*<             END IF >*/
            }
/*<    >*/
        } else if (s_cmp(c3, "QRF", (ftnlen)3, (ftnlen)3) == 0 || s_cmp(c3,
                "RQF", (ftnlen)3, (ftnlen)3) == 0 || s_cmp(c3, "LQF", (ftnlen)
                3, (ftnlen)3) == 0 || s_cmp(c3, "QLF", (ftnlen)3, (ftnlen)3)
                == 0) {
/*<             IF( SNAME ) THEN >*/
            if (sname) {
/*<                NB = 32 >*/
                nb = 32;
/*<             ELSE >*/
            } else {
/*<                NB = 32 >*/
                nb = 32;
/*<             END IF >*/
            }
/*<          ELSE IF( C3.EQ.'HRD' ) THEN >*/
        } else if (s_cmp(c3, "HRD", (ftnlen)3, (ftnlen)3) == 0) {
/*<             IF( SNAME ) THEN >*/
            if (sname) {
/*<                NB = 32 >*/
                nb = 32;
/*<             ELSE >*/
            } else {
/*<                NB = 32 >*/
                nb = 32;
/*<             END IF >*/
            }
/*<          ELSE IF( C3.EQ.'BRD' ) THEN >*/
        } else if (s_cmp(c3, "BRD", (ftnlen)3, (ftnlen)3) == 0) {
/*<             IF( SNAME ) THEN >*/
            if (sname) {
/*<                NB = 32 >*/
                nb = 32;
/*<             ELSE >*/
            } else {
/*<                NB = 32 >*/
                nb = 32;
/*<             END IF >*/
            }
/*<          ELSE IF( C3.EQ.'TRI' ) THEN >*/
        } else if (s_cmp(c3, "TRI", (ftnlen)3, (ftnlen)3) == 0) {
/*<             IF( SNAME ) THEN >*/
            if (sname) {
/*<                NB = 64 >*/
                nb = 64;
/*<             ELSE >*/
            } else {
/*<                NB = 64 >*/
                nb = 64;
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       ELSE IF( C2.EQ.'PO' ) THEN >*/
    } else if (s_cmp(c2, "PO", (ftnlen)2, (ftnlen)2) == 0) {
/*<          IF( C3.EQ.'TRF' ) THEN >*/
        if (s_cmp(c3, "TRF", (ftnlen)3, (ftnlen)3) == 0) {
/*<             IF( SNAME ) THEN >*/
            if (sname) {
/*<                NB = 64 >*/
                nb = 64;
/*<             ELSE >*/
            } else {
/*<                NB = 64 >*/
                nb = 64;
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       ELSE IF( C2.EQ.'SY' ) THEN >*/
    } else if (s_cmp(c2, "SY", (ftnlen)2, (ftnlen)2) == 0) {
/*<          IF( C3.EQ.'TRF' ) THEN >*/
        if (s_cmp(c3, "TRF", (ftnlen)3, (ftnlen)3) == 0) {
/*<             IF( SNAME ) THEN >*/
            if (sname) {
/*<                NB = 64 >*/
                nb = 64;
/*<             ELSE >*/
            } else {
/*<                NB = 64 >*/
                nb = 64;
/*<             END IF >*/
            }
/*<          ELSE IF( SNAME .AND. C3.EQ.'TRD' ) THEN >*/
        } else if (sname && s_cmp(c3, "TRD", (ftnlen)3, (ftnlen)3) == 0) {
/*<             NB = 32 >*/
            nb = 32;
/*<          ELSE IF( SNAME .AND. C3.EQ.'GST' ) THEN >*/
        } else if (sname && s_cmp(c3, "GST", (ftnlen)3, (ftnlen)3) == 0) {
/*<             NB = 64 >*/
            nb = 64;
/*<          END IF >*/
        }
/*<       ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN >*/
    } else if (cname && s_cmp(c2, "HE", (ftnlen)2, (ftnlen)2) == 0) {
/*<          IF( C3.EQ.'TRF' ) THEN >*/
        if (s_cmp(c3, "TRF", (ftnlen)3, (ftnlen)3) == 0) {
/*<             NB = 64 >*/
            nb = 64;
/*<          ELSE IF( C3.EQ.'TRD' ) THEN >*/
        } else if (s_cmp(c3, "TRD", (ftnlen)3, (ftnlen)3) == 0) {
/*<             NB = 32 >*/
            nb = 32;
/*<          ELSE IF( C3.EQ.'GST' ) THEN >*/
        } else if (s_cmp(c3, "GST", (ftnlen)3, (ftnlen)3) == 0) {
/*<             NB = 64 >*/
            nb = 64;
/*<          END IF >*/
        }
/*<       ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN >*/
    } else if (sname && s_cmp(c2, "OR", (ftnlen)2, (ftnlen)2) == 0) {
/*<          IF( C3( 1:1 ).EQ.'G' ) THEN >*/
        if (*(unsigned char *)c3 == 'G') {
/*<    >*/
            if (s_cmp(c4, "QR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "RQ",
                    (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "LQ", (ftnlen)2, (
                    ftnlen)2) == 0 || s_cmp(c4, "QL", (ftnlen)2, (ftnlen)2) ==
                     0 || s_cmp(c4, "HR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(
                    c4, "TR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "BR", (
                    ftnlen)2, (ftnlen)2) == 0) {
/*<                NB = 32 >*/
                nb = 32;
/*<             END IF >*/
            }
/*<          ELSE IF( C3( 1:1 ).EQ.'M' ) THEN >*/
        } else if (*(unsigned char *)c3 == 'M') {
/*<    >*/
            if (s_cmp(c4, "QR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "RQ",
                    (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "LQ", (ftnlen)2, (
                    ftnlen)2) == 0 || s_cmp(c4, "QL", (ftnlen)2, (ftnlen)2) ==
                     0 || s_cmp(c4, "HR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(
                    c4, "TR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "BR", (
                    ftnlen)2, (ftnlen)2) == 0) {
/*<                NB = 32 >*/
                nb = 32;
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN >*/
    } else if (cname && s_cmp(c2, "UN", (ftnlen)2, (ftnlen)2) == 0) {
/*<          IF( C3( 1:1 ).EQ.'G' ) THEN >*/
        if (*(unsigned char *)c3 == 'G') {
/*<    >*/
            if (s_cmp(c4, "QR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "RQ",
                    (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "LQ", (ftnlen)2, (
                    ftnlen)2) == 0 || s_cmp(c4, "QL", (ftnlen)2, (ftnlen)2) ==
                     0 || s_cmp(c4, "HR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(
                    c4, "TR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "BR", (
                    ftnlen)2, (ftnlen)2) == 0) {
/*<                NB = 32 >*/
                nb = 32;
/*<             END IF >*/
            }
/*<          ELSE IF( C3( 1:1 ).EQ.'M' ) THEN >*/
        } else if (*(unsigned char *)c3 == 'M') {
/*<    >*/
            if (s_cmp(c4, "QR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "RQ",
                    (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "LQ", (ftnlen)2, (
                    ftnlen)2) == 0 || s_cmp(c4, "QL", (ftnlen)2, (ftnlen)2) ==
                     0 || s_cmp(c4, "HR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(
                    c4, "TR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "BR", (
                    ftnlen)2, (ftnlen)2) == 0) {
/*<                NB = 32 >*/
                nb = 32;
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       ELSE IF( C2.EQ.'GB' ) THEN >*/
    } else if (s_cmp(c2, "GB", (ftnlen)2, (ftnlen)2) == 0) {
/*<          IF( C3.EQ.'TRF' ) THEN >*/
        if (s_cmp(c3, "TRF", (ftnlen)3, (ftnlen)3) == 0) {
/*<             IF( SNAME ) THEN >*/
            if (sname) {
/*<                IF( N4.LE.64 ) THEN >*/
                if (*n4 <= 64) {
/*<                   NB = 1 >*/
                    nb = 1;
/*<                ELSE >*/
                } else {
/*<                   NB = 32 >*/
                    nb = 32;
/*<                END IF >*/
                }
/*<             ELSE >*/
            } else {
/*<                IF( N4.LE.64 ) THEN >*/
                if (*n4 <= 64) {
/*<                   NB = 1 >*/
                    nb = 1;
/*<                ELSE >*/
                } else {
/*<                   NB = 32 >*/
                    nb = 32;
/*<                END IF >*/
                }
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       ELSE IF( C2.EQ.'PB' ) THEN >*/
    } else if (s_cmp(c2, "PB", (ftnlen)2, (ftnlen)2) == 0) {
/*<          IF( C3.EQ.'TRF' ) THEN >*/
        if (s_cmp(c3, "TRF", (ftnlen)3, (ftnlen)3) == 0) {
/*<             IF( SNAME ) THEN >*/
            if (sname) {
/*<                IF( N2.LE.64 ) THEN >*/
                if (*n2 <= 64) {
/*<                   NB = 1 >*/
                    nb = 1;
/*<                ELSE >*/
                } else {
/*<                   NB = 32 >*/
                    nb = 32;
/*<                END IF >*/
                }
/*<             ELSE >*/
            } else {
/*<                IF( N2.LE.64 ) THEN >*/
                if (*n2 <= 64) {
/*<                   NB = 1 >*/
                    nb = 1;
/*<                ELSE >*/
                } else {
/*<                   NB = 32 >*/
                    nb = 32;
/*<                END IF >*/
                }
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       ELSE IF( C2.EQ.'TR' ) THEN >*/
    } else if (s_cmp(c2, "TR", (ftnlen)2, (ftnlen)2) == 0) {
/*<          IF( C3.EQ.'TRI' ) THEN >*/
        if (s_cmp(c3, "TRI", (ftnlen)3, (ftnlen)3) == 0) {
/*<             IF( SNAME ) THEN >*/
            if (sname) {
/*<                NB = 64 >*/
                nb = 64;
/*<             ELSE >*/
            } else {
/*<                NB = 64 >*/
                nb = 64;
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       ELSE IF( C2.EQ.'LA' ) THEN >*/
    } else if (s_cmp(c2, "LA", (ftnlen)2, (ftnlen)2) == 0) {
/*<          IF( C3.EQ.'UUM' ) THEN >*/
        if (s_cmp(c3, "UUM", (ftnlen)3, (ftnlen)3) == 0) {
/*<             IF( SNAME ) THEN >*/
            if (sname) {
/*<                NB = 64 >*/
                nb = 64;
/*<             ELSE >*/
            } else {
/*<                NB = 64 >*/
                nb = 64;
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       ELSE IF( SNAME .AND. C2.EQ.'ST' ) THEN >*/
    } else if (sname && s_cmp(c2, "ST", (ftnlen)2, (ftnlen)2) == 0) {
/*<          IF( C3.EQ.'EBZ' ) THEN >*/
        if (s_cmp(c3, "EBZ", (ftnlen)3, (ftnlen)3) == 0) {
/*<             NB = 1 >*/
            nb = 1;
/*<          END IF >*/
        }
/*<       END IF >*/
    }
/*<       ILAENV = NB >*/
    ret_val = nb;
/*<       RETURN >*/
    return ret_val;

/*<   200 CONTINUE >*/
L200:

/*     ISPEC = 2:  minimum block size */

/*<       NBMIN = 2 >*/
    nbmin = 2;
/*<       IF( C2.EQ.'GE' ) THEN >*/
    if (s_cmp(c2, "GE", (ftnlen)2, (ftnlen)2) == 0) {
/*<    >*/
        if (s_cmp(c3, "QRF", (ftnlen)3, (ftnlen)3) == 0 || s_cmp(c3, "RQF", (
                ftnlen)3, (ftnlen)3) == 0 || s_cmp(c3, "LQF", (ftnlen)3, (
                ftnlen)3) == 0 || s_cmp(c3, "QLF", (ftnlen)3, (ftnlen)3) == 0)
                 {
/*<             IF( SNAME ) THEN >*/
            if (sname) {
/*<                NBMIN = 2 >*/
                nbmin = 2;
/*<             ELSE >*/
            } else {
/*<                NBMIN = 2 >*/
                nbmin = 2;
/*<             END IF >*/
            }
/*<          ELSE IF( C3.EQ.'HRD' ) THEN >*/
        } else if (s_cmp(c3, "HRD", (ftnlen)3, (ftnlen)3) == 0) {
/*<             IF( SNAME ) THEN >*/
            if (sname) {
/*<                NBMIN = 2 >*/
                nbmin = 2;
/*<             ELSE >*/
            } else {
/*<                NBMIN = 2 >*/
                nbmin = 2;
/*<             END IF >*/
            }
/*<          ELSE IF( C3.EQ.'BRD' ) THEN >*/
        } else if (s_cmp(c3, "BRD", (ftnlen)3, (ftnlen)3) == 0) {
/*<             IF( SNAME ) THEN >*/
            if (sname) {
/*<                NBMIN = 2 >*/
                nbmin = 2;
/*<             ELSE >*/
            } else {
/*<                NBMIN = 2 >*/
                nbmin = 2;
/*<             END IF >*/
            }
/*<          ELSE IF( C3.EQ.'TRI' ) THEN >*/
        } else if (s_cmp(c3, "TRI", (ftnlen)3, (ftnlen)3) == 0) {
/*<             IF( SNAME ) THEN >*/
            if (sname) {
/*<                NBMIN = 2 >*/
                nbmin = 2;
/*<             ELSE >*/
            } else {
/*<                NBMIN = 2 >*/
                nbmin = 2;
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       ELSE IF( C2.EQ.'SY' ) THEN >*/
    } else if (s_cmp(c2, "SY", (ftnlen)2, (ftnlen)2) == 0) {
/*<          IF( C3.EQ.'TRF' ) THEN >*/
        if (s_cmp(c3, "TRF", (ftnlen)3, (ftnlen)3) == 0) {
/*<             IF( SNAME ) THEN >*/
            if (sname) {
/*<                NBMIN = 8 >*/
                nbmin = 8;
/*<             ELSE >*/
            } else {
/*<                NBMIN = 8 >*/
                nbmin = 8;
/*<             END IF >*/
            }
/*<          ELSE IF( SNAME .AND. C3.EQ.'TRD' ) THEN >*/
        } else if (sname && s_cmp(c3, "TRD", (ftnlen)3, (ftnlen)3) == 0) {
/*<             NBMIN = 2 >*/
            nbmin = 2;
/*<          END IF >*/
        }
/*<       ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN >*/
    } else if (cname && s_cmp(c2, "HE", (ftnlen)2, (ftnlen)2) == 0) {
/*<          IF( C3.EQ.'TRD' ) THEN >*/
        if (s_cmp(c3, "TRD", (ftnlen)3, (ftnlen)3) == 0) {
/*<             NBMIN = 2 >*/
            nbmin = 2;
/*<          END IF >*/
        }
/*<       ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN >*/
    } else if (sname && s_cmp(c2, "OR", (ftnlen)2, (ftnlen)2) == 0) {
/*<          IF( C3( 1:1 ).EQ.'G' ) THEN >*/
        if (*(unsigned char *)c3 == 'G') {
/*<    >*/
            if (s_cmp(c4, "QR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "RQ",
                    (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "LQ", (ftnlen)2, (
                    ftnlen)2) == 0 || s_cmp(c4, "QL", (ftnlen)2, (ftnlen)2) ==
                     0 || s_cmp(c4, "HR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(
                    c4, "TR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "BR", (
                    ftnlen)2, (ftnlen)2) == 0) {
/*<                NBMIN = 2 >*/
                nbmin = 2;
/*<             END IF >*/
            }
/*<          ELSE IF( C3( 1:1 ).EQ.'M' ) THEN >*/
        } else if (*(unsigned char *)c3 == 'M') {
/*<    >*/
            if (s_cmp(c4, "QR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "RQ",
                    (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "LQ", (ftnlen)2, (
                    ftnlen)2) == 0 || s_cmp(c4, "QL", (ftnlen)2, (ftnlen)2) ==
                     0 || s_cmp(c4, "HR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(
                    c4, "TR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "BR", (
                    ftnlen)2, (ftnlen)2) == 0) {
/*<                NBMIN = 2 >*/
                nbmin = 2;
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN >*/
    } else if (cname && s_cmp(c2, "UN", (ftnlen)2, (ftnlen)2) == 0) {
/*<          IF( C3( 1:1 ).EQ.'G' ) THEN >*/
        if (*(unsigned char *)c3 == 'G') {
/*<    >*/
            if (s_cmp(c4, "QR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "RQ",
                    (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "LQ", (ftnlen)2, (
                    ftnlen)2) == 0 || s_cmp(c4, "QL", (ftnlen)2, (ftnlen)2) ==
                     0 || s_cmp(c4, "HR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(
                    c4, "TR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "BR", (
                    ftnlen)2, (ftnlen)2) == 0) {
/*<                NBMIN = 2 >*/
                nbmin = 2;
/*<             END IF >*/
            }
/*<          ELSE IF( C3( 1:1 ).EQ.'M' ) THEN >*/
        } else if (*(unsigned char *)c3 == 'M') {
/*<    >*/
            if (s_cmp(c4, "QR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "RQ",
                    (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "LQ", (ftnlen)2, (
                    ftnlen)2) == 0 || s_cmp(c4, "QL", (ftnlen)2, (ftnlen)2) ==
                     0 || s_cmp(c4, "HR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(
                    c4, "TR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "BR", (
                    ftnlen)2, (ftnlen)2) == 0) {
/*<                NBMIN = 2 >*/
                nbmin = 2;
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }
/*<       ILAENV = NBMIN >*/
    ret_val = nbmin;
/*<       RETURN >*/
    return ret_val;

/*<   300 CONTINUE >*/
L300:

/*     ISPEC = 3:  crossover point */

/*<       NX = 0 >*/
    nx = 0;
/*<       IF( C2.EQ.'GE' ) THEN >*/
    if (s_cmp(c2, "GE", (ftnlen)2, (ftnlen)2) == 0) {
/*<    >*/
        if (s_cmp(c3, "QRF", (ftnlen)3, (ftnlen)3) == 0 || s_cmp(c3, "RQF", (
                ftnlen)3, (ftnlen)3) == 0 || s_cmp(c3, "LQF", (ftnlen)3, (
                ftnlen)3) == 0 || s_cmp(c3, "QLF", (ftnlen)3, (ftnlen)3) == 0)
                 {
/*<             IF( SNAME ) THEN >*/
            if (sname) {
/*<                NX = 128 >*/
                nx = 128;
/*<             ELSE >*/
            } else {
/*<                NX = 128 >*/
                nx = 128;
/*<             END IF >*/
            }
/*<          ELSE IF( C3.EQ.'HRD' ) THEN >*/
        } else if (s_cmp(c3, "HRD", (ftnlen)3, (ftnlen)3) == 0) {
/*<             IF( SNAME ) THEN >*/
            if (sname) {
/*<                NX = 128 >*/
                nx = 128;
/*<             ELSE >*/
            } else {
/*<                NX = 128 >*/
                nx = 128;
/*<             END IF >*/
            }
/*<          ELSE IF( C3.EQ.'BRD' ) THEN >*/
        } else if (s_cmp(c3, "BRD", (ftnlen)3, (ftnlen)3) == 0) {
/*<             IF( SNAME ) THEN >*/
            if (sname) {
/*<                NX = 128 >*/
                nx = 128;
/*<             ELSE >*/
            } else {
/*<                NX = 128 >*/
                nx = 128;
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       ELSE IF( C2.EQ.'SY' ) THEN >*/
    } else if (s_cmp(c2, "SY", (ftnlen)2, (ftnlen)2) == 0) {
/*<          IF( SNAME .AND. C3.EQ.'TRD' ) THEN >*/
        if (sname && s_cmp(c3, "TRD", (ftnlen)3, (ftnlen)3) == 0) {
/*<             NX = 32 >*/
            nx = 32;
/*<          END IF >*/
        }
/*<       ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN >*/
    } else if (cname && s_cmp(c2, "HE", (ftnlen)2, (ftnlen)2) == 0) {
/*<          IF( C3.EQ.'TRD' ) THEN >*/
        if (s_cmp(c3, "TRD", (ftnlen)3, (ftnlen)3) == 0) {
/*<             NX = 32 >*/
            nx = 32;
/*<          END IF >*/
        }
/*<       ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN >*/
    } else if (sname && s_cmp(c2, "OR", (ftnlen)2, (ftnlen)2) == 0) {
/*<          IF( C3( 1:1 ).EQ.'G' ) THEN >*/
        if (*(unsigned char *)c3 == 'G') {
/*<    >*/
            if (s_cmp(c4, "QR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "RQ",
                    (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "LQ", (ftnlen)2, (
                    ftnlen)2) == 0 || s_cmp(c4, "QL", (ftnlen)2, (ftnlen)2) ==
                     0 || s_cmp(c4, "HR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(
                    c4, "TR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "BR", (
                    ftnlen)2, (ftnlen)2) == 0) {
/*<                NX = 128 >*/
                nx = 128;
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN >*/
    } else if (cname && s_cmp(c2, "UN", (ftnlen)2, (ftnlen)2) == 0) {
/*<          IF( C3( 1:1 ).EQ.'G' ) THEN >*/
        if (*(unsigned char *)c3 == 'G') {
/*<    >*/
            if (s_cmp(c4, "QR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "RQ",
                    (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "LQ", (ftnlen)2, (
                    ftnlen)2) == 0 || s_cmp(c4, "QL", (ftnlen)2, (ftnlen)2) ==
                     0 || s_cmp(c4, "HR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(
                    c4, "TR", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(c4, "BR", (
                    ftnlen)2, (ftnlen)2) == 0) {
/*<                NX = 128 >*/
                nx = 128;
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }
/*<       ILAENV = NX >*/
    ret_val = nx;
/*<       RETURN >*/
    return ret_val;

/*<   400 CONTINUE >*/
L400:

/*     ISPEC = 4:  number of shifts (used by xHSEQR) */

/*<       ILAENV = 6 >*/
    ret_val = 6;
/*<       RETURN >*/
    return ret_val;

/*<   500 CONTINUE >*/
L500:

/*     ISPEC = 5:  minimum column dimension (not used) */

/*<       ILAENV = 2 >*/
    ret_val = 2;
/*<       RETURN >*/
    return ret_val;

/*<   600 CONTINUE  >*/
L600:

/*     ISPEC = 6:  crossover point for SVD (used by xGELSS and xGESVD) */

/*<       ILAENV = INT( REAL( MIN( N1, N2 ) )*1.6E0 ) >*/
    ret_val = (integer) ((real) min(*n1,*n2) * (float)1.6);
/*<       RETURN >*/
    return ret_val;

/*<   700 CONTINUE >*/
L700:

/*     ISPEC = 7:  number of processors (not used) */

/*<       ILAENV = 1 >*/
    ret_val = 1;
/*<       RETURN >*/
    return ret_val;

/*<   800 CONTINUE >*/
L800:

/*     ISPEC = 8:  crossover point for multishift (used by xHSEQR) */

/*<       ILAENV = 50 >*/
    ret_val = 50;
/*<       RETURN >*/
    return ret_val;

/*<   900 CONTINUE >*/
L900:

/*     ISPEC = 9:  maximum size of the subproblems at the bottom of the */
/*                 computation tree in the divide-and-conquer algorithm */
/*                 (used by xGELSD and xGESDD) */

/*<       ILAENV = 25 >*/
    ret_val = 25;
/*<       RETURN >*/
    return ret_val;

/*<  1000 CONTINUE >*/
L1000:

/*     ISPEC = 10: ieee NaN arithmetic can be trusted not to trap */

/*     ILAENV = 0 */
/*<       ILAENV = 1 >*/
    ret_val = 1;
/*<       IF( ILAENV.EQ.1 ) THEN >*/
    if (ret_val == 1) {
/*<          ILAENV = IEEECK( 0, 0.0, 1.0 )  >*/
        ret_val = ieeeck_(&c__0, &c_b162, &c_b163);
/*<       END IF >*/
    }
/*<       RETURN >*/
    return ret_val;

/*<  1100 CONTINUE >*/
L1100:

/*     ISPEC = 11: infinity arithmetic can be trusted not to trap */

/*     ILAENV = 0 */
/*<       ILAENV = 1 >*/
    ret_val = 1;
/*<       IF( ILAENV.EQ.1 ) THEN >*/
    if (ret_val == 1) {
/*<          ILAENV = IEEECK( 1, 0.0, 1.0 )  >*/
        ret_val = ieeeck_(&c__1, &c_b162, &c_b163);
/*<       END IF >*/
    }
/*<       RETURN >*/
    return ret_val;

/*     End of ILAENV */

/*<       END >*/
} /* ilaenv_ */

#ifdef __cplusplus
        }
#endif
