#include "f2c.h"
#include "netlib.h"

integer ilaenv_(ispec, name, opts, n1, n2, n3, n4)
const integer *ispec;
const char *name, *opts;
const integer *n1, *n2, *n3, *n4;
{
    /* Local variables */
    static integer i;
    static logical cname, sname;
    static integer nbmin;
    static char c1[1], c2[2], c3[3], c4[2];
    static integer ic, nb, iz, nx;
    static char subnam[6];

    (void)opts; (void)n3;
/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

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
/*          < 0:  if ILAENV = -k, the k-th argument had an illegal value.  */

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

    switch ((int)*ispec) {
        case 1:  goto L100;
        case 2:  goto L100;
        case 3:  goto L100;
        case 4:  goto L400;
        case 5:  goto L500;
        case 6:  goto L600;
        case 7:  goto L700;
        case 8:  goto L800;
    }

/*     Invalid value for ISPEC */

    return -1;

L100:

/*     Convert NAME to upper case if the first character is lower case. */

    s_copy(subnam, name, 6L, 6L);
    ic = *subnam;
    iz = 'Z';
    if (iz == 90 || iz == 122) {

/*        ASCII character set */

        if (ic >= 97 && ic <= 122) {
            *subnam = (char) (ic - 32);
            for (i = 2; i <= 6; ++i) {
                ic = (integer) subnam[i - 1];
                if (ic >= 97 && ic <= 122) {
                    subnam[i - 1] = (char) (ic - 32);
                }
            }
        }

    } else if (iz == 233 || iz == 169) {

/*        EBCDIC character set */

        if ((ic >= 129 && ic <= 137) || (ic >= 145 && ic <= 153) || (ic >= 162 && ic <= 169)) {
            *subnam = (char) (ic + 64);
            for (i = 2; i <= 6; ++i) {
                ic = (integer) subnam[i - 1];
                if ((ic >= 129 && ic <= 137) || (ic >= 145 && ic <= 153) || (ic >= 162 && ic <= 169)) {
                    subnam[i - 1] = (char) (ic + 64);
                }
            }
        }

    } else if (iz == 218 || iz == 250) {

/*        Prime machines:  ASCII+128 */

        if (ic >= 225 && ic <= 250) {
            *subnam = (char) (ic - 32);
            for (i = 2; i <= 6; ++i) {
                ic = (integer) subnam[i - 1];
                if (ic >= 225 && ic <= 250) {
                    subnam[i - 1] = (char) (ic - 32);
                }
            }
        }
    }

    *c1 = *subnam;
    sname = *c1 == 'S' || *c1 == 'D';
    cname = *c1 == 'C' || *c1 == 'Z';
    if (! (cname || sname)) {
        return 1;
    }
    s_copy(c2, subnam + 1, 2L, 2L);
    s_copy(c3, subnam + 3, 3L, 3L);
    s_copy(c4, c3 + 1, 2L, 2L);

    switch ((int)*ispec) {
        case 1:  goto L110;
        case 2:  goto L200;
        case 3:  goto L300;
    }

L110:

/*     ISPEC = 1:  block size */

/*     In these examples, separate code is provided for setting NB for */
/*     real and complex.  We assume that NB will take the same value in */
/*     single or double precision. */

    nb = 1;

    if (s_cmp(c2, "GE", 2L, 2L) == 0) {
        if (s_cmp(c3, "TRF", 3L, 3L) == 0) {
            if (sname) {
                nb = 64;
            } else {
                nb = 64;
            }
        } else if (s_cmp(c3, "QRF", 3L, 3L) == 0 ||
                   s_cmp(c3, "RQF", 3L, 3L) == 0 ||
                   s_cmp(c3, "LQF", 3L, 3L) == 0 ||
                   s_cmp(c3, "QLF", 3L, 3L) == 0) {
            if (sname) {
                nb = 32;
            } else {
                nb = 32;
            }
        } else if (s_cmp(c3, "HRD", 3L, 3L) == 0) {
            if (sname) {
                nb = 32;
            } else {
                nb = 32;
            }
        } else if (s_cmp(c3, "BRD", 3L, 3L) == 0) {
            if (sname) {
                nb = 32;
            } else {
                nb = 32;
            }
        } else if (s_cmp(c3, "TRI", 3L, 3L) == 0) {
            if (sname) {
                nb = 64;
            } else {
                nb = 64;
            }
        }
    } else if (s_cmp(c2, "PO", 2L, 2L) == 0) {
        if (s_cmp(c3, "TRF", 3L, 3L) == 0) {
            if (sname) {
                nb = 64;
            } else {
                nb = 64;
            }
        }
    } else if (s_cmp(c2, "SY", 2L, 2L) == 0) {
        if (s_cmp(c3, "TRF", 3L, 3L) == 0) {
            if (sname) {
                nb = 64;
            } else {
                nb = 64;
            }
        } else if (sname && s_cmp(c3, "TRD", 3L, 3L) == 0) {
            nb = 1;
        } else if (sname && s_cmp(c3, "GST", 3L, 3L) == 0) {
            nb = 64;
        }
    } else if (cname && s_cmp(c2, "HE", 2L, 2L) == 0) {
        if (s_cmp(c3, "TRF", 3L, 3L) == 0) {
            nb = 64;
        } else if (s_cmp(c3, "TRD", 3L, 3L) == 0) {
            nb = 1;
        } else if (s_cmp(c3, "GST", 3L, 3L) == 0) {
            nb = 64;
        }
    } else if (sname && s_cmp(c2, "OR", 2L, 2L) == 0) {
        if (*c3 == 'G') {
            if (s_cmp(c4, "QR", 2L, 2L) == 0 ||
                s_cmp(c4, "RQ", 2L, 2L) == 0 ||
                s_cmp(c4, "LQ", 2L, 2L) == 0 ||
                s_cmp(c4, "QL", 2L, 2L) == 0 ||
                s_cmp(c4, "HR", 2L, 2L) == 0 ||
                s_cmp(c4, "TR", 2L, 2L) == 0 ||
                s_cmp(c4, "BR", 2L, 2L) == 0) {
                nb = 32;
            }
        } else if (*c3 == 'M') {
            if (s_cmp(c4, "QR", 2L, 2L) == 0 ||
                s_cmp(c4, "RQ", 2L, 2L) == 0 ||
                s_cmp(c4, "LQ", 2L, 2L) == 0 ||
                s_cmp(c4, "QL", 2L, 2L) == 0 ||
                s_cmp(c4, "HR", 2L, 2L) == 0 ||
                s_cmp(c4, "TR", 2L, 2L) == 0 ||
                s_cmp(c4, "BR", 2L, 2L) == 0) {
                nb = 32;
            }
        }
    } else if (cname && s_cmp(c2, "UN", 2L, 2L) == 0) {
        if (*c3 == 'G') {
            if (s_cmp(c4, "QR", 2L, 2L) == 0 ||
                s_cmp(c4, "RQ", 2L, 2L) == 0 ||
                s_cmp(c4, "LQ", 2L, 2L) == 0 ||
                s_cmp(c4, "QL", 2L, 2L) == 0 ||
                s_cmp(c4, "HR", 2L, 2L) == 0 ||
                s_cmp(c4, "TR", 2L, 2L) == 0 ||
                s_cmp(c4, "BR", 2L, 2L) == 0) {
                nb = 32;
            }
        } else if (*c3 == 'M') {
            if (s_cmp(c4, "QR", 2L, 2L) == 0 ||
                s_cmp(c4, "RQ", 2L, 2L) == 0 ||
                s_cmp(c4, "LQ", 2L, 2L) == 0 ||
                s_cmp(c4, "QL", 2L, 2L) == 0 ||
                s_cmp(c4, "HR", 2L, 2L) == 0 ||
                s_cmp(c4, "TR", 2L, 2L) == 0 ||
                s_cmp(c4, "BR", 2L, 2L) == 0) {
                nb = 32;
            }
        }
    } else if (s_cmp(c2, "GB", 2L, 2L) == 0) {
        if (s_cmp(c3, "TRF", 3L, 3L) == 0) {
            if (sname) {
                if (*n4 <= 64) {
                    nb = 1;
                } else {
                    nb = 32;
                }
            } else {
                if (*n4 <= 64) {
                    nb = 1;
                } else {
                    nb = 32;
                }
            }
        }
    } else if (s_cmp(c2, "PB", 2L, 2L) == 0) {
        if (s_cmp(c3, "TRF", 3L, 3L) == 0) {
            if (sname) {
                if (*n2 <= 64) {
                    nb = 1;
                } else {
                    nb = 32;
                }
            } else {
                if (*n2 <= 64) {
                    nb = 1;
                } else {
                    nb = 32;
                }
            }
        }
    } else if (s_cmp(c2, "TR", 2L, 2L) == 0) {
        if (s_cmp(c3, "TRI", 3L, 3L) == 0) {
            if (sname) {
                nb = 64;
            } else {
                nb = 64;
            }
        }
    } else if (s_cmp(c2, "LA", 2L, 2L) == 0) {
        if (s_cmp(c3, "UUM", 3L, 3L) == 0) {
            if (sname) {
                nb = 64;
            } else {
                nb = 64;
            }
        }
    } else if (sname && s_cmp(c2, "ST", 2L, 2L) == 0) {
        if (s_cmp(c3, "EBZ", 3L, 3L) == 0) {
            nb = 1;
        }
    }
    return nb;

L200:

/*     ISPEC = 2:  minimum block size */

    nbmin = 2;
    if (s_cmp(c2, "GE", 2L, 2L) == 0) {
        if (s_cmp(c3, "QRF", 3L, 3L) == 0 ||
            s_cmp(c3, "RQF", 3L, 3L) == 0 ||
            s_cmp(c3, "LQF", 3L, 3L) == 0 ||
            s_cmp(c3, "QLF", 3L, 3L) == 0) {
            if (sname) {
                nbmin = 2;
            } else {
                nbmin = 2;
            }
        } else if (s_cmp(c3, "HRD", 3L, 3L) == 0) {
            if (sname) {
                nbmin = 2;
            } else {
                nbmin = 2;
            }
        } else if (s_cmp(c3, "BRD", 3L, 3L) == 0) {
            if (sname) {
                nbmin = 2;
            } else {
                nbmin = 2;
            }
        } else if (s_cmp(c3, "TRI", 3L, 3L) == 0) {
            if (sname) {
                nbmin = 2;
            } else {
                nbmin = 2;
            }
        }
    } else if (s_cmp(c2, "SY", 2L, 2L) == 0) {
        if (s_cmp(c3, "TRF", 3L, 3L) == 0) {
            if (sname) {
                nbmin = 8;
            } else {
                nbmin = 8;
            }
        } else if (sname && s_cmp(c3, "TRD", 3L, 3L) == 0) {
            nbmin = 2;
        }
    } else if (cname && s_cmp(c2, "HE", 2L, 2L) == 0) {
        if (s_cmp(c3, "TRD", 3L, 3L) == 0) {
            nbmin = 2;
        }
    } else if (sname && s_cmp(c2, "OR", 2L, 2L) == 0) {
        if (*c3 == 'G') {
            if (s_cmp(c4, "QR", 2L, 2L) == 0 ||
                s_cmp(c4, "RQ", 2L, 2L) == 0 ||
                s_cmp(c4, "LQ", 2L, 2L) == 0 ||
                s_cmp(c4, "QL", 2L, 2L) == 0 ||
                s_cmp(c4, "HR", 2L, 2L) == 0 ||
                s_cmp(c4, "TR", 2L, 2L) == 0 ||
                s_cmp(c4, "BR", 2L, 2L) == 0) {
                nbmin = 2;
            }
        } else if (*c3 == 'M') {
            if (s_cmp(c4, "QR", 2L, 2L) == 0 ||
                s_cmp(c4, "RQ", 2L, 2L) == 0 ||
                s_cmp(c4, "LQ", 2L, 2L) == 0 ||
                s_cmp(c4, "QL", 2L, 2L) == 0 ||
                s_cmp(c4, "HR", 2L, 2L) == 0 ||
                s_cmp(c4, "TR", 2L, 2L) == 0 ||
                s_cmp(c4, "BR", 2L, 2L) == 0) {
                nbmin = 2;
            }
        }
    } else if (cname && s_cmp(c2, "UN", 2L, 2L) == 0) {
        if (*c3 == 'G') {
            if (s_cmp(c4, "QR", 2L, 2L) == 0 ||
                s_cmp(c4, "RQ", 2L, 2L) == 0 ||
                s_cmp(c4, "LQ", 2L, 2L) == 0 ||
                s_cmp(c4, "QL", 2L, 2L) == 0 ||
                s_cmp(c4, "HR", 2L, 2L) == 0 ||
                s_cmp(c4, "TR", 2L, 2L) == 0 ||
                s_cmp(c4, "BR", 2L, 2L) == 0) {
                nbmin = 2;
            }
        } else if (*c3 == 'M') {
            if (s_cmp(c4, "QR", 2L, 2L) == 0 ||
                s_cmp(c4, "RQ", 2L, 2L) == 0 ||
                s_cmp(c4, "LQ", 2L, 2L) == 0 ||
                s_cmp(c4, "QL", 2L, 2L) == 0 ||
                s_cmp(c4, "HR", 2L, 2L) == 0 ||
                s_cmp(c4, "TR", 2L, 2L) == 0 ||
                s_cmp(c4, "BR", 2L, 2L) == 0) {
                nbmin = 2;
            }
        }
    }
    return nbmin;

L300:

/*     ISPEC = 3:  crossover point */

    nx = 0;
    if (s_cmp(c2, "GE", 2L, 2L) == 0) {
        if (s_cmp(c3, "QRF", 3L, 3L) == 0 ||
            s_cmp(c3, "RQF", 3L, 3L) == 0 ||
            s_cmp(c3, "LQF", 3L, 3L) == 0 ||
            s_cmp(c3, "QLF", 3L, 3L) == 0) {
            if (sname) {
                nx = 128;
            } else {
                nx = 128;
            }
        } else if (s_cmp(c3, "HRD", 3L, 3L) == 0) {
            if (sname) {
                nx = 128;
            } else {
                nx = 128;
            }
        } else if (s_cmp(c3, "BRD", 3L, 3L) == 0) {
            if (sname) {
                nx = 128;
            } else {
                nx = 128;
            }
        }
    } else if (s_cmp(c2, "SY", 2L, 2L) == 0) {
        if (sname && s_cmp(c3, "TRD", 3L, 3L) == 0) {
            nx = 1;
        }
    } else if (cname && s_cmp(c2, "HE", 2L, 2L) == 0) {
        if (s_cmp(c3, "TRD", 3L, 3L) == 0) {
            nx = 1;
        }
    } else if (sname && s_cmp(c2, "OR", 2L, 2L) == 0) {
        if (*c3 == 'G') {
            if (s_cmp(c4, "QR", 2L, 2L) == 0 ||
                s_cmp(c4, "RQ", 2L, 2L) == 0 ||
                s_cmp(c4, "LQ", 2L, 2L) == 0 ||
                s_cmp(c4, "QL", 2L, 2L) == 0 ||
                s_cmp(c4, "HR", 2L, 2L) == 0 ||
                s_cmp(c4, "TR", 2L, 2L) == 0 ||
                s_cmp(c4, "BR", 2L, 2L) == 0) {
                nx = 128;
            }
        }
    } else if (cname && s_cmp(c2, "UN", 2L, 2L) == 0) {
        if (*c3 == 'G') {
            if (s_cmp(c4, "QR", 2L, 2L) == 0 ||
                s_cmp(c4, "RQ", 2L, 2L) == 0 ||
                s_cmp(c4, "LQ", 2L, 2L) == 0 ||
                s_cmp(c4, "QL", 2L, 2L) == 0 ||
                s_cmp(c4, "HR", 2L, 2L) == 0 ||
                s_cmp(c4, "TR", 2L, 2L) == 0 ||
                s_cmp(c4, "BR", 2L, 2L) == 0) {
                nx = 128;
            }
        }
    }
    return nx;

L400:

/*     ISPEC = 4:  number of shifts (used by xHSEQR) */

    return 6;

L500:

/*     ISPEC = 5:  minimum column dimension (not used) */

    return 2;

L600:

/*     ISPEC = 6:  crossover point for SVD (used by xGELSS and xGESVD) */

    return (integer) ((real) min(*n1,*n2) * 1.6f);

L700:

/*     ISPEC = 7:  number of processors (not used) */

    return 1;

L800:

/*     ISPEC = 8:  crossover point for multishift (used by xHSEQR) */

    return 50;

} /* ilaenv_ */
