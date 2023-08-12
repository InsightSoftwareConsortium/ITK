/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*keep this declaration near the top of this file -RPM*/
static const char *FileHeader = "\n\
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n\
 * Copyright by The HDF Group.                                               *\n\
 * All rights reserved.                                                      *\n\
 *                                                                           *\n\
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *\n\
 * terms governing use, modification, and redistribution, is contained in    *\n\
 * the COPYING file, which can be found at the root of the source code       *\n\
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *\n\
 * If you do not have access to either file, you may request a copy from     *\n\
 * help@hdfgroup.org.                                                        *\n\
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *";
/*
 * Purpose:    This code was borrowed heavily from the `detect.c'
 *        program in the AIO distribution from Lawrence
 *        Livermore National Laboratory.
 *
 *        Detects machine byte order and floating point
 *        format and generates a C source file (H5Tinit.c)
 *        to describe those parameters.
 *
 * Assumptions: We have an ANSI compiler.  We're on a Unix like
 *        system or configure has detected those Unix
 *        features which aren't available.  We're not
 *        running on a Vax or other machine with mixed
 *        endianness.
 *-------------------------------------------------------------------------
 */
#undef NDEBUG
#include "H5private.h"
/* Do NOT use fprintf in this file as it is not linked with the library,
 * which contains the H5system.c file in which the function is defined.
 */
#include "H5Tpublic.h"
#include "H5Rpublic.h"

#if defined(__has_attribute)
#if __has_attribute(no_sanitize_address)
#define HDF_NO_UBSAN __attribute__((no_sanitize_address))
#else
#define HDF_NO_UBSAN
#endif
#else
#define HDF_NO_UBSAN
#endif

#define MAXDETECT 64

/*
 * This structure holds information about a type that
 * was detected.
 */
typedef struct detected_t {
    const char   *varname;
    unsigned int  size;             /* total byte size                  */
    unsigned int  precision;        /* meaningful bits                  */
    unsigned int  offset;           /* bit offset to meaningful bits    */
    int           perm[32];         /* for detection of byte order      */
    hbool_t       is_vax;           /* for vax (float & double) only    */
    unsigned int  sign;             /* location of sign bit             */
    unsigned int  mpos, msize, imp; /* information about mantissa       */
    unsigned int  epos, esize;      /* information about exponent       */
    unsigned long bias;             /* exponent bias for floating pt    */
    unsigned int  comp_align;       /* alignment for structure          */
} detected_t;

FILE *rawoutstream = NULL;

/* global variables types detection code */
H5_GCC_DIAG_OFF("larger-than=")
static detected_t d_g[MAXDETECT];
H5_GCC_DIAG_ON("larger-than=")
static volatile int nd_g = 0;

static void         print_results(int nd, detected_t *d);
static void         iprint(detected_t *);
static int          byte_cmp(int, const void *, const void *, const unsigned char *);
static unsigned int bit_cmp(unsigned int, int *, void *, void *, const unsigned char *);
static void         fix_order(int, int, int *, const char **);
static unsigned int imp_bit(unsigned int, int *, void *, void *, const unsigned char *);
static unsigned int find_bias(unsigned int, unsigned int, int *, void *);
static void         precision(detected_t *);
static void         print_header(void);
static void         detect_C89_floats(void);
static void         detect_C99_floats(void);

/*-------------------------------------------------------------------------
 * Function:    precision
 *
 * Purpose:     Determine the precision and offset.
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void
precision(detected_t *d)
{
    /* A floating point */
    d->offset    = MIN3(d->mpos, d->epos, d->sign);
    d->precision = d->msize + d->esize + 1;
}

/*-------------------------------------------------------------------------
 * Function:    DETECT_F
 *
 * Purpose:     This macro takes a floating point type like `double' and
 *              a base name like `natd' and detects byte order, mantissa
 *              location, exponent location, sign bit location, presence or
 *              absence of implicit mantissa bit, and exponent bias and
 *              initializes a detected_t structure with those properties.
 *-------------------------------------------------------------------------
 */
#define DETECT_F(TYPE, VAR, INFO)                                                                            \
    {                                                                                                        \
        TYPE          _v1, _v2, _v3;                                                                         \
        unsigned char _buf1[sizeof(TYPE)], _buf3[sizeof(TYPE)];                                              \
        unsigned char _pad_mask[sizeof(TYPE)];                                                               \
        unsigned char _byte_mask;                                                                            \
        int           _i, _j, _last = (-1);                                                                  \
        const char   *_mesg;                                                                                 \
                                                                                                             \
        memset(&INFO, 0, sizeof(INFO));                                                                      \
        INFO.varname = #VAR;                                                                                 \
        INFO.size    = sizeof(TYPE);                                                                         \
                                                                                                             \
        /* Initialize padding mask */                                                                        \
        memset(_pad_mask, 0, sizeof(_pad_mask));                                                             \
                                                                                                             \
        /* Padding bits.  Set a variable to 4.0, then flip each bit and see if                               \
         * the modified variable is equal ("==") to the original.  Build a                                   \
         * padding bitmask to indicate which bits in the type are padding (i.e.                              \
         * have no effect on the value and should be ignored by subsequent                                   \
         * steps).  This is necessary because padding bits can change arbitrarily                            \
         * and interfere with detection of the various properties below unless we                            \
         * know to ignore them. */                                                                           \
        _v1 = (TYPE)4.0L;                                                                                    \
        memcpy(_buf1, (const void *)&_v1, sizeof(TYPE));                                                     \
        for (_i = 0; _i < (int)sizeof(TYPE); _i++)                                                           \
            for (_byte_mask = (unsigned char)1; _byte_mask; _byte_mask = (unsigned char)(_byte_mask << 1)) { \
                _buf1[_i] ^= _byte_mask;                                                                     \
                memcpy((void *)&_v2, (const void *)_buf1, sizeof(TYPE));                                     \
                H5_GCC_CLANG_DIAG_OFF("float-equal")                                                         \
                if (_v1 != _v2)                                                                              \
                    _pad_mask[_i] |= _byte_mask;                                                             \
                H5_GCC_CLANG_DIAG_ON("float-equal")                                                          \
                _buf1[_i] ^= _byte_mask;                                                                     \
            } /* end for */                                                                                  \
                                                                                                             \
        /* Byte Order */                                                                                     \
        for (_i = 0, _v1 = (TYPE)0.0L, _v2 = (TYPE)1.0L; _i < (int)sizeof(TYPE); _i++) {                     \
            _v3 = _v1;                                                                                       \
            _v1 += _v2;                                                                                      \
            _v2 /= (TYPE)256.0L;                                                                             \
            memcpy(_buf1, (const void *)&_v1, sizeof(TYPE));                                                 \
            memcpy(_buf3, (const void *)&_v3, sizeof(TYPE));                                                 \
            _j = byte_cmp(sizeof(TYPE), _buf3, _buf1, _pad_mask);                                            \
            if (_j >= 0) {                                                                                   \
                INFO.perm[_i] = _j;                                                                          \
                _last         = _i;                                                                          \
            }                                                                                                \
        }                                                                                                    \
        fix_order(sizeof(TYPE), _last, INFO.perm, (const char **)&_mesg);                                    \
                                                                                                             \
        if (!strcmp(_mesg, "VAX"))                                                                           \
            INFO.is_vax = TRUE;                                                                              \
                                                                                                             \
        /* Implicit mantissa bit */                                                                          \
        _v1      = (TYPE)0.5L;                                                                               \
        _v2      = (TYPE)1.0L;                                                                               \
        INFO.imp = imp_bit(sizeof(TYPE), INFO.perm, &_v1, &_v2, _pad_mask);                                  \
                                                                                                             \
        /* Sign bit */                                                                                       \
        _v1       = (TYPE)1.0L;                                                                              \
        _v2       = (TYPE)-1.0L;                                                                             \
        INFO.sign = bit_cmp(sizeof(TYPE), INFO.perm, &_v1, &_v2, _pad_mask);                                 \
                                                                                                             \
        /* Mantissa */                                                                                       \
        INFO.mpos = 0;                                                                                       \
                                                                                                             \
        _v1        = (TYPE)1.0L;                                                                             \
        _v2        = (TYPE)1.5L;                                                                             \
        INFO.msize = bit_cmp(sizeof(TYPE), INFO.perm, &_v1, &_v2, _pad_mask);                                \
        INFO.msize += 1 + (unsigned int)(INFO.imp ? 0 : 1) - INFO.mpos;                                      \
                                                                                                             \
        /* Exponent */                                                                                       \
        INFO.epos = INFO.mpos + INFO.msize;                                                                  \
                                                                                                             \
        INFO.esize = INFO.sign - INFO.epos;                                                                  \
                                                                                                             \
        _v1       = (TYPE)1.0L;                                                                              \
        INFO.bias = find_bias(INFO.epos, INFO.esize, INFO.perm, &_v1);                                       \
        precision(&(INFO));                                                                                  \
        if (!strcmp(INFO.varname, "FLOAT") || !strcmp(INFO.varname, "DOUBLE") ||                             \
            !strcmp(INFO.varname, "LDOUBLE")) {                                                              \
            COMP_ALIGNMENT(TYPE, INFO.comp_align);                                                           \
        }                                                                                                    \
    }

/* Detect alignment for C structure */
#define COMP_ALIGNMENT(TYPE, COMP_ALIGN)                                                                     \
    {                                                                                                        \
        struct {                                                                                             \
            char c;                                                                                          \
            TYPE x;                                                                                          \
        } s;                                                                                                 \
                                                                                                             \
        COMP_ALIGN = (unsigned int)((char *)(&(s.x)) - (char *)(&s));                                        \
    }

/*-------------------------------------------------------------------------
 * Function:    print_results
 *
 * Purpose:     Prints information about the detected data types.
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void
print_results(int nd, detected_t *d)
{
    int byte_order = 0; /*byte order of data types*/
    int i, j;

    /* Include files */
    fprintf(rawoutstream, "\
/****************/\n\
/* Module Setup */\n\
/****************/\n\
\n\
#include \"H5Tmodule.h\"          /* This source code file is part of the H5T module */\n\
\n\
\n\
/***********/\n\
/* Headers */\n\
/***********/\n\
#include \"H5private.h\"        /* Generic Functions            */\n\
#include \"H5Eprivate.h\"        /* Error handling              */\n\
#include \"H5FLprivate.h\"    /* Free Lists                */\n\
#include \"H5Iprivate.h\"        /* IDs                      */\n\
#include \"H5Tpkg.h\"        /* Datatypes                 */\n\
\n\
\n\
/****************/\n\
/* Local Macros */\n\
/****************/\n\
\n\
\n\
/******************/\n\
/* Local Typedefs */\n\
/******************/\n\
\n\
\n\
/********************/\n\
/* Package Typedefs */\n\
/********************/\n\
\n\
\n\
/********************/\n\
/* Local Prototypes */\n\
/********************/\n\
\n\
\n\
/********************/\n\
/* Public Variables */\n\
/********************/\n\
\n\
\n\
/*****************************/\n\
/* Library Private Variables */\n\
/*****************************/\n\
\n\
\n\
/*********************/\n\
/* Package Variables */\n\
/*********************/\n\
\n\
\n");
    fprintf(rawoutstream, "\n\
/*******************/\n\
/* Local Variables */\n\
/*******************/\n\
\n");

    /* The interface initialization function */
    fprintf(rawoutstream, "\n\
\n\
/*-------------------------------------------------------------------------\n\
 * Function:    H5T__init_native\n\
 *\n\
 * Purpose:    Initialize pre-defined native datatypes from code generated\n\
 *              during the library configuration by H5detect.\n\
 *\n\
 * Return:    Success:    non-negative\n\
 *        Failure:    negative\n\
 *\n\
 * Programmer:    Robb Matzke\n\
 *              Wednesday, December 16, 1998\n\
 *\n\
 *-------------------------------------------------------------------------\n\
 */\n\
herr_t\n\
H5T__init_native(void)\n\
{\n\
    H5T_t    *dt = NULL;\n\
    herr_t    ret_value = SUCCEED;\n\
\n\
    FUNC_ENTER_PACKAGE\n");

    for (i = 0; i < nd; i++) {
        /* The native endianness of this machine */
        /* The INFO.perm now contains `-1' for bytes that aren't used and
         * are always zero.  This happens on the Cray for `short' where
         * sizeof(short) is 8, but only the low-order 4 bytes are ever used.
         */
        if (d[i].is_vax) /* the type is a VAX floating number */
            byte_order = -1;
        else {
            for (j = 0; j < 32; j++) {
                /*Find the 1st containing valid data*/
                if (d[i].perm[j] > -1) {
                    byte_order = d[i].perm[j];
                    break;
                }
            }
        }

        /* Print a comment to describe this section of definitions. */
        fprintf(rawoutstream, "\n   /*\n");
        iprint(d + i);
        fprintf(rawoutstream, "    */\n");

        /* The part common to fixed and floating types */
        fprintf(rawoutstream, "\
    if(NULL == (dt = H5T__alloc()))\n\
        HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, FAIL, \"datatype allocation failed\");\n\
    dt->shared->state = H5T_STATE_IMMUTABLE;\n\
    dt->shared->type = H5T_FLOAT;\n\
    dt->shared->size = %d;\n",
                d[i].size); /*size            */

        if (byte_order == -1)
            fprintf(rawoutstream, "\
    dt->shared->u.atomic.order = H5T_ORDER_VAX;\n");
        else if (byte_order == 0)
            fprintf(rawoutstream, "\
    dt->shared->u.atomic.order = H5T_ORDER_LE;\n");
        else
            fprintf(rawoutstream, "\
    dt->shared->u.atomic.order = H5T_ORDER_BE;\n");

        fprintf(rawoutstream, "\
    dt->shared->u.atomic.offset = %d;\n\
    dt->shared->u.atomic.prec = %d;\n\
    dt->shared->u.atomic.lsb_pad = H5T_PAD_ZERO;\n\
    dt->shared->u.atomic.msb_pad = H5T_PAD_ZERO;\n",
                d[i].offset,                          /*offset        */
                d[i].precision);                      /*precision        */
        /*assert((d[i].perm[0]>0)==(byte_order>0));*/ /* Double-check that byte-order doesn't change */

        /* The part unique to floating point types */
        fprintf(rawoutstream, "\
dt->shared->u.atomic.u.f.sign = %d;\n\
dt->shared->u.atomic.u.f.epos = %d;\n\
dt->shared->u.atomic.u.f.esize = %d;\n\
dt->shared->u.atomic.u.f.ebias = 0x%08lx;\n\
dt->shared->u.atomic.u.f.mpos = %d;\n\
dt->shared->u.atomic.u.f.msize = %d;\n\
dt->shared->u.atomic.u.f.norm = H5T_NORM_%s;\n\
dt->shared->u.atomic.u.f.pad = H5T_PAD_ZERO;\n",
                d[i].sign,                      /*sign location */
                d[i].epos,                      /*exponent loc    */
                d[i].esize,                     /*exponent size */
                (unsigned long)(d[i].bias),     /*exponent bias */
                d[i].mpos,                      /*mantissa loc    */
                d[i].msize,                     /*mantissa size */
                d[i].imp ? "IMPLIED" : "NONE"); /*normalization */

        /* Register the type */
        fprintf(rawoutstream, "\
    if((H5T_NATIVE_%s_g = H5I_register(H5I_DATATYPE, dt, FALSE)) < 0)\n\
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, \"can't register ID for built-in datatype\");\n",
                d[i].varname);

        /* Variables for alignment of compound datatype */
        if (!strcmp(d[i].varname, "SCHAR") || !strcmp(d[i].varname, "SHORT") ||
            !strcmp(d[i].varname, "INT") || !strcmp(d[i].varname, "LONG") || !strcmp(d[i].varname, "LLONG") ||
            !strcmp(d[i].varname, "FLOAT") || !strcmp(d[i].varname, "DOUBLE") ||
            !strcmp(d[i].varname, "LDOUBLE")) {
            fprintf(rawoutstream, "    H5T_NATIVE_%s_ALIGN_g = %lu;\n", d[i].varname,
                    (unsigned long)(d[i].comp_align));
        }
    }

    /* Consider VAX a little-endian machine */
    if (byte_order == 0 || byte_order == -1) {
        fprintf(rawoutstream, "\n\
    /* Set the native order for this machine */\n\
    H5T_native_order_g = H5T_ORDER_%s;\n",
                "LE");
    }
    else {
        fprintf(rawoutstream, "\n\
    /* Set the native order for this machine */\n\
    H5T_native_order_g = H5T_ORDER_%s;\n",
                "BE");
    }

    fprintf(rawoutstream, "\
\n\
done:\n\
    if(ret_value < 0) {\n\
        if(dt != NULL) {\n\
            dt->shared = H5FL_FREE(H5T_shared_t, dt->shared);\n\
            dt = H5FL_FREE(H5T_t, dt);\n\
        } /* end if */\n\
    } /* end if */\n\
\n\
    FUNC_LEAVE_NOAPI(ret_value);\n} /* end H5T__init_native() */\n");

} /* end print_results() */

/*-------------------------------------------------------------------------
 * Function:    iprint
 *
 * Purpose:     Prints information about the fields of a floating point format.
 *
 * Return:      void

 *-------------------------------------------------------------------------
 */
static void
iprint(detected_t *d)
{
    unsigned int pass;

    for (pass = (d->size - 1) / 4;; --pass) {
        unsigned int i, k;
        /*
         * Print the byte ordering above the bit fields.
         */
        fprintf(rawoutstream, "    * ");
        for (i = MIN(pass * 4 + 3, d->size - 1); i >= pass * 4; --i) {
            fprintf(rawoutstream, "%4d", d->perm[i]);
            if (i > pass * 4)
                fputs("     ", rawoutstream);
            if (!i)
                break;
        }

        /*
         * Print the bit fields
         */
        fprintf(rawoutstream, "\n    * ");
        for (i = MIN(pass * 4 + 3, d->size - 1), k = MIN(pass * 32 + 31, 8 * d->size - 1); i >= pass * 4;
             --i) {
            unsigned int j;

            for (j = 8; j > 0; --j) {
                if (k == d->sign) {
                    fputc('S', rawoutstream);
                }
                else if (k >= d->epos && k < d->epos + d->esize) {
                    fputc('E', rawoutstream);
                }
                else if (k >= d->mpos && k < d->mpos + d->msize) {
                    fputc('M', rawoutstream);
                }
                else {
                    fputc('?', rawoutstream); /*unknown floating point bit */
                }
                --k;
            }
            if (i > pass * 4)
                fputc(' ', rawoutstream);
            if (!i)
                break;
        }
        fputc('\n', rawoutstream);
        if (!pass)
            break;
    }

    /*
     * Is there an implicit bit in the mantissa.
     */
    fprintf(rawoutstream, "    * Implicit bit? %s\n", d->imp ? "yes" : "no");
}

/*-------------------------------------------------------------------------
 * Function:    byte_cmp
 *
 * Purpose:     Compares two chunks of memory A and B and returns the
 *              byte index into those arrays of the first byte that
 *              differs between A and B.  Ignores differences where the
 *              corresponding bit in pad_mask is set to 0.
 *
 * Return:      Success:    Index of differing byte.
 *              Failure:    -1 if all bytes are the same.
 *-------------------------------------------------------------------------
 */
static int
byte_cmp(int n, const void *_a, const void *_b, const unsigned char *pad_mask)
{
    int                  i;
    const unsigned char *a = (const unsigned char *)_a;
    const unsigned char *b = (const unsigned char *)_b;

    for (i = 0; i < n; i++)
        if ((a[i] & pad_mask[i]) != (b[i] & pad_mask[i]))
            return i;

    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    bit_cmp
 *
 * Purpose:     Compares two bit vectors and returns the index for the
 *              first bit that differs between the two vectors.     The
 *              size of the vector is NBYTES.  PERM is a mapping from
 *              actual order to little endian.  Ignores differences where
 *              the corresponding bit in pad_mask is set to 0.
 *
 * Return:      Index of first differing bit.
 *
 *-------------------------------------------------------------------------
 */
static unsigned int
bit_cmp(unsigned int nbytes, int *perm, void *_a, void *_b, const unsigned char *pad_mask)
{
    unsigned int   i;
    unsigned char *a = (unsigned char *)_a;
    unsigned char *b = (unsigned char *)_b;
    unsigned char  aa, bb;

    for (i = 0; i < nbytes; i++) {
        assert(perm[i] < (int)nbytes);
        if ((aa = (unsigned char)(a[perm[i]] & pad_mask[perm[i]])) !=
            (bb = (unsigned char)(b[perm[i]] & pad_mask[perm[i]]))) {
            unsigned int j;

            for (j = 0; j < 8; j++, aa >>= 1, bb >>= 1) {
                if ((aa & 1) != (bb & 1))
                    return i * 8 + j;
            }
            fprintf(stderr, "INTERNAL ERROR");
            abort();
        }
    }
    fprintf(stderr, "INTERNAL ERROR");
    abort();
    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    fix_order
 *
 * Purpose:      Given an array PERM with elements FIRST through LAST
 *              initialized with zero origin byte numbers, this function
 *              creates a permutation vector that maps the actual order
 *              of a floating point number to little-endian.
 *
 *              This function assumes that the mantissa byte ordering
 *              implies the total ordering.
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void
fix_order(int n, int last, int *perm, const char **mesg)
{
    int i;

    if (last > 1) {
        /*
         * We have at least three points to consider.
         */
        if (perm[last] < perm[last - 1] && perm[last - 1] < perm[last - 2]) {
            /*
             * Little endian.
             */
            if (mesg)
                *mesg = "Little-endian";
            for (i = 0; i < n; i++)
                perm[i] = i;
        }
        else if (perm[last] > perm[last - 1] && perm[last - 1] > perm[last - 2]) {
            /*
             * Big endian.
             */
            if (mesg)
                *mesg = "Big-endian";
            for (i = 0; i < n; i++)
                perm[i] = (n - 1) - i;
        }
        else {
            /*
             * Bi-endian machines like VAX.
             * (NOTE: This is not an actual determination of the VAX-endianness.
             *          It could have some other endianness and fall into this
             *          case - JKM & QAK)
             */
            assert(0 == n % 2);
            if (mesg)
                *mesg = "VAX";
            for (i = 0; i < n; i += 2) {
                perm[i]     = (n - 2) - i;
                perm[i + 1] = (n - 1) - i;
            }
        }
    }
    else {
        fprintf(stderr, "Failed to detect byte order of %d-byte floating point.\n", n);
        exit(1);
    }
}

/*-------------------------------------------------------------------------
 * Function:    imp_bit
 *
 * Purpose:     Looks for an implicit bit in the mantissa.  The value
 *              of _A should be 1.0 and the value of _B should be 0.5.
 *              Some floating-point formats discard the most significant
 *              bit of the mantissa after normalizing since it will always
 *              be a one (except for 0.0).  If this is true for the native
 *              floating point values stored in _A and _B then the function
 *              returns non-zero.
 *
 *              This function assumes that the exponent occupies higher
 *              order bits than the mantissa and that the most significant
 *              bit of the mantissa is next to the least significant bit
 *              of the exponent.
 *
 *
 * Return:      Success:    Non-zero if the most significant bit
 *                          of the mantissa is discarded (ie, the
 *                          mantissa has an implicit `one' as the
 *                          most significant bit).    Otherwise,
 *                          returns zero.
 *
 *              Failure:    1
 *
 *-------------------------------------------------------------------------
 */
static unsigned int
imp_bit(unsigned int n, int *perm, void *_a, void *_b, const unsigned char *pad_mask)
{
    unsigned char *a = (unsigned char *)_a;
    unsigned char *b = (unsigned char *)_b;
    unsigned int   changed, major, minor;
    unsigned int   msmb; /* most significant mantissa bit */

    /*
     * Look for the least significant bit that has changed between
     * A and B.  This is the least significant bit of the exponent.
     */
    changed = bit_cmp(n, perm, a, b, pad_mask);

    /*
     * The bit to the right (less significant) of the changed bit should
     * be the most significant bit of the mantissa.  If it is non-zero
     * then the format does not remove the leading `1' of the mantissa.
     */
    msmb  = changed - 1;
    major = msmb / 8;
    minor = msmb % 8;

    return (a[perm[major]] >> minor) & 0x01 ? 0 : 1;
}

/*-------------------------------------------------------------------------
 * Function:  find_bias
 *
 * Purpose:   Determines the bias of the exponent.  This function should
 *            be called with _A having a value of `1'.
 *
 * Return:    The exponent bias.
 *
 *-------------------------------------------------------------------------
 */
H5_ATTR_PURE static unsigned int
find_bias(unsigned int epos, unsigned int esize, int *perm, void *_a)
{
    unsigned char *a = (unsigned char *)_a;
    unsigned char  mask;
    unsigned int   b, shift = 0, nbits, bias = 0;

    while (esize > 0) {
        nbits = MIN(esize, (8 - epos % 8));
        mask  = (unsigned char)((1 << nbits) - 1);
        b     = (unsigned int)(a[perm[epos / 8]] >> (epos % 8)) & mask;
        bias |= b << shift;

        shift += nbits;
        esize -= nbits;
        epos += nbits;
    }
    return bias;
}

/*-------------------------------------------------------------------------
 * Function:    print_header
 *
 * Purpose:     Prints the C file header for the generated file.
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void
print_header(void)
{

    time_t      now = HDtime(NULL);
    struct tm  *tm  = HDlocaltime(&now);
    char        real_name[30];
    char        host_name[256];
    int         i;
    const char *s;
#ifdef H5_HAVE_GETPWUID
    struct passwd *pwd = NULL;
#else
    int pwd      = 1;
#endif
    static const char *month_name[] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
    static const char *purpose      = "\
This machine-generated source code contains\n\
information about the various integer and\n\
floating point numeric formats found on this\n\
architecture.  The parameters below should be\n\
checked carefully and errors reported to the\n\
HDF5 maintainer.\n\
\n\
Each of the numeric formats listed below are\n\
printed from most significant bit to least\n\
significant bit even though the actual bytes\n\
might be stored in a different order in\n\
memory.     The integers above each binary byte\n\
indicate the relative order of the bytes in\n\
memory; little-endian machines have\n\
decreasing numbers while big-endian machines\n\
have increasing numbers.\n\
\n\
The fields of the numbers are printed as\n\
letters with `S' for the mantissa sign bit,\n\
`M' for the mantissa magnitude, and `E' for\n\
the exponent.  The exponent has an associated\n\
bias which can be subtracted to find the\n\
true exponent.    The radix point is assumed\n\
to be before the first `M' bit.     Any bit\n\
of a floating-point value not falling into one\n\
of these categories is printed as a question\n\
mark.  Bits of integer types are printed as\n\
`I' for 2's complement and `U' for magnitude.\n\
\n\
If the most significant bit of the normalized\n\
mantissa (always a `1' except for `0.0') is\n\
not stored then an `implicit=yes' appears\n\
under the field description.  In this case,\n\
the radix point is still assumed to be\n\
before the first `M' but after the implicit\n\
bit.\n";

    /*
     * The real name is the first item from the passwd gecos field.
     */
#ifdef H5_HAVE_GETPWUID
    {
        size_t n;
        char  *comma;
        if ((pwd = getpwuid(getuid()))) {
            if ((comma = strchr(pwd->pw_gecos, ','))) {
                n = MIN(sizeof(real_name) - 1, (unsigned)(comma - pwd->pw_gecos));
                strncpy(real_name, pwd->pw_gecos, n);
                real_name[n] = '\0';
            }
            else {
                strncpy(real_name, pwd->pw_gecos, sizeof(real_name));
                real_name[sizeof(real_name) - 1] = '\0';
            }
        }
        else
            real_name[0] = '\0';
    }
#else
    real_name[0] = '\0';
#endif

    /*
     * The FQDM of this host or the empty string.
     */
#ifdef H5_HAVE_GETHOSTNAME
    if (gethostname(host_name, sizeof(host_name)) < 0) {
        host_name[0] = '\0';
    }
#else
    host_name[0] = '\0';
#endif

    /*
     * The file header: warning, copyright notice, build information.
     */
    fprintf(rawoutstream, "/* Generated automatically by H5detect -- do not edit */\n\n\n");
    fputs(FileHeader, rawoutstream); /*the copyright notice--see top of this file */

    fprintf(rawoutstream, " *\n * Created:\t\t%s %2d, %4d\n", month_name[tm->tm_mon], tm->tm_mday,
            1900 + tm->tm_year);
    if (pwd || real_name[0] || host_name[0]) {
        fprintf(rawoutstream, " *\t\t\t");
        if (real_name[0])
            fprintf(rawoutstream, "%s <", real_name);
#ifdef H5_HAVE_GETPWUID
        if (pwd)
            fputs(pwd->pw_name, rawoutstream);
#endif
        if (host_name[0])
            fprintf(rawoutstream, "@%s", host_name);
        if (real_name[0])
            fprintf(rawoutstream, ">");
        fputc('\n', rawoutstream);
    }
    fprintf(rawoutstream, " *\n * Purpose:\t\t");
    for (s = purpose; *s; s++) {
        fputc(*s, rawoutstream);
        if ('\n' == *s && s[1])
            fprintf(rawoutstream, " *\t\t\t");
    }

    fprintf(rawoutstream, " *\n");
    fprintf(rawoutstream, " *\tDO NOT MAKE MODIFICATIONS TO THIS FILE!\n");
    fprintf(rawoutstream, " *\tIt was generated by code in `H5detect.c'.\n");

    fprintf(rawoutstream, " *\n *");
    for (i = 0; i < 73; i++)
        fputc('-', rawoutstream);
    fprintf(rawoutstream, "\n */\n\n");
}

/*-------------------------------------------------------------------------
 * Function:    detect_C89_floats
 *
 * Purpose:     Detect C89 floating point types
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void HDF_NO_UBSAN
detect_C89_floats(void)
{
    DETECT_F(float, FLOAT, d_g[nd_g]);
    nd_g++;
    DETECT_F(double, DOUBLE, d_g[nd_g]);
    nd_g++;
}

/*-------------------------------------------------------------------------
 * Function:    detect_C99_floats
 *
 * Purpose:     Detect C99 floating point types
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void HDF_NO_UBSAN
detect_C99_floats(void)
{
#if H5_SIZEOF_DOUBLE == H5_SIZEOF_LONG_DOUBLE
    /*
     * If sizeof(double)==sizeof(long double) then assume that `long double'
     * isn't supported and use `double' instead.  This suppresses warnings on
     * some systems and `long double' is probably the same as `double' here
     * anyway.
     */
    DETECT_F(double, LDOUBLE, d_g[nd_g]);
    nd_g++;
#else
    DETECT_F(long double, LDOUBLE, d_g[nd_g]);
    nd_g++;
#endif
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Main entry point.
 *
 * Return:      Success:    EXIT_SUCCESS
 *
 *-------------------------------------------------------------------------
 */
int HDF_NO_UBSAN
main(int argc, char *argv[])
{
    char *fname = NULL;
    FILE *f; /* temporary holding place for the stream pointer
              * so that rawoutstream is changed only when succeeded */

    if (argc > 1)
        fname = argv[1];

    /* First check if filename is string "NULL" */
    if (fname != NULL) {
        /* binary output */
        if ((f = fopen(fname, "w")) != NULL)
            rawoutstream = f;
    }
    if (!rawoutstream)
        rawoutstream = stdout;

    print_header();

    /* C89 floating point types */
    detect_C89_floats();

    /* C99 floating point types */
    detect_C99_floats();

    print_results(nd_g, d_g);

    if (rawoutstream && rawoutstream != stdout) {
        if (fclose(rawoutstream))
            fprintf(stderr, "closing rawoutstream");
        else
            rawoutstream = NULL;
    }

    return EXIT_SUCCESS;
}
