#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "gifti_io.h"

/*! global history and version strings, for printing */
static char * gifti_history[] =
{
  "----------------------------------------------------------------------\n"
  "history (of gifti library changes):\n"
  "\n",
  "0.0  18 July, 2007\n"
  "     (Rick Reynolds of the National Institutes of Health, SSCC/DIRP/NIMH)\n"
  "     - initial version\n"
  "0.1  31 July, 2007\n",
  "     - changed dim0..dim5 to dims[]\n"
  "     - changed nvals to size_t\n"
  "     - added gifti_init_darray_from_attrs and some validation functions\n"
  "0.2  29 October, 2007\n",
  "     - renamed gifti.[ch] to gifti_io.[ch]\n"
  "     - main data structures all start with gii (or gifti_)\n"
  "     - added user indenting\n"
  "0.3  21 November, 2007\n",
  "     - added base64 encoding/decoding, via b64_en/decode_table\n"
  "     - added gifti_list_index2string, gifti_disp_hex_data, \n"
  "             gifti_check_swap, gifti_swap_Nbytes, etc.\n"
  "     - pop_darray: check for b64 errors and byte swapping\n"
  "     - dind is size_t\n",
  "0.4  29 November, 2007\n"
  "     - added more checks and fixed nvpair value allocation\n"
  "0.5  03 December, 2007: applied changes for GIFTI Format 1.0 (11/21)\n",
  "     - replaced Category with Intent\n"
  "     - replaced Location attribute with ExternalFileName/Offset\n"
  "     - added NumberOfDataArrays attribute to GIFTI element\n"
  "     - applied new index_order strings\n"
  "0.6  10 December, 2007:\n",
  "     - can read/write Base64Binary datasets (can set compress level)\n"
  "     - removed datatype lists (have gifti_type_list)\n"
  "     - added gifti_read_da_list(), with only partial ability\n"
  "     - added GIFTI numDA attribute\n"
  "     - change size_t to long long\n"
  "0.7  11 December, 2007:\n",
  "     - added GIFTI_B64_CHECK defines\n"
  "     - set b64_check default to SKIPNCOUNT\n"
  "     - added disp_gxml_data\n"
  "0.8  12 December, 2007:\n",
  "     - added sub-surface selection, via dalist in gifti_read_da_list()\n"
  "     - added gifti_copy_DataArray, and other structures\n"
  "0.9  28 December, 2007:\n",
  "     - made zlib optional, via -DHAVE_ZLIB in compile\n"
  "       (without zlib, the user will get warnings)\n"
  "     - now users only #include gifti_io.h, not gifti_xml, expat or zlib\n"
  "     - added more comments and made tables more readable\n"
  "     - added all user-variable access functions and reset_user_vars()\n",
  "     - added gifti_free_image_contents(), gifti_disp_raw_data(),\n"
  "             gifti_clear_float_zeros() and gifti_set_DA_atrs()\n"
  "     - changed gifti_gim_DA_size to long long\n"
  "     - added GIFTI_B64_CHECK_UNDEF as 0\n"
  "     - fixed 0-width indenting and accumulating base64 errors\n"
  "0.10 03 January, 2008:\n",
  "     - added top-level gifti_create_image() interface\n"
  "     - must now link libniftiio\n"
  "     - gifti_add_empty_darray() now takes num_to_add\n"
  "     - if data was expected but not read, free it\n"
  "         (can add via gifti_alloc_DA_data())\n"
  "     - many minor changes\n"
  "0.11 11 January, 2008:\n",
  "     - attribute/data setting functions are more flexible\n"
  "     - added gifti_disp_dtd_url, gifti_set_DA_meta, gifti_valid_int_list,\n"
  "       DA_data_exists, gifti_add_to_meta \n"
  "0.12 16 January, 2008:\n",
  "     - added gifti_copy_gifti_image() and gifti_convert_to_float()\n"
  "     - added gifti_valid_LabelTable(), gifticlib_version(),\n"
  "             gifti_copy_LabelTable(), gifti_updaet_nbyper() and\n"
  "             gifti_valid_gifti_image()\n"
  "     - added control over library updates to metadata\n"
  "     - expanded checks in gifti_valid_dims\n"
  "0.13 20 February, 2008:\n",
  "     - added gifti_get_meta_value() and gifti_image_has_data()\n"
  "0.14 25 February, 2008:\n",
  "     - consider data-less metadata as valid\n"
  "0.15 18 March, 2008: added comparison functions\n",
  "     - gifti_compare_gifti_images() is top-level function\n"
  "     - also added gifti_compare_gims_only(), gifti_compare_DA_pair(),\n"
  "                  gifti_compare_nvpairs(), gifti_compare_labeltable(),\n"
  "                  gifti_compare_coordsys()\n"
  "                  gifti_strdiff() and gifti_compare_raw_data()\n"
  "0.16 25 March, 2008\n",
  "     - separate data diffs in compare_gifti_images\n"
  "     - added gifti_compare_gifti_data() and gifti_compare_DA_data()\n"
  "     - NIFTI_INTENT_NONE is considered valid\n"
  "     - write LabelTables using CDATA\n"
  "0.17 28 March, 2008 : added copy MetaData routines\n",
  "     - gifti_copy_gifti_meta, gifti_copy_DA_meta, gifti_copy_all_DA_meta,\n"
  "     - gifti_copy_DA_meta_many, gifti_copy_nvpairs\n"
  "0.18 08 May, 2008 : DataArray can now contain a list of CoordSystems\n",
  "     - modified giiDataArray struct: new numCS, coordsys is now CS**\n"
  "     - added gifti_free_CS_list, gifti_add_empty_CS\n"
  "\n"
  "------------------------ initial release version -----------------------\n",
  "1.00 13 May, 2008 : release version of gifticlib\n",
  "     - allowed external data\n"
  "     - added gifti_read/write_extern_DA_data() and\n"
  "             gifti_set_extern_filelist()\n"
  "1.01 02 June, 2008 :\n",
  "     - added CMakeLists.txt and XMLCALL update from Simon Warfield\n"
  "       (define XMLCALL for pre-1.95.7 versions of expat)\n"
  "     - added LICENSE.gifti\n"
  "1.02 02 October, 2008 :\n",
  "     - separate diffs in DAs from those in gifti_image\n"
  "     - decode additional data types: INT8, UINT16, INT64\n"
  "     - add link flags to libgiftiio_la target\n"
  "1.03 17 April, 2009 : allow DA size to vary over each external file\n",
  "1.04 27 October, 2009 : added support for LabelTable RGBA attributes\n"
  "     - valid LabelTable requires RGBA values in [0,1.0]\n"
  "     - compare_labeltable requires equality of RGBA values (no approx.)\n",
  "1.05 08 December, 2009: ignore invalid GIFTI attrs by default\n"
  "1.06 24 December, 2009: added approximate difference functions\n",
  "     - added gifti_approx_gifti_images, DA_pair, labeltables, diff_offset\n"
  "     - added gifti_triangle_diff_offset\n"
  "     - gifti_compare_coordsys takes comp_data param\n"
  "1.07 04 March, 2010: minor changes (also see NITRC IDs 4619 and 4644)\n",
  "     - for integers, make default approx test to be equality\n"
  "     - small changes to zlib failure strings\n"
  "     - cast to avoid compile warning on some systems\n"
  "     - gifti_xml.h: made NITRC gifti.dtd link that will not change\n"
  "1.08 08 March, 2010: GIfTI LabelTable format change: Index to Key\n",
  "     - both Index and Key work on read, Key is written out\n"
  "1.09 28 June, 2010: verify that num_dim is not too big\n",
  "     - the most significant dimension cannot be 1 (req by N Schmansky)\n"
};

static char gifti_version[] = "gifti library version 1.09, 28 June, 2010";

/* ---------------------------------------------------------------------- */
/*! global lists of XML strings */

/*! this should match GIFTI_IND_ORD_* */
char * gifti_index_order_list[] = {"Undefined", "RowMajorOrder",
                                                "ColumnMajorOrder"};

/*! gifti_type_list is an array of gifti_type_ele structs which list, for
    each type, the bytes per value, swapsize and corresponding name string
    (these type values are defined in nifti1.h) */
static gifti_type_ele gifti_type_list[] = {
    /* type                    nbyper  swapsize   name  */
    { DT_UNKNOWN,                 0,      0,      "Undefined"             },
    { NIFTI_TYPE_UINT8,           1,      0,      "NIFTI_TYPE_UINT8"      },
    { NIFTI_TYPE_INT16,           2,      2,      "NIFTI_TYPE_INT16"      },
    { NIFTI_TYPE_INT32,           4,      4,      "NIFTI_TYPE_INT32"      },
    { NIFTI_TYPE_FLOAT32,         4,      4,      "NIFTI_TYPE_FLOAT32"    },
    { NIFTI_TYPE_COMPLEX64,       8,      4,      "NIFTI_TYPE_COMPLEX64"  },
    { NIFTI_TYPE_FLOAT64,         8,      8,      "NIFTI_TYPE_FLOAT64"    },
    { NIFTI_TYPE_RGB24,           3,      0,      "NIFTI_TYPE_RGB24"      },
    { NIFTI_TYPE_INT8,            1,      0,      "NIFTI_TYPE_INT8"       },
    { NIFTI_TYPE_UINT16,          2,      2,      "NIFTI_TYPE_UINT16"     },
    { NIFTI_TYPE_UINT32,          4,      4,      "NIFTI_TYPE_UINT32"     },
    { NIFTI_TYPE_INT64,           8,      8,      "NIFTI_TYPE_INT64"      },
    { NIFTI_TYPE_UINT64,          8,      8,      "NIFTI_TYPE_UINT64"     },
    { NIFTI_TYPE_FLOAT128,        6,     16,      "NIFTI_TYPE_FLOAT128"   },
    { NIFTI_TYPE_COMPLEX128,     16,      8,      "NIFTI_TYPE_COMPLEX128" },
    { NIFTI_TYPE_COMPLEX256,     32,     16,      "NIFTI_TYPE_COMPLEX256" }
};

/*! this list provides a link between intent codes and their name strings */
typedef struct { int code; char * name; } gifti_intent_ele;
static gifti_intent_ele gifti_intent_list[] = {
    { NIFTI_INTENT_NONE,             "NIFTI_INTENT_NONE"        },
    { NIFTI_INTENT_CORREL,           "NIFTI_INTENT_CORREL"      },
    { NIFTI_INTENT_TTEST,            "NIFTI_INTENT_TTEST"       },
    { NIFTI_INTENT_FTEST,            "NIFTI_INTENT_FTEST"       },
    { NIFTI_INTENT_ZSCORE,           "NIFTI_INTENT_ZSCORE"      },
    { NIFTI_INTENT_CHISQ,            "NIFTI_INTENT_CHISQ"       },
    { NIFTI_INTENT_BETA,             "NIFTI_INTENT_BETA"        },
    { NIFTI_INTENT_BINOM,            "NIFTI_INTENT_BINOM"       },
    { NIFTI_INTENT_GAMMA,            "NIFTI_INTENT_GAMMA"       },
    { NIFTI_INTENT_POISSON,          "NIFTI_INTENT_POISSON"     },
    { NIFTI_INTENT_NORMAL,           "NIFTI_INTENT_NORMAL"      },
    { NIFTI_INTENT_FTEST_NONC,       "NIFTI_INTENT_FTEST_NONC"  },
    { NIFTI_INTENT_CHISQ_NONC,       "NIFTI_INTENT_CHISQ_NONC"  },
    { NIFTI_INTENT_LOGISTIC,         "NIFTI_INTENT_LOGISTIC"    },
    { NIFTI_INTENT_LAPLACE,          "NIFTI_INTENT_LAPLACE"     },
    { NIFTI_INTENT_UNIFORM,          "NIFTI_INTENT_UNIFORM"     },
    { NIFTI_INTENT_TTEST_NONC,       "NIFTI_INTENT_TTEST_NONC"  },
    { NIFTI_INTENT_WEIBULL,          "NIFTI_INTENT_WEIBULL"     },
    { NIFTI_INTENT_CHI,              "NIFTI_INTENT_CHI"         },
    { NIFTI_INTENT_INVGAUSS,         "NIFTI_INTENT_INVGAUSS"    },
    { NIFTI_INTENT_EXTVAL,           "NIFTI_INTENT_EXTVAL"      },
    { NIFTI_INTENT_PVAL,             "NIFTI_INTENT_PVAL"        },
    { NIFTI_INTENT_LOGPVAL,          "NIFTI_INTENT_LOGPVAL"     },
    { NIFTI_INTENT_LOG10PVAL,        "NIFTI_INTENT_LOG10PVAL"   },
    { NIFTI_INTENT_ESTIMATE,         "NIFTI_INTENT_ESTIMATE"    },
    { NIFTI_INTENT_LABEL,            "NIFTI_INTENT_LABEL"       },
    { NIFTI_INTENT_NEURONAME,        "NIFTI_INTENT_NEURONAME"   },
    { NIFTI_INTENT_GENMATRIX,        "NIFTI_INTENT_GENMATRIX"   },
    { NIFTI_INTENT_SYMMATRIX,        "NIFTI_INTENT_SYMMATRIX"   },
    { NIFTI_INTENT_DISPVECT,         "NIFTI_INTENT_DISPVECT"    },
    { NIFTI_INTENT_VECTOR,           "NIFTI_INTENT_VECTOR"      },
    { NIFTI_INTENT_POINTSET,         "NIFTI_INTENT_POINTSET"    },
    { NIFTI_INTENT_TRIANGLE,         "NIFTI_INTENT_TRIANGLE"    },
    { NIFTI_INTENT_QUATERNION,       "NIFTI_INTENT_QUATERNION"  },
    { NIFTI_INTENT_DIMLESS,          "NIFTI_INTENT_DIMLESS"     },
    { NIFTI_INTENT_TIME_SERIES,      "NIFTI_INTENT_TIME_SERIES" },
    { NIFTI_INTENT_NODE_INDEX,       "NIFTI_INTENT_NODE_INDEX"  },
    { NIFTI_INTENT_RGB_VECTOR,       "NIFTI_INTENT_RGB_VECTOR"  },
    { NIFTI_INTENT_RGBA_VECTOR,      "NIFTI_INTENT_RGBA_VECTOR" },
    { NIFTI_INTENT_SHAPE,            "NIFTI_INTENT_SHAPE"       }
};

/*! this should match GIFTI_ENCODING_* */
char * gifti_encoding_list[] = {
    "Undefined", "ASCII", "Base64Binary", "GZipBase64Binary",
    "ExternalFileBinary"
};

/*! this should match GIFTI_ENDIAN_* */
char * gifti_endian_list[] = {"Undefined", "BigEndian", "LittleEndian"};

/* ---------------------------------------------------------------------- */
/* local prototypes */
static int can_compare_DA_data(const giiDataArray *d1,const giiDataArray *d2,
                               int verb);
static int compare_labeltables(const giiLabelTable *t1, const giiLabelTable *t2,
                               int verb, int approx);
static int copy_data_as_float(void * dest, int dtype, void * src, int stype,
                              long long nvals);
static int DA_data_exists(gifti_image * gim, const int * dalist, int len);
static int str2list_index(char *list[], int max, const char *str);

/* ---------------------------------------------------------------------- */
/*! giftilib globals */
static gifti_globals G = { 1 };

/* ====================================================================== */

/* ---------------------------------------------------------------------- */
/*! user variable accessor functions - basically use gxml interface       */

int gifti_get_verb( void )          { return G.verb; }
int gifti_set_verb( int level )     { G.verb = level;  return 1; }
int gifti_get_indent( void )        { return gxml_get_indent(); }
int gifti_set_indent( int level )   { return gxml_set_indent(level); }
int gifti_get_b64_check( void )     { return gxml_get_b64_check(); }
int gifti_set_b64_check( int level ){ return gxml_set_b64_check(level); }
int gifti_get_update_ok( void )     { return gxml_get_update_ok(); }
int gifti_set_update_ok( int level ){ return gxml_set_update_ok(level); }
int gifti_get_zlevel( void )        { return gxml_get_zlevel(); }
int gifti_set_zlevel( int level )
{
    /* note that the default currently results in 6 */
    if( level != GZ_DEFAULT_COMPRESSION && (level < 0 || level > 9 ) ) {
        fprintf(stderr,"** invalid zlevel, must be %d (default) or {0..9}\n",
                GZ_DEFAULT_COMPRESSION);
        return 1;
    }
    return gxml_set_zlevel(level);
}

int gifti_get_xml_buf_size(void)        { return gxml_get_buf_size(); }
int gifti_set_xml_buf_size(int buf_size){ return gxml_set_buf_size(buf_size); }

/*! reset user variables to their defaults(via set to -1) */
int gifti_reset_user_vars(void)
{
    gxml_set_verb(-1);
    gxml_set_dstore(-1);
    gxml_set_indent(-1);
    gxml_set_buf_size(-1);
    gxml_set_b64_check(-1);
    gxml_set_update_ok(-1);
    gxml_set_zlevel(-1);

    return 0;
}
/* end user variable accessor functions                                   */
/* ---------------------------------------------------------------------- */

/* ====================================================================== */

/* ---------------------------------------------------------------------- */
/* begin general library functions                                        */

/*----------------------------------------------------------------------
 *! apply the attr=value GIFTI attribute to the gifti_image
 *
 *  return 0 on success
*//*-------------------------------------------------------------------*/
int gifti_str2attr_gifti(gifti_image * gim, const char *attr, const char *val)
{
    if( !gim || !attr || !val ) {
        fprintf(stderr,"** GS2AG: bad params (%p,%p,%p)\n",
                (void *)gim, (void *)attr, (void *)val);
        return 1;
    }

    if( G.verb > 2 )
        fprintf(stderr,"++ setting GIFTI attr '%s' from '%s'\n", attr, val);

    if( !strcmp(attr, "Version") ) {
        if( gim->version ) free( gim->version );  /* lose any old copy */
        gim->version = gifti_strdup(val);
    } else if( !strcmp(attr, "NumberOfDataArrays") ) {
        gim->numDA = atol(val);
        if( gim->numDA < 0 ) {
            fprintf(stderr,"** invalid NumberOfDataArrays attribute: %s\n",val);
            gim->numDA = 0;
            return 1;
        }
    } else if( !strcmp(attr, "xmlns:xsi") ||
               !strcmp(attr, "xsi:noNamespaceSchemaLocation") ) {
        if( G.verb > 1 )
            fprintf(stderr,"-- have GIFTI attr, '%s'='%s'\n",attr,val);
        return 1;
    } else {
        if( G.verb > 1 )
            fprintf(stderr,"** unknown GIFTI attrib, '%s'='%s'\n",attr,val);
        return 1;
    }

    return 0;
}

/*----------------------------------------------------------------------
 *! This is the main dataset reading routine.  Read a GIFTI dataset
 *  and return the corresponding gifti_image structure.
 *
 *  Reading data is optional, via the read_data flag.
 *  User variables should already be set (via accessor functions).
 *
 *  return an allocated gifti_image struct on success,
 *         NULL on error
*//*-------------------------------------------------------------------*/
gifti_image * gifti_read_image( const char * fname, int read_data )
{
    if( !fname ) {
        fprintf(stderr,"** gifti_read_image: missing filename\n");
        return NULL;
    }

    gxml_set_verb(G.verb);

    return gxml_read_image(fname, read_data, NULL, 0);
}

/*----------------------------------------------------------------------
 *! Similar to gifti_read_data, this function also takes an integer list of
 *  DataArray indices to populate the gifti_image structure with.
 *
 *  The indices are be zero-based, can have repeats and can be in any order.
 *  A simple example to read 3 DA elements (with 2 repeats) might be:
 *
 *    gifti_image * gim;
 *    int           ilist[5] = { 3, 0, 7, 7, 3 };
 *    gim = gifti_read_da_list("my_data.gii", 1, ilist, 5);
 *
 *  return an allocated gifti_image struct on success,
 *         NULL on error
*//*-------------------------------------------------------------------*/
gifti_image * gifti_read_da_list( const char * fname, int read_data,
                                  const int * dalist, int len )
{
    if( !fname ) {
        fprintf(stderr,"** gifti_read_da_list: missing filename\n");
        return NULL;
    }

    gxml_set_verb(G.verb);

    return gxml_read_image(fname, read_data, dalist, len);
}

/*----------------------------------------------------------------------
 *! This is the main dataset writing routine.
 *
 *  User variables should be set before this point.
 *
 *  return 0 on success
 *         1 on error
*//*-------------------------------------------------------------------*/
int gifti_write_image(gifti_image *gim, const char *fname, int write_data)
{
    int errs = 0;

    if( !gim ) {
        fprintf(stderr,"** gifti_write_image, missing gifti_image\n");
        errs++;
    } else if( !fname ) {
        fprintf(stderr,"** gifti_write_image: missing filename\n");
        errs++;
    }

    if( errs ) return 1;

    gxml_set_verb(G.verb);

    return gxml_write_image(gim, fname, write_data);
}


/*----------------------------------------------------------------------
 *! free the gifti_image struct and all its contents
 *
 *  passing NULL (to this and any child function) should be okay
 *
 *  the pointer is garbage after this call
*//*-------------------------------------------------------------------*/
int gifti_free_image( gifti_image * gim )
{
    if( !gim ) {
        if(G.verb > 2) fprintf(stderr,"** free gifti_image w/NULL pointer\n");
        return 1;
    }

    if( G.verb > 2 ) fprintf(stderr,"-- freeing gifti_image\n");

    if( gim->version ) { free(gim->version);  gim->version = NULL; }

    (void)gifti_free_nvpairs(&gim->meta);
    (void)gifti_free_LabelTable(&gim->labeltable);
    (void)gifti_free_DataArray_list(gim->darray, gim->numDA);
    (void)gifti_free_nvpairs(&gim->ex_atrs);
    free(gim);

    return 0;
}

/*----------------------------------------------------------------------
 *! free the contents of the gifti_image struct (but not the pointer)
 *
 *  the pointer is garbage after this call
*//*-------------------------------------------------------------------*/
int gifti_free_image_contents( gifti_image * gim )
{
    if( !gim ) {
        if(G.verb > 2) fprintf(stderr,"** GFIC: free w/NULL gifti_image ptr\n");
        return 1;
    }

    if( G.verb > 2 ) fprintf(stderr,"-- freeing gifti_image contents\n");

    if( gim->version ) { free(gim->version);  gim->version = NULL; }

    (void)gifti_free_nvpairs(&gim->meta);
    (void)gifti_free_LabelTable(&gim->labeltable);
    (void)gifti_free_DataArray_list(gim->darray, gim->numDA);
    (void)gifti_free_nvpairs(&gim->ex_atrs);

    return 0;
}

/*----------------------------------------------------------------------
 *! free the contents of the nvpairs struct (but not the pointer)
 *
 *  passing NULL is okay
*//*-------------------------------------------------------------------*/
int gifti_free_nvpairs( nvpairs * p )
{
    int c;

    if( !p ) {
        if( G.verb > 3 ) fprintf(stderr,"** free w/NULL nvpairs ptr\n");
        return 1;
    }

    if( G.verb > 3 ) fprintf(stderr,"-- freeing %d nvpairs\n", p->length);

    if( p->name && p->value ) {
        for( c = 0; c < p->length; c++ ) {
            if( p->name[c] ) free(p->name[c]);
            if( p->value[c] ) free(p->value[c]);
        }
        free(p->name);
        free(p->value);
        p->name = NULL;
        p->value = NULL;
    }
    p->length = 0;

    return 0;
}

/*----------------------------------------------------------------------
 *! free the contents of the LabelTable struct (but not the pointer)
 *
 *  passing NULL is okay
*//*-------------------------------------------------------------------*/
int gifti_free_LabelTable( giiLabelTable * T )
{
    int c;

    if( !T ) {
        if(G.verb > 3) fprintf(stderr,"** free w/NULL giiLabelTable ptr\n");
        return 1;
    }

    if(G.verb > 3)
        fprintf(stderr,"-- freeing %d giiLabelTable entries\n", T->length);

    if( T->key && T->label ) {
        for( c = 0; c < T->length; c++ )
            if( T->label[c] ) free(T->label[c]);
        free(T->key);
        free(T->label);
        T->key = NULL;
        T->label = NULL;
    }

    if( T->rgba ) {
        free(T->rgba);
        T->rgba = NULL;
    }

    T->length = 0;

    return 0;
}

/*----------------------------------------------------------------------
 *! free the DataArray list (the array and all its contents)
 *
 *  the darray list pointer is garbage after this call
 *
 *  passing NULL is okay
*//*-------------------------------------------------------------------*/
int gifti_free_DataArray_list(giiDataArray ** darray, int numDA)
{
    int c;

    if( !darray ) {
        if( G.verb > 3 ) fprintf(stderr,"** GFDA: free NULL darray list\n");
        return 1;
    }

    if( G.verb > 3 ) fprintf(stderr,"-- freeing %d giiDataArrays\n", numDA);

    if( numDA < 0 ) return 1;

    for( c = 0; c < numDA; c++ )
        if( gifti_free_DataArray(darray[c]) ) return 1;

    free(darray);

    return 0;
}

/*----------------------------------------------------------------------
 *! free the DataArray struct and all its contents
 *
 *  the DataArray pointer is garbage after this call
 *
 *  passing NULL is okay
*//*-------------------------------------------------------------------*/
int gifti_free_DataArray( giiDataArray * darray )
{
    if( !darray ) {
        if( G.verb > 3 ) fprintf(stderr,"** tried to free NULL darray ptr\n");
        return 1;
    }

    if( G.verb > 3 ) fprintf(stderr,"-- freeing giiDataArray\n");

    if(darray->ext_fname) { free(darray->ext_fname); darray->ext_fname = NULL; }

    (void)gifti_free_nvpairs(&darray->meta);
    (void)gifti_free_CS_list(darray);
    if( darray->data ) { free(darray->data); darray->data = NULL; }
    (void)gifti_free_nvpairs(&darray->ex_atrs);
    free(darray);

    return 0;
}

/*----------------------------------------------------------------------
 *! free the CoordSystem array from a DataArray
 *  passing NULL is okay
*//*-------------------------------------------------------------------*/
int gifti_free_CS_list( giiDataArray * da )
{
    int c;

    if( !da ) return 0;

    if( G.verb > 3 ) fprintf(stderr,"-- freeing giiCoordSystem list\n");

    if( da->coordsys && da->numCS > 0 ) {
        for( c = 0; c < da->numCS; c++ )
            gifti_free_CoordSystem(da->coordsys[c]);
        free(da->coordsys);
    }

    da->coordsys = NULL;
    da->numCS = 0;

    return 0;
}

/*----------------------------------------------------------------------
 *! free the CoordSystem struct and all its contents
 *
 *  the CoordSystem pointer is garbage after this call
 *
 *  passing NULL is okay
*//*-------------------------------------------------------------------*/
int gifti_free_CoordSystem( giiCoordSystem * cs )
{
    if( !cs ) return 0;

    if( G.verb > 3 ) fprintf(stderr,"-- freeing giiCoordSystem\n");

    if( cs->dataspace ) { free(cs->dataspace); cs->dataspace = NULL; }
    if( cs->xformspace ) { free(cs->xformspace); cs->xformspace = NULL; }

    free(cs);

    return 0;
}

/*----------------------------------------------------------------------
 *! initialize an existing DataArray structure given a list of name=value
 *  attribute pairs
 *
 *  if alen > 0, consider it the length of the attr list
 *  else         process until attr[i] == NULL
 *
 *  if add_to_extras, add any bad attribute pairs to ex_atrs
 *  else              whine about any bad ones and return
*//*-------------------------------------------------------------------*/
int gifti_set_DA_atrs(giiDataArray * da, const char ** attr, int alen,
                      int add_to_extras )
{
    int c, length = alen;

    if( !da || !attr ) {
        if(G.verb>1) fprintf(stderr,"** G_IDFA: bad params (%p,%p)\n",
                             (void *)da,(void *)attr);
        return 1;
    }

    /* if length was not passed, compute it */
    if( length <= 0 ) for( length = 0; attr[length]; length++ ) /* nada */ ;

    if( G.verb > 5 )
        fprintf(stderr,"++ init darray attrs, len %d, ex_atrs = %d\n",
                length, add_to_extras);

    /* insert attributes - if unknown, store with extras */
    for(c = 0; c < length; c += 2 )
        if( gifti_str2attr_darray(da, attr[c],attr[c+1]) ) {
            /* a bad name=value pair, maybe add to ex_atrs */
            if( add_to_extras ) {
                if( gifti_add_to_nvpairs(&da->ex_atrs,attr[c],attr[c+1]) )
                    return 1;
            }
            else {
                if( G.verb > 0 )
                    fprintf(stderr,"** set_darray_atrs, bad pair '%s'='%s'\n",
                            attr[c],attr[c+1]);
                return 1;
            }
        }

    /* and set computed values */

    da->nvals = gifti_darray_nvals(da);
    gifti_datatype_sizes(da->datatype, &da->nbyper, NULL); /* set nbyper */

    return 0;
}

/*----------------------------------------------------------------------
 *! determine whether the given DataArray struct seems valid
 *
 *  if whine is set, print error messages for any failures
 *
 *  return 1, if valid
 *         0, if not
*//*-------------------------------------------------------------------*/
int gifti_valid_DataArray(const giiDataArray * da, int whine)
{
    int errs = 0, nbyper;

    if( !da ) {
        if( whine || G.verb > 2 ) fprintf(stderr,"** invalid darray pointer\n");
        return 0;
    }

    if( ! gifti_intent_is_valid(da->intent) ) {
        if( whine || G.verb > 3 )
            fprintf(stderr,"** invalid darray intent code = %d\n", da->intent);
        errs++;
    }

    if( ! gifti_valid_datatype(da->datatype, whine) ) /* message printed */
        errs++;

    /* no checks for ext_fname and ext_offset (until reading) */

    if( da->ind_ord<=GIFTI_IND_ORD_UNDEF || da->ind_ord>GIFTI_IND_ORD_MAX ) {
        if( whine || G.verb > 3 )
            fprintf(stderr,"** invalid darray ind_ord = %d\n", da->ind_ord);
        errs++;
    }

    if( ! gifti_valid_num_dim(da->num_dim, whine) ) /* message printed */
        errs++;

    if( ! gifti_valid_dims(da, whine) ) /* message printed */
        errs++;

    if( da->encoding<=GIFTI_ENCODING_UNDEF || da->encoding>GIFTI_ENCODING_MAX ){
        if( whine || G.verb > 3 )
            fprintf(stderr,"** invalid darray encoding = %d\n", da->encoding);
        errs++;
    }

    if( da->endian<=GIFTI_ENDIAN_UNDEF || da->endian>GIFTI_ENDIAN_MAX ) {
        if( whine || G.verb > 3 )
            fprintf(stderr,"** invalid darray endian = %d\n", da->endian);
        errs++;
    }

    /* of sub-element, only verify giiMetaData */
    if( ! gifti_valid_nvpairs(&da->meta, whine) ) /* message printed */
        errs++;

    if( da->nvals <= 0 ) {
        if( whine || G.verb > 3 )
            fprintf(stderr,"** invalid darray nvals = %u\n",
                    (unsigned)da->nvals );
        errs++;
    }

    if( ! gifti_valid_nbyper(da->nbyper, whine) ) errs++;
    if( ! gifti_valid_nvpairs(&da->ex_atrs, whine) ) errs++;

    /* compare nbyper to what is expected for type */
    errs += gifti_datatype_sizes(da->datatype, &nbyper, NULL);
    if( gifti_valid_nbyper(nbyper, 0) && nbyper != da->nbyper ) {
        if( whine || G.verb > 3 )
            fprintf(stderr,"** nbyper %d, does not match type %s\n",
                    nbyper, gifti_datatype2str(da->datatype));
        errs++;
    }

    if( errs ) return 0;

    return 1;
}

/*----------------------------------------------------------------------
 *! check whether pointers are valid and consistent with length
*//*-------------------------------------------------------------------*/
int gifti_valid_nvpairs(const nvpairs * nvp, int whine)
{
    int c;

    if( !nvp ) {
        if( G.verb>3 || whine ) fprintf(stderr,"** invalid nvpairs pointer\n");
        return 0;
    }

    if( nvp->length < 0 ) {
        if( G.verb > 3 || whine )
            fprintf(stderr,"** invalid nvpair length = %d\n", nvp->length);
        return 0;
    }

    if( nvp->length == 0 ) return 1;    /* quick case: valid */

    if( !nvp->name || !nvp->value ){
        if( G.verb > 3 || whine )
            fprintf(stderr,"** invalid nvpair name, value lists = %p, %p\n",
                    (void *)nvp->name, (void *)nvp->value);
        return 0;
    }

    /* quit on first error */
    for( c = 0; c < nvp->length; c++ ) {
        if( ! nvp->name[c] ) {
            if( G.verb > 5 || whine )
                fprintf(stderr,"** invalid nvpair, missing name @ %d\n", c);
            return 0;
        }

        /* value string is not required   25 Feb 2008 */
        if( ! nvp->value[c] && G.verb > 3 )
            fprintf(stderr,"-- missing nvpair value[%d], name %s (is OK)\n",
                    c, nvp->name[c]);
    }

    return 1;
}

/*----------------------------------------------------------------------
 *! check whether pointers are valid and consistent with length
 *
 *  no check is done on the actual indices or labels
*//*-------------------------------------------------------------------*/
int gifti_valid_LabelTable(const giiLabelTable * T, int whine)
{
    float * rgba;
    int     c, c2;

    if( !T ) {
        if(G.verb>2||whine) fprintf(stderr,"** invalid LabelTable pointer\n");
        return 0;
    }

    if( T->length < 0 ) {
        if( G.verb > 3 || whine )
            fprintf(stderr,"** invalid LabelTable length = %d\n", T->length);
        return 0;
    }

    if( T->length == 0 ) return 1;    /* quick case: valid */

    if( !T->key || !T->label ){
        if( G.verb > 3 || whine )
            fprintf(stderr,"** invalid nvpair key, label = %p, %p\n",
                    (void *)T->key, (void *)T->label);
        return 0;
    }

    /* quit on first error */
    rgba = T->rgba;
    for( c = 0; c < T->length; c++ ) {
        if( ! T->label[c] ) {
            if( G.verb > 3 || whine )
                fprintf(stderr,"** invalid nvpair label[%d]\n", c);
            return 0;
        }
        if( rgba ) {
            for( c2 = 0; c2 < 4; c2++ )
                if( rgba[c2] < 0.0 || rgba[c2] > 1.0 ) {
                    if( G.verb > 3 || whine )
                        fprintf(stderr,"** RGBA values out of [0.0,1,0] at "
                                       "Label[%d]\n", c);
                    return 0;
                }
            rgba += 4; /* if list exists, go to next set */
        }
    }

    return 1;
}

/*----------------------------------------------------------------------
 *! check the bounds on num_dim
*//*-------------------------------------------------------------------*/
int gifti_valid_num_dim(int num_dim, int whine)
{
    if( num_dim <= 0 || num_dim > GIFTI_DARRAY_DIM_LEN ) {
        if( G.verb > 3 || whine )
            fprintf(stderr,"** invalid num_dim = %d\n", num_dim);
        return 0;
    }
    return 1;
}

/*----------------------------------------------------------------------
 *! check that the datatype is in the list
*//*-------------------------------------------------------------------*/
int gifti_valid_datatype(int dtype, int whine)
{
    int c;

    /* check for valid */
    for( c = sizeof(gifti_type_list) / sizeof(gifti_type_ele) - 1; c > 0; c-- )
        if( dtype == gifti_type_list[c].type ) return 1;

    if( whine || G.verb > 3 )
        fprintf(stderr,"** invalid datatype value %d\n", dtype);

    return 0;
}

/*----------------------------------------------------------------------
 *! check that nbyper is one of the values in gifti_type_list
*//*-------------------------------------------------------------------*/
int gifti_valid_nbyper(int nbyper, int whine)
{
    int c;

    /* check for valid */
    for( c = sizeof(gifti_type_list) / sizeof(gifti_type_ele) - 1; c > 0; c-- )
        if( nbyper == gifti_type_list[c].nbyper ) return 1;

    if( whine || G.verb > 3 )
        fprintf(stderr,"** invalid nbyper value %d\n", nbyper);

    return 0;
}

/*----------------------------------------------------------------------
 *! check that dimension values are consistent (and with datatype)
 *
 *      - num_dim is in range
 *      - each dims[c] is postive (c < num_dim)
 *      - nvals is product of dims
 *      - datatype is valid (required to check nbyper)
 *      - nbyper is correct
*//*-------------------------------------------------------------------*/
int gifti_valid_dims(const giiDataArray * da, int whine)
{
    long long vals = 1;
    int       c, nbyper;

    if( !da ) {
        if( G.verb > 2 ) fprintf(stderr,"** GVD: no giiDataArray\n");
        return 0;
    }

    if( ! gifti_valid_num_dim( da->num_dim, whine ) )
        return 0;

    for( c = 0; c < da->num_dim; c++ ) {
        if( da->dims[c] <= 0 ) {
            if( G.verb > 3 || whine )
                fprintf(stderr,"** invalid dims[%d] = %d\n", c, da->dims[c]);
            return 0;
        }

        vals *= da->dims[c];
    }

    /* verify computed vals and nbyper against stored ones */
    if( vals != da->nvals ) {
        if( G.verb > 3 ) {
            fprintf(stderr,"** nvals = %lld does not match %lld for dims[%d]: ",
                    da->nvals, vals, da->num_dim);
            gifti_disp_raw_data(da->dims, DT_INT32, da->num_dim, 1, stderr);
        }
        return 0;
    }

    gifti_datatype_sizes(da->datatype, &nbyper, NULL);
    if( nbyper != da->nbyper ) {
        fprintf(stderr,"** nbyper %d not correct for type %s\n",
                da->nbyper, gifti_datatype2str(da->datatype));
        return 0;
    }

    /* verify that num_dim is not too big, the most significant dimension
     * is not allowed to be 1
     * (requested by N Schmansky)                       11 Mar 2010 */
    if( da->num_dim > 1 && da->dims[da->num_dim-1] < 2 ) {
        fprintf(stderr,"** num_dim violation: num_dim = %d, yet dim[%d] = %d\n",
                       da->num_dim, da->num_dim-1, da->dims[da->num_dim-1]);
        return 0;
    }

    return 1;
}

/*----------------------------------------------------------------------
 *! set the DataArray attribute, based on the name=value string pair
 *
 *  return 0 on success
 *         1 on error
*//*-------------------------------------------------------------------*/
int gifti_str2attr_darray(giiDataArray * DA, const char *attr,
                                             const char *value)
{
    if( !DA || !attr || !value ) {
        if( G.verb > 0 )
            fprintf(stderr,"** G_S2A_D: bad params (%p,%p,%p)\n",
                    (void *)DA, (void *)attr, (void *)value);
        return 1;
    }

    if(G.verb > 3) fprintf(stderr,"++ setting DA attr '%s'='%s'\n",attr,value);

    if( !strcmp(attr, "Intent") )
        DA->intent = gifti_intent_from_string(value);
    else if( !strcmp(attr, "DataType") )
        DA->datatype = gifti_str2datatype(value);
    else if( !strcmp(attr, "ArrayIndexingOrder") )
        DA->ind_ord = gifti_str2ind_ord(value);
    else if( !strcmp(attr, "Dimensionality") ) DA->num_dim = atoi(value);
    else if( !strcmp(attr, "Dim0") )           DA->dims[0] = atoi(value);
    else if( !strcmp(attr, "Dim1") )           DA->dims[1] = atoi(value);
    else if( !strcmp(attr, "Dim2") )           DA->dims[2] = atoi(value);
    else if( !strcmp(attr, "Dim3") )           DA->dims[3] = atoi(value);
    else if( !strcmp(attr, "Dim4") )           DA->dims[4] = atoi(value);
    else if( !strcmp(attr, "Dim5") )           DA->dims[5] = atoi(value);
    else if( !strcmp(attr, "Encoding") )
        DA->encoding = gifti_str2encoding(value);
    else if( !strcmp(attr, "Endian") )
        DA->endian = gifti_str2endian(value);
    else if( !strcmp(attr, "ExternalFileName") )
        DA->ext_fname = gifti_strdup(value);
    else if( !strcmp(attr, "ExternalFileOffset") )
#if defined(_WIN32) && !defined(__MINGW32__) && !defined(__CYGWIN__)
        DA->ext_offset = atol(value);  /* There is no atoll defined in MS VC++ */
#else
        DA->ext_offset = atoll(value);  /* assumes C99 */
#endif
    else {
        if( G.verb > 1 )        /* might go into ex_atrs */
            fprintf(stderr,"** unknown giiDataArray attr, '%s'='%s'\n",
                    G_CHECK_NULL_STR(attr),G_CHECK_NULL_STR(value));
        return 1;
    }

    return 0;
}

/* return 0 (UNDEFINED) on failure */
static int str2list_index( char * list[], int max, const char *str )
{
    int index;
    if( !list || !str ) {
        if( G.verb > 0 ) fprintf(stderr,"** str2list: bad params (%p,%p)\n",
                                 (void *)list, (void *)str);
        return 0;  /* should be *_UNDEFINED */
    }

    for( index = max; index > 0; index-- )
        if( !strcmp(str, list[index]) ) return index;

    return 0;    /* failure */
}

/*----------------------------------------------------------------------
 *! return the index for a GIFTI_IND_ORD_* string
*//*-------------------------------------------------------------------*/
int gifti_str2ind_ord( const char * str )
{
    int rv = str2list_index(gifti_index_order_list,GIFTI_IND_ORD_MAX,str);
    if( rv <= GIFTI_IND_ORD_UNDEF && G.verb > 1 )
        fprintf(stderr,"** bad index order, '%s'\n", str);
    return rv;
}


/*----------------------------------------------------------------------
 *! return the index for a GIFTI_ENDODING_* string
*//*-------------------------------------------------------------------*/
int gifti_str2encoding( const char * str )
{
    int rv = str2list_index(gifti_encoding_list, GIFTI_ENCODING_MAX, str);
    if( rv <= GIFTI_ENCODING_UNDEF && G.verb > 1 )
        fprintf(stderr,"** bad data encoding, '%s'\n", str);
    return rv;
}

/*----------------------------------------------------------------------
 *! return the string at the given index of the given list
 *
 *  This function is meant to index into one of the gifti_*_list arrays,
 *  while being certain that the index is not out of range.
*//*-------------------------------------------------------------------*/
char * gifti_list_index2string(char * list[], int index)
{
    int lsize;    /* list size cannot be computed from the passed pointer */

    if     ( list == gifti_index_order_list )
        lsize = sizeof(gifti_index_order_list)/sizeof(char *);

    else if( list == gifti_encoding_list )
        lsize = sizeof(gifti_encoding_list)/sizeof(char *);

    else if( list == gifti_endian_list )
        lsize = sizeof(gifti_endian_list)/sizeof(char *);

    else {
        fprintf(stderr,"** GLI2S: invalid list\n");
        return "UNKNOWN LIST";
    }

    if( index < 0 || index >= lsize ) {
        if( G.verb > 0)
            fprintf(stderr,"** GLI2S: index %d out of range {0..%d}\n",
                index,lsize-1);
        return "INDEX OUT OF RANGE";
    }

    /* all that work, just for the expected indexing...  */

    return list[index];
}

/*----------------------------------------------------------------------
 *! return the NIFTI_TYPE_ value corresponding to the given string
*//*-------------------------------------------------------------------*/
int gifti_str2datatype(const char * str)
{
    int len = sizeof(gifti_type_list)/sizeof(gifti_type_ele);
    int c;

    for( c = len - 1; c > 0; c-- )
        if( !strcmp(str, gifti_type_list[c].name) )
            break;

    return gifti_type_list[c].type;
}


/*----------------------------------------------------------------------
 *! return the GIFTI_ENDIAN_ value corresponding to the given string
*//*-------------------------------------------------------------------*/
int gifti_str2endian( const char * str )
{
    int rv = str2list_index(gifti_endian_list, GIFTI_ENDIAN_MAX, str);
    if( rv <= GIFTI_ENCODING_UNDEF && G.verb > 1 )
        fprintf(stderr,"** bad endian, '%s'\n", G_CHECK_NULL_STR(str));
    return rv;
}


/*----------------------------------------------------------------------
 *! return the NIFTI_TYPE_ value string corresponding to the given type
*//*-------------------------------------------------------------------*/
char * gifti_datatype2str(int type)
{
    int len = sizeof(gifti_type_list)/sizeof(gifti_type_ele);
    int c;

    for( c = len - 1; c > 0; c-- )
        if( type == gifti_type_list[c].type)
            break;

    return gifti_type_list[c].name;
}


/*----------------------------------------------------------------------
 *! simply set the struct contents to empty
*//*-------------------------------------------------------------------*/
int gifti_clear_nvpairs(nvpairs * p)
{
    if( !p ) return 1;

    p->length = 0;
    p->name = NULL;
    p->value = NULL;

    return 0;
}

/*----------------------------------------------------------------------
 *! simply set the struct contents to empty
*//*-------------------------------------------------------------------*/
int gifti_clear_LabelTable(giiLabelTable * p)
{
    if( !p ) return 1;

    p->length = 0;
    p->key = NULL;
    p->label = NULL;
    p->rgba = NULL;

    return 0;
}

/*----------------------------------------------------------------------
 *! simply set the struct contents to empty
*//*-------------------------------------------------------------------*/
int gifti_clear_CoordSystem(giiCoordSystem * p)
{
    if( !p ) return 1;

    p->dataspace = NULL;
    p->xformspace = NULL;
    memset(p->xform,0,sizeof(p->xform));

    return 0;
}

/*----------------------------------------------------------------------
 *! add an empty CoordSystem struct to the DataArray
*//*-------------------------------------------------------------------*/
int gifti_add_empty_CS(giiDataArray * da)
{
    if( !da ) return 1;

    /* be safe, if anything looks bad, start clean */
    if(da->numCS <= 0 || !da->coordsys) { da->numCS = 0; da->coordsys = NULL; }

    if(G.verb > 3 )fprintf(stderr,"++ adding empty CS[%d]\n", da->numCS);

    /* realloc coordsys pointer array, and add an empty structure */
    da->coordsys = (giiCoordSystem **)realloc(da->coordsys,
                        (da->numCS+1) * sizeof(giiCoordSystem *));
    if( !da->coordsys ) {
        fprintf(stderr,"** AECS: failed to alloc %d CoordSys pointers\n",
                       da->numCS+1);
        da->numCS = 0;
        return 1;
    }

    da->coordsys[da->numCS] = (giiCoordSystem *)malloc(sizeof(giiCoordSystem));
    if( !da->coordsys[da->numCS] ) {
        fprintf(stderr,"** push_cstm: failed to alloc new CoordSystem\n");
        return 1;
    }

    gifti_clear_CoordSystem(da->coordsys[da->numCS]);

    da->numCS++;

    return 0;
}

/*----------------------------------------------------------------------
 *! add an empty DataArray struct to the gim->darray list
 *
 *  this both reallocates gim->darray and allocates gim->darray[new]
 *
 *  if num_to_add > 0: add that many elements
 *  if defaults      : init each element with default values
 *
 *  return 0 on success
 *         1 on error
*//*-------------------------------------------------------------------*/
int gifti_add_empty_darray(gifti_image * gim, int num_to_add)
{
    giiDataArray * dptr;
    int            c, ntot, nnew = num_to_add > 0 ? num_to_add : 1;

    if( !gim ) return 1;

    if(G.verb > 3)fprintf(stderr,"++ alloc darray[%d] (+%d)\n",gim->numDA,nnew);

    /* allocate for additional pointers */
    ntot = gim->numDA + nnew;
    gim->darray = (giiDataArray **)realloc(gim->darray,
                                           ntot*sizeof(giiDataArray *));

    if( !gim->darray ) {
        fprintf(stderr,"** failed realloc darray, len %d\n", ntot);
        gim->numDA = 0;
        return 1;
    }

    /* allocate the actual giiDataArray structs, inserted at the end */
    for( c = 0; c < nnew; c++ ) {
        dptr = (giiDataArray *)calloc(1, sizeof(giiDataArray));
        if( !dptr ) {
            fprintf(stderr,"** failed to alloc DA element #%d\n",gim->numDA);
            return 1;   /* leave allocated list as is */
        }
        gim->darray[gim->numDA] = dptr;
        gim->numDA++;

        /* clear any non-value attributes/elements */
        gifti_clear_DataArray(dptr);
    }

    return 0;
}

/*----------------------------------------------------------------------
 *! add the given name=value pair to the nvpairs struct
 *
 *  this allocates memory for the p->name and p->value arrays, along
 *  with duplicating the passed name/value strings
*//*-------------------------------------------------------------------*/
int gifti_add_to_nvpairs(nvpairs * p, const char * name, const char * value)
{
    int index;

    if( !p || !name || !value ) {
        if(G.verb > 1) fprintf(stderr,"** GATN: bad params(%p,%p,%p)\n",
                               (void *)p, (void *)name, (void *)value);
        return 1;
    }

    p->length++;
    p->name = (char **)realloc(p->name, p->length * sizeof(char *));
    p->value = (char **)realloc(p->value, p->length * sizeof(char *));

    if( !p->name || !p->value ) {
        fprintf(stderr,"** GATN: failed to realloc %d pointers\n",p->length);
        return 1;
    } else if ( G.verb > 3 )
        fprintf(stderr,"++ add_nvp [%d]: '%s', '%s'\n", p->length,
                name ? name : "NULL", value ? value : "NULL");

    index = p->length - 1;
    p->name[index] = gifti_strdup(name);
    p->value[index] = gifti_strdup(value);

    if( !p->name[index] || !p->value[index] ) {
        fprintf(stderr,"** GATN: failed to copy pair '%s'='%s'\n",name,value);
        return 1;
    }

    return 0;
}

/*----------------------------------------------------------------------
 *! display the contents of the nvpairs struct
*//*-------------------------------------------------------------------*/
int gifti_disp_nvpairs(const char * mesg, const nvpairs * p)
{
    int c;

    if( mesg ) { fputs(mesg, stderr); fputc(' ', stderr); }

    if( !p ){ fputs("disp: nvpairs = NULL\n", stderr); return 1; }

    fprintf(stderr,"nvpairs struct, len = %d :\n", p->length);

    for(c = 0; c < p->length; c++ )
        fprintf(stderr,"    nvpair: '%s' = '%s'\n",
                G_CHECK_NULL_STR(p->name[c]), G_CHECK_NULL_STR(p->value[c]));
    if( p->length > 0 ) fputc('\n', stderr);

    return 0;
}

/*----------------------------------------------------------------------
 *! display the contents of the LabelTable struct
*//*-------------------------------------------------------------------*/
int gifti_disp_LabelTable(const char * mesg, const giiLabelTable * p)
{
    float * rgba;
    int     c;

    if( mesg ) { fputs(mesg, stderr); fputc(' ', stderr); }

    if( !p ){ fputs("disp: giiLabelTable = NULL\n", stderr); return 1; }

    fprintf(stderr,"giiLabelTable struct, len = %d :\n", p->length);

    rgba = p->rgba;
    for(c = 0; c < p->length; c++ ) {
        fprintf(stderr,"    key %d, ", p->key[c]);
        if( rgba ) {
            fprintf(stderr,"rgba (%5.3f, %5.3f, %5.3f, %5.3f), ",
                    rgba[0], rgba[1], rgba[2], rgba[3]);
            rgba += 4;
        }
        fprintf(stderr,"label '%s'\n", G_CHECK_NULL_STR(p->label[c]));
    }

    if( p->length > 0 ) fputc('\n', stderr);

    return 0;
}

/*----------------------------------------------------------------------
 *! display the contents of the CoordSystem struct
*//*-------------------------------------------------------------------*/
int gifti_disp_CoordSystem(const char * mesg, const giiCoordSystem * p)
{
    int c1, c2;

    if( mesg ) { fputs(mesg, stderr); fputc(' ', stderr); }

    if( !p ){ fputs("disp: giiCoordSystem = NULL\n", stderr); return 1; }

    fprintf(stderr,"giiCoordSystem struct\n"
                   "    dataspace  = %s\n"
                   "    xformspace = %s\n",
                   G_CHECK_NULL_STR(p->dataspace),
                   G_CHECK_NULL_STR(p->xformspace));
    for( c1 = 0; c1 < 4; c1++ )
    {
        fprintf(stderr,"    xform[%d] :", c1);
        for( c2 = 0; c2 < 4; c2++ )
            fprintf(stderr,"  %f", p->xform[c1][c2]);
        fputc('\n', stderr);
    }

    return 0;
}

/*----------------------------------------------------------------------
 *! display the contents of the DataArray struct
 *
 *  if 'subs' is set, display the contents of the sub-structures
*//*-------------------------------------------------------------------*/
int gifti_disp_DataArray(const char * mesg, const giiDataArray * p, int subs)
{
    int c;
    fprintf(stderr,"--------------------------------------------------\n");

    if( mesg ) { fputs(mesg, stderr); fputc(' ', stderr); }

    if( !p ){ fputs("disp: giiDataArray = NULL\n", stderr); return 1; }

    fprintf(stderr,"giiDataArray struct\n"
                   "    intent   %4d = %s\n"
                   "    datatype   %2d = %s\n"
                   "    ind_ord    %2d = %s\n"
                   "    num_dim       = %d\n"
                   "    dims          = %d, %d, %d, %d, %d, %d\n"
                   "    encoding   %2d = %s\n"
                   "    endian     %2d = %s\n"
                   "    ext_fname     = %s\n"
                   "    ext_offset    = %lld\n"
                , p->intent,
                gifti_intent_to_string(p->intent),
                p->datatype, gifti_datatype2str(p->datatype),
                p->ind_ord,
                gifti_list_index2string(gifti_index_order_list, p->ind_ord),
                p->num_dim,
                p->dims[0], p->dims[1], p->dims[2],
                p->dims[3], p->dims[4], p->dims[5],
                p->encoding,
                gifti_list_index2string(gifti_encoding_list, p->encoding),
                p->endian,
                gifti_list_index2string(gifti_endian_list, p->endian),
                G_CHECK_NULL_STR(p->ext_fname), p->ext_offset
           );

    if( subs ) gifti_disp_nvpairs("darray->meta", &p->meta);
    if( subs ) for( c = 0; c < p->numCS; c++ )
                   gifti_disp_CoordSystem("darray->coordsys", p->coordsys[c]);

    fprintf(stderr,"    data       = %s\n"
                   "    nvals      = %u\n"
                   "    nbyper     = %d\n"
                   "    numCS      = %d\n",
                p->data ? "<set>" : "NULL", (unsigned)p->nvals,
                p->nbyper, p->numCS);

    if( subs ) gifti_disp_nvpairs("darray->ex_atrs", &p->ex_atrs);
    fprintf(stderr,"--------------------------------------------------\n");

    return 0;
}

/*----------------------------------------------------------------------
 *! display the contents of the gifti_image struct
 *
 *  if 'subs' is set, display the contents of the sub-structures, such
 *  as all of the DataArray elements
*//*-------------------------------------------------------------------*/
int gifti_disp_gifti_image(const char * mesg, const gifti_image * p, int subs)
{
    fprintf(stderr,"==================================================\n");

    if( mesg ) { fputs(mesg, stderr); fputc(' ', stderr); }

    if( !p ){ fputs("disp: gifti_image = NULL\n", stderr); return 1; }

    fprintf(stderr,"gifti_image struct\n"
                   "    version    = %s\n"
                   "    numDA      = %d\n",
                   G_CHECK_NULL_STR(p->version), p->numDA);

    if( subs ) {
        char buf[32];
        int  c;

        gifti_disp_nvpairs("gim->meta", &p->meta);
        gifti_disp_LabelTable("gim->labeltable", &p->labeltable);
        for( c = 0; c < p->numDA; c++ ) {
            sprintf(buf, "gim->darray[%d]", c);
            gifti_disp_DataArray(buf, p->darray[c], subs);
        }
    }

    fprintf(stderr,"gifti_image struct\n"
                   "    swapped    = %d\n"
                   "    compressed = %d\n", p->swapped, p->compressed);

    fprintf(stderr," -- darray totals: %lld MB\n",gifti_gim_DA_size(p,1));
    if( subs ) gifti_disp_nvpairs("gim->ex_atrs" , &p->ex_atrs);

    fprintf(stderr,"==================================================\n");

    return 0;
}

/*----------------------------------------------------------------------
 *! compute the number of bytes summed over all DataArray elements
 *
 *  if in_mb is set, return the (rounded) number of megabytes
 *  (i.e. 17 is 17 MB)
*//*-------------------------------------------------------------------*/
long long gifti_gim_DA_size(const gifti_image * p, int in_mb)
{
    long long bytes = 0;
    int       c;

    if( !p ) return -1;
    if( !p->darray || p->numDA <= 0 ) return 0;

    for( c = 0; c < p->numDA; c++ ) {
        if( ! p->darray[c]->data ) continue;  /* skip anything without data */
        if( p->darray[c]->nvals <= 0 || p->darray[c]->nbyper <= 0 ) {
            fprintf(stderr,"** have data[%d], but nvals = %lld, nbyper = %d\n",
                    c, p->darray[c]->nvals, p->darray[c]->nbyper);
            return 0;
        }
        bytes += p->darray[c]->nvals * p->darray[c]->nbyper;
    }

    if( bytes <= 0LL ) return 0;

    if( in_mb ) bytes = (bytes + (1<<19) ) >> 20;  /* round to nearest MB */

    return bytes;
}

/*----------------------------------------------------------------------
 *! given a datatype, return the corresponding bytes per value and swapsize
 *
 *  nbyper and swapsize are filled only if the pointers are set
*//*-------------------------------------------------------------------*/
int gifti_datatype_sizes(int datatype, int *nbyper, int *swapsize)
{
    int c;

    for( c = sizeof(gifti_type_list) / sizeof(gifti_type_ele) - 1; c > 0; c-- )
        if( datatype == gifti_type_list[c].type ) {
            if( nbyper ) *nbyper = gifti_type_list[c].nbyper;
            if( swapsize ) *swapsize = gifti_type_list[c].swapsize;
            return 0;
        }

    if( G.verb > 0 ) fprintf(stderr,"** GDS with bad datatype %d\n", datatype);
    if( nbyper ) *nbyper = 0;
    if( swapsize ) *swapsize = 0;

    return 1;
}

/*----------------------------------------------------------------------
 *! compute the total number of data values in a DataArray element
*//*-------------------------------------------------------------------*/
long long gifti_darray_nvals(const giiDataArray * da)
{
    long long ndim = 1;
    int       c;

    if(!da){ fprintf(stderr,"** GDND, no ptr\n"); return 0; }

    if( ! gifti_valid_num_dim(da->num_dim, 0) ) {
        fprintf(stderr,"** giiDataArray has illegal num_dim = %d\n",
                da->num_dim);
        return 0;
    }

    for( c = 0; c < da->num_dim; c++ ) ndim *= da->dims[c];

    if( ndim <= 0 ) {
        gifti_disp_DataArray("** bad Dim list in ", da, 0);
        return 0;
    }

    return ndim;
}

/*----------------------------------------------------------------------
 *! find giiDataArray element #index of the given intent
*//*-------------------------------------------------------------------*/
giiDataArray * gifti_find_DA(gifti_image * gim, int intent, int index)
{
    int c, nfound;

    if( !gim || !gifti_intent_is_valid(intent) || index < 0 ) {
        fprintf(stderr,"** find_DA: bad inputs (%p, %d, %d)\n",
                (void*)gim, intent, index);
        return NULL;
    }

    if ( !gim-> darray ) return NULL;

    for( c = 0, nfound = 0; c < gim->numDA; c++ )
        if( gim->darray[c] && gim->darray[c]->intent == intent ) {
            if( nfound == index )
                return gim->darray[c];  /* success */
            nfound++;   /* else, increment counter and keep looking */
        }

    return NULL;
}

/*----------------------------------------------------------------------
 *! return an allocated list of giiDataArray pointers of the given intent
 *
 *  'list' should be freed or taken
*//*-------------------------------------------------------------------*/
int gifti_find_DA_list(gifti_image * gim, int intent,
                       giiDataArray *** list, int * len)
{
    int c, nfound;

    if( !gim || !gifti_intent_is_valid(intent) || !list || !len ) {
        fprintf(stderr,"** find_DA: bad inputs (%p, %d, %p, %p)\n",
                (void *)gim, intent, (void *)list, (void *)len);
        return 1;
    }

    if ( !gim->darray ) return 1;

    /* create one big enough to hold everything */
    *len = gim->numDA;
    *list = (giiDataArray **)calloc(*len, sizeof(giiDataArray *));
    if( !*list ) {
        fprintf(stderr,"** find_DA_list: failed to alloc %d ptrs\n",*len);
        *len = 0;
        return 1;
    }

    for( c = 0, nfound = 0; c < gim->numDA; c++ )
        if( gim->darray[c] && gim->darray[c]->intent == intent )
            (*list)[nfound++] = gim->darray[c];

    /* if we didn't find any, nuke list, but do not return an error */
    if( nfound == 0 ) { free(*list); *list = NULL; *len = 0; return 0; }

    /* otherwise, reallocate a smaller list */
    if( nfound < *len ) {
        *len  = nfound;
        *list = (giiDataArray**)realloc(*list,*len*sizeof(giiDataArray*));
        if( !*list ) {
            fprintf(stderr,"** find_DA_list: failed realloc of %d ptrs\n",*len);
            *len = 0;
            return 1;
        }
    }

    return 0;
}

/*----------------------------------------------------------------------
 *! given metadata name, return the corresponding value (or NULL)
 *
 *  no allocation is done here
*//*-------------------------------------------------------------------*/
char * gifti_get_meta_value(const nvpairs * nvp, const char * name)
{
    int c;

    if( !nvp || !name ) {
        if( G.verb > 3 )
          fprintf(stderr,"** get_meta_value: NULL input (%p, %p)\n",
                (void*)nvp, name);
        return NULL;
    }

    if( G.verb > 5 )
        fprintf(stderr,"-- G_get_meta_value: looking for name = '%s'\n", name);

    if ( !nvp->name || !nvp->value || nvp->length <= 0 ) {
        if( G.verb > 3 )
            fprintf(stderr,"-- G_get_meta_value: no name/value array\n");
        return NULL;
    }

    for( c = 0; c < nvp->length; c++ )
        if( !strcmp(nvp->name[c], name) )
            break;  /* found */

    if( c >= nvp->length ) return NULL;

    if( G.verb > 3 )
        fprintf(stderr,"++ found meta '%s'='%s'\n",nvp->name[c],nvp->value[c]);

    return nvp->value[c];
}

/*----------------------------------------------------------------------
 *! return the number of 'rows' and 'columns' of a DataArray element
 *
 *  define rows to be the number of nodes, which should be the slowest
 *  changing element, depending on the index order (kuru kuru pa)
*//*-------------------------------------------------------------------*/
int gifti_DA_rows_cols(giiDataArray * da, long long * rows, long long * cols)
{
    *rows = da->dims[0];  /* init */
    *cols = 1;

    if( da->num_dim == 1 ) return 0;  /* use default */

    if( da->ind_ord == GIFTI_IND_ORD_ROW_MAJOR ) {
        /* treat Dim[0] as nodes (they change most slowly) */
        *rows = da->dims[0];
        *cols = (*rows) ? da->nvals / *rows : 1;    /* be safe */
    } else {
        if( ! gifti_valid_num_dim(da->num_dim, 1) ){
            fprintf(stderr,"** cannot assign DA_rows_cols");
            return 1;
        }

        *rows = da->dims[da->num_dim-1];  /* take highest index */
        *cols = (*rows > 0) ? da->nvals / *rows : 1;
    }

    return 0;
}

/*----------------------------------------------------------------------
 *! print the gifti library history string
*//*-------------------------------------------------------------------*/
void gifti_disp_lib_hist(void)
{
   int c, len = sizeof(gifti_history)/sizeof(char *);
   for( c = 0; c < len; c++ )
       fputs(gifti_history[c], stdout);
}

/*----------------------------------------------------------------------
 *! print the gifti library version string
*//*-------------------------------------------------------------------*/
void gifti_disp_lib_version(void)
{
    printf("%s, compiled %s\n", gifti_version, __DATE__);
}

/*----------------------------------------------------------------------
 *! return the gifti library version string
*//*-------------------------------------------------------------------*/
char * gifticlib_version(void)
{
    return gifti_version;
}

/*----------------------------------------------------------------------
 *! print the gifti DTD URL
*//*-------------------------------------------------------------------*/
void gifti_disp_dtd_url(void)
{
    printf("The GIFTI Document Type Definition (DTD) is at:\n    %s\n",
           GIFTI_XML_DTD_SOURCE);
}

/*----------------------------------------------------------------------
 *! display data in hexidecimal, on one line
 *
 *  if mesg is set, print the message first
 *  if fp is not set, print to stdout
*//*-------------------------------------------------------------------*/
int gifti_disp_hex_data(const char *mesg, const void *data, int len, FILE *fp)
{
    const char * dp = (const char *)data;
    FILE       * stream;
    int          c;

    stream = fp ? fp : stdout;

    if( !data || len < 1 ) return -1;

    if( mesg ) fputs(mesg, stream);

    for( c = 0; c < len; c++ )
        fprintf(stream, " %02x", dp[c]);

    return 0;
}

/*----------------------------------------------------------------------
 *! swap sets of 2-byte values
*//*-------------------------------------------------------------------*/
int gifti_swap_2bytes(void * data, long long nsets)
{
    char    * cp1 = (char *)data, * cp2;
    char      tval;
    long long c;

    for( c = 0; c < nsets; c++ ) {
        cp2 = cp1 + 1;
        tval = *cp1;  *cp1 = *cp2;  *cp2 = tval;
        cp1 += 2;
    }

    return 0;
}

/*----------------------------------------------------------------------
 *! swap sets of 4-byte values
*//*-------------------------------------------------------------------*/
int gifti_swap_4bytes(void * data, long long nsets)
{
    char    * cp0 = (char *)data, * cp1, * cp2;
    char      tval;
    long long c;

    for( c = 0; c < nsets; c++ ) {
        cp1 = cp0;
        cp2 = cp0 + 3;
        tval = *cp1;  *cp1 = *cp2;  *cp2 = tval;
        cp1++;  cp2--;
        tval = *cp1;  *cp1 = *cp2;  *cp2 = tval;
        cp0 += 4;
    }

    return 0;
}

/*----------------------------------------------------------------------
 *! swap sets of N-byte values
 *
 *  if N < 2         : just return
 *  if N = 2 or N = 4: call explicit function for that size (speed)
 *  else             : swap in a loop
*//*-------------------------------------------------------------------*/
int gifti_swap_Nbytes(void * data, long long nsets, int swapsize)
{
    char    * cp0, * cp1, * cp2;
    char      tval;
    long long c;
    int       offset;      /* swapsize - 1, for speed */

    if( ! data || nsets < 0 || swapsize < 0 ) {
        fprintf(stderr,"** swap_Nbytes: bad params (%p,%lld,%d)\n",
                (void *)data, nsets, swapsize);
        return 1;
    }

    if     ( swapsize  < 2 ) return 0;  /* nothing to do */
    else if( swapsize == 2 ) return gifti_swap_2bytes(data, nsets);
    else if( swapsize == 4 ) return gifti_swap_4bytes(data, nsets);

    /* peform a swap */
    cp0 = (char *)data;
    offset = swapsize-1;  /* just for speed */

    for( c = 0; c < nsets; c++ ) {
        cp1 = cp0;
        cp2 = cp0 + offset;
        while( cp2 > cp1 ) {
            tval = *cp1;  *cp1 = *cp2;  *cp2 = tval;
            cp1++;  cp2--;
        }
        cp0 += swapsize;
    }

    return 0;
}

/*----------------------------------------------------------------------
 *! return the current CPU endian: GIFTI_ENDIAN_BIG or _LITTLE
*//*-------------------------------------------------------------------*/
int gifti_get_this_endian(void)
{
   int    one = 1;
   char * cp = (char *)&one;

   if( *cp ) return GIFTI_ENDIAN_LITTLE;

   return GIFTI_ENDIAN_BIG;
}

/*----------------------------------------------------------------------
 *! if endian does not match that of this CPU, swap the data bytes
 *
 *  return whether bytes were swapped
*//*-------------------------------------------------------------------*/
int gifti_check_swap(void * data, int endian, long long nsets, int swapsize)
{
    if( !data || nsets < 0 || swapsize < 0 ) {
        fprintf(stderr,"** check_swap: bad params (%p,%lld, %d)\n",
                (void *)data, nsets, swapsize);
        return 0;
    } else if ( endian <= GIFTI_ENDIAN_UNDEF || endian > GIFTI_ENDIAN_MAX ) {
        fprintf(stderr, "** check_swap: invalid endian %d\n", endian);
        return 0;
    }

    /* if endian is the same as this one, just return */
    if( endian == gifti_get_this_endian() ) {
        if( G.verb > 2 )
            fprintf(stderr,"-- darray no swap needed : %lld sets of %d bytes\n",
                    nsets, swapsize);
        return 0;
    }

    if( G.verb > 2 )
        fprintf(stderr,"++ darray swap: %lld sets of %d bytes\n",
                nsets, swapsize);

    /* do the swap */
    (void)gifti_swap_Nbytes(data, nsets, swapsize);

    return 1;
}

/*---------------------------------------------------------------------*/
/*! Allocate and fill the data array with data read from the given
 *  external file.
 *
 *  return 0 on success
*//*-------------------------------------------------------------------*/
int gifti_read_extern_DA_data(giiDataArray * da)
{
    FILE      * fp;
    long long   nbytes, nread;

    if( !da || !da->ext_fname || !*da->ext_fname ) return 0;

    if(G.verb > 4) fprintf(stderr,"-- external read of '%s'\n",da->ext_fname);

    if( da->ext_offset < 0 ) {
        fprintf(stderr,"** want external DA data with bad offset %lld\n",
                da->ext_offset);
        return 1;
    } else if( da->data ) {
        fprintf(stderr,"** want external DA data but data already allocated\n");
        return 1;
    } else if ( !gifti_valid_dims(da, 1) ) {
        fprintf(stderr,"** cannot read external DA data with bad dims...\n");
        return 1;
    }

    /* allocate data */
    nbytes = da->nvals * da->nbyper;
    da->data = calloc(da->nvals, da->nbyper); /* zero in case of read failure */
    if( !da->data ) {
        fprintf(stderr,"** failed to alloc %lld bytes for external read\n",
                nbytes);
        return 1;
    }

    /* open file, jump to offset and read */
    fp = fopen(da->ext_fname, "r");
    if( !fp ) {
        fprintf(stderr,"** ext read: failed to open '%s'\n",da->ext_fname);
        return 1;
    }

    if( fseek(fp, da->ext_offset, SEEK_SET) ) {
        fprintf(stderr,"** ext read: failed to seek to %lld in '%s'\n",
                da->ext_offset, da->ext_fname);
        fclose(fp); return 1;
    }

    nread = fread(da->data, sizeof(char), nbytes, fp);
    fclose(fp);  /* close now in any case */

    if( nread != nbytes ) {
        fprintf(stderr,"** ext_read: read only %lld of %lld bytes from %s\n",
                nread, nbytes, da->ext_fname);
        return 1;
    }

    if(G.verb > 2)
        fprintf(stderr,"-- read %lld bytes from external '%s' @ %lld\n",
                       nbytes, da->ext_fname, da->ext_offset);

    return 0;
}

/*---------------------------------------------------------------------*/
/*! Write DA data to the given external file.
 *
 *  Note: the given ext_offset _must_ refer to the current end of file.
 *
 *  return 0 on success
*//*-------------------------------------------------------------------*/
int gifti_write_extern_DA_data(giiDataArray * da)
{
    FILE      * fp;
    long long   nbytes, nwritten, posn;

    if( !da || !da->ext_fname || !*da->ext_fname ) return 0;

    if(G.verb > 4) fprintf(stderr,"-- external write to '%s'\n",da->ext_fname);

    if( da->ext_offset < 0 ) {
        fprintf(stderr,"** bad offset for external DA data write, %lld\n",
                da->ext_offset);
        return 1;
    } else if( !da->data ) {
        fprintf(stderr,"** no data for external DA data write\n");
        return 1;
    } else if ( !gifti_valid_dims(da, 1) ) {
        fprintf(stderr,"** cannot write external DA data with bad dims...\n");
        return 1;
    }

    nbytes = da->nvals * da->nbyper;

    /* open file for append and verify that the file offset matches this one */
    fp = fopen(da->ext_fname, "a+");
    if( !fp ) {
        fprintf(stderr,"** ext write: failed to open '%s' for append\n",
                da->ext_fname);
        return 1;
    }

    /* we should be at the end of the file, which measn posn da->ext_offset */
    fseek(fp, 0, SEEK_END);  /* append should write to the end, but be sure */
    posn = ftell(fp);
    if( posn != da->ext_offset ) {
        fprintf(stderr,"** ext write: cur posn (%lld) not ext_offset (%lld)"
                       " in file %s\n", posn, da->ext_offset, da->ext_fname);
        fclose(fp); return 1;
    }

    nwritten = fwrite(da->data, sizeof(char), nbytes, fp);

    fclose(fp);         /* close now in any case */

    if( nwritten != nbytes ) {
        fprintf(stderr,"** ext_write: appended only %lld of %lld bytes to %s\n",
                nwritten, nbytes, da->ext_fname);
        return 1;
    }

    if(G.verb > 2)
        fprintf(stderr,"-- appended %lld bytes to external '%s' @ %lld\n",
                       nbytes, da->ext_fname, da->ext_offset);

    return 0;
}

/*---------------------------------------------------------------------*/
/*! Apply the file list as external files.
 *
 *  The files are assumed to be partitioned by DataArray entries.  So
 *  the list length must divide numDA evenly.
 *
 *  External files are not checked for her, as this is independent of any
 *  read or write operation.
 *
 *  return 0 on success
*//*-------------------------------------------------------------------*/
int gifti_set_extern_filelist(gifti_image * gim, int nfiles, char ** files)
{
    giiDataArray * da;
    long long      nbytes, offset;
    int            nper;
    int            daindex, findex, oindex;

    if(!gim || gim->numDA <= 0 || nfiles <= 0 || !files) {
        if(G.verb>1) fprintf(stderr,"-- set_extern_filelist: nothing to do\n");
        return 1;
    }

    nper = gim->numDA / nfiles;

    if(G.verb>4) fprintf(stderr,"-- set_extern_flist for %d files (nper=%d)\n",
                         nfiles, nper);

    if( nfiles * nper != gim->numDA ) { /* be sure division is integral */
        fprintf(stderr,"** Cannot evenly divide %d DataArrays by %d"
                       " external files\n", gim->numDA, nfiles);
        return 1;
    }

    daindex = 0;  /* DataArray index */
    for( findex = 0; findex < nfiles; findex++ ) {
        if( !files[findex] || !*files[findex] ) {
            fprintf(stderr,"** set_extern_flist: missing filename %d\n",findex);
            return 1;
        }

        /* note and check nbytes */
        nbytes = gim->darray[daindex]->nvals * gim->darray[daindex]->nbyper;
        if( nbytes <= 0 ) {
            fprintf(stderr,"** gifti_set_extern_filelist: bad nbytes\n");
            return 1;
        }

        /* within this file, offset will be multiples of nbytes */
        for( oindex=0, offset=0; oindex < nper; oindex++, offset += nbytes ) {
            da = gim->darray[daindex];

            if( nbytes != da->nvals * da->nbyper ) {
              fprintf(stderr,"** set_extern_flist: nbytes mismatch at DA[%d]\n"
                             "   (expected %lld, found %lld)\n",
                             daindex, nbytes, da->nvals * da->nbyper);
              return 1;
            }

            /* set encoding and external file fields */
            da->encoding   = GIFTI_ENCODING_EXTBIN;
            da->ext_fname  = gifti_strdup(files[findex]);
            da->ext_offset = offset;

            daindex++;  /* increment DataArray index every time */
        }
    }

    if(G.verb > 2)
        fprintf(stderr,"++ set extern file list, %d files, %d DAs per file",
                nfiles, nper);

    return 0;
}

/*---------------------------------------------------------------------*/
/*! Given a NIFTI_INTENT string, such as "NIFTI_INTENT_NODE_INDEX",
 *  return the corresponding integral intent code.  The intent code is
 *  the macro value defined in nifti1.h.
 *
 *  return 0 on failure (NIFTI_INTENT_NONE)
*//*-------------------------------------------------------------------*/
int gifti_intent_from_string( const char * name )
{
    int tablen = sizeof(gifti_intent_list)/sizeof(gifti_intent_ele);
    int c;

    if( !name ) return 0;

    for( c = tablen-1; c > 0; c-- )
        if( !strcmp(name, gifti_intent_list[c].name) )
            break;

    return gifti_intent_list[c].code;
}


/*---------------------------------------------------------------------*/
/*! Given a NIFTI_TYPE value, such as NIFTI_TYPE_INT16, return the
 *  corresponding macro label as a string.  The dtype code is the
 *  macro value defined in nifti1.h.
*//*-------------------------------------------------------------------*/
char * gifti_intent_to_string( int code )
{
    int tablen = sizeof(gifti_intent_list)/sizeof(gifti_intent_ele);
    int c;

    for( c = tablen-1; c > 0; c-- )
        if( gifti_intent_list[c].code == code )
            break;

    return gifti_intent_list[c].name;
}


/*---------------------------------------------------------------------*/
/*! Return whether the given code is a valid NIFTI_INTENT code.
*//*-------------------------------------------------------------------*/
int gifti_intent_is_valid( int code )
{
    int tablen = sizeof(gifti_intent_list)/sizeof(gifti_intent_ele);
    int c;

    for( c = tablen-1; c > 0; c-- )
        if( gifti_intent_list[c].code == code )
            break;

    return( c >= 0 );
}


/*---------------------------------------------------------------------*/
/*! duplicate the given string
*//*-------------------------------------------------------------------*/
char * gifti_strdup( const char * src )
{
    char * newstr;
    int    len;

    if( !src ) return NULL;  /* easy case */

    len = strlen(src) + 1;

    newstr = (char *)malloc(len * sizeof(char));
    if( !newstr ) {
        fprintf(stderr,"** failed gifti_strdup, len = %d\n", len);
        return NULL;
    }

    strcpy(newstr, src);

    return newstr;
}

/*---------------------------------------------------------------------*/
/*! duplicate the given giiDataArray struct, optionally including data
 *
 *  Allocate for a new struct, and fill it so that the contents are
 *  identical (sans pointers) to orig.  Sub-structure arrays will need
 *  to be allocated, also.
 *
 *  If get_data is not set, gnew->data will be left as NULL.
 *
 *  return the address of the newly allocated structure
*//*-------------------------------------------------------------------*/
giiDataArray * gifti_copy_DataArray(const giiDataArray * orig, int get_data)
{
    giiDataArray * gnew;
    int            c;

    if( ! orig ){ fprintf(stderr,"** copy_DA: input is NULL\n"); return NULL; }

    if(G.verb > 5) fprintf(stderr,"++ copying giiDataArray...\n");

    gnew = (giiDataArray *)calloc(1, sizeof(giiDataArray));
    if(!gnew){fprintf(stderr,"** copy_DA, failed to alloc DA\n"); return NULL;}

    /* cheat - start by copying the entire struct contents */
    *gnew = *orig;

    /* copy any pointer data or structures */
    gnew->ext_fname = gifti_strdup(orig->ext_fname);
    gifti_copy_nvpairs(&gnew->meta, &orig->meta);
    if( orig->numCS > 0 && orig->coordsys ) {
        gnew->coordsys = (giiCoordSystem **)malloc(gnew->numCS *
                                sizeof(giiCoordSystem *));
        if(!gnew->coordsys) {
            fprintf(stderr,"** copy_DA: failed to alloc %d CS pointers\n",
                    gnew->numCS);
            gnew->numCS = 0;
        } else
            for( c = 0; c < gnew->numCS; c++ )
                gnew->coordsys[c] = gifti_copy_CoordSystem(orig->coordsys[c]);
    }

    /* maybe the needy user wants data, too */
    if(orig->data && get_data) {
        if(G.verb > 5) fprintf(stderr,"++ copy_DA, adding data\n");
        gnew->data = malloc(gnew->nvals * gnew->nbyper);
        if(!gnew->data)       /* continue? */
            fprintf(stderr,"** copy DA, failed to alloc %lld bytes for data\n",
                    gnew->nvals * gnew->nbyper);
        memcpy(gnew->data, orig->data, gnew->nvals * gnew->nbyper);
    } else
        gnew->data = NULL;

    /* last and certainly least, ex_atrs */
    gifti_copy_nvpairs(&gnew->ex_atrs, &orig->ex_atrs);

    return gnew;
}

/*---------------------------------------------------------------------*/
/*! dupliate the giiCoordSystem struct (passing NULL is okay)
*//*-------------------------------------------------------------------*/
giiCoordSystem * gifti_copy_CoordSystem(const giiCoordSystem * src)
{
    giiCoordSystem * csnew;
    int              r, c;

    if( !src ) return NULL;     /* this may be okay */

    if( G.verb > 6 ) fprintf(stderr,"++ copy_CS\n");

    csnew = (giiCoordSystem *)malloc(sizeof(giiCoordSystem));
    if( !csnew ){ fprintf(stderr,"** copy_CS: failed alloc\n"); return NULL; }

    csnew->dataspace  = gifti_strdup(src->dataspace);
    csnew->xformspace = gifti_strdup(src->xformspace);

    for( r = 0; r < 4; r++ )
        for( c = 0; c < 4; c++ )
            csnew->xform[r][c] = src->xform[r][c];

    return csnew;
}

/*---------------------------------------------------------------------*/
/*! dupliate the contents of the giiLabelTable struct
*//*-------------------------------------------------------------------*/
int gifti_copy_LabelTable(giiLabelTable * dest, const giiLabelTable * src)
{
    int c;

    if( !src || !dest ) {
        fprintf(stderr,"** copy_LabelTable: bad params (%p,%p)\n",
                (void *)src, (void *)dest);
        return 1;
    }

    if( G.verb > 6 ) fprintf(stderr,"++ copy_LT\n");

    /* quick case: empty table */
    if( src->length <= 0 ) return gifti_clear_LabelTable(dest);

    /* otherwise, allocate and copy lists */
    dest->length = src->length;

    dest->key = (int *)malloc(dest->length * sizeof(int));
    dest->label = (char **)malloc(dest->length * sizeof(char *));
    if( src->rgba )
        dest->rgba = (float *)malloc(dest->length * 4 * sizeof(float));

    /* check for failure */
    if( !dest->key || !dest->label || (src->rgba && !dest->rgba) ) {
        fprintf(stderr,"** failed to dup label arrays of length %d\n",
                dest->length);
        gifti_free_LabelTable(dest);
        return 1;
    }

    /* copy any rgba list */
    if( dest->rgba )
        memcpy(dest->rgba, src->rgba, dest->length * 4 * sizeof(float));

    /* copy indices */
    for( c = 0; c < dest->length; c++ )
        dest->key[c] = src->key[c];

    /* copy labels */
    for( c = 0; c < dest->length; c++ )
        dest->label[c] = gifti_strdup(src->label[c]);

    return 0;
}

/*---------------------------------------------------------------------*/
/*! dupliate the contents of one nvpairs structure into an empty one
 *
 *  return 0 on success
*//*-------------------------------------------------------------------*/
int gifti_copy_nvpairs(nvpairs * dest, const nvpairs * src)
{
    if( !dest || !src ){
        fprintf(stderr,"** copy_NVP, bad params (%p,%p)\n",
                (void*)dest,(void*)src);
        return 1;
    }

    if( G.verb > 6 ) fprintf(stderr,"++ copy_nvp, length %d\n", src->length);

    /* check for a simple case */
    if( src->length <= 0 || !src->name || !src->value ) {
        dest->length = 0;
        dest->name = dest->value = NULL;
        return 0;
    }

    /* else, copy the lists */
    dest->length = src->length;
    dest->name   = gifti_copy_char_list(src->name, src->length);
    dest->value  = gifti_copy_char_list(src->value, src->length);

    return 0;
}

/*---------------------------------------------------------------------*/
/*! dupliate the list of strings
*//*-------------------------------------------------------------------*/
char ** gifti_copy_char_list(char ** list, int len)
{
    char ** newlist = NULL;
    int     c;

    if( !list || len <= 0 ) return NULL;

    newlist = (char **)malloc(len * sizeof(char *));
    if( !newlist ) {
        fprintf(stderr,"** copy_char_list fails for %d pointers\n", len);
        return NULL;
    }

    for( c = 0; c < len; c++)                   /* big finish */
        newlist[c] = gifti_strdup(list[c]);

    return newlist;
}

/*---------------------------------------------------------------------*/
/*! copy any GIFTI MetaData named 'name' from dest to src (replace old)
 *
 *  return 0 on success, 1 on failure to find, -1 on error
*//*-------------------------------------------------------------------*/
int gifti_copy_gifti_meta(gifti_image * dest, gifti_image * src,
                          const char * name)
{
    char * value;

    if( !dest || !src || !name ) {
        if( G.verb > 0 )
            fprintf(stderr,"** copy_gifti_meta: bad params(%p,%p,%p)\n",
                    (void *)dest, (void *)src, name);
        return -1;
    }

    value = gifti_get_meta_value(&src->meta, name);
    if( !value ) {
        if( G.verb > 4 )
            fprintf(stderr,"-- GCGM: did not find meta name '%s'\n", name);
        return 1;
    }

    return gifti_add_to_meta(&dest->meta, name, value, 1);
}

/*---------------------------------------------------------------------*/
/*! copy any DataArray MetaData named 'name' from dest to src (replace old)
 *  (apply to list of DAs, or to all)
 *
 *  return 0 on success, 1 on failure to find, -1 on error
*//*-------------------------------------------------------------------*/
int gifti_copy_DA_meta_many(gifti_image * dest, gifti_image * src,
                            const char * name, const int * dalist, int len)
{
    int c, index, use_list, numDA, rv = 0;

    if( !dest || !dest->darray || !src || !src->darray || !name ) {
        if( G.verb > 1 ) fprintf(stderr,"** GCDAMM: bad params\n");
        return -1;
    }

    /* if they are not equal, it is probably a user mistake to be here */
    if( dest->numDA != src->numDA ) {
        if(G.verb>0) fprintf(stderr,"-- cannot copy DA meta, numDA %d != %d\n",
                             src->numDA, dest->numDA);
        return -1;
    }

    /* the empty case is probably not an error */
    if( dest->numDA <= 0 || src->numDA <= 0 ) {
        if( G.verb > 4 ) fprintf(stderr,"-- GCDAMM: numDA %d, %d\n",
                                 src->numDA, dest->numDA);
        return 0;
    }

    /* decide whether to use dalist or all DAs */
    use_list = gifti_valid_int_list(dalist, len, 0, src->numDA-1, G.verb);

    if( use_list && G.verb > 2 )
        fprintf(stderr,"++ copy_DA_meta_many, %s (list length %d)\n",
                use_list ? "DA in list" : "all DAs", len);

    /* finally, get to work */
    numDA = use_list ? len : src->numDA;
    for( c = 0; c < numDA; c++ ) {
        index = use_list ? dalist[c] : c;  /* choose appropriate DA index */

        /* note any failures */
        rv |= gifti_copy_DA_meta(dest->darray[index], src->darray[index], name);
    }

    return rv;
}

/*---------------------------------------------------------------------*/
/*! copy any DataArray MetaData named 'name' from dest to src (replace old)
 *
 *  return 0 on success, 1 on failure to find, -1 on error
*//*-------------------------------------------------------------------*/
int gifti_copy_DA_meta(giiDataArray *dest, giiDataArray *src, const char *name)
{
    char * value;

    if( !dest || !src || !name ) {
        if( G.verb > 0 )
            fprintf(stderr,"** copy_DA_meta: bad params(%p,%p,%p)\n",
                    (void *)dest, (void *)src, name);
        return -1;
    }

    value = gifti_get_meta_value(&src->meta, name);
    if( !value ) {
        if( G.verb > 4 )
            fprintf(stderr,"-- GCDAM: did not find meta name '%s'\n", name);
        return 1;
    }

    return gifti_add_to_meta(&dest->meta, name, value, 1);
}

/*---------------------------------------------------------------------*/
/*! copy ALL DataArray MetaData from dest to src (replace old)
 *
 *  return 0 on success, 1 on failure to find, -1 on error
*//*-------------------------------------------------------------------*/
int gifti_copy_all_DA_meta(giiDataArray *dest, giiDataArray *src)
{
    int c, rv = 0;

    if( !dest || !src ) {
        if( G.verb > 0 )
            fprintf(stderr,"** copy_all_DA_meta: bad params(%p,%p)\n",
                    (void *)dest, (void *)src);
        return -1;
    }

    for( c = 0; c < src->meta.length; c++ )
        rv |= gifti_copy_DA_meta(dest, src, src->meta.name[c]);

    return rv;
}

/*---------------------------------------------------------------------*/
/*! find any differences between the two images
 *
 *  verb  0-3 = quiet, state diff, state per DA, state all diffs
 *
 *  return 0 if they are the same, 1 if they differ
*//*-------------------------------------------------------------------*/
int gifti_compare_gifti_images(const gifti_image * g1, const gifti_image * g2,
                               int comp_data, int verb)
{
    int diffs = 0, data_diffs = 0, gdiffs = 0, c, rv, numDA;
    int lverb = verb;           /* possibly override passed 'verb' */

    if( G.verb > lverb ) lverb = G.verb;

    if( !g1 || !g2 ) {
        if( !g1 && !g2 ) return 0;  /* both NULL means equal */

        if(lverb) printf("-- gifti_images differ (exactly one is NULL)\n");
        return 1;
    }

    /* check main structs */
    if( gifti_compare_gims_only(g1, g2, lverb) ) {
        if( lverb > 0 ) printf("++ gifti_images differ\n");
        if( lverb < 2 ) return 1;        /* all we need to know */
        gdiffs++;
    }

    /* get min numDA, just to be safe */
    numDA = g1->numDA < g2->numDA ? g1->numDA : g2->numDA;
    for( c = 0; c < numDA; c++ ) {
        rv = gifti_compare_DA_pair(g1->darray[c],g2->darray[c],comp_data,lverb);
        if( rv ) {
            diffs++;
            if( rv & 2 ) data_diffs++;
            if( lverb < 2 ) break;
            printf("++ DataArray[%d] - difference (data %s)\n",
                   c, ! comp_data  ? "untested" :
                        data_diffs ? "differs"  : "identical");
        }
    }

    if( diffs && lverb > 0 )
        printf("-- differences found in %d of %d DAs\n", diffs, numDA);

    /* state data diffs separately */
    if( lverb > 2 && comp_data ) {
        if( ! data_diffs ) printf("-- no data differences found\n");
        else printf("-- data differences found in %d of %d DAs\n",
                    data_diffs, numDA);
    }

    if( diffs || gdiffs ) return 1;
    return 0;
}

/*---------------------------------------------------------------------*/
/*! find any differences between the two sets of image data
 *
 *  verb  0-2+ = quiet, state diff, state per DA
 *
 *  return 0 if they are the same, 1 if they differ
*//*-------------------------------------------------------------------*/
int gifti_compare_gifti_data(const gifti_image * g1, const gifti_image * g2,
                             int verb)
{
    int lverb = verb, c, diffs = 0, numDA;

    if( G.verb > lverb ) lverb = G.verb;

    if( !g1 || !g2 ) {
        if( !g1 && !g2 ) return 0;  /* both NULL means equal */
        if(lverb) printf("-- gim data difference (exactly one gim is NULL)\n");
        return 1;
    }

    /* if numDA does not match, they differ */
    if( g1->numDA != g2->numDA ) {
        if( lverb > 0 )
            printf("-- gim data differs: numDA differs, %d vs. %d\n",
                   g1->numDA, g2->numDA);
        if( lverb < 2 ) return 1;
    }

    /* even if they differ, we may want to continue, so use minimum */
    numDA = g1->numDA < g2->numDA ? g1->numDA : g2->numDA;
    for( c = 0; c < numDA; c++ ) {
        if( gifti_compare_DA_data(g1->darray[c],g2->darray[c],lverb) ) {
            diffs++;
            if( lverb > 0 ) printf("++ data difference at DataArray[%d]\n", c);
            if( lverb < 2 ) return 1;
        }
    }

    if( diffs ) {  /* verb must be 2, so print */
        printf("-- found data diffs in %d DataArrays\n", diffs);
        return 1;
    }

    if( G.verb > 1 ) fprintf(stderr,"-- no data diffs found\n");

    return 0;
}

int gifti_compare_DA_data(const giiDataArray * d1, const giiDataArray * d2,
                          int verb)
{
    long long nbytes, offset;

    if( !d1 || !d2 ) {
        if( !d1 && !d2 ) return 0;  /* both NULL means equal */
        if(verb>1) printf("-- DA data difference (exactly one DA is NULL)\n");
        return 1;
    }

    if( ! gifti_valid_dims(d1,verb>1) || ! gifti_valid_dims(d2,verb>1) ) {
        if(verb>1) printf("-- DA data diff: dims are not valid\n");
        return 1;
    }

    nbytes = d1->nvals * d1->nbyper;
    if( nbytes != (d2->nvals * d2->nbyper) ) {
        if(verb>1) printf("-- DA data diff: nbytes differs, %lld vs. %lld\n",
                          nbytes, d2->nvals * d2->nbyper);
        return 1;
    }

    /* okay, let's test the data */
    offset = gifti_compare_raw_data(d1->data,d2->data,nbytes);

    if ( offset >= 0 ) {  /* difference in data */
        if(verb > 1) printf("-- diff in DA data at posn %lld\n",
                            offset/d1->nbyper);
        return 1;
    }

    return 0;
}

/* compare everything but darray
 * (for diffs, only print if verb > 1)
 */
int gifti_compare_gims_only(const gifti_image * g1, const gifti_image * g2,
                            int verb)
{
    int diffs = 0;
    int lverb = verb;           /* possibly override passed 'verb' */

    if( G.verb > lverb ) lverb = G.verb;

    if( !g1 || !g2 ) {
        if( !g1 && !g2 ) return 0;   /* equal */
        if( lverb > 1 )
            printf("-- comp gifti ims: have NULL %p, %p\n",(void*)g1,(void*)g2);
        return 1;   /* not equal */
    }

    if( g1->numDA != g2->numDA ) {
        diffs++;
        if( lverb > 1 )
            fprintf(stderr,"-- diff in GIFTI numDA: %d vs %d\n",
                           g1->numDA, g2->numDA);
        if( lverb <= 1 ) return 1;
    }

    if( !g1->version || !g2->version ) {  /* handle at least one NULL */
        if( g1->version || g2->version ) {
            diffs++;
            if( lverb > 1 )
                fprintf(stderr,"-- diff in GIFTI version: one is NULL\n");
            if( lverb <= 1 ) return 1;
        }
        /* else both NULL, which means equal */
    } else if ( strcmp(g1->version, g2->version) ) {
        diffs++;
        if( lverb > 1 )
            fprintf(stderr,"-- diff in GIFTI version: %s vs. %s\n",
                    g1->version, g2->version);
        if( lverb <= 1 ) return 1;
    }

    if( gifti_compare_labeltable(&g1->labeltable, &g2->labeltable, verb) ) {
        diffs++;
        if( lverb > 1 ) printf("-- diff in gifti labeltable\n");
        if( lverb <= 1 ) return 1;
    }

    if( gifti_compare_nvpairs(&g1->meta, &g2->meta, verb) ) {
        diffs++;
        if( lverb > 1 ) printf("-- diff in gifti meta\n");
        if( lverb <= 1 ) return 1;
    }


    if( g1->swapped != g2->swapped ) {
        diffs++;
        if( lverb > 1 )
            fprintf(stderr,"-- difference in GIM->swapped: %d vs %d\n",
                           g1->swapped, g2->swapped);
        if( lverb <= 1 ) return 1;
    }

    if( g1->compressed != g2->compressed ) {
        diffs++;
        if( lverb > 1 )
            fprintf(stderr,"-- difference in GIM->compressed: %d vs %d\n",
                           g1->compressed, g2->compressed);
        if( lverb <= 1 ) return 1;
    }

    if( gifti_compare_nvpairs(&g1->ex_atrs, &g2->ex_atrs, verb) ) {
        diffs++;
        if( lverb > 1 ) printf("-- diff in gifti ex_atrs\n");
        if( lverb <= 1 ) return 1;
    }

    return diffs;
}

/*! return 0 if equal, 1 if diffs, 2 if data diffs (and 3 if both diffs) */
int gifti_compare_DA_pair(const giiDataArray * d1, const giiDataArray * d2,
                          int comp_data, int verb)
{
    long long offset;
    int       c, top, diffs = 0, data_diffs = 0;
    int       lverb = verb;           /* possibly override passed 'verb' */

    if( G.verb > lverb ) lverb = G.verb;

    if( !d1 || !d2 ) {
        if( !d1 && !d2 ) return 0;   /* equal */
        if(lverb>2)
            printf("-- comp DA: have NULL: %p, %p\n", (void*)d1,(void*)d2);
        return 3;   /* not equal */
    }

    if( d1->intent != d2->intent ) {
        diffs = 1;
        if( lverb > 1 )
            printf("-- diff in DA intent: %d (%s) vs. %d (%s)\n",
                   d1->intent, gifti_intent_to_string(d1->intent),
                   d2->intent, gifti_intent_to_string(d2->intent));
        if( lverb < 3 ) return 1;
    }

    if( d1->datatype != d2->datatype ) {
        diffs = 1;
        if( lverb > 1 )
            printf("-- diff in DA datatype: %d (%s) vs. %d (%s)\n",
                   d1->datatype, gifti_datatype2str(d1->datatype),
                   d2->datatype, gifti_datatype2str(d2->datatype));
        if( lverb < 3 ) return 1;
    }

    if( d1->ind_ord != d2->ind_ord ) {
        diffs = 1;
        if( lverb > 1 )
            printf("-- diff in DA ind_ord: %d (%s) vs. %d (%s)\n",
               d1->ind_ord,
               gifti_list_index2string(gifti_index_order_list, d1->ind_ord),
               d2->ind_ord,
               gifti_list_index2string(gifti_index_order_list, d2->ind_ord));
        if( lverb < 3 ) return 1;
    }

    if( d1->num_dim != d2->num_dim ) {
        diffs = 1;
        data_diffs = 1;
        if( lverb > 1 )
            printf("-- diff in DA num_dim: %d vs. %d\n",
                   d1->num_dim, d2->num_dim );
        if( lverb < 3 ) return 3;
    }

    /* get minimum num_dim */
    top = d1->num_dim < d2->num_dim ? d1->num_dim : d2->num_dim;
    for( c = 0; c < top; c++ ) if( d1->dims[c] != d2->dims[c] ) break;
    if( c < top ) {
        diffs = 1;
        data_diffs = 1;
        if( lverb > 1 ) {
            printf("-- diff in DA dims (length %d)\n   ", top);
            gifti_disp_raw_data(d1->dims, NIFTI_TYPE_INT32, top, 0, stdout);
            printf("  vs  ");
            gifti_disp_raw_data(d2->dims, NIFTI_TYPE_INT32, top, 1, stdout);
        }
        if( lverb < 3 ) return 3;
    }

    if( d1->encoding != d2->encoding ) {
        diffs = 1;
        if( lverb > 1 )
            printf("-- diff in DA encoding: %d (%s) vs. %d (%s)\n",
               d1->encoding,
               gifti_list_index2string(gifti_encoding_list, d1->encoding),
               d2->encoding,
               gifti_list_index2string(gifti_encoding_list, d2->encoding));
        if( lverb < 3 ) return 1;
    }

    if( d1->endian != d2->endian ) {
        diffs = 1;
        if( lverb > 1 )
            printf("-- diff in DA endian: %d (%s) vs. %d (%s)\n",
               d1->endian,
               gifti_list_index2string(gifti_endian_list, d1->endian),
               d2->endian,
               gifti_list_index2string(gifti_endian_list, d2->endian));
        if( lverb < 3 ) return 1;
    }

    if( d1->ext_fname || d2->ext_fname ) {
        if( ! d1->ext_fname || !d2->ext_fname ||
              strcmp(d1->ext_fname, d2->ext_fname) ) {
            diffs = 1;
            if( lverb > 1 )
                printf("-- diff in DA ext_fname: %s vs. %s\n",
                   G_CHECK_NULL_STR(d1->ext_fname),
                   G_CHECK_NULL_STR(d2->ext_fname));
            if( lverb < 3 ) return 1;
        }
    }

    if( d1->ext_offset != d2->ext_offset ) {
        diffs = 1;
        if( lverb > 1 )
            printf("-- diff in DA ext_offset: %lld vs. %lld\n",
               d1->ext_offset, d2->ext_offset);
        if( lverb < 3 ) return 1;
    }

    if( gifti_compare_nvpairs(&d1->meta, &d2->meta, verb) ) {
        diffs = 1;
        if( lverb > 1 ) printf("-- diff in DA meta\n");
        if( lverb < 3 ) return 1;
    }

    if( d1->numCS != d2->numCS ) {
        diffs = 1;
        if( lverb > 1 ) printf("-- diff in DA numCS\n");
        if( lverb < 3 ) return 1;
    }

    /* compare each of the CoordSystem structs */
    top = d1->numCS < d2->numCS ? d1->numCS : d2->numCS;
    for( c = 0; c < top; c++ )
        if( gifti_compare_coordsys(d1->coordsys[c], d2->coordsys[c],1,verb) ) {
            diffs = 1;
            if( lverb > 1 ) printf("-- diff in DA coordsys[%d]\n", c);
            if( lverb < 3 ) return 1;
        }

    if( d1->nvals != d2->nvals ) {
        diffs = 1;
        data_diffs = 1;
        if( lverb > 1 )
            printf("-- diff in DA nvals: %lld vs. %lld\n",
               d1->nvals, d2->nvals);
        if( lverb < 3 ) return 3;
    }

    if( d1->nbyper != d2->nbyper ) {
        diffs = 1;
        data_diffs = 1;
        if( lverb > 1 )
            printf("-- diff in DA nbyper: %d vs. %d\n", d1->nbyper, d2->nbyper);
        if( lverb < 3 ) return 3;
    }

    if( gifti_compare_nvpairs(&d1->ex_atrs, &d2->ex_atrs, verb) ) {
        diffs = 1;
        if( lverb > 1 ) printf("-- diff in DA ex_atrs\n");
        if( lverb < 3 ) return 1;
    }

    /* check data last, and only if no data diffs and dims are valid */
    /* (set the 2^1 bit for a data diff)                             */
    if( comp_data && !data_diffs && gifti_valid_dims(d1, 0) ) {
        offset = gifti_compare_raw_data(d1->data,d2->data,d1->nvals*d1->nbyper);
        if ( offset >= 0 ) {
            diffs |= 2;
            if(lverb>1) printf("-- diff in DA data at position %lld\n",
                               offset/d1->nbyper);
            if(lverb<3) return 3;
        }
    }

    return diffs;
}

/*---------------------------------------------------------------------*/
/*! return 1 if gifti_images are approximately equal (from a data standpoint)
 *           note: return value is opposite from gifti_compare_gifti_images
 *
 *  verb  0-3 = quiet, state diff, state per DA, state all diffs
 *
 *  compare information close to the data:
 *      - numDA
 *      - labeltable
 *      - darray elements (pairwise)
 *  ignore version, meta, swapped, encoding, endian, ext_*, ex_atrs, etc.
 *
 *  return 1 if they are approximately the same, 0 if otherwise
*//*-------------------------------------------------------------------*/
int gifti_approx_gifti_images(const gifti_image * g1, const gifti_image * g2,
                              int comp_data, int verb)
{
    int diffs = 0, c, numDA;
    int lverb = verb;           /* possibly override passed 'verb' */

    if( G.verb > lverb ) lverb = G.verb;

    if( !g1 || !g2 ) {
        if( !g1 && !g2 ) return 1;  /* both NULL means equal */

        if(lverb) printf("-- gifti_images not approx (exactly one is NULL)\n");
        return 0;
    }

    if( g1->numDA != g2->numDA ) {
        if( lverb ) printf("-- gifti_images differ in numDA\n");
        if( lverb < 2 ) return 0;
        diffs++;
    }

    if( ! gifti_approx_labeltables(&g1->labeltable, &g2->labeltable, lverb) ) {
        if( lverb ) printf("-- gifti labeltables not approx. equal\n");
        if( lverb < 2 ) return 0;
        diffs++;
    }

    /* get min numDA, just to be safe */
    numDA = g1->numDA < g2->numDA ? g1->numDA : g2->numDA;
    for( c = 0; c < numDA; c++ ) {
        if( !gifti_approx_DA_pair(g1->darray[c],g2->darray[c],comp_data,lverb)){
            diffs++;
            if(lverb) printf("++ DataArrays[%d] - not approximately equal\n",c);
            if( lverb < 2 ) break;
        }
    }

    if( diffs && lverb > 0 ) printf("-- GIFTI: approx diffs found\n");

    return ! diffs;
}

/* ------------------------------------------------------------------------- */
/*! return 1 if DAs are approximately equal (opposite of compare functions)
 *
 * test for difference in:
 *
 *      transformation matrices : 16 values each (approx)
 *      coordinates             : 3 values per node (approx)
 *      triangles               : exact (starting node can be any of the 3)
 *      data                    : approx
 *
 * lverb = 0    : no print (no print anywhere)
 *         1    : no print (print at higher level)
 *         2    : print DA level diff
 *         3    : print all diffs
 *
 */
int gifti_approx_DA_pair(const giiDataArray * d1, const giiDataArray * d2,
                          int comp_data, int verb)
{
    int c, top, can_comp, offset, diffs = 0;
    int lverb = verb;           /* possibly override passed 'verb' */

    if( G.verb > lverb ) lverb = G.verb;

    /* deal with any NULLs for starter */
    if( !d1 && !d2 ) {
        if(lverb>2) printf("-- approx DA: have NULL\n");
        return 1;       /* yes, these are equal */
    } else if( !d1 || !d2 ) {
        if(lverb>2) printf("-- approx DA: have one NULL\n");
        return 0;       /* not approximately equal */
    }

    /* do early, to put higher level whining first */
    can_comp = can_compare_DA_data(d1, d2, verb);

    if( d1->numCS != d2->numCS ) {
        diffs = 1;
        if( lverb > 1 ) printf("-- approx DA: diff in numCS\n");
        if( lverb < 3 ) return 0;
    }

    /* compare each of the CoordSystem structs */
    top = d1->numCS < d2->numCS ? d1->numCS : d2->numCS;
    for( c = 0; c < top; c++ ) {
        /* first compare without checking the data */
        if( gifti_compare_coordsys(d1->coordsys[c], d2->coordsys[c],1,verb) ) {
            diffs = 1;
            if( lverb > 1 ) printf("-- diff in DA coordsys[%d]\n", c);
            if( lverb < 3 ) return 0;
        }
    }

    /* if we cannot or do not want to compare the data, return */
    if( ! comp_data || ! can_comp ) return !diffs;

    /* if data is coordinates, compare percent diff
     * if trianges, compare exactly, but with any starting index
     * else, compare percent diff */
    if( d1->intent == d2->intent && d1->intent==NIFTI_INTENT_TRIANGLE ) {
        /* verify that these look like triangles */
        if( d1->num_dim < 2 || d1->dims[1] != 3 ) {
            if( lverb > 1 ) printf("-- approx DA: bad dims for TRIANGLEs: "
                       "num_dim=%d, dims[1]=%d\n", d1->num_dim, d1->dims[1]);
            return 0;
        }

        offset = gifti_triangle_diff_offset(d1->data, d2->data, d1->dims[0],
                                            d1->datatype);
        if( offset >= 0 ) {
            diffs |= 2;
            if(lverb > 1) printf("-- approx DA: triange diff at offset %d\n",
                                 offset);
            if(lverb < 3) return 0;
        }
    } else {
        offset = gifti_approx_diff_offset(d1->data, d2->data, d1->nvals,
                                          d1->datatype, 1.0);
        if( offset >= 0 ) {
            diffs |= 2;
            if(lverb>1) printf("-- approx DA: data diff at offset %d\n",offset);
            if(lverb<3) return 0;
        }
    }

    return !diffs;
}


/* return whether the DA elements match so as to compare the data
 *
 * apply the same verb as above
 */
static int can_compare_DA_data(const giiDataArray *d1,const giiDataArray *d2,
                               int verb)
{
    int c, top, rv = 1;
    int lverb = verb;   /* possibly override passed 'verb' */

    if( G.verb > lverb ) lverb = G.verb;

    if( !d1 || !d2 ) {
        if(lverb>1) printf("-- comp DAs: have NULL DA(s) (%p, %p)\n",
                           (void*)d1,(void*)d2);
        return 0;   /* not equal */
    }

    if( d1->datatype != d2->datatype ) {
        rv = 0;
        if( lverb > 1 )
            printf("-- comp DAs: DA datatype diff: %d (%s) vs. %d (%s)\n",
                   d1->datatype, gifti_datatype2str(d1->datatype),
                   d2->datatype, gifti_datatype2str(d2->datatype));
        if( lverb < 3 ) return 0;
    }

    if( d1->ind_ord != d2->ind_ord ) {
        rv = 0;
        if( lverb > 1 )
            printf("-- comp DAs: ind_ord diff: %d (%s) vs. %d (%s)\n",
               d1->ind_ord,
               gifti_list_index2string(gifti_index_order_list, d1->ind_ord),
               d2->ind_ord,
               gifti_list_index2string(gifti_index_order_list, d2->ind_ord));
        if( lverb < 3 ) return 0;
    }

    if( d1->num_dim != d2->num_dim ) {
        rv = 0;
        if( lverb > 1 ) printf("-- comp DAs: num_dim diff: %d vs. %d\n",
                               d1->num_dim, d2->num_dim );
        if( lverb < 3 ) return 0;
    }

    /* get minimum num_dim */
    top = d1->num_dim < d2->num_dim ? d1->num_dim : d2->num_dim;
    for( c = 0; c < top; c++ ) if( d1->dims[c] != d2->dims[c] ) break;
    if( c < top ) {
        rv = 0;
        if( lverb > 1 ) {
            printf("-- comp DAs: DA dims diff (length %d)\n   ", top);
            gifti_disp_raw_data(d1->dims, NIFTI_TYPE_INT32, top, 0, stdout);
            printf("  vs  ");
            gifti_disp_raw_data(d2->dims, NIFTI_TYPE_INT32, top, 1, stdout);
        }
        if( lverb < 3 ) return 0;
    }

    if( d1->nvals != d2->nvals ) {
        rv = 0;
        if( lverb > 1 ) printf("-- comp DAs: nvals diff: %lld vs. %lld\n",
                               d1->nvals, d2->nvals);
        if( lverb < 3 ) return 0;
    }

    if( d1->nbyper != d2->nbyper ) {
        rv = 0;
        if( lverb > 1 ) printf("-- comp DAs: nbyper diff: %d vs. %d\n",
                               d1->nbyper, d2->nbyper);
        if( lverb < 3 ) return 0;
    }

    /* as a last test, make sure dims are valid, not just the same */
    if( ! gifti_valid_dims(d1, 0) ) {
        rv = 0;
        if( lverb > 1 ) printf("-- comp DAs: dims not valid\n");
    }

    return rv;
}

/*---------------------------------------------------------------------*/
/*! check pointers, compare lengths, then check the Names in each list,
 *  and see if there is a matching Name=Value pari
 *
 *  only state diffs in the verb=3 case
*//*-------------------------------------------------------------------*/
int gifti_compare_nvpairs(const nvpairs * p1, const nvpairs * p2, int verb)
{
    char * value;
    int    lverb = verb;        /* possibly override passed verb */
    int    c, len, diffs = 0;

    if( G.verb > lverb ) lverb = G.verb;

    if( !p1 || !p2 ) {
        if(!p1 && !p2) return 0;   /* equal */
        if(lverb>2)
            printf("-- comp nvpairs: have NULL: %p, %p\n",(void*)p1,(void*)p2);
        return 1;   /* not equal */
    }

    /* they must be valid to proceed */
    if( ! gifti_valid_nvpairs(p1, 0) || ! gifti_valid_nvpairs(p2, 0) ) {
        if( lverb > 2 ) printf("-- cannot compare invalid nvpairs\n");
        return 1;
    }

    if( p1->length != p2->length ) {
        if( lverb > 2 ) printf("-- nvp list lengths differ: %d vs %d\n",
                               p1->length, p2->length);
        if( lverb < 3 ) return 1;
    }

    /* search for mis-matches or non-existence from list 1 into list 2  */
    /* assume Names are unique (each that is not will show a mis-match) */
    len = p1->length < p2->length ? p1->length : p2->length;
    for( c = 0; c < p1->length; c++ ) {
        if( ! p1->value[c] ) continue;  /* skip anything that doesn't exist */
        value = gifti_get_meta_value(p2, p1->name[c]);
        if( !value ) {
            if( lverb > 2 )
                printf("-- nvp list 2 missing Name: '%s'\n",p1->name[c]);
            diffs++;
        }
        else if( strcmp(value, p1->value[c]) ) {
            if( lverb > 2 )
                printf("-- nvp diff for Name '%s':\n   '%s' vs. '%s'\n",
                       p1->name[c], p1->value[c], value);
            diffs++;
        }
        if( diffs && lverb < 3 ) return 1;
    }

    /* now just search for non-existence (mis-matches have been found) */
    for( c = 0; c < p2->length; c++ ) {
        if( ! p2->value[c] ) continue;  /* skip anything that doesn't exist */
        value = gifti_get_meta_value(p1, p2->name[c]);
        if( !value ) {
            if( lverb > 2 )
                printf("-- nvp list 1 missing Name: '%s'\n",p2->name[c]);
            if( lverb < 3 ) return 1;
            diffs++;
        }
    }

    return diffs;
}

/*---------------------------------------------------------------------*/
/*! check pointers, lengths and contents
 *
 *  return 0 if equal
 *
 *  only state diffs in the verb=3 case
*//*-------------------------------------------------------------------*/
int gifti_compare_labeltable(const giiLabelTable *t1, const giiLabelTable *t2,
                             int verb)
{
    return compare_labeltables(t1, t2, verb, 0);
}

/*---------------------------------------------------------------------*/
/*! return 1 if tables are approximately equal (opposite of compare function)
 *
 *  RGBA data is compared approximately
 *
 *  only state diffs in the verb=3 case
*//*-------------------------------------------------------------------*/
int gifti_approx_labeltables(const giiLabelTable *t1, const giiLabelTable *t2,
                             int verb)
{
    return( ! compare_labeltables(t1, t2, verb, 1));
}

/*---------------------------------------------------------------------*/
/*! check pointers, lengths and contents
 *
 *  only state diffs in the verb=3 case
 *
 *  if approx, compare RBGA approximately
*//*-------------------------------------------------------------------*/
static int compare_labeltables(const giiLabelTable *t1, const giiLabelTable *t2,
                               int verb, int approx)
{
    int lverb = verb;        /* possibly override passed verb */
    int c, roff, offset, diffs = 0;

    if( G.verb > lverb ) lverb = G.verb;

    if( !t1 || !t2 ) {
        if(!t1 && !t2) return 0;   /* equal */
        if(lverb>2)
            printf("-- Comp LabTab: have NULL: %p, %p\n",(void*)t1,(void*)t2);
        return 1;   /* not   */
    }

    /* if empty, return 0 */
    if( t1->length <= 0 && t2->length <= 0 ) return 0;

    if( t1->length != t2->length ) {
        if(lverb>2)printf("-- labeltable lengths diff: %d vs. %d\n",
                          t1->length, t2->length);
        return 1;  /* cannot compare without equal lengths */
    }

    /* exactly 1 RGBA list is a difference */
    if( (t1->rgba && !t2->rgba) || (!t1->rgba && t2->rgba) ) {
        if(lverb>2)printf("-- only 1 labeltable has RGBA list\n");
        if(lverb<3) return 1;
    }

    /* so lengths are positive and equal, compare index list and labels */

    /* set limit to 0.0 to compare indicies exactly */
    offset = (int)gifti_approx_diff_offset(t1->key, t2->key,
                       t1->length, NIFTI_TYPE_INT32, approx?1.0:0.0);
    if( offset >= 0 ) {
        if(lverb>2)printf("-- labeltable Key diff at index %d\n", offset);
        if(lverb<3) return 1;
        diffs++;
    }

    /* walk through list to compare labels */
    roff = 0;
    for( c = 0; c < t1->length; c++ ) {
        if( gifti_strdiff(t1->label[c], t2->label[c]) ) {
            if(lverb>2)printf("-- labeltable Label diff at index %d\n", c);
            if(lverb<3) return 1;
            diffs++;
            break;      /* stop at first difference */
        }
    }

    if( t1->rgba && t2->rgba ) {
        /* if not approx, set limit to 0.0 to compare exactly */
        offset = (int)gifti_approx_diff_offset(t1->rgba, t2->rgba,
                           4*t1->length, NIFTI_TYPE_FLOAT32, approx?1.0:0.0);
        if( offset >= 0 ) {
            offset /= 4;  /* convert from float index to RGBA index */
            if(lverb>2)printf("-- labeltable RGBA diff at index %d\n", offset);
            if(lverb<3) return 1;
            diffs++;
        }
    }

    return diffs;
}

/*---------------------------------------------------------------------*/
/*! like strcmp, but also return:
 *      0: if both pointers are NULL
 *      1: if exactly one is NULL
*//*-------------------------------------------------------------------*/
int gifti_strdiff(const char * s1, const char * s2)
{
    if( !s1 || !s2 ) {
        if( s1 || s2 ) return 1;        /* one NULL means different */
        else           return 0;        /* both NULL mean equal */
    }

    return strcmp(s1,s2);       /* fall through to normal case */
}

/*---------------------------------------------------------------------*/
/*! check pointers, compare names and xforms
 *
 *  only state diffs in the verb=3 case
*//*-------------------------------------------------------------------*/
int gifti_compare_coordsys(const giiCoordSystem *s1, const giiCoordSystem *s2,
                           int comp_data, int verb)
{
    long long offset;
    int       lverb = verb;        /* possibly override passed verb */
    int       diffs = 0;

    if( G.verb > lverb ) lverb = G.verb;

    if( !s1 || !s2 ) {
        if(!s1 && !s2) return 0;   /* equal */
        if(lverb>2)
            printf("-- Comp CoordSys: have NULL: %p, %p\n",(void*)s1,(void*)s2);
        return 1;
    }

    if( !s1->dataspace || !s2->dataspace ) {
        if( s1->dataspace || s2->dataspace ) {
            if(lverb>2)printf("-- coordsys dspace diff: exactly one is NULL\n");
            if(lverb<3) return 1;
            diffs++;
        }
    } else if( strcmp(s1->dataspace, s2->dataspace) ) {
        if(lverb>2) printf("-- coordsys dspace diff: %s vs. %s\n",
                           s1->dataspace, s2->dataspace);
        if( lverb < 3 ) return 1;
        diffs++;
    }

    if( !s1->xformspace || !s2->xformspace ) {
        if( s1->xformspace || s2->xformspace ) {
            if(lverb>2)
                printf("-- coordsys xformspace diff: exactly one is NULL\n");
            if(lverb<3) return 1;
            diffs++;
        }
    } else if( strcmp(s1->xformspace, s2->xformspace) ) {
        if(lverb>2) printf("-- coordsys xformspace diff: %s vs. %s\n",
                           s1->xformspace, s2->xformspace);
        if( lverb < 3 ) return 1;
        diffs++;
    }

    if( ! comp_data ) return diffs;     /* maybe we are done */

    offset = gifti_compare_raw_data(s1->xform, s2->xform, sizeof(s1->xform));
    if( offset >= 0 ) {
        /* convert to index (to avoid printf warning)  2 Mar 2010 */
        offset /= (long long)sizeof(double);
        if(lverb>2) printf("-- coordsys xform diff at offset %lld\n", offset);
        if( lverb < 3 ) return 1;
        diffs++;
    }

    return diffs;
}

/*---------------------------------------------------------------------*/
/*! compare raw data, returing the first location difference
 *
 * return byte position of difference, so that < 0 means no difference
*//*-------------------------------------------------------------------*/
long long gifti_compare_raw_data(const void * p1, const void * p2,
                                 long long length)
{
    long long   posn;
    char      * d1 = (char *)p1, * d2 = (char *)p2;

    if( !p1 || !p2 ) {
        if( !p1 && !p2 ) return -1; /* both NULL -> same */
        if( G.verb > 3 ) fprintf(stderr,"-- raw_data pointer diff\n");
        return 0;  /* set difference */
    }

    /* scan data until done or a difference is found */
    for( d1 = (char *)p1, d2 = (char *)p2, posn = 0;
         posn < length && *d1 == *d2;
         posn++, d1++, d2++ )
        ;

    if( posn < length ) return posn;    /* differ at posn */

    return -1;  /* equal */
}


/* make a local definition for this symmetric fractional difference */
#undef GIFTI_SFD
#define GIFTI_SFD(a,b) (fabs((a)-(double)(b))/(fabs(a)+fabs(b)))

/*---------------------------------------------------------------------*/
/*! approximate comparison of raw data, returing the first location difference
 *  (>= 0 means difference, -1 means "approximately equal")
 *
 * Compute a symmetric fractional difference:
 *
 *      SFD = abs(a-b)/(abs(a)+abs(b))
 *
 * length       - number of contiguous elements to test
 * ni_type      - NIFTI_TYPE_*, denoting the type of data to compare
 * limit        - maximum SFD to be considered approximately equal
 *
 *              * if limit = 0.0, check is faster
 *
 *              * if limit >= 1.0, apply a default requiring almost all
 *                significant bits
 *
 *              * for integers, all bits are considered significant
 *              * for real numbers, mantissa bits are considered significant
 *
 * return offset index, so that < 0 (-1)  means no difference
 *
 * (return -1 if the pointers differ in whether they are set)
*//*-------------------------------------------------------------------*/
long long gifti_approx_diff_offset(const void * p1, const void * p2,
                                   long long length, int ni_type, double limit)
{
    long long posn;
    double    llim = limit; /* local limit (passed limit or default) */

    if( !p1 || !p2 ) {
        if( !p1 && !p2 ) return -1; /* same */
        return 0;                   /* different */
    }

    switch( ni_type ) {
        default:
            fprintf(stderr,"** cannot test approx data with type %d (%s)\n",
                    ni_type, nifti_datatype_to_string(ni_type));
            return 0;

        case NIFTI_TYPE_INT8: {
            char * d1 = (char *)p1, * d2 = (char *)p2;
            if( llim >= 1.0 ) llim = 0.0;       /* require equality for ints */
            for( posn = 0; posn < length; posn++, d1++, d2++ ) {
                if( *d1 == *d2 ) continue;      /* fast check for equality */
                if( llim == 0.0 ) break;        /* fast check for inequality */
                if( GIFTI_SFD(*d1,*d2) > llim ) break; /* not approximate */
            }
            break;
        }
        case NIFTI_TYPE_INT16: {
            short * d1 = (short *)p1, * d2 = (short *)p2;
            if( llim >= 1.0 ) llim = 0.0;       /* require equality for ints */
            for( posn = 0; posn < length; posn++, d1++, d2++ ) {
                if( *d1 == *d2 ) continue;      /* fast check for equality */
                if( llim == 0.0 ) break;        /* fast check for inequality */
                if( GIFTI_SFD(*d1,*d2) > llim ) break; /* not approximate */
            }
            break;
        }
        case NIFTI_TYPE_INT32: {
            int * d1 = (int *)p1, * d2 = (int *)p2;
            if( llim >= 1.0 ) llim = 0.0;       /* require equality for ints */
            for( posn = 0; posn < length; posn++, d1++, d2++ ) {
                if( *d1 == *d2 ) continue;      /* fast check for equality */
                if( llim == 0.0 ) break;        /* fast check for inequality */
                if( GIFTI_SFD(*d1,*d2) > llim ) break; /* not approximate */
            }
            break;
        }
        case NIFTI_TYPE_INT64: {
            long long * d1 = (long long *)p1, * d2 = (long long *)p2;
            if( llim >= 1.0 ) llim = 0.0;       /* require equality for ints */
            for( posn = 0; posn < length; posn++, d1++, d2++ ) {
                if( *d1 == *d2 ) continue;      /* fast check for equality */
                if( llim == 0.0 ) break;        /* fast check for inequality */
                if( GIFTI_SFD(*d1,*d2) > llim ) break; /* not approximate */
            }
            break;
        }
        case NIFTI_TYPE_UINT8: {
            unsigned char *d1 = (unsigned char *)p1, *d2 = (unsigned char *)p2;
            if( llim >= 1.0 ) llim = 0.0;       /* require equality for ints */
            for( posn = 0; posn < length; posn++, d1++, d2++ ) {
                if( *d1 == *d2 ) continue;      /* fast check for equality */
                if( llim == 0.0 ) break;        /* fast check for inequality */
                if( GIFTI_SFD(*d1,*d2) > llim ) break; /* not approximate */
            }
            break;
        }
        case NIFTI_TYPE_UINT16: {
            unsigned short *d1=(unsigned short *)p1, *d2=(unsigned short *)p2;
            if( llim >= 1.0 ) llim = 0.0;       /* require equality for ints */
            for( posn = 0; posn < length; posn++, d1++, d2++ ) {
                if( *d1 == *d2 ) continue;      /* fast check for equality */
                if( llim == 0.0 ) break;        /* fast check for inequality */
                if( GIFTI_SFD(*d1,*d2) > llim ) break; /* not approximate */
            }
            break;
        }
        case NIFTI_TYPE_UINT32: {
            unsigned int * d1 = (unsigned int *)p1, * d2 = (unsigned int *)p2;
            if( llim >= 1.0 ) llim = 0.0;       /* require equality for ints */
            for( posn = 0; posn < length; posn++, d1++, d2++ ) {
                if( *d1 == *d2 ) continue;      /* fast check for equality */
                if( llim == 0.0 ) break;        /* fast check for inequality */
                if( GIFTI_SFD(*d1,*d2) > llim ) break; /* not approximate */
            }
            break;
        }
        case NIFTI_TYPE_UINT64: {
            unsigned long long * d1 = (unsigned long long *)p1;
            unsigned long long * d2 = (unsigned long long *)p2;
            if( llim >= 1.0 ) llim = 0.0;       /* require equality for ints */
            for( posn = 0; posn < length; posn++, d1++, d2++ ) {
                if( *d1 == *d2 ) continue;      /* fast check for equality */
                if( llim == 0.0 ) break;        /* fast check for inequality */
                if( GIFTI_SFD(*d1,*d2) > llim ) break; /* not approximate */
            }
            break;
        }
        case NIFTI_TYPE_FLOAT32: {
            float * d1 = (float *)p1, * d2 = (float *)p2;
            if( llim >= 1.0 ) llim = 1e-5;
            for( posn = 0; posn < length; posn++, d1++, d2++ ) {
                if( *d1 == *d2 ) continue;      /* fast check for equality */
                if( llim == 0.0 ) break;        /* fast check for inequality */
                if( GIFTI_SFD(*d1,*d2) > llim ) break; /* not approximate */
            }
            break;
        }
        case NIFTI_TYPE_FLOAT64: {
            double * d1 = (double *)p1, * d2 = (double *)p2;
            if( llim >= 1.0 ) llim = 1e-12;
            for( posn = 0; posn < length; posn++, d1++, d2++ ) {
                if( *d1 == *d2 ) continue;      /* fast check for equality */
                if( llim == 0.0 ) break;        /* fast check for inequality */
                if( GIFTI_SFD(*d1,*d2) > llim ) break; /* not approximate */
            }
            break;
        }
    }

    if( posn < length ) return posn;    /* differ at 1-based posn */

    return -1;  /* approximately equal */
}

/*---------------------------------------------------------------------*/
/*! compare triangles - return triangle index of first difference
 *                    - so -1 means no difference
 *
 *  require the type to be 1, 2 or 4-byte integers
 *  could cheat and compare as unsigned...
 *
 *  require consistent wrapping, but allow for varying first vertex
*//*-------------------------------------------------------------------*/
int gifti_triangle_diff_offset(const void *p1, const void *p2, int ntri,
                               int ni_type)
{
    int posn = -1;

    /* if either pointer is not set, we're out of here */
    if( !p1 || !p2 ) {
        if( !p1 && !p2 ) return -1; /* same */
        return 0;                   /* different */
    }

    if( ntri <= 0 ) return -1;

    switch( ni_type ) {
        case NIFTI_TYPE_INT8: {
            char * d1 = (char *)p1, * d2 = (char *)p2;
            for( posn = 0; posn < ntri; posn++, d1+=3, d2+=3 ) {
                if( *d1 == *d2 ) {              /* same first index */
                    if( d1[1] != d2[1] || d1[2] != d2[2] ) break;
                } else if ( d1[0] == d2[1] ) {  /* index off by 1   */
                    if( d1[1] != d2[2] || d1[2] != d2[0] ) break;
                } else if ( d1[0] == d2[2] ) {  /* index off by 2   */
                    if( d1[1] != d2[0] || d1[2] != d2[1] ) break;
                }
            }
            break;
        }
        case NIFTI_TYPE_INT16: {
            short * d1 = (short *)p1, * d2 = (short *)p2;
            for( posn = 0; posn < ntri; posn++, d1+=3, d2+=3 ) {
                if( *d1 == *d2 ) {              /* same first index */
                    if( d1[1] != d2[1] || d1[2] != d2[2] ) break;
                } else if ( d1[0] == d2[1] ) {  /* index off by 1   */
                    if( d1[1] != d2[2] || d1[2] != d2[0] ) break;
                } else if ( d1[0] == d2[2] ) {  /* index off by 2   */
                    if( d1[1] != d2[0] || d1[2] != d2[1] ) break;
                }
            }
            break;
        }
        case NIFTI_TYPE_INT32: {
            int * d1 = (int *)p1, * d2 = (int *)p2;
            for( posn = 0; posn < ntri; posn++, d1+=3, d2+=3 ) {
                if( *d1 == *d2 ) {              /* same first index */
                    if( d1[1] != d2[1] || d1[2] != d2[2] ) break;
                } else if ( d1[0] == d2[1] ) {  /* index off by 1   */
                    if( d1[1] != d2[2] || d1[2] != d2[0] ) break;
                } else if ( d1[0] == d2[2] ) {  /* index off by 2   */
                    if( d1[1] != d2[0] || d1[2] != d2[1] ) break;
                }
            }
            break;
        }
        case NIFTI_TYPE_UINT8: {
            unsigned char *d1 = (unsigned char *)p1, *d2 = (unsigned char *)p2;
            for( posn = 0; posn < ntri; posn++, d1+=3, d2+=3 ) {
                if( *d1 == *d2 ) {              /* same first index */
                    if( d1[1] != d2[1] || d1[2] != d2[2] ) break;
                } else if ( d1[0] == d2[1] ) {  /* index off by 1   */
                    if( d1[1] != d2[2] || d1[2] != d2[0] ) break;
                } else if ( d1[0] == d2[2] ) {  /* index off by 2   */
                    if( d1[1] != d2[0] || d1[2] != d2[1] ) break;
                }
            }
            break;
        }
        case NIFTI_TYPE_UINT16: {
            unsigned short *d1=(unsigned short *)p1, *d2=(unsigned short *)p2;
            for( posn = 0; posn < ntri; posn++, d1+=3, d2+=3 ) {
                if( *d1 == *d2 ) {              /* same first index */
                    if( d1[1] != d2[1] || d1[2] != d2[2] ) break;
                } else if ( d1[0] == d2[1] ) {  /* index off by 1   */
                    if( d1[1] != d2[2] || d1[2] != d2[0] ) break;
                } else if ( d1[0] == d2[2] ) {  /* index off by 2   */
                    if( d1[1] != d2[0] || d1[2] != d2[1] ) break;
                }
            }
            break;
        }
        case NIFTI_TYPE_UINT32: {
            unsigned int * d1 = (unsigned int *)p1, * d2 = (unsigned int *)p2;
            for( posn = 0; posn < ntri; posn++, d1+=3, d2+=3 ) {
                if( *d1 == *d2 ) {              /* same first index */
                    if( d1[1] != d2[1] || d1[2] != d2[2] ) break;
                } else if ( d1[0] == d2[1] ) {  /* index off by 1   */
                    if( d1[1] != d2[2] || d1[2] != d2[0] ) break;
                } else if ( d1[0] == d2[2] ) {  /* index off by 2   */
                    if( d1[1] != d2[0] || d1[2] != d2[1] ) break;
                }
            }
            break;
        }
        default: {
            fprintf(stderr,"** gifti_tri_diff: invalid type %d\n", ni_type);
            return 1;
        }
    }

    if( posn < ntri ) return posn;   /* difference offset */

    return -1;  /* no difference */
}

/*-----------------------------------------------------------------*/
/*! print raw data (nvals of type 'type') to the given file stream
 *
 *  possibly write a trailing newline
*//*-------------------------------------------------------------------*/
int gifti_disp_raw_data(const void * data, int type, int nvals, int newline,
                        FILE * stream)
{
    FILE * fp = stream ? stream : stdout;
    char * dp, fbuf[64];
    int    c, size;

    gifti_datatype_sizes(type, &size, NULL);   /* get nbyper */
    if( size == 0 ) {
        fprintf(stderr,"** GDRD: cannot print with size 0, type %d\n", type);
        return 1;
    }

    for( c = 0, dp = (char *)data; c < nvals; c++, dp += size ) {
        switch( type ) {
            case NIFTI_TYPE_INT8:
                fprintf(fp, "%d", *(char *)dp);
                break;
            case NIFTI_TYPE_INT16:
                fprintf(fp, "%d", *(short *)dp);
                break;
            case NIFTI_TYPE_INT32:
                fprintf(fp, "%d", *(int *)dp);
                break;
            case NIFTI_TYPE_INT64:
                fprintf(fp, "%lld", *(long long *)dp);
                break;
            case NIFTI_TYPE_UINT8:
                fprintf(fp, "%u", *(unsigned char *)dp);
                break;
            case NIFTI_TYPE_UINT16:
                fprintf(fp, "%u", *(unsigned short *)dp);
                break;
            case NIFTI_TYPE_UINT32:
                fprintf(fp, "%u", *(unsigned int *)dp);
                break;
            case NIFTI_TYPE_UINT64:
                fprintf(fp, "%llu", *(unsigned long long *)dp);
                break;
            case NIFTI_TYPE_FLOAT32:
                sprintf(fbuf,"%f", *(float *)dp);
                gifti_clear_float_zeros(fbuf);
                fprintf(fp, "%s", fbuf);
                break;
            case NIFTI_TYPE_FLOAT64:
                sprintf(fbuf,"%f", *(double *)dp);
                gifti_clear_float_zeros(fbuf);
                fprintf(fp, "%s", fbuf);
                break;
            default:
                fprintf(stderr,"** Gdisp_raw_data: invalid type %d\n", type);
                return 1;
        }
        if( c < nvals - 1 ) fputc(' ', fp);
    }

    if ( newline ) fputc('\n', fp);

    return 0;
}

/*----------------------------------------------------------------------
 *! remove trailing zeros from string of printed float
 *  return  1 if something was cleared
 *          0 if not
*//*-------------------------------------------------------------------*/
int gifti_clear_float_zeros( char * str )
{
   char * dp, * valp;
   int    len;

   if( !str || !*str ) return 0;  /* that was easy    */

   dp  = strchr(str, '.');        /* find '.'         */
   if( !dp ) return 0;            /* no '.', easy too */

   len = strlen(dp);

   /* never clear what is just to the right of '.' */
   for( valp = dp+len-1; (valp > dp+1) && (*valp==' ' || *valp=='0'); valp-- )
       *valp = '\0';     /* clear, so we don't worry about break conditions */

   if( valp < dp + len - 1 ) return 1;

   return 0;
}

/*----------------------------------------------------------------------
 *! set all DataArray attributes of the given name to the given value
*//*-------------------------------------------------------------------*/
int gifti_set_atr_in_DAs(gifti_image *gim, const char *name, const char *value,
                         const int * dalist, int len)
{
    int c, ind;

    if( !gim || !name || !value ) {
        fprintf(stderr,"** set_DA_atrs: bad params (%p,%p,%p)\n",
                (void *)gim, (void *)name, (void *)value);
        return 1;
    }

    if( !gim->darray ) return 0;    /* just leave */

    if( dalist && len > 0 ) {       /* set atrs for those DA in dalist */
        if( ! gifti_valid_int_list(dalist, len, 0, gim->numDA-1, 1) )
            return 1;

        /* good to go */
        for( c = 0; c < len; c++ ) {
            ind = dalist[c];    /* for clarity */
            if( ! gim->darray[ind] ) continue;  /* trioanoid */
            if( gifti_str2attr_darray(gim->darray[ind], name, value) ) {
                if( G.verb > 1 )
                    fprintf(stderr,"** bad DA attr '%s'='%s'\n",name,value);
                return 1;
            }
        }

        if( G.verb > 2 )
            fprintf(stderr,"++ set atrs in %d DAs, '%s'='%s'\n",len,name,value);

        return 0;
    }

    /* else continue, do all of them */

    for( c = 0; c < gim->numDA; c++ ) {
        if( ! gim->darray[c] ) continue;  /* trioanoid */
        if( gifti_str2attr_darray(gim->darray[c], name, value) ) {
            if(G.verb>1)fprintf(stderr,"** bad DA attr '%s'='%s'\n",name,value);
            return 1;
        }
    }

    if( G.verb > 4 )
        fprintf(stderr,"++ set attr in all DAs, '%s'='%s'\n", name, value);

    return 0;
}

/*----------------------------------------------------------------------
 *! set MetaData name/value pairs in all DAs in list (or all in gim)
*//*-------------------------------------------------------------------*/
int gifti_set_DA_meta( gifti_image *gim, const char *name, const char *value,
                       const int * dalist, int len, int replace )
{
    int c, ind;

    if( !gim || !name || !value ) {
        fprintf(stderr,"** set_DA_meta: bad params (%p,%p,%p)\n",
                (void *)gim, (void *)name, (void *)value);
        return 1;
    }

    if( !gim->darray ) return 0;    /* just leave */

    if( dalist && len > 0 ) {       /* set meta for those DA in dalist */
        if( ! gifti_valid_int_list(dalist, len, 0, gim->numDA-1, 1) )
            return 1;

        /* good to go */
        for( c = 0; c < len; c++ ) {
            ind = dalist[c];    /* for clarity */
            if( ! gim->darray[ind] ) continue;  /* trioanoid */
            if( gifti_add_to_meta(&gim->darray[ind]->meta,name,value,replace) )
                return 1;
        }

        if( G.verb > 2 )
            fprintf(stderr,"++ set meta in %d DAs, '%s'='%s'\n",len,name,value);

        return 0;
    }

    /* else continue, do all of them */

    for( c = 0; c < gim->numDA; c++ ) {
        if( ! gim->darray[c] ) continue;  /* trioanoid */
        if( gifti_add_to_meta(&gim->darray[c]->meta,name,value,replace) )
            return 1;
    }

    if( G.verb > 4 )
        fprintf(stderr,"++ set MetaData in all DAs, '%s'='%s'\n", name, value);

    return 0;
}

/*----------------------------------------------------------------------
 *! allocate and return a newly created gifti_image_struct
 *
 *  if numDA > 0, allocate and initialize gim->darray
 *  if intent is a valid NIFTI_INTENT code, set it in all DataArray elements
 *  if dtype is a valid NIFTI_TYPE, set it in all DA elements
 *  if dims is set (MUST be of length 6), set the dims in all DA elements
 *  if alloc_data, allocate zero-filled data in each DA element
 *
 *  note that if numDA <= 0, the function returns an empty gifti_image
*//*-------------------------------------------------------------------*/
gifti_image * gifti_create_image( int numDA, int intent, int dtype, int ndim,
                                  const int * dims, int alloc_data )
{
    gifti_image * gim;
    int           c, errs = 0;

    if( G.verb > 1 ) {  /* maybe start with some chit-chat */
        fprintf(stderr,"++ creating gifti_image with %d DA elements\n", numDA);
        if( G.verb > 2 ) {
            fprintf(stderr,"     intent[%d] = %s, dtype[%d] = %s,\n"
                           "     alloc_data = %d, ndim = %d, dims: ",
                    intent, gifti_intent_to_string(intent),
                    dtype,  gifti_datatype2str(dtype), alloc_data, ndim);
            if( !dims ) fprintf(stderr,"<empty>\n");
            else gifti_disp_raw_data(dims, DT_INT32, GIFTI_DARRAY_DIM_LEN,
                                     1, stderr);
        }
    }

    /* basic step - create empty image (with a version string) */
    gim = (gifti_image *)calloc(1, sizeof(gifti_image));
    if(!gim){ fprintf(stderr,"** failed to alloc gifti_image\n"); return NULL; }

    gifti_clear_gifti_image(gim);
    gim->version = gifti_strdup(GIFTI_XML_VERSION);

    if( numDA <= 0 ) return gim;        /* done */

    /* apply numDA, which is incremented in add_empty_darray() */
    gim->numDA = 0;
    if( gifti_add_empty_darray(gim, numDA) ) {  /* then cannot continue */
        gifti_free_image(gim);
        return NULL;
    }

    /* init all to defaults */
    for( c = 0; c < gim->numDA; c++ )
        errs += gifti_set_DA_defaults(gim->darray[c]);

    /* and fill in any other pieces */

    if( gifti_intent_is_valid(intent) )
        errs += gifti_set_atr_in_DAs(gim,"Intent",
                                     gifti_intent_to_string(intent), NULL, 0);

    if( gifti_valid_datatype(dtype, 1) ) {
        errs += gifti_set_atr_in_DAs(gim,"DataType", gifti_datatype2str(dtype),
                                     NULL, 0);
        errs += gifti_update_nbyper(gim);
    }

    if( dims && ndim >= 0 ) errs += gifti_set_dims_all_DA(gim, ndim, dims);

    /* don't try this if there are errors */
    if( !errs && alloc_data ) errs += gifti_alloc_DA_data(gim, NULL, 0);

    if( errs ) {  /* then fail out */
        gifti_free_image(gim);
        return NULL;
    }

    return gim;
}

/*----------------------------------------------------------------------
 *! allocate nvals*nbyper bytes of (zero-filled) data in each DataArray
 *  (in dalist or simply in gim)
 *
 *  return 0 on success
 *         1 on error
*//*-------------------------------------------------------------------*/
int gifti_alloc_DA_data(gifti_image * gim, const int * dalist, int len)
{
    giiDataArray * da;
    long long      nbytes, ntot = 0;
    int            c, index, nset = 0, use_list, numDA;

    if( !gim || !gim->darray || gim->numDA <= 0 ) return 0;

    /* decide whether to use dalist or allocate all data */
    use_list = gifti_valid_int_list(dalist, len, 0, gim->numDA-1, 0);

    if( use_list && G.verb > 2 )
        fprintf(stderr,"++ allocating data for %s\n",
                use_list ? "DA in list" : "all DAs");

    if( use_list && DA_data_exists(gim, dalist, len) ) {
        fprintf(stderr,"** data already exists for some DAs in list\n");
        return 1;
    }

    numDA = use_list ? len : gim->numDA;
    for( c = 0; c < numDA; c++ ) {
        index = use_list ? dalist[c] : c;  /* choose appropriate DA index */
        da = gim->darray[index];           /* set convenient pointer */

        if( ! da ) continue;

        /* if dimensions do not make sense, fail out */
        if( ! gifti_valid_dims(da, G.verb > 0) ) return 1;

        if( da->nvals < 0 || da->nbyper < 0 ) {
            fprintf(stderr,"** bad nvals, nbyper in DA[%d]\n",index);
            return 1;
        }
        nbytes = da->nvals * da->nbyper;
        if( nbytes <= 0 ) continue;  /* skip empty data */

        ntot += nbytes;          /* compute total bytes */
        nset++;

        da->data = calloc(nbytes, sizeof(char));
        if( !da->data ) {
            fprintf(stderr,"** gifti_alloc_DA_data: failed on DA %d of %d\n"
                           "     %lld bytes (%lld total)\n",
                           index, numDA, nbytes, ntot);
            return 1;
        }
    }

    if( G.verb > 3)
        fprintf(stderr,"++ alloc'd %lld bytes in %d DA elements\n", ntot, nset);

    return 0;
}

/*----------------------------------------------------------------------
 *! set num_dims, dims and nvals in every DataArray element
 *
 *  return 0 on success
 *         1 on error
*//*-------------------------------------------------------------------*/
int gifti_set_dims_all_DA(gifti_image * gim, int ndim, const int * dims)
{
    long long nvals;
    int       c, d, nset = 0;

    if(!gim || ndim < 0 || ndim > GIFTI_DARRAY_DIM_LEN || !dims) {
        fprintf(stderr,"** SDA_DA: bad params (%p, %d, %p)\n",
                (void *)gim, ndim, (void *)dims);
        return 1;
    }

    if( !gim->darray || gim->numDA == 0 ) return 0;

    /* first compute nvals */
    for( d = 0, nvals = 1; d < ndim; d++) nvals *= dims[d];
    if( nvals <= 0 ) {
        fprintf(stderr,"** GSDA_DA: malformed dims[%d]: ", ndim);
        gifti_disp_raw_data(dims, DT_INT32, GIFTI_DARRAY_DIM_LEN, 1, stderr);
        return 1;
    }
    if( ndim == 0 ) nvals = 0;

    /* insert num_dim and fill dims (pad with 0s) */
    for( c = 0; c < gim->numDA; c++ ) {
        if( !gim->darray[c] ) continue;  /* paranoid */
        gim->darray[c]->num_dim = ndim;
        for( d = 0; d < ndim; d++ )
            gim->darray[c]->dims[d] = dims[d];
        for( /* continue */ ; d < GIFTI_DARRAY_DIM_LEN; d++ )
            gim->darray[c]->dims[d] = 0;
        gim->darray[c]->nvals = nvals;
        nset++;
    }

    if(G.verb > 3) {
        fprintf(stderr,"++ set dims in %d of %d DA elements to: ",
                nset, gim->numDA);
        gifti_disp_raw_data(dims, DT_INT32, GIFTI_DARRAY_DIM_LEN, 1, stderr);
    }

    return 0;
}

/*----------------------------------------------------------------------
 *! update nbyper/swapsize for all DataArray elements
 *
 *  return 0 on success
 *         1 on error
*//*-------------------------------------------------------------------*/
int gifti_update_nbyper(gifti_image * gim)
{
    giiDataArray * da;
    int            c, errs = 0;

    if( !gim ) return 1;

    if( !gim->darray || gim->numDA == 0 ) return 0;

    for( c = 0; c < gim->numDA; c++ ){
        da = gim->darray[c];    /* just to look cleaner */
        if( !da ) continue;
        errs += gifti_datatype_sizes(da->datatype, &da->nbyper, NULL);
    }

    return errs;
}

/*----------------------------------------------------------------------
 *! fill DataArray element with default values
 *
 *  return 0 on success
 *         1 on error
*//*-------------------------------------------------------------------*/
int gifti_set_DA_defaults(giiDataArray * da)
{
    int c;

    if(!da) { fprintf(stderr,"** NULL in set_DA_defaults\n"); return 1; }

    if( G.verb > 6 ) fprintf(stderr,"-- setting DA defaults\n");

    gifti_clear_DataArray(da);                  /* start with empty struct */

    /* and fill with any non-NULL, non-zero values */

    da->intent = NIFTI_INTENT_NONE;
    da->datatype = NIFTI_TYPE_FLOAT32;
    da->ind_ord = GIFTI_IND_ORD_ROW_MAJOR;
    da->num_dim = 1;                            /* one value per node */

    for( c = 0; c < GIFTI_DARRAY_DIM_LEN; c++ ) da->dims[c] = 0;

    da->encoding = GIFTI_ENCODING_B64BIN;       /* zlib may not be available */
    da->endian = gifti_get_this_endian();
    da->ext_offset = 0;

    da->nvals = 0;
    da->nbyper = 0;
    gifti_datatype_sizes(da->datatype, &da->nbyper, NULL);

    return 0;
}

/*----------------------------------------------------------------------
 *! clear the DataArray element
*//*-------------------------------------------------------------------*/
int gifti_clear_DataArray(giiDataArray * da)
{
    if(!da) { fprintf(stderr,"** NULL in clear_DataArray\n"); return 1; }

    if( G.verb > 5 ) fprintf(stderr,"-- clearing DataArray\n");

    memset(da, 0, sizeof(giiDataArray));

    da->ext_fname = NULL;
    gifti_clear_nvpairs(&da->meta);
    da->coordsys = NULL;
    da->data = NULL;
    gifti_clear_nvpairs(&da->ex_atrs);

    return 0;
}

/*----------------------------------------------------------------------
 *! simply clear all contents of the passed gifti_image
 *  (being explicit with pointers)
 *
 *  return 0 on success
 *         1 on error
*//*-------------------------------------------------------------------*/
int gifti_clear_gifti_image(gifti_image * gim)
{
    if(!gim) { fprintf(stderr,"** NULL in clear_gifti_image\n"); return 1; }

    if( G.verb > 5 ) fprintf(stderr,"-- clearing gifti_image\n");

    /* set the version and clear all pointers */
    memset(gim, 0, sizeof(gim));

    gim->version = NULL;
    gifti_clear_nvpairs(&gim->meta);
    gifti_clear_LabelTable(&gim->labeltable);
    gim->darray = NULL;
    gifti_clear_nvpairs(&gim->ex_atrs);

    return 0;
}

/*----------------------------------------------------------------------
 *! read a dataset, just for numDA
 *
 *  may write faster gxml function for this, if it seems important
*//*-------------------------------------------------------------------*/
int gifti_read_dset_numDA(const char * fname)
{
    gifti_image * gim;
    int           numDA;

    if( !fname ) {
        fprintf(stderr,"** NULL to gifti_read_dset_numDA\n");
        return -1;
    }

    if( G.verb > 2 ) fprintf(stderr,"++ read dset numDA, file '%s'\n",fname);

    gim = gifti_read_da_list(fname, 0, NULL, 0);

    if( !gim ) return -1;       /* errors already printed */

    numDA = gim->numDA;

    if(G.verb > 1)
        fprintf(stderr,"++ read dset numDA, file '%s', numDA = %d\n",
                fname, numDA);

    gifti_free_image(gim);      /* lose dataset and return */

    return numDA;
}

/*----------------------------------------------------------------------
 *! return whether the list values are from min to max
*//*-------------------------------------------------------------------*/
int gifti_valid_int_list(const int *list, int len, int min, int max, int whine)
{
    int c;

    if( !list || len <= 0 ) return 0;

    for( c = 0; c < len; c++ )
        if( list[c] < min || list[c] > max ) {
            if( whine )
                fprintf(stderr,"** bad list index [%d] = %d, not in [%d,%d]\n",
                    c, list[c], min, max);
            return 0;
        }

    return 1;
}

/* return whether any DAs in list have data */
static int DA_data_exists(gifti_image * gim, const int * dalist, int len)
{
    int length, uselist = 0;
    int c, ind;

    if( !dalist || len <= 0 ) { /* then scan all DA elements */
        length = gim->numDA;
        if( length <= 0 ) return 0;
    } else if( !gifti_valid_int_list(dalist, len, 0, gim->numDA-1, 1) ) {
        return 0;
    } else {
        uselist = 1;
        length = len;
    }

    for( c = 0; c < length; c++ ) {
        ind = uselist ? dalist[c] : c;
        if( gim->darray[ind] && gim->darray[ind]->data )
            return 1;
    }

    return 0;
}

/*----------------------------------------------------------------------
 *! add the name=value pair to the MetaData lists
 *
 *  if the name already exists, fail, unless 'replace' is set
 *
 *  return 0 on success, 1 on error
*//*-------------------------------------------------------------------*/
int gifti_add_to_meta( giiMetaData * md, const char * name, const char * value,
                       int replace )
{
    int c;

    if( !md || !name || !value ) return 1;

    if( G.verb > 5 )
        fprintf(stderr,"++ GA2M: name '%s', value '%s', replace = %d\n",
                       name, value, replace);

    /* see if 'name' is already here */
    for( c = 0; c < md->length; c++ )
    {
        if( !md->name[c] && G.verb > 2 ) {
            fprintf(stderr,"** G MD[%d]: no name to check for replacement\n",c);
            continue;
        }

        if( !strcmp(md->name[c], name) ) {      /* a match, apply and return */
            if( !md->value[c] && G.verb > 2 ) {
                fprintf(stderr,"** G MD[%d]: no value to replace\n",c);
                md->value[c] = gifti_strdup(value);
                return 0;
            }

            if( replace ) {
                if( G.verb > 5 ) fprintf(stderr,"   (add via REPLACE)\n");
                if( md->value[c] ) free(md->value[c]);
                md->value[c] = gifti_strdup(value);
                return 0;
            } else {
                fprintf(stderr,"** G_add_to_meta: name '%s', already exists\n",
                               name);
                return 1;
            }
        }
    }

    /* name is new, so just add it */

    if( G.verb > 5 ) fprintf(stderr,"   (adding new entry)\n");

    md->length++;
    md->name = (char **)realloc(md->name, md->length * sizeof(char *));
    md->value = (char **)realloc(md->value, md->length * sizeof(char *));

    if( !md->name || !md->value ) {
        fprintf(stderr,"** GA2M:failed to realloc %d MD pointers\n",md->length);
        md->length = 0;
        return 1;
    }

    md->name[md->length-1] = gifti_strdup(name);
    md->value[md->length-1] = gifti_strdup(value);

    if( ! md->name[md->length-1] || ! md->value[md->length-1] )
        return 1;

    return 0;
}

/*----------------------------------------------------------------------
 *! check for validity of the gifti_image (including sub-structures)
 *
 *  if whine is set, complain about any errors
 *
 *  return 1 if valid, 0 otherwise
*//*-------------------------------------------------------------------*/
int gifti_valid_gifti_image( gifti_image * gim, int whine )
{
    int c, errs = 0;

    if( !gim ) {
        if(whine) fprintf(stderr,"** invalid: gifti_image ptr is NULL\n");
        return 0;
    }

    if( G.verb > 3 ) fprintf(stderr,"-- checking for valid gifti_image...\n");

    if( gim->numDA < 0 ) {
        if(whine) fprintf(stderr,"** invalid: numDA = %d\n", gim->numDA);
        errs ++;
    }

    if( !gim->version || strcmp(gim->version, GIFTI_XML_VERSION) ) {
        if(whine) fprintf(stderr, "** invalid: gim version = %s\n",
                          G_CHECK_NULL_STR(gim->version));
        errs++;
    }

    if( ! gifti_valid_nvpairs(&gim->meta, whine) ) errs ++;

    if( ! gifti_valid_LabelTable(&gim->labeltable, whine) ) errs ++;

    for( c = 0; c < gim->numDA; c++ ) {
        if( G.verb > 5 ) fprintf(stderr,"-- checking DA[%d]\n", c);
        if( ! gifti_valid_DataArray(gim->darray[c], whine) ) {
            if( G.verb > 3 ) fprintf(stderr,"-- DA[%d] has errors\n",c);
            errs++;
        } else if( G.verb > 4 )
            fprintf(stderr,"-- DA[%d] is VALID\n",c);
    }

    /* no check on swapped or compressed */

    if( ! gifti_valid_nvpairs(&gim->ex_atrs, whine) ) errs ++;

    if( G.verb > 2 ) {
        fprintf(stderr,"-- gifti_image: errors = %d", errs);
        if( errs ) fprintf(stderr," (INVALID)\n");
        else       fprintf(stderr," (VALID)\n");
    }

    if( errs ) return 0;
    else       return 1;
}

/*---------------------------------------------------------------------*/
/*! return whether data exists
 *
 *  - darray, each darray[i] and darray[i]->data must be set
 *
 *  return 1 if true, 0 otherwise
*//*-------------------------------------------------------------------*/
int gifti_image_has_data(const gifti_image * gim)
{
    int c;

    if( !gim || !gim->darray || gim->numDA <= 0 ) return 0;

    for( c = 0; c < gim->numDA; c++ )
        if( !gim->darray[c] ) {
            if(G.verb > 3) fprintf(stderr,"** gim missing data at ind %d\n",c);
            return 0;
        }

    return 1;
}

/*---------------------------------------------------------------------*/
/*! duplicate the given gifti_image struct, optionally including data
 *
 *  Allocate and copy all contents of the gifti_image structure and
 *  sub-structures.  If copy_data is not set, all data pointers within
 *  DataArray elements will be left as NULL.
 *
 *  return a pointer to the newly allocated structure
*//*-------------------------------------------------------------------*/
gifti_image * gifti_copy_gifti_image(const gifti_image * gold, int copy_data)
{
    gifti_image * gnew;
    int           c, errs = 0;   /* check for errors at each step */

    if( !gold ){ fprintf(stderr,"** copy_gim: input is NULL\n"); return NULL; }

    if(G.verb > 3) fprintf(stderr,"++ copying gifti_image (%s data)...\n",
                           copy_data ? "with" : "without" );

    gnew = (gifti_image *)calloc(1, sizeof(gifti_image));
    if(!gnew){fprintf(stderr,"** copy_gim, failed alloc\n"); return NULL;}

    /* copy one piece at a time */
    gnew->numDA = gold->numDA;
    gnew->version = gifti_strdup(gold->version);

    errs = gifti_copy_nvpairs(&gnew->meta, &gold->meta);
    if(!errs) errs=gifti_copy_LabelTable(&gnew->labeltable, &gold->labeltable);

    /* if needed, create and copy the DataArray list */
    if( !errs && gold->darray && gold->numDA > 0 ) {
        gnew->darray=(giiDataArray**)malloc(gold->numDA*sizeof(giiDataArray*));
        if( !gnew->darray ) {
            fprintf(stderr,"** copy_gim: failed to alloc darray of len %d\n",
                    gold->numDA);
            errs = 1;
        }

        for( c = 0; !errs && c < gold->numDA; c++ ) {
            gnew->darray[c] = gifti_copy_DataArray(gold->darray[c], copy_data);
            if( !gnew->darray[c]) errs++;
        }
    }

    /* and copy the extras */
    if( !errs ) {
        gnew->swapped = gold->swapped;
        gnew->compressed = gold->compressed;
        errs = gifti_copy_nvpairs(&gnew->ex_atrs, &gold->ex_atrs);
    }

    /* on failure, blow everything away */
    if( errs ) { gifti_free_image(gnew); return NULL; }

    return gnew;
}

/*---------------------------------------------------------------------*/
/*! convert all data to NIFTI_TYPE_FLOAT32
 *
 *  for each DataArray
 *      if data exists, convert it (free old, allocate new)
 *      else, leave as NULL
*//*-------------------------------------------------------------------*/
int gifti_convert_to_float(gifti_image * gim)
{
    giiDataArray * da;
    void         * olddata;
    int            oldtype, newtype = NIFTI_TYPE_FLOAT32; /* for future? */
    int            c, nbyper, oldnbyper;

    if( !gim ) return 1;

    if( !gim->darray || gim->numDA <= 0 ) {
        if( G.verb > 1 ) fprintf(stderr,"-- no darray to convert to float\n");
        return 0;
    }

    if( G.verb > 1 ) fprintf(stderr,"++ converting gifti_image to float\n");

    /* first, check for problems in any DA */
    for( c = 0; c < gim->numDA; c++ ) {
        da = gim->darray[c];
        if( !da ) continue;

        /* if the data has an unknown type, panic into error */
        if( da->data && !gifti_valid_datatype(da->datatype, 0) ) {
            fprintf(stderr,"** unknown dtype %d, cannot convert to floats\n",
                    da->datatype);
            return 1;
        }

        /* dimension information must be consistent */
        if( !gifti_valid_dims(da, 1) ) return 1;
    }

    /* will update nbyper in each DA, below */
    gifti_datatype_sizes(newtype, &nbyper, NULL);

    /* all is well, update all DA elements */
    for( c = 0; c < gim->numDA; c++ ) {
        da = gim->darray[c];
        if( !da ) continue;

        oldtype = da->datatype;
        oldnbyper = da->nbyper;

        if( oldtype == newtype ) {
            if(G.verb > 3) fprintf(stderr,"++ convert DA[%d] already type %s\n",
                                   c, gifti_datatype2str(newtype));
            continue; /* no change needed */
        }

        if(G.verb > 3)
            fprintf(stderr,"++ convert DA[%d] from %s to %s\n", c,
                    gifti_datatype2str(oldtype), gifti_datatype2str(newtype));

        if( oldtype == newtype ) continue; /* no change needed */

        /* start trashing DataArray: adjust datatype and nbyper */
        da->datatype = newtype;
        da->nbyper = nbyper;

        /* if there is no data, we are done with this DA */
        if( !da->data ) {
            if( G.verb > 4 ) fprintf(stderr,"-- no data to convert\n");
            continue;
        }

        /* store old data pointer, and allocate new data space */
        olddata = da->data;
        da->data = NULL;    /* so alloc_DA_data doesn't whine */
        if( gifti_alloc_DA_data(gim, &c, 1) ) return 1;

        /* copy the data, and nuke the old stuff */
        if( copy_data_as_float(da->data,newtype,olddata,oldtype,da->nvals) ) {
            /* undo this copy and return */
            free(da->data);
            da->data = olddata; da->datatype = oldtype; da->nbyper = oldnbyper;
            return 1;
        }

        free(olddata);  /* done with this, data has been copied */
    }

    return 0;
}

/* copy old data to float array */
static int copy_data_as_float(void * dest, int dtype, void * src, int stype,
                              long long nvals)
{
    float     * dptr = (float *)dest;
    long long   c;

    if( !dest || !src ) {
        fprintf(stderr,"** copy_data_as_float: missing pointers\n");
        return 1;
    }

    if( dtype != NIFTI_TYPE_FLOAT32 ) {
        fprintf(stderr,"** can't copy to float with dest type %d\n", dtype);
        return 1;
    }

    switch( stype ) {
        default: {
            fprintf(stderr,"** copy2float: can't handle src type %d\n",stype);
            return 1;
        }

        case NIFTI_TYPE_INT8: {
            char * sptr = (char *)src;
            for( c = 0; c < nvals; c++ ) dptr[c] = (float)sptr[c];
            break;
        }

        case NIFTI_TYPE_INT16: {
            short * sptr = (short *)src;
            for( c = 0; c < nvals; c++ ) dptr[c] = (float)sptr[c];
            break;
        }

        case NIFTI_TYPE_INT32: {
            int * sptr = (int *)src;
            for( c = 0; c < nvals; c++ ) dptr[c] = (float)sptr[c];
            break;
        }

        case NIFTI_TYPE_INT64: {
            long long * sptr = (long long *)src;
            for( c = 0; c < nvals; c++ ) dptr[c] = (float)sptr[c];
            break;
        }

        case NIFTI_TYPE_UINT8: {
            unsigned char * sptr = (unsigned char *)src;
            for( c = 0; c < nvals; c++ ) dptr[c] = (float)sptr[c];
            break;
        }

        case NIFTI_TYPE_UINT16: {
            unsigned short * sptr = (unsigned short *)src;
            for( c = 0; c < nvals; c++ ) dptr[c] = (float)sptr[c];
            break;
        }

        case NIFTI_TYPE_UINT32: {
            unsigned int * sptr = (unsigned int *)src;
            for( c = 0; c < nvals; c++ ) dptr[c] = (float)sptr[c];
            break;
        }

        case NIFTI_TYPE_UINT64: {
            unsigned long long * sptr = (unsigned long long *)src;
            for( c = 0; c < nvals; c++ ) dptr[c] = (float)sptr[c];
            break;
        }

        case NIFTI_TYPE_FLOAT32: {
            float * sptr = (float *)src;
            for( c = 0; c < nvals; c++ ) dptr[c] = sptr[c];
            break;
        }

        case NIFTI_TYPE_FLOAT64: {
            double * sptr = (double *)src;
            for( c = 0; c < nvals; c++ ) dptr[c] = (float)sptr[c];
            break;
        }
    }

    return 0;
}
