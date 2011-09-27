#ifndef GIFTI_IO_H
#define GIFTI_IO_H

#include <itk_zlib.h>
#include <expat.h>
#include <nifti1_io.h>
/* also #include "gifti_xml.h", but at the end */
#ifdef  __cplusplus
extern "C" {
#endif
/* ---------------------------------------------------------------------- */
/* These must be 0-based and sequential.
        - 0 matches _UNDEF
        - highest maches _MAX
        - list matches corresponding gifti_*_list
*/

#define GIFTI_IND_ORD_UNDEF     0
#define GIFTI_IND_ORD_ROW_MAJOR 1
#define GIFTI_IND_ORD_COL_MAJOR 2
#define GIFTI_IND_ORD_MAX       2

#define GIFTI_DATALOC_UNDEF     0
#define GIFTI_DATALOC_INT       1
#define GIFTI_DATALOC_EXT       2
#define GIFTI_DATALOC_MAX       2

#define GIFTI_ENCODING_UNDEF     0
#define GIFTI_ENCODING_ASCII     1      /* human readable ASCII data  */
#define GIFTI_ENCODING_B64BIN    2      /* base64 encoded binary data */
#define GIFTI_ENCODING_B64GZ     3      /* base64 compressed binary   */
#define GIFTI_ENCODING_EXTBIN    4      /* external unencoded binary  */
#define GIFTI_ENCODING_MAX       4

#define GIFTI_ENDIAN_UNDEF       0
#define GIFTI_ENDIAN_BIG         1
#define GIFTI_ENDIAN_LITTLE      2
#define GIFTI_ENDIAN_MAX         2

#define GIFTI_B64_CHECK_UNDEF    0
#define GIFTI_B64_CHECK_NONE     1      /* no checking                   */
#define GIFTI_B64_CHECK_DETECT   2      /* simply detect errors          */
#define GIFTI_B64_CHECK_COUNT    3      /* count the number of errors    */
#define GIFTI_B64_CHECK_SKIP     4      /* skip any bad chars, no count  */
#define GIFTI_B64_CHECK_SKIPNCOUNT 5    /* skip and count bad characters */
#define GIFTI_B64_CHECK_MAX      5

#define GIFTI_DARRAY_DIM_LEN     6  /* length of dims[] array */

/* use our own #def, in case we don't have zlib */
#undef GZ_DEFAULT_COMPRESSION
#ifdef HAVE_ZLIB
#define GZ_DEFAULT_COMPRESSION Z_DEFAULT_COMPRESSION
#define GIFTI_COMP_WITH_ZLIB    1       /* to show at run-time */
#else
#define GZ_DEFAULT_COMPRESSION -1
#define GIFTI_COMP_WITH_ZLIB    0
#endif

/* global declarations of matching lists */
extern char * gifti_index_order_list[];
extern char * gifti_dataloc_list[];
extern char * gifti_encoding_list[] ;
extern char * gifti_endian_list[];

/* ---------------------------------------------------------------------- */

/* notes:

        - all data should be owned by the struct, i.e. strings should be
          allocated copies, never assigned as a pointer used elsewhere
*/

typedef struct {
    int     length;
    char ** name;
    char ** value;
} nvpairs;
typedef nvpairs giiMetaData;

typedef struct {
    int      length;      /* length of each array, if allocated            */
    int    * key;         /* changed from index                 7 Mar 2010 */
    char  ** label;
    float  * rgba;        /* (optional) RGBA tuples (4*length, in [0,1.0]) */
} giiLabelTable;

typedef struct {
    char   * dataspace;
    char   * xformspace;
    double   xform[4][4];         /* ******** no type specified ********** */
} giiCoordSystem;

typedef struct {
    /* attributes */
    int               intent;     /* NIFTI_INTENT code, describing data    */
    int               datatype;   /* numerical type of Data values         */
    int               ind_ord;    /* lowest Dim to highest, or reverse     */
    int               num_dim;    /* level of DimX applied                 */
    int               dims[6];    /* dimension lengths (first num_dim set) */
    int               encoding;   /* format of Data on disk                */
    int               endian;     /* endian, if binary Encoding            */
    char            * ext_fname;  /* external filename, in cur directory   */
    long long         ext_offset; /* offset of data within external file   */

    /* elements */
    giiMetaData       meta;
    giiCoordSystem ** coordsys;   /* array of pointers to giiCoordSystem   */
    void            * data;       /* unencoded, uncompressed, swapped      */

    /* extras */
    long long         nvals;      /* number of values (product of Dims)    */
    int               nbyper;     /* number of bytes per value             */
    int               numCS;      /* number of giiCoordSystem structs      */
    nvpairs           ex_atrs;    /* extra attributes                      */
} giiDataArray;

typedef struct {
    /* attributes */
    int               numDA;      /* number of DataArrays            */
    char            * version;    /* GIFTI version string            */

    /* elements */
    giiMetaData       meta;
    giiLabelTable     labeltable;
    giiDataArray   ** darray;

    /* extras */
    int               swapped;    /* were the bytes swapped          */
    int               compressed; /* was the data compressed         */
    nvpairs           ex_atrs;    /* extra attributes                */
} gifti_image;

typedef struct {
    int verb;
} gifti_globals;

typedef struct {
    int    type;          /* should match NIFTI_TYPE_* */
    int    nbyper;        /* bytes per value           */
    int    swapsize;      /* bytes per swap piece      */
    char * name;          /* text string match type    */
} gifti_type_ele;


/* prototypes */

/* main interface protos */
gifti_image * gifti_read_image  (const char * fname, int read_data );
gifti_image * gifti_read_da_list(const char * fname, int read_data,
                                 const int * dalist, int len );
int    gifti_free_image         (gifti_image * gim);
int    gifti_valid_gifti_image  (gifti_image * gim, int whine);
int    gifti_write_image        (gifti_image *gim, const char *fname,
                                 int write_data);

gifti_image * gifti_create_image(int numDA, int intent, int dtype, int ndim,
                                 const int * dims, int alloc_data);

/* end main interface protos */

int    gifti_get_b64_check      (void);
int    gifti_set_b64_check      (int level);
int    gifti_get_indent         (void);
int    gifti_set_indent         (int level);
int    gifti_get_verb           (void);
int    gifti_set_verb           (int level);
int    gifti_get_update_ok      (void);
int    gifti_set_update_ok      (int level);
int    gifti_get_zlevel         (void);
int    gifti_set_zlevel         (int level);

/* data copy routines */
int     gifti_convert_to_float(gifti_image * gim);
char ** gifti_copy_char_list  (char ** list, int len);
int     gifti_copy_all_DA_meta(giiDataArray *dest, giiDataArray *src);
int     gifti_copy_DA_meta    (giiDataArray *dest, giiDataArray *src,
                               const char *name);
int     gifti_copy_DA_meta_many(gifti_image * dest, gifti_image * src,
                                const char * name, const int * dalist, int len);
int     gifti_copy_gifti_meta (gifti_image * dest, gifti_image * src,
                               const char * name);
int     gifti_copy_LabelTable (giiLabelTable * dest, const giiLabelTable * src);
int     gifti_copy_nvpairs    (nvpairs *dest, const nvpairs *src);

char  * gifti_strdup          (const char * src);
gifti_image    * gifti_copy_gifti_image(const gifti_image *gold, int copy_data);
giiCoordSystem * gifti_copy_CoordSystem(const giiCoordSystem *src);
giiDataArray   * gifti_copy_DataArray  (const giiDataArray *orig, int get_data);


long long gifti_darray_nvals    (const giiDataArray * da);
long long gifti_gim_DA_size     (const gifti_image * p, int in_mb);

int    gifti_check_swap         (void *data, int endian, long long nsets,
                                 int swapsize);
int    gifti_datatype_sizes     (int datatype, int *nbyper, int *swapsize);
char * gifti_datatype2str       (int type);
char * gifti_get_meta_value     (const nvpairs * nvp, const char * name);
int    gifti_get_this_endian    (void);
int    gifti_image_has_data     (const gifti_image * gim);
int    gifti_intent_from_string (const char * name);
int    gifti_intent_is_valid    (int code);
char * gifti_intent_to_string   (int code);
char * gifti_list_index2string  (char * list[], int index);
int    gifti_get_xml_buf_size   (void);
int    gifti_set_xml_buf_size   (int buf_size);
int    gifti_str2attr_gifti     (gifti_image * gim, const char * attr,
                                                    const char * val);
int    gifti_str2attr_darray    (giiDataArray * DA, const char * attr,
                                                    const char * value);
int    gifti_str2ind_ord        (const char * str);
int    gifti_str2dataloc        (const char * str);
int    gifti_str2encoding       (const char * str);
int    gifti_str2endian         (const char * str);
int    gifti_str2datatype       (const char * str);
int    gifti_swap_2bytes        (void *data, long long nsets);
int    gifti_swap_4bytes        (void *data, long long nsets);
int    gifti_swap_Nbytes        (void *data, long long nsets, int swapsize);

int    gifti_alloc_DA_data      (gifti_image * gim, const int *dalist, int len);
int    gifti_add_empty_CS       (giiDataArray * da);
int    gifti_add_empty_darray   (gifti_image * gim, int num_to_add);
int    gifti_add_to_meta        (giiMetaData * md, const char * name,
                                 const char * value, int replace);
int    gifti_add_to_nvpairs     (nvpairs * p, const char * name,
                                              const char * value);

int    gifti_free_CoordSystem   (giiCoordSystem * cs);
int    gifti_free_CS_list       (giiDataArray * da);
int    gifti_free_DataArray_list(giiDataArray ** darray, int numDA);
int    gifti_free_DataArray     (giiDataArray * darray);
int    gifti_free_LabelTable    (giiLabelTable * t);
int    gifti_free_nvpairs       (nvpairs * p);

int    gifti_read_dset_numDA    (const char * fname);
int    gifti_read_extern_DA_data(giiDataArray * da);
int    gifti_set_atr_in_DAs     (gifti_image *gim, const char *name,
                                 const char *value, const int *dalist, int len);
int    gifti_set_DA_atrs        (giiDataArray * da, const char ** attr, int len,
                                 int add_to_extras);
int    gifti_set_DA_defaults    (giiDataArray * da);
int    gifti_set_DA_meta        (gifti_image *gim, const char *name,
                                 const char *value, const int * dalist,
                                 int len, int replace);
int    gifti_set_dims_all_DA    (gifti_image * gim, int ndim, const int * dims);
int    gifti_set_extern_filelist(gifti_image * gim, int nfiles, char ** files);
int    gifti_update_nbyper      (gifti_image * gim);
int    gifti_valid_DataArray    (const giiDataArray * da, int whine);
int    gifti_valid_datatype     (int dtype, int whine);
int    gifti_valid_dims         (const giiDataArray * da, int whine);
int    gifti_valid_int_list     (const int *list, int len, int min, int max,
                                 int whine);
int    gifti_valid_LabelTable   (const giiLabelTable * T, int whine);
int    gifti_valid_nbyper       (int nbyper, int whine);
int    gifti_valid_num_dim      (int num_dim, int whine);
int    gifti_valid_nvpairs      (const nvpairs * nvp, int whine);
int    gifti_write_extern_DA_data(giiDataArray * da);

/* comparison functions */
int gifti_approx_gifti_images   (const gifti_image * g1, const gifti_image * g2,
                                 int comp_data, int verb);
int gifti_compare_gifti_images  (const gifti_image * g1, const gifti_image * g2,
                                 int comp_data, int verb);

int gifti_approx_DA_pair        (const giiDataArray*d1, const giiDataArray *d2,
                                 int comp_data, int verb);
int gifti_approx_labeltables    (const giiLabelTable*t1,const giiLabelTable *t2,
                                 int verb);
int gifti_compare_coordsys      (const giiCoordSystem * s1,
                           const giiCoordSystem * s2,int comp_data, int verb);
int gifti_compare_DA_data       (const giiDataArray *d1, const giiDataArray *d2,
                                 int verb);
int gifti_compare_DA_pair       (const giiDataArray *d1, const giiDataArray *d2,
                                 int comp_data, int verb);
int gifti_compare_gifti_data    (const gifti_image * g1, const gifti_image * g2,
                                 int verb);
int gifti_compare_gims_only     (const gifti_image * g1, const gifti_image * g2,
                                 int verb);
int gifti_compare_labeltable    (const giiLabelTable*t1, const giiLabelTable*t2,
                                 int verb);
int gifti_compare_nvpairs       (const nvpairs *p1, const nvpairs *p2,int verb);
int gifti_strdiff               (const char * s1, const char * s2);

long long gifti_approx_diff_offset(const void * p1, const void * p2,
                                 long long length, int ni_type, double limit);
long long gifti_compare_raw_data(const void * p1, const void * p2,
                                 long long length);
int       gifti_triangle_diff_offset(const void *p1, const void *p2,
                                 int ntri, int ni_type);

/* display functions */
void   gifti_disp_dtd_url        (void);
void   gifti_disp_lib_hist       (void);
void   gifti_disp_lib_version    (void);

int    gifti_disp_nvpairs        (const char *mesg, const nvpairs *p);
int    gifti_disp_LabelTable     (const char *mesg, const giiLabelTable *p);
int    gifti_disp_CoordSystem    (const char *mesg, const giiCoordSystem *p);
int    gifti_disp_DataArray      (const char *mesg, const giiDataArray *p,
                                  int subs);
int    gifti_disp_gifti_image    (const char *mesg, const gifti_image *p,
                                  int subs);

int    gifti_disp_hex_data       (const char *mesg, const void *data, int len,
                                  FILE * fp);
int    gifti_disp_raw_data       (const void *data, int type, int nvals,
                                  int newline, FILE * stream);


int    gifti_clear_DataArray     (giiDataArray * da);
int    gifti_clear_float_zeros   (char * str);
int    gifti_clear_gifti_image   (gifti_image * gim);
int    gifti_clear_nvpairs       (nvpairs * p);
int    gifti_clear_LabelTable    (giiLabelTable * p);
int    gifti_clear_CoordSystem   (giiCoordSystem * p);

giiDataArray * gifti_find_DA     (gifti_image * gim, int intent, int index);
int    gifti_find_DA_list        (gifti_image * gim, int intent,
                                  giiDataArray *** list,int *len);
int    gifti_DA_rows_cols        (giiDataArray * da, long long *rows,
                                                     long long *cols);
char * gifticlib_version         (void);

#undef G_CHECK_NULL_STR
#define G_CHECK_NULL_STR(s) (s ? s : "NULL")


#ifdef  __cplusplus
}
#endif

#include "gifti_xml.h" /* needs gifti_io.h, but users should not #include it */

#endif /* GIFTI_IO_H */
