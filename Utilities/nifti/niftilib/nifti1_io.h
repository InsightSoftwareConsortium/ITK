/** \file nifti1_io.h
    \brief Data structures for using nifti1_io API.
           - Written by Bob Cox, SSCC NIMH
           - Revisions by Rick Reynolds, SSCC NIMH
 */
#ifndef _NIFTI_IO_HEADER_
#define _NIFTI_IO_HEADER_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>

#ifndef DONT_INCLUDE_ANALYZE_STRUCT
#define DONT_INCLUDE_ANALYZE_STRUCT  /*** not needed herein ***/
#endif
#include "nifti1.h"                  /*** NIFTI-1 header specification ***/

#include <znzlib.h>

/*=================*/
#ifdef  __cplusplus
extern "C" {
#endif
/*=================*/

/*****===================================================================*****/
/*****         File nifti1_io.h == Declarations for nifti1_io.c          *****/
/*****...................................................................*****/
/*****            This code is released to the public domain.            *****/
/*****...................................................................*****/
/*****  Author: Robert W Cox, SSCC/DIRP/NIMH/NIH/DHHS/USA/EARTH          *****/
/*****  Date:   August 2003                                              *****/
/*****...................................................................*****/
/*****  Neither the National Institutes of Health (NIH), nor any of its  *****/
/*****  employees imply any warranty of usefulness of this software for  *****/
/*****  any purpose, and do not assume any liability for damages,        *****/
/*****  incidental or otherwise, caused by any use of this document.     *****/
/*****===================================================================*****/

/* 
   Modified by: Mark Jenkinson (FMRIB Centre, University of Oxford, UK)
   Date: July/August 2004 

      Mainly adding low-level IO and changing things to allow gzipped files
      to be read and written
      Full backwards compatability should have been maintained

   Modified by: Rick Reynolds (SSCC/DIRP/NIMH, National Institutes of Health)
   Date: December 2004

      Modified and added many routines for I/O.
*/

/********************** Some sample data structures **************************/

typedef struct {                   /** 4x4 matrix struct **/
  float m[4][4] ;
} mat44 ;

typedef struct {                   /** 3x3 matrix struct **/
  float m[3][3] ;
} mat33 ;

/*...........................................................................*/

/*! \struct nifti_image
    \brief High level data structure for open nifti datasets in the
           nifti1_io API.  Note that this structure is not part of the
           nifti1 format definition; it is used to implement one API
           for reading/writing formats in the nifti1 format.
 */
typedef struct {                /*!< Image storage struct **/

  int ndim ;                    /*!< last dimension greater than 1 (1..7) */
  int nx ;                      /*!< dimensions of grid array             */
  int ny ;                      /*!< dimensions of grid array             */
  int nz ;                      /*!< dimensions of grid array             */
  int nt ;                      /*!< dimensions of grid array             */
  int nu ;                      /*!< dimensions of grid array             */
  int nv ;                      /*!< dimensions of grid array             */
  int nw ;                      /*!< dimensions of grid array             */
  int dim[8] ;                  /*!< dim[0]=ndim, dim[1]=nx, etc.         */
  int nvox ;                    /*!< number of voxels = nx*ny*nz*...*nw   */
  int nbyper ;                  /*!< bytes per voxel, matches datatype    */
  int datatype ;                /*!< type of data in voxels: DT_* code    */

  float dx ;                    /*!< grid spacings      */
  float dy ;                    /*!< grid spacings      */
  float dz ;                    /*!< grid spacings      */
  float dt ;                    /*!< grid spacings      */
  float du ;                    /*!< grid spacings      */
  float dv ;                    /*!< grid spacings      */
  float dw ;                    /*!< grid spacings      */
  float pixdim[8] ;             /*!< pixdim[1]=dx, etc. */

  float scl_slope ;             /*!< scaling parameter - slope        */
  float scl_inter ;             /*!< scaling parameter - intercept    */

  float cal_min ;               /*!< calibration parameter, minimum   */
  float cal_max ;               /*!< calibration parameter, maximum   */

  int qform_code ;              /*!< codes for (x,y,z) space meaning  */
  int sform_code ;              /*!< codes for (x,y,z) space meaning  */

  int freq_dim  ;               /*!< indexes (1,2,3, or 0) for MRI    */
  int phase_dim ;               /*!< directions in dim[]/pixdim[]     */
  int slice_dim ;               /*!< directions in dim[]/pixdim[]     */

  int   slice_code  ;           /*!< code for slice timing pattern    */
  int   slice_start ;           /*!< index for start of slices        */
  int   slice_end   ;           /*!< index for end of slices          */
  float slice_duration ;        /*!< time between individual slices   */

  /*! quaternion transform parameters
    [when writing a dataset, these are used for qform, NOT qto_xyz]   */
  float quatern_b , quatern_c , quatern_d ,
        qoffset_x , qoffset_y , qoffset_z ,
        qfac      ;

  mat44 qto_xyz ;               /*!< qform: transform (i,j,k) to (x,y,z) */
  mat44 qto_ijk ;               /*!< qform: transform (x,y,z) to (i,j,k) */

  mat44 sto_xyz ;               /*!< sform: transform (i,j,k) to (x,y,z) */
  mat44 sto_ijk ;               /*!< sform: transform (x,y,z) to (i,j,k) */

  float toffset ;               /*!< time coordinate offset */

  int xyz_units  ;              /*!< dx,dy,dz units: NIFTI_UNITS_* code  */
  int time_units ;              /*!< dt       units: NIFTI_UNITS_* code  */

  int nifti_type ;              /*!< 0==ANALYZE, 1==NIFTI-1 (1 file),
                                                 2==NIFTI-1 (2 files),
                                                 3==NIFTI-ASCII (1 file) */
  int   intent_code ;           /*!< statistic type (or something)       */
  float intent_p1 ;             /*!< intent parameters                   */
  float intent_p2 ;             /*!< intent parameters                   */
  float intent_p3 ;             /*!< intent parameters                   */
  char  intent_name[16] ;       /*!< optional description of intent data */

  char descrip[80]  ;           /*!< optional text to describe dataset   */
  char aux_file[24] ;           /*!< auxiliary filename                  */

  char *fname ;                 /*!< header filename (.hdr or .nii)         */
  char *iname ;                 /*!< image filename  (.img or .nii)         */
  int   iname_offset ;          /*!< offset into iname where data starts    */
  int   swapsize ;              /*!< swap unit in image data (might be 0)   */
  int   byteorder ;             /*!< byte order on disk (MSB_ or LSB_FIRST) */
  void *data ;                  /*!< pointer to data: nbyper*nvox bytes     */

  int                num_ext ;  /*!< number of extensions in ext_list       */
  nifti1_extension * ext_list ; /*!< array of extension structs (with data) */

} nifti_image ;



/* struct for return from nifti_image_read_bricks() */
typedef struct {
  int     nbricks;    /* the number of allocated pointers in 'bricks' */
  int     bsize;      /* the length of each data block, in bytes      */
  void ** bricks;     /* array of pointers to data blocks             */
} nifti_brick_list;


/*****************************************************************************/
/*--------------- Prototypes of functions defined in this file --------------*/

char const *nifti_datatype_string   ( const int dt ) ;
char const *nifti_units_string      ( const int uu ) ;
char const *nifti_intent_string     ( const int ii ) ;
char const *nifti_xform_string      ( const int xx ) ;
char const *nifti_slice_string      ( const int ss ) ;
char const *nifti_orientation_string( const int ii ) ;

int   nifti_is_inttype( const int dt ) ;

mat44 nifti_mat44_inverse( const mat44 R ) ;

mat33 nifti_mat33_inverse( const mat33 R ) ;
mat33 nifti_mat33_polar  ( const mat33 A ) ;
float nifti_mat33_rownorm( const mat33 A ) ;
float nifti_mat33_colnorm( const mat33 A ) ;
float nifti_mat33_determ ( const mat33 R ) ;
mat33 nifti_mat33_mul    ( const mat33 A , const mat33 B ) ;

void  nifti_swap_2bytes ( const int n , void * const ar ) ;
void  nifti_swap_4bytes ( const int n , void * const ar ) ;
void  nifti_swap_8bytes ( const int n , void * const ar ) ;
void  nifti_swap_16bytes( const int n , void * const ar ) ;
void  nifti_swap_Nbytes ( const int n , const int siz , void * const ar ) ;

void  swap_nifti_header ( struct nifti_1_header * const h , const int is_nifti ) ;
int   nifti_get_filesize( const char * const pathname ) ;

/* main read/write routines */

nifti_image *nifti_image_read_bricks(char const * const hname , const int nbricks, int * const blist,
                                     nifti_brick_list * const NBL );
int          nifti_image_load_bricks(nifti_image * const nim , int nbricks, int const * blist,
                                     nifti_brick_list * const NBL );
void         nifti_free_NBL( nifti_brick_list * NBL );

nifti_image *nifti_image_read    ( const char * const hname , const int read_data ) ;
int          nifti_image_load    ( nifti_image * const nim ) ;
void         nifti_image_unload  ( nifti_image * const nim ) ;
void         nifti_image_free    ( nifti_image *nim ) ;

int          nifti_read_collapsed_image( nifti_image * const nim, const int dims [8],
                                         void ** data );

void         nifti_image_write   ( nifti_image * const nim ) ;
void         nifti_image_write_bricks(nifti_image * const nim, nifti_brick_list const * const NBL);
void         nifti_image_infodump( nifti_image const * const nim ) ;

void         nifti_disp_lib_hist( void ) ;     /* to display library history */
void         nifti_disp_lib_version( void ) ;  /* to display library version */
int          nifti_disp_matrix_orient( char const * const  mesg, const mat44 mat );

char *       nifti_image_to_ascii  ( nifti_image const * const nim ) ;
nifti_image *nifti_image_from_ascii( char * const str, int * const bytes_read ) ;

size_t       nifti_get_volsize(nifti_image const * const nim) ;

/* basic file operations */
int    nifti_set_filenames(nifti_image * const nim, char const * const prefix, const int check,
                           const int set_byte_order);
char * nifti_makehdrname  (char const * const prefix, const int nifti_type, const int check, const int comp);
char * nifti_makeimgname  (char const * const prefix, const int nifti_type, const int check, const int comp);
int    is_nifti_file      (const char * const hname);
char * nifti_find_file_extension(const char * const name);
int    nifti_validfilename(const char* const fname);

int    disp_nifti_1_header( char const * const info, nifti_1_header const * const hp ) ;
void   nifti_set_debug_level( const int level ) ;

int    valid_nifti_brick_list(nifti_image * const nim , const int nbricks, int const * const blist,
                              const int disp_error);

/* znzFile operations */
znzFile nifti_image_open(const char * hname, char * opts, nifti_image ** nim);
znzFile nifti_image_write_hdr_img(nifti_image * const nim, int write_data,
                                  const char* const opts);
znzFile nifti_image_write_hdr_img2( nifti_image * const nim , int write_opts ,
                   const char* const opts, znzFile imgfile, nifti_brick_list const * const NBL );
size_t  nifti_read_buffer(znzFileConst fp, void* const datatptr, const size_t ntot,
                         nifti_image const * const nim);
int     nifti_write_all_data(znzFileConst fp,nifti_image * const nim,nifti_brick_list const * const NBL);
size_t  nifti_write_buffer(znzFile fp, void *buffer, size_t numbytes);
nifti_image *nifti_read_ascii_image(znzFile fp, char *fname, int flen,
                         int read_data);
znzFile nifti_write_ascii_image(nifti_image * const nim, nifti_brick_list const * const NBL,
                         char const * const opts, int write_data, int leave_open);


void nifti_datatype_sizes( const int datatype , int * const nbyper, int * const swapsize ) ;

void nifti_mat44_to_quatern( const mat44 R ,
                             float * const qb, float * const qc, float * const qd,
                             float * const qx, float * const qy, float * const qz,
                             float * const dx, float * const dy, float * const dz, float * const qfac ) ;

mat44 nifti_quatern_to_mat44(  const float qb,  const float qc,  const float qd,
                               const float qx,  const float qy,  const float qz,
                               const float dx,  const float dy,  const float dz,  const float qfac );

mat44 nifti_make_orthog_mat44(  const float r11,  const float r12,  const float r13 ,
                                const float r21,  const float r22,  const float r23 ,
                                const float r31,  const float r32,  const float r33  ) ;

int nifti_short_order(void) ;              /* CPU byte order */


/* Orientation codes that might be returned from nifti_mat44_to_orientation().*/

#define NIFTI_L2R  1    /* Left to Right         */
#define NIFTI_R2L  2    /* Right to Left         */
#define NIFTI_P2A  3    /* Posterior to Anterior */
#define NIFTI_A2P  4    /* Anterior to Posterior */
#define NIFTI_I2S  5    /* Inferior to Superior  */
#define NIFTI_S2I  6    /* Superior to Inferior  */

void nifti_mat44_to_orientation( const mat44 R , int * const icod, int * const jcod, int * const kcod ) ;

/*--------------------- Low level IO routines ------------------------------*/

char * nifti_findhdrname (const char* fname);
char * nifti_findimgname (const char* fname , int nifti_type);
int    nifti_is_gzfile   (const char* fname);

char * nifti_makebasename(const char* fname);


/* other routines */
struct nifti_1_header   nifti_convert_nim2nhdr(nifti_image const * const nim);
nifti_1_header        * nifti_read_header(char const * const hname, int * const swap, const int check);
nifti_image           * nifti_copy_nim_info(nifti_image const * const src);
nifti_image           * nifti_simple_init_nim(void);
nifti_image           * nifti_convert_nhdr2nim(struct nifti_1_header nhdr,
                                               char const * const fname);

int    nifti_hdr_looks_good        (nifti_1_header const * const hdr);
int    nifti_is_valid_ecode        (const int ecode);
int    nifti_nim_is_valid          (nifti_image * const nim, const int complain);
int    nifti_nim_has_valid_dims    (nifti_image * const nim, const int complain);
int    is_valid_nifti_type         (const int nifti_type);
int    nifti_update_dims_from_array(nifti_image * const nim);
void   nifti_set_iname_offset      (nifti_image * const nim);
int    nifti_set_type_from_names   (nifti_image * const nim);
int    nifti_add_extension(nifti_image * const nim, char const * const data, const int len, const int ecode );
int    nifti_copy_extensions       (nifti_image * const nim_dest,nifti_image const * const nim_src);
int    nifti_free_extensions       (nifti_image * const nim);
int  * nifti_get_intlist           (const int nvals , char const * const str);
char * nifti_strdup                (const char * const str);
int    valid_nifti_extensions      (nifti_image const * const nim);


/*-------------------- Some C convenience macros ----------------------------*/

/* nifti extension codes */
#define NIFTI_ECODE_UNKNOWN   0
#define NIFTI_ECODE_DICOM     2
#define NIFTI_ECODE_AFNI      4
#define NIFTI_ECODE_COMMENT   6
#define NIFTI_ECODE_XCEDE     8
#define NIFTI_MAX_ECODE       8    /* this should match the maximum code */

/* nifti_type file codes */
#define NIFTI_FTYPE_ANALYZE   0
#define NIFTI_FTYPE_NIFTI1_1  1
#define NIFTI_FTYPE_NIFTI1_2  2
#define NIFTI_FTYPE_ASCII     3
#define NIFTI_MAX_FTYPE       3    /* this should match the maximum code */

/*------------------------------------------------------------------------*/
/*-- the rest of these apply only to nifti1_io.c, check for _NIFTI1_IO_C_ */
/*                                                    Feb 9, 2005 [rickr] */
#ifdef _NIFTI1_IO_C_

typedef struct {
    int debug;
} nifti_global_options;

#undef  LNI_FERR /* local nifti file error, to be compact and repetative */
#define LNI_FERR(func,msg,file)                                      \
            fprintf(stderr,"** ERROR (%s): %s '%s'\n",func,msg,file)

#undef  swap_2
#undef  swap_4
#define swap_2(s) nifti_swap_2bytes(1,&(s)) /* s: 2-byte short; swap in place */
#define swap_4(v) nifti_swap_4bytes(1,&(v)) /* v: 4-byte value; swap in place */

                        /***** isfinite() is a C99 macro, which is
                               present in many C implementations already *****/

#undef IS_GOOD_FLOAT
#undef FIXED_FLOAT

#ifdef isfinite       /* use isfinite() to check floats/doubles for goodness */
#  define IS_GOOD_FLOAT(x) isfinite(x)       /* check if x is a "good" float */
#  define FIXED_FLOAT(x)   (isfinite(x) ? (x) : 0)           /* fixed if bad */
#else
#  define IS_GOOD_FLOAT(x) 1                               /* don't check it */
#  define FIXED_FLOAT(x)   (x)                               /* don't fix it */
#endif

#undef  ASSIF                                 /* assign v to *p, if possible */
#define ASSIF(p,v) if( (p)!=NULL ) *(p) = (v)

#undef  MSB_FIRST
#undef  LSB_FIRST
#undef  REVERSE_ORDER
#define LSB_FIRST 1
#define MSB_FIRST 2
#define REVERSE_ORDER(x) (3-(x))    /* convert MSB_FIRST <--> LSB_FIRST */

#define LNI_MAX_NIA_EXT_LEN 100000  /* consider a longer extension invalid */

#endif  /* _NIFTI1_IO_C_ section */
/*------------------------------------------------------------------------*/

/*=================*/
#ifdef  __cplusplus
}
#endif
/*=================*/

#endif /* _NIFTI_IO_HEADER_ */
