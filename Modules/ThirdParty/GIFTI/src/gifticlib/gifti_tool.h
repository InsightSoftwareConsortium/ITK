#ifndef GIFTI_TOOL_H
#define GIFTI_TOOL_H

typedef struct { int len; char ** list; } gt_str_list;
typedef struct { int len; int   * list; } gt_int_list;

typedef struct {
    /* main action flags */
    int           gt_compare;   /* somehow compare 2 datasets           */
    int           gt_copy;      /* copy things between 2 datasets       */
    int           gt_display;   /* display something                    */
    int           gt_write;     /* create output datasets               */
    int           gt_modify;    /* sub-action: to modify datasets       */
    int           gt_test;      /* sub-action: check for valid datasets */

    /* action options */
    int           new_numDA;    /* numDA for new dataset                */
    int           new_intent;   /* intent for new dataset               */
    int           new_dtype;    /* dtype for new dataset                */
    int           new_ndim;     /* num_dims for new dataset             */
    int           new_dims[GIFTI_DARRAY_DIM_LEN];
    int           new_data;     /* allocate data in new dataset         */

    /* modify options */
    int           mod_add_data; /* add data to existing dataset         */
    int           mod_gim_atr;  /* modify gifti attribute               */
    int           mod_gim_meta; /* modify gifti meta data               */
    int           mod_DA_atr;   /* modify DataArray attribute           */
    int           mod_DA_meta;  /* modify DataArray meta data           */
    int           mod_to_float; /* convert all input data to FLOAT32    */

    /* compare options */
    int           comp_gifti;   /* compare structures                   */
    int           comp_data;    /* compare data in DataArrays           */
    int           comp_verb;    /* set verbose level for compare_gifti  */
    int           approx_gifti; /* approximate comparison of structures */

    /* copy options */
    int           copy_gim_meta;/* copy metadata between GIFTI elements */
    int           copy_DA_meta; /* copy metadata between DA elements    */

    /* GIFTI user options */
    int           verb;         /* verbose level                        */
    int           indent;       /* spaces per indent level              */
    int           buf_size;     /* buffer space for expat library       */
    int           b64_check;    /* check level */
    int           update_ok;    /* okay for library to update metadata  */
    int           zlevel;       /* compression level for output data    */

    int           dstore;       /* whether to store read data           */
    int           encoding;     /* encoding for output data             */
    int           set_extern;   /* set data to be external files        */
    int           show_gifti;   /* display the gifti_image              */

    char        * ofile_1D;     /* 1D output filename                   */
    char        * ofile_asc;    /* 'asc' output filename                */
    char        * ofile_gifti;  /* GIFTI output filename                */

    gt_int_list   DAlist;       /* list of DA indices to operate on     */
    gt_int_list   DAlistr;      /* list of DataArray indices to read    */
    gt_int_list   DAmodlist;    /* list of DA indices for modification  */
    gt_str_list   gim_atrs;
    gt_str_list   gim_meta;
    gt_str_list   DA_atrs;
    gt_str_list   DA_meta;
    gt_str_list   ext_files;    /* external files as data source        */
    gt_str_list   infiles;
} gt_opts;

#undef CHECK_NEXT_OPT
#define CHECK_NEXT_OPT(n,m,str)                                     \
   do { if ( (n) >= (m) ) {                                          \
           fprintf(stderr,"** option '%s': missing parameter\n",str); \
           fprintf(stderr,"   consider: '-help' option\n");            \
           return 1;      }                                             \
      } while(0)

#undef CHECK_NEXT_OPT2
#define CHECK_NEXT_OPT2(n,m,s1,s2)                                        \
   do { if ( (n) >= (m) ) {                                                \
           fprintf(stderr,"** option '%s': missing parameter '%s'\n",s1,s2);\
           fprintf(stderr,"   consider: '-help' option\n");                  \
           return 1;      }                                                   \
      } while(0)


/* protos */
gifti_image * gt_read_dataset(gt_opts * opts, char * fname);

int gt_compare       (gt_opts * opts);
int gt_copy          (gt_opts * opts);
int gt_display       (gt_opts *);
int gt_modify_dset   (gt_opts *, gifti_image * gim);
int gt_test          (gt_opts *);
int gt_write         (gt_opts *);
int gt_write_dataset (gt_opts *, gifti_image * gim);


int ewrite_data_line (void *, int, int, int, int, int, FILE *);
int ewrite_many_lines(void **, int, long long, long long, int, FILE *);
int write_1D_file    (giiDataArray **, int, char *, int);
int write_as_asc     (gifti_image *, char *);
int write_surf_file  (giiDataArray *, giiDataArray *, char *, int);

#endif /* GIFTI_TOOL_H */
