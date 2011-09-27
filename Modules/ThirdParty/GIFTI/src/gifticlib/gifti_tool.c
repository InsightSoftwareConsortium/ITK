
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gifti_io.h"
#include "gifti_tool.h"

static char * g_history[] =
{
  "----------------------------------------------------------------------\n"
  "history (of gifti_tool):\n"
  "\n",
  "0.0  28 Dec, 2007\n"
  "     (Rick Reynolds of the National Institutes of Health, SSCC/DIRP/NIMH)\n"
  "     - initial version\n"
  "0.1  03 Jan, 2008: changed structure of program\n",
  "     - can do one of display, write or test (more to come)\n"
  "     - added dset creation ability and options, via -new_dset or MAKE_IM\n"
  "         (options -new_*, for numDA, intent, dtype, ndim, dims, data)\n"
  "     - added AFNI-style DA selection, for input datasets\n"
  "0.2  11 Jan, 2008: added modification functionality\n",
  "     - added option -gifti_dtd_url\n"
  "     - added options -mod_DAs and -read_DAs (replaced -dalist)\n"
  "     - added options -mod_add_data, -mod_DA_atr, -mod_DA_meta,\n"
  "                     -mod_gim_atr, -mod_gim_meta\n"
  "       (modification takes place at dataset read time)\n"
  "     - reformatted help output\n"
  "0.3  16 Jan, 2008:\n",
  "     - added options -gifti_zlib, -gifti_test, -mod_to_float, -no_updates\n"
  "0.4  18 Mar, 2008: added comparison options\n",
  "     - added -compare_gifti, -compare_data, -compare_verb\n"
  "0.5  24 Mar, 2008: -compare_data is now separate from -compare_gifti\n",
  "0.6  28 Mar, 2008: added copy meta options:\n",
  "     - added -copy_gifti_meta, -copy_DA_meta\n"
  "1.0  13 May, 2008: based on release library version 1.0\n",
  "     - added -set_extern_filelist\n"
  "1.1  02 Oct, 2008: mention NITRC web site in help\n"
  "1.2  17 Apr, 2009: added -set_extern_filelist help and more examples\n",
  "1.3  24 Dec, 2009: added -approx_gifti option\n"
};

static char g_version[] = "gifti_tool version 1.3, 24 December 2009";

/* globals: verbosity, for now */
typedef struct { int verb; } gt_globs;
gt_globs G = { 1 };

/* local prototypes */
static int add_to_int_list(gt_int_list * ilist, int val);
static int add_to_str_list(gt_str_list * slist, char * str);
static int disp_gt_opts(char * mesg, gt_opts * opts, FILE * stream);
static int free_gt_opts(gt_opts * opts);
static int init_opts(gt_opts * opts);
static int show_help(void);
static int show_hist(void);
static int show_str_list(const char * mesg, gt_str_list * list, FILE * fp);
static int process_opts(int argc, char *argv[], gt_opts * opts);
static int show_version(void);

/* the main event */
int main( int argc, char * argv[] )
{
    gt_opts       opts;
    int           rv = 0;

    init_opts(&opts);
    rv = process_opts(argc, argv, &opts);
    if      ( rv < 0 ) return 1;        /* non-zero means terminate */
    else if ( rv > 0 ) return 0;

    /* choose top-level operation to perform */
    if     ( opts.gt_display ) rv = gt_display(&opts);
    else if( opts.gt_compare ) rv = gt_compare(&opts);
    else if( opts.gt_copy )    rv = gt_copy(&opts);
    else if( opts.gt_write )   rv = gt_write(&opts);
    else                       rv = gt_test(&opts);

    free_gt_opts(&opts);

    return rv;
}

/* process the user options
 *
 * return  1 : success, but exit program
 *         0 : success, continue
 *        -1 : failure, terminate
*/
static int process_opts(int argc, char *argv[], gt_opts * opts)
{
    int ac, c;

    if( argc <= 1 ) { show_help(); return 1; }

    for( ac = 1; ac < argc; ac++ )
    {
        /* terminal options, verbose, then alphabetical */
        if( !strcmp(argv[ac], "-help") ) {
            show_help();
            return 1;
        } else if( !strcmp(argv[ac], "-hist") ) {
            show_hist();
            return 1;
        } else if( !strcmp(argv[ac], "-ver") ) {
            show_version();
            return 1;
        } else if( !strcmp(argv[ac], "-gifti_dtd_url") ) {
            gifti_disp_dtd_url();
            return 1;
        } else if( !strcmp(argv[ac], "-gifti_hist") ) {
            gifti_disp_lib_hist();
            return 1;
        } else if( !strcmp(argv[ac], "-gifti_ver") ) {
            gifti_disp_lib_version();
            return 1;
        } else if( !strcmp(argv[ac], "-gifti_zlib") ) {
            printf("library compiled %s ZLIB\n",
                   GIFTI_COMP_WITH_ZLIB ? "with" : "without");
            return 1;
        }

        /* do this early, in case it is wanted for other options */
        else if( !strcmp(argv[ac], "-verb") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-verb");
            opts->verb = atoi(argv[ac]);
            G.verb = opts->verb;
        }

        /* now alphabetical */
        else if( !strcmp(argv[ac], "-approx_gifti") ) {
            opts->approx_gifti = 1;
            opts->gt_compare = 1;
        } else if( !strcmp(argv[ac], "-b64_check") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-b64_check");
            if     ( !strcmp(argv[ac], "NONE" ) )
                        opts->b64_check = GIFTI_B64_CHECK_NONE;
            else if( !strcmp(argv[ac], "DETECT") )
                        opts->b64_check = GIFTI_B64_CHECK_DETECT;
            else if( !strcmp(argv[ac], "COUNT") )
                        opts->b64_check = GIFTI_B64_CHECK_COUNT;
            else if( !strcmp(argv[ac], "SKIP" ) )
                        opts->b64_check = GIFTI_B64_CHECK_SKIP;
            else if( !strcmp(argv[ac], "SKIPnCOUNT" ) )
                        opts->b64_check = GIFTI_B64_CHECK_SKIPNCOUNT;
            else {
                fprintf(stderr,"** invalid parm to -b64_check: %s\n",argv[ac]);
                return -1;
            }
        } else if( !strcmp(argv[ac], "-buf_size") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-buf_size");
            opts->buf_size = atoi(argv[ac]);
        /* compare options */
        } else if( !strcmp(argv[ac], "-compare_data") ) {
            opts->comp_data = 1;
            opts->gt_compare = 1;
        } else if( !strcmp(argv[ac], "-compare_gifti") ) {
            opts->comp_gifti = 1;
            opts->gt_compare = 1;
        } else if( !strcmp(argv[ac], "-compare_verb") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-compare_verb");
            opts->comp_verb = atoi(argv[ac]);
        /* copy options */
        } else if( !strcmp(argv[ac], "-copy_gifti_meta") ) {
            opts->copy_gim_meta = 1;
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-copy_gifti_meta");
            if( add_to_str_list(&opts->gim_meta, argv[ac] ) ) return -1;
        } else if( !strcmp(argv[ac], "-copy_DA_meta") ) {
            opts->copy_DA_meta = 1;
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-copy_DA_meta");
            if( add_to_str_list(&opts->DA_meta, argv[ac] ) ) return -1;
        } else if( !strcmp(argv[ac], "-DA_index_list") ) {
            ac++;
            for( c = 0; (ac < argc) && (argv[ac][0] != '-'); ac++, c++ )
               if( add_to_int_list(&opts->DAlist, atoi(argv[ac])) ) return -1;
            if( G.verb > 1 )
                fprintf(stderr,"+d have %d DA indices\n", c);
            if( opts->DAlist.len == 0 ) {
                fprintf(stderr,"** no DA indices with -DA_index_list'\n");
                return -1;
            }
            /* and back up if we've looked too far */
            if( ac < argc && argv[ac][0] == '-') ac--;
        } else if( !strcmp(argv[ac], "-encoding") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-encoding");
            if     ( !strcmp(argv[ac], "ASCII" ) )
                opts->encoding = GIFTI_ENCODING_ASCII;
            else if( !strcmp(argv[ac], "BASE64") )
                opts->encoding = GIFTI_ENCODING_B64BIN;
            else if( !strcmp(argv[ac], "BASE64GZIP") )
                opts->encoding = GIFTI_ENCODING_B64GZ;
            else {
                fprintf(stderr,"** invalid parm to -encoding: %s\n",argv[ac]);
                return -1;
            }
        } else if( !strcmp(argv[ac], "-gifti_test") ) {
            opts->gt_test = 1;
        } else if( !strcmp(argv[ac], "-indent") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-indent");
            opts->indent = atoi(argv[ac]);
        } else if( !strncmp(argv[ac], "-infile", 7) ) { /* maybe infiles... */
            ac++;
            for( c = 0; (ac < argc) && (argv[ac][0] != '-'); ac++, c++ )
               if( add_to_str_list(&opts->infiles, argv[ac]) ) return -1;
            if( G.verb > 1 )
                fprintf(stderr,"+d have %d infile names\n", c);
            if( opts->infiles.len == 0 ) {
                fprintf(stderr,"** no filenames with '-infiles'\n");
                return -1;
            }
            /* and back up if we've looked too far */
            if( ac < argc && argv[ac][0] == '-') ac--;
        } else if( !strcmp(argv[ac], "-mod_add_data") ) {
            opts->mod_add_data = 1;
        } else if( !strcmp(argv[ac], "-mod_DA_atr") ) {
            opts->mod_DA_atr = 1;
            ac++;
            if( ac > argc-2 || argv[ac][0] == '-' || argv[ac+1][0] == '-' ) {
                fprintf(stderr,"** option -mod_DA_atr requires 2 arguments\n");
                return -1;
            }
            if( add_to_str_list(&opts->DA_atrs, argv[ac] ) ||
                add_to_str_list(&opts->DA_atrs, argv[ac+1] ) )
                return -1;
            ac++;  /* and consume last arg */
        } else if( !strcmp(argv[ac], "-mod_DA_meta") ) {
            opts->mod_DA_meta = 1;
            ac++;
            if( ac > argc-2 || argv[ac][0] == '-' || argv[ac+1][0] == '-' ) {
                fprintf(stderr,"** option -mod_DA_meta requires 2 arguments\n");
                return -1;
            }
            if( add_to_str_list(&opts->DA_meta, argv[ac] ) ||
                add_to_str_list(&opts->DA_meta, argv[ac+1] ) )
                return -1;
            ac++;  /* and consume last arg */
        } else if( !strcmp(argv[ac], "-mod_DAs") ) {
            ac++;
            for( c = 0; (ac < argc) && (argv[ac][0] != '-'); ac++, c++ )
               if(add_to_int_list(&opts->DAmodlist, atoi(argv[ac]))) return -1;
            if( G.verb > 1 )
                fprintf(stderr,"+d have %d DA mod indices\n", c);
            if( opts->DAmodlist.len == 0 ) {
                fprintf(stderr,"** no DA indices with '-mod_DAs'\n");
                return -1;
            }
            /* and back up if we've looked too far */
            if( ac < argc && argv[ac][0] == '-') ac--;
        } else if( !strcmp(argv[ac], "-mod_gim_atr") ) {
            opts->mod_gim_atr = 1;
            ac++;
            if( ac > argc-2 || argv[ac][0] == '-' || argv[ac+1][0] == '-' ) {
                fprintf(stderr,"** option -mod_gim_atr requires 2 args\n");
                return -1;
            }
            if( add_to_str_list(&opts->gim_atrs, argv[ac] ) ||
                add_to_str_list(&opts->gim_atrs, argv[ac+1] ) )
                return -1;
            ac++;  /* and consume last arg */
        } else if( !strcmp(argv[ac], "-mod_gim_meta") ) {
            opts->mod_gim_meta = 1;
            ac++;
            if( ac > argc-2 || argv[ac][0] == '-' || argv[ac+1][0] == '-' ) {
                fprintf(stderr,"** option -mod_gim_meta requires 2 args\n");
                return -1;
            }
            if( add_to_str_list(&opts->gim_meta, argv[ac] ) ||
                add_to_str_list(&opts->gim_meta, argv[ac+1] ) )
                return -1;
            ac++;  /* and consume last arg */
        } else if( !strcmp(argv[ac], "-mod_to_float") ) {
            opts->mod_to_float = 1;
        } else if( !strcmp(argv[ac], "-new_dset") ) {
            if( add_to_str_list(&opts->infiles, "MAKE_IM") ) return -1;
        } else if( !strcmp(argv[ac], "-new_numDA") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-new_numDA");
            opts->new_numDA = atol(argv[ac]);
        } else if( !strcmp(argv[ac], "-new_intent") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-new_intent");
            opts->new_intent = gifti_intent_from_string(argv[ac]);
            if( !gifti_intent_is_valid(opts->new_intent) ) {
                fprintf(stderr,"** invalid intent '%s'\n",argv[ac]);
                return -1;
            }
        } else if( !strcmp(argv[ac], "-new_dtype") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-new_dtype");
            opts->new_dtype = gifti_str2datatype(argv[ac]);
            if( opts->new_dtype == DT_UNKNOWN ) {
                fprintf(stderr,"** invalid datatype '%s'\n",argv[ac]);
                return -1;
            }
        } else if( !strcmp(argv[ac], "-new_ndim") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-new_ndim");
            opts->new_ndim = atol(argv[ac]);
        } else if( !strcmp(argv[ac], "-new_dims") ) {
            ac++;
            for( c = 0; ac < argc && c < GIFTI_DARRAY_DIM_LEN
                                  && argv[ac][0] != '-'; ac++, c++ )
               opts->new_dims[c] = atol(argv[ac]);
            if( c < GIFTI_DARRAY_DIM_LEN ) {
                fprintf(stderr, "** -new_dims have only %d of %d dims\n",
                                c, GIFTI_DARRAY_DIM_LEN);
                return -1;
            }
            ac--;  /* You've gone too far, go to your room! */
        } else if( !strcmp(argv[ac], "-new_data") ) {
            opts->new_data = 1;
        } else if( !strcmp(argv[ac], "-no_data") ) {
            opts->dstore = 0;
        } else if( !strcmp(argv[ac], "-no_updates") ) {
            opts->update_ok = 0;
        } else if( !strcmp(argv[ac], "-read_DAs") ) {
            ac++;
            for( c = 0; (ac < argc) && (argv[ac][0] != '-'); ac++, c++ )
               if( add_to_int_list(&opts->DAlistr, atoi(argv[ac])) ) return -1;
            if( G.verb > 1 )
                fprintf(stderr,"+d have %d DA indices names\n", c);
            if( opts->DAlistr.len == 0 ) {
                fprintf(stderr,"** no DA indices with -read_DAs'\n");
                return -1;
            }
            /* and back up if we've looked too far */
            if( ac < argc && argv[ac][0] == '-') ac--;
        } else if( !strcmp(argv[ac], "-set_extern_filelist") ) {
            opts->set_extern = 1;
            ac++;
            for( c = 0; (ac < argc) && (argv[ac][0] != '-'); ac++, c++ )
               if( add_to_str_list(&opts->ext_files, argv[ac]) ) return -1;
            if( G.verb > 1 )
                fprintf(stderr,"+d have %d external files\n", c);
            if( opts->ext_files.len == 0 ) {
                fprintf(stderr,
                        "** no external files with -set_extern_filelist\n");
                return -1;
            }
            /* and back up if we've looked too far */
            if( ac < argc && argv[ac][0] == '-') ac--;
        } else if( !strcmp(argv[ac], "-show_gifti") ) {
            opts->gt_display = 1;
            opts->show_gifti = 1;
        } else if( !strcmp(argv[ac], "-write_1D") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-write_1D");
            opts->ofile_1D = argv[ac];
        } else if( !strcmp(argv[ac], "-write_asc") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-write_asc");
            opts->ofile_asc = argv[ac];
        } else if( !strcmp(argv[ac], "-write_gifti") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-write_gifti");
            opts->ofile_gifti = argv[ac];
        } else if( !strcmp(argv[ac], "-zlevel") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-zlevel");
            opts->zlevel = atoi(argv[ac]);
        } else {
            fprintf(stderr,"** unknown option: '%s'\n",argv[ac]);
            return 1;
        }
    }

    /* flat whether we are modifying input data */
    opts->gt_modify = opts->mod_add_data ||
                      opts->mod_gim_atr  || opts->mod_gim_meta ||
                      opts->mod_DA_atr   || opts->mod_DA_meta  ||
                      opts->mod_to_float;

    /* flag whether we have a copying operation */
    opts->gt_copy = opts->copy_gim_meta || opts->copy_DA_meta;

    /* flag whether we have a general write operation (only if not copying) */
    if( !opts->gt_copy )
        opts->gt_write = opts->ofile_1D || opts->ofile_asc || opts->ofile_gifti;

    if( G.verb > 3 ) disp_gt_opts("options read: ", opts, stderr);

    /* be sure we have something to read */
    if( opts->infiles.len <= 0 ) {
        fprintf(stderr,"** missing option: -infiles\n");
        return 1;
    }

    /* only allow one major operation per program execution */
    c = opts->gt_compare + opts->gt_copy + opts->gt_display + opts->gt_write;
    if( c == 0 ) opts->gt_test = 1;
    else if( c > 1 ) {
        fprintf(stderr,"** only 1 major operation allowed, have %d\n", c);
        return 1;
    }

    if( opts->gt_copy && opts->gt_modify ) {
        fprintf(stderr,"** cannot mix copy and modify options\n");
        return 1;
    }

    /* apply any XML user options
     * (non-zero defaults: verb, zlevel -1)
     */
    if( opts->verb      !=  1 ) gifti_set_verb(opts->verb);
    if( opts->indent    != -1 ) gifti_set_indent(opts->indent);
    if( opts->buf_size        ) gifti_set_xml_buf_size(opts->buf_size);
    if( opts->b64_check       ) gifti_set_b64_check(opts->b64_check);
    if( opts->update_ok != -1 ) gifti_set_update_ok(opts->update_ok);
    if( opts->zlevel    != -1 ) gifti_set_zlevel(opts->zlevel);

    return 0;
}

static int free_gt_opts(gt_opts * opts)
{
    if( opts->DAlist.len  > 0 && opts->DAlist.list  ) free(opts->DAlist.list);
    if( opts->DAlistr.len > 0 && opts->DAlistr.list ) free(opts->DAlistr.list);
    if( opts->DAmodlist.len>0&&opts->DAmodlist.list) free(opts->DAmodlist.list);

    if( opts->gim_atrs.len>0 && opts->gim_atrs.list ) free(opts->gim_atrs.list);
    if( opts->gim_meta.len>0 && opts->gim_meta.list ) free(opts->gim_meta.list);
    if( opts->DA_atrs.len > 0 && opts->DA_atrs.list ) free(opts->DA_atrs.list);
    if( opts->DA_meta.len > 0 && opts->DA_meta.list ) free(opts->DA_meta.list);

    if( opts->infiles.len > 0 && opts->infiles.list ) free(opts->infiles.list);

    opts->DAlist.len = 0;    opts->DAlist.list = NULL;
    opts->DAlistr.len = 0;   opts->DAlistr.list = NULL;
    opts->DAmodlist.len = 0; opts->DAmodlist.list = NULL;

    opts->gim_atrs.len = 0;  opts->gim_atrs.list = NULL;
    opts->gim_meta.len = 0;  opts->gim_meta.list = NULL;
    opts->DA_atrs.len = 0;   opts->DA_atrs.list = NULL;
    opts->DA_meta.len = 0;   opts->DA_meta.list = NULL;

    opts->infiles.len = 0;   opts->infiles.list = NULL;

    return 0;
}


int gt_display(gt_opts * opts)
{
    gifti_image * gim;
    int           c, rv = 0;

    if( opts->infiles.len < 1 ) {
        fprintf(stderr,"** no datasets to display\n");
        return 1;
    }

    /* just display any dataset for now */
    opts->show_gifti = 1;

    for( c = 0; c < opts->infiles.len; c++ ) {
        gim = gt_read_dataset(opts, opts->infiles.list[c]);
        if( !gim ) {
            fprintf(stderr,"** gt_display: failed to read '%s'\n",
                           opts->infiles.list[c]);
            rv = 1;
        }
        else gifti_free_image(gim);
    }

    return rv;
}

/* compare gifti structures and/or included data */
int gt_compare(gt_opts * opts)
{
    gifti_image * gimA;
    gifti_image * gimB;
    int           rv0 = 0, rv1 = 0;

    if( opts->infiles.len != 2 ) {
        fprintf(stderr,"** must have exactly 2 gifti_images files to test\n");
        return 1;
    }

    gimA = gt_read_dataset(opts, opts->infiles.list[0]);
    gimB = gt_read_dataset(opts, opts->infiles.list[1]);

    if( !gimA || !gimB ) { /* if failure, make no comparison */
        gifti_free_image(gimA);
        gifti_free_image(gimB);
        return -1;
    }

    if( opts->comp_gifti || opts->comp_data ) {
        if( opts->comp_gifti ) {
            rv0 = gifti_compare_gifti_images(gimA, gimB, 0, opts->comp_verb);
            if( !rv0 && opts->comp_verb > 0 )
                printf("++ no differences between gifti_images\n");
        }
        if( opts->comp_data ) {
            rv1 = gifti_compare_gifti_data(gimA, gimB, opts->comp_verb);
            if( !rv1 && opts->comp_verb > 0 )
                printf("++ no data differences between gifti_images\n");
        }

        rv0 |= rv1;
    }

    if( opts->approx_gifti ) {
            /* return value of approx is opposite that of compare */
            rv0 = gifti_approx_gifti_images(gimA, gimB, 1, opts->comp_verb);
            if( rv0 && opts->comp_verb > 0 )
                printf("++ gifti_images are approximately equal\n");
        rv0 = !rv0;  /* invert return value for exit status */
    }

    gifti_free_image(gimA);
    gifti_free_image(gimB);

    return rv0;
}

/* copy MetaData between GIFTI elements or some DataArray elements */
int gt_copy(gt_opts * opts)
{
    gifti_image  * src;
    gifti_image  * dest;
    char        ** names;
    int            c, rv = 0;

    if( opts->infiles.len != 2 ) {
        fprintf(stderr,"** copy operation requires exactly 2 gifti_images\n");
        return 1;
    }

    if( !opts->ofile_gifti ) {
        fprintf(stderr,"** missing output filename for copy operation\n");
        return 1;
    }

    src = gt_read_dataset(opts, opts->infiles.list[0]);
    dest = gt_read_dataset(opts, opts->infiles.list[1]);

    /* if we fail to read or the metadata is not valid, bail */
    if( !src || !dest || !gifti_valid_nvpairs(&src->meta, 1)
                      || !gifti_valid_nvpairs(&dest->meta, 1) )
    {
        gifti_free_image(src);
        gifti_free_image(dest);
        return -1;
    }

    /* first go for GIFTI meta */
    if(opts->copy_gim_meta) {
        /* if ALL, copy everything, else get what's in the list */
        if(opts->gim_meta.len==1 && !strcmp(opts->gim_meta.list[0],"ALL"))
        {
            names = src->meta.name;
            for( c = 0; c < src->meta.length; c++ )
                rv |= gifti_copy_gifti_meta(dest, src, names[c]);
        } else {
            names = opts->gim_meta.list;
            for( c = 0; c < opts->gim_meta.len; c++ )
                rv |= gifti_copy_gifti_meta(dest,src,names[c]);
        }
    }

    if(opts->copy_DA_meta) {
        /* if ALL, copy everything, else get what's in the list */
        if(opts->DA_meta.len==1 && !strcmp(opts->DA_meta.list[0],"ALL")) {
            if(src->numDA != dest->numDA || src->numDA <= 0) {
                fprintf(stderr,"** bad numDA for DA MD copy, %d, %d\n",
                        src->numDA, dest->numDA);
                rv = 1;
            } else if(!src->darray || !dest->darray) {
                fprintf(stderr,"** invalid darray pointers for copy\n");
                rv = 1;
            } else {  /* all seems well, copy all meta from src to dest */
                for( c = 0; c < src->numDA; c++ )
                    rv |= gifti_copy_all_DA_meta(dest->darray[c],
                                                 src->darray[c]);
            }
        } else {
            names = opts->DA_meta.list;
            for( c = 0; c < opts->DA_meta.len; c++ )
                rv |= gifti_copy_DA_meta_many(dest, src, names[c],
                                          opts->DAlist.list, opts->DAlist.len);
        }
    }

    gt_write_dataset(opts, dest);

    gifti_free_image(src);
    gifti_free_image(dest);

    return rv;
}

int gt_test(gt_opts * opts)
{
    gifti_image * gim;
    int           c, rv = 0;

    if( opts->infiles.len < 1 ) {
        fprintf(stderr,"** no datasets to test\n");
        return 1;
    }

    /* add more test later, now we just try to read */
    for( c = 0; c < opts->infiles.len; c++ ) {
        gim = gt_read_dataset(opts, opts->infiles.list[c]);
        if( !gim ) {
            fprintf(stderr,"** gt_test: failed to read '%s'\n",
                           opts->infiles.list[c]);
            rv = 1;
        }
        else gifti_free_image(gim);
    }

    return rv;
}

/* output is desired, one of:
 *
 *   - GIFTI dataset
 *   - 1D file as text data
 *   - .asc file, as a FreeSurfer style geometry dataset
 *
 * input: there can be only one (immortal?  Sean Connery?)
 */
int gt_write(gt_opts * opts)
{
    gifti_image * gim;
    int           rv;

    if( opts->infiles.len > 1 ) {
        fprintf(stderr,"** when writing, only one input dataset is allowed\n");
        return 1;
    }

    /* actually read the dataset */
    gim = gt_read_dataset(opts, opts->infiles.list[0]);
    if( !gim ){ fprintf(stderr,"** failed gifti_read_da_list()\n"); return 1; }

    rv = gt_write_dataset(opts, gim);

    /* clean up */
    gifti_free_image(gim);  gim = NULL;

    return rv;
}

/* apply encoding, and allow other formats */
int gt_write_dataset(gt_opts * opts, gifti_image * gim)
{
    int c;

    if(!gim) {
        if(opts->verb) fprintf(stderr,"** trying to write NULL dataset\n");
        return 1;
    }

    if(opts->verb > 1)
        fprintf(stderr,"++ gt_write_dataset: gim %s, 1D %s, asc %s\n",
                G_CHECK_NULL_STR(opts->ofile_gifti),
                G_CHECK_NULL_STR(opts->ofile_1D),
                G_CHECK_NULL_STR(opts->ofile_asc));

    /* possibly adjust encoding */
    if(opts->encoding > GIFTI_ENCODING_UNDEF &&
       opts->encoding <= GIFTI_ENCODING_MAX)
        for( c = 0; c < gim->numDA; c++ ) {
            /* when modifying enconding, if we are changing _from_ external,
               then clear the external filename and offset */
            if(gim->darray[c]->encoding == GIFTI_ENCODING_EXTBIN &&
               opts->encoding != GIFTI_ENCODING_EXTBIN) {
                if(gim->darray[c]->ext_fname) {
                    if(opts->verb>2) fprintf(stderr,"-- deleting ext_fname\n");
                    free(gim->darray[c]->ext_fname);
                    gim->darray[c]->ext_fname = NULL;
                }
                gim->darray[c]->ext_offset = 0;
            }

            /* actually make the user-requested change */
            if( gim->darray[c]->encoding )
                gim->darray[c]->encoding = opts->encoding;
        }

    if(opts->ofile_gifti) {
        /* maybe user wants to point to external files */
        if(opts->set_extern) {
            if(opts->ext_files.len <= 0 || !opts->ext_files.list) {
                fprintf(stderr,"** no file list to set as external\n");
                return 1;
            }
            if(gifti_set_extern_filelist(gim, opts->ext_files.len,
                                              opts->ext_files.list))
                return 1;
        }
        gifti_write_image(gim,opts->ofile_gifti,opts->dstore);
    }
    if(opts->ofile_1D)  write_1D_file(gim->darray,gim->numDA,opts->ofile_1D,1);
    if(opts->ofile_asc) write_as_asc(gim, opts->ofile_asc);

    return 0;
}

/* read one GIFTI dataset
 *
 * The default is to just call gifti_read_da_list(), but...
 * if name == MAKE_IM   : create a new dataset
 * else if we have name : read one dataset
 *
 * Note that name may have the form dset[int list], where the integer
 * list is used to create the DataArray list.
 *
 * e.g.  dset.gii[5..12(3),0,$]
 *
 *       this would select DA elements 5,8,11,0,numDA-1
 *
 * Note that the DA list selection requires reading the dataset twice,
 * first to compute the number of DA elements.
 */
gifti_image * gt_read_dataset(gt_opts * opts, char * fname)
{
    gifti_image * gim;
    char        * fcopy = NULL, * iptr, * infile = fname;
    int         * dalist = NULL, numDA = -1;

    if( !fname || !*fname ) {
        fprintf(stderr,"** gt_read_dataset: no filename to read\n");
        return NULL;
    }

    /* first case, create a new image (fname == MAKE_IM) */
    if( !strcmp(fname, "MAKE_IM") ) {
        if( opts->verb > 1 ) fprintf(stderr,"++ creating new GIFTI dataset\n");

        gim = gifti_create_image(opts->new_numDA, opts->new_intent,
                                 opts->new_dtype, opts->new_ndim,
                                 opts->new_dims,  opts->new_data);

        if( opts->gt_modify && gt_modify_dset(opts, gim) ) {
            if( opts->verb > 1 )
                fprintf(stderr,"** bad modification to new dset, failing...\n");
            gifti_free_image(gim);
            return NULL;
        }

        if( opts->show_gifti ) gifti_disp_gifti_image("dset MAKE_IM :",gim,1);

        return gim;
    }

    /* otherwise, see if there is an int list, before using gifti_read */

    if( strchr(fname, '[') ) { /* then create an int list */
        fcopy = gifti_strdup(fname);
        infile = fcopy;         /* store for later */
        iptr = strchr(fcopy, '[');

        if(opts->verb>2) fprintf(stderr,"-- getting DA list from %s\n",iptr);
        *iptr = '\0';   /* don't need the char, but want terminated filename */

        /* read dataset just for numDA */
        numDA = gifti_read_dset_numDA(fcopy);
        if( numDA < 0 ) {
            fprintf(stderr, "** GT_RD: failed to get numDA from '%s'\n", fcopy);
            free(fcopy);
            return NULL;
        }

        dalist = nifti_get_intlist(numDA, iptr+1);
        if( !dalist ) {
            fprintf(stderr,"** GT_RD: bad int list from '%s'\n", iptr+1);
            free(fcopy);
            return NULL;
        }
        if( opts->verb > 2 ) {
            fprintf(stderr,"++ have DA list: ");
            gifti_disp_raw_data(dalist, NIFTI_TYPE_INT32, numDA, 1, stderr);
        }
    }

    /* pass either dalist or from opts */
    if( dalist && numDA > 0 )
        gim = gifti_read_da_list(infile, opts->dstore, dalist+1, dalist[0]);
    else
        gim = gifti_read_da_list(infile, opts->dstore,
                                 opts->DAlistr.list, opts->DAlistr.len);

    /* possibly make modifications */
    if( opts->gt_modify ) gt_modify_dset(opts, gim);

    /* regardless of success, check to free data and return */
    if( dalist ) free(dalist);
    if( fcopy  ) free(fcopy);

    if( opts->show_gifti ) {
        fcopy = (char *)malloc((strlen(fname)+32) * sizeof(char));
        if( !fcopy ) return gim;  /* forget it */
        sprintf(fcopy, "dset '%s' :", fname);

        gifti_disp_gifti_image(fcopy, gim, 1 );
        free(fcopy);
    }

    if( opts->gt_test ) {
        if( gifti_valid_gifti_image(gim, opts->verb > 0) )
            printf("++ gifti_image '%s' is VALID\n", fname);
        else
            printf("++ gifti_image '%s' is INVALID\n", fname);
    }

    return gim;
}

/* init any options that should not default to 0 (so 0 means something,
 * or the default is non-zero) */
static int init_opts(gt_opts * opts)
{
    memset(opts, 0, sizeof(gt_opts));

    /* gt_* should init to 0 */

    /* new flags can be set to something useful (1 DA, no data, ...) */
    opts->new_numDA = 1;
    opts->new_intent = NIFTI_INTENT_NONE;
    opts->new_dtype = NIFTI_TYPE_FLOAT32;
    opts->new_ndim = 0;
    /* opts->new_dims left with zeros */
    opts->new_data = 0;

    opts->comp_verb = 1;

    opts->verb = 1;
    opts->indent = -1;
    opts->dstore = 1;
    opts->update_ok = -1;
    opts->zlevel = -1;

    return 0;
}

static int disp_gt_opts(char * mesg, gt_opts * opts, FILE * stream)
{
    FILE * fp = stream ? stream : stdout;

    if( mesg ) fputs(mesg, fp);

    fprintf(fp, "gt_opts struct:\n"
        "    gt_compare    : %d\n"
        "    gt_display    : %d\n"
        "    gt_write      : %d\n"
        "    gt_modify     : %d\n"
        "    gt_test       : %d\n"
        "\n"
        "    new_numDA     : %d\n"
        "    new_intent    : %d\n"
        "    new_dtype     : %d\n"
        "    new_ndim      : %d\n"
        "    new_dims [%d]  : ",
        opts->gt_compare, opts->gt_display, opts->gt_write,
        opts->gt_modify, opts->gt_test,
        opts->new_numDA, opts->new_intent, opts->new_dtype, opts->new_ndim,
        GIFTI_DARRAY_DIM_LEN
        );

    gifti_disp_raw_data(opts->new_dims, NIFTI_TYPE_INT32,
                        GIFTI_DARRAY_DIM_LEN, 1, fp);
    fprintf(fp,
        "    new_data      : %d\n"
        "\n"
        "    mod_add_data  : %d\n"
        "    mod_gim_atr   : %d\n"
        "    mod_gim_meta  : %d\n"
        "    mod_DA_atr    : %d\n"
        "    mod_DA_meta   : %d\n"
        "    mod_to_float  : %d\n"
        "\n"
        "    comp_data     : %d\n"
        "    comp_verb     : %d\n"
        "\n"
        "    verb          : %d\n"
        "    indent        : %d\n"
        "    buf_size      : %d\n"
        "    b64_check     : %d\n"
        "    update_ok     : %d\n"
        "    zlevel        : %d\n"
        "\n"
        "    dstore        : %d\n"
        "    encoding      : %d\n"
        "    show_gifti    : %d\n"
        "    ofile_1D      : %s\n"
        "    ofile_asc     : %s\n"
        "    ofile_gifti   : %s\n\n",
        opts->new_data, opts->mod_add_data, opts->mod_gim_atr,
        opts->mod_gim_meta, opts->mod_DA_atr, opts->mod_DA_meta,
        opts->mod_to_float,
        opts->comp_data, opts->comp_verb,
        opts->verb, opts->indent, opts->buf_size, opts->b64_check,
        opts->update_ok, opts->zlevel,
        opts->dstore, opts->encoding, opts->show_gifti,
        G_CHECK_NULL_STR(opts->ofile_1D),
        G_CHECK_NULL_STR(opts->ofile_asc),
        G_CHECK_NULL_STR(opts->ofile_gifti));

    /* DataArray index list */
    fprintf(fp, "    DAlist[%d]     : ", opts->DAlist.len);
    if( opts->DAlist.len <= 0 || !opts->DAlist.list )
        fprintf(fp, "<empty>\n");
    else
        gifti_disp_raw_data(opts->DAlist.list, NIFTI_TYPE_INT32,
                            opts->DAlist.len, 1, fp);

    /* DataArray index list */
    fprintf(fp, "    DAlistr[%d]    : ", opts->DAlistr.len);
    if( opts->DAlistr.len <= 0 || !opts->DAlistr.list )
        fprintf(fp, "<empty>\n");
    else
        gifti_disp_raw_data(opts->DAlistr.list, NIFTI_TYPE_INT32,
                            opts->DAlistr.len, 1, fp);

    /* DataArray modification list */
    fprintf(fp, "    DAmodlist[%d]  : ", opts->DAmodlist.len);
    if( opts->DAmodlist.len <= 0 || !opts->DAmodlist.list )
        fprintf(fp, "<empty>\n");
    else
        gifti_disp_raw_data(opts->DAmodlist.list, NIFTI_TYPE_INT32,
                            opts->DAmodlist.len, 1, fp);

    show_str_list("    gim_atrs ", &opts->gim_atrs, fp);
    show_str_list("    gim_meta ", &opts->gim_meta, fp);
    show_str_list("    DA_atrs ", &opts->DA_atrs, fp);
    show_str_list("    DA_meta ", &opts->DA_meta, fp);
    show_str_list("    ext_files ", &opts->ext_files, fp);
    show_str_list("    infiles ", &opts->infiles, fp);

    return 0;
}

static int show_str_list( const char * mesg, gt_str_list * list, FILE * fp )
{
    FILE * stream = fp ? fp : stdout;
    int    c;

    if( mesg ) fprintf(stream, "%-14s", mesg);
    else       fputs("list",stream);

    if( ! list || !list->list || list->len <= 0 ) {
        fprintf(stream, "[0] : <empty>\n");
        return 0;
    }

    fprintf(stream, "[%d] : ", list->len);
    for( c = 0; c < list->len; c++ )
        fprintf(stream, "%s ", list->list[c]);
    fputc('\n', stream);

    return 0;
}

static int show_hist(void)
{
    int c, len = sizeof(g_history)/sizeof(char *);
    for( c = 0; c < len; c++)
        fputs(g_history[c], stdout);
    putchar('\n');
    return 0;
}

static int show_version(void)
{
    puts(g_version);
    return 0;
}

static int show_help()
{
    printf(
    "------------------------------------------------------------\n"
    "gifti_tool  - create, display, modify or compare GIFTI datasets\n"
    "\n"
    "  general examples:\n"
    "\n"
    "    1. read in a GIFTI dataset (set verbose level?  show GIFTI dataset?)\n"
    "\n"
    "         gifti_tool -infile dset.gii\n"
    "         gifti_tool -infile dset.gii -verb 3\n"
    "         gifti_tool -infile dset.gii -show_gifti\n"
    "\n"
    "    2. copy a GIFTI dataset\n"
    "\n"
    "      a. create a simple copy, and check for differences\n"
    "\n"
    "         gifti_tool -infile dset.gii -write_gifti copy.gii\n"
    "         diff dset.gii copy.gii\n"
    "\n"
    "      b. copy only 3 DataArray indices: 4, 0, 5\n"
    "\n"
    "         gifti_tool -infile time_series.gii -write_gifti ts3.gii  \\\n"
    "                    -read_DAs 4 0 5\n"
    "               OR\n"
    "\n"
    "         gifti_tool -infile time_series.gii'[4,0,5]'  \\\n"
    "                    -write_gifti ts3.gii\n"
    "\n"
    "    3. write datasets in other formats\n"
    "\n"
    "      a. FreeSurfer-style .asc surface dataset\n"
    "\n"
    "         gifti_tool -infile pial.gii -write_asc pial.asc\n"
    "\n"
    "      b. .1D time series surface dataset\n"
    "\n"
    "         gifti_tool -infile time_series.gii -write_1D ts.1D\n"
    "\n"
    );
    printf (
    "    4. create a new gifti dataset from nothing, where\n"
    "\n"
    "      a. - the dataset has 3 DataArray elements\n"
    "         - the data will be of type 'short' (NIFTI_TYPE_INT16)\n"
    "         - the intent codes will reflect a t-test\n"
    "         - the data will be 2-dimensional (per DataArray), 5 by 2 shorts\n"
    "         - memory will be allocated for the data (a modification option)\n"
    "         - the result will be written to created.gii\n"
    "\n"
    "         gifti_tool -new_dset                                \\\n"
    "                    -new_numDA 3 -new_dtype NIFTI_TYPE_INT16 \\\n"
    "                    -new_intent NIFTI_INTENT_TTEST           \\\n"
    "                    -new_ndim 2 -new_dims 5 2 0 0 0 0        \\\n"
    "                    -mod_add_data -write_gifti created.gii\n"
    "\n"
    "      b. - the dataset has 12 DataArray elements (40 floats each)\n"
    "         - the data is partitioned over 2 files (so 6*40 floats in each)\n"
    "\n"
    "           ** Note: since dataset creation does not add data (without\n"
    "                    -mod_add_data), this operation will not create or\n"
    "                    try to overwrite the external datafiles.\n"
    "\n"
    "         gifti_tool -new_dset -new_numDA 12                   \\\n"
    "                    -new_ndim 1 -new_dims 40 0 0 0 0 0        \\\n"
    "                    -set_extern_filelist ext1.bin ext2.bin    \\\n"
    "                    -write_gifti points_to_extern.gii\n"
    "\n");
    printf(
    "    5. modify a gifti dataset\n"
    "\n"
    "      a. apply various modifications at the GIFTI level and to all DAs\n"
    "\n"
    "         - set the Version attribute at the GIFTI level\n"
    "         - set 'Date' as GIFTI MetaData, with value of today's date\n"
    "         - set 'Description' as GIFTI MetaData, with some value\n"
    "         - set all DA Intent attributes to be an F-test\n"
    "         - set 'Name' as an attribute of all DAs, with some value\n"
    "         - read created.gii, and write to first_mod.gii\n"
    "\n"
    "         gifti_tool -mod_gim_atr Version 1.0                       \\\n"
    "                    -mod_gim_meta Date \"`date`\"                    \\\n"
    "                    -mod_gim_meta Description 'modified surface'   \\\n"
    "                    -mod_DA_atr Intent NIFTI_INTENT_FTEST          \\\n"
    "                    -mod_DA_meta Name 'same name for all DAs'      \\\n"
    "                    -infile created.gii -write_gifti first_mod.gii\n"
    "\n"
    "      b. modify the 'Name' attribute is DA index #42 only\n"
    "\n"
    "         gifti_tool -mod_DA_meta Name 'data from pickle #42'       \\\n"
    "                    -mod_DAs 42                                    \\\n"
    "                    -infile stats.gii -write_gifti mod_stats.gii\n"
    "\n"
    "      c. set the data to point to a single external data file, without\n"
    "         overwriting the external file on write (so use -no_data), \n"
    "         and where the DataArrays will point to sequential partitions\n"
    "         of the file\n"
    "\n"
    "         gifti_tool -infiles created.gii -no_data          \\\n"
    "                    -set_extern_filelist ex_data.bin       \\\n"
    "                    -write_gifti extern.gii\n"
    "\n"
    );
    printf (
    "      d. convert a POINTSET/TRIANGLE Base64 format dataset to one where\n"
    "         to one where the data is external (raw binary):\n"
    "\n"
    "           gifti_tool -infiles inflated.gii                     \\\n"
    "                      -set_extern_filelist points.data tri.data \\\n"
    "                      -write_gifti inflated.external.gii\n"
    "\n"
    "      e. convert a 5 run time series dataset from internal Base64 format\n"
    "         to one where the data is external (raw binary):\n"
    "\n"
    "         as one external file:\n"
    "\n"
    "           gifti_tool -infiles epi.5runs.gii               \\\n"
    "                      -set_extern_filelist data.5runs.bin  \\\n"
    "                      -write_gifti epi.ext.5runs.gii\n"
    "\n"
    "         as 5 external files (1 per run):\n"
    "\n"
    "           gifti_tool -infiles epi.5runs.gii                      \\\n"
    "                 -set_extern_filelist data.5runs.r{1,2,3,4,5}.bin \\\n"
    "                 -write_gifti epi.ext.5runs.gii\n"
    "\n"
    "      f. convert the previous external dataset back to internal form\n"
    "         (i.e. it should be the same as epi.5runs.gii)\n"
    "\n"
    "           gifti_tool -infiles epi.ext.5runs.gii      \\\n"
    "                      -encoding BASE64                \\\n"
    "                      -write_gifti epi.int.5runs.gii\n"
    "\n"
    );
    printf (
    "    6. compare 2 gifti datasets\n"
    "\n"
    "      a. compare GIFTI structures, compare data, and report all diffs\n"
    "\n"
    "         gifti_tool -compare_gifti -compare_data -compare_verb 3 \\\n"
    "                    -infiles created.gii first_mod.gii\n"
    "\n"
    "      b. report approximate comparison: focusing on data, but allowing\n"
    "         for small, fractional differences varying per datatype\n"
    "\n"
    "         gifti_tool -approx_gifti -compare_verb 3 \\\n"
    "                    -infiles created.gii first_mod.gii\n"
    "\n"
    "    7. copy MetaData from one dataset to another\n"
    "       (any old Value will be replaced if the Name already exists)\n"
    "\n"
    "         - copy every (ALL) MetaData element at the GIFTI level\n"
    "         - copy MetaData named 'Label' per DataArray element\n"
    "         - only apply DataArray copies to indices 0, 3 and 6\n"
    "         - first input file is the source, second is the destination\n"
    "         - write the modified 'destination.gii' dataset to meta_copy.gii\n"
    "\n"
    "         gifti_tool -copy_gifti_meta ALL                   \\\n"
    "                    -copy_DA_meta Label                    \\\n"
    "                    -DA_index_list 0 3 6                   \\\n"
    "                    -infiles source.gii destination.gii    \\\n"
    "                    -write_gifti meta_copy.gii\n"
    "\n"
    "----------------------------------------------------------------------\n"
    );
    printf (
    "\n"
    "  (all warranties are void in Montana, and after 4 pm on Tuesdays)\n"
    "\n"
    "----------------------------------------------------------------------\n"
    "  informational options:\n"
    "\n"
    "     -help             : display this help\n"
    "     -hist             : display the modification history of gifti_tool\n"
    "     -ver              : display the gifti_tool version\n"
    "     -gifti_hist       : display thd modification history of gifticlib\n"
    "     -gifti_ver        : display gifticlib version\n"
    "     -gifti_dtd_url    : display the gifti DTD URL\n"
    "     -gifti_zlib       : display whether the zlib is linked in library\n"
    "\n"
    );
    printf (
    "  ----------------------------------------\n"
    "  general/input options\n"
    "\n"
    "     -b64_check   TYPE : set method for checking base64 errors\n"
    "\n"
    "           e.g. -b64_check COUNT\n"
    "\n"
    "           This option sets the preference for how to deal with errors\n"
    "           in Base64 encoded data (whether compressed or not).  The\n"
    "           default is SKIPnCOUNT, which skips any illegal characters,\n"
    "           and reports a count of the number found.\n"
    "\n"
    "               TYPE = NONE       : no checks - assume all is well\n"
    "               TYPE = DETECT     : report whether errors were found\n"
    "               TYPE = COUNT      : count the number of bad chars\n"
    "               TYPE = SKIP       : ignore any bad characters\n"
    "               TYPE = SKIPnCOUNT : ignore but count bad characters\n"
    "\n"
    "           This default adds perhaps 10%% to the reading time.\n"
    "\n"
    "     -buf_size    SIZE : set the buffer size (given to expat library)\n"
    "\n"
    "           e.g. -buf_size 1024\n"
    "\n"
    "     -DA_index_list I0 I1 ... : specify a list of DataArray indices\n"
    "\n"
    "           e.g. -DA_index_list 0\n"
    "           e.g. -DA_index_list 0 17 19\n"
    "\n"
    "           This option is used to specify a list of DataArray indices\n"
    "           for use via some other option (such as -copy_DA_meta).\n"
    "\n"
    "           Each DataArray element corresponding to one of the given\n"
    "           indices will have the appropriate action applied, such as\n"
    "           copying a given MetaData element from the source dataset\n"
    "           to the destination dataset.\n"
    "\n"
    "           Note that this differs from -read_DAs, which specifies which\n"
    "           DataArray elements to even read in.  Both options could be\n"
    "           used in the same command, such as if one wanted to copy the\n"
    "           'Name' MetaData from index 17 of a source dataset into the\n"
    "           MetaData of the first DataArray in a dataset with only two\n"
    "           DataArray elements.\n"
    "\n"
    "           e.g. gifti_tool -infiles source.gii dest.gii        \\\n"
    "                           -write_gifti new_dest.gii           \\\n"
    "                           -copy_DA_meta Name                  \\\n"
    "                           -read_DAs 17 17                     \\\n"
    "                           -DA_index_list 0\n"
    "\n"
    "           Note that DA_index_list applies to the indices _after_ the\n"
    "           datasets are read in.\n"
    "\n"
    );
    printf (
    "     -gifti_test       : test whether each gifti dataset is valid\n"
    "\n"
    "           This performs a consistency check on each input GIFTI\n"
    "           dataset.  Lists and dimensions must be consistent.\n"
    "\n"
    "     -infile     INPUT : specify one or more GIFTI datasets as input\n"
    "\n"
    "           e.g. -input pial.gii\n"
    "           e.g. -input run1.gii run2.gii\n"
    "           e.g. -input MAKE_IM                 (create a new image)\n"
    "           e.g. -input run1.gii'[3,4,5]'       (read DAs 3,4,5    )\n"
    "           e.g. -input run1.gii'[0..16(2)]'    (read evens from 0 to 16)\n"
    "           e.g. -input run1.gii'[4..$]'        (read all but 0..3)\n"
    "\n"
    "           There are 2 special ways to specify input.  One is via the\n"
    "           name 'MAKE_IM'.  That 'input' filename tell gifti_tool to\n"
    "           create a new dataset, applying any '-new_*' options to it.\n"
    "\n"
    "               (refer to options: -new_*)\n"
    "\n"
    "           The other special way is to specify which DataArray elements\n"
    "           should be read in, using AFNI-style syntax within '[]'.  The\n"
    "           quotes prevent the shell from interpreting the brackets.\n"
    "\n"
    "           DataArray indices are zero-based.\n"
    "\n"
    "           The list of DAs can be comma-delimited, and can use '..' or\n"
    "           '-' to specify a range, and a value in parentheses to be used\n"
    "           as a step.  The '$' character means the last index (numDA-1).\n"
    "\n"
    );
    printf (
    "     -no_data          : do not read in data\n"
    "\n"
    "           This option means not to read in the Data element in any\n"
    "           DataArray, akin to reading only the header.\n"
    "\n"
    "     -no_updates       : do not allow the library to modify metadata\n"
    "\n"
    "           By default, the library may update some metadata fields, such\n"
    "           as 'gifticlib-version'.  The -no_updates option will prevent\n"
    "           that operation.\n"
    "\n"
    "     -read_DAs s0 ...  : read DataArray list indices s0,... from input\n"
    "\n"
    "           e.g. -read_DAs 0 4 3 3 8\n"
    "           e.g. -input run1.gii -read_DAs 0 2 4 6 8\n"
    "           e.g. -input run1.gii'[0..8(2)]'              (same effect)\n"
    "\n"
    "           Specify a list of DataArray indices to read.  This is a\n"
    "           simplified form of using brackets '[]' with -input names.\n"
    "\n"
    "     -show_gifti       : show final gifti image\n"
    "\n"
    "           Display all of the dataset information on the screen (sans\n"
    "           data).  This includes meta data and all DataArray elements.\n"
    "\n"
    "     -verb        VERB : set verbose level   (default: 1)\n"
    "\n"
    "           e.g. -verb 2\n"
    "\n"
    "           Print extra information to the screen.  The VERB level can\n"
    "           be from 0 to 8, currently.\n"
    "\n"
    "           Level 0 is considered 'quiet' mode, and should only report\n"
    "           serious errors.  Level 1 is the default.\n"
    "\n"
    );
    printf (
    "  ----------------------------------------\n"
    "  output options\n"
    "\n"
    "     -encoding    TYPE : set the data encoding for any output file\n"
    "\n"
    "           e.g. -encoding BASE64GZIP\n"
    "\n"
    "               TYPE = ASCII      : ASCII encoding\n"
    "               TYPE = BASE64     : base64 binary\n"
    "               TYPE = BASE64GZIP : base64 compressed binary\n"
    "\n"
    "           This operation can also be performed via -mod_DA_atr:\n"
    "           e.g. -mod_DA_atr Encoding BASE64GZIP\n"
    "\n"
    "     -set_extern_filelist F1 F2 ... : store data in external files\n"
    "\n"
    "           e.g. -set_extern_filelist run.1.data run.2.data run.3.data\n"
    "           e.g. -set_extern_filelist runs.all.data\n"
    "           e.g. -set_extern_filelist points.data triangles.data\n"
    "\n"
    "           Data is normally stored within the XML file as numerical\n"
    "           text or Base64 encoded raw or compressed data.\n"
    "\n"
    "           With use of this option, users can set to have data stored in\n"
    "           external binary files (neither encoded nor compressed) upon a\n"
    "           write operation.\n"
    "\n"
    "           External file storage is subject to a couple of restrictions:\n"
    "\n"
    "             - GIFTI requires that they are in the same directory\n"
    "\n"
    "             - the library allows multiple DataArrays per file, but each\n"
    "               DataArray within the same file must have the same size\n"
    "               (this is a gifticlib limit, not a GIFTI limit)\n"
    "\n"
    "                 OK : equal data in 1 file\n"
    "                 OK : equal data in k files, numDA is multiple of k\n"
    "                 BAD: equal data in k files, numDA is NOT multiple of k\n"
    "                 OK : points/triangles in 2 files\n"
    "                 BAD: points/triangles in 1 file (sizes differ)\n"
    "\n"
    "           The most basic use of this option is to convert data from\n"
    "           internal to external.  See examples 5d and 5e.\n"
    "\n"
    "           Note that one can also create a GIFTI dataset out of nothing\n"
    "           and use this option to point to existing external data files.\n"
    "           This would help conversion from other dataset formats.  See\n"
    "           example 5c.\n"
    "\n"
    "           Note that one can convert from an external data format to\n"
    "           internal just by modifying the -encoding.  See example 5f.\n"
    "\n"
    );
    printf (
    "     -write_1D    DSET : write out data to AFNI style 1D file\n"
    "\n"
    "           e.g. -write_1D stats.1D\n"
    "\n"
    "           Currently, all DAs need to be of the same datatype.  This\n"
    "           restriction could be lifted if there is interest.\n"
    "\n"
    "     -write_asc   DSET : write out geometry to FreeSurfer style ASC file\n"
    "\n"
    "           e.g. -write_asc pial.asc\n"
    "\n"
    "           To write a surface file in FreeSurfer asc format, it must\n"
    "           contain DataArray elements of intent NIFTI_INTENT_POINTSET\n"
    "           and NIFTI_INTENT_TRIANGLE.  The POINTSET data is written as\n"
    "           node coordinates and the TRIANGLE data as triangles (node\n"
    "           index triplets).\n"
    "\n"
    "     -write_gifti DSET : write out dataset as gifti image\n"
    "\n"
    "           e.g. -write_gifti new.pial.gii\n"
    "\n"
    "     -zlevel     LEVEL : set compression level (-1 or 0..9)\n"
    "\n"
    "           This option sets the compression level used by zlib.  Some\n"
    "           LEVEL values are noteworthy:\n"
    "\n"
    "              -1   : specify to use the default of zlib (currently 6)\n"
    "               0   : no compression (but still needs a few extra bytes)\n"
    "               1   : fastest but weakest compression\n"
    "               6   : default (good speed/compression trade-off)\n"
    "               9   : slowest but strongest compression\n"
    "\n"
    );
    printf (
    "  ----------------------------------------\n"
    "  modification options\n"
    "\n"
    "     These modification options will affect every DataArray element\n"
    "     specified by the -mod_DAs option.  If the option is not used,\n"
    "     then ALL DataArray elements will be affected.\n"
    "\n"
    "     -mod_add_data     : add data to empty DataArray elements\n"
    "\n"
    "           Allocate data in every DataArray element.  Datasets can be\n"
    "           created without any stored data.  This will allocate data\n"
    "           and fill it with zeros of the given type.\n"
    "\n"
    "     -mod_DA_atr  NAME VALUE : set the NAME=VALUE attribute pair\n"
    "\n"
    "           e.g. -mod_DA_atr Intent NIFTI_INTENT_ZSCORE\n"
    "\n"
    "           This option will set the DataArray attribute corresponding\n"
    "           to NAME to the value, VALUE.  Attribute name=value pairs are\n"
    "           specified in the gifti DTD (see -gifti_dtd_url).\n"
    "\n"
    "           One NAME=VALUE pair can be specified per -mod_DA_atr\n"
    "           option.  Multiple -mod_DA_atr options can be used.\n"
    "\n"
    "     -mod_DA_meta NAME VALUE : set the NAME=VALUE pair in DA's MetaData\n"
    "\n"
    "           e.g. -mod_DA_meta Description 'the best dataset, ever'\n"
    "\n"
    "           Add a MetaData entry to each DataArray element for this\n"
    "           NAME and VALUE.  If 'NAME' already exists, the old value\n"
    "           is replaced by VALUE.\n"
    "\n"
    "     -mod_DAs i0 i1 ...      : specify the set of DataArrays to modify\n"
    "\n"
    "           e.g. -mod_DAs 0 4 5\n"
    "\n"
    "           Specify the list of DataArray elements to modify.  All the\n"
    "           -mod_* options apply to this list of DataArray indices.  If\n"
    "           no -mod_DAs option is used, the operations apply to ALL\n"
    "           DataArray elements.\n"
    "\n"
    "           Note that the indices are zero-based, 0 .. numDA-1.\n"
    "\n"
    "     -mod_gim_atr  NAME VALUE : set the GIFTI NAME=VALUE attribute pair\n"
    "\n"
    "           e.g. -mod_gim_atr Version 3.141592\n"
    "\n"
    "           Set the GIFTI element attribute corresponding to NAME to the\n"
    "           value, VALUE.\n"
    "\n"
    "           Given that numDA is computed and version will rarely change,\n"
    "           this option will probably not feel much love.\n"
    "\n"
    "     -mod_gim_meta NAME VALUE : add this pair to the GIFTI MetaData\n"
    "\n"
    "           e.g. -mod_gim_meta date \"`date`\"\n"
    "\n"
    "           Add a MetaData entry to each DataArray element for this\n"
    "           NAME and VALUE pair.  If NAME exists, VALUE will replace\n"
    "           the old value.\n"
    "\n"
    "     -mod_to_float            : change all DataArray data to float\n"
    "\n"
    "           Convert all DataArray elements of all datasets to datatype\n"
    "           NIFTI_TYPE_FLOAT32 (4-byte floats).  If the data does not\n"
    "           actually exist, only the attribute will be set.  Otherwise\n"
    "           all of the data will be converted.  There are some types\n"
    "           for which this operation may not be appropriate.\n"
    "\n"
    );
    printf (
    "  ----------------------------------------\n"
    "\n"
    "  creation (new dataset) options\n"
    "\n"
    "     -new_dset         : create a new GIFTI dataset\n"
    "     -new_numDA  NUMDA : new dataset will have NUMDA DataArray elements\n"
    "                         e.g. -new_numDA 3\n"
    "     -new_intent INTENT: DA elements will have intent INTENT\n"
    "                         e.g. -new_intent NIFTI_INTENT_FTEST\n"
    "     -new_dtype   TYPE : set datatype to TYPE\n"
    "                         e.g. -new_dtype NIFTI_TYPE_FLOAT32\n"
    "     -new_ndim NUMDIMS : set Dimensionality to NUMDIMS (see -new_dims)\n"
    "     -new_dims D0...D5 : set dims[] to these 6 values\n"
    "                         e.g. -new_ndim 2 -new_dims 7 2 0 0 0 0\n"
    "     -new_data         : allocate space for data in created dataset\n"
    "\n"
    );
    printf (
    "  ----------------------------------------\n"
    "  comparison options\n"
    "\n"
    "     -approx_gifti            : approximate comparison of GIFTI dsets\n"
    "\n"
    "           This compares all data elements of the two GIFTI structures.\n"
    "           The attributes, MetaData, etc. are ignored if they do not\n"
    "           pertain directly to the data.\n"
    "\n"
    "           The comparisons allow for small, fractional differences,\n"
    "           which depend on the datatype.\n"
    "\n"
    "     -compare_gifti           : specifies to compare two GIFTI datasets\n"
    "\n"
    "           This compares all elements of the two GIFTI structures.\n"
    "           The attributes, LabelTabels, MetaData are compared, and then\n"
    "           each of the included DataArray elements.  All sub-structures\n"
    "           of the DataArrays are compared, except for the actual 'data',\n"
    "           which requires the '-compare_data' flag.\n"
    "\n"
    "           There must be exactly 2 input datasets to use this option.\n"
    "           See example #7 for sample usage.\n"
    "\n"
    "     -compare_data            : flag to request comparison of the data\n"
    "\n"
    "           Data comparison is done per DataArray element.\n"
    "\n"
    "           Comparing data is a separate operation from comparing GIFTI.\n"
    "           Neither implies the other.\n"
    "\n"
    "     -compare_verb LEVEL      : set the verbose level of comparisons\n"
    "\n"
    "           Data comparison is done per DataArray element.  Setting the\n"
    "           verb level will have the following effect:\n"
    "\n"
    "           0 : quiet, only return whether there was a difference\n"
    "           1 : show whether there was a difference\n"
    "           2 : show whether there was a difference per DataArray\n"
    "           3 : show all differences\n"
    "\n"
    );
    printf (
    "  ----------------------------------------\n"
    "  MetaData copy options\n"
    "\n"
    "     -copy_gifti_meta MD_NAME      : copy MetaData with name MD_NAME\n"
    "\n"
    "           e.g. -copy_gifti_meta AFNI_History\n"
    "\n"
    "           Copy the MetaData with the given name from the first input\n"
    "           dataset to the second (last).  This applies to MetaData at\n"
    "           the GIFTI level (not in the DataArray elements).\n"
    "\n"
    "     -copy_DA_meta MD_NAME         : copy MetaData with name MD_NAME\n"
    "\n"
    "           e.g. -copy_DA_meta intent_p1\n"
    "\n"
    "           Copy the MetaData with the given name from the first input\n"
    "           dataset to the second (last).  This applies to MetaData at\n"
    "           DataArray level.\n"
    "\n"
    "           This will apply to all DataArray elements, unless the\n"
    "           -DA_index_list option is used to specify a zero-based\n"
    "           index list.\n"
    "\n"
    "           see also -DA_index_list\n"
    "\n"
    );
    printf (
    "------------------------------------------------------------\n"
    "see the GIfTI community web site at:\n"
    "\n"
    "           http://www.nitrc.org/projects/gifti\n"
    "\n"
    "R Reynolds, National Institutes of Health\n"
    "------------------------------------------------------------\n"
    );
    return 0;
}

int write_as_asc(gifti_image * gim, char * prefix)
{
    giiDataArray  * dac; /* coords */
    giiDataArray  * dat; /* triangles */

    fprintf(stderr,"-- trying to write data with prefix '%s'\n", prefix);

    /* write surface file, *.1D */
    if( (dac = gifti_find_DA(gim, NIFTI_INTENT_POINTSET, 0)) &&
        (dat = gifti_find_DA(gim, NIFTI_INTENT_TRIANGLE, 0))    )
        (void) write_surf_file(dac, dat, prefix, 1);
    else {
        fprintf(stderr,"** failed to find coordinate/triangle structs\n");
        return 1;
    }

    return 0;
}


/* if dlist contains 1 element, write out as 2-D list,
   else each DA must have only 1 dimension */
int write_1D_file(giiDataArray ** dlist, int len, char * prefix, int add_suf)
{
    giiDataArray * da;
    FILE         * fp;
    char         * name = prefix;
    char         * nbuf = NULL;
    long long      rows, cols, c;

    if( add_suf && !strstr(prefix, ".1D")) {     /* create a new name */
        nbuf = (char *)malloc(strlen(prefix) + strlen(".1D") + 1);
        strcpy(nbuf, prefix);
        strcat(nbuf, ".1D");
        name = nbuf;
    }

    if( len == 1 ){     /* write out as 2D list */
        /* note the number of rows and columns */
        fprintf(stderr,"++ writing 1D '%s' from single DA\n", name);
        da = dlist[0];
        if( gifti_DA_rows_cols(da, &rows, &cols) ) {
            fprintf(stderr,"** bad return from rows_cols, failing...\n");
            if( nbuf ) free(nbuf);
            return 1;
        }

        if( !(fp = fopen(name, "w")) ) {
            fprintf(stderr,"** failed to open '%s' for 'w'\n",name);
            if( nbuf ) free(nbuf);
            return 1;
        }

        fprintf(stderr,"++ 1D write, RxC = %lld x %lld\n", rows, cols);
        if( da->ind_ord == GIFTI_IND_ORD_COL_MAJOR ) {
            fprintf(stderr,"-- writing data rows in reverse order\n");
            for(c = rows-1; c >= 0; c-- )
                ewrite_data_line(da->data, da->datatype, c, cols, 0, 0, fp);
        } else {
            fprintf(stderr,"-- writing data rows in normal order\n");
            for(c = 0; c < rows; c++ )
                ewrite_data_line(da->data, da->datatype, c, cols, 0, 0, fp);
        }
    } else {            /* write da->nvals lines of 'num values */
        void ** vlist = (void **)malloc(len * sizeof(void *));
        int     fail = 0;

        fprintf(stderr,"++ writing 1D '%s' from DA list (%d)\n", name, len);

        /* set data pointers */
        for( c = 0; c < len; c++ ) {
            vlist[c] = dlist[c]->data;
            if( !vlist[c] ) {
                fprintf(stderr,"** DA[%lld] has no data, bailing...\n", c);
                fail = 1;
            } else if( dlist[c]->nvals != dlist[0]->nvals ) {
                fprintf(stderr,"** d[%lld] has %lld vals, but d[0] has %lld\n",
                        c, dlist[c]->nvals, dlist[0]->nvals);
                fail = 1;
            } else if (dlist[c]->datatype != dlist[0]->datatype) {
                fprintf(stderr,"** d[%lld] has type %d, but d[0] has %d\n",
                        c, dlist[c]->datatype, dlist[0]->datatype);
                fail = 1;
            }

            if( fail ) {
                free(vlist);
                if( nbuf ) free(nbuf);
                return 1;
            }
        }

        /* good to go */
        if( !(fp = fopen(name, "w")) ) {
            fprintf(stderr,"** failed to open '%s' for 'w'\n",name);
            if( nbuf ) free(nbuf);
            return 1;
        }

        if(G.verb > 1) fprintf(stderr,"++ 1D write many, RxC = %lld x %d\n",
                               dlist[0]->nvals, len);
        ewrite_many_lines(vlist, dlist[0]->datatype,len, dlist[0]->nvals, 0,fp);

        free(vlist);
    }

    if( G.verb > 1 ) fprintf(stderr,"++ 1D write, apparent success\n");

    if( nbuf ) free(nbuf);
    fclose(fp);

    return 0;
}


int write_surf_file(giiDataArray * dc, giiDataArray * dt, char * prefix,
                    int add_suf)
{
    giiDataArray * da;
    FILE         * fp;
    char         * name = prefix;
    char         * nbuf = NULL;
    long long      crows, ccols, trows, tcols, rows, cols, c;

    if( add_suf && !strstr(prefix, ".asc") ) {   /* maybe create a new name */
        nbuf = (char *)malloc(strlen(prefix) + strlen(".asc") + 1);
        strcpy(nbuf, prefix);
        strcat(nbuf, ".asc");
        name = nbuf;
    }

    if( !(fp = fopen(name, "w")) ) {
        fprintf(stderr,"** failed to open '%s' for 'w'\n",name);
        if( nbuf ) free(nbuf);
        return 1;
    }

    /* note the number of rows and columns */
    if( gifti_DA_rows_cols(dc, &crows, &ccols) ) {
        fclose(fp);
        if( nbuf ) free(nbuf);
        return 1;
    } else if( gifti_DA_rows_cols(dt, &trows, &tcols) ) {
        fclose(fp);
        if( nbuf ) free(nbuf);
        return 1;
    }

    fprintf(fp, "#!ascii version of surface\n"
                "%lld %lld\n", crows, trows);

    /* write out the coordinates */

    da = dc;
    rows = crows;
    cols = ccols;

    if( da->ind_ord == GIFTI_IND_ORD_COL_MAJOR ) {
        fprintf(stderr,"-- writing coord rows in reverse order\n");
        for(c = rows-1; c >= 0; c-- )
            ewrite_data_line(da->data, da->datatype, c, cols, 0, 1, fp);
    } else {
        fprintf(stderr,"-- writing coord rows in normal order\n");
        for(c = 0; c < rows; c++ )
            ewrite_data_line(da->data, da->datatype, c, cols, 0, 1, fp);
    }

    /* write out the triangles */

    da = dt;
    rows = trows;
    cols = tcols;

    if( da->ind_ord == GIFTI_IND_ORD_COL_MAJOR ) {
        fprintf(stderr,"-- writing triangle rows in reverse order\n");
        for(c = rows-1; c >= 0; c-- )
            ewrite_data_line(da->data, da->datatype, c, cols, 0, 1, fp);
    } else {
        fprintf(stderr,"-- writing triangle rows in normal order\n");
        for(c = 0; c < rows; c++ )
            ewrite_data_line(da->data, da->datatype, c, cols, 0, 1, fp);
    }


    fclose(fp);

    return 0;
}


int ewrite_data_line(void * data, int type, int row, int cols, int spaces,
                     int trail0, FILE * fp)
{
    int c;
    if( !data || row < 0 || cols <= 0 || !fp ) {
        static int show = 1;
        if(show){
            fprintf(stderr,"** write data line: bad inputs (%p,%d,%d,%p)\n",
                   data, row, cols, (void *)fp);
            show=0;
        }
        return 1;
    }

    fprintf(fp, "%*s", spaces, "");
    switch( type ) {
        default :
            fprintf(stderr,"** write_data_line, unknown type %d\n",type);
            return -1;
        case NIFTI_TYPE_UINT8: {
            unsigned char * ptr = (unsigned char *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%u ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_INT16: {
            short * ptr = (short *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%d ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_INT32: {
            int * ptr = (int *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%d ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_FLOAT32: {
            float * ptr = (float *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%f ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_COMPLEX64: {
            float * ptr = (float *)data + row * cols;
            for(c = 0; c < 2*cols; c+=2)fprintf(fp, "%f %f   ",ptr[c],ptr[c+1]);            break;
        }
        case NIFTI_TYPE_FLOAT64: {
            double * ptr = (double *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%f ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_RGB24: {
            unsigned char * ptr = (unsigned char *)data + row * cols;
            for( c = 0; c < 3*cols; c+=3 )
                fprintf(fp, "%u %u %u   ", ptr[c], ptr[c+1], ptr[c+2]);
            break;
        }
        case NIFTI_TYPE_INT8: {
            char * ptr = (char *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%d ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_UINT16: {
            unsigned short * ptr = (unsigned short *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%u ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_UINT32: {     /* NIFTI_TYPE_UINT32 */
            unsigned int * ptr = (unsigned int *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%u ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_INT64: {
            long long * ptr = (long long *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%lld ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_UINT64: {
            unsigned long long * ptr = (unsigned long long *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%llu ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_FLOAT128: {
            fprintf(stderr,"** ewrite_data_line, won't write %s\n",
                    gifti_datatype2str(type));
            break;
        }
        case NIFTI_TYPE_COMPLEX128: {
            double * ptr = (double *)data + row * cols;
            for(c = 0; c < 2*cols; c+=2)fprintf(fp, "%f %f   ",ptr[c],ptr[c+1]);            break;
        }
        case NIFTI_TYPE_COMPLEX256: {
            fprintf(stderr,"** ewrite_data_line, won't write %s\n",
                    gifti_datatype2str(type));
            break;
        }
    }

    if( trail0 ) fputs(" 0", fp);  /* maybe write trailing zero */

    fputc('\n', fp);

    return 0;
}


/* write out as cols by rows (else we'd use ewrite_data_line) */
int ewrite_many_lines(void ** data, int type, long long cols, long long rows,
                      int spaces, FILE * fp)
{
    long long r, c;

    if( !data || rows == 0 || cols == 0 || !fp ) return 1;

    fprintf(fp, "%*s", spaces, "");
    switch( type ) {
        default :
            fprintf(stderr,"** write_data_line, unknown type %d\n",type);
            return -1;
        case NIFTI_TYPE_UINT8: {
            unsigned char ** ptr = (unsigned char **)data;
            for( r = 0; r < rows; r++ ) {
                for( c = 0; c < cols; c++ ) fprintf(fp, "%u ", ptr[c][r]);
                fputc('\n', fp);
            }
            break;
        }
        case NIFTI_TYPE_INT16: {
            short ** ptr = (short **)data;
            for( r = 0; r < rows; r++ ) {
                for( c = 0; c < cols; c++ ) fprintf(fp, "%d ", ptr[c][r]);
                fputc('\n', fp);
            }
            break;
        }
        case NIFTI_TYPE_INT32: {
            int ** ptr = (int **)data;
            for( r = 0; r < rows; r++ ) {
                for( c = 0; c < cols; c++ ) fprintf(fp, "%d ", ptr[c][r]);
                fputc('\n', fp);
            }
            break;
        }
        case NIFTI_TYPE_FLOAT32: {
            float ** ptr = (float **)data;
            for( r = 0; r < rows; r++ ) {
                for( c = 0; c < cols; c++ ) fprintf(fp, "%f ", ptr[c][r]);
                fputc('\n', fp);
            }
            break;
        }
        case NIFTI_TYPE_COMPLEX64: {    /* process as float32 pairs */
            float ** ptr = (float **)data;
            for( r = 0; r < 2*rows; r+=2 ) {
                for( c = 0; c < cols; c++ )
                    fprintf(fp, "%f %f  ", ptr[c][r], ptr[c][r+1]);
                fputc('\n', fp);
            }
            break;
        }
        case NIFTI_TYPE_FLOAT64: {
            double ** ptr = (double **)data;
            for( r = 0; r < rows; r++ ) {
                for( c = 0; c < cols; c++ ) fprintf(fp, "%f ", ptr[c][r]);
                fputc('\n', fp);
            }
            break;
        }
        case NIFTI_TYPE_RGB24: {        /* process as char triplets */
            char ** ptr = (char **)data;
            for( r = 0; r < 3*rows; r+=3 ) {
                for( c = 0; c < cols; c++ )
                    fprintf(fp, "%u %u %u  ",ptr[c][r],ptr[c][r+1],ptr[c][r+2]);
                fputc('\n', fp);
            }
            break;
        }
        case NIFTI_TYPE_INT8: {
            char ** ptr = (char **)data;
            for( r = 0; r < rows; r++ ) {
                for( c = 0; c < cols; c++ ) fprintf(fp, "%d ", ptr[c][r]);
                fputc('\n', fp);
            }
            break;
        }
        case NIFTI_TYPE_UINT16: {
            unsigned short ** ptr = (unsigned short **)data;
            for( r = 0; r < rows; r++ ) {
                for( c = 0; c < cols; c++ ) fprintf(fp, "%u ", ptr[c][r]);
                fputc('\n', fp);
            }
            break;
        }
        case NIFTI_TYPE_UINT32: {
            unsigned int ** ptr = (unsigned int **)data;
            for( r = 0; r < rows; r++ ) {
                for( c = 0; c < cols; c++ ) fprintf(fp, "%u ", ptr[c][r]);
                fputc('\n', fp);
            }
            break;
        }
        case NIFTI_TYPE_INT64: {
            long long ** ptr = (long long **)data;
            for( r = 0; r < rows; r++ ) {
                for( c = 0; c < cols; c++ ) fprintf(fp, "%lld ", ptr[c][r]);
                fputc('\n', fp);
            }
            break;
        }
        case NIFTI_TYPE_UINT64: {
            unsigned long long ** ptr = (unsigned long long **)data;
            for( r = 0; r < rows; r++ ) {
                for( c = 0; c < cols; c++ ) fprintf(fp, "%llu ", ptr[c][r]);
                fputc('\n', fp);
            }
            break;
        }
        /* just forget these ...
            case NIFTI_TYPE_FLOAT128:   { break; }
            case NIFTI_TYPE_COMPLEX128: { break; }
            case NIFTI_TYPE_COMPLEX256: { break; }
        */
    }

    return 0;
}

/*----------------------------------------------------------------------
 * - only bother to alloc one pointer at a time (don't need efficiency here)
 * - return 0 on success
 *----------------------------------------------------------------------*/
static int add_to_int_list(gt_int_list * ilist, int val)
{
   if( ilist->len == 0 ) ilist->list = NULL;  /* just to be safe */
   ilist->len++;
   ilist->list = (int *)realloc(ilist->list,ilist->len*sizeof(int));
   if( ! ilist->list ){
      fprintf(stderr,"** A2IL: failed to alloc %d (int) elements\n",ilist->len);
      return -1;
   }

   ilist->list[ilist->len-1] = val;

   return 0;
}


/*----------------------------------------------------------------------
 * - do not duplicate the string
 * - only bother to alloc one pointer at a time (don't need efficiency here)
 * - return 0 on success
 *----------------------------------------------------------------------*/
static int add_to_str_list(gt_str_list * slist, char * str)
{
   if( slist->len == 0 ) slist->list = NULL;  /* just to be safe */
   slist->len++;
   slist->list = (char **)realloc(slist->list,slist->len*sizeof(char *));
   if( ! slist->list ){
      fprintf(stderr,"** A2SL: failed to alloc %d (char *) elements\n",
              slist->len);
      return -1;
   }

   slist->list[slist->len-1] = str;

   return 0;
}

int gt_modify_dset(gt_opts * opts, gifti_image * gim)
{
    gt_int_list * ilist;
    int           c, errs = 0;

    if( !gim ) return 0;

    if( opts->verb > 2 ) fprintf(stderr,"-- starting modifications\n");

    /* modify GIFTI attributes */
    if( opts->mod_gim_atr )
        for( c = 0; c < opts->gim_atrs.len-1; c+=2 )  /* grab in pairs */
            errs += gifti_str2attr_gifti(gim, opts->gim_atrs.list[c],
                                              opts->gim_atrs.list[c+1]);

    /* modify GIFTI MetaData (replacing any 'name' matches) */
    if( opts->mod_gim_meta )
        for( c = 0; c < opts->gim_meta.len-1; c+=2 )  /* grab in pairs */
            errs += gifti_add_to_meta(&gim->meta, opts->gim_meta.list[c],
                                                  opts->gim_meta.list[c+1], 1);

    /* modify DataArray attributes */
    if( opts->mod_DA_atr ) {
        ilist = &opts->DAmodlist;
        if( ilist->list && ilist->len > 0 ) {
            if(!gifti_valid_int_list(ilist->list,ilist->len,0,gim->numDA-1,1)){
                fprintf(stderr,"** invalid DAmodlist\n");
                return 1;
            }

            /* apply to list */
            for( c = 0; c < opts->DAmodlist.len; c++ )
                errs += gifti_set_DA_atrs(gim->darray[ilist->list[c]],
                        (const char **)opts->DA_atrs.list,opts->DA_atrs.len,0);
        } else  /* apply to all DA elements */
            for( c = 0; c < gim->numDA; c++ )
                errs += gifti_set_DA_atrs(gim->darray[c],
                        (const char **)opts->DA_atrs.list,opts->DA_atrs.len, 0);
    }

    /* modify DataArray MetaData (replacing any 'name' matches) */
    if( opts->mod_DA_meta )
        for( c = 0; c < opts->DA_meta.len-1; c+=2 )  /* grab in pairs */
            errs += gifti_set_DA_meta(gim,
                                opts->DA_meta.list[c], opts->DA_meta.list[c+1],
                                opts->DAmodlist.list, opts->DAmodlist.len, 1);

    /* for data manipulation functions, do not proceed if there there errors */

    /* if desired, convert any existing data to float */
    if( !errs && opts->mod_to_float ) errs += gifti_convert_to_float(gim);

    /* do this last, in case data related attributes were modified */
    if( !errs && opts->mod_add_data )
        if(gifti_alloc_DA_data(gim, opts->DAmodlist.list, opts->DAmodlist.len))
            errs++;

    if(opts->verb>2) fprintf(stderr,"-- modifications done, %d errors\n",errs);

    return errs;
}
