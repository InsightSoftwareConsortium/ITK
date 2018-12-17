/*--------------------------------------------------------------------------*/
/*! \file   nifti_tool.c
 *  \brief  a tool for nifti file perusal, manipulation and copying
 *          written by Rick Reynolds, SSCC, NIMH, January 2005
 * <pre>
 *
 * usage: nifti_tool [options] -infiles files...
 *
 * Via this tool, one should be able to:
 *
 *       - copy a set of volumes (sub-bricks) from one dataset to another
 *       - copy a dataset, restricting some dimensions to given indices
 *
 *       - display the contents of a nifti_image (or various fields)
 *       - display the contents of a nifti1_header (or various fields)
 *       - display AFNI extensions (they are text)
 *       - display the time series at coordinates i,j,k
 *       - display the data from any collapsed image
 *
 *       - do a diff on two nifti_image structs (or various fields)
 *       - do a diff on two nifti1_header structs (or various fields)
 *
 *       - add an AFNI extension
 *       - remove any extension
 *
 *       - modify any field(s) of a nifti_image
 *       - modify any field(s) of a nifti1_struct
 *
 * usage forms:
 *
 *   nifti_tool -help
 *   nifti_tool -help_hdr
 *   nifti_tool -help_hdr1
 *   nifti_tool -help_hdr2
 *   nifti_tool -help_nim
 *   nifti_tool -help_nim1
 *   nifti_tool -help_nim2
 *   nifti_tool -help_ana
 *   nifti_tool -help_datatypes
 *   nifti_tool -hist
 *   nifti_tool -ver
 *   nifti_tool -nifti_hist
 *   nifti_tool -nifti_ver
 *   nifti_tool -with_zlib
 *
 *   nifti_tool -check_hdr -infiles f1 ...
 *   nifti_tool -check_nim -infiles f1 ...

 *   nifti_tool -disp_cext -infiles f1 ...
 *   nifti_tool -disp_exts -infiles f1 ...
 *   nifti_tool -disp_hdr  [-field fieldname] [...] -infiles f1 ...
 *   nifti_tool -disp_hdr1 [-field fieldname] [...] -infiles f1 ...
 *   nifti_tool -disp_hdr2 [-field fieldname] [...] -infiles f1 ...
 *   nifti_tool -disp_nim  [-field fieldname] [...] -infiles f1 ...
 *   nifti_tool -disp_ana  [-field fieldname] [...] -infiles f1 ...
 *   nifti_tool -disp_ts I J K [-dci_lines] -infiles f1 ...
 *   nifti_tool -disp_ci I J K T U V W [-dci_lines] -infiles f1 ...
 *
 *   nifti_tool -diff_hdr  [-field fieldname] [...] -infiles f1 f2
 *   nifti_tool -diff_hdr1 [-field fieldname] [...] -infiles f1 f2
 *   nifti_tool -diff_hdr2 [-field fieldname] [...] -infiles f1 f2
 *   nifti_tool -diff_nim  [-field fieldname] [...] -infiles f1 f2
 *
 *   nifti_tool -add_afni_ext    "extension in quotes" -infiles f1 ...
 *   nifti_tool -add_comment_ext "extension in quotes" -infiles f1 ...
 *   nifti_tool -rm_ext ext_index -infiles f1 ...
 *
 *   nifti_tool -mod_hdr  [-mod_field fieldname new_val] [...] -infiles f1 ...
 *   nifti_tool -mod_nim  [-mod_field fieldname new_val] [...] -infiles f1 ...
 *
 * </pre> */
/*-------------------------------------------------------------------------*/

/*! module history */
static const char * g_history[] =
{
  "----------------------------------------------------------------------\n"
  "nifti_tool modification history:\n"
  "\n",
  "0.1  30 December 2004 [rickr]\n"
  "     (Rick Reynolds of the National Institutes of Health, SSCC/DIRP/NIMH)\n"
  "   - skeleton version: options read and printed\n"
  "\n",
  "1.0  07 January 2005 [rickr]\n"
  "   - initial release version\n"
  "\n",
  "1.1  14 January 2005 [rickr]\n"
  "   - changed all non-error/non-debug output from stderr to stdout\n"
  "       note: creates a mis-match between normal output and debug messages\n"
  "   - modified act_diff_hdrs and act_diff_nims to do the processing in\n"
  "       lower-level functions\n",
  "   - added functions diff_hdrs, diff_hdrs_list, diff_nims, diff_nims_list\n"
  "   - added function get_field, to return a struct pointer via a fieldname\n"
  "   - made 'quiet' output more quiet (no description on output)\n"
  "   - made hdr and nim_fields arrays global, so do not pass in main()\n"
  "   - return (from main()) after first act_diff() difference\n"
  "\n",
  "1.2  9 February 2005 [rickr] - minor\n"
  "   - defined a local NTL_FERR macro (so it does not come from nifti1_io.h)\n"
  "   - added new set_byte_order parameter to nifti_set_filenames\n"
  "\n",
  "1.3  23 February 2005 [rickr] - sourceforge.net merge\n"
  "   - moved to utils directory\n"
  "   - added simple casts of 3 pointers for -pedantic warnings\n"
  "   - added a doxygen comment for the file\n"
  "\n",
  "1.4  02 March 2005 [rickr] - small update\n"
  "   - no validation in nifti_read_header calls\n"
  "\n",
  "1.5  05 April 2005 [rickr] - small update\n"
  "   - refuse mod_hdr for gzipped files (we cannot do partial overwrites)\n"
  "\n",
  "1.6  08 April 2005 [rickr] - added cbl, cci and dts functionality\n"
  "   - added -cbl: 'copy brick list' dataset copy functionality\n"
  "   - added -ccd: 'copy collapsed data' dataset copy functionality\n"
  "   - added -disp_ts: 'disp time series' data display functionality\n"
  "   - moved raw data display to disp_raw_data()\n"
  "\n",
  "1.7  14 April 2005 [rickr] - added data display functionality\n"
  "   - added -dci: 'display collapsed image' functionality\n"
  "   - modified -dts to use -dci\n"
  "   - modified and updated the help in use_full()\n"
  "   - changed copy_collapsed_dims to copy_collapsed_image, etc.\n",
  "   - fixed problem in disp_raw_data() for printing NT_DT_CHAR_PTR\n"
  "   - modified act_disp_ci():\n"
  "       o was act_disp_ts(), now displays arbitrary collapsed image data\n"
  "       o added missed debug filename act_disp_ci()\n"
  "       o can now save free() of data pointer for end of file loop\n",
  "   - modified disp_raw_data()\n"
  "       o takes a flag for whether to print newline\n"
  "       o trailing spaces and zeros are removed from printing floats\n"
  "   - added clear_float_zeros(), to remove trailing zeros\n"
  "\n",
  "1.8  19 April 2005 [rickr] - COMMENT extensions\n"
  "   - added int_list struct, and keep_hist,etypes,command fields to nt_opts\n"
  "   - added -add_comment_ext action\n"
  "   - allowed for removal of multiple extensions, including option of ALL\n"
  "   - added -keep_hist option, to store the command as a COMMENT extension\n",
  "     (includes fill_cmd_string() and add_int(), is done for all actions)\n"
  "   - added remove_ext_list(), for removing a list of extensions by indices\n"
  "   - added -strip action, to strip all extensions and descrip fields\n"
  "\n",
  "1.9  25 Aug 2005 [rickr] - const/string cleanup for warnings\n",
  "1.10 18 Nov 2005 [rickr] - added check_hdr and check_nim actions\n",
  "1.11 31 Jan 2006 [rickr] - check for new vox_offset in act_mod_hdrs\n",
  "1.12 02 Mar 2006 [rickr]\n"
  "   - in act_cbl(), check for nt = 0 because of niftilib update 1.17\n",
  "1.13 24 Apr 2006 [rickr] - act_disp_ci(): remove time series length check\n",
  "1.14 04 Jun 2007 [rickr] - free_opts_mem(), to appease valgrind\n",
  "1.15 05 Jun 2007 [rickr] - act_check_hdrs: free(nim)->nifti_image_free()\n",
  "1.16 12 Jun 2007 [rickr] - allow creation of datasets via MAKE_IM\n",
  "   - added nt_image_read, nt_read_header and nt_read_bricks\n"
  "     to wrap nifti read functions, allowing creation of new datasets\n"
  "   - added -make_im, -new_dim, -new_datatype and -copy_im\n"
  "1.17 13 Jun 2007 [rickr] - added help for -copy_im, enumerate examples\n",
  "1.18 23 Jun 2007 [rickr] - main returns 0 on -help, -hist, -ver\n"
  "1.19 28 Nov 2007 [rickr] - added -help_datatypes\n",
  "1.20 13 Jun 2008 [rickr]\n"
  "   - added -with_zlib\n"
  "   - added ability to create extension from text file (for J. Gunter)\n",
  "1.21 03 Aug 2008 [rickr] - ANALYZE 7.5 support\n"
  "   - added -help_ana, -disp_ana,\n"
  "    -swap_as_analyze, -swap_as_nifti, -swap_as_old\n"
  "1.22 08 Oct 2008 [rickr] - allow cbl with indices in 0..nt*nu*nv*nw-1\n"
  "1.23 06 Jul 2010 [rickr]\n",
  "   - in nt_read_bricks, bsize computation should allow for large integers\n"
  "1.24 26 Sep 2012 [rickr]\n",
  "   - changed ana originator from char to short\n"
  "2.00 29 Aug 2013 [rickr] - NIFTI-2\n",
  "2.01 28 Apr 2015 [rickr] - disp_hdr1/disp_hdr2 to read as those types\n"
  "2.02 01 Jun 2015 [rickr]\n",
  "   - disp_hdr detects type\n"
  "   - diff_hdr detects type\n"
  "   - have diff_hdr1/diff_hdr2 to read as those types\n"
  "2.03 23 Jul 2015 [rickr] - handle a couple of unknown version cases\n",
  "2.04 05 Aug 2015 [rickr] - incorporate nifti-2 writing library change\n",
  "2.05 24 Jul 2017 [rickr]\n"
  "   - display ANALYZE header via appropriate NIFTI-1\n"
  "   - apply more PRId64 for 64-bit int I/O\n"
  "----------------------------------------------------------------------\n"
};
static char g_version[] = "version 2.05 (July 24, 2017)";
static int  g_debug = 1;

#define _NIFTI_TOOL_C_
#include "nifti2_io.h"
#include "nifti_tool.h"

/* local prototypes */
static int free_opts_mem(nt_opts * nopt);
static int num_volumes(nifti_image * nim);
static char * read_file_text(const char * filename, int * length);

#define NTL_FERR(func,msg,file)                                      \
            fprintf(stderr,"** ERROR (%s): %s '%s'\n",func,msg,file)

/* val may be a function call, so evalulate first, and return result */
#define FREE_RETURN(val) \
        do{ int tval=(val); free_opts_mem(&opts); return tval; } while(0)

/* these are effectively constant, and are built only for verification */
field_s g_hdr1_fields[NT_HDR1_NUM_FIELDS];    /* nifti_1_header fields */
field_s g_hdr2_fields[NT_HDR2_NUM_FIELDS];    /* nifti_2_header fields */
field_s g_ana_fields [NT_ANA_NUM_FIELDS];     /* nifti_analyze75       */
field_s g_nim1_fields[NT_NIM_NUM_FIELDS];     /* nifti_image fields    */
field_s g_nim2_fields[NT_NIM_NUM_FIELDS];     /* nifti2_image fields   */

int main( int argc, char * argv[] )
{
   nt_opts opts;
   int     rv;

   if( (rv = process_opts(argc, argv, &opts)) != 0)  /* then return */
   {
      if( rv < 0 ) FREE_RETURN(1);  /* free opts memory, and return */
      else         FREE_RETURN(0);  /* valid usage */
   }

   if( (rv = verify_opts(&opts, argv[0])) != 0 )
      FREE_RETURN(rv);

   /* now perform the requested action(s) */

   if( (rv = fill_hdr1_field_array(g_hdr1_fields)) != 0 )
      FREE_RETURN(rv);

   if( (rv = fill_hdr2_field_array(g_hdr2_fields)) != 0 )
      FREE_RETURN(rv);

   if( (rv = fill_nim1_field_array(g_nim1_fields)) != 0 )
      FREE_RETURN(rv);

   if( (rv = fill_nim2_field_array(g_nim2_fields)) != 0 )
      FREE_RETURN(rv);

   if( (rv = fill_ana_field_array(g_ana_fields)) != 0 )
      FREE_RETURN(rv);

   /* 'check' functions, first */
   if( opts.check_hdr || opts.check_nim ) /* allow for both */
      FREE_RETURN( act_check_hdrs(&opts) );

   /* copy or dts functions  -- do not continue after these */
   if( opts.cbl )             FREE_RETURN( act_cbl(&opts) );
   if( opts.cci )             FREE_RETURN( act_cci(&opts) );
   if( opts.dts || opts.dci ) FREE_RETURN( act_disp_ci(&opts) );

   /* perform modifications early, in case we allow multiple actions */
   if( opts.strip     && ((rv = act_strip    (&opts)) != 0) ) FREE_RETURN(rv);

   if( opts.add_exts  && ((rv = act_add_exts (&opts)) != 0) ) FREE_RETURN(rv);
   if( opts.rm_exts   && ((rv = act_rm_ext   (&opts)) != 0) ) FREE_RETURN(rv);

   if( opts.mod_hdr   && ((rv = act_mod_hdrs (&opts)) != 0) ) FREE_RETURN(rv);
   if( opts.mod_nim   && ((rv = act_mod_nims (&opts)) != 0) ) FREE_RETURN(rv);

   if((opts.swap_hdr  || opts.swap_ana || opts.swap_old )
                      && ((rv = act_swap_hdrs (&opts)) != 0) ) FREE_RETURN(rv);

   /* if a diff, return wither a difference exists (like the UNIX command) */
   if( opts.diff_hdr  && ((rv = act_diff_hdrs (&opts)) != 0) ) FREE_RETURN(rv);
   if( opts.diff_hdr1 && ((rv = act_diff_hdr1s(&opts)) != 0) ) FREE_RETURN(rv);
   if( opts.diff_hdr2 && ((rv = act_diff_hdr2s(&opts)) != 0) ) FREE_RETURN(rv);
   if( opts.diff_nim  && ((rv = act_diff_nims(&opts))  != 0) ) FREE_RETURN(rv);

   /* last action type is display */
   if( opts.disp_exts && ((rv = act_disp_exts(&opts)) != 0) ) FREE_RETURN(rv);
   if( opts.disp_cext && ((rv = act_disp_cext(&opts)) != 0) ) FREE_RETURN(rv);
   if( opts.disp_hdr  && ((rv = act_disp_hdr (&opts)) != 0) ) FREE_RETURN(rv);
   if( opts.disp_hdr1 && ((rv = act_disp_hdr1(&opts)) != 0) ) FREE_RETURN(rv);
   if( opts.disp_hdr2 && ((rv = act_disp_hdr2(&opts)) != 0) ) FREE_RETURN(rv);
   if( opts.disp_nim  && ((rv = act_disp_nims(&opts)) != 0) ) FREE_RETURN(rv);
   if( opts.disp_ana  && ((rv = act_disp_anas(&opts)) != 0) ) FREE_RETURN(rv);

   FREE_RETURN(0);
}

/*----------------------------------------------------------------------
 * process user options, return 0 on success
 *----------------------------------------------------------------------*/
int process_opts( int argc, char * argv[], nt_opts * opts )
{
   int ac;

   memset(opts, 0, sizeof(*opts));

   opts->prefix = NULL;
   opts->debug = 1;  /* init debug level to basic output */

   /* init options for creating a new dataset via "MAKE_IM" */
   opts->new_datatype = NIFTI_TYPE_INT16;
   opts->new_dim[0] = 3;
   opts->new_dim[1] = 1;  opts->new_dim[2] = 1;  opts->new_dim[3] = 1;

   if( argc < 2 ) return usage(argv[0], USE_FULL);

   /* terminal options are first, the rest are sorted */
   for( ac = 1; ac < argc; ac++ )
   {
      if( ! strcmp(argv[ac], "-help_datatypes") )
      {
         ac++;
         if( ac >= argc )
            nifti_disp_type_list(3);  /* show all types */
         else if( argv[ac][0] == 'd' || argv[ac][0] == 'D' )
            nifti_disp_type_list(1);  /* show DT_* types */
         else if( argv[ac][0] == 't' || argv[ac][0] == 'T' )
            nifti_test_datatype_sizes(1); /* test each nbyper and swapsize */
         else
            nifti_disp_type_list(2);  /* show NIFTI_* types */
         return 1;
      }
      else if( ! strcmp(argv[ac], "-help_hdr") )
         return usage(argv[0], USE_FIELD_HDR2);
      else if( ! strcmp(argv[ac], "-help_hdr1") )
         return usage(argv[0], USE_FIELD_HDR1);
      else if( ! strcmp(argv[ac], "-help_hdr2") )
         return usage(argv[0], USE_FIELD_HDR2);
      else if( ! strcmp(argv[ac], "-help_nim") )
         return usage(argv[0], USE_FIELD_NIM2);
      else if( ! strcmp(argv[ac], "-help_nim1") )
         return usage(argv[0], USE_FIELD_NIM1);
      else if( ! strcmp(argv[ac], "-help_nim2") )
         return usage(argv[0], USE_FIELD_NIM2);
      else if( ! strcmp(argv[ac], "-help_ana") )
         return usage(argv[0], USE_FIELD_ANA);
      else if( ! strcmp(argv[ac], "-help") )
         return usage(argv[0], USE_FULL);
      else if( ! strcmp(argv[ac], "-hist") )
         return usage(argv[0], USE_HIST);
      else if( ! strcmp(argv[ac], "-ver") )
         return usage(argv[0], USE_VERSION);
      else if( ! strcmp(argv[ac], "-nifti_hist") )
      {
         nifti_disp_lib_hist(1);
         nifti_disp_lib_hist(2);
         return 1;
      }
      else if( ! strcmp(argv[ac], "-nifti_ver") )
      {
         nifti_disp_lib_version();
         return 1;
      }
      else if( ! strcmp(argv[ac], "-with_zlib") ) {
         printf("Was NIfTI library compiled with zlib?  %s\n",
                nifti_compiled_with_zlib() ? "YES" : "NO");
         return 1;
      }

      /* begin normal execution options... */
      else if( ! strcmp(argv[ac], "-add_ext") )
      {
         int new_ecode;
         ac++;
         CHECK_NEXT_OPT(ac, argc, "-add_ext");
         new_ecode = atoll(argv[ac]);
         if( add_int(&opts->etypes, new_ecode) ) return -1;
         ac++;
         CHECK_NEXT_OPT(ac, argc, "-add_ext");
         if( add_string(&opts->elist, argv[ac]) ) return -1; /* add extension */
         opts->add_exts = 1;
      }
      else if( ! strcmp(argv[ac], "-add_afni_ext") )
      {
         ac++;
         CHECK_NEXT_OPT(ac, argc, "-add_afni_ext");
         if( add_string(&opts->elist, argv[ac]) ) return -1; /* add extension */
         if( add_int(&opts->etypes, NIFTI_ECODE_AFNI) ) return -1;
         opts->add_exts = 1;
      }
      else if( ! strncmp(argv[ac], "-add_comment_ext", 12) )
      {
         ac++;
         CHECK_NEXT_OPT(ac, argc, "-add_comment_ext");
         if( add_string(&opts->elist, argv[ac]) ) return -1; /* add extension */
         if( add_int(&opts->etypes, NIFTI_ECODE_COMMENT) ) return -1;
         opts->add_exts = 1;
      }
      else if( ! strcmp(argv[ac], "-check_hdr") )
         opts->check_hdr = 1;
      else if( ! strcmp(argv[ac], "-check_nim") )
         opts->check_nim = 1;
      else if( ! strcmp(argv[ac], "-copy_brick_list") ||
               ! strcmp(argv[ac], "-copy_im") ||
               ! strcmp(argv[ac], "-cbl") )
      {
         opts->cbl = 1;
      }
      else if( ! strcmp(argv[ac], "-copy_collapsed_image") ||
               ! strcmp(argv[ac], "-cci") )
      {
         /* we need to read in the 7 dimension values */
         int index;
         opts->ci_dims[0] = 0;
         for( index = 1; index < 8; index++ )
         {
            ac++;
            CHECK_NEXT_OPT_MSG(ac,argc,"-cci","7 dimension values are requred");
            if( ! isdigit(argv[ac][0]) && strcmp(argv[ac],"-1") ){
               fprintf(stderr,"** -cci param %d (= '%s') is not a valid\n"
                       "   consider: 'nifti_tool -help'\n",index,argv[ac]);
               return -1;
            }
            opts->ci_dims[index] = atoll(argv[ac]);
         }

         opts->cci = 1;
      }
      else if( ! strcmp(argv[ac], "-debug") )
      {
         ac++;
         CHECK_NEXT_OPT(ac, argc, "-debug");
         opts->debug = atoi(argv[ac]);
         g_debug = opts->debug;
      }
      else if( ! strcmp(argv[ac], "-diff_hdr") )
         opts->diff_hdr = 1;
      else if( ! strcmp(argv[ac], "-diff_hdr1") )
         opts->diff_hdr1 = 1;
      else if( ! strcmp(argv[ac], "-diff_hdr2") )
         opts->diff_hdr2 = 1;
      else if( ! strcmp(argv[ac], "-diff_nim") )
         opts->diff_nim = 1;
      else if( ! strncmp(argv[ac], "-disp_exts", 9) )
         opts->disp_exts = 1;
      else if( ! strncmp(argv[ac], "-disp_cext", 9) )
         opts->disp_cext = 1;
      else if( ! strcmp(argv[ac], "-disp_hdr") )
         opts->disp_hdr = 1;
      else if( ! strcmp(argv[ac], "-disp_hdr1") )
         opts->disp_hdr1 = 1;
      else if( ! strcmp(argv[ac], "-disp_hdr2") )
         opts->disp_hdr2 = 1;
      else if( ! strcmp(argv[ac], "-disp_nim") )
         opts->disp_nim = 1;
      else if( ! strcmp(argv[ac], "-disp_ana") )
         opts->disp_ana = 1;
      else if( ! strcmp(argv[ac], "-dci_lines") ||   /* before -dts */
               ! strcmp(argv[ac], "-dts_lines") )
      {
         opts->dci_lines = 1;
      }
      else if( ! strcmp(argv[ac], "-disp_collapsed_image") ||
               ! strcmp(argv[ac], "-disp_ci") )
      {
         /* we need to read in the 7 dimension values */
         int index;
         opts->ci_dims[0] = 0;
         for( index = 1; index < 8; index++ )
         {
            ac++;
            CHECK_NEXT_OPT_MSG(ac,argc,"-disp_ci",
                               "7 dimension values are requred");
            if( ! isdigit(argv[ac][0]) && strcmp(argv[ac],"-1") ){
               fprintf(stderr,"** -disp_ci param %d (= '%s') is not a valid\n"
                       "   consider: 'nifti_tool -help'\n",index,argv[ac]);
               return -1;
            }
            opts->ci_dims[index] = atoll(argv[ac]);
         }

         opts->dci = 1;
      }
      else if( ! strcmp(argv[ac], "-disp_ts") ||
               ! strcmp(argv[ac], "-dts") )
      {
         /* we need to read in the ijk indices into the ci_dims array */
         int index;
         for( index = 1; index <= 3; index++ )
         {
            ac++;
            CHECK_NEXT_OPT_MSG(ac,argc,"-dts","i,j,k indices are requied\n");
            if( ! isdigit(argv[ac][0]) ){
               fprintf(stderr,"** -dts param %d (= '%s') is not a number\n"
                       "   consider: 'nifti_tool -help'\n",index,argv[ac]);
               return -1;
            }
            opts->ci_dims[index] = atoll(argv[ac]);
         }
         /* and fill the rest of the array */
         opts->ci_dims[0] = 0;
         for( index = 4; index < 8; index++ ) opts->ci_dims[index] = -1;

         opts->dts = 1;
      }
      else if( ! strcmp(argv[ac], "-field") )
      {
         ac++;
         CHECK_NEXT_OPT(ac, argc, "-field");
         if( add_string(&opts->flist, argv[ac]) ) return -1; /* add field */
      }
      else if( ! strncmp(argv[ac], "-infiles", 3) )
      {
         int count;
         /* for -infiles, get all next arguments until a '-' or done */
         ac++;
         for( count = 0; (ac < argc) && (argv[ac][0] != '-'); ac++, count++ )
            if( add_string(&opts->infiles, argv[ac]) ) return -1;/* add field */
         if( count > 0 && ac < argc ) ac--;  /* more options to process */
         if( g_debug > 2 ) fprintf(stderr,"+d have %d file names\n", count);
      }
      else if( ! strncmp(argv[ac], "-make_image", 8) )
      {
         opts->make_im = 1;  /* will setup later, as -cbl and MAKE_IM */
      }
      else if( ! strcmp(argv[ac], "-mod_field") )
      {
         ac++;
         CHECK_NEXT_OPT(ac, argc, "-mod_field");
         if( add_string(&opts->flist, argv[ac]) ) return -1; /* add field */
         ac++;
         CHECK_NEXT_OPT(ac, argc, "-mod_field (2)");
         if( add_string(&opts->vlist, argv[ac]) ) return -1; /* add value */
      }
      else if( ! strcmp(argv[ac], "-mod_hdr") )
         opts->mod_hdr = 1;
      else if( ! strcmp(argv[ac], "-mod_nim") )
         opts->mod_nim = 1;
      else if( ! strcmp(argv[ac], "-keep_hist") )
         opts->keep_hist = 1;
      else if( ! strcmp(argv[ac], "-new_dim") )
      {
         /* we need to read in the 8 dimension values */
         int index;
         for( index = 0; index < 8; index++ )
         {
            ac++;
            CHECK_NEXT_OPT_MSG(ac,argc,"-new_dim","8 dim values are requred");
            if( ! isdigit(argv[ac][0]) && strcmp(argv[ac],"-1") ){
               fprintf(stderr,"** -new_dim param %d (= '%s') is not a valid\n"
                       "   consider: 'nifti_tool -help'\n",index,argv[ac]);
               return -1;
            }
            opts->new_dim[index] = atoll(argv[ac]);
         }
      }
      else if( ! strcmp(argv[ac], "-new_datatype") )
      {
         ac++;
         CHECK_NEXT_OPT(ac, argc, "-new_datatype");
         opts->new_datatype = atoi(argv[ac]);
      }
      else if( ! strcmp(argv[ac], "-overwrite") )
         opts->overwrite = 1;
      else if( ! strcmp(argv[ac], "-prefix") )
      {
         ac++;
         CHECK_NEXT_OPT(ac, argc, "-prefix");
         opts->prefix = argv[ac];
      }
      else if( ! strcmp(argv[ac], "-quiet") )
         opts->debug = 0;
      else if( ! strcmp(argv[ac], "-rm_ext") )
      {
         ac++;
         CHECK_NEXT_OPT(ac, argc, "-rm_ext");
         if( strcmp(argv[ac],"ALL") == 0 )  /* special case, pass -1 */
         {
            if( add_string(&opts->elist, "-1") ) return -1;
         }
         else
         {
            int index = atoi(argv[ac]);
            if( (index != -1) && ((index > 1000) || !isdigit(*argv[ac])) ){
               fprintf(stderr,
                    "** '-rm_ext' requires an extension index (read '%s')\n",
                    argv[ac]);
               return -1;
            }
            if( add_string(&opts->elist, argv[ac]) ) return -1;
         }
         opts->rm_exts = 1;
      }
      else if( ! strncmp(argv[ac], "-strip_extras", 6) )
         opts->strip = 1;
      else if( ! strcmp(argv[ac], "-swap_as_analyze") )
         opts->swap_ana = 1;
      else if( ! strcmp(argv[ac], "-swap_as_nifti") )
         opts->swap_hdr = 1;
      else if( ! strcmp(argv[ac], "-swap_as_old") )
         opts->swap_old = 1;
      else
      {
         fprintf(stderr,"** unknown option: '%s'\n", argv[ac]);
         return -1;
      }
   }

   if( opts->make_im )
   {
      if( opts->infiles.len > 0 )
      {
         fprintf(stderr,"** -infiles is invalid when using -make_im\n");
         return -1;
      }
      /* apply -make_im via -cbl and "MAKE_IM" */
      opts->cbl = 1;
      if( add_string(&opts->infiles, NT_MAKE_IM_NAME) ) return -1;
   }

   /* verify for programming purposes */
   if( opts->add_exts && ( opts->elist.len != opts->etypes.len ) )
   {
      fprintf(stderr,"** ext list length (%d) != etype length (%d)\n",
              opts->elist.len, opts->etypes.len);
      return -1;
   }

   g_debug = opts->debug;
   nifti_set_debug_level(g_debug);

   fill_cmd_string(opts, argc, argv);  /* copy this command */

   if( g_debug > 2 ) disp_nt_opts("options read: ", opts);

   return 0;
}


/*----------------------------------------------------------------------
 * verify that the options make sense
 *----------------------------------------------------------------------*/
int verify_opts( nt_opts * opts, char * prog )
{
   int ac, errs = 0;   /* number of requested action types */

   /* check that only one of disp, diff, mod or add_*_ext is used */
   ac  = (opts->check_hdr || opts->check_nim                   ) ? 1 : 0;
   ac += (opts->diff_hdr  || opts->diff_hdr1 || opts->diff_hdr2
                          || opts->diff_nim                    ) ? 1 : 0;
   ac += (opts->disp_hdr  || opts->disp_hdr1 || opts->disp_hdr2
                          || opts->disp_nim  || opts->disp_ana
                          || opts->disp_exts || opts->disp_cext) ? 1 : 0;
   ac += (opts->mod_hdr   || opts->mod_nim                     ) ? 1 : 0;
   ac += (opts->swap_hdr  || opts->swap_ana  || opts->swap_old ) ? 1 : 0;
   ac += (opts->add_exts  || opts->rm_exts                     ) ? 1 : 0;
   ac += (opts->strip                                          ) ? 1 : 0;
   ac += (opts->cbl                                            ) ? 1 : 0;
   ac += (opts->cci                                            ) ? 1 : 0;
   ac += (opts->dts       || opts->dci                         ) ? 1 : 0;

   if( ac < 1 )
   {
      fprintf(stderr,
              "** no action option, so nothing to do...\n"
              "   (try one of '-add...', '-diff...', '-disp...' or '-mod...')\n"
              "   (see '%s -help' for details)\n", prog);
      return 1;
   }
   else if( ac > 1 )
   {
      fprintf(stderr,
         "** only one action option is allowed, please use only one of:\n"
         "        '-add_...', '-check_...', '-diff_...', '-disp_...',\n"
         "        '-mod_...', '-strip', '-dts', '-cbl' or '-cci'\n"
         "   (see '%s -help' for details)\n", prog);
      return 1;
   }

   /* can modify nifti_1_header or nifti_image, but not both */
   if( opts->mod_hdr && opts->mod_nim )
   {
      fprintf(stderr,"** cannot use both '-mod_hdr' and '-mod_nim'\n");
      return 1;
   }

   /* can add or remove extensions, but not both */
   if( opts->add_exts && opts->rm_exts )
   {
      fprintf(stderr,"** cannot use both '-add_*_ext' and '-rm_ext'\n");
      return 1;
   }
   if( (opts->add_exts || opts->rm_exts) && opts->elist.len <= 0 )
   {
      fprintf(stderr,"** missing extensions to add or remove\n");
      return 1;
   }

   /* if modify, then we need fields and corresponding values */
   if( opts->mod_hdr || opts->mod_nim )
   {
      if( opts->flist.len <= 0 )
      {
         fprintf(stderr,"** missing field to modify (need '-mod_field' opt)\n");
         return 1;
      }
      if( opts->flist.len != opts->vlist.len )
      {
         fprintf(stderr,"** error: modifying %d fields with %d values\n",
                 opts->flist.len, opts->vlist.len);
         return 1;
      }
   }

   /* verify the number of files given for each of 4 action types */

   /* -diff_... : require nfiles == 2 */
   if( opts->diff_hdr1 || opts->diff_nim )
   {
     if( opts->infiles.len != 2 )
     {
      fprintf(stderr,"** '-diff_XXX' options require exactly 2 inputs files\n");
      return 1;
     }
   }
   /* if we are making changes, but not overwriting... */
   else if( (opts->elist.len > 0 || opts->mod_hdr || opts->mod_nim ||
             opts->swap_hdr || opts->swap_ana || opts->swap_old ) &&
            !opts->overwrite )
   {
      if( opts->infiles.len > 1 )
      {
         fprintf(stderr,"** without -overwrite, only one input file may be"
                          " modified at a time\n");
         errs++;
      }
      else if( ! opts->prefix )
      {
         fprintf(stderr,"** missing -prefix for output file\n");
         errs++;
      }
   }

   if( opts->dci_lines && ! opts->dts && ! opts->dci )
   {
      fprintf(stderr,"** option '-dci_lines' must only be used with '-dts'\n");
      errs++;
   }

   if( opts->infiles.len <= 0 ) /* in any case */
   {
      fprintf(stderr,"** missing input files (see -infiles option)\n");
      errs++;
   }

   if ( opts->overwrite && opts->prefix )
   {
      fprintf(stderr, "** please specify only one of -prefix and -overwrite\n");
      errs++;
   }

   if( errs ) return 1;

   if( g_debug > 1 ) fprintf(stderr,"+d options seem valid\n");

   return 0;
}


/*----------------------------------------------------------------------
 * re-assemble the command string into opts->command
 *----------------------------------------------------------------------*/
int fill_cmd_string( nt_opts * opts, int argc, char * argv[])
{
   char * cp;
   int    len, remain = NT_CMD_LEN;  /* NT_CMD_LEN is max command len */
   int    c, ac;
   int    has_space;  /* arguments containing space must be quoted */
   int    skip = 0;   /* counter to skip some of the arguments     */

   /* get the first argument separately */
   len = sprintf( opts->command, "\n  command: %s", argv[0] );
   cp = opts->command + len;
   remain -= len;

   /* get the rest, with special attention to input files */
   for( ac = 1; ac < argc; ac++ )
   {
      if( skip ){ skip--;  continue; }  /* then skip these arguments */

      len = strlen(argv[ac]);
      if( len + 3 >= remain ) {  /* extra 3 for space and possible '' */
         fprintf(stderr,"FCS: no space remaining for command, continuing...\n");
         return 1;
      }

      /* put the argument in, possibly with '' */

      has_space = 0;
      for( c = 0; c < len-1; c++ )
         if( isspace(argv[ac][c]) ){ has_space = 1; break; }
      if( has_space ) len = sprintf(cp, " '%s'", argv[ac]);
      else            len = sprintf(cp, " %s",   argv[ac]);

      remain -= len;

      /* infiles is okay, but after the *next* argument, we may skip files */
      /* (danger, will robinson!  hack alert!) */
      if( !strcmp(argv[ac-1],"-infiles") )
      {
         /* if more than 4 (just to be arbitrary) input files,
            include only the first and last */
         if( opts->infiles.len > 4 )
            skip = opts->infiles.len - 2;
      }

      cp += len;
   }

   if( g_debug > 1 ){
      fprintf(stderr,"+d filled command string, %d args, %d bytes\n",
              argc, (int)(cp - opts->command));
      if( g_debug > 2 ) fprintf(stderr,"%s\n", opts->command);
   }

   return 0;
}


/*----------------------------------------------------------------------
 * - only bother to alloc one pointer at a time (don't need efficiency here)
 * - return 0 on success
 *----------------------------------------------------------------------*/
int add_int(int_list * ilist, int val)
{
   if( ilist->len == 0 ) ilist->list = NULL;  /* just to be safe */
   ilist->len++;
   ilist->list = (int *)realloc(ilist->list,ilist->len*sizeof(int));
   if( ! ilist->list ){
      fprintf(stderr,"** failed to alloc %d (int *) elements\n",ilist->len);
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
int add_string(str_list * slist, const char * str)
{
   if( slist->len == 0 ) slist->list = NULL;  /* just to be safe */
   slist->len++;
   slist->list = (const char **)realloc(slist->list,slist->len*sizeof(char *));
   if( ! slist->list ){
      fprintf(stderr,"** failed to alloc %d (char *) elements\n",slist->len);
      return -1;
   }

   slist->list[slist->len-1] = str;

   return 0;
}


/*----------------------------------------------------------------------
 * display information on using the program
 *----------------------------------------------------------------------*/
int usage(char * prog, int level)
{
   int c, len;
   if( level == USE_SHORT )
   {
      fprintf(stdout,"usage %s [options] -infiles files...\n", prog);
      fprintf(stdout,"usage %s -help\n", prog);
      return -1;
   }
   else if( level == USE_FULL )
      use_full();  /* let's not allow paths in here */
   else if( level == USE_HIST )
   {
      len = sizeof(g_history)/sizeof(char *);
      for( c = 0; c < len; c++)
          fputs(g_history[c], stdout);
   }
   else if( level == USE_FIELD_HDR1 )
   {
      field_s nhdr_fields[NT_HDR1_NUM_FIELDS];  /* just do it all here */

      fill_hdr1_field_array(nhdr_fields);
      disp_field_s_list("nifti_1_header: ", nhdr_fields, NT_HDR1_NUM_FIELDS);
      printf("   sizeof(nifti_1_header) = %d\n", (int)sizeof(nifti_1_header));
   }
   else if( level == USE_FIELD_HDR2 )
   {
      field_s nhdr_fields[NT_HDR2_NUM_FIELDS];  /* just do it all here */

      fill_hdr2_field_array(nhdr_fields);
      disp_field_s_list("nifti_2_header: ", nhdr_fields, NT_HDR2_NUM_FIELDS);
      printf("   sizeof(nifti_2_header) = %d\n", (int)sizeof(nifti_2_header));
   }
   else if( level == USE_FIELD_ANA )
   {
      field_s nhdr_fields[NT_ANA_NUM_FIELDS];  /* just do it all here */

      fill_ana_field_array(nhdr_fields);
      disp_field_s_list("nifti_analyze75: ",nhdr_fields,NT_ANA_NUM_FIELDS);
      printf("   sizeof(nifti_analyze75) = %d\n", (int)sizeof(nifti_analyze75));
   }
   else if( level == USE_FIELD_NIM1 )
   {
      field_s nim_fields[NT_NIM_NUM_FIELDS];

      fill_nim1_field_array(nim_fields);
      disp_field_s_list("nifti1_image: ", nim_fields, NT_NIM_NUM_FIELDS);
      printf("   sizeof(nifti1_image) = %d\n", (int)sizeof(nifti1_image));
   }
   else if( level == USE_FIELD_NIM2 )
   {
      field_s nim_fields[NT_NIM_NUM_FIELDS];

      fill_nim2_field_array(nim_fields);
      disp_field_s_list("nifti2_image: ", nim_fields, NT_NIM_NUM_FIELDS);
      printf("   sizeof(nifti2_image) = %d\n", (int)sizeof(nifti2_image));
   }
   else if( level == USE_VERSION )
      fprintf(stdout, "%s, %s\n", prog, g_version);
   else {
      fprintf(stdout,"** illegal level for usage(): %d\n", level);
      return -1;
   }

   return 1;
}


/*----------------------------------------------------------------------
 * full usage
 *----------------------------------------------------------------------*/
int use_full()
{
   printf(
   "nifti_tool\n"
   "\n"
   "   - display, modify or compare nifti structures in datasets\n"
   "   - copy a dataset by selecting a list of volumes from the original\n"
   "   - copy a dataset, collapsing any dimensions, each to a single index\n"
   "   - display a time series for a voxel, or more generally, the data\n"
   "       from any collapsed image, in ASCII text\n");
   printf(
   "\n"
   "  This program can be used to display information from nifti datasets,\n"
   "  to modify information in nifti datasets, to look for differences\n"
   "  between two nifti datasets (like the UNIX 'diff' command), and to copy\n"
   "  a dataset to a new one, either by restricting any dimensions, or by\n"
   "  copying a list of volumes (the time dimension) from a dataset.\n"
   "\n");
   printf(
   "  Only one action type is allowed, e.g. one cannot modify a dataset\n"
   "  and then take a 'diff'.\n"
   "\n");
   printf(
   "  one can display - any or all fields in the nifti_1_header structure\n"
   "                  - any or all fields in the nifti_image structure\n"
   "                  - any or all fields in the nifti_analyze75 structure\n"
   "                  - the extensions in the nifti_image structure\n"
   "                  - the time series from a 4-D dataset, given i,j,k\n"
   "                  - the data from any collapsed image, given dims. list\n"
   "\n");
   printf(
   "  one can check   - perform internal check on the nifti_1_header struct\n"
   "                    (by nifti_hdr_looks_good())\n"
   "                  - perform internal check on the nifti_image struct\n"
   "                    (by nifti_nim_is_valid())\n"
   "\n");
   printf(
   "  one can modify  - any or all fields in the nifti_1_header structure\n"
   "                  - any or all fields in the nifti_image structure\n"
   "                  - swap all fields in NIFTI or ANALYZE header structure\n"
   "          add/rm  - any or all extensions in the nifti_image structure\n"
   "          remove  - all extensions and descriptions from the datasets\n"
   "\n");
   printf(
   "  one can compare - any or all field pairs of nifti_1_header structures\n"
   "                  - any or all field pairs of nifti_image structures\n"
   "\n"
   "  one can copy    - an arbitrary list of dataset volumes (time points)\n"
   "                  - a dataset, collapsing across arbitrary dimensions\n"
   "                    (restricting those dimensions to the given indices)\n"
   "\n"
   "  one can create  - a new dataset out of nothing\n"
   "\n");
   printf(
   "  Note: to learn about which fields exist in either of the structures,\n"
   "        or to learn a field's type, size of each element, or the number\n"
   "        of elements in the field, use either the '-help_hdr' option, or\n"
   "        the '-help_nim' option.  No further options are required.\n"
   "\n"
   "        See -help_hdr, -help_hdr1, -help_hdr2, -help_ana,\n"
   "            -help_nim, -help_nim1, -help_nim2.\n"
   "\n"
   "  ------------------------------\n");
   printf(
   "\n"
   "  usage styles:\n"
   "\n"
   "    nifti_tool -help                 : show this help\n"
   "    nifti_tool -help_hdr             : show nifti_2_header field info\n"
   "    nifti_tool -help_hdr1            : show nifti_1_header field info\n"
   "    nifti_tool -help_hdr2            : show nifti_2_header field info\n"
   "    nifti_tool -help_nim             : show nifti_image (2) field info\n"
   "    nifti_tool -help_nim1            : show nifti1_image field info\n"
   "    nifti_tool -help_nim2            : show nifti2_image field info\n"
   "    nifti_tool -help_ana             : show nifti_analyze75 field info\n"
   "    nifti_tool -help_datatypes       : show datatype table\n"
   "\n");
   printf(
   "    nifti_tool -ver                  : show the current version\n"
   "    nifti_tool -hist                 : show the modification history\n"
   "    nifti_tool -nifti_ver            : show the nifti library version\n"
   "    nifti_tool -nifti_hist           : show the nifti library history\n"
   "    nifti_tool -with_zlib            : was library compiled with zlib\n"
   "\n"
   "\n");
   printf(
   "    nifti_tool -check_hdr -infiles f1 ...\n"
   "    nifti_tool -check_nim -infiles f1 ...\n"
   "\n");
   printf(
   "    nifti_tool -copy_brick_list -infiles f1'[indices...]'\n"
   "    nifti_tool -copy_collapsed_image I J K T U V W -infiles f1\n"
   "    nifti_tool -copy_im -infiles f1\n"
   "\n");
   printf(
   "    nifti_tool -make_im -prefix new_im.nii\n"
   "\n");
   printf(
   "    nifti_tool -disp_hdr  [-field FIELDNAME] [...] -infiles f1 ...\n"
   "    nifti_tool -disp_hdr1 [-field FIELDNAME] [...] -infiles f1 ...\n"
   "    nifti_tool -disp_hdr2 [-field FIELDNAME] [...] -infiles f1 ...\n"
   "    nifti_tool -disp_nim  [-field FIELDNAME] [...] -infiles f1 ...\n"
   "    nifti_tool -disp_ana  [-field FIELDNAME] [...] -infiles f1 ...\n"
   "    nifti_tool -disp_exts -infiles f1 ...\n"
   "    nifti_tool -disp_cext -infiles f1 ...\n"
   "    nifti_tool -disp_ts I J K [-dci_lines] -infiles f1 ...\n"
   "    nifti_tool -disp_ci I J K T U V W [-dci_lines] -infiles f1 ...\n"
   "\n");
   printf(
   "    nifti_tool -mod_hdr  [-mod_field FIELDNAME NEW_VAL] [...] -infiles f1\n"
   "    nifti_tool -mod_nim  [-mod_field FIELDNAME NEW_VAL] [...] -infiles f1\n"
   "\n"
   "    nifti_tool -swap_as_nifti   -overwrite -infiles f1\n"
   "    nifti_tool -swap_as_analyze -overwrite -infiles f1\n"
   "    nifti_tool -swap_as_old     -overwrite -infiles f1\n"
   "\n");
   printf(
   "    nifti_tool -add_afni_ext    'extension in quotes' [...] -infiles f1\n"
   "    nifti_tool -add_comment_ext 'extension in quotes' [...] -infiles f1\n"
   "    nifti_tool -add_comment_ext 'file:FILENAME' [...] -infiles f1\n"
   "    nifti_tool -rm_ext INDEX [...] -infiles f1 ...\n"
   "    nifti_tool -strip_extras -infiles f1 ...\n"
   "\n");
   printf(
   "    nifti_tool -diff_hdr  [-field FIELDNAME] [...] -infiles f1 f2\n"
   "    nifti_tool -diff_hdr1 [-field FIELDNAME] [...] -infiles f1 f2\n"
   "    nifti_tool -diff_hdr2 [-field FIELDNAME] [...] -infiles f1 f2\n"
   "    nifti_tool -diff_nim  [-field FIELDNAME] [...] -infiles f1 f2\n"
   "\n"
   "  ------------------------------\n");

   printf(
   "\n"
   "  selected examples:\n"
   "\n"
   "    A. checks header (for problems):\n"
   "\n"
   "      1. nifti_tool -check_hdr -infiles dset0.nii dset1.nii\n"
   "      2. nifti_tool -check_hdr -infiles *.nii *.hdr\n"
   "      3. nifti_tool -check_hdr -quiet -infiles *.nii *.hdr\n"
   "\n");
   printf(
   "    B. show header differences:\n"
   "\n"
   "      1. nifti_tool -diff_hdr  -infiles dset0.nii dset1.nii \n"
   "      2. nifti_tool -diff_hdr1 -infiles dset0.nii dset1.nii \n"
   "      3. nifti_tool -diff_hdr2 -field dim -field intent_code  \\\n"
   "                    -infiles dset0.nii dset1.nii \n"
   "      4. nifti_tool -diff_hdr1 -new_dims 3 10 20 30 0 0 0 0   \\\n"
   "                    -infiles my_dset.nii MAKE_IM \n"
   "\n"
   "    C. display structures or fields:\n"
   "\n");
   printf(
   "      1. nifti_tool -disp_hdr -infiles dset0.nii dset1.nii dset2.nii\n"
  "      2. nifti_tool -disp_hdr1 -field dim -field descrip -infiles dset.nii\n"
  "      3. nifti_tool -disp_hdr2 -field dim -field descrip -infiles dset.nii\n"
   "      4. nifti_tool -disp_exts -infiles dset0.nii dset1.nii dset2.nii\n"
   "      5. nifti_tool -disp_cext -infiles dset0.nii dset1.nii dset2.nii\n"
   "      6. nifti_tool -disp_ts 23 0 172 -infiles dset1_time.nii\n"
   "      7. nifti_tool -disp_ci 23 0 172 -1 0 0 0 -infiles dset1_time.nii\n"
   "\n");
   printf(
   "      8. nifti_tool -disp_ana -infiles analyze.hdr\n"
   "      9. nifti_tool -disp_nim -infiles nifti.nii\n"
   "\n");
   printf(
   "    D. create a new dataset from nothing:\n"
   "\n"
   "      1. nifti_tool -make_im -prefix new_im.nii \n"
   "      2. nifti_tool -make_im -prefix float_im.nii \\\n"
   "                    -new_dims 3 10 20 30 0 0 0 0  -new_datatype 16\n");
   printf(
   "      3. nifti_tool -mod_hdr -mod_field descrip 'dataset with mods'  \\\n"
   "                    -new_dims 3 10 20 30 0 0 0 0                     \\\n"
   "                    -prefix new_desc.nii -infiles MAKE_IM\n"
   "\n");
   printf(
   "    E. copy dataset, brick list or collapsed image:\n"
   "\n"
   "      1. nifti_tool -copy_im -prefix new.nii -infiles dset0.nii\n"
   "      2. nifti_tool -cbl -prefix new_07.nii -infiles dset0.nii'[0,7]'\n"
   "      3. nifti_tool -cbl -prefix new_partial.nii \\\n"
   "                    -infiles dset0.nii'[3..$(2)]'\n"
   "\n"
   "      4. nifti_tool -cci 5 4 17 -1 -1 -1 -1 -prefix new_5_4_17.nii\n"
   "      5. nifti_tool -cci 5 0 17 -1 -1 2 -1  -keep_hist \\\n"
   "                    -prefix new_5_0_17_2.nii\n"
   "\n");
   printf(
   "    F. modify the header (modify fields or swap entire header):\n"
   "\n"
   "      1. nifti_tool -mod_hdr -prefix dnew -infiles dset0.nii  \\\n"
   "                    -mod_field dim '4 64 64 20 30 1 1 1 1'\n"
   "      2. nifti_tool -mod_hdr -prefix dnew -infiles dset0.nii  \\\n"
   "                    -mod_field descrip 'beer, brats and cheese, mmmmm...'\n"
   );
   printf(
 "      3. cp old_dset.hdr nifti_swap.hdr \n"
 "         nifti_tool -swap_as_nifti -overwrite -infiles nifti_swap.hdr\n"
 "      4. cp old_dset.hdr analyze_swap.hdr \n"
 "         nifti_tool -swap_as_analyze -overwrite -infiles analyze_swap.hdr\n"
 "      5. nifti_tool -swap_as_old -prefix old_swap.hdr -infiles old_dset.hdr\n"
 "         nifti_tool -diff_hdr1 -infiles nifti_swap.hdr old_swap.hdr\n"
   "\n");
   printf(
   "    G. strip, add or remove extensions:\n"
   "       (in example #3, the extension is copied from a text file)\n"
   "\n"
   "\n"
   "      1. nifti_tool -strip -overwrite -infiles *.nii\n"
   "      2. nifti_tool -add_comment 'converted from MY_AFNI_DSET+orig' \\\n"
   "                    -prefix dnew -infiles dset0.nii\n"
   );
   printf(
   "      3. nifti_tool -add_comment 'file:my.extension.txt' \\\n"
   "                    -prefix dnew -infiles dset0.nii\n"
   "      4. nifti_tool -rm_ext ALL -prefix dset1 -infiles dset0.nii\n"
   "      5. nifti_tool -rm_ext 2 -rm_ext 3 -rm_ext 5 -overwrite \\\n"
   "                    -infiles dset0.nii\n"
   "\n"
   "  ------------------------------\n");
   printf(
   "\n"
   "  options for check actions:\n"
   "\n");
   printf(
   "    -check_hdr         : check for a valid nifti_1_header struct\n"
   "\n"
   "       This action is used to check the nifti_1_header structure for\n"
   "       problems.  The nifti_hdr_looks_good() function is used for the\n"
   "       test, and currently checks:\n"
   "       \n"
   "         dim[], sizeof_hdr, magic, datatype\n"
   "       \n"
   "       More tests can be requested of the author.\n"
   "\n");
   printf(
   "       e.g. perform checks on the headers of some datasets\n"
   "       nifti_tool -check_hdr -infiles dset0.nii dset1.nii\n"
   "       nifti_tool -check_hdr -infiles *.nii *.hdr\n"
   "       \n"
   "       e.g. add the -quiet option, so that only errors are reported\n"
   "       nifti_tool -check_hdr -quiet -infiles *.nii *.hdr\n"
   "\n");
   printf(
   "    -check_nim         : check for a valid nifti_image struct\n"
   "\n"
   "       This action is used to check the nifti_image structure for\n"
   "       problems.  This is tested via both nifti_convert_n1hdr2nim()\n"
   "       and nifti_nim_is_valid(), though other functions are called\n"
   "       below them, of course.  Current checks are:\n"
   "\n");
   printf(
   "         dim[], sizeof_hdr, datatype, fname, iname, nifti_type\n"
   "       \n"
   "       Note that creation of a nifti_image structure depends on good\n"
   "       header fields.  So errors are terminal, meaning this check would\n"
   "       probably report at most one error, even if more exist.  The\n"
   "       -check_hdr action is more complete.\n"
   "\n");
   printf(
   "       More tests can be requested of the author.\n"
   "\n");
   printf(
   "             e.g. nifti_tool -check_nim -infiles dset0.nii dset1.nii\n"
   "             e.g. nifti_tool -check_nim -infiles *.nii *.hdr\n"
   "\n");
   printf(
   "  ------------------------------\n");

   printf(
   "\n"
   "  options for create action:\n"
   "\n");
   printf(
   "    -make_im           : create a new dataset from nothing\n"
   "\n"
   "       With this the user can create a new dataset of a basic style,\n"
   "       which can then be modified with other options.  This will create\n"
   "       zero-filled data of the appropriate size.\n"
   "       \n");
   printf(
   "       The default is a 1x1x1 image of shorts.  These settings can be\n"
   "       modified with the -new_dim option, to set the 8 dimension values,\n"
   "       and the -new_datatype, to provide the integral type for the data.\n"
   "\n");
   printf(
   "       See -new_dim, -new_datatype and -infiles for more information.\n"
   "       \n"
   "       Note that any -infiles dataset of the name MAKE_IM will also be\n"
   "       created on the fly.\n"
   "\n");
   printf(
   "    -new_dim D0 .. D7  : specify the dim array for the a new dataset.\n"
   "\n"
   "         e.g. -new_dim 4 64 64 27 120 0 0 0\n"
   "\n"
   "       This dimension list will apply to any dataset created via\n"
   "       MAKE_IM or -make_im.  All 8 values are required.  Recall that\n"
   "       D0 is the number of dimensions, and D1 through D7 are the sizes.\n"
   "       \n");
   printf(
   "    -new_datatype TYPE : specify the dim array for the a new dataset.\n"
   "\n"
   "         e.g. -new_datatype 16\n"
   "         default: -new_datatype 4   (short)\n"
   "\n"
   "       This dimension list will apply to any dataset created via\n"
   "       MAKE_IM or -make_im.  TYPE should be one of the NIFTI_TYPE_*\n"
   "       numbers, from nifti1.h.\n"
   "       \n");
   printf(
   "  ------------------------------\n");
   printf(
   "\n"
   "  options for copy actions:\n"
   "\n"
   "    -copy_brick_list   : copy a list of volumes to a new dataset\n"
   "    -cbl               : (a shorter, alternative form)\n"
   "    -copy_im           : (a shorter, alternative form)\n"
   "\n");
   printf(
   "       This action allows the user to copy a list of volumes (over time)\n"
   "       from one dataset to another.  The listed volumes can be in any\n"
   "       order and contain repeats, but are of course restricted to\n"
   "       the set of values {1, 2, ..., nt-1}, from dimension 4.\n"
   "\n");
   printf(
   "       This option is a flag.  The index list is specified with the input\n"
   "       dataset, contained in square brackets.  Note that square brackets\n"
   "       are special to most UNIX shells, so they should be contained\n"
   "       within single quotes.  Syntax of an index list:\n"
   "\n"
   "       notes:\n"
   "\n");
   printf(
   "         - indices start at zero\n"
   "         - indices end at nt-1, which has the special symbol '$'\n"
   "         - single indices should be separated with commas, ','\n"
   "             e.g. -infiles dset0.nii'[0,3,8,5,2,2,2]'\n"
   "         - ranges may be specified using '..' or '-' \n");
   printf(
   "             e.g. -infiles dset0.nii'[2..95]'\n"
   "             e.g. -infiles dset0.nii'[2..$]'\n"
   "         - ranges may have step values, specified in ()\n"
   "           example: 2 through 95 with a step of 3, i.e. {2,5,8,11,...,95}\n"
   "             e.g. -infiles dset0.nii'[2..95(3)]'\n"
   "\n");
   printf(
   "       This functionality applies only to 3 or 4-dimensional datasets.\n"
   "\n"
   "       e.g. to copy a dataset:\n"
   "       nifti_tool -copy_im -prefix new.nii -infiles dset0.nii\n"
   "\n");
   printf(
   "       e.g. to copy sub-bricks 0 and 7:\n"
   "       nifti_tool -cbl -prefix new_07.nii -infiles dset0.nii'[0,7]'\n"
   "\n"
   "       e.g. to copy an entire dataset:\n"
   "       nifti_tool -cbl -prefix new_all.nii -infiles dset0.nii'[0..$]'\n"
   "\n");
   printf(
   "       e.g. to copy every other time point, skipping the first three:\n"
   "       nifti_tool -cbl -prefix new_partial.nii \\\n"
   "                  -infiles dset0.nii'[3..$(2)]'\n"
   "\n"
   "\n"
   "    -copy_collapsed_image ... : copy a list of volumes to a new dataset\n"
   "    -cci I J K T U V W        : (a shorter, alternative form)\n"
   "\n");
   printf(
   "       This action allows the user to copy a collapsed dataset, where\n"
   "       some dimensions are collapsed to a given index.  For instance, the\n"
   "       X dimension could be collapsed to i=42, and the time dimensions\n"
   "       could be collapsed to t=17.  To collapse a dimension, set Di to\n"
   "       the desired index, where i is in {0..ni-1}.  Any dimension that\n"
   "       should not be collapsed must be listed as -1.\n"
   "\n");
   printf(
   "       Any number (of valid) dimensions can be collapsed, even down to a\n"
   "       a single value, by specifying enough valid indices.  The resulting\n"
   "       dataset will then have a reduced number of non-trivial dimensions.\n"
   "\n"
   "       Assume dset0.nii has nim->dim[8] = { 4, 64, 64, 21, 80, 1, 1, 1 }.\n"
   "       Note that this is a 4-dimensional dataset.\n"
   "\n");
   printf(
   "         e.g. copy the time series for voxel i,j,k = 5,4,17\n"
   "         nifti_tool -cci 5 4 17 -1 -1 -1 -1 -prefix new_5_4_17.nii\n"
   "\n"
   "         e.g. read the single volume at time point 26\n"
   "         nifti_tool -cci -1 -1 -1 26 -1 -1 -1 -prefix new_t26.nii\n"
   "\n");
   printf(
   "       Assume dset1.nii has nim->dim[8] = { 6, 64, 64, 21, 80, 4, 3, 1 }.\n"
   "       Note that this is a 6-dimensional dataset.\n"
   "\n"
   "         e.g. copy all time series for voxel i,j,k = 5,0,17, with v=2\n"
   "              (and add the command to the history)\n"
   "         nifti_tool -cci 5 0 17 -1 -1 2 -1  -keep_hist \\\n"
   "                    -prefix new_5_0_17_2.nii\n"
   "\n");
   printf(
   "         e.g. copy all data where i=3, j=19 and v=2\n"
   "              (I do not claim to know a good reason to do this)\n"
   "         nifti_tool -cci 3 19 -1 -1 -1 2 -1 -prefix new_mess.nii\n"
   "\n"
   "       See '-disp_ci' for more information (which displays/prints the\n"
   "       data, instead of copying it to a new dataset).\n"
   "\n"
   "  ------------------------------\n");

   printf(
   "\n"
   "  options for display actions:\n"
   "\n"
   "    -disp_hdr          : display nifti_*_header fields for datasets\n"
   "\n"
   "       This flag means the user wishes to see some of the nifti_*_header\n"
   "       fields in one or more nifti datasets. The user may want to specify\n"
   "       multiple '-field' options along with this.  This option requires\n"
   "       one or more files input, via '-infiles'.\n"
   "\n"
   "       This displays the header in its native format.\n"
   "\n");
   printf(
   "       If no '-field' option is present, all fields will be displayed.\n"
   "\n"
   "       e.g. to display the contents of all fields:\n"
   "       nifti_tool -disp_hdr -infiles dset0.nii\n"
   "       nifti_tool -disp_hdr -infiles dset0.nii dset1.nii dset2.nii\n"
   "\n"
   "       e.g. to display the contents of select fields:\n"
   "       nifti_tool -disp_hdr -field dim -infiles dset0.nii\n"
   "       nifti_tool -disp_hdr -field dim -field descrip -infiles dset0.nii\n"
   "\n");
   printf(
   "\n"
   "    -disp_hdr1          : display nifti_1_header fields for datasets\n"
   "\n"
   "       Like -disp_hdr, but only display NIFTI-1 format.\n"
   "\n"
   "       This attempts to convert other NIFTI versions to NIFTI-1.\n"
   "\n");
   printf(
   "\n"
   "    -disp_hdr2          : display nifti_2_header fields for datasets\n"
   "\n"
   "       Like -disp_hdr, but only display NIFTI-2 format.\n"
   "\n"
   "       This attempts to convert other NIFTI versions to NIFTI-2.\n"
   "\n");
   printf(
   "    -disp_nim          : display nifti_image fields for datasets\n"
   "\n"
   "       This flag option works the same way as the '-disp_hdr' option,\n"
   "       except that the fields in question are from the nifti_image\n"
   "       structure.\n"
   "\n");
   printf(
   "    -disp_ana          : display nifti_analyze75 fields for datasets\n"
   "\n"
   "       This flag option works the same way as the '-disp_hdr' option,\n"
   "       except that the fields in question are from the nifti_analyze75\n"
   "       structure.\n"
   "\n");
   printf(
   "    -disp_cext         : display CIFTI-type extensions\n"
   "\n"
   "       This flag option is used to display all CIFTI extension data.\n"
   "\n");
   printf(
   "    -disp_exts         : display all AFNI-type extensions\n"
   "\n"
   "       This flag option is used to display all nifti_1_extension data,\n"
   "       for extensions of type AFNI (4), COMMENT (6) or CIFTI (32).\n"
   "\n");
   printf(
   "       e.g. to display the extensions in datasets:\n"
   "       nifti_tool -disp_exts -infiles dset0.nii\n"
   "       nifti_tool -disp_exts -infiles dset0.nii dset1.nii dset2.nii\n"
   "\n");
   printf(
   "    -disp_ts I J K    : display ASCII time series at i,j,k = I,J,K\n"
   "\n"
   "       This option is used to display the time series data for the voxel\n"
   "       at i,j,k indices I,J,K.  The data is displayed in text, either all\n"
   "       on one line (the default), or as one number per line (via the\n"
   "       '-dci_lines' option).\n"
   "\n");
   printf(
   "       Notes:\n"
   "\n"
   "         o This function applies only to 4-dimensional datasets.\n"
   "         o The '-quiet' option can be used to suppress the text header,\n"
   "           leaving only the data.\n"
   "         o This option is short for using '-disp_ci' (display collapsed\n"
   "           image), restricted to 4-dimensional datasets.  i.e. :\n"
   "               -disp_ci I J K -1 -1 -1 -1\n"
   "\n");
   printf(
   "       e.g. to display the time series at voxel 23, 0, 172:\n"
   "       nifti_tool -disp_ts 23 0 172            -infiles dset1_time.nii\n"
   "       nifti_tool -disp_ts 23 0 172 -dci_lines -infiles dset1_time.nii\n"
   "       nifti_tool -disp_ts 23 0 172 -quiet     -infiles dset1_time.nii\n"
   "\n");
   printf(
   "    -disp_collapsed_image  : display ASCII values for collapsed dataset\n"
   "    -disp_ci I J K T U V W : (a shorter, alternative form)\n"
   "\n"
   "       This option is used to display all of the data from a collapsed\n"
   "       image, given the dimension list.  The data is displayed in text,\n"
   "       either all on one line (the default), or as one number per line\n"
   "       (by using the '-dci_lines' flag).\n"
   "\n");
   printf(
   "       The '-quiet' option can be used to suppress the text header.\n"
   "\n"
   "       e.g. to display the time series at voxel 23, 0, 172:\n"
   "       nifti_tool -disp_ci 23 0 172 -1 0 0 0 -infiles dset1_time.nii\n"
   "\n"
   "       e.g. to display z-slice 14, at time t=68:\n"
   "       nifti_tool -disp_ci -1 -1 14 68 0 0 0 -infiles dset1_time.nii\n"
   "\n"
   "       See '-ccd' for more information, which copies such data to a new\n"
   "       dataset, instead of printing it to the terminal window.\n"
   "\n"
   "  ------------------------------\n");
   printf(
   "\n"
   "  options for modification actions:\n"
   "\n"
   "    -mod_hdr           : modify nifti_1_header fields for datasets\n"
   "\n"
   "       This action is used to modify some of the nifti_1_header fields in\n"
   "       one or more datasets.  The user must specify a list of fields to\n"
   "       modify via one or more '-mod_field' options, which include field\n"
   "       names, along with the new (set of) values.\n"
   "\n");
   printf(
   "       The user can modify a dataset in place, or use '-prefix' to\n"
   "       produce a new dataset, to which the changes have been applied.\n"
   "       It is recommended to normally use the '-prefix' option, so as not\n"
   "       to ruin a dataset.\n"
   "\n");
   printf(
   "       Note that some fields have a length greater than 1, meaning that\n"
   "       the field is an array of numbers, or a string of characters.  In\n"
   "       order to modify an array of numbers, the user must provide the\n"
   "       correct number of values, and contain those values in quotes, so\n"
   "       that they are seen as a single option.\n"
   "\n");
   printf(
   "       To modify a string field, put the string in quotes.\n"
   "\n"
   "       The '-mod_field' option takes a field_name and a list of values.\n"
   "\n"
   "       e.g. to modify the contents of various fields:\n"
   "\n");
   printf(
   "       nifti_tool -mod_hdr -prefix dnew -infiles dset0.nii  \\\n"
   "                  -mod_field qoffset_x -17.325\n"
   "       nifti_tool -mod_hdr -prefix dnew -infiles dset0.nii  \\\n"
   "                  -mod_field dim '4 64 64 20 30 1 1 1 1'\n"
   "       nifti_tool -mod_hdr -prefix dnew -infiles dset0.nii  \\\n"
   "                  -mod_field descrip 'beer, brats and cheese, mmmmm...'\n"
   "\n");
   printf(
   "       e.g. to modify the contents of multiple fields:\n"
   "       nifti_tool -mod_hdr -prefix dnew -infiles dset0.nii  \\\n"
   "                  -mod_field qoffset_x -17.325 -mod_field slice_start 1\n"
   "\n"
   "       e.g. to modify the contents of multiple files (must overwrite):\n"
   "       nifti_tool -mod_hdr -overwrite -mod_field qoffset_x -17.325   \\\n"
   "                  -infiles dset0.nii dset1.nii\n"
   "\n");
   printf(
   "    -mod_nim          : modify nifti_image fields for datasets\n"
   "\n"
   "       This action option is used the same way that '-mod_hdr' is used,\n"
   "       except that the fields in question are from the nifti_image\n"
   "       structure.\n"
   "\n");
   printf(
   "    -strip_extras     : remove extensions and descriptions from datasets\n"
   "\n"
   "       This action is used to attempt to 'clean' a dataset of general\n"
   "       text, in order to make it more anonymous.  Extensions and the\n"
   "       nifti_image descrip field are cleared by this action.\n"
   "\n");
   printf(
   "       e.g. to strip all *.nii datasets in this directory:\n"
   "       nifti_tool -strip -overwrite -infiles *.nii\n"
   "\n");
   printf(
   "    -swap_as_nifti    : swap the header according to nifti_1_header\n"
   "\n"
   "       Perhaps a NIfTI header is mal-formed, and the user explicitly\n"
   "       wants to swap it before performing other operations.  This action\n"
   "       will swap the field bytes under the assumption that the header is\n"
   "       in the NIfTI format.\n"
   "\n");
   printf(
   "       ** The recommended course of action is to make a copy of the\n"
   "          dataset and overwrite the header via -overwrite.  If the header\n"
   "          needs such an operation, it is likely that the data would not\n"
   "          otherwise be read in correctly.\n"
   "\n");
   printf(
   "    -swap_as_analyze  : swap the header according to nifti_analyze75\n"
   "\n"
   "       Perhaps an ANALYZE header is mal-formed, and the user explicitly\n"
   "       wants to swap it before performing other operations.  This action\n"
   "       will swap the field bytes under the assumption that the header is\n"
   "       in the ANALYZE 7.5 format.\n"
   "\n");
   printf(
   "       ** The recommended course of action is to make a copy of the\n"
   "          dataset and overwrite the header via -overwrite.  If the header\n"
   "          needs such an operation, it is likely that the data would not\n"
   "          otherwise be read in correctly.\n"
   "\n");
   printf(
   "    -swap_as_old      : swap the header using the old method\n"
   "\n"
   "       As of library version 1.35 (3 Aug, 2008), nifticlib now swaps all\n"
   "       fields of a NIfTI dataset (including UNUSED ones), and it swaps\n"
   "       ANALYZE datasets according to the nifti_analyze75 structure.\n"
   "       This is a significant different in the case of ANALYZE datasets.\n"
   "\n");
   printf(
   "       The -swap_as_old option was added to compare the results of the\n"
   "       swapping methods, or to undo one swapping method and replace it\n"
   "       with another (such as to undo the old method and apply the new).\n"
   "\n");
   printf("  ------------------------------\n");
   printf(
   "\n"
   "  options for adding/removing extensions:\n"
   "\n"
   "    -add_afni_ext EXT : add an AFNI extension to the dataset\n"
   "\n"
   "       This option is used to add AFNI-type extensions to one or more\n"
   "       datasets.  This option may be used more than once to add more than\n"
   "       one extension.\n"
   "\n"
   "       If EXT is of the form 'file:FILENAME', then the extension will\n"
   "       be read from the file, FILENAME.\n"
   "\n");
   printf(
   "       The '-prefix' option is recommended, to create a new dataset.\n"
   "       In such a case, only a single file may be taken as input.  Using\n"
   "       '-overwrite' allows the user to overwrite the current file, or\n"
   "       to add the extension(s) to multiple files, overwriting them.\n"
   "\n");
   printf(
   "       e.g. to add a generic AFNI extension:\n"
   "       nifti_tool -add_afni_ext 'wow, my first extension' -prefix dnew \\\n"
   "                  -infiles dset0.nii\n"
   "\n"
   "       e.g. to add multiple AFNI extensions:\n"
   "       nifti_tool -add_afni_ext 'wow, my first extension :)'      \\\n"
   "                  -add_afni_ext 'look, my second...'              \\\n"
   "                  -prefix dnew -infiles dset0.nii\n"
   "\n");
   printf(
   "       e.g. to add an extension, and overwrite the dataset:\n"
   "       nifti_tool -add_afni_ext 'some AFNI extension' -overwrite \\\n"
   "                  -infiles dset0.nii dset1.nii \n"
   "\n");
   printf(
   "    -add_comment_ext EXT : add a COMMENT extension to the dataset\n"
   "\n"
   "       This option is used to add COMMENT-type extensions to one or more\n"
   "       datasets.  This option may be used more than once to add more than\n"
   "       one extension.  This option may also be used with '-add_afni_ext'.\n"
   "\n"
   "       If EXT is of the form 'file:FILENAME', then the extension will\n"
   "       be read from the file, FILENAME.\n"
   "\n");
   printf(
   "       The '-prefix' option is recommended, to create a new dataset.\n"
   "       In such a case, only a single file may be taken as input.  Using\n"
   "       '-overwrite' allows the user to overwrite the current file, or\n"
   "       to add the extension(s) to multiple files, overwriting them.\n"
   "\n");
   printf(
   "       e.g. to add a comment about the dataset:\n"
   "       nifti_tool -add_comment 'converted from MY_AFNI_DSET+orig' \\\n"
   "                  -prefix dnew                                    \\\n"
   "                  -infiles dset0.nii\n"
   "\n");
   printf(
   "       e.g. to add multiple extensions:\n"
   "       nifti_tool -add_comment  'add a comment extension'         \\\n"
   "                  -add_afni_ext 'and an AFNI XML style extension' \\\n"
   "                  -add_comment  'dataset copied from dset0.nii'   \\\n"
   "                  -prefix dnew -infiles dset0.nii\n"
   "\n");
   printf(
   "    -rm_ext INDEX     : remove the extension given by INDEX\n"
   "\n"
   "       This option is used to remove any single extension from the\n"
   "       dataset.  Multiple extensions require multiple options.\n"
   "\n"
   "       notes  - extension indices begin with 0 (zero)\n"
   "              - to view the current extensions, see '-disp_exts'\n"
   "              - all extensions can be removed using ALL or -1 for INDEX\n"
   "\n");
   printf(
   "       e.g. to remove the extension #0:\n"
   "       nifti_tool -rm_ext 0 -overwrite -infiles dset0.nii\n"
   "\n"
   "       e.g. to remove ALL extensions:\n"
   "       nifti_tool -rm_ext ALL -prefix dset1 -infiles dset0.nii\n"
   "       nifti_tool -rm_ext -1  -prefix dset1 -infiles dset0.nii\n"
   "\n");
   printf(
   "       e.g. to remove the extensions #2, #3 and #5:\n"
   "       nifti_tool -rm_ext 2 -rm_ext 3 -rm_ext 5 -overwrite \\\n"
   "                  -infiles dset0.nii\n"
   "\n"
   "  ------------------------------\n");

   printf(
   "\n"
   "  options for showing differences:\n"
   "\n"
   "    -diff_hdr         : display header field diffs between two datasets\n"
   "\n"
   "       This option is used to find differences between two NIFTI-*\n"
   "       dataset headers.  If any fields are different, the contents of\n"
   "       those fields are displayed (unless the '-quiet' option is used).\n"
   "\n"
   "       The NIFTI versions must agree.\n"
   "\n"
   "    -diff_hdr1        : display header diffs between NIFTI-1 datasets\n"
   "\n"
   "       This option is used to find differences between two NIFTI-1\n"
   "       dataset headers.\n"
   "\n"
   "    -diff_hdr2        : display header diffs between NIFTI-2 datasets\n"
   "\n"
   "       This option is used to find differences between two NIFTI-2\n"
   "       dataset headers.\n"
   "\n");
   printf(
   "       A list of fields can be specified by using multiple '-field'\n"
   "       options.  If no '-field' option is given, all fields will be\n"
   "       checked.\n"
   "\n"
   "       Exactly two dataset names must be provided via '-infiles'.\n"
   "\n"
   "       e.g. to display all nifti_2_header field differences:\n"
   "       nifti_tool -diff_hdr2 -infiles dset0.nii dset1.nii\n"
   "\n");
   printf(
   "       e.g. to display selected field differences:\n"
   "       nifti_tool -diff_hdr -field dim -field intent_code  \\\n"
   "                  -infiles dset0.nii dset1.nii \n"
   "\n"
   "    -diff_nim         : display nifti_image field diffs between datasets\n"
   "\n"
   "       This option works the same as '-diff_hdr', except that the fields\n"
   "       in question are from the nifti_image structure.\n"
   "\n"
   "  ------------------------------\n");

   printf(
   "\n"
   "  miscellaneous options:\n"
   "\n"
   "    -debug LEVEL      : set the debugging level\n"
   "\n"
   "       Level 0 will attempt to operate with no screen output, but errors.\n"
   "       Level 1 is the default.\n"
   "       Levels 2 and 3 give progressively more information.\n"
   "\n"
   "       e.g. -debug 2\n"
   "\n");
   printf(
   "    -field FIELDNAME  : provide a field to work with\n"
   "\n"
   "       This option is used to provide a field to display, modify or\n"
   "       compare.  This option can be used along with one of the action\n"
   "       options presented above.\n"
   "\n"
   "       See '-disp_hdr', above, for complete examples.\n"
   "\n"
   "       e.g. nifti_tool -field descrip\n"
   "       e.g. nifti_tool -field descrip -field dim\n"
   "\n");
   printf(
   "    -infiles file0... : provide a list of files to work with\n"
   "\n"
   "       This parameter is required for any of the actions, in order to\n"
   "       provide a list of files to process.  If input filenames do not\n"
   "       have an extension, the directory we be searched for any\n"
   "       appropriate files (such as .nii or .hdr).\n"
   "\n");
   printf(
   "       Note: if the filename has the form MAKE_IM, then a new dataset\n"
   "       will be created, without the need for file input.\n"
   "\n");
   printf(
   "       See '-mod_hdr', above, for complete examples.\n"
   "\n"
   "       e.g. nifti_tool -infiles file0.nii\n"
   "       e.g. nifti_tool -infiles file1.nii file2 file3.hdr\n"
   "\n");
   printf(
   "    -mod_field NAME 'VALUE_LIST' : provide new values for a field\n"
   "\n"
   "       This parameter is required for any the modification actions.\n"
   "       If the user wants to modify any fields of a dataset, this is\n"
   "       where the fields and values are specified.\n"
   "\n");
   printf(
   "       NAME is a field name (in either the nifti_1_header structure or\n"
   "       the nifti_image structure).  If the action option is '-mod_hdr',\n"
   "       then NAME must be the name of a nifti_1_header field.  If the\n"
   "       action is '-mod_nim', NAME must be from a nifti_image structure.\n"
   "\n");
   printf(
   "       VALUE_LIST must be one or more values, as many as are required\n"
   "       for the field, contained in quotes if more than one is provided.\n"
   "\n"
   "       Use 'nifti_tool -help_hdr' to get a list of nifti_2_header fields\n"
   "       Use 'nifti_tool -help_hdr1' to get a list of nifti_1_header fields\n"
   "       Use 'nifti_tool -help_hdr2' to get a list of nifti_2_header fields\n"
   "       Use 'nifti_tool -help_nim' to get a list of nifti_image fields\n"
   "       Use 'nifti_tool -help_nim1' to get a list of nifti1_image fields\n"
   "       Use 'nifti_tool -help_nim2' to get a list of nifti2_image fields\n"
   "       Use 'nifti_tool -help_ana' to get a list of nifti_analyze75 fields\n"
   "\n"
   "       See '-mod_hdr', above, for complete examples.\n"
   "\n");
   printf(
   "       e.g. modifying nifti_1_header fields:\n"
   "            -mod_field descrip 'toga, toga, toga'\n"
   "            -mod_field qoffset_x 19.4 -mod_field qoffset_z -11\n"
   "            -mod_field pixdim '1 0.9375 0.9375 1.2 1 1 1 1'\n"
   "\n");
   printf(
   "    -keep_hist         : add the command as COMMENT (to the 'history')\n"
   "\n"
   "        When this option is used, the current command will be added\n"
   "        as a NIFTI_ECODE_COMMENT type extension.  This provides the\n"
   "        ability to keep a history of commands affecting a dataset.\n"
   "\n"
   "       e.g. -keep_hist\n"
   "\n");
   printf(
   "    -overwrite        : any modifications will be made to input files\n"
   "\n"
   "       This option is used so that all field modifications, including\n"
   "       extension additions or deletions, will be made to the files that\n"
   "       are input.\n"
   "\n");
   printf(
   "       In general, the user is recommended to use the '-prefix' option\n"
   "       to create new files.  But if overwriting the contents of the\n"
   "       input files is preferred, this is how to do it.\n"
   "\n"
   "       See '-mod_hdr' or '-add_afni_ext', above, for complete examples.\n"
   "\n"
   "       e.g. -overwrite\n"
   "\n");
   printf(
   "    -prefix           : specify an output file to write change into\n"
   "\n"
   "       This option is used to specify an output file to write, after\n"
   "       modifications have been made.  If modifications are being made,\n"
   "       then either '-prefix' or '-overwrite' is required.\n"
   "\n"
   "       If no extension is given, the output extension will be '.nii'.\n"
   "\n");
   printf(
   "       e.g. -prefix new_dset\n"
   "       e.g. -prefix new_dset.nii\n"
   "       e.g. -prefix new_dset.hdr\n"
   "\n"
   "    -quiet            : report only errors or requested information\n"
   "\n"
   "       This option is equivalent to '-debug 0'.\n"
   "\n"
   "  ------------------------------\n");

   printf(
   "\n"
   "  basic help options:\n"
   "\n"
   "    -help             : show this help\n"
   "\n"
   "       e.g.  nifti_tool -help\n"
   "\n"
   "    -help_hdr         : show nifti_2_header field info\n"
   "\n"
   "       e.g.  nifti_tool -help_hdr\n"
   "\n"
   "    -help_hdr1        : show nifti_1_header field info\n"
   "\n"
   "       e.g.  nifti_tool -help_hdr1\n"
   "\n"
   "    -help_hdr2        : show nifti_2_header field info\n"
   "\n"
   "       e.g.  nifti_tool -help_hdr2\n"
   "\n"
   "    -help_nim         : show nifti_image field info (currently NIFTI-2)\n"
   "\n"
   "       e.g.  nifti_tool -help_nim\n"
   "\n"
   "    -help_nim1         : show nifti1_image field info\n"
   "\n"
   "       e.g.  nifti_tool -help_nim1\n"
   "\n"
   "    -help_nim2         : show nifti2_image field info\n"
   "\n"
   "       e.g.  nifti_tool -help_nim2\n"
   "\n"
   "    -help_ana         : show nifti_analyze75 field info\n"
   "\n"
   "       e.g.  nifti_tool -help_ana\n"
   );

   printf(
   "\n"
   "    -help_datatypes [TYPE] : display datatype table\n"
   "\n"
   "       e.g.  nifti_tool -help_datatypes\n"
   "       e.g.  nifti_tool -help_datatypes N\n"
   "\n"
   "       This displays the contents of the nifti_type_list table.\n"
   "       An additional 'D' or 'N' parameter will restrict the type\n"
   "       name to 'DT_' or 'NIFTI_TYPE_' names, 'T' will test.\n");

   printf(
   "\n"
   "    -ver              : show the program version number\n"
   "\n"
   "       e.g.  nifti_tool -ver\n"
   "\n"
   "    -hist             : show the program modification history\n"
   "\n"
   "       e.g.  nifti_tool -hist\n"
   "\n");
   printf(
   "    -nifti_ver        : show the nifti library version number\n"
   "\n"
   "       e.g.  nifti_tool -nifti_ver\n"
   "\n"
   "    -nifti_hist       : show the nifti library modification history\n"
   "\n"
   "       e.g.  nifti_tool -nifti_hist\n"
   "\n"
   "    -with_zlib        : print whether library was compiled with zlib\n"
   "\n"
   "       e.g.  nifti_tool -with_zlib\n"
   "\n"
   "  ------------------------------\n"
   "\n"
   "  R. Reynolds\n"
   "  compiled: %s\n"
   "  %s\n\n",
   __DATE__, g_version );

   return 1;
}


/*----------------------------------------------------------------------
 * display the contents of the struct and all lists
 *----------------------------------------------------------------------*/
int disp_nt_opts( const char *mesg, nt_opts * opts)
{
   int c;

   if( mesg ) fputs(mesg, stderr);
   if( ! opts )
   {
      fprintf(stderr,"** disp_nt_opts: missing opts\n");
      return -1;
   }

   fprintf(stderr,"nt_opts @ %p\n"
                  "   check_hdr, check_nim = %d, %d\n"
                  "   diff_hdr1, diff_hdr2 = %d, %d\n"
                  "   diff_hdr,  diff_nim  = %d, %d\n"
                  "   disp_hdr1, disp_hdr2 = %d, %d\n"
                  "   disp_hdr,  disp_nim  = %d, %d\n"
                  "   disp_ana, disp_exts  = %d, %d\n"
                  "   disp_cext            = %d\n"
                  "   add_exts, rm_exts    = %d, %d\n"
                  "   mod_hdr,  mod_nim    = %d, %d\n"
                  "   swap_hdr, swap_ana   = %d, %d\n"
                  "   swap_old             = %d\n"
                  "   cbl, cci             = %d, %d\n"
                  "   dts, dci_lines       = %d, %d\n"
                  "   make_im              = %d\n",
            (void *)opts,
            opts->check_hdr, opts->check_nim,
            opts->diff_hdr1, opts->diff_hdr2,
            opts->diff_hdr, opts->diff_nim,
            opts->disp_hdr1, opts->disp_hdr2, opts->disp_hdr, opts->disp_nim,
            opts->disp_ana, opts->disp_exts, opts->disp_cext,
            opts->add_exts, opts->rm_exts,
            opts->mod_hdr, opts->mod_nim,
            opts->swap_hdr, opts->swap_ana, opts->swap_old,
            opts->cbl, opts->cci,
            opts->dts, opts->dci_lines, opts->make_im );

   fprintf(stderr,"   ci_dims[8]          = ");
   disp_raw_data(opts->ci_dims, DT_INT64, 8, ' ', 1);
   fprintf(stderr,"   new_dim[8]          = ");
   disp_raw_data(opts->new_dim, DT_INT64, 8, ' ', 1);

   fprintf(stderr,"\n"
                  "   new_datatype        = %d\n"
                  "   debug, keep_hist    = %d, %d\n"
                  "   overwrite           = %d\n"
                  "   prefix              = '%s'\n",
            opts->new_datatype, opts->debug, opts->keep_hist, opts->overwrite,
            opts->prefix ? opts->prefix : "(NULL)" );

   fprintf(stderr,"   elist   (length %d)  :\n", opts->elist.len);
   for( c = 0; c < opts->elist.len; c++ )
       fprintf(stderr,"      %d : %s\n", c, opts->elist.list[c]);

   fprintf(stderr,"   etypes  (length %d)  : ", opts->etypes.len);
   disp_raw_data(opts->etypes.list, DT_INT32, opts->etypes.len, ' ', 0);
   fputc('\n',stderr);

   fprintf(stderr,"   flist   (length %d)  :\n", opts->flist.len);
   for( c = 0; c < opts->flist.len; c++ )
       fprintf(stderr,"      %d : %s\n", c, opts->flist.list[c]);

   fprintf(stderr,"   vlist   (length %d)  :\n", opts->vlist.len);
   for( c = 0; c < opts->vlist.len; c++ )
       fprintf(stderr,"      %d : %s\n", c, opts->vlist.list[c]);

   fprintf(stderr,"   infiles (length %d)  :\n", opts->infiles.len);
   for( c = 0; c < opts->infiles.len; c++ )
       fprintf(stderr,"      %d : %s\n", c, opts->infiles.list[c]);

   fprintf(stderr,"   command len         : %d\n",(int)strlen(opts->command));

   return 0;
}


/*----------------------------------------------------------------------
 * For each file, add all extensions with type NIFTI_ECODE_AFNI.
 * Though it should not matter, copy the trailing null characters.
 *----------------------------------------------------------------------*/
int act_add_exts( nt_opts * opts )
{
   nifti_image      * nim;
   const char       * ext;
   char             * edata = NULL;
   int                fc, ec, elen;

   if( g_debug > 2 ){
      fprintf(stderr,"+d adding %d extensions to %d files...\n"
                     "   extension types are: ",
              opts->elist.len, opts->infiles.len);
      disp_raw_data(opts->etypes.list, DT_INT32, opts->etypes.len, ' ', 1);
   }

   if( opts->prefix && opts->infiles.len != 1 ){
      fprintf(stderr,"** error: we have a prefix but %d files\n",
              opts->infiles.len);
      return 1;
   }

   if( opts->elist.len <= 0 ) return 0;

   for( fc = 0; fc < opts->infiles.len; fc++ )
   {
      nim = nt_image_read( opts, opts->infiles.list[fc], 1 );
      if( !nim ) return 1;  /* errors come from the library */

      for( ec = 0; ec < opts->elist.len; ec++ ){
         ext = opts->elist.list[ec];
         elen = strlen(ext);
         if( !strncmp(ext,"file:",5) ){
            edata = read_file_text(ext+5, &elen);
            if( !edata || elen <= 0 ) {
               fprintf(stderr,"** failed to read extension data from '%s'\n",
                       ext+5);
               continue;
            }
            ext = edata;
         }

         if( ! nifti_is_valid_ecode(opts->etypes.list[ec]) )
            fprintf(stderr,"** warning: applying unknown ECODE %d\n",
                    opts->etypes.list[ec]);

         if( nifti_add_extension(nim, ext, elen, opts->etypes.list[ec]) ){
            nifti_image_free(nim);
            return 1;
         }

         /* if extension came from file, free the data */
         if( edata ){ free(edata); edata = NULL; }
      }

      if( opts->keep_hist && nifti_add_extension(nim, opts->command,
                             strlen(opts->command), NIFTI_ECODE_COMMENT) )
         fprintf(stderr,"** failed to add command to image as extension\n");

      if( opts->prefix &&
          nifti_set_filenames(nim, opts->prefix, !opts->overwrite, 1) )
      {
         nifti_image_free(nim);
         return 1;
      }

      if( g_debug > 1 )
         fprintf(stderr,"+d writing %s with %d new extension(s)\n",
                 opts->infiles.list[fc], opts->elist.len);

      nifti_image_write(nim);
      nifti_image_free(nim);
   }

   if( g_debug > 0 )
      fprintf(stderr,"+d added %d extension(s) to %d files\n",
              opts->elist.len, opts->infiles.len);

   return 0;
}

/*----------------------------------------------------------------------
 * Return the allocated file contents.
 *----------------------------------------------------------------------*/
static char * read_file_text(const char * filename, int * length)
{
   FILE * fp;
   char * text;
   int    len, bytes;

   if( !filename || !length ) {
      fprintf(stderr,"** bad params to read_file_text\n");
      return NULL;
   }

   len = nifti_get_filesize(filename);
   if( len <= 0 ) {
      fprintf(stderr,"** RFT: file '%s' appears empty\n", filename);
      return NULL;
   }

   fp = fopen(filename, "r");
   if( !fp ) {
      fprintf(stderr,"** RFT: failed to open '%s' for reading\n", filename);
      return NULL;
   }

   /* allocate the bytes, and fill them with the file contents */

   text = (char *)malloc(len * sizeof(char));
   if( !text ) {
      fprintf(stderr,"** RFT: failed to allocate %d bytes\n", len);
      fclose(fp);
      return NULL;
   }

   bytes = fread(text, sizeof(char), len, fp);
   fclose(fp); /* in any case */

   if( bytes != len ) {
      fprintf(stderr,"** RFT: read only %d of %d bytes from %s\n",
                     bytes, len, filename);
      free(text);
      return NULL;
   }

   /* success */

   if( g_debug > 1 ) {
      fprintf(stderr,"++ found extension of length %d in file %s\n",
              len, filename);
      if( g_debug > 2 )
         fprintf(stderr,"++ text is:\n%s\n", text);
   }

   *length = len;

   return text;
}

/*----------------------------------------------------------------------
 * For each file, strip the extra fields.
 *
 * Clear extensions and descrip field.  No other generic strings will get
 * passed to nifti_1_header struct.
 *
 * - this may make the datasets more anonymous
 * - no history is appended here
 *----------------------------------------------------------------------*/
int act_strip( nt_opts * opts )
{
   nifti_image      * nim;
   int                fc;

   if( g_debug > 2 )
      fprintf(stderr,"+d stripping extras from %d files\n", opts->infiles.len);

   if( opts->prefix && opts->infiles.len != 1 ){
      fprintf(stderr,"** error: we have a prefix but %d files\n",
              opts->infiles.len);
      return 1;
   }

   for( fc = 0; fc < opts->infiles.len; fc++ )
   {
      nim = nt_image_read( opts, opts->infiles.list[fc], 1 );
      if( !nim ) return 1;  /* errors come from the library */

      /* now remove the extensions */
      nifti_free_extensions(nim);
      memset(nim->descrip, 0, 80);

      if( opts->prefix &&
          nifti_set_filenames(nim, opts->prefix, !opts->overwrite, 1) ){
         nifti_image_free(nim);
         return 1;
      }

      if( g_debug > 1 )
         fprintf(stderr,"+d writing %s without extensions or 'descrip'\n",
                 nim->fname);

      nifti_image_write(nim);

      if( g_debug > 3 ) nifti_image_infodump(nim);
      nifti_image_free(nim);
   }

   if( g_debug > 0 )
      fprintf(stderr,"+d stripped extras from %d files\n", opts->infiles.len);

   return 0;
}


/*----------------------------------------------------------------------
 * For each file, remove the given extension for the given indices.
 *
 * Note that index = -1 means to remove them all.
 *----------------------------------------------------------------------*/
int act_rm_ext( nt_opts * opts )
{
   nifti_image      * nim;
   int                fc, ext_ind, num_ext;

   if( g_debug > 2 )
      fprintf(stderr,"+d removing %d extensions from %d files...\n",
              opts->elist.len, opts->infiles.len);

   if( opts->elist.len <= 0 ) return 0;

   if( opts->prefix && opts->infiles.len != 1 ){
      fprintf(stderr,"** error: we have a prefix but %d files\n",
              opts->infiles.len);
      return 1;
   }
   else if( opts->overwrite && opts->infiles.len != 1 &&
            strcmp(opts->elist.list[0], "-1") ) {
      fprintf(stderr,"** error: for multiple files, can only delete ALL\n");
      return 1;
   }

   ext_ind = atoi(opts->elist.list[0]);
   if( ext_ind < -1 ){
      fprintf(stderr,"** bad extension index to remove: %d\n", ext_ind);
      return 1;
   }

   if( g_debug > 1 ) fprintf(stderr,"+d removing extension index %d\n",ext_ind);

   for( fc = 0; fc < opts->infiles.len; fc++ )
   {
      nim = nt_image_read( opts, opts->infiles.list[fc], 1 );
      if( !nim ) return 1;  /* errors come from the library */

      /* note the number of extensions for later */
      num_ext = nim->num_ext;

      /* now remove the extensions */
      if( remove_ext_list(nim, opts->elist.list, opts->elist.len) )
         return 1;

      if( opts->keep_hist && nifti_add_extension(nim, opts->command,
                             strlen(opts->command), NIFTI_ECODE_COMMENT) )
         fprintf(stderr,"** failed to add command to image as extension\n");

      if( opts->prefix &&
          nifti_set_filenames(nim, opts->prefix, !opts->overwrite, 1) ){
         nifti_image_free(nim);
         return 1;
      }

      if( g_debug > 1 )
         fprintf(stderr,"+d writing %s with %d fewer extension(s)\n",
                 nim->fname, ext_ind == -1 ? num_ext : opts->elist.len);

      nifti_image_write(nim);
      nifti_image_free(nim);
   }

   if( g_debug > 0 )
      fprintf(stderr,"+d removed %s extension(s) from %d files\n",
              ext_ind == -1 ? "ALL" : "1", opts->infiles.len);

   return 0;
}


/*----------------------------------------------------------------------
 * remove extensions by index
 *
 * return: 0 on success, -1 on failure
 *----------------------------------------------------------------------*/
int remove_ext_list( nifti_image * nim, const char ** elist, int len )
{
   int * marks;
   int   c, ec, extval;

   if( len > nim->num_ext ){
      fprintf(stderr, "** cannot remove %d exts from image '%s' with only %d\n",
              len, nim->fname, nim->num_ext);
      return -1;
   }

   if( len <= 0 ){
      fprintf(stderr,"** REL: (%d) no extensions to remove?\n",len);
      return -1;
   }

   extval = atoi(elist[0]);  /* check the first value */

   /* first special case, elist[0] == -1 */
   if( extval == -1 )
   {
      if( g_debug > 1 )
          fprintf(stderr,"+d removing ALL (%d) extensions from '%s'\n",
                  nim->num_ext, nim->fname );
      nifti_free_extensions(nim);
      return 0;
   }

   if( g_debug > 2 )
      fprintf(stderr,"+d removing %d exts from '%s'\n", len, nim->fname );

   if( ! (marks = (int *)calloc(nim->num_ext, sizeof(int))) ) {
      fprintf(stderr,"** failed to alloc %d marks\n",nim->num_ext);
      return -1;
   }

   /* mark all extensions for removal */
   for( ec = 0; ec < len; ec++ )
   {
      extval = atoi(elist[ec]);

      if( extval < 0 || extval >= nim->num_ext ){
         fprintf(stderr,"** ext #%d (= %d) is out of range [0,%d] for %s\n",
                 ec, extval, nim->num_ext-1, nim->fname);
         free(marks); return -1;
      }

      if( marks[extval] ){
         fprintf(stderr,"** ext #%d (= %d) is a duplicate", ec, extval);
         free(marks); return -1;
      }

      marks[extval]++;
   }

   /* now remove them - count from top down to do lazy programming */
   for( ec = nim->num_ext-1; ec >= 0; ec-- )
   {
      if( !marks[ec] ) continue;   /* do not delete this one */

      if( g_debug > 2 )
         disp_nifti1_extension("+d removing ext: ",nim->ext_list+ec,-1);

      /* delete this data, and shift the list down (yeah, inefficient) */
      if( nim->ext_list[ec].edata ) free( nim->ext_list[ec].edata );

      /* move anything above down one */
      for( c = ec+1; c < nim->num_ext; c++ )
         nim->ext_list[c-1] = nim->ext_list[c];

      nim->num_ext--;
   }

   if( g_debug > 3 ) fprintf(stderr,"-d done removing extensions\n");

   if( nim->num_ext == 0 ){  /* did we trash the only extension? */
      if( g_debug > 1 )
         fprintf(stderr,"-d removed ALL extensions from %s\n",nim->fname);
      free(nim->ext_list);
      nim->ext_list = NULL;
   }

   free(marks);
   return 0;
}


/*----------------------------------------------------------------------
 * check for diffs between all fields in opts->flist, or in the
 * entire header - the 2 NIFTI versions must match
 *
 * if quiet mode (debug == 0) return on first diff
 *
 * return: 1 if diffs exist, 0 otherwise
 *----------------------------------------------------------------------*/
int act_diff_hdrs( nt_opts * opts )
{
   void * nhdr0, * nhdr1;
   int    diffs = 0, nva=0, nvb=0;

   if( opts->infiles.len != 2 ){
      fprintf(stderr,"** -diff_hdr requires 2 -infiles, have %d\n",
              opts->infiles.len);
      return 1;
   }

   if( g_debug > 2 )
      fprintf(stderr,"-d nifti_*_header diff between %s and %s...\n",
              opts->infiles.list[0], opts->infiles.list[1]);

   /* get the nifiti headers (but do not validate them) */

   /* nhdr0 = nt_read_header(opts, opts->infiles.list[0], NULL, 0); */
   nhdr0 = nt_read_header(opts->infiles.list[0], &nva, NULL, 0,
                          opts->new_datatype, opts->new_dim);
   if( ! nhdr0 ) return 1;  /* errors have been printed */

   nhdr1 = nt_read_header(opts->infiles.list[1], &nvb, NULL, 0,
                          opts->new_datatype, opts->new_dim);
   if( ! nhdr1 ){ free(nhdr0); return 1; }

   if( g_debug > 1 ) {
      fprintf(stderr,"\n-d nifti_?_header diffs between '%s' and '%s'...\n",
              opts->infiles.list[0], opts->infiles.list[1]);
      fprintf(stderr,"   have NIFTI-%d and NIFTI-%d\n", nva, nvb);
   }

   /* treat ANALYZE as NIFTI-1 */
   if( nva <= 0 || nvb <= 0 ) {
      if( nva < 0 || nvb < 0 )
         fprintf(stderr,"** resetting invalid NIFTI version(s) to 1\n");
      if( nva <= 0 ) nva = 1;
      if( nvb <= 0 ) nvb = 1;
   }

   /* a difference is fatal */
   if( nva != nvb ) {
      fprintf(stderr,"** %s is NIFTI-%d, while %s is NIFTI-%d\n"
                     "   they must match\n", opts->infiles.list[0], nva,
                     opts->infiles.list[1], nvb);
      free(nhdr0);  free(nhdr1);  return 1;
   }

   if( opts->flist.len <= 0 ) {
      if(nva==1) diffs = diff_hdr1s(nhdr0, nhdr1, g_debug > 0);
      else       diffs = diff_hdr2s(nhdr0, nhdr1, g_debug > 0);
   } else {
      if(nva==1) diffs = diff_hdr1s_list(nhdr0, nhdr1, &opts->flist, g_debug>0);
      else       diffs = diff_hdr2s_list(nhdr0, nhdr1, &opts->flist, g_debug>0);
   }

   if( diffs == 0 && g_debug > 1 )
      fprintf(stderr,"+d no differences found\n");
   else if ( g_debug > 2 )
      fprintf(stderr,"+d %d differences found\n", diffs);

   free(nhdr0);
   free(nhdr1);

   return (diffs > 0);
}


/*----------------------------------------------------------------------
 * check for diffs between all fields in opts->flist, or in the
 * entire nifti_1_header
 *
 * if quiet mode (debug == 0) return on first diff
 *
 * return: 1 if diffs exist, 0 otherwise
 *----------------------------------------------------------------------*/
int act_diff_hdr1s( nt_opts * opts )
{
   nifti_1_header * nhdr0, * nhdr1;
   int              diffs = 0, nv=1;

   if( opts->infiles.len != 2 ){
      fprintf(stderr,"** -diff_hdr1 requires 2 -infiles, have %d\n",
              opts->infiles.len);
      return 1;
   }

   if( g_debug > 2 )
      fprintf(stderr,"-d nifti_1_header diff between %s and %s...\n",
              opts->infiles.list[0], opts->infiles.list[1]);

   /* get the nifiti headers (but do not validate them) */

   /* nhdr0 = nt_read_header(opts, opts->infiles.list[0], NULL, 0); */
   nhdr0 = nt_read_header(opts->infiles.list[0], &nv, NULL, 0,
                          opts->new_datatype, opts->new_dim);
   if( ! nhdr0 ) return 1;  /* errors have been printed */

   nhdr1 = nt_read_header(opts->infiles.list[1], &nv, NULL, 0,
                          opts->new_datatype, opts->new_dim);
   if( ! nhdr1 ){ free(nhdr0); return 1; }

   if( g_debug > 1 )
      fprintf(stderr,"\n-d nifti_1_header diffs between '%s' and '%s'...\n",
              opts->infiles.list[0], opts->infiles.list[1]);

   if( opts->flist.len <= 0 )
      diffs = diff_hdr1s(nhdr0, nhdr1, g_debug > 0);
   else
      diffs = diff_hdr1s_list(nhdr0, nhdr1, &opts->flist, g_debug > 0);

   if( diffs == 0 && g_debug > 1 )
      fprintf(stderr,"+d no differences found\n");
   else if ( g_debug > 2 )
      fprintf(stderr,"+d %d differences found\n", diffs);

   free(nhdr0);
   free(nhdr1);

   return (diffs > 0);
}


/*----------------------------------------------------------------------
 * check for diffs between all fields in opts->flist, or in the
 * entire nifti_2_header
 *
 * if quiet mode (debug == 0) return on first diff
 *
 * return: 1 if diffs exist, 0 otherwise
 *----------------------------------------------------------------------*/
int act_diff_hdr2s( nt_opts * opts )
{
   nifti_2_header * nhdr0, * nhdr1;
   int              diffs = 0, nv=2;

   if( opts->infiles.len != 2 ){
      fprintf(stderr,"** -diff_hdr2 requires 2 -infiles, have %d\n",
              opts->infiles.len);
      return 1;
   }

   if( g_debug > 2 )
      fprintf(stderr,"-d nifti_2_header diff between %s and %s...\n",
              opts->infiles.list[0], opts->infiles.list[1]);

   /* get the nifiti headers (but do not validate them) */

   /* nhdr0 = nt_read_header(opts, opts->infiles.list[0], NULL, 0); */
   nhdr0 = nt_read_header(opts->infiles.list[0], &nv, NULL, 0,
                          opts->new_datatype, opts->new_dim);
   if( ! nhdr0 ) return 1;  /* errors have been printed */

   nhdr1 = nt_read_header(opts->infiles.list[1], &nv, NULL, 0,
                          opts->new_datatype, opts->new_dim);
   if( ! nhdr1 ){ free(nhdr0); return 1; }

   if( g_debug > 1 )
      fprintf(stderr,"\n-d nifti_2_header diffs between '%s' and '%s'...\n",
              opts->infiles.list[0], opts->infiles.list[1]);

   if( opts->flist.len <= 0 )
      diffs = diff_hdr2s(nhdr0, nhdr1, g_debug > 0);
   else
      diffs = diff_hdr2s_list(nhdr0, nhdr1, &opts->flist, g_debug > 0);

   if( diffs == 0 && g_debug > 1 )
      fprintf(stderr,"+d no differences found\n");
   else if ( g_debug > 2 )
      fprintf(stderr,"+d %d differences found\n", diffs);

   free(nhdr0);
   free(nhdr1);

   return (diffs > 0);
}


/*----------------------------------------------------------------------
 * check for diffs between all fields in opts->flist, or in the
 * entire nifti_image
 *
 * if quiet mode (debug == 0) return on first diff
 *
 * return: 1 if diffs exist, 0 otherwise
 *----------------------------------------------------------------------*/
int act_diff_nims( nt_opts * opts )
{
   nifti_image * nim0, * nim1;
   int           diffs = 0;

   if( opts->infiles.len != 2 ){
      fprintf(stderr,"** -diff_nim requires 2 -infiles, have %d\n",
              opts->infiles.len);
      return 1;
   }

   if( g_debug > 2 )
      fprintf(stderr,"-d nifti_image diff between %s and %s...\n",
              opts->infiles.list[0], opts->infiles.list[1]);

   /* get the nifiti images */

   nim0 = nt_image_read(opts, opts->infiles.list[0], 0);
   if( ! nim0 ) return 1;  /* errors have been printed */

   nim1 = nt_image_read(opts, opts->infiles.list[1], 0);
   if( ! nim1 ){ free(nim0); return 1; }

   if( g_debug > 1 )
      fprintf(stderr,"\n-d nifti_image diffs between '%s' and '%s'...\n",
              opts->infiles.list[0], opts->infiles.list[1]);

   if( opts->flist.len <= 0 )
      diffs = diff_nims(nim0, nim1, g_debug > 0);
   else
      diffs = diff_nims_list(nim0, nim1, &opts->flist, g_debug > 0);

   if( diffs == 0 && g_debug > 1 )
      fprintf(stderr,"+d no differences found\n");
   else if ( g_debug > 2 )
      fprintf(stderr,"+d %d differences found\n", diffs);

   nifti_image_free(nim0);
   nifti_image_free(nim1);

   return (diffs > 0);
}


/*----------------------------------------------------------------------
 * for each file, read nifti1_header
 *   if checking header, check it
 *   if checking nifti_image, convert and check it
 *----------------------------------------------------------------------*/
int act_check_hdrs( nt_opts * opts )
{
   nifti_1_header *  nhdr;
   nifti_image    *  nim;
   int               filenum, rv, nver=1;

   if( g_debug > 2 )
      fprintf(stderr,"-d checking hdrs/nims for %d nifti datasets...\n",
              opts->infiles.len);

   for( filenum = 0; filenum < opts->infiles.len; filenum++ )
   {
      /* do not validate the header structure */
      nhdr = nt_read_header(opts->infiles.list[filenum], &nver, NULL, 0,
                             opts->new_datatype, opts->new_dim);
      if( !nhdr ) continue;  /* errors are printed from library */

      if( opts->check_hdr )
      {
          if( g_debug > 1 )
             fprintf(stdout,"\nchecking nifti_1_header for file '%s'\n",
                     opts->infiles.list[filenum]);

          rv = nifti_hdr1_looks_good(nhdr);

          if( rv && g_debug > 0 )  /* if quiet, no GOOD response */
             printf("header IS GOOD for file %s\n",opts->infiles.list[filenum]);
          else if( ! rv )
             printf("header FAILURE for file %s\n",opts->infiles.list[filenum]);
      }

      if( opts->check_nim )
      {
          nim = nifti_convert_n1hdr2nim(*nhdr, opts->infiles.list[filenum]);
          if( !nim ) continue;  /* errors are printed from library */

          if( g_debug > 1 )
             fprintf(stdout,"\nchecking nifti_image for file '%s'\n",
                     opts->infiles.list[filenum]);

          rv = nifti_nim_is_valid(nim, 1); /* complain about errors */

          if( rv && g_debug > 0 )  /* if quiet, no GOOD response */
             printf("nifti_image IS GOOD for file %s\n",
                    opts->infiles.list[filenum]);
          else if( ! rv )
             printf("nifti_image FAILURE for file %s\n",
                    opts->infiles.list[filenum]);

          nifti_image_free(nim);
      }

      free(nhdr);
   }

   return 0;
}


/*----------------------------------------------------------------------
 * display all extensions for each dataset
 *----------------------------------------------------------------------*/
int act_disp_exts( nt_opts * opts )
{
   nifti_image * nim;
   char          mesg[32], *mptr;
   int           ec, fc;

   if( g_debug > 2 )
      fprintf(stderr,"-d displaying all extensions for %d files...\n",
              opts->infiles.len);

   for( fc = 0; fc < opts->infiles.len; fc++ )
   {
      nim = nt_image_read(opts, opts->infiles.list[fc], 0);
      if( !nim ) return 1;  /* errors are printed from library */

      if( g_debug > 0 )
         fprintf(stdout,"header file '%s', num_ext = %d\n",
                 nim->fname, nim->num_ext);
      for( ec = 0; ec < nim->num_ext; ec++ )
      {
         sprintf(mesg, "    ext #%d : ", ec);
         if( g_debug > 0 ) mptr = mesg;
         else              mptr = NULL;

         disp_nifti1_extension(mptr, nim->ext_list + ec, -1);
      }

      nifti_image_free(nim);
   }

   return 0;
}


/*----------------------------------------------------------------------
 * display all GIFTI extensions for each dataset
 *----------------------------------------------------------------------*/
int act_disp_cext( nt_opts * opts )
{
   nifti_image * nim;
   int           ec, fc, found;

   if( g_debug > 2 )
      fprintf(stderr,"-d displaying CIFTI extensions for %d files...\n",
              opts->infiles.len);

   for( fc = 0; fc < opts->infiles.len; fc++ )
   {
      nim = nt_image_read(opts, opts->infiles.list[fc], 0);
      if( !nim ) return 1;  /* errors are printed from library */

      if( g_debug > 1 )
         fprintf(stdout,"header file '%s', num_ext = %d\n",
                 nim->fname, nim->num_ext);
      found = 0;
      for( ec = 0; ec < nim->num_ext; ec++ )
      {
         if( nim->ext_list[ec].ecode != NIFTI_ECODE_CIFTI ) continue;
         found++;

         if( found == 1 && g_debug > 1 )
            fprintf(stdout,"header file '%s', ext %d of %d is CIFTI\n",
                    nim->fname, ec, nim->num_ext);

         disp_cifti_extension(NULL, nim->ext_list + ec, -1);
      }

      if( g_debug && ! found )
         fprintf(stdout,"header file '%s': no CIFTI extension\n", nim->fname);

      nifti_image_free(nim);
   }

   return 0;
}


/*----------------------------------------------------------------------
 * for each file, read nifti*_header and display all fields
 *----------------------------------------------------------------------*/
int act_disp_hdr( nt_opts * opts )
{
   void        * nhdr;
   field_s     * fnhdr;
   const char ** sptr;
   int           nfields, filenum, fc, nver=0;

   if( g_debug > 2 )
      fprintf(stderr,"-d displaying %d fields for %d nifti datasets...\n",
              opts->flist.len, opts->infiles.len);

   for( filenum = 0; filenum < opts->infiles.len; filenum++ )
   {
      /* do not validate the header structure */
      nver = 0;
      nhdr = nt_read_header(opts->infiles.list[filenum], &nver, NULL, g_debug>1,
                             opts->new_datatype, opts->new_dim);
      if( !nhdr ) return 1;  /* errors are printed from library */

      if( nver < 0 ) {
         fprintf(stderr,"** resetting invalid NIFTI version to 1\n");
         nver = 1;
      }

      /* set the number of fields to display */
      nfields = opts->flist.len > 0 ? opts->flist.len :
                   nver <= 1 ? NT_HDR1_NUM_FIELDS : NT_HDR2_NUM_FIELDS;

      if( g_debug > 0 )
         fprintf(stdout,"\nN-%d header file '%s', num_fields = %d\n",
                 nver, opts->infiles.list[filenum], nfields);
      if( g_debug > 1 ) {
         if( nver <= 1 )
            fprintf(stderr,"-d header is: %s\n",
                    nifti_hdr1_looks_good(nhdr) ? "valid" : "invalid");
         else
            fprintf(stderr,"-d header is: %s\n",
                    nifti_hdr2_looks_good(nhdr) ? "valid" : "invalid");
      }

      if( opts->flist.len <= 0 ) { /* then display all fields */
         if( nver <= 1 ) fnhdr = g_hdr1_fields;
         else            fnhdr = g_hdr2_fields;
         disp_field("\nall fields:\n", fnhdr, nhdr, nfields, g_debug>0);
      } else { /* print only the requested fields... */
         /* must locate each field before printing it */
         sptr = opts->flist.list;
         for( fc = 0; fc < opts->flist.len; fc++ )
         {
            if( nver == 1 ) fnhdr = get_hdr1_field(*sptr, filenum == 0);
            else            fnhdr = get_hdr2_field(*sptr, filenum == 0);
            if( fnhdr ) disp_field(NULL, fnhdr, nhdr, 1, g_debug>0 && fc == 0);
            sptr++;
         }
      }

      free(nhdr);
   }

   return 0;
}


/*----------------------------------------------------------------------
 * for each file, read nifti1_header and display all fields
 *----------------------------------------------------------------------*/
int act_disp_hdr1( nt_opts * opts )
{
   nifti_1_header *  nhdr;
   field_s        *  fnhdr;
   const char     ** sptr;
   int               nfields, filenum, fc, nver=-1;

   /* set the number of fields to display */
   nfields = opts->flist.len > 0 ? opts->flist.len : NT_HDR1_NUM_FIELDS;

   if( g_debug > 2 )
      fprintf(stderr,"-d displaying %d fields for %d nifti datasets...\n",
              nfields, opts->infiles.len);

   for( filenum = 0; filenum < opts->infiles.len; filenum++ )
   {
      /* do not validate the header structure */
      nhdr = nt_read_header(opts->infiles.list[filenum], &nver, NULL, g_debug>1,
                            opts->new_datatype, opts->new_dim);
      if( !nhdr ) return 1;  /* errors are printed from library */

      if( g_debug > 0 )
         fprintf(stdout,"\nheader file '%s', num_fields = %d\n",
                 opts->infiles.list[filenum], nfields);
      if( g_debug > 1 )
         fprintf(stderr,"-d header is: %s\n",
                 nifti_hdr1_looks_good(nhdr) ? "valid" : "invalid");

      if( opts->flist.len <= 0 ) /* then display all fields */
         disp_field("\nall fields:\n", g_hdr1_fields, nhdr, nfields, g_debug>0);
      else  /* print only the requested fields... */
      {
         /* must locate each field before printing it */
         sptr = opts->flist.list;
         for( fc = 0; fc < opts->flist.len; fc++ )
         {
            fnhdr = get_hdr1_field(*sptr, filenum == 0);
            if( fnhdr ) disp_field(NULL, fnhdr, nhdr, 1, g_debug>0 && fc == 0);
            sptr++;
         }
      }

      free(nhdr);
   }

   return 0;
}


/*----------------------------------------------------------------------
 * for each file, read nifti1_header and display all fields
 *----------------------------------------------------------------------*/
int act_disp_hdr2( nt_opts * opts )
{
   nifti_2_header *  nhdr;
   field_s        *  fnhdr;
   const char     ** sptr;
   int               nfields, filenum, fc, nver=-2;

   /* set the number of fields to display */
   nfields = opts->flist.len > 0 ? opts->flist.len : NT_HDR2_NUM_FIELDS;

   if( g_debug > 2 )
      fprintf(stderr,"-d displaying %d N-2 fields for %d nifti datasets...\n",
              nfields, opts->infiles.len);

   for( filenum = 0; filenum < opts->infiles.len; filenum++ )
   {
      /* do not validate the header structure */

      nhdr = nt_read_header(opts->infiles.list[filenum], &nver, NULL, g_debug>1,
                             opts->new_datatype, opts->new_dim);
      if( !nhdr ) return 1;  /* errors are printed from library */

      if( g_debug > 0 )
         fprintf(stdout,"\nNIFTI-2 header file '%s', num_fields = %d\n",
                 opts->infiles.list[filenum], nfields);

      if( g_debug > 1 )
         fprintf(stderr,"-d header is: %s\n",
                 nifti_hdr2_looks_good(nhdr) ? "valid" : "invalid");


      if( opts->flist.len <= 0 ) /* then display all fields */
         disp_field("\nall fields:\n", g_hdr2_fields, nhdr, nfields, g_debug>0);
      else  /* print only the requested fields... */
      {
         /* must locate each field before printing it */
         sptr = opts->flist.list;
         for( fc = 0; fc < opts->flist.len; fc++ )
         {
            fnhdr = get_hdr2_field(*sptr, filenum == 0);
            if( fnhdr ) disp_field(NULL, fnhdr, nhdr, 1, g_debug>0 && fc == 0);
            sptr++;
         }
      }

      free(nhdr);
   }

   return 0;
}


/*----------------------------------------------------------------------
 * for each file, read nifti_analyze75 and display all fields
 *----------------------------------------------------------------------*/
int act_disp_anas( nt_opts * opts )
{
   nifti_analyze75  * nhdr;
   field_s          * fnhdr;
   const char      ** sptr;
   int                nfields, filenum, fc, nver=1;

   /* set the number of fields to display */
   nfields = opts->flist.len > 0 ? opts->flist.len : NT_ANA_NUM_FIELDS;

   if( g_debug > 2 )
      fprintf(stderr,"-d displaying %d fields for %d ANALYZE datasets...\n",
              nfields, opts->infiles.len);

   for( filenum = 0; filenum < opts->infiles.len; filenum++ )
   {
      /* do not validate the header structure */
      nhdr = nt_read_header(opts->infiles.list[filenum], &nver, NULL, 0,
                             opts->new_datatype, opts->new_dim);
      if( !nhdr ) return 1;  /* errors are printed from library */

      if( g_debug > 0 )
         fprintf(stdout,"\nanalyze header file '%s', num_fields = %d\n",
                 opts->infiles.list[filenum], nfields);
      if( g_debug > 1 )
         fprintf(stderr,"-d analyze header is: %s\n",
                 nifti_hdr1_looks_good((nifti_1_header *)nhdr) ?
                 "valid" : "invalid");

      if( opts->flist.len <= 0 ) /* then display all fields */
         disp_field("\nall fields:\n", g_ana_fields, nhdr, nfields, g_debug>0);
      else  /* print only the requested fields... */
      {
         /* must locate each field before printing it */
         sptr = opts->flist.list;
         for( fc = 0; fc < opts->flist.len; fc++ )
         {
            fnhdr = get_hdr1_field(*sptr, filenum == 0);
            if( fnhdr ) disp_field(NULL, fnhdr, nhdr, 1, g_debug>0 && fc == 0);
            sptr++;
         }
      }

      free(nhdr);
   }

   return 0;
}


/*----------------------------------------------------------------------
 * for each file, get nifti_image and display all fields
 *----------------------------------------------------------------------*/
int act_disp_nims( nt_opts * opts )
{
   nifti_image *  nim;
   field_s     *  fnim;
   const char  ** sptr;
   int            nfields, filenum, fc;

   /* set the number of fields to display */
   nfields = opts->flist.len > 0 ? opts->flist.len : NT_NIM_NUM_FIELDS;

   if( g_debug > 2 )
      fprintf(stderr,"-d displaying %d fields for %d nifti datasets...\n",
              nfields, opts->infiles.len);

   for( filenum = 0; filenum < opts->infiles.len; filenum++ )
   {
      nim = nt_image_read(opts, opts->infiles.list[filenum], 0);
      if( !nim ) return 1;  /* errors are printed from library */

      if( g_debug > 0 )
         fprintf(stdout,"\nheader file '%s', num_fields = %d, fields:\n\n",
                 nim->fname, nfields);

      if( opts->flist.len <= 0 ) /* then display all fields */
         disp_field("all fields:\n", g_nim2_fields, nim, nfields, g_debug > 0);
      else  /* print only the requested fields... */
      {
         /* must locate each field before printing it */
         sptr = opts->flist.list;
         for( fc = 0; fc < opts->flist.len; fc++ )
         {
            fnim = get_nim_field(*sptr, filenum == 0);
            if( fnim ) disp_field(NULL, fnim, nim, 1, g_debug > 0 && fc == 0);
            sptr++;
         }
      }

      nifti_image_free(nim);
   }

   return 0;
}


/*----------------------------------------------------------------------
 * - read header
 * - modify header
 * - if -prefix duplicate file
 * - else if swapped, swap back
 * - overwrite file header      (allows (danger-of) no evaluation of data)
 *----------------------------------------------------------------------*/
int act_mod_hdrs( nt_opts * opts )
{
   nifti_1_header * nhdr;
   nifti_image    * nim;         /* for reading/writing entire datasets */
   int              filec, swap, nver=1;
   const char     * fname;
   char           * dupname;
   char             func[] = { "act_mod_hdrs" };

   if( g_debug > 2 )
      fprintf(stderr,"-d modifying %d fields for %d nifti headers...\n",
              opts->flist.len, opts->infiles.len);
   if( opts->flist.len <= 0 || opts->infiles.len <= 0 ) return 0;

   for( filec = 0; filec < opts->infiles.len; filec++ )
   {
      fname = opts->infiles.list[filec];  /* for convenience and mod file */

      if( nifti_is_gzfile(fname) ){
         fprintf(stderr,"** sorry, cannot modify a gzipped file: %s\n", fname);
         continue;
      }

/* rcr - this should be mod_hdr1s */

      /* do not validate the header structure */
      nhdr = nt_read_header(fname, &nver, &swap, 0,
                             opts->new_datatype, opts->new_dim);
      if( !nhdr ) return 1;

      if( g_debug > 1 )
      {
         fprintf(stderr,"-d modifying %d fields of '%s' header\n",
                 opts->flist.len, fname);
         fprintf(stderr,"-d header is: %s\n",
                 nifti_hdr1_looks_good(nhdr) ? "valid" : "invalid");
      }

      /* okay, let's actually trash the data fields */
      if( modify_all_fields(nhdr, opts, g_hdr1_fields, NT_HDR1_NUM_FIELDS) )
      {
         free(nhdr);
         return 1;
      }

      dupname = NULL;                     /* unless we duplicate file   */

      /* possibly duplicate the current dataset before writing new header */
      if( opts->prefix )
      {
         nim = nt_image_read(opts, fname, 1); /* get data */
         if( !nim ) {
            fprintf(stderr,"** failed to dup file '%s' before modifying\n",
                    fname);
            return 1;
         }
         if( opts->keep_hist && nifti_add_extension(nim, opts->command,
                                strlen(opts->command), NIFTI_ECODE_COMMENT) )
               fprintf(stderr,"** failed to add command to image as exten\n");
         if( nifti_set_filenames(nim, opts->prefix, 1, 1) )
         {
            NTL_FERR(func,"failed to set prefix for new file: ",opts->prefix);
            nifti_image_free(nim);
            return 1;
         }
         dupname = nifti_strdup(nim->fname);  /* so we know to free it */
         fname = dupname;
         nifti_image_write(nim);  /* create the duplicate file */
         /* if we added a history note, get the new offset into the header */
         /* mod: if the new offset is valid, use it    31 Jan 2006 [rickr] */
         if( nim->iname_offset >= 348 ) nhdr->vox_offset = nim->iname_offset;
         nifti_image_free(nim);
      }
      else if ( swap )
         swap_nifti_header(nhdr, NIFTI_VERSION(*nhdr));

      /* if all is well, overwrite header in fname dataset */
      (void)write_hdr_to_file(nhdr, fname); /* errors printed in function */

      if( dupname ) free(dupname);
      free(nhdr);
   }

   return 0;
}


/*----------------------------------------------------------------------
 * - read header
 * - swap header
 * - if -prefix duplicate file
 * - overwrite file header      (allows (danger-of) no evaluation of data)
 *----------------------------------------------------------------------*/
int act_swap_hdrs( nt_opts * opts )
{
   nifti_1_header * nhdr;
   nifti_image    * nim;         /* for reading/writing entire datasets */
   int              filec, swap, nver=1;
   const char     * fname;
   char           * dupname;
   char             func[] = { "act_mod_hdrs" };

   /* count requested operations: "there can be only one", and not Sean */
   swap = opts->swap_hdr + opts->swap_ana + opts->swap_old;
   if( swap > 1 ) {
      fprintf(stderr,"** can perform only one swap method\n");
      return 1;
   } else if( ! swap )
      return 0; /* probably shouldn't be here */

   if( g_debug > 2 )
      fprintf(stderr,"-d swapping headers of %d files...\n",opts->infiles.len);

   for( filec = 0; filec < opts->infiles.len; filec++ )
   {
      fname = opts->infiles.list[filec];  /* for convenience and mod file */

      if( nifti_is_gzfile(fname) ){
         fprintf(stderr,"** sorry, cannot swap a gzipped header: %s\n", fname);
         continue;
      }

/* rcr - this should be swap_hdr1s */

      /* do not validate the header structure */
      nhdr = nt_read_header(fname, &nver, &swap, 0, opts->new_datatype,
                                                    opts->new_dim);
      if( !nhdr ) return 1;

      if( g_debug > 1 ) {
         const char * str = "NIfTI";
         if( opts->swap_ana || (opts->swap_old && !NIFTI_VERSION(*nhdr)) )
            str = "ANALYZE";
         fprintf(stderr,"-d %sswapping %s header of file %s\n",
                 opts->swap_old ? "OLD " : "", str, fname);
      }

      if( ! swap ) {    /* if not yet swapped, do as the user requested */

         if( opts->swap_old ) old_swap_nifti_header(nhdr, NIFTI_VERSION(*nhdr));
         else                 swap_nifti_header(nhdr, opts->swap_ana ? 0 : 1);

      } else {          /* swapped already: if not correct, need to undo */

         /* if swapped the wrong way, undo and swap as the user requested */
         if ( opts->swap_ana && NIFTI_VERSION(*nhdr) ) {
            /* want swapped as ANALYZE, but was swapped as NIFTI */
            swap_nifti_header(nhdr, 1);  /* undo NIFTI */
            swap_nifti_header(nhdr, 0);  /* swap ANALYZE */
         } else if( opts->swap_hdr && !NIFTI_VERSION(*nhdr) ) {
            /* want swapped as NIFTI, but was swapped as ANALYZE */
            swap_nifti_header(nhdr, 0);  /* undo ANALYZE */
            swap_nifti_header(nhdr, 1);  /* swap NIFTI */
         } else if ( opts->swap_old ) {
            /* undo whichever was done and apply the old way */
            swap_nifti_header(nhdr, NIFTI_VERSION(*nhdr));
            old_swap_nifti_header(nhdr, NIFTI_VERSION(*nhdr));
         }

         /* else it was swapped the right way to begin with */

      }

      dupname = NULL;                     /* unless we duplicate file   */

      /* possibly duplicate the current dataset before writing new header */
      if( opts->prefix )
      {
         nim = nt_image_read(opts, fname, 1); /* get data */
         if( !nim ) {
            fprintf(stderr,"** failed to dup file '%s' before modifying\n",
                    fname);
            return 1;
         }
         if( opts->keep_hist && nifti_add_extension(nim, opts->command,
                                strlen(opts->command), NIFTI_ECODE_COMMENT) )
               fprintf(stderr,"** failed to add command to image as exten\n");
         if( nifti_set_filenames(nim, opts->prefix, 1, 1) )
         {
            NTL_FERR(func,"failed to set prefix for new file: ",opts->prefix);
            nifti_image_free(nim);
            return 1;
         }
         dupname = nifti_strdup(nim->fname);  /* so we know to free it */
         fname = dupname;
         nifti_image_write(nim);  /* create the duplicate file */
         /* if we added a history note, get the new offset into the header */
         /* mod: if the new offset is valid, use it    31 Jan 2006 [rickr] */
         if( nim->iname_offset >= 348 ) nhdr->vox_offset = nim->iname_offset;
         nifti_image_free(nim);
      }

      /* if all is well, overwrite header in fname dataset */
      (void)write_hdr_to_file(nhdr, fname); /* errors printed in function */

      if( dupname ) free(dupname);
      free(nhdr);
   }

   return 0;
}


/*----------------------------------------------------------------------
 * - read image w/data, modify and write
 *----------------------------------------------------------------------*/
int act_mod_nims( nt_opts * opts )
{
   nifti_image    * nim;         /* for reading/writing entire datasets */
   int              filec;
   char             func[] = { "act_mod_nims" };

   if( g_debug > 2 )
      fprintf(stderr,"-d modifying %d fields for %d nifti images...\n",
              opts->flist.len, opts->infiles.len);
   if( opts->flist.len <= 0 || opts->infiles.len <= 0 ) return 0;

   for( filec = 0; filec < opts->infiles.len; filec++ )
   {
      nim = nt_image_read(opts, opts->infiles.list[filec], 1); /* with data */

      if( g_debug > 1 )
         fprintf(stderr,"-d modifying %d fields from '%s' image\n",
                 opts->flist.len, opts->infiles.list[filec]);

      /* okay, let's actually trash the data fields */
      if( modify_all_fields(nim, opts, g_nim2_fields, NT_NIM_NUM_FIELDS) )
      {
         nifti_image_free(nim);
         return 1;
      }

      /* add command as COMMENT extension */
      if( opts->keep_hist && nifti_add_extension(nim, opts->command,
                             strlen(opts->command), NIFTI_ECODE_COMMENT) )
         fprintf(stderr,"** failed to add command to image as extension\n");

      /* possibly duplicate the current dataset before writing new header */
      if( opts->prefix )
         if( nifti_set_filenames(nim, opts->prefix, 1, 1) )
         {
            NTL_FERR(func,"failed to set prefix for new file: ",opts->prefix);
            nifti_image_free(nim);
            return 1;
         }

      nifti_image_write(nim);  /* and write it out, piece of cake :) */
      nifti_image_free(nim);
   }

   return 0;
}


/*----------------------------------------------------------------------
 * overwrite nifti_1_header in the given file
 *----------------------------------------------------------------------*/
int write_hdr_to_file( nifti_1_header * nhdr, const char * fname )
{
   znzFile fp;
   size_t  bytes;
   char    func[] = { "write_hdr_to_file" };
   int     rv = 0;

   fp = znzopen(fname,"r+b",nifti_is_gzfile(fname));
   if( znz_isnull(fp) ){
      NTL_FERR(func, "failed to re-open mod file", fname);
      return 1;
   }

   bytes = znzwrite(nhdr, 1, sizeof(nifti_1_header), fp);
   if( bytes != sizeof(nifti_1_header)){
      NTL_FERR(func, "failed to write header to file",fname);
      fprintf(stderr,"  - wrote %d of %d bytes\n",
              (int)bytes,(int)sizeof(nifti_1_header));
      rv = 1;
   }

   if( g_debug > 3 )
      disp_nifti_1_header("+d writing new header to file : ", nhdr);

   znzclose(fp);

   return rv;
}


/*----------------------------------------------------------------------
 * modify all fields in the list
 *----------------------------------------------------------------------*/
int modify_all_fields( void * basep, nt_opts * opts, field_s * fields, int flen)
{
   field_s * fp;
   int       fc, lc;  /* field and list counters */

   if( opts->flist.len <= 0 ) return 0;
   if( opts->flist.len != opts->vlist.len ){
      fprintf(stderr,"** have %d fields but %d new values\n",
              opts->flist.len, opts->vlist.len);
      return 1;
   }

   for( lc = 0; lc < opts->flist.len; lc++ )
   {
      /* is it in the list? */
      fp = fields;
      for( fc = 0; fc < flen; fc++, fp++ )
         if( strcmp(opts->flist.list[lc], fp->name) == 0 ) break;

      if( fc == flen )    /* do no modifications on failure */
      {
         fprintf(stderr,"** field '%s' not found in structure\n",
                 opts->flist.list[lc]);
         return 1;
      }

      if( modify_field( basep, fp, opts->vlist.list[lc]) )
         return 1;
   }

   return 0;
}


/*----------------------------------------------------------------------
 * modify a single field with the given value field
 *
 * pointer fields are not allowed here
 *----------------------------------------------------------------------*/
int modify_field(void * basep, field_s * field, const char * data)
{
   float   fval;
   const char  * posn = data;
   int     val, max, fc, nchars;

   if( g_debug > 1 )
      fprintf(stderr,"+d modifying field '%s' with '%s'\n", field->name, data);

   if( !data || strlen(data) == 0 )
   {
      fprintf(stderr,"** no data for '%s' field modification\n",field->name);
      return 1;
   }

   switch( field->type )
   {
         case DT_UNKNOWN:
         case NT_DT_POINTER:
         case NT_DT_CHAR_PTR:
         case NT_DT_EXT_PTR:
         default:
            fprintf(stderr,"** refusing to modify a pointer field, '%s'\n",
                    field->name);
            return 1;

         case DT_INT8:
         {
            max = 127;
            for( fc = 0; fc < field->len; fc++ )
            {
               if( sscanf(posn, " %d%n", &val, &nchars) != 1 )
               {
                  fprintf(stderr,"** found %d of %d modify values\n",
                          fc,field->len);
                  return 1;
               }
               if( val > max || val < -(max+1) )
               {
                  fprintf(stderr,
                    "** mod val #%d (= %d) outside byte range [-%d,%d]\n",
                    fc, val, max+1, max);
                  return 1;
               }
               /* otherwise, we're good */
               (((char *)basep + field->offset))[fc] = (char)val;
               if( g_debug > 1 )
                  fprintf(stderr,"+d setting posn %d of '%s' to %d\n",
                          fc, field->name, val);
               posn += nchars;
            }
         }
         break;

         case DT_INT16:
         {
            max = 32767;
            for( fc = 0; fc < field->len; fc++ )
            {
               if( sscanf(posn, " %d%n", &val, &nchars) != 1 )
               {
                  fprintf(stderr,"** found %d of %d modify values\n",
                          fc,field->len);
                  return 1;
               }
               if( val > max || val < -(max+1) )
               {
                  fprintf(stderr,
                    "** mod val #%d (= %d) outside byte range [-%d,%d]\n",
                    fc, val, max+1, max);
                  return 1;
               }
               /* otherwise, we're good */
               ((short *)((char *)basep + field->offset))[fc] = (short)val;
               if( g_debug > 1 )
                  fprintf(stderr,"+d setting posn %d of '%s' to %d\n",
                          fc, field->name, val);
               posn += nchars;
            }
         }
         break;

         case DT_INT32:
         {
            for( fc = 0; fc < field->len; fc++ )
            {
               if( sscanf(posn, " %d%n", &val, &nchars) != 1 )
               {
                  fprintf(stderr,"** found %d of %d modify values\n",
                          fc,field->len);
                  return 1;
               }
               ((int *)((char *)basep + field->offset))[fc] = val;
               if( g_debug > 1 )
                  fprintf(stderr,"+d setting posn %d of '%s' to %d\n",
                          fc, field->name, val);
               posn += nchars;
            }
         }
         break;

         case DT_INT64:
         {
            int64_t v64;
            for( fc = 0; fc < field->len; fc++ )
            {
               if( sscanf(posn, " %" PRId64 "%n", &v64, &nchars) != 1 )
               {
                  fprintf(stderr,"** found %d of %d modify values\n",
                          fc,field->len);
                  return 1;
               }
               ((int64_t *)((char *)basep + field->offset))[fc] = v64;
               if( g_debug > 1 )
                  fprintf(stderr,"+d setting posn %d of '%s' to %" PRId64 "\n",
                          fc, field->name, v64);
               posn += nchars;
            }
         }
         break;

         case DT_FLOAT32:
         {
            for( fc = 0; fc < field->len; fc++ )
            {
               if( sscanf(posn, " %f%n", &fval, &nchars) != 1 )
               {
                  fprintf(stderr,"** found %d of %d modify values\n",
                          fc,field->len);
                  return 1;
               }
               /* otherwise, we're good */
               ((float *)((char *)basep + field->offset))[fc] = fval;
               if( g_debug > 1 )
                  fprintf(stderr,"+d setting posn %d of '%s' to %f\n",
                          fc, field->name, fval);
               posn += nchars;
            }
         }
         break;

         case DT_FLOAT64:
         {
            double f64;
            for( fc = 0; fc < field->len; fc++ )
            {
               if( sscanf(posn, " %lf%n", &f64, &nchars) != 1 )
               {
                  fprintf(stderr,"** found %d of %d modify values\n",
                          fc,field->len);
                  return 1;
               }
               /* otherwise, we're good */
               ((float *)((char *)basep + field->offset))[fc] = f64;
               if( g_debug > 1 )
                  fprintf(stderr,"+d setting posn %d of '%s' to %f\n",
                          fc, field->name, f64);
               posn += nchars;
            }
         }
         break;

         case NT_DT_STRING:
         {
            char * dest = (char *)basep + field->offset;
            nchars = strlen(data);
            strncpy(dest, data, field->len);
            if( nchars < field->len )  /* clear the rest */
               memset(dest+nchars, '\0', field->len-nchars);
         }
         break;
   }

   return 0;
}


/*----------------------------------------------------------------------
 * fill the nifti_1_header field list
 *----------------------------------------------------------------------*/
int fill_hdr1_field_array( field_s * nh_fields )
{
   nifti_1_header   nhdr;
   field_s        * nhf = nh_fields;
   int              rv, errs;

   memset(nhf, 0, NT_HDR1_NUM_FIELDS*sizeof(field_s));

   /* this macro takes (TYPE, NAME, NUM) and does:
         fill_field(nhdr, TYPE, NT_OFF(nhdr,NAME), NUM, "NAME");
         nhf++;
   */
   errs = 0;
   NT_SFILL(nhdr, nhf, DT_INT32,     sizeof_hdr,     1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, NT_DT_STRING, data_type,     10, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, NT_DT_STRING, db_name,       18, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT32,     extents,        1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT16,     session_error,  1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, NT_DT_STRING, regular,        1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT8,      dim_info,       1, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, DT_INT16,     dim,            8, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   intent_p1,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   intent_p2,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   intent_p3,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT16,     intent_code,    1, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, DT_INT16,     datatype,       1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT16,     bitpix,         1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT16,     slice_start,    1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   pixdim,         8, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   vox_offset,     1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   scl_slope,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   scl_inter,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT16,     slice_end,      1, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, DT_INT8,      slice_code,     1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT8,      xyzt_units,     1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   cal_max,        1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   cal_min,        1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   slice_duration, 1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   toffset,        1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT32,     glmax,          1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT32,     glmin,          1, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, NT_DT_STRING, descrip,       80, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, NT_DT_STRING, aux_file,      24, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT16,     qform_code,     1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT16,     sform_code,     1, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, DT_FLOAT32,   quatern_b,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   quatern_c,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   quatern_d,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   qoffset_x,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   qoffset_y,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   qoffset_z,      1, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, DT_FLOAT32,   srow_x,         4, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   srow_y,         4, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   srow_z,         4, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, NT_DT_STRING, intent_name,   16, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, NT_DT_STRING, magic,          4, rv);  errs += rv;

   if( errs > 0 ){
      fprintf(stderr, "** %d fill_fields errors!\n", errs);
      return 1;
   }

   /* failure here is a serious problem */
   if( check_total_size("nifti_1_header test: ", nh_fields,
                        NT_HDR1_NUM_FIELDS, sizeof(nhdr)) )
      return 1;

   if( g_debug > 3 )
      disp_field_s_list("nh_fields: ", nh_fields, NT_HDR1_NUM_FIELDS);

   return 0;
}

/*----------------------------------------------------------------------
 * fill the nifti_2_header field list
 *----------------------------------------------------------------------*/
int fill_hdr2_field_array( field_s * nh_fields )
{
   nifti_2_header   nhdr;
   field_s        * nhf = nh_fields;
   int              rv, errs;

   memset(nhf, 0, NT_HDR2_NUM_FIELDS*sizeof(field_s));

   /* this macro takes (TYPE, NAME, NUM) and does:
         fill_field(nhdr, TYPE, NT_OFF(nhdr,NAME), NUM, "NAME");
         nhf++;
   */
   errs = 0;
   NT_SFILL(nhdr, nhf, DT_INT32,     sizeof_hdr,     1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, NT_DT_STRING, magic,          8, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, DT_INT16,     datatype,       1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT16,     bitpix,         1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT64,     dim,            8, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, DT_FLOAT64,   intent_p1,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT64,   intent_p2,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT64,   intent_p3,      1, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, DT_FLOAT64,   pixdim,         8, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, DT_INT64,     vox_offset,     1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT64,   scl_slope,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT64,   scl_inter,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT64,   cal_max,        1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT64,   cal_min,        1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT64,   slice_duration, 1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT64,   toffset,        1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT64,     slice_start,    1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT64,     slice_end,      1, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, NT_DT_STRING, descrip,       80, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, NT_DT_STRING, aux_file,      24, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT32,     qform_code,     1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT32,     sform_code,     1, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, DT_FLOAT64,   quatern_b,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT64,   quatern_c,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT64,   quatern_d,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT64,   qoffset_x,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT64,   qoffset_y,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT64,   qoffset_z,      1, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, DT_FLOAT64,   srow_x,         4, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT64,   srow_y,         4, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT64,   srow_z,         4, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, DT_INT32,     slice_code,     1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT32,     xyzt_units,     1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT32,     intent_code,    1, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, NT_DT_STRING, intent_name,   16, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT8,      dim_info,       1, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, NT_DT_STRING, unused_str,    15, rv);  errs += rv;

   if( errs > 0 ){
      fprintf(stderr, "** %d fill_fields errors!\n", errs);
      return 1;
   }

   /* failure here is a serious problem */
   if( check_total_size("nifti_2_header test: ", nh_fields,
                        NT_HDR2_NUM_FIELDS, sizeof(nhdr)) )
      return 1;

   if( g_debug > 3 )
      disp_field_s_list("n2h_fields: ", nh_fields, NT_HDR2_NUM_FIELDS);

   return 0;
}


/*----------------------------------------------------------------------
 * fill the nifti_image field list
 *----------------------------------------------------------------------*/
int fill_nim1_field_array( field_s * nim_fields )
{
   nifti1_image  nim;
   field_s     * nif = nim_fields;
   int           rv, errs;

   memset(nif, 0, NT_NIM_NUM_FIELDS*sizeof(field_s));

   errs = 0;

   NT_SFILL(nim, nif, DT_INT32,             ndim,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,               nx,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,               ny,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,               nz,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,               nt,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,               nu,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,               nv,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,               nw,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,              dim,  8, rv);  errs += rv;
   /* nvox: int32 -> size_t, 29 Jul 2007 -> int64_t, 29 Aug 2013 */
   NT_SFILL(nim, nif, DT_INT64,             nvox,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,           nbyper,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,         datatype,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,             dx,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,             dy,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,             dz,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,             dt,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,             du,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,             dv,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,             dw,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,         pixdim,  8, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      scl_slope,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      scl_inter,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,        cal_min,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,        cal_max,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,       qform_code,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,       sform_code,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,         freq_dim,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,        phase_dim,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,        slice_dim,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,       slice_code,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,      slice_start,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,        slice_end,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32, slice_duration,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      quatern_b,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      quatern_c,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      quatern_d,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      qoffset_x,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      qoffset_y,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      qoffset_z,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,           qfac,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,        qto_xyz, 16, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,        qto_ijk, 16, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,        sto_xyz, 16, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,        sto_ijk, 16, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,        toffset,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,        xyz_units,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,       time_units,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,       nifti_type,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,      intent_code,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      intent_p1,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      intent_p2,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      intent_p3,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, NT_DT_STRING,  intent_name, 16, rv);  errs += rv;
   NT_SFILL(nim, nif, NT_DT_STRING,      descrip, 80, rv);  errs += rv;
   NT_SFILL(nim, nif, NT_DT_STRING,     aux_file, 24, rv);  errs += rv;
   NT_SFILL(nim, nif, NT_DT_CHAR_PTR,      fname,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, NT_DT_CHAR_PTR,      iname,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,     iname_offset,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,         swapsize,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,        byteorder,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, NT_DT_POINTER,        data,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,          num_ext,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, NT_DT_EXT_PTR,    ext_list,  1, rv);  errs += rv;

   if( errs > 0 ){
      fprintf(stderr, "** %d fill_fields errors "
                      "(note that pointers get aligned)\n", errs);
      return 1;
   }

   if( g_debug > 3 )  /* failure here is not an error condition */
       check_total_size("nifti1_image test: ", nim_fields,
                        NT_NIM_NUM_FIELDS, sizeof(nim));

   if( g_debug > 3 )
      disp_field_s_list("nim1_fields: ", nim_fields, NT_NIM_NUM_FIELDS);

   return 0;
}


/*----------------------------------------------------------------------
 * fill the nifti2_image field list
 *----------------------------------------------------------------------*/
int fill_nim2_field_array( field_s * nim_fields )
{
   nifti2_image  nim;
   field_s     * nif = nim_fields;
   int           rv, errs;

   memset(nif, 0, NT_NIM_NUM_FIELDS*sizeof(field_s));

   errs = 0;

   NT_SFILL(nim, nif, DT_INT32,             ndim,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT64,               nx,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT64,               ny,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT64,               nz,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT64,               nt,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT64,               nu,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT64,               nv,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT64,               nw,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT64,              dim,  8, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT64,             nvox,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,           nbyper,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,         datatype,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,             dx,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,             dy,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,             dz,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,             dt,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,             du,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,             dv,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,             dw,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,         pixdim,  8, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,      scl_slope,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,      scl_inter,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,        cal_min,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,        cal_max,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,       qform_code,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,       sform_code,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,         freq_dim,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,        phase_dim,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,        slice_dim,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,       slice_code,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT64,      slice_start,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT64,        slice_end,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64, slice_duration,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,      quatern_b,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,      quatern_c,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,      quatern_d,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,      qoffset_x,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,      qoffset_y,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,      qoffset_z,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,           qfac,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,        qto_xyz, 16, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,        qto_ijk, 16, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,        sto_xyz, 16, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,        sto_ijk, 16, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,        toffset,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,        xyz_units,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,       time_units,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,       nifti_type,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,      intent_code,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,      intent_p1,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,      intent_p2,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT64,      intent_p3,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, NT_DT_STRING,  intent_name, 16, rv);  errs += rv;
   NT_SFILL(nim, nif, NT_DT_STRING,      descrip, 80, rv);  errs += rv;
   NT_SFILL(nim, nif, NT_DT_STRING,     aux_file, 24, rv);  errs += rv;
   NT_SFILL(nim, nif, NT_DT_CHAR_PTR,      fname,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, NT_DT_CHAR_PTR,      iname,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT64,     iname_offset,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,         swapsize,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,        byteorder,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, NT_DT_POINTER,        data,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,          num_ext,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, NT_DT_EXT_PTR,    ext_list,  1, rv);  errs += rv;

   if( errs > 0 ){
      fprintf(stderr, "** %d fill_fields errors "
                      "(note that pointers get aligned)\n", errs);
      return 1;
   }

   if( g_debug > 4 )  /* failure here is not an error condition */
       check_total_size("nifti2_image test: ", nim_fields,
                        NT_NIM_NUM_FIELDS, sizeof(nim));

   if( g_debug > 3 )
      disp_field_s_list("nim2_fields: ", nim_fields, NT_NIM_NUM_FIELDS);

   return 0;
}


/*----------------------------------------------------------------------
 * fill the nifti_analyze75 field list
 *----------------------------------------------------------------------*/
int fill_ana_field_array( field_s * ah_fields )
{
   nifti_analyze75   nhdr;
   field_s         * ahf = ah_fields;
   int               rv, errs;

   memset(ahf, 0, NT_ANA_NUM_FIELDS*sizeof(field_s));

   /* this macro takes (TYPE, NAME, NUM) and does:
         fill_field(nhdr, TYPE, NT_OFF(nhdr,NAME), NUM, "NAME");
         nhf++;
   */
   errs = 0;
   NT_SFILL(nhdr, ahf, DT_INT32,     sizeof_hdr,     1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, NT_DT_STRING, data_type,     10, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, NT_DT_STRING, db_name,       18, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_INT32,     extents,        1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_INT16,     session_error,  1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, NT_DT_STRING, regular,        1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_INT8,      hkey_un0,       1, rv);  errs += rv;

   NT_SFILL(nhdr, ahf, DT_INT16,     dim,            8, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_INT16,     unused8,        1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_INT16,     unused9,        1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_INT16,     unused10,       1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_INT16,     unused11,       1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_INT16,     unused12,       1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_INT16,     unused13,       1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_INT16,     unused14,       1, rv);  errs += rv;

   NT_SFILL(nhdr, ahf, DT_INT16,     datatype,       1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_INT16,     bitpix,         1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_INT16,     dim_un0,        1, rv);  errs += rv;

   NT_SFILL(nhdr, ahf, DT_FLOAT32,   pixdim,         8, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_FLOAT32,   vox_offset,     1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_FLOAT32,   funused1,       1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_FLOAT32,   funused2,       1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_FLOAT32,   funused3,       1, rv);  errs += rv;

   NT_SFILL(nhdr, ahf, DT_FLOAT32,   cal_max,        1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_FLOAT32,   cal_min,        1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_FLOAT32,   compressed,     1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_FLOAT32,   verified,       1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_INT32,     glmax,          1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_INT32,     glmin,          1, rv);  errs += rv;

   NT_SFILL(nhdr, ahf, NT_DT_STRING, descrip,       80, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, NT_DT_STRING, aux_file,      24, rv);  errs += rv;

   NT_SFILL(nhdr, ahf, DT_INT8,      orient,         1, rv);  errs += rv;
   /* originator is 5 (3) shorts, not 10 chars        26 Sep 2012 [rickr] */
   NT_SFILL(nhdr, ahf, DT_INT16,     originator,     5, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, NT_DT_STRING, generated,     10, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, NT_DT_STRING, scannum,       10, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, NT_DT_STRING, patient_id,    10, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, NT_DT_STRING, exp_date,      10, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, NT_DT_STRING, exp_time,      10, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, NT_DT_STRING, hist_un0,       3, rv);  errs += rv;

   NT_SFILL(nhdr, ahf, DT_INT32,     views     ,     1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_INT32,     vols_added,     1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_INT32,     start_field,    1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_INT32,     field_skip,     1, rv);  errs += rv;

   NT_SFILL(nhdr, ahf, DT_INT32,     omax,           1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_INT32,     omin,           1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_INT32,     smax,           1, rv);  errs += rv;
   NT_SFILL(nhdr, ahf, DT_INT32,     smin,           1, rv);  errs += rv;

   if( errs > 0 ){
      fprintf(stderr, "** %d ana fill_fields errors!\n", errs);
      return 1;
   }

   /* failure here is a serious problem */
   if( check_total_size("nifti_analyze75 test: ", ah_fields, NT_ANA_NUM_FIELDS,
                        sizeof(nhdr)) )
      return 1;

   if( g_debug > 3 )
      disp_field_s_list("ah_fields: ", ah_fields, NT_ANA_NUM_FIELDS);

   return 0;
}


/*----------------------------------------------------------------------
 * compare sizes to offset, including total
 *----------------------------------------------------------------------*/
int check_total_size( const char *mesg, field_s * fields, int nfields, int tot_size )
{
   field_s * fp;
   int       c, total;
   int       bad_offs;

   total = 0;
   bad_offs = 0;
   for( c = 0, fp = fields; c < nfields; c++, fp++ ){
      if( fp->offset != total ){
         if( g_debug > 3 )
            fprintf(stderr,"** bad offset for field '%s'\n"
                           "   offset = %d, total = %d\n",
                           fp->name, fp->offset, total);
         bad_offs++;
      }

      total += fp->size * fp->len;
   }

   if( g_debug > 1  || (g_debug > 0 && bad_offs > 0) ){
      fputs(mesg, stderr);  c = 0;
      if( bad_offs > 0 ){
         fprintf(stderr,"** found %d bad offsets\n", bad_offs);  c++; }
      if( total != tot_size ){
         fprintf(stderr,"** computed total %d not equal to struct size %d\n",
                 total, tot_size);   c++; }
      if( c == 0 ) fputs("... okay\n", stderr);
   }

   if( bad_offs > 0 ) return 1;

   return 0;
}


/*----------------------------------------------------------------------
 * fill the field structure with the given data
 *----------------------------------------------------------------------*/
int fill_field( field_s * fp, int type, int offset, int num, const char * name )
{
   fp->type   = type;
   fp->offset = offset;
   fp->size   = 1;     /* init before check */
   fp->len    = num;

   strncpy(fp->name, name, NT_FIELD_NAME_LEN-1);

   switch( type ){
      case DT_UNKNOWN:
      case DT_INT8:
      case NT_DT_STRING:
         fp->size = 1;
         break;

      case DT_INT16:
         fp->size = 2;
         break;

      case DT_INT32:
      case DT_FLOAT32:
         fp->size = 4;
         break;

      case DT_INT64:
      case DT_UINT64:
      case DT_FLOAT64:
      case DT_COMPLEX64:
         fp->size = 8;
         break;

      case NT_DT_POINTER:
      case NT_DT_CHAR_PTR:
      case NT_DT_EXT_PTR:
         fp->size = (int)sizeof(void *);
         break;

      default:
         fprintf(stderr,"** fill_field: invalid type %d\n", type );
         return 1;
   }

   return 0;
}


/*----------------------------------------------------------------------
 * return a string matching the type
 *----------------------------------------------------------------------*/
const char * field_type_str( int type )
{


   if( type == DT_INT8 )        return "DT_INT8";
   if( type == DT_INT16 )       return "DT_INT16";
   if( type == DT_INT32 )       return "DT_INT32";
   if( type == DT_INT64 )       return "DT_INT64";

   if( type == DT_UINT8 )       return "DT_UINT8";
   if( type == DT_UINT16 )      return "DT_UINT16";
   if( type == DT_UINT32 )      return "DT_UINT32";
   if( type == DT_UINT64 )      return "DT_UINT64";

   if( type == DT_FLOAT32 )     return "DT_FLOAT32";
   if( type == DT_FLOAT64 )     return "DT_FLOAT64";
   if( type == DT_COMPLEX64 )   return "DT_COMPLEX64";
   if( type == DT_COMPLEX128 )  return "DT_COMPLEX128";

   if( type == DT_RGB24 )       return "DT_RGB24";

   if( type == NT_DT_STRING )   return "NT_DT_STRING";
   if( type == NT_DT_POINTER )  return "NT_DT_POINTER";
   if( type == NT_DT_CHAR_PTR ) return "NT_DT_CHAR_PTR"; /* longest: 14 */
   if( type == NT_DT_EXT_PTR )  return "NT_DT_EXT_PTR";


   return "DT_UNKNOWN";  /* for DT_UNKNOWN, or as an else */
}

#define NT_MAX_DT_STR_LEN 14

/*----------------------------------------------------------------------
 * display the contents of all of the field structures
 *----------------------------------------------------------------------*/
int disp_field_s_list( const char *mesg, field_s * fp, int nfields )
{
   int c, total=0;

   if( mesg ) fputs(mesg, stdout);

   fprintf(stdout," %d fields:\n"
           "   name                  size   len   offset   type\n"
           "   -------------------   ----   ---   ------   --------------\n",
           nfields);

   for( c = 0; c < nfields; c++, fp++ ) {
      fprintf(stdout,"   %-*s  %4d    %3d   %4d     %-14s\n",
                     NT_FIELD_NAME_LEN-1, fp->name, fp->size, fp->len,
                     fp->offset, field_type_str(fp->type));
      total += fp->size*fp->len;
   }

   fprintf(stdout, "\n   total size: %d\n\n", total);

   return 0;
}


/*----------------------------------------------------------------------
 * display the contents of all of the field structures
 *----------------------------------------------------------------------*/
int disp_field(const char *mesg, field_s *fieldp, void * str, int nfields, int header)
{
   field_s * fp;
   int       c;

   if( mesg ) fputs(mesg, stdout);

   if( header && g_debug > 0 ){
      fprintf(stdout, "  name                offset  nvals  values\n");
      fprintf(stdout, "  ------------------- ------  -----  ------\n");
   }

   fp = fieldp;
   for( c = 0; c < nfields; c++, fp++ )
   {
      /* start by displaying the field information */
      if( g_debug > 0 )
         fprintf(stdout, "  %-*.*s %4d    %3d    ",
                      NT_FIELD_NAME_LEN-1, NT_FIELD_NAME_LEN-1, fp->name,
                      fp->offset, fp->len);

      /* now, print the value(s), depending on the type */
      switch( fp->type ){
         case DT_UNKNOWN:
         default:
            fprintf(stdout,"(unknown data type)\n");
            break;

         case DT_INT8:    case DT_UINT8:
         case DT_INT16:   case DT_UINT16:
         case DT_INT32:   case DT_UINT32:
         case DT_INT64:
         case DT_FLOAT32: case DT_FLOAT64:
            disp_raw_data((char *)str+fp->offset, fp->type, fp->len, ' ', 1);
            break;

         case NT_DT_POINTER:
            fprintf(stdout,"(raw data of unknown type)\n");
            break;

         case NT_DT_CHAR_PTR:  /* look for string of length <= 40 */
         {
            char * sp;
            int    len;

            /* start by sucking the pointer stored here */
            sp = *(char **)((char *)str + fp->offset);

            if( ! sp ){ fprintf(stdout,"(NULL)\n");  break; }  /* anything? */

            /* see if we have a printable string here */
            for(len = 0; len <= 40 && *sp && isprint(*sp); len++, sp++ )
               ;
            if( len > 40 )
               fprintf(stdout,"(apparent long string)\n");
            else if ( len == 0 )
               fprintf(stdout,"(empty string)\n");
            else if( *sp && !isprint(*sp) )  /* if no termination, it's bad */
               fprintf(stdout,"(non-printable string)\n");
            else  /* woohoo!  a good string */
               fprintf(stdout,"'%.40s'\n",*(char **)((char *)str + fp->offset));
            break;
         }

         case NT_DT_EXT_PTR:
         {
            nifti1_extension * extp;

            /* yank the address sitting there into extp */
            extp = *(nifti1_extension **)((char *)str + fp->offset);

            /* the user may use -disp_exts to display all of them */
            if( extp ) disp_nifti1_extension(NULL, extp, 6);
            else fprintf(stdout,"(NULL)\n");
            break;
         }

         case NT_DT_STRING:
         {
            char * charp = (char *)str + fp->offset;
            fprintf(stdout,"%.*s\n", fp->len, charp);
            break;
         }
      }
   }

   return 0;
}


/*----------------------------------------------------------------------
 * no display, just return whether any fields differ
 *----------------------------------------------------------------------*/
int diff_field(field_s *fieldp, void * str0, void * str1, int nfields)
{
   field_s * fp;
   char    * cp0, * cp1;
   int       fnum, c, size;

   fp = fieldp;
   for( fnum = 0; fnum < nfields; fnum++, fp++ )
   {
      switch( fp->type ){
         case DT_UNKNOWN:     /* all basic types are easy */
         case DT_INT8:
         case DT_INT16:
         case DT_INT32:
         case DT_INT64:
         case DT_FLOAT32:
         case DT_FLOAT64:
         case NT_DT_STRING:
            size = fp->size * fp->len;  /* total field size */
            cp0 = (char *)str0 + fp->offset;
            cp1 = (char *)str1 + fp->offset;
            for( c = 0; c < size; c++, cp0++, cp1++ )
               if( *cp0 != *cp1 ) break;

            if(c < size) return 1;  /* found a diff */

            break;

         case NT_DT_POINTER:     /* let's pass on these - no diff */
         case NT_DT_CHAR_PTR:

            break;

         case NT_DT_EXT_PTR:
         {
            nifti1_extension * ext0, * ext1;

            ext0 = *(nifti1_extension **)((char *)str0 + fp->offset);
            ext1 = *(nifti1_extension **)((char *)str1 + fp->offset);

            if( ! ext0 && ! ext1 ) break;     /* continue on */

            if( ext0 && ! ext1 )   return 1;  /* pointer diff is diff */
            if( ! ext0 && ext1 )   return 1;

            /* just check size and type for a single extension */
            if( ext0->esize != ext1->esize ) return 1;
            if( ext0->ecode != ext1->ecode ) return 1;

            break;
         }
      }
   }

   return 0;   /* no diffs found */
}


/*----------------------------------------------------------------------
 * display a single extension
 *----------------------------------------------------------------------*/
int disp_cifti_extension( const char *mesg, nifti1_extension * ext, int maxlen)
{
   FILE * outfp = stdout;
   int    len;
   if( mesg ) fputs(mesg, outfp);

   if( !ext ) {
      fprintf(stderr,"** no extension to display\n");
      return 1;
   }

   if( ext->ecode != NIFTI_ECODE_CIFTI ) {
      fprintf(stderr,"** extension code %d is not CIFTI\n", ext->ecode);
      return 1;
   }

   if( g_debug > 1 )
      fprintf(outfp,"ecode = %d, esize = %d, edata = ", ext->ecode,ext->esize);

   if( !ext->edata )
      fprintf(outfp,"(NULL)\n");
   else {
      len = ext->esize-8;
      if( maxlen >= 0 && len > maxlen ) len = maxlen;
      fprintf(outfp,"%.*s\n", len, (char *)ext->edata);
   }

   fflush(outfp);

   return 0;
}


/*----------------------------------------------------------------------
 * display a single extension
 *----------------------------------------------------------------------*/
int disp_nifti1_extension( const char *mesg, nifti1_extension * ext, int maxlen)
{
   FILE * outfp = stdout;
   int    len;
   if( mesg ) fputs(mesg, outfp);

   if( !ext )
   {
      fprintf(stderr,"** no extension to display\n");
      return 1;
   }

   if( g_debug > 0 )
      fprintf(outfp,"ecode = %d, esize = %d, edata = ", ext->ecode,ext->esize);

   if( !ext->edata )
      fprintf(outfp,"(NULL)\n");
   else if ( ext->ecode == NIFTI_ECODE_AFNI ||
             ext->ecode == NIFTI_ECODE_COMMENT ||
             ext->ecode == NIFTI_ECODE_CIFTI )
   {
      len = ext->esize-8;
      if( maxlen >= 0 && len > maxlen ) len = maxlen;
      fprintf(outfp,"%.*s\n", len, (char *)ext->edata);
   }
   else
      fprintf(outfp,"(unknown data type)\n");

   fflush(outfp);

   return 0;
}


/*----------------------------------------------------------------------
 * return the appropritate pointer into the g_hdr1_fields struct
 *----------------------------------------------------------------------*/
field_s * get_hdr1_field( const char * fname, int show_fail )
{
   field_s * fp;
   int       c;

   if( ! fname || *fname == '\0' ) return NULL;

   fp = g_hdr1_fields;
   for( c = 0; c < NT_HDR1_NUM_FIELDS; c++, fp++ )
      if( strcmp(fname, fp->name) == 0 ) break;

   if( c == NT_HDR1_NUM_FIELDS )
   {
      if( show_fail > 0 )
        fprintf(stderr,"** get_hdr1_field: field not found in hdr: %s\n",fname);
      return NULL;
   }

   return fp;
}

/*----------------------------------------------------------------------
 * return the appropritate pointer into the g_hdr1_fields struct
 *----------------------------------------------------------------------*/
field_s * get_hdr2_field( const char * fname, int show_fail )
{
   field_s * fp;
   int       c;

   if( ! fname || *fname == '\0' ) return NULL;

   fp = g_hdr2_fields;
   for( c = 0; c < NT_HDR2_NUM_FIELDS; c++, fp++ )
      if( strcmp(fname, fp->name) == 0 ) break;

   if( c == NT_HDR2_NUM_FIELDS )
   {
      if( show_fail > 0 )
        fprintf(stderr,"** get_hdr2_field: field not found in hdr: %s\n",fname);
      return NULL;
   }

   return fp;
}


/*----------------------------------------------------------------------
 * return the appropritate pointer into the g_hdr1_fields struct
 *----------------------------------------------------------------------*/
field_s * get_nim_field( const char * fname, int show_fail )
{
   field_s * fp;
   int       c;

   if( ! fname || *fname == '\0' ) return NULL;

   fp = g_nim2_fields;
   for( c = 0; c < NT_NIM_NUM_FIELDS; c++, fp++ )
      if( strcmp(fname, fp->name) == 0 ) break;

   if( c == NT_NIM_NUM_FIELDS )
   {
      if( show_fail > 0 )
         fprintf(stderr,"** get_nim_field: field not found in hdr: %s\n",fname);
      return NULL;
   }

   return fp;
}


/*----------------------------------------------------------------------
 * return the number of fields that differ
 *----------------------------------------------------------------------*/
int diff_hdr1s( nifti_1_header * s0, nifti_1_header * s1, int display )
{
   field_s * fp = g_hdr1_fields;
   int       c, ndiff = 0;

   for( c = 0; c < NT_HDR1_NUM_FIELDS; c++, fp++ )
      if( diff_field(fp, s0, s1, 1) )
      {
         if( display ) disp_field(NULL, fp, s0, 1, ndiff == 0);
         if( display ) disp_field(NULL, fp, s1, 1, 0);
         ndiff++;
      }

   return ndiff;
}


/*----------------------------------------------------------------------
 * return the number of fields that differ
 *----------------------------------------------------------------------*/
int diff_hdr2s( nifti_2_header * s0, nifti_2_header * s1, int display )
{
   field_s * fp = g_hdr2_fields;
   int       c, ndiff = 0;

   for( c = 0; c < NT_HDR2_NUM_FIELDS; c++, fp++ )
      if( diff_field(fp, s0, s1, 1) )
      {
         if( display ) disp_field(NULL, fp, s0, 1, ndiff == 0);
         if( display ) disp_field(NULL, fp, s1, 1, 0);
         ndiff++;
      }

   return ndiff;
}


/*----------------------------------------------------------------------
 * return the number of fields that differ
 *----------------------------------------------------------------------*/
int diff_nims( nifti_image * s0, nifti_image * s1, int display )
{
   field_s * fp = g_nim2_fields;
   int       c, ndiff = 0;

   for( c = 0; c < NT_NIM_NUM_FIELDS; c++, fp++ )
      if( diff_field(fp, s0, s1, 1) )
      {
         if( display ) disp_field(NULL, fp, s0, 1, ndiff == 0);
         if( display ) disp_field(NULL, fp, s1, 1, 0);
         ndiff++;
      }

   return ndiff;
}


/*----------------------------------------------------------------------
 * return the number of fields that differ
 *----------------------------------------------------------------------*/
int diff_hdr1s_list( nifti_1_header * s0, nifti_1_header * s1, str_list * slist,
                     int display )
{
   field_s  * fp;
   const char    ** sptr;
   int        c, ndiff = 0;

   sptr = slist->list;
   for( c = 0; c < slist->len; c++ )
   {
      fp = get_hdr1_field(*sptr, 1);    /* "not found" displayed in func */
      if( fp && diff_field(fp, s0, s1, 1) )
      {
         if( display ) disp_field(NULL, fp, s0, 1, ndiff == 0);
         if( display ) disp_field(NULL, fp, s1, 1, 0);
         ndiff++;
      }
      sptr++;
   }

   return ndiff;
}


/*----------------------------------------------------------------------
 * return the number of fields that differ
 *----------------------------------------------------------------------*/
int diff_hdr2s_list( nifti_2_header * s0, nifti_2_header * s1, str_list * slist,
                     int display )
{
   field_s  * fp;
   const char    ** sptr;
   int        c, ndiff = 0;

   sptr = slist->list;
   for( c = 0; c < slist->len; c++ )
   {
      fp = get_hdr2_field(*sptr, 1);    /* "not found" displayed in func */
      if( fp && diff_field(fp, s0, s1, 1) )
      {
         if( display ) disp_field(NULL, fp, s0, 1, ndiff == 0);
         if( display ) disp_field(NULL, fp, s1, 1, 0);
         ndiff++;
      }
      sptr++;
   }

   return ndiff;
}


/*----------------------------------------------------------------------
 * return the number of fields that differ
 *----------------------------------------------------------------------*/
int diff_nims_list( nifti_image * s0, nifti_image * s1, str_list * slist,
                    int display )
{
   field_s  * fp;
   const char    ** sptr;
   int        c, ndiff = 0;

   sptr = slist->list;
   for( c = 0; c < slist->len; c++ )
   {
      fp = get_nim_field(*sptr, 1);    /* "not found" displayed in func */
      if( fp && diff_field(fp, s0, s1, 1) )
      {
         if( display ) disp_field(NULL, fp, s0, 1, ndiff == 0);
         if( display ) disp_field(NULL, fp, s1, 1, 0);
         ndiff++;
      }
      sptr++;
   }

   return ndiff;
}


/*----------------------------------------------------------------------
 * display data from collapsed_image
 *----------------------------------------------------------------------*/
int act_disp_ci( nt_opts * opts )
{
   nifti_image *  nim;
   void        *  data = NULL;
   char           space = ' ';  /* use space or newline */
   int            filenum, len, err;

   if( opts->dci_lines ) space = '\n';  /* then use newlines as separators */

   if( g_debug > 2 && opts->dts )
   {
      fprintf(stderr,"-d displaying time series at (i,j,k) = ("
                     "%" PRId64 ",%" PRId64 ",%" PRId64 ")\n"
                     "      for %d nifti datasets...\n\n", opts->ci_dims[1],
              opts->ci_dims[2], opts->ci_dims[3], opts->infiles.len);
   }
   else if ( g_debug > 2 ) /* the general collapsed image form */
   {
      fprintf(stderr,"-d displaying collapsed image for %d datasets...\n\n"
                     "   dims = ", opts->infiles.len);
      disp_raw_data(opts->ci_dims, DT_INT64, 8, ' ', 1);
   }

   for( filenum = 0; filenum < opts->infiles.len; filenum++ )
   {
      err = 0;
      nim = nt_image_read(opts, opts->infiles.list[filenum], 0);
      if( !nim ) continue;  /* errors are printed from library */
      if( opts->dts && nim->ndim != 4 )
      {
         fprintf(stderr,"** error: dataset '%s' is not 4-dimensional\n",
                 nim->fname);
         err++;
      }

      switch( nim->datatype )
      {
         case DT_INT8:    case DT_INT16:   case DT_INT32:  case DT_INT64:
         case DT_UINT8:   case DT_UINT16:  case DT_UINT32:
         case DT_FLOAT32: case DT_FLOAT64:
               if( g_debug > 1 )
                  fprintf(stderr,"-d datatype %d of size %d\n",
                          nim->datatype, nim->nbyper);
               break;
         default:
               fprintf(stderr,"** dataset '%s' has unknown type %d\n",
                       nim->fname, nim->datatype);
               err++;
               break;
      }

      if( err ) { nifti_image_free(nim);  continue; }

      len = nifti_read_collapsed_image(nim, opts->ci_dims, &data);
      if( len < 0 || !data )
      {
         fprintf(stderr,"** FAILURE for dataset '%s'\n", nim->fname);
         if( data ) free(data);
         err++;
      }

      /* remove check for length of time series  24 Apr 2006 */

      if( err ){ nifti_image_free(nim);  continue; }

      /* now just print the results */
      if( g_debug > 0 )
      {
         fprintf(stdout,"\ndataset '%s' @ (", nim->fname);
         if( opts->dts ) disp_raw_data(opts->ci_dims+1, DT_INT64, 3, ' ', 0);
         else            disp_raw_data(opts->ci_dims+1, DT_INT64, 7, ' ', 0);
         fprintf(stdout,")\n");
      }

      disp_raw_data(data, nim->datatype, len / nim->nbyper, space, 1);

      nifti_image_free(nim);
   }

   if( data ) free(data);

   return 0;
}


int disp_raw_data( void * data, int type, int nvals, char space, int newline )
{
   char * dp, fbuf[32];
   int    c, size;

   nifti_datatype_sizes( type, &size, NULL );   /* get nbyper */

   for( c = 0, dp = (char *)data; c < nvals; c++, dp += size )
   {
      switch( type )
      {
         case DT_INT8:
               printf("%d", *(char *)dp);
               break;
         case DT_INT16:
               printf("%d", *(short *)dp);
               break;
         case DT_INT32:
               printf("%d", *(int *)dp);
               break;
         case DT_INT64:
               printf("%"PRId64, *(int64_t *)dp);
               break;
         case DT_UINT8:
               printf("%u", *(unsigned char *)dp);
               break;
         case DT_UINT16:
               printf("%u", *(unsigned short *)dp);
               break;
         case DT_UINT32:
               printf("%u", *(unsigned int *)dp);
               break;
         case DT_FLOAT32:
         {
               sprintf(fbuf,"%f", *(float *)dp);
               clear_float_zeros(fbuf);
               printf("%s", fbuf);
               break;
         }
         case DT_FLOAT64:
         {
               sprintf(fbuf,"%f", *(double *)dp);
               clear_float_zeros(fbuf);
               printf("%s", fbuf);
               break;
         }
         default:
               fprintf(stderr,"** disp_raw_data: unknown type %d\n", type);
               return 1;
      }
      if( c < nvals - 1 ) fputc(space,stdout);
   }

   if ( newline ) fputc('\n',stdout);

   return 0;
}

/*----------------------------------------------------------------------
 * remove trailing zeros from string of printed float
 * return  1 if something was cleared
 *         0 if not
 *----------------------------------------------------------------------*/
int clear_float_zeros( char * str )
{
   char * dp  = strchr(str, '.'), * valp;
   int    len;

   if( !dp ) return 0;      /* nothing to clear */

   len = strlen(dp);

   /* never clear what is just to the right of '.' */
   for( valp = dp+len-1; (valp > dp+1) && (*valp==' ' || *valp=='0'); valp-- )
       *valp = '\0';     /* clear, so we don't worry about break conditions */

   if( valp < dp + len - 1 ) return 1;
   return 0;
}

/* return the number of volumes in the nifti_image */
static int num_volumes(nifti_image * nim)
{
   int ind, nvols = 1;

   if( nim->dim[0] < 1 ) return 0;

   for( ind = 4; ind <= nim->dim[0]; ind++ )
        nvols *= nim->dim[ind];

   return nvols;
}


/*----------------------------------------------------------------------
 * create a new dataset using sub-brick selection
 *----------------------------------------------------------------------*/
int act_cbl( nt_opts * opts )
{
   nifti_brick_list   NBL;
   nifti_image      * nim;
   char             * fname, * selstr, * cp;
   int64_t          * blist;
   int                err = 0;

   if( g_debug > 2 )
      fprintf(stderr,"-d copying file info from '%s' to '%s'\n",
              opts->infiles.list[0], opts->prefix);

   /* sanity checks */
   if( ! opts->prefix ) {
      fprintf(stderr,"** error: -prefix is required with -cbl function\n");
      return 1;
   } else if( opts->infiles.len > 1 ) {
      fprintf(stderr,"** sorry, at the moment -cbl allows only 1 input\n");
      return 1;
   }

   /* remove selector from fname, and copy selector string */
   fname = nifti_strdup(opts->infiles.list[0]);
   cp = strchr(fname,'[');  if( !cp )  cp = strchr(fname,'{');

   if( !cp ) {
      if( g_debug > 1 )
         fprintf(stderr,"-d using -cbl without brick list in '%s'\n",fname);
      selstr = nifti_strdup("[0..$]");
   } else {
      selstr = nifti_strdup(cp);
      *cp = '\0';    /* remove selection string from fname */
   }

   if( g_debug > 1 )
      fprintf(stderr,"+d -cbl: using '%s' for selection string\n", selstr);

   nim = nt_image_read(opts, fname, 0);  /* get image */
   if( !nim ) return 1;

   /* since nt can be zero now (sigh), check for it   02 Mar 2006 [rickr] */
   blist = nifti_get_int64list(nim->nt > 0 ? num_volumes(nim) : 1, selstr);
   nifti_image_free(nim);             /* throw away, will re-load */
   if( !blist ) {
      fprintf(stderr,"** failed sub-brick selection using '%s'\n",selstr);
      free(fname);  free(selstr);  return 1;
   }

   nim = nt_read_bricks(opts, fname, blist[0], blist+1, &NBL);
   free(blist);  /* with this */
   if( !nim ){  free(fname);  free(selstr);  return 1; }

   if( g_debug > 1 ) fprintf(stderr,"+d sub-bricks loaded\n");

   /* add command as COMMENT extension */
   if( opts->keep_hist && nifti_add_extension(nim, opts->command,
                          strlen(opts->command), NIFTI_ECODE_COMMENT) )
      fprintf(stderr,"** failed to add command to image as extension\n");

   /* replace filenames using prefix */
   if( nifti_set_filenames(nim, opts->prefix, 1, 1) )
   {
      fprintf(stderr,"** failed to set names, prefix = '%s'\n",opts->prefix);
      err++;
   }

   if(g_debug>2) disp_field("new nim:\n",g_nim2_fields,nim,NT_NIM_NUM_FIELDS,1);

   /* and finally, write out results */
   if( err == 0 && nifti_nim_is_valid(nim, g_debug) )
      nifti_image_write_bricks(nim, &NBL);

   nifti_image_free(nim);
   nifti_free_NBL(&NBL);
   free(fname);
   free(selstr);

   return 0;
}


/*----------------------------------------------------------------------
 * create a new dataset using read_collapsed_image
 *----------------------------------------------------------------------*/
int act_cci( nt_opts * opts )
{
   nifti_image      * nim;
   int                c;

   if( g_debug > 2 )
      fprintf(stderr,"-d collapsing file info from '%s' to '%s'\n",
              opts->infiles.list[0], opts->prefix);

   /* sanity checks */
   if( ! opts->prefix ) {
      fprintf(stderr,"** error: -prefix is required with -cci function\n");
      return 1;
   } else if( opts->infiles.len > 1 ) {
      fprintf(stderr,"** sorry, at the moment -cci allows only 1 input\n");
      return 1;
   }

   nim = nt_image_read(opts, opts->infiles.list[0], 0);
   if( !nim ) return 1;
   nim->data = NULL;    /* just to be sure */

   if( nifti_read_collapsed_image(nim, opts->ci_dims, &nim->data) < 0 )
   {
      nifti_image_free(nim);
      return 1;
   }

   /* add command as COMMENT extension */
   if( opts->keep_hist && nifti_add_extension(nim, opts->command,
                          strlen(opts->command), NIFTI_ECODE_COMMENT) )
      fprintf(stderr,"** failed to add command to image as extension\n");

   /* replace filenames using prefix */
   if( nifti_set_filenames(nim, opts->prefix, 1, 1) )
   {
      fprintf(stderr,"** failed to set names, prefix = '%s'\n",opts->prefix);
      nifti_image_free(nim);
      return 1;
   }

   for( c = 1; c < 8; c++ )  /* nuke any collapsed dimension */
      if( opts->ci_dims[c] >= 0 ) nim->dim[c] = 1;

   nifti_update_dims_from_array(nim);

   if(g_debug>2) disp_field("new nim:\n",g_nim2_fields,nim,NT_NIM_NUM_FIELDS,1);

   /* and finally, write out results */
   if( nifti_nim_is_valid(nim, g_debug) ) nifti_image_write(nim);

   nifti_image_free(nim);

   return 0;
}


/*----------------------------------------------------------------------
 * free all of the lists in the struct
 * note: strings were not allocated
 *----------------------------------------------------------------------*/
static int free_opts_mem( nt_opts * nopt )
{
    if( !nopt ) return 1;

    if( nopt->elist.list   ) free(nopt->elist.list);
    if( nopt->etypes.list  ) free(nopt->etypes.list);
    if( nopt->flist.list   ) free(nopt->flist.list);
    if( nopt->vlist.list   ) free(nopt->vlist.list);
    if( nopt->infiles.list ) free(nopt->infiles.list);

    return 0;
}


/*----------------------------------------------------------------------
 * wrapper for nifti_image_read
 *
 * this adds the option to generage an empty image, if the
 * filename starts with "MAKE_IM"
 *----------------------------------------------------------------------*/
nifti_image * nt_image_read( nt_opts * opts, const char * fname, int doread )
{
    if( !opts || !fname  ) {
        fprintf(stderr,"** nt_image_read: bad params (%p,%p)\n",
                (void *)opts, (void *)fname);
        return NULL;
    }

    /* if the user does not want an empty image, do normal image_read */
    if( strncmp(fname,NT_MAKE_IM_NAME,strlen(NT_MAKE_IM_NAME)) ) {
        if(g_debug > 1)
            fprintf(stderr,"-d calling nifti_image_read(%s,%d)\n",fname,doread);
        return nifti_image_read(fname, doread);
    }

    /* so generate an emtpy image */
    if(g_debug > 1) {
        fprintf(stderr,"+d NT_IR: generating EMPTY IMAGE from %s...\n",fname);
        if(g_debug > 2) {
            printf("   new_dim[8] = ");
            disp_raw_data(opts->new_dim, DT_INT64, 8, ' ', 1);
            printf("   new_datatype = %d\n", opts->new_datatype);
            fflush(stdout);
        }
    }

    /* create a new nifti_image, with data depending on doread */
    return nifti_make_new_nim(opts->new_dim, opts->new_datatype, doread);
}


/*----------------------------------------------------------------------
 * wrapper for nifti_read_header
 *
 * this adds the option to generage an empty image, if the
 * filename starts with "MAKE_IM"
 *----------------------------------------------------------------------*/
void * nt_read_header(const char * fname, int * nver, int * swapped, int check,
                      int new_datatype, int64_t new_dim[8])
{
    nifti_image * nim = NULL;
    void        * nptr = NULL;
    char          func[] = { "nt_read_header" };
    int           nv;

    /* swapped is not necessary */
    if( !fname ) {
        fprintf(stderr,"** nt_read_header: missing fname\n");
        return NULL;
    }

    /* if the user does not want an empty image, do normal image_read */
    if( strncmp(fname,NT_MAKE_IM_NAME,strlen(NT_MAKE_IM_NAME)) ) {
        if(g_debug > 1)
            fprintf(stderr,"-d calling nifti_read_n1_hdr(%s,...)\n", fname);

        /* if not set or 0, return whatever is found */
        if( ! nver || ! *nver ) return nifti_read_header(fname, nver, check);

        if( *nver < -2 || *nver > 2 ) {
           fprintf(stderr,"** nt_read_header, illegal nver = %d\n", *nver);
           return NULL;
        }

        /* 1 or 2 means try to return only that */
        if( *nver == 1 ) return nifti_read_n1_hdr(fname, swapped, check);
        if( *nver == 2 ) return nifti_read_n2_hdr(fname, swapped, check);

        /* handle negatives, start by simply reading the header */
        nptr = nifti_read_header(fname, &nv, check);
        if( !nptr ) return NULL;
        if( g_debug > 1 ) fprintf(stderr,"-d have NIFTI-%d header, %s\n",
                                  nv, fname);

        /* negative means convert, if necessary */
        if( *nver == -1 ) {
           nifti_1_header * hdr=NULL;
           if( nv <= 1 ) return nptr;

           /* else assume 2: convert headers via nim? */
           hdr = (nifti_1_header *)malloc(sizeof(nifti_1_header));
           if( !hdr ) {
              fprintf(stderr,"** %s: failed to alloc nifti_1_header\n", func);
              return NULL;
           }

           nim = nifti_convert_n2hdr2nim(*(nifti_2_header*)nptr, NULL);
           if( !nim ) {
              fprintf(stderr,"** %s: failed n2hdr2nim on %s\n", func, fname);
              return NULL;
           }
           if( nifti_convert_nim2n1hdr(nim, hdr) ) {
              fprintf(stderr,"** %s: failed convert_nim2n1hdr on %s\n",
                      func, fname);
              return NULL;
           }
           return hdr;
        }
        else { /* assume -2 */
           nifti_2_header * hdr=NULL;
           if( nv == 2 ) return nptr;

           /* else assume 2: convert headers via nim? */
           hdr = (nifti_2_header *)malloc(sizeof(nifti_2_header));
           if( !hdr ) {
              fprintf(stderr,"** %s: failed to alloc nifti_2_header\n", func);
              return NULL;
           }

           nim = nifti_convert_n1hdr2nim(*(nifti_1_header*)nptr, NULL);
           if( !nim ) {
              fprintf(stderr,"** %s: failed n1hdr2nim on %s\n", func, fname);
              return NULL;
           }
           if( nifti_convert_nim2n2hdr(nim, hdr) ) {
              fprintf(stderr,"** %s: failed convert_nim2n2hdr on %s\n",
                      func, fname);
              return NULL;
           }

           return hdr;
        }
    }

    /* else "MAKE_IM", so generate an emtpy image */

    if(g_debug > 1) {
        fprintf(stderr,"+d NT_RH: generating EMPTY IMAGE from %s...\n",fname);
        if(g_debug > 2) {
            printf("   new_dim[8] = ");
            disp_raw_data(new_dim, DT_INT64, 8, ' ', 1);
            printf("   new_datatype = %d\n", new_datatype);
            fflush(stdout);
        }
    }

    /* return creation of new header */
    if( (nver && *nver == 1) || (nver && *nver == -1))
       return nifti_make_new_n1_header(new_dim, new_datatype);
    else {
       if( nver ) *nver = 2;
       return nifti_make_new_n2_header(new_dim, new_datatype);
    }
}


/*----------------------------------------------------------------------
 * wrapper for nifti_image_read_bricks
 *
 * Similar to nt_read_header(), this adds the option to generage an
 * empty image if the filename starts with "MAKE_IM".
 *
 * the returned object is a (max 4-D) nifti_image
 *----------------------------------------------------------------------*/
nifti_image * nt_read_bricks(nt_opts * opts, char * fname, int len,
                             int64_t * list, nifti_brick_list * NBL)
{
    nifti_image * nim;
    int           c;

    /* swapped is not necessary */
    if( !opts || !fname || !NBL ) {
        fprintf(stderr,"** nt_read_bricks: bad params (%p,%p,%p)\n",
                (void *)opts, (void *)fname, (void *)NBL);
        return NULL;
    }

    /* if the user does not want an empty image, do normal read_bricks */
    if( strncmp(fname,NT_MAKE_IM_NAME,strlen(NT_MAKE_IM_NAME)) ) {
        if(g_debug > 1)
           fprintf(stderr,"-d calling nifti_image_read_bricks(%s,...)\n",fname);
        return nifti_image_read_bricks(fname, len, list, NBL);
    }

    /* so generate an emtpy image */
    if(g_debug > 1) {
        fprintf(stderr,"+d NT_RB: generating EMPTY IMAGE from %s...\n",fname);
        if(g_debug > 2) {
            printf("   new_dim[8] = ");
            disp_raw_data(opts->new_dim, DT_INT64, 8, ' ', 1);
            printf("   new_datatype = %d\n", opts->new_datatype);
            if( list && len > 0 ) {
                printf("   brick_list[%d] = ", len);
                disp_raw_data(list, DT_INT64, len, ' ', 1);
            }
            fflush(stdout);  /* disp_raw_data uses stdout */
        }
    }

    /* first, get nim struct without data */
    nim = nifti_make_new_nim(opts->new_dim, opts->new_datatype, 0);
    if( !nim ) {
        fprintf(stderr,"** nt_read_bricks, nifti_make_new_nim failure\n");
        return NULL;
    }

    /* now populate NBL (can be based only on len and nim) */
    NBL->nbricks = len;
    NBL->bsize = nim->nbyper * nim->nx * nim->ny * nim->nz;
    NBL->bricks = (void **)calloc(NBL->nbricks, sizeof(void *));
    if( !NBL->bricks ){
        fprintf(stderr,"** NRB: failed to alloc %" PRId64 " pointers\n",
                NBL->nbricks);
        nifti_image_free(nim);
        return NULL;
    }

    if(g_debug > 1)
        fprintf(stderr,"+d NRB, allocating "
                "%" PRId64 " bricks of %" PRId64 " bytes...\n",
                NBL->nbricks, NBL->bsize);

    /* now allocate the data pointers */
    for( c = 0; c < len; c++ ) {
        NBL->bricks[c] = calloc(1, NBL->bsize);
        if( !NBL->bricks[c] ){
            fprintf(stderr,
                    "** NRB: failed to alloc brick %d of %" PRId64 " bytes\n",
                    c, NBL->bsize);
            nifti_free_NBL(NBL); nifti_image_free(nim); return NULL;
        }
    }

    return nim;
}
