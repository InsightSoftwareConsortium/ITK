/* ----------------------------------------------------------------------
 * A basic example to read/write a nifti dataset (e.g. cp command).
 *
 * compile example (consider -pedantic or -Wall):
 *
 * gcc -o clib_01_read_write clib_01_read_write.c       \
 *     -I../include -L../lib -lniftiio -lznz -lz -lm
 *
 * R Reynolds   14 Apr 2009
 *----------------------------------------------------------------------
 */
#include <stdio.h>

#include <nifti1_io.h>

int show_help( void )
{
   printf(
      "cib_01_read_write: short exmample of reading/writing NIfTI\n"
      "\n"
      "    This program is to demonstrate how to read a NIfTI dataset,\n"
      "    set output filenames and write a NIfTI dataset, all via the\n"
      "    standard NIfTI C library.\n"
      "\n"
      "    basic usage: cib_01_read_write -input FILE_IN -output FILE_OUT\n"
      "\n"
      "    options:     -help           : show this help\n"
      "                 -verb LEVEL     : the verbose level to LEVEL\n"
      "\n");
   return 0;
}

int main(int argc, char * argv[])
{
   nifti_image * nim=NULL;
   char        * fin=NULL, * fout=NULL;
   int           ac;

   if( argc < 2 ) return show_help();   /* typing '-help' is sooo much work */

   /* process user options: 4 are valid presently */
   for( ac = 1; ac < argc; ac++ ) {
      if( ! strncmp(argv[ac], "-h", 2) ) {
         return show_help();
      }
      else if( ! strcmp(argv[ac], "-input") ) {
         if( ++ac >= argc ) {
            fprintf(stderr, "** missing argument for -input\n");
            return 1;
         }
         fin = argv[ac];  /* no string copy, just pointer assignment */
      }
      else if( ! strcmp(argv[ac], "-output") ) {
         if( ++ac >= argc ) {
            fprintf(stderr, "** missing argument for -output\n");
            return 2;
         }
         fout = argv[ac];
      }
      else if( ! strcmp(argv[ac], "-verb") ) {
         if( ++ac >= argc ) {
            fprintf(stderr, "** missing argument for -verb\n");
            return 2;
         }
         nifti_set_debug_level(atoi(argv[ac]));
      }
      else {
         fprintf(stderr,"** invalid option, '%s'\n", argv[ac]);
         return 1;
      }
   }

   if( !fin  ) { fprintf(stderr, "** missing option '-input'\n");  return 1; }
   if( !fout ) { fprintf(stderr, "** missing option '-output'\n"); return 1; }

   /* read input dataset, including data */
   nim = nifti_image_read(fin, 1);
   if( !nim ) {
      fprintf(stderr,"** failed to read NIfTI image from '%s'\n", fin);
      return 2;
   }

   /* assign nifti_image fname/iname pair, based on output filename
      (request to 'check' image and 'set_byte_order' here) */
   if( nifti_set_filenames(nim, fout, 1, 1) ) return 1;

   /* if we get here, write the output dataset */
   nifti_image_write( nim );

   /* and clean up memory */
   nifti_image_free( nim );

   return 0;
}
