/* ----------------------------------------------------------------------
 * A basic example to read/write a nifti dataset (e.g. cp command).
 *
 * compile example (consider -pedantic or -Wall):
 *
 * gcc -o clib_02_nifti2 clib_02.nifti2.c        \
 *     -I../include -L../lib -lniftiio -lznz -lz -lm
 *
 * OR
 *
 * gcc -o clib_02_nifti2 clib_02.nifti2.c -I ../niftilib        \
 *     -I ../znzlib ../niftilib/nifti2_io.o ../znzlib/znzlib.o -lz -lm
 *
 * R Reynolds   3 Apr 2014
 *----------------------------------------------------------------------
 */
#include <stdio.h>

#include <nifti2_io.h>
int disp_float(nifti_image * nim, int vol, int slice, int line, int offset);

int show_help( void )
{
   printf(
      "clib_02_nifti2: short exmample of reading/writing NIfTI2\n"
      "\n"
      "    This program is to demonstrate how to read a NIfTI-2 dataset,\n"
      "    set output filenames and write a NIfTI-2 dataset, all via the\n"
      "    standard NIfTI C library.\n"
      "\n"
      "    basic usage: clib_02_nifti2 -input FILE_IN -output FILE_OUT\n"
      "                 clib_02_nifti2 -input FILE_IN -disp_float_example\n"
      "\n"
      "    options:\n"
      "\n"
      "       -help               : show this help\n"
      "       -disp_float_example : show some voxel's data\n"
      "       -input  INFILE      : specify input dataset\n"
      "       -output OUTFILE     : specify output dataset\n"
      "       -verb LEVEL         : set the verbose level to LEVEL\n"
      "\n");
   return 0;
}

int main(int argc, char * argv[])
{
   nifti_image * nim=NULL;
   char        * fin=NULL, * fout=NULL;
   int           ac, disp_float_eg=0;

   if( argc < 2 ) return show_help();   /* typing '-help' is sooo much work */

   /* process user options: 4 are valid presently */
   for( ac = 1; ac < argc; ac++ ) {
      if( ! strncmp(argv[ac], "-h", 2) ) {
         return show_help();
      }
      else if( ! strcmp(argv[ac], "-disp_float_example") ) {
         disp_float_eg = 1;
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
   /* read input dataset, including data */
   nim = nifti_image_read(fin, 1);
   if( !nim ) {
      fprintf(stderr,"** failed to read NIfTI image from '%s'\n", fin);
      return 2;
   }

   if( disp_float_eg ) disp_float(nim, 0, 1, 2, 3);

   if( !fout ) { fprintf(stderr, "-- no output requested \n"); return 0; }

   /* assign nifti_image fname/iname pair, based on output filename
      (request to 'check' image and 'set_byte_order' here) */
   if( nifti_set_filenames(nim, fout, 1, 1) ) return 1;

   /* if we get here, write the output dataset */
   nifti_image_write( nim );

   /* and clean up memory */
   nifti_image_free( nim );

   return 0;
}

int disp_float(nifti_image * nim, int vol, int slice, int line, int offset)
{
   float     * dp, * d2;
   long long   lloff;
   int         nx, nxy, nxyz;

   if( ! nim ) return 1;

   if( nim->datatype != NIFTI_TYPE_FLOAT32 ) {
      fprintf(stderr,"** datatype not float, have %s\n",
              nifti_datatype_to_string(nim->datatype));
      return 1;
   }

   nx = nim->nx;
   nxy = nim->nx * nim->ny;
   nxyz = nim->nx * nim->ny * nim->nz;

   /* set a float pointer to the beginning of the data,
      so no more casting is needed */
   dp = (float *)nim->data;

   /* check limits */
   if( vol >= nim->nt || vol < 0 ) {
      fprintf(stderr,"** vol index %d is out of range [0, %lld]\n",
              vol, (long long)nim->nt-1);
      return 1;
   }
   if( slice >= nim->nz || slice < 0 ) {
      fprintf(stderr,"** slice index %d is out of range [0, %lld]\n",
              slice, (long long)nim->nz-1);
      return 1;
   }
   if( line >= nim->ny || line < 0 ) {
      fprintf(stderr,"** line index %d is out of range [0, %lld]\n",
              line, (long long)nim->ny-1);
      return 1;
   }
   if( offset >= nim->nx || offset < 0 ) {
      fprintf(stderr,"** offset %d is out of range [0, %lld]\n",
              offset, (long long)nim->nx-1);
      return 1;
   }


   d2 = dp + vol*nxyz + slice*nxy + line*nx + offset;
   /* or */
   lloff = vol*nxyz + slice*nxy + line*nx + offset;
   d2 = dp + lloff;

   printf("data[%d,%d,%d,%d] = %f\n", vol, slice, line, offset, *d2);
   /* or  */
   printf("data[%lld] = %f\n", lloff, dp[lloff]);

   return 0; /* success */
}
