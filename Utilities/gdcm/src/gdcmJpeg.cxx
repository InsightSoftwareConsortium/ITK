/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmJpeg.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
                                                                                
  Copyright (c) CREATIS (Centre de Recherche et d'Applications en Traitement de
  l'Image). All rights reserved. See Doc/License.txt or
  http://www.creatis.insa-lyon.fr/Public/Gdcm/License.html for details.
                                                                                
     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.
                                                                                
=========================================================================*/
#include "gdcmFileHelper.h"
#include "gdcmJPEGFragment.h"
#include "gdcmDebug.h"

#if defined(__sgi) && !defined(__GNUC__)
// Try to get rid of the warning:
//cc-3505 CC: WARNING File = /usr/include/internal/setjmp_core.h, Line = 74
//  setjmp not marked as unknown_control_flow because it is not declared as a
//          function
//
//  #pragma unknown_control_flow (setjmp)
#  if   (_COMPILER_VERSION >= 730)
#  pragma set woff 3505
#  endif
#endif
#ifdef _MSC_VER
// Let us get rid of this funny warning on /W4:
// warning C4611: interaction between '_setjmp' and C++ object
// destruction is non-portable
#pragma warning( disable : 4611 )
#endif

#include <setjmp.h>
#include <fstream>

#if defined(__BORLANDC__)
   #include <mem.h> // for memset
#endif 

#include "jdatasrc.cxx"
#include "jdatadst.cxx"

namespace gdcm 
{

 /**
 * \brief   routine for JPEG decompression 
 * @param fp pointer to an already open file descriptor 
 *                      8 significant bits per pixel
 * @param im_buf Points to array (of R,G,B-order) data to compress
 * @param quality compression quality
 * @param image_height Number of rows in image 
 * @param image_width Number of columns in image
 * @return 1 on success, 0 on error
 */
 
bool gdcm_write_JPEG_file (std::ostream *fp, void *im_buf, 
                           int image_width, int image_height, int quality)
{

   JSAMPLE *image_buffer = (JSAMPLE*) im_buf;

  /* This struct contains the JPEG compression parameters and pointers to
   * working space (which is allocated as needed by the JPEG library).
   * It is possible to have several such structures, representing multiple
   * compression/decompression processes, in existence at once.  We refer
   * to any one struct (and its associated working data) as a "JPEG object".
   */
  struct jpeg_compress_struct cinfo;
  /* This struct represents a JPEG error handler.  It is declared separately
   * because applications often want to supply a specialized error handler
   * (see the second half of this file for an example).  But here we just
   * take the easy way out and use the standard error handler, which will
   * print a message on stderr and call exit() if compression fails.
   * Note that this struct must live as long as the main JPEG parameter
   * struct, to avoid dangling-pointer problems.
   */
  struct jpeg_error_mgr jerr;
  /* More stuff */
  //FILE*  outfile;    /* target FILE* /
  JSAMPROW row_pointer[1];   /* pointer to JSAMPLE row[s] */
  int row_stride;            /* physical row width in image buffer */

  /* Step 1: allocate and initialize JPEG compression object */

  /* We have to set up the error handler first, in case the initialization
   * step fails.  (Unlikely, but it could happen if you are out of memory.)
   * This routine fills in the contents of struct jerr, and returns jerr's
   * address which we place into the link field in cinfo.
   */
  cinfo.err = jpeg_std_error(&jerr);
  /* Now we can initialize the JPEG compression object. */
  jpeg_create_compress(&cinfo);

  /* Step 2: specify data destination (eg, a file) */
  /* Note: steps 2 and 3 can be done in either order. */

  /* Here we use the library-supplied code to send compressed data to a
   * stdio stream.  You can also write your own code to do something else.
   * VERY IMPORTANT: use "b" option to fopen() if you are on a machine that
   * requires it in order to write binary files.
   */
 // if ((outfile = fopen(filename, "wb")) == NULL) {
 //   fprintf(stderr, "can't open %s\n", filename);
 //   exit(1);
 //
 // }
  assert( 0 );
  (void)fp;
  //jpeg_stdio_dest(&cinfo, fp, 0, 0, image_width, image_height, quality);

  /* Step 3: set parameters for compression */

  /* First we supply a description of the input image.
   * Four fields of the cinfo struct must be filled in:
   */
  cinfo.image_width = image_width;/* image width and height, in pixels */
  cinfo.image_height = image_height;
  cinfo.input_components = 3;     /* # of color components per pixel */
  cinfo.in_color_space = JCS_RGB; /* colorspace of input image */
  /* Now use the library's routine to set default compression parameters.
   * (You must set at least cinfo.in_color_space before calling this,
   * since the defaults depend on the source color space.)
   */
  jpeg_set_defaults(&cinfo);
  /* Now you can set any non-default parameters you wish to.
   * Here we just illustrate the use of quality (quantization table) scaling:
   */
  jpeg_set_quality(&cinfo, quality, TRUE /* limit to baseline-JPEG values */);

  /* Step 4: Start compressor */

  /* TRUE ensures that we will write a complete interchange-JPEG file.
   * Pass TRUE unless you are very sure of what you're doing.
   */
  jpeg_start_compress(&cinfo, TRUE);

  /* Step 5: while (scan lines remain to be written) */
  /*           jpeg_write_scanlines(...); */

  /* Here we use the library's state variable cinfo.next_scanline as the
   * loop counter, so that we don't have to keep track ourselves.
   * To keep things simple, we pass one scanline per call; you can pass
   * more if you wish, though.
   */
  row_stride = image_width * 3;/* JSAMPLEs per row in image_buffer */

  while (cinfo.next_scanline < cinfo.image_height) {
    /* jpeg_write_scanlines expects an array of pointers to scanlines.
     * Here the array is only one element long, but you could pass
     * more than one scanline at a time if that's more convenient.
     */
    row_pointer[0] = & image_buffer[cinfo.next_scanline * row_stride];

    (void) jpeg_write_scanlines(&cinfo, row_pointer, 1);
  }

  /* Step 6: Finish compression */

  jpeg_finish_compress(&cinfo);
  
  /* After finish_compress, we can close the output file. */
  
 // fclose(fp); --> the caller will close (multiframe treatement)

  /* Step 7: release JPEG compression object */

  /* This is an important step since it will release a good deal of memory. */
  jpeg_destroy_compress(&cinfo);

  /* And we're done! */

  return true;
}

//-----------------------------------------------------------------------------
struct my_error_mgr {
   struct jpeg_error_mgr pub; /* "public" fields */
   jmp_buf setjmp_buffer;     /* for return to caller */
};
typedef struct my_error_mgr* my_error_ptr;
//-----------------------------------------------------------------------------

/*
 * Here's the routine that will replace the standard error_exit method:
 */
extern "C" {
METHODDEF(void) my_error_exit (j_common_ptr cinfo) {
   /* cinfo->err really points to a my_error_mgr struct, so coerce pointer */
   my_error_ptr myerr = (my_error_ptr) cinfo->err;

   /* Always display the message. */
   /* We could postpone this until after returning, if we chose. */
   (*cinfo->err->output_message) (cinfo);

   /* Return control to the setjmp point */
   longjmp(myerr->setjmp_buffer, 1);
}

//METHODDEF(void) my_output_message (j_common_ptr cinfo)
//{
//   char buffer[JMSG_LENGTH_MAX];
// 
//   /* Create the message */
//   (*cinfo->err->format_message) (cinfo, buffer);
//
//   // Custom display message, we could be more fancy and throw an exception:
//   gdcmErrorMacro( buffer );
//}

}
//-----------------------------------------------------------------------------
 
/**
 * \brief   routine for JPEG decompression 
 * @param fp pointer to an already open file descriptor 
 *                      8 significant bits per pixel
 * @param image_buffer to receive uncompressed pixels
 * @param statesuspension Suspension State basically it should be 3 otherwise more complex to handle
 * @return 1 on success, 0 on error
 */
void *SampBuffer; 
bool JPEGFragment::ReadJPEGFile (std::ifstream *fp, void *image_buffer, int &statesuspension)
{
   pImage = (uint8_t*)image_buffer;
   // This struct contains the JPEG decompression parameters and pointers to
   // working space (which is allocated as needed by the JPEG library).

   static struct jpeg_decompress_struct cinfo;

   // -------------- inside, we found :
   // JDIMENSION image_width;       // input image width 
   // JDIMENSION image_height;      // input image height 
   // int input_components;         // nb of color components in input image 
   // J_COLOR_SPACE in_color_space; // colorspace of input image 
   // double input_gamma;           // image gamma of input image 

   // We use our private extension JPEG error handler.
   // Note that this struct must live as long as the main JPEG parameter
   // struct, to avoid dangling-pointer problems.

   struct my_error_mgr jerr;

   JSAMPARRAY buffer;// Output row buffer
  
   // rappel :
   // ------
   // typedef unsigned char JSAMPLE;
   // typedef JSAMPLE FAR *JSAMPROW;/* ptr to one image row of pixel samples. */
   // typedef JSAMPROW *JSAMPARRAY;/* ptr to some rows (a 2-D sample array) */
   // typedef JSAMPARRAY *JSAMPIMAGE;/* a 3-D sample array: top index is color */

   int row_stride;// physical row width in output buffer
  
  // We set up the normal JPEG error routines, then override error_exit.
  
  cinfo.err = jpeg_std_error(&jerr.pub);
  // for any jpeg error call my_error_exit
  jerr.pub.error_exit = my_error_exit;
  // for any output message call my_output_message
  //jerr.pub.output_message = my_output_message;

  // Establish the setjmp return context for my_error_exit to use.
  if (setjmp(jerr.setjmp_buffer))
  {
    // If we get here, the JPEG code has signaled an error.
    // We need to clean up the JPEG object, close the input file, and return.

    gdcmErrorMacro( "Serious Problem !" );
    jpeg_destroy_decompress(&cinfo);
    return 0;
  }
  // Now we can initialize the JPEG decompression object.
  if ( statesuspension == 0 )
    {
    jpeg_create_decompress(&cinfo);
    jpeg_stdio_src(&cinfo, fp, this, 1);
    }
  else
    {
    jpeg_stdio_src(&cinfo, fp, this, 0);
    }
   // Step 3: read file parameters with jpeg_read_header()

   if ( statesuspension < 2 )
   {
      if ( jpeg_read_header(&cinfo, TRUE) == JPEG_SUSPENDED )
      {
      // Suspension in jpeg_read_header
      statesuspension = 2; 
      }
 
      // Step 4: set parameters for decompression
      // prevent the library from performing any color space conversion
      if ( cinfo.process == JPROC_LOSSLESS )
      {
         cinfo.jpeg_color_space = JCS_UNKNOWN;
         cinfo.out_color_space = JCS_UNKNOWN;
      }
   }

   // Step 5: Start decompressor
   if (statesuspension < 3 )
   {
      if ( jpeg_start_decompress(&cinfo) == FALSE )
      {
         // Suspension: jpeg_start_decompress
         statesuspension = 3;
      }

      // JSAMPLEs per row in output buffer
      row_stride = cinfo.output_width * cinfo.output_components*2;
  
      // Make a one-row-high sample array that will go away when done with image
      buffer = (*cinfo.mem->alloc_sarray)
            ((j_common_ptr) &cinfo, JPOOL_IMAGE, row_stride, 1);

      // Step 6: while (scan lines remain to be read)

      // Save the buffer in case of suspension to be able to reuse it later:
      SampBuffer = buffer;
   }
   else
   {
      // Suspension: re-use the buffer:
      buffer = (JSAMPARRAY)SampBuffer;
   }
   int bufsize = cinfo.output_width * cinfo.output_components;
   size_t rowsize = bufsize * sizeof(JSAMPLE);

   while (cinfo.output_scanline < cinfo.output_height)
   {
      if ( jpeg_read_scanlines(&cinfo, buffer, 1) == 0 )
        {
        // Suspension in jpeg_read_scanlines
        statesuspension = 3;
        return true;
        }
// The ijg has no notion of big endian, therefore always swap the jpeg stream
#if (defined(GDCM_WORDS_BIGENDIAN) || defined(GDCM_FORCE_BIGENDIAN_EMULATION)) && (CMAKE_BITS_IN_JSAMPLE != 8)
      uint16_t *buffer16 = (uint16_t*)*buffer;
      uint16_t *pimage16 = (uint16_t*)pImage;
      for(unsigned int i=0;i<rowsize/2;i++)
        pimage16[i] = (buffer16[i] >> 8) | (buffer16[i] << 8 );
#else
      memcpy( pImage, *buffer,rowsize);
#endif //GDCM_WORDS_BIGENDIAN
      pImage+=rowsize;
   }

   // Step 7: Finish decompression
   if ( jpeg_finish_decompress(&cinfo) == FALSE )
     {
     // Suspension: jpeg_finish_decompress
     statesuspension = 4;
     }
   
   // Step 8: Release JPEG decompression object
   jpeg_destroy_decompress(&cinfo);

   // At this point you may want to check to see whether any corrupt-data
   // warnings occurred (test whether jerr.pub.num_warnings is nonzero).

   return true;
}

#ifdef _MSC_VER
// Put the warning back
#pragma warning( default : 4611 )
#endif

} // end namespace gdcm
