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
#include "gdcmFile.h"

/*
DICOM provides a mechanism for supporting the use of JPEG Image Compression 
through the Encapsulated Format (see PS 3.3 of the DICOM Standard). 
Annex A defines a number of Transfer Syntaxes which reference 
the JPEG Standard and provide a number of lossless (bit preserving) 
and lossy compression schemes.
In order to facilitate interoperability of implementations conforming 
to the DICOM Standard which elect to use one or more 
of the Transfer Syntaxes for JPEG Image Compression, the following policy is specified:

  Any implementation which conforms to the DICOM Standard and has elected 
  to support any one of the Transfer Syntaxes for lossless JPEG Image Compression, 
  shall support the following lossless compression: 
  The subset (first-order horizontal prediction [Selection Value 1) of JPEG Process 14 
  (DPCM, non-hierarchical with Huffman coding) (see Annex F of the DICOM Standard).

   Any implementation which conforms to the DICOM Standard and has elected 
   to support any one of the Transfer Syntaxes for 8-bit lossy JPEG Image Compression, 
   shall support the JPEG Baseline Compression (coding Process 1).

   Any implementation which conforms to the DICOM Standard and has elected 
   to support any one of the Transfer Syntaxes for 12-bit lossy JPEG Image Compression, 
   shall support the JPEG Compression Process 4.

Note: The DICOM conformance statement shall differentiate between implementations 
that can simply receive JPEG encoded images and those that can receive and process 
JPEG encoded images (see PS 3.2 of the DICOM Standard).

The use of the DICOM Encapsulated Format to support JPEG Compressed Pixel Data 
implies that the Data Elements which are related to the Native Format Pixel Data encoding
(e.g. Bits Allocated, Bits Stored, High Bit, Pixel Representation, Rows, Columns, etc.) 
shall contain values which are consistent with the characteristics 
of the uncompressed pixel data from which the compressed Data Stream was derived. 
The Pixel Data characteristics included in the JPEG Interchange Format 
shall be used to decode the compressed data stream.

Run Length Encoding Compression

DICOM provides a mechanism for supporting the use of Run Length Encoding (RLE) 
Compression which is a byte oriented lossless compression scheme through 
the encapsulated Format (see PS 3.3 of this Standard). 
Annex G of the DICOM Standard defines RLE Compression and its Transfer Syntax.

Note: The RLE Compression algorithm described in Annex G 
of the DICOM Standard is the compression used in 
the TIFF 6.0 specification known as the "PackBits" scheme.

The use of the DICOM Encapsulated Format to support RLE Compressed Pixel Data 
implies that the Data Elements which are related to the Native Format Pixel Data encoding (
e.g. Bits Allocated, Bits Stored, High Bit, Pixel Representation, Rows, Columns, etc.) 
shall contain values which are consistent with the characteristics 
of the uncompressed pixel data from which the compressed data is derived
*/

/*
 * <setjmp.h> is used for the optional error recovery mechanism shown in
 * the second part of the example.
 */

/*
 * Include file for users of JPEG library.
 * You will need to have included system headers that define at least
 * the typedefs FILE and size_t before you can include jpeglib.h.
 * (stdio.h is sufficient on ANSI-conforming systems.)
 * You may also wish to include "jerror.h".
 */

#include <setjmp.h>
#include <fstream>
#include "jdatasrc.cxx"
#include "jdatadst.cxx"

namespace gdcm 
{
/******************** JPEG COMPRESSION SAMPLE INTERFACE *******************/

/* This half of the example shows how to feed data into the JPEG compressor.
 * We present a minimal version that does not worry about refinements such
 * as error recovery (the JPEG code will just exit() if it gets an error).
 */

/*
 * IMAGE DATA FORMATS:
 *
 * The standard input image format is a rectangular array of pixels, with
 * each pixel having the same number of "component" values (color channels).
 * Each pixel row is an array of JSAMPLEs (which typically are unsigned chars).
 * If you are working with color data, then the color values for each pixel
 * must be adjacent in the row; for example, R,G,B,R,G,B,R,G,B,... for 24-bit
 * RGB color.
 *
 * For this example, we'll assume that this data structure matches the way
 * our application has stored the image in memory, so we can just pass a
 * pointer to our image buffer.  In particular, let's say that the image is
 * RGB color and is described by:
 */


//extern JSAMPLE * image_buffer; /* Points to large array of R,G,B-order data */
//extern int image_height;       /* Number of rows in image */
//extern int image_width;        /* Number of columns in image */



/*
 * Sample routine for JPEG compression.  We assume that the target file name
 * and a compression quality factor are passed in.
 */

 /**
 * \ingroup File
 * \brief   routine for JPEG decompression 
 * @param fp pointer to an already open file descriptor 
 *                      8 significant bits per pixel
 * @param im_buf Points to array (of R,G,B-order) data to compress
 * @param quality compression quality
 * @param image_height Number of rows in image 
 * @param image_width Number of columns in image
 * @return 1 on success, 0 on error
 */
 
bool gdcm_write_JPEG_file (std::ofstream* fp, void*  im_buf, 
                           int image_width, int image_height, int quality)
{

   JSAMPLE* image_buffer = (JSAMPLE*) im_buf;

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
  jpeg_stdio_dest(&cinfo, fp);

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

  return true; //???
}



/*
 * SOME FINE POINTS:
 *
 * In the above loop, we ignored the return value of jpeg_write_scanlines,
 * which is the number of scanlines actually written.  We could get away
 * with this because we were only relying on the value of cinfo.next_scanline,
 * which will be incremented correctly.  If you maintain additional loop
 * variables then you should be careful to increment them properly.
 * Actually, for output to a stdio stream you needn't worry, because
 * then jpeg_write_scanlines will write all the lines passed (or else exit
 * with a fatal error).  Partial writes can only occur if you use a data
 * destination module that can demand suspension of the compressor.
 * (If you don't know what that's for, you don't need it.)
 *
 * If the compressor requires full-image buffers (for entropy-coding
 * optimization or a multi-scan JPEG file), it will create temporary
 * files for anything that doesn't fit within the maximum-memory setting.
 * (Note that temp files are NOT needed if you use the default parameters.)
 * On some systems you may need to set up a signal handler to ensure that
 * temporary files are deleted if the program is interrupted.  See libjpeg.doc.
 *
 * Scanlines MUST be supplied in top-to-bottom order if you want your JPEG
 * files to be compatible with everyone else's.  If you cannot readily read
 * your data in that order, you'll need an intermediate array to hold the
 * image.  See rdtarga.c or rdbmp.c for examples of handling bottom-to-top
 * source data using the JPEG code's internal virtual-array mechanisms.
 */



/******************** JPEG DECOMPRESSION SAMPLE INTERFACE *******************/

/* This half of the example shows how to read data from the JPEG decompressor.
 * It's a bit more refined than the above, in that we show:
 *   (a) how to modify the JPEG library's standard error-reporting behavior;
 *   (b) how to allocate workspace using the library's memory manager.
 *
 * Just to make this example a little different from the first one, we'll
 * assume that we do not intend to put the whole image into an in-memory
 * buffer, but to send it line-by-line someplace else.  We need a one-
 * scanline-high JSAMPLE array as a work buffer, and we will let the JPEG
 * memory manager allocate it for us.  This approach is actually quite useful
 * because we don't need to remember to deallocate the buffer separately: it
 * will go away automatically when the JPEG object is cleaned up.
 */

/*
 * ERROR HANDLING:
 *
 * The JPEG library's standard error handler (jerror.c) is divided into
 * several "methods" which you can override individually.  This lets you
 * adjust the behavior without duplicating a lot of code, which you might
 * have to update with each future release.
 *
 * Our example here shows how to override the "error_exit" method so that
 * control is returned to the library's caller when a fatal error occurs,
 * rather than calling exit() as the standard error_exit method does.
 *
 * We use C's setjmp/longjmp facility to return control.  This means that the
 * routine which calls the JPEG library must first execute a setjmp() call to
 * establish the return point.  We want the replacement error_exit to do a
 * longjmp().  But we need to make the setjmp buffer accessible to the
 * error_exit routine.  To do this, we make a private extension of the
 * standard JPEG error handler object.  (If we were using C++, we'd say we
 * were making a subclass of the regular error handler.)
 *
 * Here's the extended error handler struct:
 */

//-----------------------------------------------------------------------------
struct my_error_mgr {
   struct jpeg_error_mgr pub; /* "public" fields */
   jmp_buf setjmp_buffer;     /* for return to caller */
};

//-----------------------------------------------------------------------------
typedef struct my_error_mgr* my_error_ptr;

/*
 * Here's the routine that will replace the standard error_exit method:
 */
METHODDEF(void) my_error_exit (j_common_ptr cinfo) {
   /* cinfo->err really points to a my_error_mgr struct, so coerce pointer */
   my_error_ptr myerr = (my_error_ptr) cinfo->err;

   /* Always display the message. */
   /* We could postpone this until after returning, if we chose. */
   (*cinfo->err->output_message) (cinfo);

   /* Return control to the setjmp point */
   longjmp(myerr->setjmp_buffer, 1);
}

//-----------------------------------------------------------------------------
/*
 * Sample routine for JPEG decompression.  We assume that the source file name
 * is passed in.  We want to return 1 on success, 0 on error.
 */
 
 /**
 * \brief   routine for JPEG decompression 
 * @param fp pointer to an already open file descriptor 
 *                      8 significant bits per pixel
 * @param image_buffer to receive uncompressed pixels
 * @return 1 on success, 0 on error
 */
 
bool gdcm_read_JPEG_file ( std::ifstream* fp, void* image_buffer )
{
   char* pimage;

   /* This struct contains the JPEG decompression parameters and pointers to
    * working space (which is allocated as needed by the JPEG library).
    */
   struct jpeg_decompress_struct cinfo;

   /* -------------- inside, we found :
    * JDIMENSION image_width;       // input image width 
    * JDIMENSION image_height;      // input image height 
    * int input_components;         // nb of color components in input image 
    * J_COLOR_SPACE in_color_space; // colorspace of input image 
    * double input_gamma;           // image gamma of input image 
    * -------------- */

   /* We use our private extension JPEG error handler.
    * Note that this struct must live as long as the main JPEG parameter
    * struct, to avoid dangling-pointer problems.
    */
   struct my_error_mgr jerr;
   /* More stuff */

   JSAMPARRAY buffer;/* Output row buffer */
  
   // rappel :
   // ------
   // typedef unsigned char JSAMPLE;
   // typedef JSAMPLE FAR *JSAMPROW;/* ptr to one image row of pixel samples. */
   // typedef JSAMPROW *JSAMPARRAY;/* ptr to some rows (a 2-D sample array) */
   // typedef JSAMPARRAY *JSAMPIMAGE;/* a 3-D sample array: top index is color */

   int row_stride;/* physical row width in output buffer */
  
#ifdef GDCM_JPG_DEBUG
   printf("entree dans File::gdcm_read_JPEG_file (i.e. 8), depuis gdcmJpeg\n");
#endif //GDCM_JPG_DEBUG

   /* In this example we want to open the input file before doing anything else,
    * so that the setjmp() error recovery below can assume the file is open.
    * VERY IMPORTANT: use "b" option to fopen() if you are on a machine that
    * requires it in order to read binary files.
    */
    
  /* Step 1: allocate and initialize JPEG decompression object */  
#ifdef GDCM_JPG_DEBUG
  printf("Entree Step 1\n");
#endif //GDCM_JPG_DEBUG
  
  /* We set up the normal JPEG error routines, then override error_exit. */
  
  cinfo.err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = my_error_exit;
  
  /* Establish the setjmp return context for my_error_exit to use. */  
  if (setjmp(jerr.setjmp_buffer))
  {
    /* If we get here, the JPEG code has signaled an error.
     * We need to clean up the JPEG object, close the input file, and return.
     */
    jpeg_destroy_decompress(&cinfo);
    return 0;
  }
  /* Now we can initialize the JPEG decompression object. */
  jpeg_create_decompress(&cinfo);

   /* Step 2: specify data source (eg, a file) */
#ifdef GDCM_JPG_DEBUG
  printf("Entree Step 2\n");
#endif //GDCM_JPG_DEBUG

   jpeg_stdio_src(&cinfo, fp);

   /* Step 3: read file parameters with jpeg_read_header() */
#ifdef GDCM_JPG_DEBUG
  printf("Entree Step 3\n");
#endif //GDCM_JPG_DEBUG

   (void) jpeg_read_header(&cinfo, TRUE);
   
   /* We can ignore the return value from jpeg_read_header since
    *   (a) suspension is not possible with the stdio data source, and
    *   (b) we passed TRUE to reject a tables-only JPEG file as an error.
    * See libjpeg.doc for more info.
    */

    // prevent the library from performing any color space conversion
   if( cinfo.process == JPROC_LOSSLESS )
   {
      cinfo.jpeg_color_space = JCS_UNKNOWN;
      cinfo.out_color_space = JCS_UNKNOWN;
   }


#ifdef GDCM_JPG_DEBUG
      printf("--------------Header contents :----------------\n");
      printf("image_width %d image_height %d\n", 
              cinfo.image_width , cinfo.image_height);
      printf("bits of precision in image data  %d \n", 
              cinfo.output_components);
      printf("nb of color components returned  %d \n", 
              cinfo.data_precision);
#endif //GDCM_JPG_DEBUG


   /*
    * JDIMENSION image_width;       // input image width 
    * JDIMENSION image_height;      // input image height 
    * int output_components;        // # of color components returned 
    * J_COLOR_SPACE in_color_space; // colorspace of input image 
    * double input_gamma;           // image gamma of input image
    * int data_precision;           // bits of precision in image data 
    */

   /* Step 4: set parameters for decompression */
#ifdef GDCM_JPG_DEBUG
  printf("Entree Step 4\n");
#endif //GDCM_JPG_DEBUG
   /* In this example, we don't need to change any of the defaults set by
    * jpeg_read_header(), so we do nothing here.
    */

   /* Step 5: Start decompressor */
#ifdef GDCM_JPG_DEBUG
   printf("Entree Step 5\n");
#endif //GDCM_JPG_DEBUG

   (void) jpeg_start_decompress(&cinfo);
   /* We can ignore the return value since suspension is not possible
    * with the stdio data source.
    */

   /* We may need to do some setup of our own at this point before reading
    * the data.  After jpeg_start_decompress() we have the correct scaled
    * output image dimensions available, as well as the output colormap
    * if we asked for color quantization.
    * In this example, we need to make an output work buffer of the right size.
    */ 

   /* JSAMPLEs per row in output buffer */
   row_stride = cinfo.output_width * cinfo.output_components*2;
  
#ifdef GDCM_JPG_DEBUG
  printf ("cinfo.output_width %d cinfo.output_components %d  row_stride %d\n",
                      cinfo.output_width, cinfo.output_components,row_stride);
#endif //GDCM_JPG_DEBUG

   /* Make a one-row-high sample array that will go away when done with image */
   buffer = (*cinfo.mem->alloc_sarray)
            ((j_common_ptr) &cinfo, JPOOL_IMAGE, row_stride, 1);

   /* Step 6: while (scan lines remain to be read) */
#ifdef GDCM_JPG_DEBUG
    printf("Entree Step 6\n"); 
#endif //GDCM_JPG_DEBUG
   /*           jpeg_read_scanlines(...); */

   /* Here we use the library's state variable cinfo.output_scanline as the
    * loop counter, so that we don't have to keep track ourselves.
    */
#ifdef GDCM_JPG_DEBUG
      printf ("cinfo.output_height %d  cinfo.output_width %d\n",
               cinfo.output_height,cinfo.output_width);
#endif //GDCM_JPG_DEBUG
   pimage=(char *)image_buffer;
  
   int bufsize = cinfo.output_width * cinfo.output_components;
   size_t rowsize = bufsize * sizeof(JSAMPLE);

   while (cinfo.output_scanline < cinfo.output_height) {
      /* jpeg_read_scanlines expects an array of pointers to scanlines.
       * Here the array is only one element long, but you could ask for
       * more than one scanline at a time if that's more convenient.
       */

     //printf( "scanlines: %d\n",cinfo.output_scanline);
      (void) jpeg_read_scanlines(&cinfo, buffer, 1);
      memcpy( pimage, *buffer,rowsize); 
      pimage+=rowsize;
   }

  /* Step 7: Finish decompression */
#ifdef GDCM_JPG_DEBUG
   printf("Entree Step 7\n");
#endif //GDCM_JPG_DEBUG

   (void) jpeg_finish_decompress(&cinfo);
   
   /* We can ignore the return value since suspension is not possible
    * with the stdio data source.
    */

   /* Step 8: Release JPEG decompression object */

#ifdef GDCM_JPG_DEBUG
  printf("Entree Step 8\n");
#endif //GDCM_JPG_DEBUG

   /* This is an important step since it will release a good deal of memory. */

   jpeg_destroy_decompress(&cinfo);

   /* After finish_decompress, we can close the input file.
    * Here we postpone it until after no more JPEG errors are possible,
    * so as to simplify the setjmp error logic above.  (Actually, I don't
    * think that jpeg_destroy can do an error exit, but why assume anything...)
    */

   /* At this point you may want to check to see whether any corrupt-data
    * warnings occurred (test whether jerr.pub.num_warnings is nonzero).
    */

   /* And we're done! */

   return true;
}


/*
 * SOME FINE POINTS:
 *
 * In the above code, we ignored the return value of jpeg_read_scanlines,
 * which is the number of scanlines actually read.  We could get away with
 * this because we asked for only one line at a time and we weren't using
 * a suspending data source.  See libjpeg.doc for more info.
 *
 * We cheated a bit by calling alloc_sarray() after jpeg_start_decompress();
 * we should have done it beforehand to ensure that the space would be
 * counted against the JPEG max_memory setting.  In some systems the above
 * code would risk an out-of-memory error.  However, in general we don't
 * know the output image dimensions before jpeg_start_decompress(), unless we
 * call jpeg_calc_output_dimensions().  See libjpeg.doc for more about this.
 *
 * Scanlines are returned in the same order as they appear in the JPEG file,
 * which is standardly top-to-bottom.  If you must emit data bottom-to-top,
 * you can use one of the virtual arrays provided by the JPEG memory manager
 * to invert the data.  See wrbmp.c for an example.
 *
 * As with compression, some operating modes may require temporary files.
 * On some systems you may need to set up a signal handler to ensure that
 * temporary files are deleted if the program is interrupted.  See libjpeg.doc.
 */
 
//----------------------------------------------------------------------------

} // end namespace gdcm
