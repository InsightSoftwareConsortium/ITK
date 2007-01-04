/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmJpeg2000.cxx
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
#include "gdcmDebug.h"

#include <iostream>
#include <fstream>

#if defined(__BORLANDC__)
   #include <mem.h> // for memset
#endif 
#include <stdio.h> // for fprintf

extern "C" {
  #include "openjpeg/openjpeg.h"
}

namespace gdcm 
{
//-----------------------------------------------------------------------------
 /**
 * \brief   routine for JPEG decompression 
 * @param raw raw
 * @param inputdata inputdata
 * @param inputlength inputlength 
 * @return 1 on success, 0 on error
 */

/**
sample error callback expecting a FILE* client object
*/
void error_callback(const char *msg, void *) {
  std::cerr << "Error in gdcmopenjpeg" << msg << std::endl;
}
/**
sample warning callback expecting a FILE* client object
*/
void warning_callback(const char *msg, void *) {
  std::cerr << "Warning in gdcmopenjpeg" << msg << std::endl;
}
/**
sample debug callback expecting no client object
*/
void info_callback(const char *, void *) {
//  std::cerr << "Info in gdcmopenjpeg" << msg << std::endl;
}

#define J2K_CFMT 0
#define JP2_CFMT 1
#define JPT_CFMT 2
#define MJ2_CFMT 3
#define PXM_DFMT 0
#define PGX_DFMT 1
#define BMP_DFMT 2
#define YUV_DFMT 3

/*
 * Divide an integer by a power of 2 and round upwards.
 *
 * a divided by 2^b
 */
inline int int_ceildivpow2(int a, int b) {
  return (a + (1 << b) - 1) >> b;
}

/*
 * The following function was copy paste from j2k_to_image.c with part from convert.c
 */
bool gdcm_read_JPEG2000_file (void* raw, char *inputdata, size_t inputlength)
{
  opj_dparameters_t parameters;  /* decompression parameters */
  opj_event_mgr_t event_mgr;    /* event manager */
  opj_image_t *image;
  opj_dinfo_t* dinfo;  /* handle to a decompressor */
  opj_cio_t *cio;
  unsigned char *src = (unsigned char*)inputdata; 
  int file_length = static_cast< int >( inputlength );

  /* configure the event callbacks (not required) */
  memset(&event_mgr, 0, sizeof(opj_event_mgr_t));
  event_mgr.error_handler = error_callback;
  event_mgr.warning_handler = warning_callback;
  event_mgr.info_handler = info_callback;

  /* set decoding parameters to default values */
  opj_set_default_decoder_parameters(&parameters);
 
   // default blindly copied
   parameters.cp_layer=0;
   parameters.cp_reduce=0;
//   parameters.decod_format=-1;
//   parameters.cod_format=-1;

      /* JPEG-2000 codestream */
    parameters.decod_format = J2K_CFMT;
    assert(parameters.decod_format == J2K_CFMT);
  parameters.cod_format = PGX_DFMT;
  assert(parameters.cod_format == PGX_DFMT);

      /* get a decoder handle */
      dinfo = opj_create_decompress(CODEC_J2K);

      /* catch events using our callbacks and give a local context */
      opj_set_event_mgr((opj_common_ptr)dinfo, &event_mgr, NULL);

      /* setup the decoder decoding parameters using user parameters */
      opj_setup_decoder(dinfo, &parameters);

      /* open a byte stream */
      cio = opj_cio_open((opj_common_ptr)dinfo, src, file_length);

      /* decode the stream and fill the image structure */
      image = opj_decode(dinfo, cio);
      if(!image) {
        opj_destroy_decompress(dinfo);
        opj_cio_close(cio);
        return 1;
      }
      
      /* close the byte stream */
      opj_cio_close(cio);

  /* free the memory containing the code-stream */
  delete[] src;  //FIXME

   // Copy buffer
   for (int compno = 0; compno < image->numcomps; compno++)
   {
      opj_image_comp_t *comp = &image->comps[compno];

      int w = image->comps[compno].w;
      int wr = int_ceildivpow2(image->comps[compno].w, image->comps[compno].factor);

      //int h = image.comps[compno].h;
      int hr = int_ceildivpow2(image->comps[compno].h, image->comps[compno].factor);

      if (comp->prec <= 8)
      {
         uint8_t *data8 = (uint8_t*)raw + compno;
         for (int i = 0; i < wr * hr; i++)
         {
            int v = image->comps[compno].data[i / wr * w + i % wr];
            *data8 = (uint8_t)v;
            data8 += image->numcomps;
         }
      }
      else if (comp->prec <= 16)
      {
         uint16_t *data16 = (uint16_t*)raw + compno;
         for (int i = 0; i < wr * hr; i++)
         {
            int v = image->comps[compno].data[i / wr * w + i % wr];
            *data16 = (uint16_t)v;
            data16 += image->numcomps;
         }
      }
      else
      {
         uint32_t *data32 = (uint32_t*)raw + compno;
         for (int i = 0; i < wr * hr; i++)
         {
            int v = image->comps[compno].data[i / wr * w + i % wr];
            *data32 = (uint32_t)v;
            data32 += image->numcomps;
         }
      }
      //free(image.comps[compno].data);
   }


  /* free remaining structures */
  if(dinfo) {
    opj_destroy_decompress(dinfo);
  }

  /* free image data structure */
  opj_image_destroy(image);

  return true;
}

template<typename T>
void rawtoimage_fill(T *inputbuffer, int w, int h, int numcomps, opj_image_t *image)
{
  T *p = inputbuffer;
  for (int i = 0; i < w * h; i++)
    {
    for(int compno = 0; compno < numcomps; compno++)
      {
      /* compno : 0 = GREY, (0, 1, 2) = (R, G, B) */
      image->comps[compno].data[i] = *p;
      ++p;
      }
    }
}

opj_image_t* rawtoimage(char *inputbuffer, opj_cparameters_t *parameters,
  int fragment_size, int image_width, int image_height, int sample_pixel,
  int bitsallocated, int sign, int quality)
{
  (void)quality;
  (void)fragment_size;
  int w, h;
  int numcomps;
  OPJ_COLOR_SPACE color_space;
  opj_image_cmptparm_t cmptparm[3]; /* maximum of 3 components */
  opj_image_t * image = NULL;

  assert( sample_pixel == 1 || sample_pixel == 3 );
  if( sample_pixel == 1 )
    {
    numcomps = 1;
    color_space = CLRSPC_GRAY;
    }
  else // sample_pixel == 3
    {
    numcomps = 3;
    color_space = CLRSPC_SRGB;
    }
  int subsampling_dx = parameters->subsampling_dx;
  int subsampling_dy = parameters->subsampling_dy;

  // FIXME
  w = image_width;
  h = image_height;

  /* initialize image components */
  memset(&cmptparm[0], 0, 3 * sizeof(opj_image_cmptparm_t));
  //assert( bitsallocated == 8 );
  for(int i = 0; i < numcomps; i++) {
    cmptparm[i].prec = bitsallocated;
    cmptparm[i].bpp = bitsallocated;
    cmptparm[i].sgnd = sign;
    cmptparm[i].dx = subsampling_dx;
    cmptparm[i].dy = subsampling_dy;
    cmptparm[i].w = w;
    cmptparm[i].h = h;
  }

  /* create the image */
  image = opj_image_create(numcomps, &cmptparm[0], color_space);
  if(!image) {
    return NULL;
  }
  /* set image offset and reference grid */
  image->x0 = parameters->image_offset_x0;
  image->y0 = parameters->image_offset_y0;
  image->x1 = parameters->image_offset_x0 + (w - 1) * subsampling_dx + 1;
  image->y1 = parameters->image_offset_y0 + (h - 1) * subsampling_dy + 1;

  /* set image data */

  //assert( fragment_size == numcomps*w*h*(bitsallocated/8) );
  if (bitsallocated <= 8)
    {
    if( sign )
      {
      rawtoimage_fill<int8_t>((int8_t*)inputbuffer,w,h,numcomps,image);
      }
    else
      {
      rawtoimage_fill<uint8_t>((uint8_t*)inputbuffer,w,h,numcomps,image);
      }
    }
  else if (bitsallocated <= 16)
    {
    if( sign )
      {
      rawtoimage_fill<int16_t>((int16_t*)inputbuffer,w,h,numcomps,image);
      }
    else
      {
      rawtoimage_fill<uint16_t>((uint16_t*)inputbuffer,w,h,numcomps,image);
      }
    }
  else if (bitsallocated <= 32)
    {
    if( sign )
      {
      rawtoimage_fill<int32_t>((int32_t*)inputbuffer,w,h,numcomps,image);
      }
    else
      {
      rawtoimage_fill<uint32_t>((uint32_t*)inputbuffer,w,h,numcomps,image);
      }
    }
  else
    {
    abort();
    }

  return image;
}

/*
 * The following function was copy paste from image_to_j2k.c with part from convert.c
 */
bool gdcm_write_JPEG2000_file (std::ostream *fp, char *inputdata, size_t inputlength, 
  int image_width, int image_height, int numZ, int sample_pixel, int bitsallocated,
  int sign, int quality)
{
//// input_buffer is ONE image
//// fragment_size is the size of this image (fragment)
  (void)numZ;
  bool bSuccess;
  //bool delete_comment = true;
  opj_cparameters_t parameters;  /* compression parameters */
  opj_event_mgr_t event_mgr;    /* event manager */
  opj_image_t *image = NULL;
  //quality = 100;

  /*
  configure the event callbacks (not required)
  setting of each callback is optionnal
  */
  memset(&event_mgr, 0, sizeof(opj_event_mgr_t));
  event_mgr.error_handler = error_callback;
  event_mgr.warning_handler = warning_callback;
  event_mgr.info_handler = info_callback;

  /* set encoding parameters to default values */
  memset(&parameters, 0, sizeof(parameters));
  opj_set_default_encoder_parameters(&parameters);

  /* if no rate entered, lossless by default */
  parameters.tcp_rates[0] = 0;
  parameters.tcp_numlayers = 1;
  parameters.cp_disto_alloc = 1;

  if(parameters.cp_comment == NULL) {
    const char comment[] = "Created by ITK/GDCM/OpenJPEG version 1.0";
    parameters.cp_comment = (char*)malloc(strlen(comment) + 1);
    strcpy(parameters.cp_comment, comment);
    /* no need to delete parameters.cp_comment on exit */
    //delete_comment = false;
  }

  
  /* decode the source image */
  /* ----------------------- */

  image = rawtoimage((char*)inputdata, &parameters, 
    static_cast<int>( inputlength ), 
    image_width, image_height,
    sample_pixel, bitsallocated, sign, quality);
  if (!image) {
    return 1;
  }

    /* encode the destination image */
  /* ---------------------------- */
   parameters.cod_format = J2K_CFMT; /* J2K format output */
    int codestream_length;
    opj_cio_t *cio = NULL;
    //FILE *f = NULL;

    /* get a J2K compressor handle */
    opj_cinfo_t* cinfo = opj_create_compress(CODEC_J2K);

    /* catch events using our callbacks and give a local context */
    opj_set_event_mgr((opj_common_ptr)cinfo, &event_mgr, stderr);

    /* setup the encoder parameters using the current image and using user parameters */
    opj_setup_encoder(cinfo, &parameters, image);

    /* open a byte stream for writing */
    /* allocate memory for all tiles */
    cio = opj_cio_open((opj_common_ptr)cinfo, NULL, 0);

    /* encode the image */
    bSuccess = opj_encode(cinfo, cio, image, parameters.index);
    if (!bSuccess) {
      opj_cio_close(cio);
      fprintf(stderr, "failed to encode image\n");
      return 1;
    }
    codestream_length = cio_tell(cio);

    /* write the buffer to disk */
    //f = fopen(parameters.outfile, "wb");
    //if (!f) {
    //  fprintf(stderr, "failed to open %s for writing\n", parameters.outfile);
    //  return 1;
    //}
    //fwrite(cio->buffer, 1, codestream_length, f);
//#define MDEBUG
#ifdef MDEBUG
    static int c = 0;
    itksys_ios::ostringstream os;
    os << "/tmp/debug";
    os << c;
    c++;
    os << ".j2k";
    std::ofstream debug(os.str().c_str());
    debug.write((char*)(cio->buffer), codestream_length);
    debug.close();
#endif
    fp->write((char*)(cio->buffer), codestream_length);
    //fclose(f);

    /* close and free the byte stream */
    opj_cio_close(cio);

    /* free remaining compression structures */
    opj_destroy_compress(cinfo);


      /* free user parameters structure */
  //if(delete_comment) {
    if(parameters.cp_comment) free(parameters.cp_comment);
  //}
  if(parameters.cp_matrice) free(parameters.cp_matrice);

  /* free image data */
  opj_image_destroy(image);




  return true;
}

#if 0
// For openjpeg 0.97
bool gdcm_read_JPEG2000_file (void* raw, char *inputdata, size_t inputlength)
{
   j2k_image_t img;
   j2k_cp_t cp;
 
   // default blindly copied
   cp.layer=0;
   cp.reduce=0;
   cp.decod_format=-1;
   cp.cod_format=-1;
 
   cp.cod_format=J2K_CFMT;
   cp.decod_format = PGX_DFMT;
   int len = inputlength;
   unsigned char *src = (unsigned char*)inputdata;
 
   // Decompression
   if (!j2k_decode(src, len, &img, &cp))
   {
      gdcmErrorMacro( "ERROR -> j2k_to_image: failed to decode image!" );
      return false;
   }
 
   // Copy buffer
   for (int compno = 0; compno < img.numcomps; compno++)
   {
      j2k_comp_t *comp = &img.comps[compno];
  
      int w = img.comps[compno].w;
      int wr = int_ceildivpow2(img.comps[compno].w, img.comps[compno].factor);
  
      //int h = img.comps[compno].h;
      int hr = int_ceildivpow2(img.comps[compno].h, img.comps[compno].factor);
  
      if (comp->prec <= 8)
      {
         uint8_t *data8 = (uint8_t*)raw;
         for (int i = 0; i < wr * hr; i++) 
         {
            int v = img.comps[compno].data[i / wr * w + i % wr];
            *data8++ = (uint8_t)v;
         }
      }
      else if (comp->prec <= 16)
      {
         uint16_t *data16 = (uint16_t*)raw;
         for (int i = 0; i < wr * hr; i++) 
         {
            int v = img.comps[compno].data[i / wr * w + i % wr];
            *data16++ = (uint16_t)v;
         }
      }
      else
      {
         uint32_t *data32 = (uint32_t*)raw;
         for (int i = 0; i < wr * hr; i++) 
         {
            int v = img.comps[compno].data[i / wr * w + i % wr];
            *data32++ = (uint32_t)v;
         }
      }
      free(img.comps[compno].data);
   }
 
   // Free remaining structures
   j2k_dec_release();
   // FIXME
   delete[] inputdata;
 
   return true;
}
#endif

#if 0
bool gdcm_read_JASPER_file (void* raw, char *inputdata, size_t inputlength)
{
#if 0
  std::cerr << "Inputlenght=" << inputlength << std::endl;
  std::ofstream out("/tmp/jpeg2000.jpc", std::ios::binary);
  out.write((char*)inputdata,inputlength);
  out.close();
#endif
  jas_init(); //important...
  jas_stream_t *jasStream = 
    jas_stream_memopen((char *)inputdata, inputlength);
    
  int fmtid;
  if ((fmtid = jas_image_getfmt(jasStream)) < 0) 
    {
    gdcmErrorMacro("unknown image format");
    return false;
    }

  // Decode the image. 
  jas_image_t *jasImage /* = NULL*/; // Useless assignation
  if (!(jasImage = jas_image_decode(jasStream, fmtid, 0))) 
    {
    gdcmErrorMacro("cannot decode image");
    return false;
    }

  // close the stream. 
  jas_stream_close(jasStream);
  int numcmpts = jas_image_numcmpts(jasImage);
  int width = jas_image_cmptwidth(jasImage, 0);
  int height = jas_image_cmptheight(jasImage, 0);
  int prec = jas_image_cmptprec(jasImage, 0);
  int i, j, k;

  // The following should serioulsy be rewritten I cannot believe we need to
  // do a per pixel decompression, there should be a way to read a full
  // scanline...
  if (prec == 8)
    {
    uint8_t *data8 = (uint8_t*)raw;
    for ( i = 0; i < height; i++)
      for ( j = 0; j < width; j++)
        for ( k= 0; k < numcmpts; k++)
          *data8++ = (uint8_t)(jas_image_readcmptsample(jasImage, k, j ,i ));
    }
  else if (prec <= 16)
    {
    uint16_t *data16 = (uint16_t*)raw;
    for ( i = 0; i < height; i++) 
      for ( j = 0; j < width; j++) 
        for ( k= 0; k < numcmpts; k++)
          *data16++ = (uint16_t)(jas_image_readcmptsample(jasImage, k, j ,i ));
    }
  else if (prec <= 32)
    {
    uint32_t *data32 = (uint32_t*)raw;
    for ( i = 0; i < height; i++) 
      for ( j = 0; j < width; j++) 
        for ( k= 0; k < numcmpts; k++)
          *data32++ = (uint32_t)(jas_image_readcmptsample(jasImage, k, j ,i ));
    }

  jas_image_destroy(jasImage);
  jas_image_clearfmts();

  //FIXME
  //delete the jpeg temp buffer
#if 0
  std::ofstream rawout("/tmp/jpeg2000.raw");
  rawout.write((char*)raw,height*width*numcmpts*((prec+4)/8));
  rawout.close();
#endif
  delete[] inputdata;

  return true;
}
#endif

//-----------------------------------------------------------------------------
} // end namespace gdcm

