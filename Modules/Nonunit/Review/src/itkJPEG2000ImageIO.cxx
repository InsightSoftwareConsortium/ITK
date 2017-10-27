/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkJPEG2000ImageIO.h"
#include "itksys/SystemTools.hxx"

// for memset
// for malloc

#define USE_OPJ_DEPRECATED

extern "C"
{
  #include "openjpeg.h"
  #include "j2k.h"
  #include "jp2.h"
}


namespace itk
{

class JPEG2000ImageIOInternal
{
public:

  typedef enum {
    J2K_CFMT = 0,
    JP2_CFMT = 1,
    JPT_CFMT = 2,
    MJ2_CFMT = 3
    } DecodingFormatType;

  typedef enum {
    PXM_DFMT = 0,
    PGX_DFMT = 1,
    BMP_DFMT = 2,
    YUV_DFMT = 3
    } DFMFormatType;

  opj_codec_t *m_Dinfo;

  OPJ_UINT32 m_TileWidth;
  OPJ_UINT32 m_TileHeight;

  OPJ_UINT32 m_TileStartX;
  OPJ_UINT32 m_TileStartY;

  OPJ_UINT32 m_NumberOfTilesInX;
  OPJ_UINT32 m_NumberOfTilesInY;

  opj_dparameters_t m_DecompressionParameters;  /* decompression parameters */
};


JPEG2000ImageIO::JPEG2000ImageIO()
{
  this->m_Internal.TakeOwnership( new JPEG2000ImageIOInternal );

  //   opj_dparameters_t m_DecompressionParameters;
  opj_set_default_decoder_parameters(& this->m_Internal->m_DecompressionParameters);

  this->SetNumberOfDimensions(2); // JPEG2000 is 2D. (by now...)
  this->SetNumberOfComponents(1);

  this->m_Internal->m_Dinfo = ITK_NULLPTR;

  this->m_Internal->m_TileWidth = 0;
  this->m_Internal->m_TileHeight = 0;

  this->m_Internal->m_TileStartX = 0;
  this->m_Internal->m_TileStartY = 0;

  this->m_Internal->m_NumberOfTilesInX = 0;
  this->m_Internal->m_NumberOfTilesInY = 0;

  this->AddSupportedReadExtension(".j2k");
  this->AddSupportedReadExtension(".jp2");
  this->AddSupportedReadExtension(".jpt");

  this->AddSupportedWriteExtension(".j2k");
  this->AddSupportedWriteExtension(".jp2");
  this->AddSupportedWriteExtension(".jpt");
}

JPEG2000ImageIO::~JPEG2000ImageIO()
{
}

void JPEG2000ImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

bool JPEG2000ImageIO::CanReadFile(const char *filename)
{
  itkDebugMacro(<< "JPEG2000ImageIO::CanReadFile()");

  //
  // If the file exists, and have extension .j2k or jp2 or jpt, then we are good
  // to read it.
  //
  if ( !itksys::SystemTools::FileExists(filename) )
    {
    itkDebugMacro(<< "File doesn't exist");
    return false;
    }

  std::string extension = itksys::SystemTools::GetFilenameLastExtension(filename);

  if( extension == ".j2k" ||
      extension == ".jp2" ||
      extension == ".jpt" )
    {
    return true;
    }

  return false;
}

void JPEG2000ImageIO::SetTileSize(int x, int y)
{
  this->m_Internal->m_TileWidth = x;
  this->m_Internal->m_TileHeight = y;
  this->Modified();
}


void JPEG2000ImageIO::ReadImageInformation()
{
  itkDebugMacro(<< "ReadImageInformation()");

  FILE *l_file = fopen(this->m_FileName.c_str(), "rb");

  /* decompression parameters */

  if ( !l_file )
    {
    itkExceptionMacro(
      "JPEG2000ImageIO failed to open file for reading: "
      << this->GetFileName()
      << std::endl
      << "Reason: "
      << itksys::SystemTools::GetLastSystemError() );
    }

  /* set decoding parameters to default values */
  opj_set_default_decoder_parameters(& (this->m_Internal->m_DecompressionParameters) );

  opj_stream_t *cio = opj_stream_create_default_file_stream(l_file, true);

  this->m_Internal->m_Dinfo = ITK_NULLPTR;  /* handle to a decompressor */

  opj_image_t *l_image = ITK_NULLPTR;

  /* decode the code-stream */
  /* ---------------------- */

  std::string extension = itksys::SystemTools::GetFilenameLastExtension(this->m_FileName);

  if ( extension == ".j2k" )
    {
     this->m_Internal->m_DecompressionParameters.decod_format = JPEG2000ImageIOInternal::J2K_CFMT;
    }
  else if ( extension == ".jp2" )
    {
     this->m_Internal->m_DecompressionParameters.decod_format = JPEG2000ImageIOInternal::JP2_CFMT;
    }
  else if ( extension == ".jpt" )
    {
     this->m_Internal->m_DecompressionParameters.decod_format = JPEG2000ImageIOInternal::JPT_CFMT;
    }

  switch (  this->m_Internal->m_DecompressionParameters.decod_format )
    {
    case JPEG2000ImageIOInternal::J2K_CFMT:
      {
      /* JPEG-2000 codestream */

      /* get a decoder handle */
      this->m_Internal->m_Dinfo = opj_create_decompress(CODEC_J2K);
      if (!this->m_Internal->m_Dinfo)
        {
        opj_stream_destroy(cio);
        fclose(l_file);
        itkExceptionMacro(
          "JPEG2000ImageIO failed to read file: "
          << this->GetFileName()
          << std::endl
          << "Reason: opj_create_decompress(CODEC_J2K) returns ITK_NULLPTR" );
        }
      break;
      }
    case JPEG2000ImageIOInternal::JP2_CFMT:
      {
      /* JPEG 2000 compressed image data */
      /* get a decoder handle */
      this->m_Internal->m_Dinfo = opj_create_decompress(CODEC_JP2);
      if (!this->m_Internal->m_Dinfo)
        {
        opj_stream_destroy(cio);
        fclose(l_file);
        itkExceptionMacro(
          "JPEG2000ImageIO failed to read file: "
          << this->GetFileName()
          << std::endl
          << "Reason: opj_create_decompress(CODEC_JP2) returns ITK_NULLPTR" );
        }
      break;
      }
    case JPEG2000ImageIOInternal::JPT_CFMT:
      {
      /* JPEG 2000, JPIP */
      /* get a decoder handle */
      this->m_Internal->m_Dinfo = opj_create_decompress(CODEC_JPT);
      if (!this->m_Internal->m_Dinfo)
        {
        opj_stream_destroy(cio);
        fclose(l_file);
        itkExceptionMacro(
          "JPEG2000ImageIO failed to read file: "
          << this->GetFileName()
          << std::endl
          << "Reason: opj_create_decompress(CODEC_JPT) returns ITK_NULLPTR" );
        }
      break;
      }
    default:
      opj_stream_destroy(cio);
      fclose(l_file);
      itkExceptionMacro(
        "JPEG2000ImageIO failed to read file: "
        << this->GetFileName()
        << std::endl
        << "Reason: "
        << "Unknown decode format: "
        << this->m_Internal->m_DecompressionParameters.decod_format );
    }
  /* catch events using our callbacks and give a local context */
  /* setup the decoder decoding parameters using user parameters */
  /* No reading of image information done */
  bool bResult = opj_setup_decoder(this->m_Internal->m_Dinfo, & (this->m_Internal->m_DecompressionParameters) );
  if ( !bResult )
    {
    opj_stream_destroy(cio);
    itkExceptionMacro(
      "JPEG2000ImageIO failed to read file: "
      << this->GetFileName()
      << std::endl
      << "Reason: opj_setup_decoder returns false");
    }

  // Image parameters - first tile
  OPJ_INT32 l_tile_x0;
  OPJ_INT32 l_tile_y0;

  // Image parameters - tile width, height and number of tiles
  OPJ_UINT32 l_tile_width;
  OPJ_UINT32 l_tile_height;
  OPJ_UINT32 l_nb_tiles_x;
  OPJ_UINT32 l_nb_tiles_y;

  itkDebugMacro(<< "Trying to read header now...");

  bResult = opj_read_header(
    this->m_Internal->m_Dinfo,
    &l_image,
    &l_tile_x0,
    &l_tile_y0,
    &l_tile_width,
    &l_tile_height,
    &l_nb_tiles_x,
    &l_nb_tiles_y,
    cio);

  if ( !bResult )
    {
    opj_stream_destroy(cio);
    itkExceptionMacro(
      "JPEG2000ImageIO failed to read file: "
      << this->GetFileName()
      << std::endl
      << "Reason: opj_read_header returns false");
    }

  if ( !l_image )
    {
    opj_stream_destroy(cio);
    itkExceptionMacro(
      "JPEG2000ImageIO failed to read file: "
      << this->GetFileName()
      << std::endl
      << "Reason: Error whle reading image header");
    }

  this->m_Internal->m_TileStartX = l_tile_x0;
  this->m_Internal->m_TileStartY = l_tile_y0;

  this->m_Internal->m_TileWidth  = l_tile_width;
  this->m_Internal->m_TileHeight = l_tile_height;

  this->m_Internal->m_NumberOfTilesInX = l_nb_tiles_x;
  this->m_Internal->m_NumberOfTilesInY = l_nb_tiles_y;


  itkDebugMacro(<< "Number of Components = " << l_image->numcomps);
  this->SetNumberOfComponents(  l_image->numcomps );

  if ( l_image->comps[0].prec  == 8 )
    {
    this->SetComponentType( UCHAR );
    }
  else if ( l_image->comps[0].prec  == 16 )
    {
    this->SetComponentType( USHORT );
    }
  else
    {
    opj_stream_destroy(cio);
    itkExceptionMacro(
      "JPEG2000ImageIO failed to read file: "
      << this->GetFileName()
      << std::endl
      << "Reason: "
      << "Unknown precision in file: "
      << l_image->comps[0].prec );
    }

  switch (this->GetNumberOfComponents())
    {
    case 1:
      this->SetPixelType( SCALAR );
      break;
    case 3:
      if ( l_image->color_space != CLRSPC_SRGB )
        {
        itkWarningMacro(<< "file does not specify color space, assuming sRGB");
        }
      this->SetPixelType( RGB );
      break;
    default:
      this->SetPixelType( VECTOR );
    }

  itkDebugMacro(<< "bits per pixel = " << l_image->comps[0].prec);
  itkDebugMacro(<< "Color space = " << l_image->color_space);
  itkDebugMacro(<< "Tile Start X = " << this->m_Internal->m_TileStartX);
  itkDebugMacro(<< "Tile Start Y = " << this->m_Internal->m_TileStartY);
  itkDebugMacro(<< "Tile Width = " << this->m_Internal->m_TileWidth);
  itkDebugMacro(<< "Tile Height = " << this->m_Internal->m_TileHeight);
  itkDebugMacro(<< "Number of Tiles X = " << this->m_Internal->m_NumberOfTilesInX);
  itkDebugMacro(<< "Number of Tiles Y = " << this->m_Internal->m_NumberOfTilesInY);

  itkDebugMacro(<< "image->x1 = " <<  l_image->x1);
  itkDebugMacro(<< "image->y1 = " <<  l_image->y1);

  this->SetDimensions(0,  l_image->x1);
  this->SetDimensions(1,  l_image->y1);

  this->SetSpacing(0, 1.0);    // FIXME : Get the real pixel resolution.
  this->SetSpacing(1, 1.0);    // FIXME : Get the real pixel resolution.

  /* close the byte stream */
  opj_stream_destroy(cio);
  fclose(l_file);

  if ( this->m_Internal->m_Dinfo )
    {
    opj_destroy_codec(this->m_Internal->m_Dinfo);
    this->m_Internal->m_Dinfo = ITK_NULLPTR;
    }

  if( l_image )
    {
    opj_image_destroy( l_image );
    }
}

void JPEG2000ImageIO::Read(void *buffer)
{
  itkDebugMacro(<< "JPEG2000ImageIO::Read() Begin");

  FILE *l_file = fopen(this->m_FileName.c_str(), "rb");

  if ( !l_file )
    {
    itkExceptionMacro(
      "JPEG2000ImageIO failed to open file for reading: "
      << this->GetFileName()
      << std::endl
      << "Reason: "
      << itksys::SystemTools::GetLastSystemError() );
    }

  opj_stream_t *l_stream = opj_stream_create_default_file_stream(l_file, true);
  if ( !l_stream )
    {
    itkExceptionMacro(
      "JPEG2000ImageIO failed to read file: "
      << this->GetFileName()
      << std::endl
      << "Reason: opj_stream_create_default_file_stream returns ITK_NULLPTR" );
    }

  this->m_Internal->m_Dinfo  = ITK_NULLPTR;  /* handle to a decompressor */

  opj_image_t *l_image = ITK_NULLPTR;

  /* decode the code-stream */
  /* ---------------------- */
  switch (  this->m_Internal->m_DecompressionParameters.decod_format )
    {
    case JPEG2000ImageIOInternal::J2K_CFMT:
      {
      /* JPEG-2000 codestream */
      /* get a decoder handle */
      this->m_Internal->m_Dinfo = opj_create_decompress(CODEC_J2K);
      if ( !this->m_Internal->m_Dinfo )
        {
        itkExceptionMacro(
          "JPEG2000ImageIO failed to read file: "
          << this->GetFileName()
          << std::endl
          << "Reason: opj_create_decompress(CODEC_J2K) returns ITK_NULLPTR" );
        }
      break;
      }
    case JPEG2000ImageIOInternal::JP2_CFMT:
      {
      /* JPEG 2000 compressed image data */
      /* get a decoder handle */
      this->m_Internal->m_Dinfo = opj_create_decompress(CODEC_JP2);
      if ( !this->m_Internal->m_Dinfo )
        {
        itkExceptionMacro(
          "JPEG2000ImageIO failed to read file: "
          << this->GetFileName()
          << std::endl
          << "Reason: opj_create_decompress(CODEC_JP2) returns ITK_NULLPTR" );
        }
      break;
      }
    case JPEG2000ImageIOInternal::JPT_CFMT:
      {
      /* JPEG 2000, JPIP */
      /* get a decoder handle */
      this->m_Internal->m_Dinfo = opj_create_decompress(CODEC_JPT);
      if ( !this->m_Internal->m_Dinfo )
        {
        itkExceptionMacro(
          "JPEG2000ImageIO failed to read file: "
          << this->GetFileName()
          << std::endl
          << "Reason: opj_create_decompress(CODEC_JPT) returns ITK_NULLPTR" );
        }
      break;
      }
    default:
      fclose(l_file);
      itkExceptionMacro(
        "JPEG2000ImageIO failed to read file: "
        << this->GetFileName()
        << std::endl
        << "Reason: "
        << "Unknown decode format: "
        << this->m_Internal->m_DecompressionParameters.decod_format );
}
  /* catch events using our callbacks and give a local context */

  /* setup the decoder decoding parameters using user parameters */
  if (!opj_setup_decoder(this->m_Internal->m_Dinfo, &( this->m_Internal->m_DecompressionParameters ) ))
    {
    itkExceptionMacro(
      "JPEG2000ImageIO failed to read file: "
      << this->GetFileName()
      << std::endl
      << "Reason: opj_setup_decoder returns false" );
    }

  OPJ_INT32 l_tile_x0, l_tile_y0;

  OPJ_UINT32 l_tile_width;
  OPJ_UINT32 l_tile_height;
  OPJ_UINT32 l_nb_tiles_x;
  OPJ_UINT32 l_nb_tiles_y;

  bool bResult = opj_read_header(
    this->m_Internal->m_Dinfo,
    &l_image,
    &l_tile_x0,
    &l_tile_y0,
    &l_tile_width,
    &l_tile_height,
    &l_nb_tiles_x,
    &l_nb_tiles_y,
    l_stream);

  if ( !bResult )
    {
    opj_destroy_codec(this->m_Internal->m_Dinfo);
    this->m_Internal->m_Dinfo = ITK_NULLPTR;
    opj_stream_destroy(l_stream);
    fclose(l_file);
    itkExceptionMacro(
      "JPEG2000ImageIO failed to read file: "
      << this->GetFileName()
      << std::endl
      << "Reason: opj_read_header returns false");
    }

 ImageIORegion regionToRead = this->GetIORegion();

  ImageIORegion::SizeType  size  = regionToRead.GetSize();
  ImageIORegion::IndexType start = regionToRead.GetIndex();

  const unsigned int sizex = size[0];
  const unsigned int sizey = size[1];
  // const unsigned int sizez = size[2];

  const unsigned int startx = start[0];
  const unsigned int starty = start[1];
  // const unsigned int startz = start[2];

  OPJ_INT32 p_start_x = static_cast< OPJ_INT32 >( startx );
  OPJ_INT32 p_start_y = static_cast< OPJ_INT32 >( starty );
  OPJ_INT32 p_end_x   = static_cast< OPJ_INT32 >( startx + sizex );
  OPJ_INT32 p_end_y   = static_cast< OPJ_INT32 >( starty + sizey );

  itkDebugMacro(<< "opj_set_decode_area() before");
  itkDebugMacro(<< "p_start_x = " << p_start_x);
  itkDebugMacro(<< "p_start_y = " << p_start_y);
  itkDebugMacro(<< "p_end_x = " << p_end_x);
  itkDebugMacro(<< "p_end_y = " << p_end_y);

  bResult = opj_set_decode_area(
    this->m_Internal->m_Dinfo,
    p_start_x,
    p_start_y,
    p_end_x,
    p_end_y
    );

  itkDebugMacro(<< "opj_set_decode_area() after");

  if ( !bResult )
    {
    opj_destroy_codec(this->m_Internal->m_Dinfo);
    this->m_Internal->m_Dinfo = ITK_NULLPTR;
    opj_stream_destroy(l_stream);
    fclose(l_file);
    itkExceptionMacro(
      "JPEG2000ImageIO failed to read file: "
      << this->GetFileName()
      << std::endl
      << "Reason: opj_set_decode_area returns false");
    }

  OPJ_INT32 l_current_tile_x0;
  OPJ_INT32 l_current_tile_y0;
  OPJ_INT32 l_current_tile_x1;
  OPJ_INT32 l_current_tile_y1;

  OPJ_UINT32 l_tile_index;
  OPJ_UINT32 l_data_size;

  OPJ_UINT32 l_nb_comps;

  OPJ_UINT32 l_max_data_size = 1000;

  bool l_go_on = true;

  OPJ_BYTE *l_data = (OPJ_BYTE *)malloc(1000);

  while ( l_go_on )
    {
    bool tileHeaderRead = opj_read_tile_header(
      this->m_Internal->m_Dinfo,
      &l_tile_index,
      &l_data_size,
      &l_current_tile_x0,
      &l_current_tile_y0,
      &l_current_tile_x1,
      &l_current_tile_y1,
      &l_nb_comps,
      &l_go_on,
      l_stream);

    if ( !tileHeaderRead )
      {
      free(l_data);
      opj_stream_destroy(l_stream);
      fclose(l_file);
      opj_destroy_codec(this->m_Internal->m_Dinfo);
      opj_image_destroy(l_image);
      itkExceptionMacro(
        "JPEG2000ImageIO failed to read file: "
        << this->GetFileName()
        << std::endl
        << "Reason: opj_read_tile_header returns false");
      }

    itkDebugMacro(<< "l_tile_index " << l_tile_index);
    itkDebugMacro(<< "l_data_size " << l_data_size);
    itkDebugMacro(<< "l_current_tile_x0 " << l_current_tile_x0);
    itkDebugMacro(<< "l_current_tile_y0 " << l_current_tile_y0);
    itkDebugMacro(<< "l_current_tile_x1 " << l_current_tile_x1);
    itkDebugMacro(<< "l_current_tile_y1 " << l_current_tile_y1);
    itkDebugMacro(<< "l_nb_comps " << l_nb_comps);
    itkDebugMacro(<< "l_go_on " << l_go_on);

    if ( l_go_on )
      {
      if ( l_data_size > l_max_data_size )
        {
        l_data = (OPJ_BYTE *)realloc(l_data, l_data_size);

        if ( !l_data )
          {
          opj_stream_destroy(l_stream);
          fclose(l_file);
          opj_destroy_codec(this->m_Internal->m_Dinfo);
          opj_image_destroy(l_image);
          itkExceptionMacro(
            "JPEG2000ImageIO failed to read file: "
            << this->GetFileName()
            << std::endl
            << "Reason: Error reallocating memory");
          }

        itkDebugMacro(<< "reallocated for " << l_data_size);

        l_max_data_size = l_data_size;
        }

      bool decodeTileData = opj_decode_tile_data(
        this->m_Internal->m_Dinfo,
        l_tile_index,
        l_data,
        l_data_size,
        l_stream);

      if ( !decodeTileData )
        {
        free(l_data);
        opj_stream_destroy(l_stream);
        fclose(l_file);
        opj_destroy_codec(this->m_Internal->m_Dinfo);
        opj_image_destroy(l_image);
        itkExceptionMacro(
          "JPEG2000ImageIO failed to read file: "
          << this->GetFileName()
          << std::endl
          << "Reason: opj_decode_tile_data returns false");
        }

      OPJ_BYTE *l_data_ptr = l_data;

      const SizeValueType tsizex = l_current_tile_x1 - l_current_tile_x0;
      const SizeValueType tsizey = l_current_tile_y1 - l_current_tile_y0;
      const SizeValueType numberOfPixels = tsizex * tsizey;
      const SizeValueType numberOfComponents = this->GetNumberOfComponents();
      const SizeValueType sizePerComponentInBytes = l_data_size / ( numberOfPixels * numberOfComponents );
      const SizeValueType sizePerChannelInBytes = l_data_size / ( numberOfComponents );

      itkDebugMacro(<< "sizePerComponentInBytes: " << sizePerComponentInBytes);
      itkDebugMacro(<< "sizePerChannelInBytes:   " << sizePerChannelInBytes);

      const SizeValueType sizePerStrideXInBytes = sizePerChannelInBytes / tsizey;
      const SizeValueType initialStrideInBytes =
        ( l_current_tile_y0 - p_start_y ) * sizex * sizePerComponentInBytes * numberOfComponents;
      const SizeValueType priorStrideInBytes =
        ( l_current_tile_x0 - p_start_x ) * sizePerComponentInBytes * numberOfComponents;
      const SizeValueType postStrideInBytes =
        ( p_end_x - l_current_tile_x1 ) * sizePerComponentInBytes * numberOfComponents;

      itkDebugMacro(<< "sizePerStrideYInBytes:   " << sizePerChannelInBytes / tsizex );
      itkDebugMacro(<< "sizePerStrideXInBytes:   " << sizePerStrideXInBytes);
      itkDebugMacro(<< "initialStrideInBytes:    " << initialStrideInBytes);
      itkDebugMacro(<< "priorStrideInBytes:      " << priorStrideInBytes);
      itkDebugMacro(<< "postStrideInBytes:       " << postStrideInBytes);


      //TODO: Read the void buffer within the tile ROI. How do we specify the
      // tile ROI iteration
      for ( unsigned int k = 0; k < numberOfComponents; k++ )
        {
        unsigned char *charBuffer = (unsigned char *)buffer;
        charBuffer += k * sizePerComponentInBytes;

        charBuffer += initialStrideInBytes;

        for ( SizeValueType m = 0; m < tsizey; m++ )
          {
          charBuffer += priorStrideInBytes;
          for ( SizeValueType j = 0; j < sizePerStrideXInBytes; j++ )
            {
            *charBuffer = (unsigned char)( *l_data_ptr++ );
            charBuffer += numberOfComponents;
            }
          charBuffer += postStrideInBytes;
          }
        }
      }
//       l_go_on = 0;
    }

//  l_image = opj_decode( this->m_Dinfo, l_stream );

  if ( !opj_end_decompress(this->m_Internal->m_Dinfo, l_stream) )
    {
    free(l_data);
    opj_stream_destroy(l_stream);
    fclose(l_file);
    opj_destroy_codec(this->m_Internal->m_Dinfo);
    opj_image_destroy(l_image);
    itkExceptionMacro(
      "JPEG2000ImageIO failed to read file: "
      << this->GetFileName()
      << std::endl
      << "Reason: opj_end_decompress returns false");
    }

  if ( !l_image )
    {
    opj_destroy_codec(this->m_Internal->m_Dinfo);
    this->m_Internal->m_Dinfo = ITK_NULLPTR;
    opj_stream_destroy(l_stream);
    fclose(l_file);
    itkExceptionMacro(
      "JPEG2000ImageIO failed to read file: "
      << this->GetFileName()
      << std::endl
      << "Reason: error whle reading image");
    }

  /* close the byte stream */
  opj_stream_destroy(l_stream);
  fclose(l_file);

  if ( this->m_Internal->m_Dinfo )
    {
    opj_destroy_codec(this->m_Internal->m_Dinfo);
    this->m_Internal->m_Dinfo = ITK_NULLPTR;
    }

  if( l_image )
    {
    opj_image_destroy( l_image );
    }

  if( l_data )
    {
    free( l_data );
    }

  itkDebugMacro(<< "JPEG2000ImageIO::Read() End");
}

bool JPEG2000ImageIO::CanWriteFile(const char *filename)
{
  std::string extension = itksys::SystemTools::GetFilenameLastExtension(filename);

  if( extension == ".j2k" ||
      extension == ".jp2" ||
      extension == ".jpt" )
    {
    return true;
    }

  return false;
}

void
JPEG2000ImageIO
::WriteImageInformation(void)
{
  itkDebugMacro(<< "WriteImageInformation()");

  // the IORegion is not required to be set so we must use GetNumberOfDimensions
  if ( this->GetNumberOfDimensions() != 2 )
    {
    itkExceptionMacro(
      "JPEG2000ImageIO failed to write file: "
      << this->GetFileName()
      << std::endl
      << "Reason: "
      << "JPEG 2000 writer can only write 2-dimensional images" );
    }

  if ( this->GetComponentType() != UCHAR
       && this->GetComponentType() != USHORT )
    {
    itkExceptionMacro(
      "JPEG2000ImageIO failed to write file: "
      << this->GetFileName()
      << std::endl
      << "Reason: "
      << "JPEG 2000 writer only supports unsigned char/unsigned short int" );
    }

  if ( this->GetNumberOfComponents() != 1
       && this->GetNumberOfComponents() != 3 )
    {
    itkExceptionMacro(
      "JPEG2000ImageIO failed to write file: "
      << this->GetFileName()
      << std::endl
      << "Reason: "
      << "JPEG 2000 writer only supports supports 1 or 3 components" );
    }
}

/**
 *
 */
void
JPEG2000ImageIO
::Write(const void *buffer)
{
  itkDebugMacro(<< "Write() " <<  this->GetNumberOfComponents() );

  bool bSuccess;

  opj_cparameters_t parameters;
  opj_set_default_encoder_parameters(&parameters);

  std::string extension = itksys::SystemTools::GetFilenameLastExtension( this->m_FileName.c_str() );
  if ( extension == ".j2k" )
    {
    parameters.cod_format = JPEG2000ImageIOInternal::J2K_CFMT;
    }
  else if ( extension == ".jp2" )
    {
    parameters.cod_format = JPEG2000ImageIOInternal::JP2_CFMT;
    }

  strncpy(parameters.outfile, this->m_FileName.c_str(), sizeof( parameters.outfile ) - 1);

  /* if no rate entered, lossless by default */
  if ( parameters.tcp_numlayers == 0 )
    {
    parameters.tcp_rates[0] = 0; /* MOD antonin : losslessbug */
    parameters.tcp_numlayers++;
    parameters.cp_disto_alloc = 1;
    }

  if ( ( parameters.cp_tx0 > parameters.image_offset_x0 ) || ( parameters.cp_ty0 > parameters.image_offset_y0 ) )
    {
    itkExceptionMacro(
      "JPEG2000ImageIO failed to write file: "
      << this->GetFileName()
      << std::endl
      << "Reason: "
      << "Error: Tile offset dimension is unnappropriate -->"
      << "  TX0(" << parameters.cp_tx0 << ") <= IMG_X0( " << parameters.image_offset_x0
      << ") TYO(" << parameters.cp_ty0 << ") <= IMG_Y0( " << parameters.image_offset_y0 << ") " );
    }

  for ( int i = 0; i < parameters.numpocs; i++ )
    {
    if ( parameters.POC[i].prg == -1 )
      {
      // TODO should this be and excpetion? can we continue loading
      // and expect good results
      std::cerr << "Unrecognized progression order in option -P (POC n " << i+1;
      std::cerr << ") [LRCP, RLCP, RPCL, PCRL, CPRL] !!"  << std::endl;
      }
    }

  /* Create comment for codestream */
  if ( parameters.cp_comment == ITK_NULLPTR )
    {
    const char   comment[] = "Created by OpenJPEG version ";
    const SizeValueType clen = strlen(comment);
    const char * version = opj_version();

    /* UniPG>> */
#ifdef USE_JPWL
    parameters.cp_comment = (char *)malloc(clen + strlen(version) + 11);
    sprintf(parameters.cp_comment, "%s%s with JPWL", comment, version);
#else
    parameters.cp_comment = (char *)malloc(clen + strlen(version) + 1);
    sprintf(parameters.cp_comment, "%s%s", comment, version);
#endif
    /* <<UniPG */
    }

  if ( this->m_Internal->m_TileWidth > 0 )
    {
    parameters.cp_tdx = this->m_Internal->m_TileWidth;
    parameters.cp_tdy = this->m_Internal->m_TileHeight;
    parameters.tile_size_on = true;
    }

//--------------------------------------------------------
// Copy the contents into the image structure
  int w, h;
  w = this->m_Dimensions[0];
  h = this->m_Dimensions[1];


  // Compute the proper number of resolutions to use.
  // This is mostly done for images smaller than 64 pixels
  // along any dimension.
  unsigned int numberOfResolutions = 0;

  int tw = w >> 1;
  int th = h >> 1;

  while( tw && th )
    {
    numberOfResolutions++;
    tw >>= 1;
    th >>= 1;
    }

  // Clamp the number of resolutions to 6.
  if( numberOfResolutions > 6 )
    {
    numberOfResolutions = 6;
    }

  parameters.numresolution = numberOfResolutions;

  OPJ_COLOR_SPACE      color_space = CLRSPC_GRAY;
  opj_image_cmptparm_t cmptparm[3];

  if ( this->GetNumberOfComponents() == 3 )
    {

    color_space = ( this->GetPixelType() == RGB ) ? CLRSPC_SRGB : CLRSPC_UNSPECIFIED;

    /* initialize image components */
    memset( &cmptparm[0], 0, 3 * sizeof( opj_image_cmptparm_t ) );
    for ( unsigned int i = 0; i < 3; i++ )
      {
      cmptparm[i].prec = 8;
      cmptparm[i].bpp = 8;
      cmptparm[i].sgnd = 0;
      cmptparm[i].dx = 1;
      cmptparm[i].dy = 1; //this->GetSpacing( 1 )
      cmptparm[i].w =  w;
      cmptparm[i].h = h;
      }
    }

  if ( this->GetNumberOfComponents() == 1 )
    {
    color_space = CLRSPC_GRAY;

    /* initialize image components */
    memset( &cmptparm[0], 0, sizeof( opj_image_cmptparm_t ) );

    if ( this->GetComponentType() == UCHAR )
      {
      cmptparm[0].prec = 8;
      cmptparm[0].bpp = 8;
      }

    if ( this->GetComponentType() == USHORT )
      {
      cmptparm[0].prec = 16;
      cmptparm[0].bpp = 16;
      }

    cmptparm[0].sgnd = 0;
    cmptparm[0].dx = 1;
    cmptparm[0].dy = 1; //this->GetSpacing( 1 )
    cmptparm[0].w =  w;
    cmptparm[0].h = h;
    }

  opj_image_t *l_image = opj_image_create(this->GetNumberOfComponents(), &cmptparm[0], color_space);

  if ( !l_image )
    {
    itkExceptionMacro(
      "JPEG2000ImageIO failed to write file: "
      << this->GetFileName()
      << std::endl
      << "Reason: opj_image_create returns false");
    }

  l_image->numcomps = this->GetNumberOfComponents();

  int subsampling_dx = parameters.subsampling_dx;
  int subsampling_dy = parameters.subsampling_dy;
  l_image->x0 = parameters.image_offset_x0;
  l_image->y0 = parameters.image_offset_y0;
  l_image->x1 = !l_image->x0 ? ( w - 1 ) * subsampling_dx + 1 : l_image->x0 + ( w - 1 ) * subsampling_dx + 1;
  l_image->y1 = !l_image->y0 ? ( h - 1 ) * subsampling_dy + 1 : l_image->y0 + ( h - 1 ) * subsampling_dy + 1;

  // HERE, copy the buffer
  SizeValueType index = 0;
  SizeValueType numberOfPixels = SizeValueType(w) * SizeValueType(h);
  itkDebugMacro(<< " START COPY BUFFER");
  if ( this->GetComponentType() == UCHAR )
    {
    const unsigned char *charBuffer = (const unsigned char *)buffer;
    for ( SizeValueType j = 0; j < numberOfPixels; j++ )
      {
      for ( unsigned int k = 0; k < this->GetNumberOfComponents(); k++ )
        {
        l_image->comps[k].data[index] = *charBuffer++;
        }
      index++;
      }
    }

  if ( this->GetComponentType() == USHORT )
    {
    const unsigned short *shortBuffer = (const unsigned short *)buffer;
    for ( SizeValueType j = 0; j < numberOfPixels; j++ )
      {
      for ( unsigned int k = 0; k < this->GetNumberOfComponents(); k++ )
        {
        l_image->comps[k].data[index] = *shortBuffer++;
        }
      index++;
      }
    }
  itkDebugMacro(<< " END COPY BUFFER");
//--------------------------------------------------------------------

  opj_codec_t *cinfo = ITK_NULLPTR;
  if ( extension == ".j2k" )
    {
    cinfo = opj_create_compress(CODEC_J2K);
    if ( !cinfo )
      {
      itkExceptionMacro(
        "JPEG2000ImageIO failed to write file: "
        << this->GetFileName()
        << std::endl
        << "Reason: opj_create_compress(CODEC_J2K) returns ITK_NULLPTR" );
      }
    }
  else if ( extension == ".jp2" )
    {
    cinfo = opj_create_compress(CODEC_JP2);
    if ( !cinfo )
      {
      itkExceptionMacro(
        "JPEG2000ImageIO failed to write file: "
        << this->GetFileName()
        << std::endl
        << "Reason: opj_create_compress(CODEC_JP2) returns ITK_NULLPTR" );
      }
    }
  else if ( extension == ".jpt" )
    {
    cinfo = opj_create_compress(CODEC_JPT);
    if ( !cinfo )
      {
      itkExceptionMacro(
        "JPEG2000ImageIO failed to write file: "
        << this->GetFileName()
        << std::endl
        << "Reason: opj_create_compress(CODEC_JPT) returns ITK_NULLPTR" );
      }
    }
  else
    {
    itkExceptionMacro(
      "JPEG2000ImageIO failed to write file: "
      << this->GetFileName()
      << std::endl
      << "Reason: unknown encode format: "
      << extension );
    }

  if ( this->GetNumberOfComponents() == 3 )
    {
    parameters.tcp_mct = 1;
    }
  else
    {
    parameters.tcp_mct = 0;
    }

  if (!opj_setup_encoder(cinfo, &parameters, l_image))
    {
    itkExceptionMacro(
      "JPEG2000ImageIO failed to write file: "
      << this->GetFileName()
      << std::endl
      << "Reason: opj_setup_encoder returns false" );
    }

  FILE *l_file = fopen(parameters.outfile, "wb");
  if ( !l_file )
    {
    itkExceptionMacro(
      "JPEG2000ImageIO failed to open file for writing: "
      << this->GetFileName()
      << std::endl
      << "Reason: "
      << itksys::SystemTools::GetLastSystemError() );
    }

  /* open a byte stream for writing */
  /* allocate memory for all tiles */
  opj_stream_t *cio = opj_stream_create_default_file_stream(l_file, false);
  if ( !cio )
    {
    itkExceptionMacro(
      "JPEG2000ImageIO failed to write file: "
      << this->GetFileName()
      << std::endl
      << "Reason: "
      << "opj_stream_create_default_file_stream returns false" );
    }

  if( parameters.cp_comment )
    {
    free( parameters.cp_comment );
    }

  bSuccess = opj_start_compress(cinfo, l_image, cio);
  if ( !bSuccess )
    {
    opj_stream_destroy(cio);
    fclose(l_file);
    itkExceptionMacro(
      "JPEG2000ImageIO failed to write file: "
      << this->GetFileName()
      << std::endl
      << "Reason: opj_start_compress returns false" );
    }

  bSuccess = bSuccess && opj_encode(cinfo, cio);
  if ( !bSuccess )
    {
    opj_stream_destroy(cio);
    fclose(l_file);
    itkExceptionMacro(
      "JPEG2000ImageIO failed to write file: "
      << this->GetFileName()
      << std::endl
      << "Reason: opj_encode returns false" );
    }

  bSuccess = bSuccess && opj_end_compress(cinfo, cio);
  if ( !bSuccess )
    {
    opj_stream_destroy(cio);
    fclose(l_file);
    itkExceptionMacro(
      "JPEG2000ImageIO failed to write file: "
      << this->GetFileName()
      << std::endl
      << "Reason: opj_end_compress returns false" );
    }

  /* close and free the byte stream */
  opj_stream_destroy(cio);
  fclose(l_file);

  /* free remaining compression structures */
  opj_destroy_codec(cinfo);

  /* free image data */
  opj_image_destroy(l_image);
}

JPEG2000ImageIO::SizeType
JPEG2000ImageIO::GetHeaderSize(void) const
{
  return 0;
}

/** Given a requested region, determine what could be the region that we can
 * read from the file. This is called the streamable region, which will be
 * smaller than the LargestPossibleRegion and greater or equal to the
RequestedRegion */
ImageIORegion
JPEG2000ImageIO
::GenerateStreamableReadRegionFromRequestedRegion(const ImageIORegion & requestedRegion) const
{
  itkDebugMacro(<< "JPEG2000ImageIO::GenerateStreamableReadRegionFromRequestedRegion()");
  itkDebugMacro(<< "Requested region = " << requestedRegion );

  ImageIORegion streamableRegion(this->m_NumberOfDimensions);

  if ( !m_UseStreamedReading )
    {
    return ImageIOBase::GenerateStreamableReadRegionFromRequestedRegion( requestedRegion );
    }
  else
    {
    // Compute the required set of tiles that fully contain the requested region
    streamableRegion = requestedRegion;

    this->ComputeRegionInTileBoundaries(0, this->m_Internal->m_TileWidth, streamableRegion);
    this->ComputeRegionInTileBoundaries(1, this->m_Internal->m_TileHeight, streamableRegion);
    }

  itkDebugMacro(<< "Streamable region = " << streamableRegion );

  return streamableRegion;
}

void
JPEG2000ImageIO
::ComputeRegionInTileBoundaries(unsigned int dimension,
                                SizeValueType tileSize, ImageIORegion & streamableRegion) const
{
  SizeValueType  requestedSize = streamableRegion.GetSize(dimension);
  IndexValueType requestedIndex = streamableRegion.GetIndex(dimension);

  IndexValueType startQuantizedInTileSize = requestedIndex - ( requestedIndex % tileSize );
  IndexValueType requestedEnd = requestedIndex + requestedSize;
  SizeValueType  extendedSize = requestedEnd - startQuantizedInTileSize;
  SizeValueType  tileRemanent = extendedSize % tileSize;

  SizeValueType sizeQuantizedInTileSize = extendedSize;

  if ( tileRemanent )
    {
    sizeQuantizedInTileSize += tileSize - tileRemanent;
    }

  IndexValueType endQuantizedInTileSize = startQuantizedInTileSize + sizeQuantizedInTileSize - 1;

  if ( endQuantizedInTileSize > static_cast<int>(this->GetDimensions(dimension)) )
    {
    sizeQuantizedInTileSize = this->GetDimensions(dimension) - startQuantizedInTileSize;
    }

  streamableRegion.SetSize(dimension, sizeQuantizedInTileSize);
  streamableRegion.SetIndex(dimension, startQuantizedInTileSize);
}

bool
JPEG2000ImageIO
::CanStreamWrite( void )
{
  // we currently can't stream write for now...
  return false;
}


} // end namespace itk
