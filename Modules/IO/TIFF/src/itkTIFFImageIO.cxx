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

#include "itkTIFFImageIO.h"
#include "itkTIFFReaderInternal.h"
#include "itksys/SystemTools.hxx"

#include "itk_tiff.h"

namespace itk
{

bool TIFFImageIO::CanReadFile(const char *file)
{
  // First check the extension
  std::string filename = file;

  if (  filename == "" )
    {
    itkDebugMacro(<< "No filename specified.");
    return false;
    }

  // Now check if this is a valid TIFF image
  TIFFErrorHandler save = TIFFSetErrorHandler(ITK_NULLPTR);
  int              res = m_InternalImage->Open(file);
  if ( res )
    {
    TIFFSetErrorHandler(save);
    return true;
    }
  m_InternalImage->Clean();
  TIFFSetErrorHandler(save);
  return false;
}

void TIFFImageIO::ReadGenericImage(void *out,
                                   unsigned int width,
                                   unsigned int height)
{

  if ( m_ComponentType == UCHAR )
    {
    this->ReadGenericImage<unsigned char>(out, width, height);
    }
  else if ( m_ComponentType == CHAR )
    {
    this->ReadGenericImage<char>(out, width, height);
    }
  else if ( m_ComponentType == USHORT )
    {
    this->ReadGenericImage<unsigned short>(out, width, height);
    }
  else if ( m_ComponentType == SHORT )
    {
    this->ReadGenericImage<short>(out, width, height);
    }
  else if ( m_ComponentType == FLOAT )
    {
    this->ReadGenericImage<float>(out, width, height);
    }
}

int TIFFImageIO::EvaluateImageAt(void *out, void *in)
{
  unsigned char *image = (unsigned char *)out;
  unsigned char *source = (unsigned char *)in;

  int            increment;
  unsigned short red, green, blue, alpha;

  switch ( this->GetFormat() )
    {
    case TIFFImageIO::GRAYSCALE:
      if ( m_InternalImage->m_Photometrics ==
           PHOTOMETRIC_MINISBLACK )
        {
        if ( m_ComponentType == USHORT )
          {
          unsigned short *image_us = (unsigned short *)out;
          unsigned short *source_us = (unsigned short *)in;
          *image_us = *source_us;
          }
        else if ( m_ComponentType == SHORT )
          {
          short *image_us = (short *)out;
          short *source_us = (short *)in;
          *image_us = *source_us;
          }
        else if ( m_ComponentType == CHAR )
          {
          char *image_us = (char *)out;
          char *source_us = (char *)in;
          *image_us = *source_us;
          }
        else if ( m_ComponentType == FLOAT )
          {
          float *image_us = (float *)out;
          float *source_us = (float *)in;
          *image_us = *source_us;
          }
        else
          {
          *image = *source;
          }
        }
      else
        {
        *image = ~( *source );
        }
      increment = 1;
      break;
    case TIFFImageIO::PALETTE_GRAYSCALE:
      this->GetColor(*source, &red, &green, &blue);
      *image = static_cast< unsigned char >( red >> 8 );
      increment = 1;
      break;
    case TIFFImageIO::RGB_:
      if ( m_ComponentType == USHORT )
        {
        unsigned short *image_us = (unsigned short *)out;
        unsigned short *source_us = (unsigned short *)in;

        red   = *( source_us );
        green = *( source_us + 1 );
        blue  = *( source_us + 2 );
        *( image_us )   = red;
        *( image_us + 1 ) = green;
        *( image_us + 2 ) = blue;
        if ( m_InternalImage->m_SamplesPerPixel == 4 )
          {
          alpha = *( source_us + 3 );
          *( image_us + 3 ) = alpha;
          }
        }
      else
        {
        red   = *( source );
        green = *( source + 1 );
        blue  = *( source + 2 );
        *( image )   = red;
        *( image + 1 ) = green;
        *( image + 2 ) = blue;
        if ( m_InternalImage->m_SamplesPerPixel == 4 )
          {
          alpha = *( source + 3 );
          *( image + 3 ) = alpha;
          }
        }
      increment = m_InternalImage->m_SamplesPerPixel;
      break;
    case TIFFImageIO::PALETTE_RGB:
      if ( m_ComponentType == USHORT )
        {
        unsigned short *image_us = (unsigned short *)out;
        unsigned short *source_us = (unsigned short *)in;
        this->GetColor(*source_us, &red, &green, &blue);
        *( image_us )   = red << 8;
        *( image_us + 1 ) = green << 8;
        *( image_us + 2 ) = blue << 8;
        }
      else if ( m_ComponentType == SHORT )
        {
        short *image_us = (short *)out;
        short *source_us = (short *)in;
        this->GetColor(*source_us, &red, &green, &blue);
        *( image_us )   = red << static_cast<short int>(8);
        *( image_us + 1 ) = green << static_cast<short int>(8);
        *( image_us + 2 ) = blue << static_cast<short int>(8);
        }
      else if ( m_ComponentType == CHAR )
        {
        this->GetColor(*source, &red, &green, &blue);
        *( image )   = static_cast< char >( red >> 8 );
        *( image + 1 ) = static_cast< char >( green >> 8 );
        *( image + 2 ) = static_cast< char >( blue >> 8 );
        }
      else
        {
        this->GetColor(*source, &red, &green, &blue);
        *( image )   = static_cast< unsigned char >( red >> 8 );
        *( image + 1 ) = static_cast< unsigned char >( green >> 8 );
        *( image + 2 ) = static_cast< unsigned char >( blue >> 8 );
        }
      increment = 3;
      break;
    default:
      return 0;
    }

  return increment;
}

void TIFFImageIO::GetColor(int index, unsigned short *red,
                           unsigned short *green, unsigned short *blue)
{
  *red   = 0;
  *green = 0;
  *blue  = 0;
  if ( index < 0 )
    {
    itkExceptionMacro(<< "Color index has to be greater than 0");
    }
  if ( m_TotalColors > 0
       && m_ColorRed && m_ColorGreen && m_ColorBlue )
    {
    if ( index >= m_TotalColors )
      {
      itkExceptionMacro(<< "Color index has to be less than number of colors ("
                        << m_TotalColors << ")");
      }
    *red   = *( m_ColorRed   + index );
    *green = *( m_ColorGreen + index );
    *blue  = *( m_ColorBlue  + index );
    return;
    }

  unsigned short photometric;

  if ( !TIFFGetField(m_InternalImage->m_Image, TIFFTAG_PHOTOMETRIC, &photometric) )
    {
    if ( m_InternalImage->m_Photometrics != PHOTOMETRIC_PALETTE )
      {
      itkExceptionMacro(<< "You can only access colors for palette images");
      }
    }

  unsigned short *red_orig, *green_orig, *blue_orig;

  switch ( m_InternalImage->m_BitsPerSample )
    {
    case 1:
    case 2:
    case 4:
    case 8:
    case 16:
      break;
    default:
      itkExceptionMacro(<<  "Sorry, can not handle image with "
                        << m_InternalImage->m_BitsPerSample
                        << "-bit samples");
    }
  if ( !TIFFGetField(m_InternalImage->m_Image, TIFFTAG_COLORMAP,
                     &red_orig, &green_orig, &blue_orig) )
    {
    itkExceptionMacro(<< "Missing required \"Colormap\" tag");
    }
  m_TotalColors = ( 1L << m_InternalImage->m_BitsPerSample );

  if ( index >= m_TotalColors )
    {
    itkExceptionMacro(<< "Color index has to be less than number of colors ("
                      << m_TotalColors << ")");
    }
  m_ColorRed   =   red_orig;
  m_ColorGreen = green_orig;
  m_ColorBlue  =  blue_orig;

  *red   = *( red_orig   + index );
  *green = *( green_orig + index );
  *blue  = *( blue_orig  + index );
}

unsigned int TIFFImageIO::GetFormat()
{
  unsigned int cc;

  if ( m_ImageFormat != TIFFImageIO::NOFORMAT )
    {
    return m_ImageFormat;
    }

  switch ( m_InternalImage->m_Photometrics )
    {
    case PHOTOMETRIC_RGB:
    case PHOTOMETRIC_YCBCR:
      m_ImageFormat = TIFFImageIO::RGB_;
      return m_ImageFormat;
    case PHOTOMETRIC_MINISWHITE:
    case PHOTOMETRIC_MINISBLACK:
      m_ImageFormat = TIFFImageIO::GRAYSCALE;
      return m_ImageFormat;
    case PHOTOMETRIC_PALETTE:
      for ( cc = 0; cc < 256; cc++ )
        {
        unsigned short red, green, blue;
        this->GetColor(cc, &red, &green, &blue);
        if ( red != green || red != blue )
          {
          m_ImageFormat = TIFFImageIO::PALETTE_RGB;
          return m_ImageFormat;
          }
        }
      m_ImageFormat = TIFFImageIO::PALETTE_GRAYSCALE;
      return m_ImageFormat;
    }
  m_ImageFormat = TIFFImageIO::OTHER;
  return m_ImageFormat;
}

/** Read a tiled tiff */
void TIFFImageIO::ReadTiles(void *buffer)
{
  unsigned char *volume = reinterpret_cast< unsigned char * >( buffer );

  for ( unsigned int col = 0; col < m_InternalImage->m_Width; col += m_InternalImage->m_TileWidth )
    {
    for ( unsigned int row = 0; row < m_InternalImage->m_Height; row += m_InternalImage->m_TileHeight )
      {
      const size_t sz = static_cast<size_t>(m_InternalImage->m_TileWidth)
        * static_cast<size_t>(m_InternalImage->m_TileHeight)
        * static_cast<size_t>(m_InternalImage->m_SamplesPerPixel);
      unsigned char *tempImage = new unsigned char[sz];

      if ( TIFFReadTile(m_InternalImage->m_Image, tempImage, col, row, 0, 0) < 0 )
        {
        itkExceptionMacro(<< "Cannot read tile : " << row << "," << col << " from file");
        }

      unsigned int xx, yy;
      for ( yy = 0; yy < m_InternalImage->m_TileHeight; yy++ )
        {
        for ( xx = 0; xx <  m_InternalImage->m_TileWidth; xx++ )
          {
          for ( unsigned int i = 0; i < m_InternalImage->m_SamplesPerPixel; i++ )
            {
            *volume = *( tempImage++ );
            volume++;
            }
          }
        }
      delete tempImage;
      }
    }
}

/** Read a multipage tiff */
void TIFFImageIO::ReadVolume(void *buffer)
{
  const int width  = m_InternalImage->m_Width;
  const int height = m_InternalImage->m_Height;

  for ( unsigned int page = 0; page < m_InternalImage->m_NumberOfPages; page++ )
    {
    if ( m_InternalImage->m_IgnoredSubFiles > 0 )
      {
      int32 subfiletype = 6;
      if ( TIFFGetField(m_InternalImage->m_Image, TIFFTAG_SUBFILETYPE, &subfiletype) )
        {
        if ( subfiletype & FILETYPE_REDUCEDIMAGE
             || subfiletype & FILETYPE_MASK )
          {
          // skip subfile
          TIFFReadDirectory(m_InternalImage->m_Image);
          continue;
          }
        }
      }


    const size_t pixelOffset = static_cast<size_t>(width)
      * static_cast<size_t>(height)
      * static_cast<size_t>(this->GetNumberOfComponents())
      * static_cast<size_t>(page);

    ReadCurrentPage(buffer, pixelOffset);

    TIFFReadDirectory(m_InternalImage->m_Image);
    }
}

void TIFFImageIO::Read(void *buffer)
{

  // re-open the file if it was closed
  if ( !m_InternalImage->m_IsOpen )
    {
    if ( !this->CanReadFile( m_FileName.c_str() ) )
      {
      itkExceptionMacro(<< "Cannot open file " << this->m_FileName << "!");
      }
    }

  if ( m_InternalImage->m_Compression == COMPRESSION_OJPEG )
    {
    itkExceptionMacro(<< "This reader cannot read old JPEG compression");
    }

  // The IO region should be of dimensions 3 otherwise we read only the first
  // page
  if ( m_InternalImage->m_NumberOfPages > 0
       && this->GetIORegion().GetImageDimension() > 2 )
    {
    this->ReadVolume(buffer);
    }
  else if ( m_InternalImage->m_NumberOfTiles > 0
           && this->GetIORegion().GetImageDimension() > 2 )
    {
    this->ReadTiles(buffer);
    }
  else
    {
    this->ReadCurrentPage(buffer,0);
    }

  m_InternalImage->Clean();
}

TIFFImageIO::TIFFImageIO()
{
  this->SetNumberOfDimensions(2);
  m_PixelType = SCALAR;
  m_ComponentType = UCHAR;

  this->InitializeColors();
  m_InternalImage = new TIFFReaderInternal;

  m_Spacing[0] = 1.0;
  m_Spacing[1] = 1.0;

  m_Origin[0] = 0.0;
  m_Origin[1] = 0.0;

  m_Compression = TIFFImageIO::PackBits;
  m_JPEGQuality = 75;

  this->AddSupportedWriteExtension(".tif");
  this->AddSupportedWriteExtension(".TIF");
  this->AddSupportedWriteExtension(".tiff");
  this->AddSupportedWriteExtension(".TIFF");

  this->AddSupportedReadExtension(".tif");
  this->AddSupportedReadExtension(".TIF");
  this->AddSupportedReadExtension(".tiff");
  this->AddSupportedReadExtension(".TIFF");
}

TIFFImageIO::~TIFFImageIO()
{
  m_InternalImage->Clean();
  delete m_InternalImage;
}

void TIFFImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Compression: " << m_Compression << "\n";
  os << indent << "JPEGQuality: " << m_JPEGQuality << "\n";
}

void TIFFImageIO::InitializeColors()
{
  m_ColorRed    = ITK_NULLPTR;
  m_ColorGreen  = ITK_NULLPTR;
  m_ColorBlue   = ITK_NULLPTR;
  m_TotalColors = -1;
  m_ImageFormat = TIFFImageIO::NOFORMAT;
}

void TIFFImageIO::ReadImageInformation()
{
  // If the internal image was not open we open it.
  // This is usually done when the user sets the ImageIO manually
  if ( !m_InternalImage->m_IsOpen )
    {
    if ( !this->CanReadFile( m_FileName.c_str() ) )
      {
      itkExceptionMacro(<< "Cannot open file " << this->m_FileName << "!");
      }
    }

  m_Spacing[0] = 1.0;
  m_Spacing[1] = 1.0;

  // If we have some spacing information we use it
  if ( m_InternalImage->m_ResolutionUnit > 0
       && m_InternalImage->m_XResolution > 0
       && m_InternalImage->m_YResolution > 0
       )
    {
    if ( m_InternalImage->m_ResolutionUnit == 2 ) // inches
      {
      m_Spacing[0] = 25.4 / m_InternalImage->m_XResolution;
      m_Spacing[1] = 25.4 / m_InternalImage->m_YResolution;
      }
    else if ( m_InternalImage->m_ResolutionUnit == 3 ) // cm
      {
      m_Spacing[0] = 10.0 / m_InternalImage->m_XResolution;
      m_Spacing[1] = 10.0 / m_InternalImage->m_YResolution;
      }
    }

  m_Origin[0] = 0.0;
  m_Origin[1] = 0.0;

  int width  = m_InternalImage->m_Width;
  int height = m_InternalImage->m_Height;

  m_Dimensions[0] = width;
  m_Dimensions[1] = height;

  switch ( this->GetFormat() )
    {
    case TIFFImageIO::GRAYSCALE:
    case TIFFImageIO::PALETTE_GRAYSCALE:
      this->SetNumberOfComponents(1);
      this->SetPixelType(SCALAR);
      break;
    case TIFFImageIO::RGB_:
      this->SetNumberOfComponents(m_InternalImage->m_SamplesPerPixel);
      this->SetPixelType(RGB);
      break;
    case TIFFImageIO::PALETTE_RGB:
      this->SetNumberOfComponents(3);
      this->SetPixelType(RGB);
      break;
    default:
      this->SetNumberOfComponents(4);
      this->SetPixelType(RGBA);
    }

  if ( !m_InternalImage->CanRead() )
    {
    this->SetNumberOfComponents(4);
    this->SetPixelType(RGBA);
    }

  if ( m_InternalImage->m_BitsPerSample <= 8 )
    {
    if ( m_InternalImage->m_SampleFormat == 2 )
      {
      m_ComponentType = CHAR;
      }
    else
      {
      m_ComponentType = UCHAR;
      }
    }
  else if ( m_InternalImage->m_BitsPerSample == 32 )
    {
    if ( m_InternalImage->m_SampleFormat == 3 )
      {
      m_ComponentType = FLOAT;
      }
    }
  else
    {
    if ( m_InternalImage->m_SampleFormat == 2 )
      {
      m_ComponentType = SHORT;
      }
    else
      {
      m_ComponentType = USHORT;
      }
    }

  // if the tiff file is multi-pages
  if ( m_InternalImage->m_NumberOfPages - m_InternalImage->m_IgnoredSubFiles > 1 )
    {
    this->SetNumberOfDimensions(3);
    if ( m_InternalImage->m_SubFiles > 0 )
      {
      m_Dimensions[2] = m_InternalImage->m_SubFiles;
      }
    else
      {
      m_Dimensions[2] = m_InternalImage->m_NumberOfPages - m_InternalImage->m_IgnoredSubFiles;
      }
    m_Spacing[2] = 1.0;
    m_Origin[2] = 0.0;
    }

  // if the tiff is tiled
  if ( m_InternalImage->m_NumberOfTiles > 1 )
    {
    this->SetNumberOfDimensions(3);
    m_Dimensions[0] = m_InternalImage->m_TileWidth;
    m_Dimensions[1] = m_InternalImage->m_TileHeight;
    m_Dimensions[2] = m_InternalImage->m_NumberOfTiles;
    m_Spacing[2] = 1.0;
    m_Origin[2] = 0.0;
    }
}

bool TIFFImageIO::CanWriteFile(const char *name)
{
  std::string filename = name;

  if ( filename == "" )
    {
    return false;
    }

  std::string::size_type TIFFPos = filename.rfind(".TIFF");
  if ( ( TIFFPos != std::string::npos )
       && ( TIFFPos == filename.length() - 5 ) )
    {
    return true;
    }

  TIFFPos = filename.rfind(".tiff");
  if ( ( TIFFPos != std::string::npos )
       && ( TIFFPos == filename.length() - 5 ) )
    {
    return true;
    }

  TIFFPos = filename.rfind(".tif");
  if ( ( TIFFPos != std::string::npos )
       && ( TIFFPos == filename.length() - 4 ) )
    {
    return true;
    }

  TIFFPos = filename.rfind(".TIF");
  if ( ( TIFFPos != std::string::npos )
       && ( TIFFPos == filename.length() - 4 ) )
    {
    return true;
    }

  return false;
}

void TIFFImageIO::WriteImageInformation()
{
}

void TIFFImageIO::Write(const void *buffer)
{
  if ( m_NumberOfDimensions == 2 || m_NumberOfDimensions == 3 )
    {
    this->InternalWrite(buffer);
    }
  else
    {
    itkExceptionMacro(<< "TIFF Writer can only write 2-d or 3-d images");
    }
}

void TIFFImageIO::InternalWrite(const void *buffer)
{
  const char *outPtr = (const char *)buffer;

  unsigned int page, pages = 1;

  const SizeValueType width =  m_Dimensions[0];
  const SizeValueType height = m_Dimensions[1];
  if ( m_NumberOfDimensions == 3 )
    {
    pages = m_Dimensions[2];
    }

  int    scomponents = this->GetNumberOfComponents();
  float  resolution_x = static_cast< float >( m_Spacing[0] != 0.0 ? 25.4 / m_Spacing[0] : 0.0);
  float  resolution_y = static_cast< float >( m_Spacing[1] != 0.0 ? 25.4 / m_Spacing[1] : 0.0);
  // rowsperstrip is set to a default value but modified based on the tif scanlinesize before
  // passing it into the TIFFSetField (see below).
  uint32 rowsperstrip = ( uint32 ) - 1;
  int    bps;

  switch ( this->GetComponentType() )
    {
    case UCHAR:
      bps = 8;
      break;
    case CHAR:
      bps = 8;
      break;
    case USHORT:
      bps = 16;
      break;
    case SHORT:
      bps = 16;
      break;
    case FLOAT:
      bps = 32;
      break;
    default:
      itkExceptionMacro(
        << "TIFF supports unsigned/signed char, unsigned/signed short, and float");
    }

  uint16_t predictor;

  const char *mode = "w";

  // If the size of the image is greater then 2GB then use big tiff
  const SizeType oneKiloByte = 1024;
  const SizeType oneMegaByte = 1024 * oneKiloByte;
  const SizeType oneGigaByte = 1024 * oneMegaByte;
  const SizeType twoGigaBytes = 2 * oneGigaByte;

  if ( this->GetImageSizeInBytes() > twoGigaBytes )
    {
#ifdef TIFF_INT64_T  // detect if libtiff4
    // Adding the "8" option enables the use of big tiff
    mode = "w8";
#else
    itkExceptionMacro( << "Size of image exceeds the limit of libtiff." );
#endif
    }

  TIFF *tif = TIFFOpen(m_FileName.c_str(), mode );
  if ( !tif )
    {
    itkExceptionMacro( "Error while trying to open file for writing: "
                       << this->GetFileName()
                       << std::endl
                       << "Reason: "
                       << itksys::SystemTools::GetLastSystemError() );
    }

  if ( this->GetComponentType() == SHORT
       || this->GetComponentType() == CHAR )
    {
    TIFFSetField(tif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_INT);
    }
  else if ( this->GetComponentType() == FLOAT )
    {
    TIFFSetField(tif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_IEEEFP);
    }

  uint32 w = width;
  uint32 h = height;

  if ( m_NumberOfDimensions == 3 )
    {
    TIFFCreateDirectory(tif);
    }
  for ( page = 0; page < pages; page++ )
    {
    TIFFSetDirectory(tif, page);
    TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, w);
    TIFFSetField(tif, TIFFTAG_IMAGELENGTH, h);
    TIFFSetField(tif, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);
    TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, scomponents);
    TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, bps); // Fix for stype
    TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
    if ( this->GetComponentType() == SHORT
         || this->GetComponentType() == CHAR )
      {
      TIFFSetField(tif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_INT);
      }
    else if ( this->GetComponentType() == FLOAT )
      {
      TIFFSetField(tif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_IEEEFP);
      }
    TIFFSetField(tif, TIFFTAG_SOFTWARE, "InsightToolkit");

    if ( scomponents > 3 )
      {
      // if number of scalar components is greater than 3, that means we assume
      // there is alpha.
      uint16  extra_samples = scomponents - 3;
      uint16 *sample_info = new uint16[scomponents - 3];
      sample_info[0] = EXTRASAMPLE_ASSOCALPHA;
      int cc;
      for ( cc = 1; cc < scomponents - 3; cc++ )
        {
        sample_info[cc] = EXTRASAMPLE_UNSPECIFIED;
        }
      TIFFSetField(tif, TIFFTAG_EXTRASAMPLES, extra_samples,
                   sample_info);
      delete[] sample_info;
      }

    int compression;

    if ( m_UseCompression )
      {
      switch ( m_Compression )
        {
        case TIFFImageIO::LZW:
          itkWarningMacro(<< "LZW compression is patented outside US so it is disabled. packbits compression will be used instead");
        case TIFFImageIO::PackBits:
          compression = COMPRESSION_PACKBITS; break;
        case TIFFImageIO::JPEG:
          compression = COMPRESSION_JPEG; break;
        case TIFFImageIO::Deflate:
          compression = COMPRESSION_DEFLATE; break;
        default:
          compression = COMPRESSION_NONE;
        }
      }
    else
      {
      compression = COMPRESSION_NONE;
      }

    TIFFSetField(tif, TIFFTAG_COMPRESSION, compression); // Fix for compression

    uint16 photometric = ( scomponents == 1 ) ? PHOTOMETRIC_MINISBLACK : PHOTOMETRIC_RGB;

    if ( compression == COMPRESSION_JPEG )
      {
      TIFFSetField(tif, TIFFTAG_JPEGQUALITY, m_JPEGQuality);
      TIFFSetField(tif, TIFFTAG_JPEGCOLORMODE, JPEGCOLORMODE_RGB);
      }
    else if ( compression == COMPRESSION_DEFLATE )
      {
      predictor = 2;
      TIFFSetField(tif, TIFFTAG_PREDICTOR, predictor);
      }

    TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, photometric); // Fix for scomponents

    // Previously, rowsperstrip was set to a default value so that it would be calculated using
    // the STRIP_SIZE_DEFAULT defined to be 8 kB in tiffiop.h.
    // However, this a very conservative small number, and it leads to very small strips resulting
    // in many io operations, which can be slow when written over networks that require
    // encryption/decryption of each packet (such as sshfs).
    // Conversely, if the value is too high, a lot of extra memory is required to store the strips
    // before they are written out.
    // Experiments writing TIFF images to drives mapped by sshfs showed that a good tradeoff is
    // achieved when the STRIP_SIZE_DEFAULT is increased to 1 MB.
    // This results in an increase in memory usage but no increase in writing time when writing
    // locally and significant writing time improvement when writing over sshfs.
    // For example, writing a 2048x2048 uint16 image with 8 kB per strip leads to 2 rows per strip
    // and takes about 120 seconds writing over sshfs.
    // Using 1 MB per strip leads to 256 rows per strip, which takes only 4 seconds to write over sshfs.
    // Rather than change that value in the third party libtiff library, we instead compute the
    // rowsperstrip here to lead to this same value.
#ifdef TIFF_INT64_T // detect if libtiff4
    uint64_t scanlinesize=TIFFScanlineSize64(tif);
#else
    tsize_t scanlinesize=TIFFScanlineSize(tif);
#endif
    if (scanlinesize == 0)
      {
      itkExceptionMacro("TIFFScanlineSize returned 0");
      }
    rowsperstrip = (uint32_t)(1024*1024 / scanlinesize );
    if ( rowsperstrip < 1 )
      {
      rowsperstrip = 1;
      }

    TIFFSetField( tif,
                  TIFFTAG_ROWSPERSTRIP,
                  TIFFDefaultStripSize(tif, rowsperstrip) );

    if ( resolution_x > 0 && resolution_y > 0 )
      {
      TIFFSetField(tif, TIFFTAG_XRESOLUTION, resolution_x);
      TIFFSetField(tif, TIFFTAG_YRESOLUTION, resolution_y);
      TIFFSetField(tif, TIFFTAG_RESOLUTIONUNIT, RESUNIT_INCH);
      }

    if ( m_NumberOfDimensions == 3 )
      {
      // We are writing single page of the multipage file
      TIFFSetField(tif, TIFFTAG_SUBFILETYPE, FILETYPE_PAGE);
      // Set the page number
      TIFFSetField(tif, TIFFTAG_PAGENUMBER, page, pages);
      }
    int rowLength; // in bytes

    switch ( this->GetComponentType() )
      {
      case UCHAR:
        rowLength = sizeof( unsigned char );
        break;
      case USHORT:
        rowLength = sizeof( unsigned short );
        break;
      case CHAR:
        rowLength = sizeof( char );
        break;
      case SHORT:
        rowLength = sizeof( short );
        break;
      case FLOAT:
        rowLength = sizeof( float );
        break;
      default:
        itkExceptionMacro(
          << "TIFF supports unsigned/signed char, unsigned/signed short, and float");
      }

    rowLength *= this->GetNumberOfComponents();
    rowLength *= width;

    int row = 0;
    for ( unsigned int idx2 = 0; idx2 < height; idx2++ )
      {
      if ( TIFFWriteScanline(tif, const_cast< char * >( outPtr ), row, 0) < 0 )
        {
        itkExceptionMacro(<< "TIFFImageIO: error out of disk space");
        }
      outPtr += rowLength;
      ++row;
      }

    if ( m_NumberOfDimensions == 3 )
      {
      TIFFWriteDirectory(tif);
      }
    }
  TIFFClose(tif);
}


// With the TIFF 4.0 (aka bigtiff ) interface the tiff field structure
// was renamed and became an opaque type requiring function to
// access. The follow are some macros for portable access.
#ifdef ITK_TIFF_HAS_TIFFFieldReadCount
#define itkTIFFFieldReadCount( TIFFField ) TIFFFieldReadCount( TIFFField )
#define itkTIFFFieldPassCount( TIFFField ) TIFFFieldPassCount( TIFFField )
#define itkTIFFFieldDataType( TIFFField )  TIFFFieldDataType( TIFFField )
#define itkTIFFField TIFFField
#elif defined(ITK_TIFF_HAS_TIFFField)
} // end namespace itk
/// Tiff 4.0.0-4.0.2 had _TIFFField as a private structure, but missing the
/// required access mehods added in 4.0.3, This is a copy of the
/// structure from tiff_dir.h.
typedef enum {ITK_TIFF_MOC_1 = 0,ITK_TIFF_MOC_2=51 } ITK_TIFF_MOC_TIFFSetGetFieldType;
struct _TIFFField {
  uint32 field_tag;                       /* field's tag */
  short field_readcount;                  /* read count/TIFF_VARIABLE/TIFF_SPP */
  short field_writecount;                 /* write count/TIFF_VARIABLE */
  TIFFDataType field_type;                /* type of associated data */
  uint32 reserved;                        /* reserved for future extension */
  ITK_TIFF_MOC_TIFFSetGetFieldType set_field_type;     /* type to be passed to TIFFSetField */
  ITK_TIFF_MOC_TIFFSetGetFieldType get_field_type;     /* type to be passed to TIFFGetField */
  unsigned short field_bit;               /* bit in fieldsset bit vector */
  unsigned char field_oktochange;         /* if true, can change while writing */
  unsigned char field_passcount;          /* if true, pass dir count on set */
  char* field_name;                       /* ASCII name */
  TIFFFieldArray* field_subfields;        /* if field points to child ifds, child ifd field definition array */
};
namespace itk {
#define itkTIFFFieldReadCount( TIFFField ) ((TIFFField)->field_readcount)
#define itkTIFFFieldPassCount( TIFFField ) ((TIFFField)->field_passcount)
#define itkTIFFFieldDataType( TIFFField ) ((TIFFField)->field_type)
#define itkTIFFField TIFFField
#else // libtiff version 3
#define itkTIFFFieldReadCount( TIFFFieldInfo ) ((TIFFFieldInfo)->field_readcount)
#define itkTIFFFieldPassCount( TIFFFieldInfo ) ((TIFFFieldInfo)->field_passcount)
#define itkTIFFFieldDataType( TIFFFieldInfo ) ((TIFFFieldInfo)->field_type)
#define itkTIFFField TIFFFieldInfo
#endif


bool TIFFImageIO::CanFindTIFFTag(unsigned int t)
{
  // m_InternalImage needs to be valid
  if ( !m_InternalImage )
    {
    itkExceptionMacro(<< "Need to call CanReadFile before");
    }

  ttag_t           tag = t;     // 32bits integer

  const itkTIFFField *fld = TIFFFieldWithTag(m_InternalImage->m_Image, tag);

  if ( fld == ITK_NULLPTR )
    {
    return false;
    }
  return true;
}

void * TIFFImageIO::ReadRawByteFromTag(unsigned int t, unsigned int & value_count)
{
  // m_InternalImage needs to be valid
  if ( !m_InternalImage )
    {
    itkExceptionMacro(<< "Need to call CanReadFile before");
    }
  ttag_t           tag = t;
  void *           raw_data = ITK_NULLPTR;

  const itkTIFFField *fld = TIFFFieldWithTag(m_InternalImage->m_Image, tag);

  if ( fld == ITK_NULLPTR )
    {
    itkExceptionMacro(<< "fld is NULL");
    }

  if ( !itkTIFFFieldPassCount( fld ) )
    {
    return ITK_NULLPTR;
    }

  int ret = 0;
  if ( itkTIFFFieldReadCount( fld ) == TIFF_VARIABLE2 )
    {
    uint32_t cnt;
    ret = TIFFGetField(m_InternalImage->m_Image, tag, &cnt, &raw_data);
    value_count = cnt;
    }
  else if ( itkTIFFFieldReadCount( fld ) == TIFF_VARIABLE )
    {
    uint16 cnt;
    ret = TIFFGetField(m_InternalImage->m_Image, tag, &cnt, &raw_data);
    value_count = cnt;
    }

  if ( ret != 1 )
    {
    itkExceptionMacro(<< "Tag cannot be found");
    }
  else
    {
    if ( itkTIFFFieldDataType( fld ) != TIFF_BYTE  )
      {
      itkExceptionMacro(<< "Tag is not of type TIFF_BYTE");
      }
    }

  return raw_data;
}


void TIFFImageIO::ReadCurrentPage(void *buffer, size_t pixelOffset)
{
  const int width  = m_InternalImage->m_Width;
  const int height = m_InternalImage->m_Height;

  // It is necessary to re-initialize the colors for each page so
  // that the colormap is reset in the GetColor method.  This is
  // also true in the case that each slice has a different colormap.
  this->InitializeColors();

  if ( !m_InternalImage->CanRead() )
    {
    const size_t sz = static_cast<size_t>(width)
      * static_cast<size_t>(height);

    uint32 *tempImage = new uint32[sz];

    if ( !TIFFReadRGBAImage(m_InternalImage->m_Image,
                            width, height,
                            tempImage, 1) )
      {
      if ( tempImage != buffer )
        {
        delete[] tempImage;
        }
      itkExceptionMacro(<< "Cannot read TIFF image or as a TIFF RGBA image");
      }


    if ( m_ComponentType == USHORT )
      {
      unsigned short *out = (unsigned short *)(buffer) + pixelOffset;
      RGBAImageToBuffer<unsigned short>(out, tempImage);
      }
    else if ( m_ComponentType == SHORT )
      {
      short *out = (short *)(buffer) + pixelOffset;
      RGBAImageToBuffer<short>(out, tempImage);
      }
    else if ( m_ComponentType == CHAR )
      {
      char *out = (char *)(buffer) + pixelOffset;
      RGBAImageToBuffer<char>(out, tempImage);
      }
    else
      {
      unsigned char *out = (unsigned char *)(buffer) + pixelOffset;
      RGBAImageToBuffer<unsigned char>(out, tempImage);
      }
    if ( tempImage != buffer )
      {
      delete[] tempImage;
      }
    }
  else
    {
    unsigned int format = this->GetFormat();

    switch ( format )
      {
      case TIFFImageIO::GRAYSCALE:
      case TIFFImageIO::RGB_:
      case TIFFImageIO::PALETTE_GRAYSCALE:
        if ( m_ComponentType == USHORT )
          {
          unsigned short *volume = reinterpret_cast< unsigned short * >( buffer );
          volume += pixelOffset;
          this->ReadGenericImage(volume, width, height);
          }
        else if ( m_ComponentType == SHORT )
          {
          short *volume = reinterpret_cast< short * >( buffer );
          volume += pixelOffset;
          this->ReadGenericImage(volume, width, height);
          }
        else if ( m_ComponentType == CHAR )
          {
          char *volume = reinterpret_cast< char * >( buffer );
          volume += pixelOffset;
          this->ReadGenericImage(volume, width, height);
          }
        else if ( m_ComponentType == FLOAT )
          {
          float *volume = reinterpret_cast< float * >( buffer );
          volume += pixelOffset;
          this->ReadGenericImage(volume, width, height);
          }
        else
          {
          unsigned char *volume = reinterpret_cast< unsigned char * >( buffer );
          volume += pixelOffset;
          this->ReadGenericImage(volume, width, height);
          }
        break;
      case TIFFImageIO::PALETTE_RGB:
        // This differs from PALLETTE_GRAYSCALE only in that the
        // volume is incremented by 3 times more since the colormap
        // consists of RGB.
        if ( m_ComponentType == USHORT )
          {
          unsigned short *volume = reinterpret_cast< unsigned short * >( buffer );
          volume += pixelOffset;
          this->ReadGenericImage(volume, width, height);
          }
        else if ( m_ComponentType == SHORT )
          {
          short *volume = reinterpret_cast< short * >( buffer );
          volume += pixelOffset;
          this->ReadGenericImage(volume, width, height);
          }
        else if ( m_ComponentType == CHAR )
          {
          char *volume = reinterpret_cast< char * >( buffer );
          volume += pixelOffset;
          this->ReadGenericImage(volume, width, height);
          }
        else
          {
          unsigned char *volume = reinterpret_cast< unsigned char * >( buffer );
          volume += pixelOffset;
          this->ReadGenericImage(volume, width, height);
          }
        break;
      default:
        return;
      }
    }

}

template <typename TComponent>
void TIFFImageIO::ReadGenericImage(void *_out,
                                   unsigned int width,
                                   unsigned int height)
{
  typedef TComponent ComponentType;

#ifdef TIFF_INT64_T // detect if libtiff4
   uint64_t isize = TIFFScanlineSize64(m_InternalImage->m_Image);
  uint64_t cc;
#else
  tsize_t isize = TIFFScanlineSize(m_InternalImage->m_Image);
  tsize_t cc;
#endif

  size_t         inc;
  tdata_t        buf = _TIFFmalloc(isize);
  isize /= sizeof(ComponentType);

  ComponentType *out = static_cast< ComponentType* >( _out );
  ComponentType *image;

  // It is necessary to re-initialize the colors for eachread so
  // that the colormap remains valid.
  this->InitializeColors();

  if ( m_InternalImage->m_PlanarConfig != PLANARCONFIG_CONTIG )
    {
    itkExceptionMacro(<< "This reader can only do PLANARCONFIG_CONTIG");
    }

  if ( m_InternalImage->m_Orientation != ORIENTATION_TOPLEFT
    && m_InternalImage->m_Orientation != ORIENTATION_BOTLEFT )
    {
    itkExceptionMacro(<< "This reader can only do ORIENTATION_TOPLEFT and  ORIENTATION_BOTLEFT.");
    }


  switch ( this->GetFormat() )
    {
    case TIFFImageIO::GRAYSCALE:
    case TIFFImageIO::PALETTE_GRAYSCALE:
      inc = 1;
      break;
    case TIFFImageIO::RGB_:
      inc = m_InternalImage->m_SamplesPerPixel;
      break;
    case TIFFImageIO::PALETTE_RGB:
      inc = 3;
      break;
    default:
      inc = 1;
      break;
    }

  for ( int row = 0; row < (int)height; ++row )
    {
    if ( TIFFReadScanline(m_InternalImage->m_Image, buf, row, 0) <= 0 )
      {
      itkExceptionMacro(<< "Problem reading the row: " << row);
      }

    if ( m_InternalImage->m_Orientation == ORIENTATION_TOPLEFT )
      {
      image = out + (size_t) (row) * width * inc;
      }
    else // bottom left
      {
      image = out + (size_t) (width) * inc * ( height - ( row + 1 ) );
      }

    for ( cc = 0; cc < isize;
          cc += m_InternalImage->m_SamplesPerPixel )
      {
      inc = this->EvaluateImageAt(image,
                                  static_cast< ComponentType * >( buf )
                                  + cc);
      image += inc;
      }
    }

  _TIFFfree(buf);
}

template <typename TComponent>
void  TIFFImageIO::RGBAImageToBuffer( void *out, const uint32_t *tempImage )
{
  typedef TComponent ComponentType;

  const int width  = m_InternalImage->m_Width;
  const int height = m_InternalImage->m_Height;

  ComponentType *fimage = (ComponentType *)out;

  for ( int yy = 0; yy < height; ++yy )
    {
    const uint32 *ssimage = tempImage + ( height - yy - 1 ) * (size_t) width;
    for ( int xx = 0; xx < width; ++xx )
      {
      const ComponentType red   = static_cast< ComponentType >( TIFFGetR(*ssimage) );
      const ComponentType green = static_cast< ComponentType >( TIFFGetG(*ssimage) );
      const ComponentType blue  = static_cast< ComponentType >( TIFFGetB(*ssimage) );
      const ComponentType alpha = static_cast< ComponentType >( TIFFGetA(*ssimage) );

      *( fimage  ) = red;
      *( fimage + 1 ) = green;
      *( fimage + 2 ) = blue;
      *( fimage + 3 ) = alpha;
      fimage += 4;
      ++ssimage;
      }
    }
}

} // end namespace itk
