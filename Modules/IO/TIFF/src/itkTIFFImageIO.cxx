/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkMetaDataObject.h"

#include "itk_tiff.h"

namespace itk
{

bool
TIFFImageIO::CanReadFile(const char * file)
{
  // First check the filename
  std::string filename = file;

  if (filename.empty())
  {
    itkDebugMacro(<< "No filename specified.");
    return false;
  }

  // Now check if this is a valid TIFF image
  TIFFErrorHandler save = TIFFSetErrorHandler(nullptr);
  int              res = m_InternalImage->Open(file);
  if (res)
  {
    TIFFSetErrorHandler(save);
    return true;
  }
  m_InternalImage->Clean();
  TIFFSetErrorHandler(save);
  return false;
}

void
TIFFImageIO::ReadGenericImage(void * out, unsigned int width, unsigned int height)
{

  if (m_ComponentType == IOComponentEnum::UCHAR)
  {
    this->ReadGenericImage<unsigned char>(out, width, height);
  }
  else if (m_ComponentType == IOComponentEnum::CHAR)
  {
    this->ReadGenericImage<char>(out, width, height);
  }
  else if (m_ComponentType == IOComponentEnum::USHORT)
  {
    this->ReadGenericImage<unsigned short>(out, width, height);
  }
  else if (m_ComponentType == IOComponentEnum::SHORT)
  {
    this->ReadGenericImage<short>(out, width, height);
  }
  else if (m_ComponentType == IOComponentEnum::FLOAT)
  {
    this->ReadGenericImage<float>(out, width, height);
  }
}

void
TIFFImageIO::GetColor(uint64_t index, uint16_t * red, uint16_t * green, uint16_t * blue)
{
  *red = 0;
  *green = 0;
  *blue = 0;

  if (m_TotalColors > 0 && m_ColorRed && m_ColorGreen && m_ColorBlue)
  {
    index %= m_TotalColors;

    *red = *(m_ColorRed + index);
    *green = *(m_ColorGreen + index);
    *blue = *(m_ColorBlue + index);
    return;
  }
}

unsigned int
TIFFImageIO::GetFormat()
{

  if (m_ImageFormat != TIFFImageIO::NOFORMAT)
  {
    return m_ImageFormat;
  }

  switch (m_InternalImage->m_Photometrics)
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
      if (m_TotalColors > 0)
      {
        if (this->GetExpandRGBPalette())
        {
          for (uint64_t cc = 0; cc < m_TotalColors; ++cc)
          {
            uint16_t red, green, blue;
            this->GetColor(cc, &red, &green, &blue);
            if (red != green || red != blue)
            {
              m_ImageFormat = TIFFImageIO::PALETTE_RGB;
              return m_ImageFormat;
            }
          }
          m_ImageFormat = TIFFImageIO::PALETTE_GRAYSCALE;
          return m_ImageFormat;
        }
        else
        { // if not expanding read grayscale palette as palette
          m_ImageFormat = TIFFImageIO::PALETTE_RGB;
          return m_ImageFormat;
        }
      }
  }
  m_ImageFormat = TIFFImageIO::OTHER;
  return m_ImageFormat;
}


/** Read a multipage tiff */
void
TIFFImageIO::ReadVolume(void * buffer)
{
  const size_t width{ m_InternalImage->m_Width };
  const size_t height{ m_InternalImage->m_Height };

  for (uint16 page = 0; page < m_InternalImage->m_NumberOfPages; page++)
  {
    if (m_InternalImage->m_IgnoredSubFiles > 0)
    {
      int32 subfiletype = 6;
      if (TIFFGetField(m_InternalImage->m_Image, TIFFTAG_SUBFILETYPE, &subfiletype))
      {
        if (subfiletype & FILETYPE_REDUCEDIMAGE || subfiletype & FILETYPE_MASK)
        {
          // skip subfile
          TIFFReadDirectory(m_InternalImage->m_Image);
          continue;
        }
      }
    }


    const size_t pixelOffset = width * height * this->GetNumberOfComponents() * page;

    ReadCurrentPage(buffer, pixelOffset);

    TIFFReadDirectory(m_InternalImage->m_Image);
  }
}

void
TIFFImageIO::Read(void * buffer)
{

  // re-open the file if it was closed
  if (!m_InternalImage->m_IsOpen)
  {
    if (!this->CanReadFile(m_FileName.c_str()))
    {
      itkExceptionMacro(<< "Cannot open file " << this->m_FileName << "!");
    }
  }

  // The IO region should be of dimensions 3 otherwise we read only the first
  // page
  if (m_InternalImage->m_NumberOfPages > 0 && this->GetIORegion().GetImageDimension() > 2)
  {
    this->ReadVolume(buffer);
  }
  else
  {
    this->ReadCurrentPage(buffer, 0);
  }

  m_InternalImage->Clean();
}

TIFFImageIO::TIFFImageIO()
  : m_ColorPalette(0)

{
  this->SetNumberOfDimensions(2);
  this->Self::SetJPEGQuality(75);
  this->Self::SetCompressor("");

  m_ComponentType = IOComponentEnum::UCHAR;
  m_PixelType = IOPixelEnum::SCALAR;

  m_ColorRed = nullptr;
  m_ColorGreen = nullptr;
  m_ColorBlue = nullptr;

  m_InternalImage = new TIFFReaderInternal;

  m_Spacing[0] = 1.0;
  m_Spacing[1] = 1.0;

  m_Origin[0] = 0.0;
  m_Origin[1] = 0.0;

  const char * extensions[] = { ".tif", ".TIF", ".tiff", ".TIFF" };

  for (auto ext : extensions)
  {
    this->AddSupportedWriteExtension(ext);
    this->AddSupportedReadExtension(ext);
  }
}

TIFFImageIO::~TIFFImageIO()
{
  m_InternalImage->Clean();
  delete m_InternalImage;
}

void
TIFFImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Compression: " << m_Compression << std::endl;
  os << indent << "JPEGQuality: " << this->GetJPEGQuality() << std::endl;
  if (!m_ColorPalette.empty())
  {
    os << indent << "Image RGB palette:"
       << "\n";
    for (size_t i = 0; i < m_ColorPalette.size(); ++i)
    {
      os << indent << "[" << i << "]" << itk::NumericTraits<PaletteType::value_type>::PrintType(m_ColorPalette[i])
         << std::endl;
    }
  }
}


void
TIFFImageIO::InternalSetCompressor(const std::string & _compressor)
{
  if (_compressor.empty() || _compressor == "PACKBITS")
  {
    this->SetCompression(PackBits);
  }
  else if (_compressor == "NOCOMPRESSION")
  {
    this->SetCompression(NoCompression);
  }
  else if (_compressor == "JPEG")
  {
    this->SetCompression(JPEG);
  }
  else if (_compressor == "DEFLATE")
  {
    this->SetCompression(Deflate);
  }
  else if (_compressor == "LZW")
  {
    this->SetCompression(LZW);
  }
  else
  {
    this->Superclass::InternalSetCompressor(_compressor);
  }
}

void
TIFFImageIO::InitializeColors()
{
  m_ColorRed = nullptr;
  m_ColorGreen = nullptr;
  m_ColorBlue = nullptr;
  m_TotalColors = 0;
  m_ImageFormat = TIFFImageIO::NOFORMAT;

  if (m_InternalImage == nullptr)
  {
    return;
  }

  unsigned short *red_orig, *green_orig, *blue_orig;
  if (!TIFFGetField(m_InternalImage->m_Image, TIFFTAG_COLORMAP, &red_orig, &green_orig, &blue_orig))
  {
    return;
  }

  switch (m_InternalImage->m_BitsPerSample)
  {
    case 1:
    case 2:
    case 4:
    case 8:
    case 16:
      break;
    default:
      itkExceptionMacro(<< "Sorry, can not handle image with " << m_InternalImage->m_BitsPerSample << "-bit samples");
  }

  m_TotalColors = uint64_t{ 1 } << m_InternalImage->m_BitsPerSample;

  m_ColorRed = red_orig;
  m_ColorGreen = green_orig;
  m_ColorBlue = blue_orig;
}


void
TIFFImageIO::ReadImageInformation()
{
  // If the internal image was not open we open it.
  // This is usually done when the user sets the ImageIO manually
  if (!m_InternalImage->m_IsOpen)
  {
    if (!this->CanReadFile(m_FileName.c_str()))
    {
      itkExceptionMacro(<< "Cannot open file " << this->m_FileName << "!");
    }
  }

  ReadTIFFTags();

  // if the tiff file is multi-pages
  if (m_InternalImage->m_NumberOfPages - m_InternalImage->m_IgnoredSubFiles > 1)
  {
    this->SetNumberOfDimensions(3);
    if (m_InternalImage->m_SubFiles > 0)
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
  else
  {
    this->SetNumberOfDimensions(2);
  }

  m_Spacing[0] = 1.0;
  m_Spacing[1] = 1.0;

  // If we have some spacing information we use it
  if (m_InternalImage->m_ResolutionUnit > 0 && m_InternalImage->m_XResolution > 0 && m_InternalImage->m_YResolution > 0)
  {
    if (m_InternalImage->m_ResolutionUnit == 2) // inches
    {
      m_Spacing[0] = 25.4 / static_cast<double>(m_InternalImage->m_XResolution);
      m_Spacing[1] = 25.4 / static_cast<double>(m_InternalImage->m_YResolution);
    }
    else if (m_InternalImage->m_ResolutionUnit == 3) // cm
    {
      m_Spacing[0] = 10.0 / static_cast<double>(m_InternalImage->m_XResolution);
      m_Spacing[1] = 10.0 / static_cast<double>(m_InternalImage->m_YResolution);
    }
  }

  m_Origin[0] = 0.0;
  m_Origin[1] = 0.0;

  m_Dimensions[0] = m_InternalImage->m_Width;
  m_Dimensions[1] = m_InternalImage->m_Height;

  if (m_InternalImage->m_BitsPerSample <= 8)
  {
    if (m_InternalImage->m_SampleFormat == 2)
    {
      m_ComponentType = IOComponentEnum::CHAR;
    }
    else
    {
      m_ComponentType = IOComponentEnum::UCHAR;
    }
  }
  else if (m_InternalImage->m_BitsPerSample == 32)
  {
    if (m_InternalImage->m_SampleFormat == 3)
    {
      m_ComponentType = IOComponentEnum::FLOAT;
    }
  }
  else
  {
    if (m_InternalImage->m_SampleFormat == 2)
    {
      m_ComponentType = IOComponentEnum::SHORT;
    }
    else
    {
      m_ComponentType = IOComponentEnum::USHORT;
    }
  }

  m_IsReadAsScalarPlusPalette = false;
  switch (this->GetFormat())
  {
    case TIFFImageIO::PALETTE_GRAYSCALE:
    case TIFFImageIO::GRAYSCALE:
      this->SetNumberOfComponents(1);
      this->SetPixelType(IOPixelEnum::SCALAR);
      break;
    case TIFFImageIO::RGB_:
      this->SetNumberOfComponents(m_InternalImage->m_SamplesPerPixel);
      this->SetPixelType(IOPixelEnum::RGB);
      break;
    case TIFFImageIO::PALETTE_RGB:
    {
      if (this->GetExpandRGBPalette())
      {
        this->SetNumberOfComponents(3);
        this->SetPixelType(IOPixelEnum::RGB);
      }
      else
      {
        this->SetNumberOfComponents(1);
        this->SetPixelType(IOPixelEnum::SCALAR);
        m_IsReadAsScalarPlusPalette = true;
      }
      break;
    }
    default:
      // CanRead should be false
      this->SetNumberOfComponents(4);
      this->SetPixelType(IOPixelEnum::RGBA);
  }

  bool isPalette =
    (this->GetFormat() == TIFFImageIO::PALETTE_GRAYSCALE || this->GetFormat() == TIFFImageIO::PALETTE_RGB) &&
    m_TotalColors > 0;
  bool isPaletteShortType = false;
  if (isPalette)
  {
    // detect if palette appears to be 8-bit or 16-bit
    for (uint64_t cc = 0; cc < m_TotalColors; ++cc)
    {
      uint16_t red, green, blue;
      this->GetColor(cc, &red, &green, &blue);
      if (red > 255 || green > 255 || blue > 255)
      {
        isPaletteShortType = true;
        break;
      }
    }
  }

  if (isPalette && this->GetExpandRGBPalette())
  {
    // put the component type the same as the palette type
    m_ComponentType = (isPaletteShortType) ? IOComponentEnum::USHORT : IOComponentEnum::UCHAR;
  }


  if (!m_InternalImage->CanRead())
  {
    //  exception if compression is not supported
    if (TIFFIsCODECConfigured(this->m_InternalImage->m_Compression) != 1)
    {
      const TIFFCodec * c = TIFFFindCODEC(this->m_InternalImage->m_Compression);
      const char *      codecName = (c != nullptr) ? static_cast<const char *>(c->name) : "unknown";

      itkExceptionMacro("TIFF CODEC \"" << codecName << "\" is not supported.");
    }

    char emsg[1024];
    if (TIFFRGBAImageOK(m_InternalImage->m_Image, emsg) != 1)
    {
      itkExceptionMacro("Unable to read tiff file: " << emsg);
    }


    if (!m_IsReadAsScalarPlusPalette)
    {
      itkDebugMacro(<< "Using TIFFReadRGBAImage");
      this->SetNumberOfComponents(4);
      this->SetPixelType(IOPixelEnum::RGBA);
      m_ComponentType = IOComponentEnum::UCHAR;
    }
    else
    {
      itkDebugMacro(<< "Using TIFFReadRGBImage");
      itkWarningMacro(<< "Could not read this palette image as scalar+Palette because of its TIFF format");
      // can't read as scalar+palette so reset type to RGB
      m_IsReadAsScalarPlusPalette = false;
      this->SetNumberOfComponents(3);
      this->SetPixelType(IOPixelEnum::RGB); // RGBA is not valid for palette, so cannot be
      m_ComponentType = (isPaletteShortType) ? IOComponentEnum::USHORT : IOComponentEnum::UCHAR;
    }
  }

  if (!m_IsReadAsScalarPlusPalette)
  {
    // make sure the palette is empty
    m_ColorPalette.resize(0);
  }
}

bool
TIFFImageIO::CanWriteFile(const char * name)
{
  std::string filename = name;

  if (filename.empty())
  {
    return false;
  }

  return this->HasSupportedWriteExtension(name, false);
}

void
TIFFImageIO::WriteImageInformation()
{}

void
TIFFImageIO::Write(const void * buffer)
{
  if (m_NumberOfDimensions == 2 || m_NumberOfDimensions == 3)
  {
    this->InternalWrite(buffer);
  }
  else
  {
    itkExceptionMacro(<< "TIFF Writer can only write 2-d or 3-d images");
  }
}

void
TIFFImageIO::InternalWrite(const void * buffer)
{
  const auto * outPtr = static_cast<const char *>(buffer);

  uint16 page, pages = 1;

  const SizeValueType width = m_Dimensions[0];
  const SizeValueType height = m_Dimensions[1];
  if (m_NumberOfDimensions == 3)
  {
    pages = static_cast<uint16>(m_Dimensions[2]);
  }

  auto   scomponents = static_cast<uint16>(this->GetNumberOfComponents());
  double resolution_x{ m_Spacing[0] != 0.0 ? 25.4 / m_Spacing[0] : 0.0 };
  double resolution_y{ m_Spacing[1] != 0.0 ? 25.4 / m_Spacing[1] : 0.0 };
  // rowsperstrip is set to a default value but modified based on the tif scanlinesize before
  // passing it into the TIFFSetField (see below).
  auto     rowsperstrip = uint32{ 0 };
  uint16_t bps;

  switch (this->GetComponentType())
  {
    case IOComponentEnum::UCHAR:
      bps = 8;
      break;
    case IOComponentEnum::CHAR:
      bps = 8;
      break;
    case IOComponentEnum::USHORT:
      bps = 16;
      break;
    case IOComponentEnum::SHORT:
      bps = 16;
      break;
    case IOComponentEnum::FLOAT:
      bps = 32;
      break;
    default:
      itkExceptionMacro(<< "TIFF supports unsigned/signed char, unsigned/signed short, and float");
  }

  uint16_t predictor;

  const char * mode = "w";

  // If the size of the image is greater than 2 GiB then use big tiff
  constexpr SizeType oneKibiByte = 1024;
  const SizeType     oneMebiByte = 1024 * oneKibiByte;
  const SizeType     oneGibiByte = 1024 * oneMebiByte;
  const SizeType     twoGibiBytes = 2 * oneGibiByte;

  if (this->GetImageSizeInBytes() > twoGibiBytes)
  {
#ifdef TIFF_INT64_T // detect if libtiff4
    // Adding the "8" option enables the use of big tiff
    mode = "w8";
#else
    itkExceptionMacro(<< "Size of image exceeds the limit of libtiff.");
#endif
  }

  TIFF * tif = TIFFOpen(m_FileName.c_str(), mode);
  if (!tif)
  {
    itkExceptionMacro("Error while trying to open file for writing: " << this->GetFileName() << std::endl
                                                                      << "Reason: "
                                                                      << itksys::SystemTools::GetLastSystemError());
  }

  if (this->GetComponentType() == IOComponentEnum::SHORT || this->GetComponentType() == IOComponentEnum::CHAR)
  {
    TIFFSetField(tif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_INT);
  }
  else if (this->GetComponentType() == IOComponentEnum::FLOAT)
  {
    TIFFSetField(tif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_IEEEFP);
  }

  auto w = static_cast<uint32>(width);
  auto h = static_cast<uint32>(height);

  if (m_NumberOfDimensions == 3)
  {
    TIFFCreateDirectory(tif);
  }
  for (page = 0; page < pages; page++)
  {
    TIFFSetDirectory(tif, page);
    TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, w);
    TIFFSetField(tif, TIFFTAG_IMAGELENGTH, h);
    TIFFSetField(tif, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);
    TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, scomponents);
    TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, bps); // Fix for stype
    TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
    if (this->GetComponentType() == IOComponentEnum::SHORT || this->GetComponentType() == IOComponentEnum::CHAR)
    {
      TIFFSetField(tif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_INT);
    }
    else if (this->GetComponentType() == IOComponentEnum::FLOAT)
    {
      TIFFSetField(tif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_IEEEFP);
    }
    TIFFSetField(tif, TIFFTAG_SOFTWARE, "InsightToolkit");

    if (scomponents > 3)
    {
      // if number of scalar components is greater than 3, that means we assume
      // there is alpha.
      uint16 extra_samples = scomponents - 3;
      auto * sample_info = new uint16[scomponents - 3];
      sample_info[0] = EXTRASAMPLE_ASSOCALPHA;
      for (uint16 cc = 1; cc < scomponents - 3; cc++)
      {
        sample_info[cc] = EXTRASAMPLE_UNSPECIFIED;
      }
      TIFFSetField(tif, TIFFTAG_EXTRASAMPLES, extra_samples, sample_info);
      delete[] sample_info;
    }

    uint16 compression;

    if (m_UseCompression)
    {
      switch (m_Compression)
      {
        case TIFFImageIO::LZW:
          itkWarningMacro(
            << "LZW compression is patented outside US so it is disabled. packbits compression will be used instead");
          ITK_FALLTHROUGH;
        case TIFFImageIO::PackBits:
          compression = COMPRESSION_PACKBITS;
          break;
        case TIFFImageIO::JPEG:
          compression = COMPRESSION_JPEG;
          break;
        case TIFFImageIO::Deflate:
          compression = COMPRESSION_DEFLATE;
          break;
        default:
          compression = COMPRESSION_NONE;
      }
    }
    else
    {
      compression = COMPRESSION_NONE;
    }

    TIFFSetField(tif, TIFFTAG_COMPRESSION, compression); // Fix for compression

    bool paletteAllocated = false;
    if (scomponents == 1)
    {
      if (this->GetWritePalette())
      {
        TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_PALETTE);
        this->AllocateTiffPalette(bps);
        TIFFSetField(tif, TIFFTAG_COLORMAP, m_ColorRed, m_ColorGreen, m_ColorBlue);
        paletteAllocated = true;
      }
      else
      {
        TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
      }
    }
    else
    {
      if (this->GetWritePalette())
      {
        itkWarningMacro(<< "Could not write this image as palette because pixel is not scalar");
      }
      TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
    }
    if (compression == COMPRESSION_JPEG)
    {
      TIFFSetField(tif, TIFFTAG_JPEGQUALITY, this->GetJPEGQuality());
      TIFFSetField(tif, TIFFTAG_JPEGCOLORMODE, JPEGCOLORMODE_RGB);
    }
    else if (compression == COMPRESSION_DEFLATE)
    {
      predictor = PREDICTOR_NONE;
      TIFFSetField(tif, TIFFTAG_PREDICTOR, predictor);
    }


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
    uint64_t scanlinesize = TIFFScanlineSize64(tif);
#else
    tsize_t scanlinesize = TIFFScanlineSize(tif);
#endif
    if (scanlinesize == 0)
    {
      itkExceptionMacro("TIFFScanlineSize returned 0");
    }
    rowsperstrip = static_cast<uint32_t>(1024 * 1024 / scanlinesize);
    if (rowsperstrip < 1)
    {
      rowsperstrip = 1;
    }

    TIFFSetField(tif, TIFFTAG_ROWSPERSTRIP, TIFFDefaultStripSize(tif, rowsperstrip));

    if (resolution_x > 0 && resolution_y > 0)
    {
      TIFFSetField(tif, TIFFTAG_XRESOLUTION, resolution_x);
      TIFFSetField(tif, TIFFTAG_YRESOLUTION, resolution_y);
      TIFFSetField(tif, TIFFTAG_RESOLUTIONUNIT, RESUNIT_INCH);
    }

    if (m_NumberOfDimensions == 3)
    {
      // We are writing single page of the multipage file
      TIFFSetField(tif, TIFFTAG_SUBFILETYPE, FILETYPE_PAGE);
      // Set the page number
      TIFFSetField(tif, TIFFTAG_PAGENUMBER, page, pages);
    }
    SizeValueType rowLength; // in bytes

    switch (this->GetComponentType())
    {
      case IOComponentEnum::UCHAR:
        rowLength = sizeof(unsigned char);
        break;
      case IOComponentEnum::USHORT:
        rowLength = sizeof(unsigned short);
        break;
      case IOComponentEnum::CHAR:
        rowLength = sizeof(char);
        break;
      case IOComponentEnum::SHORT:
        rowLength = sizeof(short);
        break;
      case IOComponentEnum::FLOAT:
        rowLength = sizeof(float);
        break;
      default:
        itkExceptionMacro(<< "TIFF supports unsigned/signed char, unsigned/signed short, and float");
    }

    rowLength *= this->GetNumberOfComponents();
    rowLength *= width;

    uint32 row = 0;
    for (unsigned int idx2 = 0; idx2 < height; idx2++)
    {
      if (TIFFWriteScanline(tif, const_cast<char *>(outPtr), row, 0) < 0)
      {
        itkExceptionMacro(<< "TIFFImageIO: error out of disk space");
      }
      outPtr += rowLength;
      ++row;
    }

    if (m_NumberOfDimensions == 3)
    {
      TIFFWriteDirectory(tif);
    }
    if (paletteAllocated)
    {
      _TIFFfree(m_ColorRed);
      _TIFFfree(m_ColorGreen);
      _TIFFfree(m_ColorBlue);
    }
  }
  TIFFClose(tif);
}


// With the TIFF 4.0 (aka bigtiff ) interface the tiff field structure
// was renamed and became an opaque type requiring function to
// access. The follow are some macros for portable access.
#ifdef ITK_TIFF_HAS_TIFFFieldReadCount
#  define itkTIFFFieldName(TIFFField) TIFFFieldName(TIFFField)
#  define itkTIFFFieldReadCount(TIFFField) TIFFFieldReadCount(TIFFField)
#  define itkTIFFFieldPassCount(TIFFField) TIFFFieldPassCount(TIFFField)
#  define itkTIFFFieldDataType(TIFFField) TIFFFieldDataType(TIFFField)
#  define itkTIFFField TIFFField
#elif defined(ITK_TIFF_HAS_TIFFField)
} // end namespace itk
/// Tiff 4.0.0-4.0.2 had _TIFFField as a private structure, but missing the
/// required access mehods added in 4.0.3, This is a copy of the
/// structure from tiff_dir.h.
typedef enum
{
  ITK_TIFF_MOC_1 = 0,
  ITK_TIFF_MOC_2 = 51
} ITK_TIFF_MOC_TIFFSetGetFieldType;
struct _TIFFField
{
  uint32 field_tag;                                /* field's tag */
  short field_readcount;                           /* read count/TIFF_VARIABLE/TIFF_SPP */
  short field_writecount;                          /* write count/TIFF_VARIABLE */
  TIFFDataType field_type;                         /* type of associated data */
  uint32 reserved;                                 /* reserved for future extension */
  ITK_TIFF_MOC_TIFFSetGetFieldType set_field_type; /* type to be passed to TIFFSetField */
  ITK_TIFF_MOC_TIFFSetGetFieldType get_field_type; /* type to be passed to TIFFGetField */
  unsigned short field_bit;                        /* bit in fieldsset bit vector */
  unsigned char field_oktochange;                  /* if true, can change while writing */
  unsigned char field_passcount;                   /* if true, pass dir count on set */
  char * field_name;                               /* ASCII name */
  TIFFFieldArray * field_subfields;                /* if field points to child ifds, child ifd field definition array */
};
namespace itk
{
#  define itkTIFFFieldName(TIFFField) ((TIFFField)->field_name)
#  define itkTIFFFieldReadCount(TIFFField) ((TIFFField)->field_readcount)
#  define itkTIFFFieldPassCount(TIFFField) ((TIFFField)->field_passcount)
#  define itkTIFFFieldDataType(TIFFField) ((TIFFField)->field_type)
#  define itkTIFFField TIFFField
#else // libtiff version 3
#  define itkTIFFFieldName(TIFFField) ((TIFFField)->field_name)
#  define itkTIFFFieldReadCount(TIFFFieldInfo) ((TIFFFieldInfo)->field_readcount)
#  define itkTIFFFieldPassCount(TIFFFieldInfo) ((TIFFFieldInfo)->field_passcount)
#  define itkTIFFFieldDataType(TIFFFieldInfo) ((TIFFFieldInfo)->field_type)
#  define itkTIFFField TIFFFieldInfo
#endif

namespace
{
size_t
itkTIFFDataSize(TIFFDataType type)
{
  switch (type)
  {
    case TIFF_BYTE:
    case TIFF_SBYTE:
    case TIFF_ASCII:
    case TIFF_UNDEFINED:
      return 1;
    case TIFF_SHORT:
    case TIFF_SSHORT:
      return 2;
    case TIFF_LONG:
    case TIFF_SLONG:
    case TIFF_FLOAT:
    case TIFF_IFD:
    case TIFF_RATIONAL:
    case TIFF_SRATIONAL:
      return 4;
    case TIFF_DOUBLE:
#ifdef TIFF_INT64_T // detect if libtiff4
    case TIFF_LONG8:
    case TIFF_SLONG8:
    case TIFF_IFD8:
#endif
      return 8;
    default:
      return 0;
  }
}
} // namespace


bool
TIFFImageIO::CanFindTIFFTag(unsigned int t)
{
  // m_InternalImage needs to be valid
  if (!m_InternalImage)
  {
    itkExceptionMacro(<< "Need to call CanReadFile before");
  }

  ttag_t tag = t; // 32bits integer

  const itkTIFFField * fld = TIFFFieldWithTag(m_InternalImage->m_Image, tag);

  if (fld == nullptr)
  {
    return false;
  }
  return true;
}

void *
TIFFImageIO::ReadRawByteFromTag(unsigned int t, unsigned int & value_count)
{
  // m_InternalImage needs to be valid
  if (!m_InternalImage)
  {
    itkExceptionMacro(<< "Need to call CanReadFile before");
  }
  ttag_t tag = t;
  void * raw_data = nullptr;

  const itkTIFFField * fld = TIFFFieldWithTag(m_InternalImage->m_Image, tag);

  if (fld == nullptr)
  {
    itkExceptionMacro(<< "fld is nullptr");
  }

  if (!itkTIFFFieldPassCount(fld))
  {
    return nullptr;
  }

  int ret = 0;
  if (itkTIFFFieldReadCount(fld) == TIFF_VARIABLE2)
  {
    uint32_t cnt;
    ret = TIFFGetField(m_InternalImage->m_Image, tag, &cnt, &raw_data);
    value_count = cnt;
  }
  else if (itkTIFFFieldReadCount(fld) == TIFF_VARIABLE)
  {
    uint16 cnt;
    ret = TIFFGetField(m_InternalImage->m_Image, tag, &cnt, &raw_data);
    value_count = cnt;
  }

  if (ret != 1)
  {
    itkExceptionMacro(<< "Tag cannot be found");
  }
  else
  {
    if (itkTIFFFieldDataType(fld) != TIFF_BYTE)
    {
      itkExceptionMacro(<< "Tag is not of type TIFF_BYTE");
    }
  }

  return raw_data;
}

void
TIFFImageIO::PopulateColorPalette()
{
  if (m_TotalColors > 0)
  {
    // if a palette have been found store it in m_ColorPalette
    m_ColorPalette.resize(m_TotalColors);
    for (uint64_t cc = 0; cc < m_TotalColors; ++cc)
    {
      uint16_t red, green, blue;
      this->GetColor(cc, &red, &green, &blue);

      RGBPixelType p;
      p.SetRed(red);
      p.SetGreen(green);
      p.SetBlue(blue);
      m_ColorPalette[cc] = p;
    }
  }
  else
  {
    // otherwise make sure that the stored palette is empty in the case it was already set
    m_ColorPalette.resize(0);
  }
}

void
TIFFImageIO::AllocateTiffPalette(uint16_t bps)
{
  m_ColorRed = nullptr;
  m_ColorGreen = nullptr;
  m_ColorBlue = nullptr;

  // bpp is 16 at maximum for palette image
  tmsize_t array_size = tmsize_t{ 1 } << bps * sizeof(uint16);
  m_ColorRed = static_cast<uint16 *>(_TIFFmalloc(array_size));
  if (!m_ColorRed)
  {
    _TIFFfree(m_ColorRed);
    itkExceptionMacro("Can't allocate space for Red channel of component tables.");
  }
  m_ColorGreen = static_cast<uint16 *>(_TIFFmalloc(array_size));
  if (!m_ColorGreen)
  {
    _TIFFfree(m_ColorRed);
    _TIFFfree(m_ColorGreen);
    itkExceptionMacro("Can't allocate space for Green channel of component tables.");
  }
  m_ColorBlue = static_cast<uint16 *>(_TIFFmalloc(array_size));
  if (!m_ColorBlue)
  {
    _TIFFfree(m_ColorRed);
    _TIFFfree(m_ColorGreen);
    _TIFFfree(m_ColorBlue);
    itkExceptionMacro("Can't allocate space for Blue channel of component tables.");
  }
  // TIFF palette length is fixed for a given bpp
  uint64 TIFFPaletteLength = uint64{ 1 } << bps;
  for (size_t i = 0; i < TIFFPaletteLength; ++i)
  {
    if (i < m_ColorPalette.size())
    {
      m_ColorRed[i] = m_ColorPalette[i].GetRed();
      m_ColorGreen[i] = m_ColorPalette[i].GetGreen();
      m_ColorBlue[i] = m_ColorPalette[i].GetBlue();
    }
    else
    {
      // default color is black
      m_ColorRed[i] = 0;
      m_ColorGreen[i] = 0;
      m_ColorBlue[i] = 0;
    }
  }
}

void
TIFFImageIO::ReadTIFFTags()
{
  // This method reads the custom (and ascii baseline tags), and
  // places them into the meta-data dictionary

  // this method is based on libtiff's PrintDirectory method used by
  // the tiffinfo tool

  MetaDataDictionary & dict = this->GetMetaDataDictionary();

  void * raw_data = nullptr;
  bool   mem_alloc = false;

  const int tagCount = TIFFGetTagListCount(m_InternalImage->m_Image);

  this->InitializeColors();
  this->PopulateColorPalette();

  for (int i = 0; i < tagCount; ++i)
  {

    // clean up allocation from prior iteration
    if (mem_alloc)
    {
      _TIFFfree(raw_data);
      mem_alloc = false;
    }
    raw_data = nullptr;

    uint32 tag = TIFFGetTagListEntry(m_InternalImage->m_Image, i);

    const itkTIFFField * field = TIFFFieldWithTag(m_InternalImage->m_Image, tag);

    if (field == nullptr)
    {
      continue;
    }


    const char * field_name = itkTIFFFieldName(field);
    int          value_count = 0;


    const int read_count = itkTIFFFieldReadCount(field);

    // check if tag required count argument with GetField
    if (itkTIFFFieldPassCount(field))
    {
      if (read_count == TIFF_VARIABLE2)
      {
        uint32_t cnt;
        if (TIFFGetField(m_InternalImage->m_Image, tag, &cnt, &raw_data) != 1)
        {
          continue;
        }
        value_count = static_cast<int>(cnt);
      }
      else if (read_count == TIFF_VARIABLE)
      {
        uint16 cnt;
        if (TIFFGetField(m_InternalImage->m_Image, tag, &cnt, &raw_data) != 1)
        {
          continue;
        }
        value_count = cnt;
      }
    }
    else
    {
      if (read_count == TIFF_VARIABLE || read_count == TIFF_VARIABLE2)
      {
        value_count = 1;
      }
      else if (read_count == TIFF_SPP)
      {
        value_count = m_InternalImage->m_SamplesPerPixel;
      }
      else
      {
        value_count = read_count;
      }

      if (itkTIFFFieldDataType(field) == TIFF_ASCII || read_count == TIFF_VARIABLE || read_count == TIFF_VARIABLE2 ||
          read_count == TIFF_SPP || value_count > 1)
      {
        if (TIFFGetField(m_InternalImage->m_Image, tag, &raw_data) != 1)
        {
          continue;
        }
      }
      else
      {
        const size_t dataSize = itkTIFFDataSize(itkTIFFFieldDataType(field));
        raw_data = _TIFFmalloc(static_cast<tmsize_t>(dataSize * static_cast<size_t>(value_count)));
        mem_alloc = true;
        if (TIFFGetField(m_InternalImage->m_Image, tag, raw_data) != 1)
        {
          continue;
        }
      }
    }

    if (raw_data == nullptr)
    {
      continue;
    }

    itkDebugMacro(<< "TiffInfo tag " << field_name << "(" << tag << "): " << itkTIFFFieldDataType(field) << " "
                  << value_count << " " << raw_data);


#define itkEncapsulate(T1, T2)                                                                                         \
  if (value_count > 1)                                                                                                 \
  {                                                                                                                    \
    auto      v_c = static_cast<size_t>(value_count);                                                                  \
    Array<T1> a(v_c);                                                                                                  \
    for (unsigned int cnt = 0; cnt < v_c; ++cnt)                                                                       \
    {                                                                                                                  \
      a[cnt] = (static_cast<const T2 *>(raw_data))[cnt];                                                               \
    }                                                                                                                  \
    EncapsulateMetaData<itk::Array<T1>>(dict, field_name, a);                                                          \
  }                                                                                                                    \
  else                                                                                                                 \
  {                                                                                                                    \
    EncapsulateMetaData<T1>(dict, field_name, (static_cast<const T2 *>(raw_data))[0]);                                 \
  }

    try
    {
      switch (itkTIFFFieldDataType(field))
      {
        case TIFF_ASCII:
          if (value_count > 1)
          {
            EncapsulateMetaData<std::string>(
              dict, field_name, std::string(static_cast<const char *>(raw_data), static_cast<size_t>(value_count)));
          }
          else
          {
            EncapsulateMetaData<std::string>(dict, field_name, std::string(static_cast<const char *>(raw_data)));
          }
          break;
        case TIFF_BYTE:
          EncapsulateMetaData<Array<char>>(
            dict, field_name, Array<char>(static_cast<const char *>(raw_data), static_cast<size_t>(value_count)));
          break;
        case TIFF_SHORT:
          itkEncapsulate(unsigned short, uint16);
          break;
        case TIFF_LONG:
          EncapsulateMetaData<unsigned int>(dict, field_name, (static_cast<const uint32 *>(raw_data))[0]);
          break;
        case TIFF_SBYTE:
          EncapsulateMetaData<signed char>(dict, field_name, (static_cast<const int8 *>(raw_data))[0]);
          break;
        case TIFF_SSHORT:
          EncapsulateMetaData<short>(dict, field_name, (static_cast<const int16 *>(raw_data))[0]);
          break;
        case TIFF_SLONG:
          itkEncapsulate(int, int32);
          break;
        case TIFF_FLOAT:
          itkEncapsulate(float, float);
          break;
        case TIFF_DOUBLE:
          itkEncapsulate(double, double);
          break;
        case TIFF_IFD:
#ifdef TIFF_INT64_T // detect if libtiff4
        case TIFF_LONG8:
        case TIFF_SLONG8:
        case TIFF_IFD8:
#endif
        case TIFF_RATIONAL:
        case TIFF_SRATIONAL:
        case TIFF_UNDEFINED:
        default:
          itkWarningMacro(<< field_name << " has unsupported data type (" << itkTIFFFieldDataType(field)
                          << ") for meta-data dictionary.");
          break;
      }
    }
    catch (...)
    {
      if (mem_alloc)
      {
        _TIFFfree(raw_data);
        mem_alloc = false;
      }
    }
  }

  if (mem_alloc)
  {
    _TIFFfree(raw_data);
  }
}


void
TIFFImageIO::ReadCurrentPage(void * buffer, size_t pixelOffset)
{
  const uint32 width = m_InternalImage->m_Width;
  const uint32 height = m_InternalImage->m_Height;


  if (!m_InternalImage->CanRead())
  {
    uint32 * tempImage = nullptr;

    if (this->GetNumberOfComponents() == 4 && m_ComponentType == IOComponentEnum::UCHAR)
    {
      tempImage = static_cast<uint32 *>(buffer) + (pixelOffset / 4);
    }
    else
    {
      itkExceptionMacro("Logic Error: Unexpected buffer type!")
    }

    if (!TIFFReadRGBAImageOriented(m_InternalImage->m_Image, width, height, tempImage, ORIENTATION_TOPLEFT, 1))
    {
      itkExceptionMacro(<< "Cannot read TIFF image as a TIFF RGBA image");
    }

    unsigned char * out = static_cast<unsigned char *>(buffer) + pixelOffset;
    RGBAImageToBuffer<unsigned char>(out, tempImage);
  }
  else
  {

    this->InitializeColors();


    if (m_ComponentType == IOComponentEnum::USHORT)
    {
      auto * volume = static_cast<unsigned short *>(buffer);
      volume += pixelOffset;
      this->ReadGenericImage(volume, width, height);
    }
    else if (m_ComponentType == IOComponentEnum::SHORT)
    {
      auto * volume = static_cast<short *>(buffer);
      volume += pixelOffset;
      this->ReadGenericImage(volume, width, height);
    }
    else if (m_ComponentType == IOComponentEnum::CHAR)
    {
      auto * volume = static_cast<char *>(buffer);
      volume += pixelOffset;
      this->ReadGenericImage(volume, width, height);
    }
    else if (m_ComponentType == IOComponentEnum::FLOAT)
    {
      auto * volume = static_cast<float *>(buffer);
      volume += pixelOffset;
      this->ReadGenericImage(volume, width, height);
    }
    else
    {
      auto * volume = static_cast<unsigned char *>(buffer);
      volume += pixelOffset;
      this->ReadGenericImage(volume, width, height);
    }
  }
}

template <typename TComponent>
void
TIFFImageIO::ReadGenericImage(void * _out, unsigned int width, unsigned int height)
{
  using ComponentType = TComponent;

#ifdef TIFF_INT64_T // detect if libtiff4
  uint64_t isize = TIFFScanlineSize64(m_InternalImage->m_Image);
#else
  tsize_t isize = TIFFScanlineSize(m_InternalImage->m_Image);
#endif

  size_t  inc;
  tdata_t buf = _TIFFmalloc(static_cast<tmsize_t>(isize));
  isize /= sizeof(ComponentType);

  auto *          out = static_cast<ComponentType *>(_out);
  ComponentType * image;

  if (m_InternalImage->m_PlanarConfig != PLANARCONFIG_CONTIG && m_InternalImage->m_SamplesPerPixel != 1)
  {
    itkExceptionMacro(<< "This reader can only do PLANARCONFIG_CONTIG or single-component PLANARCONFIG_SEPARATE");
  }

  if (m_InternalImage->m_Orientation != ORIENTATION_TOPLEFT && m_InternalImage->m_Orientation != ORIENTATION_BOTLEFT)
  {
    itkExceptionMacro(<< "This reader can only do ORIENTATION_TOPLEFT and  ORIENTATION_BOTLEFT.");
  }


  switch (this->GetFormat())
  {
    case TIFFImageIO::GRAYSCALE:
    case TIFFImageIO::PALETTE_GRAYSCALE:
      inc = 1;
      break;
    case TIFFImageIO::RGB_:
      inc = m_InternalImage->m_SamplesPerPixel;
      break;
    case TIFFImageIO::PALETTE_RGB:
    {
      if (GetExpandRGBPalette())
      {
        inc = 3;
      }
      else
      {
        inc = 1;
      }
      break;
    }
    default:
      inc = 1;
      break;
  }

  for (uint32 row = 0; row < height; ++row)
  {
    if (TIFFReadScanline(m_InternalImage->m_Image, buf, row, 0) <= 0)
    {
      itkExceptionMacro(<< "Problem reading the row: " << row);
    }

    if (m_InternalImage->m_Orientation == ORIENTATION_TOPLEFT)
    {
      image = out + inc * row * width;
    }
    else // bottom left
    {
      image = out + inc * width * (height - (row + 1));
    }

    switch (this->GetFormat())
    {
      case TIFFImageIO::GRAYSCALE:
        // check inverted
        PutGrayscale<ComponentType>(image, static_cast<ComponentType *>(buf), width, 1, 0, 0);
        break;
      case TIFFImageIO::RGB_:
        PutRGB_<ComponentType>(image, static_cast<ComponentType *>(buf), width, 1, 0, 0);
        break;

      case TIFFImageIO::PALETTE_GRAYSCALE:
        switch (m_InternalImage->m_BitsPerSample)
        {
          case 8:
            PutPaletteGrayscale<ComponentType, unsigned char>(image, static_cast<unsigned char *>(buf), width, 1, 0, 0);
            break;
          case 16:
            PutPaletteGrayscale<ComponentType, unsigned short>(
              image, static_cast<unsigned short *>(buf), width, 1, 0, 0);
            break;
          default:
            itkExceptionMacro(<< "Sorry, can not handle image with " << m_InternalImage->m_BitsPerSample
                              << "-bit samples with palette.");
        }
        break;
      case TIFFImageIO::PALETTE_RGB:
        if (!this->GetIsReadAsScalarPlusPalette())
        {
          switch (m_InternalImage->m_BitsPerSample)
          {
            case 8:
              PutPaletteRGB<ComponentType, unsigned char>(image, static_cast<unsigned char *>(buf), width, 1, 0, 0);
              break;
            case 16:
              PutPaletteRGB<ComponentType, unsigned short>(image, static_cast<unsigned short *>(buf), width, 1, 0, 0);
              break;
            default:
              itkExceptionMacro(<< "Sorry, can not handle image with " << m_InternalImage->m_BitsPerSample
                                << "-bit samples with palette.");
          }
        }
        else
        {
          switch (m_InternalImage->m_BitsPerSample)
          {
            case 8:
              PutPaletteScalar<ComponentType, unsigned char>(image, static_cast<unsigned char *>(buf), width, 1, 0, 0);
              break;
            case 16:
              PutPaletteScalar<ComponentType, unsigned short>(
                image, static_cast<unsigned short *>(buf), width, 1, 0, 0);
              break;
            default:
              itkExceptionMacro(<< "Sorry, can not handle image with " << m_InternalImage->m_BitsPerSample
                                << "-bit samples with palette.");
          }
        }
        break;

      default:
        itkExceptionMacro("Logic Error: Unexpected format!");
    }
  }

  _TIFFfree(buf);
}

// iso component scalar
template <typename TType>
void
TIFFImageIO::PutGrayscale(TType *      to,
                          TType *      from,
                          unsigned int xsize,
                          unsigned int ysize,
                          unsigned int toskew,
                          unsigned int fromskew)
{
  for (unsigned int y = ysize; y-- > 0;)
  {
    std::copy_n(from, xsize, to);
    to += xsize;
    to += toskew;
    from += xsize;
    from += fromskew;
  }
}


// iso component scalar
template <typename TType>
void
TIFFImageIO::PutRGB_(TType *      to,
                     TType *      from,
                     unsigned int xsize,
                     unsigned int ysize,
                     unsigned int toskew,
                     unsigned int fromskew)
{
  const size_t samplesPerPixel = m_InternalImage->m_SamplesPerPixel;
  const size_t linesize = samplesPerPixel * xsize;

  for (unsigned int y = ysize; y-- > 0;)
  {
    std::copy_n(from, linesize, to);

    to += linesize;
    to += toskew;
    from += linesize;
    from += fromskew;
  }
}

template <typename TType, typename TFromType>
void
TIFFImageIO::PutPaletteRGB(TType *      to,
                           TFromType *  from,
                           unsigned int xsize,
                           unsigned int ysize,
                           unsigned int toskew,
                           unsigned int fromskew)
{
  for (unsigned int y = ysize; y-- > 0;)
  {
    for (unsigned int x = xsize; x-- > 0;)
    {
      const TFromType index = (*from) % m_TotalColors;

      const auto red = static_cast<TType>(*(m_ColorRed + index));
      const auto green = static_cast<TType>(*(m_ColorGreen + index));
      const auto blue = static_cast<TType>(*(m_ColorBlue + index));
      *(to) = red;
      *(to + 1) = green;
      *(to + 2) = blue;
      to += 3;
      ++from;
    }
    to += toskew;
    from += fromskew;
  }
}


template <typename TType, typename TFromType>
void
TIFFImageIO::PutPaletteGrayscale(TType *      to,
                                 TFromType *  from,
                                 unsigned int xsize,
                                 unsigned int ysize,
                                 unsigned int toskew,
                                 unsigned int fromskew)
{
  for (unsigned int y = ysize; y-- > 0;)
  {
    for (unsigned int x = xsize; x-- > 0;)
    {
      const TFromType index = *from % m_TotalColors;

      *(to) = static_cast<TType>(*(m_ColorRed + index));

      ++to;
      ++from;
    }
    to += toskew;
    from += fromskew;
  }
}

template <typename TType, typename TFromType>
void
TIFFImageIO::PutPaletteScalar(TType *      to,
                              TFromType *  from,
                              unsigned int xsize,
                              unsigned int ysize,
                              unsigned int toskew,
                              unsigned int fromskew)
{
  for (unsigned int y = ysize; y-- > 0;)
  {
    for (unsigned int x = xsize; x-- > 0;)
    {
      const TFromType index = *from % m_TotalColors;

      *(to) = static_cast<TType>(index);

      ++to;
      ++from;
    }
    to += toskew;
    from += fromskew;
  }
}

template <typename TComponent>
void
TIFFImageIO::RGBAImageToBuffer(void * out, const uint32_t * tempImage)
{
  using ComponentType = TComponent;

  const size_t width = m_InternalImage->m_Width;
  const size_t height = m_InternalImage->m_Height;

  auto * fimage = static_cast<ComponentType *>(out);

  for (size_t yy = 0; yy < height; ++yy)
  {
    for (size_t xx = 0; xx < width; ++xx)
    {
      const auto red = static_cast<ComponentType>(TIFFGetR(*tempImage));
      const auto green = static_cast<ComponentType>(TIFFGetG(*tempImage));
      const auto blue = static_cast<ComponentType>(TIFFGetB(*tempImage));
      const auto alpha = static_cast<ComponentType>(TIFFGetA(*tempImage));

      *(fimage) = red;
      *(fimage + 1) = green;
      *(fimage + 2) = blue;
      *(fimage + 3) = alpha;
      fimage += 4;
      ++tempImage;
    }
  }
}

} // end namespace itk
