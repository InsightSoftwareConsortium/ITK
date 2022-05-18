/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkJPEGImageIO.h"
#include "itksys/SystemTools.hxx"

#include "itk_jpeg.h"
#include <csetjmp>

#define JPEGIO_JPEG_MESSAGES 1

#if (defined JPEGIO_JPEG_MESSAGES && JPEGIO_JPEG_MESSAGES == 1)
#  include <cstdio>
#endif

// create an error handler for jpeg that
// can longjmp out of the jpeg library
struct itk_jpeg_error_mgr
{
  struct jpeg_error_mgr pub;           /* "public" fields */
  jmp_buf               setjmp_buffer; /* for return to caller */
};

extern "C"
{
  METHODDEF(void) itk_jpeg_error_exit(j_common_ptr cinfo)
  {
    /* cinfo->err really points to an itk_jpeg_error_mgr struct, so coerce pointer
     */
    itk_jpeg_error_mgr * myerr = (itk_jpeg_error_mgr *)cinfo->err;

    /* Always display the message. */
    /* We could postpone this until after returning, if we chose. */
    (*cinfo->err->output_message)(cinfo);

    /* Return control to the setjmp point */
    longjmp(myerr->setjmp_buffer, 1);
  }

  METHODDEF(void) itk_jpeg_output_message(j_common_ptr cinfo)
  {
#if (defined JPEGIO_JPEG_MESSAGES && JPEGIO_JPEG_MESSAGES == 1)
    char buffer[JMSG_LENGTH_MAX + 1];
    (*cinfo->err->format_message)(cinfo, buffer);
    printf("%s\n", buffer);
#else
    (void)cinfo;
#endif
  }
}

namespace itk
{

// simple class to call fopen on construct and
// fclose on destruct
class JPEGFileWrapper
{
public:
  JPEGFileWrapper(const char * const fname, const char * const openMode)
    : m_FilePointer(nullptr)
  {
    m_FilePointer = fopen(fname, openMode);
  }

  virtual ~JPEGFileWrapper()
  {
    if (m_FilePointer != nullptr)
    {
      fclose(m_FilePointer);
    }
  }

  FILE * volatile m_FilePointer;
};

bool
JPEGImageIO::CanReadFile(const char * file)
{
  // First check the extension
  std::string filename = file;

  if (filename.empty())
  {
    itkDebugMacro(<< "No filename specified.");
    return false;
  }

  bool extensionFound = this->HasSupportedReadExtension(file, false);

  if (!extensionFound)
  {
    itkDebugMacro(<< "The filename extension is not recognized");
    return false;
  }

  // Now check the file header
  JPEGFileWrapper JPEGfp(file, "rb");
  if (JPEGfp.m_FilePointer == nullptr)
  {
    return false;
  }

  // read the first two bytes
  unsigned char magic[2];
  auto          n = static_cast<int>(fread(magic, sizeof(magic), 1, JPEGfp.m_FilePointer));
  if (n != 1)
  {
    return false;
  }

  // check for the magic stuff:
  // 0xFF followed by 0xD8
  if (magic[0] != 0xFF || magic[1] != 0xD8)
  {
    return false;
  }
  // go back to the start of the file
  fseek(JPEGfp.m_FilePointer, 0, SEEK_SET);
  // magic number is ok, try and read the header
  struct itk_jpeg_error_mgr     jerr;
  struct jpeg_decompress_struct cinfo;
  cinfo.err = jpeg_std_error(&jerr.pub);
  // for any jpeg error call itk_jpeg_error_exit
  jerr.pub.error_exit = itk_jpeg_error_exit;
  // for any output message call itk_jpeg_output_message
  jerr.pub.output_message = itk_jpeg_output_message;
  // set the jump point
  if (setjmp(jerr.setjmp_buffer))
  {
    jpeg_destroy_decompress(&cinfo);
    return false;
  }
  // initialize the JPEG decompression object
  jpeg_create_decompress(&cinfo);
  // specify data source
  jpeg_stdio_src(&cinfo, JPEGfp.m_FilePointer);
  // read file parameters
  jpeg_read_header(&cinfo, TRUE);

  // if no errors have occurred yet, then it must be jpeg
  jpeg_destroy_decompress(&cinfo);

  return true;
}

void
JPEGImageIO::ReadVolume(void *)
{}

void
JPEGImageIO::Read(void * buffer)
{
  // use this class so return will call close
  JPEGFileWrapper JPEGfp(this->GetFileName(), "rb");
  FILE *          fp = JPEGfp.m_FilePointer;
  if (!fp)
  {
    itkExceptionMacro("Error JPEGImageIO could not open file: " << this->GetFileName() << std::endl
                                                                << "Reason: "
                                                                << itksys::SystemTools::GetLastSystemError());
  }

  // create jpeg decompression object and error handler
  struct jpeg_decompress_struct cinfo;
  struct itk_jpeg_error_mgr     jerr;

  cinfo.err = jpeg_std_error(&jerr.pub);
  // for any jpeg error call itk_jpeg_error_exit
  jerr.pub.error_exit = itk_jpeg_error_exit;
  // for any output message call itk_jpeg_output_message
  jerr.pub.output_message = itk_jpeg_output_message;

  if (setjmp(jerr.setjmp_buffer))
  {
    jpeg_destroy_decompress(&cinfo);
    itkExceptionMacro("JPEG fatal error in the file: " << this->GetFileName());
  }

  jpeg_create_decompress(&cinfo);

  // set the source file
  jpeg_stdio_src(&cinfo, fp);

  // read the header
  jpeg_read_header(&cinfo, TRUE);

  // jpeg_calc_output_dimensions used in ReadImageInformation,
  // so has to be used here too
  jpeg_calc_output_dimensions(&cinfo);

  // prepare to read the bulk data
  jpeg_start_decompress(&cinfo);

  auto * volatile row_pointers = new JSAMPROW[cinfo.output_height];

  {
    const auto rowbytes = cinfo.output_width * this->GetNumberOfComponents();
    auto *     tempImage = static_cast<JSAMPLE *>(buffer);
    for (size_t ui = 0; ui < cinfo.output_height; ++ui)
    {
      row_pointers[ui] = tempImage + rowbytes * ui;
    }
  }

  // read the bulk data
  if (m_IsCMYK && m_CMYKtoRGB)
  {
    JSAMPROW buf1[1];
    auto * volatile buf0 = new JSAMPLE[cinfo.output_width * 4];
    buf1[0] = buf0;

    while (cinfo.output_scanline < cinfo.output_height)
    {
      if (setjmp(jerr.setjmp_buffer))
      {
        jpeg_destroy_decompress(&cinfo);
        delete[] row_pointers;
        delete[] buf0;
        itkWarningMacro(<< "JPEG error in the file " << this->GetFileName());
        return;
      }

      jpeg_read_scanlines(&cinfo, buf1, 1);

      if (cinfo.output_scanline > 0)
      {
        const size_t scanline = cinfo.output_scanline - 1;
        for (size_t i = 0; i < cinfo.output_width; ++i)
        {
          // Gimp approach: the following code assumes inverted CMYK values,
          // even when an APP14 marker doesn't exist. This is the behavior
          // of recent versions of PhotoShop as well.
          const float K = buf1[0][4 * i + 3];
          row_pointers[scanline][3 * i + 0] = static_cast<JSAMPLE>(buf1[0][4 * i + 0] * K / 255.0f);
          row_pointers[scanline][3 * i + 1] = static_cast<JSAMPLE>(buf1[0][4 * i + 1] * K / 255.0f);
          row_pointers[scanline][3 * i + 2] = static_cast<JSAMPLE>(buf1[0][4 * i + 2] * K / 255.0f);
        }
      }
    }

    // finish the decompression step
    jpeg_finish_decompress(&cinfo);

    // destroy the decompression object
    jpeg_destroy_decompress(&cinfo);

    delete[] buf0;
  }
  else
  {
    while (cinfo.output_scanline < cinfo.output_height)
    {
      if (setjmp(jerr.setjmp_buffer))
      {
        jpeg_destroy_decompress(&cinfo);
        delete[] row_pointers;
        itkWarningMacro(<< "JPEG error in the file " << this->GetFileName());
        return;
      }

      jpeg_read_scanlines(&cinfo, &row_pointers[cinfo.output_scanline], cinfo.output_height - cinfo.output_scanline);
    }

    // finish the decompression step
    jpeg_finish_decompress(&cinfo);

    // destroy the decompression object
    jpeg_destroy_decompress(&cinfo);
  }

  delete[] row_pointers;
}

JPEGImageIO::JPEGImageIO()
{
  this->SetNumberOfDimensions(2);

  // 12bits is not working right now, but this should be doable
#if BITS_IN_JSAMPLE == 8
  m_ComponentType = IOComponentEnum::UCHAR;
#else
#  error "JPEG files with more than 8 bits per sample are not supported"
#endif

  m_UseCompression = false;
  this->Self::SetQuality(95);

  m_Spacing[0] = 1.0;
  m_Spacing[1] = 1.0;

  m_Origin[0] = 0.0;
  m_Origin[1] = 0.0;

  const char * extensions[] = { ".jpg", ".JPG", ".jpeg", ".JPEG" };

  for (auto ext : extensions)
  {
    this->AddSupportedWriteExtension(ext);
    this->AddSupportedReadExtension(ext);
  }
}

JPEGImageIO::~JPEGImageIO() = default;

void
JPEGImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Quality : " << this->GetQuality() << "\n";
  os << indent << "Progressive : " << m_Progressive << "\n";
  os << indent << "CMYK to RGB : " << m_CMYKtoRGB << "\n";
  os << indent << "IsCMYK : " << m_IsCMYK << "\n";
}

void
JPEGImageIO::ReadImageInformation()
{
  m_Spacing[0] = 1.0; // We'll look for JPEG pixel size information later,
  m_Spacing[1] = 1.0; // but set the defaults now

  m_Origin[0] = 0.0;
  m_Origin[1] = 0.0;

  m_IsCMYK = false;

  // use this class so return will call close
  JPEGFileWrapper JPEGfp(m_FileName.c_str(), "rb");
  FILE *          fp = JPEGfp.m_FilePointer;
  if (!fp)
  {
    itkExceptionMacro("Error JPEGImageIO could not open file: " << this->GetFileName() << std::endl
                                                                << "Reason: "
                                                                << itksys::SystemTools::GetLastSystemError());
  }

  // create jpeg decompression object and error handler
  struct jpeg_decompress_struct cinfo;
  struct itk_jpeg_error_mgr     jerr;

  cinfo.err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = itk_jpeg_error_exit;
  if (setjmp(jerr.setjmp_buffer))
  {
    jpeg_destroy_decompress(&cinfo);
    itkExceptionMacro("Error JPEGImageIO could not open file: " << this->GetFileName());
  }
  jpeg_create_decompress(&cinfo);

  // set the source file
  jpeg_stdio_src(&cinfo, fp);

  // read the header
  jpeg_read_header(&cinfo, TRUE);

  // jpeg_calc_output_dimensions to calculate cinfo.output_components
  jpeg_calc_output_dimensions(&cinfo);
  if (sizeof(void *) < 8 && (static_cast<unsigned long long>(cinfo.output_width) * cinfo.output_height *
                             cinfo.output_components) > 0xffffffff)
  {
    jpeg_destroy_decompress(&cinfo);
    itkExceptionMacro(<< "JPEG image is too big " << this->GetFileName());
  }

  // pull out the width/height
  this->SetNumberOfDimensions(2);
  m_Dimensions[0] = cinfo.output_width;
  m_Dimensions[1] = cinfo.output_height;

  switch (cinfo.output_components)
  {
    case 1:
      m_PixelType = IOPixelEnum::SCALAR;
      this->SetNumberOfComponents(1);
      break;
    case 3:
      m_PixelType = IOPixelEnum::RGB;
      this->SetNumberOfComponents(3);
      break;
    case 4:
      if (cinfo.out_color_space == JCS_CMYK)
      {
        m_IsCMYK = true;
        if (m_CMYKtoRGB)
        {
          m_PixelType = IOPixelEnum::RGB;
          this->SetNumberOfComponents(3);
        }
        else
        {
          m_PixelType = IOPixelEnum::VECTOR;
          this->SetNumberOfComponents(4);
        }
        break;
      }
      // else fallthrough
    default:
      m_PixelType = IOPixelEnum::VECTOR;
      this->SetNumberOfComponents(cinfo.output_components);
      itkWarningMacro("JPEG image may be opened incorrectly");
      break;
  }

  // If we have some spacing information we use it
  if (cinfo.density_unit > 0 && cinfo.X_density > 0 && cinfo.Y_density > 0)
  {
    if (cinfo.density_unit == 1) // inches
    {
      m_Spacing[0] = 25.4 / cinfo.X_density;
      m_Spacing[1] = 25.4 / cinfo.Y_density;
    }
    else if (cinfo.density_unit == 2) // cm
    {
      m_Spacing[0] = 10.0 / cinfo.X_density;
      m_Spacing[1] = 10.0 / cinfo.Y_density;
    }
  }

  // close the file
  jpeg_destroy_decompress(&cinfo);
}

bool
JPEGImageIO::CanWriteFile(const char * name)
{
  std::string filename = name;

  if (filename.empty())
  {
    return false;
  }

  return this->HasSupportedWriteExtension(name, false);
}

void
JPEGImageIO::WriteImageInformation()
{}

void
JPEGImageIO::Write(const void * buffer)
{
  // the IORegion is not required to be set so we must use GetNumberOfDimensions
  if (this->GetNumberOfDimensions() != 2)
  {
    itkExceptionMacro(<< "JPEG Writer can only write 2-dimensional images");
  }

  if (this->GetComponentType() != IOComponentEnum::UCHAR)
  {
    itkExceptionMacro(<< "JPEG supports unsigned char only");
  }

  this->WriteSlice(m_FileName, buffer);
}

void
JPEGImageIO::WriteSlice(std::string & fileName, const void * const buffer)
{
  // use this class so return will call close
  JPEGFileWrapper JPEGfp(fileName.c_str(), "wb");
  FILE *          fp = JPEGfp.m_FilePointer;
  if (!fp)
  {
    itkExceptionMacro("Unable to open file " << fileName << " for writing." << std::endl
                                             << "Reason: " << itksys::SystemTools::GetLastSystemError());
  }

  // set the information about image
  const SizeValueType width = m_Dimensions[0];
  const SizeValueType height = m_Dimensions[1];
  if (width > JPEG_MAX_DIMENSION || height > JPEG_MAX_DIMENSION)
  {
    itkExceptionMacro(<< "JPEG: image is too large");
  }
  const volatile int num_comp = this->GetNumberOfComponents();
  if (num_comp > MAX_COMPONENTS)
  {
    itkExceptionMacro(<< "JPEG: too many components");
  }

  auto * volatile row_pointers = new JSAMPROW[height];

  struct itk_jpeg_error_mgr   jerr;
  struct jpeg_compress_struct cinfo;
  cinfo.err = jpeg_std_error(&jerr.pub);

  // set the jump point
  if (setjmp(jerr.setjmp_buffer))
  {
    jpeg_destroy_compress(&cinfo);
    delete[] row_pointers;
    itkExceptionMacro(<< "JPEG error, failed to write " << fileName);
  }

  jpeg_create_compress(&cinfo);

  // set the destination file
  jpeg_stdio_dest(&cinfo, fp);

  cinfo.image_width = static_cast<JDIMENSION>(width);
  cinfo.image_height = static_cast<JDIMENSION>(height);
  cinfo.input_components = num_comp;

  switch (cinfo.input_components)
  {
    case 1:
      cinfo.in_color_space = JCS_GRAYSCALE;
      break;
    case 3:
      cinfo.in_color_space = JCS_RGB;
      break;
    default:
      cinfo.in_color_space = JCS_UNKNOWN;
      itkWarningMacro("Image may be saved incorrectly as JPEG");
      break;
  }

  // set the compression parameters
  jpeg_set_defaults(&cinfo);
  jpeg_set_quality(&cinfo, this->GetQuality(), TRUE);
  if (m_Progressive)
  {
    jpeg_simple_progression(&cinfo);
  }

  if (m_Spacing[0] > 0 && m_Spacing[1] > 0)
  {
    // store the spacing information as pixels per inch or cm, depending on which option
    // retains as much precision as possible
    std::vector<UINT16> densityPerInch(2);
    densityPerInch[0] = static_cast<UINT16>(25.4 / m_Spacing[0] + 0.5);
    densityPerInch[1] = static_cast<UINT16>(25.4 / m_Spacing[1] + 0.5);

    std::vector<UINT16> densityPerCm(2);
    densityPerCm[0] = static_cast<UINT16>(10.0 / m_Spacing[0] + 0.5);
    densityPerCm[1] = static_cast<UINT16>(10.0 / m_Spacing[1] + 0.5);

    if (itk::Math::abs(25.4 / m_Spacing[0] - densityPerInch[0]) +
          itk::Math::abs(25.4 / m_Spacing[1] - densityPerInch[1]) <=
        itk::Math::abs(10.0 / m_Spacing[0] - densityPerCm[0]) + itk::Math::abs(10.0 / m_Spacing[1] - densityPerCm[1]))
    {
      cinfo.density_unit = 1;
      cinfo.X_density = densityPerInch[0];
      cinfo.Y_density = densityPerInch[1];
    }
    else
    {
      cinfo.density_unit = 0;
      cinfo.X_density = densityPerCm[0];
      cinfo.Y_density = densityPerCm[1];
    }
  }

  // start compression
  jpeg_start_compress(&cinfo, TRUE);

  {
    const auto * ptr = static_cast<const JSAMPLE *>(buffer);
    const auto   rowbytes = num_comp * width;
    for (size_t ui = 0; ui < height; ++ui)
    {
      row_pointers[ui] = const_cast<JSAMPLE *>(ptr) + rowbytes * ui;
    }
  }
  while (cinfo.next_scanline < cinfo.image_height)
  {
    // usually one iteration
    const auto remaining = cinfo.image_height - cinfo.next_scanline;
    jpeg_write_scanlines(&cinfo, &row_pointers[cinfo.next_scanline], remaining);
  }

  // finish the compression
  jpeg_finish_compress(&cinfo);

  // clean up
  jpeg_destroy_compress(&cinfo);
  delete[] row_pointers;
}
} // end namespace itk
