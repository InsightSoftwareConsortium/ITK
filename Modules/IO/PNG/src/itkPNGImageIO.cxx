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
#include "itkPNGImageIO.h"
#include "itk_png.h"
#include "itksys/SystemTools.hxx"
#include "itkMakeUniqueForOverwrite.h"
#include <string>
#include <csetjmp>

extern "C"
{
  void
  itkPNGWriteErrorFunction(png_structp png_ptr, png_const_charp itkNotUsed(error_msg))
  {
    longjmp(png_jmpbuf(png_ptr), 1);
  }

  void
  itkPNGWriteWarningFunction(png_structp itkNotUsed(png_ptr), png_const_charp itkNotUsed(warning_msg))
  {}
}

namespace itk
{

// simple class to call fopen on construct and
// fclose on destruct
class PNGFileWrapper
{
public:
  PNGFileWrapper(const char * const fname, const char * const openMode)
    : m_FilePointer(nullptr)
  {
    m_FilePointer = fopen(fname, openMode);
  }

  virtual ~PNGFileWrapper()
  {
    if (m_FilePointer)
    {
      fclose(m_FilePointer);
    }
  }

  FILE * volatile m_FilePointer;
};

bool
PNGImageIO::CanReadFile(const char * file)
{
  // First check the filename
  std::string filename = file;

  if (filename.empty())
  {
    itkDebugMacro(<< "No filename specified.");
    return false;
  }

  // Now check the file header
  PNGFileWrapper pngfp(file, "rb");
  if (pngfp.m_FilePointer == nullptr)
  {
    return false;
  }
  unsigned char header[8];
  size_t        temp = fread(header, 1, 8, pngfp.m_FilePointer);
  if (temp != 8)
  {
    return false;
  }
  bool is_png = !png_sig_cmp(header, 0, 8);
  if (!is_png)
  {
    return false;
  }
  png_structp png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, (png_voidp) nullptr, nullptr, nullptr);
  if (!png_ptr)
  {
    return false;
  }

  png_infop info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr)
  {
    png_destroy_read_struct(&png_ptr, (png_infopp) nullptr, (png_infopp) nullptr);
    return false;
  }

  png_infop end_info = png_create_info_struct(png_ptr);
  if (!end_info)
  {
    png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp) nullptr);
    return false;
  }
  png_destroy_read_struct(&png_ptr, &info_ptr, &end_info);

  return true;
}

void
PNGImageIO::ReadVolume(void *)
{}

void
PNGImageIO::Read(void * buffer)
{
  itkDebugMacro("Read: file dimensions = " << this->GetNumberOfDimensions());
  // use this class so return will call close
  PNGFileWrapper pngfp(this->GetFileName(), "rb");
  FILE *         fp = pngfp.m_FilePointer;
  if (!fp)
  {
    itkExceptionMacro("PNGImageIO could not open file: " << this->GetFileName() << " for reading." << std::endl
                                                         << "Reason: " << itksys::SystemTools::GetLastSystemError());
  }
  unsigned char header[8];
  size_t        temp = fread(header, 1, 8, fp);
  if (temp != 8)
  {
    itkExceptionMacro("PNGImageIO failed to read header for file: " << this->GetFileName() << std::endl
                                                                    << "Reason: fread read only " << temp
                                                                    << " instead of 8");
  }

  bool is_png = !png_sig_cmp(header, 0, 8);
  if (!is_png)
  {
    itkExceptionMacro("File is not png type: " << this->GetFileName());
  }
  png_structp png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, (png_voidp) nullptr, nullptr, nullptr);
  if (!png_ptr)
  {
    itkExceptionMacro("File is not png type" << this->GetFileName());
  }

  png_infop info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr)
  {
    png_destroy_read_struct(&png_ptr, (png_infopp) nullptr, (png_infopp) nullptr);
    itkExceptionMacro("File is not png type " << this->GetFileName());
  }

  png_infop end_info = png_create_info_struct(png_ptr);
  if (!end_info)
  {
    png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp) nullptr);
    itkExceptionMacro("File is not png type " << this->GetFileName());
  }

  if (setjmp(png_jmpbuf(png_ptr)))
  {
    png_destroy_read_struct(&png_ptr, &info_ptr, &end_info);
    itkExceptionMacro("PNG critical error in " << this->GetFileName());
  }

  png_init_io(png_ptr, fp);
  png_set_sig_bytes(png_ptr, 8);

  png_read_info(png_ptr, info_ptr);

  png_uint_32 width, height;
  int         bitDepth, colorType, interlaceType;
  int         compression_type, filter_method;
  png_get_IHDR(
    png_ptr, info_ptr, &width, &height, &bitDepth, &colorType, &interlaceType, &compression_type, &filter_method);

  if (colorType == PNG_COLOR_TYPE_PALETTE)
  {
    if (this->GetExpandRGBPalette())
    { // convert palette to RGB
      png_set_palette_to_rgb(png_ptr);
    }
    else
    {
      // unpack the pixels
      png_set_packing(png_ptr);
    }
  }

  // minimum of a byte per pixel
  if (colorType == PNG_COLOR_TYPE_GRAY && bitDepth < 8)
  {
#if (PNG_LIBPNG_VER_MAJOR < 2 && PNG_LIBPNG_VER_MINOR < 4)
    png_set_gray_1_2_4_to_8(png_ptr);
#else
    png_set_expand_gray_1_2_4_to_8(png_ptr);
#endif
  }

  // add alpha if any alpha found
  if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS))
  {
    png_set_tRNS_to_alpha(png_ptr);
  }

  if (bitDepth > 8)
  {
#ifndef ITK_WORDS_BIGENDIAN
    png_set_swap(png_ptr);
#endif
  }

#if (PNG_LIBPNG_VER_MAJOR < 2 && PNG_LIBPNG_VER_MINOR < 5)
  if (info_ptr->valid & PNG_INFO_sBIT)
  {
    png_set_shift(png_ptr, &(info_ptr->sig_bit));
  }
#else
  if (png_get_valid(png_ptr, info_ptr, PNG_INFO_sBIT))
  {
    png_color_8p bits;
    png_get_sBIT(png_ptr, info_ptr, &bits);
    png_set_shift(png_ptr, bits);
  }
#endif
  // have libpng handle interlacing
  // int number_of_passes = png_set_interlace_handling(png_ptr);
  // update the info now that we have defined the filters
  png_read_update_info(png_ptr, info_ptr);

  auto       rowbytes = static_cast<SizeValueType>(png_get_rowbytes(png_ptr, info_ptr));
  auto *     tempImage = static_cast<unsigned char *>(buffer);
  const auto row_pointers = make_unique_for_overwrite<png_bytep[]>(height);
  for (unsigned int ui = 0; ui < height; ++ui)
  {
    row_pointers[ui] = tempImage + rowbytes * ui;
  }

  png_set_error_fn(png_ptr, (png_voidp) nullptr, itkPNGWriteErrorFunction, itkPNGWriteWarningFunction);
  if (setjmp(png_jmpbuf(png_ptr)))
  {
    png_destroy_read_struct(&png_ptr, &info_ptr, &end_info);
    itkExceptionMacro("Error while reading file: " << this->GetFileName() << std::endl);
  }
  png_read_image(png_ptr, row_pointers.get());
  // close the file
  png_read_end(png_ptr, nullptr);
  png_destroy_read_struct(&png_ptr, &info_ptr, &end_info);
}

PNGImageIO::PNGImageIO()
  : m_ColorPalette(0) // palette has no elements by default
{
  this->SetNumberOfDimensions(2);

  m_ComponentType = IOComponentEnum::UCHAR;
  m_PixelType = IOPixelEnum::SCALAR;

  this->Self::UseCompressionOff();

  // Determines the level of compression for written files.
  // Range 0-9; 0 = none, 9 = maximum , default = 4 */
  this->Self::SetMaximumCompressionLevel(9);
  this->Self::SetCompressionLevel(4);


  m_Spacing[0] = 1.0;
  m_Spacing[1] = 1.0;

  m_Origin[0] = 0.0;
  m_Origin[1] = 0.0;

  constexpr const char * const extensions[] = { ".png", ".PNG" };

  for (auto ext : extensions)
  {
    this->AddSupportedWriteExtension(ext);
    this->AddSupportedReadExtension(ext);
  }
}

PNGImageIO::~PNGImageIO() = default;

void
PNGImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "CompressionLevel: " << this->GetCompressionLevel() << std::endl;
  if (!m_ColorPalette.empty())
  {
    os << indent << "ColorPalette:" << std::endl;
    for (unsigned int i = 0; i < m_ColorPalette.size(); ++i)
    {
      os << indent << "[" << i << "]" << itk::NumericTraits<PaletteType::value_type>::PrintType(m_ColorPalette[i])
         << std::endl;
    }
  }
}

void
PNGImageIO::ReadImageInformation()
{
  m_Spacing[0] = 1.0; // We'll look for PNG pixel size information later,
  m_Spacing[1] = 1.0; // but set the defaults now

  m_Origin[0] = 0.0;
  m_Origin[1] = 0.0;

  // use this class so return will call close
  PNGFileWrapper pngfp(m_FileName.c_str(), "rb");
  FILE *         fp = pngfp.m_FilePointer;
  if (!fp)
  {
    return;
  }
  unsigned char header[8];
  size_t        temp = fread(header, 1, 8, fp);
  if (temp != 8)
  {
    itkExceptionMacro("PNGImageIO failed to read header for file: " << this->GetFileName() << std::endl
                                                                    << "Reason: fread read only " << temp
                                                                    << " instead of 8");
  }

  bool is_png = !png_sig_cmp(header, 0, 8);
  if (!is_png)
  {
    return;
  }
  png_structp png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, (png_voidp) nullptr, nullptr, nullptr);
  if (!png_ptr)
  {
    return;
  }

  png_infop info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr)
  {
    png_destroy_read_struct(&png_ptr, (png_infopp) nullptr, (png_infopp) nullptr);
    return;
  }

  png_infop end_info = png_create_info_struct(png_ptr);
  if (!end_info)
  {
    png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp) nullptr);
    return;
  }

  if (setjmp(png_jmpbuf(png_ptr)))
  {
    png_destroy_read_struct(&png_ptr, &info_ptr, &end_info);
    itkExceptionMacro("PNG critical error in " << this->GetFileName());
  }

  png_init_io(png_ptr, fp);
  png_set_sig_bytes(png_ptr, 8);

  png_read_info(png_ptr, info_ptr);

  png_uint_32 width, height;
  int         bitDepth, colorType, interlaceType;
  int         compression_type, filter_method;
  png_get_IHDR(
    png_ptr, info_ptr, &width, &height, &bitDepth, &colorType, &interlaceType, &compression_type, &filter_method);

  m_IsReadAsScalarPlusPalette = false;
  if (colorType == PNG_COLOR_TYPE_PALETTE)
  {
    if (m_ExpandRGBPalette)
    { // convert palettes to RGB
      png_set_palette_to_rgb(png_ptr);
    }
    else
    {
      // Unpack the pixels
      png_set_packing(png_ptr);

      m_IsReadAsScalarPlusPalette = true;

      png_colorp palette;
      int        num_entry;
      png_get_PLTE(png_ptr, info_ptr, &palette, &num_entry);

      if (num_entry < 0)
      {
        num_entry = 0;
      }
      auto num_entryI(static_cast<size_t>(num_entry));

      m_ColorPalette.resize(num_entryI);
      for (size_t c = 0; c < num_entryI; ++c)
      {
        RGBPixelType p;
        p.SetRed(palette[c].red);
        p.SetGreen(palette[c].green);
        p.SetBlue(palette[c].blue);
        m_ColorPalette[c] = p;
      }
    }
  }
  if (!m_IsReadAsScalarPlusPalette)
  {
    // make sure not palette is left
    m_ColorPalette.resize(0);
  }

  // minimum of a byte per pixel
  if (colorType == PNG_COLOR_TYPE_GRAY && bitDepth < 8)
  {
#if (PNG_LIBPNG_VER_MAJOR < 2 && PNG_LIBPNG_VER_MINOR < 4)
    png_set_gray_1_2_4_to_8(png_ptr);
#else
    png_set_expand_gray_1_2_4_to_8(png_ptr);
#endif
  }

  // add alpha if any alpha found
  if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS))
  {
    png_set_tRNS_to_alpha(png_ptr);
  }

  // update the info now that we have defined the filters
  png_read_update_info(png_ptr, info_ptr);
  this->SetNumberOfDimensions(2);
  m_Dimensions[0] = width;
  m_Dimensions[1] = height;
  if (bitDepth <= 8)
  {
    m_PixelType = IOPixelEnum::SCALAR;
    m_ComponentType = IOComponentEnum::UCHAR;
  }
  else
  {
    m_PixelType = IOPixelEnum::SCALAR;
    m_ComponentType = IOComponentEnum::USHORT;
  }
  this->SetNumberOfComponents(png_get_channels(png_ptr, info_ptr));

  if (this->GetNumberOfComponents() == 3)
  {
    m_PixelType = IOPixelEnum::RGB;
  }
  else if (this->GetNumberOfComponents() == 4)
  {
    m_PixelType = IOPixelEnum::RGBA;
  }

  // see if the PNG file stored spacing information,
  double px_width = 1.0;
  double px_height = 1.0;

#if defined(PNG_sCAL_SUPPORTED) && defined(PNG_FLOATING_POINT_SUPPORTED)
  int units = PNG_SCALE_UNKNOWN;

  if (PNG_INFO_sCAL == png_get_sCAL(png_ptr, info_ptr, &units, &px_width, &px_height) && units == PNG_SCALE_UNKNOWN &&
      (px_width != 1.0 || px_height != 1.0))
  {
    // Only libpng <1.5 can read sCAL with SCALE_UNKNOWN, warn this is
    // not going to be compatible with newer libpngs
    itkWarningMacro("PNG sCAL SCALE_UNKNOWN detected with non-unit spacing. This is no longer supported by libpng. "
                    "Re-saving this file is recommended.");
  }
#endif

  m_Spacing[0] = px_width;
  m_Spacing[1] = px_height;

  // clean up
  png_destroy_read_struct(&png_ptr, &info_ptr, &end_info);
}

bool
PNGImageIO::CanWriteFile(const char * name)
{
  std::string filename = name;

  if (filename.empty())
  {
    return false;
  }

  return this->HasSupportedWriteExtension(name, false);
}

void
PNGImageIO::WriteImageInformation()
{}

void
PNGImageIO::Write(const void * buffer)
{
  this->WriteSlice(m_FileName, buffer);
}

void
PNGImageIO::WriteSlice(const std::string & fileName, const void * const buffer)
{
  // use this class so return will call close
  PNGFileWrapper pngfp(fileName.c_str(), "wb");
  FILE *         fp = pngfp.m_FilePointer;

  if (!fp)
  {
    // IMPORTANT: The itkExceptionMacro() cannot be used here due to a bug in
    // Visual
    //            Studio 7.1 in release mode. That compiler will corrupt the
    // RTTI type
    //            of the Exception and prevent the catch() from recognizing it.
    //            For details, see Bug #1872 in the bugtracker.

    itk::ExceptionObject excp(__FILE__, __LINE__, "Problem while opening the file.", ITK_LOCATION);
    throw excp;
  }

  volatile int bitDepth;
  switch (this->GetComponentType())
  {
    case IOComponentEnum::UCHAR:
      bitDepth = 8;
      break;

    case IOComponentEnum::USHORT:
      bitDepth = 16;
      break;

    default:
    {
      // IMPORTANT: The itkExceptionMacro() cannot be used here due to a bug in
      // Visual
      //            Studio 7.1 in release mode. That compiler will corrupt the
      // RTTI type
      //            of the Exception and prevent the catch() from recognizing
      // it.
      //            For details, see Bug #1872 in the bugtracker.
      itk::ExceptionObject excp(__FILE__, __LINE__, "PNG supports unsigned char and unsigned short", ITK_LOCATION);
      throw excp;
    }
  }

  png_structp png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, (png_voidp) nullptr, nullptr, nullptr);
  if (!png_ptr)
  {
    itkExceptionMacro(<< "Unable to write PNG file! png_create_write_struct failed.");
  }

  png_infop info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr)
  {
    png_destroy_write_struct(&png_ptr, (png_infopp) nullptr);
    itkExceptionMacro(<< "Unable to write PNG file!. png_create_info_struct failed.");
  }

  png_init_io(png_ptr, fp);

  png_set_error_fn(png_ptr, (png_voidp) nullptr, itkPNGWriteErrorFunction, itkPNGWriteWarningFunction);
  if (setjmp(png_jmpbuf(png_ptr)))
  {
    itkExceptionMacro("Error while writing Slice to file: " << this->GetFileName() << std::endl
                                                            << "Reason: " << itksys::SystemTools::GetLastSystemError());
  }

  int          colorType;
  unsigned int numComp = this->GetNumberOfComponents();
  switch (numComp)
  {
    case 1:
      if (this->GetWritePalette())
      {
        colorType = PNG_COLOR_TYPE_PALETTE;
      }
      else
      {
        colorType = PNG_COLOR_TYPE_GRAY;
      }
      break;
    case 2:
      colorType = PNG_COLOR_TYPE_GRAY_ALPHA;
      break;
    case 3:
      colorType = PNG_COLOR_TYPE_RGB;
      break;
    default:
      colorType = PNG_COLOR_TYPE_RGB_ALPHA;
      break;
  }

  png_uint_32 width, height;
  double      rowSpacing, colSpacing;
  width = this->GetDimensions(0);
  colSpacing = m_Spacing[0];

  if (m_NumberOfDimensions > 1)
  {
    height = this->GetDimensions(1);
    rowSpacing = m_Spacing[1];
  }
  else
  {
    height = 1;
    rowSpacing = 1;
  }

  png_set_IHDR(png_ptr,
               info_ptr,
               width,
               height,
               bitDepth,
               colorType,
               PNG_INTERLACE_NONE,
               PNG_COMPRESSION_TYPE_DEFAULT,
               PNG_FILTER_TYPE_DEFAULT);
  // interlaceType - PNG_INTERLACE_NONE or
  //                 PNG_INTERLACE_ADAM7

  png_color * palette = nullptr;
  bool        paletteAllocated = false;
  if (colorType == PNG_COLOR_TYPE_PALETTE)
  {
    auto         inputPaletteLength = static_cast<unsigned int>(m_ColorPalette.size());
    unsigned int PNGPaletteLength = inputPaletteLength;

    // discard colors exceeding PNG max number
    PNGPaletteLength = (PNGPaletteLength <= PNG_MAX_PALETTE_LENGTH) ? PNGPaletteLength : PNG_MAX_PALETTE_LENGTH;

    // min palette size is 2 for PNG
    PNGPaletteLength = (PNGPaletteLength < 2) ? 2 : PNGPaletteLength;

    palette = static_cast<png_color *>(png_malloc(png_ptr, PNGPaletteLength * sizeof(png_color)));
    paletteAllocated = true;

    for (unsigned int i = 0; i < PNGPaletteLength; ++i)
    {
      if (i < inputPaletteLength)
      {
        // fill the palette with the user given palette
        png_color * col = &palette[i];
        col->red = m_ColorPalette[i].GetRed();
        col->green = m_ColorPalette[i].GetGreen();
        col->blue = m_ColorPalette[i].GetBlue();
      }
      else
      {
        // this handle the case where the user given palette size is < 2
        // set to black by default
        png_color * col = &palette[i];
        col->red = 0;
        col->green = 0;
        col->blue = 0;
      }
    }

    // set palette
    png_set_PLTE(png_ptr, info_ptr, palette, static_cast<int>(PNGPaletteLength));
  }

  if (m_UseCompression)
  {
    // Set the image compression level.
    png_set_compression_level(png_ptr, this->GetCompressionLevel());
  }

  // write out the spacing information:
  //      set the unit_type to unknown.  if we add units to ITK, we should
  //          convert pixel size to meters and store units as meters (png
  //          has three set of units: meters, radians, and unknown).
#if defined(PNG_sCAL_SUPPORTED) && defined(PNG_FLOATING_POINT_SUPPORTED)
  png_set_sCAL(png_ptr, info_ptr, PNG_SCALE_METER, colSpacing, rowSpacing);
#endif

  png_write_info(png_ptr, info_ptr);
  // default is big endian
  if (bitDepth > 8)
  {
#ifndef ITK_WORDS_BIGENDIAN
    png_set_swap(png_ptr);
#endif
  }
  const auto row_pointers = make_unique_for_overwrite<png_bytep[]>(height);

  {
    const int                      rowInc = width * numComp * bitDepth / 8;
    volatile const unsigned char * outPtr = ((const unsigned char *)buffer);
    for (unsigned int ui = 0; ui < height; ++ui)
    {
      row_pointers[ui] = const_cast<png_byte *>(outPtr);
      outPtr = const_cast<unsigned char *>(outPtr) + rowInc;
    }
  }
  png_write_image(png_ptr, row_pointers.get());
  png_write_end(png_ptr, info_ptr);

  if (paletteAllocated)
  {
    // free palette memory
    png_free(png_ptr, palette);
    palette = nullptr;
  }

  png_destroy_write_struct(&png_ptr, &info_ptr);
}
} // end namespace itk
