/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPNGImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkPNGImageIO.h"
#include "png.h"

namespace itk
{

// simple class to call fopen on construct and
// fclose on destruct
struct PNGFileWrapper
{
  PNGFileWrapper(const char* fname)
    {
      m_FilePointer = fopen(fname, "rb");
    }
  FILE* m_FilePointer;
  ~PNGFileWrapper()
    {
      if(m_FilePointer)
        {
        fclose(m_FilePointer);
        }
    }
};

  
  
bool PNGImageIO::CanReadFile(const char* file) 
{ 
  PNGFileWrapper pngfp(file);
  FILE* fp = pngfp.m_FilePointer;
  if(!fp)
    {
    return false;
    }
  unsigned char header[8];
  fread(header, 1, 8, fp);
  bool is_png = !png_sig_cmp(header, 0, 8);
  if(!is_png)
    {
    return false;
    }
  png_structp png_ptr = png_create_read_struct
    (PNG_LIBPNG_VER_STRING, (png_voidp)NULL,
     NULL, NULL);
  if (!png_ptr)
    {
    return false;
    }
  
  png_infop info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr)
    {
    png_destroy_read_struct(&png_ptr,
                            (png_infopp)NULL, (png_infopp)NULL);
    return false;
    }
  
  png_infop end_info = png_create_info_struct(png_ptr);
  if (!end_info)
    {
    png_destroy_read_struct(&png_ptr, &info_ptr,
                            (png_infopp)NULL);
    return false;
    }
  png_destroy_read_struct(&png_ptr, &info_ptr,
                          (png_infopp)NULL);
  
  return true;
}
  
const std::type_info& PNGImageIO::GetPixelType() const
{
  switch(m_PixelType)
    {
    case UCHAR:
      return typeid(unsigned char);
    case USHORT:
      return typeid(unsigned short);
    case CHAR:
    case SHORT:
    case UINT:
    case INT:
    case ULONG:
    case LONG:
    case FLOAT:
    case DOUBLE:
    case RGB:
    case RGBA:
      {
      itkErrorMacro ("Invalid type: " << m_PixelType << ", only unsigned char and unsigned short are allowed.");
      return this->ConvertToTypeInfo(m_PixelType);      
      }
    case UNKNOWN:
      itkErrorMacro ("Unknown pixel type: " << m_PixelType);
    }
  return typeid(ImageIOBase::UnknownType);
}

  
unsigned int PNGImageIO::GetComponentSize() const
{
  switch(m_PixelType)
    {
    case UCHAR:
      return sizeof(unsigned char);
    case USHORT:
      return sizeof(unsigned short);
    case CHAR:
    case SHORT:
    case UINT:
    case INT:
    case ULONG:
    case LONG:
    case FLOAT:
    case DOUBLE:
    case RGB:
    case RGBA:
    case UNKNOWN:
      {
      itkErrorMacro ("Invalid type: " << m_PixelType << ", only unsigned char and unsigned short are allowed.");
      return 0;
      }
    }
  return 1;
}

  
void PNGImageIO::Read(void* buffer)
{
  // use this class so return will call close
  PNGFileWrapper pngfp(this->GetFileName()); 
  FILE* fp = pngfp.m_FilePointer;
  if(!fp)
    {
    itkErrorMacro("Error PNGImageIO could not open file: " 
                  << this->GetFileName());
    return;
    }
  unsigned char header[8];
  fread(header, 1, 8, fp);
  bool is_png = !png_sig_cmp(header, 0, 8);
  if(!is_png)
    {
    itkErrorMacro("Error File is not png type" << this->GetFileName());
    return;
    }
  png_structp png_ptr = png_create_read_struct
    (PNG_LIBPNG_VER_STRING, (png_voidp)NULL,
     NULL, NULL);
  if (!png_ptr)
    {
    itkErrorMacro("Error File is not png type" << this->GetFileName());
    return;
    }
  
  png_infop info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr)
    {
    png_destroy_read_struct(&png_ptr,
                            (png_infopp)NULL, (png_infopp)NULL);
    itkErrorMacro("Error File is not png type" << this->GetFileName());
    return;
    }

  png_infop end_info = png_create_info_struct(png_ptr);
  if (!end_info)
    {
    png_destroy_read_struct(&png_ptr, &info_ptr,
                            (png_infopp)NULL);
    itkErrorMacro("Error File is not png type" << this->GetFileName());
    return;
    }
  
  png_init_io(png_ptr, fp);
  png_set_sig_bytes(png_ptr, 8);

  png_read_info(png_ptr, info_ptr);

  png_uint_32 width, height;
  int bit_depth, color_type, interlace_type;
  int compression_type, filter_method;
  png_get_IHDR(png_ptr, info_ptr, 
               &width, &height,
               &bit_depth, &color_type, &interlace_type,
               &compression_type, &filter_method);

  // convert palettes to RGB
  if (color_type == PNG_COLOR_TYPE_PALETTE)
    {
    png_set_palette_to_rgb(png_ptr);
    }

  // minimum of a byte per pixel
  if (color_type == PNG_COLOR_TYPE_GRAY && bit_depth < 8) 
    {
    png_set_gray_1_2_4_to_8(png_ptr);
    }

  // add alpha if any alpha found
  if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS)) 
    {
    png_set_tRNS_to_alpha(png_ptr);
    }

  if (bit_depth > 8)
    {
#ifndef ITK_WORDS_BIGENDIAN
    png_set_swap(png_ptr);
#endif
    }

   // have libpng handle interlacing
  //int number_of_passes = png_set_interlace_handling(png_ptr);
  // update the info now that we have defined the filters
  png_read_update_info(png_ptr, info_ptr);

  unsigned long rowbytes = png_get_rowbytes(png_ptr, info_ptr);
  unsigned char *tempImage = static_cast<unsigned char*>(buffer);
  png_bytep *row_pointers = new png_bytep [height];
  for (unsigned int ui = 0; ui < height; ++ui)
    {
    row_pointers[ui] = tempImage + rowbytes*ui;
    }
  png_read_image(png_ptr, row_pointers);
  delete [] row_pointers;
  // close the file
  png_read_end(png_ptr, NULL);
  png_destroy_read_struct(&png_ptr, &info_ptr, &end_info);

}


PNGImageIO::PNGImageIO()
{
  this->SetNumberOfDimensions(2);
  m_PixelType = UCHAR;
}

PNGImageIO::~PNGImageIO()
{
}

void PNGImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "PixelType " << m_PixelType << "\n";
}

  
  
void PNGImageIO::ReadImageInformation()
{
  // use this class so return will call close
  PNGFileWrapper pngfp(m_FileName.c_str());
  FILE* fp = pngfp.m_FilePointer;
  if(!fp)
    {
    return;
    }
  unsigned char header[8];
  fread(header, 1, 8, fp);
  bool is_png = !png_sig_cmp(header, 0, 8);
  if(!is_png)
    {
    return;
    }
  png_structp png_ptr = png_create_read_struct
    (PNG_LIBPNG_VER_STRING, (png_voidp)NULL,
     NULL, NULL);
  if (!png_ptr)
    {
    return;
    }
  
  png_infop info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr)
    {
    png_destroy_read_struct(&png_ptr,
                            (png_infopp)NULL, (png_infopp)NULL);
    return;
    }

  png_infop end_info = png_create_info_struct(png_ptr);
  if (!end_info)
    {
    png_destroy_read_struct(&png_ptr, &info_ptr,
                            (png_infopp)NULL);
    return;
    }
  
  png_init_io(png_ptr, fp);
  png_set_sig_bytes(png_ptr, 8);

  png_read_info(png_ptr, info_ptr);

  png_uint_32 width, height;
  int bit_depth, color_type, interlace_type;
  int compression_type, filter_method;
  png_get_IHDR(png_ptr, info_ptr, 
               &width, &height,
               &bit_depth, &color_type, &interlace_type,
               &compression_type, &filter_method);

  // convert palettes to RGB
  if (color_type == PNG_COLOR_TYPE_PALETTE)
    {
    png_set_palette_to_rgb(png_ptr);
    }

  // minimum of a byte per pixel
  if (color_type == PNG_COLOR_TYPE_GRAY && bit_depth < 8) 
    {
    png_set_gray_1_2_4_to_8(png_ptr);
    }

  // add alpha if any alpha found
  if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS)) 
    {
    png_set_tRNS_to_alpha(png_ptr);
    }

  // update the info now that we have defined the filters
  png_read_update_info(png_ptr, info_ptr);
  this->SetNumberOfDimensions(2);
  this->m_Dimensions[0] = width;
  this->m_Dimensions[1] = height;
  if (bit_depth <= 8)
    {
    m_PixelType = UCHAR;
    }
  else
    {
    m_PixelType = USHORT;
    }
  this->SetNumberOfComponents(png_get_channels(png_ptr, info_ptr));
  png_destroy_read_struct(&png_ptr, &info_ptr,
                          (png_infopp)NULL);

  m_Spacing[0] = 1.0;  // Is there any spacing information
  m_Spacing[1] = 1.0;  // in PNG ?

  m_Origin[0] = 0.0;
  m_Origin[1] = 0.0;
  return;
}


} // end namespace itk
