/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPNGImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
  return this->ReadHeader(file);
}
  
const std::type_info& PNGImageIO::GetPixelType() const
{
  switch(m_PNGPixelType)
    {
    case UCHAR:
      return typeid(unsigned char);
    case USHORT:
      return typeid(unsigned short);
    default:
      return typeid(unsigned char);
    }
}

  
unsigned int PNGImageIO::GetComponentSize() const
{
  switch(m_PNGPixelType)
    {
    case UCHAR:
      return sizeof(unsigned char);
    case USHORT:
      return sizeof(unsigned short);
    }
  return 1;
}

  
void PNGImageIO::Load(void* buffer)
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

  int rowbytes = png_get_rowbytes(png_ptr, info_ptr);
  unsigned char *tempImage = static_cast<unsigned char*>(buffer);
  png_bytep *row_pointers = new png_bytep [height];
  for (int ui = 0; ui < height; ++ui)
    {
    row_pointers[ui] = tempImage + rowbytes*ui;
    }
  png_read_image(png_ptr, row_pointers);
  delete [] row_pointers;
  // close the file
  png_read_end(png_ptr, NULL);
  png_destroy_read_struct(&png_ptr, &info_ptr, &end_info);
}


const double* 
PNGImageIO::GetOrigin() const
{
  return m_Origin;
}


const double* 
PNGImageIO::GetSpacing() const
{
  return m_Spacing;
}


PNGImageIO::PNGImageIO()
{
  this->SetNumberOfDimensions(2);
  m_PNGPixelType = UCHAR;
}

PNGImageIO::~PNGImageIO()
{
}

void PNGImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "PNGPixelType " << m_PNGPixelType << "\n";
}

  
  
bool PNGImageIO::ReadHeader(const char* fname)
{
  PNGFileWrapper pngfp(fname); // use this class so return will call close
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
  this->SetFileName(fname); 
  if (bit_depth <= 8)
    {
    m_PNGPixelType = UCHAR;
    }
  else
    {
    m_PNGPixelType = USHORT;
    }
  this->SetNumberOfComponents(png_get_channels(png_ptr, info_ptr));
  return true;
}


} // end namespace itk
