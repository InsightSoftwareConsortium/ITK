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
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"

namespace itk
{

// simple class to call fopen on construct and
// fclose on destruct
struct PNGFileWrapper
{
  PNGFileWrapper(const char* fname, const char *openMode)
    {
      m_FilePointer = fopen(fname, openMode);
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
  // First check the extension
  std::string filename = file;
  if ( !( filename != "" &&
          filename.find(".png") < filename.length() ) )
    {
    itkDebugMacro(<<"The filename extension is not recognized");
    return false;
    }

  // Now check the file header
  PNGFileWrapper pngfp(file,"rb");
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
                          &end_info);
  
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
      return typeid(char);
    case SHORT:
      return typeid(short);
    case UINT:
      return typeid(unsigned int);
    case INT:
      return typeid(int);
    case ULONG:
      return typeid(unsigned long);
    case LONG:
      return typeid(long);
    case FLOAT:
      return typeid(float);
    case DOUBLE:
      return typeid(double);
    case RGB:
      return typeid(RGBPixel<unsigned char>);
    case RGBA:
      return typeid(RGBAPixel<unsigned char>);
    default:
      {
      itkExceptionMacro ("Invalid type: " << m_PixelType << ", only unsigned char, unsigned short, RGB<unsigned char> are allowed.");
      return this->ConvertToTypeInfo(m_PixelType);      
      }
    case UNKNOWN:
      itkExceptionMacro ("Unknown pixel type: " << m_PixelType);
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
      return sizeof(char);
    case SHORT:
      return sizeof(short);
    case UINT:
      return sizeof(unsigned int);
    case INT:
      return sizeof(int);
    case ULONG:
      return sizeof(unsigned long);
    case LONG:
      return sizeof(long);
    case FLOAT:
      return sizeof(float);
    case DOUBLE:
      return sizeof(double);
    case RGB:
      return sizeof(unsigned char);
    case RGBA:
      return sizeof(unsigned char);
    case UNKNOWN:
      {
      itkExceptionMacro ("Invalid type: " << m_PixelType 
                  << ", only unsigned char and unsigned short are allowed.");
      return 0;
      }
    }
  return 1;
}

  
void PNGImageIO::ReadVolume(void*)
{
  
}

  
void PNGImageIO::Read(void* buffer)
{
  //std::cout << "Read: file dimensions = " << this->GetNumberOfDimensions() << std::endl;
  // use this class so return will call close
  PNGFileWrapper pngfp(this->GetFileName(),"rb"); 
  FILE* fp = pngfp.m_FilePointer;
  if(!fp)
    {
    itkExceptionMacro("Error PNGImageIO could not open file: " 
                  << this->GetFileName());
    return;
    }
  unsigned char header[8];
  fread(header, 1, 8, fp);
  bool is_png = !png_sig_cmp(header, 0, 8);
  if(!is_png)
    {
    itkExceptionMacro("Error File is not png type" << this->GetFileName());
    return;
    }
  png_structp png_ptr = png_create_read_struct
    (PNG_LIBPNG_VER_STRING, (png_voidp)NULL,
     NULL, NULL);
  if (!png_ptr)
    {
    itkExceptionMacro("Error File is not png type" << this->GetFileName());
    return;
    }
  
  png_infop info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr)
    {
    png_destroy_read_struct(&png_ptr,
                            (png_infopp)NULL, (png_infopp)NULL);
    itkExceptionMacro("Error File is not png type" << this->GetFileName());
    return;
    }

  png_infop end_info = png_create_info_struct(png_ptr);
  if (!end_info)
    {
    png_destroy_read_struct(&png_ptr, &info_ptr,
                            (png_infopp)NULL);
    itkExceptionMacro("Error File is not png type" << this->GetFileName());
    return;
    }
  
  png_init_io(png_ptr, fp);
  png_set_sig_bytes(png_ptr, 8);

  png_read_info(png_ptr, info_ptr);

  png_uint_32 width, height;
  int bitDepth, colorType, interlaceType;
  int compression_type, filter_method;
  png_get_IHDR(png_ptr, info_ptr, 
               &width, &height,
               &bitDepth, &colorType, &interlaceType,
               &compression_type, &filter_method);

  // convert palettes to RGB
  if (colorType == PNG_COLOR_TYPE_PALETTE)
    {
    png_set_palette_to_rgb(png_ptr);
    }

  // minimum of a byte per pixel
  if (colorType == PNG_COLOR_TYPE_GRAY && bitDepth < 8) 
    {
    png_set_gray_1_2_4_to_8(png_ptr);
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

   // have libpng handle interlacing
  //int number_of_passes = png_set_interlace_handling(png_ptr);
  // update the info now that we have defined the filters
  png_read_update_info(png_ptr, info_ptr);

  unsigned long rowbytes = png_get_rowbytes(png_ptr, info_ptr);
  unsigned char *tempImage = static_cast<unsigned char*>(buffer);
  png_bytep *row_pointers = new png_bytep [height];
  for (unsigned int ui = 0; ui < height; ++ui)
    {
    row_pointers[height - ui - 1] = tempImage + rowbytes*ui;
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
  m_UseCompression = false;
  m_CompressionLevel = 4; // Range 0-9; 0 = no file compression, 9 = maximum file compression
  m_Spacing[0] = 1.0;
  m_Spacing[1] = 1.0;
  
  m_Origin[0] = 0.0;
  m_Origin[1] = 0.0;
}

PNGImageIO::~PNGImageIO()
{
}

void PNGImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "PixelType " << m_PixelType << "\n";
  os << indent << "Use Compression : " << m_UseCompression << "\n";
  os << indent << "Compression Level : " << m_CompressionLevel << "\n";
}

  
  
void PNGImageIO::ReadImageInformation()
{
  m_Spacing[0] = 1.0;  // We'll look for PNG pixel size information later,
  m_Spacing[1] = 1.0;  // but set the defaults now

  m_Origin[0] = 0.0;
  m_Origin[1] = 0.0;

  // use this class so return will call close
  PNGFileWrapper pngfp(m_FileName.c_str(),"rb");
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
  int bitDepth, colorType, interlaceType;
  int compression_type, filter_method;
  png_get_IHDR(png_ptr, info_ptr, 
               &width, &height,
               &bitDepth, &colorType, &interlaceType,
               &compression_type, &filter_method);

  // convert palettes to RGB
  if (colorType == PNG_COLOR_TYPE_PALETTE)
    {
    png_set_palette_to_rgb(png_ptr);
    }

  // minimum of a byte per pixel
  if (colorType == PNG_COLOR_TYPE_GRAY && bitDepth < 8) 
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
  m_Dimensions[0] = width;
  m_Dimensions[1] = height;
  if (bitDepth <= 8)
    {
    m_PixelType = UCHAR;
    }
  else
    {
    m_PixelType = USHORT;
    }
  this->SetNumberOfComponents(png_get_channels(png_ptr, info_ptr));


  // see if the PNG file stored spacing information,
  // ignore the units (for now).
  double px_width = 1.0, px_height = 1.0; // use default values if not in file
  int units = PNG_SCALE_UNKNOWN;
  png_get_sCAL(png_ptr, info_ptr, &units, &px_width, &px_height);

  m_Spacing[0] = px_width;
  m_Spacing[1] = px_height;


  // clean up
  png_destroy_read_struct(&png_ptr, &info_ptr,
                          &end_info);

  return;
}

bool PNGImageIO::CanWriteFile( const char * name )
{
  std::string filename = name;
  if ( filename != "" &&
       filename.find(".png") < filename.length() )
    {
    return true;
    }
  return false;
}


void PNGImageIO::WriteImageInformation(void)
{
}

void PNGImageIO::Write(const void* buffer)
{
  ImageIORegion ioRegion = this->GetIORegion();

  // Check the image region for proper dimensions, etc.
  unsigned int numDims = this->GetNumberOfDimensions();
  if ( numDims < 2 || numDims > 3 )
    {
    itkExceptionMacro(<<"PNG Writer can only write 2 or 3-dimensional images");
    return;
    }
  
  // loop over the z axis and write the slices
  std::string fileName;
  int numSlices = (numDims < 3 ? 1 : this->GetDimensions(2));
  unsigned long sliceSizeInBytes = 
                  this->GetDimensions(0) * 
                  this->GetDimensions(1) *
                  this->GetNumberOfComponents() *
                  this->GetComponentSize();
  
  for ( int fileNum=0; fileNum < numSlices; fileNum++ )
    {
    // determine the name
    if ( m_FileName != "" )
      {
      fileName = m_FileName;
      }
    else 
      {
      char fullName[1024];
      sprintf (fullName, "%s%03d.png", m_FilePrefix.c_str(), fileNum);
      fileName = fullName;
      }
    this->WriteSlice(fileName,buffer,fileNum * sliceSizeInBytes);
    }

}

void PNGImageIO::WriteSlice(std::string& fileName, const void* buffer, 
                            unsigned long offset)
{
  const unsigned char *outPtr = ( (const unsigned char *) buffer) + offset;

  // use this class so return will call close
  PNGFileWrapper pngfp(fileName.c_str(),"wb");
  FILE* fp = pngfp.m_FilePointer;
  if(!fp)
    {
    itkExceptionMacro("Unable to open file " << fileName);
    }

  int bitDepth;
  switch (this->GetComponentType())
    {
    case UCHAR:
      bitDepth = 8;
      break;

    case USHORT:
      bitDepth = 16;
      break;

    default:
      itkExceptionMacro(<<"PNG supports unsigned char and unsigned short");
      ;
    }
  
  png_structp png_ptr = png_create_write_struct
    (PNG_LIBPNG_VER_STRING, (png_voidp)NULL, NULL, NULL);
  if (!png_ptr)
    {
    itkExceptionMacro(<<"Unable to write PNG file!");
    }
  
  png_infop info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr)
    {
    png_destroy_write_struct(&png_ptr,
                             (png_infopp)NULL);
    itkExceptionMacro(<<"Unable to write PNG file!");
    }

  png_init_io(png_ptr, fp);
  
  png_uint_32 width, height;
  width = this->GetDimensions(0);
  height = this->GetDimensions(1);

  int colorType;
  unsigned int numComp = this->GetNumberOfComponents();
  switch ( numComp )
    {
    case 1: colorType = PNG_COLOR_TYPE_GRAY;
      break;
    case 2: colorType = PNG_COLOR_TYPE_GRAY_ALPHA;
      break;
    case 3: colorType = PNG_COLOR_TYPE_RGB;
      break;
    default: colorType = PNG_COLOR_TYPE_RGB_ALPHA;
      break;
    }
  
  png_set_IHDR(png_ptr, info_ptr, width, height,
               bitDepth, colorType, PNG_INTERLACE_NONE,
               PNG_COMPRESSION_TYPE_DEFAULT, 
               PNG_FILTER_TYPE_DEFAULT);
  // interlaceType - PNG_INTERLACE_NONE or
  //                 PNG_INTERLACE_ADAM7
    
  if(m_UseCompression)
  {
    png_set_compression_level(png_ptr, m_CompressionLevel); // Set the image compression level.
  }

  // write out the spacing information:
  //      set the unit_type to unknown.  if we add units to ITK, we should
  //          convert pixel size to meters and store units as meters (png
  //          has three set of units: meters, radians, and unknown).
  png_set_sCAL(png_ptr, info_ptr, PNG_SCALE_UNKNOWN, m_Spacing[0],
               m_Spacing[1]);

  png_write_info(png_ptr, info_ptr);
  // default is big endian
  if (bitDepth > 8)
    {
#ifndef ITK_WORDS_BIGENDIAN
    png_set_swap(png_ptr);
#endif
    }
  png_byte **row_pointers = new png_byte *[height];
  int rowInc = width*numComp*bitDepth/8;
  for (unsigned int ui = 0; ui < height; ui++)
    {
    row_pointers[height - ui - 1] = (png_byte *)outPtr;
    outPtr = (unsigned char *)outPtr + rowInc;
    }
  png_write_image(png_ptr, row_pointers);
  png_write_end(png_ptr, info_ptr);

  delete [] row_pointers;
  png_destroy_write_struct(&png_ptr, &info_ptr);
}


} // end namespace itk

