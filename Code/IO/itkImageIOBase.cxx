/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIOBase.cxx
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
#include "itkImageIOBase.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"


namespace itk
{

ImageIOBase::ImageIOBase() :
  m_PixelType(UNKNOWN),
  m_ComponentType(UNKNOWN),
  m_NumberOfDimensions(0),
  m_ByteOrder(OrderNotApplicable),
  m_FileType(TypeNotApplicable)
{
  Reset(false);
}

  
void ImageIOBase::Reset(const bool freeDynamic)
{
  m_Initialized = false;
  m_FileName = "";
  m_NumberOfComponents = 1;
  for (unsigned int i=0; i < m_NumberOfDimensions; i++)
    {
    m_Dimensions[i] = 0;
    m_Strides[i] = 0;
    }
  m_NumberOfDimensions = 0;
}

ImageIOBase::~ImageIOBase()
{
}

void ImageIOBase::Resize(const unsigned int numDimensions,
                    const unsigned int* dimensions)
{
  m_NumberOfDimensions = numDimensions;
  if (dimensions != NULL)
    {
    for (unsigned int i=0; i < m_NumberOfDimensions; i++)
      {
      m_Dimensions[i] = dimensions[i];
      }
    ComputeStrides();
  }
}

void ImageIOBase::SetDimensions(unsigned int i, unsigned int dim)
{
  if ( i < 0 || i > m_Dimensions.size() ) {return;}
  this->Modified();
  m_Dimensions.insert(m_Dimensions.begin()+i,dim);
}

void ImageIOBase::SetOrigin(unsigned int i, double origin)
{
  if ( i < 0 || i > m_Origin.size() ) {return;}
  this->Modified();
  m_Origin.insert(m_Origin.begin()+i,origin);
}

void ImageIOBase::SetSpacing(unsigned int i, double spacing)
{
  if ( i < 0 || i > m_Spacing.size() ) {return;}
  this->Modified();
  m_Spacing.insert(m_Spacing.begin()+i,spacing);
}

const std::type_info& ImageIOBase::GetPixelType() const
{
  switch(m_PixelType)
    {
    case UCHAR:
      return typeid(unsigned char);
    case CHAR:
      return typeid(char);
    case USHORT:
      return typeid(unsigned short);
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
      itkErrorMacro (<< "Invalid type: " << m_PixelType );
    }
  return typeid(ImageIOBase::UnknownType);
}

void ImageIOBase::SetPixelType(const IODataType ctype)
{
  if ( m_PixelType != ctype )
    {
    this->Modified();
    m_PixelType = ctype;
    }
}

bool ImageIOBase::SetPixelType(const std::type_info& ptype)
{
  this->SetNumberOfComponents(1);
  if ( ptype == typeid(double) )
    {
    this->SetPixelType(ImageIOBase::DOUBLE);
    this->SetComponentType(ImageIOBase::DOUBLE);
    }
  else if ( ptype == typeid(float) )
    {
    this->SetPixelType(ImageIOBase::FLOAT);
    this->SetComponentType(ImageIOBase::FLOAT);
    }
  else if ( ptype == typeid(long) )
    {
    this->SetPixelType(ImageIOBase::LONG);
    this->SetComponentType(ImageIOBase::LONG);
    }
  else if ( ptype == typeid(unsigned long) )
    {
    this->SetPixelType(ImageIOBase::ULONG);
    this->SetComponentType(ImageIOBase::ULONG);
    }
  else if ( ptype == typeid(int) )
    {
    this->SetPixelType(ImageIOBase::INT);
    this->SetComponentType(ImageIOBase::INT);
    }
  else if ( ptype == typeid(unsigned int) )
    {
    this->SetPixelType(ImageIOBase::UINT);
    this->SetComponentType(ImageIOBase::UINT);
    }
  else if ( ptype == typeid(short) )
    {
    this->SetPixelType(ImageIOBase::SHORT);
    this->SetComponentType(ImageIOBase::SHORT);
    }
  else if ( ptype == typeid(unsigned short) )
    {
    this->SetPixelType(ImageIOBase::USHORT);
    this->SetComponentType(ImageIOBase::USHORT);
    }
  else if ( ptype == typeid(char) )
    {
    this->SetPixelType(ImageIOBase::CHAR);
    this->SetComponentType(ImageIOBase::CHAR);
    }
  else if ( ptype == typeid(unsigned char) )
    {
    this->SetPixelType(ImageIOBase::UCHAR);
    this->SetComponentType(ImageIOBase::UCHAR);
    }
  else if ( ptype == typeid(RGBPixel<unsigned char>) )
    {
    this->SetNumberOfComponents(3);
    this->SetPixelType(ImageIOBase::UCHAR);
    this->SetComponentType(ImageIOBase::UCHAR);
    }
  else if ( ptype == typeid(RGBAPixel<unsigned char>) )
    {
    this->SetNumberOfComponents(4);
    this->SetPixelType(ImageIOBase::UCHAR);
    this->SetComponentType(ImageIOBase::UCHAR);
    }
  else
    {
    itkErrorMacro(<<"Pixel type currently not supported");
    this->SetPixelType(ImageIOBase::UNKNOWN);
    this->SetComponentType(ImageIOBase::UNKNOWN);
    return false;
    }
  return true;
}

void ImageIOBase::ComputeStrides()
{
  unsigned int i;

  m_Strides[0] = this->GetComponentSize();
  m_Strides[1] = m_NumberOfComponents * m_Strides[0];
  for (i = 2; i <= (m_NumberOfDimensions+1); i++)
    {
    m_Strides[i] = m_Dimensions[i-2] * m_Strides[i-1];
    }
}


// Calculates the image size in PIXELS
unsigned int ImageIOBase::GetImageSizeInPixels() const
{
  unsigned int i;
  unsigned int numPixels = 1;

  for (i = 0; i < m_NumberOfDimensions; i++)
    {
    numPixels *= m_Dimensions[i];
    }

  return numPixels;
}

unsigned int ImageIOBase::GetImageSizeInComponents() const
{
  return GetImageSizeInPixels() * m_NumberOfComponents;
}

unsigned int ImageIOBase::GetImageSizeInBytes () const
{
  return (this->GetImageSizeInComponents() * this->GetComponentSize());
}

unsigned int ImageIOBase::GetComponentStride() const
{
  return m_Strides[0];
}

unsigned int ImageIOBase::GetPixelStride () const
{
  return m_Strides[1];
}

unsigned int ImageIOBase::GetRowStride () const
{
  return m_Strides[2];
}

unsigned int ImageIOBase::GetSliceStride () const
{
  return m_Strides[3];
}

void ImageIOBase::SetNumberOfDimensions(unsigned int dim)
{
  if(dim != m_NumberOfDimensions)
    {
    m_Dimensions.resize(dim);
    m_NumberOfDimensions = dim;
    this->Modified();
    }
}

const std::type_info& 
ImageIOBase::ConvertToTypeInfo(IODataType t ) const
{
  switch(t)
    {
    case UCHAR:
      return typeid(unsigned char);
    case CHAR:
      return typeid(char);
    case USHORT:
      return typeid(unsigned short);
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
      itkErrorMacro (<< "Invalid type: " << m_PixelType );
    }
  return typeid(ImageIOBase::UnknownType);
}

unsigned int 
ImageIOBase::GetSizeOfType(IODataType t) const
{
  switch(t)
    {
    case UCHAR:
      return sizeof(unsigned char);
    case CHAR:
      return sizeof(char);
    case USHORT:
      return sizeof(unsigned short);
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
      return sizeof(RGBPixel<unsigned char>);
    case RGBA:
      return sizeof(RGBAPixel<unsigned char>);
    }
  return 0;

}

unsigned int ImageIOBase::GetPixelSize() const
{
  switch(m_ComponentType)
    {
    case UCHAR:
      return sizeof(unsigned char);
    case CHAR:
      return sizeof(char);
    case USHORT:
      return sizeof(unsigned short);
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
      return sizeof(RGBPixel<unsigned char>);
    case RGBA:
      return sizeof(RGBAPixel<unsigned char>);
    }

  return 0;
}

unsigned int ImageIOBase::GetComponentSize() const
{
  switch(m_ComponentType)
    {
    case UCHAR:
      return sizeof(unsigned char);
    case CHAR:
      return sizeof(char);
    case USHORT:
      return sizeof(unsigned short);
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
      return sizeof(RGBPixel<unsigned char>);
    case RGBA:
      return sizeof(RGBAPixel<unsigned char>);
    }

  return 0;
}

std::string ImageIOBase::ReturnTypeAsString(IODataType t) const
{
  std::string s;
  switch(t)
    {
    case UCHAR:
      return (s = "unsigned_char");
    case CHAR:
      return (s = "char");
    case USHORT:
      return (s = "unsigned_short");
    case SHORT:
      return (s = "short");
    case UINT:
      return (s = "unsigned_int");
    case INT:
      return (s = "int");
    case ULONG:
      return (s = "unsigned_long");
    case LONG:
      return (s = "long");
    case FLOAT:
      return (s = "float");
    case DOUBLE:
      return (s = "double");
    case RGB:
      return (s = "rgb");
    case RGBA:
      return (s = "rgba");
    }
  return (s="unknown");

}

template <class TComponent>
static void WriteBuffer(std::ostream& os, TComponent *buffer, unsigned int num)
{
  TComponent *ptr = buffer;
  for (unsigned int i=0; i < num; i++)
    {
    if ( !(i%6) && i ) os << "\n";
    os << *ptr++ << " ";
    }
}

void ImageIOBase::WriteBufferAsASCII(std::ostream& os, void *buffer, 
                                     IODataType ctype, unsigned int numComp)
{
  switch (ctype)
    {
    case UCHAR:
      {
      unsigned char *buf = reinterpret_cast<unsigned char*>(buffer);
      WriteBuffer(os, buf, numComp);
      }
      break;
    case CHAR:
      {
      char *buf = reinterpret_cast<char*>(buffer);
      WriteBuffer(os, buf, numComp);
      }
      break;

    case USHORT:
      {
      unsigned short *buf = reinterpret_cast<unsigned short*>(buffer);
      WriteBuffer(os, buf, numComp);
      }
      break;

    case SHORT:
      {
      short *buf = reinterpret_cast<short*>(buffer);
      WriteBuffer(os, buf, numComp);
      }
      break;

    case UINT:
      {
      unsigned int *buf = reinterpret_cast<unsigned int*>(buffer);
      WriteBuffer(os, buf, numComp);
      }
      break;

    case INT:
      {
      int *buf = reinterpret_cast<int*>(buffer);
      WriteBuffer(os, buf, numComp);
      }
      break;

    case ULONG:
      {
      unsigned long *buf = reinterpret_cast<unsigned long*>(buffer);
      WriteBuffer(os, buf, numComp);
      }
      break;

    case LONG:
      {
      long *buf = reinterpret_cast<long*>(buffer);
      WriteBuffer(os, buf, numComp);
      }
      break;

    case FLOAT:
      {
      float *buf = reinterpret_cast<float*>(buffer);
      WriteBuffer(os, buf, numComp);
      }
      break;

    case DOUBLE:
      {
      double *buf = reinterpret_cast<double*>(buffer);
      WriteBuffer(os, buf, numComp);
      }
      break;

    default:
      ;
    }

}


void ImageIOBase::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Filename: " << m_FileName << std::endl;
  os << indent << "Number of Components/Pixel: " << m_NumberOfComponents << "\n";
  os << indent << "Pixel Type: " << this->GetPixelType().name() << std::endl;
  os << indent << "Component Type: " 
     << this->ReturnTypeAsString(m_ComponentType) << std::endl;
  os << indent << "Dimensions: ( ";
  for (unsigned int i=0; i < m_NumberOfDimensions; i++)
    {
    os << m_Dimensions[i] << " ";
    }
  os << ")" << std::endl;
}

} //namespace itk
