/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIOBase.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImageIOBase.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"
#include "itkOffset.h"
#include "itkVector.h"
#include "itkPoint.h"
#include "itkCovariantVector.h"


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

  
void ImageIOBase::Reset(const bool)
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
  if ( i > m_Dimensions.size() ) {return;}
  this->Modified();
  m_Dimensions[i] = dim;
}

void ImageIOBase::SetOrigin(unsigned int i, double origin)
{
  if ( i > m_Origin.size() ) {return;}
  this->Modified();
  m_Origin[i] = origin;
}

void ImageIOBase::SetSpacing(unsigned int i, double spacing)
{
  if (i > m_Spacing.size() ) {return;}
  this->Modified();
  m_Spacing[i] = spacing;
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
    case UNKNOWN:
      itkExceptionMacro ("Unknown pixel type: " << m_PixelType);
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
  else if ( ptype == typeid(RGBPixel<float>) )
    {
    this->SetNumberOfComponents(3);
    this->SetPixelType(ImageIOBase::FLOAT);
    this->SetComponentType(ImageIOBase::FLOAT);
    }
  else if ( ptype == typeid(RGBAPixel<unsigned char>) )
    {
    this->SetNumberOfComponents(4);
    this->SetPixelType(ImageIOBase::UCHAR);
    this->SetComponentType(ImageIOBase::UCHAR);
    }
  else if ( ptype == typeid(Offset<2>) )
    {
    this->SetNumberOfComponents(2);
    this->SetPixelType(ImageIOBase::OFFSET);
    this->SetComponentType(ImageIOBase::LONG);
    }
  else if ( ptype == typeid(Offset<3>) )
    {
    this->SetNumberOfComponents(3);
    this->SetPixelType(ImageIOBase::OFFSET);
    this->SetComponentType(ImageIOBase::LONG);
    }
  else if ( ptype == typeid(Offset<4>) )
    {
    this->SetNumberOfComponents(4);
    this->SetPixelType(ImageIOBase::OFFSET);
    this->SetComponentType(ImageIOBase::LONG);
    }
  else if ( ptype == typeid(CovariantVector<float,2>) )
    {
    this->SetNumberOfComponents(2);
    this->SetPixelType(ImageIOBase::FLOAT);
    this->SetComponentType(ImageIOBase::FLOAT);
    }
  else if ( ptype == typeid(CovariantVector<float,3>) )
    {
    this->SetNumberOfComponents(3);
    this->SetPixelType(ImageIOBase::FLOAT);
    this->SetComponentType(ImageIOBase::FLOAT);
    }
  else if ( ptype == typeid(CovariantVector<float,4>) )
    {
    this->SetNumberOfComponents(4);
    this->SetPixelType(ImageIOBase::FLOAT);
    this->SetComponentType(ImageIOBase::FLOAT);
    }
  else
    {
    itkExceptionMacro("Pixel type currently not supported.");
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
    m_Dimensions.resize( dim );
    m_Origin.resize( dim );
    m_Spacing.resize( dim );
    m_Strides.resize( dim+2 );
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
      itkExceptionMacro ("Invalid type: " << m_PixelType );
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
    case UNKNOWN:
      itkExceptionMacro ("Unknown pixel type: " << t);
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
    case UNKNOWN:
      itkExceptionMacro ("Unknown pixel type: " << m_ComponentType);
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
    case UNKNOWN:
      itkExceptionMacro ("Unknown pixel type: " << m_ComponentType);
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
    case UNKNOWN:
      itkExceptionMacro ("Unknown pixel type: " << t);
    }
  return (s="unknown");

}

template <class TComponent>
static void WriteBuffer(std::ostream& os, const TComponent *buffer, unsigned int num)
{
  const TComponent *ptr = buffer;
  for (unsigned int i=0; i < num; i++)
    {
    if ( !(i%6) && i ) os << "\n";
    os << *ptr++ << " ";
    }
}

void ImageIOBase::WriteBufferAsASCII(std::ostream& os, const void *buffer, 
                                     IODataType ctype, unsigned int numComp)
{
  switch (ctype)
    {
    case UCHAR:
      {
      typedef const unsigned char * Type;
      Type buf = reinterpret_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
      }
      break;
    case CHAR:
      {
      typedef const char * Type;
      Type buf = reinterpret_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
      }
      break;

    case USHORT:
      {
      typedef const unsigned short * Type;
      Type buf = reinterpret_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
      }
      break;

    case SHORT:
      {
      typedef const short * Type;
      Type buf = reinterpret_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
      }
      break;

    case UINT:
      {
      typedef const unsigned int * Type;
      Type buf = reinterpret_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
      }
      break;

    case INT:
      {
      typedef const int * Type;
      Type buf = reinterpret_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
      }
      break;

    case ULONG:
      {
      typedef const unsigned long * Type;
      Type buf = reinterpret_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
      }
      break;

    case LONG:
      {
      typedef const long * Type;
      Type buf = reinterpret_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
      }
      break;

    case FLOAT:
      {
      typedef const float * Type;
      Type buf = reinterpret_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
      }
      break;

    case DOUBLE:
      {
      typedef const double * Type;
      Type buf = reinterpret_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
      }
      break;

    default:
      ;
    }

}


template <class TComponent>
static void ReadBuffer(std::istream& is, TComponent *buffer, unsigned int num)
{
  TComponent *ptr = buffer;
  for (unsigned int i=0; i < num; i++, ptr++)
    {
    is >> *ptr;
    }
}

void ImageIOBase::ReadBufferAsASCII(std::istream& is, void *buffer, 
                                    IODataType ctype, unsigned int numComp)
{
  switch (ctype)
    {
    case UCHAR:
      {
      unsigned char *buf = reinterpret_cast<unsigned char*>(buffer);
      ReadBuffer(is, buf, numComp);
      }
      break;
    case CHAR:
      {
      char *buf = reinterpret_cast<char*>(buffer);
      ReadBuffer(is, buf, numComp);
      }
      break;

    case USHORT:
      {
      unsigned short *buf = reinterpret_cast<unsigned short*>(buffer);
      ReadBuffer(is, buf, numComp);
      }
      break;

    case SHORT:
      {
      short *buf = reinterpret_cast<short*>(buffer);
      ReadBuffer(is, buf, numComp);
      }
      break;

    case UINT:
      {
      unsigned int *buf = reinterpret_cast<unsigned int*>(buffer);
      ReadBuffer(is, buf, numComp);
      }
      break;

    case INT:
      {
      int *buf = reinterpret_cast<int*>(buffer);
      ReadBuffer(is, buf, numComp);
      }
      break;

    case ULONG:
      {
      unsigned long *buf = reinterpret_cast<unsigned long*>(buffer);
      ReadBuffer(is, buf, numComp);
      }
      break;

    case LONG:
      {
      long *buf = reinterpret_cast<long*>(buffer);
      ReadBuffer(is, buf, numComp);
      }
      break;

    case FLOAT:
      {
      float *buf = reinterpret_cast<float*>(buffer);
      ReadBuffer(is, buf, numComp);
      }
      break;

    case DOUBLE:
      {
      double *buf = reinterpret_cast<double*>(buffer);
      ReadBuffer(is, buf, numComp);
      }
      break;

    default:
      ;
    }

}


void ImageIOBase::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << m_FileName << std::endl;
  os << indent << "FilePrefix: " << m_FilePrefix << std::endl;
  os << indent << "FileType: " << m_FileType << std::endl;
  os << indent << "ByteOrder: " << m_ByteOrder << std::endl;
  os << indent << "IORegion: " << m_IORegion << std::endl;
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
