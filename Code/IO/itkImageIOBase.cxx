/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIOBase.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
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
  m_PixelType(SCALAR),
  m_ComponentType(UNKNOWNCOMPONENTTYPE),
  m_ByteOrder(OrderNotApplicable),
  m_FileType(TypeNotApplicable),
  m_NumberOfDimensions(0)
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
  m_UseCompression = false;
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

const std::type_info& ImageIOBase::GetComponentTypeInfo() const
{
  switch(m_ComponentType)
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
    case UNKNOWNCOMPONENTTYPE:
    default:
      itkExceptionMacro ("Unknown component type: " << m_ComponentType);
    }
  return typeid(ImageIOBase::UnknownType);
}

// 
// This macro enforces pixel type information to be available for all different
// pixel types.
//
#define itkCheckPTypeMacro(type, ntype) \
    ( ptype == typeid(type) ) \
    { \
    this->SetPixelType(ImageIOBase::SCALAR); \
    this->SetComponentType(ImageIOBase::ntype); \
    } \
  else if ( ptype == typeid(RGBPixel<type>) ) \
    { \
    this->SetNumberOfComponents(3); \
    this->SetComponentType(ImageIOBase::ntype); \
    this->SetPixelType(ImageIOBase::RGB); \
    } \
  else if ( ptype == typeid(RGBAPixel<type>) ) \
    { \
    this->SetNumberOfComponents(4); \
    this->SetComponentType(ImageIOBase::ntype); \
    this->SetPixelType(ImageIOBase::RGBA); \
    } \
  else if ( ptype == typeid(Vector<type,2>) ) \
    { \
    this->SetNumberOfComponents(2); \
    this->SetPixelType(ImageIOBase::VECTOR); \
    this->SetComponentType(ImageIOBase::ntype); \
    } \
  else if ( ptype == typeid(Vector<type,3>) ) \
    { \
    this->SetNumberOfComponents(3); \
    this->SetPixelType(ImageIOBase::VECTOR); \
    this->SetComponentType(ImageIOBase::ntype); \
    } \
  else if ( ptype == typeid(Vector<type,4>) ) \
    { \
    this->SetNumberOfComponents(4); \
    this->SetPixelType(ImageIOBase::VECTOR); \
    this->SetComponentType(ImageIOBase::ntype); \
    } \
  else if ( ptype == typeid(Vector<type,5>) ) \
    { \
    this->SetNumberOfComponents(5); \
    this->SetPixelType(ImageIOBase::VECTOR); \
    this->SetComponentType(ImageIOBase::ntype); \
    } \
  else if ( ptype == typeid(Vector<type,6>) ) \
    { \
    this->SetNumberOfComponents(6); \
    this->SetPixelType(ImageIOBase::VECTOR); \
    this->SetComponentType(ImageIOBase::ntype); \
    } \
  else if ( ptype == typeid(Vector<type,7>) ) \
    { \
    this->SetNumberOfComponents(7); \
    this->SetPixelType(ImageIOBase::VECTOR); \
    this->SetComponentType(ImageIOBase::ntype); \
    } \
  else if ( ptype == typeid(CovariantVector<type,2>) ) \
    { \
    this->SetNumberOfComponents(2); \
    this->SetPixelType(ImageIOBase::COVARIANTVECTOR); \
    this->SetComponentType(ImageIOBase::ntype); \
    } \
  else if ( ptype == typeid(CovariantVector<type,3>) ) \
    { \
    this->SetNumberOfComponents(3); \
    this->SetPixelType(ImageIOBase::COVARIANTVECTOR); \
    this->SetComponentType(ImageIOBase::ntype); \
    } \
  else if ( ptype == typeid(CovariantVector<type,4>) ) \
    { \
    this->SetNumberOfComponents(4); \
    this->SetPixelType(ImageIOBase::COVARIANTVECTOR); \
    this->SetComponentType(ImageIOBase::ntype); \
    } \
  else if ( ptype == typeid(CovariantVector<type,5>) ) \
    { \
    this->SetNumberOfComponents(5); \
    this->SetPixelType(ImageIOBase::COVARIANTVECTOR); \
    this->SetComponentType(ImageIOBase::ntype); \
    } \
  else if ( ptype == typeid(CovariantVector<type,6>) ) \
    { \
    this->SetNumberOfComponents(6); \
    this->SetPixelType(ImageIOBase::COVARIANTVECTOR); \
    this->SetComponentType(ImageIOBase::ntype); \
    } \
  else if ( ptype == typeid(CovariantVector<type,7>) ) \
    { \
    this->SetNumberOfComponents(7); \
    this->SetPixelType(ImageIOBase::COVARIANTVECTOR); \
    this->SetComponentType(ImageIOBase::ntype); \
    }



bool ImageIOBase::SetPixelTypeInfo(const std::type_info& ptype)
{
  this->SetNumberOfComponents(1);
  this->SetPixelType(ImageIOBase::UNKNOWNPIXELTYPE);
  this->SetComponentType(ImageIOBase::UNKNOWNCOMPONENTTYPE);

  if itkCheckPTypeMacro(char, CHAR)
  else if itkCheckPTypeMacro(unsigned char, UCHAR)
  else if itkCheckPTypeMacro(short, SHORT)
  else if itkCheckPTypeMacro(unsigned short, USHORT)
  else if itkCheckPTypeMacro(int, INT)

  // Breaking up the if-else chain due to 
  // nesting level limitations of Visual Studio.
  
  if itkCheckPTypeMacro(unsigned int, UINT)
  else if itkCheckPTypeMacro(long, LONG)
  else if itkCheckPTypeMacro(unsigned long, ULONG)
  else if itkCheckPTypeMacro(float, FLOAT)
  else if itkCheckPTypeMacro(double, DOUBLE)

  // Breaking up the if-else chain due to 
  // nesting level limitations of Visual Studio.

  if ( ptype == typeid(Offset<2>) )
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



  if( this->GetPixelType()     == ImageIOBase::UNKNOWNPIXELTYPE    ||
      this->GetComponentType() == ImageIOBase::UNKNOWNCOMPONENTTYPE )
    {
    itkExceptionMacro("Pixel type currently not supported.");
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
  return (this->GetImageSizeInPixels() * m_NumberOfComponents);
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



bool 
ImageIOBase ::ReadBufferAsBinary(std::istream& is, void *buffer, unsigned int num)
{

  const unsigned int numberOfBytesToBeRead = num;

  is.read( static_cast<char *>( buffer ), numberOfBytesToBeRead );

  const unsigned int numberOfBytesRead = is.gcount();

#ifdef __APPLE_CC__
  // fail() is broken in the Mac. It returns true when reaches eof().
  if ( numberOfBytesRead != numberOfBytesToBeRead )
#else
    if ( ( numberOfBytesRead != numberOfBytesToBeRead )  || is.fail() )
#endif
      {
      return false; // read failed
      }

  return true;

}


unsigned int ImageIOBase::GetPixelSize() const
{
  if (m_ComponentType == UNKNOWNCOMPONENTTYPE
      || m_PixelType == UNKNOWNPIXELTYPE)
    {
    itkExceptionMacro ("Unknown pixel or component type: ("
                       << m_PixelType << ", " << m_ComponentType << ")");
    return 0;
    }
  
  return this->GetComponentSize() * this->GetNumberOfComponents();
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
    case UNKNOWNCOMPONENTTYPE:
    default:
      itkExceptionMacro ("Unknown component type: " << m_ComponentType);
    }

  return 0;
}

std::string ImageIOBase::GetFileTypeAsString(FileType t) const
{
  std::string s;
  switch(t)
    {
    case ASCII:
      return s = "ASCII";
    case Binary:
      return s = "Binary";
    case TypeNotApplicable:
    default:
      return s = "TypeNotApplicable";
    }
  return s="TypeNotApplicable";
}

std::string ImageIOBase::GetByteOrderAsString(ByteOrder t) const
{
  std::string s;
  switch(t)
    {
    case BigEndian:
      return s = "BigEndian";
    case LittleEndian:
      return s = "LittleEndian";
    case OrderNotApplicable:
    default:
      return s = "OrderNotApplicable";
    }
  return s="OrderNotApplicable";
}

std::string ImageIOBase::GetComponentTypeAsString(IOComponentType t) const
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
    case UNKNOWNCOMPONENTTYPE:
    default:
      return (s = "unknown");
    }
  return (s="unknown");

}

std::string ImageIOBase::GetPixelTypeAsString(IOPixelType t) const
{
  std::string s;
  switch(t)
    {
    case SCALAR:
      return (s = "scalar");
    case VECTOR:
      return (s = "vector");
    case COVARIANTVECTOR:
      return (s = "covariant_vector");
    case POINT:
      return (s = "point");
    case OFFSET:
      return (s = "offset");
    case RGB:
      return (s = "rgb");
    case RGBA:
      return (s = "rgba");
    case UNKNOWNPIXELTYPE:
    default:
      itkExceptionMacro ("Unknown pixel type: " << t);
    }
  return (s="unknown");

}

namespace {
template <class TComponent>
void WriteBuffer(std::ostream& os, const TComponent *buffer, unsigned int num)
{
  const TComponent *ptr = buffer;
  for (unsigned int i=0; i < num; i++)
    {
    if ( !(i%6) && i ) os << "\n";
    os << *ptr++ << " ";
    }
}
}
void ImageIOBase::WriteBufferAsASCII(std::ostream& os, const void *buffer, 
                                     IOComponentType ctype,
                                     unsigned int numComp)
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
                                    IOComponentType ctype,
                                    unsigned int numComp)
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
  os << indent << "FileType: " << this->GetFileTypeAsString(m_FileType) << std::endl;
  os << indent << "ByteOrder: " << this->GetByteOrderAsString(m_ByteOrder) << std::endl;
  os << indent << "IORegion: " << std::endl;
  m_IORegion.Print(os, indent.GetNextIndent());
  os << indent << "Number of Components/Pixel: " << m_NumberOfComponents << "\n";
  os << indent << "Pixel Type: " << this->GetPixelTypeAsString(m_PixelType) << std::endl;
  os << indent << "Component Type: " << this->GetComponentTypeAsString(m_ComponentType)
     << std::endl;
  os << indent << "Dimensions: ( ";
  for (unsigned int i=0; i < m_NumberOfDimensions; i++)
    {
    os << m_Dimensions[i] << " ";
    }
  os << ")" << std::endl;

  if (m_UseCompression)
    {
    os << indent << "UseCompression: On" << std::endl;
    }
  else
    {
    os << indent << "UseCompression: Off" << std::endl;
    }

}

} //namespace itk
