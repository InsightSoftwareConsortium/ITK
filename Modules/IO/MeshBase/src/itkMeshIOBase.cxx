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

#include "itkMeshIOBase.h"

namespace itk
{
MeshIOBase
::MeshIOBase():
  m_ByteOrder(OrderNotApplicable),
  m_FileType(ASCII),
  m_UseCompression(false),
  m_PointComponentType(UNKNOWNCOMPONENTTYPE),
  m_CellComponentType(UNKNOWNCOMPONENTTYPE),
  m_PointPixelComponentType(UNKNOWNCOMPONENTTYPE),
  m_CellPixelComponentType(UNKNOWNCOMPONENTTYPE),
  m_PointPixelType(SCALAR),
  m_CellPixelType(SCALAR),
  m_NumberOfPointPixelComponents(0),
  m_NumberOfCellPixelComponents(0),
  m_PointDimension(0),
  m_NumberOfPoints(itk::NumericTraits< SizeValueType >::ZeroValue()),
  m_NumberOfCells(itk::NumericTraits< SizeValueType >::ZeroValue()),
  m_NumberOfPointPixels(itk::NumericTraits< SizeValueType >::ZeroValue()),
  m_NumberOfCellPixels(itk::NumericTraits< SizeValueType >::ZeroValue()),
  m_CellBufferSize(itk::NumericTraits< SizeValueType >::ZeroValue()),
  m_UpdatePoints(false),
  m_UpdateCells(false),
  m_UpdatePointData(false),
  m_UpdateCellData(false)
{}

const MeshIOBase::ArrayOfExtensionsType &
MeshIOBase
::GetSupportedReadExtensions() const
{
  return this->m_SupportedReadExtensions;
}

const MeshIOBase::ArrayOfExtensionsType &
MeshIOBase
::GetSupportedWriteExtensions() const
{
  return this->m_SupportedWriteExtensions;
}

void
MeshIOBase
::AddSupportedReadExtension(const char *extension)
{
  this->m_SupportedReadExtensions.push_back(extension);
}

void
MeshIOBase
::AddSupportedWriteExtension(const char *extension)
{
  this->m_SupportedWriteExtensions.push_back(extension);
}

unsigned int
MeshIOBase
::GetComponentSize(IOComponentType componentType) const
{
  switch ( componentType )
    {
    case UCHAR:
      return sizeof( unsigned char );
    case CHAR:
      return sizeof( char );
    case USHORT:
      return sizeof( unsigned short );
    case SHORT:
      return sizeof( short );
    case UINT:
      return sizeof( unsigned int );
    case INT:
      return sizeof( int );
    case ULONG:
      return sizeof( unsigned long );
    case LONG:
      return sizeof( long );
    case LONGLONG:
      return sizeof( long long );
    case ULONGLONG:
      return sizeof( unsigned long long );
    case FLOAT:
      return sizeof( float );
    case DOUBLE:
      return sizeof( double );
    case LDOUBLE:
      return sizeof( long double );
    case UNKNOWNCOMPONENTTYPE:
    default:
      itkExceptionMacro ("Unknown component type: " << componentType);
    }
}

std::string
MeshIOBase
::GetFileTypeAsString(FileType t) const
{
  switch ( t )
    {
    case ASCII:
      return std::string("ASCII");
    case BINARY:
      return std::string("BINARY");
    case TYPENOTAPPLICABLE:
      break;
    default:
      break;
    }
  return std::string("TYPENOTAPPLICABLE");
}

std::string
MeshIOBase::
GetByteOrderAsString(ByteOrder t) const
{
  switch ( t )
    {
    case BigEndian:
      return std::string("BigEndian");
    case LittleEndian:
      return std::string("LittleEndian");
    case OrderNotApplicable:
      break;
    default:
      break;
    }
  return std::string("OrderNotApplicable");
}

std::string
MeshIOBase
::GetComponentTypeAsString(IOComponentType t) const
{
  switch ( t )
    {
    case UCHAR:
      return std::string( "unsigned_char" );
    case CHAR:
      return std::string( "char" );
    case USHORT:
      return std::string( "unsigned_short" );
    case SHORT:
      return std::string( "short" );
    case UINT:
      return std::string( "unsigned_int" );
    case INT:
      return std::string( "int" );
    case ULONG:
      return std::string( "unsigned_long" );
    case LONG:
      return std::string( "long" );
    case LONGLONG:
      return std::string( "long_long" );
    case ULONGLONG:
      return std::string( "unsigned_long_long" );
    case FLOAT:
      return std::string( "float" );
    case DOUBLE:
      return std::string( "double" );
    case LDOUBLE:
      return std::string( "long_double" );
    case UNKNOWNCOMPONENTTYPE:
      break;
    default:
      break;
    }
  return std::string( "unknown" );
}

std::string
MeshIOBase
::GetPixelTypeAsString(IOPixelType t) const
{
  switch ( t )
    {
    case SCALAR:
      return std::string( "scalar" );
    case VECTOR:
      return std::string( "vector" );
    case COVARIANTVECTOR:
      return std::string( "covariant_vector" );
    case POINT:
      return std::string( "point" );
    case OFFSET:
      return std::string( "offset" );
    case RGB:
      return std::string( "rgb" );
    case RGBA:
      return std::string( "rgba" );
    case SYMMETRICSECONDRANKTENSOR:
      return std::string( "symmetric_second_rank_tensor" );
    case DIFFUSIONTENSOR3D:
      return std::string( "diffusion_tensor_3D" );
    case COMPLEX:
      return std::string( "complex" );
    case UNKNOWNPIXELTYPE:
      break;
    default:
      break;
    }
  itkExceptionMacro ("Unknown pixel type: " << t);
}

void
MeshIOBase
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << m_FileName << std::endl;
  os << indent << "FileType: " << GetFileTypeAsString(m_FileType) << std::endl;
  os << indent << "ByteOrder: " << GetByteOrderAsString(m_ByteOrder) << std::endl;
  os << indent << "Point dimension: " << m_PointDimension << std::endl;
  os << indent << "Point component type: " << GetComponentTypeAsString(m_PointComponentType) << std::endl;
  os << indent << "Cell  component type: " << GetComponentTypeAsString(m_CellComponentType) << std::endl;
  os << indent << "Number of point pixel components: " << m_NumberOfPointPixelComponents << std::endl;
  os << indent << "Number of cell  pixel components: " << m_NumberOfCellPixelComponents << std::endl;
  os << indent << "Number of points: " << m_NumberOfPointPixels << std::endl;
  os << indent << "Number of cells: " << m_NumberOfCellPixels << std::endl;
  os << indent << "Number of point pixels: " << m_NumberOfPoints << std::endl;
  os << indent << "Number of cell pixels: " << m_NumberOfCells << std::endl;
  os << indent << "Point pixel type: " << GetPixelTypeAsString(m_PointPixelType) << std::endl;
  os << indent << "Cell  pixel type: " << GetPixelTypeAsString(m_CellPixelType) << std::endl;
  os << indent << "Point pixel component type: " << GetComponentTypeAsString(m_PointPixelComponentType) << std::endl;
  os << indent << "Cell  pixel component type: " << GetComponentTypeAsString(m_CellPixelComponentType) << std::endl;
}
} // namespace itk end
