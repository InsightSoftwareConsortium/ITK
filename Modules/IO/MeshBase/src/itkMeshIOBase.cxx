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

#include "itkMeshIOBase.h"

namespace itk
{
MeshIOBase ::MeshIOBase()
  : m_NumberOfPoints(itk::NumericTraits<SizeValueType>::ZeroValue())
  , m_NumberOfCells(itk::NumericTraits<SizeValueType>::ZeroValue())
  , m_NumberOfPointPixels(itk::NumericTraits<SizeValueType>::ZeroValue())
  , m_NumberOfCellPixels(itk::NumericTraits<SizeValueType>::ZeroValue())
  , m_CellBufferSize(itk::NumericTraits<SizeValueType>::ZeroValue())

{}

const MeshIOBase::ArrayOfExtensionsType &
MeshIOBase ::GetSupportedReadExtensions() const
{
  return this->m_SupportedReadExtensions;
}

const MeshIOBase::ArrayOfExtensionsType &
MeshIOBase ::GetSupportedWriteExtensions() const
{
  return this->m_SupportedWriteExtensions;
}

void
MeshIOBase ::AddSupportedReadExtension(const char * extension)
{
  this->m_SupportedReadExtensions.push_back(extension);
}

void
MeshIOBase ::AddSupportedWriteExtension(const char * extension)
{
  this->m_SupportedWriteExtensions.push_back(extension);
}

unsigned int
MeshIOBase ::GetComponentSize(IOComponentEnum componentType) const
{
  switch (componentType)
  {
    case IOComponentEnum::UCHAR:
      return sizeof(unsigned char);
    case IOComponentEnum::CHAR:
      return sizeof(char);
    case IOComponentEnum::USHORT:
      return sizeof(unsigned short);
    case IOComponentEnum::SHORT:
      return sizeof(short);
    case IOComponentEnum::UINT:
      return sizeof(unsigned int);
    case IOComponentEnum::INT:
      return sizeof(int);
    case IOComponentEnum::ULONG:
      return sizeof(unsigned long);
    case IOComponentEnum::LONG:
      return sizeof(long);
    case IOComponentEnum::LONGLONG:
      return sizeof(long long);
    case IOComponentEnum::ULONGLONG:
      return sizeof(unsigned long long);
    case IOComponentEnum::FLOAT:
      return sizeof(float);
    case IOComponentEnum::DOUBLE:
      return sizeof(double);
    case IOComponentEnum::LDOUBLE:
      return sizeof(long double);
    case IOComponentEnum::UNKNOWNCOMPONENTTYPE:
    default:
      itkExceptionMacro("Unknown component type: " << static_cast<char>(componentType));
  }
}

std::string
MeshIOBase ::GetFileTypeAsString(IOFileEnum t) const
{
  switch (t)
  {
    case IOFileEnum::ASCII:
      return std::string("ASCII");
    case IOFileEnum::BINARY:
      return std::string("BINARY");
    case IOFileEnum::TYPENOTAPPLICABLE:
      break;
    default:
      break;
  }
  return std::string("TYPENOTAPPLICABLE");
}

std::string
MeshIOBase::GetByteOrderAsString(IOByteOrderEnum t) const
{
  switch (t)
  {
    case IOByteOrderEnum::BigEndian:
      return std::string("BigEndian");
    case IOByteOrderEnum::LittleEndian:
      return std::string("LittleEndian");
    case IOByteOrderEnum::OrderNotApplicable:
      break;
    default:
      break;
  }
  return std::string("OrderNotApplicable");
}

std::string
MeshIOBase ::GetComponentTypeAsString(IOComponentEnum t) const
{
  switch (t)
  {
    case IOComponentEnum::UCHAR:
      return std::string("unsigned_char");
    case IOComponentEnum::CHAR:
      return std::string("char");
    case IOComponentEnum::USHORT:
      return std::string("unsigned_short");
    case IOComponentEnum::SHORT:
      return std::string("short");
    case IOComponentEnum::UINT:
      return std::string("unsigned_int");
    case IOComponentEnum::INT:
      return std::string("int");
    case IOComponentEnum::ULONG:
      return std::string("unsigned_long");
    case IOComponentEnum::LONG:
      return std::string("long");
    case IOComponentEnum::LONGLONG:
      return std::string("long_long");
    case IOComponentEnum::ULONGLONG:
      return std::string("unsigned_long_long");
    case IOComponentEnum::FLOAT:
      return std::string("float");
    case IOComponentEnum::DOUBLE:
      return std::string("double");
    case IOComponentEnum::LDOUBLE:
      return std::string("long_double");
    case IOComponentEnum::UNKNOWNCOMPONENTTYPE:
      return std::string("unknown");
    default:
      break;
  }
  itkExceptionMacro("Unknown component type: " << static_cast<char>(t));
}

std::string
MeshIOBase ::GetPixelTypeAsString(IOPixelEnum t) const
{
  switch (t)
  {
    case IOPixelEnum::SCALAR:
      return std::string("scalar");
    case IOPixelEnum::VECTOR:
      return std::string("vector");
    case IOPixelEnum::COVARIANTVECTOR:
      return std::string("covariant_vector");
    case IOPixelEnum::POINT:
      return std::string("point");
    case IOPixelEnum::OFFSET:
      return std::string("offset");
    case IOPixelEnum::RGB:
      return std::string("rgb");
    case IOPixelEnum::RGBA:
      return std::string("rgba");
    case IOPixelEnum::SYMMETRICSECONDRANKTENSOR:
      return std::string("symmetric_second_rank_tensor");
    case IOPixelEnum::DIFFUSIONTENSOR3D:
      return std::string("diffusion_tensor_3D");
    case IOPixelEnum::COMPLEX:
      return std::string("complex");
    case IOPixelEnum::FIXEDARRAY:
      return std::string("fixed_array");
    case IOPixelEnum::ARRAY:
      return std::string("array");
    case IOPixelEnum::MATRIX:
      return std::string("matrix");
    case IOPixelEnum::VARIABLELENGTHVECTOR:
      return std::string("variable_length_vector");
    case IOPixelEnum::VARIABLESIZEMATRIX:
      return std::string("variable_size_matrix");
    case IOPixelEnum::UNKNOWNPIXELTYPE:
      return std::string("unknown");
    default:
      break;
  }
  itkExceptionMacro("Unknown pixel type: " << static_cast<char>(t));
}

void
MeshIOBase ::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << m_FileName << std::endl;
  os << indent << "IOFileEnum: " << GetFileTypeAsString(m_FileType) << std::endl;
  os << indent << "IOByteOrderEnum: " << GetByteOrderAsString(m_ByteOrder) << std::endl;
  os << indent << "Point dimension: " << m_PointDimension << std::endl;
  os << indent << "Point component type: " << GetComponentTypeAsString(m_PointComponentType) << std::endl;
  os << indent << "Cell  component type: " << GetComponentTypeAsString(m_CellComponentType) << std::endl;
  os << indent << "Number of point pixel components: " << m_NumberOfPointPixelComponents << std::endl;
  os << indent << "Number of cell  pixel components: " << m_NumberOfCellPixelComponents << std::endl;
  os << indent << "Number of points: " << m_NumberOfPoints << std::endl;
  os << indent << "Number of cells: " << m_NumberOfCells << std::endl;
  os << indent << "Number of point pixels: " << m_NumberOfPointPixels << std::endl;
  os << indent << "Number of cell pixels: " << m_NumberOfCellPixels << std::endl;
  os << indent << "Point pixel type: " << GetPixelTypeAsString(m_PointPixelType) << std::endl;
  os << indent << "Cell  pixel type: " << GetPixelTypeAsString(m_CellPixelType) << std::endl;
  os << indent << "Point pixel component type: " << GetComponentTypeAsString(m_PointPixelComponentType) << std::endl;
  os << indent << "Cell  pixel component type: " << GetComponentTypeAsString(m_CellPixelComponentType) << std::endl;
}
} // namespace itk
