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

#include "itkMeshIOBase.h"

namespace itk
{
MeshIOBase::MeshIOBase()
  : m_NumberOfPoints(SizeValueType{})
  , m_NumberOfCells(SizeValueType{})
  , m_NumberOfPointPixels(SizeValueType{})
  , m_NumberOfCellPixels(SizeValueType{})
  , m_CellBufferSize(SizeValueType{})

{}

const MeshIOBase::ArrayOfExtensionsType &
MeshIOBase::GetSupportedReadExtensions() const
{
  return this->m_SupportedReadExtensions;
}

const MeshIOBase::ArrayOfExtensionsType &
MeshIOBase::GetSupportedWriteExtensions() const
{
  return this->m_SupportedWriteExtensions;
}

void
MeshIOBase::AddSupportedReadExtension(const char * extension)
{
  this->m_SupportedReadExtensions.emplace_back(extension);
}

void
MeshIOBase::AddSupportedWriteExtension(const char * extension)
{
  this->m_SupportedWriteExtensions.emplace_back(extension);
}

unsigned int
MeshIOBase::GetComponentSize(IOComponentEnum componentType) const
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
MeshIOBase::GetFileTypeAsString(IOFileEnum t) const
{
  switch (t)
  {
    case IOFileEnum::ASCII:
      return { "ASCII" };
    case IOFileEnum::BINARY:
      return { "BINARY" };
    case IOFileEnum::TYPENOTAPPLICABLE:
      break;
    default:
      break;
  }
  return { "TYPENOTAPPLICABLE" };
}

std::string
MeshIOBase::GetByteOrderAsString(IOByteOrderEnum t) const
{
  switch (t)
  {
    case IOByteOrderEnum::BigEndian:
      return { "BigEndian" };
    case IOByteOrderEnum::LittleEndian:
      return { "LittleEndian" };
    case IOByteOrderEnum::OrderNotApplicable:
      break;
    default:
      break;
  }
  return { "OrderNotApplicable" };
}

std::string
MeshIOBase::GetComponentTypeAsString(IOComponentEnum t)
{
  switch (t)
  {
    case IOComponentEnum::UCHAR:
      return { "unsigned_char" };
    case IOComponentEnum::CHAR:
      return { "char" };
    case IOComponentEnum::USHORT:
      return { "unsigned_short" };
    case IOComponentEnum::SHORT:
      return { "short" };
    case IOComponentEnum::UINT:
      return { "unsigned_int" };
    case IOComponentEnum::INT:
      return { "int" };
    case IOComponentEnum::ULONG:
      return { "unsigned_long" };
    case IOComponentEnum::LONG:
      return { "long" };
    case IOComponentEnum::LONGLONG:
      return { "long_long" };
    case IOComponentEnum::ULONGLONG:
      return { "unsigned_long_long" };
    case IOComponentEnum::FLOAT:
      return { "float" };
    case IOComponentEnum::DOUBLE:
      return { "double" };
    case IOComponentEnum::LDOUBLE:
      return { "long_double" };
    case IOComponentEnum::UNKNOWNCOMPONENTTYPE:
      return { "unknown" };
    default:
      break;
  }
  itkGenericExceptionMacro("Unknown component type: " << static_cast<char>(t));
}

std::string
MeshIOBase::GetPixelTypeAsString(IOPixelEnum t)
{
  switch (t)
  {
    case IOPixelEnum::SCALAR:
      return { "scalar" };
    case IOPixelEnum::VECTOR:
      return { "vector" };
    case IOPixelEnum::COVARIANTVECTOR:
      return { "covariant_vector" };
    case IOPixelEnum::POINT:
      return { "point" };
    case IOPixelEnum::OFFSET:
      return { "offset" };
    case IOPixelEnum::RGB:
      return { "rgb" };
    case IOPixelEnum::RGBA:
      return { "rgba" };
    case IOPixelEnum::SYMMETRICSECONDRANKTENSOR:
      return { "symmetric_second_rank_tensor" };
    case IOPixelEnum::DIFFUSIONTENSOR3D:
      return { "diffusion_tensor_3D" };
    case IOPixelEnum::COMPLEX:
      return { "complex" };
    case IOPixelEnum::FIXEDARRAY:
      return { "fixed_array" };
    case IOPixelEnum::ARRAY:
      return { "array" };
    case IOPixelEnum::MATRIX:
      return { "matrix" };
    case IOPixelEnum::VARIABLELENGTHVECTOR:
      return { "variable_length_vector" };
    case IOPixelEnum::VARIABLESIZEMATRIX:
      return { "variable_size_matrix" };
    case IOPixelEnum::UNKNOWNPIXELTYPE:
      return { "unknown" };
    default:
      break;
  }
  itkGenericExceptionMacro("Unknown pixel type: " << static_cast<char>(t));
}

void
MeshIOBase::PrintSelf(std::ostream & os, Indent indent) const
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
