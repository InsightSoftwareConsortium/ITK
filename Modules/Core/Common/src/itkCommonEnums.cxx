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
#include "itkCommonEnums.h"

namespace itk
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const CommonEnums::IOPixel value)
{
  return out << [value] {
    switch (value)
    {
      case CommonEnums::IOPixel::UNKNOWNPIXELTYPE:
        return "itk::CommonEnums::IOPixel::UNKNOWNPIXELTYPE";
      case CommonEnums::IOPixel::SCALAR:
        return "itk::CommonEnums::IOPixel::SCALAR";
      case CommonEnums::IOPixel::RGB:
        return "itk::CommonEnums::IOPixel::RGB";
      case CommonEnums::IOPixel::RGBA:
        return "itk::CommonEnums::IOPixel::RGBA";
      case CommonEnums::IOPixel::OFFSET:
        return "itk::CommonEnums::IOPixel::OFFSET";
      case CommonEnums::IOPixel::VECTOR:
        return "itk::CommonEnums::IOPixel::VECTOR";
      case CommonEnums::IOPixel::POINT:
        return "itk::CommonEnums::IOPixel::POINT";
      case CommonEnums::IOPixel::COVARIANTVECTOR:
        return "itk::CommonEnums::IOPixel::COVARIANTVECTOR";
      case CommonEnums::IOPixel::SYMMETRICSECONDRANKTENSOR:
        return "itk::CommonEnums::IOPixel::SYMMETRICSECONDRANKTENSOR";
      case CommonEnums::IOPixel::DIFFUSIONTENSOR3D:
        return "itk::CommonEnums::IOPixel::DIFFUSIONTENSOR3D";
      case CommonEnums::IOPixel::COMPLEX:
        return "itk::CommonEnums::IOPixel::COMPLEX";
      case CommonEnums::IOPixel::FIXEDARRAY:
        return "itk::CommonEnums::IOPixel::FIXEDARRAY";
      case CommonEnums::IOPixel::ARRAY:
        return "itk::CommonEnums::IOPixel::ARRAY";
      case CommonEnums::IOPixel::MATRIX:
        return "itk::CommonEnums::IOPixel::MATRIX";
      case CommonEnums::IOPixel::VARIABLELENGTHVECTOR:
        return "itk::CommonEnums::IOPixel::VARIABLELENGTHVECTOR";
      case CommonEnums::IOPixel::VARIABLESIZEMATRIX:
        return "itk::CommonEnums::IOPixel::VARIABLESIZEMATRIX";
      default:
        return "INVALID VALUE FOR itk::CommonEnums::IOPixel";
    }
  }();
}

/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const CommonEnums::IOComponent value)
{
  return out << [value] {
    switch (value)
    {
      case CommonEnums::IOComponent::UNKNOWNCOMPONENTTYPE:
        return "itk::CommonEnums::IOComponent::UNKNOWNCOMPONENTTYPE";
      case CommonEnums::IOComponent::UCHAR:
        return "itk::CommonEnums::IOComponent::UCHAR";
      case CommonEnums::IOComponent::CHAR:
        return "itk::CommonEnums::IOComponent::CHAR";
      case CommonEnums::IOComponent::USHORT:
        return "itk::CommonEnums::IOComponent::USHORT";
      case CommonEnums::IOComponent::SHORT:
        return "itk::CommonEnums::IOComponent::SHORT";
      case CommonEnums::IOComponent::UINT:
        return "itk::CommonEnums::IOComponent::UINT";
      case CommonEnums::IOComponent::INT:
        return "itk::CommonEnums::IOComponent::INT";
      case CommonEnums::IOComponent::ULONG:
        return "itk::CommonEnums::IOComponent::ULONG";
      case CommonEnums::IOComponent::LONG:
        return "itk::CommonEnums::IOComponent::LONG";
      case CommonEnums::IOComponent::LONGLONG:
        return "itk::CommonEnums::IOComponent::LONGLONG";
      case CommonEnums::IOComponent::ULONGLONG:
        return "itk::CommonEnums::IOComponent::ULONGLONG";
      case CommonEnums::IOComponent::FLOAT:
        return "itk::CommonEnums::IOComponent::FLOAT";
      case CommonEnums::IOComponent::DOUBLE:
        return "itk::CommonEnums::IOComponent::DOUBLE";
      case CommonEnums::IOComponent::LDOUBLE:
        return "itk::CommonEnums::IOComponent::LDOUBLE";
      default:
        return "INVALID VALUE FOR itk::CommonEnums::IOComponent";
    }
  }();
}

/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const CommonEnums::IOFile value)
{
  return out << [value] {
    switch (value)
    {
      case CommonEnums::IOFile::ASCII:
        return "itk::CommonEnums::IOFile::ASCII";
      case CommonEnums::IOFile::Binary:
        return "itk::CommonEnums::IOFile::Binary";
      case CommonEnums::IOFile::TypeNotApplicable:
        return "itk::CommonEnums::IOFile::TypeNotApplicable";
        //            case CommonEnums::IOFile::BINARY:
        //                return "itk::CommonEnums::IOFile::BINARY";
        //            case CommonEnums::IOFile::TYPENOTAPPLICABLE:
        //                return "itk::CommonEnums::IOFile::TYPENOTAPPLICABLE";
      default:
        return "INVALID VALUE FOR itk::CommonEnums::IOFile";
    }
  }();
}

/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const CommonEnums::IOFileMode value)
{
  return out << [value] {
    switch (value)
    {
      case CommonEnums::IOFileMode::ReadMode:
        return "itk::CommonEnums::IOFileMode::ReadMode";
      case CommonEnums::IOFileMode::WriteMode:
        return "itk::CommonEnums::IOFileMode::WriteMode";
      default:
        return "INVALID VALUE FOR itk::CommonEnums::IOFileMode";
    }
  }();
}

/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const CommonEnums::IOByteOrder value)
{
  return out << [value] {
    switch (value)
    {
      case CommonEnums::IOByteOrder::BigEndian:
        return "itk::CommonEnums::IOByteOrder::BigEndian";
      case CommonEnums::IOByteOrder::LittleEndian:
        return "itk::CommonEnums::IOByteOrder::LittleEndian";
      case CommonEnums::IOByteOrder::OrderNotApplicable:
        return "itk::CommonEnums::IOByteOrder::OrderNotApplicable";
      default:
        return "INVALID VALUE FOR itk::CommonEnums::IOByteOrder";
    }
  }();
}

/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const CommonEnums::CellGeometry value)
{
  return out << [value] {
    switch (value)
    {
      case CommonEnums::CellGeometry::VERTEX_CELL:
        return "itk::CommonEnums::CellGeometry::VERTEX_CELL";
      case CommonEnums::CellGeometry::LINE_CELL:
        return "itk::CommonEnums::CellGeometry::LINE_CELL";
      case CommonEnums::CellGeometry::POLYLINE_CELL:
        return "itk::CommonEnums::CellGeometry::POLYLINE_CELL";
      case CommonEnums::CellGeometry::TRIANGLE_CELL:
        return "itk::CommonEnums::CellGeometry::TRIANGLE_CELL";
      case CommonEnums::CellGeometry::QUADRILATERAL_CELL:
        return "itk::CommonEnums::CellGeometry::QUADRILATERAL_CELL";
      case CommonEnums::CellGeometry::POLYGON_CELL:
        return "itk::CommonEnums::CellGeometry::POLYGON_CELL";
      case CommonEnums::CellGeometry::TETRAHEDRON_CELL:
        return "itk::CommonEnums::CellGeometry::TETRAHEDRON_CELL";
      case CommonEnums::CellGeometry::HEXAHEDRON_CELL:
        return "itk::CommonEnums::CellGeometry::HEXAHEDRON_CELL";
      case CommonEnums::CellGeometry::QUADRATIC_EDGE_CELL:
        return "itk::CommonEnums::CellGeometry::QUADRATIC_EDGE_CELL";
      case CommonEnums::CellGeometry::LAST_ITK_CELL:
        return "itk::CommonEnums::CellGeometry::LAST_ITK_CELL";
      case CommonEnums::CellGeometry::MAX_ITK_CELLS:
        return "itk::CommonEnums::CellGeometry::MAX_ITK_CELLS";
      default:
        return "INVALID VALUE FOR itk::CommonEnums::CellGeometry";
    }
  }();
}

/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const MeshEnums::MeshClassCellsAllocationMethod value)
{
  return out << [value] {
    switch (value)
    {
      case MeshEnums::MeshClassCellsAllocationMethod::CellsAllocationMethodUndefined:
        return "itk::MeshEnums::MeshClassCellsAllocationMethod::CellsAllocationMethodUndefined";
      case MeshEnums::MeshClassCellsAllocationMethod::CellsAllocatedAsStaticArray:
        return "itk::MeshEnums::MeshClassCellsAllocationMethod::CellsAllocatedAsStaticArray";
      case MeshEnums::MeshClassCellsAllocationMethod::CellsAllocatedAsADynamicArray:
        return "itk::MeshEnums::MeshClassCellsAllocationMethod::CellsAllocatedAsADynamicArray";
      case MeshEnums::MeshClassCellsAllocationMethod::CellsAllocatedDynamicallyCellByCell:
        return "itk::MeshEnums::MeshClassCellsAllocationMethod::CellsAllocatedDynamicallyCellByCell";
      default:
        return "INVALID VALUE FOR itk::MeshEnums::MeshClassCellsAllocationMethod";
    }
  }();
}

/** Define how to print enumerations */
std::ostream &
operator<<(std::ostream & out, const OctreeEnums::Octree value)
{
  return out << [value] {
    switch (value)
    {
      case OctreeEnums::Octree::UNKNOWN_PLANE:
        return "itk::OctreeEnums::Octree::UNKNOWN_PLANE";
      case OctreeEnums::Octree::SAGITAL_PLANE:
        return "itk::OctreeEnums::Octree::SAGITAL_PLANE";
      case OctreeEnums::Octree::CORONAL_PLANE:
        return "itk::OctreeEnums::Octree::CORONAL_PLANE";
      case OctreeEnums::Octree::TRANSVERSE_PLANE:
        return "itk::OctreeEnums::Octree::TRANSVERSE_PLANE";
      default:
        return "INVALID VALUE FOR itk::OctreeEnums::Octree";
    }
  }();
}
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const OctreeEnums::LeafIdentifier value)
{
  return out << [value] {
    switch (value)
    {
      case OctreeEnums::LeafIdentifier::ZERO:
        return "itk::OctreeEnums::LeafIdentifier::ZERO";
      case OctreeEnums::LeafIdentifier::ONE:
        return "itk::OctreeEnums::LeafIdentifier::ONE";
      case OctreeEnums::LeafIdentifier::TWO:
        return "itk::OctreeEnums::LeafIdentifier::TWO";
      case OctreeEnums::LeafIdentifier::THREE:
        return "itk::OctreeEnums::LeafIdentifier::THREE";
      case OctreeEnums::LeafIdentifier::FOUR:
        return "itk::OctreeEnums::LeafIdentifier::FOUR";
      case OctreeEnums::LeafIdentifier::FIVE:
        return "itk::OctreeEnums::LeafIdentifier::FIVE";
      case OctreeEnums::LeafIdentifier::SIX:
        return "itk::OctreeEnums::LeafIdentifier::SIX";
      case OctreeEnums::LeafIdentifier::SEVEN:
        return "itk::OctreeEnums::LeafIdentifier::SEVEN";
      default:
        return "INVALID VALUE FOR itk::OctreeEnums::LeafIdentifier";
    }
  }();
}


/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const ObjectEnums::RegionEnum value)
{
  return out << [value] {
    switch (value)
    {
      case ObjectEnums::RegionEnum::ITK_UNSTRUCTURED_REGION:
        return "itk::ObjectEnums::RegionEnum::ITK_UNSTRUCTURED_REGION";
      case ObjectEnums::RegionEnum::ITK_STRUCTURED_REGION:
        return "itk::ObjectEnums::RegionEnum::ITK_STRUCTURED_REGION";
      default:
        return "INVALID VALUE FOR itk::ObjectEnums::RegionEnum";
    }
  }();
}

/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const ObjectFactoryEnums::InsertionPosition value)
{
  return out << [value] {
    switch (value)
    {
      case ObjectFactoryEnums::InsertionPosition::INSERT_AT_FRONT:
        return "itk::ObjectFactoryEnums::InsertionPosition::INSERT_AT_FRONT";
      case ObjectFactoryEnums::InsertionPosition::INSERT_AT_BACK:
        return "itk::ObjectFactoryEnums::InsertionPosition::INSERT_AT_BACK";
      case ObjectFactoryEnums::InsertionPosition::INSERT_AT_POSITION:
        return "itk::ObjectFactoryEnums::InsertionPosition::INSERT_AT_POSITION";
      default:
        return "INVALID VALUE FOR itk::ObjectFactoryEnums::InsertionPosition";
    }
  }();
}
} // namespace itk
