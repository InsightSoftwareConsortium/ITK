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

// Place common enumerations to be used throughout the toolkit in this file
#ifndef itkCommonEnums_h
#define itkCommonEnums_h

#include "itkIntTypes.h"
#include <ostream>

namespace itk
{

/** \class CommonEnums
 *
 * \brief Common enums used across the toolkit
 *
 * This class encapsulates the common enum classes. It is required for
 * wrapping.
 *
 * \ingroup ITKCommon
 **/
class CommonEnums
{
public:
  // Used in Input/Output for Images/Mesh/Transform types
  /**
   * \class IOPixel
   * \ingroup ITKCommon
   * Enums used to manipulate the point/cell pixel type. The pixel type provides
   * context for automatic data conversions (for instance, RGB to
   * SCALAR, VECTOR to SCALAR).
   */
  enum class IOPixel : uint8_t
  {
    UNKNOWNPIXELTYPE,
    SCALAR,
    RGB,
    RGBA,
    OFFSET,
    VECTOR,
    POINT,
    COVARIANTVECTOR,
    SYMMETRICSECONDRANKTENSOR,
    DIFFUSIONTENSOR3D,
    COMPLEX,
    FIXEDARRAY,
    ARRAY,
    MATRIX,
    VARIABLELENGTHVECTOR,
    VARIABLESIZEMATRIX
  };

  /**
   * \class IOComponent
   * \ingroup ITKCommon
   * Enums used to manipulate the component type. The component type
   * refers to the actual storage class associated with either a
   * SCALAR pixel type or elements of a compound pixel.
   */
  enum class IOComponent : uint8_t
  {
    UNKNOWNCOMPONENTTYPE,
    UCHAR,
    CHAR,
    USHORT,
    SHORT,
    UINT,
    INT,
    ULONG,
    LONG,
    LONGLONG,
    ULONGLONG,
    FLOAT,
    DOUBLE,
    LDOUBLE
  };

  /**
   * \class IOFile
   * \ingroup ITKCommon
   * Enums used to specify write style: whether binary or ASCII. Some
   * subclasses use this, some ignore it.
   */
  enum class IOFile : uint8_t
  {
    ASCII = 0,
    Binary = 1,
    TypeNotApplicable = 2,
    // for backward compatibility
    BINARY = 1,           // Spelling difference between IOMeshBase and IOImageBase
    TYPENOTAPPLICABLE = 2 // Spelling difference between IOMeshBase and IOImageBase
  };

  /**
   * \class IOFileMode
   * \ingroup ITKCommon
   * Mode in which the files is intended to be used
   */
  enum class IOFileMode : uint8_t
  {
    ReadMode,
    WriteMode
  };

  /**
   * \class IOByteOrder
   * \ingroup ITKCommon
   * Enums used to specify byte order; whether Big Endian or Little Endian.
   * Some subclasses use this, some ignore it.
   */
  enum class IOByteOrder : uint8_t
  {
    BigEndian,
    LittleEndian,
    OrderNotApplicable
  };

  /**
   * \class CellGeometry
   * \ingroup ITKCommon
   * Enums used to specify cell type */
  enum class CellGeometry : uint8_t
  {
    VERTEX_CELL = 0,
    LINE_CELL = 1,
    TRIANGLE_CELL = 2,
    QUADRILATERAL_CELL = 3,
    POLYGON_CELL = 4,
    TETRAHEDRON_CELL = 5,
    HEXAHEDRON_CELL = 6,
    QUADRATIC_EDGE_CELL = 7,
    QUADRATIC_TRIANGLE_CELL = 8,
    LAST_ITK_CELL = 9,
    MAX_ITK_CELLS = 255
  };

}; // CommonEnums

// Convenience
using IOPixelEnum = CommonEnums::IOPixel;
using IOComponentEnum = CommonEnums::IOComponent;
using IOFileEnum = CommonEnums::IOFile;
using IOFileModeEnum = CommonEnums::IOFileMode;
using IOByteOrderEnum = CommonEnums::IOByteOrder;
using CellGeometryEnum = CommonEnums::CellGeometry;

#if !defined(ITK_LEGACY_REMOVE)
/** Expose old names for backwards compatibility*/
using IOPixelType = CommonEnums::IOPixel;
using IOComponentType = CommonEnums::IOComponent;
using IOFileType = CommonEnums::IOFile;
using IOFileModeType = CommonEnums::IOFileMode;
using IOByteOrderType = CommonEnums::IOByteOrder;
using CellGeometryType = CommonEnums::CellGeometry;
#endif
// Define how to print enumeration
extern ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, IOPixelEnum value);
extern ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, IOComponentEnum value);
extern ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, IOFileEnum value);
extern ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, IOFileModeEnum value);
extern ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, IOByteOrderEnum value);
extern ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, CellGeometryEnum value);

/** \class MeshEnums
 * \ingroup ITKCommon
 */
class MeshEnums
{
public:
  /** \class MeshClassCellsAllocationMethod
   * \ingroup ITKCommon
   * Enum defining the possible methods used to allocate memory for
   * the Cells */
  enum class MeshClassCellsAllocationMethod : uint8_t
  {
    CellsAllocationMethodUndefined,
    CellsAllocatedAsStaticArray,
    CellsAllocatedAsADynamicArray,
    CellsAllocatedDynamicallyCellByCell
  };
};
extern ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, MeshEnums::MeshClassCellsAllocationMethod value);
/**\class OctreeEnums
 * \brief
 * \ingroup ITKCommon
 */
class OctreeEnums
{
public:
  /**
   * \class Octree
   * \ingroup ITKCommon
   * The enumeration to define the planar orientation of the octree
   */
  enum class Octree : uint8_t
  {
    UNKNOWN_PLANE,   ///< The plane is Unknown
    SAGITAL_PLANE,   ///< The plane is Sagital
    CORONAL_PLANE,   ///< The plane is Coronal
    TRANSVERSE_PLANE ///< The plane is Transverse
  };
  /***
   * \class LeafIdentifier
   * \ingroup ITKCommon
   */
  enum class LeafIdentifier : uint8_t
  {
    ZERO = 0,
    ONE = 1,
    TWO = 2,
    THREE = 3,
    FOUR = 4,
    FIVE = 5,
    SIX = 6,
    SEVEN = 7
  };
};

// Define how to print enumeration
extern ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, const OctreeEnums::Octree value);
extern ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, const OctreeEnums::LeafIdentifier value);
/**\class ObjectEnums
 * \ingroup ITKCommon
 */
class ObjectEnums
{
public:
  /**\class RegionEnum
   * \ingroup ITKCommon
   * Enums used to describe the extent types. */
  enum class RegionEnum : uint8_t
  {
    ITK_UNSTRUCTURED_REGION,
    ITK_STRUCTURED_REGION
  };
};

extern ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, const ObjectEnums::RegionEnum value);

/**\class ObjectFactoryEnums
 * \ingroup ITKCommon
 */
class ObjectFactoryEnums
{
public:
  /** \class InsertionPosition
   *  \ingroup ITKCommon
   *  Position at which the new factory will be registered in the
   *  internal factory container.
   */
  enum class InsertionPosition : uint8_t
  {
    INSERT_AT_FRONT,
    INSERT_AT_BACK,
    INSERT_AT_POSITION
  };
};
extern ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, const ObjectFactoryEnums::InsertionPosition value);

} // namespace itk

#endif // itkCommonEnums_h
