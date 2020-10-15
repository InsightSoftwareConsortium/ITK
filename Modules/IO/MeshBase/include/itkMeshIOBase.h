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
#ifndef itkMeshIOBase_h
#define itkMeshIOBase_h
#include "ITKIOMeshBaseExport.h"

#include "itkByteSwapper.h"
#include "itkCellInterface.h"
#include "itkCovariantVector.h"
#include "itkDiffusionTensor3D.h"
#include "itkIntTypes.h"
#include "itkLightProcessObject.h"
#include "itkMatrix.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"
#include "itkSymmetricSecondRankTensor.h"
#include "itkVariableLengthVector.h"
#include "itkVariableSizeMatrix.h"
#include "itkVector.h"
#include "itkNumberToString.h"
#include "itkCommonEnums.h"

#include <string>
#include <complex>
#include <fstream>

namespace itk
{
/**
 *\class MeshIOBase
 * \brief Abstract superclass defines mesh IO interface.
 *
 * MeshIOBase is a class that reads and/or writes Mesh / QuadEdgeMesh data
 * of a particular format (such as PNG or raw binary). The
 * MeshIOBase encapsulates both the reading and writing of data. The
 * MeshIOBase is used by the MeshFileReader class (to read data)
 * and the MeshFileWriter (to write data) into a single file.
 * Normally the user does not directly
 * manipulate this class other than to instantiate it, set the FileName,
 * and assign it to a MeshFileReader or MeshFileWriter.
 *
 * A Pluggable factory pattern is used this allows different kinds of readers
 * to be registered (even at run time) without having to modify the
 * code in this class.
 *
 * \author Wanlin Zhu. Uviversity of New South Wales, Australia.
 *
 * \sa MeshFileWriter
 * \sa MeshFileReader
 *
 * \ingroup IOFilters
 * \ingroup ITKIOMeshBase
 *
 */

class ITKIOMeshBase_EXPORT MeshIOBase : public LightProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MeshIOBase);

  /** Standard class type aliases. */
  using Self = MeshIOBase;
  using Superclass = LightProcessObject;
  using ConstPointer = SmartPointer<const Self>;
  using Pointer = SmartPointer<Self>;

  /** Type for the list of strings to be used for extensions.  */
  using ArrayOfExtensionsType = std::vector<std::string>;

  /** Type for representing size of bytes, and or positions along a file */
  using StreamOffsetType = std::streamoff;

  using SizeValueType = IdentifierType;

  /**
   * \class UnknownType
   * Used to return information when types are unknown.
   * \ingroup ITKIOMeshBase
   */
  class UnknownType
  {};

  /** Run-time type information (and related methods). */
  itkTypeMacro(MeshIOBase, LightProcessObject);

  /** Set/Get the name of the file to be read. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);

#if !defined(ITK_LEGACY_REMOVE)
  /**Exposes enums values for backwards compatibility*/
  static constexpr IOPixelEnum UNKNOWNPIXELTYPE = IOPixelEnum::UNKNOWNPIXELTYPE;
  static constexpr IOPixelEnum SCALAR = IOPixelEnum::SCALAR;
  static constexpr IOPixelEnum RGB = IOPixelEnum::RGB;
  static constexpr IOPixelEnum RGBA = IOPixelEnum::RGBA;
  static constexpr IOPixelEnum OFFSET = IOPixelEnum::OFFSET;
  static constexpr IOPixelEnum VECTOR = IOPixelEnum::VECTOR;
  static constexpr IOPixelEnum POINT = IOPixelEnum::POINT;
  static constexpr IOPixelEnum COVARIANTVECTOR = IOPixelEnum::COVARIANTVECTOR;
  static constexpr IOPixelEnum SYMMETRICSECONDRANKTENSOR = IOPixelEnum::SYMMETRICSECONDRANKTENSOR;
  static constexpr IOPixelEnum DIFFUSIONTENSOR3D = IOPixelEnum::DIFFUSIONTENSOR3D;
  static constexpr IOPixelEnum COMPLEX = IOPixelEnum::COMPLEX;
  static constexpr IOPixelEnum FIXEDARRAY = IOPixelEnum::FIXEDARRAY;
  static constexpr IOPixelEnum ARRAY = IOPixelEnum::ARRAY;
  static constexpr IOPixelEnum MATRIX = IOPixelEnum::MATRIX;
  static constexpr IOPixelEnum VARIABLELENGTHVECTOR = IOPixelEnum::VARIABLELENGTHVECTOR;
  static constexpr IOPixelEnum VARIABLESIZEMATRIX = IOPixelEnum::VARIABLESIZEMATRIX;
#endif

#if !defined(ITK_LEGACY_REMOVE)
  /**Exposes enums values for backwards compatibility*/
  static constexpr IOComponentEnum UNKNOWNCOMPONENTTYPE = IOComponentEnum::UNKNOWNCOMPONENTTYPE;
  static constexpr IOComponentEnum UCHAR = IOComponentEnum::UCHAR;
  static constexpr IOComponentEnum CHAR = IOComponentEnum::CHAR;
  static constexpr IOComponentEnum USHORT = IOComponentEnum::USHORT;
  static constexpr IOComponentEnum SHORT = IOComponentEnum::SHORT;
  static constexpr IOComponentEnum UINT = IOComponentEnum::UINT;
  static constexpr IOComponentEnum INT = IOComponentEnum::INT;
  static constexpr IOComponentEnum ULONG = IOComponentEnum::ULONG;
  static constexpr IOComponentEnum LONG = IOComponentEnum::LONG;
  static constexpr IOComponentEnum ULONGLONG = IOComponentEnum::ULONGLONG;
  static constexpr IOComponentEnum LONGLONG = IOComponentEnum::LONGLONG;
  static constexpr IOComponentEnum FLOAT = IOComponentEnum::FLOAT;
  static constexpr IOComponentEnum DOUBLE = IOComponentEnum::DOUBLE;
  static constexpr IOComponentEnum LDOUBLE = IOComponentEnum::LDOUBLE;
#endif

#if !defined(ITK_LEGACY_REMOVE)
  /**Exposes enums values for backwards compatibility*/
  static constexpr IOFileEnum ASCII = IOFileEnum::ASCII;
  static constexpr IOFileEnum BINARY = IOFileEnum::BINARY;
  static constexpr IOFileEnum TYPENOTAPPLICABLE = IOFileEnum::TYPENOTAPPLICABLE;
#endif

#if !defined(ITK_LEGACY_REMOVE)
  /**Exposes enums values for backwards compatibility*/
  static constexpr IOByteOrderEnum BigEndian = IOByteOrderEnum::BigEndian;
  static constexpr IOByteOrderEnum LittleEndian = IOByteOrderEnum::LittleEndian;
  static constexpr IOByteOrderEnum OrderNotApplicable = IOByteOrderEnum::OrderNotApplicable;
#endif

#if !defined(ITK_LEGACY_REMOVE)
  /**Exposes enums values for backwards compatibility*/
  static constexpr IOFileModeEnum ReadMode = IOFileModeEnum::ReadMode;
  static constexpr IOFileModeEnum WriteMode = IOFileModeEnum::WriteMode;
#endif

#if !defined(ITK_LEGACY_REMOVE)
  /**Exposes enums values for backwards compatibility*/
  static constexpr CellGeometryEnum VERTEX_CELL = CellGeometryEnum::VERTEX_CELL;
  static constexpr CellGeometryEnum LINE_CELL = CellGeometryEnum::LINE_CELL;
  static constexpr CellGeometryEnum TRIANGLE_CELL = CellGeometryEnum::TRIANGLE_CELL;
  static constexpr CellGeometryEnum QUADRILATERAL_CELL = CellGeometryEnum::QUADRILATERAL_CELL;
  static constexpr CellGeometryEnum POLYGON_CELL = CellGeometryEnum::POLYGON_CELL;
  static constexpr CellGeometryEnum TETRAHEDRON_CELL = CellGeometryEnum::TETRAHEDRON_CELL;
  static constexpr CellGeometryEnum HEXAHEDRON_CELL = CellGeometryEnum::HEXAHEDRON_CELL;
  static constexpr CellGeometryEnum QUADRATIC_EDGE_CELL = CellGeometryEnum::QUADRATIC_EDGE_CELL;
  static constexpr CellGeometryEnum QUADRATIC_TRIANGLE_CELL = CellGeometryEnum::QUADRATIC_TRIANGLE_CELL;
  static constexpr CellGeometryEnum LAST_ITK_CELL = CellGeometryEnum::LAST_ITK_CELL;
  static constexpr CellGeometryEnum MAX_ITK_CELLS = CellGeometryEnum::MAX_ITK_CELLS;
#endif

  /** Set/Get the type of the point/cell pixel. The PixelTypes provides context
   * to the IO mechanisms for data conversions.  PixelTypes can be
   * SCALAR, RGB, RGBA, VECTOR, COVARIANTVECTOR, POINT, INDEX. If
   * the PIXELTYPE is SCALAR, then the NumberOfComponents should be 1.
   * Any other of PIXELTYPE will have more than one component. */
  itkSetEnumMacro(PointPixelType, ::itk::CommonEnums::IOPixel);
  itkGetEnumMacro(PointPixelType, ::itk::CommonEnums::IOPixel);
  itkSetEnumMacro(CellPixelType, ::itk::CommonEnums::IOPixel);
  itkGetEnumMacro(CellPixelType, ::itk::CommonEnums::IOPixel);

  /** Set/Get the component type of the point, cell, point data and cell data.
    This is always a native type. */
  itkSetEnumMacro(PointComponentType, ::itk::CommonEnums::IOComponent);
  itkGetEnumMacro(PointComponentType, ::itk::CommonEnums::IOComponent);
  itkSetEnumMacro(CellComponentType, ::itk::CommonEnums::IOComponent);
  itkGetEnumMacro(CellComponentType, ::itk::CommonEnums::IOComponent);
  itkSetEnumMacro(PointPixelComponentType, ::itk::CommonEnums::IOComponent);
  itkGetEnumMacro(PointPixelComponentType, ::itk::CommonEnums::IOComponent);
  itkSetEnumMacro(CellPixelComponentType, ::itk::CommonEnums::IOComponent);
  itkGetEnumMacro(CellPixelComponentType, ::itk::CommonEnums::IOComponent);

  template <typename T>
  struct MapComponentType
  {
    static constexpr IOComponentEnum CType = IOComponentEnum::UNKNOWNCOMPONENTTYPE;
  };

  template <typename T>
  void
  SetPixelType(const T & itkNotUsed(dummy), bool UsePointPixel = true)
  {
    if (UsePointPixel)
    {
      SetNumberOfPointPixelComponents(1);
      SetPointPixelComponentType(MapComponentType<T>::CType);
      SetPointPixelType(IOPixelEnum::SCALAR);
    }
    else
    {
      SetNumberOfCellPixelComponents(1);
      SetCellPixelComponentType(MapComponentType<T>::CType);
      SetCellPixelType(IOPixelEnum::SCALAR);
    }
  }

  template <typename T>
  void
  SetPixelType(const RGBPixel<T> & itkNotUsed(dummy), bool UsePointPixel = true)
  {
    if (UsePointPixel)
    {
      SetNumberOfPointPixelComponents(3);
      SetPointPixelComponentType(MapComponentType<T>::CType);
      SetPointPixelType(IOPixelEnum::RGB);
    }
    else
    {
      SetNumberOfCellPixelComponents(3);
      SetCellPixelComponentType(MapComponentType<T>::CType);
      SetCellPixelType(IOPixelEnum::RGB);
    }
  }

  template <typename T>
  void
  SetPixelType(const RGBAPixel<T> & itkNotUsed(dummy), bool UsePointPixel = true)
  {
    if (UsePointPixel)
    {
      SetNumberOfPointPixelComponents(4);
      SetPointPixelComponentType(MapComponentType<T>::CType);
      SetPointPixelType(IOPixelEnum::RGBA);
    }
    else
    {
      SetNumberOfCellPixelComponents(4);
      SetCellPixelComponentType(MapComponentType<T>::CType);
      SetCellPixelType(IOPixelEnum::RGBA);
    }
  }

  template <typename T, unsigned int VLength>
  void
  SetPixelType(const Vector<T, VLength> & itkNotUsed(dummy), bool UsePointPixel = true)
  {
    if (UsePointPixel)
    {
      SetNumberOfPointPixelComponents(VLength);
      SetPointPixelComponentType(MapComponentType<T>::CType);
      SetPointPixelType(IOPixelEnum::VECTOR);
    }
    else
    {
      SetNumberOfCellPixelComponents(VLength);
      SetCellPixelComponentType(MapComponentType<T>::CType);
      SetCellPixelType(IOPixelEnum::VECTOR);
    }
  }

  template <typename T, unsigned int VLength>
  void
  SetPixelType(const CovariantVector<T, VLength> & itkNotUsed(dummy), bool UsePointPixel = true)
  {
    if (UsePointPixel)
    {
      SetNumberOfPointPixelComponents(VLength);
      SetPointPixelComponentType(MapComponentType<T>::CType);
      SetPointPixelType(IOPixelEnum::COVARIANTVECTOR);
    }
    else
    {
      SetNumberOfCellPixelComponents(VLength);
      SetCellPixelComponentType(MapComponentType<T>::CType);
      SetCellPixelType(IOPixelEnum::COVARIANTVECTOR);
    }
  }

  template <typename T, unsigned int VLength>
  void
  SetPixelType(const FixedArray<T, VLength> & itkNotUsed(dummy), bool UsePointPixel = true)
  {
    if (UsePointPixel)
    {
      SetNumberOfPointPixelComponents(VLength);
      SetPointPixelComponentType(MapComponentType<T>::CType);
      SetPointPixelType(IOPixelEnum::FIXEDARRAY);
    }
    else
    {
      SetNumberOfCellPixelComponents(VLength);
      SetCellPixelComponentType(MapComponentType<T>::CType);
      SetCellPixelType(IOPixelEnum::FIXEDARRAY);
    }
  }

  template <typename T, unsigned int VLength>
  void
  SetPixelType(const SymmetricSecondRankTensor<T, VLength> itkNotUsed(dummy), bool UsePointPixel = true)
  {
    if (UsePointPixel)
    {
      SetNumberOfPointPixelComponents(VLength * (VLength + 1) / 2);
      SetPointPixelComponentType(MapComponentType<T>::CType);
      SetPointPixelType(IOPixelEnum::SYMMETRICSECONDRANKTENSOR);
    }
    else
    {
      SetNumberOfCellPixelComponents(VLength * (VLength + 1) / 2);
      SetCellPixelComponentType(MapComponentType<T>::CType);
      SetCellPixelType(IOPixelEnum::SYMMETRICSECONDRANKTENSOR);
    }
  }

  template <typename T>
  void
  SetPixelType(const DiffusionTensor3D<T> & itkNotUsed(dummy), bool UsePointPixel = true)
  {
    if (UsePointPixel)
    {
      SetNumberOfPointPixelComponents(6);
      SetPointPixelComponentType(MapComponentType<T>::CType);
      SetPointPixelType(IOPixelEnum::DIFFUSIONTENSOR3D);
    }
    else
    {
      SetNumberOfCellPixelComponents(6);
      SetCellPixelComponentType(MapComponentType<T>::CType);
      SetCellPixelType(IOPixelEnum::DIFFUSIONTENSOR3D);
    }
  }

  template <typename T, unsigned int NR, unsigned int NC>
  void
  SetPixelType(const Matrix<T, NR, NC> & itkNotUsed(dummy), bool UsePointPixel = true)
  {
    if (UsePointPixel)
    {
      SetNumberOfPointPixelComponents(NR * NC);
      SetPointPixelComponentType(MapComponentType<T>::CType);
      SetPointPixelType(IOPixelEnum::MATRIX);
    }
    else
    {
      SetNumberOfCellPixelComponents(NR * NC);
      SetCellPixelComponentType(MapComponentType<T>::CType);
      SetCellPixelType(IOPixelEnum::MATRIX);
    }
  }

  template <typename T>
  void
  SetPixelType(const std::complex<T> & itkNotUsed(dummy), bool UsePointPixel = true)
  {
    if (UsePointPixel)
    {
      SetNumberOfPointPixelComponents(2);
      SetPointPixelComponentType(MapComponentType<T>::CType);
      SetPointPixelType(IOPixelEnum::COMPLEX);
    }
    else
    {
      SetNumberOfCellPixelComponents(2);
      SetCellPixelComponentType(MapComponentType<T>::CType);
      SetCellPixelType(IOPixelEnum::COMPLEX);
    }
  }

  template <typename T>
  void
  SetPixelType(const Array<T> & array, bool UsePointPixel = true)
  {
    if (UsePointPixel)
    {
      SetNumberOfPointPixelComponents(array.Size());
      SetPointPixelComponentType(MapComponentType<T>::CType);
      SetPointPixelType(IOPixelEnum::ARRAY);
    }
    else
    {
      SetNumberOfCellPixelComponents(array.Size());
      SetCellPixelComponentType(MapComponentType<T>::CType);
      SetCellPixelType(IOPixelEnum::ARRAY);
    }
  }

  template <typename T>
  void
  SetPixelType(const VariableLengthVector<T> & vector, bool UsePointPixel = true)
  {
    if (UsePointPixel)
    {
      SetNumberOfPointPixelComponents(vector.Size());
      SetPointPixelComponentType(MapComponentType<T>::CType);
      SetPointPixelType(IOPixelEnum::VARIABLELENGTHVECTOR);
    }
    else
    {
      SetNumberOfCellPixelComponents(vector.Size());
      SetCellPixelComponentType(MapComponentType<T>::CType);
      SetCellPixelType(IOPixelEnum::VARIABLELENGTHVECTOR);
    }
  }

  template <typename T>
  void
  SetPixelType(const VariableSizeMatrix<T> & matrix, bool UsePointPixel = true)
  {
    if (UsePointPixel)
    {
      SetNumberOfPointPixelComponents(matrix.Rows() * matrix.Cols());
      SetPointPixelComponentType(MapComponentType<T>::CType);
      SetPointPixelType(IOPixelEnum::VARIABLESIZEMATRIX);
    }
    else
    {
      SetNumberOfCellPixelComponents(matrix.Rows() * matrix.Cols());
      SetCellPixelComponentType(MapComponentType<T>::CType);
      SetCellPixelType(IOPixelEnum::VARIABLESIZEMATRIX);
    }
  }

  /** Set/Get the number of components per pixel in the mesh. This may
   * be set by the reading process. For SCALAR pixel types,
   * NumberOfComponents will be 1.  For other pixel types,
   * NumberOfComponents will be greater than or equal to one. */
  itkSetMacro(NumberOfPointPixelComponents, unsigned int);
  itkGetConstMacro(NumberOfPointPixelComponents, unsigned int);
  itkSetMacro(NumberOfCellPixelComponents, unsigned int);
  itkGetConstMacro(NumberOfCellPixelComponents, unsigned int);
  itkSetMacro(PointDimension, unsigned int);
  itkGetConstMacro(PointDimension, unsigned int);
  itkSetMacro(NumberOfPoints, SizeValueType);
  itkGetConstMacro(NumberOfPoints, SizeValueType);
  itkSetMacro(NumberOfCells, SizeValueType);
  itkGetConstMacro(NumberOfCells, SizeValueType);
  itkSetMacro(NumberOfPointPixels, SizeValueType);
  itkGetConstMacro(NumberOfPointPixels, SizeValueType);
  itkSetMacro(NumberOfCellPixels, SizeValueType);
  itkGetConstMacro(NumberOfCellPixels, SizeValueType);
  itkSetMacro(CellBufferSize, SizeValueType);
  itkGetConstMacro(CellBufferSize, SizeValueType);
  itkSetMacro(UpdatePoints, bool);
  itkGetConstMacro(UpdatePoints, bool);
  itkSetMacro(UpdateCells, bool);
  itkGetConstMacro(UpdateCells, bool);
  itkSetMacro(UpdatePointData, bool);
  itkGetConstMacro(UpdatePointData, bool);
  itkSetMacro(UpdateCellData, bool);
  itkGetConstMacro(UpdateCellData, bool);

  unsigned int
  GetComponentSize(IOComponentEnum componentType) const;

  /** Convenience method returns the IOComponentEnum as a string. This can be
   * used for writing output files. */
  std::string GetComponentTypeAsString(IOComponentEnum) const;

  /** Convenience method returns the IOPixelEnum as a string. This can be
   * used for writing output files. */
  std::string GetPixelTypeAsString(IOPixelEnum) const;

  /** These methods control whether the file is written binary or ASCII.
   * Many file formats (i.e., subclasses) ignore this flag. */
  itkSetEnumMacro(FileType, IOFileEnum);
  itkGetEnumMacro(FileType, IOFileEnum);

  void
  SetFileTypeToASCII()
  {
    this->SetFileType(IOFileEnum::ASCII);
  }

  void
  SetFileTypeToBinary()
  {
    this->SetFileType(IOFileEnum::BINARY);
  }

  /** These methods indicate the byte ordering of the file you are
   * trying to read in. These methods will then either swap or not
   * swap the bytes depending on the byte ordering of the machine it
   * is being run on. For example, reading in a BigEndian file on a
   * BigEndian machine will result in no swapping. Trying to read the
   * same file on a LittleEndian machine will result in swapping.
   * Note: most UNIX machines are BigEndian while PC's and VAX's are
   * LittleEndian. So if the file you are reading in was generated on
   * a VAX or PC, SetByteOrderToLittleEndian() otherwise
   * SetByteOrderToBigEndian().  Some MeshIOBase subclasses
   * ignore these methods. */
  itkSetEnumMacro(ByteOrder, IOByteOrderEnum);
  itkGetEnumMacro(ByteOrder, IOByteOrderEnum);

  void
  SetByteOrderToBigEndian()
  {
    this->SetByteOrder(IOByteOrderEnum::BigEndian);
  }

  void
  SetByteOrderToLittleEndian()
  {
    this->SetByteOrder(IOByteOrderEnum::LittleEndian);
  }

  /** Set/Get a boolean to use the compression or not. */
  itkSetMacro(UseCompression, bool);
  itkGetConstMacro(UseCompression, bool);
  itkBooleanMacro(UseCompression);

  /** Convenience method returns the IOFileEnum as a string. This can be
   * used for writing output files. */
  std::string GetFileTypeAsString(IOFileEnum) const;

  /** Convenience method returns the IOByteOrderEnum as a string. This can be
   * used for writing output files. */
  std::string GetByteOrderAsString(IOByteOrderEnum) const;

  /*-------- This part of the interfaces deals with reading data ----- */
  /** Determine the file type. Returns true if this MeshIO can read the
   * file specified. */
  virtual bool
  CanReadFile(const char *) = 0;

  /** Determin the required information and whether need to ReadPoints,
    ReadCells, ReadPointData and ReadCellData */
  virtual void
  ReadMeshInformation() = 0;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void
  ReadPoints(void * buffer) = 0;

  virtual void
  ReadCells(void * buffer) = 0;

  virtual void
  ReadPointData(void * buffer) = 0;

  virtual void
  ReadCellData(void * buffer) = 0;

  /*-------- This part of the interfaces deals with writing data ----- */

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual bool
  CanWriteFile(const char *) = 0;

  virtual void
  WriteMeshInformation() = 0;

  virtual void
  WritePoints(void * buffer) = 0;

  virtual void
  WriteCells(void * buffer) = 0;

  virtual void
  WritePointData(void * buffer) = 0;

  virtual void
  WriteCellData(void * buffer) = 0;

  virtual void
  Write() = 0;

  /** This method returns an array with the list of filename extensions
   * supported for reading by this MeshIO class. This is intended to
   * facilitate GUI and application level integration.
   */
  const ArrayOfExtensionsType &
  GetSupportedReadExtensions() const;

  /** This method returns an array with the list of filename extensions
   * supported for writing by this MeshIO class. This is intended to
   * facilitate GUI and application level integration.
   */
  const ArrayOfExtensionsType &
  GetSupportedWriteExtensions() const;

protected:
  MeshIOBase();
  ~MeshIOBase() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Insert an extension to the list of supported extensions for reading. */
  void
  AddSupportedReadExtension(const char * extension);

  /** Insert an extension to the list of supported extensions for writing. */
  void
  AddSupportedWriteExtension(const char * extension);

  /** Read data from input file stream to buffer with ascii style */
  template <typename T>
  void
  ReadBufferAsAscii(T * buffer, std::ifstream & inputFile, SizeValueType numberOfComponents)
  {
    for (SizeValueType i = 0; i < numberOfComponents; i++)
    {
      inputFile >> buffer[i];
    }
  }

  /** Read data from input file to buffer with binary style */
  template <typename T>
  void
  ReadBufferAsBinary(T * buffer, std::ifstream & inputFile, SizeValueType numberOfComponents)
  {
    inputFile.read(reinterpret_cast<char *>(buffer), numberOfComponents * sizeof(T));

    if (m_ByteOrder == IOByteOrderEnum::BigEndian)
    {
      if (itk::ByteSwapper<T>::SystemIsLittleEndian())
      {
        itk::ByteSwapper<T>::SwapRangeFromSystemToBigEndian(buffer, numberOfComponents);
      }
    }
    else if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
    {
      if (itk::ByteSwapper<T>::SystemIsBigEndian())
      {
        itk::ByteSwapper<T>::SwapRangeFromSystemToLittleEndian(buffer, numberOfComponents);
      }
    }
  }

  /** Write buffer to output file stream with ascii style */
  template <typename T>
  void
  WriteBufferAsAscii(T *             buffer,
                     std::ofstream & outputFile,
                     SizeValueType   numberOfLines,
                     SizeValueType   numberOfComponents)
  {
    NumberToString<T> convert;
    for (SizeValueType ii = 0; ii < numberOfLines; ii++)
    {
      for (SizeValueType jj = 0; jj < numberOfComponents; jj++)
      {
        outputFile << convert(buffer[ii * numberOfComponents + jj]) << "  ";
      }
      outputFile << '\n';
    }
  }

  /** Write buffer to output file stream with binary style */
  template <typename TOutput, typename TInput>
  void
  WriteBufferAsBinary(TInput * buffer, std::ofstream & outputFile, SizeValueType numberOfComponents)
  {
    if (typeid(TInput) == typeid(TOutput))
    {
      if (m_ByteOrder == IOByteOrderEnum::BigEndian && itk::ByteSwapper<TInput>::SystemIsLittleEndian())
      {
        itk::ByteSwapper<TInput>::SwapRangeFromSystemToBigEndian(buffer, numberOfComponents);
      }
      else if (m_ByteOrder == IOByteOrderEnum::LittleEndian && itk::ByteSwapper<TInput>::SystemIsBigEndian())
      {
        itk::ByteSwapper<TInput>::SwapRangeFromSystemToLittleEndian(buffer, numberOfComponents);
      }

      outputFile.write(reinterpret_cast<char *>(buffer), numberOfComponents);
    }
    else
    {
      auto * data = new TOutput[numberOfComponents];
      for (SizeValueType ii = 0; ii < numberOfComponents; ii++)
      {
        data[ii] = static_cast<TOutput>(buffer[ii]);
      }

      if (m_ByteOrder == IOByteOrderEnum::BigEndian && itk::ByteSwapper<TOutput>::SystemIsLittleEndian())
      {
        itk::ByteSwapper<TOutput>::SwapRangeFromSystemToBigEndian(data, numberOfComponents);
      }
      else if (m_ByteOrder == IOByteOrderEnum::LittleEndian && itk::ByteSwapper<TOutput>::SystemIsBigEndian())
      {
        itk::ByteSwapper<TOutput>::SwapRangeFromSystemToLittleEndian(data, numberOfComponents);
      }

      outputFile.write(reinterpret_cast<char *>(data), numberOfComponents);
      delete[] data;
    }
  }

  /** Read cells from a data buffer, used when writting cells. This function
    write all kind of cells as it is stored in cells container. It is used when
    cells container have only one kind of cells */
  template <typename TInput, typename TOutput>
  void
  ReadCellsBuffer(TInput * input, TOutput * output)
  {
    if (input && output)
    {
      SizeValueType inputIndex = NumericTraits<SizeValueType>::ZeroValue();
      SizeValueType outputIndex = NumericTraits<SizeValueType>::ZeroValue();
      for (SizeValueType ii = 0; ii < m_NumberOfCells; ii++)
      {
        inputIndex++; // ignore the cell type
        auto numberOfPoints = static_cast<unsigned int>(input[inputIndex++]);
        for (unsigned int jj = 0; jj < numberOfPoints; jj++)
        {
          output[outputIndex++] = static_cast<TOutput>(input[inputIndex++]);
        }
      }
    }
  }

  /** Read cells from input buffer, used when Writting cells. This function only
    write specified type of cells(used when input cells container composes
    multiple type of cells and only want to write a specified cell type */
  template <typename TInput, typename TOutput>
  void
  ReadCellsBuffer(TInput * input, TOutput * output, CellGeometryEnum type)
  {
    if (input && output)
    {
      SizeValueType inputIndex = itk::NumericTraits<SizeValueType>::ZeroValue();
      SizeValueType outputIndex = itk::NumericTraits<SizeValueType>::ZeroValue();

      for (SizeValueType ii = 0; ii < m_NumberOfCells; ii++)
      {
        auto cellType = static_cast<CellGeometryEnum>(input[inputIndex++]);
        auto nn = static_cast<unsigned int>(input[inputIndex++]);
        if (cellType == type)
        {
          output[outputIndex++] = nn;
          for (unsigned int jj = 0; jj < nn; jj++)
          {
            output[outputIndex++] = static_cast<TOutput>(input[inputIndex++]);
          }
        }
        else
        {
          inputIndex += nn;
        }
      }
    }
  }

  /** Write cells to a data buffer, used when readding mesh, used for cellType
    with constant number of points */
  template <typename TInput, typename TOutput>
  void
  WriteCellsBuffer(TInput *         input,
                   TOutput *        output,
                   CellGeometryEnum cellType,
                   unsigned int     numberOfPoints,
                   SizeValueType    numberOfCells)
  {
    if (input && output)
    {
      SizeValueType inputIndex = NumericTraits<SizeValueType>::ZeroValue();
      SizeValueType outputIndex = NumericTraits<SizeValueType>::ZeroValue();
      for (SizeValueType ii = 0; ii < numberOfCells; ii++)
      {
        output[outputIndex++] = static_cast<TOutput>(cellType);
        output[outputIndex++] = static_cast<TOutput>(numberOfPoints);
        for (unsigned int jj = 0; jj < numberOfPoints; jj++)
        {
          output[outputIndex++] = static_cast<TOutput>(input[inputIndex++]);
        }
      }
    }
  }

  /** Write cells to a data buffer, used when readding mesh, used for cellType
    with non-constant number of points */
  template <typename TInput, typename TOutput>
  void
  WriteCellsBuffer(TInput * input, TOutput * output, CellGeometryEnum cellType, SizeValueType numberOfCells)
  {
    if (input && output)
    {
      SizeValueType inputIndex = NumericTraits<SizeValueType>::ZeroValue();
      SizeValueType outputIndex = NumericTraits<SizeValueType>::ZeroValue();
      for (SizeValueType ii = 0; ii < numberOfCells; ii++)
      {
        auto numberOfPoints = static_cast<unsigned int>(input[inputIndex++]);
        output[outputIndex++] = static_cast<TOutput>(cellType);
        output[outputIndex++] = static_cast<TOutput>(numberOfPoints);
        for (unsigned int jj = 0; jj < numberOfPoints; jj++)
        {
          output[outputIndex++] = static_cast<TOutput>(input[inputIndex++]);
        }
      }
    }
  }

protected:
  /** Big or Little Endian, and the type of the file. (May be ignored.) */
  IOByteOrderEnum m_ByteOrder{ IOByteOrderEnum::OrderNotApplicable };
  IOFileEnum      m_FileType{ IOFileEnum::ASCII };

  /** Filename to read */
  std::string m_FileName;

  /** Should we compress the data? */
  bool m_UseCompression{ false };

  /** Used internally to keep track of the type of the component. */
  IOComponentEnum m_PointComponentType{ IOComponentEnum::UNKNOWNCOMPONENTTYPE };
  IOComponentEnum m_CellComponentType{ IOComponentEnum::UNKNOWNCOMPONENTTYPE };
  IOComponentEnum m_PointPixelComponentType{ IOComponentEnum::UNKNOWNCOMPONENTTYPE };
  IOComponentEnum m_CellPixelComponentType{ IOComponentEnum::UNKNOWNCOMPONENTTYPE };

  /** Used internally to keep track of the type of the pixel. */
  IOPixelEnum m_PointPixelType{ IOPixelEnum::SCALAR };
  IOPixelEnum m_CellPixelType{ IOPixelEnum::SCALAR };

  /** Stores the number of components per pixel. This will be 1 for
   * grayscale images, 3 for RGBPixel images, and 4 for RGBPixelA images. */
  unsigned int m_NumberOfPointPixelComponents{ 0 };
  unsigned int m_NumberOfCellPixelComponents{ 0 };

  /** The number of independent dimensions in the point. */
  unsigned int m_PointDimension{ 3 };

  /** The number of points and cells */
  SizeValueType m_NumberOfPoints;
  SizeValueType m_NumberOfCells;
  SizeValueType m_NumberOfPointPixels;
  SizeValueType m_NumberOfCellPixels;

  /** The buffer size of cells */
  SizeValueType m_CellBufferSize;

  /** Flags indicate whether read or write points, cells, point data and cell
    data */
  bool m_UpdatePoints{ false };
  bool m_UpdateCells{ false };
  bool m_UpdatePointData{ false };
  bool m_UpdateCellData{ false };

private:
  ArrayOfExtensionsType m_SupportedReadExtensions;
  ArrayOfExtensionsType m_SupportedWriteExtensions;
};
#define MESHIOBASE_TYPEMAP(type, ctype)                                                                                \
  template <>                                                                                                          \
  struct MeshIOBase::MapComponentType<type>                                                                            \
  {                                                                                                                    \
    static constexpr IOComponentEnum CType = ctype;                                                                    \
  }

MESHIOBASE_TYPEMAP(unsigned char, IOComponentEnum::UCHAR);
MESHIOBASE_TYPEMAP(char, IOComponentEnum::CHAR);
MESHIOBASE_TYPEMAP(unsigned short, IOComponentEnum::USHORT);
MESHIOBASE_TYPEMAP(short, IOComponentEnum::SHORT);
MESHIOBASE_TYPEMAP(unsigned int, IOComponentEnum::UINT);
MESHIOBASE_TYPEMAP(int, IOComponentEnum::INT);
MESHIOBASE_TYPEMAP(unsigned long, IOComponentEnum::ULONG);
MESHIOBASE_TYPEMAP(long, IOComponentEnum::LONG);
MESHIOBASE_TYPEMAP(unsigned long long, IOComponentEnum::ULONGLONG);
MESHIOBASE_TYPEMAP(long long, IOComponentEnum::LONGLONG);
MESHIOBASE_TYPEMAP(float, IOComponentEnum::FLOAT);
MESHIOBASE_TYPEMAP(double, IOComponentEnum::DOUBLE);
MESHIOBASE_TYPEMAP(long double, IOComponentEnum::LDOUBLE);
#undef MESHIOBASE_TYPEMAP
} // end namespace itk

#endif
