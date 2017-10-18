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
#ifndef itkMeshIOBase_h
#define itkMeshIOBase_h
#include "ITKIOMeshExport.h"

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

#include <string>
#include <complex>
#include <fstream>

namespace itk
{
/** \class MeshIOBase
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
 * \ingroup ITKIOMesh
 *
 */

class ITKIOMesh_EXPORT MeshIOBase:public LightProcessObject
{
public:
  /** Standard class typedefs. */
  typedef MeshIOBase                 Self;
  typedef LightProcessObject         Superclass;
  typedef SmartPointer< const Self > ConstPointer;
  typedef SmartPointer< Self >       Pointer;

  /** Type for the list of strings to be used for extensions.  */
  typedef  std::vector< std::string > ArrayOfExtensionsType;

  /** Type for representing size of bytes, and or positions along a file */
  typedef std::streamoff StreamOffsetType;

  typedef IdentifierType SizeValueType;

  /**
    * \class UnknownType
    * Used to return information when types are unknown.
    * \ingroup ITKIOMesh
    */
  class UnknownType {};

  /** Run-time type information (and related methods). */
  itkTypeMacro(MeshIOBase, LightProcessObject);

  /** Set/Get the name of the file to be read. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);

  /** Enums used to manipulate the point/cell pixel type. The pixel type provides
     * context for automatic data conversions (for instance, RGB to
     * SCALAR, VECTOR to SCALAR). */
  typedef  enum {UNKNOWNPIXELTYPE, SCALAR, RGB, RGBA, OFFSET, VECTOR,
                 POINT, COVARIANTVECTOR, SYMMETRICSECONDRANKTENSOR,
                 DIFFUSIONTENSOR3D, COMPLEX, FIXEDARRAY, ARRAY, MATRIX,
                 VARIABLELENGTHVECTOR, VARIABLESIZEMATRIX}  IOPixelType;

  /** Enums used to manipulate the component type. The component type
   * refers to the actual storage class associated with either a
   * SCALAR pixel type or elements of a compound pixel. */
  typedef  enum {UNKNOWNCOMPONENTTYPE, UCHAR, CHAR, USHORT, SHORT, UINT, INT,
                 ULONG, LONG, LONGLONG, ULONGLONG, FLOAT, DOUBLE, LDOUBLE} IOComponentType;

  /** Enums used to specify write style: whether binary or ASCII. Some
    * subclasses use this, some ignore it. */
  typedef  enum {ASCII, BINARY, TYPENOTAPPLICABLE} FileType;

  /** Enums used to specify byte order; whether Big Endian or Little Endian.
  * Some subclasses use this, some ignore it. */
  typedef  enum {BigEndian, LittleEndian, OrderNotApplicable} ByteOrder;

  /** Enums used to specify cell type */
  typedef  enum {VERTEX_CELL = 0, LINE_CELL, TRIANGLE_CELL,
                 QUADRILATERAL_CELL, POLYGON_CELL, TETRAHEDRON_CELL, HEXAHEDRON_CELL,
                 QUADRATIC_EDGE_CELL, QUADRATIC_TRIANGLE_CELL,
                 LAST_ITK_CELL, MAX_ITK_CELLS = 255}  CellGeometryType;

  /** Set/Get the type of the point/cell pixel. The PixelTypes provides context
    * to the IO mechanisms for data conversions.  PixelTypes can be
    * SCALAR, RGB, RGBA, VECTOR, COVARIANTVECTOR, POINT, INDEX. If
    * the PIXELTYPE is SCALAR, then the NumberOfComponents should be 1.
    * Any other of PIXELTYPE will have more than one component. */
  itkSetEnumMacro(PointPixelType, IOPixelType);
  itkGetEnumMacro(PointPixelType, IOPixelType);
  itkSetEnumMacro(CellPixelType, IOPixelType);
  itkGetEnumMacro(CellPixelType, IOPixelType);

  /** Set/Get the component type of the point, cell, point data and cell data.
    This is always a native type. */
  itkSetEnumMacro(PointComponentType, IOComponentType);
  itkGetEnumMacro(PointComponentType, IOComponentType);
  itkSetEnumMacro(CellComponentType, IOComponentType);
  itkGetEnumMacro(CellComponentType, IOComponentType);
  itkSetEnumMacro(PointPixelComponentType, IOComponentType);
  itkGetEnumMacro(PointPixelComponentType, IOComponentType);
  itkSetEnumMacro(CellPixelComponentType, IOComponentType);
  itkGetEnumMacro(CellPixelComponentType, IOComponentType);

  template< typename T >
  struct MapComponentType {
    static ITK_CONSTEXPR_VAR IOComponentType CType = UNKNOWNCOMPONENTTYPE;
  };

  template< typename T >
  void SetPixelType(const T & itkNotUsed(dummy), bool UsePointPixel = true)
  {
    if ( UsePointPixel )
      {
      SetNumberOfPointPixelComponents(1);
      SetPointPixelComponentType(MapComponentType< T >::CType);
      SetPointPixelType(SCALAR);
      }
    else
      {
      SetNumberOfCellPixelComponents(1);
      SetCellPixelComponentType(MapComponentType< T >::CType);
      SetCellPixelType(SCALAR);
      }
  }

  template< typename T >
  void SetPixelType(const RGBPixel< T > & itkNotUsed(dummy), bool UsePointPixel = true)
  {
    if ( UsePointPixel )
      {
      SetNumberOfPointPixelComponents(3);
      SetPointPixelComponentType(MapComponentType< T >::CType);
      SetPointPixelType(RGB);
      }
    else
      {
      SetNumberOfCellPixelComponents(3);
      SetCellPixelComponentType(MapComponentType< T >::CType);
      SetCellPixelType(RGB);
      }
  }

  template< typename T >
  void SetPixelType(const RGBAPixel< T > & itkNotUsed(dummy), bool UsePointPixel = true)
  {
    if ( UsePointPixel )
      {
      SetNumberOfPointPixelComponents(4);
      SetPointPixelComponentType(MapComponentType< T >::CType);
      SetPointPixelType(RGBA);
      }
    else
      {
      SetNumberOfCellPixelComponents(4);
      SetCellPixelComponentType(MapComponentType< T >::CType);
      SetCellPixelType(RGBA);
      }
  }

  template< typename T, unsigned int VLength >
  void SetPixelType(const Vector< T, VLength > & itkNotUsed(dummy), bool UsePointPixel = true)
  {
    if ( UsePointPixel )
      {
      SetNumberOfPointPixelComponents(VLength);
      SetPointPixelComponentType(MapComponentType< T >::CType);
      SetPointPixelType(VECTOR);
      }
    else
      {
      SetNumberOfCellPixelComponents(VLength);
      SetCellPixelComponentType(MapComponentType< T >::CType);
      SetCellPixelType(VECTOR);
      }
  }

  template< typename T, unsigned int VLength >
  void SetPixelType(const CovariantVector< T, VLength > & itkNotUsed(dummy), bool UsePointPixel = true)
  {
    if ( UsePointPixel )
      {
      SetNumberOfPointPixelComponents(VLength);
      SetPointPixelComponentType(MapComponentType< T >::CType);
      SetPointPixelType(COVARIANTVECTOR);
      }
    else
      {
      SetNumberOfCellPixelComponents(VLength);
      SetCellPixelComponentType(MapComponentType< T >::CType);
      SetCellPixelType(COVARIANTVECTOR);
      }
  }

  template< typename T, unsigned int VLength >
  void SetPixelType(const FixedArray< T, VLength > & itkNotUsed(dummy), bool UsePointPixel = true)
  {
    if ( UsePointPixel )
      {
      SetNumberOfPointPixelComponents(VLength);
      SetPointPixelComponentType(MapComponentType< T >::CType);
      SetPointPixelType(FIXEDARRAY);
      }
    else
      {
      SetNumberOfCellPixelComponents(VLength);
      SetCellPixelComponentType(MapComponentType< T >::CType);
      SetCellPixelType(FIXEDARRAY);
      }
  }

  template< typename T, unsigned int VLength >
  void SetPixelType(const SymmetricSecondRankTensor< T, VLength > itkNotUsed(dummy), bool UsePointPixel = true)
  {
    if ( UsePointPixel )
      {
      SetNumberOfPointPixelComponents(VLength * ( VLength + 1 ) / 2);
      SetPointPixelComponentType(MapComponentType< T >::CType);
      SetPointPixelType(SYMMETRICSECONDRANKTENSOR);
      }
    else
      {
      SetNumberOfCellPixelComponents(VLength * ( VLength + 1 ) / 2);
      SetCellPixelComponentType(MapComponentType< T >::CType);
      SetCellPixelType(SYMMETRICSECONDRANKTENSOR);
      }
  }

  template< typename T >
  void SetPixelType(const DiffusionTensor3D< T > & itkNotUsed(dummy), bool UsePointPixel = true)
  {
    if ( UsePointPixel )
      {
      SetNumberOfPointPixelComponents(6);
      SetPointPixelComponentType(MapComponentType< T >::CType);
      SetPointPixelType(DIFFUSIONTENSOR3D);
      }
    else
      {
      SetNumberOfCellPixelComponents(6);
      SetCellPixelComponentType(MapComponentType< T >::CType);
      SetCellPixelType(DIFFUSIONTENSOR3D);
      }
  }

  template< typename T, unsigned int NR, unsigned int NC >
  void SetPixelType(const Matrix< T, NR, NC > & itkNotUsed(dummy), bool UsePointPixel = true)
  {
    if ( UsePointPixel )
      {
      SetNumberOfPointPixelComponents(NR * NC);
      SetPointPixelComponentType(MapComponentType< T >::CType);
      SetPointPixelType(MATRIX);
      }
    else
      {
      SetNumberOfCellPixelComponents(NR * NC);
      SetCellPixelComponentType(MapComponentType< T >::CType);
      SetCellPixelType(MATRIX);
      }
  }

  template< typename T >
  void SetPixelType(const std::complex< T > & itkNotUsed(dummy), bool UsePointPixel = true)
  {
    if ( UsePointPixel )
      {
      SetNumberOfPointPixelComponents(2);
      SetPointPixelComponentType(MapComponentType< T >::CType);
      SetPointPixelType(COMPLEX);
      }
    else
      {
      SetNumberOfCellPixelComponents(2);
      SetCellPixelComponentType(MapComponentType< T >::CType);
      SetCellPixelType(COMPLEX);
      }
  }

  template< typename T >
  void SetPixelType(const Array< T > & array, bool UsePointPixel = true)
  {
    if ( UsePointPixel )
      {
      SetNumberOfPointPixelComponents( array.Size() );
      SetPointPixelComponentType(MapComponentType< T >::CType);
      SetPointPixelType(ARRAY);
      }
    else
      {
      SetNumberOfCellPixelComponents( array.Size() );
      SetCellPixelComponentType(MapComponentType< T >::CType);
      SetCellPixelType(ARRAY);
      }
  }

  template< typename T >
  void SetPixelType(const VariableLengthVector< T > & vector, bool UsePointPixel = true)
  {
    if ( UsePointPixel )
      {
      SetNumberOfPointPixelComponents( vector.Size() );
      SetPointPixelComponentType(MapComponentType< T >::CType);
      SetPointPixelType(VARIABLELENGTHVECTOR);
      }
    else
      {
      SetNumberOfCellPixelComponents( vector.Size() );
      SetCellPixelComponentType(MapComponentType< T >::CType);
      SetCellPixelType(VARIABLELENGTHVECTOR);
      }
  }

  template< typename T >
  void SetPixelType(const VariableSizeMatrix< T > & matrix, bool UsePointPixel = true)
  {
    if ( UsePointPixel )
      {
      SetNumberOfPointPixelComponents( matrix.Rows() * matrix.Cols() );
      SetPointPixelComponentType(MapComponentType< T >::CType);
      SetPointPixelType(VARIABLESIZEMATRIX);
      }
    else
      {
      SetNumberOfCellPixelComponents( matrix.Rows() * matrix.Cols() );
      SetCellPixelComponentType(MapComponentType< T >::CType);
      SetCellPixelType(VARIABLESIZEMATRIX);
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

  unsigned int GetComponentSize(IOComponentType componentType) const;

  /** Convenience method returns the IOComponentType as a string. This can be
     * used for writing output files. */
  std::string GetComponentTypeAsString(IOComponentType) const;

  /** Convenience method returns the IOPixelType as a string. This can be
   * used for writing output files. */
  std::string GetPixelTypeAsString(IOPixelType) const;

  /** These methods control whether the file is written binary or ASCII.
  * Many file formats (i.e., subclasses) ignore this flag. */
  itkSetEnumMacro(FileType, FileType);
  itkGetEnumMacro(FileType, FileType);

  void SetFileTypeToASCII()
  {
    this->SetFileType(ASCII);
  }

  void SetFileTypeToBinary()
  {
    this->SetFileType(BINARY);
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
  itkSetEnumMacro(ByteOrder, ByteOrder);
  itkGetEnumMacro(ByteOrder, ByteOrder);

  void SetByteOrderToBigEndian()
  {
    this->SetByteOrder(BigEndian);
  }

  void SetByteOrderToLittleEndian()
  {
    this->SetByteOrder(LittleEndian);
  }

  /** Set/Get a boolean to use the compression or not. */
  itkSetMacro(UseCompression, bool);
  itkGetConstMacro(UseCompression, bool);
  itkBooleanMacro(UseCompression);

  /** Convenience method returns the FileType as a string. This can be
     * used for writing output files. */
  std::string GetFileTypeAsString(FileType) const;

  /** Convenience method returns the ByteOrder as a string. This can be
   * used for writing output files. */
  std::string GetByteOrderAsString(ByteOrder) const;

  /*-------- This part of the interfaces deals with reading data ----- */
  /** Determine the file type. Returns true if this MeshIO can read the
     * file specified. */
  virtual bool CanReadFile(const char *) = 0;

  /** Determin the required information and whether need to ReadPoints,
    ReadCells, ReadPointData and ReadCellData */
  virtual void ReadMeshInformation() = 0;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void ReadPoints(void *buffer) = 0;

  virtual void ReadCells(void *buffer) = 0;

  virtual void ReadPointData(void *buffer) = 0;

  virtual void ReadCellData(void *buffer) = 0;

  /*-------- This part of the interfaces deals with writing data ----- */

  /** Writes the data to disk from the memory buffer provided. Make sure
     * that the IORegions has been set properly. */
  virtual bool CanWriteFile(const char *)  = 0;

  virtual void WriteMeshInformation() = 0;

  virtual void WritePoints(void *buffer) = 0;

  virtual void WriteCells(void *buffer) = 0;

  virtual void WritePointData(void *buffer) = 0;

  virtual void WriteCellData(void *buffer) = 0;

  virtual void Write() = 0;

  /** This method returns an array with the list of filename extensions
   * supported for reading by this MeshIO class. This is intended to
   * facilitate GUI and application level integration.
   */
  const ArrayOfExtensionsType & GetSupportedReadExtensions() const;

  /** This method returns an array with the list of filename extensions
   * supported for writing by this MeshIO class. This is intended to
   * facilitate GUI and application level integration.
   */
  const ArrayOfExtensionsType & GetSupportedWriteExtensions() const;

protected:
  MeshIOBase();
  virtual ~MeshIOBase() ITK_OVERRIDE {}

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Insert an extension to the list of supported extensions for reading. */
  void AddSupportedReadExtension(const char *extension);

  /** Insert an extension to the list of supported extensions for writing. */
  void AddSupportedWriteExtension(const char *extension);

  /** Read data from input file stream to buffer with ascii style */
  template< typename T >
  void ReadBufferAsAscii(T *buffer, std::ifstream & inputFile, SizeValueType numberOfComponents)
  {
    for ( SizeValueType i = 0; i < numberOfComponents; i++ )
      {
      inputFile >> buffer[i];
      }
  }

  /** Read data from input file to buffer with binary style */
  template< typename T >
  void ReadBufferAsBinary(T *buffer, std::ifstream & inputFile, SizeValueType numberOfComponents)
  {
    inputFile.read( reinterpret_cast< char * >( buffer ), numberOfComponents * sizeof( T ) );

    if ( m_ByteOrder == BigEndian )
      {
      if ( itk::ByteSwapper< T >::SystemIsLittleEndian() )
        {
        itk::ByteSwapper< T >::SwapRangeFromSystemToBigEndian(buffer, numberOfComponents);
        }
      }
    else if ( m_ByteOrder == LittleEndian )
      {
      if ( itk::ByteSwapper< T >::SystemIsBigEndian() )
        {
        itk::ByteSwapper< T >::SwapRangeFromSystemToLittleEndian(buffer, numberOfComponents);
        }
      }
  }

  /** Write buffer to output file stream with ascii style */
  template< typename T >
  void WriteBufferAsAscii(T *buffer, std::ofstream & outputFile, SizeValueType numberOfLines, SizeValueType numberOfComponents)
  {
    NumberToString<T> convert;
    for ( SizeValueType ii = 0; ii < numberOfLines; ii++ )
      {
      for ( SizeValueType jj = 0; jj < numberOfComponents; jj++ )
        {
        outputFile << convert(buffer[ii * numberOfComponents + jj]) << "  ";
        }
      outputFile << '\n';
      }
  }

  /** Write buffer to output file stream with binary style */
  template< typename TOutput, typename TInput >
  void WriteBufferAsBinary(TInput *buffer, std::ofstream & outputFile, SizeValueType numberOfComponents)
  {
    if ( typeid( TInput ) == typeid( TOutput ) )
      {
      if ( m_ByteOrder == BigEndian && itk::ByteSwapper< TInput >::SystemIsLittleEndian() )
        {
        itk::ByteSwapper< TInput >::SwapRangeFromSystemToBigEndian(buffer, numberOfComponents);
        }
      else if ( m_ByteOrder == LittleEndian && itk::ByteSwapper< TInput >::SystemIsBigEndian() )
        {
        itk::ByteSwapper< TInput >::SwapRangeFromSystemToLittleEndian(buffer, numberOfComponents);
        }

      outputFile.write(reinterpret_cast< char * >( buffer ), numberOfComponents);
      }
    else
      {
      TOutput *data = new TOutput[numberOfComponents];
      for ( SizeValueType ii = 0; ii < numberOfComponents; ii++ )
        {
        data[ii] = static_cast< TOutput >( buffer[ii] );
        }

      if ( m_ByteOrder == BigEndian && itk::ByteSwapper< TOutput >::SystemIsLittleEndian() )
        {
        itk::ByteSwapper< TOutput >::SwapRangeFromSystemToBigEndian(data, numberOfComponents);
        }
      else if ( m_ByteOrder == LittleEndian && itk::ByteSwapper< TOutput >::SystemIsBigEndian() )
        {
        itk::ByteSwapper< TOutput >::SwapRangeFromSystemToLittleEndian(data, numberOfComponents);
        }

      outputFile.write(reinterpret_cast< char * >( data ), numberOfComponents);
      delete[] data;
      }
  }

  /** Read cells from a data buffer, used when writting cells. This function
    write all kind of cells as it is stored in cells container. It is used when
    cells container have only one kind of cells */
  template< typename TInput, typename TOutput >
  void ReadCellsBuffer(TInput *input, TOutput *output)
  {
    if ( input && output )
      {
      SizeValueType inputIndex = NumericTraits< SizeValueType >::ZeroValue();
      SizeValueType outputIndex = NumericTraits< SizeValueType >::ZeroValue();
      for ( SizeValueType ii = 0; ii < m_NumberOfCells; ii++ )
        {
        inputIndex++; // ignore the cell type
        unsigned int numberOfPoints = static_cast< unsigned int >( input[inputIndex++] );
        for ( unsigned int jj = 0; jj < numberOfPoints; jj++ )
          {
          output[outputIndex++] = static_cast< TOutput >( input[inputIndex++] );
          }
        }
      }
  }

  /** Read cells from input buffer, used when Writting cells. This function only
    write specified type of cells(used when input cells container composes
    multiple type of cells and only want to write a specified cell type */
  template< typename TInput, typename TOutput >
  void ReadCellsBuffer(TInput *input, TOutput *output, MeshIOBase::CellGeometryType type)
  {
    if ( input && output )
      {
      SizeValueType inputIndex = itk::NumericTraits< SizeValueType >::ZeroValue();
      SizeValueType outputIndex = itk::NumericTraits< SizeValueType >::ZeroValue();

      for ( SizeValueType ii = 0; ii < m_NumberOfCells; ii++ )
        {
        MeshIOBase::CellGeometryType cellType = static_cast< MeshIOBase::CellGeometryType >( input[inputIndex++] );
        unsigned int                 nn = static_cast< unsigned int >( input[inputIndex++] );
        if ( cellType == type )
          {
          output[outputIndex++] = nn;
          for ( unsigned int jj = 0; jj < nn; jj++ )
            {
            output[outputIndex++] = static_cast< TOutput >( input[inputIndex++] );
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
  template< typename TInput, typename TOutput >
  void WriteCellsBuffer(TInput *input, TOutput *output, CellGeometryType cellType, unsigned int numberOfPoints, SizeValueType numberOfCells)
  {
    if ( input && output )
      {
      SizeValueType inputIndex = NumericTraits< SizeValueType >::ZeroValue();
      SizeValueType outputIndex = NumericTraits< SizeValueType >::ZeroValue();
      for ( SizeValueType ii = 0; ii < numberOfCells; ii++ )
        {
        output[outputIndex++] = static_cast< TOutput >( cellType );
        output[outputIndex++] = static_cast< TOutput >( numberOfPoints );
        for ( unsigned int jj = 0; jj < numberOfPoints; jj++ )
          {
          output[outputIndex++] = static_cast< TOutput >( input[inputIndex++] );
          }
        }
      }
  }

  /** Write cells to a data buffer, used when readding mesh, used for cellType
    with non-constant number of points */
  template< typename TInput, typename TOutput >
  void WriteCellsBuffer(TInput *input, TOutput *output, CellGeometryType cellType, SizeValueType numberOfCells)
  {
    if ( input && output )
      {
      SizeValueType inputIndex = NumericTraits< SizeValueType >::ZeroValue();
      SizeValueType outputIndex = NumericTraits< SizeValueType >::ZeroValue();
      for ( SizeValueType ii = 0; ii < numberOfCells; ii++ )
        {
        unsigned int numberOfPoints = static_cast< unsigned int >( input[inputIndex++] );
        output[outputIndex++] = static_cast< TOutput >( cellType );
        output[outputIndex++] = static_cast< TOutput >( numberOfPoints );
        for ( unsigned int jj = 0; jj < numberOfPoints; jj++ )
          {
          output[outputIndex++] = static_cast< TOutput >( input[inputIndex++] );
          }
        }
      }
  }

protected:
  /** Big or Little Endian, and the type of the file. (May be ignored.) */
  ByteOrder m_ByteOrder;
  FileType  m_FileType;

  /** Filename to read */
  std::string m_FileName;

  /** Should we compress the data? */
  bool m_UseCompression;

  /** Used internally to keep track of the type of the component. */
  IOComponentType m_PointComponentType;
  IOComponentType m_CellComponentType;
  IOComponentType m_PointPixelComponentType;
  IOComponentType m_CellPixelComponentType;

  /** Used internally to keep track of the type of the pixel. */
  IOPixelType m_PointPixelType;
  IOPixelType m_CellPixelType;

  /** Stores the number of components per pixel. This will be 1 for
    * grayscale images, 3 for RGBPixel images, and 4 for RGBPixelA images. */
  unsigned int m_NumberOfPointPixelComponents;
  unsigned int m_NumberOfCellPixelComponents;

  /** The number of independent dimensions in the point. */
  SizeValueType m_PointDimension;

  /** The number of points and cells */
  SizeValueType m_NumberOfPoints;
  SizeValueType m_NumberOfCells;
  SizeValueType m_NumberOfPointPixels;
  SizeValueType m_NumberOfCellPixels;

  /** The buffer size of cells */
  SizeValueType m_CellBufferSize;

  /** Flags indicate whether read or write points, cells, point data and cell
    data */
  bool m_UpdatePoints;
  bool m_UpdateCells;
  bool m_UpdatePointData;
  bool m_UpdateCellData;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MeshIOBase);

  ArrayOfExtensionsType m_SupportedReadExtensions;
  ArrayOfExtensionsType m_SupportedWriteExtensions;
};
#define MESHIOBASE_TYPEMAP(type, ctype)            \
  template< >                                      \
  struct MeshIOBase:: MapComponentType< type >     \
  {                                                \
    static ITK_CONSTEXPR_VAR IOComponentType CType = ctype;    \
  }

MESHIOBASE_TYPEMAP(unsigned char, UCHAR);
MESHIOBASE_TYPEMAP(char, CHAR);
MESHIOBASE_TYPEMAP(unsigned short, USHORT);
MESHIOBASE_TYPEMAP(short, SHORT);
MESHIOBASE_TYPEMAP(unsigned int, UINT);
MESHIOBASE_TYPEMAP(int, INT);
MESHIOBASE_TYPEMAP(unsigned long, ULONG);
MESHIOBASE_TYPEMAP(long, LONG);
MESHIOBASE_TYPEMAP(unsigned long long, ULONGLONG);
MESHIOBASE_TYPEMAP(long long, LONGLONG);
MESHIOBASE_TYPEMAP(float, FLOAT);
MESHIOBASE_TYPEMAP(double, DOUBLE);
MESHIOBASE_TYPEMAP(long double, LDOUBLE);
#undef MESHIOBASE_TYPEMAP
} // end namespace itk

#endif
