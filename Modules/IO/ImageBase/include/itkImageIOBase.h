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
#ifndef itkImageIOBase_h
#define itkImageIOBase_h
#include "ITKIOImageBaseExport.h"

#include "itkIOConfigure.h"

#include "itkLightProcessObject.h"
#include "itkIndent.h"
#include "itkImageIORegion.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"
#include "itkCovariantVector.h"
#include "itkSymmetricSecondRankTensor.h"
#include "itkDiffusionTensor3D.h"
#include "itkArray.h"
#include "itkVariableSizeMatrix.h"
#include "itkImageRegionSplitterBase.h"
#include "itkCommonEnums.h"

#include "vnl/vnl_vector.h"
#include "vcl_compiler.h"

#include <fstream>
#include <string>

namespace itk
{
// Forward reference for VariableLengthVector
template <typename TValue>
class VariableLengthVector;

/** \class ImageIOBase
 * \brief Abstract superclass defines image IO interface.
 *
 * ImageIOBase is a class that reads and/or writes image data
 * of a particular format (such as PNG or raw binary). The
 * ImageIOBase encapsulates both the reading and writing of data. The
 * ImageIOBase is used by the ImageFileReader class (to read data)
 * and the ImageFileWriter (to write data) into a single file. The
 * ImageSeriesReader and ImageSeriesWriter classes are used to read
 * and write data (in conjunction with ImageIOBase) when the data is
 * represented by a series of files. Normally the user does not directly
 * manipulate this class other than to instantiate it, set the FileName,
 * and assign it to a ImageFileReader/ImageFileWriter or
 * ImageSeriesReader/ImageSeriesWriter.
 *
 * A Pluggable factory pattern is used this allows different kinds of
 * readers to be registered (even at run time) without having to
 * modify the code in this class.
 *
 * \sa ImageFileWriter
 * \sa ImageFileReader
 * \sa ImageSeriesWriter
 * \sa ImageSeriesReader
 *
 * \ingroup IOFilters
 *
 * \ingroup ITKIOImageBase
 */
class ITKIOImageBase_EXPORT ImageIOBase : public LightProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImageIOBase);

  /** Standard class type aliases. */
  using Self = ImageIOBase;
  using Superclass = LightProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageIOBase, Superclass);

  /** Set/Get the name of the file to be read. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);

  /** Types for managing image size and image index components. */
  using IndexValueType = ::itk::IndexValueType;
  using SizeValueType = ::itk::SizeValueType;

  /**
   * \class UnknownType
   * Used to return information when types are unknown.
   * \ingroup ITKIOImageBase
   */
  class UnknownType
  {};

#if !defined(ITK_LEGACY_REMOVE)
  /**Exposes enums values for backwards compatibility*/
  using IOPixelType = IOPixelEnum;
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
  static constexpr IOPixelEnum MATRIX = IOPixelEnum::MATRIX;
#endif

  using IOComponentEnum = itk::IOComponentEnum;
#if !defined(ITK_LEGACY_REMOVE)
  /**Exposes enums values for backwards compatibility*/
  using IOComponentType = IOComponentEnum;
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
#endif

  using IOFileEnum = itk::IOFileEnum;
#if !defined(ITK_LEGACY_REMOVE)
  /**Exposes enums values for backwards compatibility*/
  static constexpr IOFileEnum ASCII = IOFileEnum::ASCII;
  static constexpr IOFileEnum Binary = IOFileEnum::Binary;
  static constexpr IOFileEnum TypeNotApplicable = IOFileEnum::TypeNotApplicable;
#endif

  using IOByteOrderEnum = itk::IOByteOrderEnum;
#if !defined(ITK_LEGACY_REMOVE)
  using ByteOrder = itk::IOByteOrderEnum;
  /**Exposes enums values for backwards compatibility*/
  static constexpr IOByteOrderEnum BigEndian = IOByteOrderEnum::BigEndian;
  static constexpr IOByteOrderEnum LittleEndian = IOByteOrderEnum::LittleEndian;
  static constexpr IOByteOrderEnum OrderNotApplicable = IOByteOrderEnum::OrderNotApplicable;
#endif

  /** Set/Get the number of independent variables (dimensions) in the
   * image being read or written. Note this is not necessarily what
   * is written, rather the IORegion controls that. */
  void
  SetNumberOfDimensions(unsigned int);

  itkGetConstMacro(NumberOfDimensions, unsigned int);

  /** Set/Get the image dimensions in the x, y, z, etc. directions.
   * GetDimensions() is typically used after reading the data; the
   * SetDimensions() is used prior to writing the data. */
  virtual void
  SetDimensions(unsigned int i, SizeValueType dim);

  virtual SizeValueType
  GetDimensions(unsigned int i) const
  {
    return m_Dimensions[i];
  }

  /** Set/Get the image origin on a axis-by-axis basis. The SetOrigin() method
   * is required when writing the image. */
  virtual void
  SetOrigin(unsigned int i, double origin);

  virtual double
  GetOrigin(unsigned int i) const
  {
    return m_Origin[i];
  }

  /** Set/Get the image spacing on an axis-by-axis basis. The
   * SetSpacing() method is required when writing the image. */
  virtual void
  SetSpacing(unsigned int i, double spacing);

  virtual double
  GetSpacing(unsigned int i) const
  {
    return m_Spacing[i];
  }

  /** Set/Get the image direction on an axis-by-axis basis. The
   * SetDirection() method is required when writing the image. */
  virtual void
  SetDirection(unsigned int i, const std::vector<double> & direction);

  virtual void
  SetDirection(unsigned int i, const vnl_vector<double> & direction);

  virtual std::vector<double>
  GetDirection(unsigned int i) const
  {
    return m_Direction[i];
  }

  /** Return the directions to be assigned by default to recipient
   *  images whose dimension is smaller than the image dimension in file.  */
  virtual std::vector<double>
  GetDefaultDirection(unsigned int k) const;

  /** Specify the region of the image data to either read or
   * write. The IORegion specifies the part of the image to read or
   * write. Regions are defined with an index and a size vector. These
   * vectors define the start (lower-left corner) and length of the
   * region within the image. Make sure that the IORegion lies within
   * the image. */
  itkSetMacro(IORegion, ImageIORegion);
  itkGetConstReferenceMacro(IORegion, ImageIORegion);

  /** Set/Get the type of the pixel. The PixelTypes provides context
   * to the IO mechanisms for data conversions.  PixelTypes can be
   * SCALAR, RGB, RGBA, VECTOR, COVARIANTVECTOR, POINT, INDEX. If
   * the PIXELTYPE is SCALAR, then the NumberOfComponents should be 1.
   * Any other of PIXELTYPE will have more than one component. */
  itkSetEnumMacro(PixelType, ::itk::CommonEnums::IOPixel);
  itkGetEnumMacro(PixelType, ::itk::CommonEnums::IOPixel);

  /** Set/Get the component type of the image. This is always a native
   * type. */
  itkSetEnumMacro(ComponentType, IOComponentEnum);
  itkGetEnumMacro(ComponentType, IOComponentEnum);
  /** get the type_info for the current pixel component type.
   * This function is DEPRECATED and only provided for backwards
   * compatibility.  There is no use for this method that can't
   * be satisfied by calling GetComponentType.
   */
  virtual const std::type_info &
  GetComponentTypeInfo() const;

  /** Set/Get the number of components per pixel in the image. This may
   * be set by the reading process. For SCALAR pixel types,
   * NumberOfComponents will be 1.  For other pixel types,
   * NumberOfComponents will be greater than or equal to one. */
  itkSetMacro(NumberOfComponents, unsigned int);
  itkGetConstReferenceMacro(NumberOfComponents, unsigned int);

  /** Set/Get a boolean to use the compression or not. */
  itkSetMacro(UseCompression, bool);
  itkGetConstMacro(UseCompression, bool);
  itkBooleanMacro(UseCompression);

  /** \brief Set/Get a compression level hint
   *
   * If compression is enabled by UseCompression, then the value
   * may be used for as the compression level dependent upon the
   * compressor.
   **/
  itkSetClampMacro(CompressionLevel, int, 1, this->GetMaximumCompressionLevel());
  itkGetConstMacro(CompressionLevel, int);

  /** \brief Set/Get the compression algorithm to use
   *
   * If compression is enabled by UseCompression, then the value is
   * used to select the compression algorithm. An empty string
   * represent the default compressor. If string identifier is not
   * recognized a warning is produced and the default is used.
   *
   * \note These compression hints may be ignored if the ImageIO does
   * not support compression or the compression is not enabled.
   **/
  virtual void
  SetCompressor(std::string _c);
  itkGetConstReferenceMacro(Compressor, std::string);

  /** Set/Get a boolean to use streaming while reading or not. */
  itkSetMacro(UseStreamedReading, bool);
  itkGetConstMacro(UseStreamedReading, bool);
  itkBooleanMacro(UseStreamedReading);

  /** Set/Get a boolean to use streaming while writing or not. */
  itkSetMacro(UseStreamedWriting, bool);
  itkGetConstMacro(UseStreamedWriting, bool);
  itkBooleanMacro(UseStreamedWriting);

  /** Set/Get a boolean to perform RGB palette expansion.
   * If true, palette image is read as RGB,
   * if false, palette image is read as Scalar+Palette.
   * A RGB image is always read as RGB.*/
  itkSetMacro(ExpandRGBPalette, bool);
  itkGetConstMacro(ExpandRGBPalette, bool);
  itkBooleanMacro(ExpandRGBPalette);

  /** Set/Get a boolean to include a color palette while writing
   * the image file. Applies only for scalar Pixels*/
  itkSetMacro(WritePalette, bool);
  itkGetConstMacro(WritePalette, bool);
  itkBooleanMacro(WritePalette);

  /** Determine whether a paletized image file has been read as a scalar image
   *  plus a color palette.
   *  ExpandRGBPalette must be set to true, and the file must be a
   *  palette image file supported for palette reading.*/
  itkGetConstMacro(IsReadAsScalarPlusPalette, bool);

  /** Convenience method returns the IOComponentEnum as a string. This can be
   * used for writing output files. */
  static std::string GetComponentTypeAsString(IOComponentEnum);

  /** Convenience method returns the IOComponentEnum corresponding to a string. */
  static IOComponentEnum
  GetComponentTypeFromString(const std::string & typeString);

  /** Convenience method returns the IOPixelEnum as a string. This can be
   * used for writing output files. */
  static std::string GetPixelTypeAsString(IOPixelEnum);

  /** Convenience method returns the IOPixelEnum corresponding to a string. */
  static IOPixelEnum
  GetPixelTypeFromString(const std::string & pixelString);

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
    this->SetFileType(IOFileEnum::Binary);
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
   * SetByteOrderToBigEndian().  Some ImageIOBase subclasses
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

  /** Convenience method returns the IOFileEnum as a string. This can be
   * used for writing output files. */
  std::string GetFileTypeAsString(IOFileEnum) const;

  /** Convenience method returns the IOByteOrderEnum as a string. This can be
   * used for writing output files. */
  std::string GetByteOrderAsString(IOByteOrderEnum) const;

  /** Type for representing size of bytes, and or positions along a file */
  using SizeType = ::itk::intmax_t;

  /** Type for representing size of bytes, and or positions along a memory
    buffer */
  using BufferSizeType = ::itk::OffsetValueType;

  /** Convenient method for accessing the number of bytes to get to
   * the next pixel. Returns m_Strides[1];
   *
   * Please note that this methods depends the private methods
   * ComputeStrides being called, otherwise this is the incorrect value.
   */
  virtual SizeType
  GetPixelStride() const;

  /** Return the number of pixels in the image. */
  SizeType
  GetImageSizeInPixels() const;

  /** Return the number of bytes in the image. */
  SizeType
  GetImageSizeInBytes() const;

  /** Return the number of pixels times the number
   * of components in the image. */
  SizeType
  GetImageSizeInComponents() const;

  /** Compute the size (in bytes) of the components of a pixel. For
   * example, and RGB pixel of unsigned char would have a
   * component size of 1 byte. This method can be invoked only after
   * the component type is set. */
  virtual unsigned int
  GetComponentSize() const;

  /*-------- This part of the interfaces deals with reading data ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool
  CanReadFile(const char *) = 0;

  /** Determine if the ImageIO can stream reading from the
      current settings. Default is false. If this is queried after
      the header of the file has been read then it will indicate if
      that file can be streamed */
  virtual bool
  CanStreamRead()
  {
    return false;
  }

  /** Read the spacing and dimensions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void
  ReadImageInformation() = 0;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void
  Read(void * buffer) = 0;

  /*-------- This part of the interfaces deals with writing data ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool
  CanWriteFile(const char *) = 0;

  /** Determine if the ImageIO can stream write from the
   *  current settings.
   *
   * There are two types of non exclusive streaming: pasting subregions, and iterative
   *
   */
  virtual bool
  CanStreamWrite()
  {
    return false;
  }

  /** Writes the spacing and dimensions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void
  WriteImageInformation() = 0;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. The buffer is cast to a
   * pointer to the beginning of the image data. */
  virtual void
  Write(const void * buffer) = 0;

  /* --- Support reading and writing data as a series of files. --- */

  /** The different types of ImageIO's can support data of varying
   * dimensionality. For example, some file formats are strictly 2D
   * while others can support 2D, 3D, or even n-D. This method returns
   * true/false as to whether the ImageIO can support the dimension
   * indicated. */
  virtual bool
  SupportsDimension(unsigned long dim)
  {
    return (dim == 2);
  }

  /** Method for supporting streaming.  Given a requested region, determine what
   * could be the region that we can read from the file. This is called the
   * streamable region, which will be equal or smaller than the
   * LargestPossibleRegion (unless it was dimensionaly clipped) and
   * greater or equal to the RequestedRegion
   *
   * the resulting IORegion may be a greater dimensions the the
   * requested IORegion, if the the derived class is unable to read
   * the requested region. For example if the file has a size of [ 10,
   * 10, 10] but the requested region is [10, 10] the return may be 3 dimensions.
   */
  virtual ImageIORegion
  GenerateStreamableReadRegionFromRequestedRegion(const ImageIORegion & requested) const;

  /** Before this method is called all the configuration will be done,
   * that is Streaming/PasteRegion/Compression/Filename etc
   * If pasting is being used the number of requested splits is for that
   * region not the largest. The derived ImageIO class should verify that
   * the file is capable of being written with this configuration.
   * If pasted is enabled and is not support or does not work with the file,
   * then an exception should be thrown.
   *
   * The default implementation depends on CanStreamWrite.
   * If false then 1 is returned (unless pasting is indicated), so that the whole file will be updated in one region.
   * If true then its assumed that any arbitrary region can be written
   * to any file. So the users request will be respected. If a derived
   * class has more restrictive conditions then they should be checked
   */
  virtual unsigned int
  GetActualNumberOfSplitsForWriting(unsigned int          numberOfRequestedSplits,
                                    const ImageIORegion & pasteRegion,
                                    const ImageIORegion & largestPossibleRegion);

  /** returns the ith IORegion
   *
   * numberOfActualSplits should be the value returned from GetActualNumberOfSplitsForWriting with the same parameters
   *
   * Derieved classes should overload this method to return a compatible region
   */
  virtual ImageIORegion
  GetSplitRegionForWriting(unsigned int          ithPiece,
                           unsigned int          numberOfActualSplits,
                           const ImageIORegion & pasteRegion,
                           const ImageIORegion & largestPossibleRegion);

  /** Type for the list of strings to be used for extensions.  */
  using ArrayOfExtensionsType = std::vector<std::string>;

  /** This method returns an array with the list of filename extensions
   * supported for reading by this ImageIO class. This is intended to
   * facilitate GUI and application level integration.
   */
  const ArrayOfExtensionsType &
  GetSupportedReadExtensions() const;

  /** This method returns an array with the list of filename extensions
   * supported for writing by this ImageIO class. This is intended to
   * facilitate GUI and application level integration.
   */
  const ArrayOfExtensionsType &
  GetSupportedWriteExtensions() const;

  template <typename TPixel>
  void
  SetTypeInfo(const TPixel *);

  /** Map between C++ Pixel type and ImageIOBase ComponentType */
  template <typename TPixel>
  struct MapPixelType
  {
    static constexpr IOComponentEnum CType = IOComponentEnum::UNKNOWNCOMPONENTTYPE;
  };
  template <typename TPixel>
  void
  SetPixelTypeInfo(const TPixel *)
  {
    this->SetNumberOfComponents(1);
    this->SetPixelType(IOPixelEnum::SCALAR);
    this->SetComponentType(MapPixelType<TPixel>::CType);
  }
  template <typename TPixel>
  void
  SetPixelTypeInfo(const RGBPixel<TPixel> *)
  {
    this->SetNumberOfComponents(3);
    this->SetPixelType(IOPixelEnum::RGB);
    this->SetComponentType(MapPixelType<TPixel>::CType);
  }
  template <typename TPixel>
  void
  SetPixelTypeInfo(const RGBAPixel<TPixel> *)
  {
    this->SetNumberOfComponents(4);
    this->SetPixelType(IOPixelEnum::RGBA);
    this->SetComponentType(MapPixelType<TPixel>::CType);
  }
  template <unsigned VLength>
  void
  SetPixelTypeInfo(const Offset<VLength> *)
  {
    this->SetNumberOfComponents(VLength);
    this->SetPixelType(IOPixelEnum::OFFSET);
    this->SetComponentType(IOComponentEnum::LONG);
  }
  template <typename TPixel, unsigned VLength>
  void
  SetPixelTypeInfo(const Vector<TPixel, VLength> *)
  {
    this->SetNumberOfComponents(VLength);
    this->SetPixelType(IOPixelEnum::VECTOR);
    this->SetComponentType(MapPixelType<TPixel>::CType);
  }
  template <typename TCoordRep, unsigned NPointDimension>
  void
  SetPixelTypeInfo(const Point<TCoordRep, NPointDimension> *)
  {
    this->SetNumberOfComponents(NPointDimension);
    this->SetPixelType(IOPixelEnum::POINT);
    this->SetComponentType(MapPixelType<TCoordRep>::CType);
  }
  template <typename TPixel, unsigned VLength>
  void
  SetPixelTypeInfo(const CovariantVector<TPixel, VLength> *)
  {
    this->SetNumberOfComponents(VLength);
    this->SetPixelType(IOPixelEnum::COVARIANTVECTOR);
    this->SetComponentType(MapPixelType<TPixel>::CType);
  }
  template <typename TPixel, unsigned VLength>
  void
  SetPixelTypeInfo(const SymmetricSecondRankTensor<TPixel, VLength> *)
  {
    this->SetNumberOfComponents(VLength * (VLength + 1) / 2);
    this->SetPixelType(IOPixelEnum::SYMMETRICSECONDRANKTENSOR);
    this->SetComponentType(MapPixelType<TPixel>::CType);
  }
  template <typename TPixel>
  void
  SetPixelTypeInfo(const DiffusionTensor3D<TPixel> *)
  {
    this->SetNumberOfComponents(6);
    this->SetPixelType(IOPixelEnum::DIFFUSIONTENSOR3D);
    this->SetComponentType(MapPixelType<TPixel>::CType);
  }
  template <typename TPixel>
  void
  SetPixelTypeInfo(const std::complex<TPixel> *)
  {
    this->SetNumberOfComponents(2);
    this->SetPixelType(IOPixelEnum::COMPLEX);
    this->SetComponentType(MapPixelType<TPixel>::CType);
  }
  template <typename TPixel, unsigned VLength>
  void
  SetPixelTypeInfo(const FixedArray<TPixel, VLength> *)
  {
    this->SetNumberOfComponents(VLength);
    this->SetPixelType(IOPixelEnum::FIXEDARRAY);
    this->SetComponentType(MapPixelType<TPixel>::CType);
  }
  template <typename TPixel>
  void
  SetPixelTypeInfo(const VariableLengthVector<TPixel> *)
  {
    this->SetNumberOfComponents(1);
    this->SetPixelType(IOPixelEnum::VARIABLELENGTHVECTOR);
    this->SetComponentType(MapPixelType<TPixel>::CType);
  }
  template <typename TValue>
  void
  SetPixelTypeInfo(const Array<TValue> *)
  {
    this->SetNumberOfComponents(1);
    this->SetPixelType(IOPixelEnum::ARRAY);
    this->SetComponentType(MapPixelType<TValue>::CType);
  }
  template <typename TPixel, unsigned VLength>
  void
  SetPixelTypeInfo(const Matrix<TPixel, VLength, VLength> *)
  {
    this->SetNumberOfComponents(VLength * VLength);
    this->SetPixelType(IOPixelEnum::MATRIX);
    this->SetComponentType(MapPixelType<TPixel>::CType);
  }
  template <typename TValue>
  void
  SetPixelTypeInfo(const VariableSizeMatrix<TValue> *)
  {
    this->SetNumberOfComponents(1);
    this->SetPixelType(IOPixelEnum::VARIABLESIZEMATRIX);
    this->SetComponentType(MapPixelType<TValue>::CType);
  }


protected:
  ImageIOBase();
  ~ImageIOBase() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  virtual const ImageRegionSplitterBase *
  GetImageRegionSplitter() const;

  /** Check fileName as an extensions contained in the supported
   * extension list. If ignoreCase is true, the case of the characters
   * is ignored.
   */
  virtual bool
  HasSupportedReadExtension(const char * fileName, bool ignoreCase = true);
  virtual bool
  HasSupportedWriteExtension(const char * fileName, bool ignoreCase = true);

  /** Used internally to keep track of the type of the pixel. */
  IOPixelEnum m_PixelType{ IOPixelEnum::SCALAR };

  /** Used internally to keep track of the type of the component. It is set
   * when ComputeStrides() is invoked. */
  IOComponentEnum m_ComponentType{ IOComponentEnum::UNKNOWNCOMPONENTTYPE };

  /** Big or Little Endian, and the type of the file. (May be ignored.) */
  IOByteOrderEnum m_ByteOrder{ IOByteOrderEnum::OrderNotApplicable };

  IOFileEnum m_FileType{ IOFileEnum::TypeNotApplicable };

  /** Does the ImageIOBase object have enough info to be of use? */
  bool m_Initialized;

  /** Filename to read */
  std::string m_FileName;

  /** Stores the number of components per pixel. This will be 1 for
   * grayscale images, 3 for RGBPixel images, and 4 for RGBPixelA images. */
  unsigned int m_NumberOfComponents;

  /** The number of independent dimensions in the image. */
  unsigned int m_NumberOfDimensions{ 0 };

  /** Should we compress the data? */
  bool m_UseCompression{ false };


  int         m_CompressionLevel{ 30 };
  int         m_MaximumCompressionLevel{ 100 };
  std::string m_Compressor{ "uninitialized" };

  /** Set/Get enforced maximum compression level value to limit range  */
  virtual void
  SetMaximumCompressionLevel(int);
  itkGetConstMacro(MaximumCompressionLevel, int);

  /** Called when the compressor changes value. The compressor string
   * is converted to uppercase for case insensitive comparisons.
   **/
  virtual void
  InternalSetCompressor(const std::string & _compressor);

  /** Should we use streaming for reading */
  bool m_UseStreamedReading;

  /** Should we use streaming for writing */
  bool m_UseStreamedWriting;

  /** Should we expand RGB palette or stay scalar */
  bool m_ExpandRGBPalette;

  /** true if a RGB palette has been read and the image
   * kept scalar */
  bool m_IsReadAsScalarPlusPalette;

  /** Should we try to include a RGB palette while writing the image  */
  bool m_WritePalette;

  /** The region to read or write. The region contains information about the
   * data within the region to read or write. */
  ImageIORegion m_IORegion;

  /** The array which stores the number of pixels in the x, y, z directions. */
  std::vector<SizeValueType> m_Dimensions;

  /** The array which stores the spacing of pixels in the
   * x, y, z directions. */
  std::vector<double> m_Spacing;

  /** The array which stores the origin of the image. */
  std::vector<double> m_Origin;

  /** The arrays which store the direction cosines of the image. */
  std::vector<std::vector<double>> m_Direction;

  /** Stores the number of bytes it takes to get to the next 'thing'
   * e.g. component, pixel, row, slice, etc. */
  std::vector<SizeType> m_Strides;

  /** Return the object to an initialized state, ready to be used */
  virtual void
  Reset(const bool freeDynamic = true);

  /** Resize the ImageIOBase object to new dimensions. */
  void
  Resize(const unsigned int numDimensions, const unsigned int * dimensions);

  /** Compute the size (in bytes) of the pixel. For
   * example, and RGB pixel of unsigned char would have size 3 bytes. */
  virtual unsigned int
  GetPixelSize() const;

  /** Calculates the different strides (distance from one thing to the next).
   * Upon return,
   * strides[0] = bytes to get to the next component of a pixel,
   * strides[1] = bytes to get to the next pixel in x direction,
   * strides[2] = bytes to get to the next row in y direction,
   * strides[3] = bytes to get to the next slice in z direction, etc. */
  void
  ComputeStrides();

  /** Convenient method for accessing number of bytes to get to the next pixel
   * component. Returns m_Strides[0]. */
  SizeType
  GetComponentStride() const;

  /** Convenient method for accessing the number of bytes to get to the
   * next row. Returns m_Strides[2]. */
  SizeType
  GetRowStride() const;

  /** Convenient method for accessing the number of bytes to get to the
   * next slice. Returns m_Strides[3]. */
  SizeType
  GetSliceStride() const;

  /** \brief Opens a file for reading and random access
   *
   * \param[out] inputStream is an istream presumed to be opened for reading
   * \param[in] filename is the name of the file
   * \param[in] ascii optional (default is false);
   *                  if true than the file will be opened in ASCII mode,
   *                  which generally only applies to Windows
   *
   * The stream is closed if it's already opened. If an error is
   * encountered than an exception will be thrown.
   */
  virtual void
  OpenFileForReading(std::ifstream & inputStream, const std::string & filename, bool ascii = false);

  /** \brief Opens a file for writing and random access
   *
   * \param[out] outputStream is an ostream presumed to be opened for writing
   * \param[in] filename is the name of the file
   * \param[in] truncate optional (default is true);
   *                     if true than the file's existing content is truncated,
   *                     if false than the file is opened for reading and
   *                     writing with existing content intact
   * \param[in] ascii optional (default is false);
   *                  if true than the file will be opened in ASCII mode,
   *                  which generally only applies to Windows
   *
   * The stream is closed if it's already opened. If an error is
   * encountered than an exception will be thrown.
   */
  virtual void
  OpenFileForWriting(std::ofstream &     outputStream,
                     const std::string & filename,
                     bool                truncate = true,
                     bool                ascii = false);

  /** Convenient method to write a buffer as ASCII text. */
  virtual void
  WriteBufferAsASCII(std::ostream & os, const void * buffer, IOComponentEnum ctype, SizeType numComp);

  /** Convenient method to read a buffer as ASCII text. */
  virtual void
  ReadBufferAsASCII(std::istream & is, void * buffer, IOComponentEnum ctype, SizeType numComp);

  /** Convenient method to read a buffer as binary. Return true on success. */
  bool
  ReadBufferAsBinary(std::istream & is, void * buffer, SizeType num);

  /** Insert an extension to the list of supported extensions for reading. */
  void
  AddSupportedReadExtension(const char * extension);

  /** Insert an extension to the list of supported extensions for writing. */
  void
  AddSupportedWriteExtension(const char * extension);


  void
  SetSupportedReadExtensions(const ArrayOfExtensionsType &);
  void
  SetSupportedWriteExtensions(const ArrayOfExtensionsType &);

  /** an implementation of ImageRegionSplitter:GetNumberOfSplits
   */
  virtual unsigned int
  GetActualNumberOfSplitsForWritingCanStreamWrite(unsigned int          numberOfRequestedSplits,
                                                  const ImageIORegion & pasteRegion) const;

  /** an implementation of  ImageRegionSplitter:GetSplit
   */
  virtual ImageIORegion
  GetSplitRegionForWritingCanStreamWrite(unsigned int          ithPiece,
                                         unsigned int          numberOfActualSplits,
                                         const ImageIORegion & pasteRegion) const;

private:
  bool
  HasSupportedExtension(const char *, const ArrayOfExtensionsType &, bool ignoreCase = true);

  ArrayOfExtensionsType m_SupportedReadExtensions;
  ArrayOfExtensionsType m_SupportedWriteExtensions;
};

/** Utility function for writing RAW bytes */
extern ITKIOImageBase_EXPORT void
WriteRawBytesAfterSwapping(IOComponentEnum componentType,
                           const void *    buffer,
                           std::ofstream & file,
                           IOByteOrderEnum byteOrder,
                           SizeValueType   numberOfBytes,
                           SizeValueType   numberOfComponents);

/** Utility function for reading RAW bytes */
extern ITKIOImageBase_EXPORT void
ReadRawBytesAfterSwapping(IOComponentEnum componentType,
                          void *          buffer,
                          IOByteOrderEnum byteOrder,
                          SizeValueType   numberOfComponents);

#define IMAGEIOBASE_TYPEMAP(type, ctype)                                                                               \
  template <>                                                                                                          \
  struct ImageIOBase::MapPixelType<type>                                                                               \
  {                                                                                                                    \
    static constexpr IOComponentEnum CType = ctype;                                                                    \
  }

// the following typemaps are not platform independent
IMAGEIOBASE_TYPEMAP(signed char, IOComponentEnum::CHAR);
IMAGEIOBASE_TYPEMAP(char, std::numeric_limits<char>::is_signed ? IOComponentEnum::CHAR : IOComponentEnum::UCHAR);
IMAGEIOBASE_TYPEMAP(unsigned char, IOComponentEnum::UCHAR);
IMAGEIOBASE_TYPEMAP(short, IOComponentEnum::SHORT);
IMAGEIOBASE_TYPEMAP(unsigned short, IOComponentEnum::USHORT);
IMAGEIOBASE_TYPEMAP(int, IOComponentEnum::INT);
IMAGEIOBASE_TYPEMAP(unsigned int, IOComponentEnum::UINT);
IMAGEIOBASE_TYPEMAP(long, IOComponentEnum::LONG);
IMAGEIOBASE_TYPEMAP(unsigned long, IOComponentEnum::ULONG);
IMAGEIOBASE_TYPEMAP(long long, IOComponentEnum::LONGLONG);
IMAGEIOBASE_TYPEMAP(unsigned long long, IOComponentEnum::ULONGLONG);
IMAGEIOBASE_TYPEMAP(float, IOComponentEnum::FLOAT);
IMAGEIOBASE_TYPEMAP(double, IOComponentEnum::DOUBLE);
#undef IMAGIOBASE_TYPEMAP

} // end namespace itk

#endif // itkImageIOBase_h
