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
#include "itkImageRegionSplitterBase.h"

#include "vnl/vnl_vector.h"
#include "vcl_compiler.h"

#include <fstream>
#include <string>

namespace itk
{
// Forward reference for VariableLengthVector
template <typename TValue> class VariableLengthVector;

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
class ITKIOImageBase_EXPORT ImageIOBase:public LightProcessObject
{
public:
  /** Standard class typedefs. */
  typedef ImageIOBase          Self;
  typedef LightProcessObject   Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageIOBase, Superclass);

  /** Set/Get the name of the file to be read. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);

  /** Types for managing image size and image index components. */
  typedef ::itk::IndexValueType  IndexValueType;
  typedef ::itk::SizeValueType   SizeValueType;

  /**
   * \class UnknownType
   * Used to return information when types are unknown.
   * \ingroup ITKIOImageBase
   */
  class UnknownType {};

  /** Enums used to manipulate the pixel type. The pixel type provides
   * context for automatic data conversions (for instance, RGB to
   * SCALAR, VECTOR to SCALAR). */
  typedef  enum { UNKNOWNPIXELTYPE, SCALAR, RGB, RGBA, OFFSET, VECTOR,
                  POINT, COVARIANTVECTOR, SYMMETRICSECONDRANKTENSOR,
                  DIFFUSIONTENSOR3D, COMPLEX, FIXEDARRAY, MATRIX }  IOPixelType;

  /** Enums used to manipulate the component type. The component type
   * refers to the actual storage class associated with either a
   * SCALAR pixel type or elements of a compound pixel.
   */
  typedef  enum { UNKNOWNCOMPONENTTYPE, UCHAR, CHAR, USHORT, SHORT, UINT, INT,
                  ULONG, LONG, ULONGLONG, LONGLONG, FLOAT, DOUBLE } IOComponentType;

  /** Set/Get the number of independent variables (dimensions) in the
   * image being read or written. Note this is not necessarily what
   * is written, rather the IORegion controls that. */
  void SetNumberOfDimensions(unsigned int);

  itkGetConstMacro(NumberOfDimensions, unsigned int);

  /** Set/Get the image dimensions in the x, y, z, etc. directions.
   * GetDimensions() is typically used after reading the data; the
   * SetDimensions() is used prior to writing the data. */
  virtual void SetDimensions(unsigned int i, SizeValueType dim);

  virtual SizeValueType GetDimensions(unsigned int i) const
  { return m_Dimensions[i]; }

  /** Set/Get the image origin on a axis-by-axis basis. The SetOrigin() method
   * is required when writing the image. */
  virtual void SetOrigin(unsigned int i, double origin);

  virtual double GetOrigin(unsigned int i) const
  {
    return m_Origin[i];
  }

  /** Set/Get the image spacing on an axis-by-axis basis. The
   * SetSpacing() method is required when writing the image. */
  virtual void SetSpacing(unsigned int i, double spacing);

  virtual double GetSpacing(unsigned int i) const
  {
    return m_Spacing[i];
  }

  /** Set/Get the image direction on an axis-by-axis basis. The
   * SetDirection() method is required when writing the image. */
  virtual void SetDirection(unsigned int i, const std::vector< double > & direction);

  virtual void SetDirection(unsigned int i, const vnl_vector< double > & direction);

  virtual std::vector< double > GetDirection(unsigned int i) const
  {
    return m_Direction[i];
  }

  /** Return the directions to be assigned by default to recipient
   *  images whose dimension is smaller than the image dimension in file.  */
  virtual std::vector< double > GetDefaultDirection(unsigned int i) const;

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
  itkSetEnumMacro(PixelType, IOPixelType);
  itkGetEnumMacro(PixelType, IOPixelType);

  /** Set/Get the component type of the image. This is always a native
   * type. */
  itkSetEnumMacro(ComponentType, IOComponentType);
  itkGetEnumMacro(ComponentType, IOComponentType);
  /** get the type_info for the current pixel component type.
   * This function is DEPRECATED and only provided for backwards
   * compatibility.  There is no use for this method that can't
   * be satisfied by calling GetComponentType.
   */
  virtual const std::type_info & GetComponentTypeInfo() const;

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

  /** Determine whether a paletized image file has been read as a scalar image
   *  plus a color palette.
   *  ExpandRGBPalette must be set to true, and the file must be a
   *  palette image file supported for palette reading.*/
  itkGetConstMacro(IsReadAsScalarPlusPalette, bool);

  /** Convenience method returns the IOComponentType as a string. This can be
   * used for writing output files. */
  static std::string GetComponentTypeAsString(IOComponentType);

  /** Convenience method returns the IOComponentType corresponding to a string. */
  static IOComponentType GetComponentTypeFromString(const std::string &typeString);

  /** Convenience method returns the IOPixelType as a string. This can be
   * used for writing output files. */
  static std::string GetPixelTypeAsString(IOPixelType);

  /** Convenience method returns the IOPixelType corresponding to a string. */
  static IOPixelType GetPixelTypeFromString(const std::string &pixelString);

  /** Enums used to specify write style: whether binary or ASCII. Some
   * subclasses use this, some ignore it. */
  typedef  enum { ASCII, Binary, TypeNotApplicable } FileType;

  /** Enums used to specify byte order; whether Big Endian or Little Endian.
   * Some subclasses use this, some ignore it. */
  typedef  enum { BigEndian, LittleEndian, OrderNotApplicable } ByteOrder;

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
    this->SetFileType(Binary);
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

  /** Convenience method returns the FileType as a string. This can be
   * used for writing output files. */
  std::string GetFileTypeAsString(FileType) const;

  /** Convenience method returns the ByteOrder as a string. This can be
   * used for writing output files. */
  std::string GetByteOrderAsString(ByteOrder) const;

  /** Type for representing size of bytes, and or positions along a file */
  typedef ::itk::intmax_t          SizeType;

  /** Type for representing size of bytes, and or positions along a memory
    buffer */
  typedef ::itk::OffsetValueType   BufferSizeType;

  /** Convenient method for accessing the number of bytes to get to
   * the next pixel. Returns m_Strides[1];
   *
   * Please note that this methods depends the private methods
   * ComputeStrides being called, otherwise this is the incorrect value.
   */
  virtual SizeType GetPixelStride() const;

  /** Return the number of pixels in the image. */
  SizeType GetImageSizeInPixels() const;

  /** Return the number of bytes in the image. */
  SizeType GetImageSizeInBytes() const;

  /** Return the number of pixels times the number
   * of components in the image. */
  SizeType GetImageSizeInComponents() const;

  /** Compute the size (in bytes) of the components of a pixel. For
   * example, and RGB pixel of unsigned char would have a
   * component size of 1 byte. This method can be invoked only after
   * the component type is set. */
  virtual unsigned int GetComponentSize() const;

  /*-------- This part of the interfaces deals with reading data ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *) = 0;

  /** Determine if the ImageIO can stream reading from the
      current settings. Default is false. If this is queried after
      the header of the file has been read then it will indicate if
      that file can be streamed */
  virtual bool CanStreamRead()
  {
    return false;
  }

  /** Read the spacing and dimensions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void ReadImageInformation() = 0;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer) = 0;

  /*-------- This part of the interfaces deals with writing data ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanWriteFile(const char *)  = 0;

  /** Determine if the ImageIO can stream write from the
   *  current settings.
   *
   * There are two types of non exclusive streaming: pasteing subregions, and iterative
   *
   */
  virtual bool CanStreamWrite()
  {
    return false;
  }

  /** Writes the spacing and dimensions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void WriteImageInformation() = 0;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. The buffer is cast to a
   * pointer to the beginning of the image data. */
  virtual void Write(const void *buffer) = 0;

  /* --- Support reading and writing data as a series of files. --- */

  /** The different types of ImageIO's can support data of varying
   * dimensionality. For example, some file formats are strictly 2D
   * while others can support 2D, 3D, or even n-D. This method returns
   * true/false as to whether the ImageIO can support the dimension
   * indicated. */
  virtual bool SupportsDimension(unsigned long dim)
  {
    return ( dim == 2 );
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
   * then an excepetion should be thrown.
   *
   * The default implementation depends on CanStreamWrite.
   * If false then 1 is returned (unless pasting is indicated), so that the whole file will be updated in one region.
   * If true then its assumed that any arbitrary region can be written
   * to any file. So the users request will be respected. If a derived
   * class has more restictive conditions then they should be checked
   */
  virtual unsigned int GetActualNumberOfSplitsForWriting(unsigned int numberOfRequestedSplits,
                                                         const ImageIORegion & pasteRegion,
                                                         const ImageIORegion & largestPossibleRegion);

  /** returns the ith IORegion
   *
   * numberOfActualSplits should be the value returned from GetActualNumberOfSplitsForWriting with the same parameters
   *
   * Derieved classes should overload this method to return a compatible region
   */
  virtual ImageIORegion GetSplitRegionForWriting(unsigned int ithPiece,
                                                 unsigned int numberOfActualSplits,
                                                 const ImageIORegion & pasteRegion,
                                                 const ImageIORegion & largestPossibleRegion);

  /** Type for the list of strings to be used for extensions.  */
  typedef  std::vector< std::string > ArrayOfExtensionsType;

  /** This method returns an array with the list of filename extensions
   * supported for reading by this ImageIO class. This is intended to
   * facilitate GUI and application level integration.
   */
  const ArrayOfExtensionsType & GetSupportedReadExtensions() const;

  /** This method returns an array with the list of filename extensions
   * supported for writing by this ImageIO class. This is intended to
   * facilitate GUI and application level integration.
   */
  const ArrayOfExtensionsType & GetSupportedWriteExtensions() const;

  template <typename TPixel>
    void SetTypeInfo(const TPixel *);

  /** Map between C++ Pixel type and ImageIOBase ComponentType */
  template <typename TPixel>
    struct MapPixelType
  {
    static ITK_CONSTEXPR_VAR IOComponentType CType =
      UNKNOWNCOMPONENTTYPE;
  };
  template <typename TPixel>
    void SetPixelTypeInfo(const TPixel *)
  {
    this->SetNumberOfComponents(1);
    this->SetPixelType(SCALAR);
    this->SetComponentType(MapPixelType<TPixel>::CType);
  }
  template <typename TPixel>
    void SetPixelTypeInfo(const RGBPixel< TPixel > *)
  {
    this->SetNumberOfComponents(3);
    this->SetPixelType(RGB);
    this->SetComponentType(MapPixelType<TPixel>::CType);
  }
  template <typename TPixel>
    void SetPixelTypeInfo(const RGBAPixel< TPixel > *)
  {
    this->SetNumberOfComponents(4);
    this->SetPixelType(RGBA);
    this->SetComponentType(MapPixelType<TPixel>::CType);
  }
  template <typename TPixel, unsigned VLength>
    void SetPixelTypeInfo(const Vector< TPixel , VLength > *)
  {
    this->SetNumberOfComponents(VLength);
    this->SetPixelType(VECTOR);
    this->SetComponentType(MapPixelType<TPixel>::CType);
  }
  template <typename TPixel>
    void SetPixelTypeInfo(const VariableLengthVector< TPixel > *)
  {
    this->SetNumberOfComponents(1);
    this->SetPixelType(VECTOR);
    this->SetComponentType(MapPixelType<TPixel>::CType);
  }
  template <typename TPixel, unsigned VLength>
    void SetPixelTypeInfo(const CovariantVector< TPixel,VLength > *)
  {
    this->SetNumberOfComponents(VLength);
    this->SetPixelType(COVARIANTVECTOR);
    this->SetComponentType(MapPixelType<TPixel>::CType);
  }
  template <typename TPixel,unsigned VLength>
    void SetPixelTypeInfo(const FixedArray< TPixel,VLength > *)
  {
    this->SetNumberOfComponents(VLength);
    this->SetPixelType(COVARIANTVECTOR);
    this->SetComponentType(MapPixelType<TPixel>::CType);
  }

  template <typename TPixel, unsigned VLength>
    void SetPixelTypeInfo(const SymmetricSecondRankTensor<TPixel,VLength> *)
  {
    this->SetNumberOfComponents(VLength * (VLength + 1) / 2 );
    this->SetPixelType(SYMMETRICSECONDRANKTENSOR);
    this->SetComponentType(MapPixelType<TPixel>::CType);
  }

  template <typename TPixel>
    inline void SetPixelTypeInfo(const DiffusionTensor3D< TPixel > *)
  {
    this->SetNumberOfComponents(6);
    this->SetPixelType(DIFFUSIONTENSOR3D);
    this->SetComponentType(MapPixelType<TPixel>::CType);
    }

  template <typename TPixel, unsigned VLength>
    void SetPixelTypeInfo(const Matrix< TPixel,VLength, VLength > *)
  {
    this->SetNumberOfComponents(VLength * VLength);
    this->SetPixelType(MATRIX);
    this->SetComponentType(MapPixelType<TPixel>::CType);
  }

  template <typename TPixel>
    void SetPixelTypeInfo(const std::complex< TPixel > *)
  {
    this->SetNumberOfComponents(2);
    this->SetPixelType(COMPLEX);
    this->SetComponentType(MapPixelType<TPixel>::CType);
  }

  template <unsigned VLength>
    void SetPixelTypeInfo(const Offset< VLength > *)
  {
    this->SetNumberOfComponents(VLength);
    this->SetPixelType(ImageIOBase::OFFSET);
    this->SetComponentType(ImageIOBase::LONG);
  }

protected:
  ImageIOBase();
  ~ImageIOBase() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual const ImageRegionSplitterBase* GetImageRegionSplitter() const;

  /** Used internally to keep track of the type of the pixel. */
  IOPixelType m_PixelType;

  /** Used internally to keep track of the type of the component. It is set
   * when ComputeStrides() is invoked. */
  IOComponentType m_ComponentType;

  /** Big or Little Endian, and the type of the file. (May be ignored.) */
  ByteOrder m_ByteOrder;

  FileType m_FileType;

  /** Does the ImageIOBase object have enough info to be of use? */
  bool m_Initialized;

  /** Filename to read */
  std::string m_FileName;

  /** Stores the number of components per pixel. This will be 1 for
   * grayscale images, 3 for RGBPixel images, and 4 for RGBPixelA images. */
  unsigned int m_NumberOfComponents;

  /** The number of independent dimensions in the image. */
  unsigned int m_NumberOfDimensions;

  /** Should we compress the data? */
  bool m_UseCompression;

  /** Should we use streaming for reading */
  bool m_UseStreamedReading;

  /** Should we use streaming for writing */
  bool m_UseStreamedWriting;

  /** Should we expand RGB palette or stay scalar */
  bool m_ExpandRGBPalette;

  /** true if a RGB palette has been read and the image
    * kept scalar */
  bool m_IsReadAsScalarPlusPalette;

  /** The region to read or write. The region contains information about the
   * data within the region to read or write. */
  ImageIORegion m_IORegion;

  /** The array which stores the number of pixels in the x, y, z directions. */
  std::vector< SizeValueType > m_Dimensions;

  /** The array which stores the spacing of pixels in the
   * x, y, z directions. */
  std::vector< double > m_Spacing;

  /** The array which stores the origin of the image. */
  std::vector< double > m_Origin;

  /** The arrays which store the direction cosines of the image. */
  std::vector< std::vector< double > > m_Direction;

  /** Stores the number of bytes it takes to get to the next 'thing'
   * e.g. component, pixel, row, slice, etc. */
  std::vector< SizeType > m_Strides;

  /** Return the object to an initialized state, ready to be used */
  virtual void Reset(const bool freeDynamic = true);

  /** Resize the ImageIOBase object to new dimensions. */
  void Resize(const unsigned int numDimensions,
              const unsigned int *dimensions);

  /** Compute the size (in bytes) of the pixel. For
   * example, and RGB pixel of unsigned char would have size 3 bytes. */
  virtual unsigned int GetPixelSize() const;

  /** Calculates the different strides (distance from one thing to the next).
   * Upon return,
   * strides[0] = bytes to get to the next component of a pixel,
   * strides[1] = bytes to get to the next pixel in x direction,
   * strides[2] = bytes to get to the next row in y direction,
   * strides[3] = bytes to get to the next slice in z direction, etc. */
  void ComputeStrides();

  /** Convenient method for accessing number of bytes to get to the next pixel
   * component. Returns m_Strides[0]. */
  SizeType GetComponentStride() const;

  /** Convenient method for accessing the number of bytes to get to the
   * next row. Returns m_Strides[2]. */
  SizeType GetRowStride() const;

  /** Convenient method for accessing the number of bytes to get to the
   * next slice. Returns m_Strides[3]. */
  SizeType GetSliceStride() const;

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
  virtual void OpenFileForReading(std::ifstream & inputStream, const std::string & filename,
                                  bool ascii = false);

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
  virtual void OpenFileForWriting(std::ofstream & outputStream, const std::string & filename,
                                  bool truncate = true, bool ascii = false);

  /** Convenient method to write a buffer as ASCII text. */
  virtual void WriteBufferAsASCII(std::ostream & os, const void *buffer,
                          IOComponentType ctype,
                          SizeType numberOfBytesToWrite);

  /** Convenient method to read a buffer as ASCII text. */
  virtual void ReadBufferAsASCII(std::istream & os, void *buffer,
                         IOComponentType ctype,
                         SizeType numberOfBytesToBeRead);

  /** Convenient method to read a buffer as binary. Return true on success. */
  bool ReadBufferAsBinary(std::istream & os, void *buffer, SizeType numberOfBytesToBeRead);

  /** Insert an extension to the list of supported extensions for reading. */
  void AddSupportedReadExtension(const char *extension);

  /** Insert an extension to the list of supported extensions for writing. */
  void AddSupportedWriteExtension(const char *extension);

  /** an implementation of ImageRegionSplitter:GetNumberOfSplits
   */
  virtual unsigned int GetActualNumberOfSplitsForWritingCanStreamWrite(unsigned int numberOfRequestedSplits,
                                                                       const ImageIORegion & pasteRegion) const;

  /** an implementation of  ImageRegionSplitter:GetSplit
   */
  virtual ImageIORegion GetSplitRegionForWritingCanStreamWrite(unsigned int ithPiece,
                                                               unsigned int numberOfActualSplits,
                                                               const ImageIORegion & pasteRegion) const;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageIOBase);

  ArrayOfExtensionsType m_SupportedReadExtensions;
  ArrayOfExtensionsType m_SupportedWriteExtensions;
};

#define IMAGEIOBASE_TYPEMAP(type,ctype)                         \
  template <> struct ImageIOBase::MapPixelType<type>    \
  {                                                     \
    static ITK_CONSTEXPR_VAR IOComponentType CType = ctype; \
  }

// the following typemaps are not platform independent
#if  VCL_CHAR_IS_SIGNED
IMAGEIOBASE_TYPEMAP(signed char, CHAR);
#endif // VCL_CHAR_IS_SIGNED
IMAGEIOBASE_TYPEMAP(char, CHAR);
IMAGEIOBASE_TYPEMAP(unsigned char, UCHAR);
IMAGEIOBASE_TYPEMAP(short, SHORT);
IMAGEIOBASE_TYPEMAP(unsigned short, USHORT);
IMAGEIOBASE_TYPEMAP(int, INT);
IMAGEIOBASE_TYPEMAP(unsigned int, UINT);
IMAGEIOBASE_TYPEMAP(long, LONG);
IMAGEIOBASE_TYPEMAP(unsigned long, ULONG);
IMAGEIOBASE_TYPEMAP(long long, LONGLONG);
IMAGEIOBASE_TYPEMAP(unsigned long long, ULONGLONG);
IMAGEIOBASE_TYPEMAP(float, FLOAT);
IMAGEIOBASE_TYPEMAP(double, DOUBLE);
#undef IMAGIOBASE_TYPEMAP


} // end namespace itk

#endif // itkImageIOBase_h
