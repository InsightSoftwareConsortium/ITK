#ifndef __itkImageIO_h
#define __itkImageIO_h

#include "itkIntTypes.h"
#include "itkProcessObject.h"
#include "itkObjectFactory.h"
#include "itkImageIOCommon.h"
#include <string>
#include "itkIndent.h"
#include <deque>
#include <ctype.h>

namespace itk
{

class ITK_EXPORT ImageIO : public ProcessObject
{
public:
  /**
   * Smart pointer typedef support.
   */
  typedef ImageIO            Self;
  typedef SmartPointer<Self>  Pointer;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ProcessObject  Superclass;

  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ImageIO, Superclass);

  /**
   * Default load; do whatever is appropriate for the filetype.
   */
  virtual void Load() = 0;

  /**
   * Load a 2D image. If fileName="" (the default), will read from m_FileName
   */
  virtual void Load2D(const std::string fileName="") = 0;

  /**
   * Load a 2D slice from a volume dataset.
   * fileName is file to read from. default="", which uses m_FileName instead
   * sliceNum is the slice # to load (starting at 0). default = 0.
   * offset is the offset, in bytes, into fileData at which the data should be loaded
   *  default = 0
   */
  virtual void Load2DSlice(const std::string fileName="",
                           const unsigned int sliceNum=0,
                           const unsigned int offset=0) = 0;

  /**
   * Load multiple slices to form a volume dataset
   * filePattern is the string of characters after which to start appending numbers
   * (default is to use fName as the pattern)
   * startSlice is the starting file number (e.g. if 5, then start at "testSave005")
   * (default is to start w/ first slice found in directory)
   * endSlice is the ending slice number.
   * (default is to end w/ last consecutively numbered file in directory
   */
  virtual void LoadSeveralSlices (const std::string filePattern="",
                                  const int startSlice=-1,
                                  const int endSlice=-1);

  /**
   * Read a file's header to determine image dimensions, etc.
   * fileName is file to read from. default="", which uses m_FileName instead
   */
  virtual void ReadHeader(const std::string fileName="") = 0;

  /**
   * Default save; do whatever is appropriate for the filetype
   */
  virtual void Save(const std::string headerFile="", const std::string dataFile="") = 0;

  /**
   * Save a 3D image
   */
  virtual void Save3D(const std::string headerFile="", const std::string dataFile="") = 0;

  /**
   * Returns the file extension that a particular ImageIO subclass
   * is capable of handling (e.g. .jpg, .mhd, etc.)
   * Currently only a single string is returned, but can be modified
   * so that a whole list of strings is returned.
   */
  typedef std::deque<std::string> FileExtensionsListType;
  //virtual std::string GetSupportedFileExtensions() const = 0;
  virtual FileExtensionsListType GetSupportedFileExtensions() const = 0;

  /**
   * Set the filename
   */
  itkSetStringMacro(FullFileName);

  /**
   * Get the filename
   */
  itkGetStringMacro(FullFileName);

  /**
   * Display information about an instance of this class
   */
  virtual void Print(std::ostream& os);

  /**
   * Return the size in x, y, z, etc. dimension
   */
  unsigned int GetDimensions(unsigned int i) const;

  /**
   * Return the guts of this class, FileData, which holds the raw
   * pixels of the image loaded from disk
   */
  void* GetFileData();

  /**
   * Convenient method for accessing # bytes to get to the next pixel.
   * Returns m_Strides[1];
   */
  unsigned int GetPixelStride () const;

protected:
  /**
   * Default constructor
   */
  ImageIO();

  /**
   * Destructor
   */
  ~ImageIO();

  /**
   * Does the ImageIO object have enough info to be of use?
   */
  bool m_Initialized;

  /**
   * Full filename: pathname + filename + file extension
   */
  std::string m_FullFileName;

  /**
   * Atomic pixel type being stored
   */
  AtomicPixelType m_PixelType;

  /**
   * Stores the number of components per pixel. This will be 1 for grayscale images,
   * 3 for RGBPixel images, and 4 for RGBPixelA images.
   */
  unsigned int m_ComponentsPerPixel;

  /**
   * Set the number of components per pixel
   */
  itkSetMacro(ComponentsPerPixel, unsigned int);

  /**
   * The number of dimensions in the image
   */
  unsigned int m_NumDimensions;

  /**
   * Set the number of dimensions in an image
   */
  itkSetMacro(NumDimensions, unsigned int);

  /**
   * The array which stores the number of pixels in the x, y, z directions.
   */
  unsigned int m_Dimensions[ITK_MAX_DIMENSIONS];

  /**
   * Stores the number of bytes it takes to get to the next 'thing'
   * e.g. component, pixel, row, slice, etc.
   */
  unsigned int m_Strides[ITK_MAX_DIMENSIONS];

  /**
   * Stores the raw pixels of the image
   */
  void* m_FileData;

  /**
   * Print info about myself
   */
  void PrintSelf(std::ostream& os, Indent indent);

  /**
   * Return the object to an initialized state, ready to be used
   */
  virtual void Reset(const bool freeDynamic = true);

  /**
   * Resize the ImageIO object to new dimensions
   */
  void Resize(const unsigned int numDimensions, const unsigned int* dimensions);

  /**
   * Calculates the different strides (distance from one thing to the next).
   * Upon return,
   * strides[0] = bytes to get to the next component of a pixel,
   * strides[1] = bytes to get to the next pixel in x direction,
   * strides[2] = bytes to get to the next row in y direction,
   * strides[3] = bytes to get to the next slice in z direction, etc.
   */
  void CalcStrides();

  /**
   * Return the # of pixels in the image
   */
  unsigned int ImageSizeInPixels() const;

  /**
   * Return the # of components in the image
   */
  unsigned int ImageSizeInComps() const;

  /**
   * Return the # of bytes in the image
   */
  unsigned int ImageSizeInBytes() const;

  /**
   * Convenient method for accessing # bytes to get to the next pixel component.
   * Returns m_Strides[0];
   */
  unsigned int GetComponentStride() const;

  /**
   * Convenient method for accessing # bytes to get to the next row.
   * Returns m_Strides[2];
   */
  unsigned int GetRowStride () const;

  /**
   * Convenient method for accessing # bytes to get to the next slice.
   * Returns m_Strides[3];
   */
  unsigned int GetSliceStride () const;

};

} // end namespace itk

#endif // __itkImageIO_h
