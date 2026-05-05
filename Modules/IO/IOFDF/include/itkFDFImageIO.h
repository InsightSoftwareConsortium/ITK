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

#ifndef itkFDFImageIO_h
#define itkFDFImageIO_h
#include "IOFDFExport.h"
#include "itkImageIOBase.h"

namespace itk
{

/** \class FDFImageIO
 *
 * \brief ImageIO object for reading and writing FDF images
 *
 * \ingroup IOIOFDF
 *
 */
class IOFDF_EXPORT FDFImageIO : public ImageIOBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FDFImageIO);

  /** Standard class type alias. */
  using Self = FDFImageIO;
  using Superclass = ImageIOBase;
  using Pointer = SmartPointer<Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkOverrideGetNameOfClassMacro(FDFImageIO);

  bool
  SupportsDimension(unsigned long dim) override
  {
    if (dim == 2 || dim == 3)
    {
      return true;
    }
    else
    {
      return false;
    }
  }


  /*-------- This part of the interface deals with reading data. ------ */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  bool
  CanReadFile(const char *) override;

  /** Set the spacing and diemention information for the set filename. */
  void
  ReadImageInformation() override;

  /** Get the type of the pixel.  */
  //   virtual const std::type_info& GetPixelType() const;

  /** Reads the data from disk into the memory buffer provided. */
  void
  Read(void * buffer) override;

  /** Reads 3D data from multiple files assuming one slice per file. */
  virtual void
  ReadVolume(void * buffer);

  /** Compute the size (in bytes) of the components of a pixel. For
   * example, and RGB pixel of unsigned char would have a
   * component size of 1 byte. */
  //   virtual unsigned int GetComponentSize() const;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  bool
  CanWriteFile(const char *) override;

  /** Writes the spacing and dimentions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  void
  WriteImageInformation() override;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  void
  Write(const void * buffer) override;

protected:
  FDFImageIO();
  ~FDFImageIO() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  WriteSlice(std::string & fileName, const void * buffer);

  int
  ReadHeader(const char * FileNameToRead);

private:
  void
  SwapBytesIfNecessary(void * buffer, unsigned long numberOfPixels);

  // Position after ReadImageInformation.
  size_t m_InputPosition;

  std::string        m_SpatialRank;
  std::string        m_Checksum;
  std::string        m_Bits;
  std::vector<int>   m_Size;
  std::vector<float> m_Location;
  std::vector<float> m_Span;
  std::vector<float> m_Roi;
};

} // end namespace itk


#define RAISE_EXCEPTION()                            \
  {                                                  \
    ExceptionObject exception(__FILE__, __LINE__);   \
    exception.SetDescription("File cannot be read"); \
    throw exception;                                 \
  }

#endif
