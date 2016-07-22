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
  /** Standard class typedefs. */
  typedef FDFImageIO         Self;
  typedef ImageIOBase        Superclass;
  typedef SmartPointer<Self> Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FDFImageIO, ImageIOBase);

  virtual bool
  SupportsDimension(unsigned long dim) ITK_OVERRIDE
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
  virtual bool
  CanReadFile(const char *) ITK_OVERRIDE;

  /** Set the spacing and diemention information for the set filename. */
  virtual void
  ReadImageInformation() ITK_OVERRIDE;

  /** Get the type of the pixel.  */
  //   virtual const std::type_info& GetPixelType() const;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void
  Read(void * buffer) ITK_OVERRIDE;

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
  virtual bool
  CanWriteFile(const char *) ITK_OVERRIDE;

  /** Writes the spacing and dimentions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void
  WriteImageInformation() ITK_OVERRIDE;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  virtual void
  Write(const void * buffer) ITK_OVERRIDE;

protected:
  FDFImageIO();
  ~FDFImageIO();
  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void
  WriteSlice(std::string & fileName, const void * buffer);

  int
  ReadHeader(const char * FileNameToRead);

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FDFImageIO);

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
