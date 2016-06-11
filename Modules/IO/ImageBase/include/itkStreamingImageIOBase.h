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
#ifndef itkStreamingImageIOBase_h
#define itkStreamingImageIOBase_h
#include "ITKIOImageBaseExport.h"

#include "itkImageIOBase.h"

#include <fstream>

namespace itk
{
/** \class StreamingImageIOBase
 *
 * \brief A base class for specific ImageIO file formats which support
 * streaming
 *
 * This class overloads the methods needed to enable streaming. These
 * methods are utilized by the ImageFileReader and
 * ImageFileWriter. The implementation supports streaming of an
 * arbitrary sized region as well as pasting to new or existing file (
 * of the same name, size, and pixel type ).
 * \sa CanStreamWrite CanStreamRead
 * GenerateStreamableReadRegionFromRequestedRegion GetActualNumberOfSplitsForWriting
 *
 * Additionally low level IO methods are provided to read and write an IORegion from
 * a file.
 * \sa StreamReadBufferAsBinary StreamWriteBufferAsBinary
 *
 * This implementation was taken fron the Insight Joural:
 * https://hdl.handle.net/10380/3171
 *
 * \sa itk::ImageFileReader itk::ImageFileWriter
 * \ingroup IOFilters
 * \ingroup ITKIOImageBase
 */
class ITKIOImageBase_EXPORT StreamingImageIOBase:public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef StreamingImageIOBase Self;
  typedef ImageIOBase          Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(StreamingImageIOBase, ImageIOBase);

  // see super class for documentation
  //
  // overidden to return true
  virtual bool CanStreamWrite(void) ITK_OVERRIDE;

  // see super class for documentation
  //
  // overidden to return true
  virtual bool CanStreamRead(void) ITK_OVERRIDE;

  // see super class for documentation
  //
  // If UseStreamedReading is true, then returned region is the
  // requested region parameter.
  virtual ImageIORegion GenerateStreamableReadRegionFromRequestedRegion(const ImageIORegion & requested) const ITK_OVERRIDE;

  // see super class for documentation
  //
  // Verifies the set file name meets the pasting requirements, then calls
  // GetActualNumberOfSplitsForWritingCanStreamWrite
  virtual unsigned int GetActualNumberOfSplitsForWriting(unsigned int numberOfRequestedSplits,
                                                         const ImageIORegion & pasteRegion,
                                                         const ImageIORegion & largestPossibleRegion) ITK_OVERRIDE;

protected:
  StreamingImageIOBase();
  // virtual ~StreamingImageIOBase(); not needed
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** \brief Returns true if GetIORegion is not the same size as the
   * largest region give by GetNumberOfDimensions.
   *
   * This compares the IORegion to the size of the image in the
   * file. With out regaurd to the dimensions of either, if the
   * images represent the same region then false is returned.
   */
  virtual bool RequestedToStream() const;

  /** \brief Reimplemented from super class to get around 2GB
   * read/write limitation
   *
   * \todo Move this method to itk::ImageIOBase
   */
  virtual bool ReadBufferAsBinary(std::istream & is, void *buffer, SizeType num);

  /** \brief Reimplemented from super class to get around 2GB
   * read/write limitation.
   *
   * \todo Move this methods to itk::ImageIOBase
   */
  virtual bool WriteBufferAsBinary(std::ostream & is, const void *buffer, SizeType num);

  /** \brief Reads the set IORegion from os into buffer
   *
   * \param os is an istream presumed to be opened for reading in binary
   * mode
   * \param buffer is pointer to an allocated block of memory
   * suitable to hold the IORegion of the pixel type
   *
   * This methods relies on GetDataPosition to determin where the
   * data is located in the file. It uses m_IORegion to determin the
   * requested region to read.
   *
   * The files data is assumed to be unpadded and continuous in the
   * file for the size of the image in the dimensions of the
   * m_IORegion. This means that the image file could be broken into
   * slices, but not blocks for this methods to be used.
   */
  virtual bool StreamReadBufferAsBinary(std::istream & os, void *buffer);

  /** \brief Writes the set IORegion from buffer into os
   *
   * \param os is an ostream presumed to be opened for writing and
   * reading
   * \param buffer is a pointer to the data in a continuous block
   * for the region
   *
   * This methods relies on GetDataPosition to determin where the data
   * is located in the file. It usesy m_IORegion determin the requested
   * region to written.
   */
  virtual bool StreamWriteBufferAsBinary(std::ostream & os, const void *buffer);

  /** \brief Returns the size of the header in the file */
  virtual SizeType GetHeaderSize(void) const = 0;

  /** \brief Returns the byte offset into the file where the data is located
   *
   * The default implementation is to return the header size.
   */
  virtual SizeType GetDataPosition(void) const { return this->GetHeaderSize(); }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(StreamingImageIOBase);
};
} // namespace itk

#endif
