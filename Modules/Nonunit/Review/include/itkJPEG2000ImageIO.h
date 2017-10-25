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
#ifndef itkJPEG2000ImageIO_h
#define itkJPEG2000ImageIO_h


#include <fstream>
#include "itkStreamingImageIOBase.h"
#include "itkAutoPointer.h"

namespace itk
{

class JPEG2000ImageIOInternal;

/** \class JPEG2000ImageIO
 *
 * \brief Supports for the JPEG2000 file format based on openjpeg
 *
 *  JPEG2000 offers a large collection of interesting features including:
 *  compression (lossless and lossy), streaming, multi-channel images.
 *
 *
 * This code was contributed in the Insight Journal paper:
 * "Support for Streaming the JPEG2000 File Format"
 * by Mosaliganti K., Ibanez L., Megason S
 * https://hdl.handle.net/10380/3187
 * http://www.insight-journal.org/browse/publication/741
 *
 *
 *  \ingroup IOFilters
 * \ingroup ITKReview
 */
class JPEG2000ImageIO:public StreamingImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef JPEG2000ImageIO      Self;
  typedef StreamingImageIOBase Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(JPEG2000ImageIO, StreamingImageIOBase);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation() ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer) ITK_OVERRIDE;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  virtual bool CanWriteFile(const char *) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteImageInformation() ITK_OVERRIDE;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(const void *buffer) ITK_OVERRIDE;

  /** Method for supporting streaming.  Given a requested region, determine what
   * could be the region that we can read from the file. This is called the
   * streamable region, which will be smaller than the LargestPossibleRegion and
   * greater or equal to the RequestedRegion */
  virtual ImageIORegion
  GenerateStreamableReadRegionFromRequestedRegion(const ImageIORegion & requested) const ITK_OVERRIDE;

  /** Method required by the base class StreamingImageIOBase */
  virtual SizeType GetHeaderSize(void) const ITK_OVERRIDE;

  /** Define the tile size to use when writing out an image. */
  void SetTileSize(int x, int y);

  /** Currently JPEG2000 does not support streamed writing
   *
   * These methods are re-overridden to not support streaming for
   * now...
   */
  virtual bool CanStreamWrite( void ) ITK_OVERRIDE;

protected:
  JPEG2000ImageIO();
  ~JPEG2000ImageIO() ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(JPEG2000ImageIO);

  AutoPointer< JPEG2000ImageIOInternal >  m_Internal;

  typedef ImageIORegion::SizeValueType  SizeValueType;
  typedef ImageIORegion::IndexValueType IndexValueType;

  void ComputeRegionInTileBoundaries(unsigned int dimension,
                                     SizeValueType tileSize, ImageIORegion & streamableRegion) const;
};
} // end namespace itk

#endif // itkJPEG2000ImageIO_h
