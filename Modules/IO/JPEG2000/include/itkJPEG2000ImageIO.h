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
#ifndef itkJPEG2000ImageIO_h
#define itkJPEG2000ImageIO_h


#include <fstream>
#include "ITKIOJPEG2000Export.h"
#include "itkStreamingImageIOBase.h"
#include "memory"


namespace itk
{

class JPEG2000ImageIOInternal;
class JPEG2000ImageIOInternalEnums;
/**\class JPEG2000ImageIOInternalEnums
 * \brief This class contains all enum classes used by JPEG2000ImageIOInternal class.
 * \ingroup ITKIOJPEG2000
 */
class JPEG2000ImageIOInternalEnums
{
public:
  /**
   * \class DecodingFormat
   * \ingroup ITKIOJPEG2000
   * */
  enum class DecodingFormat : uint8_t
  {
    J2K_CFMT = 0,
    JP2_CFMT = 1,
    JPT_CFMT = 2,
    MJ2_CFMT = 3
  };
  /**\class DFMFormat
   * \ingroup ITKIOJPEG2000
   * */
  enum class DFMFormat : uint8_t
  {
    PXM_DFMT = 0,
    PGX_DFMT = 1,
    BMP_DFMT = 2,
    YUV_DFMT = 3
  };
};
// Define how to print enumeration
extern ITKIOJPEG2000_EXPORT std::ostream &
                            operator<<(std::ostream & out, const JPEG2000ImageIOInternalEnums::DecodingFormat value);
extern ITKIOJPEG2000_EXPORT std::ostream &
                            operator<<(std::ostream & out, const JPEG2000ImageIOInternalEnums::DFMFormat value);

/**
 *\class JPEG2000ImageIO
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
 * \ingroup ITKIOJPEG2000
 */
class ITKIOJPEG2000_EXPORT JPEG2000ImageIO : public StreamingImageIOBase
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(JPEG2000ImageIO);

  /** Standard class type aliases. */
  using Self = JPEG2000ImageIO;
  using Superclass = StreamingImageIOBase;
  using Pointer = SmartPointer<Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(JPEG2000ImageIO, StreamingImageIOBase);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  bool
  CanReadFile(const char *) override;

  /** Set the spacing and dimension information for the set filename. */
  void
  ReadImageInformation() override;

  /** Reads the data from disk into the memory buffer provided. */
  void
  Read(void * buffer) override;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  bool
  CanWriteFile(const char *) override;

  /** Set the spacing and dimension information for the set filename. */
  void
  WriteImageInformation() override;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  void
  Write(const void * buffer) override;

  /** Method for supporting streaming.  Given a requested region, determine what
   * could be the region that we can read from the file. This is called the
   * streamable region, which will be smaller than the LargestPossibleRegion and
   * greater or equal to the RequestedRegion */
  ImageIORegion
  GenerateStreamableReadRegionFromRequestedRegion(const ImageIORegion & requested) const override;

  /** Method required by the base class StreamingImageIOBase */
  SizeType
  GetHeaderSize() const override;

  /** Define the tile size to use when writing out an image. */
  void
  SetTileSize(int x, int y);

  /** Currently JPEG2000 does not support streamed writing
   *
   * These methods are re-overridden to not support streaming for
   * now...
   */
  bool
  CanStreamWrite() override;

protected:
  JPEG2000ImageIO();
  ~JPEG2000ImageIO() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  std::unique_ptr<JPEG2000ImageIOInternal> m_Internal;

  using SizeValueType = ImageIORegion::SizeValueType;
  using IndexValueType = ImageIORegion::IndexValueType;

  void
  ComputeRegionInTileBoundaries(unsigned int dimension, SizeValueType tileSize, ImageIORegion & streamableRegion) const;
};
} // end namespace itk

#endif // itkJPEG2000ImageIO_h
