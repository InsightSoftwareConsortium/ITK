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
/**
 * \file   itkIPLCommonImageIO.h
 *         Much of the code for this file reader/writer was taken from
 *         the University of Iowa Imaging Group library with the
 *         permission of the authors, Milan Sonka, Joseph Reinhardt,
 *         Ryan Long, Hans Johnson, Gary Christensen, and others.
 *         The specification for this file format is taken from the
 *         web site http://analyzedirect.com/support/10.0Documents/Analyze_Resource_01.pdf
 * \author Kent Williams
 *         The University of Iowa 2003
 * \brief This file was written as a modification to the itkMetaImageIO
 *        as a new method for reading in files from the GE4 scanner.
 */

#ifndef itkIPLCommonImageIO_h
#define itkIPLCommonImageIO_h
#include "ITKIOIPLExport.h"

#include "itkImageIOBase.h"
#include "itkIPLFileNameList.h"
#include "itkGEImageHeader.h"

namespace itk
{
/**
 *\class IPLCommonImageIO
 *
 * \author Hans J. Johnson
 * \brief Class that defines how to read GE4 file format.
 *
 * \ingroup IOFilters
 * \ingroup ITKIOIPL
 */
class ITKIOIPL_EXPORT IPLCommonImageIO : public ImageIOBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(IPLCommonImageIO);

  /** Standard class type aliases. */
  using Self = IPLCommonImageIO;
  using Superclass = ImageIOBase;
  using Pointer = SmartPointer<Self>;

  using U8 = unsigned char;
  using S8 = signed char;
  using U16 = unsigned short;
  using S16 = signed short;
  using U32 = unsigned int;
  using S32 = signed int;
  using U64 = uint64_t;
  using S64 = int64_t;
  using F32 = float;
  using F64 = double;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(IPLCommonImageIO, Superclass);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this ImageIO implementation.
   * \author Hans J Johnson
   * \param FileNameToRead The name of the file to test for reading.
   * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this ImageIO can read the file specified.
   */
  bool
  CanReadFile(const char * FileNameToRead) override;

  /** Set the spacing and dimension information for the set filename. */
  void
  ReadImageInformation() override;

  /** Optionally, modify spacing, origin and direction */
  virtual void
  ModifyImageInformation()
  {}

  /** Reads the data from disk into the memory buffer provided. */
  void
  Read(void * buffer) override;

  /** Compute the size (in bytes) of the components of a pixel. For
   * example, and RGB pixel of unsigned char would have a
   * component size of 1 byte. */
  unsigned int
  GetComponentSize() const override;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine if the file can be written with this ImageIO implementation.
   * \param FileNameToWrite The name of the file to test for writing.
   * \author Hans J. Johnson
   * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this ImageIO can write the file specified.
   */
  bool
  CanWriteFile(const char * FileNameToWrite) override;

  /** Set the spacing and dimension information for the set filename. */
  void
  WriteImageInformation() override;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  void
  Write(const void * buffer) override;

  /** Set sorting method by name ascending. */
  virtual void
  SortImageListByNameAscend();

  /** Set sorting method by name descending. */
  virtual void
  SortImageListByNameDescend();

protected:
  IPLCommonImageIO();
  ~IPLCommonImageIO() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  int
  AddElementToList(char const * const filename,
                   const float        sliceLocation,
                   const int          offset,
                   const int          XDim,
                   const int          YDim,
                   const float        XRes,
                   const float        YRes,
                   const int          Key1,
                   const int          Key2);

  void
  sortImageListAscend();

  void
  sortImageListDescend();

  int
  statTimeToAscii(void * clock, char * timeString, int len);

  virtual GEImageHeader *
  ReadHeader(const char * FileNameToRead);

  //
  // data members
  GEImageHeader *   m_ImageHeader;
  IOByteOrderEnum   m_SystemByteOrder;
  IPLFileNameList * m_FilenameList;
  //
  // return 0 on success, -1 on failure
  int
  GetStringAt(std::ifstream & f, std::streamoff Offset, char * buf, size_t amount, bool throw_exception = true);

  int
  GetIntAt(std::ifstream & f, std::streamoff Offset, int * ip, bool throw_exception = true);

  int
  GetShortAt(std::ifstream & f, std::streamoff Offset, short * ip, bool throw_exception = true);

  int
  GetFloatAt(std::ifstream & f, std::streamoff Offset, float * ip, bool throw_exception = true);

  int
  GetDoubleAt(std::ifstream & f, std::streamoff Offset, double * ip, bool throw_exception = true);

  short
  hdr2Short(char * hdr);

  int
  hdr2Int(char * hdr);

  float
  hdr2Float(char * hdr);

  double
  hdr2Double(char * hdr);
};
} // end namespace itk
#define RAISE_EXCEPTION()                                                                                              \
  {                                                                                                                    \
    ExceptionObject exception(__FILE__, __LINE__);                                                                     \
    exception.SetDescription("File cannot be read");                                                                   \
    throw exception;                                                                                                   \
  }                                                                                                                    \
  ITK_MACROEND_NOOP_STATEMENT

#define IOCHECK()                                                                                                      \
  if (f.fail())                                                                                                        \
  {                                                                                                                    \
    if (f.is_open())                                                                                                   \
    {                                                                                                                  \
      f.close();                                                                                                       \
    }                                                                                                                  \
    RAISE_EXCEPTION();                                                                                                 \
  }                                                                                                                    \
  ITK_MACROEND_NOOP_STATEMENT

#endif // itkIPLCommonImageIO_h
