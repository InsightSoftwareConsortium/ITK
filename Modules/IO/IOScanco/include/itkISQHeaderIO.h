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

#ifndef itkISQHeaderIO_h
#define itkISQHeaderIO_h
#include "itkScancoHeaderIO.h"

struct ISQEncodedPreHeader;
struct ISQEncodedHeaderBlock;
struct RADEncodedHeaderBlock;

namespace itk
{
class ISQHeaderIO : public ScancoHeaderIO
{
public:
  using ScancoHeaderIO::ScancoHeaderIO;

  /** Read the header from an infile.
   * Fills in the m_HeaderData structure with the header information.
   * \param infile The input file stream to read the header from.
   * \throws std::runtime_error if the file cannot be opened or read.
   * \note Overrides base class virtual method
   */
  unsigned long
  ReadHeader(std::ifstream & infile) override;

  /** Write the header to an open file stream.
   * \param outfile Pointer to an open std::ofstream where the header will be written.
   * \throws std::runtime_error if headerData is null.
   * \returns number of bytes written to the file.
   */
  unsigned long
  WriteHeader(std::ofstream & outfile, unsigned long imageSize) override;

private:
  /** Read date into creation and modification date strings
   * \param year The Gregorian year.
   * \param month The Gregorian month (1-12).
   * \param day The Gregorian day (1-31).
   * \param hour The hour (0-23).
   * \param minute The minute (0-59).
   * \param second The second (0-59).
   * \param millis The milliseconds (0-999).
   */
  void
  ReadDateValues(const int year,
                 const int month,
                 const int day,
                 const int hour,
                 const int minute,
                 const int second,
                 const int milli);

  /** Parse and Save pixel and physical dimension values
   * Values are converted into appropriate units
   * \see unit conversions in itkScancoImageIO.h
   * \param imageData encoded data of at least 24 bytes to read from
   * \returns true if the file is a RAD file based on the dimension data,
   *          false otherise
   */
  bool
  ReadDimensionData(ISQEncodedPreHeader * imageData);

  /** Read RAD header values and convert units appropriately
   * \see unit conversions in itkScancoImageIO.h
   * \param headerData struct holding encoded RAD header data
   */
  void
  ReadRADHeader(RADEncodedHeaderBlock * headerData);

  /** Read ISQ header values and convert units appropriately
   * \see unit conversions in itkScancoImageIO.h
   * \param headerData struct holding encoded ISQ header data
   */
  void
  ReadISQHeader(ISQEncodedHeaderBlock * headerData);

  /** Read in extended header data
   * This may include a multi-header section with calibration data
   * \param buffer pointer to start of extended header data
   * \param length length of extended header,
   *               this can be calculated from the data offset found in the last 4 bytes of the main header block
   * \note length must be at least 3 blocks
   * \note buffer must be at least length bytes
   */
  void
  ReadExtendedHeader(const char * buffer, unsigned long length);

  /** Write Calibration data to extended header blocks
   * \param outfile file to write data to
   * \note requires first block (512 bytes) to be written with header data
   * \returns number of bytes written to the file
   */
  unsigned long
  WriteExtendedHeader(std::ofstream & outfile);

  unsigned long m_HeaderSize;
};

} // namespace itk

#endif // itkISQHeaderIO_h
