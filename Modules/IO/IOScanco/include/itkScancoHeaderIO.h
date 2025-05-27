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

#ifndef itkScancoHeaderIO_h
#define itkScancoHeaderIO_h
#include "itkScancoDataManipulation.h"
#include <stdexcept>
#include <string>
#include <fstream>

namespace itk
{
/** \class ScancoHeaderIO
 *
 * \brief Read Scanco image headers.
 *
 * \ingroup IOScanco
 */
class ScancoHeaderIO
{
public:
  enum class ScancoFileVersions
  {
    UNRECOGNIZED = 0,
    CTHEADER = 1,
    AIM_020 = 2,
    AIM_030 = 3
  };

  /** Constructor that initializes the header data pointer.
   * If the pointer is null, the object will fail to be created
   * and will throw an exception.
   *
   * \param headerData Pointer to the location to store the header data
   * \note m_RawHeader will be overwritten on read and/or write
   * \param filename optional filename to read the header from or write to.
   * \ifnot filename is provided, it can be specified later using ReadHeader or WriteHeader
   * \see ReadHeader(std::string filename) and WriteHeader(std::string filename)
   * \throws std::runtime_error if headerData is null.
   */
  ScancoHeaderIO(itkScancoHeaderData * headerData, std::string filename = "");

  /** Destructor */
  ~ScancoHeaderIO() = default;

  void
  SetFilename(const std::string filename);

  void
  SetHeaderData(itkScancoHeaderData * headerData);

  /** Read the header from a file.
   * If the filename is empty, it will use the m_FileName member variable.
   * If m_FileName is also empty, it will throw an exception.
   * \param filename The name of the file to read the header from. [optional]
   * \note If the filename is provided, it will overwrite the m_FileName member variable.
   * \throws std::runtime_error if no filename is provided and m_FileName is empty.
   * \returns number of bytes read from the file.
   */
  unsigned long
  ReadHeader(const std::string filename = "");

  /** Read the header from an infile.
   * \param infile The input file stream to read the header from.
   * \throws std::runtime_error if the file cannot be opened or read.
   * \note Subclasses should implement this method to read the header
   *       in the specific format they support.
   * \returns number of bytes read from the file.
   */
  virtual unsigned long
  ReadHeader(std::ifstream & infile);


  /** Write the header to a file.
   * If the filename is empty, it will use the m_FileName member variable.
   * If m_FileName is also empty, it will throw an exception.
   * \param filename The name of the file to write the header to [optional]
   * \param imageSize size of the image that will be written after the header, in bytes
   * \note If the filename is provided, it will overwrite the m_FileName member variable.
   * \throws std::runtime_error if no filename is provided and m_FileName is empty.
   * \returns number of bytes written to the file.
   */
  unsigned long
  WriteHeader(unsigned long imageSize, std::string filename = "");

  /** Write the header to an open file stream.
   * \param outfile Pointer to an open std::ofstream where the header will be written.
   * \param imageSize size of the image that will be written after the header, in bytes
   * \throws std::runtime_error if headerData is null.
   * \note sublcasses should implement this method to write the header
   *       in the specific format they support.
   * \returns number of bytes written to the file.
   */
  virtual unsigned long
  WriteHeader(std::ofstream & outfile, unsigned long imageSize);

protected:
  itkScancoHeaderData * m_HeaderData{ nullptr }; // Pointer data structure to be filled with header information
  std::string           m_FileName = "";
};
} // end namespace itk

#endif // itkScancoHeaderIO_h
