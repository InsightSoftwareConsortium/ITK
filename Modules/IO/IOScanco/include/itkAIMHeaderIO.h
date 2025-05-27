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

#ifndef itkAIMHeaderIO_h
#define itkAIMHeaderIO_h
#include "itkScancoHeaderIO.h"
#include <sstream>

struct AIMV020StructHeader;
struct AIMV030StructHeader;

namespace itk
{
class AIMHeaderIO : public ScancoHeaderIO
{
public:
  using ScancoHeaderIO::ScancoHeaderIO;

  ~AIMHeaderIO();

  /** Read the header from an infile.
   * Fills in the m_HeaderData structure with the header information.
   * \param infile The input file stream to read the header from.
   * \throws std::runtime_error if the file cannot be opened or read.
   * \note Overrides base class virtual method
   */
  unsigned long
  ReadHeader(std::ifstream & infile) override;

  // todo: @ebald19 document
  unsigned long
  WriteHeader(std::ofstream & outfile, unsigned long imageSize) override;

protected:
  /** Read the AIM pre-header from the file stream.
   * \param file The input file stream to read the header from.
   * \param length The length of the pre-header to read.
   * \param offset The offset in the file to start reading from.
   * \returns -1 on failure, 0 on success.
   */
  int
  ReadPreHeader(std::ifstream & file, unsigned long length, unsigned long offset = 0);

  /** Read the AIM image structure header from a buffer.
   * \param headerData pointer to structure containing image structure header
   * \returns -1 on failure, 0 on success.
   */
  void
  ReadImgStructHeader(AIMV020StructHeader * headerData);

  /** Read the AIM image structure header from a buffer.
   * \param headerData pointer to structure containing image structure header
   * \overload
   */
  void
  ReadImgStructHeader(AIMV030StructHeader * headerData);

  /** Read the AIM processing log from the file stream.
   * \param file The input file stream to read the header from.
   * \param length The length of the processing log to read.
   * \param offset The offset in the file to start reading from.
   * \returns -1 on failure, 0 on success.
   */
  int
  ReadProcessingLog(std::ifstream & infile, unsigned long offset, unsigned long length);

  AIMV020StructHeader
  WriteStructHeaderV020();

  AIMV030StructHeader
  WriteStructHeaderV030();

  std::string
  WriteProcessingLog();

  size_t
  WritePreHeader(std::ofstream & outfile, size_t imageSize, ScancoFileVersions version = ScancoFileVersions::AIM_020);

private:
  unsigned int m_IntSize{ 4 }; // Size of integers in the header (4 for AIM v020, 8 for AIM v030)

  /** Header Size = m_PreHeaderSize + m_ImgStructSize + m_ProcessingLogSize */
  unsigned long m_PreHeaderSize{ 0 };
  unsigned long m_ImgStructSize{ 0 };
  unsigned long m_ProcessingLogSize{ 0 };
};
} // namespace itk
#endif // itkAIMHeaderIO_h
