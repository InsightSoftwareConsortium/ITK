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

  /** Write the header to an open file stream.
   * \param outfile Pointer to an open std::ofstream where the header will be written.
   * \param imageSize size of the image that will be written after the header, in bytes
   * \throws std::runtime_error if headerData is null.
   * \note overrides base class virtual method
   * \returns number of bytes written to the file.
   */
  unsigned long
  WriteHeader(std::ofstream & outfile, unsigned long imageSize) override;

protected:
  /** Read the AIM pre-header from the file stream.
   * \param file The input file stream to read the header from.
   * \param offset The offset in the file to start reading from.
   * \note m_IntSize is used to determine the size of integers in the pre-header,
   *      and should be set appropriately before calling
   * \returns -1 on failure, 0 on success.
   */
  int
  ReadPreHeader(std::ifstream & file, unsigned long offset = 0);

  /** Read the AIM v020 image structure header from a data structure.
   * \param headerData pointer to structure containing image structure header
   */
  void
  ReadImgStructHeader(AIMV020StructHeader * headerData);

  /** Read the AIM v030 image structure header from a data structure.
   * \param headerData pointer to structure containing image structure header
   * \overload ReadImgStructHeader for AIM v020
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

  /** Write the image structure header to an AIM v020 data structure
   * \returns AIMV020StructHeader structure filled with encoded header data
   */
  AIMV020StructHeader
  WriteStructHeaderV020();

  /** Write the image structure header to an AIM v030 data structure
   * \returns AIMV030StructHeader structure filled with encoded header data
   */
  AIMV030StructHeader
  WriteStructHeaderV030();

  /** Write the processing log to a string.
   * \returns std::string containing the processing log populated with the header data
   */
  std::string
  WriteProcessingLog();

  /** Write the image pre-header to a file
   * \param outfile The output file stream to write the pre-header to
   * \param imageSize size of the image that will be written after the header, in bytes
   * \param version The AIM file version to write (default is AIM_020)
   * \returns number of bytes written to the file
   * \note pre-header is written into the current file pointer of the outfile stream
   * \note pre-header blocks are written based on imageSize, m_ImgStructSize, and m_ProcessingLogSize,
   *       which should be set accordingly
   */
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
