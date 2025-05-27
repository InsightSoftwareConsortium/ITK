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

#include "itkScancoHeaderIO.h"
#include "itkScancoDataManipulation.h"
#include "itksys/SystemTools.hxx"
#include <fstream>
#include <cstring>

namespace itk
{
ScancoHeaderIO::ScancoHeaderIO(itkScancoHeaderData * headerData, std::string filename)
  : m_HeaderData(headerData)
  , m_FileName(filename)
{
  if (this->m_HeaderData == nullptr)
  {
    throw std::runtime_error("ScancoHeaderIO: headerData pointer is null.");
  }
}

unsigned long
ScancoHeaderIO::ReadHeader(std::ifstream & infile)
{
  throw std::runtime_error("ScancoHeaderIO::ReadHeader(std::ifstream&) not implemented.");
}

unsigned long
ScancoHeaderIO::WriteHeader(std::ofstream & outfile, unsigned long imageSize)
{
  throw std::runtime_error("ScancoHeaderIO::WriteHeader(std::ofstream&) not implemented.");
}

void
ScancoHeaderIO::SetFilename(const std::string filename)
{
  this->m_FileName = filename;
}

void
ScancoHeaderIO::SetHeaderData(itkScancoHeaderData * headerData)
{
  this->m_HeaderData = headerData;
}

unsigned long
ScancoHeaderIO::ReadHeader(const std::string filename)
{
  if (filename.empty() && this->m_FileName.empty())
  {
    throw std::runtime_error("ScancoHeaderIO: No filename provided.");
  }
  if (!filename.empty())
  {
    this->m_FileName = filename;
  }

  std::ifstream infile;

#ifdef _MSC_VER
  const std::wstring uncpath = itksys::SystemTools::ConvertToWindowsExtendedPath(this->m_FileName.c_str());
  infile.open(uncpath.c_str(), std::ios::binary);
#else
  infile.open(this->m_FileName.c_str(), std::ios::in | std::ios::binary);
#endif

  if (!infile.is_open() || infile.fail())
  {
    throw std::runtime_error("Could not open file: " + this->m_FileName +
                             " for reading. Reason: " + std::strerror(errno) + "\n");
  }
  return this->ReadHeader(infile);
}

unsigned long
ScancoHeaderIO::WriteHeader(unsigned long imageSize, const std::string filename)
{
  if (filename.empty() && this->m_FileName.empty())
  {
    throw std::runtime_error("ScancoHeaderWriter: No filename provided.");
  }
  else if (imageSize == 0)
  {
    throw std::runtime_error("ScancoHeaderWriter: No image bytes to write");
  }
  if (!filename.empty())
  {
    this->m_FileName = filename;
  }

  std::ofstream outfile;
  // Open the file for writing

  std::ios::openmode mode = std::ios::out | std::ios::trunc | std::ios::binary;

  outfile.open(filename.c_str(), mode);

  if (!outfile.is_open() || outfile.fail())
  {
    throw std::runtime_error("Could not open file: " + this->m_FileName +
                             " for writing. Reason: " + std::strerror(errno) + "/n");
  }

  return this->WriteHeader(outfile, imageSize);
}


} // end namespace itk
