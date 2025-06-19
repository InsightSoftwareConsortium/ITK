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

#include "itkISQHeaderIO.h"
#include "itkMath.h"
#include <cstring>

typedef char        EncodedByte;
typedef EncodedByte EncodedInt[4];
typedef EncodedInt  EncodedIntTuple[3];
typedef EncodedByte EncodedDouble[8];

// Structures to store encoded data to write to a file
// using little-endian format

struct ISQEncodedPreHeader
{
  EncodedByte     m_Version[16];
  EncodedInt      m_DataType;
  EncodedInt      m_ImageSizeBytes;
  EncodedInt      m_ImageSizeBlocks;
  EncodedInt      m_PatientIndex;
  EncodedInt      m_ScannerID;
  EncodedByte     m_CreationDate[8];
  EncodedIntTuple m_PixelDimensions;
  EncodedIntTuple m_PhysicalDimensions;
};

struct ISQEncodedHeaderBlock
{
  ISQEncodedPreHeader m_PreHeader;
  EncodedInt          m_SliceThickness;
  EncodedInt          m_SliceIncrement;
  EncodedInt          m_StartPosition;
  EncodedInt          m_DataMin;
  EncodedInt          m_DataMax;
  EncodedInt          m_MuScaling;
  EncodedInt          m_NumberOfSamples;
  EncodedInt          m_NumberOfProjections;
  EncodedInt          m_ScanDistance;
  EncodedInt          m_ScannerType;
  EncodedInt          m_SampleTime;
  EncodedInt          m_MeasurementIndex;
  EncodedInt          m_Site;
  EncodedInt          m_ReferenceLine;
  EncodedInt          m_ReconstructionAlg;
  EncodedByte         m_PatientName[40] = { 0 };
  EncodedInt          m_Energy = { 0 };    /* V */
  EncodedInt          m_Intensity = { 0 }; /* uA */
  EncodedByte         m_Fill[83 * 4] = { 0 };
  EncodedInt          m_DataOffset; /* in 512-byte-blocks */
};

struct RADEncodedHeaderBlock
{
  ISQEncodedPreHeader m_PreHeader;
  EncodedInt          m_MeasurementIndex;
  EncodedInt          m_DataMin;
  EncodedInt          m_DataMax;
  EncodedInt          m_MuScaling;
  EncodedByte         m_PatientName[40] = { 0 };
  EncodedInt          m_ZPosition;
  EncodedByte         m_UnknownFill[4];
  EncodedInt          m_SampleTime;
  EncodedInt          m_Energy = { 0 };    /* V */
  EncodedInt          m_Intensity = { 0 }; /* uA */
  EncodedInt          m_ReferenceLine;
  EncodedInt          m_StartPosition;
  EncodedInt          m_EndPosition;
  EncodedByte         m_Fill[88 * 4] = { 0 };
  EncodedInt          m_DataOffset; /* in 512-byte-blocks */
};

struct ISQCalibrationHeaderBlock
{
  EncodedByte m_Fill1[136];
  EncodedByte m_Title[16];
  EncodedInt  m_CalHeaderSize; // size of calibration header in blocks
  EncodedByte m_Fill2[356];

  EncodedByte m_Fill3[28];
  EncodedByte m_CalibrationData[64];
  EncodedByte m_Fill4[420];

  EncodedByte   m_Fill5[120];
  EncodedInt    m_RescaleType;
  EncodedByte   m_Fill6[12];
  EncodedByte   m_RescaleUnits[16];
  EncodedDouble m_RescaleSlope;
  EncodedDouble m_RescaleIntercept;
  EncodedByte   m_Fill7[8];
  EncodedDouble m_MuWater;
  EncodedByte   m_Fill8[328];
};

constexpr const char * ISQVersionString = "CTDATA-HEADER_V1";

namespace itk
{

unsigned long
ISQHeaderIO::ReadHeader(std::ifstream & infile)
{
  if (!infile.is_open())
  {
    throw std::runtime_error("ISQHeaderIO: Input file stream is not open.");
  }

  // Populate the data structure with the raw header information
  // Initially read 'pre header' block to retrieve the header size and version
  ISQEncodedPreHeader * imageInfo = new ISQEncodedPreHeader;
  infile.read((char *)imageInfo, sizeof(ISQEncodedPreHeader));

  int versionType = CheckVersion(imageInfo->m_Version);

  if (versionType != static_cast<int>(ScancoFileVersions::CTHEADER))
  {
    throw std::runtime_error("ISQHeaderIO: cannot read file, is not an ISQ.");
  }

  memcpy(this->m_HeaderData->m_Version, imageInfo->m_Version, 16);
  this->m_HeaderData->m_Version[16] = '\0'; // ensure null-terminated string
  this->m_HeaderData->m_PixelData.m_ComponentType = DecodeInt(imageInfo->m_DataType);
  // Ignore image blocks and bytes size information
  // This will be re-populated on write
  this->m_HeaderData->m_PatientIndex = DecodeInt(imageInfo->m_PatientIndex);
  this->m_HeaderData->m_ScannerID = DecodeInt(imageInfo->m_ScannerID);
  int year, month, day, hour, minute, second, milli;
  DecodeDate(imageInfo->m_CreationDate, year, month, day, hour, minute, second, milli);
  this->ReadDateValues(year, month, day, hour, minute, second, milli);

  bool isRAD = this->ReadDimensionData(imageInfo);

  delete imageInfo;
  if (isRAD)
  {
    RADEncodedHeaderBlock * headerInfo = new RADEncodedHeaderBlock;
    infile.seekg(0, std::ios::beg);
    infile.read((char *)headerInfo, sizeof(RADEncodedHeaderBlock));
    this->ReadRADHeader(headerInfo);
    delete headerInfo;
  }
  else
  {
    ISQEncodedHeaderBlock * headerInfo = new ISQEncodedHeaderBlock;
    infile.seekg(0, std::ios::beg);
    infile.read((char *)headerInfo, sizeof(ISQEncodedHeaderBlock));
    this->ReadISQHeader(headerInfo);
    delete headerInfo;
  }

  // Check if there is extended header data
  if (this->m_HeaderSize > sizeof(ISQEncodedHeaderBlock))
  {
    // clear the buffers in case they were previously full
    delete[] this->m_HeaderData->m_RawHeader;
    this->m_HeaderData->m_RawHeader = new char[this->m_HeaderSize];
    infile.seekg(0, std::ios::beg);
    infile.read(this->m_HeaderData->m_RawHeader, this->m_HeaderSize);
    if (static_cast<unsigned long>(infile.gcount()) < this->m_HeaderSize)
    {
      return 0;
    }
  }

  this->ReadExtendedHeader(this->m_HeaderData->m_RawHeader + sizeof(ISQEncodedHeaderBlock),
                           this->m_HeaderSize - sizeof(ISQEncodedHeaderBlock));

  return this->m_HeaderSize;
}

unsigned long
ISQHeaderIO::WriteHeader(std::ofstream & outfile, unsigned long imageSize)
{
  // Write Normal Header
  unsigned long bytesWritten = 0;
  outfile.seekp(0, std::ios::beg);

  if (imageSize == 0)
  {
    throw std::runtime_error("Cannot write to file, no image data length set");
    return 0;
  }

  ISQEncodedHeaderBlock header = { 0 };

  PadString(header.m_PreHeader.m_Version, this->m_HeaderData->m_Version, 16);
  EncodeInt(3, header.m_PreHeader.m_DataType);
  EncodeInt(imageSize, header.m_PreHeader.m_ImageSizeBytes);
  EncodeInt(imageSize / ScancoHeaderBlockSize, header.m_PreHeader.m_ImageSizeBlocks);
  EncodeInt(this->m_HeaderData->m_PatientIndex, header.m_PreHeader.m_PatientIndex);
  EncodeInt(this->m_HeaderData->m_ScannerID, header.m_PreHeader.m_ScannerID);
  EncodeDateFromString(header.m_PreHeader.m_CreationDate, this->m_HeaderData->m_CreationDate);

  for (unsigned int dimension = 0; dimension < 3; ++dimension)
  {
    // pixdim
    EncodeInt(this->m_HeaderData->m_PixelData.m_Dimensions[dimension], header.m_PreHeader.m_PixelDimensions[dimension]);
  }
  for (unsigned int dimension = 0; dimension < 3; ++dimension)
  {
    // physdim
    EncodeInt(this->m_HeaderData->m_PixelData.m_Spacing[dimension] * m_HeaderData->m_PixelData.m_Dimensions[dimension] *
                1e3,
              header.m_PreHeader.m_PhysicalDimensions[dimension]);
  }
  EncodeInt((int)(this->m_HeaderData->m_SliceThickness * 1e3), header.m_SliceThickness);
  EncodeInt((int)(this->m_HeaderData->m_SliceIncrement * 1e3), header.m_SliceIncrement);
  EncodeInt((int)(this->m_HeaderData->m_StartPosition * 1e3), header.m_StartPosition);
  EncodeInt((int)(this->m_HeaderData->m_DataRange[0]), header.m_DataMin);
  EncodeInt((int)(this->m_HeaderData->m_DataRange[1]), header.m_DataMax);
  EncodeInt((int)this->m_HeaderData->m_MuScaling, header.m_MuScaling);
  EncodeInt(this->m_HeaderData->m_NumberOfSamples, header.m_NumberOfSamples);
  EncodeInt(this->m_HeaderData->m_NumberOfProjections, header.m_NumberOfProjections);
  EncodeInt((int)(this->m_HeaderData->m_ScanDistance * 1e3), header.m_ScanDistance);
  EncodeInt((int)(this->m_HeaderData->m_ScannerType), header.m_ScannerType);
  EncodeInt((int)(this->m_HeaderData->m_SampleTime * 1e3), header.m_SampleTime);
  EncodeInt((int)(this->m_HeaderData->m_MeasurementIndex), header.m_MeasurementIndex);
  EncodeInt((int)(this->m_HeaderData->m_Site), header.m_Site);
  EncodeInt((int)(this->m_HeaderData->m_ReferenceLine * 1e3), header.m_ReferenceLine);
  EncodeInt((int)(this->m_HeaderData->m_ReconstructionAlg), header.m_ReconstructionAlg);
  PadString(header.m_PatientName, this->m_HeaderData->m_PatientName, 40);
  EncodeInt((int)(this->m_HeaderData->m_Energy * 1e3), header.m_Energy);
  EncodeInt((int)(this->m_HeaderData->m_Intensity * 1e3), header.m_Intensity);
  const std::size_t fillSize = 83 * 4;
  std::memset(header.m_Fill, 0x00, fillSize);

  EncodeInt(4, header.m_DataOffset); // We will add 4 block offset for the extended header

  outfile.write((char *)&header, ScancoHeaderBlockSize);
  // Write Extended Header
  unsigned long extendedHeaderLength = this->WriteExtendedHeader(outfile);
  bytesWritten += extendedHeaderLength + ScancoHeaderBlockSize;

  return bytesWritten;
}

void
ISQHeaderIO::ReadDateValues(const int year,
                            int       month,
                            const int day,
                            const int hour,
                            const int minute,
                            const int second,
                            const int milli)
{
  // Convert date information into a string
  month = ((month > 12 || month < 1) ? 0 : month);
  static const char * months[] = { "XXX", "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                                   "JUL", "AUG", "SEP", "OCT", "NOV", "DEC" };
  DateToString(this->m_HeaderData->m_CreationDate, year, month, day, hour, minute, second, milli);
  DateToString(this->m_HeaderData->m_ModificationDate, year, month, day, hour, minute, second, milli);
}

bool
ISQHeaderIO::ReadDimensionData(ISQEncodedPreHeader * imageData)
{
  int pixdim[3];
  int physdim[3];
  for (int i = 0; i < 3; i++)
  {
    pixdim[i] = DecodeInt(imageData->m_PixelDimensions[i]);
  }

  for (int i = 0; i < 3; i++)
  {
    physdim[i] = DecodeInt(imageData->m_PhysicalDimensions[i]);
  }

  const bool isRAD = (this->m_HeaderData->m_PixelData.m_ComponentType == 9 || physdim[2] == 0);

  // Perform a sanity check on the dimensions
  for (int i = 0; i < 3; ++i)
  {
    this->m_HeaderData->m_ScanDimensionsPixels[i] = pixdim[i];
    if (pixdim[i] < 1)
    {
      pixdim[i] = 1;
    }
    this->m_HeaderData->m_PixelData.m_Dimensions[i] = pixdim[i];

    this->m_HeaderData->m_ScanDimensionsPhysical[i] = (isRAD ? physdim[i] * 1e-6 : physdim[i] * 1e-3);
    if (physdim[i] == 0)
    {
      physdim[i] = 1.0;
    }

    this->m_HeaderData->m_PixelData.m_Spacing[i] =
      (isRAD && i == 2) ? 1.0
                        : this->m_HeaderData->m_ScanDimensionsPhysical[i] /
                            this->m_HeaderData->m_ScanDimensionsPixels[i]; // RAD doesn't have a third dimension

    this->m_HeaderData->m_PixelData.m_Origin[i] = 0.0;
  }

  return isRAD;
}

void
ISQHeaderIO::ReadRADHeader(RADEncodedHeaderBlock * headerData)
{
  this->m_HeaderData->m_MeasurementIndex = DecodeInt(headerData->m_MeasurementIndex);
  this->m_HeaderData->m_DataRange[0] = DecodeInt(headerData->m_DataMin);
  this->m_HeaderData->m_DataRange[1] = DecodeInt(headerData->m_DataMax);
  this->m_HeaderData->m_MuScaling = DecodeInt(headerData->m_MuScaling);
  StripString(this->m_HeaderData->m_PatientName, headerData->m_PatientName, 40);
  this->m_HeaderData->m_ZPosition = DecodeInt(headerData->m_ZPosition) * 1e-3;
  this->m_HeaderData->m_SampleTime = DecodeInt(headerData->m_SampleTime) * 1e-3;
  this->m_HeaderData->m_Energy = DecodeInt(headerData->m_Energy) * 1e-3;
  this->m_HeaderData->m_Intensity = DecodeInt(headerData->m_Intensity) * 1e-3;
  this->m_HeaderData->m_ReferenceLine = DecodeInt(headerData->m_ReferenceLine) * 1e-3;
  this->m_HeaderData->m_StartPosition = DecodeInt(headerData->m_StartPosition) * 1e-3;
  this->m_HeaderData->m_EndPosition = DecodeInt(headerData->m_EndPosition) * 1e-3;
  this->m_HeaderSize = (DecodeInt(headerData->m_DataOffset) + 1) * 512;
}

void
ISQHeaderIO::ReadISQHeader(ISQEncodedHeaderBlock * headerData)
{
  this->m_HeaderData->m_SliceThickness = DecodeInt(headerData->m_SliceThickness) * 1e-3;
  this->m_HeaderData->m_SliceIncrement = DecodeInt(headerData->m_SliceIncrement) * 1e-3;

  this->m_HeaderData->m_StartPosition = DecodeInt(headerData->m_StartPosition) * 1e-3;

  this->m_HeaderData->m_EndPosition =
    this->m_HeaderData->m_StartPosition + this->m_HeaderData->m_ScanDimensionsPhysical[2] *
                                            (this->m_HeaderData->m_ScanDimensionsPixels[2] - 1) /
                                            this->m_HeaderData->m_ScanDimensionsPixels[2];
  this->m_HeaderData->m_DataRange[0] = DecodeInt(headerData->m_DataMin);
  this->m_HeaderData->m_DataRange[1] = DecodeInt(headerData->m_DataMax);
  this->m_HeaderData->m_MuScaling = DecodeInt(headerData->m_MuScaling);
  this->m_HeaderData->m_NumberOfSamples = DecodeInt(headerData->m_NumberOfSamples);
  this->m_HeaderData->m_NumberOfProjections = DecodeInt(headerData->m_NumberOfProjections);
  this->m_HeaderData->m_ScanDistance = DecodeInt(headerData->m_ScanDistance) * 1e-3;
  this->m_HeaderData->m_ScannerType = DecodeInt(headerData->m_ScannerType);
  this->m_HeaderData->m_SampleTime = DecodeInt(headerData->m_SampleTime) * 1e-3;
  this->m_HeaderData->m_MeasurementIndex = DecodeInt(headerData->m_MeasurementIndex);
  this->m_HeaderData->m_Site = DecodeInt(headerData->m_Site);
  this->m_HeaderData->m_ReferenceLine = DecodeInt(headerData->m_ReferenceLine) * 1e-3;
  this->m_HeaderData->m_ReconstructionAlg = DecodeInt(headerData->m_ReconstructionAlg);
  StripString(this->m_HeaderData->m_PatientName, headerData->m_PatientName, 40);
  this->m_HeaderData->m_Energy = DecodeInt(headerData->m_Energy) * 1e-3;
  this->m_HeaderData->m_Intensity = DecodeInt(headerData->m_Intensity) * 1e-3;
  this->m_HeaderSize = (DecodeInt(headerData->m_DataOffset) + 1) * 512;

  // fix m_SliceThickness and m_SliceIncrement if they were truncated
  double computedSpacing =
    this->m_HeaderData->m_ScanDimensionsPhysical[2] / this->m_HeaderData->m_ScanDimensionsPixels[2];
  if (itk::Math::abs(computedSpacing - this->m_HeaderData->m_SliceThickness) < 1.1e-3)
  {
    this->m_HeaderData->m_SliceThickness = computedSpacing;
  }
  if (itk::Math::abs(computedSpacing - this->m_HeaderData->m_SliceIncrement) < 1.1e-3)
  {
    this->m_HeaderData->m_SliceIncrement = computedSpacing;
  }
}

void
ISQHeaderIO::ReadExtendedHeader(const char * buffer, unsigned long length)
{
  unsigned long               bytesRead = 0;
  unsigned long               hSkip = 0; // track skipped header blocks
  unsigned long               hBlocks = 0;
  ISQCalibrationHeaderBlock * calHeader = nullptr;
  int                         calHeaderSize = 0;

  if (length < ScancoHeaderBlockSize * 3)
  {
    return;
  }

  if (strncmp(buffer + 8, "MultiHeader     ", 16) == 0)
  {
    bytesRead += ScancoHeaderBlockSize; // Skip multiheader if it exists
    hSkip += 1;
  }

  for (int i = 0; i < 4; ++i)
  {
    const char * headerName = buffer + bytesRead + (i * 128) + 8;
    if (strncmp(headerName, "Calibration     ", 16) == 0)
    {
      hBlocks = DecodeInt(buffer + bytesRead + (i * 128) + 24);
      if ((hSkip + hBlocks) * ScancoHeaderBlockSize > length)
      {
        break;
      }
      calHeader = new ISQCalibrationHeaderBlock;
      memcpy((char *)calHeader, buffer + (hSkip * ScancoHeaderBlockSize), sizeof(ISQCalibrationHeaderBlock));
      calHeaderSize = hBlocks * ScancoHeaderBlockSize;
      break;
    }
    hSkip += hBlocks;
    bytesRead += hBlocks;
  }

  if (calHeader && calHeaderSize >= 1024)
  {
    // Read Calibration data from header
    StripString(this->m_HeaderData->m_CalibrationData, calHeader->m_CalibrationData, 64);
    // std::string calFile(h + 112, 256);
    // std::string s3(h + 376, 256);
    this->m_HeaderData->m_RescaleType = DecodeInt(calHeader->m_RescaleType);
    StripString(this->m_HeaderData->m_RescaleUnits, calHeader->m_RescaleUnits, 16);
    // std::string s5(h + 700, 16);
    // std::string calFilter(h + 772, 16);
    this->m_HeaderData->m_RescaleSlope = DecodeDouble(calHeader->m_RescaleSlope);
    this->m_HeaderData->m_RescaleIntercept = DecodeDouble(calHeader->m_RescaleIntercept);
    this->m_HeaderData->m_MuWater = DecodeDouble(calHeader->m_MuWater);
  }

  delete calHeader;
}

unsigned long
ISQHeaderIO::WriteExtendedHeader(std::ofstream & outfile)
{
  ISQCalibrationHeaderBlock calHeader = { 0 };
  outfile.seekp(ScancoHeaderBlockSize, std::ios::beg); // seek past first header block

  // First block adds multiheader
  char * buffer = new char[ScancoHeaderBlockSize];
  memset(buffer, 0x00, ScancoHeaderBlockSize);
  memcpy(buffer + 8, "MultiHeader     ", 16);
  outfile.write(buffer, ScancoHeaderBlockSize);
  delete[] buffer;

  // Next block adds calibration data
  memcpy(calHeader.m_Title, "Calibration     ", 16);
  PadString(calHeader.m_CalibrationData, this->m_HeaderData->m_CalibrationData, 64);
  EncodeInt(2, calHeader.m_CalHeaderSize);

  // Last block adds density scaling data:
  EncodeInt((int)this->m_HeaderData->m_RescaleType, calHeader.m_RescaleType);
  PadString(calHeader.m_RescaleUnits, this->m_HeaderData->m_RescaleUnits, 16);
  EncodeDouble(this->m_HeaderData->m_RescaleSlope, calHeader.m_RescaleSlope);
  EncodeDouble(this->m_HeaderData->m_RescaleIntercept, calHeader.m_RescaleIntercept);
  EncodeDouble(this->m_HeaderData->m_MuWater, calHeader.m_MuWater);
  outfile.write((char *)&calHeader, sizeof(ISQCalibrationHeaderBlock));

  return ScancoHeaderBlockSize + sizeof(ISQCalibrationHeaderBlock);
}

} // namespace itk
