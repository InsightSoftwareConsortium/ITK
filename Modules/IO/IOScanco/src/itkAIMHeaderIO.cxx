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
#include "itkAIMHeaderIO.h"

#include <iostream> //remove later
#include <sstream>
#include <cstring>

constexpr const char * AIM020String = "AIMDATA_V020   ";
constexpr const char * AIM030String = "AIMDATA_V030   ";

typedef char            EncodedByte;
typedef char            EncodedInt4Byte[4];
typedef char            EncodedInt8Byte[8];
typedef EncodedInt4Byte EncodedTuple4Byte[3];
typedef EncodedInt8Byte EncodedTuple8Byte[3];

struct AIMV020StructHeader
{
  EncodedInt4Byte   m_Version;
  EncodedInt4Byte   m_ProcLog;
  EncodedInt4Byte   m_Data;
  EncodedInt4Byte   m_ID;
  EncodedInt4Byte   m_Reference;
  EncodedInt4Byte   m_Type;
  EncodedTuple4Byte m_Position;
  EncodedTuple4Byte m_Dimension;
  EncodedTuple4Byte m_Offset;
  EncodedTuple4Byte m_SupDimension;
  EncodedTuple4Byte m_SupPosition;
  EncodedTuple4Byte m_SubDimension;
  EncodedTuple4Byte m_TestOffset;
  EncodedTuple4Byte m_ElementSize;
  EncodedByte       m_Fill[20];
};

struct AIMV030StructHeader
{
  EncodedInt4Byte   m_Version;
  EncodedInt4Byte   m_ID;
  EncodedInt4Byte   m_Reference;
  EncodedInt4Byte   m_Type;
  EncodedTuple8Byte m_Position;
  EncodedTuple8Byte m_Dimension;
  EncodedTuple8Byte m_Offset;
  EncodedTuple8Byte m_SupDimension;
  EncodedTuple8Byte m_SupPosition;
  EncodedTuple8Byte m_SubDimension;
  EncodedTuple8Byte m_TestOffset;
  EncodedTuple8Byte m_ElementSize;
};

struct AIMPreHeaderV020
{
  EncodedInt4Byte m_PreHeaderLength;
  EncodedInt4Byte m_ImageStructLength;
  EncodedInt4Byte m_ProcessingLogLength;
  EncodedInt4Byte m_ImageDataLength;
  EncodedInt4Byte m_AssociatedDataLength;
};

struct AIMPreHeaderV030
{
  // todo: check this is correct for 030
  EncodedInt8Byte m_PreHeaderLength;
  EncodedInt8Byte m_ImageStructLength;
  EncodedInt8Byte m_ProcessingLogLength;
  EncodedInt8Byte m_ImageDataLength;
  EncodedInt8Byte m_AssociatedDataLength;
};

namespace itk
{
unsigned long
AIMHeaderIO::ReadHeader(std::ifstream & infile)
{
  unsigned long bytesRead = 0; // Use as offset for reading the header

  if (!infile.is_open())
  {
    throw std::runtime_error("AIMHeaderIO: Input file stream is not open.");
  }

  // Populate the data structure with the raw header information
  // Initially read one block to retrieve the header size and version
  char * headerBytes = new char[ScancoHeaderBlockSize];
  infile.read(headerBytes, ScancoHeaderBlockSize);
  int versionType = CheckVersion(headerBytes);

  // The size of the data values depends on the file version
  switch (versionType)
  {
    case static_cast<int>(ScancoFileVersions::AIM_020):
      this->m_IntSize = 4; // AIM v020 uses 32-bit [4 byte] integers
      strcpy(this->m_HeaderData->m_Version, AIM020String);
      break;

    case static_cast<int>(ScancoFileVersions::AIM_030):
      this->m_IntSize = 8; // AIM v030 uses 64-bit [8 byte] integers
      strcpy(this->m_HeaderData->m_Version, AIM030String);
      bytesRead += 16; // Skip the version string
      break;

    default:
      throw std::runtime_error("AIMHeaderIO: Unrecognized file version.");
      break;
  }

  // todo: @ebald19 check offset is still right for AIMV030

  // Read the pre-header to get remaining header and log size
  this->m_PreHeaderSize = DecodeInt(headerBytes + bytesRead);
  this->ReadPreHeader(infile, this->m_PreHeaderSize);
  bytesRead += this->m_PreHeaderSize;

  std::cout << "DEBUG: struct header size read" << this->m_ImgStructSize << std::endl;

  unsigned long headerSize = this->m_PreHeaderSize + this->m_ImgStructSize + this->m_ProcessingLogSize;

  if (headerSize > ScancoHeaderBlockSize)
  {
    // Allocate more space for the header and read the rest into the raw header
    delete[] headerBytes;
    delete[] this->m_HeaderData->m_RawHeader;
    headerBytes = new char[headerSize];
    // headerSize does not include the version string from v030
    infile.seekg(versionType == static_cast<int>(ScancoFileVersions::AIM_030) ? 16 : 0, std::ios::beg);
    infile.read(headerBytes, headerSize);
  }
  // we have read the full header, save to our data structure
  this->m_HeaderData->m_RawHeader = headerBytes;

  // Read the image structure header (version-dependent)
  switch (versionType)
  {
    case static_cast<int>(ScancoFileVersions::AIM_020):
    {
      AIMV020StructHeader * structHeader = new AIMV020StructHeader;
      memcpy((char *)structHeader, headerBytes + bytesRead, this->m_ImgStructSize);
      this->ReadImgStructHeader(structHeader);
      delete structHeader;
      break;
    }
    case static_cast<int>(ScancoFileVersions::AIM_030):
    {
      AIMV030StructHeader * structHeader = new AIMV030StructHeader;
      memcpy((char *)structHeader, headerBytes + bytesRead, this->m_ImgStructSize);
      this->ReadImgStructHeader(structHeader);
      delete structHeader;
      break;
    }
    default:
      throw std::runtime_error("Error: invalid AIM type.");
      break;
  }
  bytesRead += this->m_ImgStructSize;

  // Read the processing log
  this->ReadProcessingLog(infile, bytesRead, this->m_ProcessingLogSize);
  bytesRead += this->m_ProcessingLogSize;

  // these items are not in the processing log
  this->m_HeaderData->m_SliceThickness = this->m_HeaderData->m_PixelData.m_Spacing[2];
  this->m_HeaderData->m_SliceIncrement = this->m_HeaderData->m_PixelData.m_Spacing[2];

  return headerSize; // Return the size of the header
  // todo: @ebald19 see if adding version length is necessary
}

unsigned long
AIMHeaderIO::WriteHeader(std::ofstream & outfile, unsigned long imageSize)
{

  // todo: take image data length and add in pre-header if different than original
  // This just prints original header
  int bytesWritten = 0;
  if (!outfile.is_open())
  {
    throw std::runtime_error("AIMHeaderIO: Output file stream is not open.");
  }

  outfile.seekp(0, std::ios::beg);

  std::string processingLog = this->WriteProcessingLog();
  this->m_ProcessingLogSize = processingLog.length();

  // Write the version string if AIM v030
  if (!strcmp(this->m_HeaderData->m_Version, AIM030String))
  {
    outfile.write(AIM030String, 16);
    bytesWritten += 16;
    AIMV030StructHeader structHeader = this->WriteStructHeaderV030();
    this->m_ImgStructSize = sizeof(structHeader);
    this->m_PreHeaderSize = this->WritePreHeader(outfile, imageSize, ScancoFileVersions::AIM_030);
    outfile.write((char *)&structHeader, this->m_ImgStructSize);
  }
  else
  {
    AIMV020StructHeader structHeader = this->WriteStructHeaderV020();
    this->m_ImgStructSize = sizeof(structHeader);
    std::cout << "DEBUG: struct header size written" << sizeof(structHeader) << std::endl;
    this->m_PreHeaderSize = this->WritePreHeader(outfile, imageSize, ScancoFileVersions::AIM_020);
    outfile.write((char *)&structHeader, this->m_ImgStructSize);
  }

  outfile.write(processingLog.c_str(), this->m_ProcessingLogSize);

  if (outfile.tellp() != bytesWritten + this->m_PreHeaderSize + this->m_ImgStructSize + this->m_ProcessingLogSize)
  {
    throw std::runtime_error("Error: write size mismatch");
  }

  return bytesWritten + this->m_PreHeaderSize + this->m_ImgStructSize + this->m_ProcessingLogSize;
}

int
AIMHeaderIO::ReadPreHeader(std::ifstream & file, unsigned long length, unsigned long offset)
{
  unsigned long bytesRead = this->m_IntSize; // We assume the pre-header length (int) has already been read

  if (!file.is_open())
  {
    return -1;
  }

  if (length == 0)
  {
    return -1; // No pre-header to read
  }

  // Seek to the offset to skip the version string if necessary
  file.seekg(offset, std::ios::beg);

  char * preHeader = new char[length];
  file.read(preHeader, length);

  this->m_ImgStructSize = DecodeInt(preHeader + bytesRead);
  bytesRead += this->m_IntSize;
  this->m_ProcessingLogSize = DecodeInt(preHeader + bytesRead);
  bytesRead += this->m_IntSize;

  // these items are not in the processing log
  this->m_HeaderData->m_SliceThickness = this->m_HeaderData->m_PixelData.m_Spacing[2];
  this->m_HeaderData->m_SliceIncrement = this->m_HeaderData->m_PixelData.m_Spacing[2];

  delete[] preHeader;
  return 0; // Success
}

void
AIMHeaderIO::ReadImgStructHeader(AIMV020StructHeader * headerData)
{

  this->m_HeaderData->m_PixelData.m_ComponentType = DecodeInt(headerData->m_Type);

  int i = 0;
  for (int & imageDimension : this->m_HeaderData->m_PixelData.m_Dimensions)
  {
    imageDimension = DecodeInt(headerData->m_Dimension[i]);
    i++;
  }

  // Element spacing is stored as a float
  i = 0;
  for (double & spacing : this->m_HeaderData->m_PixelData.m_Spacing)
  {
    spacing = DecodeFloat(headerData->m_ElementSize[i]);
    if (spacing == 0)
    {
      spacing = 1.0;
    }
    i++;
  }

  // Set the pixel data origin
  for (int i = 0; i < 3; ++i)
  {
    this->m_HeaderData->m_PixelData.m_Origin[i] =
      DecodeInt(headerData->m_Position[i]) * this->m_HeaderData->m_PixelData.m_Spacing[i];
  }
}

void
AIMHeaderIO::ReadImgStructHeader(AIMV030StructHeader * headerData)
{
  this->m_HeaderData->m_PixelData.m_ComponentType = DecodeInt(headerData->m_Type);

  int i = 0;
  for (int & imageDimension : this->m_HeaderData->m_PixelData.m_Dimensions)
  {
    imageDimension = DecodeInt(headerData->m_Dimension[i]);
    i++;
  }

  // element spacing is a 64-bit integer
  i = 0;
  for (double & spacing : this->m_HeaderData->m_PixelData.m_Spacing)
  {
    spacing = 1e-6 * DecodeInt(headerData->m_ElementSize[i]);
    if (spacing == 0)
    {
      spacing = 1.0;
    }
    i++;
  }

  // Set the pixel data origin
  for (int i = 0; i < 3; ++i)
  {
    this->m_HeaderData->m_PixelData.m_Origin[i] =
      DecodeInt(headerData->m_Position[i]) * this->m_HeaderData->m_PixelData.m_Spacing[i];
  }
}

int
AIMHeaderIO::ReadProcessingLog(std::ifstream & infile, unsigned long offset, unsigned long length)
{
  int         bytesRead = 0;
  std::string readString = "";

  if (length == 0)
  {
    return -1; // No image structure header to read
  }

  infile.seekg(offset, std::ios::beg);

  while (bytesRead < length)
  {
    getline(infile, readString);

    if (infile.eof() || infile.fail() || infile.bad())
    {
      return -1; // Error reading the file
    }

    bytesRead += readString.length() + 1; // +1 for the newline character

    if (readString[0] == '!')
    {
      // Skip comment lines
      continue;
    }

    // Assume keys are separated by (at least) two spaces
    std::string key = readString.substr(0, readString.find("  "));

    std::string value = readString.substr(readString.find("  ") + 2);

    value = value.substr(value.find_first_not_of(" "),
                         value.find_last_not_of(' ') - value.find_first_not_of(" ") + 1); // Trim trailing spaces

    // std::cout << "key: " << key << "value: " << value << std::endl;

    // check for known keys
    if (key == "Time")
    {
      strncpy(this->m_HeaderData->m_ModificationDate, value.c_str(), value.length() + 1);
    }
    else if (key == "Original Creation-Date")
    {
      strncpy(this->m_HeaderData->m_CreationDate, value.c_str(), value.length() + 1);
    }
    else if (key == "Orig-ISQ-Dim-p")
    {
      for (int & ScanDimensionsPixel : this->m_HeaderData->m_ScanDimensionsPixels)
      {
        ScanDimensionsPixel = std::stol(value);
      }
    }
    else if (key == "Orig-ISQ-Dim-um")
    {
      for (double & i : this->m_HeaderData->m_ScanDimensionsPhysical)
      {
        i = stod(value) * 1e-3;
      }
    }
    else if (key == "Patient Name")
    {
      strncpy(this->m_HeaderData->m_PatientName, value.c_str(), value.length() + 1);
    }
    else if (key == "Index Patient")
    {
      this->m_HeaderData->m_PatientIndex = stol(value);
    }
    else if (key == "Index Measurement")
    {
      this->m_HeaderData->m_MeasurementIndex = stol(value);
    }
    else if (key == "Site")
    {
      this->m_HeaderData->m_Site = stol(value);
    }
    else if (key == "Scanner ID")
    {
      this->m_HeaderData->m_ScannerID = stol(value);
    }
    else if (key == "Scanner type")
    {
      this->m_HeaderData->m_ScannerType = stol(value);
    }
    else if (key == "Position Slice 1 [um]")
    {
      this->m_HeaderData->m_StartPosition = stod(value) * 1e-3;
      this->m_HeaderData->m_EndPosition =
        this->m_HeaderData->m_StartPosition +
        this->m_HeaderData->m_PixelData.m_Spacing[2] * (this->m_HeaderData->m_PixelData.m_Dimensions[2] - 1);
    }
    else if (key == "No. samples")
    {
      this->m_HeaderData->m_NumberOfSamples = stol(value);
    }
    else if (key == "No. projections per 180")
    {
      this->m_HeaderData->m_NumberOfProjections = stol(value);
    }
    else if (key == "Scan Distance [um]")
    {
      this->m_HeaderData->m_ScanDistance = stod(value) * 1e-3;
    }
    else if (key == "Integration time [us]")
    {
      this->m_HeaderData->m_SampleTime = stod(value) * 1e-3;
    }
    else if (key == "Reference line [um]")
    {
      this->m_HeaderData->m_ReferenceLine = stod(value) * 1e-3;
    }
    else if (key == "Reconstruction-Alg.")
    {
      this->m_HeaderData->m_ReconstructionAlg = stol(value);
    }
    else if (key == "Energy [V]")
    {
      this->m_HeaderData->m_Energy = stod(value) * 1e-3;
    }
    else if (key == "Intensity [uA]")
    {
      this->m_HeaderData->m_Intensity = stod(value) * 1e-3;
    }
    else if (key == "Mu_Scaling")
    {
      this->m_HeaderData->m_MuScaling = stol(value);
    }
    else if (key == "Minimum data value")
    {
      this->m_HeaderData->m_DataRange[0] = stod(value);
    }
    else if (key == "Maximum data value")
    {
      this->m_HeaderData->m_DataRange[1] = stod(value);
    }
    else if (key == "Calib. default unit type")
    {
      this->m_HeaderData->m_RescaleType = stol(value);
    }
    else if (key == "Calibration Data")
    {
      strncpy(this->m_HeaderData->m_CalibrationData, value.c_str(), value.length() + 1);
    }
    else if (key == "Density: unit")
    {
      strncpy(this->m_HeaderData->m_RescaleUnits, value.c_str(), value.length() + 1);
    }
    else if (key == "Density: slope")
    {
      this->m_HeaderData->m_RescaleSlope = stod(value);
    }
    else if (key == "Density: intercept")
    {
      this->m_HeaderData->m_RescaleIntercept = stod(value);
    }
    else if (key == "HU: mu water")
    {
      this->m_HeaderData->m_MuWater = stod(value);
    }
  }
  return bytesRead;
}

size_t
AIMHeaderIO::WritePreHeader(std::ofstream & outfile, size_t imageSize, ScancoFileVersions version)
{
  if (!outfile.is_open())
  {
    return 0;
  }

  if (version == ScancoFileVersions::AIM_020)
  {
    AIMPreHeaderV020 preHeader{ 0 };
    EncodeInt(sizeof(AIMPreHeaderV020), preHeader.m_PreHeaderLength);
    EncodeInt(this->m_ImgStructSize, preHeader.m_ImageStructLength);
    EncodeInt(this->m_ProcessingLogSize, preHeader.m_ProcessingLogLength);
    EncodeInt(imageSize, preHeader.m_ImageDataLength);
    EncodeInt(0, preHeader.m_AssociatedDataLength); // No associated data handling
    outfile.write((char *)&preHeader, sizeof(preHeader));
    return sizeof(preHeader);
  }
  else if (version == ScancoFileVersions::AIM_030)
  {
    AIMPreHeaderV030 preHeader{ 0 };
    EncodeInt64(sizeof(AIMPreHeaderV030), preHeader.m_PreHeaderLength);
    EncodeInt64(sizeof(AIMPreHeaderV020), preHeader.m_PreHeaderLength);
    EncodeInt64(this->m_ImgStructSize, preHeader.m_ImageStructLength);
    EncodeInt64(this->m_ProcessingLogSize, preHeader.m_ProcessingLogLength);
    EncodeInt64(imageSize, preHeader.m_ImageDataLength);
    EncodeInt64(0, preHeader.m_AssociatedDataLength); // No associated data handling
    outfile.write((char *)&preHeader, sizeof(preHeader));
    return sizeof(preHeader);
  }
  else
  {
    throw std::runtime_error("AIMHeaderIO::WritePreHeader: Invalid AIM file version to write.");
  }
}

AIMV020StructHeader
AIMHeaderIO::WriteStructHeaderV020()
{
  AIMV020StructHeader structHeader{ 0 };
  EncodeInt(this->m_HeaderData->m_PixelData.m_ComponentType, structHeader.m_Type);

  EncodeFloat(1.6, structHeader.m_Version);

  int i = 0;
  for (int & imageDimension : this->m_HeaderData->m_PixelData.m_Dimensions)
  {
    EncodeInt(imageDimension, structHeader.m_Dimension[i]);
    i++;
  }

  // Element spacing is stored as a float
  i = 0;
  for (double & spacing : this->m_HeaderData->m_PixelData.m_Spacing)
  {
    EncodeFloat(spacing, structHeader.m_ElementSize[i]);
    i++;
  }

  // pixel data origin
  for (int i = 0; i < 3; ++i)
  {
    EncodeInt(this->m_HeaderData->m_PixelData.m_Origin[i] / this->m_HeaderData->m_PixelData.m_Spacing[i],
              structHeader.m_Position[i]);
  }
  return structHeader;
}

AIMV030StructHeader
AIMHeaderIO::WriteStructHeaderV030()
{
  AIMV030StructHeader structHeader{ 0 };
  EncodeInt(this->m_HeaderData->m_PixelData.m_ComponentType, structHeader.m_Type);

  int i = 0;
  for (int & imageDimension : this->m_HeaderData->m_PixelData.m_Dimensions)
  {
    EncodeInt64((int64_t)imageDimension, structHeader.m_Dimension[i]);
    i++;
  }

  // element spacing is a 64-bit integer
  i = 0;
  for (double & spacing : this->m_HeaderData->m_PixelData.m_Spacing)
  {
    EncodeInt64(spacing * 1e6, structHeader.m_ElementSize[i]);
    i++;
  }

  // Set the pixel data origin
  for (int i = 0; i < 3; ++i)
  {
    EncodeInt64(this->m_HeaderData->m_PixelData.m_Origin[i] / this->m_HeaderData->m_PixelData.m_Spacing[i],
                structHeader.m_Position[i]);
  }

  return structHeader;
}

std::string
AIMHeaderIO::WriteProcessingLog()
{
  std::ostringstream outLog{ "" };
  GetCurrentDateString(this->m_HeaderData->m_ModificationDate);

  outLog << "! " << std::endl;
  outLog << "! Processing Log " << std::endl;
  outLog << "!" << std::endl;
  outLog << "!-------------------------------------------------------------------------------" << std::endl;
  outLog << "Created by                    ITKIOScanco" << std::endl;
  outLog << "Time                          " << this->m_HeaderData->m_ModificationDate << std::endl;
  outLog << "Original Creation-Date        " << this->m_HeaderData->m_CreationDate << std::endl;
  outLog << "Orig-ISQ-Dim-p                                   " << this->m_HeaderData->m_ScanDimensionsPixels[0]
         << "       " << this->m_HeaderData->m_ScanDimensionsPixels[1] << "        "
         << this->m_HeaderData->m_ScanDimensionsPixels[2] << std::endl;
  outLog << "Orig-ISQ-Dim-um                                  " << this->m_HeaderData->m_ScanDimensionsPhysical[0] * 1e3
         << "       " << this->m_HeaderData->m_ScanDimensionsPhysical[1] * 1e3 << "        "
         << this->m_HeaderData->m_ScanDimensionsPhysical[2] * 1e3 << std::endl;
  outLog << "!-------------------------------------------------------------------------------" << std::endl;
  outLog << "Patient Name                  " << this->m_HeaderData->m_PatientName << std::endl;
  outLog << "Index Patient                                    " << this->m_HeaderData->m_PatientIndex << std::endl;
  outLog << "Index Measurement                               " << this->m_HeaderData->m_MeasurementIndex << std::endl;
  outLog << "!-------------------------------------------------------------------------------" << std::endl;
  outLog << "Site                                                " << this->m_HeaderData->m_Site << std::endl;
  outLog << "Scanner ID                                       " << this->m_HeaderData->m_ScannerID << std::endl;
  outLog << "Scanner type                                        " << this->m_HeaderData->m_ScannerType << std::endl;
  outLog << "Position Slice 1 [um]                          " << this->m_HeaderData->m_StartPosition * 1e3 << std::endl;
  outLog << "No. samples                                      " << this->m_HeaderData->m_NumberOfSamples << std::endl;
  outLog << "No. projections per 180                           " << this->m_HeaderData->m_NumberOfProjections
         << std::endl;
  outLog << "Scan Distance [um]                             " << this->m_HeaderData->m_ScanDistance * 1e3 << std::endl;
  outLog << "Integration time [us]                          " << this->m_HeaderData->m_SampleTime * 1e3 << std::endl;
  outLog << "Reference line [um]                                 " << this->m_HeaderData->m_ReferenceLine * 1e3
         << std::endl;
  outLog << "Reconstruction-Alg.                                 " << this->m_HeaderData->m_ReconstructionAlg
         << std::endl;
  outLog << "Energy [V]                                      " << this->m_HeaderData->m_Energy * 1e3 << std::endl;
  outLog << "Intensity [uA]                                   " << this->m_HeaderData->m_Intensity * 1e3 << std::endl;
  outLog << "!-------------------------------------------------------------------------------" << std::endl;
  outLog << "Mu_Scaling                                       " << this->m_HeaderData->m_MuScaling << std::endl;
  outLog << "Calibration Data              " << this->m_HeaderData->m_CalibrationData << "           " << std::endl;
  outLog << "Calib. default unit type      " << this->m_HeaderData->m_RescaleType
         << "                                      " << std::endl;
  outLog << "Density: unit                 " << this->m_HeaderData->m_RescaleUnits
         << "                                         " << std::endl;
  outLog << "Density: slope                         " << this->m_HeaderData->m_RescaleSlope << std::endl;
  outLog << "Density: intercept                     " << this->m_HeaderData->m_RescaleIntercept << std::endl;
  outLog << "HU: mu water                                  " << this->m_HeaderData->m_MuWater << std::endl;
  outLog << "!-------------------------------------------------------------------------------" << std::endl;
  outLog << "Minimum data value                            " << this->m_HeaderData->m_DataRange[0] << std::endl;
  outLog << "Maximum data value                            " << this->m_HeaderData->m_DataRange[1] << std::endl;

  return outLog.str();
}

AIMHeaderIO::~AIMHeaderIO() {}

} // namespace itk
