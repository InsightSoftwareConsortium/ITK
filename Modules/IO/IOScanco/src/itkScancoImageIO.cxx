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
/*=========================================================================

  Program: DICOM for VTK

  Copyright (c) 2015 David Gobbi
  All rights reserved.
  See Copyright.txt or http://dgobbi.github.io/bsd3.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/

#include "itkScancoImageIO.h"
#include "itkSpatialOrientationAdapter.h"
#include "itkIOCommon.h"
#include "itksys/SystemTools.hxx"
#include "itkMath.h"
#include "itkIntTypes.h"
#include "itkByteSwapper.h"
#include "itkMetaDataObject.h"
#include "itkScancoDataManipulation.h"

#include <algorithm>
#include <ctime>

namespace itk
{

ScancoImageIO::ScancoImageIO()

{
  this->m_FileType = IOFileEnum::Binary;
  this->m_ByteOrder = IOByteOrderEnum::LittleEndian;

  this->AddSupportedWriteExtension(".isq");

  this->AddSupportedReadExtension(".isq");
  this->AddSupportedReadExtension(".rsq");
  this->AddSupportedReadExtension(".rad");
  this->AddSupportedReadExtension(".aim");

  this->m_RawHeader = nullptr;
}


ScancoImageIO::~ScancoImageIO() { delete[] this->m_RawHeader; }


void
ScancoImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

bool
ScancoImageIO::CanReadFile(const char * filename)
{
  try
  {
    std::ifstream infile;
    this->OpenFileForReading(infile, filename);

    bool canRead = false;
    if (infile.good())
    {
      // header is a 512 byte block
      char buffer[512];
      infile.read(buffer, 512);
      if (!infile.bad())
      {
        int fileType = CheckVersion(buffer);
        canRead = (fileType > 0);
      }
    }

    infile.close();

    return canRead;
  }
  catch (...) // file cannot be opened, access denied etc.
  {
    return false;
  }
}


void
ScancoImageIO::InitializeHeader()
{
  memset(this->m_Version, 0, 18);
  memset(this->m_PatientName, 0, 42);
  this->m_PatientIndex = 0;
  this->m_ScannerID = 0;
  memset(this->m_CreationDate, 0, 32);
  memset(this->m_ModificationDate, 0, 32);
  this->ScanDimensionsPixels[0] = 0;
  this->ScanDimensionsPixels[1] = 0;
  this->ScanDimensionsPixels[2] = 0;
  this->ScanDimensionsPhysical[0] = 0;
  this->ScanDimensionsPhysical[1] = 0;
  this->ScanDimensionsPhysical[2] = 0;
  this->m_SliceThickness = 0;
  this->m_SliceIncrement = 0;
  this->m_StartPosition = 0;
  this->m_EndPosition = 0;
  this->m_ZPosition = 0;
  this->m_DataRange[0] = 0;
  this->m_DataRange[1] = 0;
  this->m_MuScaling = 1.0;
  this->m_NumberOfSamples = 0;
  this->m_NumberOfProjections = 0;
  this->m_ScanDistance = 0;
  this->m_SampleTime = 0;
  this->m_ScannerType = 0;
  this->m_MeasurementIndex = 0;
  this->m_Site = 0;
  this->m_ReconstructionAlg = 0;
  this->m_ReferenceLine = 0;
  this->m_Energy = 0;
  this->m_Intensity = 0;

  this->m_RescaleType = 0;
  memset(this->m_RescaleUnits, 0, 18);
  memset(this->m_CalibrationData, 0, 66);
  this->m_RescaleSlope = 1.0;
  this->m_RescaleIntercept = 0.0;
  this->m_MuWater = 0.70329999923706055;

  this->m_Compression = 0;
  this->m_HeaderInitialized = true;
}

int
ScancoImageIO::ReadISQHeader(std::ifstream * file, unsigned long bytesRead)
{
  if (bytesRead < 512)
  {
    return 0;
  }

  char * h = this->m_RawHeader;
  StripString(this->m_Version, h, 16);
  h += 16;
  int dataType = DecodeInt(h);
  h += 4;
  const int numBytes = DecodeInt(h);
  h += 4;
  (void)numBytes;
  const int numBlocks = DecodeInt(h);
  h += 4;
  (void)numBlocks;
  this->m_PatientIndex = DecodeInt(h);
  h += 4;
  this->m_ScannerID = DecodeInt(h);
  h += 4;
  int year, month, day, hour, minute, second, milli;
  DecodeDate(h, year, month, day, hour, minute, second, milli);
  h += 8;
  int pixdim[3], physdim[3];
  pixdim[0] = DecodeInt(h);
  h += 4;
  pixdim[1] = DecodeInt(h);
  h += 4;
  pixdim[2] = DecodeInt(h);
  h += 4;
  physdim[0] = DecodeInt(h);
  h += 4;
  physdim[1] = DecodeInt(h);
  h += 4;
  physdim[2] = DecodeInt(h);
  h += 4;

  const bool isRAD = (dataType == 9 || physdim[2] == 0);

  if (isRAD) // RAD file
  {
    this->m_MeasurementIndex = DecodeInt(h);
    h += 4;
    this->m_DataRange[0] = DecodeInt(h);
    h += 4;
    this->m_DataRange[1] = DecodeInt(h);
    h += 4;
    this->m_MuScaling = DecodeInt(h);
    h += 4;
    StripString(this->m_PatientName, h, 40);
    h += 40;
    this->m_ZPosition = DecodeInt(h) * 1e-3;
    h += 4;
    /* unknown */ h += 4;
    this->m_SampleTime = DecodeInt(h) * 1e-3;
    h += 4;
    this->m_Energy = DecodeInt(h) * 1e-3;
    h += 4;
    this->m_Intensity = DecodeInt(h) * 1e-3;
    h += 4;
    this->m_ReferenceLine = DecodeInt(h) * 1e-3;
    h += 4;
    this->m_StartPosition = DecodeInt(h) * 1e-3;
    h += 4;
    this->m_EndPosition = DecodeInt(h) * 1e-3;
    h += 4;
    h += 88 * 4;
  }
  else // ISQ file or RSQ file
  {
    this->m_SliceThickness = DecodeInt(h) * 1e-3;
    h += 4;
    this->m_SliceIncrement = DecodeInt(h) * 1e-3;
    h += 4;
    this->m_StartPosition = DecodeInt(h) * 1e-3;
    h += 4;
    this->m_EndPosition = this->m_StartPosition + physdim[2] * 1e-3 * (pixdim[2] - 1) / pixdim[2];
    this->m_DataRange[0] = DecodeInt(h);
    h += 4;
    this->m_DataRange[1] = DecodeInt(h);
    h += 4;
    this->m_MuScaling = DecodeInt(h);
    h += 4;
    this->m_NumberOfSamples = DecodeInt(h);
    h += 4;
    this->m_NumberOfProjections = DecodeInt(h);
    h += 4;
    this->m_ScanDistance = DecodeInt(h) * 1e-3;
    h += 4;
    this->m_ScannerType = DecodeInt(h);
    h += 4;
    this->m_SampleTime = DecodeInt(h) * 1e-3;
    h += 4;
    this->m_MeasurementIndex = DecodeInt(h);
    h += 4;
    this->m_Site = DecodeInt(h);
    h += 4;
    this->m_ReferenceLine = DecodeInt(h) * 1e-3;
    h += 4;
    this->m_ReconstructionAlg = DecodeInt(h);
    h += 4;
    StripString(this->m_PatientName, h, 40);
    h += 40;
    this->m_Energy = DecodeInt(h) * 1e-3;
    h += 4;
    this->m_Intensity = DecodeInt(h) * 1e-3;
    h += 4;
    h += 83 * 4;
  }

  int dataOffset = DecodeInt(h);

  // fix m_SliceThickness and m_SliceIncrement if they were truncated
  if (physdim[2] != 0)
  {
    double computedSpacing = physdim[2] * 1e-3 / pixdim[2];
    if (itk::Math::abs(computedSpacing - this->m_SliceThickness) < 1.1e-3)
    {
      this->m_SliceThickness = computedSpacing;
    }
    if (itk::Math::abs(computedSpacing - this->m_SliceIncrement) < 1.1e-3)
    {
      this->m_SliceIncrement = computedSpacing;
    }
  }

  // Convert date information into a string
  month = ((month > 12 || month < 1) ? 0 : month);
  static const char * months[] = { "XXX", "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                                   "JUL", "AUG", "SEP", "OCT", "NOV", "DEC" };
  snprintf(this->m_CreationDate,
           32,
           "%d-%s-%d %02d:%02d:%02d.%03d",
           (day % 100),
           months[month],
           (year % 10000),
           (hour % 100),
           (minute % 100),
           (second % 100),
           (milli % 1000));
  snprintf(this->m_ModificationDate,
           32,
           "%d-%s-%d %02d:%02d:%02d.%03d",
           (day % 100),
           months[month],
           (year % 10000),
           (hour % 100),
           (minute % 100),
           (second % 100),
           (milli % 1000));

  // Perform a sanity check on the dimensions
  for (int i = 0; i < 3; ++i)
  {
    this->ScanDimensionsPixels[i] = pixdim[i];
    if (pixdim[i] < 1)
    {
      pixdim[i] = 1;
    }
    this->ScanDimensionsPhysical[i] = (isRAD ? physdim[i] * 1e-6 : physdim[i] * 1e-3);
    if (physdim[i] == 0)
    {
      physdim[i] = 1.0;
    }
  }

  this->SetNumberOfDimensions(3);
  for (unsigned int i = 0; i < m_NumberOfDimensions; ++i)
  {
    this->SetDimensions(i, pixdim[i]);
    if (isRAD) // RAD file
    {
      if (i == 2)
      {
        this->SetSpacing(i, 1.0);
      }
      else
      {
        this->SetSpacing(i, physdim[i] * 1e-6 / pixdim[i]);
      }
    }
    else
    {
      this->SetSpacing(i, physdim[i] * 1e-3 / pixdim[i]);
    }
    this->SetOrigin(i, 0.0);
  }

  this->SetPixelType(IOPixelEnum::SCALAR);
  this->SetComponentType(IOComponentEnum::SHORT);

  // total header size
  const SizeValueType headerSize = static_cast<SizeValueType>(dataOffset + 1) * 512;
  this->m_HeaderSize = headerSize;

  // read the rest of the header
  if (headerSize > bytesRead)
  {
    h = new char[headerSize];
    memcpy(h, this->m_RawHeader, bytesRead);
    delete[] this->m_RawHeader;
    this->m_RawHeader = h;
    file->read(h + bytesRead, headerSize - bytesRead);
    if (static_cast<unsigned long>(file->gcount()) < headerSize - bytesRead)
    {
      return 0;
    }
  }

  // decode the extended header (lots of guesswork)
  if (headerSize >= 2048)
  {
    char * calHeader = nullptr;
    int    calHeaderSize = 0;
    h = this->m_RawHeader + 512;
    unsigned long hskip = 1;
    char *        headerName = h + 8;
    if (strncmp(headerName, "MultiHeader     ", 16) == 0)
    {
      h += 512;
      hskip += 1;
    }
    unsigned long hsize = 0;
    for (int i = 0; i < 4; ++i)
    {
      hsize = DecodeInt(h + i * 128 + 24);
      if ((1 + hskip + hsize) * 512 > headerSize)
      {
        break;
      }
      headerName = h + i * 128 + 8;
      if (strncmp(headerName, "Calibration     ", 16) == 0)
      {
        calHeader = this->m_RawHeader + (1 + hskip) * 512;
        calHeaderSize = hsize * 512;
      }
      hskip += hsize;
    }

    if (calHeader && calHeaderSize >= 1024)
    {
      h = calHeader;
      StripString(this->m_CalibrationData, h + 28, 64);
      // std::string calFile(h + 112, 256);
      // std::string s3(h + 376, 256);
      this->m_RescaleType = DecodeInt(h + 632);
      StripString(this->m_RescaleUnits, h + 648, 16);
      // std::string s5(h + 700, 16);
      // std::string calFilter(h + 772, 16);
      this->m_RescaleSlope = DecodeDouble(h + 664);
      this->m_RescaleIntercept = DecodeDouble(h + 672);
      this->m_MuWater = DecodeDouble(h + 688);
    }
  }

  return 1;
}


int
ScancoImageIO::ReadAIMHeader(std::ifstream * file, unsigned long bytesRead)
{
  if (bytesRead < 160)
  {
    return 0;
  }

  char *        h = this->m_RawHeader;
  int           intSize = 0;
  unsigned long headerSize = 0;

  // True for AIM v030, False for AIM v020
  bool versionV030 = !strcmp(h, "AIMDATA_V030   ");

  if (versionV030)
  {
    // All header data is saved as 64-bit ints (8 bytes), except for the datatype which is a 32-bit int
    // AIMDATA_V030 data has 16-bits (2 bytes) extra at the front of the header for a string containing "AIMDATA_V030"
    intSize = 8;
    strcpy(this->m_Version, h);
    headerSize = 16;
    h += headerSize;
  }
  else
  {
    // All header data is saved as 32-bit ints (4 bytes), except for element size which is saved as a float
    intSize = 4;
    strcpy(this->m_Version, "AIMDATA_V020   ");
  }

  // Read the pre-header
  // AIM header is divided into 3 sections: a preheader, a struct containing volume info, and a processing log
  char * preheader = h;
  int    preheaderSize = DecodeInt(h);
  h += intSize;
  int structSize = DecodeInt(h);
  h += intSize;
  int logSize = DecodeInt(h);
  h += intSize;

  // read the rest of the header
  headerSize += preheaderSize + structSize + logSize;
  this->m_HeaderSize = headerSize;

  if (headerSize > bytesRead)
  {
    h = new char[headerSize];
    memcpy(h, this->m_RawHeader, bytesRead);
    preheader = h + (preheader - this->m_RawHeader);
    delete[] this->m_RawHeader;
    this->m_RawHeader = h;
    file->read(h + bytesRead, headerSize - bytesRead);
    if (static_cast<unsigned long>(file->gcount()) < headerSize - bytesRead)
    {
      return 0;
    }
  }

  // decode the struct header
  h = preheader + preheaderSize;

  // The datatype value is stored as a 4 byte block for both AIM v020 and v030.
  // There are some extra bytes before the datatype:
  // 3 4-byte blocks for AIMDATA_V030
  // 5 4-byte blocks for AIMDATA_V020
  if (versionV030)
  {
    h += 12;
  }
  else
  {
    h += 20;
  }

  int dataType = DecodeInt(h);
  h += 4;

  // The struct contains the following data:
  // structValues[0] to structValues[2] = image position
  // structValues[3] to structValues[5] = image dimensions
  // structValues[6] to structValues[21] = 15 blocks of zeros
  int structValues[21];
  for (int & structValue : structValues)
  {
    structValue = DecodeInt(h);
    h += intSize;
  }

  // Element Size is float in AIMDATA_V020, but 64-bit int in AIMDATA_V030
  float elementSize[3];
  if (versionV030)
  {
    // AIMDATA_V030
    for (float & i : elementSize)
    {
      i = 1e-6 * DecodeInt(h);
      if (i == 0)
      {
        i = 1.0;
      }
      h += intSize;
    }
  }
  else
  {
    // AIMDATA_V020
    for (float & i : elementSize)
    {
      i = DecodeFloat(h);
      if (i == 0)
      {
        i = 1.0;
      }
      h += intSize;
    }
  }

  // number of components per pixel is 1 by default
  this->SetPixelType(IOPixelEnum::SCALAR);
  this->m_Compression = 0;

  // a limited selection of data types are supported
  // (only 0x00010001 (char) and 0x00020002 (short) are fully tested)
  switch (dataType)
  {
    case 0x00160001:
      this->SetComponentType(IOComponentEnum::UCHAR);
      break;
    case 0x000d0001:
      this->SetComponentType(IOComponentEnum::UCHAR);
      break;
    case 0x00120003:
      this->SetComponentType(IOComponentEnum::UCHAR);
      this->SetPixelType(IOPixelEnum::VECTOR);
      this->SetNumberOfDimensions(3);
      break;
    case 0x00010001:
      this->SetComponentType(IOComponentEnum::CHAR);
      break;
    case 0x00060003:
      this->SetComponentType(IOComponentEnum::CHAR);
      this->SetPixelType(IOPixelEnum::VECTOR);
      this->SetNumberOfDimensions(3);
      break;
    case 0x00170002:
      this->SetComponentType(IOComponentEnum::USHORT);
      break;
    case 0x00020002:
      this->SetComponentType(IOComponentEnum::SHORT);
      break;
    case 0x00030004:
      this->SetComponentType(IOComponentEnum::INT);
      break;
    case 0x001a0004:
      this->SetComponentType(IOComponentEnum::FLOAT);
      break;
    case 0x00150001:
      this->m_Compression = 0x00b2; // run-length compressed bits
      this->SetComponentType(IOComponentEnum::CHAR);
      break;
    case 0x00080002:
      this->m_Compression = 0x00c2; // run-length compressed signed char
      this->SetComponentType(IOComponentEnum::CHAR);
      break;
    case 0x00060001:
      this->m_Compression = 0x00b1; // packed bits
      this->SetComponentType(IOComponentEnum::CHAR);
      break;
    default:
      itkExceptionMacro("Unrecognized data type in AIM file: " << dataType);
  }

  this->SetNumberOfDimensions(3);
  for (unsigned int i = 0; i < m_NumberOfDimensions; ++i)
  {
    this->SetDimensions(i, structValues[3 + i]);
    this->SetSpacing(i, elementSize[i]);
    // the origin will reflect the cropping of the data
    this->SetOrigin(i, elementSize[i] * structValues[i]);
  }

  // decode the processing log
  h = preheader + preheaderSize + structSize;
  char * logEnd = h + logSize;

  while (h != logEnd && *h != '\0')
  {
    // skip newline and go to next line
    if (*h == '\n')
    {
      ++h;
    }

    // search for the end of this line
    char * lineEnd = h;
    while (lineEnd != logEnd && *lineEnd != '\n' && *lineEnd != '\0')
    {
      ++lineEnd;
    }

    // if not a comment, search for keys
    if (h != lineEnd && *h != '!' && (*lineEnd == '\n' || *lineEnd == '\0'))
    {
      // key and value are separated by multiple spaces
      char * key = h;
      while (h + 1 != lineEnd && (h[0] != ' ' || h[1] != ' '))
      {
        ++h;
      }
      // this gives the length of the key
      size_t keylen = h - key;
      // skip to the end of the spaces
      while (h != lineEnd && *h == ' ')
      {
        ++h;
      }
      // this is where the value starts
      char * value = h;
      size_t valuelen = lineEnd - value;
      // look for trailing spaces
      while (valuelen > 0 && (h[valuelen - 1] == ' ' || h[valuelen - 1] == '\r'))
      {
        --valuelen;
      }

      // convert into a std::string for convenience
      std::string skey(key, keylen);

      // check for known keys
      if (skey == "Time")
      {
        valuelen = (valuelen > 31 ? 31 : valuelen);
        strncpy(this->m_ModificationDate, value, valuelen);
        this->m_ModificationDate[valuelen] = '\0';
      }
      else if (skey == "Original Creation-Date")
      {
        valuelen = (valuelen > 31 ? 31 : valuelen);
        strncpy(this->m_CreationDate, value, valuelen);
        this->m_CreationDate[valuelen] = '\0';
      }
      else if (skey == "Orig-ISQ-Dim-p")
      {
        for (int & ScanDimensionsPixel : this->ScanDimensionsPixels)
        {
          ScanDimensionsPixel = strtol(value, &value, 10);
        }
      }
      else if (skey == "Orig-ISQ-Dim-um")
      {
        for (double & i : this->ScanDimensionsPhysical)
        {
          i = strtod(value, &value) * 1e-3;
        }
      }
      else if (skey == "Patient Name")
      {
        valuelen = (valuelen > 41 ? 41 : valuelen);
        strncpy(this->m_PatientName, value, valuelen);
        this->m_PatientName[valuelen] = '\0';
      }
      else if (skey == "Index Patient")
      {
        this->m_PatientIndex = strtol(value, nullptr, 10);
      }
      else if (skey == "Index Measurement")
      {
        this->m_MeasurementIndex = strtol(value, nullptr, 10);
      }
      else if (skey == "Site")
      {
        this->m_Site = strtol(value, nullptr, 10);
      }
      else if (skey == "Scanner ID")
      {
        this->m_ScannerID = strtol(value, nullptr, 10);
      }
      else if (skey == "Scanner type")
      {
        this->m_ScannerType = strtol(value, nullptr, 10);
      }
      else if (skey == "Position Slice 1 [um]")
      {
        this->m_StartPosition = strtod(value, nullptr) * 1e-3;
        this->m_EndPosition = this->m_StartPosition + elementSize[2] * (structValues[5] - 1);
      }
      else if (skey == "No. samples")
      {
        this->m_NumberOfSamples = strtol(value, nullptr, 10);
      }
      else if (skey == "No. projections per 180")
      {
        this->m_NumberOfProjections = strtol(value, nullptr, 10);
      }
      else if (skey == "Scan Distance [um]")
      {
        this->m_ScanDistance = strtod(value, nullptr) * 1e-3;
      }
      else if (skey == "Integration time [us]")
      {
        this->m_SampleTime = strtod(value, nullptr) * 1e-3;
      }
      else if (skey == "Reference line [um]")
      {
        this->m_ReferenceLine = strtod(value, nullptr) * 1e-3;
      }
      else if (skey == "Reconstruction-Alg.")
      {
        this->m_ReconstructionAlg = strtol(value, nullptr, 10);
      }
      else if (skey == "Energy [V]")
      {
        this->m_Energy = strtod(value, nullptr) * 1e-3;
      }
      else if (skey == "Intensity [uA]")
      {
        this->m_Intensity = strtod(value, nullptr) * 1e-3;
      }
      else if (skey == "Mu_Scaling")
      {
        this->m_MuScaling = strtol(value, nullptr, 10);
      }
      else if (skey == "Minimum data value")
      {
        this->m_DataRange[0] = strtod(value, nullptr);
      }
      else if (skey == "Maximum data value")
      {
        this->m_DataRange[1] = strtod(value, nullptr);
      }
      else if (skey == "Calib. default unit type")
      {
        this->m_RescaleType = strtol(value, nullptr, 10);
      }
      else if (skey == "Calibration Data")
      {
        valuelen = (valuelen > 64 ? 64 : valuelen);
        strncpy(this->m_CalibrationData, value, valuelen);
        this->m_CalibrationData[valuelen] = '\0';
      }
      else if (skey == "Density: unit")
      {
        valuelen = (valuelen > 16 ? 16 : valuelen);
        strncpy(this->m_RescaleUnits, value, valuelen);
        this->m_RescaleUnits[valuelen] = '\0';
      }
      else if (skey == "Density: slope")
      {
        this->m_RescaleSlope = strtod(value, nullptr);
      }
      else if (skey == "Density: intercept")
      {
        this->m_RescaleIntercept = strtod(value, nullptr);
      }
      else if (skey == "HU: mu water")
      {
        this->m_MuWater = strtod(value, nullptr);
      }
    }
    // skip to the end of the line
    h = lineEnd;
  }

  // these items are not in the processing log
  this->m_SliceThickness = elementSize[2];
  this->m_SliceIncrement = elementSize[2];

  return 1;
}


void
ScancoImageIO::ReadImageInformation()
{
  this->InitializeHeader();

  if (this->m_FileName.empty())
  {
    itkExceptionMacro("FileName has not been set.");
  }

  std::ifstream infile;
  this->OpenFileForReading(infile, this->m_FileName);

  // header is a 512 byte block
  this->m_RawHeader = new char[512];
  infile.read(this->m_RawHeader, 512);
  int           fileType = 0;
  unsigned long bytesRead = 0;
  if (!infile.bad())
  {
    bytesRead = static_cast<unsigned long>(infile.gcount());
    fileType = CheckVersion(this->m_RawHeader);
  }

  if (fileType == 0)
  {
    infile.close();
    itkExceptionMacro("Unrecognized header in: " << m_FileName);
  }

  if (fileType == 1)
  {
    this->ReadISQHeader(&infile, bytesRead);
  }
  else
  {
    this->ReadAIMHeader(&infile, bytesRead);
  }

  infile.close();
  this->PopulateMetaDataDictionary();
}

void
ScancoImageIO::PopulateMetaDataDictionary()
{
  MetaDataDictionary & thisDic = this->GetMetaDataDictionary();
  EncapsulateMetaData<std::string>(thisDic, "Version", std::string(this->m_Version));
  EncapsulateMetaData<std::string>(thisDic, "PatientName", std::string(this->m_PatientName));
  EncapsulateMetaData<int>(thisDic, "PatientIndex", this->m_PatientIndex);
  EncapsulateMetaData<int>(thisDic, "ScannerID", this->m_ScannerID);
  EncapsulateMetaData<std::string>(thisDic, "CreationDate", std::string(this->m_CreationDate));
  EncapsulateMetaData<std::string>(thisDic, "ModificationDate", std::string(this->m_ModificationDate));
  EncapsulateMetaData<double>(thisDic, "SliceThickness", this->m_SliceThickness);
  EncapsulateMetaData<double>(thisDic, "SliceIncrement", this->m_SliceIncrement);
  std::vector<double> dataRange(2);
  dataRange[0] = this->m_DataRange[0];
  dataRange[1] = this->m_DataRange[1];
  EncapsulateMetaData<std::vector<double>>(thisDic, "DataRange", dataRange);
  EncapsulateMetaData<double>(thisDic, "MuScaling", this->m_MuScaling);
  EncapsulateMetaData<int>(thisDic, "NumberOfSamples", this->m_NumberOfSamples);
  EncapsulateMetaData<int>(thisDic, "NumberOfProjections", this->m_NumberOfProjections);
  EncapsulateMetaData<double>(thisDic, "ScanDistance", this->m_ScanDistance);
  EncapsulateMetaData<double>(thisDic, "SampleTime", this->m_SampleTime);
  EncapsulateMetaData<int>(thisDic, "ScannerType", this->m_ScannerType);
  EncapsulateMetaData<int>(thisDic, "MeasurementIndex", this->m_MeasurementIndex);
  EncapsulateMetaData<int>(thisDic, "Site", this->m_Site);
  EncapsulateMetaData<int>(thisDic, "ReconstructionAlg", this->m_ReconstructionAlg);
  EncapsulateMetaData<double>(thisDic, "ReferenceLine", this->m_ReferenceLine);
  EncapsulateMetaData<double>(thisDic, "Energy", this->m_Energy);
  EncapsulateMetaData<double>(thisDic, "Intensity", this->m_Intensity);
  EncapsulateMetaData<int>(thisDic, "RescaleType", this->m_RescaleType);
  EncapsulateMetaData<std::string>(thisDic, "RescaleUnits", std::string(this->m_RescaleUnits));
  EncapsulateMetaData<std::string>(thisDic, "CalibrationData", std::string(this->m_CalibrationData));
  EncapsulateMetaData<double>(thisDic, "RescaleSlope", this->m_RescaleSlope);
  EncapsulateMetaData<double>(thisDic, "RescaleIntercept", this->m_RescaleIntercept);
  EncapsulateMetaData<double>(thisDic, "MuWater", this->m_MuWater);
}

void
ScancoImageIO::SetHeaderFromMetaDataDictionary()
{
  MetaDataDictionary & metaData = this->GetMetaDataDictionary();

  std::string stringMeta;
  if (ExposeMetaData<std::string>(metaData, "Version", stringMeta))
  {
    strncpy(this->m_Version, stringMeta.c_str(), 18);
  }
  if (ExposeMetaData<std::string>(metaData, "PatientName", stringMeta))
  {
    strncpy(this->m_PatientName, stringMeta.c_str(), 42);
  }

  ExposeMetaData<int>(metaData, "PatientIndex", this->m_PatientIndex);
  ExposeMetaData<int>(metaData, "ScannerID", this->m_ScannerID);

  if (ExposeMetaData<std::string>(metaData, "CreationDate", stringMeta))
  {
    strncpy(this->m_CreationDate, stringMeta.c_str(), 32);
  }
  if (ExposeMetaData<std::string>(metaData, "ModificationDate", stringMeta))
  {
    strncpy(this->m_ModificationDate, stringMeta.c_str(), 32);
  }

  ExposeMetaData<double>(metaData, "SliceThickness", this->m_SliceThickness);
  ExposeMetaData<double>(metaData, "SliceIncrement", this->m_SliceIncrement);

  std::vector<double> dataRange;
  if (ExposeMetaData<std::vector<double>>(metaData, "DataRange", dataRange) && dataRange.size() >= 2)
  {
    this->m_DataRange[0] = dataRange[0];
    this->m_DataRange[1] = dataRange[1];
  }

  ExposeMetaData<double>(metaData, "MuScaling", this->m_MuScaling);
  ExposeMetaData<int>(metaData, "NumberOfSamples", this->m_NumberOfSamples);
  ExposeMetaData<int>(metaData, "NumberOfProjections", this->m_NumberOfProjections);
  ExposeMetaData<double>(metaData, "ScanDistance", this->m_ScanDistance);
  ExposeMetaData<double>(metaData, "SampleTime", this->m_SampleTime);
  ExposeMetaData<int>(metaData, "ScannerType", this->m_ScannerType);
  ExposeMetaData<int>(metaData, "MeasurementIndex", this->m_MeasurementIndex);
  ExposeMetaData<int>(metaData, "Site", this->m_Site);
  ExposeMetaData<int>(metaData, "ReconstructionAlg", this->m_ReconstructionAlg);
  ExposeMetaData<double>(metaData, "ReferenceLine", this->m_ReferenceLine);
  ExposeMetaData<double>(metaData, "Energy", this->m_Energy);
  ExposeMetaData<double>(metaData, "Intensity", this->m_Intensity);
  ExposeMetaData<double>(metaData, "Intensity", this->m_Intensity);

  ExposeMetaData<int>(metaData, "RescaleType", this->m_RescaleType);
  if (ExposeMetaData<std::string>(metaData, "RescaleUnits", stringMeta))
  {
    strncpy(this->m_RescaleUnits, stringMeta.c_str(), 18);
  }
  if (ExposeMetaData<std::string>(metaData, "CalibrationData", stringMeta))
  {
    strncpy(this->m_CalibrationData, stringMeta.c_str(), 66);
  }

  ExposeMetaData<double>(metaData, "RescaleSlope", this->m_RescaleSlope);
  ExposeMetaData<double>(metaData, "RescaleIntercept", this->m_RescaleIntercept);
  ExposeMetaData<double>(metaData, "MuWater", this->m_MuWater);
}

template <typename TBufferType>
void
ScancoImageIO::RescaleToHU(TBufferType * buffer, size_t size)
{
  double slope = this->m_RescaleSlope;
  double intercept = this->m_RescaleIntercept;

  // This code causes rescaling to Hounsfield units
  if (this->m_MuScaling > 1.0 && this->m_MuWater > 0)
  {
    // mu(voxel) = intensity(voxel) / m_MuScaling
    // HU(voxel) = mu(voxel) * 1000/m_MuWater - 1000
    // Or, HU(voxel) = intensity(voxel) * (1000 / m_MuWater * m_MuScaling) - 1000
    slope = 1000.0 / (this->m_MuWater * this->m_MuScaling);
    intercept = -1000.0;
  }

  for (size_t i = 0; i < size; i++)
  {
    float bufferValue = static_cast<float>(buffer[i]);
    bufferValue = bufferValue * slope + intercept;
    buffer[i] = static_cast<TBufferType>(bufferValue);
  }
}

void
ScancoImageIO::Read(void * buffer)
{
  std::ifstream infile;
  this->OpenFileForReading(infile, this->m_FileName);

  // seek to the data
  infile.seekg(this->m_HeaderSize);

  // get the size of the compressed data
  int intSize = 4;
  if (strcmp(this->m_Version, "AIMDATA_V030   ") == 0)
  {
    // header uses 64-bit ints (8 bytes)
    intSize = 8;
  }

  // Dimensions of the data
  const int xsize = this->GetDimensions(0);
  const int ysize = this->GetDimensions(1);
  const int zsize = this->GetDimensions(2);
  size_t    outSize = xsize;
  outSize *= ysize;
  outSize *= zsize;
  outSize *= this->GetComponentSize();

  // For the input (compressed) data
  char * input = nullptr;
  size_t size = 0;

  if (this->m_Compression == 0)
  {
    infile.read(reinterpret_cast<char *>(buffer), outSize);
    size = outSize;
  }
  else if (this->m_Compression == 0x00b1)
  {
    // Compute the size of the binary packed data
    size_t xinc = (xsize + 1) / 2;
    size_t yinc = (ysize + 1) / 2;
    size_t zinc = (zsize + 1) / 2;
    size = xinc * yinc * zinc + 1;
    input = new char[size];
    infile.read(input, size);
  }
  else if (this->m_Compression == 0x00b2 || this->m_Compression == 0x00c2)
  {
    // Get the size of the compressed data
    char head[8];
    infile.read(head, intSize);
    size = static_cast<unsigned int>(DecodeInt(head));
    if (intSize == 8)
    {
      // Read the high word of a 64-bit int
      unsigned int high = DecodeInt(head + 4);
      size += (static_cast<uint64_t>(high) << 32);
    }
    input = new char[size - intSize];
    size -= intSize;
    infile.read(input, size);
  }

  // confirm that enough data was read
  size_t shortread = size - infile.gcount();
  if (shortread != 0)
  {
    itkExceptionMacro("File is truncated, " << shortread << " bytes are missing");
  }

  // Close the file
  infile.close();

  auto * dataPtr = reinterpret_cast<unsigned char *>(buffer);

  if (this->m_Compression == 0x00b1)
  {
    // Unpack binary data, each byte becomes a 2x2x2 block of voxels
    size_t        xinc = (xsize + 1) / 2;
    size_t        yinc = (ysize + 1) / 2;
    unsigned char v = input[size - 1];
    v = (v == 0 ? 0x7f : v);
    unsigned char bit = 0;
    for (int i = 0; i < zsize; i++)
    {
      bit ^= (bit & 2);
      for (int j = 0; j < ysize; j++)
      {
        char * inPtr = input + (i * yinc + j) * xinc;
        bit ^= (bit & 1);
        for (int k = 0; k < xsize; k++)
        {
          unsigned char c = *inPtr;
          *dataPtr++ = ((c >> bit) & 1) * v;
          inPtr += (bit & 1);
          bit ^= 1;
        }
        bit ^= 2;
      }
      bit ^= 4;
    }
  }
  else if (this->m_Compression == 0x00b2)
  {
    // Decompress binary run-lengths
    bool          flip = false;
    unsigned char v = input[flip];
    char *        inPtr = input + 2;
    size -= 2;
    if (size > 0)
    {
      do
      {
        unsigned char l = *inPtr++;
        if (l == 255)
        {
          l = 254;
          flip = !flip;
        }
        if (l > outSize)
        {
          l = static_cast<unsigned char>(outSize);
        }
        outSize -= l;
        if (l > 0)
        {
          do
          {
            *dataPtr++ = v;
          } while (--l);
        }
        flip = !flip;
        v = input[flip];
      } while (--size != 0 && outSize != 0);
    }
  }
  else if (this->m_Compression == 0x00c2)
  {
    // Decompress 8-bit run-lengths
    char * inPtr = input;
    size /= 2;
    if (size > 0)
    {
      do
      {
        unsigned char l = inPtr[0];
        unsigned char v = inPtr[1];
        inPtr += 2;
        if (l > outSize)
        {
          l = static_cast<unsigned char>(outSize);
        }
        outSize -= l;
        if (l > 0)
        {
          do
          {
            *dataPtr++ = v;
          } while (--l);
        }
      } while (--size != 0 && outSize != 0);
    }
  }

  delete[] input;

  // Convert the image to HU.
  // Only SHORT images have been tested
  IOComponentEnum dataType = this->m_ComponentType;

  // The size of the buffer will change depending on the data type
  size_t bufferSize = outSize;

  if (this->m_RescaleSlope != 1.0 || this->m_RescaleIntercept != 0.0)
  {
    switch (dataType)
    {
      case IOComponentEnum::CHAR:
        RescaleToHU(reinterpret_cast<char *>(buffer), bufferSize);
        break;
      case IOComponentEnum::UCHAR:
        RescaleToHU(reinterpret_cast<unsigned char *>(buffer), bufferSize);
        break;
      case IOComponentEnum::SHORT:
        bufferSize /= 2;
        RescaleToHU(reinterpret_cast<short *>(buffer), bufferSize);
        break;
      case IOComponentEnum::USHORT:
        bufferSize /= 2;
        RescaleToHU(reinterpret_cast<unsigned short *>(buffer), bufferSize);
        break;
      case IOComponentEnum::INT:
        bufferSize /= 4;
        RescaleToHU(reinterpret_cast<int *>(buffer), bufferSize);
        break;
      case IOComponentEnum::UINT:
        bufferSize /= 4;
        RescaleToHU(reinterpret_cast<unsigned int *>(buffer), bufferSize);
        break;
      case IOComponentEnum::FLOAT:
        bufferSize /= 4;
        RescaleToHU(reinterpret_cast<float *>(buffer), bufferSize);
        break;
      default:
        itkExceptionMacro("Unrecognized data type in file: " << dataType);
    }
  }
}


bool
ScancoImageIO::CanWriteFile(const char * name)
{
  const std::string filename = name;

  if (filename.empty())
  {
    return false;
  }

  return this->HasSupportedWriteExtension(name, true);
}


void
ScancoImageIO::WriteISQHeader(std::ofstream * file)
{
  if (!this->m_HeaderInitialized)
  {
    this->InitializeHeader();
  }
  this->SetHeaderFromMetaDataDictionary();
  // now overwrite some values which we don't want taken from metadata
  this->SetVersion("CTDATA-HEADER_V1");
  this->m_MuScaling = 1.0; // we don't want rescaling to occur on reading

  delete[] this->m_RawHeader;
  this->m_RawHeader = new char[512];
  char * header = this->m_RawHeader;

  PadString(header, this->m_Version, 16);
  header += 16;
  // 3 -> ISQ data type
  EncodeInt(3, header);
  header += 4;
  const auto numberOfBytes = static_cast<SizeValueType>(this->GetImageSizeInBytes());
  if (numberOfBytes > NumericTraits<int>::max())
  {
    EncodeInt(0, header);
    header += 4;
  }
  else
  {
    EncodeInt(numberOfBytes, header);
    header += 4;
  }
  EncodeInt(numberOfBytes / 512, header);
  header += 4;
  EncodeInt(this->m_PatientIndex, header);
  header += 4;
  EncodeInt(this->m_ScannerID, header);
  header += 4;
  EncodeDateFromString(header, this->m_CreationDate);
  header += 8;
  for (unsigned int dimension = 0; dimension < 3; ++dimension)
  {
    // pixdim
    EncodeInt(this->GetDimensions(dimension), header);
    header += 4;
  }
  for (unsigned int dimension = 0; dimension < 3; ++dimension)
  {
    // physdim
    EncodeInt(this->GetSpacing(dimension) * this->GetDimensions(dimension) * 1e3, header);
    header += 4;
  }
  EncodeInt((int)(this->m_SliceThickness * 1e3), header);
  header += 4;
  EncodeInt((int)(this->m_SliceIncrement * 1e3), header);
  header += 4;
  EncodeInt((int)(this->m_StartPosition * 1e3), header);
  header += 4;
  EncodeInt((int)(this->m_DataRange[0]), header);
  header += 4;
  EncodeInt((int)(this->m_DataRange[1]), header);
  header += 4;
  EncodeInt((int)(this->m_MuScaling), header);
  header += 4;
  EncodeInt(this->m_NumberOfSamples, header);
  header += 4;
  EncodeInt(this->m_NumberOfProjections, header);
  header += 4;
  EncodeInt((int)(this->m_ScanDistance * 1e3), header);
  header += 4;
  EncodeInt((int)(this->m_ScannerType), header);
  header += 4;
  EncodeInt((int)(this->m_SampleTime * 1e3), header);
  header += 4;
  EncodeInt((int)(this->m_MeasurementIndex), header);
  header += 4;
  EncodeInt((int)(this->m_Site), header);
  header += 4;
  EncodeInt((int)(this->m_ReferenceLine * 1e3), header);
  header += 4;
  EncodeInt((int)(this->m_ReconstructionAlg), header);
  header += 4;
  PadString(header, this->m_PatientName, 40);
  header += 40;
  EncodeInt((int)(this->m_Energy * 1e3), header);
  header += 4;
  EncodeInt((int)(this->m_Intensity * 1e3), header);
  header += 4;
  const std::size_t fillSize = 83 * 4;
  std::memset(header, 0x00, fillSize);
  header += fillSize;
  const int dataOffset = 0;
  EncodeInt(dataOffset, header);
  header += 4;

  this->m_HeaderSize = static_cast<SizeValueType>(dataOffset + 1) * 512;
  this->m_Compression = 0;

  file->write(this->m_RawHeader, 512);
}


void
ScancoImageIO::WriteImageInformation()
{
  if (this->m_FileName.empty())
  {
    itkExceptionMacro("FileName has not been set.");
  }

  std::ofstream outFile;
  this->OpenFileForWriting(outFile, m_FileName);

  this->WriteISQHeader(&outFile);

  outFile.close();
}


void
ScancoImageIO::Write(const void * buffer)
{
  this->WriteImageInformation();

  std::ofstream outFile;
  this->OpenFileForWriting(outFile, m_FileName, false);
  outFile.seekp(this->m_HeaderSize, std::ios::beg);

  const auto numberOfBytes = static_cast<SizeValueType>(this->GetImageSizeInBytes());
  const auto numberOfComponents = static_cast<SizeValueType>(this->GetImageSizeInComponents());

  if (this->GetComponentType() != IOComponentEnum::SHORT)
  {
    itkExceptionMacro("ScancoImageIO only supports writing short files.");
  }

  if (ByteSwapper<short>::SystemIsBigEndian())
  {
    char * tempmemory = new char[numberOfBytes];
    memcpy(tempmemory, buffer, numberOfBytes);
    {
      ByteSwapper<short>::SwapRangeFromSystemToBigEndian(reinterpret_cast<short *>(tempmemory), numberOfComponents);
    }

    // Write the actual pixel data
    outFile.write(static_cast<const char *>(tempmemory), numberOfBytes);
    delete[] tempmemory;
  }
  else
  {
    outFile.write(static_cast<const char *>(buffer), numberOfBytes);
  }

  outFile.close();
}

} // end namespace itk
