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
#include "itkAIMHeaderIO.h"
#include "itkISQHeaderIO.h"

#include <algorithm>
#include <ctime>
#include <filesystem>

namespace itk
{
ScancoImageIO::ScancoImageIO()

{
  this->m_FileType = IOFileEnum::Binary;
  this->m_ByteOrder = IOByteOrderEnum::LittleEndian;

  this->AddSupportedWriteExtension(".isq");
  this->AddSupportedWriteExtension(".aim");

  this->AddSupportedReadExtension(".isq");
  this->AddSupportedReadExtension(".rsq");
  this->AddSupportedReadExtension(".rad");
  this->AddSupportedReadExtension(".aim");

  this->m_HeaderData.m_RawHeader = nullptr;
}


ScancoImageIO::~ScancoImageIO()
{
  delete this->m_HeaderIO;
  delete[] this->m_HeaderData.m_RawHeader;
}


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
  memset(this->m_HeaderData.m_Version, 0, 18);
  memset(this->m_HeaderData.m_PatientName, 0, 42);
  this->m_HeaderData.m_PatientIndex = 0;
  this->m_HeaderData.m_ScannerID = 0;
  memset(this->m_HeaderData.m_CreationDate, 0, 32);
  memset(this->m_HeaderData.m_ModificationDate, 0, 32);
  this->m_HeaderData.m_ScanDimensionsPixels[0] = 0;
  this->m_HeaderData.m_ScanDimensionsPixels[1] = 0;
  this->m_HeaderData.m_ScanDimensionsPixels[2] = 0;
  this->m_HeaderData.m_ScanDimensionsPhysical[0] = 0;
  this->m_HeaderData.m_ScanDimensionsPhysical[1] = 0;
  this->m_HeaderData.m_ScanDimensionsPhysical[2] = 0;
  this->m_HeaderData.m_SliceThickness = 0;
  this->m_HeaderData.m_SliceIncrement = 0;
  this->m_HeaderData.m_StartPosition = 0;
  this->m_HeaderData.m_EndPosition = 0;
  this->m_HeaderData.m_ZPosition = 0;
  this->m_HeaderData.m_DataRange[0] = 0;
  this->m_HeaderData.m_DataRange[1] = 0;
  this->m_HeaderData.m_MuScaling = 1.0;
  this->m_HeaderData.m_NumberOfSamples = 0;
  this->m_HeaderData.m_NumberOfProjections = 0;
  this->m_HeaderData.m_ScanDistance = 0;
  this->m_HeaderData.m_SampleTime = 0;
  this->m_HeaderData.m_ScannerType = 0;
  this->m_HeaderData.m_MeasurementIndex = 0;
  this->m_HeaderData.m_Site = 0;
  this->m_HeaderData.m_ReconstructionAlg = 0;
  this->m_HeaderData.m_ReferenceLine = 0;
  this->m_HeaderData.m_Energy = 0;
  this->m_HeaderData.m_Intensity = 0;

  this->m_HeaderData.m_RescaleType = 0;
  memset(this->m_HeaderData.m_RescaleUnits, 0, 18);
  memset(this->m_HeaderData.m_CalibrationData, 0, 66);
  this->m_HeaderData.m_RescaleSlope = 1.0;
  this->m_HeaderData.m_RescaleIntercept = 0.0;
  this->m_HeaderData.m_MuWater = 0.70329999923706055;

  this->m_Compression = 0;
  this->m_HeaderInitialized = true;
}

void
ScancoImageIO::ParseAIMComponentType(int dataType)
{
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
}

void
ScancoImageIO::SetHeaderIO()
{
  std::string fileExtension = this->m_FileName.substr(this->m_FileName.find_last_of('.') + 1);
  std::transform(
    fileExtension.begin(), fileExtension.end(), fileExtension.begin(), [](unsigned char c) { return std::toupper(c); });

  delete this->m_HeaderIO;
  if (fileExtension == "AIM")
  {
    this->m_HeaderIO = new AIMHeaderIO(&this->m_HeaderData, this->m_FileName);
    this->m_FileExtension = ScancoImageIO::ScancoFileExtensions::AIM;
  }
  else if (fileExtension == "RAD" || fileExtension == "ISQ" || fileExtension == "RSQ")
  {
    this->m_HeaderIO = new ISQHeaderIO(&this->m_HeaderData, this->m_FileName);

    if (fileExtension == "RAD")
    {
      this->m_FileExtension = ScancoImageIO::ScancoFileExtensions::RAD;
    }
    else if (fileExtension == "RSQ")
    {
      this->m_FileExtension = ScancoImageIO::ScancoFileExtensions::RSQ;
    }
    else
    {
      this->m_FileExtension = ScancoImageIO::ScancoFileExtensions::ISQ;
    }
  }
  else
  {
    this->m_FileExtension = ScancoImageIO::ScancoFileExtensions::UNRECOGNIZED;
  }
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

  this->SetHeaderIO();
  if (this->m_FileExtension == ScancoImageIO::ScancoFileExtensions::UNRECOGNIZED)
  {
    infile.close();
    itkExceptionMacro("Incompatible filetype in: " << this->m_FileName);
  }

  this->m_HeaderSize = this->m_HeaderIO->ReadHeader(infile);

  // set dimensions/spacing
  this->SetNumberOfDimensions(3);

  for (unsigned int i = 0; i < m_NumberOfDimensions; ++i)
  {
    this->SetDimensions(i, this->m_HeaderData.m_PixelData.m_Dimensions[i]);
    this->SetSpacing(i, this->m_HeaderData.m_PixelData.m_Spacing[i]);
    // the origin will reflect the cropping of the data
    this->SetOrigin(i, this->m_HeaderData.m_PixelData.m_Origin[i]);
  }

  if (this->m_FileExtension == ScancoImageIO::ScancoFileExtensions::AIM)
  {
    this->ParseAIMComponentType(this->m_HeaderData.m_PixelData.m_ComponentType);
  }
  else
  {
    this->SetComponentType(IOComponentEnum::SHORT);
    this->m_Compression = 0;
  }
  this->SetPixelType(IOPixelEnum::SCALAR);

  infile.close();
  this->PopulateMetaDataDictionary();
}

void
ScancoImageIO::PopulateMetaDataDictionary()
{
  std::vector<double>  dataRange(2);
  std::vector<int>     pixelDimensions(3);
  std::vector<double>  physicalDimensions(3);
  MetaDataDictionary & thisDic = this->GetMetaDataDictionary();
  EncapsulateMetaData<std::string>(thisDic, "Version", std::string(this->m_HeaderData.m_Version));
  EncapsulateMetaData<std::string>(thisDic, "PatientName", std::string(this->m_HeaderData.m_PatientName));
  EncapsulateMetaData<int>(thisDic, "PatientIndex", this->m_HeaderData.m_PatientIndex);
  EncapsulateMetaData<int>(thisDic, "ScannerID", this->m_HeaderData.m_ScannerID);
  EncapsulateMetaData<std::string>(thisDic, "CreationDate", std::string(this->m_HeaderData.m_CreationDate));
  EncapsulateMetaData<std::string>(thisDic, "ModificationDate", std::string(this->m_HeaderData.m_ModificationDate));
  EncapsulateMetaData<double>(thisDic, "SliceThickness", this->m_HeaderData.m_SliceThickness);
  EncapsulateMetaData<double>(thisDic, "SliceIncrement", this->m_HeaderData.m_SliceIncrement);
  dataRange[0] = this->m_HeaderData.m_DataRange[0];
  dataRange[1] = this->m_HeaderData.m_DataRange[1];
  EncapsulateMetaData<std::vector<double>>(thisDic, "DataRange", dataRange);
  EncapsulateMetaData<double>(thisDic, "MuScaling", this->m_HeaderData.m_MuScaling);
  EncapsulateMetaData<int>(thisDic, "NumberOfSamples", this->m_HeaderData.m_NumberOfSamples);
  EncapsulateMetaData<int>(thisDic, "NumberOfProjections", this->m_HeaderData.m_NumberOfProjections);
  EncapsulateMetaData<double>(thisDic, "ScanDistance", this->m_HeaderData.m_ScanDistance);
  EncapsulateMetaData<double>(thisDic, "SampleTime", this->m_HeaderData.m_SampleTime);
  EncapsulateMetaData<int>(thisDic, "ScannerType", this->m_HeaderData.m_ScannerType);
  EncapsulateMetaData<int>(thisDic, "MeasurementIndex", this->m_HeaderData.m_MeasurementIndex);
  EncapsulateMetaData<int>(thisDic, "Site", this->m_HeaderData.m_Site);
  EncapsulateMetaData<int>(thisDic, "ReconstructionAlg", this->m_HeaderData.m_ReconstructionAlg);
  EncapsulateMetaData<double>(thisDic, "ReferenceLine", this->m_HeaderData.m_ReferenceLine);
  EncapsulateMetaData<double>(thisDic, "Energy", this->m_HeaderData.m_Energy);
  EncapsulateMetaData<double>(thisDic, "Intensity", this->m_HeaderData.m_Intensity);
  EncapsulateMetaData<int>(thisDic, "RescaleType", this->m_HeaderData.m_RescaleType);
  EncapsulateMetaData<std::string>(thisDic, "RescaleUnits", std::string(this->m_HeaderData.m_RescaleUnits));
  EncapsulateMetaData<std::string>(thisDic, "CalibrationData", std::string(this->m_HeaderData.m_CalibrationData));
  EncapsulateMetaData<double>(thisDic, "RescaleSlope", this->m_HeaderData.m_RescaleSlope);
  EncapsulateMetaData<double>(thisDic, "RescaleIntercept", this->m_HeaderData.m_RescaleIntercept);
  EncapsulateMetaData<double>(thisDic, "MuWater", this->m_HeaderData.m_MuWater);
  EncapsulateMetaData<double>(thisDic, "StartPosition", this->m_HeaderData.m_StartPosition);

  for (int i = 0; i < this->GetNumberOfDimensions(); i++)
  {
    pixelDimensions[i] = this->m_HeaderData.m_ScanDimensionsPixels[i];
    physicalDimensions[i] = this->m_HeaderData.m_ScanDimensionsPhysical[i];
  }

  EncapsulateMetaData<std::vector<int>>(thisDic, "PixelDimensions", pixelDimensions);
  EncapsulateMetaData<std::vector<double>>(thisDic, "PhysicalDimensions", physicalDimensions);
}

void
ScancoImageIO::SetHeaderFromMetaDataDictionary()
{
  std::vector<int>     pixelDimensions;
  std::vector<double>  physicalDimensions;
  MetaDataDictionary & metaData = this->GetMetaDataDictionary();

  std::string stringMeta;
  if (ExposeMetaData<std::string>(metaData, "Version", stringMeta))
  {
    strncpy(this->m_HeaderData.m_Version, stringMeta.c_str(), 18);
  }
  if (ExposeMetaData<std::string>(metaData, "PatientName", stringMeta))
  {
    strncpy(this->m_HeaderData.m_PatientName, stringMeta.c_str(), 42);
  }

  ExposeMetaData<int>(metaData, "PatientIndex", this->m_HeaderData.m_PatientIndex);
  ExposeMetaData<int>(metaData, "ScannerID", this->m_HeaderData.m_ScannerID);

  if (ExposeMetaData<std::string>(metaData, "CreationDate", stringMeta))
  {
    strncpy(this->m_HeaderData.m_CreationDate, stringMeta.c_str(), 32);
  }
  if (ExposeMetaData<std::string>(metaData, "ModificationDate", stringMeta))
  {
    strncpy(this->m_HeaderData.m_ModificationDate, stringMeta.c_str(), 32);
  }

  ExposeMetaData<double>(metaData, "SliceThickness", this->m_HeaderData.m_SliceThickness);
  ExposeMetaData<double>(metaData, "SliceIncrement", this->m_HeaderData.m_SliceIncrement);

  std::vector<double> dataRange;
  if (ExposeMetaData<std::vector<double>>(metaData, "DataRange", dataRange) && dataRange.size() >= 2)
  {
    this->m_HeaderData.m_DataRange[0] = dataRange[0];
    this->m_HeaderData.m_DataRange[1] = dataRange[1];
  }

  ExposeMetaData<double>(metaData, "MuScaling", this->m_HeaderData.m_MuScaling);
  ExposeMetaData<int>(metaData, "NumberOfSamples", this->m_HeaderData.m_NumberOfSamples);
  ExposeMetaData<int>(metaData, "NumberOfProjections", this->m_HeaderData.m_NumberOfProjections);
  ExposeMetaData<double>(metaData, "ScanDistance", this->m_HeaderData.m_ScanDistance);
  ExposeMetaData<double>(metaData, "SampleTime", this->m_HeaderData.m_SampleTime);
  ExposeMetaData<int>(metaData, "ScannerType", this->m_HeaderData.m_ScannerType);
  ExposeMetaData<int>(metaData, "MeasurementIndex", this->m_HeaderData.m_MeasurementIndex);
  ExposeMetaData<int>(metaData, "Site", this->m_HeaderData.m_Site);
  ExposeMetaData<int>(metaData, "ReconstructionAlg", this->m_HeaderData.m_ReconstructionAlg);
  ExposeMetaData<double>(metaData, "ReferenceLine", this->m_HeaderData.m_ReferenceLine);
  ExposeMetaData<double>(metaData, "Energy", this->m_HeaderData.m_Energy);
  ExposeMetaData<double>(metaData, "Intensity", this->m_HeaderData.m_Intensity);

  ExposeMetaData<int>(metaData, "RescaleType", this->m_HeaderData.m_RescaleType);
  if (ExposeMetaData<std::string>(metaData, "RescaleUnits", stringMeta))
  {
    strncpy(this->m_HeaderData.m_RescaleUnits, stringMeta.c_str(), 18);
  }
  if (ExposeMetaData<std::string>(metaData, "CalibrationData", stringMeta))
  {
    strncpy(this->m_HeaderData.m_CalibrationData, stringMeta.c_str(), 66);
  }

  ExposeMetaData<double>(metaData, "RescaleSlope", this->m_HeaderData.m_RescaleSlope);
  ExposeMetaData<double>(metaData, "RescaleIntercept", this->m_HeaderData.m_RescaleIntercept);
  ExposeMetaData<double>(metaData, "MuWater", this->m_HeaderData.m_MuWater);
  ExposeMetaData<double>(metaData, "StartPosition", this->m_HeaderData.m_StartPosition);

  if (!ExposeMetaData<std::vector<int>>(metaData, "PixelDimensions", pixelDimensions) ||
      pixelDimensions.size() < this->GetNumberOfDimensions())
  {
    return;
  }

  if (!ExposeMetaData<std::vector<double>>(metaData, "PhysicalDimensions", physicalDimensions) ||
      physicalDimensions.size() < this->GetNumberOfDimensions())
  {
    return;
  }

  for (int i = 0; i < this->GetNumberOfDimensions(); i++)
  {
    this->m_HeaderData.m_ScanDimensionsPixels[i] = pixelDimensions[i];
    this->m_HeaderData.m_ScanDimensionsPhysical[i] = physicalDimensions[i];
  }
}

template <typename TBufferType>
void
ScancoImageIO::RescaleToHU(TBufferType * buffer, size_t size)
{
  double slope = this->m_HeaderData.m_RescaleSlope;
  double intercept = this->m_HeaderData.m_RescaleIntercept;

  // This code causes rescaling to Hounsfield units
  if (this->m_HeaderData.m_MuScaling > 1.0 && this->m_HeaderData.m_MuWater > 0)
  {
    // mu(voxel) = intensity(voxel) / m_MuScaling
    // HU(voxel) = mu(voxel) * 1000/m_MuWater - 1000
    // Or, HU(voxel) = intensity(voxel) * (1000 / m_MuWater * m_MuScaling) - 1000
    slope = 1000.0 / (this->m_HeaderData.m_MuWater * this->m_HeaderData.m_MuScaling);
    intercept = -1000.0;

    for (size_t i = 0; i < size; i++)
    {
      float bufferValue = static_cast<float>(buffer[i]);
      bufferValue = bufferValue * slope + intercept;
      buffer[i] = static_cast<TBufferType>(bufferValue);
    }
  }
}

/** Rescale the image data to Scanco Units */
template <typename TBufferType>
void
ScancoImageIO::RescaleToScanco(TBufferType * buffer, size_t size)
{
  double slope = this->m_HeaderData.m_RescaleSlope;
  double intercept = this->m_HeaderData.m_RescaleIntercept;

  // This code causes rescaling to Hounsfield units
  if (this->m_HeaderData.m_MuScaling > 1.0 && this->m_HeaderData.m_MuWater > 0)
  {
    // mu(voxel) = intensity(voxel) / m_MuScaling
    // HU(voxel) = mu(voxel) * 1000/m_MuWater - 1000
    // Or, HU(voxel) = intensity(voxel) * (1000 / m_MuWater * m_MuScaling) - 1000
    slope = 1000.0 / (this->m_HeaderData.m_MuWater * this->m_HeaderData.m_MuScaling);
    intercept = -1000.0;

    for (size_t i = 0; i < size; i++)
    {
      float bufferValue = static_cast<float>(buffer[i]);
      bufferValue = (bufferValue - intercept) / slope;
      buffer[i] = static_cast<TBufferType>(bufferValue);
    }
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
  if (strcmp(this->m_HeaderData.m_Version, "AIMDATA_V030   ") == 0)
  {
    // header uses 64-bit ints (8 bytes)
    intSize = 8;
  }

  // Dimensions of the data
  const int xsize = this->GetDimensions(0);
  const int ysize = this->GetDimensions(1);
  const int zsize = this->GetDimensions(2);
  size_t    outSize = this->GetImageSizeInBytes();

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

  if (this->m_HeaderData.m_RescaleSlope != 1.0 || this->m_HeaderData.m_RescaleIntercept != 0.0)
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
ScancoImageIO::SetDataTypeFromComponentEnum()
{
  if (this->m_ComponentType == IOComponentEnum::SHORT)
  {
    this->m_HeaderData.m_PixelData.m_ComponentType = 0x00020002; // short
  }
  else if (this->m_ComponentType == IOComponentEnum::FLOAT)
  {
    this->m_HeaderData.m_PixelData.m_ComponentType = 0x001a0004; // float
  }
  else if (this->m_ComponentType == IOComponentEnum::UCHAR)
  {
    this->m_HeaderData.m_PixelData.m_ComponentType = 0x00160001; // unsigned char
  }
  else if (this->m_ComponentType == IOComponentEnum::CHAR)
  {
    this->m_HeaderData.m_PixelData.m_ComponentType = 0x00010001; // char
  }
  else
  {
    itkExceptionMacro("ScancoImageIO only supports writing short, float, or unsigned char files.");
  }
}

void
ScancoImageIO::WriteImageInformation()
{
  if (this->m_FileName.empty())
  {
    itkExceptionMacro("FileName has not been set.");
  }

  std::ofstream outFile;
  this->OpenFileForWriting(outFile, this->m_FileName);

  if (!this->m_HeaderInitialized)
  {
    this->InitializeHeader();
  }
  this->SetHeaderFromMetaDataDictionary();

  this->SetHeaderIO();

  if (this->m_FileExtension == ScancoFileExtensions::UNRECOGNIZED)
  {
    itkExceptionMacro("ScancoImageIO: Cannot write file, incompatible extension type.");
  }
  else if (this->m_FileExtension == ScancoFileExtensions::AIM)
  {
    if (strcmp(this->m_HeaderData.m_Version, "AIMDATA_V020   ") == 0)
    {
      // writing to version 020 can be specified, but default is v030
      this->SetVersion("AIMDATA_V020   ");
    }
    else
    {
      this->SetVersion("AIMDATA_V030   ");
    }
  }
  else
  {
    this->SetVersion("CTDATA-HEADER_V1");
  }

  this->SetDataTypeFromComponentEnum();

  for (unsigned int i = 0; i < m_NumberOfDimensions; ++i)
  {
    this->m_HeaderData.m_PixelData.m_Dimensions[i] = this->GetDimensions(i);
    this->m_HeaderData.m_PixelData.m_Spacing[i] = this->GetSpacing(i);
    // the origin will reflect the cropping of the data
    this->m_HeaderData.m_PixelData.m_Origin[i] = this->GetOrigin(i);
  }

  this->m_HeaderSize =
    static_cast<SizeValueType>(this->m_HeaderIO->WriteHeader(outFile, (unsigned long)this->GetImageSizeInBytes()));

  this->m_Compression = 0;

  outFile.close();
}


void
ScancoImageIO::Write(const void * buffer)
{
  this->WriteImageInformation();

  std::ofstream outFile;
  this->OpenFileForWriting(outFile, this->m_FileName, false);
  outFile.seekp(this->m_HeaderSize, std::ios::beg);

  const auto numberOfBytes = static_cast<SizeValueType>(this->GetImageSizeInBytes());
  const auto numberOfComponents = static_cast<SizeValueType>(this->GetImageSizeInComponents());

  if (this->GetComponentType() != IOComponentEnum::SHORT && this->GetComponentType() != IOComponentEnum::FLOAT)
  {
    itkExceptionMacro("ScancoImageIO only supports writing short or float files.");
  }

  char * tempmemory = new char[numberOfBytes];
  memcpy(tempmemory, buffer, numberOfBytes);

  bool bigEndian = ByteSwapper<short>::SystemIsBigEndian();

  if (this->m_HeaderData.m_RescaleSlope != 1.0 || this->m_HeaderData.m_RescaleIntercept != 0.0)
  {
    switch (this->GetComponentType())
    {
      case IOComponentEnum::CHAR:
        RescaleToScanco(reinterpret_cast<char *>(tempmemory), numberOfComponents);
        if (bigEndian)
        {
          ByteSwapper<char>::SwapRangeFromSystemToBigEndian(reinterpret_cast<char *>(tempmemory), numberOfComponents);
        }
        break;
      case IOComponentEnum::UCHAR:
        RescaleToScanco(reinterpret_cast<unsigned char *>(tempmemory), numberOfComponents);
        if (bigEndian)
        {
          ByteSwapper<unsigned char>::SwapRangeFromSystemToBigEndian(reinterpret_cast<unsigned char *>(tempmemory),
                                                                     numberOfComponents);
        }
        break;
      case IOComponentEnum::SHORT:
        RescaleToScanco(reinterpret_cast<short *>(tempmemory), numberOfComponents);
        if (bigEndian)
        {
          ByteSwapper<short>::SwapRangeFromSystemToBigEndian(reinterpret_cast<short *>(tempmemory), numberOfComponents);
        }
        break;
      case IOComponentEnum::USHORT:
        RescaleToScanco(reinterpret_cast<unsigned short *>(tempmemory), numberOfComponents);
        if (bigEndian)
        {
          ByteSwapper<unsigned short>::SwapRangeFromSystemToBigEndian(reinterpret_cast<unsigned short *>(tempmemory),
                                                                      numberOfComponents);
        }
        break;
      case IOComponentEnum::INT:
        RescaleToScanco(reinterpret_cast<int *>(tempmemory), numberOfComponents);
        if (bigEndian)
        {
          ByteSwapper<int>::SwapRangeFromSystemToBigEndian(reinterpret_cast<int *>(tempmemory), numberOfComponents);
        }
        break;
      case IOComponentEnum::UINT:
        RescaleToScanco(reinterpret_cast<unsigned int *>(tempmemory), numberOfComponents);
        if (bigEndian)
        {
          ByteSwapper<unsigned int>::SwapRangeFromSystemToBigEndian(reinterpret_cast<unsigned int *>(tempmemory),
                                                                    numberOfComponents);
        }
        break;
      case IOComponentEnum::FLOAT:
        RescaleToScanco(reinterpret_cast<float *>(tempmemory), numberOfComponents);
        if (bigEndian)
        {
          ByteSwapper<float>::SwapRangeFromSystemToBigEndian(reinterpret_cast<float *>(tempmemory), numberOfComponents);
        }
        break;
      default:
        itkExceptionMacro("Unrecognized data type in file: " << this->m_ComponentType);
    }
  }
  outFile.write(static_cast<const char *>(tempmemory), numberOfBytes);

  delete[] tempmemory;
  outFile.close();
}

} // end namespace itk
