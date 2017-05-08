/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

namespace itk
{

ScancoImageIO ::ScancoImageIO()
{
  this->m_FileType = Binary;
  this->m_ByteOrder = LittleEndian;

  this->AddSupportedWriteExtension(".isq");
  this->AddSupportedWriteExtension(".rsq");
  this->AddSupportedWriteExtension(".rad");
  this->AddSupportedWriteExtension(".aim");

  this->AddSupportedReadExtension(".isq");
  this->AddSupportedReadExtension(".rsq");
  this->AddSupportedReadExtension(".rad");
  this->AddSupportedReadExtension(".aim");
}


ScancoImageIO ::~ScancoImageIO() {}


void
ScancoImageIO ::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}


int
ScancoImageIO ::CheckVersion(const char header[16])
{
  int fileType = 0;

  if (strncmp(header, "CTDATA-HEADER_V1", 16) == 0)
  {
    fileType = 1;
  }
  else if (strcmp(header, "AIMDATA_V030   ") == 0)
  {
    fileType = 3;
  }
  else
  {
    int preHeaderSize = ScancoImageIO::DecodeInt(header);
    int imageHeaderSize = ScancoImageIO::DecodeInt(header + 4);
    if (preHeaderSize == 20 && imageHeaderSize == 140)
    {
      fileType = 2;
    }
  }

  return fileType;
}


int
ScancoImageIO ::DecodeInt(const void * data)
{
  const unsigned char * cp = static_cast<const unsigned char *>(data);
  return (cp[0] | (cp[1] << 8) | (cp[2] << 16) | (cp[3] << 24));
}


float
ScancoImageIO ::DecodeFloat(const void * data)
{
  const unsigned char * cp = static_cast<const unsigned char *>(data);
  // different ordering and exponent bias than IEEE 754 float
  union
  {
    float        f;
    unsigned int i;
  } v;
  v.i = (cp[0] << 16) | (cp[1] << 24) | cp[2] | (cp[3] << 8);
  return 0.25 * v.f;
}


double
ScancoImageIO ::DecodeDouble(const void * data)
{
  // different ordering and exponent bias than IEEE 754 double
  const unsigned char * cp = static_cast<const unsigned char *>(data);
  union
  {
    double   d;
    uint64_t l;
  } v;
  unsigned int l1, l2;
  l1 = (cp[0] << 16) | (cp[1] << 24) | cp[2] | (cp[3] << 8);
  l2 = (cp[4] << 16) | (cp[5] << 24) | cp[6] | (cp[7] << 8);
  v.l = (static_cast<uint64_t>(l1) << 32) | l2;
  return v.d * 0.25;
}


bool
ScancoImageIO ::CanReadFile(const char * filename)
{
  std::ifstream infile(filename, std::ios::in | std::ios::binary);

  bool canRead = false;
  if (infile.good())
  {
    // header is a 512 byte block
    char buffer[512];
    infile.read(buffer, 512);
    if (!infile.bad())
    {
      int fileType = ScancoImageIO::CheckVersion(buffer);
      canRead = (fileType > 0);
    }
  }

  infile.close();

  return canRead;
}


void
ScancoImageIO ::ReadImageInformation()
{
  if (!m_ScancoImage.Read(m_FileName.c_str(), false))
  {
    itkExceptionMacro("File cannot be read: " << this->GetFileName() << " for reading." << std::endl
                                              << "Reason: " << itksys::SystemTools::GetLastSystemError());
  }

  if (m_ScancoImage.BinaryData())
  {
    this->SetFileType(Binary);
  }
  else
  {
    this->SetFileType(ASCII);
  }

  this->SetNumberOfComponents(m_ScancoImage.ElementNumberOfChannels());

  itk::ScancoDataDictionary & thisScancoDict = this->GetScancoDataDictionary();
  switch (m_ScancoImage.ElementType())
  {
    default:
    case MET_OTHER:
    case MET_NONE:
      this->SetPixelType(UNKNOWNPIXELTYPE);
      this->SetComponentType(UNKNOWNCOMPONENTTYPE);
      break;
    case MET_CHAR:
    case MET_ASCII_CHAR:
      this->SetPixelType(SCALAR);
      this->SetComponentType(CHAR);
      break;
    case MET_CHAR_ARRAY:
    case MET_STRING:
      this->SetPixelType(VECTOR);
      this->SetComponentType(CHAR);
      break;
    case MET_UCHAR:
      this->SetPixelType(SCALAR);
      this->SetComponentType(UCHAR);
      break;
    case MET_UCHAR_ARRAY:
      this->SetPixelType(VECTOR);
      this->SetComponentType(UCHAR);
      break;
    case MET_SHORT:
      this->SetPixelType(SCALAR);
      this->SetComponentType(SHORT);
      break;
    case MET_SHORT_ARRAY:
      this->SetPixelType(VECTOR);
      this->SetComponentType(SHORT);
      break;
    case MET_USHORT:
      this->SetPixelType(SCALAR);
      this->SetComponentType(USHORT);
      break;
    case MET_USHORT_ARRAY:
      this->SetPixelType(VECTOR);
      this->SetComponentType(USHORT);
      break;
    case MET_INT:
      this->SetPixelType(SCALAR);
      if (sizeof(int) == MET_ValueTypeSize[MET_INT])
      {
        this->SetComponentType(INT);
      }
      else if (sizeof(long) == MET_ValueTypeSize[MET_INT])
      {
        this->SetComponentType(LONG);
      }
      break;
    case MET_INT_ARRAY:
      this->SetPixelType(VECTOR);
      if (sizeof(int) == MET_ValueTypeSize[MET_INT])
      {
        this->SetComponentType(INT);
      }
      else if (sizeof(long) == MET_ValueTypeSize[MET_INT])
      {
        this->SetComponentType(LONG);
      }
      break;
    case MET_UINT:
      this->SetPixelType(SCALAR);
      if (sizeof(unsigned int) == MET_ValueTypeSize[MET_UINT])
      {
        this->SetComponentType(UINT);
      }
      else if (sizeof(unsigned long) == MET_ValueTypeSize[MET_UINT])
      {
        this->SetComponentType(ULONG);
      }
      break;
    case MET_UINT_ARRAY:
      this->SetPixelType(VECTOR);
      if (sizeof(int) == MET_ValueTypeSize[MET_INT])
      {
        this->SetComponentType(UINT);
      }
      else if (sizeof(long) == MET_ValueTypeSize[MET_INT])
      {
        this->SetComponentType(ULONG);
      }
      break;
    case MET_LONG:
      this->SetPixelType(SCALAR);
      if (sizeof(long) == MET_ValueTypeSize[MET_LONG])
      {
        this->SetComponentType(LONG);
      }
      else if (sizeof(int) == MET_ValueTypeSize[MET_LONG])
      {
        this->SetComponentType(INT);
      }
      break;
    case MET_LONG_ARRAY:
      this->SetPixelType(VECTOR);
      if (sizeof(long) == MET_ValueTypeSize[MET_LONG])
      {
        this->SetComponentType(LONG);
      }
      else if (sizeof(int) == MET_ValueTypeSize[MET_LONG])
      {
        this->SetComponentType(INT);
      }
      break;
    case MET_ULONG:
      this->SetPixelType(SCALAR);
      if (sizeof(unsigned long) == MET_ValueTypeSize[MET_ULONG])
      {
        this->SetComponentType(ULONG);
      }
      else if (sizeof(unsigned int) == MET_ValueTypeSize[MET_ULONG])
      {
        this->SetComponentType(UINT);
      }
      break;
    case MET_ULONG_ARRAY:
      this->SetPixelType(VECTOR);
      if (sizeof(unsigned long) == MET_ValueTypeSize[MET_ULONG])
      {
        this->SetComponentType(ULONG);
      }
      else if (sizeof(unsigned int) == MET_ValueTypeSize[MET_ULONG])
      {
        this->SetComponentType(UINT);
      }
      break;
    case MET_LONG_LONG:
      this->SetPixelType(SCALAR);
      if (sizeof(long) == MET_ValueTypeSize[MET_LONG_LONG])
      {
        this->SetComponentType(LONG);
      }
      else if (sizeof(int) == MET_ValueTypeSize[MET_LONG_LONG])
      {
        this->SetComponentType(INT);
      }
      else
      {
        this->SetComponentType(UNKNOWNCOMPONENTTYPE);
      }
      break;
    case MET_LONG_LONG_ARRAY:
      this->SetPixelType(VECTOR);
      if (sizeof(long) == MET_ValueTypeSize[MET_LONG_LONG])
      {
        this->SetComponentType(LONG);
      }
      else if (sizeof(int) == MET_ValueTypeSize[MET_LONG_LONG])
      {
        this->SetComponentType(INT);
      }
      else
      {
        this->SetComponentType(UNKNOWNCOMPONENTTYPE);
      }
      break;
    case MET_ULONG_LONG:
      this->SetPixelType(SCALAR);
      if (sizeof(unsigned long) == MET_ValueTypeSize[MET_ULONG_LONG])
      {
        this->SetComponentType(ULONG);
      }
      else if (sizeof(unsigned int) == MET_ValueTypeSize[MET_ULONG_LONG])
      {
        this->SetComponentType(UINT);
      }
      else
      {
        this->SetComponentType(UNKNOWNCOMPONENTTYPE);
      }
      break;
    case MET_ULONG_LONG_ARRAY:
      this->SetPixelType(VECTOR);
      if (sizeof(unsigned long) == MET_ValueTypeSize[MET_ULONG_LONG])
      {
        this->SetComponentType(ULONG);
      }
      else if (sizeof(unsigned int) == MET_ValueTypeSize[MET_ULONG_LONG])
      {
        this->SetComponentType(UINT);
      }
      else
      {
        this->SetComponentType(UNKNOWNCOMPONENTTYPE);
      }
      break;
    case MET_FLOAT:
      this->SetPixelType(SCALAR);
      if (sizeof(float) == MET_ValueTypeSize[MET_FLOAT])
      {
        this->SetComponentType(FLOAT);
      }
      else if (sizeof(double) == MET_ValueTypeSize[MET_FLOAT])
      {
        this->SetComponentType(DOUBLE);
      }
      break;
    case MET_FLOAT_ARRAY:
      this->SetPixelType(VECTOR);
      if (sizeof(float) == MET_ValueTypeSize[MET_FLOAT])
      {
        this->SetComponentType(FLOAT);
      }
      else if (sizeof(double) == MET_ValueTypeSize[MET_FLOAT])
      {
        this->SetComponentType(DOUBLE);
      }
      break;
    case MET_DOUBLE:
      this->SetPixelType(SCALAR);
      this->SetComponentType(DOUBLE);
      if (sizeof(double) == MET_ValueTypeSize[MET_DOUBLE])
      {
        this->SetComponentType(DOUBLE);
      }
      else if (sizeof(float) == MET_ValueTypeSize[MET_DOUBLE])
      {
        this->SetComponentType(FLOAT);
      }
      break;
    case MET_DOUBLE_ARRAY:
      this->SetPixelType(VECTOR);
      if (sizeof(double) == MET_ValueTypeSize[MET_DOUBLE])
      {
        this->SetComponentType(DOUBLE);
      }
      else if (sizeof(float) == MET_ValueTypeSize[MET_DOUBLE])
      {
        this->SetComponentType(FLOAT);
      }
      break;
    case MET_FLOAT_MATRIX:
      this->SetPixelType(VECTOR);
      if (sizeof(float) == MET_ValueTypeSize[MET_FLOAT])
      {
        this->SetComponentType(FLOAT);
      }
      else if (sizeof(double) == MET_ValueTypeSize[MET_FLOAT])
      {
        this->SetComponentType(DOUBLE);
      }
      this->SetNumberOfComponents(m_NumberOfComponents * m_NumberOfComponents);
      break;
  }

  // BUG: 8732
  // The above use to MET_*_ARRAY may not be correct, as this ScancoIO
  // ElementType was not designed to indicate vectors, but something
  // else
  //
  // if the file has multiple components then we default to a vector
  // pixel type, support could be added to ScancoIO format to define
  // different pixel types
  if (m_ScancoImage.ElementNumberOfChannels() > 1)
  {
    this->SetPixelType(VECTOR);
  }

  this->SetNumberOfDimensions(m_ScancoImage.NDims());

  unsigned int i;
  for (i = 0; i < m_NumberOfDimensions; i++)
  {
    this->SetDimensions(i, m_ScancoImage.DimSize(i) / m_SubSamplingFactor);
    this->SetSpacing(i, m_ScancoImage.ElementSpacing(i) * m_SubSamplingFactor);
    this->SetOrigin(i, m_ScancoImage.Position(i));
  }

  //
  // Read direction cosines
  //
  const double *     transformMatrix = m_ScancoImage.TransformMatrix();
  vnl_vector<double> directionAxis(this->GetNumberOfDimensions());
  for (unsigned int ii = 0; ii < this->GetNumberOfDimensions(); ii++)
  {
    for (unsigned int jj = 0; jj < this->GetNumberOfDimensions(); jj++)
    {
      directionAxis[jj] = transformMatrix[ii * this->GetNumberOfDimensions() + jj];
    }
    this->SetDirection(ii, directionAxis);
  }

  std::string classname(this->GetNameOfClass());
  EncapsulateScancoData<std::string>(thisScancoDict, ITK_InputFilterName, classname);
  //
  // save the metadatadictionary in the ScancoImage header.
  // NOTE: The ScancoIO library only supports typeless strings as metadata
  int dictFields = m_ScancoImage.GetNumberOfAdditionalReadFields();
  for (int f = 0; f < dictFields; f++)
  {
    std::string key(m_ScancoImage.GetAdditionalReadFieldName(f));
    std::string value(m_ScancoImage.GetAdditionalReadFieldValue(f));
    EncapsulateScancoData<std::string>(thisScancoDict, key, value);
  }

  //
  // Read some metadata
  //
  ScancoDataDictionary & metaDict = this->GetScancoDataDictionary();

  // Look at default metaio fields
  if (m_ScancoImage.DistanceUnits() != MET_DISTANCE_UNITS_UNKNOWN)
  {
    EncapsulateScancoData<std::string>(metaDict, ITK_VoxelUnits, std::string(m_ScancoImage.DistanceUnitsName()));
  }

  if (strlen(m_ScancoImage.AcquisitionDate()) > 0)
  {
    EncapsulateScancoData<std::string>(metaDict, ITK_ExperimentDate, std::string(m_ScancoImage.AcquisitionDate()));
  }
}

void
ScancoImageIO::Read(void * buffer)
{
  const unsigned int nDims = this->GetNumberOfDimensions();

  // this will check to see if we are actually streaming
  // we initialize with the dimensions of the file, since if
  // largestRegion and ioRegion don't match, we'll use the streaming
  // path since the comparison will fail
  ImageIORegion largestRegion(nDims);

  for (unsigned int i = 0; i < nDims; i++)
  {
    largestRegion.SetIndex(i, 0);
    largestRegion.SetSize(i, this->GetDimensions(i));
  }

  if (largestRegion != m_IORegion)
  {
    int * indexMin = new int[nDims];
    int * indexMax = new int[nDims];
    for (unsigned int i = 0; i < nDims; i++)
    {
      if (i < m_IORegion.GetImageDimension())
      {
        indexMin[i] = m_IORegion.GetIndex()[i];
        indexMax[i] = indexMin[i] + m_IORegion.GetSize()[i] - 1;
      }
      else
      {
        indexMin[i] = 0;
        // this is zero since this is a (size - 1)
        indexMax[i] = 0;
      }
    }

    if (!m_ScancoImage.ReadROI(indexMin, indexMax, m_FileName.c_str(), true, buffer, m_SubSamplingFactor))
    {
      delete[] indexMin;
      delete[] indexMax;
      itkExceptionMacro("File cannot be read: " << this->GetFileName() << " for reading." << std::endl
                                                << "Reason: " << itksys::SystemTools::GetLastSystemError());
    }

    delete[] indexMin;
    delete[] indexMax;

    m_ScancoImage.ElementByteOrderFix(m_IORegion.GetNumberOfPixels());
  }
  else
  {
    if (!m_ScancoImage.Read(m_FileName.c_str(), true, buffer))
    {
      itkExceptionMacro("File cannot be read: " << this->GetFileName() << " for reading." << std::endl
                                                << "Reason: " << itksys::SystemTools::GetLastSystemError());
    }

    // since we are not streaming m_IORegion may not be set, so
    m_ScancoImage.ElementByteOrderFix(this->GetImageSizeInPixels());
  }
}

ScancoImage *
ScancoImageIO::GetScancoImagePointer(void)
{
  return &m_ScancoImage;
}

bool
ScancoImageIO::CanWriteFile(const char * name)
{
  std::string filename = name;

  if (filename == "")
  {
    return false;
  }

  std::string::size_type mhaPos = filename.rfind(".mha");
  if ((mhaPos != std::string::npos) && (mhaPos == filename.length() - 4))
  {
    return true;
  }

  std::string::size_type mhdPos = filename.rfind(".mhd");
  if ((mhdPos != std::string::npos) && (mhdPos == filename.length() - 4))
  {
    return true;
  }

  return false;
}

void
ScancoImageIO ::WriteImageInformation(void)
{
  ScancoDataDictionary & metaDict = this->GetScancoDataDictionary();
  std::string            metaDataStr;

  // Look at default metaio fields
  if (ExposeScancoData<std::string>(metaDict, ITK_VoxelUnits, metaDataStr))
  {
    // Handle analyze style unit string
    if (metaDataStr == "um. ")
    {
      m_ScancoImage.DistanceUnits(MET_DISTANCE_UNITS_UM);
    }
    else if (metaDataStr == "mm. ")
    {
      m_ScancoImage.DistanceUnits(MET_DISTANCE_UNITS_MM);
    }
    else if (metaDataStr == "cm. ")
    {
      m_ScancoImage.DistanceUnits(MET_DISTANCE_UNITS_CM);
    }
    else
    {
      m_ScancoImage.DistanceUnits(metaDataStr.c_str());
    }
  }

  if (ExposeScancoData<std::string>(metaDict, ITK_ExperimentDate, metaDataStr))
  {
    m_ScancoImage.AcquisitionDate(metaDataStr.c_str());
  }

  // Save out the metadatadictionary key/value pairs as part of
  // the metaio header.
  std::vector<std::string>                 keys = metaDict.GetKeys();
  std::vector<std::string>::const_iterator keyIt;
  for (keyIt = keys.begin(); keyIt != keys.end(); ++keyIt)
  {
    if (*keyIt == ITK_ExperimentDate || *keyIt == ITK_VoxelUnits)
    {
      continue;
    }
    // try for common scalar types
    std::ostringstream strs;
    double             dval = 0.0;
    float              fval = 0.0F;
    long               lval = 0L;
    unsigned long      ulval = 0L;
    int                ival = 0;
    unsigned           uval = 0;
    short              shval = 0;
    unsigned short     ushval = 0;
    char               cval = 0;
    unsigned char      ucval = 0;
    bool               bval = false;
    std::string        value = "";
    if (ExposeScancoData<std::string>(metaDict, *keyIt, value))
    {
      strs << value;
    }
    else if (ExposeScancoData<double>(metaDict, *keyIt, dval))
    {
      strs << dval;
    }
    else if (ExposeScancoData<float>(metaDict, *keyIt, fval))
    {
      strs << fval;
    }
    else if (ExposeScancoData<long>(metaDict, *keyIt, lval))
    {
      strs << lval;
    }
    else if (ExposeScancoData<unsigned long>(metaDict, *keyIt, ulval))
    {
      strs << ulval;
    }
    else if (ExposeScancoData<int>(metaDict, *keyIt, ival))
    {
      strs << ival;
    }
    else if (ExposeScancoData<unsigned int>(metaDict, *keyIt, uval))
    {
      strs << uval;
    }
    else if (ExposeScancoData<short>(metaDict, *keyIt, shval))
    {
      strs << shval;
    }
    else if (ExposeScancoData<unsigned short>(metaDict, *keyIt, ushval))
    {
      strs << ushval;
    }
    else if (ExposeScancoData<char>(metaDict, *keyIt, cval))
    {
      strs << cval;
    }
    else if (ExposeScancoData<unsigned char>(metaDict, *keyIt, ucval))
    {
      strs << ucval;
    }
    else if (ExposeScancoData<bool>(metaDict, *keyIt, bval))
    {
      strs << bval;
    }

    value = strs.str();

    if (value == "")
    {
      // if the value is an empty string then the resulting entry in
      // the header will not be able to be read the the metaIO
      // library, which results is a unreadable/corrupt file.
      itkWarningMacro("Unsupported or empty metaData item " << *keyIt << " of type "
                                                            << metaDict[*keyIt]->GetScancoDataObjectTypeName()
                                                            << "found, won't be written to image file");

      // so this entry should be skipped.
      continue;
    }

    // Rolling this back out so that the tests pass.
    // The meta image AddUserField requires control of the memory space.
    m_ScancoImage.AddUserField((*keyIt).c_str(), MET_STRING, static_cast<int>(value.size()), value.c_str(), true, -1);
  }
}

/**
 *
 */
void
ScancoImageIO ::Write(const void * buffer)
{
  const unsigned int numberOfDimensions = this->GetNumberOfDimensions();

  bool binaryData = true;

  if (this->GetFileType() == ASCII)
  {
    binaryData = false;
  }

  int nChannels = this->GetNumberOfComponents();

  MET_ValueEnumType eType = MET_OTHER;
  switch (m_ComponentType)
  {
    default:
    case UNKNOWNCOMPONENTTYPE:
      eType = MET_OTHER;
      break;
    case CHAR:
      eType = MET_CHAR;
      break;
    case UCHAR:
      eType = MET_UCHAR;
      break;
    case SHORT:
      eType = MET_SHORT;
      break;
    case USHORT:
      eType = MET_USHORT;
      break;
    case LONG:
      if (sizeof(long) == MET_ValueTypeSize[MET_LONG])
      {
        eType = MET_LONG;
      }
      else if (sizeof(long) == MET_ValueTypeSize[MET_INT])
      {
        eType = MET_INT;
      }
      else if (sizeof(long) == MET_ValueTypeSize[MET_LONG_LONG])
      {
        eType = MET_LONG_LONG;
      }
      break;
    case ULONG:
      if (sizeof(long) == MET_ValueTypeSize[MET_LONG])
      {
        eType = MET_ULONG;
      }
      else if (sizeof(long) == MET_ValueTypeSize[MET_INT])
      {
        eType = MET_UINT;
      }
      else if (sizeof(long) == MET_ValueTypeSize[MET_LONG_LONG])
      {
        eType = MET_ULONG_LONG;
      }
      break;
    case INT:
      eType = MET_INT;
      if (sizeof(int) == MET_ValueTypeSize[MET_INT])
      {
        eType = MET_INT;
      }
      else if (sizeof(int) == MET_ValueTypeSize[MET_LONG])
      {
        eType = MET_LONG;
      }
      break;
    case UINT:
      if (sizeof(int) == MET_ValueTypeSize[MET_INT])
      {
        eType = MET_UINT;
      }
      else if (sizeof(int) == MET_ValueTypeSize[MET_LONG])
      {
        eType = MET_ULONG;
      }
      break;
    case FLOAT:
      if (sizeof(float) == MET_ValueTypeSize[MET_FLOAT])
      {
        eType = MET_FLOAT;
      }
      else if (sizeof(float) == MET_ValueTypeSize[MET_DOUBLE])
      {
        eType = MET_DOUBLE;
      }
      break;
    case DOUBLE:
      if (sizeof(double) == MET_ValueTypeSize[MET_DOUBLE])
      {
        eType = MET_DOUBLE;
      }
      else if (sizeof(double) == MET_ValueTypeSize[MET_FLOAT])
      {
        eType = MET_FLOAT;
      }
      break;
  }

  int *    dSize = new int[numberOfDimensions];
  float *  eSpacing = new float[numberOfDimensions];
  double * eOrigin = new double[numberOfDimensions];
  for (unsigned int ii = 0; ii < numberOfDimensions; ++ii)
  {
    dSize[ii] = this->GetDimensions(ii);
    eSpacing[ii] = static_cast<float>(this->GetSpacing(ii));
    eOrigin[ii] = this->GetOrigin(ii);
  }

  m_ScancoImage.InitializeEssential(numberOfDimensions, dSize, eSpacing, eType, nChannels, const_cast<void *>(buffer));
  m_ScancoImage.Position(eOrigin);
  m_ScancoImage.BinaryData(binaryData);

  // Write the image Information
  this->WriteImageInformation();

  if (numberOfDimensions == 3)
  {
    SpatialOrientation::ValidCoordinateOrientationFlags coordOrient =
      SpatialOrientation::ITK_COORDINATE_ORIENTATION_INVALID;
    std::vector<double>                      dirx, diry, dirz;
    SpatialOrientationAdapter::DirectionType dir;
    dirx = this->GetDirection(0);
    diry = this->GetDirection(1);
    dirz = this->GetDirection(2);
    for (unsigned ii = 0; ii < 3; ii++)
    {
      dir[ii][0] = dirx[ii];
      dir[ii][1] = diry[ii];
      dir[ii][2] = dirz[ii];
    }
    coordOrient = SpatialOrientationAdapter().FromDirectionCosines(dir);

    switch (coordOrient)
    {
      default:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSP:
      {
        m_ScancoImage.AnatomicalOrientation(0, MET_ORIENTATION_RL);
        break;
      }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSP:
      {
        m_ScancoImage.AnatomicalOrientation(0, MET_ORIENTATION_LR);
        break;
      }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASR:
      {
        m_ScancoImage.AnatomicalOrientation(0, MET_ORIENTATION_AP);
        break;
      }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSR:
      {
        m_ScancoImage.AnatomicalOrientation(0, MET_ORIENTATION_PA);
        break;
      }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRP:
      {
        m_ScancoImage.AnatomicalOrientation(0, MET_ORIENTATION_IS);
        break;
      }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRP:
      {
        m_ScancoImage.AnatomicalOrientation(0, MET_ORIENTATION_SI);
        break;
      }
    }
    switch (coordOrient)
    {
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRP:
      {
        m_ScancoImage.AnatomicalOrientation(1, MET_ORIENTATION_RL);
        break;
      }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLP:
      {
        m_ScancoImage.AnatomicalOrientation(1, MET_ORIENTATION_LR);
        break;
      }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAR:
      {
        m_ScancoImage.AnatomicalOrientation(1, MET_ORIENTATION_AP);
        break;
      }
      default:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPR:
      {
        m_ScancoImage.AnatomicalOrientation(1, MET_ORIENTATION_PA);
        break;
      }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP:
      {
        m_ScancoImage.AnatomicalOrientation(1, MET_ORIENTATION_IS);
        break;
      }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSP:
      {
        m_ScancoImage.AnatomicalOrientation(1, MET_ORIENTATION_SI);
        break;
      }
    }
    switch (coordOrient)
    {
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAR:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPR:
      {
        m_ScancoImage.AnatomicalOrientation(2, MET_ORIENTATION_RL);
        break;
      }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAL:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPL:
      {
        m_ScancoImage.AnatomicalOrientation(2, MET_ORIENTATION_LR);
        break;
      }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLA:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRA:
      {
        m_ScancoImage.AnatomicalOrientation(2, MET_ORIENTATION_AP);
        break;
      }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLP:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRP:
      {
        m_ScancoImage.AnatomicalOrientation(2, MET_ORIENTATION_PA);
        break;
      }
      default:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAI:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI:
      {
        m_ScancoImage.AnatomicalOrientation(2, MET_ORIENTATION_IS);
        break;
      }
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAS:
      case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPS:
      {
        m_ScancoImage.AnatomicalOrientation(2, MET_ORIENTATION_SI);
        break;
      }
    }
  }
  // Propagage direction cosine information.
  double * transformMatrix = static_cast<double *>(malloc(numberOfDimensions * numberOfDimensions * sizeof(double)));
  if (transformMatrix)
  {
    for (unsigned int ii = 0; ii < numberOfDimensions; ++ii)
    {
      for (unsigned int jj = 0; jj < numberOfDimensions; ++jj)
      {
        transformMatrix[ii * numberOfDimensions + jj] = this->GetDirection(ii)[jj];
      }
    }
    m_ScancoImage.TransformMatrix(transformMatrix);
    free(transformMatrix);
  }

  m_ScancoImage.CompressedData(m_UseCompression);

  // this is a check to see if we are actually streaming
  // we initialize with m_IORegion to match dimensions
  ImageIORegion largestRegion(m_IORegion);
  for (unsigned int ii = 0; ii < numberOfDimensions; ++ii)
  {
    largestRegion.SetIndex(ii, 0);
    largestRegion.SetSize(ii, this->GetDimensions(ii));
  }

  if (m_UseCompression && (largestRegion != m_IORegion))
  {
    std::cout << "Compression in use: cannot stream the file writing" << std::endl;
  }
  else if (largestRegion != m_IORegion)
  {
    int * indexMin = new int[numberOfDimensions];
    int * indexMax = new int[numberOfDimensions];
    for (unsigned int ii = 0; ii < numberOfDimensions; ++ii)
    {
      // the dimensions of m_IORegion should match out requested
      // dimensions, but ImageIORegion will throw an
      // exception if out of bounds
      indexMin[ii] = m_IORegion.GetIndex()[ii];
      indexMax[ii] = m_IORegion.GetIndex()[ii] + m_IORegion.GetSize()[ii] - 1;
    }

    if (!m_ScancoImage.WriteROI(indexMin, indexMax, m_FileName.c_str()))
    {
      delete[] dSize;
      delete[] eSpacing;
      delete[] eOrigin;
      delete[] indexMin;
      delete[] indexMax;
      itkExceptionMacro("File ROI cannot be written: " << this->GetFileName() << std::endl
                                                       << "Reason: " << itksys::SystemTools::GetLastSystemError());
    }

    delete[] indexMin;
    delete[] indexMax;
  }
  else
  {
    if (!m_ScancoImage.Write(m_FileName.c_str()))
    {
      delete[] dSize;
      delete[] eSpacing;
      delete[] eOrigin;
      itkExceptionMacro("File cannot be written: " << this->GetFileName() << std::endl
                                                   << "Reason: " << itksys::SystemTools::GetLastSystemError());
    }
  }

  delete[] dSize;
  delete[] eSpacing;
  delete[] eOrigin;
}

/** Given a requested region, determine what could be the region that we can
 * read from the file. This is called the streamable region, which will be
 * smaller than the LargestPossibleRegion and greater or equal to the
 * RequestedRegion */
ImageIORegion
ScancoImageIO ::GenerateStreamableReadRegionFromRequestedRegion(const ImageIORegion & requestedRegion) const
{
  //
  // The default implementations determines that the streamable region is
  // equal to the largest possible region of the image.
  //
  ImageIORegion streamableRegion(this->m_NumberOfDimensions);

  if (!m_UseStreamedReading)
  {
    for (unsigned int i = 0; i < this->m_NumberOfDimensions; i++)
    {
      streamableRegion.SetSize(i, this->m_Dimensions[i]);
      streamableRegion.SetIndex(i, 0);
    }
  }
  else
  {
    streamableRegion = requestedRegion;
  }

  return streamableRegion;
}

unsigned int
ScancoImageIO::GetActualNumberOfSplitsForWriting(unsigned int          numberOfRequestedSplits,
                                                 const ImageIORegion & pasteRegion,
                                                 const ImageIORegion & largestPossibleRegion)
{
  if (this->GetUseCompression())
  {
    // we can not stream or paste with compression
    if (pasteRegion != largestPossibleRegion)
    {
      itkExceptionMacro("Pasting and compression is not supported! Can't write:" << this->GetFileName());
    }
    else if (numberOfRequestedSplits != 1)
    {
      itkDebugMacro("Requested streaming and compression");
      itkDebugMacro("Scanco IO is not streaming now!");
    }
    return 1;
  }

  if (!itksys::SystemTools::FileExists(m_FileName.c_str()))
  {
    // file doesn't exits so we don't have potential problems
  }
  else if (pasteRegion != largestPossibleRegion)
  {
    // we are going to be pasting (may be streaming too)

    // need to check to see if the file is compatible
    std::string errorMessage;
    Pointer     headerImageIOReader = Self::New();

    try
    {
      headerImageIOReader->SetFileName(m_FileName.c_str());
      headerImageIOReader->ReadImageInformation();
    }
    catch (...)
    {
      errorMessage = "Unable to read information from file: " + m_FileName;
    }

    // we now need to check that the following match:
    // 1)file is not compressed
    // 2)pixel type
    // 3)dimensions
    // 4)size/origin/spacing
    // 5)direction cosines
    //

    if (errorMessage.size())
    {
      // 0) Can't read file
    }
    // 1)file is not compressed
    else if (headerImageIOReader->m_ScancoImage.CompressedData())
    {
      errorMessage = "File is compressed: " + m_FileName;
    }
    // 2)pixel type
    // this->GetPixelType() is not verified because the metaio file format
    // stores all multi-component types as arrays, so it does not
    // distinguish between pixel types. Also as long as the compoent
    // and number of compoents match we should be able to paste, that
    // is the numbers should be the same it is just the interpretation
    // that is not matching
    else if (headerImageIOReader->GetNumberOfComponents() != this->GetNumberOfComponents() ||
             headerImageIOReader->GetComponentType() != this->GetComponentType())
    {
      errorMessage = "Component type does not match in file: " + m_FileName;
    }
    // 3)dimensions/size
    else if (headerImageIOReader->GetNumberOfDimensions() != this->GetNumberOfDimensions())
    {
      errorMessage = "Dimensions does not match in file: " + m_FileName;
    }
    else
    {
      for (unsigned int i = 0; i < this->GetNumberOfDimensions(); ++i)
      {
        // 4)size/origin/spacing
        if (headerImageIOReader->GetDimensions(i) != this->GetDimensions(i) ||
            Math::NotExactlyEquals(headerImageIOReader->GetSpacing(i), this->GetSpacing(i)) ||
            Math::NotExactlyEquals(headerImageIOReader->GetOrigin(i), this->GetOrigin(i)))
        {
          errorMessage = "Size, spacing or origin does not match in file: " + m_FileName;
          break;
        }
        // 5)direction cosines
        if (headerImageIOReader->GetDirection(i) != this->GetDirection(i))
        {
          errorMessage = "Direction cosines does not match in file: " + m_FileName;
          break;
        }
      }
    }

    if (errorMessage.size())
    {
      itkExceptionMacro("Unable to paste because pasting file exists and is different. " << errorMessage);
    }
    else if (headerImageIOReader->GetPixelType() != this->GetPixelType())
    {
      // since there is currently poor support for pixel types in
      // ScancoIO we will just warn when it does not match
      itkWarningMacro("Pixel types does not match file, but component type and number of components do.");
    }
  }
  else if (numberOfRequestedSplits != 1)
  {
    // we are going be streaming

    // need to remove the file incase the file doesn't match our
    // current header/meta data information
    if (!itksys::SystemTools::RemoveFile(m_FileName.c_str()))
    {
      itkExceptionMacro("Unable to remove file for streaming: " << m_FileName);
    }
  }

  return GetActualNumberOfSplitsForWritingCanStreamWrite(numberOfRequestedSplits, pasteRegion);
}

ImageIORegion
ScancoImageIO::GetSplitRegionForWriting(unsigned int          ithPiece,
                                        unsigned int          numberOfActualSplits,
                                        const ImageIORegion & pasteRegion,
                                        const ImageIORegion & itkNotUsed(largestPossibleRegion))
{
  return GetSplitRegionForWritingCanStreamWrite(ithPiece, numberOfActualSplits, pasteRegion);
}
} // end namespace itk
