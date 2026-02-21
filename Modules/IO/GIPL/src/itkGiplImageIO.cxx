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
#include "itkGiplImageIO.h"
#include "itkByteSwapper.h"
#include "itkMakeUniqueForOverwrite.h"
#include <iostream>
#include "itk_zlib.h"

namespace itk
{
class GiplImageIOInternals
{
public:
  gzFile m_GzFile;
};

/*  IMAGE TYPE DEFINITIONS  */

#define GIPL_BINARY 1
#define GIPL_CHAR 7
#define GIPL_U_CHAR 8
#define GIPL_SHORT 15
#define GIPL_U_SHORT 16
#define GIPL_U_INT 31
#define GIPL_INT 32
#define GIPL_FLOAT 64
#define GIPL_DOUBLE 65
// #define GIPL_C_SHORT 144
// #define GIPL_C_INT 160
// #define GIPL_C_FLOAT 192
// #define GIPL_C_DOUBLE 193
// #define GIPL_SURFACE 200
// #define GIPL_POLYGON 201

/*  ORIENTATION DEFINITIONS (flag1)  */

// #define UNDEFINED 0
// #define UNDEFINED_PROJECTION 1
// #define AP_PROJECTION 2
// #define LATERAL_PROJECTION 3
// #define OBLIQUE_PROJECTION 4
// #define UNDEFINED_TOMO 8
// #define AXIAL 9
// #define CORONAL 10
// #define SAGITTAL 11
// #define OBLIQUE_TOMO 12

/*  FORMAT DEFINITIONS  */

// #define FORMAT_GIPL 0
// #define FORMAT_GIPL_STRING "Gipl"
// #define FORMAT_MAYO 1
// #define FORMAT_MAYO_STRING "Mayo"
// #define FORMAT_NM_IGE 2
// #define FORMAT_NM_IGE_STRING "Starcam"

#define GIPL_MAGIC_NUMBER 0xefffe9b0
#define GIPL_MAGIC_NUMBER2 0x2ae389b8

GiplImageIO::GiplImageIO()
  : m_Internal(std::make_unique<GiplImageIOInternals>())
{
  m_ByteOrder = IOByteOrderEnum::BigEndian;
}

GiplImageIO::~GiplImageIO()
{
  if (m_IsCompressed)
  {
    if (m_Internal->m_GzFile != nullptr)
    {
      gzclose(m_Internal->m_GzFile);
      m_Internal->m_GzFile = nullptr;
    }
  }
  else
  {
    m_Ifstream.close();
  }
}

bool
GiplImageIO::CanReadFile(const char * filename)
{
  // First check the filename extension
  const bool extensionFound = CheckExtension(filename);

  if (!extensionFound)
  {
    itkDebugMacro("The filename extension is not recognized");
    return false;
  }

  // Now check the content
  if (m_IsCompressed == false)
  {
    std::ifstream inputStream;
    try
    {
      this->OpenFileForReading(inputStream, filename);
    }
    catch (const ExceptionObject &)
    {
      return false;
    }

    inputStream.seekg(252);
    unsigned int magic_number = 0;
    inputStream.read(reinterpret_cast<char *>(&magic_number), static_cast<std::streamsize>(sizeof(unsigned int)));

    if (m_ByteOrder == IOByteOrderEnum::BigEndian)
    {
      ByteSwapper<unsigned int>::SwapFromSystemToBigEndian(&magic_number);
    }
    else if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
    {
      ByteSwapper<unsigned int>::SwapFromSystemToLittleEndian(&magic_number);
    }

    if ((magic_number == GIPL_MAGIC_NUMBER) || (magic_number == GIPL_MAGIC_NUMBER2))
    {
      inputStream.close();
      return true;
    }

    inputStream.close();
  }
  else
  {
    m_Internal->m_GzFile = gzopen(filename, "rb");
    if (m_Internal->m_GzFile == nullptr)
    {
      return false;
    }

    gzseek(m_Internal->m_GzFile, 252, SEEK_SET);
    unsigned int magic_number = 0;
    gzread(
      m_Internal->m_GzFile, reinterpret_cast<char *>(&magic_number), static_cast<unsigned int>(sizeof(unsigned int)));

    if (m_ByteOrder == IOByteOrderEnum::BigEndian)
    {
      ByteSwapper<unsigned int>::SwapFromSystemToBigEndian(&magic_number);
    }
    else if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
    {
      ByteSwapper<unsigned int>::SwapFromSystemToLittleEndian(&magic_number);
    }

    if ((magic_number == GIPL_MAGIC_NUMBER) || (magic_number == GIPL_MAGIC_NUMBER2))
    {
      gzclose(m_Internal->m_GzFile);
      m_Internal->m_GzFile = nullptr;
      return true;
    }
    gzclose(m_Internal->m_GzFile);
    m_Internal->m_GzFile = nullptr;
  }
  return false;
}

bool
GiplImageIO::CanWriteFile(const char * name)
{
  const std::string filename = name;

  if (filename.empty())
  {
    itkDebugMacro("No filename specified.");
  }

  const bool extensionFound = CheckExtension(name);

  if (!extensionFound)
  {
    itkDebugMacro("The filename extension is not recognized");
    return false;
  }

  return true;
}

void
GiplImageIO::Read(void * buffer)
{
  const uint32_t dimensions = this->GetNumberOfDimensions();
  uint32_t       numberOfPixels = 1;

  for (unsigned int dim = 0; dim < dimensions; ++dim)
  {
    numberOfPixels *= static_cast<uint32_t>(m_Dimensions[dim]);
  }

  auto * p = static_cast<char *>(buffer);
  if (m_IsCompressed)
  {
    gzread(m_Internal->m_GzFile, p, static_cast<unsigned int>(this->GetImageSizeInBytes()));
  }
  else
  {
    m_Ifstream.read(p, static_cast<std::streamsize>(this->GetImageSizeInBytes()));
  }

  bool success = false;
  if (m_IsCompressed)
  {
    success = p != nullptr;
  }
  else
  {
    success = !m_Ifstream.bad();
  }

  if (m_IsCompressed)
  {
    gzclose(m_Internal->m_GzFile);
    m_Internal->m_GzFile = nullptr;
  }
  else
  {
    m_Ifstream.close();
  }
  if (!success)
  {
    itkExceptionStringMacro("Error reading image data.");
  }

  SwapBytesIfNecessary(buffer, numberOfPixels);
}

void
GiplImageIO::ReadImageInformation()
{
  CheckExtension(m_FileName.c_str());

  if (m_IsCompressed)
  {
    m_Internal->m_GzFile = gzopen(m_FileName.c_str(), "rb");
    if (m_Internal->m_GzFile == nullptr)
    {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("File cannot be read");
      throw exception;
    }
  }
  else
  {
    this->OpenFileForReading(m_Ifstream, m_FileName);
  }

  unsigned short dims[4];

  unsigned int numberofdimension = 0;
  for (unsigned short & dim : dims)
  {
    dim = 0;
  }

  for (unsigned int i = 0; i < 4; ++i)
  {
    if (m_IsCompressed)
    {
      gzread(
        m_Internal->m_GzFile, reinterpret_cast<char *>(&dims[i]), static_cast<unsigned int>(sizeof(unsigned short)));
    }
    else
    {
      m_Ifstream.read(reinterpret_cast<char *>(&dims[i]), sizeof(unsigned short));
    }
    if (m_ByteOrder == IOByteOrderEnum::BigEndian)
    {
      ByteSwapper<unsigned short>::SwapFromSystemToBigEndian(&dims[i]);
    }
    else if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
    {
      ByteSwapper<unsigned short>::SwapFromSystemToLittleEndian(&dims[i]);
    }

    if (dims[i] > 0)
    {
      if (i < 3)
      {
        ++numberofdimension;
      }
      else if (dims[i] > 1)
      {
        ++numberofdimension;
      }
    }
  }

  this->SetNumberOfDimensions(numberofdimension);

  for (unsigned int i = 0; i < numberofdimension; ++i)
  {
    m_Dimensions[i] = dims[i];
  }

  unsigned short image_type = 0;

  if (m_IsCompressed)
  {
    gzread(m_Internal->m_GzFile, reinterpret_cast<char *>(&image_type), sizeof(unsigned short));
  }
  else
  {
    m_Ifstream.read(reinterpret_cast<char *>(&image_type), sizeof(unsigned short));
  }

  if (m_ByteOrder == IOByteOrderEnum::BigEndian)
  {
    ByteSwapper<unsigned short>::SwapFromSystemToBigEndian(&image_type);
  }

  m_PixelType = IOPixelEnum::SCALAR;
  switch (image_type)
  {
    case GIPL_BINARY:
      m_ComponentType = IOComponentEnum::UCHAR;
      break;
    case GIPL_CHAR:
      m_ComponentType = IOComponentEnum::CHAR;
      break;
    case GIPL_U_CHAR:
      m_ComponentType = IOComponentEnum::UCHAR;
      break;
    case GIPL_SHORT:
      m_ComponentType = IOComponentEnum::SHORT;
      break;
    case GIPL_U_SHORT:
      m_ComponentType = IOComponentEnum::USHORT;
      break;
    case GIPL_U_INT:
      m_ComponentType = IOComponentEnum::UINT;
      break;
    case GIPL_INT:
      m_ComponentType = IOComponentEnum::INT;
      break;
    case GIPL_FLOAT:
      m_ComponentType = IOComponentEnum::FLOAT;
      break;
    case GIPL_DOUBLE:
      m_ComponentType = IOComponentEnum::DOUBLE;
      break;
  }

  float pixdim[4]; /*   10   16  X,Y,Z,T pixel dimensions mm */
  for (unsigned int i = 0; i < 4; ++i)
  {
    if (m_IsCompressed)
    {
      gzread(m_Internal->m_GzFile, reinterpret_cast<char *>(&pixdim[i]), sizeof(float));
    }
    else
    {
      m_Ifstream.read(reinterpret_cast<char *>(&pixdim[i]), sizeof(float));
    }
    if (m_ByteOrder == IOByteOrderEnum::BigEndian)
    {
      ByteSwapper<float>::SwapFromSystemToBigEndian(&pixdim[i]);
    }
    else if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
    {
      ByteSwapper<float>::SwapFromSystemToLittleEndian(&pixdim[i]);
    }

    if (i < numberofdimension)
    {
      m_Spacing[i] = pixdim[i];
    }
  }

  char line1[80]; /*   26   80  Patient / Text field        */
  for (char & it : line1)
  {
    if (m_IsCompressed)
    {
      gzread(m_Internal->m_GzFile, &it, static_cast<unsigned int>(sizeof(char)));
    }
    else
    {
      m_Ifstream.read(&it, sizeof(char));
    }
  }

  float matrix[20]; /*  106   80                              */
  for (float & it : matrix)
  {
    if (m_IsCompressed)
    {
      gzread(m_Internal->m_GzFile, reinterpret_cast<char *>(&it), static_cast<unsigned int>(sizeof(float)));
    }
    else
    {
      m_Ifstream.read(reinterpret_cast<char *>(&it), sizeof(float));
    }

    if (m_ByteOrder == IOByteOrderEnum::BigEndian)
    {
      ByteSwapper<float>::SwapFromSystemToBigEndian(&it);
    }
    else if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
    {
      ByteSwapper<float>::SwapFromSystemToLittleEndian(&it);
    }
  }

  char flag1 = 0; /*  186    1  Orientation flag (below)    */
  if (m_IsCompressed)
  {
    gzread(m_Internal->m_GzFile, &flag1, static_cast<unsigned int>(sizeof(char)));
  }
  else
  {
    m_Ifstream.read(&flag1, sizeof(char));
  }

  char flag2 = 0; /*  187    1                              */
  if (m_IsCompressed)
  {
    gzread(m_Internal->m_GzFile, &flag2, static_cast<unsigned int>(sizeof(char)));
  }
  else
  {
    m_Ifstream.read(&flag2, sizeof(char));
  }

  double min = NAN; /*  188    8  Minimum voxel value         */
  if (m_IsCompressed)
  {
    gzread(m_Internal->m_GzFile, reinterpret_cast<char *>(&min), static_cast<unsigned int>(sizeof(double)));
  }
  else
  {
    m_Ifstream.read(reinterpret_cast<char *>(&min), sizeof(double));
  }

  double max = NAN; /*  196    8  Maximum voxel value         */
  if (m_IsCompressed)
  {
    gzread(m_Internal->m_GzFile, reinterpret_cast<char *>(&max), static_cast<unsigned int>(sizeof(double)));
  }
  else
  {
    m_Ifstream.read(reinterpret_cast<char *>(&max), sizeof(double));
  }

  double origin[4]; /*  204   32  X,Y,Z,T offset              */
  for (unsigned int i = 0; i < 4; ++i)
  {
    if (m_IsCompressed)
    {
      gzread(m_Internal->m_GzFile, reinterpret_cast<char *>(&origin[i]), static_cast<unsigned int>(sizeof(double)));
    }
    else
    {
      m_Ifstream.read(reinterpret_cast<char *>(&origin[i]), sizeof(double));
    }

    if (m_ByteOrder == IOByteOrderEnum::BigEndian)
    {
      ByteSwapper<double>::SwapFromSystemToBigEndian(&origin[i]);
    }
    else if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
    {
      ByteSwapper<double>::SwapFromSystemToLittleEndian(&origin[i]);
    }

    if (i < numberofdimension)
    {
      m_Origin[i] = origin[i];
    }
  }

  float pixval_offset = NAN; /*  236    4                              */
  if (m_IsCompressed)
  {
    gzread(m_Internal->m_GzFile, reinterpret_cast<char *>(&pixval_offset), static_cast<unsigned int>(sizeof(float)));
  }
  else
  {
    m_Ifstream.read(reinterpret_cast<char *>(&pixval_offset), sizeof(float));
  }

  if (m_ByteOrder == IOByteOrderEnum::BigEndian)
  {
    ByteSwapper<float>::SwapFromSystemToBigEndian(&pixval_offset);
  }
  else if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
  {
    ByteSwapper<float>::SwapFromSystemToLittleEndian(&pixval_offset);
  }

  float pixval_cal = NAN; /*  240    4                              */
  if (m_IsCompressed)
  {
    gzread(m_Internal->m_GzFile, reinterpret_cast<char *>(&pixval_cal), static_cast<unsigned int>(sizeof(float)));
  }
  else
  {
    m_Ifstream.read(reinterpret_cast<char *>(&pixval_cal), sizeof(float));
  }

  if (m_ByteOrder == IOByteOrderEnum::BigEndian)
  {
    ByteSwapper<float>::SwapFromSystemToBigEndian(&pixval_cal);
  }
  else if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
  {
    ByteSwapper<float>::SwapFromSystemToLittleEndian(&pixval_cal);
  }

  float user_def1 = NAN; /*  244    4  Inter-slice Gap             */
  if (m_IsCompressed)
  {
    gzread(m_Internal->m_GzFile, reinterpret_cast<char *>(&user_def1), static_cast<unsigned int>(sizeof(float)));
  }
  else
  {
    m_Ifstream.read(reinterpret_cast<char *>(&user_def1), sizeof(float));
  }

  if (m_ByteOrder == IOByteOrderEnum::BigEndian)
  {
    ByteSwapper<float>::SwapFromSystemToBigEndian(&user_def1);
  }
  else if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
  {
    ByteSwapper<float>::SwapFromSystemToLittleEndian(&user_def1);
  }

  float user_def2 = NAN; /*  248    4  User defined field          */
  if (m_IsCompressed)
  {
    gzread(m_Internal->m_GzFile, reinterpret_cast<char *>(&user_def2), static_cast<unsigned int>(sizeof(float)));
  }
  else
  {
    m_Ifstream.read(reinterpret_cast<char *>(&user_def2), sizeof(float));
  }

  if (m_ByteOrder == IOByteOrderEnum::BigEndian)
  {
    ByteSwapper<float>::SwapFromSystemToBigEndian(&user_def2);
  }
  else if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
  {
    ByteSwapper<float>::SwapFromSystemToLittleEndian(&user_def2);
  }

  unsigned int magic_number = 0; /*  252    4 Magic Number                 */
  if (m_IsCompressed)
  {
    gzread(
      m_Internal->m_GzFile, reinterpret_cast<char *>(&magic_number), static_cast<unsigned int>(sizeof(unsigned int)));
  }
  else
  {
    m_Ifstream.read(reinterpret_cast<char *>(&magic_number), sizeof(unsigned int));
  }

  if (m_ByteOrder == IOByteOrderEnum::BigEndian)
  {
    ByteSwapper<unsigned int>::SwapFromSystemToBigEndian(&magic_number);
  }
  else if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
  {
    ByteSwapper<unsigned int>::SwapFromSystemToLittleEndian(&magic_number);
  }
}

void
GiplImageIO::SwapBytesIfNecessary(void * buffer, SizeValueType numberOfPixels)
{
  switch (m_ComponentType)
  {
    case IOComponentEnum::SCHAR:
    case IOComponentEnum::UCHAR:
    {
      // For CHAR and UCHAR, it is not necessary to swap bytes.
      break;
    }
    case IOComponentEnum::SHORT:
    {
      if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
      {
        ByteSwapper<short>::SwapRangeFromSystemToLittleEndian(static_cast<short *>(buffer), numberOfPixels);
      }
      else if (m_ByteOrder == IOByteOrderEnum::BigEndian)
      {
        ByteSwapper<short>::SwapRangeFromSystemToBigEndian(static_cast<short *>(buffer), numberOfPixels);
      }
      break;
    }
    case IOComponentEnum::USHORT:
    {
      if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
      {
        ByteSwapper<unsigned short>::SwapRangeFromSystemToLittleEndian(static_cast<unsigned short *>(buffer),
                                                                       numberOfPixels);
      }
      else if (m_ByteOrder == IOByteOrderEnum::BigEndian)
      {
        ByteSwapper<unsigned short>::SwapRangeFromSystemToBigEndian(static_cast<unsigned short *>(buffer),
                                                                    numberOfPixels);
      }
      break;
    }
    case IOComponentEnum::FLOAT:
    {
      if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
      {
        ByteSwapper<float>::SwapRangeFromSystemToLittleEndian(static_cast<float *>(buffer), numberOfPixels);
      }
      else if (m_ByteOrder == IOByteOrderEnum::BigEndian)
      {
        ByteSwapper<float>::SwapRangeFromSystemToBigEndian(static_cast<float *>(buffer), numberOfPixels);
      }
      break;
    }
    case IOComponentEnum::DOUBLE:
    {
      if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
      {
        ByteSwapper<double>::SwapRangeFromSystemToLittleEndian(static_cast<double *>(buffer), numberOfPixels);
      }
      else if (m_ByteOrder == IOByteOrderEnum::BigEndian)
      {
        ByteSwapper<double>::SwapRangeFromSystemToBigEndian(static_cast<double *>(buffer), numberOfPixels);
      }
      break;
    }
    default:
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Pixel Type Unknown");
      throw exception;
  }
}

void
GiplImageIO::WriteImageInformation()
{
  // not possible to write a Gipl file
}

/** The write function is not implemented */
void
GiplImageIO::Write(const void * buffer)
{
  CheckExtension(m_FileName.c_str());

  const unsigned int nDims = this->GetNumberOfDimensions();

  if (m_IsCompressed)
  {
    m_Internal->m_GzFile = gzopen(m_FileName.c_str(), "wb");
    if (m_Internal->m_GzFile == nullptr)
    {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("File cannot be write");
      throw exception;
    }
  }
  else
  {
    this->OpenFileForWriting(m_Ofstream, m_FileName);
  }

  for (unsigned int i = 0; i < 4; ++i)
  {
    unsigned short value = 0;
    if (i < nDims)
    {
      value = this->GetDimensions(i);
      if (m_ByteOrder == IOByteOrderEnum::BigEndian)
      {
        ByteSwapper<unsigned short>::SwapFromSystemToBigEndian(&value);
      }
      else if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
      {
        ByteSwapper<unsigned short>::SwapFromSystemToLittleEndian(&value);
      }

      if (m_IsCompressed)
      {
        gzwrite(
          m_Internal->m_GzFile, reinterpret_cast<char *>(&value), static_cast<unsigned int>(sizeof(unsigned short)));
      }
      else
      {
        m_Ofstream.write(reinterpret_cast<char *>(&value), sizeof(unsigned short));
      }
    }
    else
    {
      value = 1;
      if (m_ByteOrder == IOByteOrderEnum::BigEndian)
      {
        ByteSwapper<unsigned short>::SwapFromSystemToBigEndian(&value);
      }
      else if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
      {
        ByteSwapper<unsigned short>::SwapFromSystemToLittleEndian(&value);
      }
      if (m_IsCompressed)
      {
        gzwrite(
          m_Internal->m_GzFile, reinterpret_cast<char *>(&value), static_cast<unsigned int>(sizeof(unsigned short)));
      }
      else
      {
        m_Ofstream.write(reinterpret_cast<char *>(&value), sizeof(unsigned short));
      }
    }
  }

  unsigned short image_type = 0;
  switch (m_ComponentType)
  {
    case IOComponentEnum::SCHAR:
      image_type = GIPL_CHAR;
      break;
    case IOComponentEnum::UCHAR:
      image_type = GIPL_U_CHAR;
      break;
    case IOComponentEnum::SHORT:
      image_type = GIPL_SHORT;
      break;
    case IOComponentEnum::USHORT:
      image_type = GIPL_U_SHORT;
      break;
    case IOComponentEnum::UINT:
      image_type = GIPL_U_INT;
      break;
    case IOComponentEnum::INT:
      image_type = GIPL_INT;
      break;
    case IOComponentEnum::FLOAT:
      image_type = GIPL_FLOAT;
      break;
    case IOComponentEnum::DOUBLE:
      image_type = GIPL_DOUBLE;
      break;
    default:
      itkExceptionMacro("Invalid type: " << m_ComponentType);
  }

  if (m_ByteOrder == IOByteOrderEnum::BigEndian)
  {
    ByteSwapper<unsigned short>::SwapFromSystemToBigEndian(&image_type);
  }
  if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
  {
    ByteSwapper<unsigned short>::SwapFromSystemToLittleEndian(&image_type);
  }

  if (m_IsCompressed)
  {
    gzwrite(
      m_Internal->m_GzFile, reinterpret_cast<char *>(&image_type), static_cast<unsigned int>(sizeof(unsigned short)));
  }
  else
  {
    m_Ofstream.write(reinterpret_cast<char *>(&image_type), sizeof(unsigned short));
  }

  /*   10   16  X,Y,Z,T pixel dimensions mm */
  for (unsigned int i = 0; i < 4; ++i)
  {
    if (i < nDims)
    {
      auto value = static_cast<float>(m_Spacing[i]);
      if (m_ByteOrder == IOByteOrderEnum::BigEndian)
      {
        ByteSwapper<float>::SwapFromSystemToBigEndian(&value);
      }
      if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
      {
        ByteSwapper<float>::SwapFromSystemToLittleEndian(&value);
      }
      if (m_IsCompressed)
      {
        gzwrite(m_Internal->m_GzFile, reinterpret_cast<char *>(&value), static_cast<unsigned int>(sizeof(float)));
      }
      else
      {
        m_Ofstream.write(reinterpret_cast<char *>(&value), sizeof(float));
      }
    }
    else
    {
      float value = 1.0f;
      if (m_ByteOrder == IOByteOrderEnum::BigEndian)
      {
        ByteSwapper<float>::SwapFromSystemToBigEndian(&value);
      }
      if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
      {
        ByteSwapper<float>::SwapFromSystemToLittleEndian(&value);
      }
      if (m_IsCompressed)
      {
        gzwrite(m_Internal->m_GzFile, reinterpret_cast<char *>(&value), static_cast<unsigned int>(sizeof(float)));
      }
      else
      {
        m_Ofstream.write(reinterpret_cast<char *>(&value), sizeof(float));
      }
    }
  }

  char line1[80]; /*   26   80  Patient / Text field        */

  for (char & i : line1)
  {
    i = 0; // initialize
  }

  snprintf(line1, sizeof(line1), "No Patient Information");
  for (char & i : line1)
  {
    if (m_IsCompressed)
    {
      gzwrite(m_Internal->m_GzFile, &i, static_cast<unsigned int>(sizeof(char)));
    }
    else
    {
      m_Ofstream.write(&i, sizeof(char));
    }
  }

  float matrix[20]; /*  106   80                              */
  for (float & i : matrix)
  {
    i = 0; // write zeros
    if (m_IsCompressed)
    {
      gzwrite(m_Internal->m_GzFile, reinterpret_cast<char *>(&i), static_cast<unsigned int>(sizeof(float)));
    }
    else
    {
      m_Ofstream.write(reinterpret_cast<char *>(&i), sizeof(float));
    }
  }

  char flag1 = 0; /*  186    1  Orientation flag (below)    */
  if (m_IsCompressed)
  {
    gzwrite(m_Internal->m_GzFile, &flag1, static_cast<unsigned int>(sizeof(char)));
  }
  else
  {
    m_Ofstream.write(&flag1, sizeof(char));
  }

  char flag2 = 0; /*  187    1                              */
  if (m_IsCompressed)
  {
    gzwrite(m_Internal->m_GzFile, &flag2, static_cast<unsigned int>(sizeof(char)));
  }
  else
  {
    m_Ofstream.write(&flag2, sizeof(char));
  }

  double min = 0; /*  188    8  Minimum voxel value         */
  if (m_IsCompressed)
  {
    gzwrite(m_Internal->m_GzFile, reinterpret_cast<char *>(&min), static_cast<unsigned int>(sizeof(double)));
  }
  else
  {
    m_Ofstream.write(reinterpret_cast<char *>(&min), sizeof(double));
  }

  double max = 0; /*  196    8  Maximum voxel value         */
  if (m_IsCompressed)
  {
    gzwrite(m_Internal->m_GzFile, reinterpret_cast<char *>(&max), static_cast<unsigned int>(sizeof(double)));
  }
  else
  {
    m_Ofstream.write(reinterpret_cast<char *>(&max), sizeof(double));
  }

  double origin[4]; /*  204   32  X,Y,Z,T offset              */
  for (unsigned int i = 0; i < 4; ++i)
  {
    if (i < nDims)
    {
      origin[i] = m_Origin[i];
    }
    else
    {
      origin[i] = 0;
    }

    if (m_ByteOrder == IOByteOrderEnum::BigEndian)
    {
      ByteSwapper<double>::SwapFromSystemToBigEndian(&origin[i]);
    }
    if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
    {
      ByteSwapper<double>::SwapFromSystemToLittleEndian(&origin[i]);
    }

    if (m_IsCompressed)
    {
      gzwrite(m_Internal->m_GzFile, reinterpret_cast<char *>(&origin[i]), static_cast<unsigned int>(sizeof(double)));
    }
    else
    {
      m_Ofstream.write(reinterpret_cast<char *>(&origin[i]), sizeof(double));
    }
  }

  float pixval_offset = 0; /*  236    4                            */
  if (m_IsCompressed)
  {
    gzwrite(m_Internal->m_GzFile, reinterpret_cast<char *>(&pixval_offset), static_cast<unsigned int>(sizeof(float)));
  }
  else
  {
    m_Ofstream.write(reinterpret_cast<char *>(&pixval_offset), sizeof(float));
  }

  float pixval_cal = 0; /*  240    4                              */
  if (m_IsCompressed)
  {
    gzwrite(m_Internal->m_GzFile, reinterpret_cast<char *>(&pixval_cal), static_cast<unsigned int>(sizeof(float)));
  }
  else
  {
    m_Ofstream.write(reinterpret_cast<char *>(&pixval_cal), sizeof(float));
  }

  float user_def1 = 0; /*  244    4  Inter-slice Gap             */
  if (m_IsCompressed)
  {
    gzwrite(m_Internal->m_GzFile, reinterpret_cast<char *>(&user_def1), static_cast<unsigned int>(sizeof(float)));
  }
  else
  {
    m_Ofstream.write(reinterpret_cast<char *>(&user_def1), sizeof(float));
  }

  float user_def2 = 0; /*  248    4  User defined field          */
  if (m_IsCompressed)
  {
    gzwrite(m_Internal->m_GzFile, reinterpret_cast<char *>(&user_def2), static_cast<unsigned int>(sizeof(float)));
  }
  else
  {
    m_Ofstream.write(reinterpret_cast<char *>(&user_def2), sizeof(float));
  }

  unsigned int magic_number = GIPL_MAGIC_NUMBER; /*  252    4 Magic Number
                                                  */
  if (m_ByteOrder == IOByteOrderEnum::BigEndian)
  {
    ByteSwapper<unsigned int>::SwapFromSystemToBigEndian(&magic_number);
  }
  if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
  {
    ByteSwapper<unsigned int>::SwapFromSystemToLittleEndian(&magic_number);
  }

  if (m_IsCompressed)
  {
    gzwrite(
      m_Internal->m_GzFile, reinterpret_cast<char *>(&magic_number), static_cast<unsigned int>(sizeof(unsigned int)));
  }
  else
  {
    m_Ofstream.write(reinterpret_cast<char *>(&magic_number), sizeof(unsigned int));
  }

  // Actually do the writing
  //
  this->ComputeStrides();
  if (m_FileType == IOFileEnum::ASCII)
  {
    this->WriteBufferAsASCII(m_Ofstream, buffer, this->GetComponentType(), this->GetImageSizeInComponents());
  }
  else // binary
  {
    const auto numberOfBytes = static_cast<SizeValueType>(this->GetImageSizeInBytes());
    const auto numberOfComponents = static_cast<SizeValueType>(this->GetImageSizeInComponents());

    // Swap bytes if necessary
    if (m_ByteOrder == IOByteOrderEnum::LittleEndian)
    {
      const auto tempBuffer = make_unique_for_overwrite<char[]>(numberOfBytes);
      memcpy(tempBuffer.get(), buffer, numberOfBytes);
      SwapBytesIfNecessary(tempBuffer.get(), numberOfComponents);
      if (m_IsCompressed)
      {
        gzwrite(m_Internal->m_GzFile, tempBuffer.get(), numberOfBytes);
      }
      else
      {
        m_Ofstream.write(tempBuffer.get(), numberOfBytes);
      }
    }
    else if (m_ByteOrder == IOByteOrderEnum::BigEndian)
    {
      const auto tempBuffer = make_unique_for_overwrite<char[]>(numberOfBytes);
      memcpy(tempBuffer.get(), buffer, numberOfBytes);
      SwapBytesIfNecessary(tempBuffer.get(), numberOfComponents);
      if (m_IsCompressed)
      {
        gzwrite(m_Internal->m_GzFile, tempBuffer.get(), numberOfBytes);
      }
      else
      {
        m_Ofstream.write(tempBuffer.get(), numberOfBytes);
      }
    }
    else
    {
      if (m_IsCompressed)
      {
        gzwrite(m_Internal->m_GzFile, const_cast<void *>(buffer), numberOfBytes);
      }
      else
      {
        m_Ofstream.write(static_cast<const char *>(buffer), numberOfBytes);
      }
    }
  }

  if (m_IsCompressed)
  {
    gzclose(m_Internal->m_GzFile);
    m_Internal->m_GzFile = nullptr;
  }
  else
  {
    m_Ofstream.close();
  }
}

void
GiplImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "PixelType " << m_PixelType << '\n';
}

bool
GiplImageIO::CheckExtension(const char * filename)
{
  const std::string fname = filename;

  if (fname.empty())
  {
    itkDebugMacro("No filename specified.");
    return false;
  }

  bool extensionFound = false;
  m_IsCompressed = false;

  std::string::size_type giplPos = fname.rfind(".gipl");
  if ((giplPos != std::string::npos) && (giplPos == fname.length() - 5))
  {
    extensionFound = true;
  }

  giplPos = fname.rfind(".gipl.gz");
  if ((giplPos != std::string::npos) && (giplPos == fname.length() - 8))
  {
    extensionFound = true;
    m_IsCompressed = true;
  }

  return extensionFound;
}
} // end namespace itk
