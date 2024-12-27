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

#include "itkMetaImageIO.h"
#include "itkAnatomicalOrientation.h"
#include "itkIOCommon.h"
#include "itksys/SystemTools.hxx"
#include "itkMath.h"
#include "itkSingleton.h"
#include "itkMakeUniqueForOverwrite.h"
#include "metaImageUtils.h"

// Function to join strings with a delimiter similar to python's ' '.join([1, 2, 3 ])
template <typename ContainerType, typename DelimiterType, typename StreamType>
static auto
_join(const ContainerType & elements, const DelimiterType & delimiter, StreamType & strs) -> void
{
  for (size_t i = 0; i < elements.size(); ++i)
  {
    strs << elements[i];
    if (i != elements.size() - 1)
    {
      strs << delimiter;
    }
  }
}

namespace itk
{
// Explicitly set std::numeric_limits<double>::max_digits10 this will provide
// better accuracy when writing out floating point number in MetaImage header.
itkGetGlobalValueMacro(MetaImageIO, unsigned int, DefaultDoublePrecision, 17);

unsigned int * MetaImageIO::m_DefaultDoublePrecision;

MetaImageIO::MetaImageIO()
{
  itkInitGlobalsMacro(DefaultDoublePrecision);
  m_FileType = IOFileEnum::Binary;
  m_SubSamplingFactor = 1;
  if (MET_SystemByteOrderMSB())
  {
    m_ByteOrder = IOByteOrderEnum::BigEndian;
  }
  else
  {
    m_ByteOrder = IOByteOrderEnum::LittleEndian;
  }

  this->AddSupportedWriteExtension(".mha");
  this->AddSupportedWriteExtension(".mhd");

  this->AddSupportedReadExtension(".mha");
  this->AddSupportedReadExtension(".mhd");
  // set behavior of MetaImageIO independently of the default value in MetaImage
  this->SetDoublePrecision(GetDefaultDoublePrecision());

  this->Self::SetCompressor("");
  this->Self::SetMaximumCompressionLevel(9);
  this->Self::SetCompressionLevel(2);
}

MetaImageIO::~MetaImageIO() = default;

void
MetaImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  m_MetaImage.PrintInfo();
  os << indent << "SubSamplingFactor: " << m_SubSamplingFactor << '\n';
}

void
MetaImageIO::SetDataFileName(const char * filename)
{
  m_MetaImage.ElementDataFileName(filename);
}

// This method will only test if the header looks like a
// MetaImage.  Some code is redundant with ReadImageInformation
// a StateMachine could provide a better implementation
bool
MetaImageIO::CanReadFile(const char * filename)
{
  // First check the extension
  const std::string fname = filename;

  if (fname.empty())
  {
    itkDebugMacro("No filename specified.");
    return false;
  }

  return m_MetaImage.CanRead(filename);
}

void
MetaImageIO::ReadImageInformation()
{
  if (!m_MetaImage.Read(m_FileName.c_str(), false))
  {
    itkExceptionMacro("File cannot be read: " << this->GetFileName() << " for reading." << std::endl
                                              << "Reason: " << itksys::SystemTools::GetLastSystemError());
  }

  if (m_MetaImage.BinaryData())
  {
    this->SetFileType(IOFileEnum::Binary);
  }
  else
  {
    this->SetFileType(IOFileEnum::ASCII);
  }

  this->SetNumberOfComponents(m_MetaImage.ElementNumberOfChannels());

  // Set default value
  this->SetComponentType(IOComponentEnum::UNKNOWNCOMPONENTTYPE);
  itk::MetaDataDictionary & thisMetaDict = this->GetMetaDataDictionary();
  switch (m_MetaImage.ElementType())
  {
    default:
    case MET_OTHER:
    case MET_NONE:
      this->SetPixelType(IOPixelEnum::UNKNOWNPIXELTYPE);
      this->SetComponentType(IOComponentEnum::UNKNOWNCOMPONENTTYPE);
      break;
    case MET_CHAR:
    case MET_ASCII_CHAR:
      this->SetPixelType(IOPixelEnum::SCALAR);
      this->SetComponentType(IOComponentEnum::CHAR);
      break;
    case MET_CHAR_ARRAY:
    case MET_STRING:
      this->SetPixelType(IOPixelEnum::VECTOR);
      this->SetComponentType(IOComponentEnum::CHAR);
      break;
    case MET_UCHAR:
      this->SetPixelType(IOPixelEnum::SCALAR);
      this->SetComponentType(IOComponentEnum::UCHAR);
      break;
    case MET_UCHAR_ARRAY:
      this->SetPixelType(IOPixelEnum::VECTOR);
      this->SetComponentType(IOComponentEnum::UCHAR);
      break;
    case MET_SHORT:
      this->SetPixelType(IOPixelEnum::SCALAR);
      this->SetComponentType(IOComponentEnum::SHORT);
      break;
    case MET_SHORT_ARRAY:
      this->SetPixelType(IOPixelEnum::VECTOR);
      this->SetComponentType(IOComponentEnum::SHORT);
      break;
    case MET_USHORT:
      this->SetPixelType(IOPixelEnum::SCALAR);
      this->SetComponentType(IOComponentEnum::USHORT);
      break;
    case MET_USHORT_ARRAY:
      this->SetPixelType(IOPixelEnum::VECTOR);
      this->SetComponentType(IOComponentEnum::USHORT);
      break;
    case MET_INT:
      this->SetPixelType(IOPixelEnum::SCALAR);
      if constexpr (sizeof(int) == MET_ValueTypeSize[MET_INT])
      {
        this->SetComponentType(IOComponentEnum::INT);
      }
      else if constexpr (sizeof(long) == MET_ValueTypeSize[MET_INT])
      {
        this->SetComponentType(IOComponentEnum::LONG);
      }
      break;
    case MET_INT_ARRAY:
      this->SetPixelType(IOPixelEnum::VECTOR);
      if constexpr (sizeof(int) == MET_ValueTypeSize[MET_INT])
      {
        this->SetComponentType(IOComponentEnum::INT);
      }
      else if constexpr (sizeof(long) == MET_ValueTypeSize[MET_INT])
      {
        this->SetComponentType(IOComponentEnum::LONG);
      }
      break;
    case MET_UINT:
      this->SetPixelType(IOPixelEnum::SCALAR);
      if constexpr (sizeof(unsigned int) == MET_ValueTypeSize[MET_UINT])
      {
        this->SetComponentType(IOComponentEnum::UINT);
      }
      else if constexpr (sizeof(unsigned long) == MET_ValueTypeSize[MET_UINT])
      {
        this->SetComponentType(IOComponentEnum::ULONG);
      }
      break;
    case MET_UINT_ARRAY:
      this->SetPixelType(IOPixelEnum::VECTOR);
      if constexpr (sizeof(int) == MET_ValueTypeSize[MET_INT])
      {
        this->SetComponentType(IOComponentEnum::UINT);
      }
      else if constexpr (sizeof(long) == MET_ValueTypeSize[MET_INT])
      {
        this->SetComponentType(IOComponentEnum::ULONG);
      }
      break;
    case MET_LONG:
      this->SetPixelType(IOPixelEnum::SCALAR);
      if constexpr (sizeof(long) == MET_ValueTypeSize[MET_LONG])
      {
        this->SetComponentType(IOComponentEnum::LONG);
      }
      else if constexpr (sizeof(int) == MET_ValueTypeSize[MET_LONG])
      {
        this->SetComponentType(IOComponentEnum::INT);
      }
      break;
    case MET_LONG_ARRAY:
      this->SetPixelType(IOPixelEnum::VECTOR);
      if constexpr (sizeof(long) == MET_ValueTypeSize[MET_LONG])
      {
        this->SetComponentType(IOComponentEnum::LONG);
      }
      else if constexpr (sizeof(int) == MET_ValueTypeSize[MET_LONG])
      {
        this->SetComponentType(IOComponentEnum::INT);
      }
      break;
    case MET_ULONG:
      this->SetPixelType(IOPixelEnum::SCALAR);
      if constexpr (sizeof(unsigned long) == MET_ValueTypeSize[MET_ULONG])
      {
        this->SetComponentType(IOComponentEnum::ULONG);
      }
      else if constexpr (sizeof(unsigned int) == MET_ValueTypeSize[MET_ULONG])
      {
        this->SetComponentType(IOComponentEnum::UINT);
      }
      break;
    case MET_ULONG_ARRAY:
      this->SetPixelType(IOPixelEnum::VECTOR);
      if constexpr (sizeof(unsigned long) == MET_ValueTypeSize[MET_ULONG])
      {
        this->SetComponentType(IOComponentEnum::ULONG);
      }
      else if constexpr (sizeof(unsigned int) == MET_ValueTypeSize[MET_ULONG])
      {
        this->SetComponentType(IOComponentEnum::UINT);
      }
      break;
    case MET_LONG_LONG:
      this->SetPixelType(IOPixelEnum::SCALAR);
      if constexpr (sizeof(long long) == MET_ValueTypeSize[MET_LONG_LONG])
      {
        this->SetComponentType(IOComponentEnum::LONGLONG);
      }
      else if constexpr (sizeof(int) == MET_ValueTypeSize[MET_LONG_LONG])
      {
        this->SetComponentType(IOComponentEnum::INT);
      }
      else if constexpr (sizeof(long) == MET_ValueTypeSize[MET_LONG_LONG])
      {
        this->SetComponentType(IOComponentEnum::LONG);
      }
      break;
    case MET_LONG_LONG_ARRAY:
      this->SetPixelType(IOPixelEnum::VECTOR);
      if constexpr (sizeof(long long) == MET_ValueTypeSize[MET_LONG_LONG])
      {
        this->SetComponentType(IOComponentEnum::LONGLONG);
      }
      else if constexpr (sizeof(int) == MET_ValueTypeSize[MET_LONG_LONG])
      {
        this->SetComponentType(IOComponentEnum::INT);
      }
      else if constexpr (sizeof(long) == MET_ValueTypeSize[MET_LONG_LONG])
      {
        this->SetComponentType(IOComponentEnum::LONG);
      }
      break;
    case MET_ULONG_LONG:
      this->SetPixelType(IOPixelEnum::SCALAR);
      if constexpr (sizeof(unsigned long long) == MET_ValueTypeSize[MET_ULONG_LONG])
      {
        this->SetComponentType(IOComponentEnum::ULONGLONG);
      }
      else if constexpr (sizeof(unsigned int) == MET_ValueTypeSize[MET_ULONG_LONG])
      {
        this->SetComponentType(IOComponentEnum::UINT);
      }
      else if constexpr (sizeof(unsigned long) == MET_ValueTypeSize[MET_ULONG_LONG])
      {
        this->SetComponentType(IOComponentEnum::ULONG);
      }
      break;
    case MET_ULONG_LONG_ARRAY:
      this->SetPixelType(IOPixelEnum::VECTOR);
      if constexpr (sizeof(unsigned long long) == MET_ValueTypeSize[MET_ULONG_LONG])
      {
        this->SetComponentType(IOComponentEnum::ULONGLONG);
      }
      else if constexpr (sizeof(unsigned int) == MET_ValueTypeSize[MET_ULONG_LONG])
      {
        this->SetComponentType(IOComponentEnum::UINT);
      }
      else if constexpr (sizeof(unsigned long) == MET_ValueTypeSize[MET_ULONG_LONG])
      {
        this->SetComponentType(IOComponentEnum::ULONG);
      }
      break;
    case MET_FLOAT:
      this->SetPixelType(IOPixelEnum::SCALAR);
      if constexpr (sizeof(float) == MET_ValueTypeSize[MET_FLOAT])
      {
        this->SetComponentType(IOComponentEnum::FLOAT);
      }
      else if constexpr (sizeof(double) == MET_ValueTypeSize[MET_FLOAT])
      {
        this->SetComponentType(IOComponentEnum::DOUBLE);
      }
      break;
    case MET_FLOAT_ARRAY:
      this->SetPixelType(IOPixelEnum::VECTOR);
      if constexpr (sizeof(float) == MET_ValueTypeSize[MET_FLOAT])
      {
        this->SetComponentType(IOComponentEnum::FLOAT);
      }
      else if constexpr (sizeof(double) == MET_ValueTypeSize[MET_FLOAT])
      {
        this->SetComponentType(IOComponentEnum::DOUBLE);
      }
      break;
    case MET_DOUBLE:
      this->SetPixelType(IOPixelEnum::SCALAR);
      this->SetComponentType(IOComponentEnum::DOUBLE);
      if constexpr (sizeof(double) == MET_ValueTypeSize[MET_DOUBLE])
      {
        this->SetComponentType(IOComponentEnum::DOUBLE);
      }
      else if constexpr (sizeof(float) == MET_ValueTypeSize[MET_DOUBLE])
      {
        this->SetComponentType(IOComponentEnum::FLOAT);
      }
      break;
    case MET_DOUBLE_ARRAY:
      this->SetPixelType(IOPixelEnum::VECTOR);
      if constexpr (sizeof(double) == MET_ValueTypeSize[MET_DOUBLE])
      {
        this->SetComponentType(IOComponentEnum::DOUBLE);
      }
      else if constexpr (sizeof(float) == MET_ValueTypeSize[MET_DOUBLE])
      {
        this->SetComponentType(IOComponentEnum::FLOAT);
      }
      break;
    case MET_FLOAT_MATRIX:
      this->SetPixelType(IOPixelEnum::VECTOR);
      if constexpr (sizeof(float) == MET_ValueTypeSize[MET_FLOAT])
      {
        this->SetComponentType(IOComponentEnum::FLOAT);
      }
      else if constexpr (sizeof(double) == MET_ValueTypeSize[MET_FLOAT])
      {
        this->SetComponentType(IOComponentEnum::DOUBLE);
      }
      this->SetNumberOfComponents(m_NumberOfComponents * m_NumberOfComponents);
      break;
  }

  // BUG: 8732
  // The above use to MET_*_ARRAY may not be correct, as this MetaIO
  // ElementType was not designed to indicate vectors, but something
  // else
  //
  // if the file has multiple components then we default to a vector
  // pixel type, support could be added to MetaIO format to define
  // different pixel types
  if (m_MetaImage.ElementNumberOfChannels() > 1)
  {
    this->SetPixelType(IOPixelEnum::VECTOR);
  }

  this->SetNumberOfDimensions(m_MetaImage.NDims());


  for (unsigned int i = 0; i < m_NumberOfDimensions; ++i)
  {
    this->SetDimensions(i, m_MetaImage.DimSize(i) / m_SubSamplingFactor);
    this->SetSpacing(i, m_MetaImage.ElementSpacing(i) * m_SubSamplingFactor);
    this->SetOrigin(i, m_MetaImage.Position(i));
  }

  //
  // Read direction cosines
  //
  const double *     transformMatrix = m_MetaImage.TransformMatrix();
  vnl_vector<double> directionAxis(this->GetNumberOfDimensions());
  for (unsigned int ii = 0; ii < this->GetNumberOfDimensions(); ++ii)
  {
    for (unsigned int jj = 0; jj < this->GetNumberOfDimensions(); ++jj)
    {
      directionAxis[jj] = transformMatrix[ii * this->GetNumberOfDimensions() + jj];
    }
    this->SetDirection(ii, directionAxis);
  }

  const std::string classname(this->GetNameOfClass());
  EncapsulateMetaData<std::string>(thisMetaDict, ITK_InputFilterName, classname);

  // metaImage has a Modality tag which is not stored as part of its
  // metadata dictionary
  std::string modality;
  MET_ImageModalityToString(m_MetaImage.Modality(), modality);
  EncapsulateMetaData<std::string>(thisMetaDict, "Modality", modality);
  //
  // save the metadatadictionary in the MetaImage header.
  // NOTE: The MetaIO library only supports typeless strings as metadata
  const int dictFields = m_MetaImage.GetNumberOfAdditionalReadFields();
  for (int f = 0; f < dictFields; ++f)
  {
    const std::string key(m_MetaImage.GetAdditionalReadFieldName(f));
    const std::string value(m_MetaImage.GetAdditionalReadFieldValue(f));
    EncapsulateMetaData<std::string>(thisMetaDict, key, value);
  }

  //
  // Read some metadata
  //
  MetaDataDictionary & metaDict = this->GetMetaDataDictionary();

  // Look at default metaio fields
  if (m_MetaImage.DistanceUnits() != MET_DISTANCE_UNITS_UNKNOWN)
  {
    EncapsulateMetaData<std::string>(metaDict, ITK_VoxelUnits, std::string(m_MetaImage.DistanceUnitsName()));
  }

  if (strlen(m_MetaImage.AcquisitionDate()) > 0)
  {
    EncapsulateMetaData<std::string>(metaDict, ITK_ExperimentDate, std::string(m_MetaImage.AcquisitionDate()));
  }
}

void
MetaImageIO::Read(void * buffer)
{
  const unsigned int nDims = this->GetNumberOfDimensions();

  // this will check to see if we are actually streaming
  // we initialize with the dimensions of the file, since if
  // largestRegion and ioRegion don't match, we'll use the streaming
  // path since the comparison will fail
  ImageIORegion largestRegion(nDims);

  for (unsigned int i = 0; i < nDims; ++i)
  {
    largestRegion.SetIndex(i, 0);
    largestRegion.SetSize(i, this->GetDimensions(i));
  }

  if (largestRegion != m_IORegion)
  {
    const auto indexMin = make_unique_for_overwrite<int[]>(nDims);
    const auto indexMax = make_unique_for_overwrite<int[]>(nDims);
    for (unsigned int i = 0; i < nDims; ++i)
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

    if (!m_MetaImage.ReadROI(indexMin.get(), indexMax.get(), m_FileName.c_str(), true, buffer, m_SubSamplingFactor))
    {
      itkExceptionMacro("File cannot be read: " << this->GetFileName() << " for reading." << std::endl
                                                << "Reason: " << itksys::SystemTools::GetLastSystemError());
    }

    m_MetaImage.ElementByteOrderFix(m_IORegion.GetNumberOfPixels());
  }
  else
  {
    if (!m_MetaImage.Read(m_FileName.c_str(), true, buffer))
    {
      itkExceptionMacro("File cannot be read: " << this->GetFileName() << " for reading." << std::endl
                                                << "Reason: " << itksys::SystemTools::GetLastSystemError());
    }

    // since we are not streaming m_IORegion may not be set, so
    m_MetaImage.ElementByteOrderFix(this->GetImageSizeInPixels());
  }
}

MetaImage *
MetaImageIO::GetMetaImagePointer()
{
  return &m_MetaImage;
}

bool
MetaImageIO::CanWriteFile(const char * name)
{
  const std::string filename = name;

  if (filename.empty())
  {
    return false;
  }

  return this->HasSupportedWriteExtension(name, true);
}

void
MetaImageIO::WriteImageInformation()
{

  MetaDataDictionary & metaDict = this->GetMetaDataDictionary();
  std::string          metaDataStr;

  // Look at default metaio fields
  if (ExposeMetaData<std::string>(metaDict, ITK_VoxelUnits, metaDataStr))
  {
    // Handle analyze style unit string
    if (metaDataStr == "um. ")
    {
      m_MetaImage.DistanceUnits(MET_DISTANCE_UNITS_UM);
    }
    else if (metaDataStr == "mm. ")
    {
      m_MetaImage.DistanceUnits(MET_DISTANCE_UNITS_MM);
    }
    else if (metaDataStr == "cm. ")
    {
      m_MetaImage.DistanceUnits(MET_DISTANCE_UNITS_CM);
    }
    else
    {
      m_MetaImage.DistanceUnits(metaDataStr.c_str());
    }
  }

  if (ExposeMetaData<std::string>(metaDict, ITK_ExperimentDate, metaDataStr))
  {
    m_MetaImage.AcquisitionDate(metaDataStr.c_str());
  }

  // Save out the metadatadictionary key/value pairs as part of
  // the metaio header.
  const std::vector<std::string> keys = metaDict.GetKeys();
  for (auto & key : keys)
  {
    if (key == ITK_ExperimentDate || key == ITK_VoxelUnits)
    {
      continue;
    }
    // try for common scalar types
    std::ostringstream  strs;
    double              dval = 0.0;
    float               fval = 0.0F;
    long                lval = 0L;
    unsigned long       ulval = 0L;
    long long           llval = 0LL;
    unsigned long long  ullval = 0uLL;
    int                 ival = 0;
    unsigned int        uval = 0;
    short               shval = 0;
    unsigned short      ushval = 0;
    char                cval = 0;
    unsigned char       ucval = 0;
    bool                bval = false;
    std::vector<double> vval(0);
    std::string         value = "";
    if (ExposeMetaData<std::string>(metaDict, key, value))
    {
      strs << value;
    }
    else if (ExposeMetaData<double>(metaDict, key, dval))
    {
      strs << dval;
    }
    else if (ExposeMetaData<float>(metaDict, key, fval))
    {
      strs << fval;
    }
    else if (ExposeMetaData<long>(metaDict, key, lval))
    {
      strs << lval;
    }
    else if (ExposeMetaData<unsigned long>(metaDict, key, ulval))
    {
      strs << ulval;
    }
    else if (ExposeMetaData<long long>(metaDict, key, llval))
    {
      strs << llval;
    }
    else if (ExposeMetaData<unsigned long long>(metaDict, key, ullval))
    {
      strs << ullval;
    }
    else if (ExposeMetaData<int>(metaDict, key, ival))
    {
      strs << ival;
    }
    else if (ExposeMetaData<unsigned int>(metaDict, key, uval))
    {
      strs << uval;
    }
    else if (ExposeMetaData<short>(metaDict, key, shval))
    {
      strs << shval;
    }
    else if (ExposeMetaData<unsigned short>(metaDict, key, ushval))
    {
      strs << ushval;
    }
    else if (ExposeMetaData<char>(metaDict, key, cval))
    {
      strs << cval;
    }
    else if (ExposeMetaData<unsigned char>(metaDict, key, ucval))
    {
      strs << ucval;
    }
    else if (ExposeMetaData<bool>(metaDict, key, bval))
    {
      strs << bval;
    }
    else if (ExposeMetaData<std::vector<double>>(metaDict, key, vval))
    {
      _join(vval, ' ', strs);
    }
    else if (WriteMatrixInMetaData<1>(strs, metaDict, key) || WriteMatrixInMetaData<2>(strs, metaDict, key) ||
             WriteMatrixInMetaData<3>(strs, metaDict, key) || WriteMatrixInMetaData<4>(strs, metaDict, key) ||
             WriteMatrixInMetaData<5>(strs, metaDict, key) || WriteMatrixInMetaData<6>(strs, metaDict, key))
    {
      // Nothing to do, everything is done in WriteMatrixInMetaData
    }

    value = strs.str();

    if (value.empty())
    {
      // if the value is an empty string then the resulting entry in
      // the header will not be able to be read by the metaIO
      // library, which results is an unreadable/corrupt file.
      itkWarningMacro("Unsupported or empty metaData item " << key << " of type "
                                                            << metaDict[key]->GetMetaDataObjectTypeName()
                                                            << "found, won't be written to image file");
      // so this entry should be skipped.
      continue;
    }

    // Rolling this back out so that the tests pass.
    // The meta image AddUserField requires control of the memory space.
    m_MetaImage.AddUserField(key.c_str(), MET_STRING, static_cast<int>(value.size()), value.c_str(), true, -1);
  }
}

/**
 *
 */
void
MetaImageIO::Write(const void * buffer)
{
  const unsigned int numberOfDimensions = this->GetNumberOfDimensions();

  bool binaryData = true;

  if (this->GetFileType() == IOFileEnum::ASCII)
  {
    binaryData = false;
  }

  const int nChannels = this->GetNumberOfComponents();

  MET_ValueEnumType eType = MET_OTHER;
  switch (m_ComponentType)
  {
    default:
    case IOComponentEnum::UNKNOWNCOMPONENTTYPE:
      eType = MET_OTHER;
      break;
    case IOComponentEnum::CHAR:
      eType = MET_CHAR;
      break;
    case IOComponentEnum::UCHAR:
      eType = MET_UCHAR;
      break;
    case IOComponentEnum::SHORT:
      eType = MET_SHORT;
      break;
    case IOComponentEnum::USHORT:
      eType = MET_USHORT;
      break;
    case IOComponentEnum::LONG:
      if constexpr (sizeof(long) == MET_ValueTypeSize[MET_LONG])
      {
        eType = MET_LONG;
      }
      else if constexpr (sizeof(long) == MET_ValueTypeSize[MET_INT])
      {
        eType = MET_INT;
      }
      else if constexpr (sizeof(long) == MET_ValueTypeSize[MET_LONG_LONG])
      {
        eType = MET_LONG_LONG;
      }
      break;
    case IOComponentEnum::ULONG:
      if constexpr (sizeof(long) == MET_ValueTypeSize[MET_LONG])
      {
        eType = MET_ULONG;
      }
      else if constexpr (sizeof(long) == MET_ValueTypeSize[MET_INT])
      {
        eType = MET_UINT;
      }
      else if constexpr (sizeof(long) == MET_ValueTypeSize[MET_LONG_LONG])
      {
        eType = MET_ULONG_LONG;
      }
      break;
    case IOComponentEnum::LONGLONG:

      if constexpr (sizeof(long long) == MET_ValueTypeSize[MET_LONG_LONG])
      {
        eType = MET_LONG_LONG;
      }
      break;
    case IOComponentEnum::ULONGLONG:
      if constexpr (sizeof(long long) == MET_ValueTypeSize[MET_ULONG_LONG])
      {
        eType = MET_ULONG_LONG;
      }
      break;
    case IOComponentEnum::INT:
      if constexpr (sizeof(int) == MET_ValueTypeSize[MET_INT])
      {
        eType = MET_INT;
      }
      else if constexpr (sizeof(int) == MET_ValueTypeSize[MET_LONG])
      {
        eType = MET_LONG;
      }
      break;
    case IOComponentEnum::UINT:
      if constexpr (sizeof(int) == MET_ValueTypeSize[MET_INT])
      {
        eType = MET_UINT;
      }
      else if constexpr (sizeof(int) == MET_ValueTypeSize[MET_LONG])
      {
        eType = MET_ULONG;
      }
      break;
    case IOComponentEnum::FLOAT:
      if constexpr (sizeof(float) == MET_ValueTypeSize[MET_FLOAT])
      {
        eType = MET_FLOAT;
      }
      else if constexpr (sizeof(float) == MET_ValueTypeSize[MET_DOUBLE])
      {
        eType = MET_DOUBLE;
      }
      break;
    case IOComponentEnum::DOUBLE:
      if constexpr (sizeof(double) == MET_ValueTypeSize[MET_DOUBLE])
      {
        eType = MET_DOUBLE;
      }
      else if constexpr (sizeof(double) == MET_ValueTypeSize[MET_FLOAT])
      {
        eType = MET_FLOAT;
      }
      break;
  }

  const auto dSize = make_unique_for_overwrite<int[]>(numberOfDimensions);
  const auto eSpacing = make_unique_for_overwrite<double[]>(numberOfDimensions);
  const auto eOrigin = make_unique_for_overwrite<double[]>(numberOfDimensions);
  for (unsigned int ii = 0; ii < numberOfDimensions; ++ii)
  {
    dSize[ii] = this->GetDimensions(ii);
    eSpacing[ii] = this->GetSpacing(ii);
    eOrigin[ii] = this->GetOrigin(ii);
  }

  m_MetaImage.InitializeEssential(
    numberOfDimensions, dSize.get(), eSpacing.get(), eType, nChannels, const_cast<void *>(buffer));
  m_MetaImage.Position(eOrigin.get());
  m_MetaImage.BinaryData(binaryData);

  // Write the image Information
  this->WriteImageInformation();

  if (numberOfDimensions == 3)
  {
    AnatomicalOrientation::DirectionType dir;
    std::vector<double>                  dirx = this->GetDirection(0);
    std::vector<double>                  diry = this->GetDirection(1);
    std::vector<double>                  dirz = this->GetDirection(2);
    for (unsigned int ii = 0; ii < 3; ++ii)
    {
      dir[ii][0] = dirx[ii];
      dir[ii][1] = diry[ii];
      dir[ii][2] = dirz[ii];
    }
    const AnatomicalOrientation coordOrient(dir);

    // Mapping from DICOM CoordinateEnum defined as the increasing direction to
    // the MetaIO enum which has from/to orientation defined.
    const std::map<AnatomicalOrientation::CoordinateEnum, int> axisToMetOrientation{
      { AnatomicalOrientation::CoordinateEnum::RightToLeft, MET_ORIENTATION_RL },
      { AnatomicalOrientation::CoordinateEnum::LeftToRight, MET_ORIENTATION_LR },
      { AnatomicalOrientation::CoordinateEnum::AnteriorToPosterior, MET_ORIENTATION_AP },
      { AnatomicalOrientation::CoordinateEnum::PosteriorToAnterior, MET_ORIENTATION_PA },
      { AnatomicalOrientation::CoordinateEnum::InferiorToSuperior, MET_ORIENTATION_IS },
      { AnatomicalOrientation::CoordinateEnum::SuperiorToInferior, MET_ORIENTATION_SI }
    };

    m_MetaImage.AnatomicalOrientation(0, axisToMetOrientation.at(coordOrient.GetPrimaryTerm()));
    m_MetaImage.AnatomicalOrientation(1, axisToMetOrientation.at(coordOrient.GetSecondaryTerm()));
    m_MetaImage.AnatomicalOrientation(2, axisToMetOrientation.at(coordOrient.GetTertiaryTerm()));
  }
  // Propagate direction cosine information.
  auto * transformMatrix = static_cast<double *>(malloc(numberOfDimensions * numberOfDimensions * sizeof(double)));
  if (transformMatrix)
  {
    for (unsigned int ii = 0; ii < numberOfDimensions; ++ii)
    {
      for (unsigned int jj = 0; jj < numberOfDimensions; ++jj)
      {
        transformMatrix[ii * numberOfDimensions + jj] = this->GetDirection(ii)[jj];
      }
    }
    m_MetaImage.TransformMatrix(transformMatrix);
    free(transformMatrix);
  }

  m_MetaImage.CompressedData(m_UseCompression);
  m_MetaImage.CompressionLevel(this->GetCompressionLevel());

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
    const auto indexMin = make_unique_for_overwrite<int[]>(numberOfDimensions);
    const auto indexMax = make_unique_for_overwrite<int[]>(numberOfDimensions);
    for (unsigned int ii = 0; ii < numberOfDimensions; ++ii)
    {
      // the dimensions of m_IORegion should match out requested
      // dimensions, but ImageIORegion will throw an
      // exception if out of bounds
      indexMin[ii] = m_IORegion.GetIndex()[ii];
      indexMax[ii] = m_IORegion.GetIndex()[ii] + m_IORegion.GetSize()[ii] - 1;
    }

    if (!m_MetaImage.WriteROI(indexMin.get(), indexMax.get(), m_FileName.c_str()))
    {
      itkExceptionMacro("File ROI cannot be written: " << this->GetFileName() << std::endl
                                                       << "Reason: " << itksys::SystemTools::GetLastSystemError());
    }
  }
  else
  {
    if (!m_MetaImage.Write(m_FileName.c_str()))
    {
      itkExceptionMacro("File cannot be written: " << this->GetFileName() << std::endl
                                                   << "Reason: " << itksys::SystemTools::GetLastSystemError());
    }
  }
}

/** Given a requested region, determine what could be the region that we can
 * read from the file. This is called the streamable region, which will be
 * smaller than the LargestPossibleRegion and greater or equal to the
 * RequestedRegion */
ImageIORegion
MetaImageIO::GenerateStreamableReadRegionFromRequestedRegion(const ImageIORegion & requestedRegion) const
{
  //
  // The default implementations determines that the streamable region is
  // equal to the largest possible region of the image.
  //
  ImageIORegion streamableRegion(this->m_NumberOfDimensions);

  if (!m_UseStreamedReading)
  {
    for (unsigned int i = 0; i < this->m_NumberOfDimensions; ++i)
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
MetaImageIO::GetActualNumberOfSplitsForWriting(unsigned int          numberOfRequestedSplits,
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
      itkDebugMacro("Meta IO is not streaming now!");
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
    std::string   errorMessage;
    const Pointer headerImageIOReader = Self::New();

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

    if (!errorMessage.empty())
    {
      // 0) Can't read file
    }
    // 1)file is not compressed
    else if (headerImageIOReader->m_MetaImage.CompressedData())
    {
      errorMessage = "File is compressed: " + m_FileName;
    }
    // 2)pixel type
    // this->GetPixelType() is not verified because the metaio file format
    // stores all multi-component types as arrays, so it does not
    // distinguish between pixel types. Also as long as the component
    // and number of components match we should be able to paste, that
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

    if (!errorMessage.empty())
    {
      itkExceptionMacro("Unable to paste because pasting file exists and is different. " << errorMessage);
    }
    else if (headerImageIOReader->GetPixelType() != this->GetPixelType())
    {
      // since there is currently poor support for pixel types in
      // MetaIO we will just warn when it does not match
      itkWarningMacro("Pixel types does not match file, but component type and number of components do.");
    }
  }
  else if (numberOfRequestedSplits != 1)
  {
    // we are going be streaming

    // need to remove the file incase the file doesn't match our
    // current header/meta data information
    if (!itksys::SystemTools::RemoveFile(m_FileName))
    {
      itkExceptionMacro("Unable to remove file for streaming: " << m_FileName);
    }
  }

  return GetActualNumberOfSplitsForWritingCanStreamWrite(numberOfRequestedSplits, pasteRegion);
}

ImageIORegion
MetaImageIO::GetSplitRegionForWriting(unsigned int          ithPiece,
                                      unsigned int          numberOfActualSplits,
                                      const ImageIORegion & pasteRegion,
                                      const ImageIORegion & itkNotUsed(largestPossibleRegion))
{
  return GetSplitRegionForWritingCanStreamWrite(ithPiece, numberOfActualSplits, pasteRegion);
}

void
MetaImageIO::SetDefaultDoublePrecision(unsigned int precision)
{
  itkInitGlobalsMacro(DefaultDoublePrecision);
  *m_DefaultDoublePrecision = precision;
}

unsigned int
MetaImageIO::GetDefaultDoublePrecision()
{
  itkInitGlobalsMacro(DefaultDoublePrecision);
  return *MetaImageIO::GetDefaultDoublePrecisionPointer();
}

} // end namespace itk
