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

#include "itkImageIOBase.h"
#include "itkImageRegionSplitterSlowDimension.h"
#include <mutex>
#include "itksys/SystemTools.hxx"
#include "itkPrintHelper.h"


namespace itk
{
ImageIOBase::ImageIOBase() { Reset(false); }

void
ImageIOBase::Reset(const bool)
{
  m_Initialized = false;
  m_FileName = "";
  m_NumberOfComponents = 1;
  for (unsigned int i = 0; i < m_NumberOfDimensions; ++i)
  {
    m_Dimensions[i] = 0;
    m_Strides[i] = 0;
  }
  m_NumberOfDimensions = 0;

  m_UseStreamedReading = false;
  m_UseStreamedWriting = false;
  m_ExpandRGBPalette = true;
  m_IsReadAsScalarPlusPalette = false;
  m_WritePalette = false;
}

ImageIOBase::~ImageIOBase() = default;

const ImageIOBase::ArrayOfExtensionsType &
ImageIOBase::GetSupportedWriteExtensions() const
{
  return this->m_SupportedWriteExtensions;
}

const ImageIOBase::ArrayOfExtensionsType &
ImageIOBase::GetSupportedReadExtensions() const
{
  return this->m_SupportedReadExtensions;
}

void
ImageIOBase::AddSupportedReadExtension(const char * extension)
{
  this->m_SupportedReadExtensions.emplace_back(extension);
}

void
ImageIOBase::AddSupportedWriteExtension(const char * extension)
{
  this->m_SupportedWriteExtensions.emplace_back(extension);
}

void
ImageIOBase::SetSupportedReadExtensions(const ArrayOfExtensionsType & extensions)
{
  this->m_SupportedReadExtensions = extensions;
}

void
ImageIOBase::SetSupportedWriteExtensions(const ArrayOfExtensionsType & extensions)
{
  this->m_SupportedWriteExtensions = extensions;
}

void
ImageIOBase::Resize(const unsigned int numDimensions, const unsigned int * dimensions)
{
  m_NumberOfDimensions = numDimensions;
  if (dimensions != nullptr)
  {
    for (unsigned int i = 0; i < m_NumberOfDimensions; ++i)
    {
      m_Dimensions[i] = dimensions[i];
    }
    ComputeStrides();
  }
}

void
ImageIOBase::SetDimensions(unsigned int i, SizeValueType dim)
{
  if (i >= m_Dimensions.size())
  {
    itkExceptionMacro("Index: " << i << " is out of bounds, expected maximum is " << m_Dimensions.size());
  }
  this->Modified();
  m_Dimensions[i] = dim;
}

void
ImageIOBase::SetOrigin(unsigned int i, double origin)
{
  if (i >= m_Origin.size())
  {
    itkExceptionMacro("Index: " << i << " is out of bounds, expected maximum is " << m_Origin.size());
  }
  this->Modified();
  m_Origin[i] = origin;
}

void
ImageIOBase::SetSpacing(unsigned int i, double spacing)
{
  if (i >= m_Spacing.size())
  {
    itkExceptionMacro("Index: " << i << " is out of bounds, expected maximum is " << m_Spacing.size());
  }
  this->Modified();
  m_Spacing[i] = spacing;
}

void
ImageIOBase::SetDirection(unsigned int i, const std::vector<double> & direction)
{
  if (i >= m_Direction.size())
  {
    itkExceptionMacro("Index: " << i << " is out of bounds, expected maximum is " << m_Direction.size());
  }
  this->Modified();
  m_Direction[i] = direction;
}

void
ImageIOBase::SetDirection(unsigned int i, const vnl_vector<double> & direction)
{
  if (i >= m_Direction.size())
  {
    itkExceptionMacro("Index: " << i << " is out of bounds, expected maximum is " << m_Direction.size());
  }
  this->Modified();
  std::vector<double> v;
  v.resize(m_Direction.size());
  // Note: direction.size() <= v.size().
  for (unsigned int j = 0; j < direction.size(); ++j)
  {
    v[j] = direction[j];
  }
  m_Direction[i] = v;
}

const std::type_info &
ImageIOBase::GetComponentTypeInfo() const
{
  switch (m_ComponentType)
  {
    case IOComponentEnum::UCHAR:
      return typeid(unsigned char);
    case IOComponentEnum::CHAR:
      return typeid(char);
    case IOComponentEnum::USHORT:
      return typeid(unsigned short);
    case IOComponentEnum::SHORT:
      return typeid(short);
    case IOComponentEnum::UINT:
      return typeid(unsigned int);
    case IOComponentEnum::INT:
      return typeid(int);
    case IOComponentEnum::ULONG:
      return typeid(unsigned long);
    case IOComponentEnum::LONG:
      return typeid(long);
    case IOComponentEnum::ULONGLONG:
      return typeid(unsigned long long);
    case IOComponentEnum::LONGLONG:
      return typeid(long long);
    case IOComponentEnum::FLOAT:
      return typeid(float);
    case IOComponentEnum::DOUBLE:
      return typeid(double);
    case IOComponentEnum::UNKNOWNCOMPONENTTYPE:
    default:
      itkExceptionMacro("Unknown component type: " << m_ComponentType);
  }
}

void
ImageIOBase::ComputeStrides()
{
  m_Strides[0] = this->GetComponentSize();
  m_Strides[1] = m_NumberOfComponents * m_Strides[0];
  for (unsigned int i = 2; i <= (m_NumberOfDimensions + 1); ++i)
  {
    m_Strides[i] = static_cast<SizeType>(m_Dimensions[i - 2]) * m_Strides[i - 1];
  }
}

// Calculates the image size in PIXELS
ImageIOBase::SizeType
ImageIOBase::GetImageSizeInPixels() const
{
  SizeType numPixels = 1;
  for (unsigned int i = 0; i < m_NumberOfDimensions; ++i)
  {
    numPixels *= m_Dimensions[i];
  }

  return numPixels;
}

ImageIOBase::SizeType
ImageIOBase::GetImageSizeInComponents() const
{
  return (this->GetImageSizeInPixels() * m_NumberOfComponents);
}

ImageIOBase::SizeType
ImageIOBase::GetImageSizeInBytes() const
{
  return (this->GetImageSizeInComponents() * this->GetComponentSize());
}

ImageIOBase::SizeType
ImageIOBase::GetComponentStride() const
{
  return m_Strides[0];
}

ImageIOBase::SizeType
ImageIOBase::GetPixelStride() const
{
  return m_Strides[1];
}

ImageIOBase::SizeType
ImageIOBase::GetRowStride() const
{
  return m_Strides[2];
}

ImageIOBase::SizeType
ImageIOBase::GetSliceStride() const
{
  return m_Strides[3];
}

void
ImageIOBase::SetNumberOfDimensions(unsigned int dim)
{
  if (dim != m_NumberOfDimensions)
  {
    m_Origin.resize(dim);
    m_Spacing.resize(dim);
    m_Direction.resize(dim);
    m_Strides.resize(dim + 2);
    m_NumberOfDimensions = dim;
    m_Dimensions.resize(dim);
    m_Direction.resize(dim);
    std::vector<double> axis(dim);
    for (unsigned int i = 0; i < dim; ++i)
    {
      for (unsigned int j = 0; j < dim; ++j)
      {
        if (i == j)
        {
          axis[j] = 1.0;
        }
        else
        {
          axis[j] = 0.0;
        }
      }
      this->SetDirection(i, axis);
      this->SetOrigin(i, 0.0);
      this->SetSpacing(i, 1.0);
    }
    this->Modified();
  }
}

bool
ImageIOBase::ReadBufferAsBinary(std::istream & is, void * buffer, ImageIOBase::SizeType num)
{
  const auto numberOfBytesToBeRead = Math::CastWithRangeCheck<std::streamsize>(num);

  is.read(static_cast<char *>(buffer), numberOfBytesToBeRead);

  const std::streamsize numberOfBytesRead = is.gcount();

#ifdef __APPLE_CC__
  // fail() is broken in the Mac. It returns true when reaches eof().
  if (numberOfBytesRead != numberOfBytesToBeRead)
#else
  if ((numberOfBytesRead != numberOfBytesToBeRead) || is.fail())
#endif
  {
    return false; // read failed
  }

  return true;
}

unsigned int
ImageIOBase::GetPixelSize() const
{
  if (m_ComponentType == IOComponentEnum::UNKNOWNCOMPONENTTYPE || m_PixelType == IOPixelEnum::UNKNOWNPIXELTYPE)
  {
    itkExceptionMacro("Unknown pixel or component type: (" << m_PixelType << ", " << m_ComponentType << ')');
  }

  return this->GetComponentSize() * this->GetNumberOfComponents();
}


void
ImageIOBase::SetCompressor(std::string _c)
{
  if (this->m_Compressor != _c)
  {
    this->m_Compressor = _c;
    this->Modified();

    std::transform(_c.begin(), _c.end(), _c.begin(), ::toupper);
    this->InternalSetCompressor(_c);
  }
}

void
ImageIOBase::SetMaximumCompressionLevel(int _MaximumCompressionLevel)
{
  this->m_MaximumCompressionLevel = _MaximumCompressionLevel;
  this->SetCompressionLevel(this->GetCompressionLevel());
}

void
ImageIOBase::InternalSetCompressor(const std::string & _compressor)
{
  if (!_compressor.empty())
  {
    itkWarningMacro("Unknown compressor: \"" << _compressor << "\", setting to default.");
    this->SetCompressor("");
  }
}

unsigned int
ImageIOBase::GetComponentSize() const
{
  switch (m_ComponentType)
  {
    case IOComponentEnum::UCHAR:
      return sizeof(unsigned char);
    case IOComponentEnum::CHAR:
      return sizeof(char);
    case IOComponentEnum::USHORT:
      return sizeof(unsigned short);
    case IOComponentEnum::SHORT:
      return sizeof(short);
    case IOComponentEnum::UINT:
      return sizeof(unsigned int);
    case IOComponentEnum::INT:
      return sizeof(int);
    case IOComponentEnum::ULONG:
      return sizeof(unsigned long);
    case IOComponentEnum::LONG:
      return sizeof(long);
    case IOComponentEnum::ULONGLONG:
      return sizeof(unsigned long long);
    case IOComponentEnum::LONGLONG:
      return sizeof(long long);
    case IOComponentEnum::FLOAT:
      return sizeof(float);
    case IOComponentEnum::DOUBLE:
      return sizeof(double);
    case IOComponentEnum::UNKNOWNCOMPONENTTYPE:
    default:
      itkExceptionMacro("Unknown component type: " << m_ComponentType);
  }
}

std::string
ImageIOBase::GetFileTypeAsString(IOFileEnum t) const
{
  switch (t)
  {
    case IOFileEnum::ASCII:
      return { "ASCII" };
    case IOFileEnum::Binary:
      return { "Binary" };
    case IOFileEnum::TypeNotApplicable:
    default:
      return { "TypeNotApplicable" };
  }
  // Not reachable return s = "TypeNotApplicable";
}

std::string
ImageIOBase::GetByteOrderAsString(IOByteOrderEnum t) const
{
  switch (t)
  {
    case IOByteOrderEnum::BigEndian:
      return { "BigEndian" };
    case IOByteOrderEnum::LittleEndian:
      return { "LittleEndian" };
    case IOByteOrderEnum::OrderNotApplicable:
    default:
      return { "OrderNotApplicable" };
  }
}

std::string
ImageIOBase::GetComponentTypeAsString(IOComponentEnum t)
{
  switch (t)
  {
    case IOComponentEnum::UCHAR:
      return { "unsigned_char" };
    case IOComponentEnum::CHAR:
      return { "char" };
    case IOComponentEnum::USHORT:
      return { "unsigned_short" };
    case IOComponentEnum::SHORT:
      return { "short" };
    case IOComponentEnum::UINT:
      return { "unsigned_int" };
    case IOComponentEnum::INT:
      return { "int" };
    case IOComponentEnum::ULONG:
      return { "unsigned_long" };
    case IOComponentEnum::LONG:
      return { "long" };
    case IOComponentEnum::ULONGLONG:
      return { "unsigned_long_long" };
    case IOComponentEnum::LONGLONG:
      return { "long_long" };
    case IOComponentEnum::FLOAT:
      return { "float" };
    case IOComponentEnum::DOUBLE:
      return { "double" };
    case IOComponentEnum::UNKNOWNCOMPONENTTYPE:
      return { "unknown" };
    default:
      return { "unknown" };
  }
}

IOComponentEnum
ImageIOBase::GetComponentTypeFromString(const std::string & typeString)
{
  if (typeString.compare("unsigned_char") == 0)
  {
    return IOComponentEnum::UCHAR;
  }
  if (typeString.compare("char") == 0)
  {
    return IOComponentEnum::CHAR;
  }
  else if (typeString.compare("unsigned_short") == 0)
  {
    return IOComponentEnum::USHORT;
  }
  else if (typeString.compare("short") == 0)
  {
    return IOComponentEnum::SHORT;
  }
  else if (typeString.compare("unsigned_int") == 0)
  {
    return IOComponentEnum::UINT;
  }
  else if (typeString.compare("int") == 0)
  {
    return IOComponentEnum::INT;
  }
  else if (typeString.compare("unsigned_long") == 0)
  {
    return IOComponentEnum::ULONG;
  }
  else if (typeString.compare("long") == 0)
  {
    return IOComponentEnum::LONG;
  }
  else if (typeString.compare("unsigned_long_long") == 0)
  {
    return IOComponentEnum::ULONGLONG;
  }
  else if (typeString.compare("long_long") == 0)
  {
    return IOComponentEnum::LONGLONG;
  }
  else if (typeString.compare("float") == 0)
  {
    return IOComponentEnum::FLOAT;
  }
  else if (typeString.compare("double") == 0)
  {
    return IOComponentEnum::DOUBLE;
  }
  else
  {
    return IOComponentEnum::UNKNOWNCOMPONENTTYPE;
  }
}

std::string
ImageIOBase::GetPixelTypeAsString(IOPixelEnum t)
{
  switch (t)
  {
    case IOPixelEnum::SCALAR:
      return { "scalar" };
    case IOPixelEnum::VECTOR:
      return { "vector" };
    case IOPixelEnum::COVARIANTVECTOR:
      return { "covariant_vector" };
    case IOPixelEnum::POINT:
      return { "point" };
    case IOPixelEnum::OFFSET:
      return { "offset" };
    case IOPixelEnum::RGB:
      return { "rgb" };
    case IOPixelEnum::RGBA:
      return { "rgba" };
    case IOPixelEnum::SYMMETRICSECONDRANKTENSOR:
      return { "symmetric_second_rank_tensor" };
    case IOPixelEnum::DIFFUSIONTENSOR3D:
      return { "diffusion_tensor_3D" };
    case IOPixelEnum::COMPLEX:
      return { "complex" };
    case IOPixelEnum::FIXEDARRAY:
      return { "fixed_array" };
    case IOPixelEnum::MATRIX:
      return { "matrix" };
    case IOPixelEnum::UNKNOWNPIXELTYPE:
      return { "unknown" };
    default:
      return { "unknown" };
  }
}

IOPixelEnum
ImageIOBase::GetPixelTypeFromString(const std::string & pixelString)
{
  if (pixelString.compare("scalar") == 0)
  {
    return IOPixelEnum::SCALAR;
  }
  if (pixelString.compare("vector") == 0)
  {
    return IOPixelEnum::VECTOR;
  }
  else if (pixelString.compare("covariant_vector") == 0)
  {
    return IOPixelEnum::COVARIANTVECTOR;
  }
  else if (pixelString.compare("point") == 0)
  {
    return IOPixelEnum::POINT;
  }
  else if (pixelString.compare("offset") == 0)
  {
    return IOPixelEnum::OFFSET;
  }
  else if (pixelString.compare("rgb") == 0)
  {
    return IOPixelEnum::RGB;
  }
  else if (pixelString.compare("rgba") == 0)
  {
    return IOPixelEnum::RGBA;
  }
  else if (pixelString.compare("symmetric_second_rank_tensor") == 0)
  {
    return IOPixelEnum::SYMMETRICSECONDRANKTENSOR;
  }
  else if (pixelString.compare("diffusion_tensor_3D") == 0)
  {
    return IOPixelEnum::DIFFUSIONTENSOR3D;
  }
  else if (pixelString.compare("complex") == 0)
  {
    return IOPixelEnum::COMPLEX;
  }
  else if (pixelString.compare("fixed_array") == 0)
  {
    return IOPixelEnum::FIXEDARRAY;
  }
  else if (pixelString.compare("matrix") == 0)
  {
    return IOPixelEnum::MATRIX;
  }
  else
  {
    return IOPixelEnum::UNKNOWNPIXELTYPE;
  }
}

void
ImageIOBase::OpenFileForReading(std::ifstream & inputStream, const std::string & filename, bool ascii)
{
  // Make sure that we have a file to
  if (filename.empty())
  {
    itkExceptionMacro("A FileName must be specified.");
  }

  // Close file from any previous image
  if (inputStream.is_open())
  {
    inputStream.close();
  }

  // Open the new file for reading
  itkDebugMacro("Opening file for reading: " << filename);

  std::ios::openmode mode = std::ios::in;
  if (!ascii)
  {
    mode |= std::ios::binary;
  }

#ifdef _MSC_VER
  const std::wstring uncpath = itksys::SystemTools::ConvertToWindowsExtendedPath(filename.c_str());
  inputStream.open(uncpath.c_str(), std::ios::binary);
#else
  inputStream.open(filename.c_str(), mode);
#endif

  if (!inputStream.is_open() || inputStream.fail())
  {
    itkExceptionMacro("Could not open file: " << filename << " for reading." << std::endl
                                              << "Reason: " << itksys::SystemTools::GetLastSystemError());
  }
}

void
ImageIOBase::OpenFileForWriting(std::ofstream & outputStream, const std::string & filename, bool truncate, bool ascii)
{
  // Make sure that we have a file to
  if (filename.empty())
  {
    itkExceptionMacro("A FileName must be specified.");
  }

  // Close file from any previous image
  if (outputStream.is_open())
  {
    outputStream.close();
  }

  // Open the new file for writing
  itkDebugMacro("Opening file for writing: " << filename);

  std::ios::openmode mode = std::ios::out;
  if (truncate)
  {
    // typically, ios::out also implies ios::trunc, but being explicit is safer
    mode |= std::ios::trunc;
  }
  else
  {
    mode |= std::ios::in;
    // opening a nonexistent file for reading + writing is not allowed on some platforms
    if (!itksys::SystemTools::FileExists(filename.c_str()))
    {
      itksys::SystemTools::Touch(filename, true);
      // don't worry about failure here, errors should be detected later when the file
      // is "actually" opened, unless there is a race condition
    }
  }
  if (!ascii)
  {
    mode |= std::ios::binary;
  }

  outputStream.open(filename.c_str(), mode);

  if (!outputStream.is_open() || outputStream.fail())
  {
    itkExceptionMacro("Could not open file: " << filename << " for writing." << std::endl
                                              << "Reason: " << itksys::SystemTools::GetLastSystemError());
  }
}

namespace
{
template <typename TComponent>
void
WriteBuffer(std::ostream & os, const TComponent * buffer, ImageIOBase::SizeType num)
{
  const TComponent * ptr = buffer;

  using PrintType = typename itk::NumericTraits<TComponent>::PrintType;
  for (ImageIOBase::SizeType i = 0; i < num; ++i)
  {
    if (!(i % 6) && i)
    {
      os << '\n';
    }
    os << PrintType(*ptr++) << ' ';
  }
}
} // namespace
void
ImageIOBase::WriteBufferAsASCII(std::ostream &        os,
                                const void *          buffer,
                                IOComponentEnum       ctype,
                                ImageIOBase::SizeType numComp)
{
  switch (ctype)
  {
    case IOComponentEnum::UCHAR:
    {
      using Type = const unsigned char *;
      auto buf = static_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
    }
    break;
    case IOComponentEnum::CHAR:
    {
      using Type = const char *;
      auto buf = static_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
    }
    break;

    case IOComponentEnum::USHORT:
    {
      using Type = const unsigned short *;
      auto buf = static_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
    }
    break;

    case IOComponentEnum::SHORT:
    {
      using Type = const short *;
      auto buf = static_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
    }
    break;

    case IOComponentEnum::UINT:
    {
      using Type = const unsigned int *;
      auto buf = static_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
    }
    break;

    case IOComponentEnum::INT:
    {
      using Type = const int *;
      auto buf = static_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
    }
    break;

    case IOComponentEnum::ULONG:
    {
      using Type = const unsigned long *;
      auto buf = static_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
    }
    break;

    case IOComponentEnum::LONG:
    {
      using Type = const long *;
      auto buf = static_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
    }
    break;

    case IOComponentEnum::ULONGLONG:
    {
      using Type = const unsigned long long *;
      auto buf = static_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
    }
    break;

    case IOComponentEnum::LONGLONG:
    {
      using Type = const long long *;
      auto buf = static_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
    }
    break;

    case IOComponentEnum::FLOAT:
    {
      using Type = const float *;
      auto buf = static_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
    }
    break;

    case IOComponentEnum::DOUBLE:
    {
      using Type = const double *;
      auto buf = static_cast<Type>(buffer);
      WriteBuffer(os, buf, numComp);
    }
    break;

    default:
      break;
  }
}

namespace
{
template <typename TComponent>
void
ReadBuffer(std::istream & is, TComponent * buffer, ImageIOBase::SizeType num)
{
  using PrintType = typename itk::NumericTraits<TComponent>::PrintType;
  PrintType    temp;
  TComponent * ptr = buffer;
  for (ImageIOBase::SizeType i = 0; i < num; i++, ptr++)
  {
    is >> temp;
    *ptr = static_cast<TComponent>(temp);
  }
}
} // namespace
void
ImageIOBase::ReadBufferAsASCII(std::istream & is, void * buffer, IOComponentEnum ctype, ImageIOBase::SizeType numComp)
{
  switch (ctype)
  {
    case IOComponentEnum::UCHAR:
    {
      auto * buf = static_cast<unsigned char *>(buffer);
      ReadBuffer(is, buf, numComp);
    }
    break;
    case IOComponentEnum::CHAR:
    {
      auto * buf = static_cast<char *>(buffer);
      ReadBuffer(is, buf, numComp);
    }
    break;

    case IOComponentEnum::USHORT:
    {
      auto * buf = static_cast<unsigned short *>(buffer);
      ReadBuffer(is, buf, numComp);
    }
    break;

    case IOComponentEnum::SHORT:
    {
      auto * buf = static_cast<short *>(buffer);
      ReadBuffer(is, buf, numComp);
    }
    break;

    case IOComponentEnum::UINT:
    {
      auto * buf = static_cast<unsigned int *>(buffer);
      ReadBuffer(is, buf, numComp);
    }
    break;

    case IOComponentEnum::INT:
    {
      auto * buf = static_cast<int *>(buffer);
      ReadBuffer(is, buf, numComp);
    }
    break;

    case IOComponentEnum::ULONG:
    {
      auto * buf = static_cast<unsigned long *>(buffer);
      ReadBuffer(is, buf, numComp);
    }
    break;

    case IOComponentEnum::LONG:
    {
      auto * buf = static_cast<long *>(buffer);
      ReadBuffer(is, buf, numComp);
    }
    break;

    case IOComponentEnum::ULONGLONG:
    {
      auto * buf = static_cast<unsigned long long *>(buffer);
      ReadBuffer(is, buf, numComp);
    }
    break;

    case IOComponentEnum::LONGLONG:
    {
      auto * buf = static_cast<long long *>(buffer);
      ReadBuffer(is, buf, numComp);
    }
    break;

    case IOComponentEnum::FLOAT:
    {
      auto * buf = static_cast<float *>(buffer);
      ReadBuffer(is, buf, numComp);
    }
    break;

    case IOComponentEnum::DOUBLE:
    {
      auto * buf = static_cast<double *>(buffer);
      ReadBuffer(is, buf, numComp);
    }
    break;

    default:
      break;
  }
}

namespace
{
std::mutex                       ioDefaultSplitterMutex;
ImageRegionSplitterBase::Pointer ioDefaultSplitter;

} // namespace

const ImageRegionSplitterBase *
ImageIOBase::GetImageRegionSplitter() const
{
  if (ioDefaultSplitter.IsNull())
  {
    // thread safe lazy initialization,  prevent race condition on
    // setting, with an atomic set if null.
    const std::lock_guard<std::mutex> lockGuard(ioDefaultSplitterMutex);
    if (ioDefaultSplitter.IsNull())
    {
      ioDefaultSplitter = ImageRegionSplitterSlowDimension::New().GetPointer();
    }
  }
  return ioDefaultSplitter;
}


bool
ImageIOBase::HasSupportedReadExtension(const char * fileName, bool ignoreCase)
{
  return this->HasSupportedExtension(fileName, this->GetSupportedReadExtensions(), ignoreCase);
}


bool
ImageIOBase::HasSupportedWriteExtension(const char * fileName, bool ignoreCase)
{

  return this->HasSupportedExtension(fileName, this->GetSupportedWriteExtensions(), ignoreCase);
}


bool
ImageIOBase::HasSupportedExtension(const char *                               filename,
                                   const ImageIOBase::ArrayOfExtensionsType & supportedExtensions,
                                   bool                                       ignoreCase)
{

  std::string ext = itksys::SystemTools::GetFilenameLastExtension(filename);
  if (ignoreCase)
  {
    std::transform(ext.begin(), ext.end(), ext.begin(), ::tolower);
  }

  for (auto && candidate : supportedExtensions)
  {
    if (ignoreCase)
    {
      size_t n = candidate.size();
      if (n == ext.size() && n > 0)
      {

        while (true)
        {
          --n;
          if (ext[n] != ::tolower(candidate[n]))
          {
            break;
          }
          if (n == 0)
          {
            return true;
          }
        }
      }
    }
    else
    {
      if (candidate == ext)
      {
        return true;
      }
    }
  }
  return false;
}

unsigned int
ImageIOBase::GetActualNumberOfSplitsForWritingCanStreamWrite(unsigned int          numberOfRequestedSplits,
                                                             const ImageIORegion & pasteRegion) const
{
  const ImageRegionSplitterBase * splitter = this->GetImageRegionSplitter();
  return splitter->GetNumberOfSplits(pasteRegion, numberOfRequestedSplits);
}

unsigned int
ImageIOBase::GetActualNumberOfSplitsForWriting(unsigned int          numberOfRequestedSplits,
                                               const ImageIORegion & pasteRegion,
                                               const ImageIORegion & largestPossibleRegion)
{
  if (this->CanStreamWrite())
  {
    return GetActualNumberOfSplitsForWritingCanStreamWrite(numberOfRequestedSplits, pasteRegion);
  }
  if (pasteRegion != largestPossibleRegion)
  {
    itkExceptionMacro("Pasting is not supported! Can't write:" << this->GetFileName());
  }
  if (numberOfRequestedSplits != 1)
  {
    itkDebugMacro("Requested more then 1 splits for streaming");
    itkDebugMacro("This IO class does not support streaming!");
  }
  return 1;
}

ImageIORegion
ImageIOBase::GetSplitRegionForWritingCanStreamWrite(unsigned int          ithPiece,
                                                    unsigned int          numberOfActualSplits,
                                                    const ImageIORegion & pasteRegion) const
{
  ImageIORegion splitRegion = pasteRegion;

  const ImageRegionSplitterBase * splitter = this->GetImageRegionSplitter();
  splitter->GetSplit(ithPiece, numberOfActualSplits, splitRegion);

  return splitRegion;
}

ImageIORegion
ImageIOBase::GetSplitRegionForWriting(unsigned int          ithPiece,
                                      unsigned int          numberOfActualSplits,
                                      const ImageIORegion & pasteRegion,
                                      const ImageIORegion & largestPossibleRegion)
{
  if (this->CanStreamWrite())
  {
    return GetSplitRegionForWritingCanStreamWrite(ithPiece, numberOfActualSplits, pasteRegion);
  }
  return largestPossibleRegion;
}

/** Given a requested region, determine what could be the region that we can
 * read from the file. This is called the streamable region, which will be
 * smaller than the LargestPossibleRegion and greater or equal to the
 * RequestedRegion */
ImageIORegion
ImageIOBase::GenerateStreamableReadRegionFromRequestedRegion(const ImageIORegion & requested) const
{
  //
  // The default implementations determines that the streamable region is
  // equal to the minimal size of the image in the file. That is two
  // say the return ImageIORegion::GetImageSizeInPixels() is equal to
  // the number in the file.
  //

  // Since the image in the file may have a lower or higher dimension
  // than the image type over which the ImageFileReader is
  // being instantiated we must choose an image dimension which will
  // represent all the pixels. That is we can trim trailing 1s.

  unsigned int minIODimension = this->m_NumberOfDimensions;

  while (minIODimension)
  {
    if (this->m_Dimensions[minIODimension - 1] == 1)
    {
      --minIODimension;
    }
    else
    {
      break;
    }
  }

  // dimension size we use to represent the region
  const unsigned int maxDimension =
    minIODimension > requested.GetImageDimension() ? minIODimension : requested.GetImageDimension();

  // First: allocate with the correct dimensions
  ImageIORegion streamableRegion(maxDimension);

  // Second: copy only the number of dimension that the file has.
  for (unsigned int i = 0; i < minIODimension; ++i)
  {
    streamableRegion.SetSize(i, this->m_Dimensions[i]);
    streamableRegion.SetIndex(i, 0);
  }

  // Third: set the rest to the default : start = 0, size = 1
  for (unsigned int j = minIODimension; j < streamableRegion.GetImageDimension(); ++j)
  {
    streamableRegion.SetSize(j, 1);
    streamableRegion.SetIndex(j, 0);
  }

  // Finally: return the streamable region
  return streamableRegion;
}

/** Return the directions that this particular ImageIO would use by default
 *  in the case the recipient image dimension is smaller than the dimension
 *  of the image in file. */
std::vector<double>
ImageIOBase::GetDefaultDirection(unsigned int k) const
{
  std::vector<double> axis;
  axis.resize(this->GetNumberOfDimensions());

  // Fill up with the equivalent of a line from an Identity matrix
  for (double & axi : axis)
  {
    axi = 0.0;
  }

  axis[k] = 1.0;

  return axis;
}

void
ImageIOBase::PrintSelf(std::ostream & os, Indent indent) const
{
  using namespace print_helper;

  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << m_FileName << std::endl;
  os << indent << "IOFileEnum: " << this->GetFileTypeAsString(m_FileType) << std::endl;
  os << indent << "IOByteOrderEnum: " << this->GetByteOrderAsString(m_ByteOrder) << std::endl;
  os << indent << "IORegion: " << std::endl;
  m_IORegion.Print(os, indent.GetNextIndent());
  os << indent << "NumberOfComponents/Pixel: " << m_NumberOfComponents << std::endl;
  os << indent << "PixeType: " << Self::GetPixelTypeAsString(m_PixelType) << std::endl;
  os << indent << "ComponentType: " << Self::GetComponentTypeAsString(m_ComponentType) << std::endl;
  os << indent << "Dimensions: " << m_Dimensions << std::endl;
  os << indent << "Origin: " << m_Origin << std::endl;
  os << indent << "Spacing: " << m_Spacing << std::endl;
  os << indent << "Direction: " << std::endl;
  for (const auto & direction : m_Direction)
  {
    os << indent << direction << std::endl;
  }
  itkPrintSelfBooleanMacro(UseCompression);
  os << indent << "CompressionLevel: " << m_CompressionLevel << std::endl;
  os << indent << "MaximumCompressionLevel: " << m_MaximumCompressionLevel << std::endl;
  os << indent << "Compressor: " << m_Compressor << std::endl;
  itkPrintSelfBooleanMacro(UseStreamedReading);
  itkPrintSelfBooleanMacro(UseStreamedWriting);
  itkPrintSelfBooleanMacro(ExpandRGBPalette);
  itkPrintSelfBooleanMacro(IsReadAsScalarPlusPalette);
  itkPrintSelfBooleanMacro(WritePalette);
}

} // namespace itk
