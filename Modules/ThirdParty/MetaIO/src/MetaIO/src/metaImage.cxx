/*============================================================================
  MetaIO
  Copyright 2000-2010 Insight Software Consortium

  Distributed under the OSI-approved BSD License (the "License");
  see accompanying file Copyright.txt for details.

  This software is distributed WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the License for more information.
============================================================================*/
#include "metaImage.h"

#include <algorithm> // for std::min & std::max
#include <cstring> // for memcpy

#if defined(__BORLANDC__) && (__BORLANDC__ >= 0x0580)
#  include <mem.h>
#endif

#if defined(_WIN32)
#  include <io.h>
#endif

// support for access
#ifndef _WIN32
#  include <climits>
#  include <csignal> /* sigprocmask */
#  include <sys/ioctl.h>
#  include <sys/param.h>
#  include <unistd.h>
#endif

namespace
{

void
openReadStream(METAIO_STREAM::ifstream & inputStream, const std::string & fname)
{
  inputStream.open(fname.c_str(), std::ios::in | std::ios::binary);
}

void
openWriteStream(METAIO_STREAM::ofstream & outputStream, const std::string & fname, bool append)
{
  if (!append)
  {
    outputStream.open(fname.c_str(), std::ios::binary | std::ios::out);
  }
  else
  {
    outputStream.open(fname.c_str(), std::ios::binary | std::ios::app | std::ios::out);
  }
}

} // end anonymous namespace

#if (METAIO_USE_NAMESPACE)
namespace METAIO_NAMESPACE
{
#endif

// 1 Gigabyte is the maximum chunk to read/write in on function call
static const std::streamoff MaxIOChunk = 1024 * 1024 * 1024;

std::set<std::string> MetaImage::m_ImageReservedKeywords = {
    "Modality",
    "DimSize",
    "SequenceID",
    "ElementSizeValid",
    "ElementSize",
    "ElementType",
    "ElementDataFileName",
 };

//
// MetaImage Constructors
//
MetaImage::MetaImage()
{
  META_DEBUG_PRINT( "MetaImage()" );

  AddReservedKeywords(m_ImageReservedKeywords);
  m_CompressionTable = new MET_CompressionTableType;
  m_CompressionTable->compressedStream = nullptr;
  m_CompressionTable->buffer = nullptr;
  MetaImage::Clear();
}

//
MetaImage::MetaImage(const char * _headerName)
{
  META_DEBUG_PRINT( "MetaImage()" );

  AddReservedKeywords(m_ImageReservedKeywords);
  m_CompressionTable = new MET_CompressionTableType;
  m_CompressionTable->compressedStream = nullptr;
  m_CompressionTable->buffer = nullptr;
  MetaImage::Clear();

  Read(_headerName);
}

//
MetaImage::MetaImage(MetaImage * _im)
{
  META_DEBUG_PRINT( "MetaImage()" );

  AddReservedKeywords(m_ImageReservedKeywords);
  m_CompressionTable = new MET_CompressionTableType;
  m_CompressionTable->compressedStream = nullptr;
  m_CompressionTable->buffer = nullptr;
  MetaImage::Clear();

  InitializeEssential(_im->NDims(),
                      _im->DimSize(),
                      _im->ElementSpacing(),
                      _im->ElementType(),
                      _im->ElementNumberOfChannels(),
                      _im->ElementData(),
                      false);
  MetaImage::CopyInfo(_im);
}

//
void
MetaImage::InitHelper(int               _nDims,
                      const int *       _dimSize,
                      const double *    _elementSpacing,
                      MET_ValueEnumType _elementType,
                      int               _elementNumberOfChannels,
                      void *            _elementData)
{
  META_DEBUG_PRINT( "MetaImage()" );

  m_CompressionTable = new MET_CompressionTableType;
  m_CompressionTable->buffer = nullptr;
  m_CompressionTable->compressedStream = nullptr;
  Clear();

  if (_elementData == nullptr)
  {
    InitializeEssential(_nDims, _dimSize, _elementSpacing, _elementType, _elementNumberOfChannels, nullptr, true);
  }
  else
  {
    InitializeEssential(_nDims, _dimSize, _elementSpacing, _elementType, _elementNumberOfChannels, _elementData, false);
  }
}

//
MetaImage::MetaImage(int               _nDims,
                     const int *       _dimSize,
                     const float *     _elementSpacing,
                     MET_ValueEnumType _elementType,
                     int               _elementNumberOfChannels,
                     void *            _elementData)
{
  AddReservedKeywords(m_ImageReservedKeywords);
  // Only consider at most 10 element of spacing:
  // See MetaObject::InitializeEssential(_nDims)
  double tmpElementSpacing[10];
  int    ndims = std::max(std::min(_nDims, 10), 0);
  for (int i = 0; i < ndims; ++i)
  {
    tmpElementSpacing[i] = static_cast<double>(_elementSpacing[i]);
  }
  InitHelper(_nDims, _dimSize, tmpElementSpacing, _elementType, _elementNumberOfChannels, _elementData);
}

//
MetaImage::MetaImage(int               _nDims,
                     const int *       _dimSize,
                     const double *    _elementSpacing,
                     MET_ValueEnumType _elementType,
                     int               _elementNumberOfChannels,
                     void *            _elementData)
{
  AddReservedKeywords(m_ImageReservedKeywords);
  InitHelper(_nDims, _dimSize, _elementSpacing, _elementType, _elementNumberOfChannels, _elementData);
}

//
MetaImage::MetaImage(int               _x,
                     int               _y,
                     double            _elementSpacingX,
                     double            _elementSpacingY,
                     MET_ValueEnumType _elementType,
                     int               _elementNumberOfChannels,
                     void *            _elementData)
{
  META_DEBUG_PRINT( "MetaImage()" );

  AddReservedKeywords(m_ImageReservedKeywords);
  m_CompressionTable = new MET_CompressionTableType;
  m_CompressionTable->compressedStream = nullptr;
  m_CompressionTable->buffer = nullptr;
  MetaImage::Clear();

  int ds[2];
  ds[0] = _x;
  ds[1] = _y;

  double es[2];
  es[0] = _elementSpacingX;
  es[1] = _elementSpacingY;

  if (_elementData == nullptr)
  {
    InitializeEssential(2, ds, es, _elementType, _elementNumberOfChannels, nullptr, true);
  }
  else
  {
    InitializeEssential(2, ds, es, _elementType, _elementNumberOfChannels, _elementData, false);
  }
}

//
MetaImage::MetaImage(int               _x,
                     int               _y,
                     int               _z,
                     double            _elementSpacingX,
                     double            _elementSpacingY,
                     double            _elementSpacingZ,
                     MET_ValueEnumType _elementType,
                     int               _elementNumberOfChannels,
                     void *            _elementData)
{
  META_DEBUG_PRINT( "MetaImage()" );

  AddReservedKeywords(m_ImageReservedKeywords);
  m_CompressionTable = new MET_CompressionTableType;
  m_CompressionTable->compressedStream = nullptr;
  m_CompressionTable->buffer = nullptr;
  MetaImage::Clear();

  int ds[3];
  ds[0] = _x;
  ds[1] = _y;
  ds[2] = _z;

  double es[3];
  es[0] = _elementSpacingX;
  es[1] = _elementSpacingY;
  es[2] = _elementSpacingZ;

  if (_elementData == nullptr)
  {
    InitializeEssential(3, ds, es, _elementType, _elementNumberOfChannels, nullptr, true);
  }
  else
  {
    InitializeEssential(3, ds, es, _elementType, _elementNumberOfChannels, _elementData, false);
  }
}

//
MetaImage::~MetaImage()
{
  MetaImage::M_ResetValues();
}

//
void
MetaImage::PrintInfo() const
{
  int i;

  MetaObject::PrintInfo();

  std::string s;
  MET_ImageModalityToString(m_Modality, s);
  std::cout << "Modality = " << s << '\n';

  std::cout << "DimSize = ";
  for (i = 0; i < m_NDims; i++)
  {
    std::cout << m_DimSize[i] << " ";
  }
  std::cout << '\n';
  std::cout << "SubQuantity = ";
  for (i = 0; i < m_NDims; i++)
  {
    std::cout << m_SubQuantity[i] << " ";
  }
  std::cout << '\n';

  std::cout << "Quantity = " << m_Quantity << '\n';


  std::cout << "HeaderSize = " << m_HeaderSize << '\n';

  std::cout << "SequenceID = ";
  for (i = 0; i < m_NDims; i++)
  {
    std::cout << m_SequenceID[i] << " ";
  }
  std::cout << '\n';

  std::cout << "ElementOrigin = ";
  for (i = 0; i < m_NDims; i++)
  {
    std::cout << m_ElementOrigin[i] << " ";
  }
  std::cout << '\n';

  std::cout << "ElementDirection = ";
  for (i = 0; i < m_NDims*m_NDims; i++)
  {
    std::cout << m_ElementDirection[i] << " ";
  }
  std::cout << '\n';

  std::cout << "ElementSizeValid = " << static_cast<int>(m_ElementSizeValid) << '\n';
  std::cout << "ElementSize = ";
  for (i = 0; i < m_NDims; i++)
  {
    std::cout << m_ElementSize[i] << " ";
  }
  std::cout << '\n';

  char str[22];
  MET_TypeToString(m_ElementType, str);
  std::cout << "ElementType = " << str << '\n';

  std::cout << "ElementNumberOfChannels = " << m_ElementNumberOfChannels << '\n';

  if (m_ElementMinMaxValid)
  {
    std::cout << "Min and Max are valid" << '\n';
    std::cout << "   Min = " << m_ElementMin << '\n';
    std::cout << "   Max = " << m_ElementMax << '\n';
  }
  else
  {
    std::cout << "Min and Max are not valid" << '\n';
  }

  std::cout << "ElementToIntensityFunctionSlope = " << m_ElementToIntensityFunctionSlope << '\n';
  std::cout << "ElementToIntensityFunctionOffset = " << m_ElementToIntensityFunctionOffset << '\n';


  std::cout << "AutoFreeElementData = " << ((m_AutoFreeElementData) ? "True" : "False") << '\n';

  std::cout << "ElementData = " << ((m_ElementData == nullptr) ? "NULL" : "Valid") << '\n';

  std::cout << "ElementDataFileName = " << m_ElementDataFileName << '\n';
}

void
MetaImage::CopyInfo(const MetaObject * _object)
{
  MetaObject::CopyInfo(_object);

  if (_object)
  {
    const MetaImage * im;
    try
    {
      im = dynamic_cast<const MetaImage *>(_object);
    }
    catch (...)
    {
      return;
    }

    if (im)
    {
      Modality(im->Modality());

      HeaderSize(im->HeaderSize());

      SequenceID(im->SequenceID());

      ElementSizeValid(im->ElementSizeValid());
      if (im->ElementSizeValid())
      {
        ElementSize(im->ElementSize());
      }

      ElementDirection(im->ElementDirection());
      ElementOrigin(im->ElementOrigin());

      ElementMinMaxValid(im->ElementMinMaxValid());
      if (im->ElementMinMaxValid())
      {
        ElementMin(im->ElementMin());
        ElementMax(im->ElementMax());
      }

      ElementToIntensityFunctionSlope(im->ElementToIntensityFunctionSlope());
      ElementToIntensityFunctionOffset(im->ElementToIntensityFunctionOffset());
    }
  }
}

/** Clear function */
void
MetaImage::Clear()
{
  META_DEBUG_PRINT( "MetaImage: Clear" );

  m_Modality = MET_MOD_UNKNOWN;

  m_DimSize[0] = 0;
  m_SubQuantity[0] = 0;
  m_Quantity = 0;

  m_HeaderSize = 0;

  memset(m_ElementOrigin, 0, sizeof(m_ElementOrigin));
  memset(m_ElementDirection, 0, sizeof(m_ElementDirection));
  for (int i = 0; i < m_NDims; i++)
  {
    m_ElementDirection[i*m_NDims+i] = 1;
  }

  memset(m_SequenceID, 0, sizeof(m_SequenceID));

  m_ElementSizeValid = false;
  memset(m_ElementSize, 0, sizeof(m_ElementSize));

  m_ElementType = MET_NONE;

  m_ElementNumberOfChannels = 1;

  m_ElementMinMaxValid = false;
  m_ElementMin = 0;
  m_ElementMax = 0;

  m_ElementToIntensityFunctionSlope = 1;
  m_ElementToIntensityFunctionOffset = 0;

  m_AutoFreeElementData = true;

  m_ElementData = nullptr;

  m_ElementDataFileName = "";

  MetaObject::Clear();

  strcpy(m_ObjectTypeName, "Image");

  // Change the default for this object
  m_BinaryData = true;

  if (m_CompressionTable)
  {
    if (m_CompressionTable->compressedStream)
    {
      inflateEnd(m_CompressionTable->compressedStream);
      delete m_CompressionTable->compressedStream;
      delete[] m_CompressionTable->buffer;
      m_CompressionTable->buffer = nullptr;
    }
    m_CompressionTable->compressedStream = nullptr;
    m_CompressionTable->offsetList.clear();
  }
  else
  {
    m_CompressionTable = new MET_CompressionTableType;
    m_CompressionTable->compressedStream = nullptr;
  }
}

bool
MetaImage::InitializeEssential(int               _nDims,
                               const int *       _dimSize,
                               const float *     _elementSpacing,
                               MET_ValueEnumType _elementType,
                               int               _elementNumberOfChannels,
                               void *            _elementData,
                               bool              _allocElementMemory,
                               bool              _initializePosition)
{
  // Only consider at most 10 element of spacing:
  // See MetaObject::InitializeEssential(_nDims)
  double tmpElementSpacing[10];
  int    ndims = std::max(std::min(_nDims, 10), 0);
  for (int i = 0; i < ndims; ++i)
  {
    tmpElementSpacing[i] = static_cast<double>(_elementSpacing[i]);
  }
  return InitializeEssential(
    _nDims, _dimSize, tmpElementSpacing, _elementType, _elementNumberOfChannels, _elementData, _allocElementMemory, _initializePosition);
}

bool
MetaImage::InitializeEssential(int               _nDims,
                               const int *       _dimSize,
                               const double *    _elementSpacing,
                               MET_ValueEnumType _elementType,
                               int               _elementNumberOfChannels,
                               void *            _elementData,
                               bool              _allocElementMemory,
                               bool              _initializePosition)
{
  META_DEBUG_PRINT( "MetaImage: Initialize" );

  if (_nDims != m_NDims)
  {
    // This conditional avoids overwriting _elementSpacing when
    //   InitializeEssential is called using m_ElementSpacing as
    //   an argument (that is passed by reference). MetaObject::InitializeEssential
    //   overwrites m_ElementSpacing with 1s.
    MetaObject::InitializeEssential(_nDims);
  }

  int i;
  if (!m_CompressionTable)
  {
    m_CompressionTable = new MET_CompressionTableType;
    m_CompressionTable->buffer = nullptr;
    m_CompressionTable->compressedStream = nullptr;
  }
  m_SubQuantity[0] = 1;
  m_Quantity = 1;
  m_ElementType = _elementType;
  m_ElementNumberOfChannels = _elementNumberOfChannels;

  m_ElementSizeValid = true;
  for (i = 0; i < m_NDims; i++)
  {
    m_DimSize[i] = _dimSize[i];
    m_Quantity *= _dimSize[i];
    if (i > 0)
    {
      m_SubQuantity[i] = m_SubQuantity[i - 1] * m_DimSize[i - 1];
    }
    m_ElementSpacing[i] = _elementSpacing[i];
    if (m_ElementSize[i] == 0)
    {
      m_ElementSize[i] = m_ElementSpacing[i];
      m_ElementSizeValid = false;
    }
  }
  if (_initializePosition)
  {
    for (i = 0; i < m_NDims; i++)
    {
      m_ElementOrigin[i] = 0;
      for (int j = 0; j < m_NDims; j++)
      {
        if (i != j)
        {
          m_ElementDirection[i*m_NDims+j] = 0;
        }
        else
        {
          m_ElementDirection[i*m_NDims+j] = 1;
        }
      }
    } 
  }

  if (_elementData != nullptr)
  {
    m_AutoFreeElementData = false;
    m_ElementData = _elementData;
  }
  else if (_allocElementMemory)
  {
    m_AutoFreeElementData = true;
    MET_SizeOfType(m_ElementType, &i);
    m_ElementData = new char[static_cast<size_t>(m_Quantity * m_ElementNumberOfChannels * i)];
    if (m_ElementData == nullptr)
    {
      std::cerr << "MetaImage:: M_Allocate:: Insufficient memory" << '\n';
      return false;
    }
  }
  else
  {
    m_AutoFreeElementData = true;
    m_ElementData = nullptr;
  }

  return true;
}


int
MetaImage::HeaderSize() const
{
  return m_HeaderSize;
}

void
MetaImage::HeaderSize(int _headerSize)
{
  m_HeaderSize = _headerSize;
}

MET_ImageModalityEnumType
MetaImage::Modality() const
{
  return m_Modality;
}

void
MetaImage::Modality(MET_ImageModalityEnumType _modality)
{
  m_Modality = _modality;
}

const int *
MetaImage::DimSize() const
{
  return m_DimSize;
}

int
MetaImage::DimSize(int _i) const
{
  return m_DimSize[_i];
}

std::streamoff
MetaImage::Quantity() const
{
  return m_Quantity;
}

const std::streamoff *
MetaImage::SubQuantity() const
{
  return m_SubQuantity;
}

std::streamoff
MetaImage::SubQuantity(int _i) const
{
  return m_SubQuantity[_i];
}

const float *
MetaImage::SequenceID() const
{
  return m_SequenceID;
}

float
MetaImage::SequenceID(int _i) const
{
  return m_SequenceID[_i];
}

void
MetaImage::SequenceID(const float * _sequenceID)
{
  memcpy(m_SequenceID, _sequenceID, m_NDims * sizeof(float));
}

void
MetaImage::SequenceID(int _i, float _value)
{
  m_SequenceID[_i] = _value;
}

bool
MetaImage::ElementSizeValid() const
{
  return m_ElementSizeValid;
}

void
MetaImage::ElementSizeValid(bool _elementSizeValid)
{
  m_ElementSizeValid = _elementSizeValid;
}

const double *
MetaImage::ElementSize() const
{
  return m_ElementSize;
}

double
MetaImage::ElementSize(int _i) const
{
  return m_ElementSize[_i];
}

void
MetaImage::ElementSize(const double * _elementSize)
{
  memcpy(m_ElementSize, _elementSize, m_NDims * sizeof(*m_ElementSize));
  m_ElementSizeValid = true;
}

void
MetaImage::ElementSize(const float * _elementSize)
{
  for (int i = 0; i < m_NDims; ++i)
  {
    m_ElementSize[i] = static_cast<double>(_elementSize[i]);
  }
  m_ElementSizeValid = true;
}


void
MetaImage::ElementSize(int _i, double _value)
{
  m_ElementSize[_i] = _value;
  m_ElementSizeValid = true;
}

const double *
MetaImage::ElementOrigin() const
{
  return m_ElementOrigin;
}

double
MetaImage::ElementOrigin(int _i) const
{
  return m_ElementOrigin[_i];
}

void
MetaImage::ElementOrigin(const double * _elementOrigin)
{
  memcpy(m_ElementOrigin, _elementOrigin, m_NDims * sizeof(*m_ElementOrigin));
}

void
MetaImage::ElementOrigin(const float * _elementOrigin)
{
  for (int i = 0; i < m_NDims; ++i)
  {
    m_ElementOrigin[i] = static_cast<double>(_elementOrigin[i]);
  }
}


void
MetaImage::ElementOrigin(int _i, double _value)
{
  m_ElementOrigin[_i] = _value;
}

const double *
MetaImage::ElementDirection() const
{
  return m_ElementDirection;
}

double
MetaImage::ElementDirection(int _i, int _j) const
{
  return m_ElementDirection[_i * m_NDims + _j];
}

void
MetaImage::ElementDirection(const double * _direction)
{
  int i;
  for (i = 0; i < m_NDims * m_NDims; i++)
  {
    m_ElementDirection[i] = _direction[i];
  }
}

void
MetaImage::ElementDirection(int _i, int _j, double _value)
{
  m_ElementDirection[_i * m_NDims + _j] = _value;
}

MET_ValueEnumType
MetaImage::ElementType() const
{
  return m_ElementType;
}

void
MetaImage::ElementType(MET_ValueEnumType _elementType)
{
  m_ElementType = _elementType;
}

int
MetaImage::ElementNumberOfChannels() const
{
  return m_ElementNumberOfChannels;
}

void
MetaImage::ElementNumberOfChannels(int _elementNumberOfChannels)
{
  m_ElementNumberOfChannels = _elementNumberOfChannels;
}

void
MetaImage::ElementByteOrderSwap(std::streamoff _quantity)
{

  // use the user provided value if provided or the internal ivar
  std::streamoff quantity = _quantity ? _quantity : m_Quantity;

  META_DEBUG_PRINT( "MetaImage: ElementByteOrderSwap" );

  int eSize;
  MET_SizeOfType(m_ElementType, &eSize);
  switch (eSize)
  {
    default:
    case 0:
    case 1:
    {
      break;
    }
    case 2:
    {
      for (size_t i = 0; i < static_cast<size_t>(quantity * m_ElementNumberOfChannels); i++)
      {
        (static_cast<MET_USHORT_TYPE *>(m_ElementData))[i] = MET_ByteOrderSwapShort((static_cast<MET_USHORT_TYPE *>(m_ElementData))[i]);
      }
      break;
    }
    case 4:
    {
      for (size_t i = 0; i < static_cast<size_t>(quantity * m_ElementNumberOfChannels); i++)
      {
        (static_cast<MET_UINT_TYPE *>(m_ElementData))[i] = MET_ByteOrderSwapLong((static_cast<MET_UINT_TYPE *>(m_ElementData))[i]);
      }
      break;
    }
    case 8:
    {
      char * data = static_cast<char *>(m_ElementData);
      for (size_t i = 0; i < static_cast<size_t>(quantity * m_ElementNumberOfChannels); i++)
      {
        MET_ByteOrderSwap8(data);
        data += 8;
      }
      break;
    }
  }
  m_BinaryDataByteOrderMSB = !m_BinaryDataByteOrderMSB;
}

bool
MetaImage::ElementByteOrderFix(std::streamoff _quantity)
{
  if (m_BinaryDataByteOrderMSB != MET_SystemByteOrderMSB())
  {
    ElementByteOrderSwap(_quantity);
    return true;
  }
  return true;
}

bool
MetaImage::ElementMinMaxValid() const
{
  return m_ElementMinMaxValid;
}

void
MetaImage::ElementMinMaxValid(bool _elementMinMaxValid)
{
  m_ElementMinMaxValid = _elementMinMaxValid;
}

void
MetaImage::ElementMinMaxRecalc()
{
  double tf;

  if (m_ElementData == nullptr)
  {
    return;
  }

  ElementByteOrderFix();

  MET_ValueToDouble(m_ElementType, m_ElementData, 0, &tf);
  m_ElementMin = tf;
  m_ElementMax = tf;
  for (size_t i = 1; i < static_cast<size_t>(m_Quantity * m_ElementNumberOfChannels); i++)
  {
    MET_ValueToDouble(m_ElementType, m_ElementData, i, &tf);
    if (tf < m_ElementMin)
    {
      m_ElementMin = tf;
    }
    else if (tf > m_ElementMax)
    {
      m_ElementMax = tf;
    }
  }

  m_ElementMinMaxValid = true;
}

double
MetaImage::ElementMin() const
{
  return m_ElementMin;
}

void
MetaImage::ElementMin(double _elementMin)
{
  m_ElementMin = _elementMin;
}

double
MetaImage::ElementMax() const
{
  return m_ElementMax;
}

void
MetaImage::ElementMax(double _elementMax)
{
  m_ElementMax = _elementMax;
}

double
MetaImage::ElementToIntensityFunctionSlope() const
{
  return m_ElementToIntensityFunctionSlope;
}

void
MetaImage::ElementToIntensityFunctionSlope(double _elementToIntensityFunctionSlope)
{
  m_ElementToIntensityFunctionSlope = _elementToIntensityFunctionSlope;
}

double
MetaImage::ElementToIntensityFunctionOffset() const
{
  return m_ElementToIntensityFunctionOffset;
}

void
MetaImage::ElementToIntensityFunctionOffset(double _elementOffset)
{
  m_ElementToIntensityFunctionOffset = _elementOffset;
}

bool
MetaImage::AutoFreeElementData() const
{
  return m_AutoFreeElementData;
}

void
MetaImage::AutoFreeElementData(bool _autoFreeElementData)
{
  m_AutoFreeElementData = _autoFreeElementData;
}

const char *
MetaImage::ElementDataFileName() const
{
  return m_ElementDataFileName.c_str();
}

void
MetaImage::ElementDataFileName(const char * _elementDataFileName)
{
  m_ElementDataFileName = _elementDataFileName;
}

void *
MetaImage::ElementData()
{
  return m_ElementData;
}

double
MetaImage::ElementData(std::streamoff _i) const
{
  double tf = 0;
  MET_ValueToDouble(m_ElementType, m_ElementData, _i, &tf);

  return tf;
}

bool
MetaImage::ElementData(std::streamoff _i, double _v)
{
  if (_i < m_Quantity)
  {
    MET_DoubleToValue(_v, m_ElementType, m_ElementData, _i);
    return true;
  }
  return false;
}

void
MetaImage::ElementData(void * _elementData, bool _autoFreeElementData)
{
  if (m_AutoFreeElementData)
  {
    delete[]static_cast<char *>(m_ElementData);
  }
  m_ElementData = _elementData;
  m_AutoFreeElementData = _autoFreeElementData;
}

bool
MetaImage::ConvertElementDataTo(MET_ValueEnumType _elementType, double _toMin, double _toMax)
{
  int eSize;
  MET_SizeOfType(_elementType, &eSize);
  const size_t newElementDataSize = static_cast<size_t>(m_Quantity * m_ElementNumberOfChannels * eSize);
  void * newElementData = new char[newElementDataSize];

  ElementByteOrderFix();
  if (!ElementMinMaxValid())
  {
    ElementMinMaxRecalc();
  }

  for (size_t i = 0; i < static_cast<size_t>(m_Quantity * m_ElementNumberOfChannels); i++)
  {
    MET_ValueToValueN(
      m_ElementType, m_ElementData, i, _elementType, newElementData, newElementDataSize, m_ElementMin, m_ElementMax, _toMin, _toMax);
  }

  if (m_AutoFreeElementData)
  {
    delete[]static_cast<char *>(m_ElementData);
  }
  m_ElementData = newElementData;
  m_ElementType = _elementType;
  m_ElementMinMaxValid = true;
  m_ElementMin = _toMin;
  m_ElementMax = _toMax;
  m_AutoFreeElementData = true;

  return true;
}

bool
MetaImage::ConvertElementDataToIntensityData(MET_ValueEnumType _elementType)
{
  ElementByteOrderFix();
  if (!ElementMinMaxValid())
  {
    ElementMinMaxRecalc();
  }

  double toMin = m_ElementMin + m_ElementToIntensityFunctionOffset;
  double toMax = (m_ElementMax - m_ElementMin) * m_ElementToIntensityFunctionSlope + m_ElementMin;

  return ConvertElementDataTo(_elementType, toMin, toMax);
}

bool
MetaImage::ConvertIntensityDataToElementData(MET_ValueEnumType _elementType)
{
  ElementByteOrderFix();
  if (!ElementMinMaxValid())
  {
    ElementMinMaxRecalc();
  }

  double toMin = m_ElementMin - m_ElementToIntensityFunctionOffset;
  double toMax = (m_ElementMax - m_ElementMin) / m_ElementToIntensityFunctionSlope + toMin;

  return ConvertElementDataTo(_elementType, toMin, toMax);
}

// return true if the file exists
bool
MetaImage::M_FileExists(const char * filename)
{
#ifdef _MSC_VER
#  define access _access
#endif
#ifndef R_OK
#  define R_OK 04
#endif
  if (access(filename, R_OK) != 0)
  {
    return false;
  }
  else
  {
    return true;
  }
}

bool
MetaImage::FileIsFullPath(const char * in_name)
{
#if defined(_WIN32) || defined(__CYGWIN__)
  // On Windows, the name must be at least two characters long.
  if (strlen(in_name) < 2)
  {
    return false;
  }
  if (in_name[1] == ':')
  {
    return true;
  }
  if (in_name[0] == '\\')
  {
    return true;
  }
#else
  // On UNIX, the name must be at least one character long.
  if (strlen(in_name) < 1)
  {
    return false;
  }
#endif
#if !defined(_WIN32)
  if (in_name[0] == '~')
  {
    return true;
  }
#endif
  // On UNIX, the name must begin in a '/'.
  // On Windows, if the name begins in a '/', then it is a full
  // network path.
  if (in_name[0] == '/')
  {
    return true;
  }
  return false;
}

// Return the value of a tag
std::string
MetaImage::M_GetTagValue(const std::string & buffer, const char * tag)
{
  size_t stringPos = buffer.find(tag);
  if (stringPos == std::string::npos)
  {
    return "";
  }

  size_t pos2 = buffer.find('=', stringPos);
  if (pos2 == std::string::npos)
  {
    pos2 = buffer.find(':', stringPos);
  }

  if (pos2 == std::string::npos)
  {
    return "";
  }

  // Get the element data filename
  std::string value;
  bool        firstspace = true;
  size_t      index = pos2 + 1;
  while (index < buffer.size() && buffer[index] != '\r' && buffer[index] != '\n')
  {
    if (buffer[index] != ' ')
    {
      firstspace = false;
    }
    if (!firstspace)
    {
      value += buffer[index];
    }
    index++;
  }

  return value;
}

bool
MetaImage::CanRead(const char * _headerName)
{
  // First check the extension
  std::string fname = _headerName;
  if (fname.empty())
  {
    return false;
  }

  bool extensionFound = false;

  std::string::size_type stringPos = fname.rfind(".mhd");
  if ((stringPos != std::string::npos) && (stringPos == fname.length() - 4))
  {
    extensionFound = true;
  }

  stringPos = fname.rfind(".mha");
  if ((stringPos != std::string::npos) && (stringPos == fname.length() - 4))
  {
    extensionFound = true;
  }

  if (!extensionFound)
  {
    return false;
  }

  // Now check the file content
  METAIO_STREAM::ifstream inputStream;

  openReadStream(inputStream, fname);

  if (inputStream.fail())
  {
    return false;
  }

  char * buf = new char[8001];
  inputStream.read(buf, 8000);
  auto fileSize = static_cast<unsigned long>(inputStream.gcount());
  buf[fileSize] = 0;
  std::string header(buf);
  header.resize(fileSize);
  delete[] buf;
  inputStream.close();

  stringPos = header.find("NDims");
  if (stringPos == std::string::npos)
  {
    return false;
  }

  std::string elementDataFileName = M_GetTagValue(header, "ElementDataFile");

  return true;
}

bool
MetaImage::Read(const char * _headerName, bool _readElements, void * _buffer)
{
  M_Destroy();

  Clear();

  M_SetupReadFields();

  if (_headerName != nullptr)
  {
    m_FileName = _headerName;
  }

  M_PrepareNewReadStream();

  auto * tmpReadStream = new METAIO_STREAM::ifstream;

  openReadStream(*tmpReadStream, m_FileName);

  if (!tmpReadStream->is_open())
  {
    delete tmpReadStream;
    return false;
  }

  if (!this->ReadStream(0, tmpReadStream, _readElements, _buffer))
  {
    tmpReadStream->close();
    delete tmpReadStream;
    return false;
  }

  tmpReadStream->close();

  delete tmpReadStream;

  return true;
}

bool
MetaImage::CanReadStream(METAIO_STREAM::ifstream * _stream)
{
  if (!strncmp(MET_ReadType(*_stream).c_str(), "Image", 5))
  {
    return true;
  }
  return false;
}


bool
MetaImage::ReadStream(int _nDims, METAIO_STREAM::ifstream * _stream, bool _readElements, void * _buffer)
{
  if (!MetaObject::ReadStream(_nDims, _stream))
  {
    std::cerr << "MetaImage: Read: Cannot parse file" << '\n';
    return false;
  }

  if (_readElements)
  {
    if (_buffer == nullptr)
    {
      InitializeEssential(
        m_NDims, m_DimSize, m_ElementSpacing, m_ElementType, m_ElementNumberOfChannels, nullptr, true, false);
    }
    else
    {
      InitializeEssential(
        m_NDims, m_DimSize, m_ElementSpacing, m_ElementType, m_ElementNumberOfChannels, _buffer, false, false);
    }

    int         i;
    size_t      j;
    bool        usePath;
    std::string pathName;
    std::string fName;
    usePath = MET_GetFilePath(m_FileName, pathName);

    if ("Local" == m_ElementDataFileName || "LOCAL" == m_ElementDataFileName || "local" == m_ElementDataFileName)
    {
      if (!M_ReadElements(_stream, m_ElementData, m_Quantity))
      {
        return false;
      }
    }
    else if ("LIST" == m_ElementDataFileName.substr(0, 4))
    {
      int     fileImageDim = m_NDims - 1;
      int     nWrds;
      char ** wrds;
      MET_StringToWordArray(m_ElementDataFileName.c_str(), &nWrds, &wrds);
      if (nWrds > 1)
      {
        fileImageDim = static_cast<int>(atof(wrds[1]));
      }
      for (i = 0; i < nWrds; i++)
      {
        delete[] wrds[i];
      }
      delete[] wrds;
      if ((fileImageDim == 0) || (fileImageDim > m_NDims))
      {
        // if optional file dimension size is not given or is larger than
        // overall dimension then default to a size of m_NDims - 1.
        fileImageDim = m_NDims - 1;
      }
      std::string s;
      auto *      readStreamTemp = new METAIO_STREAM::ifstream;
      int         elementSize;
      MET_SizeOfType(m_ElementType, &elementSize);
      elementSize *= m_ElementNumberOfChannels;
      int totalFiles = 1;
      for (i = m_NDims; i > fileImageDim; i--)
      {
        totalFiles *= m_DimSize[i - 1];
      }
      for (i = 0; i < totalFiles && !_stream->eof(); i++)
      {
        std::getline(*_stream, s);
        if (!_stream->eof())
        {
          j = s.length() - 1;
          while (j > 0 && (isspace(s[j]) || !isprint(s[j])))
          {
            s[j--] = '\0';
          }
          if (usePath && !FileIsFullPath(s.c_str()))
          {
            fName = pathName + s;
          }
          else
          {
            fName = s;
          }

          openReadStream(*readStreamTemp, fName);
          if (!readStreamTemp->is_open())
          {
            std::cerr << "MetaImage: Read: cannot open slice" << '\n';
            delete readStreamTemp;
            return false;
          }
          if (!M_ReadElements(readStreamTemp,
                              &((static_cast<char *>(m_ElementData))[i * m_SubQuantity[fileImageDim] * elementSize]),
                              m_SubQuantity[fileImageDim]))
          {
            readStreamTemp->close();
            delete readStreamTemp;
            return false;
          }

          readStreamTemp->close();
        }
      }
      delete readStreamTemp;
    }
    else if (m_ElementDataFileName.find('%') != std::string::npos)
    {
      int elementSize;
      MET_SizeOfType(m_ElementType, &elementSize);
      elementSize *= m_ElementNumberOfChannels;

      int         nWrds;
      char **     wrds;
      int         minV = 1;
      int         maxV = m_DimSize[m_NDims - 1];
      int         stepV = 1;
      std::string s;
      auto *      readStreamTemp = new METAIO_STREAM::ifstream;
      MET_StringToWordArray(m_ElementDataFileName.c_str(), &nWrds, &wrds);
      if (nWrds >= 2)
      {
        minV = static_cast<int>(atof(wrds[1]));
        maxV = minV + m_DimSize[m_NDims - 1] - 1;
      }
      if (nWrds >= 3)
      {
        maxV = static_cast<int>(atof(wrds[2]));
        stepV = (maxV - minV) / (m_DimSize[m_NDims - 1]);
      }
      if (nWrds >= 4)
      {
        stepV = static_cast<int>(atof(wrds[3]));
      }
      if (nWrds >= 5)
      {
        // In this case, the filename must have had spaces in the
        // name.  The filename was parsed into multiple pieces by the
        // MET_StringToWordArray, which parses based on spaces.
        // Thus, we need to reconstruct the filename in this case.
        // The last three wrds must be numbers.  If they are not, we give an error.
        for (i = nWrds - 3; i < nWrds; i++)
        {
          for (j = 0; j < strlen(wrds[i]); j++)
          {
            if (!isdigit(wrds[i][j]))
            {
              std::cerr << "MetaImage: Read: Last three arguments in element data filename must be numbers!" << '\n';
              return false;
            }
          }
        }
        stepV = static_cast<int>(atof(wrds[nWrds - 1]));
        maxV = static_cast<int>(atof(wrds[nWrds - 2]));
        minV = static_cast<int>(atof(wrds[nWrds - 3]));
        for (i = 1; i < nWrds - 3; i++)
        {
          std::strncat(wrds[0], " ", METAIO_MAX_WORD_SIZE);
          std::strncat(wrds[0], wrds[i], METAIO_MAX_WORD_SIZE);
        }
      }
      // If the specified size of the third dimension is less than the size
      // specified by the regular expression, we should only read a volume with the specified
      // size.  Otherwise, the code will crash when trying to fill m_ElementData more than it can hold.
      // Therefore, we modify maxV to ensure that the images spanned by minV:stepV:maxV are less than or equal
      // to the size in the last dimension.
      int numberOfImages = 1 + (maxV - minV) / stepV;
      if (numberOfImages > m_DimSize[m_NDims - 1])
      {
        maxV = (m_DimSize[m_NDims - 1] - 1) * stepV + minV;
      }
      int cnt = 0;
      for (i = minV; i <= maxV; i += stepV)
      {
        s = string_format(wrds[0], i);
        if (usePath && !FileIsFullPath(s.c_str()))
        {
          fName = pathName + s;
        }
        else
        {
          fName = s;
        }
        openReadStream(*readStreamTemp, fName);
        if (!readStreamTemp->is_open())
        {
          std::cerr << "MetaImage: Read: cannot construct file" << '\n';
          delete readStreamTemp;
          for (i = 0; i < nWrds; i++)
          {
            delete[] wrds[i];
          }
          return false;
        }

        if (!M_ReadElements(readStreamTemp,
                            &((static_cast<char *>(m_ElementData))[cnt * m_SubQuantity[m_NDims - 1] * elementSize]),
                            m_SubQuantity[m_NDims - 1]))
        {
          readStreamTemp->close();
          delete readStreamTemp;
          for (i = 0; i < nWrds; i++)
          {
            delete[] wrds[i];
          }
          return false;
        }

        cnt++;

        readStreamTemp->close();
      }
      delete readStreamTemp;
      for (i = 0; i < nWrds; i++)
      {
        delete[] wrds[i];
      }
      delete[] wrds;
    }
    else
    {
      if (usePath && !FileIsFullPath(m_ElementDataFileName.c_str()))
      {
        fName = pathName + m_ElementDataFileName;
      }
      else
      {
        fName = m_ElementDataFileName;
      }

      auto * readStreamTemp = new METAIO_STREAM::ifstream;

      const char * extensions[] = { "", ".gz", ".Z", nullptr };
      for (unsigned ii = 0; extensions[ii] != nullptr; ii++)
      {
        std::string tempFName(fName);
        tempFName += extensions[ii];
        openReadStream(*readStreamTemp, tempFName);
        if (readStreamTemp->is_open())
        {
          if (ii > 0)
          {
            this->CompressedData(true);
            this->BinaryData(true);
          }
          break;
        }
      }

      if (!readStreamTemp->is_open())
      {
        std::cerr << "MetaImage: Read: Cannot open data file" << '\n';
        if (m_ReadStream)
        {
          m_ReadStream->close();
        }
        return false;
      }
      if (!M_ReadElements(readStreamTemp, m_ElementData, m_Quantity))
      {
        readStreamTemp->close();
        delete readStreamTemp;
        return false;
      }

      readStreamTemp->close();
      delete readStreamTemp;
    }
  }

  return true;
}


bool
MetaImage::Write(const char * _headName,
                 const char * _dataName,
                 bool         _writeElements,
                 const void * _constElementData,
                 bool         _append)
{
  if (_headName != nullptr)
  {
    FileName(_headName);
  }

  bool userDataFileName = true;
  if (_dataName == nullptr && m_ElementDataFileName.empty())
  {
    userDataFileName = false;
    int sPtr = 0;
    MET_GetFileSuffixPtr(m_FileName, &sPtr);
    if (!strcmp(&m_FileName[sPtr], "mha"))
    {
      ElementDataFileName("LOCAL");
    }
    else
    {
      if (!_append)
      {
        MET_SetFileSuffix(m_FileName, "mhd");
      }
      m_ElementDataFileName = m_FileName;
      if (m_CompressedData)
      {
        MET_SetFileSuffix(m_ElementDataFileName, "zraw");
      }
      else
      {
        MET_SetFileSuffix(m_ElementDataFileName, "raw");
      }
    }
  }
  else if (_dataName != nullptr)
  {
    userDataFileName = false;
    ElementDataFileName(_dataName);
  }

  // make sure suffix is valid
  if (!_append)
  {
    if (m_ElementDataFileName == "LOCAL")
    {
      MET_SetFileSuffix(m_FileName, "mha");
    }
    else
    {
      MET_SetFileSuffix(m_FileName, "mhd");
    }
  }

  std::string pathName;
  bool        usePath = MET_GetFilePath(m_FileName, pathName);
  if (usePath)
  {
    std::string elementPathName;
    MET_GetFilePath(m_ElementDataFileName, elementPathName);
    if (pathName == elementPathName)
    {
      elementPathName = m_ElementDataFileName.substr(pathName.length());
      m_ElementDataFileName = elementPathName;
    }
  }

  auto * tmpWriteStream = new METAIO_STREAM::ofstream;

  openWriteStream(*tmpWriteStream, m_FileName, _append);

  if (!tmpWriteStream->is_open())
  {
    if (!userDataFileName)
    {
      ElementDataFileName("");
    }

    delete tmpWriteStream;

    return false;
  }

  bool result = MetaImage::WriteStream(tmpWriteStream, _writeElements, _constElementData);

  if (!userDataFileName)
  {
    ElementDataFileName("");
  }

  tmpWriteStream->close();
  delete tmpWriteStream;

  return result;
}

bool
MetaImage::WriteStream(METAIO_STREAM::ofstream * _stream, bool _writeElements, const void * _constElementData)
{
  if (m_WriteStream != nullptr)
  {
    std::cerr << "MetaArray: WriteStream: two files open?" << '\n';
    delete m_WriteStream;
  }

  m_WriteStream = _stream;

  unsigned char * compressedElementData = nullptr;
  if (m_BinaryData && m_CompressedData && m_ElementDataFileName.find('%') == std::string::npos)
  // compressed & !slice/file
  {
    int elementSize;
    MET_SizeOfType(m_ElementType, &elementSize);
    int elementNumberOfBytes = elementSize * m_ElementNumberOfChannels;

    if (_constElementData == nullptr)
    {
      compressedElementData = MET_PerformCompression(static_cast<const unsigned char *>(m_ElementData),
                                                     m_Quantity * elementNumberOfBytes,
                                                     &m_CompressedDataSize,
                                                     m_CompressionLevel);
    }
    else
    {
      compressedElementData = MET_PerformCompression(static_cast<const unsigned char *>(_constElementData),
                                                     m_Quantity * elementNumberOfBytes,
                                                     &m_CompressedDataSize,
                                                     m_CompressionLevel);
    }
  }

  M_SetupWriteFields();

  if (!M_Write())
  {
    return false;
  }

  bool writeResult = true;
  if (_writeElements)
  {
    if (m_BinaryData && m_CompressedData && m_ElementDataFileName.find('%') == std::string::npos)
    // compressed & !slice/file
    {
      writeResult = M_WriteElements(m_WriteStream, compressedElementData, m_CompressedDataSize);

      delete[] compressedElementData;
      m_CompressedDataSize = 0;
    }
    else
    {
      if (_constElementData == nullptr)
      {
        writeResult = M_WriteElements(m_WriteStream, m_ElementData, m_Quantity);
      }
      else
      {
        writeResult = M_WriteElements(m_WriteStream, _constElementData, m_Quantity);
      }
    }
  }

  m_WriteStream = nullptr;

  return writeResult;
}


/** Write a portion of an image */
bool
MetaImage::WriteROI(int *        _indexMin,
                    int *        _indexMax,
                    const char * _headName,
                    const char * _dataName,
                    bool         _writeElements,
                    const void * _constElementData,
                    bool         _append)
{
  if (_headName != nullptr)
  {
    FileName(_headName);
  }

  if (!_writeElements)
  {
    return false;
  }

  bool writeResult = true;

  // Check if the file exists
  if (M_FileExists(_headName))
  {
    char * elementData = const_cast<char *>(static_cast<const char *>(_constElementData));
    if (elementData == nullptr)
    {
      elementData = static_cast<char *>(m_ElementData);
    }
    if (elementData == nullptr)
    {
      std::cerr << "Element data is NULL" << '\n';
      return false;
    }

    // Find the start of the data
    auto * readStream = new METAIO_STREAM::ifstream;
    readStream->open(m_FileName.c_str(), std::ios::binary | std::ios::in);

    // File must be readable
    if (!MetaObject::ReadStream(m_NDims, readStream))
    {
      std::cerr << "MetaImage: Read: Cannot parse file" << '\n';
      delete readStream;
      return false;
    }

    // File must not be compressed
    if (m_CompressedData)
    {
      std::cerr << "MetaImage cannot insert ROI into a compressed file." << '\n';
      readStream->close();
      delete readStream;
      return false;
    }

    InitializeEssential(m_NDims,
                        m_DimSize,
                        m_ElementSpacing,
                        m_ElementType,
                        m_ElementNumberOfChannels,
                        nullptr,
                        false); // no memory allocation

    std::string    filename = ElementDataFileName();
    std::streampos dataPos = 0;

    // local file
    if (filename == "LOCAL")
    {
      filename = m_FileName;
      dataPos = readStream->tellg();
    }
    else if (filename == "LIST" || strstr(filename.c_str(), "%"))
    {
      std::cerr << "MetaImage cannot insert ROI into a list of files." << '\n';
      readStream->close();
      delete readStream;
      return false;
    }

    readStream->close();
    delete readStream;

    // Write the region
    if (!M_FileExists(filename.c_str()))
    {
      std::string pathName;
      MET_GetFilePath(_headName, pathName);
      filename = pathName + filename;
    }

    auto * tmpWriteStream = new METAIO_STREAM::ofstream;
    tmpWriteStream->open(filename.c_str(), std::ios::binary | std::ios::in | std::ios::out);

    if (!tmpWriteStream->is_open())
    {
      std::cerr << "Cannot open ROI file: " << filename.c_str() << '\n';
      delete tmpWriteStream;
      return false;
    }

    int elementSize;
    MET_SizeOfType(m_ElementType, &elementSize);
    int elementNumberOfBytes = elementSize * m_ElementNumberOfChannels;

    // seek to the end and write one byte to allocate the entire file size
    std::streamoff seekoff = m_Quantity * elementNumberOfBytes;
    tmpWriteStream->seekp(0, std::ios::end);
    if (tmpWriteStream->tellp() != (dataPos + seekoff))
    {
      seekoff = seekoff - 1;
      tmpWriteStream->seekp(dataPos + seekoff, std::ios::beg);
      const char zerobyte = 0;
      tmpWriteStream->write(&zerobyte, 1);
    }

    if (!elementData)
    {
      std::cerr << "Element data is NULL" << '\n';
      delete tmpWriteStream;
      return false;
    }

    writeResult = M_WriteElementsROI(tmpWriteStream, elementData, dataPos, _indexMin, _indexMax);

    tmpWriteStream->close();
    delete tmpWriteStream;
  }
  else // the file doesn't exist
  {
    if (m_CompressedData)
    {
      std::cerr << "MetaImage cannot write an ROI using compression." << '\n';
      return false;
    }

    // Get the data filename right...
    bool userDataFileName = true;
    if (_dataName == nullptr && m_ElementDataFileName.empty())
    {
      userDataFileName = false;
      int sPtr = 0;
      MET_GetFileSuffixPtr(m_FileName, &sPtr);
      if (!strcmp(&m_FileName[sPtr], "mha"))
      {
        ElementDataFileName("LOCAL");
      }
      else
      {
        if (!_append)
        {
          MET_SetFileSuffix(m_FileName, "mhd");
        }
        m_ElementDataFileName = m_FileName;
        if (m_CompressedData)
        {
          MET_SetFileSuffix(m_ElementDataFileName, "zraw");
        }
        else
        {
          MET_SetFileSuffix(m_ElementDataFileName, "raw");
        }
      }
    }
    else if (_dataName != nullptr)
    {
      userDataFileName = false;
      ElementDataFileName(_dataName);
    }

    if (m_ElementDataFileName == "LIST" || m_ElementDataFileName.find('%') != std::string::npos)
    {
      std::cerr << "MetaImage cannot insert ROI into a list of files." << '\n';
      return false;
    }

    // make sure the header suffix is valid, unless forcing to match an
    // existing file via the append bool argument.
    if (!_append)
    {
      if (m_ElementDataFileName == "LOCAL")
      {
        MET_SetFileSuffix(m_FileName, "mha");
      }
      else
      {
        MET_SetFileSuffix(m_FileName, "mhd");
      }
    }

    std::string pathName;
    bool        usePath = MET_GetFilePath(m_FileName, pathName);
    if (usePath)
    {
      std::string elementPathName;
      MET_GetFilePath(m_ElementDataFileName, elementPathName);
      if (pathName == elementPathName)
      {
        elementPathName = m_ElementDataFileName.substr(pathName.length());
        m_ElementDataFileName = elementPathName;
      }
    }

    auto * tmpWriteStream = new METAIO_STREAM::ofstream;

    openWriteStream(*tmpWriteStream, m_FileName, _append);

    if (!tmpWriteStream->is_open())
    {
      if (!userDataFileName)
      {
        ElementDataFileName("");
      }
      delete tmpWriteStream;
      return false;
    }

    // Write the ROI header file
    char * elementData = const_cast<char *>(static_cast<const char *>(_constElementData));
    if (elementData == nullptr)
    {
      elementData = static_cast<char *>(m_ElementData);
    }

    m_WriteStream = tmpWriteStream;
    M_SetupWriteFields();
    if (!M_Write())
    {
      tmpWriteStream->close();
      delete tmpWriteStream;
      return false;
    }

    std::streampos dataPos = m_WriteStream->tellp();

    // If data is in a separate file, set dataPos and point to that file.
    //   ( we've already verified the name isn't LIST and doesn't
    //     contain % )
    if (m_ElementDataFileName != "LOCAL")
    {
      m_WriteStream = nullptr;
      tmpWriteStream->close();

      dataPos = 0;

      std::string dataFileName;
      if (usePath && !FileIsFullPath(m_ElementDataFileName.c_str()))
      {
        dataFileName = pathName + m_ElementDataFileName;
      }
      else
      {
        dataFileName = m_ElementDataFileName;
      }

      openWriteStream(*tmpWriteStream, dataFileName, _append);
      m_WriteStream = tmpWriteStream;
    }

    int elementSize;
    MET_SizeOfType(m_ElementType, &elementSize);
    int elementNumberOfBytes = elementSize * m_ElementNumberOfChannels;

    // write the last byte in the file to allocate it
    std::streamoff seekoff = m_Quantity * elementNumberOfBytes;
    seekoff -= 1;
    m_WriteStream->seekp(seekoff, std::ios::cur);
    const char zerobyte = 0;
    m_WriteStream->write(&zerobyte, 1);

    writeResult = M_WriteElementsROI(m_WriteStream, elementData, dataPos, _indexMin, _indexMax);

    m_WriteStream = nullptr;

    if (!userDataFileName)
    {
      ElementDataFileName("");
    }

    tmpWriteStream->close();
    delete tmpWriteStream;
  }

  return writeResult;
}

bool
MetaImage::M_WriteElementsROI(METAIO_STREAM::ofstream * _fstream,
                              const void *    _data,
                              std::streampos  _dataPos,
                              const int *     _indexMin,
                              const int *     _indexMax)
{
  const char * data = static_cast<const char *>(_data);

  int elementSize;
  MET_SizeOfType(m_ElementType, &elementSize);
  const int elementNumberOfBytes = elementSize * m_ElementNumberOfChannels;

  // Write the IO region line by line
  int * currentIndex = new int[m_NDims];
  for (int i = 0; i < m_NDims; i++)
  {
    currentIndex[i] = _indexMin[i];
  }

  // Optimize the size of the buffer to written depending on the
  // region shape
  // This calculate the number of continuous bytes in the file
  // which can be written
  std::streamoff elementsToWrite = 1;
  int            movingDirection = 0;
  do
  {
    elementsToWrite *= _indexMax[movingDirection] - _indexMin[movingDirection] + 1;
    ++movingDirection;
  } while (movingDirection < m_NDims && _indexMin[movingDirection - 1] == 0 &&
           _indexMax[movingDirection - 1] == m_DimSize[movingDirection - 1] - 1);

  // write line by line
  bool done = false;
  while (!done)
  {
    // Seek to the right position
    std::streamoff seekoff = _dataPos;
    for (int i = 0; i < m_NDims; i++)
    {
      seekoff += m_SubQuantity[i] * currentIndex[i] * elementNumberOfBytes;
    }
    _fstream->seekp(seekoff, std::ios::beg);

    if (!M_WriteElementData(_fstream, data, elementsToWrite))
    {
      delete[] currentIndex;
      return false;
    }
    data += elementsToWrite * elementNumberOfBytes;

    // check if there is only one write needed
    if (movingDirection >= m_NDims)
    {
      break;
    }

    ++currentIndex[movingDirection];

    // Check if we are still in the region
    for (int j = movingDirection; j < m_NDims; j++)
    {
      if (currentIndex[j] > _indexMax[j])
      {
        if (j == m_NDims - 1)
        {
          done = true;
          break;
        }
        else
        {
          currentIndex[j] = _indexMin[j];
          currentIndex[j + 1]++;
        }
      }
    }
  } // end writing  loop

  delete[] currentIndex;

  return true;
}

bool
MetaImage::Append(const char * _headName)
{
  META_DEBUG_PRINT( "MetaImage: Append" );

  return this->Write(_headName, nullptr, true, nullptr, true);
}

void
MetaImage::M_ResetValues()
{
  if (m_AutoFreeElementData && m_ElementData != nullptr)
  {
    delete[]static_cast<char *>(m_ElementData);
  }

  m_ElementData = nullptr;

  if (m_CompressionTable && m_CompressionTable->compressedStream)
  {
    inflateEnd(m_CompressionTable->compressedStream);
    delete m_CompressionTable->compressedStream;
    delete[] m_CompressionTable->buffer;
    m_CompressionTable->buffer = nullptr;
  }
  delete m_CompressionTable;
  m_CompressionTable = nullptr;

  M_Destroy();
}

void
MetaImage::M_SetupReadFields()
{
  META_DEBUG_PRINT( "MetaImage: M_SetupReadFields" );

  MetaObject::M_SetupReadFields();

  MET_FieldRecordType * mF;

  int nDimsRecNum = MET_GetFieldRecordNumber("NDims", &m_Fields);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "DimSize", MET_INT_ARRAY, true, nDimsRecNum);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "HeaderSize", MET_INT, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "Modality", MET_STRING, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ImagePosition", MET_FLOAT_ARRAY, false, nDimsRecNum);
  m_Fields.push_back(mF);

  bool fieldRequired = false;
  if (m_FileFormatVersion == 1)
  {
    fieldRequired = true;
  }
  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ElementOrigin", MET_FLOAT_ARRAY, fieldRequired, nDimsRecNum);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ElementDirection", MET_FLOAT_MATRIX, fieldRequired, nDimsRecNum);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "SequenceID", MET_INT_ARRAY, false, nDimsRecNum);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ElementMin", MET_FLOAT, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ElementMax", MET_FLOAT, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ElementNumberOfChannels", MET_INT, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ElementSize", MET_FLOAT_ARRAY, false, nDimsRecNum);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType; // Handled as with DICOM: set, but not used...
  MET_InitReadField(mF, "ElementNBits", MET_INT, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType; // Used by ConvertElementToIntensity funcs
  MET_InitReadField(mF, "ElementToIntensityFunctionSlope", MET_FLOAT, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType; // Used by ConvertElementToIntensity funcs
  MET_InitReadField(mF, "ElementToIntensityFunctionOffset", MET_FLOAT, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ElementType", MET_STRING, true);
  mF->required = true;
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ElementDataFile", MET_STRING, true);
  mF->required = true;
  mF->terminateRead = true;
  m_Fields.push_back(mF);
}

void
MetaImage::M_SetupWriteFields()
{
  MetaObject::M_SetupWriteFields();

  MET_FieldRecordType * mF;

  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "DimSize", MET_INT_ARRAY, static_cast<size_t>(m_NDims), m_DimSize);
  m_Fields.push_back(mF);

  char s[22];
  if (m_HeaderSize > 0 || m_HeaderSize == -1)
  {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "HeaderSize", MET_INT);
    m_Fields.push_back(mF);
  }

  int i;
  if (m_Modality != MET_MOD_UNKNOWN)
  {
    mF = new MET_FieldRecordType;
    strcpy(s, MET_ValueTypeName[m_Modality]);
    MET_InitWriteField(mF, "Modality", MET_STRING, strlen(s), s);
    m_Fields.push_back(mF);
  }

  i = MET_GetFieldRecordNumber("AnatomicalOrientation", &m_Fields);
  if (i < 0)
  {
    const char * str = AnatomicalOrientationAcronym();
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "AnatomicalOrientation", MET_STRING, strlen(str), str);
    m_Fields.push_back(mF);
  }

  bool valid = false;
  for (i = 0; i < 4; i++)
  {
    if (m_SequenceID[i] != 0)
    {
      valid = true;
      break;
    }
  }
  if (valid)
  {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "SequenceID", MET_FLOAT_ARRAY, static_cast<size_t>(m_NDims), m_SequenceID);
    m_Fields.push_back(mF);
  }

  // Determine if new API was used...
  if (m_APIVersion == 0)
  {
    // Using old API, and writing old format
    for (i = 0; i < m_NDims; i++)
    {
      if (m_ElementOrigin[i] != 0)
      {
        std::cout << "Warning: ElementOrigin is not supported in MetaIO API version 0" << '\n';
        std::cout << "      ElementOrigin will not be written to file." << '\n';
        std::cout << "      Use MetaImage::APIVersion to select API version 1 otherwise" << '\n';
        std::cout << "      information will be lost." << '\n';
        break;
      }
      for (int j = 0; j < m_NDims; j++)
      {
        if ((i != j && m_ElementDirection[i*m_NDims+j] != 0) ||
            (i == j && m_ElementDirection[i*m_NDims+j] != 1 && m_ElementDirection[i*m_NDims+j] != 0))
        {
          std::cout << "Warning: ElementDirection is not supported in MetaIO API version 0" << '\n';
          std::cout << "      ElementDirection will not be written to file." << '\n';
          std::cout << "      Use MetaImage::APIVersion to select API version 1 otherwise" << '\n';
          std::cout << "      information will be lost." << '\n';
          i = m_NDims;
          break;
        }
      }
    }
  }

  // Dev wants to write old file format
  if (m_FileFormatVersion == 0)
  {
    // Developer used old API
    if (m_APIVersion == 0)
    {
      // Old behavior
      // Do not write ElementOrigin or ElementDirection
    }
    else // Developer used new API, so convert to old file format
    {
      // Convert to old format
      // Will result in loss of ObjectToParent transform, if used in a scene
      mF = MET_GetFieldRecord("Offset", &m_Fields);
      MET_InitWriteField(mF, "Offset", MET_FLOAT_ARRAY, static_cast<size_t>(m_NDims), m_ElementOrigin);

      mF = MET_GetFieldRecord("TransformMatrix", &m_Fields);
      MET_InitWriteField(mF, "TransformMatrix", MET_FLOAT_MATRIX, static_cast<size_t>(m_NDims), m_ElementDirection);
    }
  }
  else // Dev wants to Write new file format, so don't change anything
  {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "ElementOrigin", MET_FLOAT_ARRAY, static_cast<size_t>(m_NDims), m_ElementOrigin);
    m_Fields.push_back(mF);

    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "ElementDirection", MET_FLOAT_MATRIX, static_cast<size_t>(m_NDims), m_ElementDirection);
    m_Fields.push_back(mF);
  }

  if (m_ElementMinMaxValid)
  {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "ElementMin", MET_FLOAT, m_ElementMin);
    m_Fields.push_back(mF);

    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "ElementMax", MET_FLOAT, m_ElementMax);
    m_Fields.push_back(mF);
  }

  if (m_ElementNumberOfChannels > 1)
  {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "ElementNumberOfChannels", MET_INT, m_ElementNumberOfChannels);
    m_Fields.push_back(mF);
  }

  if (m_ElementSizeValid)
  {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "ElementSize", MET_FLOAT_ARRAY, static_cast<size_t>(m_NDims), m_ElementSize);
    m_Fields.push_back(mF);
  }

  if (m_ElementToIntensityFunctionSlope != 1 || m_ElementToIntensityFunctionOffset != 0)
  {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "ElementToIntensityFunctionSlope", MET_FLOAT, m_ElementToIntensityFunctionSlope);
    m_Fields.push_back(mF);
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "ElementToIntensityFunctionOffset", MET_FLOAT, m_ElementToIntensityFunctionOffset);
    m_Fields.push_back(mF);
  }

  mF = new MET_FieldRecordType;
  MET_TypeToString(m_ElementType, s);
  MET_InitWriteField(mF, "ElementType", MET_STRING, strlen(s), s);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "ElementDataFile", MET_STRING, m_ElementDataFileName.length(), m_ElementDataFileName.c_str());
  mF->terminateRead = true;
  m_Fields.push_back(mF);
}

bool
MetaImage::M_Read()
{
  META_DEBUG_PRINT( "MetaImage: M_Read: Loading Header" );
  if (!MetaObject::M_Read())
  {
    std::cerr << "MetaImage: M_Read: Error parsing file" << '\n';
    return false;
  }

  META_DEBUG_PRINT( "MetaImage: M_Read: Parsing Header" );
  MET_FieldRecordType * mF;

  int i;
  mF = MET_GetFieldRecord("DimSize", &m_Fields);
  if (mF && mF->defined)
  {
    for (i = 0; i < m_NDims; i++)
    {
      m_DimSize[i] = static_cast<int>(mF->value[i]);
    }
  }

  mF = MET_GetFieldRecord("HeaderSize", &m_Fields);
  if (mF && mF->defined)
  {
    m_HeaderSize = static_cast<int>(mF->value[0]);
  }

  mF = MET_GetFieldRecord("Modality", &m_Fields);
  if (mF && mF->defined)
  {
    std::string temp(reinterpret_cast<char * >(mF->value));
    MET_StringToImageModality(temp, &m_Modality);
  }

  mF = MET_GetFieldRecord("SequenceID", &m_Fields);
  if (mF && mF->defined)
  {
    for (i = 0; i < m_NDims; i++)
    {
      m_SequenceID[i] = static_cast<float>(mF->value[i]);
    }
  }

  // ImagePosition is not written by MetaIO library, so it is probably
  // unused unless someone wrote a custom MetaIO writer.  Will maintain this
  // usage/definition to maintain backward compatibility.
  mF = MET_GetFieldRecord("ImagePosition", &m_Fields);
  if (mF && mF->defined)
  {
    for (i = 0; i < m_NDims; i++)
    {
      m_Offset[i] = mF->value[i];
    }
  }

  if (m_FileFormatVersion == 0)
  {
    // Old file was read, but developer wants to use the new API,
    //   so convert old fields to new variables...
    if (m_APIVersion == 1)
    {
      for (i = 0; i < m_NDims; i++)
      {
        m_ElementOrigin[i] = m_Offset[i];
        m_Offset[i] = 0;
      }
      for (i = 0; i < m_NDims; i++)
      {
        for (int j = 0; j < m_NDims; j++)
        {
          m_ElementDirection[i*m_NDims+j] = m_TransformMatrix[i*m_NDims+j];
          if (i != j)
          {
            m_TransformMatrix[i*m_NDims+j] = 0;
          }
          else
          {
            m_TransformMatrix[i*m_NDims+j] = 1;
          }
        }
      }
    }
    // otherwise do nothing
  }
  else // New file was read
  {
    // New file was read, but developer wants to use the old API,
    //   so convert new fields to old variables...
    // May result in loss of ObjectToParentTransform for images that
    //   were stored as part of a scene.
    if (m_APIVersion == 0)
    {
      mF = MET_GetFieldRecord("ElementOrigin", &m_Fields);
      if (mF && mF->defined)
      {
        for (i = 0; i < m_NDims; i++)
        {
          m_Offset[i] = mF->value[i];
        }
      }
    
      mF = MET_GetFieldRecord("ElementDirection", &m_Fields);
      if (mF && mF->defined)
      {
        for (i = 0; i < m_NDims*m_NDims; i++)
        {
          m_TransformMatrix[i] = mF->value[i];
        }
      }
    }
    else
    // New file was read, and developer wants to use the new API,
    {
      mF = MET_GetFieldRecord("ElementOrigin", &m_Fields);
      if (mF && mF->defined)
      {
        for (i = 0; i < m_NDims; i++)
        {
          m_ElementOrigin[i] = mF->value[i];
        }
      }
    
      mF = MET_GetFieldRecord("ElementDirection", &m_Fields);
      if (mF && mF->defined)
      {
        for (i = 0; i < m_NDims*m_NDims; i++)
        {
          m_ElementDirection[i] = mF->value[i];
        }
      }
    }
  }

  mF = MET_GetFieldRecord("ElementMin", &m_Fields);
  if (mF && mF->defined)
  {
    m_ElementMin = mF->value[0];
  }

  mF = MET_GetFieldRecord("ElementMax", &m_Fields);
  if (mF && mF->defined)
  {
    m_ElementMax = mF->value[0];
  }

  mF = MET_GetFieldRecord("ElementNumberOfChannels", &m_Fields);
  if (mF && mF->defined)
  {
    m_ElementNumberOfChannels = static_cast<int>(mF->value[0]);
  }


  mF = MET_GetFieldRecord("ElementSize", &m_Fields);
  if (mF && mF->defined)
  {
    m_ElementSizeValid = true;
    for (i = 0; i < m_NDims; i++)
    {
      m_ElementSize[i] = mF->value[i];
    }
    mF = MET_GetFieldRecord("ElementSpacing", &m_Fields);
    if (!mF || !(mF->defined))
    {
      for (i = 0; i < m_NDims; i++)
      {
        m_ElementSpacing[i] = m_ElementSize[i];
      }
    }
  }
  else
  {
    m_ElementSizeValid = false;
    for (i = 0; i < m_NDims; i++)
    {
      m_ElementSize[i] = m_ElementSpacing[i];
    }
  }

  m_ElementToIntensityFunctionSlope = 1;
  m_ElementToIntensityFunctionOffset = 0;
  mF = MET_GetFieldRecord("ElementToIntensityFunctionSlope", &m_Fields);
  if (mF && mF->defined)
  {
    m_ElementToIntensityFunctionSlope = mF->value[0];
  }
  mF = MET_GetFieldRecord("ElementToIntensityFunctionOffset", &m_Fields);
  if (mF && mF->defined)
  {
    m_ElementToIntensityFunctionOffset = mF->value[0];
  }

  mF = MET_GetFieldRecord("ElementType", &m_Fields);
  if (mF && mF->defined)
  {
    MET_StringToType(reinterpret_cast<char *>(mF->value), &m_ElementType);
  }

  mF = MET_GetFieldRecord("ElementDataFile", &m_Fields);
  if (mF && mF->defined)
  {
    m_ElementDataFileName = reinterpret_cast<char *>(mF->value);
  }

  return true;
}

bool
MetaImage::M_ReadElements(METAIO_STREAM::ifstream * _fstream, void * _data, std::streamoff _dataQuantity)
{
  META_DEBUG_PRINT( "MetaImage: M_ReadElements" );

  if (m_HeaderSize > 0)
  {
    _fstream->seekg(m_HeaderSize, std::ios::beg);
    if (!_fstream->good())
    {
      std::cerr << "MetaImage: Read: header not read correctly" << '\n';
      return false;
    }
  }

  int elementSize;
  MET_SizeOfType(m_ElementType, &elementSize);
  std::streamoff readSize = _dataQuantity * m_ElementNumberOfChannels * elementSize;
  META_DEBUG_PRINT( "MetaImage: M_ReadElements: ReadSize = " << readSize );

  if (m_HeaderSize == -1)
  {
    META_DEBUG_PRINT( "MetaImage: M_ReadElements: Skipping header" );
    _fstream->seekg(-readSize, std::ios::end);
  }

  // If compressed we inflate
  if (m_BinaryData && m_CompressedData)
  {
    // if m_CompressedDataSize is not defined we assume the size of the
    // file is the size of the compressed data
    bool compressedDataDeterminedFromFile = false;
    if (m_CompressedDataSize == 0)
    {
      compressedDataDeterminedFromFile = true;
      _fstream->seekg(0, std::ios::end);
      m_CompressedDataSize = _fstream->tellg();
      _fstream->seekg(0, std::ios::beg);
    }

    auto * compr = new unsigned char[static_cast<size_t>(m_CompressedDataSize)];

    if (!M_ReadElementData(_fstream, compr, m_CompressedDataSize))
    {
      delete[] compr;
      return false;
    }

    MET_PerformUncompression(compr, m_CompressedDataSize, static_cast<unsigned char *>(_data), readSize);

    if (compressedDataDeterminedFromFile)
    {
      m_CompressedDataSize = 0;
    }

    delete[] compr;
  }
  else // if not compressed
  {
    if (!m_BinaryData)
    {
      if (!M_ReadElementData(_fstream, _data, _dataQuantity))
      {
        return false;
      }
    }
    else
    {

      if (!M_ReadElementData(_fstream, _data, _dataQuantity))
      {
        return false;
      }
    }
  }

  return true;
}

bool
MetaImage::M_WriteElements(METAIO_STREAM::ofstream * _fstream, const void * _data, std::streamoff _dataQuantity)
{

  if (m_ElementDataFileName == "LOCAL")
  {
    if (!MetaImage::M_WriteElementData(_fstream, _data, _dataQuantity))
    {
      return false;
    }
  }
  else // write the data in a separate file
  {
    std::string dataFileName;
    std::string pathName;
    bool        usePath = MET_GetFilePath(m_FileName, pathName);
    if (usePath && !FileIsFullPath(m_ElementDataFileName.c_str()))
    {
      dataFileName = pathName + m_ElementDataFileName;
    }
    else
    {
      dataFileName = m_ElementDataFileName;
    }

    if (dataFileName.find('%') != std::string::npos) // write slice by slice
    {
      int         i;
      std::string fName;
      int         elementSize;
      MET_SizeOfType(m_ElementType, &elementSize);
      std::streamoff elementNumberOfBytes = elementSize * m_ElementNumberOfChannels;
      std::streamoff sliceNumberOfBytes = m_SubQuantity[m_NDims - 1] * elementNumberOfBytes;

      auto * writeStreamTemp = new METAIO_STREAM::ofstream;
      for (i = 1; i <= m_DimSize[m_NDims - 1]; i++)
      {
        fName = string_format(dataFileName, i);

        openWriteStream(*writeStreamTemp, fName, false);

        if (!m_CompressedData)
        {
          // BUG? This looks wrong to me as the third parameter should
          // contain the number of elements/quantity, not number of bytes -BCL
          if (!MetaImage::M_WriteElementData(
               writeStreamTemp, &((static_cast<const char *>(_data))[(i - 1) * sliceNumberOfBytes]), sliceNumberOfBytes))
          {
            writeStreamTemp->close();
            delete writeStreamTemp;
            return false;
          }
        }
        else
        {
          unsigned char * compressedData = nullptr;
          std::streamoff  compressedDataSize = 0;

          // Compress the data slice by slice
          compressedData = MET_PerformCompression(&((static_cast<const unsigned char *>(_data))[(i - 1) * sliceNumberOfBytes]),
                                                  sliceNumberOfBytes,
                                                  &compressedDataSize,
                                                  m_CompressionLevel);

          // Write the compressed data
          if (!MetaImage::M_WriteElementData(writeStreamTemp, compressedData, compressedDataSize))
          {
            delete[] compressedData;
            writeStreamTemp->close();
            delete writeStreamTemp;
            return false;
          }

          delete[] compressedData;
        }

        writeStreamTemp->close();
      }

      delete writeStreamTemp;
    }
    else // write the image in one unique other file
    {
      auto * writeStreamTemp = new METAIO_STREAM::ofstream;
      openWriteStream(*writeStreamTemp, dataFileName, false);

      if (!MetaImage::M_WriteElementData(writeStreamTemp, _data, _dataQuantity))
      {
        writeStreamTemp->close();
        delete writeStreamTemp;
        return false;
      }

      writeStreamTemp->close();
      delete writeStreamTemp;
    }
  }

  return true;
}


bool
MetaImage::M_WriteElementData(METAIO_STREAM::ofstream * _fstream, const void * _data, std::streamoff _dataQuantity)
{
  if (!m_BinaryData)
  {

    double tf;
    for (std::streamoff i = 0; i < _dataQuantity; i++)
    {
      MET_ValueToDouble(m_ElementType, _data, i, &tf);
      if ((i + 1) / 10 == (i + 1.0) / 10.0)
      {
        (*_fstream) << tf << '\n';
      }
      else
      {
        (*_fstream) << tf << " ";
      }
    }
  }
  else
  {
    if (m_CompressedData)
    {
      // the data is written in writes no bigger then MaxIOChunk
      std::streamoff bytesRemaining = _dataQuantity;
      while (bytesRemaining)
      {
        std::streamoff chunkToWrite = bytesRemaining > MaxIOChunk ? MaxIOChunk : bytesRemaining;
        _fstream->write(static_cast<const char *>(_data), static_cast<size_t>(chunkToWrite));
        _data = static_cast<const char *>(_data) + chunkToWrite; // <- Note: data is changed
        bytesRemaining -= chunkToWrite;
      }
    }
    else
    {
      int elementSize;
      MET_SizeOfType(m_ElementType, &elementSize);
      std::streamoff elementNumberOfBytes = elementSize * m_ElementNumberOfChannels;

      // the data is written in writes no bigger then MaxIOChunk
      std::streamoff bytesRemaining = _dataQuantity * elementNumberOfBytes;
      while (bytesRemaining)
      {
        std::streamoff chunkToWrite = bytesRemaining > MaxIOChunk ? MaxIOChunk : bytesRemaining;
        _fstream->write(static_cast<const char *>(_data), static_cast<size_t>(chunkToWrite));
        _data = static_cast<const char *>(_data) + chunkToWrite; // <- Note: _data is changed
        bytesRemaining -= chunkToWrite;
      }
    }
  }

  // check if the io stream did not fail in the process of writing
  if (_fstream->fail())
  {
    std::cerr << "MetaImage: M_WriteElementsData: file stream is fail after write" << '\n';
    return false;
  }

  return true;
}

/** Streaming related functions */
bool
MetaImage::ReadROI(int *        _indexMin,
                   int *        _indexMax,
                   const char * _headerName,
                   bool         _readElements,
                   void *       _buffer,
                   unsigned int subSamplingFactor)
{
  M_Destroy();

  Clear();

  M_SetupReadFields();

  if (_headerName != nullptr)
  {
    m_FileName = _headerName;
  }

  M_PrepareNewReadStream();

  auto * tmpReadStream = new METAIO_STREAM::ifstream;

  openReadStream(*tmpReadStream, m_FileName);

  if (!tmpReadStream->is_open())
  {
    delete tmpReadStream;
    return false;
  }

  if (!this->ReadROIStream(_indexMin, _indexMax, 0, tmpReadStream, _readElements, _buffer, subSamplingFactor))
  {
    tmpReadStream->close();
    delete tmpReadStream;
    return false;
  }

  tmpReadStream->close();

  delete tmpReadStream;

  return true;
}

/** Read the ROI Stream */
bool
MetaImage::ReadROIStream(int *           _indexMin,
                         int *           _indexMax,
                         int             _nDims,
                         METAIO_STREAM::ifstream * _stream,
                         bool            _readElements,
                         void *          _buffer,
                         unsigned int    subSamplingFactor)
{
  if (!MetaObject::ReadStream(_nDims, _stream))
  {
    std::cerr << "MetaImage: Read: Cannot parse file" << '\n';
    return false;
  }

  if (_readElements)
  {
    if (_buffer == nullptr)
    {
      InitializeEssential(
        m_NDims, m_DimSize, m_ElementSpacing, m_ElementType, m_ElementNumberOfChannels, nullptr, true);
    }
    else
    {
      InitializeEssential(
        m_NDims, m_DimSize, m_ElementSpacing, m_ElementType, m_ElementNumberOfChannels, _buffer, false);
    }

    // Streaming related. We need to update some of the fields
    std::streamoff quantity = 1;
    int            i;
    size_t         j;
    for (i = 0; i < m_NDims; i++)
    {
      quantity *= (_indexMax[i] - _indexMin[i] + 1);
    }

    bool        usePath;
    std::string pathName;
    std::string fName;
    usePath = MET_GetFilePath(m_FileName, pathName);

    if ("Local" == m_ElementDataFileName || "LOCAL" == m_ElementDataFileName || "local" == m_ElementDataFileName)
    {
      if (!M_ReadElementsROI(_stream, m_ElementData, quantity, _indexMin, _indexMax, subSamplingFactor, m_Quantity))
      {
        return false;
      }
    }
    else if ("LIST" == m_ElementDataFileName.substr(0, 4))
    {
      int     nWrds;
      char ** wrds;
      MET_StringToWordArray(m_ElementDataFileName.c_str(), &nWrds, &wrds);
      for (i = 0; i < nWrds; i++)
      {
        delete[] wrds[i];
      }
      delete[] wrds;
      char   s[1024];
      auto * readStreamTemp = new METAIO_STREAM::ifstream;
      int    elementSize;
      MET_SizeOfType(m_ElementType, &elementSize);
      elementSize *= m_ElementNumberOfChannels;

      int minV = _indexMin[m_NDims - 1];
      int maxV = minV + (_indexMax[m_NDims - 1] - _indexMin[m_NDims - 1]);

      int cnt = 0;

      // Read the previous lines
      for (i = 0; i < minV; i++)
      {
        _stream->getline(s, 1024);
      }

      for (i = minV; i <= maxV; i += 1)
      {
        _stream->getline(s, 1024);
        if (!_stream->eof())
        {
          j = strlen(s) - 1;
          while (j > 0 && (isspace(s[j]) || !isprint(s[j])))
          {
            s[j--] = '\0';
          }
          if (usePath && !FileIsFullPath(s))
          {
            fName = pathName + s;
          }
          else
          {
            fName = s;
          }

          openReadStream(*readStreamTemp, fName);
          if (!readStreamTemp->is_open())
          {
            std::cerr << "MetaImage: Read: cannot open slice" << '\n';
            delete readStreamTemp;
            return false;
          }

          // read only one slice
          int * indexMin = new int[m_NDims];
          int * indexMax = new int[m_NDims];
          quantity = 1;
          for (int k = 0; k < m_NDims - 1; k++)
          {
            quantity *= _indexMax[k] - _indexMin[k] + 1;
            indexMin[k] = _indexMin[k];
            indexMax[k] = _indexMax[k];
          }
          indexMin[m_NDims - 1] = 0;
          indexMax[m_NDims - 1] = 0;

          if (!M_ReadElementsROI(readStreamTemp,
                                 &((static_cast<char *>(m_ElementData))[cnt * quantity * elementSize]),
                                 quantity,
                                 indexMin,
                                 indexMax,
                                 subSamplingFactor,
                                 m_SubQuantity[m_NDims - 1]))
          {
            readStreamTemp->close();
            delete readStreamTemp;
            return false;
          }

          cnt++;
          readStreamTemp->close();
        }
      }
      delete readStreamTemp;
    }
    else if (m_ElementDataFileName.find('%') != std::string::npos)
    {
      int elementSize;
      MET_SizeOfType(m_ElementType, &elementSize);
      elementSize *= m_ElementNumberOfChannels;

      int         nWrds;
      char **     wrds;
      int         minV = 1;
      int         maxV;
      int         stepV = 1;
      std::string s;
      auto *      readStreamTemp = new METAIO_STREAM::ifstream;
      MET_StringToWordArray(m_ElementDataFileName.c_str(), &nWrds, &wrds);
      if (nWrds >= 2)
      {
        minV = static_cast<int>(atof(wrds[1]));
      }
      if (nWrds >= 3)
      {
        maxV = static_cast<int>(atof(wrds[2]));
        stepV = (maxV - minV) / (m_DimSize[m_NDims - 1]);
      }
      if (nWrds >= 4)
      {
        stepV = static_cast<int>(atof(wrds[3]));
      }
      if (nWrds >= 5)
      {
        // In this case, the filename must have had spaces in the
        // name.  The filename was parsed into multiple pieces by the
        // MET_StringToWordArray, which parses based on spaces.
        // Thus, we need to reconstruct the filename in this case.
        // The last three wrds must be numbers.  If they are not, we give an error.
        for (i = nWrds - 3; i < nWrds; i++)
        {
          for (j = 0; j < strlen(wrds[i]); j++)
          {
            if (!isdigit(wrds[i][j]))
            {
              std::cerr << "MetaImage: Read: Last three arguments must be numbers!" << '\n';
              continue;
            }
          }
        }
        stepV = static_cast<int>(atof(wrds[nWrds - 1]));
        minV = static_cast<int>(atof(wrds[nWrds - 3]));
        for (i = 1; i < nWrds - 3; i++)
        {
          std::strncat(wrds[0], " ", METAIO_MAX_WORD_SIZE);
          std::strncat(wrds[0], wrds[i], METAIO_MAX_WORD_SIZE);
        }
      }
      int cnt = 0;

      // Uses the _indexMin and _indexMax
      minV += _indexMin[m_NDims - 1];
      maxV = minV + (_indexMax[m_NDims - 1] - _indexMin[m_NDims - 1]) * stepV;

      for (i = minV; i <= maxV; i += stepV)
      {
        s = string_format(wrds[0], i);
        if (usePath && !FileIsFullPath(s.c_str()))
        {
          fName = pathName + s;
        }
        else
        {
          fName = s;
        }


        openReadStream(*readStreamTemp, fName);
        if (!readStreamTemp->is_open())
        {
          std::cerr << "MetaImage: Read: cannot construct file" << '\n';
          for (i = 0; i < nWrds; i++)
          {
            delete[] wrds[i];
          }
          delete[] wrds;
          delete readStreamTemp;
          return false;
        }

        // read only one slice
        int * indexMin = new int[m_NDims];
        int * indexMax = new int[m_NDims];
        quantity = 1;
        for (int k = 0; k < m_NDims - 1; k++)
        {
          quantity *= _indexMax[k] - _indexMin[k] + 1;
          indexMin[k] = _indexMin[k];
          indexMax[k] = _indexMax[k];
        }
        indexMin[m_NDims - 1] = 0;
        indexMax[m_NDims - 1] = 0;

        if (!M_ReadElementsROI(readStreamTemp,
                               &((static_cast<char *>(m_ElementData))[cnt * quantity * elementSize]),
                               quantity,
                               indexMin,
                               indexMax,
                               subSamplingFactor,
                               m_SubQuantity[m_NDims - 1]))
        {
          delete[] indexMin;
          delete[] indexMax;
          readStreamTemp->close();
          for (i = 0; i < nWrds; i++)
          {
            delete[] wrds[i];
          }
          delete[] wrds;
          delete readStreamTemp;
          return false;
        }

        cnt++;

        delete[] indexMin;
        delete[] indexMax;

        readStreamTemp->close();
      }

      for (i = 0; i < nWrds; i++)
      {
        delete[] wrds[i];
      }
      delete[] wrds;

      delete readStreamTemp;
    }
    else
    {
      if (usePath && !FileIsFullPath(m_ElementDataFileName.c_str()))
      {
        fName = pathName + m_ElementDataFileName;
      }
      else
      {
        fName = m_ElementDataFileName;
      }

      auto * readStreamTemp = new METAIO_STREAM::ifstream;

      const char * extensions[] = { "", ".gz", ".Z", nullptr };
      for (unsigned ii = 0; extensions[ii] != nullptr; ii++)
      {
        std::string tempFName(fName);
        tempFName += extensions[ii];
        openReadStream(*readStreamTemp, tempFName);
        if (readStreamTemp->is_open())
        {
          if (ii > 0)
          {
            this->CompressedData(true);
            this->BinaryData(true);
          }
          break;
        }
      }

      if (!readStreamTemp->is_open())
      {
        std::cerr << "MetaImage: ReadROI: Cannot open data file" << '\n';
        if (m_ReadStream)
        {
          m_ReadStream->close();
        }
        delete readStreamTemp;
        return false;
      }

      if (!M_ReadElementsROI(readStreamTemp, m_ElementData, quantity, _indexMin, _indexMax, subSamplingFactor, m_Quantity))
      {
        readStreamTemp->close();
        delete readStreamTemp;
        return false;
      }

      readStreamTemp->close();
      delete readStreamTemp;
    }
  }
  return true;
}

/** Read an ROI */
bool
MetaImage::M_ReadElementsROI(METAIO_STREAM::ifstream * _fstream,
                             void *          _data,
                             std::streamoff  _dataQuantity,
                             int *           _indexMin,
                             int *           _indexMax,
                             unsigned int    subSamplingFactor,
                             std::streamoff  _totalDataQuantity)
{
  if (_totalDataQuantity == 0)
  {
    _totalDataQuantity = _dataQuantity;
  }

  for (int dim = 0; dim < m_NDims; dim++)
  {
    _indexMin[dim] *= subSamplingFactor;
    _indexMax[dim] *= subSamplingFactor;
  }


  META_DEBUG_PRINT( "MetaImage: M_ReadElementsROI" );

  if (m_HeaderSize > 0)
  {
    _fstream->seekg(m_HeaderSize, std::ios::beg);
    if (!_fstream->good())
    {
      std::cerr << "MetaImage: M_ReadElementsROI: header not read correctly" << '\n';
      return false;
    }
  }

  int elementSize;
  MET_SizeOfType(m_ElementType, &elementSize);
  std::streamoff readSize = _dataQuantity * m_ElementNumberOfChannels * elementSize;
  int            elementNumberOfBytes = elementSize * m_ElementNumberOfChannels;

  META_DEBUG_PRINT( "MetaImage: M_ReadElementsROI: ReadSize = " << readSize );

  if (m_HeaderSize == -1)
  {
    META_DEBUG_PRINT( "MetaImage: M_ReadElementsROI: Skipping header" );
    std::streamoff headSize = _totalDataQuantity * m_ElementNumberOfChannels * elementSize;
    _fstream->seekg(-headSize, std::ios::end);
  }

  std::streampos dataPos = _fstream->tellg();
  std::streamoff i;

  // If compressed we inflate
  if (m_BinaryData && m_CompressedData)
  {
    // if m_CompressedDataSize is not defined we assume the size of the
    // file is the size of the compressed data
    if (m_CompressedDataSize == 0)
    {
      _fstream->seekg(0, std::ios::end);
      m_CompressedDataSize = _fstream->tellg();
      _fstream->seekg(0, std::ios::beg);
    }

    auto * data = static_cast<unsigned char *>(_data);
    // Initialize the index
    int * currentIndex = new int[m_NDims];
    for (i = 0; i < m_NDims; i++)
    {
      currentIndex[i] = _indexMin[i];
    }

    // Optimize the size of the buffer to read depending on the
    // region shape
    // This calculate the number of continuous bytes in the file
    // which can be read
    std::streamoff elementsToRead = 1;
    int            movingDirection = 0;
    do
    {
      elementsToRead *= _indexMax[movingDirection] - _indexMin[movingDirection] + 1;
      ++movingDirection;
    } while (subSamplingFactor == 1 && movingDirection < m_NDims && _indexMin[movingDirection - 1] == 0 &&
             _indexMax[movingDirection - 1] == m_DimSize[movingDirection - 1] - 1);

    std::streamoff bytesToRead = elementsToRead * elementNumberOfBytes;
    std::streamoff gc = 0;

    bool done = false;
    while (!done)
    {
      // Seek to the right position
      std::streamoff seekoff = 0;
      for (i = 0; i < m_NDims; i++)
      {
        seekoff += m_SubQuantity[i] * elementNumberOfBytes * currentIndex[i];
      }


      if (subSamplingFactor > 1)
      {
        auto *         subdata = new unsigned char[static_cast<size_t>(bytesToRead)];
        std::streamoff rOff =
          MET_UncompressStream(_fstream, seekoff, subdata, bytesToRead, m_CompressedDataSize, m_CompressionTable);
        // if there was a read error
        if (rOff == -1)
        {
          delete[] currentIndex;
          return false;
        }

        for (std::streamoff p = 0; p < bytesToRead; p += (subSamplingFactor * m_ElementNumberOfChannels * elementSize))
        {
          for (int s = 0; s < m_ElementNumberOfChannels * elementSize; s++)
          {
            *data = subdata[p + s];
            gc++;
            data++;
          }
        }
        delete[] subdata;
      }
      else
      {
        std::streamoff rOff =
          MET_UncompressStream(_fstream, seekoff, data, bytesToRead, m_CompressedDataSize, m_CompressionTable);
        if (rOff == -1)
        {
          delete[] currentIndex;
          return false;
        }
        gc += bytesToRead;
        data += bytesToRead;
      }

      if (gc == readSize)
      {
        break;
      }

      // Go forward
      if (m_NDims == 1)
      {
        break;
      }

      currentIndex[movingDirection] += subSamplingFactor;

      // Check if we are still in the region
      for (i = 1; i < m_NDims; i++)
      {
        if (currentIndex[i] > _indexMax[i])
        {
          if (i == m_NDims - 1)
          {
            done = true;
            break;
          }
          else
          {
            currentIndex[i] = _indexMin[i];
            currentIndex[i + 1] += subSamplingFactor;
          }
        }
      }
    }

    if (gc != readSize)
    {
      std::cerr << "MetaImage: M_ReadElementsROI: compressed data not read completely" << '\n';
      std::cerr << "   ideal = " << readSize << " : actual = " << gc << '\n';
      delete[] currentIndex;
      return false;
    }

    delete[] currentIndex;
  }
  else // if not compressed
  {
    double tf = 0;
    MET_SizeOfType(m_ElementType, &elementSize);

    char * data = static_cast<char *>(_data);
    // Initialize the index
    int * currentIndex = new int[m_NDims];
    for (i = 0; i < m_NDims; i++)
    {
      currentIndex[i] = _indexMin[i];
    }

    // Optimize the size of the buffer to read depending on the
    // region shape
    // This calculate the number of continuous bytes in the file
    // which can be read
    std::streamoff elementsToRead = 1;
    int            movingDirection = 0;
    do
    {
      elementsToRead *= _indexMax[movingDirection] - _indexMin[movingDirection] + 1;
      ++movingDirection;
    } while (subSamplingFactor == 1 && movingDirection < m_NDims && _indexMin[movingDirection - 1] == 0 &&
             _indexMax[movingDirection - 1] == m_DimSize[movingDirection - 1] - 1);

    // readLine *= m_ElementNumberOfChannels*elementSize;
    std::streamoff gc = 0;

    bool done = false;
    while (!done)
    {
      // Seek to the right position
      std::streamoff seekoff = 0;
      for (i = 0; i < m_NDims; i++)
      {
        seekoff += m_SubQuantity[i] * m_ElementNumberOfChannels * elementSize * currentIndex[i];
      }

      _fstream->seekg(dataPos + seekoff, std::ios::beg);

      // Read a line
      if (subSamplingFactor > 1)
      {
        if (!m_BinaryData) // Not binary data
        {
          for (i = 0; i < elementsToRead; i += subSamplingFactor)
          {
            *_fstream >> tf;
            MET_DoubleToValue(tf, m_ElementType, _data, i);

            for (unsigned int j = 0; j < subSamplingFactor; j++)
            {
              _fstream->get();
            }
          }
        }
        else // Binary data
        {
          char * subdata = new char[static_cast<size_t>(elementsToRead * elementNumberOfBytes)];

          _fstream->read(subdata, size_t(elementsToRead * elementNumberOfBytes));

          for (std::streamoff p = 0; p < elementsToRead * elementNumberOfBytes;
               p += (subSamplingFactor * elementNumberOfBytes))
          {
            for (int s = 0; s < elementNumberOfBytes; s++)
            {
              *data = subdata[p + s];
              gc++;
              data++;
            }
          }
          delete[] subdata;
        }
      }
      else
      {
        if (!m_BinaryData) // Not binary data
        {
          // anyone using ROI reading of ASCII??
          // does this work? what about incrementing data?
          // what about data sizes and random access of file?
          std::streamoff blockSize = elementsToRead * m_ElementNumberOfChannels * elementSize;
          if (!M_ReadElementData(_fstream, data, static_cast<size_t>(blockSize)))
          {
            delete[] currentIndex;
            return false;
          }
          gc += blockSize;
        }
        else // binary data
        {
          if (!M_ReadElementData(_fstream, data, elementsToRead))
          {
            delete[] currentIndex;
            return false;
          }
          gc += elementsToRead * elementNumberOfBytes;
          data += elementsToRead * elementNumberOfBytes;
        }
      }

      // I don't think this check is really needed -BCL
      if (gc == readSize)
      {
        break;
      }

      // check if there is only one read needed
      if (movingDirection >= m_NDims)
      {
        break;
      }

      // Go forward
      currentIndex[movingDirection] += subSamplingFactor;

      // Check if we are still in the region
      for (i = movingDirection; i < m_NDims; i++)
      {
        if (currentIndex[i] > _indexMax[i])
        {
          if (i == m_NDims - 1)
          {
            done = true;
            break;
          }
          else
          {
            currentIndex[i] = _indexMin[i];
            currentIndex[i + 1] += subSamplingFactor;
          }
        }
      }
    }

    delete[] currentIndex;

    if (gc != readSize)
    {
      std::cerr << "MetaImage: M_ReadElementsROI: data not read completely" << '\n';
      std::cerr << "   ideal = " << readSize << " : actual = " << gc << '\n';
      return false;
    }
  }

  return true;
}


bool
MetaImage::M_ReadElementData(METAIO_STREAM::ifstream * _fstream, void * _data, std::streamoff _dataQuantity)
{
  // NOTE: this method is different from WriteElementData
  std::streamoff gc = 0;

  if (!m_BinaryData)
  {
    double tf = 0;

    for (int i = 0; i < _dataQuantity; i++)
    {
      *_fstream >> tf;
      MET_DoubleToValue(tf, m_ElementType, _data, i);
      _fstream->get();
      ++gc;
    }
  }
  else
  {
    if (m_CompressedData)
    {

      // the data is read with calls no bigger then MaxIOChunk
      std::streamoff bytesRemaining = _dataQuantity;
      while (bytesRemaining)
      {
        std::streamoff chunkToRead = bytesRemaining > MaxIOChunk ? MaxIOChunk : bytesRemaining;
        _fstream->read(static_cast<char *>(_data), static_cast<size_t>(chunkToRead));
        _data = static_cast<char *>(_data) + chunkToRead;
        bytesRemaining -= chunkToRead;
        std::streamsize numberOfBytesRead = _fstream->gcount();
        gc += numberOfBytesRead;
      }
    }
    else
    {
      int elementSize;
      MET_SizeOfType(m_ElementType, &elementSize);
      std::streamoff elementNumberOfBytes = elementSize * m_ElementNumberOfChannels;

      // the data is read with calls no bigger than MaxIOChunk
      std::streamoff bytesRemaining = _dataQuantity * elementNumberOfBytes;
      while (bytesRemaining)
      {
        std::streamoff chunkToRead = bytesRemaining > MaxIOChunk ? MaxIOChunk : bytesRemaining;
        _fstream->read(static_cast<char *>(_data), static_cast<size_t>(chunkToRead));
        _data = static_cast<char *>(_data) + chunkToRead;
        bytesRemaining -= chunkToRead;
        std::streamsize numberOfBytesRead = _fstream->gcount();
        gc += numberOfBytesRead;
      }
      // convert to number of bytes so that it'll match gc's units
      _dataQuantity *= elementNumberOfBytes;
    }
  }

  // check that we actually read the correct number of bytes
  if (gc != _dataQuantity)
  {
    std::cerr << "MetaImage: M_ReadElementsData: data not read completely" << '\n';
    std::cerr << "   ideal = " << _dataQuantity << " : actual = " << gc << '\n';
    return false;
  }

  // check if the io stream did not fail in the process of reading
  if (_fstream->fail())
  {
    std::cerr << "MetaImage: M_ReadElementsData: file stream is fail after read" << '\n';
    return false;
  }

  return true;
}


#if (METAIO_USE_NAMESPACE)
}
#endif
