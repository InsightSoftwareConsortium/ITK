/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkImageIOBase.h"
#include "itkByteSwapper.h"

namespace
{

template <typename TStrongType>
void
_WriteRawBytesAfterSwappingUtility(const void *         buffer,
                                   std::ofstream &      file,
                                   itk::IOByteOrderEnum byteOrder,
                                   itk::SizeValueType   numberOfBytes,
                                   itk::SizeValueType   numberOfComponents)
{

  using InternalByteSwapperType = itk::ByteSwapper<TStrongType>;
  const itk::SizeValueType numberOfPixels = numberOfBytes / (sizeof(TStrongType));
  if (byteOrder == itk::IOByteOrderEnum::LittleEndian && InternalByteSwapperType::SystemIsBigEndian())
  {
    auto * tempBuffer = new TStrongType[numberOfPixels];
    memcpy((char *)tempBuffer, buffer, numberOfBytes);
    InternalByteSwapperType::SwapRangeFromSystemToLittleEndian((TStrongType *)tempBuffer, numberOfComponents);
    file.write((char *)tempBuffer, numberOfBytes);
    delete[] tempBuffer;
  }
  else if (byteOrder == itk::IOByteOrderEnum::BigEndian && InternalByteSwapperType::SystemIsLittleEndian())
  {
    auto * tempBuffer = new TStrongType[numberOfPixels];
    memcpy((char *)tempBuffer, buffer, numberOfBytes);
    InternalByteSwapperType::SwapRangeFromSystemToBigEndian((TStrongType *)tempBuffer, numberOfComponents);
    file.write((char *)tempBuffer, numberOfBytes);
    delete[] tempBuffer;
  }
  else
  {
    file.write(static_cast<const char *>(buffer), numberOfBytes);
  }
}

template <typename TStrongType>
void
_ReadRawBytesAfterSwappingUtility(void * buffer, itk::IOByteOrderEnum byteOrder, itk::SizeValueType numberOfComponents)
{
  using InternalByteSwapperType = itk::ByteSwapper<TStrongType>;
  if (byteOrder == itk::IOByteOrderEnum::LittleEndian)
  {
    InternalByteSwapperType::SwapRangeFromSystemToLittleEndian((TStrongType *)buffer, numberOfComponents);
  }
  else if (byteOrder == itk::IOByteOrderEnum::BigEndian)
  {
    InternalByteSwapperType::SwapRangeFromSystemToBigEndian((TStrongType *)buffer, numberOfComponents);
  }
}
} // namespace

namespace itk
{

void
WriteRawBytesAfterSwapping(IOComponentEnum componentType,
                           const void *    buffer,
                           std::ofstream & file,
                           IOByteOrderEnum byteOrder,
                           SizeValueType   numberOfBytes,
                           SizeValueType   numberOfComponents)
{
  // Swap bytes if necessary
  if (componentType == IOComponentEnum::USHORT)
  {
    _WriteRawBytesAfterSwappingUtility<unsigned short>(buffer, file, byteOrder, numberOfBytes, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::SHORT)
  {
    _WriteRawBytesAfterSwappingUtility<short>(buffer, file, byteOrder, numberOfBytes, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::CHAR)
  {
    _WriteRawBytesAfterSwappingUtility<char>(buffer, file, byteOrder, numberOfBytes, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::UCHAR)
  {
    _WriteRawBytesAfterSwappingUtility<unsigned char>(buffer, file, byteOrder, numberOfBytes, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::UINT)
  {
    _WriteRawBytesAfterSwappingUtility<unsigned int>(buffer, file, byteOrder, numberOfBytes, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::INT)
  {
    _WriteRawBytesAfterSwappingUtility<int>(buffer, file, byteOrder, numberOfBytes, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::LONG)
  {
    _WriteRawBytesAfterSwappingUtility<long>(buffer, file, byteOrder, numberOfBytes, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::LONGLONG)
  {
    _WriteRawBytesAfterSwappingUtility<long long>(buffer, file, byteOrder, numberOfBytes, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::ULONG)
  {
    _WriteRawBytesAfterSwappingUtility<unsigned long>(buffer, file, byteOrder, numberOfBytes, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::ULONGLONG)
  {
    _WriteRawBytesAfterSwappingUtility<unsigned long long>(buffer, file, byteOrder, numberOfBytes, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::FLOAT)
  {
    _WriteRawBytesAfterSwappingUtility<float>(buffer, file, byteOrder, numberOfBytes, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::DOUBLE)
  {
    _WriteRawBytesAfterSwappingUtility<double>(buffer, file, byteOrder, numberOfBytes, numberOfComponents);
  }
}

void
ReadRawBytesAfterSwapping(IOComponentEnum componentType,
                          void *          buffer,
                          IOByteOrderEnum byteOrder,
                          SizeValueType   numberOfComponents)
{
  // Swap bytes if necessary
  if (componentType == IOComponentEnum::USHORT)
  {
    _ReadRawBytesAfterSwappingUtility<unsigned short>(buffer, byteOrder, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::SHORT)
  {
    _ReadRawBytesAfterSwappingUtility<short>(buffer, byteOrder, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::CHAR)
  {
    _ReadRawBytesAfterSwappingUtility<char>(buffer, byteOrder, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::UCHAR)
  {
    _ReadRawBytesAfterSwappingUtility<unsigned char>(buffer, byteOrder, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::UINT)
  {
    _ReadRawBytesAfterSwappingUtility<unsigned int>(buffer, byteOrder, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::INT)
  {
    _ReadRawBytesAfterSwappingUtility<int>(buffer, byteOrder, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::LONG)
  {
    _ReadRawBytesAfterSwappingUtility<long>(buffer, byteOrder, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::LONGLONG)
  {
    _ReadRawBytesAfterSwappingUtility<long long>(buffer, byteOrder, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::ULONG)
  {
    _ReadRawBytesAfterSwappingUtility<unsigned long>(buffer, byteOrder, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::ULONGLONG)
  {
    _ReadRawBytesAfterSwappingUtility<unsigned long long>(buffer, byteOrder, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::FLOAT)
  {
    _ReadRawBytesAfterSwappingUtility<float>(buffer, byteOrder, numberOfComponents);
  }
  else if (componentType == IOComponentEnum::DOUBLE)
  {
    _ReadRawBytesAfterSwappingUtility<double>(buffer, byteOrder, numberOfComponents);
  }
}
} // namespace itk
