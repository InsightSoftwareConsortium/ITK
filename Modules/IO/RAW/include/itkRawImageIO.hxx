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
#ifndef itkRawImageIO_hxx
#define itkRawImageIO_hxx

#include "itkRawImageIO.h"
#include "itkIntTypes.h"

namespace itk
{
template< typename TPixel, unsigned int VImageDimension >
RawImageIO< TPixel, VImageDimension >::RawImageIO():
  ImageIOBase()
{
  this->SetNumberOfComponents(1);
  this->SetPixelTypeInfo( static_cast<const PixelType *>(ITK_NULLPTR) );
  this->SetNumberOfDimensions(VImageDimension);

  for ( unsigned int idx = 0; idx < VImageDimension; ++idx )
    {
    m_Spacing.insert(m_Spacing.begin() + idx, 1.0);
    m_Origin.insert(m_Origin.begin() + idx, 0.0);
    }

  m_HeaderSize = 0;
  m_ManualHeaderSize = false;

  // Left over from short reader
  m_ImageMask = 0xffff;
  m_ByteOrder = ImageIOBase::BigEndian;
  m_FileDimensionality = 2;
  m_FileType = Binary;
}

template< typename TPixel, unsigned int VImageDimension >
RawImageIO< TPixel, VImageDimension >::~RawImageIO()
{}

template< typename TPixel, unsigned int VImageDimension >
void RawImageIO< TPixel, VImageDimension >::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ImageMask: " << m_ImageMask << std::endl;
  os << indent << "FileDimensionality: " << m_FileDimensionality << std::endl;
}

template< typename TPixel, unsigned int VImageDimension >
SizeValueType RawImageIO< TPixel, VImageDimension >::GetHeaderSize()
{
  std::ifstream file;

  if ( m_FileName == "" )
    {
    itkExceptionMacro(<< "A FileName must be specified.");
    }

  if ( !m_ManualHeaderSize )
    {
    if ( m_FileType == ASCII )
      {
      return 0;                          //cannot determine it
      }
    this->ComputeStrides();

    // make sure we figure out a filename to open
    this->OpenFileForReading( file, m_FileName );

    // Get the size of the header from the size of the image
    file.seekg(0, std::ios::end);

    m_HeaderSize = static_cast<SizeValueType>(
      static_cast<typename ::itk::intmax_t>( file.tellg() )
      - static_cast<typename ::itk::intmax_t>( this->m_Strides[m_FileDimensionality + 1] )
    );
    }

  return m_HeaderSize;
}

template< typename TPixel, unsigned int VImageDimension >
void RawImageIO< TPixel, VImageDimension >
::SetHeaderSize(SizeValueType size)
{
  if ( size != m_HeaderSize )
    {
    m_HeaderSize = size;
    this->Modified();
    }
  m_ManualHeaderSize = true;
}

template< typename TPixel, unsigned int VImageDimension >
void RawImageIO< TPixel, VImageDimension >
::Read(void *buffer)
{
  std::ifstream file;

  // Open the file
  this->OpenFileForReading( file, m_FileName );
  this->ComputeStrides();

  // Offset into file
  SizeValueType streamStart = this->GetHeaderSize();
  file.seekg( (OffsetValueType)streamStart, std::ios::beg );
  if ( file.fail() )
    {
    itkExceptionMacro(<< "File seek failed");
    }

  const SizeValueType numberOfBytesToBeRead =
    static_cast< SizeValueType >( this->GetImageSizeInBytes() );

  itkDebugMacro(<< "Reading " << numberOfBytesToBeRead << " bytes");

  if ( m_FileType == Binary )
    {
    if ( !this->ReadBufferAsBinary(file, buffer, numberOfBytesToBeRead) )
      {
      itkExceptionMacro(<< "Read failed: Wanted "
                        << numberOfBytesToBeRead
                        << " bytes, but read "
                        << file.gcount() << " bytes.");
      }
    }
  else
    {
    this->ReadBufferAsASCII( file, buffer, this->GetComponentType(),
                             this->GetImageSizeInComponents() );
    }

  itkDebugMacro(<< "Reading Done");

#define itkReadRawBytesAfterSwappingMacro(StrongType, WeakType)   \
  ( this->GetComponentType() == WeakType )                        \
    {                                                             \
    typedef ByteSwapper< StrongType > InternalByteSwapperType;    \
    if ( m_ByteOrder == LittleEndian )                            \
      {                                                           \
      InternalByteSwapperType::SwapRangeFromSystemToLittleEndian( \
        (StrongType *)buffer, this->GetImageSizeInComponents() ); \
      }                                                           \
    else if ( m_ByteOrder == BigEndian )                          \
      {                                                           \
      InternalByteSwapperType::SwapRangeFromSystemToBigEndian(    \
        (StrongType *)buffer, this->GetImageSizeInComponents() ); \
      }                                                           \
    }

  // Swap bytes if necessary
  if itkReadRawBytesAfterSwappingMacro(unsigned short, USHORT)
  else if itkReadRawBytesAfterSwappingMacro(short, SHORT)
  else if itkReadRawBytesAfterSwappingMacro(char, CHAR)
  else if itkReadRawBytesAfterSwappingMacro(unsigned char, UCHAR)
  else if itkReadRawBytesAfterSwappingMacro(unsigned int, UINT)
  else if itkReadRawBytesAfterSwappingMacro(int, INT)
  else if itkReadRawBytesAfterSwappingMacro(long, LONG)
  else if itkReadRawBytesAfterSwappingMacro(unsigned long, ULONG)
  else if itkReadRawBytesAfterSwappingMacro(float, FLOAT)
  else if itkReadRawBytesAfterSwappingMacro(double, DOUBLE)
}

template< typename TPixel, unsigned int VImageDimension >
bool RawImageIO< TPixel, VImageDimension >
::CanWriteFile(const char *fname)
{
  std::string filename(fname);

  if ( filename == "" )
    {
    return false;
    }

  return true;
}

template< typename TPixel, unsigned int VImageDimension >
void RawImageIO< TPixel, VImageDimension >
::Write(const void *buffer)
{
  std::ofstream file;

  // Open the file
  //
  this->OpenFileForWriting( file, m_FileName );

  // Set up for reading
  this->ComputeStrides();

  // Actually do the writing
  //
  if ( m_FileType == ASCII )
    {
    this->WriteBufferAsASCII( file, buffer, this->GetComponentType(),
                              this->GetImageSizeInComponents() );
    }
  else //binary
    {
    const SizeValueType numberOfBytes      = this->GetImageSizeInBytes();
    const SizeValueType numberOfComponents = this->GetImageSizeInComponents();

#define itkWriteRawBytesAfterSwappingMacro(StrongType, WeakType)        \
  ( this->GetComponentType() == WeakType )                              \
    {                                                                   \
    typedef ByteSwapper< StrongType > InternalByteSwapperType;          \
    const SizeValueType numberOfPixels = numberOfBytes/(sizeof(StrongType)); \
    if ( m_ByteOrder == LittleEndian )                                  \
      {                                                                 \
      StrongType *tempBuffer = new StrongType[numberOfPixels];          \
      memcpy((char *)tempBuffer, buffer, numberOfBytes);          \
      InternalByteSwapperType::SwapRangeFromSystemToLittleEndian(       \
        (StrongType *)tempBuffer, numberOfComponents);                  \
      file.write((char *)tempBuffer, numberOfBytes);                            \
      delete[] tempBuffer;                                              \
      }                                                                 \
    else if ( m_ByteOrder == BigEndian )                                \
      {                                                                 \
      StrongType *tempBuffer = new StrongType[numberOfPixels];          \
      memcpy((char *)tempBuffer, buffer, numberOfBytes);          \
      InternalByteSwapperType::SwapRangeFromSystemToBigEndian(          \
        (StrongType *)tempBuffer, numberOfComponents);                  \
      file.write((char *)tempBuffer, numberOfBytes);                            \
      delete[] tempBuffer;                                              \
      }                                                                 \
    else                                                                \
      {                                                                 \
      file.write(static_cast< const char * >( buffer ), numberOfBytes); \
      }                                                                 \
    }

    // Swap bytes if necessary
    if itkWriteRawBytesAfterSwappingMacro(unsigned short, USHORT)
    else if itkWriteRawBytesAfterSwappingMacro(short, SHORT)
    else if itkWriteRawBytesAfterSwappingMacro(char, CHAR)
    else if itkWriteRawBytesAfterSwappingMacro(unsigned char, UCHAR)
    else if itkWriteRawBytesAfterSwappingMacro(unsigned int, UINT)
    else if itkWriteRawBytesAfterSwappingMacro(int, INT)
    else if itkWriteRawBytesAfterSwappingMacro(long, LONG)
    else if itkWriteRawBytesAfterSwappingMacro(unsigned long, ULONG)
    else if itkWriteRawBytesAfterSwappingMacro(float, FLOAT)
    else if itkWriteRawBytesAfterSwappingMacro(double, DOUBLE)
    }
}
} // namespace itk
#endif
