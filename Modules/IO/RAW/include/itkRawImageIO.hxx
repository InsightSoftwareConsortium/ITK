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
  this->SetPixelTypeInfo( static_cast<const PixelType *>(nullptr) );
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

  if ( m_FileName.empty() )
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

template < typename TStrongType >
static void
itkWriteRawBytesAfterSwappingMacro( const void * buffer, std::ofstream & file, ImageIOBase::ByteOrder byteOrder,
                                    SizeValueType numberOfBytes, SizeValueType numberOfComponents )
{

  using InternalByteSwapperType = ByteSwapper< TStrongType >;
  const SizeValueType numberOfPixels = numberOfBytes / ( sizeof( TStrongType ) );
  if ( byteOrder == ImageIOBase::LittleEndian )
  {
    TStrongType * tempBuffer = new TStrongType[numberOfPixels];
    memcpy( (char *)tempBuffer, buffer, numberOfBytes );
    InternalByteSwapperType::SwapRangeFromSystemToLittleEndian( (TStrongType *)tempBuffer, numberOfComponents );
    file.write( (char *)tempBuffer, numberOfBytes );
    delete[] tempBuffer;
  }
  else if ( byteOrder == ImageIOBase::BigEndian )
  {
    TStrongType * tempBuffer = new TStrongType[numberOfPixels];
    memcpy( (char *)tempBuffer, buffer, numberOfBytes );
    InternalByteSwapperType::SwapRangeFromSystemToBigEndian( (TStrongType *)tempBuffer, numberOfComponents );
    file.write( (char *)tempBuffer, numberOfBytes );
    delete[] tempBuffer;
  }
  else
  {
    file.write( static_cast< const char * >( buffer ), numberOfBytes );
  }
}

template < typename TStrongType >
static void
itkReadRawBytesAfterSwappingMacro( void * buffer, ImageIOBase::ByteOrder byteOrder, SizeValueType numberOfComponents )
{
  using InternalByteSwapperType = ByteSwapper< TStrongType >;
  if ( byteOrder == ImageIOBase::LittleEndian )
  {
    InternalByteSwapperType::SwapRangeFromSystemToLittleEndian( (TStrongType *)buffer, numberOfComponents );
  }
  else if ( byteOrder == ImageIOBase::BigEndian )
  {
    InternalByteSwapperType::SwapRangeFromSystemToBigEndian( (TStrongType *)buffer, numberOfComponents );
  }
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

  const auto numberOfBytesToBeRead = static_cast< SizeValueType >( this->GetImageSizeInBytes() );

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

  itkDebugMacro( << "Reading Done" );
  const SizeValueType numberOfComponents = this->GetImageSizeInComponents();
  // Swap bytes if necessary
  if ( this->GetComponentType() == USHORT )
  {
    itkReadRawBytesAfterSwappingMacro< unsigned short >( buffer, m_ByteOrder, numberOfComponents );
  }
  else if ( this->GetComponentType() == SHORT )
  {
    itkReadRawBytesAfterSwappingMacro< short >( buffer, m_ByteOrder, numberOfComponents );
  }
  else if ( this->GetComponentType() == CHAR )
  {
    itkReadRawBytesAfterSwappingMacro< char >( buffer, m_ByteOrder, numberOfComponents );
  }
  else if ( this->GetComponentType() == UCHAR )
  {
    itkReadRawBytesAfterSwappingMacro< unsigned char >( buffer, m_ByteOrder, numberOfComponents );
  }
  else if ( this->GetComponentType() == UINT )
  {
    itkReadRawBytesAfterSwappingMacro< unsigned int >( buffer, m_ByteOrder, numberOfComponents );
  }
  else if ( this->GetComponentType() == INT )
  {
    itkReadRawBytesAfterSwappingMacro< int >( buffer, m_ByteOrder, numberOfComponents );
  }
  else if ( this->GetComponentType() == LONG )
  {
    itkReadRawBytesAfterSwappingMacro< long >( buffer, m_ByteOrder, numberOfComponents );
  }
  else if ( this->GetComponentType() == ULONG )
  {
    itkReadRawBytesAfterSwappingMacro< unsigned long >( buffer, m_ByteOrder, numberOfComponents );
  }
  else if ( this->GetComponentType() == FLOAT )
  {
    itkReadRawBytesAfterSwappingMacro< float >( buffer, m_ByteOrder, numberOfComponents );
  }
  else if ( this->GetComponentType() == DOUBLE )
  {
    itkReadRawBytesAfterSwappingMacro< double >( buffer, m_ByteOrder, numberOfComponents );
  }
}

template< typename TPixel, unsigned int VImageDimension >
bool RawImageIO< TPixel, VImageDimension >
::CanWriteFile(const char *fname)
{
  std::string filename(fname);

  if ( filename.empty() )
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
    const SizeValueType numberOfBytes = this->GetImageSizeInBytes();
    const SizeValueType numberOfComponents = this->GetImageSizeInComponents();
    // Swap bytes if necessary
    if ( this->GetComponentType() == USHORT )
    {
      itkWriteRawBytesAfterSwappingMacro< unsigned short >(
        buffer, file, m_ByteOrder, numberOfBytes, numberOfComponents );
    }
    else if ( this->GetComponentType() == SHORT )
    {
      itkWriteRawBytesAfterSwappingMacro< short >( buffer, file, m_ByteOrder, numberOfBytes, numberOfComponents );
    }
    else if ( this->GetComponentType() == CHAR )
    {
      itkWriteRawBytesAfterSwappingMacro< char >( buffer, file, m_ByteOrder, numberOfBytes, numberOfComponents );
    }
    else if ( this->GetComponentType() == UCHAR )
    {
      itkWriteRawBytesAfterSwappingMacro< unsigned char >(
        buffer, file, m_ByteOrder, numberOfBytes, numberOfComponents );
    }
    else if ( this->GetComponentType() == UINT )
    {
      itkWriteRawBytesAfterSwappingMacro< unsigned int >(
        buffer, file, m_ByteOrder, numberOfBytes, numberOfComponents );
    }
    else if ( this->GetComponentType() == INT )
    {
      itkWriteRawBytesAfterSwappingMacro< int >( buffer, file, m_ByteOrder, numberOfBytes, numberOfComponents );
    }
    else if ( this->GetComponentType() == LONG )
    {
      itkWriteRawBytesAfterSwappingMacro< long >( buffer, file, m_ByteOrder, numberOfBytes, numberOfComponents );
    }
    else if ( this->GetComponentType() == ULONG )
    {
      itkWriteRawBytesAfterSwappingMacro< unsigned long >(
        buffer, file, m_ByteOrder, numberOfBytes, numberOfComponents );
    }
    else if ( this->GetComponentType() == FLOAT )
    {
      itkWriteRawBytesAfterSwappingMacro< float >( buffer, file, m_ByteOrder, numberOfBytes, numberOfComponents );
    }
    else if ( this->GetComponentType() == DOUBLE )
    {
      itkWriteRawBytesAfterSwappingMacro< double >( buffer, file, m_ByteOrder, numberOfBytes, numberOfComponents );
    }
  }
}
} // namespace itk
#endif
