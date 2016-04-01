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
#include "itkStreamingImageIOBase.h"

#include "itksys/SystemTools.hxx"
#include "itkMath.h"

namespace itk
{
StreamingImageIOBase::StreamingImageIOBase():
  ImageIOBase()
{}

void StreamingImageIOBase::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

bool StreamingImageIOBase
::StreamReadBufferAsBinary(std::istream & file, void *_buffer)
{
  itkDebugMacro(<< "StreamingReadBufferAsBinary called");

  char *buffer = static_cast< char * >( _buffer );
  // Offset into file
  std::streampos dataPos = this->GetDataPosition();

  itkDebugStatement(
  const std::streamsize sizeOfRegion = static_cast< std::streamsize >( m_IORegion.GetNumberOfPixels() )
                                 * this->GetPixelSize();
  );

  // compute the number of continuous bytes to be read
  std::streamsize sizeOfChunk = 1;
  unsigned int    movingDirection = 0;
  do
    {
    sizeOfChunk *= m_IORegion.GetSize(movingDirection);
    ++movingDirection;
    }
  while ( movingDirection < m_IORegion.GetImageDimension()
          && m_IORegion.GetSize(movingDirection - 1) == this->GetDimensions(movingDirection - 1) );
  sizeOfChunk *= this->GetPixelSize();

  ImageIORegion::IndexType currentIndex = m_IORegion.GetIndex();

  while ( m_IORegion.IsInside(currentIndex) )
    {
    // calculate the position to seek to in the file
    std::streampos seekPos = 0;
    SizeValueType  subDimensionQuantity = 1;
    for ( unsigned int i = 0; i < m_IORegion.GetImageDimension(); ++i )
      {
      seekPos = seekPos + static_cast< std::streamoff >( subDimensionQuantity
                                                         * this->GetPixelSize()
                                                         * currentIndex[i] );
      subDimensionQuantity *= this->GetDimensions(i);
      }

    itkDebugMacro(
      << "Reading " << sizeOfChunk << " of " << sizeOfRegion << " bytes for " << m_FileName << " at " << dataPos
      + seekPos << " position in file");

    file.seekg(dataPos + seekPos, std::ios::beg);

    if ( ! this->ReadBufferAsBinary(file, buffer, sizeOfChunk) )
      {
      itkExceptionMacro( "Error reading in ReadBufferAsBinary!" );
      }

    // increment the buffer pointer
    buffer += sizeOfChunk;

    if ( file.fail() )
      {
      itkExceptionMacro(<< "Fail reading");
      }

    if ( movingDirection == m_IORegion.GetImageDimension() )
      {
      break;
      }

    // increment index to next chunk
    ++currentIndex[movingDirection];
    for ( unsigned int i = movingDirection; i < m_IORegion.GetImageDimension() - 1; ++i )
      {
      // when reaching the end of the moving index dimension carry to
      // higher dimensions
      if ( static_cast< ImageIORegion::SizeValueType >( currentIndex[i]  - m_IORegion.GetIndex(i) ) >=
           m_IORegion.GetSize(i) )
        {
        currentIndex[i] = m_IORegion.GetIndex(i);
        ++currentIndex[i + 1];
        }
      }
    }

  return true;
}

bool StreamingImageIOBase::ReadBufferAsBinary(std::istream & is, void *buffer, StreamingImageIOBase::SizeType num)
{
  // some systems have a limit of 2GB to be read at once
  const SizeType maxChunk = 1024 * 1024 * 1024;

  std::streamsize bytesRemaining = static_cast< std::streamsize >( num );

  while ( bytesRemaining )
    {
    std::streamsize bytesToRead = bytesRemaining > maxChunk ? maxChunk : bytesRemaining;

    itkDebugMacro(<< "Reading " << bytesToRead << " of " << bytesRemaining << " bytes for " << m_FileName);

    is.read(static_cast< char * >( buffer ),  bytesToRead);

    if ( ( is.gcount() != bytesToRead ) || is.fail() )
      {
      return false;
      }
    buffer =  static_cast< char * >( buffer ) + bytesToRead;
    bytesRemaining -= bytesToRead;
    }

  return true;
}

bool StreamingImageIOBase::WriteBufferAsBinary(std::ostream & os,
                                               const void *buffer,
                                               StreamingImageIOBase::SizeType num)
{
  // some systems have a limit of 2GB to be written at once
  const SizeType maxChunk = 1024 * 1024 * 1024;

  std::streamsize bytesRemaining = num;

  while ( bytesRemaining )
    {
    SizeType bytesToWrite = bytesRemaining > maxChunk ? maxChunk : bytesRemaining;

    itkDebugMacro(<< "Writing " << bytesToWrite << " of " << bytesRemaining << " bytes for " << m_FileName);

    os.write(static_cast< const char * >( buffer ), bytesToWrite);
    if ( os.fail() )
      {
      return false;
      }

    buffer =  static_cast< const char * >( buffer ) + bytesToWrite;
    bytesRemaining -= bytesToWrite;
    }

  return true;
}

bool StreamingImageIOBase::StreamWriteBufferAsBinary(std::ostream & file, const void *_buffer)
{
  itkDebugMacro(<< "StreamingWriteBufferAsBinary called");

  const char *buffer = static_cast< const char * >( _buffer );
  // Offset into file
  std::streampos dataPos = this->GetDataPosition();

  // compute the number of continuous bytes to be written
  std::streamsize sizeOfChunk = 1;
  unsigned int    movingDirection = 0;
  do
    {
    sizeOfChunk *= m_IORegion.GetSize(movingDirection);
    ++movingDirection;
    }
  while ( movingDirection < m_IORegion.GetImageDimension()
          && m_IORegion.GetSize(movingDirection - 1) == this->GetDimensions(movingDirection - 1) );
  sizeOfChunk *= this->GetPixelSize();

  ImageIORegion::IndexType currentIndex = m_IORegion.GetIndex();
  while ( m_IORegion.IsInside(currentIndex) )
    {
    // calculate the position to seek to in the file
    std::streampos seekPos = 0;
    SizeValueType  subDimensionQuantity = 1;
    for ( unsigned int i = 0; i < m_IORegion.GetImageDimension(); ++i )
      {
      seekPos = seekPos + static_cast< std::streamoff >( subDimensionQuantity
                                                         * this->GetPixelSize()
                                                         * currentIndex[i] );
      subDimensionQuantity *= this->GetDimensions(i);
      }

    file.seekp(dataPos + seekPos, std::ios::beg);
    if ( !this->WriteBufferAsBinary(file, buffer, sizeOfChunk) )
      {
      itkExceptionMacro( "Error reading in WriteBufferAsBinary!" );
      }

    // increment the buffer pointer
    buffer += sizeOfChunk;

    itkDebugMacro(
      << "Writing " << sizeOfChunk << " of " <<  " ?? bytes for " << m_FileName << " at " << dataPos + seekPos
      << " position in file");

    if ( file.fail() )
      {
      itkExceptionMacro(<< "Fail writing");
      }

    if ( movingDirection == m_IORegion.GetImageDimension() )
      {
      break;
      }

    // increment index to next chunk
    ++currentIndex[movingDirection];
    for ( unsigned int i = movingDirection; i < m_IORegion.GetImageDimension() - 1; ++i )
      {
      // when reaching the end of the movingDirection dimension carry to
      // higher dimensions
      if ( static_cast< ImageIORegion::SizeValueType >( currentIndex[i] - m_IORegion.GetIndex(i) )
           >=  m_IORegion.GetSize(i) )
        {
        currentIndex[i] = m_IORegion.GetIndex(i);
        ++currentIndex[i + 1];
        }
      }
    }

  return true;
}

bool StreamingImageIOBase::CanStreamRead(void)
{
  return true;
}

bool StreamingImageIOBase::CanStreamWrite(void)
{
  return true;
}

unsigned int
StreamingImageIOBase::GetActualNumberOfSplitsForWriting(unsigned int numberOfRequestedSplits,
                                                        const ImageIORegion & pasteRegion,
                                                        const ImageIORegion & largestPossibleRegion)
{
  if ( !const_cast<StreamingImageIOBase*>(this)->CanStreamWrite() )
    {
    // ImageIOs may not always be able to stream,
    // fall back to super classses non-streaming implementation
    return ImageIOBase::GetActualNumberOfSplitsForWriting( numberOfRequestedSplits,
                                                           pasteRegion,
                                                           largestPossibleRegion );
    }
  else if ( !itksys::SystemTools::FileExists( m_FileName.c_str() ) )
    {
    // file doesn't exits so we don't have potential problems
    }
  else if ( pasteRegion != largestPossibleRegion )
    {
    // we are going to be pasting (may be streaming too)

    // need to check to see if the file is compatible
    std::string errorMessage;
    Pointer     headerImageIOReader = dynamic_cast< StreamingImageIOBase * >( this->CreateAnother().GetPointer() );

    try
      {
      headerImageIOReader->SetFileName( m_FileName.c_str() );
      headerImageIOReader->ReadImageInformation();
      }
    catch ( ... )
      {
      errorMessage = "Unable to read information from file: " + m_FileName;
      }

    // we now need to check that the following match:
    // 2)pixel type
    // 3)dimensions
    // 4)size/origin/spacing
    // 5)direction cosines
    //
    // todo check for byte order

    if ( errorMessage.size() )
      {
      // 0) Can't read file
      }
    // 2)pixel type
    // this->GetPixelType() is not verified because the metaio file format
    // stores all multi-component types as arrays, so it does not
    // distinguish between pixel types. Also as long as the compoent
    // and number of compoents match we should be able to paste, that
    // is the numbers should be the same it is just the interpretation
    // that is not matching
    else if ( headerImageIOReader->GetNumberOfComponents() != this->GetNumberOfComponents()
              || headerImageIOReader->GetComponentType() != this->GetComponentType() )
      {
      errorMessage = "Component type does not match in file: " + m_FileName;
      }
    // 3)dimensions/size
    else if ( headerImageIOReader->GetNumberOfDimensions() != this->GetNumberOfDimensions() )
      {
      errorMessage = "Dimensions does not match in file: " + m_FileName;
      }
    else
      {
      for ( unsigned int i = 0; i < this->GetNumberOfDimensions(); ++i )
        {
        // 4)size/origin/spacing
        if ( headerImageIOReader->GetDimensions(i) != this->GetDimensions(i)
             || Math::NotExactlyEquals(headerImageIOReader->GetSpacing(i), this->GetSpacing(i))
             || Math::NotExactlyEquals(headerImageIOReader->GetOrigin(i), this->GetOrigin(i)) )
          {
          errorMessage = "Size, spacing or origin does not match in file: " + m_FileName;
          break;
          }
        // 5)direction cosines
        if ( headerImageIOReader->GetDirection(i) != this->GetDirection(i) )
          {
          errorMessage = "Direction cosines does not match in file: " + m_FileName;
          break;
          }
        }
      }

    if ( errorMessage.size() )
      {
      itkExceptionMacro("Unable to paste because pasting file exists and is different. " << errorMessage);
      }
    else if ( headerImageIOReader->GetPixelType() != this->GetPixelType() )
      {
      // since there is currently poor support for pixel types in
      // MetaIO we will just warn when it does not match
      itkWarningMacro("Pixel types does not match file, but component type and number of components do.");
      }
    }
  else if ( numberOfRequestedSplits != 1 )
    {
    // we are going be streaming

    // need to remove the file incase the file doesn't match our
    // current header/meta data information
    if ( !itksys::SystemTools::RemoveFile( m_FileName.c_str() ) )
      {
      itkExceptionMacro("Unable to remove file for streaming: " << m_FileName);
      }
    }

  return GetActualNumberOfSplitsForWritingCanStreamWrite(numberOfRequestedSplits, pasteRegion);
}

ImageIORegion StreamingImageIOBase::GenerateStreamableReadRegionFromRequestedRegion(
  const ImageIORegion & requestedRegion) const
{
  // This implementation returns the requestedRegion if
  // streaming is enabled and we are capable

  ImageIORegion streamableRegion(this->m_NumberOfDimensions);

  if ( !m_UseStreamedReading || !const_cast<StreamingImageIOBase*>(this)->CanStreamRead() )
    {
    return ImageIOBase::GenerateStreamableReadRegionFromRequestedRegion( requestedRegion );
    }
  else
    {
    streamableRegion = requestedRegion;
    }

  return streamableRegion;
}

bool StreamingImageIOBase::RequestedToStream(void) const
{
  // we choose the max dimension and then pad the smaller with ones
  //
  // This enables a 2D request from a 3D volume to get the first slice,
  // and a 4D with a 1-sized 4th dimension to equal the 3D volume
  // as well.
  unsigned int maxNumberOfDimension = std::max( this->GetNumberOfDimensions(),
                                                    this->GetIORegion().GetImageDimension() );

  ImageIORegion ioregion(maxNumberOfDimension);
  ImageIORegion largestRegion(maxNumberOfDimension);

  for ( unsigned int i = 0; i < maxNumberOfDimension; i++ )
    {
    largestRegion.SetIndex(i, 0);
    if ( i < this->GetNumberOfDimensions() )
      {
      largestRegion.SetSize( i, this->GetDimensions(i) );
      }
    else
      {
      largestRegion.SetSize(i, 1);
      }

    if ( i < this->GetIORegion().GetImageDimension() )
      {
      ioregion.SetIndex( i, this->GetIORegion().GetIndex(i) );
      ioregion.SetSize( i, this->GetIORegion().GetSize(i) );
      }
    else
      {
      ioregion.SetIndex(i, 0);
      ioregion.SetSize(i, 1);
      }
    }

  return ( largestRegion != ioregion );
}
} // namespace itk
