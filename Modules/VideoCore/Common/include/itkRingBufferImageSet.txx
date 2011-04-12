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
#ifndef __itkRingBufferImageSet_txx
#define __itkRingBufferImageSet_txx

#include "itkRingBufferImageSet.h"
#include "itkPixelTraits.h"
#include "itkConvertPixelBuffer.h"

namespace itk
{

//-CONSTRUCTOR DESTRUCTOR PRINT------------------------------------------------

//
// Constructor
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::RingBufferImageSet()
{
  this->m_Allocated = false;
  this->Initialize();
}


//
// Destructor
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::~RingBufferImageSet()
{
  this->Deallocate();
}


//
// PrintSelf
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
void
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "RingBufferImageSet:" << std::endl;
  os << indent << "NumberOfBuffers" << this->GetNumberOfBuffers() << std::endl;
  if (this->m_BufferArray.size() != 0)
    {
    os << indent << "PixelContainer:" << std::endl;
    this->m_BufferArray[0]->Print(os, indent.GetNextIndent());
    }
}


//-PUBLIC METHODS--------------------------------------------------------------

//
// Initialize
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
void
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::Initialize()
{
  // Start with no source
  this->m_Source = NULL;
  this->m_SourceOutputIndex = 0;

  // Set Head index to the last buffer so calls to GetNextImage
  // will place the image in buffer 0
  this->m_HeadIndex = VNumberOfBuffers-1;
  this->m_VideoIO = NULL;

  if (this->m_Allocated)
    {
    this->Deallocate();
    }
  this->m_Allocated = false;

  // Create the buffers, but don't allocate yet
  this->m_BufferArray.empty();
  for (unsigned int i = 0; i < VNumberOfBuffers; ++i)
    {
    this->m_BufferArray.push_back(PixelContainer::New());
    this->m_BufferValidFlags.push_back(false);
    }
  this->m_Head = this->m_BufferArray[this->m_HeadIndex];

  // Set Modified time
  this->Modified();

}


//
// Allocate
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
void
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::Allocate()
{
  // Make sure the VideoIO is open for reading
  if (!this->m_VideoIO || !this->m_VideoIO->GetImageSizeInBytes())
    {
    itkExceptionMacro("Cannot allocate buffers without open VideoIO");
    }

  // Make sure the input video has the same number of dimensions as the desired
  // output
  //
  // Note: This may be changed with the implementation of the Image
  //       Interpretation Layer
  if (this->m_VideoIO->GetNumberOfDimensions() != this->GetImageDimension())
    {
    itkExceptionMacro("Cannot convert " << this->m_VideoIO->GetNumberOfDimensions() << "D "
      "image set to " << this->GetImageDimension() << "D");
    }

  // See if a buffer conversion is needed
  ImageIOBase::IOComponentType ioType = ImageIOBase
    ::MapPixelType< ITK_TYPENAME ConvertPixelTraits::ComponentType >::CType;
  if ( m_VideoIO->GetComponentType() != ioType ||
       m_VideoIO->GetNumberOfComponents() != ConvertPixelTraits::GetNumberOfComponents() )
    {
    // the pixel types don't match so a type conversion needs to be
    // performed
    itkDebugMacro( << "Buffer conversion required from: "
                   << m_VideoIO->GetComponentTypeAsString(m_VideoIO->GetComponentType())
                   << " to: "
                   << m_VideoIO->GetComponentTypeAsString(ioType)
                   << " ConvertPixelTraits::NumComponents "
                   << ConvertPixelTraits::GetNumberOfComponents()
                   << " m_VideoIO->NumComponents "
                   << m_VideoIO->GetNumberOfComponents() );
    this->m_PixelConversionNeeded = true;
    }
  else
    {
    this->m_PixelConversionNeeded = false;
    }

  // Get image buffer size. Always allocate the size reported by the VideoIO.
  // Any conversion that needs to be done will happen when output is created
  size_t bufferSize = this->m_VideoIO->GetImageSizeInBytes();

  // Allocate the buffers
  for (unsigned int i = 0; i < this->GetNumberOfBuffers(); ++i)
    {
    m_BufferArray[i]->Reserve(bufferSize);
    m_BufferArray[i]->ContainerManageMemoryOn();
    this->m_BufferValidFlags[i] = false;
    }
  this->m_Allocated = true;

  // Set Modified time
  this->Modified();
}


//
// Deallocate
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
void
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::Deallocate()
{
  if (this->m_Allocated)
    {
    for (unsigned int i = 0; i < this->GetNumberOfBuffers(); ++i)
      {
      m_BufferArray[i]->Reserve(0);
      m_BufferArray[i]->Squeeze();
      this->m_BufferValidFlags[i] = false;
      }
    }
  this->m_Allocated = false;
}


//
// MoveHead
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
void
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::MoveHead( int offset )
{
  // Compute the new Head index
  this->m_HeadIndex = this->GetOffsetBufferIndex(offset);

  // Update the pointer
  this->m_Head = this->m_BufferArray[this->m_HeadIndex];

  // Mark as modified
  this->Modified();
}


//
// MoveHeadForward
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
void
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::MoveHeadForward()
{
  this->MoveHead(1);
}


//
// MoveHeadBackward
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
void
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::MoveHeadBackward()
{
  this->MoveHead(-1);
}


//
// BufferIsValid
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
bool
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::BufferIsValid(int offset)
{
  unsigned int bufferIndex = this->GetOffsetBufferIndex(offset);
  return this->m_BufferValidFlags[bufferIndex];
}

//
// BufferNextImage
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
void
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::BufferNextImage()
{
  // WARNING: This assumes that buffers have been allocated. It will SEGFAULT if not
  this->m_VideoIO->Read(reinterpret_cast<void*>(this->m_Head->GetBufferPointer()));
  this->m_BufferValidFlags[this->m_HeadIndex] = true;

  // Mark as modified
  this->Modified();
}


//
// GetBufferedImage
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
typename RingBufferImageSet<TPixel, VImageDimension, VNumberOfBuffers>::ImageType::Pointer
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::GetBufferedImage( int offset )
{
  // Get the right buffer
  unsigned int bufferIndex = this->GetOffsetBufferIndex(offset);

  // Set up an image to return
  typename ImageType::Pointer output = ImageType::New();

  // Set up the information
  this->PrepareOutputImage(output);

  // Now fill it
  this->GetBufferedImage(bufferIndex, output);

  // Return the resulting image
  return output;

}


//
// GetBufferedImage
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
void
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::GetBufferedImage( int offset,
  typename RingBufferImageSet<TPixel, VImageDimension, VNumberOfBuffers>::ImageType* image )
{
  // Get the right buffer
  unsigned int bufferIndex = this->GetOffsetBufferIndex(offset);

  // Allocate the data
  image->Allocate();

  // Copy the data
  if (this->m_PixelConversionNeeded)
    {
    this->DoConvertBuffer(bufferIndex, image);
    }

  // If pixel types are the same, copy the data over directly. This could be
  // done by just giving the output image the internal buffer and allocating a
  // new one, but then future attempts to get this same buffered image would
  // fail, so we copy every time.
  else
    {
    std::copy(reinterpret_cast< const PixelType* >(
                this->m_BufferArray[bufferIndex]->GetImportPointer()),
              reinterpret_cast< const PixelType* >(
                this->m_BufferArray[bufferIndex]->GetImportPointer())
                + image->GetBufferedRegion().GetNumberOfPixels(),
              image->GetPixelContainer()->GetBufferPointer() );
    }
}


//
// SetVideoIO
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
void
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::SetVideoIO(VideoIOBase* videoIO)
{
  this->m_VideoIO = videoIO;
}


//
// GenerateOutputInformation
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
void
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::GenerateOutputInformation()
{
  // First, check to see if we need to use the source to get set up
  if (this->m_Source != 0)
    {
    this->m_Source->UpdateOutputData(this->GetOutput());
    }

  this->PrepareOutputImage(this->GetOutput());
}


//
// UpdateOutputData
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
void
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::UpdateOutputData( DataObject *itkNotUsed(output) )
{
  // Generate the data
  this->GenerateData();

}


//
// ConnectSource
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
bool
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::ConnectSource(ProcessObject *arg, unsigned long idx) const
{
  if ( m_Source != arg || m_SourceOutputIndex != idx )
    {
    itkDebugMacro("connecting source  " << arg
                                        << ", source output index " << idx);

    m_Source = arg;
    m_SourceOutputIndex = idx;
    this->Modified();
    return true;
    }
  else
    {
    itkDebugMacro("could not connect source  " << arg
                                               << ", source output index " << idx);

    return false;
    }
}


//
// DisconnectSource
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
bool
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::DisconnectSource(ProcessObject *arg, unsigned long idx) const
{
  if ( m_Source == arg && m_SourceOutputIndex == idx )
    {
    itkDebugMacro("disconnecting source  " << arg
                                           << ", source output index " << idx);

    m_Source = 0;
    m_SourceOutputIndex = 0;
    this->Modified();
    return true;
    }
  else
    {
    itkDebugMacro("could not disconnect source  " << arg
                                                  << ", source output index " << idx);
    return false;
    }
}


//-PROTECTED METHODS-----------------------------------------------------------

//
// PrepareOutputImage
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
void
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::PrepareOutputImage(
  typename RingBufferImageSet<TPixel, VImageDimension, VNumberOfBuffers>::ImageType* output )
{
  
  // Set spacing, origin, and direction
  typename ImageType::PointType origin;
  typename ImageType::SpacingType spacing;
  typename ImageType::DirectionType direction;

  // Set up largest possible region
  typename ImageType::RegionType region;
  typename ImageType::SizeType size;
  typename ImageType::IndexType start;
  for (unsigned int i = 0; i < this->GetImageDimension(); ++i)
    {
    size[i] = this->m_VideoIO->GetDimensions(i);
    origin[i] = this->m_VideoIO->GetOrigin(i);
    spacing[i] = this->m_VideoIO->GetSpacing(i);
    for (unsigned int j = 0; j < ImageType::ImageDimension; ++j)
      {
      direction[j][i] = this->m_VideoIO->GetDirection(i)[j];
      }
    }
  start.Fill(0);
  region.SetSize(size);
  region.SetIndex(start);
  output->SetLargestPossibleRegion(region);
  output->SetBufferedRegion(region);
  output->SetRequestedRegion(region);

  output->SetSpacing(spacing);
  output->SetOrigin(origin);
  output->SetDirection(direction);
}


//
// GenerateData
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
void
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::GenerateData()
{
  // Buffer the next image from the frame
  this->BufferNextImage();

  // Set output
  this->GetBufferedImage(0, this->GetOutput());

  // Mark modified time
  this->GetOutput()->DataHasBeenGenerated();
  this->Modified();
}


//
// GetOffsetBufferIndex
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
unsigned int
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::GetOffsetBufferIndex(int offset)
{
  int pos = (int)this->m_HeadIndex + offset;
  int moddedPos = std::abs(pos) % this->GetNumberOfBuffers();
  if (pos < 0)
    {
    moddedPos = -moddedPos + this->GetNumberOfBuffers();
    }
  return moddedPos;
}


//
// DoConvertBuffer (much borrowed from itkImageFileReader)
//
template< class TPixel,
          unsigned int VImageDimension,
          unsigned int VNumberOfBuffers >
void
RingBufferImageSet< TPixel, VImageDimension, VNumberOfBuffers >
::DoConvertBuffer(unsigned int bufferIndex, 
  RingBufferImageSet<TPixel, VImageDimension, VNumberOfBuffers>::ImageType* output)
{
  PixelType* outputData = output->GetPixelContainer()->GetBufferPointer();
  void* inputData = static_cast<void*>(this->m_BufferArray[bufferIndex]->GetBufferPointer());
  unsigned int numberOfPixels = output->GetPixelContainer()->Size();
  bool isVectorImage(strcmp(output->GetNameOfClass(),
                            "VectorImage") == 0);
#define ITK_CONVERT_BUFFER_IF_BLOCK(_CType,type)                        \
  else if(m_VideoIO->GetComponentType() == _CType)                      \
    {                                                                   \
    if (isVectorImage)                                                  \
      {                                                                 \
      ConvertPixelBuffer<type,                                          \
                         PixelType,                                     \
                         ConvertPixelTraits                             \
                         >                                              \
        ::ConvertVectorImage(static_cast< type * >( inputData ),        \
                             m_VideoIO->GetNumberOfComponents(),        \
                             outputData,                                \
                             numberOfPixels);                           \
      }                                                                 \
    else                                                                \
      {                                                                 \
      ConvertPixelBuffer<type,                                          \
                         PixelType,                                     \
                         ConvertPixelTraits                             \
                         >                                              \
        ::Convert(static_cast< type * >( inputData ),                   \
                  m_VideoIO->GetNumberOfComponents(),                   \
                  outputData,                                           \
                  numberOfPixels);                                      \
      }                                                                 \
    }

  if(0) {}
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::UCHAR,unsigned char)
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::CHAR,char)
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::USHORT,unsigned short)
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::SHORT,short)
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::UINT,unsigned int)
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::INT,int)
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::ULONG,unsigned long)
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::LONG,long)
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::FLOAT,float)
  ITK_CONVERT_BUFFER_IF_BLOCK(ImageIOBase::DOUBLE,double)
  else
    {
#define TYPENAME(x)                                     \
    m_VideoIO->GetComponentTypeAsString                 \
      (ImageIOBase::MapPixelType<x>::CType)

    ExceptionObject e(__FILE__, __LINE__);
    std::ostringstream       msg;
    msg << "Couldn't convert component type: "
        << std::endl << "    "
        << m_VideoIO->GetComponentTypeAsString( m_VideoIO->GetComponentType() )
        << std::endl << "to one of: "
        << std::endl << "    " << TYPENAME( unsigned char )
        << std::endl << "    " << TYPENAME( char )
        << std::endl << "    " << TYPENAME( unsigned short )
        << std::endl << "    " << TYPENAME( short )
        << std::endl << "    " << TYPENAME( unsigned int )
        << std::endl << "    " << TYPENAME( int )
        << std::endl << "    " << TYPENAME( unsigned long )
        << std::endl << "    " << TYPENAME( long )
        << std::endl << "    " << TYPENAME( float )
        << std::endl << "    " << TYPENAME( double )
        << std::endl;
    e.SetDescription( msg.str().c_str() );
    e.SetLocation(ITK_LOCATION);
    throw e;
    return;
    }
#undef ITK_CONVERT_BUFFER_IF_BLOCK

}



} // end namespace itk

#endif
