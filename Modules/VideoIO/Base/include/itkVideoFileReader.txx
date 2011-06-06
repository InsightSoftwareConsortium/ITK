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

#ifndef __itkVideoFileReader_txx
#define __itkVideoFileReader_txx

#include "itkConvertPixelBuffer.h"

namespace itk
{

//-CONSTRUCTOR DESTRUCTOR PRINT------------------------------------------------

//
// Constructor
//
template< class TOutputVideoStream >
VideoFileReader< TOutputVideoStream >
::VideoFileReader()
{
  // Initialize members
  m_FileName = "";
  m_VideoIO = NULL;
  m_PixelConversionNeeded = false;
  m_IFrameSafe = true;

  // TemporalProcessObject inherited members
  this->TemporalProcessObject::m_UnitOutputNumberOfFrames = 1;
  this->TemporalProcessObject::m_FrameSkipPerOutput = 1;
  this->TemporalProcessObject::m_InputStencilCurrentFrameIndex = 0;
}


//
// Destructor
//
template< class TOutputVideoStream >
VideoFileReader< TOutputVideoStream >
::~VideoFileReader()
{}


//
// PrintSelf
//
template< class TOutputVideoStream >
void
VideoFileReader< TOutputVideoStream >
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << m_FileName << std::endl;
  if (m_VideoIO)
    {
    std::cout << indent << "VideoIO:" << std::endl;
    m_VideoIO->Print(os, indent.GetNextIndent());
    }
}

//-PUBLIC METHODS--------------------------------------------------------------

//
// GenerateOutputInformation
//
template< class TOutputVideoStream >
void
VideoFileReader< TOutputVideoStream >
::UpdateOutputInformation()
{
  //
  // Use the VideoIOFactory to generate a VideoIOBase if needed
  //
  if (m_VideoIO.IsNull())
    {
    this->InitializeVideoIO();
    }

  //
  // Check that the desired dimension mateches that read from the file
  //
  if (m_VideoIO->GetNumberOfDimensions() != FrameType::ImageDimension)
    {
    itkExceptionMacro("Output dimension doesn't match dimension of read data");
    }

  //
  // Set up the largest possible temporal region for the output
  //
  TemporalRegion largestPossibleTemporalRegion;
  largestPossibleTemporalRegion.SetFrameStart(0);
  if (m_IFrameSafe)
    {
    largestPossibleTemporalRegion.SetFrameDuration(m_VideoIO->GetLastIFrame()+1);
    }
  else
    {
    largestPossibleTemporalRegion.SetFrameDuration(m_VideoIO->GetFrameTotal());
    }
  this->GetOutput()->SetLargestPossibleTemporalRegion(largestPossibleTemporalRegion);

  //
  // Set up the information for the output frames
  //

  // Set up largest possible spatial region
  typename FrameType::RegionType region;
  typename FrameType::SizeType size;
  typename FrameType::IndexType start;
  typename FrameType::PointType origin;
  typename FrameType::SpacingType spacing;
  typename FrameType::DirectionType direction;
  for (unsigned int i = 0; i < FrameType::ImageDimension; ++i)
    {
    size[i] = m_VideoIO->GetDimensions(i);
    origin[i] = m_VideoIO->GetOrigin(i);
    spacing[i] = m_VideoIO->GetSpacing(i);
    for (unsigned int j = 0; j < FrameType::ImageDimension; ++j)
      {
      direction[j][i] = m_VideoIO->GetDirection(i)[j];
      }
    }
  start.Fill(0);
  region.SetSize(size);
  region.SetIndex(start);
  this->GetOutput()->SetAllLargestPossibleSpatialRegions(region);
  this->GetOutput()->SetAllBufferedSpatialRegions(region);
  this->GetOutput()->SetAllRequestedSpatialRegions(region);

  this->GetOutput()->SetAllFramesSpacing(spacing);
  this->GetOutput()->SetAllFramesOrigin(origin);
  this->GetOutput()->SetAllFramesDirection(direction);
}

//
// GetCurrentPositionFrame
//
template< class TOutputVideoStream >
unsigned long
VideoFileReader< TOutputVideoStream >
::GetCurrentPositionFrame()
{
  if(m_VideoIO.IsNull())
    {
    this->InitializeVideoIO();
    }
  return m_VideoIO->GetCurrentFrame();
}


//
// GetCurrentPositionRatio
//
template< class TOutputVideoStream >
double
VideoFileReader< TOutputVideoStream >
::GetCurrentPositionRatio()
{
  if(m_VideoIO.IsNull())
    {
    this->InitializeVideoIO();
    }
  return m_VideoIO->GetRatio();
}


//
// GetCurrentPositionMSec
//
template< class TOutputVideoStream >
double
VideoFileReader< TOutputVideoStream >
::GetCurrentPositionMSec()
{
  if(m_VideoIO.IsNull())
    {
    this->InitializeVideoIO();
    }
  return m_VideoIO->GetPositionInMSec();
}


//
// GetNumberOfFrames
//
template< class TOutputVideoStream >
unsigned long
VideoFileReader< TOutputVideoStream >
::GetNumberOfFrames()
{
  if(m_VideoIO.IsNull())
    {
    this->InitializeVideoIO();
    }
  return m_VideoIO->GetFrameTotal();
}


//
// GetFpS
//
template< class TOutputVideoStream >
double
VideoFileReader< TOutputVideoStream >
::GetFpS()
{
  if(m_VideoIO.IsNull())
    {
    this->InitializeVideoIO();
    }
  return m_VideoIO->GetFpS();
}


//-PROTECTED METHODS-----------------------------------------------------------

//
// InitializeVideoIO
//
template< class TOutputVideoStream >
void
VideoFileReader< TOutputVideoStream >
::InitializeVideoIO()
{
  m_VideoIO = itk::VideoIOFactory::CreateVideoIO(
                                itk::VideoIOFactory::ReadFileMode,
                                m_FileName.c_str());
  m_VideoIO->SetFileName(m_FileName.c_str());
  m_VideoIO->ReadImageInformation();

  // Make sure the input video has the same number of dimensions as the desired
  // output
  //
  // Note: This may be changed with the implementation of the Image
  //       Interpretation Layer
  if (m_VideoIO->GetNumberOfDimensions() != FrameType::ImageDimension)
    {
    itkExceptionMacro("Cannot convert " << m_VideoIO->GetNumberOfDimensions() << "D "
      "image set to " << FrameType::ImageDimension << "D");
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
    m_PixelConversionNeeded = true;
    }
  else
    {
    m_PixelConversionNeeded = false;
    }
}


//
// TemporalStreamingGenerateData
//
template< class TOutputVideoStream >
void
VideoFileReader< TOutputVideoStream >
::TemporalStreamingGenerateData()
{
  // Allocate the output frames
  this->AllocateOutputs();

  // Get the frame number for the frame we're reading
  unsigned long frameNum = this->GetOutput()->GetRequestedTemporalRegion().GetFrameStart();

  // Figure out if we need to skip frames
  unsigned long currentIOFrame = m_VideoIO->GetCurrentFrame();
  if (frameNum != currentIOFrame)
    {
    m_VideoIO->SetNextFrameToRead(frameNum);
    }

  // Read a single frame
  if (m_PixelConversionNeeded)
    {
    // Set up temporary buffer for reading
    size_t bufferSize = m_VideoIO->GetImageSizeInBytes();
    char* loadBuffer = new char[bufferSize];

    // Read into a temporary buffer
    m_VideoIO->Read(static_cast<void*>(loadBuffer));

    // Convert the buffer into the output buffer location
    this->DoConvertBuffer(static_cast<void*>(loadBuffer), frameNum);
    }
  else
    {
    FrameType* frame = this->GetOutput()->GetFrame(frameNum);
    m_VideoIO->Read(reinterpret_cast<void*>(frame->GetBufferPointer()));
    }

  // Mark ourselves modified
  this->Modified();
}


//
// DoConvertBuffer (much borrowed from itkImageFileReader)
//
template< class TOutputVideoStream >
void
VideoFileReader< TOutputVideoStream >::
DoConvertBuffer(void* inputData, unsigned long frameNumber)
{
  PixelType* outputData =
    this->GetOutput()->GetFrame(frameNumber)->GetPixelContainer()->GetBufferPointer();
  unsigned int numberOfPixels =
    this->GetOutput()->GetFrame(frameNumber)->GetPixelContainer()->Size();
  bool isVectorImage(strcmp(this->GetOutput()->GetFrame(frameNumber)->GetNameOfClass(),
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
#define TYPENAME_VideoFileReader(x)                                     \
    m_VideoIO->GetComponentTypeAsString                 \
      (ImageIOBase::MapPixelType<x>::CType)

    ExceptionObject e(__FILE__, __LINE__);
    std::ostringstream       msg;
    msg << "Couldn't convert component type: "
        << std::endl << "    "
        << m_VideoIO->GetComponentTypeAsString( m_VideoIO->GetComponentType() )
        << std::endl << "to one of: "
        << std::endl << "    " << TYPENAME_VideoFileReader( unsigned char )
        << std::endl << "    " << TYPENAME_VideoFileReader( char )
        << std::endl << "    " << TYPENAME_VideoFileReader( unsigned short )
        << std::endl << "    " << TYPENAME_VideoFileReader( short )
        << std::endl << "    " << TYPENAME_VideoFileReader( unsigned int )
        << std::endl << "    " << TYPENAME_VideoFileReader( int )
        << std::endl << "    " << TYPENAME_VideoFileReader( unsigned long )
        << std::endl << "    " << TYPENAME_VideoFileReader( long )
        << std::endl << "    " << TYPENAME_VideoFileReader( float )
        << std::endl << "    " << TYPENAME_VideoFileReader( double )
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
