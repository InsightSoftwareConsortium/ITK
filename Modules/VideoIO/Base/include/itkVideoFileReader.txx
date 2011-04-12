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
template< class TInputImage >
VideoFileReader< TInputImage >
::VideoFileReader()
{
  // Initialize members
  this->m_FileName = "";
  this->m_Output = ImageType::New();
  this->m_VideoIO = NULL;
  this->m_PixelConversionNeeded = false;
  this->m_OutputAllocated = false;

  // Set up output
  this->SetNthOutput(0, this->m_Output);

  // Add modification timestamp
  this->Modified();
}


//
// Destructor
//
template< class TInputImage >
VideoFileReader< TInputImage >
::~VideoFileReader()
{}


//
// PrintSelf
//
template< class TInputImage >
void
VideoFileReader< TInputImage >
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << this->m_FileName << std::endl;
  if (!this->m_Output.IsNull())
    {
    std::cout << indent << "OutputImage:" << std::endl;
    this->m_Output->Print(os, indent.GetNextIndent());
    }
}

//-PUBLIC METHODS--------------------------------------------------------------

//
// GenerateOutputInformation
//
template< class TInputImage >
void
VideoFileReader< TInputImage >
::GenerateOutputInformation()
{
  //
  // Use the VideoIOFactory to generate a VideoIOBase if needed
  //
  if (this->m_VideoIO.IsNull())
    {
    this->InitializeVideoIO();
    }

  //
  // Create a new output if it doesn't already exist
  //
  if (!this->m_OutputAllocated)
    {
    // Set spacing, origin, and direction
    typename ImageType::PointType origin;
    typename ImageType::SpacingType spacing;
    typename ImageType::DirectionType direction;

    // Set up largest possible region
    typename ImageType::RegionType region;
    typename ImageType::SizeType size;
    typename ImageType::IndexType start;
    for (unsigned int i = 0; i < ImageType::ImageDimension; ++i)
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
    this->m_Output->SetLargestPossibleRegion(region);
    this->m_Output->SetBufferedRegion(region);
    this->m_Output->SetRequestedRegion(region);

    this->m_Output->SetSpacing(spacing);
    this->m_Output->SetOrigin(origin);
    this->m_Output->SetDirection(direction);

    // Allocate the output
    this->m_Output->Allocate();

    // Don't need to do this again
    this->m_OutputAllocated = true;
    }
}


//
// UpdateOutputData
//
template< class TInputImage >
void
VideoFileReader< TInputImage >
::UpdateOutputData(DataObject*)
{

  // Read the next frame from the videoIO and do conversion if necessary
  if (this->m_PixelConversionNeeded)
    {
    // Set up temporary buffer for reading
    size_t bufferSize = this->m_VideoIO->GetImageSizeInBytes();
    char* loadBuffer = new char[bufferSize];

    // Read into a temporary buffer
    this->m_VideoIO->Read(static_cast<void*>(loadBuffer));

    // Convert the buffer into the output buffer location
    this->DoConvertBuffer(static_cast<void*>(loadBuffer));
    }
  else
    {
    this->m_VideoIO->Read(reinterpret_cast<void*>(
      this->m_Output->GetPixelContainer()->GetBufferPointer()));
    }

  // Mark ourselves modified
  this->Modified();
}


//
// GetOutput
//
template< class TInputImage >
typename TInputImage::Pointer
VideoFileReader< TInputImage >
::GetOutput()
{
  return this->m_Output;
}




//
// GetCurrentPositionFrame
//
template< class TInputImage >
unsigned long
VideoFileReader< TInputImage >
::GetCurrentPositionFrame()
{
  if(this->m_VideoIO.IsNull())
    {
    this->InitializeVideoIO();
    }
  return this->m_VideoIO->GetCurrentFrame();
}


//
// GetCurrentPositionRatio
//
template< class TInputImage >
double
VideoFileReader< TInputImage >
::GetCurrentPositionRatio()
{
  if(this->m_VideoIO.IsNull())
    {
    this->InitializeVideoIO();
    }
  return this->m_VideoIO->GetRatio();
}


//
// GetCurrentPositionMSec
//
template< class TInputImage >
double
VideoFileReader< TInputImage >
::GetCurrentPositionMSec()
{
  if(this->m_VideoIO.IsNull())
    {
    this->InitializeVideoIO();
    }
  return this->m_VideoIO->GetPositionInMSec();
}


//
// GetNumberOfFrames
//
template< class TInputImage >
unsigned long
VideoFileReader< TInputImage >
::GetNumberOfFrames()
{
  if(this->m_VideoIO.IsNull())
    {
    this->InitializeVideoIO();
    }
  return this->m_VideoIO->GetFrameTotal();
}


//
// GetFpS
//
template< class TInputImage >
double
VideoFileReader< TInputImage >
::GetFpS()
{
  if(this->m_VideoIO.IsNull())
    {
    this->InitializeVideoIO();
    }
  return this->m_VideoIO->GetFpS();
}





//-PROTECTED METHODS-----------------------------------------------------------

//
// InitializeVideoIO
//
template< class TImageType >
void
VideoFileReader< TImageType >
::InitializeVideoIO()
{
  this->m_VideoIO = itk::VideoIOFactory::CreateVideoIO(
                                itk::VideoIOFactory::ReadFileMode,
                                this->m_FileName.c_str());
  this->m_VideoIO->SetFileName(this->m_FileName.c_str());
  this->m_VideoIO->ReadImageInformation();

  // Make sure the input video has the same number of dimensions as the desired
  // output
  //
  // Note: This may be changed with the implementation of the Image
  //       Interpretation Layer
  if (this->m_VideoIO->GetNumberOfDimensions() != ImageType::ImageDimension)
    {
    itkExceptionMacro("Cannot convert " << this->m_VideoIO->GetNumberOfDimensions() << "D "
      "image set to " << ImageType::ImageDimension << "D");
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
}



//
// DoConvertBuffer (much borrowed from itkImageFileReader)
//
template< class TImageType >
void
VideoFileReader< TImageType >
::DoConvertBuffer(void* inputData)
{
  PixelType* outputData = this->m_Output->GetPixelContainer()->GetBufferPointer();
  unsigned int numberOfPixels = this->m_Output->GetPixelContainer()->Size();
  bool isVectorImage(strcmp(this->m_Output->GetNameOfClass(),
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
