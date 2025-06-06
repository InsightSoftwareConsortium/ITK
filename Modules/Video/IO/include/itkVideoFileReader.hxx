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

#ifndef itkVideoFileReader_hxx
#define itkVideoFileReader_hxx

#include "itkConvertPixelBuffer.h"
#include "itkMakeUniqueForOverwrite.h"


namespace itk
{

template <typename TOutputVideoStream>
VideoFileReader<TOutputVideoStream>::VideoFileReader()
{
  // Initialize members
  m_FileName = "";
  m_VideoIO = nullptr;
  m_PixelConversionNeeded = false;
  m_IFrameSafe = true;

  // TemporalProcessObject inherited members
  this->SetUnitOutputNumberOfFrames(1);
  this->SetFrameSkipPerOutput(1);
  this->SetInputStencilCurrentFrameIndex(0);
}

template <typename TOutputVideoStream>
void
VideoFileReader<TOutputVideoStream>::UpdateOutputInformation()
{
  //
  // Use the VideoIOFactory to generate a VideoIOBase if needed
  //
  if (m_VideoIO.IsNull())
  {
    this->InitializeVideoIO();
  }

  //
  // Check that the desired dimension matches that read from the file
  //
  if (m_VideoIO->GetNumberOfDimensions() != FrameDimension)
  {
    itkExceptionMacro("Output dimension doesn't match dimension of read "
                      << "data. Expected " << FrameDimension << " but the IO "
                      << "method has " << m_VideoIO->GetNumberOfDimensions() << '.');
  }

  //
  // Set up the largest possible temporal region for the output
  //
  TemporalRegion largestPossibleTemporalRegion;
  largestPossibleTemporalRegion.SetFrameStart(0);
  if (m_IFrameSafe)
  {
    largestPossibleTemporalRegion.SetFrameDuration(m_VideoIO->GetLastIFrame() + 1);
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
  SizeType      size;
  PointType     origin;
  SpacingType   spacing;
  DirectionType direction;
  for (unsigned int i = 0; i < FrameDimension; ++i)
  {
    size[i] = m_VideoIO->GetDimensions(i);
    origin[i] = m_VideoIO->GetOrigin(i);
    spacing[i] = m_VideoIO->GetSpacing(i);
    std::vector<double> directionInI = m_VideoIO->GetDirection(i);
    for (unsigned int j = 0; j < FrameDimension; ++j)
    {
      direction[j][i] = directionInI[j];
    }
  }
  const RegionType region(size);

  const VideoStreamPointer output = this->GetOutput();

  output->SetAllLargestPossibleSpatialRegions(region);
  output->SetAllBufferedSpatialRegions(region);
  output->SetAllRequestedSpatialRegions(region);

  output->SetAllFramesSpacing(spacing);
  output->SetAllFramesOrigin(origin);
  output->SetAllFramesDirection(direction);
}

template <typename TOutputVideoStream>
auto
VideoFileReader<TOutputVideoStream>::GetCurrentPositionFrame() -> FrameOffsetType
{
  if (m_VideoIO.IsNull())
  {
    this->InitializeVideoIO();
  }
  return m_VideoIO->GetCurrentFrame();
}

template <typename TOutputVideoStream>
auto
VideoFileReader<TOutputVideoStream>::GetCurrentPositionRatio() -> TemporalRatioType
{
  if (m_VideoIO.IsNull())
  {
    this->InitializeVideoIO();
  }
  return m_VideoIO->GetRatio();
}

template <typename TOutputVideoStream>
auto
VideoFileReader<TOutputVideoStream>::GetCurrentPositionMSec() -> TemporalRatioType
{
  if (m_VideoIO.IsNull())
  {
    this->InitializeVideoIO();
  }
  return m_VideoIO->GetPositionInMSec();
}

template <typename TOutputVideoStream>
auto
VideoFileReader<TOutputVideoStream>::GetNumberOfFrames() -> FrameOffsetType
{
  if (m_VideoIO.IsNull())
  {
    this->InitializeVideoIO();
  }
  return m_VideoIO->GetFrameTotal();
}

template <typename TOutputVideoStream>
auto
VideoFileReader<TOutputVideoStream>::GetFramesPerSecond() -> TemporalRatioType
{
  if (m_VideoIO.IsNull())
  {
    this->InitializeVideoIO();
  }
  return m_VideoIO->GetFramesPerSecond();
}

template <typename TOutputVideoStream>
void
VideoFileReader<TOutputVideoStream>::InitializeVideoIO()
{
  m_VideoIO = itk::VideoIOFactory::CreateVideoIO(itk::VideoIOFactory::IOModeEnum::ReadFileMode, m_FileName.c_str());
  m_VideoIO->SetFileName(m_FileName.c_str());
  m_VideoIO->ReadImageInformation();

  // Make sure the input video has the same number of dimensions as the desired
  // output
  //
  // Note: This may be changed with the implementation of the Image
  //       Interpretation Layer
  if (m_VideoIO->GetNumberOfDimensions() != FrameType::ImageDimension)
  {
    itkExceptionMacro("Cannot convert " << m_VideoIO->GetNumberOfDimensions()
                                        << "D "
                                           "image set to "
                                        << FrameType::ImageDimension << 'D');
  }

  // See if a buffer conversion is needed
  const IOComponentEnum ioType = ImageIOBase::MapPixelType<typename ConvertPixelTraits::ComponentType>::CType;
  if (m_VideoIO->GetComponentType() != ioType ||
      m_VideoIO->GetNumberOfComponents() != ConvertPixelTraits::GetNumberOfComponents())
  {
    // the pixel types don't match so a type conversion needs to be
    // performed
    itkDebugMacro("Buffer conversion required from: "
                  << ImageIOBase::GetComponentTypeAsString(m_VideoIO->GetComponentType())
                  << " to: " << ImageIOBase::GetComponentTypeAsString(ioType) << " ConvertPixelTraits::NumComponents "
                  << ConvertPixelTraits::GetNumberOfComponents() << " m_VideoIO->NumComponents "
                  << m_VideoIO->GetNumberOfComponents());
    m_PixelConversionNeeded = true;
  }
  else
  {
    m_PixelConversionNeeded = false;
  }
}

template <typename TOutputVideoStream>
void
VideoFileReader<TOutputVideoStream>::TemporalStreamingGenerateData()
{
  // Allocate the output frames
  this->AllocateOutputs();

  // Get the frame number for the frame we're reading
  typename VideoStreamType::Pointer            output;
  typename VideoStreamType::TemporalRegionType requestedTemporalRegion;
  output = this->GetOutput();
  requestedTemporalRegion = output->GetRequestedTemporalRegion();
  const FrameOffsetType frameNum = requestedTemporalRegion.GetFrameStart();

  // Figure out if we need to skip frames
  const FrameOffsetType currentIOFrame = m_VideoIO->GetCurrentFrame();
  if (frameNum != currentIOFrame)
  {
    m_VideoIO->SetNextFrameToRead(frameNum);
  }

  // Read a single frame
  if (this->m_PixelConversionNeeded)
  {
    // Set up temporary buffer for reading
    const size_t bufferSize = m_VideoIO->GetImageSizeInBytes();
    const auto   loadBuffer = make_unique_for_overwrite<char[]>(bufferSize);

    // Read into a temporary buffer
    this->m_VideoIO->Read(static_cast<void *>(loadBuffer.get()));

    // Convert the buffer into the output buffer location
    this->DoConvertBuffer(static_cast<void *>(loadBuffer.get()), frameNum);
  }
  else
  {
    FrameType * frame = this->GetOutput()->GetFrame(frameNum);
    m_VideoIO->Read(reinterpret_cast<void *>(frame->GetBufferPointer()));
  }

  // Mark ourselves modified
  this->Modified();
}

template <typename TOutputVideoStream>
void
VideoFileReader<TOutputVideoStream>::DoConvertBuffer(const void * inputData, FrameOffsetType frameNumber)
{
  PixelType *        outputData = this->GetOutput()->GetFrame(frameNumber)->GetPixelContainer()->GetBufferPointer();
  const unsigned int numberOfPixels = this->GetOutput()->GetFrame(frameNumber)->GetPixelContainer()->Size();
  const bool isVectorImage(strcmp(this->GetOutput()->GetFrame(frameNumber)->GetNameOfClass(), "VectorImage") == 0);
#define ITK_CONVERT_BUFFER_IF_BLOCK(_CType, type)                                                              \
  else if (m_VideoIO->GetComponentType() == _CType)                                                            \
  {                                                                                                            \
    if (isVectorImage)                                                                                         \
    {                                                                                                          \
      ConvertPixelBuffer<type, PixelType, ConvertPixelTraits>::ConvertVectorImage(                             \
        static_cast<const type *>(inputData), m_VideoIO->GetNumberOfComponents(), outputData, numberOfPixels); \
    }                                                                                                          \
    else                                                                                                       \
    {                                                                                                          \
      ConvertPixelBuffer<type, PixelType, ConvertPixelTraits>::Convert(                                        \
        static_cast<const type *>(inputData), m_VideoIO->GetNumberOfComponents(), outputData, numberOfPixels); \
    }                                                                                                          \
  }

  if (false)
  {
  }
  ITK_CONVERT_BUFFER_IF_BLOCK(IOComponentEnum::UCHAR, unsigned char)
  ITK_CONVERT_BUFFER_IF_BLOCK(IOComponentEnum::CHAR, char)
  ITK_CONVERT_BUFFER_IF_BLOCK(IOComponentEnum::USHORT, unsigned short)
  ITK_CONVERT_BUFFER_IF_BLOCK(IOComponentEnum::SHORT, short)
  ITK_CONVERT_BUFFER_IF_BLOCK(IOComponentEnum::UINT, unsigned int)
  ITK_CONVERT_BUFFER_IF_BLOCK(IOComponentEnum::INT, int)
  ITK_CONVERT_BUFFER_IF_BLOCK(IOComponentEnum::ULONG, unsigned long)
  ITK_CONVERT_BUFFER_IF_BLOCK(IOComponentEnum::LONG, long)
  ITK_CONVERT_BUFFER_IF_BLOCK(IOComponentEnum::ULONGLONG, unsigned long long)
  ITK_CONVERT_BUFFER_IF_BLOCK(IOComponentEnum::LONGLONG, long long)
  ITK_CONVERT_BUFFER_IF_BLOCK(IOComponentEnum::FLOAT, float)
  ITK_CONVERT_BUFFER_IF_BLOCK(IOComponentEnum::DOUBLE, double)
  else
  {
#define TYPENAME_VideoFileReader(x) ImageIOBase::GetComponentTypeAsString(ImageIOBase::MapPixelType<x>::CType)

    ExceptionObject    e(__FILE__, __LINE__);
    std::ostringstream msg;
    msg << "Couldn't convert component type: " << std::endl
        << "    " << ImageIOBase::GetComponentTypeAsString(m_VideoIO->GetComponentType()) << std::endl
        << "to one of: " << std::endl
        << "    " << TYPENAME_VideoFileReader(unsigned char) << std::endl
        << "    " << TYPENAME_VideoFileReader(char) << std::endl
        << "    " << TYPENAME_VideoFileReader(unsigned short) << std::endl
        << "    " << TYPENAME_VideoFileReader(short) << std::endl
        << "    " << TYPENAME_VideoFileReader(unsigned int) << std::endl
        << "    " << TYPENAME_VideoFileReader(int) << std::endl
        << "    " << TYPENAME_VideoFileReader(FrameOffsetType) << std::endl
        << "    " << TYPENAME_VideoFileReader(long) << std::endl
        << "    " << TYPENAME_VideoFileReader(float) << std::endl
        << "    " << TYPENAME_VideoFileReader(double) << std::endl;
    e.SetDescription(msg.str().c_str());
    e.SetLocation(ITK_LOCATION);
    throw e;
  }
#undef ITK_CONVERT_BUFFER_IF_BLOCK
}

template <typename TOutputVideoStream>
void
VideoFileReader<TOutputVideoStream>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << this->m_FileName << std::endl;
  itkPrintSelfObjectMacro(VideoIO);
}

} // end namespace itk

#endif
