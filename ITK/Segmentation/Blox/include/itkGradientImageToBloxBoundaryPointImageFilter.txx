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
#ifndef __itkGradientImageToBloxBoundaryPointImageFilter_txx
#define __itkGradientImageToBloxBoundaryPointImageFilter_txx

#include "itkProgressReporter.h"
#include "itkGradientImageToBloxBoundaryPointImageFilter.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{
template< typename TInputImage >
GradientImageToBloxBoundaryPointImageFilter< TInputImage >
::GradientImageToBloxBoundaryPointImageFilter()
{
  itkDebugMacro(<< "GradientImageToBloxBoundaryPointImageFilter::GradientImageToBloxBoundaryPointImageFilter() called");

  // The default threshold level is 128 (for no particular reason)
  m_Threshold = 128;

  for ( unsigned int j = 0; j < NDimensions; j++ )
    {
    m_BloxResolution[j] = 10.0;
    }
}

template< typename TInputImage >
void
GradientImageToBloxBoundaryPointImageFilter< TInputImage >
::SetBloxResolution(float bloxResolution)
{
  unsigned int j = 0;

  for ( j = 0; j < NDimensions; j++ )
    {
    if ( bloxResolution != m_BloxResolution[j] ) { break; }
    }
  if ( j < NDimensions )
    {
    this->Modified();
    for ( j = 0; j < NDimensions; j++ )
      {
      m_BloxResolution[j] = bloxResolution;
      if ( m_BloxResolution[j] < 1 )
        {
        m_BloxResolution[j] = 1;
        }
      }
    }
}

template< typename TInputImage >
void
GradientImageToBloxBoundaryPointImageFilter< TInputImage >
::GenerateInputRequestedRegion()
{
  itkDebugMacro(<< "GradientImageToBloxBoundaryPointImageFilter::GenerateInputRequestedRegion() called");

  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  InputImagePointer inputPtr =
    const_cast< TInputImage * >( this->GetInput() );
  OutputImagePointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // we need to compute the input requested region (size and start index)
  const typename TOutputImage::SizeType & outputRequestedRegionSize =
    outputPtr->GetRequestedRegion().GetSize();
  const typename TOutputImage::IndexType & outputRequestedRegionStartIndex =
    outputPtr->GetRequestedRegion().GetIndex();

  SizeType  inputRequestedRegionSize;
  IndexType inputRequestedRegionStartIndex;

  for ( unsigned int i = 0; i < TInputImage::ImageDimension; i++ )
    {
    inputRequestedRegionSize[i] =  static_cast< SizeValueType >(
      outputRequestedRegionSize[i] * m_BloxResolution[i] );
    inputRequestedRegionStartIndex[i] =  static_cast< IndexValueType >(
      outputRequestedRegionStartIndex[i] * m_BloxResolution[i] );
    }

  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion.SetSize(inputRequestedRegionSize);
  inputRequestedRegion.SetIndex(inputRequestedRegionStartIndex);

  inputPtr->SetRequestedRegion(inputRequestedRegion);
}

template< typename TInputImage >
void
GradientImageToBloxBoundaryPointImageFilter< TInputImage >
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  InputImageConstPointer inputPtr  = this->GetInput();
  OutputImagePointer     outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // we need to compute the output spacing, the output image size, and the
  // output image start index
  const typename TInputImage::SpacingType & inputSpacing = inputPtr->GetSpacing();
  const typename TInputImage::SizeType &   inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TInputImage::IndexType &  inputStartIndex = inputPtr->GetLargestPossibleRegion().GetIndex();

  typename TOutputImage::SpacingType outputSpacing;

  SizeType  outputSize;
  IndexType outputStartIndex;

  for ( unsigned int i = 0; i < TOutputImage::ImageDimension; i++ )
    {
    outputSpacing[i] = inputSpacing[i] * m_BloxResolution[i];

    outputSize[i] = static_cast< SizeValueType >(
      vcl_floor(static_cast< float >( inputSize[i] ) / m_BloxResolution[i]) );

    if ( outputSize[i] < 1 )
      {
      outputSize[i] = 1;
      }

    outputStartIndex[i] = static_cast< IndexValueType >(
      vcl_ceil(static_cast< float >( inputStartIndex[i] ) / m_BloxResolution[i]) );
    }

  outputPtr->SetSpacing(outputSpacing);

  // compute origin offset
  // The physical center's of the input and output should be the same
  ContinuousIndex< double, TOutputImage::ImageDimension > inputCenterIndex;
  ContinuousIndex< double, TOutputImage::ImageDimension > outputCenterIndex;
  for ( unsigned int j = 0; j < TOutputImage::ImageDimension; j++ )
    {
    inputCenterIndex[j] = inputStartIndex[j] + ( inputSize[j] - 1 ) / 2.0;
    outputCenterIndex[j] = outputStartIndex[j] + ( outputSize[j] - 1 ) / 2.0;
    }

  typename TOutputImage::PointType inputCenterPoint;
  typename TOutputImage::PointType outputCenterPoint;
  inputPtr->TransformContinuousIndexToPhysicalPoint(inputCenterIndex, inputCenterPoint);
  outputPtr->TransformContinuousIndexToPhysicalPoint(outputCenterIndex, outputCenterPoint);

  typename TOutputImage::PointType outputOrigin = outputPtr->GetOrigin();
  outputOrigin = outputOrigin + ( inputCenterPoint - outputCenterPoint );
  outputPtr->SetOrigin(outputOrigin);

  typename TOutputImage::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize(outputSize);
  outputLargestPossibleRegion.SetIndex(outputStartIndex);

  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);
}

template< typename TInputImage >
void
GradientImageToBloxBoundaryPointImageFilter< TInputImage >
::GenerateData()
{
  itkDebugMacro(<< "GradientImageToBloxBoundaryPointImageFilter::GenerateData() called");

  // Get the input and output pointers
  InputImageConstPointer inputPtr  = this->GetInput(0);
  OutputImagePointer     outputPtr = this->GetOutput(0);

  // Allocate the output
  typename TOutputImage::RegionType outputRequestedRegion = outputPtr->GetRequestedRegion();
  outputPtr->SetBufferedRegion(outputRequestedRegion);
  outputPtr->Allocate();

  typename TInputImage::RegionType inputRequestedRegion = inputPtr->GetRequestedRegion();

  // Create a progress reporter
  ProgressReporter progress( this, 0, inputRequestedRegion.GetNumberOfPixels() );

  // Position to figure out pixel location
  TPositionType inputPosition;

  // Create an iterator to walk the input image
  typedef ImageRegionConstIterator< TInputImage > TInputIterator;

  TInputIterator inputIt = TInputIterator(inputPtr, inputRequestedRegion);

  // Keep track of how many boundary points we found (for debugging)
  SizeValueType numBP = 0;
  SizeValueType numBPadded = 0;

  // Get the index of the pixel
  IndexType bloxIndex;

  for ( inputIt.GoToBegin(); !inputIt.IsAtEnd(); ++inputIt )
    {
    // Figure out the magnitude of the gradient
    double mag = 0;

    for ( unsigned int i = 0; i < NDimensions; i++ )
      {
      const double component = inputIt.Get()[i];
      mag += component * component;
      }

    mag = vcl_sqrt(mag);

    // If the pixel meets threshold requirements, add it to the image
    if ( mag >= m_Threshold )
      {
      numBP++;

      // Convert the index of the input pixel to the physical location of the
      // boundary point in the input image
      inputPtr->TransformIndexToPhysicalPoint(inputIt.GetIndex(), inputPosition);

      // Transform the physical location to a blox index
      outputPtr->TransformPhysicalPointToIndex(inputPosition, bloxIndex);

      // check if it is inside of the output image.
      if ( outputRequestedRegion.IsInside(bloxIndex) )
        {
        // Create a new boundary point item and set its parameters
        BloxBoundaryPointItem< NDimensions > *pItem = new BloxBoundaryPointItem< NDimensions >;
        pItem->SetPhysicalPosition(inputPosition);
        pItem->SetGradient( inputIt.Get() );

        outputPtr->GetPixel(bloxIndex).push_back(pItem);
        }
      numBPadded++;
      }

    progress.CompletedPixel();
    }

  outputPtr->SetNumBoundaryPoints(numBP);

  itkDebugMacro(<< "Finished looking for boundary points\n"
                << "I found " << numBP << " points\n"
                << "I added " << numBPadded << " points\n");
}

template< typename TInputImage >
void
GradientImageToBloxBoundaryPointImageFilter< TInputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Threshold level: " << m_Threshold << std::endl;
  os << indent << "BloxResolution: " << m_BloxResolution << std::endl;
}
} // end namespace

#endif
