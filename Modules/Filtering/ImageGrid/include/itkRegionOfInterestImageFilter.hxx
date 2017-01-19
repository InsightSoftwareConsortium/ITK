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
#ifndef itkRegionOfInterestImageFilter_hxx
#define itkRegionOfInterestImageFilter_hxx

#include "itkRegionOfInterestImageFilter.h"
#include "itkImageAlgorithm.h"
#include "itkObjectFactory.h"
#include "itkProgressReporter.h"
#include "itkImage.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
RegionOfInterestImageFilter< TInputImage, TOutputImage >
::RegionOfInterestImageFilter()
{}

template< typename TInputImage, typename TOutputImage >
void
RegionOfInterestImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // Get pointer to the input
  typename Superclass::InputImagePointer inputPtr =
    const_cast< TInputImage * >( this->GetInput() );

  if ( inputPtr )
    {
    // Request the region of interest
    inputPtr->SetRequestedRegion(m_RegionOfInterest);
    }
}

template< typename TInputImage, typename TOutputImage >
void
RegionOfInterestImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *output)
{
  // Call the superclass' implementation of this method
  Superclass::EnlargeOutputRequestedRegion(output);

  // Generate everything in the region of interest
  output->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage, typename TOutputImage >
void
RegionOfInterestImageFilter< TInputImage, TOutputImage >
::GenerateOutputInformation()
{
  // Do not call the superclass' implementation of this method since
  // this filter allows the input the output to be of different dimensions

  // Get pointers to the input and output
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput();
  typename Superclass::InputImageConstPointer inputPtr  = this->GetInput();

  if ( !outputPtr || !inputPtr )
    {
    return;
    }

  // Set the output image size to the same value as the region of interest.
  RegionType region;
  IndexType  start;
  start.Fill(0);

  region.SetSize( m_RegionOfInterest.GetSize() );
  region.SetIndex(start);

  // Copy Information without modification.
  outputPtr->CopyInformation(inputPtr);

  // Adjust output region
  outputPtr->SetLargestPossibleRegion(region);

  // Correct origin of the extracted region.
  IndexType roiStart( m_RegionOfInterest.GetIndex() );
  typename Superclass::OutputImageType::PointType outputOrigin;
  inputPtr->TransformIndexToPhysicalPoint(roiStart, outputOrigin);
  outputPtr->SetOrigin(outputOrigin);
}

template< typename TInputImage, typename TOutputImage >
void
RegionOfInterestImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const RegionType & outputRegionForThread,
                       ThreadIdType threadId)
{

  // Get the input and output pointers
  const TInputImage *inputPtr  = this->GetInput();
  TOutputImage      *outputPtr = this->GetOutput();

  // Support progress methods/callbacks
  ProgressReporter progress( this, threadId, 1 );

  // Define the portion of the input to walk for this thread
  InputImageRegionType inputRegionForThread;
  inputRegionForThread.SetSize( outputRegionForThread.GetSize() );

  IndexType start;
  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    start[i] = m_RegionOfInterest.GetIndex()[i] +
               outputRegionForThread.GetIndex()[i];
    }

  inputRegionForThread.SetIndex(start);

  ImageAlgorithm::Copy( inputPtr, outputPtr, inputRegionForThread, outputRegionForThread );

  progress.CompletedPixel();
}

template< typename TInputImage, typename TOutputImage >
void
RegionOfInterestImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "RegionOfInterest: " << m_RegionOfInterest << std::endl;
}
} // end namespace itk

#endif
