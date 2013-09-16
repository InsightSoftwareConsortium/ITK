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
#ifndef __itkRegionOfInterestImageFilter_hxx
#define __itkRegionOfInterestImageFilter_hxx

#include "itkRegionOfInterestImageFilter.h"
#include "itkImageAlgorithm.h"
#include "itkObjectFactory.h"
#include "itkProgressReporter.h"
#include "itkImage.h"

namespace itk
{
/**
 *
 */
template< typename TInputImage, typename TOutputImage >
RegionOfInterestImageFilter< TInputImage, TOutputImage >
::RegionOfInterestImageFilter()
{}

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
void
RegionOfInterestImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "RegionOfInterest: " << m_RegionOfInterest << std::endl;
}

template< typename TInputImage, typename TOutputImage >
void
RegionOfInterestImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointer to the input
  typename Superclass::InputImagePointer inputPtr =
    const_cast< TInputImage * >( this->GetInput() );

  if ( inputPtr )
    {
    // request the region of interest
    inputPtr->SetRequestedRegion(m_RegionOfInterest);
    }
}

template< typename TInputImage, typename TOutputImage >
void
RegionOfInterestImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *output)
{
  // call the superclass' implementation of this method
  Superclass::EnlargeOutputRequestedRegion(output);

  // generate everything in the region of interest
  output->SetRequestedRegionToLargestPossibleRegion();
}

/**
 * RegionOfInterestImageFilter can produce an image which is a different size
 * than its input image.  As such, RegionOfInterestImageFilter needs to provide an
 * implementation for GenerateOutputInformation() in order to inform
 * the pipeline execution model.  The original documentation of this
 * method is below.
 *
 * \sa ProcessObject::GenerateOutputInformaton()
 */
template< typename TInputImage, typename TOutputImage >
void
RegionOfInterestImageFilter< TInputImage, TOutputImage >
::GenerateOutputInformation()
{
  // do not call the superclass' implementation of this method since
  // this filter allows the input the output to be of different dimensions

  // get pointers to the input and output
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

/**
   * RegionOfInterestImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData().  ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()
   */
template< typename TInputImage, typename TOutputImage >
void
RegionOfInterestImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const RegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  itkDebugMacro(<< "Actually executing");

  // Get the input and output pointers
  const TInputImage *inputPtr  = this->GetInput();
  TOutputImage      *outputPtr = this->GetOutput();

  // support progress methods/callbacks
  ProgressReporter progress( this, threadId, 1 );

  // Define the portion of the input to walk for this thread
  InputImageRegionType inputRegionForThread;
  inputRegionForThread.SetSize( outputRegionForThread.GetSize() );

  IndexType start;
  IndexType roiStart( m_RegionOfInterest.GetIndex() );
  IndexType threadStart( outputRegionForThread.GetIndex() );
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    start[i] = roiStart[i] + threadStart[i];
    }

  inputRegionForThread.SetIndex(start);

  ImageAlgorithm::Copy( inputPtr, outputPtr, inputRegionForThread, outputRegionForThread );

  progress.CompletedPixel();

}
} // end namespace itk

#endif
