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
#ifndef itkPadImageFilter_hxx
#define itkPadImageFilter_hxx

#include "itkPadImageFilter.h"

#include "itkImageAlgorithm.h"
#include "itkImageRegionExclusionIteratorWithIndex.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkObjectFactory.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 *
 */
template< typename TInputImage, typename TOutputImage >
PadImageFilter< TInputImage, TOutputImage >
::PadImageFilter()
{
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    m_PadLowerBound[j] = 0;
    m_PadUpperBound[j] = 0;
    }
}

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
void
PadImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Output Pad Lower Bounds: [";

  if ( ImageDimension >= 1 )
    {
    os << m_PadLowerBound[0];
    }
  for ( unsigned int j = 1; j < ImageDimension; j++ )
    {
    os << ", " << m_PadLowerBound[j];
    }
  os << "]" << std::endl;

  os << indent << "Output Pad Upper Bounds: [";
  if ( ImageDimension >= 1 )
    {
    os << m_PadUpperBound[0];
    }
  for ( unsigned int j = 1; j < ImageDimension; j++ )
    {
    os << ", " << m_PadUpperBound[j];
    }
  os << "]" << std::endl;
}

/**
 * PadImageFilter produces an image which is a different resolution
 * than its input image.  As such, PadImageFilter needs to provide an
 * implementation for GenerateOutputInformation() in order to inform
 * the pipeline execution model.  The original documentation of this
 * method is below.
 *
 * \sa ProcessObject::GenerateOutputInformaton()
 */
template< typename TInputImage, typename TOutputImage >
void
PadImageFilter< TInputImage, TOutputImage >
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  typename Superclass::InputImageConstPointer inputPtr  = this->GetInput();
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput();

  if ( !outputPtr || !inputPtr )
    {
    return;
    }

  // we need to compute the output image size, and the
  // output image start index
  unsigned int i;
  typename TOutputImage::SizeType outputSize;
  typename TOutputImage::IndexType outputStartIndex;
  typename TInputImage::SizeType inputSize;
  typename TInputImage::IndexType inputStartIndex;

  inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  inputStartIndex = inputPtr->GetLargestPossibleRegion().GetIndex();

  for ( i = 0; i < TOutputImage::ImageDimension; i++ )
    {
    outputSize[i] = inputSize[i] + m_PadLowerBound[i] + m_PadUpperBound[i];
    outputStartIndex[i] = inputStartIndex[i] - static_cast< OffsetValueType >( m_PadLowerBound[i] );
    }

  typename TOutputImage::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize(outputSize);
  outputLargestPossibleRegion.SetIndex(outputStartIndex);

  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);
}

} // end namespace itk

#endif
