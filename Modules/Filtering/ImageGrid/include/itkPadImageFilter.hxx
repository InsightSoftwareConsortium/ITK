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
#ifndef __itkPadImageFilter_hxx
#define __itkPadImageFilter_hxx

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
template< class TInputImage, class TOutputImage >
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
template< class TInputImage, class TOutputImage >
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

  os << indent << "Boundary Condition: ";
  if ( m_BoundaryCondition )
    {
    m_BoundaryCondition->Print( os, indent );
    }
  else
    {
    os << "NULL" << std::endl;
    }
}

/**
 * PadImageFilter needs a smaller input requested region than
 * output requested region.  As such, PadImageFilter needs to
 * provide an implementation for GenerateInputRequestedRegion() in
 * order to inform the pipeline execution model.
 *
 * \sa ProcessObject::GenerateInputRequestedRegion()
 */
template< class TInputImage, class TOutputImage >
void
PadImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  // Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  typename Superclass::InputImagePointer inputPtr =
    const_cast< TInputImage * >( this->GetInput() );
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  const InputImageRegionType & inputLargestPossibleRegion =
    inputPtr->GetLargestPossibleRegion();
  const OutputImageRegionType & outputRequestedRegion =
    outputPtr->GetRequestedRegion();

  // Ask the boundary condition for the input requested region.
  InputImageRegionType inputRequestedRegion =
    m_BoundaryCondition->GetInputRequestedRegion( inputLargestPossibleRegion,
                                                  outputRequestedRegion );

  inputPtr->SetRequestedRegion( inputRequestedRegion );
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
template< class TInputImage, class TOutputImage >
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

template< class TInputImage, class TOutputImage >
void
PadImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  // Use the region copy method to copy the input image values to the
  // output image.
  OutputImageRegionType copyRegion( outputRegionForThread );
  bool regionOverlaps = copyRegion.Crop( this->GetInput()->GetLargestPossibleRegion() );
  if ( regionOverlaps )
    {
    // Do a block copy for the overlapping region.
    ImageAlgorithm::Copy( this->GetInput(), this->GetOutput(), copyRegion, copyRegion );

    // Use the boundary condition for pixels outside the input image region.
    typename OutputImageSizeType::SizeValueType numberOfPixels =
      outputRegionForThread.GetNumberOfPixels() - copyRegion.GetNumberOfPixels();
    ProgressReporter progress( this, threadId, numberOfPixels );

    ImageRegionExclusionIteratorWithIndex< TOutputImage > outIter( this->GetOutput(),
                                                                   outputRegionForThread );
    outIter.SetExclusionRegion( copyRegion );
    outIter.GoToBegin();
    while ( !outIter.IsAtEnd() )
      {
      OutputImagePixelType value = static_cast< OutputImagePixelType >
        ( m_BoundaryCondition->GetPixel( outIter.GetIndex(), this->GetInput() ) );
      outIter.Set( value );
      ++outIter;
      progress.CompletedPixel();
      }
    }
  else
    {
    // There is no overlap. Appeal to the boundary condition for every pixel.
    ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

    ImageRegionIteratorWithIndex< TOutputImage > outIter( this->GetOutput(),
                                                          outputRegionForThread );
    outIter.GoToBegin();
    while ( !outIter.IsAtEnd() )
      {
      OutputImagePixelType value = static_cast< OutputImagePixelType >
        ( m_BoundaryCondition->GetPixel( outIter.GetIndex(), this->GetInput() ) );
      outIter.Set( value );
      ++outIter;
      progress.CompletedPixel();
      }
    }
}

template< class TInputImage, class TOutputImage >
void
PadImageFilter< TInputImage, TOutputImage >
::InternalSetBoundaryCondition( const BoundaryConditionPointerType boundaryCondition )
{
  m_BoundaryCondition = boundaryCondition;
}


} // end namespace itk

#endif
