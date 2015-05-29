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
#ifndef itkPadImageFilterBase_hxx
#define itkPadImageFilterBase_hxx

#include "itkPadImageFilterBase.h"

#include "itkImageAlgorithm.h"
#include "itkImageRegionExclusionIteratorWithIndex.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkObjectFactory.h"
#include "itkProgressReporter.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
PadImageFilterBase< TInputImage, TOutputImage >
::PadImageFilterBase()
{
  m_BoundaryCondition = ITK_NULLPTR;
}

template< typename TInputImage, typename TOutputImage >
void
PadImageFilterBase< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  if ( m_BoundaryCondition )
    {
    m_BoundaryCondition->Print( os, indent );
    }
  else
    {
    os << "ITK_NULLPTR" << std::endl;
    }
}


template< typename TInputImage, typename TOutputImage >
void
PadImageFilterBase< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // Get pointers to the input and output.
  typename Superclass::InputImagePointer inputPtr =
    const_cast< TInputImage * >( this->GetInput() );
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput();

  const InputImageRegionType & inputLargestPossibleRegion =
    inputPtr->GetLargestPossibleRegion();
  const OutputImageRegionType & outputRequestedRegion =
    outputPtr->GetRequestedRegion();

  // Ask the boundary condition for the input requested region.
  if ( !m_BoundaryCondition )
    {
    itkExceptionMacro( << "Boundary condition is ITK_NULLPTR so no request region can be generated.");
    }
  InputImageRegionType inputRequestedRegion =
    m_BoundaryCondition->GetInputRequestedRegion( inputLargestPossibleRegion,
                                                  outputRequestedRegion );

  inputPtr->SetRequestedRegion( inputRequestedRegion );
}

template< typename TInputImage, typename TOutputImage >
void
PadImageFilterBase< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{

  typename Superclass::OutputImagePointer outputPtr = this->GetOutput();
  typename Superclass::InputImageConstPointer inputPtr  = this->GetInput();
  // Use the region copy method to copy the input image values to the
  // output image.
  OutputImageRegionType copyRegion( outputRegionForThread );
  bool regionOverlaps = copyRegion.Crop( inputPtr->GetLargestPossibleRegion() );
  if ( regionOverlaps )
    {
    // Do a block copy for the overlapping region.
    ImageAlgorithm::Copy( inputPtr.GetPointer(), outputPtr.GetPointer(), copyRegion, copyRegion );

    // Use the boundary condition for pixels outside the input image region.
    typename OutputImageSizeType::SizeValueType numberOfPixels =
      outputRegionForThread.GetNumberOfPixels() - copyRegion.GetNumberOfPixels();
    ProgressReporter progress( this, threadId, numberOfPixels );

    ImageRegionExclusionIteratorWithIndex< TOutputImage > outIter( outputPtr,
                                                                   outputRegionForThread );
    outIter.SetExclusionRegion( copyRegion );
    outIter.GoToBegin();
    while ( !outIter.IsAtEnd() )
      {
      OutputImagePixelType value = static_cast< OutputImagePixelType >
        ( m_BoundaryCondition->GetPixel( outIter.GetIndex(), inputPtr ) );
      outIter.Set( value );
      ++outIter;
      progress.CompletedPixel();
      }
    }
  else
    {
    // There is no overlap. Appeal to the boundary condition for every pixel.
    ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

    ImageRegionIteratorWithIndex< TOutputImage > outIter( outputPtr,
                                                          outputRegionForThread );
    outIter.GoToBegin();
    while ( !outIter.IsAtEnd() )
      {
      OutputImagePixelType value = static_cast< OutputImagePixelType >
        ( m_BoundaryCondition->GetPixel( outIter.GetIndex(), inputPtr ) );
      outIter.Set( value );
      ++outIter;
      progress.CompletedPixel();
      }
    }
}

template< typename TInputImage, typename TOutputImage >
void
PadImageFilterBase< TInputImage, TOutputImage >
::InternalSetBoundaryCondition( const BoundaryConditionPointerType boundaryCondition )
{
  m_BoundaryCondition = boundaryCondition;
}


} // end namespace itk

#endif
