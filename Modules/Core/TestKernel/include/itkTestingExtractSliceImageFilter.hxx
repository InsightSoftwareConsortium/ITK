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
#ifndef itkTestingExtractSliceImageFilter_hxx
#define itkTestingExtractSliceImageFilter_hxx

#include "itkTestingExtractSliceImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkObjectFactory.h"
#include "itkProgressReporter.h"

namespace itk
{
namespace Testing
{

template< typename TInputImage, typename TOutputImage >
ExtractSliceImageFilter< TInputImage, TOutputImage >
::ExtractSliceImageFilter():
#ifdef ITKV3_COMPATIBILITY
  m_DirectionCollaspeStrategy(DIRECTIONCOLLAPSETOGUESS)
#else
  m_DirectionCollaspeStrategy(DIRECTIONCOLLAPSETOUNKOWN)
#endif
{}


template< typename TInputImage, typename TOutputImage >
void
ExtractSliceImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ExtractionRegion: " << m_ExtractionRegion << std::endl;
  os << indent << "OutputImageRegion: " << m_OutputImageRegion << std::endl;
  os << indent << "DirectionCollaspeStrategy: " << m_DirectionCollaspeStrategy << std::endl;
}


template< typename TInputImage, typename TOutputImage >
void
ExtractSliceImageFilter< TInputImage, TOutputImage >
::CallCopyOutputRegionToInputRegion(InputImageRegionType & destRegion,
                                    const OutputImageRegionType & srcRegion)
{
  ExtractSliceImageFilterRegionCopierType extractImageRegionCopier;

  extractImageRegionCopier(destRegion, srcRegion, m_ExtractionRegion);
}


template< typename TInputImage, typename TOutputImage >
void
ExtractSliceImageFilter< TInputImage, TOutputImage >
::SetExtractionRegion(InputImageRegionType extractRegion)
{

  unsigned int         nonzeroSizeCount = 0;
  InputImageSizeType   inputSize = extractRegion.GetSize();
  OutputImageSizeType  outputSize;
  outputSize.Fill(0);
  OutputImageIndexType outputIndex;
  outputIndex.Fill(0);

  /**
   * check to see if the number of non-zero entries in the extraction region
   * matches the number of dimensions in the output image.
   */
  for ( unsigned int i = 0; i < InputImageDimension; ++i )
    {
    if ( inputSize[i] )
      {
      if (nonzeroSizeCount < OutputImageDimension)
        {
        outputSize[nonzeroSizeCount] = inputSize[i];
        outputIndex[nonzeroSizeCount] = extractRegion.GetIndex()[i];
        }
      nonzeroSizeCount++;
      }
    }
  if ( nonzeroSizeCount != OutputImageDimension )
    {
    itkExceptionMacro("Extraction Region not consistent with output image");
    }

  m_ExtractionRegion = extractRegion;
  m_OutputImageRegion.SetSize(outputSize);
  m_OutputImageRegion.SetIndex(outputIndex);
  this->Modified();
}


template< typename TInputImage, typename TOutputImage >
void
ExtractSliceImageFilter< TInputImage, TOutputImage >
::GenerateOutputInformation()
{
  // do not call the superclass' implementation of this method since
  // this filter allows the input and the output to be of different dimensions

  // get pointers to the input and output
  TOutputImage      * outputPtr = this->GetOutput();
  const TInputImage * inputPtr  = this->GetInput();

  if ( !outputPtr || !inputPtr )
    {
    return;
    }

  // Set the output image size to the same value as the extraction region.
  outputPtr->SetLargestPossibleRegion(m_OutputImageRegion);

  // Set the output spacing and origin
  const ImageBase< InputImageDimension > *phyData;

  phyData =
    dynamic_cast< const ImageBase< InputImageDimension > * >( this->GetInput() );

  if ( phyData == ITK_NULLPTR )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::ExtractSliceImageFilter::GenerateOutputInformation "
                       << "cannot cast input to "
                       << typeid( ImageBase< InputImageDimension > * ).name() );
    }
  // Copy what we can from the image from spacing and origin of the input
  // This logic needs to be augmented with logic that select which
  // dimensions to copy

  const typename InputImageType::SpacingType &
    inputSpacing = inputPtr->GetSpacing();
  const typename InputImageType::DirectionType &
    inputDirection = inputPtr->GetDirection();
  const typename InputImageType::PointType &
    inputOrigin = inputPtr->GetOrigin();

  typename OutputImageType::SpacingType outputSpacing;
  typename OutputImageType::DirectionType outputDirection;
  typename OutputImageType::PointType outputOrigin;
  outputOrigin.Fill(0.0);

  if ( static_cast< unsigned int >( OutputImageDimension ) >
       static_cast< unsigned int >( InputImageDimension ) )
    {
    // copy the input to the output and fill the rest of the
    // output with zeros.
    for ( unsigned int i = 0; i < InputImageDimension; ++i )
      {
      outputSpacing[i] = inputSpacing[i];
      outputOrigin[i] = inputOrigin[i];
      for ( unsigned int dim = 0; dim < InputImageDimension; ++dim )
        {
        outputDirection[i][dim] = inputDirection[i][dim];
        }
      }
    for (unsigned int i=InputImageDimension; i < OutputImageDimension; ++i )
      {
      outputSpacing[i] = 1.0;
      outputOrigin[i] = 0.0;
      for ( unsigned int dim = 0; dim < InputImageDimension; ++dim )
        {
        outputDirection[i][dim] = 0.0;
        }
      outputDirection[i][i] = 1.0;
      }
    }
  else
    {
    // copy the non-collapsed part of the input spacing and origing to the
    // output
    outputDirection.SetIdentity();
    int nonZeroCount = 0;
    for ( unsigned int i = 0; i < InputImageDimension; ++i )
      {
      if ( m_ExtractionRegion.GetSize()[i] )
        {
        outputSpacing[nonZeroCount] = inputSpacing[i];
        outputOrigin[nonZeroCount] = inputOrigin[i];
        int nonZeroCount2 = 0;
        for ( unsigned int dim = 0; dim < InputImageDimension; ++dim )
          {
          if ( m_ExtractionRegion.GetSize()[dim] )
            {
            outputDirection[nonZeroCount][nonZeroCount2] =
              inputDirection[nonZeroCount][dim];
            ++nonZeroCount2;
            }
          }
        nonZeroCount++;
        }
      }
    }
  // if the filter changes from a higher to a lower dimension, or
  // if, after rebuilding the direction cosines, there's a zero
  // length cosine vector, reset the directions to identity.
  switch(m_DirectionCollaspeStrategy)
    {
    case DIRECTIONCOLLAPSETOIDENTITY:
    {
    outputDirection.SetIdentity();
    }
    break;
    case DIRECTIONCOLLAPSETOSUBMATRIX:
    {
    if ( vnl_determinant( outputDirection.GetVnlMatrix() ) == 0.0 )
      {
      itkExceptionMacro( << "Invalid submatrix extracted for collapsed direction." );
      }
    }
    break;
    case DIRECTIONCOLLAPSETOGUESS:
    {
    if ( vnl_determinant( outputDirection.GetVnlMatrix() ) == 0.0 )
      {
      outputDirection.SetIdentity();
      }
    }
    break;
    case DIRECTIONCOLLAPSETOUNKOWN:
    default:
    {
    itkExceptionMacro( << "It is required that the strategy for collapsing the direction matrix be explicitly specified. "
                       << "Set with either myfilter->SetDirectionCollapseToIdentity() or myfilter->SetDirectionCollapseToSubmatrix() "
                       << typeid( ImageBase< InputImageDimension > * ).name() );
    }
    }

  // set the spacing and origin
  outputPtr->SetSpacing(outputSpacing);
  outputPtr->SetDirection(outputDirection);
  outputPtr->SetOrigin(outputOrigin);
  outputPtr->SetNumberOfComponentsPerPixel(
    inputPtr->GetNumberOfComponentsPerPixel() );
}


template< typename TInputImage, typename TOutputImage >
void
ExtractSliceImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{

  itkDebugMacro(<< "Actually executing");

  // Get the input and output pointers
  const TInputImage * inputPtr = this->GetInput();
  TOutputImage     * outputPtr = this->GetOutput();

  // support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  // Define the portion of the input to walk for this thread
  InputImageRegionType inputRegionForThread;
  this->CallCopyOutputRegionToInputRegion(inputRegionForThread, outputRegionForThread);

  // Define the iterators.
  typedef ImageRegionIterator< TOutputImage >     OutputIterator;
  typedef ImageRegionConstIterator< TInputImage > InputIterator;

  OutputIterator outIt(outputPtr, outputRegionForThread);
  InputIterator  inIt(inputPtr, inputRegionForThread);

  // walk the output region, and sample the input image
  while ( !outIt.IsAtEnd() )
    {
    // copy the input pixel to the output
    outIt.Set( static_cast< OutputImagePixelType >( inIt.Get() ) );
    ++outIt;
    ++inIt;
    progress.CompletedPixel();
    }
}


template< typename TInputImage, typename TOutputImage >
void
ExtractSliceImageFilter< TInputImage, TOutputImage >
::SetInput(const TInputImage *input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0, const_cast< TInputImage * >( input ) );
}


template< typename TInputImage, typename TOutputImage >
const TInputImage *
ExtractSliceImageFilter< TInputImage, TOutputImage >
::GetInput(void) const
{
  return itkDynamicCastInDebugMode< const TInputImage * >( this->GetPrimaryInput() );
}

} // end namespace Testing
} // end namespace itk

#endif
