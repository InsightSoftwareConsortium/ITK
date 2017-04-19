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
#ifndef itkExtractImageFilter_hxx
#define itkExtractImageFilter_hxx

#include "itkExtractImageFilter.h"
#include "itkImageAlgorithm.h"
#include "itkObjectFactory.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 *
 */
template< typename TInputImage, typename TOutputImage >
ExtractImageFilter< TInputImage, TOutputImage >
::ExtractImageFilter():
#ifdef ITKV3_COMPATIBILITY
  m_DirectionCollapseStrategy(DIRECTIONCOLLAPSETOGUESS)
#else
  m_DirectionCollapseStrategy(DIRECTIONCOLLAPSETOUNKOWN)
#endif
{
  Superclass::InPlaceOff();
}

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
void
ExtractImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ExtractionRegion: " << m_ExtractionRegion << std::endl;
  os << indent << "OutputImageRegion: " << m_OutputImageRegion << std::endl;
  os << indent << "DirectionCollapseStrategy: " << m_DirectionCollapseStrategy << std::endl;
}

template< typename TInputImage, typename TOutputImage >
void
ExtractImageFilter< TInputImage, TOutputImage >
::CallCopyOutputRegionToInputRegion(InputImageRegionType & destRegion,
                                    const OutputImageRegionType & srcRegion)
{
  ExtractImageFilterRegionCopierType extractImageRegionCopier;

  extractImageRegionCopier(destRegion, srcRegion, m_ExtractionRegion);
}

template< typename TInputImage, typename TOutputImage >
void
ExtractImageFilter< TInputImage, TOutputImage >
::SetExtractionRegion(InputImageRegionType extractRegion)
{
  m_ExtractionRegion = extractRegion;

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
      outputSize[nonzeroSizeCount] = inputSize[i];
      outputIndex[nonzeroSizeCount] = extractRegion.GetIndex()[i];
      nonzeroSizeCount++;
      }
    }

  if ( nonzeroSizeCount != OutputImageDimension )
    {
    itkExceptionMacro("Extraction Region not consistent with output image");
    }

  m_OutputImageRegion.SetSize(outputSize);
  m_OutputImageRegion.SetIndex(outputIndex);
  this->Modified();
}

/**
 * ExtractImageFilter can produce an image which is a different resolution
 * than its input image.  As such, ExtractImageFilter needs to provide an
 * implementation for GenerateOutputInformation() in order to inform
 * the pipeline execution model.  The original documentation of this
 * method is below.
 *
 * \sa ProcessObject::GenerateOutputInformaton()
 */
template< typename TInputImage, typename TOutputImage >
void
ExtractImageFilter< TInputImage, TOutputImage >
::GenerateOutputInformation()
{
  // do not call the superclass' implementation of this method since
  // this filter allows the input and the output to be of different dimensions

  // get pointers to the input and output
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput();
  typename Superclass::InputImageConstPointer inputPtr  = this->GetInput();

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

  if ( phyData )
    {
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
                inputDirection[i][dim];
              ++nonZeroCount2;
              }
            }
          nonZeroCount++;
          }
        }
      }
    // if the filter changes from a higher to a lower dimension, or
    // if, after rebuilding the direction cosines, there's a zero
    // length cosine vector, reset the directions to identity
    // or throw an exception, depending on the collapse strategy.
    if( static_cast<int>(InputImageDimension) != static_cast<int>(OutputImageDimension) )
      {
      switch(m_DirectionCollapseStrategy)
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
      }
    // set the spacing and origin
    outputPtr->SetSpacing(outputSpacing);
    outputPtr->SetDirection(outputDirection);
    outputPtr->SetOrigin(outputOrigin);
    outputPtr->SetNumberOfComponentsPerPixel(
      inputPtr->GetNumberOfComponentsPerPixel() );
    }
  else
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::ExtractImageFilter::GenerateOutputInformation "
                       << "cannot cast input to "
                       << typeid( ImageBase< InputImageDimension > * ).name() );
    }
}

template< typename TInputImage, typename TOutputImage >
void
ExtractImageFilter< TInputImage, TOutputImage >
::GenerateData()
{

  // InPlace::AllocateOutputs set the running in place ivar.
  // This method will be called again, by GenerateData, but there is
  // no harm done.
  this->AllocateOutputs();

  // The input matched the output, nothing to do.
  if ( this->GetRunningInPlace() )
    {
    OutputImageType *outputPtr = this->GetOutput();

    // the in-place grafting copies the meta data, this needs to be
    // set back.
    outputPtr->SetLargestPossibleRegion(m_OutputImageRegion);

    this->UpdateProgress( 1.0 );
    return;
    }

  this->Superclass::GenerateData();
}

/**
 * ExtractImageFilter can be implemented as a multithreaded filter.
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
ExtractImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  itkDebugMacro(<< "Actually executing");

  // Get the input and output pointers
  const InputImageType *inputPtr = this->GetInput();
  OutputImageType      *outputPtr = this->GetOutput();

  // support progress methods/callbacks
  ProgressReporter progress( this, threadId, 1 );

  // Define the portion of the input to walk for this thread
  InputImageRegionType inputRegionForThread;
  this->CallCopyOutputRegionToInputRegion(inputRegionForThread, outputRegionForThread);

  // copy the input pixel to the output
  ImageAlgorithm::Copy( inputPtr, outputPtr, inputRegionForThread, outputRegionForThread );
  progress.CompletedPixel();

}
} // end namespace itk

#endif
