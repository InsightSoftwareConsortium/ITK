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

#ifndef itkKrcahEigentoScalarParameterEstimationImageFilter_hxx
#define itkKrcahEigentoScalarParameterEstimationImageFilter_hxx

#include "itkKrcahEigentoScalarParameterEstimationImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkProgressReporter.h"
#include "itkMath.h"

namespace itk
{
template< typename TInputImage, typename TMaskImage >
KrcahEigentoScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::KrcahEigentoScalarParameterEstimationImageFilter():
  m_Alpha(0.5f),
  m_Beta(0.5f),
  m_Gamma(0.5f),
  m_ParameterSet(UseImplementationParameters),
  m_BackgroundValue(NumericTraits< MaskPixelType >::Zero),
  m_NumVoxels(1),
  m_AccumulatedFrobeniusNorm(1)
{
  this->SetNumberOfRequiredInputs( 1 );
}

template< typename TInputImage, typename TMaskImage >
void
KrcahEigentoScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::AllocateOutputs()
{
  // Pass the input through as the output
  InputImagePointer image = const_cast< TInputImage * >( this->GetInput() );

  this->GraftOutput(image);
}

template< typename TInputImage, typename TMaskImage >
void
KrcahEigentoScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  if ( this->GetInput() )
  {
  InputImagePointer image =
    const_cast< typename Superclass::InputImageType * >( this->GetInput() );
  image->SetRequestedRegionToLargestPossibleRegion();
  }
}

template< typename TInputImage, typename TMaskImage >
void
KrcahEigentoScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::EnlargeOutputRequestedRegion(DataObject *data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage, typename TMaskImage >
void
KrcahEigentoScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::BeforeThreadedGenerateData()
{
  ThreadIdType numberOfThreads = this->GetNumberOfThreads();

  /* Resize threads */
  m_AccumulatedFrobeniusNorm.SetSize(numberOfThreads);
  m_NumVoxels.SetSize(numberOfThreads);

  m_AccumulatedFrobeniusNorm.Fill(NumericTraits< RealType >::ZeroValue());
  m_NumVoxels.Fill(NumericTraits< SizeValueType >::ZeroValue());
}

template< typename TInputImage, typename TMaskImage >
void
KrcahEigentoScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::AfterThreadedGenerateData()
{
  ThreadIdType numberOfThreads = this->GetNumberOfThreads();

  /* Determine default parameters */
  switch(m_ParameterSet)
  {
    case UseImplementationParameters:
      m_Alpha = 0.5f;
      m_Beta = 0.5f;
      m_Gamma = 0.5f;
      break;
    case UseJournalParameters:
      m_Alpha = 0.5f;
      m_Beta = 0.5f;
      m_Gamma = 0.25f;
      break;
    default:
      itkExceptionMacro(<< "Have bad parameterset enumeration " << m_ParameterSet);
      break;
  }

  /* Accumulate over threads */
  SizeValueType numVoxels = NumericTraits< SizeValueType >::ZeroValue();
  RealType accumulatedFrobeniusNorm = NumericTraits< RealType >::ZeroValue();

  for (unsigned int i = 0; i < numberOfThreads; ++i )
  {
    numVoxels += m_NumVoxels[i];
    accumulatedFrobeniusNorm += m_AccumulatedFrobeniusNorm[i];
  }

  /* Do derived measure */
  if (numVoxels > 0) {
    RealType averageFrobeniusNorm = (RealType)accumulatedFrobeniusNorm / (RealType)numVoxels;
    m_Gamma = m_Gamma * averageFrobeniusNorm;
  }
}

template< typename TInputImage, typename TMaskImage >
void
KrcahEigentoScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::ThreadedGenerateData(const OutputRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  const SizeValueType size0 = outputRegionForThread.GetSize(0);
  if (size0 == 0)
  {
    return;
  }

  /* Determine which function to call */
  RealType (Self::*normFunction)(InputPixelType);
  switch(m_ParameterSet)
  {
    case UseImplementationParameters:
      normFunction = &Self::CalculateNormAccordingToImplementation;
      break;
    case UseJournalParameters:
      normFunction = &Self::CalculateNormAccordingToJournalArticle;
      break;
    default:
      itkExceptionMacro(<< "Have bad parameterset enumeration " << m_ParameterSet);
      break;
  }

  /* Count starts zero */
  SizeValueType numVoxels = NumericTraits< SizeValueType >::ZeroValue();
  RealType accumulatedFrobeniusNorm = NumericTraits< RealType >::ZeroValue();

  /* Get input pointer */
  InputImagePointer inputPointer = const_cast< TInputImage * >( this->GetInput() );

  /* Get mask pointer */
  MaskImagePointer maskPointer = TMaskImage::New();
  maskPointer = const_cast<TMaskImage*>(this->GetMaskImage());

  /* Setup progress reporter */
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  /* Setup iterator */
  ImageRegionConstIteratorWithIndex< TInputImage > inputIt(inputPointer, outputRegionForThread);

  /* Iterate and count */
  inputIt.GoToBegin();
  while ( !inputIt.IsAtEnd() )
  {
    if ( (!maskPointer) ||  (maskPointer->GetPixel(inputIt.GetIndex()) != m_BackgroundValue) )
    {
      numVoxels++;

      /* Compute norm */
      accumulatedFrobeniusNorm += (this->*normFunction)(inputIt.Get());
    }
    ++inputIt;
    progress.CompletedPixel();
  }

  /* Store this thread */
  m_AccumulatedFrobeniusNorm[threadId] = accumulatedFrobeniusNorm;
  m_NumVoxels[threadId] = numVoxels;
}

template< typename TInputImage, typename TMaskImage >
typename KrcahEigentoScalarParameterEstimationImageFilter< TInputImage, TMaskImage >::RealType
KrcahEigentoScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::CalculateNormAccordingToImplementation(InputPixelType pixel) {
  /* Sum of the absolute value of the eigenvalues */
  RealType norm = 0;
  for( unsigned int i = 0; i < pixel.Length; ++i) {
    norm += Math::abs(pixel[i]);
  }
  return norm;
}

template< typename TInputImage, typename TMaskImage >
typename KrcahEigentoScalarParameterEstimationImageFilter< TInputImage, TMaskImage >::RealType
KrcahEigentoScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::CalculateNormAccordingToJournalArticle(InputPixelType pixel) {
  /* Sum of the eigenvalues */
  RealType norm = 0;
  for( unsigned int i = 0; i < pixel.Length; ++i) {
    norm += pixel[i];
  }
  return norm;
}

template< typename TInputImage, typename TMaskImage >
void
KrcahEigentoScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m_Alpha: " << m_Alpha << std::endl;
  os << indent << "m_Beta: " << m_Beta << std::endl;
  os << indent << "m_Gamma: " << m_Gamma << std::endl;
  os << indent << "m_BackgroundValue: " << m_BackgroundValue << std::endl;
  os << indent << "m_ParameterSet: " << m_ParameterSet << std::endl;
}

} // end namespace itk

#endif // itkKrcahEigentoScalarParameterEstimationImageFilter_hxx
