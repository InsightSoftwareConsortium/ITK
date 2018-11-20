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

#ifndef itkKrcahEigenToScalarParameterEstimationImageFilter_hxx
#define itkKrcahEigenToScalarParameterEstimationImageFilter_hxx

#include "itkKrcahEigenToScalarParameterEstimationImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkProgressReporter.h"
#include "itkMath.h"

namespace itk
{
template< typename TInputImage, typename TMaskImage >
KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::KrcahEigenToScalarParameterEstimationImageFilter():
  m_ParameterSet(UseImplementationParameters),
  m_BackgroundValue(NumericTraits< MaskPixelType >::Zero),
  m_NumVoxels(1),
  m_AccumulatedAverageTrace(1)
{
  /* We require an input, optional mask */
  this->SetNumberOfRequiredInputs( 1 );
  this->SetNumberOfRequiredOutputs( 4 );

  /* Allocate all decorators */
  for ( int i = 1; i < 4; ++i )
  {
    typename RealTypeDecoratedType::Pointer output = RealTypeDecoratedType::New();
    output->Set( 0.5 );
    this->ProcessObject::SetNthOutput( i,  output.GetPointer() );
  }
  this->DynamicMultiThreadingOff();
}

template< typename TInputImage, typename TMaskImage >
void
KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::AllocateOutputs()
{
  /* Pass the input through as the output */
  InputImagePointer image = const_cast< TInputImage * >( this->GetInput() );
  this->GraftOutput(image);
}

template< typename TInputImage, typename TMaskImage >
void
KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  
  if ( this->GetInput() )
  {
    InputImagePointer image = const_cast< typename Superclass::InputImageType * >( this->GetInput() );
    image->SetRequestedRegionToLargestPossibleRegion();
  }
  if ( this->GetMaskImage() )
  {
    MaskImagePointer mask = const_cast< TMaskImage * >( this->GetMaskImage() );
    mask->SetRequestedRegionToLargestPossibleRegion();
  }
}

template< typename TInputImage, typename TMaskImage >
void
KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::EnlargeOutputRequestedRegion(DataObject *data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage, typename TMaskImage >
void
KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::BeforeThreadedGenerateData()
{
  ThreadIdType numberOfThreads = this->GetNumberOfThreads();

  /* Resize threads */
  m_AccumulatedAverageTrace.SetSize(numberOfThreads);
  m_NumVoxels.SetSize(numberOfThreads);

  m_AccumulatedAverageTrace.Fill(NumericTraits< RealType >::ZeroValue());
  m_NumVoxels.Fill(NumericTraits< SizeValueType >::ZeroValue());
}

template< typename TInputImage, typename TMaskImage >
void
KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::AfterThreadedGenerateData()
{
  ThreadIdType numberOfThreads = this->GetNumberOfThreads();

  /* Determine default parameters */
  RealType alpha, beta, gamma;
  switch(m_ParameterSet)
  {
    case UseImplementationParameters:
      alpha = Math::sqrt2 * 0.5f;
      beta = Math::sqrt2 * 0.5f;
      gamma = Math::sqrt2 * 0.5f;
      break;
    case UseJournalParameters:
      alpha = 0.5f;
      beta = 0.5f;
      gamma = 0.25f;
      break;
    default:
      itkExceptionMacro(<< "Have bad parameterset enumeration " << m_ParameterSet);
      break;
  }

  /* Accumulate over threads */
  SizeValueType numVoxels = NumericTraits< SizeValueType >::ZeroValue();
  RealType accumulatedAverageTrace = NumericTraits< RealType >::ZeroValue();

  for (unsigned int i = 0; i < numberOfThreads; ++i )
  {
    numVoxels += m_NumVoxels[i];
    accumulatedAverageTrace += m_AccumulatedAverageTrace[i];
  }

  /* Do derived measure */
  if (numVoxels > 0) {
    RealType averageTrace = (RealType)accumulatedAverageTrace / (RealType)numVoxels;
    gamma = gamma * averageTrace;
  }

  /* Assign outputs parameters */
  this->GetAlphaOutput()->Set( alpha );
  this->GetBetaOutput()->Set( beta );
  this->GetGammaOutput()->Set( gamma );
}

template< typename TInputImage, typename TMaskImage >
void
KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::ThreadedGenerateData(const OutputRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  /* Determine which function to call */
  RealType (Self::*traceFunction)(InputPixelType);
  switch(m_ParameterSet)
  {
    case UseImplementationParameters:
      traceFunction = &Self::CalculateTraceAccordingToImplementation;
      break;
    case UseJournalParameters:
      traceFunction = &Self::CalculateTraceAccordingToJournalArticle;
      break;
    default:
      itkExceptionMacro(<< "Have bad parameterset enumeration " << m_ParameterSet);
      break;
  }

  /* Count starts zero */
  SizeValueType numVoxels = NumericTraits< SizeValueType >::ZeroValue();
  RealType accumulatedAverageTrace = NumericTraits< RealType >::ZeroValue();

  /* Get input pointer */
  InputImageConstPointer inputPointer = this->GetInput();

  /* Get mask pointer */
  MaskImageConstPointer maskPointer = this->GetMaskImage();

  /* If we have a mask pointer we need to crop outputRegionForThread to the mask region */
  InputRegionType croppedRegion = outputRegionForThread;
  if (maskPointer) {
    croppedRegion.Crop( maskPointer->GetLargestPossibleRegion() );
    /* No check for one region being inside the other. Superclass::GenerateInputRequestedRegion()
     * takes care of the case of the mask region being outside the image region. It's actually
     * impossible to determine if the mask region is valid inside ThreadedGenerateData because
     * outputRegionForThread is a sub region of the output region.
     */
  }

  /* If size is zero, return */
  const SizeValueType size0 = croppedRegion.GetSize(0);
  if (size0 == 0)
  {
    return;
  }

  /* Setup progress reporter */
  ProgressReporter progress( this, threadId, croppedRegion.GetNumberOfPixels() );

  /* Setup iterator */
  ImageRegionConstIteratorWithIndex< TInputImage > inputIt(inputPointer, croppedRegion);

  /* Iterate and count */
  inputIt.GoToBegin();
  while ( !inputIt.IsAtEnd() )
  {
    if ( (!maskPointer) ||  (maskPointer->GetPixel(inputIt.GetIndex()) != m_BackgroundValue) )
    {
      numVoxels++;

      /* Compute trace */
      accumulatedAverageTrace += (this->*traceFunction)(inputIt.Get());
    }
    ++inputIt;
    progress.CompletedPixel();
  }

  /* Store this thread */
  m_AccumulatedAverageTrace[threadId] = accumulatedAverageTrace;
  m_NumVoxels[threadId] = numVoxels;
}

template< typename TInputImage, typename TMaskImage >
typename KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >::RealType
KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::CalculateTraceAccordingToImplementation(InputPixelType pixel) {
  /* Sum of the absolute value of the eigenvalues */
  RealType trace = 0;
  for( unsigned int i = 0; i < pixel.Length; ++i) {
    trace += Math::abs(pixel[i]);
  }
  return trace;
}

template< typename TInputImage, typename TMaskImage >
typename KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >::RealType
KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::CalculateTraceAccordingToJournalArticle(InputPixelType pixel) {
  /* Sum of the eigenvalues */
  RealType trace = 0;
  for( unsigned int i = 0; i < pixel.Length; ++i) {
    trace += pixel[i];
  }
  return trace;
}

template< typename TInputImage, typename TMaskImage >
typename KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >::RealTypeDecoratedType *
KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::GetAlphaOutput() {
  return static_cast< RealTypeDecoratedType * >( this->ProcessObject::GetOutput(1) );
}

template< typename TInputImage, typename TMaskImage >
const typename KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >::RealTypeDecoratedType *
KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::GetAlphaOutput() const {
  return static_cast< const RealTypeDecoratedType * >( this->ProcessObject::GetOutput(1) );
}

template< typename TInputImage, typename TMaskImage >
typename KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >::RealTypeDecoratedType *
KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::GetBetaOutput() {
  return static_cast< RealTypeDecoratedType * >( this->ProcessObject::GetOutput(2) );
}

template< typename TInputImage, typename TMaskImage >
const typename KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >::RealTypeDecoratedType *
KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::GetBetaOutput() const {
  return static_cast< const RealTypeDecoratedType * >( this->ProcessObject::GetOutput(2) );
}

template< typename TInputImage, typename TMaskImage >
typename KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >::RealTypeDecoratedType *
KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::GetGammaOutput() {
  return static_cast< RealTypeDecoratedType * >( this->ProcessObject::GetOutput(3) );
}

template< typename TInputImage, typename TMaskImage >
const typename KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >::RealTypeDecoratedType *
KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::GetGammaOutput() const {
  return static_cast< const RealTypeDecoratedType * >( this->ProcessObject::GetOutput(3) );
}

template< typename TInputImage, typename TMaskImage >
void
KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m_Alpha: " << this->GetAlpha() << std::endl;
  os << indent << "m_Beta: " << this->GetBeta() << std::endl;
  os << indent << "m_Gamma: " << this->GetGamma() << std::endl;
  os << indent << "m_BackgroundValue: " << m_BackgroundValue << std::endl;
  os << indent << "m_ParameterSet: " << m_ParameterSet << std::endl;
}

} // end namespace itk

#endif // itkKrcahEigenToScalarParameterEstimationImageFilter_hxx
