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

#ifndef itkDescoteauxEigenToScalarParameterEstimationImageFilter_hxx
#define itkDescoteauxEigenToScalarParameterEstimationImageFilter_hxx

#include "itkDescoteauxEigenToScalarParameterEstimationImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkProgressReporter.h"
#include "itkMath.h"

namespace itk
{
template< typename TInputImage, typename TMaskImage >
DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::DescoteauxEigenToScalarParameterEstimationImageFilter():
  m_FrobeniusNormWeight(0.5f),
  m_BackgroundValue(NumericTraits< MaskPixelType >::Zero),
  m_MaxFrobeniusNorm(1)
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
}

template< typename TInputImage, typename TMaskImage >
void
DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::AllocateOutputs()
{
  /* Pass the input through as the output */
  InputImagePointer image = const_cast< TInputImage * >( this->GetInput() );
  this->GraftOutput(image);
}

template< typename TInputImage, typename TMaskImage >
void
DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
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
DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::EnlargeOutputRequestedRegion(DataObject *data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage, typename TMaskImage >
void
DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::BeforeThreadedGenerateData()
{
  ThreadIdType numberOfThreads = this->GetNumberOfThreads();

  /* Resize threads */
  m_MaxFrobeniusNorm.SetSize(numberOfThreads);
  m_MaxFrobeniusNorm.Fill(NumericTraits< RealType >::ZeroValue());
}

template< typename TInputImage, typename TMaskImage >
void
DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::AfterThreadedGenerateData()
{
  ThreadIdType numberOfThreads = this->GetNumberOfThreads();

  /* Determine default parameters */
  RealType alpha, beta, c;
  alpha = 0.5f;
  beta = 0.5f;
  c = m_FrobeniusNormWeight;

  /* Accumulate over threads */
  RealType maxFrobeniusNorm = NumericTraits< RealType >::ZeroValue();

  for (unsigned int i = 0; i < numberOfThreads; ++i )
  {
    if (m_MaxFrobeniusNorm[i] > maxFrobeniusNorm)
    {
      maxFrobeniusNorm = m_MaxFrobeniusNorm[i];
    }
  }

  /* Scale c */
  if (maxFrobeniusNorm > 0) {
    c = c * maxFrobeniusNorm;
  }

  /* Assign outputs parameters */
  this->GetAlphaOutput()->Set( alpha );
  this->GetBetaOutput()->Set( beta );
  this->GetCOutput()->Set( c );
}

template< typename TInputImage, typename TMaskImage >
void
DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::ThreadedGenerateData(const OutputRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  const SizeValueType size0 = outputRegionForThread.GetSize(0);
  if (size0 == 0)
  {
    return;
  }

  /* Count starts zero */
  RealType maxFrobeniusNorm = NumericTraits< RealType >::ZeroValue();
  RealType thisFrobeniusNorm;

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
      /* Compute max norm */
      thisFrobeniusNorm = this->CalculateFrobeniusNorm(inputIt.Get());
      if (thisFrobeniusNorm > maxFrobeniusNorm)
      {
        maxFrobeniusNorm = thisFrobeniusNorm;
      }
    }
    ++inputIt;
    progress.CompletedPixel();
  }

  /* Store this thread */
  m_MaxFrobeniusNorm[threadId] = maxFrobeniusNorm;
}

template< typename TInputImage, typename TMaskImage >
typename DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >::RealType
DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::CalculateFrobeniusNorm(InputPixelType pixel) {
  /* Forbenius norm is given by the square root of the sum of squares 
   * of the eigenvalues for real, symmetric matricies
   */
  RealType norm = 0;
  for( unsigned int i = 0; i < pixel.Length; ++i) {
    norm += pixel[i]*pixel[i];
  }
  return sqrt(norm);
}

template< typename TInputImage, typename TMaskImage >
typename DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >::RealTypeDecoratedType *
DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::GetAlphaOutput() {
  return static_cast< RealTypeDecoratedType * >( this->ProcessObject::GetOutput(1) );
}

template< typename TInputImage, typename TMaskImage >
const typename DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >::RealTypeDecoratedType *
DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::GetAlphaOutput() const {
  return static_cast< const RealTypeDecoratedType * >( this->ProcessObject::GetOutput(1) );
}

template< typename TInputImage, typename TMaskImage >
typename DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >::RealTypeDecoratedType *
DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::GetBetaOutput() {
  return static_cast< RealTypeDecoratedType * >( this->ProcessObject::GetOutput(2) );
}

template< typename TInputImage, typename TMaskImage >
const typename DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >::RealTypeDecoratedType *
DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::GetBetaOutput() const {
  return static_cast< const RealTypeDecoratedType * >( this->ProcessObject::GetOutput(2) );
}

template< typename TInputImage, typename TMaskImage >
typename DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >::RealTypeDecoratedType *
DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::GetCOutput() {
  return static_cast< RealTypeDecoratedType * >( this->ProcessObject::GetOutput(3) );
}

template< typename TInputImage, typename TMaskImage >
const typename DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >::RealTypeDecoratedType *
DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::GetCOutput() const {
  return static_cast< const RealTypeDecoratedType * >( this->ProcessObject::GetOutput(3) );
}

template< typename TInputImage, typename TMaskImage >
void
DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Alpha: " << this->GetAlpha() << std::endl;
  os << indent << "Beta: " << this->GetBeta() << std::endl;
  os << indent << "C: " << this->GetC() << std::endl;
  os << indent << "BackgroundValue: " << GetBackgroundValue() << std::endl;
  os << indent << "FrobeniusNormWeight: " << GetFrobeniusNormWeight() << std::endl;
}

} // end namespace

#endif // itkDescoteauxEigenToScalarParameterEstimationImageFilter_hxx
