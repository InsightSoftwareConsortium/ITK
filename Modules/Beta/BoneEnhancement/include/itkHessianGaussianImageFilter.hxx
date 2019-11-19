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

#ifndef itkHessianGaussianImageFilter_hxx
#define itkHessianGaussianImageFilter_hxx

#include "itkHessianGaussianImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkProgressAccumulator.h"
#include "itkGaussianDerivativeOperator.h"
#include "itkMath.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage >
HessianGaussianImageFilter< TInputImage, TOutputImage >
::HessianGaussianImageFilter()
{
  // Create Derivative Filter
  m_DerivativeFilter = DerivativeFilterType::New();
  m_DerivativeFilter->SetInput( this->GetInput() );
  m_DerivativeFilter->ReleaseDataFlagOn(); // output is only used once
  m_DerivativeFilter->UseImageSpacingOn();

  // Create image adaptor
  m_ImageAdaptor = OutputImageAdaptorType::New();

  // Setup defaults
  this->SetNormalizeAcrossScale(false);
  this->SetSigma(1.0);
}

/**
 * Set value of Sigma
 */
template< typename TInputImage, typename TOutputImage >
void
HessianGaussianImageFilter< TInputImage, TOutputImage >
::SetSigma(RealType sigma)
{
  m_DerivativeFilter->SetVariance(sigma*sigma);

  this->Modified();
}

/**
 * Get value of Sigma
 */
template< typename TInputImage, typename TOutputImage >
typename HessianGaussianImageFilter< TInputImage, TOutputImage >
::RealType
 HessianGaussianImageFilter< TInputImage, TOutputImage >
::GetSigma() const
{
  return sqrt(m_DerivativeFilter->GetVariance()[0]);
}

/**
 * Set Normalize Across Scale Space
 */
template< typename TInputImage, typename TOutputImage >
void
HessianGaussianImageFilter< TInputImage, TOutputImage >
::SetNormalizeAcrossScale(bool normalize)
{
  m_DerivativeFilter->SetNormalizeAcrossScale(normalize);

  this->Modified();
}

/**
 * Get Normalize Across SCale Space
 */
template< typename TInputImage, typename TOutputImage >
bool
HessianGaussianImageFilter< TInputImage, TOutputImage >
::GetNormalizeAcrossScale() const
{
  return m_DerivativeFilter->GetNormalizeAcrossScale();
}

template< typename TInputImage, typename TOutputImage >
void
HessianGaussianImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
throw( InvalidRequestedRegionError )
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input
  typename Superclass::InputImagePointer inputPtr =
    const_cast< TInputImage * >( this->GetInput() );

  if ( !inputPtr )
    {
    return;
    }

  // Build an operator so that we can determine the kernel size
  GaussianDerivativeOperator< InternalRealType, ImageDimension >  oper;
  typename TInputImage::SizeType                                  radius;

  for ( unsigned int i = 0; i < TInputImage::ImageDimension; i++ )
    {
    // Determine the size of the operator in this dimension.  Note that the
    // Gaussian is built as a 1D operator in each of the specified directions.
    oper.SetDirection(i);
    if ( this->GetInput()->GetSpacing()[i] == 0.0 )
      {
      itkExceptionMacro(<< "Pixel spacing cannot be zero");
      }
    else
      {
      oper.SetSpacing(this->GetInput()->GetSpacing()[i]);
      }

    // GaussianDerivativeOperator modifies the variance when setting image
    // spacing
    oper.SetVariance(this->m_DerivativeFilter->GetVariance()[i]);
    oper.SetMaximumError(this->m_DerivativeFilter->GetMaximumError()[i]);
    oper.SetMaximumKernelWidth(this->m_DerivativeFilter->GetMaximumKernelWidth());
    oper.CreateDirectional();

    radius[i] = oper.GetRadius(i);
    }

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius(radius);

  // crop the input requested region at the input's largest possible region
  if ( inputRequestedRegion.Crop( inputPtr->GetLargestPossibleRegion() ) )
    {
    inputPtr->SetRequestedRegion(inputRequestedRegion);
    return;
    }
  else
    {
    // Couldn't crop the region (requested region is outside the largest
    // possible region).  Throw an exception.

    // store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion(inputRequestedRegion);

    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    e.SetLocation(ITK_LOCATION);
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}

/**
 * Compute filter for Gaussian kernel
 */
template< typename TInputImage, typename TOutputImage >
void
HessianGaussianImageFilter< TInputImage, TOutputImage >
::GenerateData(void)
{
  itkDebugMacro(<< "HessianGaussianImageFilter generating data ");

  // Create a process accumulator for tracking the progress of this
  // minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Compute the contribution of each filter to the total progress.
  // For a DxD matrix, we do the following number of computations:
  //    \sum_{i=1}^D \sum_{j=i}^D 1 = D(D+1)/2
  const double weight =
    1.0 / ( ImageDimension * ( ImageDimension + 1 ) / 2.0 );

  progress->RegisterInternalFilter(m_DerivativeFilter, weight);

  const typename TInputImage::ConstPointer inputImage( this->GetInput() );

  // Setup Image Adaptor
  m_ImageAdaptor->SetImage( this->GetOutput() );

  m_ImageAdaptor->SetLargestPossibleRegion(
    this->GetOutput()->GetLargestPossibleRegion() );

  m_ImageAdaptor->SetBufferedRegion(
    this->GetOutput()->GetRequestedRegion() );

  m_ImageAdaptor->SetRequestedRegion(
    this->GetOutput()->GetRequestedRegion() );

  m_ImageAdaptor->Allocate();

  m_DerivativeFilter->SetInput(inputImage);
  m_DerivativeFilter->GetOutput()->SetRequestedRegion(this->GetOutput()->GetRequestedRegion());

  unsigned int element = 0;
  int order[ImageDimension];

  for ( unsigned int dima = 0; dima < ImageDimension; dima++ )
    {
    for ( unsigned int dimb = dima; dimb < ImageDimension; dimb++ )
    {
      // All directions have zero order derivative initially
      for (int k = 0; k < ImageDimension; ++k) {
        order[k] = 0;
      }

      // Now set derivative directions. Note that this takes care
      // of the case when dima == dimb
      order[dima] = order[dima] + 1;
      order[dimb] = order[dimb] + 1;

      // Set order and update
      m_DerivativeFilter->SetOrder(order);
      m_DerivativeFilter->Update();
      typename RealImageType::Pointer derivativeImage;
      derivativeImage = m_DerivativeFilter->GetOutput();

      progress->ResetFilterProgressAndKeepAccumulatedProgress();

      // Copy the results to the corresponding component
      // on the output image of vectors
      m_ImageAdaptor->SelectNthElement(element++);

      ImageRegionIteratorWithIndex< RealImageType > it(
        derivativeImage,
        derivativeImage->GetRequestedRegion() );

      ImageRegionIteratorWithIndex< OutputImageAdaptorType > ot(
        m_ImageAdaptor,
        m_ImageAdaptor->GetRequestedRegion() );

      const RealType spacingA = inputImage->GetSpacing()[dima];
      const RealType spacingB = inputImage->GetSpacing()[dimb];

      const RealType factor = spacingA * spacingB;

      it.GoToBegin();
      ot.GoToBegin();
      while ( !it.IsAtEnd() )
        {
        ot.Set(it.Get() / factor);
        ++it;
        ++ot;
        }

      derivativeImage->ReleaseData();
      }
    }
}

template< typename TInputImage, typename TOutputImage >
void
HessianGaussianImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "DerivativeFilter: " << m_DerivativeFilter << std::endl;
}

} // end namespace itk

#endif // itkHessianGaussianImageFilter_hxx
