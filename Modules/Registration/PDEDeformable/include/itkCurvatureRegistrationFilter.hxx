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
#ifndef itkCurvatureRegistrationFilter_hxx
#define itkCurvatureRegistrationFilter_hxx
#if defined( ITK_USE_FFTWF ) || defined( ITK_USE_FFTWD )
#include "itkCurvatureRegistrationFilter.h"

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"

#include "itkMath.h"

namespace itk
{
/**
 * Default constructor
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TImageForceFunction >
CurvatureRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField, TImageForceFunction >
::CurvatureRegistrationFilter()
{
  typename RegistrationFunctionType::Pointer drfp;
  drfp = RegistrationFunctionType::New();

  this->SetDifferenceFunction( static_cast< FiniteDifferenceFunctionType * >(
                                 drfp.GetPointer() ) );

  this->SetTimeStep(100.0);
  this->SetConstraintWeight(0.01);

  m_PlanBackwardDCT = m_PlanForwardDCT = ITK_NULLPTR;
  m_DisplacementFieldComponentImage = m_DisplacementFieldComponentImageDCT = ITK_NULLPTR;

  for ( unsigned int dim = 0; dim < ImageDimension; ++dim )
    {
    m_DiagonalElements[dim] = ITK_NULLPTR;
    }
}

/**
 * Destructor.
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TImageForceFunction >
CurvatureRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField, TImageForceFunction >
::~CurvatureRegistrationFilter()
{
  if ( m_PlanForwardDCT )
    {
    fftw_destroy_plan(m_PlanForwardDCT);
    }
  if ( m_PlanBackwardDCT )
    {
    fftw_destroy_plan(m_PlanBackwardDCT);
    }

  for ( unsigned int dim = 0; dim < ImageDimension; ++dim )
    {
    delete[] m_DiagonalElements[dim];
    }
}

template< typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TImageForceFunction >
void
CurvatureRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField, TImageForceFunction >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

/**
 * Set the function state values before each iteration
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TImageForceFunction >
void
CurvatureRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField, TImageForceFunction >
::Initialize()
{
  RegistrationFunctionType *drfp =
    dynamic_cast< RegistrationFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to CurvatureRegistrationFunction");
    }

  drfp->SetDisplacementField( this->GetDisplacementField() );

  const SizeValueType numberOfPixels = this->GetFixedImage()->GetLargestPossibleRegion().GetNumberOfPixels();

  // allocate temporary storage for DCT, potentially aligned for SIMD processing
  if ( m_DisplacementFieldComponentImage )
    {
    fftw_free(m_DisplacementFieldComponentImage);
    }
  m_DisplacementFieldComponentImage = static_cast< RealTypeDFT * >( fftw_malloc( numberOfPixels * sizeof( RealTypeDFT ) ) );

  if ( m_DisplacementFieldComponentImageDCT )
    {
    fftw_free(m_DisplacementFieldComponentImageDCT);
    }
  m_DisplacementFieldComponentImageDCT = static_cast< RealTypeDFT * >( fftw_malloc( numberOfPixels * sizeof( RealTypeDFT ) ) );

  fftw_r2r_kind fftForward[ImageDimension];
  fftw_r2r_kind fftBackward[ImageDimension];

  int fixedImageDimensionsFFTW[ImageDimension];
  for ( unsigned int dim = 0; dim < ImageDimension; ++dim )
    {
    const unsigned int currSize = this->GetFixedImage()->GetLargestPossibleRegion().GetSize(dim);
    m_FixedImageDimensions[dim] = currSize;
    fixedImageDimensionsFFTW[ImageDimension - 1 - dim] = static_cast< int >( currSize ); //
                                                                                         // reverse
                                                                                         // order
                                                                                         // for
                                                                                         // FFTW!

#ifdef SLOW_DCT
    fftForward[dim] = FFTW_REDFT00;
    fftBackward[dim] = FFTW_REDFT00;
#else
/**
 * There is something funky going on numerically with DCTs, so FFTW
 * gives us the choice to either take a factor-2 performance hit
 * (which is what we're doing) or use the following: */
    fftForward[dim] = FFTW_REDFT01;
    fftBackward[dim] = FFTW_REDFT10;

    /**
     * Not sure whether the latter works, since it involves different boundary
     * conditions that my screw us. Also, is there a shift of the DCT coefficients
     * that would require us to change the computation of the filter elements in
     * m_DiagonalElements below??
     *
     *  Strangely enough, the fast method seems to produce better results. */
#endif
    }

  if ( m_PlanForwardDCT )
    {
    fftw_destroy_plan(m_PlanForwardDCT);
    }
  m_PlanForwardDCT = fftw_plan_r2r
                       (ImageDimension, fixedImageDimensionsFFTW, m_DisplacementFieldComponentImage,
                       m_DisplacementFieldComponentImageDCT, fftForward, FFTW_MEASURE | FFTW_DESTROY_INPUT);

  if ( m_PlanBackwardDCT )
    {
    fftw_destroy_plan(m_PlanBackwardDCT);
    }
  m_PlanBackwardDCT = fftw_plan_r2r
                        (ImageDimension, fixedImageDimensionsFFTW, m_DisplacementFieldComponentImageDCT,
                        m_DisplacementFieldComponentImage, fftBackward, FFTW_MEASURE | FFTW_DESTROY_INPUT);

  // compute components of diagonal matrix elements
  for ( unsigned int dim = 0; dim < ImageDimension; ++dim )
    {
    delete[] m_DiagonalElements[dim];
    m_DiagonalElements[dim] = new RealTypeDFT[m_FixedImageDimensions[dim]];
    for ( unsigned int idx = 0; idx < m_FixedImageDimensions[dim]; ++idx )
      {
#ifdef SLOW_DCT
      m_DiagonalElements[dim][idx] = -2 + 2 * std::cos(itk::Math::pi * idx / m_FixedImageDimensions[dim]);
#else
      m_DiagonalElements[dim][idx] = -2 + 2 * std::cos(itk::Math::pi * ( idx + 1 ) / m_FixedImageDimensions[dim]);
#endif
      }
    }

  // call the superclass  implementation
  Superclass::Initialize();
}

/*
 * Get the metric value from the difference function
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TImageForceFunction >
double
CurvatureRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField, TImageForceFunction >
::GetMetric() const
{
  RegistrationFunctionType *drfp =
    dynamic_cast< RegistrationFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to CurvatureRegistrationFunction");
    }

  return drfp->GetMetric();
}

/*
 * Get the metric value from the difference function
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TImageForceFunction >
void
CurvatureRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField, TImageForceFunction >
::ApplyUpdate(const TimeStepType& dt)
{
  // unused dt parameter
  (void)dt;
  DisplacementFieldPointer update = this->GetUpdateBuffer();

  ImageRegionConstIterator< DisplacementFieldType >    itInDeformation;
  ImageRegionIterator< DisplacementFieldType >         itOutDeformation;
  ImageRegionConstIterator< DisplacementFieldType >    itInUpdate;
  ImageRegionConstIteratorWithIndex< FixedImageType > fixedImageIteratorWithIndex;

  itInDeformation = ImageRegionConstIterator< DisplacementFieldType >
                      ( this->GetDisplacementField(), this->GetDisplacementField()->GetLargestPossibleRegion() );
  itOutDeformation = ImageRegionIterator< DisplacementFieldType >
                       ( this->GetDisplacementField(), this->GetDisplacementField()->GetLargestPossibleRegion() );
  itInUpdate = ImageRegionConstIterator< DisplacementFieldType >
                 ( update, update->GetLargestPossibleRegion() );
  fixedImageIteratorWithIndex = ImageRegionConstIteratorWithIndex< FixedImageType >
                                  ( this->GetFixedImage(), this->GetFixedImage()->GetLargestPossibleRegion() );

  const SizeValueType numberOfPixels = this->GetFixedImage()->GetLargestPossibleRegion().GetNumberOfPixels();

  RealTypeDFT normFactorDCT =
#ifdef SLOW_DCT
    1.0 / ( numberOfPixels - 1 ); // norm factor for fw/bw DCT
#else
    1.0 / numberOfPixels; // norm factor for fw/bw DCT
#endif
  for ( unsigned int dim = 0; dim < ImageDimension; ++dim )
    {
    normFactorDCT *= 0.5;
    }

  for ( unsigned int l = 0; l < DeformationVectorDimension; ++l )
    {
    // extract l-th component of deformation field.
    itInDeformation.GoToBegin();
    itInUpdate.GoToBegin();
    for ( SizeValueType offset1 = 0;
          ( offset1 < numberOfPixels ) && !itInDeformation.IsAtEnd() && !itInUpdate.IsAtEnd();
          ++offset1 )
      {
      this->m_DisplacementFieldComponentImage[offset1] = this->m_TimeStep *itInUpdate. Value()[l]
                                                        + itInDeformation.Value()[l];
      ++itInUpdate;
      ++itInDeformation;
      }

    // run DFT
    fftw_execute(this->m_PlanForwardDCT);

    // multiply matrix diagonal elements
    fixedImageIteratorWithIndex.GoToBegin();
    SizeValueType offset = 0;
    while ( !fixedImageIteratorWithIndex.IsAtEnd() )
      {
      typename TFixedImage::IndexType index = fixedImageIteratorWithIndex.GetIndex();
      RealTypeDFT d = 0;
      for ( unsigned int dim = 0; dim < ImageDimension; ++dim )
        {
        d += m_DiagonalElements[dim][index[dim]];
        }
      d *= ( d * this->m_TimeStep * this->m_ConstraintWeight );
      d += 1.0;
      this->m_DisplacementFieldComponentImageDCT[offset++] /= d;
      ++fixedImageIteratorWithIndex;
      }

    // run (inverse) DFT
    fftw_execute(this->m_PlanBackwardDCT);

    // update deformation field
    itOutDeformation.GoToBegin();
    for ( SizeValueType offset1 = 0; ( offset1 < numberOfPixels ) && !itOutDeformation.IsAtEnd(); ++offset1 )
      {
      itOutDeformation.Value()[l] = this->m_DisplacementFieldComponentImage[offset1] * normFactorDCT;
      ++itOutDeformation;
      }
    }

  this->GetDisplacementField()->Modified();
}
} // end namespace itk

#endif //defined(ITK_USE_FFTWF) || defined(ITK_USE_FFTWD)
#endif
