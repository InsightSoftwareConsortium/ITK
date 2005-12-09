/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCurvatureRegistrationFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(USE_FFTWF) || defined(USE_FFTWD)
#ifndef _itkCurvatureRegistrationFilter_txx
#define _itkCurvatureRegistrationFilter_txx
#include "itkCurvatureRegistrationFilter.h"

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"

#include "vnl/vnl_math.h"

namespace itk {

/*
 * Default constructor
 */
template <class TFixedImage, class TMovingImage, class TDeformationField, class TImageForceFunction>
CurvatureRegistrationFilter<TFixedImage,TMovingImage,TDeformationField,TImageForceFunction>
::CurvatureRegistrationFilter()
{
 
  typename RegistrationFunctionType::Pointer drfp;
  drfp = RegistrationFunctionType::New();

  this->SetDifferenceFunction( static_cast<FiniteDifferenceFunctionType *>(
                                 drfp.GetPointer() ) );

  this->SetTimeStep( 100.0 );
  this->SetConstraintWeight( 0.01 );

  m_PlanBackwardDCT = m_PlanForwardDCT = NULL;
  m_DeformationFieldComponentImage = m_DeformationFieldComponentImageDCT = NULL;

  for ( int dim = 0; dim < ImageDimension; ++dim )
    {
    m_DiagonalElements[dim] = NULL;
    }
}


/*
 * Destructor.
 */
template <class TFixedImage, class TMovingImage, class TDeformationField, class TImageForceFunction>
CurvatureRegistrationFilter<TFixedImage,TMovingImage,TDeformationField,TImageForceFunction>
::~CurvatureRegistrationFilter()
{
  if ( m_PlanForwardDCT )
    fftw_destroy_plan( m_PlanForwardDCT );
  if ( m_PlanBackwardDCT )
    fftw_destroy_plan( m_PlanBackwardDCT );

  for ( int dim = 0; dim < ImageDimension; ++dim )
    if ( m_DiagonalElements[dim] )
      delete[] m_DiagonalElements[dim];
}


template <class TFixedImage, class TMovingImage, class TDeformationField, class TImageForceFunction>
void
CurvatureRegistrationFilter<TFixedImage,TMovingImage,TDeformationField,TImageForceFunction>
::PrintSelf(std::ostream& os, Indent indent) const
{ 
  Superclass::PrintSelf( os, indent );
}

/*
 * Set the function state values before each iteration
 */
template <class TFixedImage, class TMovingImage, class TDeformationField, class TImageForceFunction>
void
CurvatureRegistrationFilter<TFixedImage,TMovingImage,TDeformationField,TImageForceFunction>
::Initialize()
{
  RegistrationFunctionType *drfp = 
    dynamic_cast<RegistrationFunctionType *>
      (this->GetDifferenceFunction().GetPointer());
 
  if( !drfp )
   {
   itkExceptionMacro( << 
     "Could not cast difference function to CurvatureRegistrationFunction" );
   }
   
  drfp->SetDeformationField( this->GetDeformationField() );

  const size_t numberOfPixels = this->GetFixedImage()->GetLargestPossibleRegion().GetNumberOfPixels();

  // allocate temporary storage for DCT, potentially aligned for SIMD processing
  if ( m_DeformationFieldComponentImage )
    fftw_free( m_DeformationFieldComponentImage );
  m_DeformationFieldComponentImage = static_cast<RealTypeDFT*>( fftw_malloc( numberOfPixels * sizeof( RealTypeDFT ) ) );

  if ( m_DeformationFieldComponentImageDCT )
    fftw_free( m_DeformationFieldComponentImageDCT );
  m_DeformationFieldComponentImageDCT = static_cast<RealTypeDFT*>( fftw_malloc( numberOfPixels * sizeof( RealTypeDFT ) ) );

  fftw_r2r_kind fftForward[ImageDimension];
  fftw_r2r_kind fftBackward[ImageDimension];

  int fixedImageDimensionsFFTW[ImageDimension];
  for ( int dim = 0; dim < ImageDimension; ++dim )
    {
    m_FixedImageDimensions[dim] = fixedImageDimensionsFFTW[ImageDimension-1-dim] = // reverse order for FFTW!
      this->GetFixedImage()->GetLargestPossibleRegion().GetSize( dim );

//#define SLOW_DCT
#ifdef SLOW_DCT
    fftForward[dim] = FFTW_REDFT00;
    fftBackward[dim] = FFTW_REDFT00;
#else
/*
    There is something funky going on numerically with DCTs, so FFTW 
    gives us the choice to either take a factor-2 performance hit
    (which is what we're doing) or use the following:
*/
    fftForward[dim] = FFTW_REDFT01;
    fftBackward[dim] = FFTW_REDFT10;
/*
    Not sure whether the latter works, since it involves different boundary
    conditions that my screw us. Also, is there a shift of the DCT coefficients
    that would require us to change the computation of the filter elements in
    m_DiagonalElements below??

    Strangely enough, the fast method seems to produce better results.
*/
#endif
    }

  if ( m_PlanForwardDCT )
    fftw_destroy_plan( m_PlanForwardDCT );
  m_PlanForwardDCT = fftw_plan_r2r
    ( ImageDimension, fixedImageDimensionsFFTW, m_DeformationFieldComponentImage, 
      m_DeformationFieldComponentImageDCT, fftForward, FFTW_MEASURE | FFTW_DESTROY_INPUT );

  if ( m_PlanBackwardDCT )
    fftw_destroy_plan( m_PlanBackwardDCT );
  m_PlanBackwardDCT = fftw_plan_r2r
    ( ImageDimension, fixedImageDimensionsFFTW, m_DeformationFieldComponentImageDCT, 
      m_DeformationFieldComponentImage, fftBackward, FFTW_MEASURE | FFTW_DESTROY_INPUT ); 

  // compute components of diagonal matrix elements
  for ( int dim = 0; dim < ImageDimension; ++dim )
    {
    if ( m_DiagonalElements[dim] )
      delete[] m_DiagonalElements[dim];
    m_DiagonalElements[dim] = new RealTypeDFT[m_FixedImageDimensions[dim]];
    for ( int idx = 0; idx < m_FixedImageDimensions[dim]; ++idx )
      {
#ifdef SLOW_DCT
      m_DiagonalElements[dim][idx] = -2 + 2 * cos( vnl_math::pi * idx / m_FixedImageDimensions[dim] );
#else
      m_DiagonalElements[dim][idx] = -2 + 2 * cos( vnl_math::pi * (idx+1) / m_FixedImageDimensions[dim] );
#endif
      }
    }

  // call the superclass  implementation
  Superclass::Initialize();
}


/*
 * Get the metric value from the difference function
 */
template <class TFixedImage, class TMovingImage, class TDeformationField, class TImageForceFunction>
double
CurvatureRegistrationFilter<TFixedImage,TMovingImage,TDeformationField,TImageForceFunction>
::GetMetric() const
{
 
  RegistrationFunctionType *drfp = 
    dynamic_cast<RegistrationFunctionType *>
      (this->GetDifferenceFunction().GetPointer());
 
  if( !drfp )
   {
   itkExceptionMacro( << 
     "Could not cast difference function to CurvatureRegistrationFunction" );
   }
   
  return drfp->GetMetric(); 
}


/*
 * Get the metric value from the difference function
 */
template <class TFixedImage, class TMovingImage, class TDeformationField, class TImageForceFunction>
void
CurvatureRegistrationFilter<TFixedImage,TMovingImage,TDeformationField,TImageForceFunction>
::ApplyUpdate(TimeStepType dt)
{
  DeformationFieldPointer update = this->GetUpdateBuffer();

  ImageRegionConstIterator<DeformationFieldType> itInDeformation;
  ImageRegionIterator<DeformationFieldType> itOutDeformation;
  ImageRegionConstIterator<DeformationFieldType> itInUpdate;
  ImageRegionConstIteratorWithIndex<FixedImageType> fixedImageIteratorWithIndex;

  itInDeformation = ImageRegionConstIterator<DeformationFieldType>
    ( this->GetDeformationField(), this->GetDeformationField()->GetLargestPossibleRegion() );
  itOutDeformation = ImageRegionIterator<DeformationFieldType>
    ( this->GetDeformationField(), this->GetDeformationField()->GetLargestPossibleRegion() );
  itInUpdate = ImageRegionConstIterator<DeformationFieldType>
    ( update, update->GetLargestPossibleRegion() );
  fixedImageIteratorWithIndex = ImageRegionConstIteratorWithIndex<FixedImageType>
    ( this->GetFixedImage(), this->GetFixedImage()->GetLargestPossibleRegion() );

  const size_t numberOfPixels = this->GetFixedImage()->GetLargestPossibleRegion().GetNumberOfPixels();

  RealTypeDFT normFactorDCT = 
#ifdef SLOW_DCT
    1.0 / (numberOfPixels-1); // norm factor for fw/bw DCT
#else
    1.0 / numberOfPixels; // norm factor for fw/bw DCT
#endif
  for ( int dim = 0; dim < ImageDimension; ++dim )
    normFactorDCT *= 0.5;

  for ( unsigned int l = 0; l < DeformationVectorDimension; ++l )
    {
    // extract l-th component of deformation field.
    itInDeformation.GoToBegin();
    itInUpdate.GoToBegin();
    for ( size_t offset = 0; (offset < numberOfPixels) && !itInDeformation.IsAtEnd() && !itInUpdate.IsAtEnd(); ++offset )
      {
      this->m_DeformationFieldComponentImage[offset] = this->m_TimeStep * itInUpdate.Value()[l] + itInDeformation.Value()[l];
      ++itInUpdate;
      ++itInDeformation;
      }

    // run DFT
    fftw_execute( this->m_PlanForwardDCT );

    // multiply matrix diagonal elements
    fixedImageIteratorWithIndex.GoToBegin();
    size_t offset = 0;
    while ( ! fixedImageIteratorWithIndex.IsAtEnd() )
      {
      typename TFixedImage::IndexType index = fixedImageIteratorWithIndex.GetIndex();
      RealTypeDFT d = 0;
      for ( int dim = 0; dim < ImageDimension; ++dim )
        {
        d += m_DiagonalElements[dim][ index[ dim ] ];
        }
      d *= (d * this->m_TimeStep * this->m_ConstraintWeight);
      d += 1.0;
      this->m_DeformationFieldComponentImageDCT[offset++] /= d;
      ++fixedImageIteratorWithIndex;
      }

    // run (inverse) DFT
    fftw_execute( this->m_PlanBackwardDCT );

    // update deformation field
    itOutDeformation.GoToBegin();
    for ( size_t offset = 0; (offset < numberOfPixels) && ! itOutDeformation.IsAtEnd(); ++offset )
      {
      itOutDeformation.Value()[l] = this->m_DeformationFieldComponentImage[offset] * normFactorDCT;
      ++itOutDeformation;
      }
    }

  this->GetDeformationField()->Modified();
}



} // end namespace itk

#endif
#endif //defined(USE_FFTWF) || defined(USE_FFTWD)
