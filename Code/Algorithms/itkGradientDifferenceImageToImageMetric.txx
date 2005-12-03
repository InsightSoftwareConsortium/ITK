/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkGradientDifferenceImageToImageMetric.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkGradientDifferenceImageToImageMetric_txx
#define _itkGradientDifferenceImageToImageMetric_txx

#include "itkGradientDifferenceImageToImageMetric.h"
#include "itkImageRegionConstIteratorWithIndex.h"

#include <iostream>
#include <iomanip>
#include <stdio.h>

#include "itkSimpleFilterWatcher.h"
namespace itk
{

/*
 * Constructor
 */
template <class TFixedImage, class TMovingImage> 
GradientDifferenceImageToImageMetric<TFixedImage,TMovingImage>
::GradientDifferenceImageToImageMetric()
{
  unsigned int iDimension;

  m_TransformMovingImageFilter = 0;

  for (iDimension=0; iDimension<FixedImageDimension; iDimension++) 
    { 
    m_MinFixedGradient[iDimension] = 0;
    m_MaxFixedGradient[iDimension] = 0;

    m_Variance[iDimension] = 0;
    }

  for (iDimension=0; iDimension<MovedImageDimension; iDimension++) 
    {
    m_MinMovedGradient[iDimension] = 0;
    m_MaxMovedGradient[iDimension] = 0;
    }
}


/*
 * Initialize
 */
template <class TFixedImage, class TMovingImage> 
void
GradientDifferenceImageToImageMetric<TFixedImage,TMovingImage>
::Initialize(void) throw ( ExceptionObject )
{
  unsigned int iFilter;  // Index of Sobel filters for each dimension


  if ( ! this->GetComputeGradient() )
    {
    itkExceptionMacro(<<"Gradients must be calculated");
    }

  // Initialise the base class

  Superclass::Initialize();


  // Create the filter to resample the moving image

  m_TransformMovingImageFilter = TransformMovingImageFilterType::New();

  m_TransformMovingImageFilter->SetTransform(    this->m_Transform );
  m_TransformMovingImageFilter->SetInterpolator( this->m_Interpolator );
  m_TransformMovingImageFilter->SetInput( this->m_MovingImage );

  m_TransformMovingImageFilter->SetDefaultPixelValue( 0 );

  m_TransformMovingImageFilter->SetSize( this->m_FixedImage->GetLargestPossibleRegion().GetSize() );
  m_TransformMovingImageFilter->SetOutputOrigin( this->m_FixedImage->GetOrigin() );
  m_TransformMovingImageFilter->SetOutputSpacing( this->m_FixedImage->GetSpacing() );
  m_TransformMovingImageFilter->SetOutputDirection( this->m_FixedImage->GetDirection() );


  // Compute the image gradients
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~

  // Compute the gradient of the fixed image


  m_CastFixedImageFilter = CastFixedImageFilterType::New();
  m_CastFixedImageFilter->SetInput( this->m_FixedImage );

  for (iFilter=0; iFilter<FixedImageDimension; iFilter++)
    {
    m_FixedSobelOperators[iFilter].SetDirection( iFilter );
    m_FixedSobelOperators[iFilter].CreateDirectional();

    m_FixedSobelFilters[iFilter] = FixedSobelFilter::New();

    m_FixedSobelFilters[iFilter]->OverrideBoundaryCondition( &m_FixedBoundCond );
    m_FixedSobelFilters[iFilter]->SetOperator( m_FixedSobelOperators[iFilter] );

    m_FixedSobelFilters[iFilter]->SetInput( m_CastFixedImageFilter->GetOutput() );

    m_FixedSobelFilters[iFilter]->UpdateLargestPossibleRegion();
    }

  ComputeVariance();

  // Compute the gradient of the transformed moving image

  m_CastMovedImageFilter = CastMovedImageFilterType::New();
  m_CastMovedImageFilter->SetInput( m_TransformMovingImageFilter->GetOutput() );

  for (iFilter=0; iFilter<MovedImageDimension; iFilter++) 
    {
    m_MovedSobelOperators[iFilter].SetDirection( iFilter );
    m_MovedSobelOperators[iFilter].CreateDirectional();

    m_MovedSobelFilters[iFilter] = MovedSobelFilter::New();

    m_MovedSobelFilters[iFilter]->OverrideBoundaryCondition( &m_MovedBoundCond );
    m_MovedSobelFilters[iFilter]->SetOperator( m_MovedSobelOperators[iFilter] );

    m_MovedSobelFilters[iFilter]->SetInput( m_CastMovedImageFilter->GetOutput() );

    m_MovedSobelFilters[iFilter]->UpdateLargestPossibleRegion();
    }

}


/*
 * PrintSelf
 */
template <class TFixedImage, class TMovingImage> 
void
GradientDifferenceImageToImageMetric<TFixedImage,TMovingImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
}



/*
 * Compute the range of the moved image gradients
 */
template <class TFixedImage, class TMovingImage> 
void
GradientDifferenceImageToImageMetric<TFixedImage,TMovingImage>
::ComputeMovedGradientRange( void ) const
{
  unsigned int iDimension;
  MovedGradientPixelType gradient;


  for (iDimension=0; iDimension<FixedImageDimension; iDimension++) 
    {

    typedef itk::ImageRegionConstIteratorWithIndex< 
      MovedGradientImageType > IteratorType;

    IteratorType iterate( m_MovedSobelFilters[iDimension]->GetOutput(),
                          this->GetFixedImageRegion() );

    gradient = iterate.Get();

    m_MinMovedGradient[iDimension] = gradient;
    m_MaxMovedGradient[iDimension] = gradient;

    while ( ! iterate.IsAtEnd() ) 
      {

      gradient = iterate.Get();
    
      if (gradient > m_MaxMovedGradient[iDimension])
        {
        m_MaxMovedGradient[iDimension] = gradient;
        }

      if (gradient < m_MinMovedGradient[iDimension])
        {
        m_MinMovedGradient[iDimension] = gradient;
        }


      ++iterate;
      }
    }
}


/*
 * Compute the gradient variances in each dimension.
 */
template <class TFixedImage, class TMovingImage> 
void
GradientDifferenceImageToImageMetric<TFixedImage,TMovingImage>
::ComputeVariance( void ) const
{
  unsigned int iDimension;
  unsigned long nPixels;
  FixedGradientPixelType mean[FixedImageDimension];
  FixedGradientPixelType gradient;


  for (iDimension=0; iDimension<FixedImageDimension; iDimension++) 
    {

    typedef itk::ImageRegionConstIteratorWithIndex< 
      FixedGradientImageType > IteratorType;

    IteratorType iterate( m_FixedSobelFilters[iDimension]->GetOutput(),
                          this->GetFixedImageRegion() );

    // Calculate the mean gradients

    nPixels =  0;

    gradient = iterate.Get();


    mean[iDimension] = 0;

    m_MinMovedGradient[iDimension] = gradient;
    m_MaxMovedGradient[iDimension] = gradient;


    while ( ! iterate.IsAtEnd() ) 
      {

      gradient = iterate.Get();
      mean[iDimension] += gradient;
    
      if (gradient > m_MaxFixedGradient[iDimension]) 
        {
        m_MaxFixedGradient[iDimension] = gradient;
        }

      if (gradient < m_MinFixedGradient[iDimension])
        {
        m_MinFixedGradient[iDimension] = gradient;
        }
    
      nPixels++;
      ++iterate;
      }

    if (nPixels > 0) mean[iDimension] /= nPixels;

    // Calculate the variance

    iterate.GoToBegin();

    m_Variance[iDimension] = 0;

    while ( ! iterate.IsAtEnd() ) 
      {

      gradient = iterate.Get();
      gradient -= mean[iDimension];

      m_Variance[iDimension] += gradient*gradient;

      ++iterate;
      }

    m_Variance[iDimension] /= nPixels;
    }
}


/*
 * Get the value of the similarity measure
 */
template <class TFixedImage, class TMovingImage> 
typename GradientDifferenceImageToImageMetric<TFixedImage,TMovingImage>::MeasureType
GradientDifferenceImageToImageMetric<TFixedImage,TMovingImage>
::ComputeMeasure( const TransformParametersType & parameters,
                  const double *subtractionFactor ) const
{
  unsigned int iDimension;

  this->SetTransformParameters( parameters );
  m_TransformMovingImageFilter->UpdateLargestPossibleRegion();
  MeasureType measure = NumericTraits< MeasureType >::Zero;

  for (iDimension=0; iDimension<FixedImageDimension; iDimension++) 
    {
    if (m_Variance[iDimension] == NumericTraits< MovedGradientPixelType >::Zero)
      {
      continue;
      }
    // Iterate over the fixed and moving gradient images
    // calculating the similarity measure

    MovedGradientPixelType movedGradient;
    FixedGradientPixelType fixedGradient;

    MovedGradientPixelType diff;

    typedef  itk::ImageRegionConstIteratorWithIndex< FixedGradientImageType > 
      FixedIteratorType;

    FixedIteratorType fixedIterator( m_FixedSobelFilters[iDimension]->GetOutput(),
                                     this->GetFixedImageRegion() );

    typedef  itk::ImageRegionConstIteratorWithIndex< MovedGradientImageType > 
      MovedIteratorType;

    MovedIteratorType movedIterator( m_MovedSobelFilters[iDimension]->GetOutput(),
                                     this->GetFixedImageRegion() );

    m_FixedSobelFilters[iDimension]->UpdateLargestPossibleRegion();
    m_MovedSobelFilters[iDimension]->UpdateLargestPossibleRegion();

    this->m_NumberOfPixelsCounted = 0;

    while ( ! fixedIterator.IsAtEnd() ) 
      {

      // Get the moving and fixed image gradients
  
      movedGradient = movedIterator.Get();
      fixedGradient  = fixedIterator.Get();

      // And calculate the gradient difference

      diff = fixedGradient - subtractionFactor[iDimension]*movedGradient; 
        
      measure += m_Variance[iDimension] 
        / ( m_Variance[iDimension] + diff*diff ); 

      ++fixedIterator;
      ++movedIterator;
      }
    }
  
  return measure;
}


/*
 * Get the value of the similarity measure
 */
template <class TFixedImage, class TMovingImage> 
typename GradientDifferenceImageToImageMetric<TFixedImage,TMovingImage>::MeasureType
GradientDifferenceImageToImageMetric<TFixedImage,TMovingImage>
::GetValue( const TransformParametersType & parameters ) const
{
  unsigned int iFilter;                        // Index of Sobel filters for each dimension
  bool maximumFound = false;
  bool firstIteration = true;
  unsigned int iDimension;

  this->SetTransformParameters( parameters );
  m_TransformMovingImageFilter->UpdateLargestPossibleRegion();

  // Update the gradient images

  for (iFilter=0; iFilter<MovedImageDimension; iFilter++) 
    {
    m_MovedSobelFilters[iFilter]->UpdateLargestPossibleRegion();
    }

  // Compute the range of the moved image gradients
  // NB: Ideally this should be a filter as the computation is only 
  //     required if the moved gradient image has been updated. 
  //     However for the moment we'll assume that this is the case
  //     whenever GetValue() is called. 

  ComputeMovedGradientRange();

  // Compute the scale factor step size

  MovedGradientPixelType stepSize[FixedImageDimension];

  for (iDimension=0; iDimension<FixedImageDimension; iDimension++)
    {
    if (m_MaxMovedGradient[iDimension] != m_MinMovedGradient[iDimension])
      {
      stepSize[iDimension] = 
        ((m_MaxFixedGradient[iDimension] - m_MinFixedGradient[iDimension])
         / (m_MaxMovedGradient[iDimension] - m_MinMovedGradient[iDimension]))/50.;
      }
    else
      {
      stepSize[iDimension] = 0;
      }
    }
  
  // Compute the similarity measure

  MovedGradientPixelType subtractionFactor[FixedImageDimension];
  MeasureType currentMeasure;
  MeasureType maxMeasure;

  for (iDimension=0; iDimension<FixedImageDimension; iDimension++)
    {
    subtractionFactor[iDimension] = 0.;
    }

  while (! maximumFound) 
    {
    if (! firstIteration) 
      {
      for (iDimension=0; iDimension<FixedImageDimension; iDimension++)
        {
        subtractionFactor[iDimension] += stepSize[iDimension];
        }
      }

    // Compute the new value of the measure for this subtraction factor

    currentMeasure = ComputeMeasure( parameters, subtractionFactor );

    if (firstIteration)
      {
      maxMeasure = currentMeasure;
      firstIteration = false;
      }
    else 
      {
      if (currentMeasure > maxMeasure) 
        {  
        maxMeasure = currentMeasure;
        }

      else 
        {
        maximumFound = true;

        for (iDimension=0; iDimension<FixedImageDimension; iDimension++)
          {
          subtractionFactor[iDimension] -= stepSize[iDimension];
          }
        }
      }
    }

  return maxMeasure;
}



/*
 * Get the Derivative Measure
 */
template < class TFixedImage, class TMovingImage> 
void
GradientDifferenceImageToImageMetric<TFixedImage,TMovingImage>
::GetDerivative( const TransformParametersType & parameters, 
                 DerivativeType & derivative           ) const
{

  const double delta = 0.001;
  TransformParametersType testPoint;
  testPoint = parameters;

  const unsigned int numberOfParameters = this->GetNumberOfParameters();
  derivative = DerivativeType( numberOfParameters );

  for( unsigned int i=0; i<numberOfParameters; i++) 
    {
    testPoint[i] -= delta;
    const MeasureType valuep0 = this->GetValue( testPoint );
    testPoint[i] += 2*delta;
    const MeasureType valuep1 = this->GetValue( testPoint );
    derivative[i] = (valuep1 - valuep0 ) / ( 2 * delta );
    testPoint[i] = parameters[i];
    }
}


/*
 * Get both the match Measure and theDerivative Measure 
 */
template <class TFixedImage, class TMovingImage> 
void
GradientDifferenceImageToImageMetric<TFixedImage,TMovingImage>
::GetValueAndDerivative(const TransformParametersType & parameters, 
                        MeasureType & Value, DerivativeType  & Derivative) const
{
  Value      = this->GetValue( parameters );
  this->GetDerivative( parameters, Derivative );
}

} // end namespace itk


#endif
