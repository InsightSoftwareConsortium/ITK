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


namespace itk
{

/*
 * Constructor
 */
template <class TFixedImage, class TMovingImage> 
GradientDifferenceImageToImageMetric<TFixedImage,TMovingImage>
::GradientDifferenceImageToImageMetric()
{
  int iDimension;

  m_MovedImage = 0;
  m_TransformMovingImageFilter = 0;

  for (iDimension=0; iDimension<FixedImageDimension; iDimension++) 
    { 
    m_FixedGradientImage[iDimension] = 0; 
    
    m_MinFixedGradient[iDimension] = 0;
    m_MaxFixedGradient[iDimension] = 0;

    m_Variance[iDimension] = 0;
    }

  for (iDimension=0; iDimension<MovedImageDimension; iDimension++) 
    {
    m_MovedGradientImage[iDimension] = 0; 

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
  int iFilter;                        // Index of Sobel filters for each dimension


  if ( ! m_ComputeGradient ) {
     itkExceptionMacro(<<"Gradients must be calculated");
  }

  // Initialise the base class

  Superclass::Initialize();


  // Create the filter to resample the moving image

  m_TransformMovingImageFilter = TransformMovingImageFilterType::New();

  m_TransformMovingImageFilter->SetTransform(    m_Transform );
  m_TransformMovingImageFilter->SetInterpolator( m_Interpolator );

  m_TransformMovingImageFilter->SetDefaultPixelValue( 0 );

  m_TransformMovingImageFilter->SetOutputSpacing( m_FixedImage->GetSpacing() );
  m_TransformMovingImageFilter->SetOutputOrigin(  m_FixedImage->GetOrigin() );

  m_TransformMovingImageFilter->SetSize( m_FixedImage->GetLargestPossibleRegion().GetSize() );

  m_TransformMovingImageFilter->SetInput( m_MovingImage );

  m_MovedImage = m_TransformMovingImageFilter->GetOutput();


  // Compute the image gradients
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~

  // Compute the gradient of the fixed image


  for (iFilter=0; iFilter<FixedImageDimension; iFilter++)
    {
    m_CastFixedImageFilter = CastFixedImageFilterType::New();
    m_CastFixedImageFilter->SetInput( m_FixedImage );

    m_FixedSobelFilters[iFilter] = FixedSobelFilter::New();

    m_FixedSobelOperators[iFilter].SetDirection( iFilter );
    m_FixedSobelOperators[iFilter].CreateOperator();

    ZeroFluxNeumannBoundaryCondition< FixedGradientImageType > fixedBoundCond;

    m_FixedSobelFilters[iFilter]->OverrideBoundaryCondition( &fixedBoundCond );
    m_FixedSobelFilters[iFilter]->SetOperator( m_FixedSobelOperators[iFilter] );

    m_FixedSobelFilters[iFilter]->SetInput( m_CastFixedImageFilter->GetOutput() );

    m_FixedGradientImage[iFilter] = m_FixedSobelFilters[iFilter]->GetOutput();  

    m_FixedSobelFilters[iFilter]->Update();
    }

  ComputeVariance();

  // Compute the gradient of the transformed moving image


  for (iFilter=0; iFilter<MovedImageDimension; iFilter++) 
    {
    m_CastMovedImageFilter = CastMovedImageFilterType::New();
    m_CastMovedImageFilter->SetInput( m_MovedImage );

    m_MovedSobelFilters[iFilter] = MovedSobelFilter::New();

    m_MovedSobelOperators[iFilter].SetDirection( iFilter );
    m_MovedSobelOperators[iFilter].CreateOperator();

    ZeroFluxNeumannBoundaryCondition< MovedGradientImageType > movedBoundCond;

    m_MovedSobelFilters[iFilter]->OverrideBoundaryCondition( &movedBoundCond );
    m_MovedSobelFilters[iFilter]->SetOperator( m_MovedSobelOperators[iFilter] );

    m_MovedSobelFilters[iFilter]->SetInput( m_CastMovedImageFilter->GetOutput() );

    m_MovedGradientImage[iFilter] = m_MovedSobelFilters[iFilter]->GetOutput();  

    m_MovedSobelFilters[iFilter]->Update();
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

  unsigned int iDimension;
  for (iDimension=0; iDimension<FixedImageDimension; iDimension++) 
  {
    os << indent << "Moving Gradient Image, dimension " << iDimension 
       << ": " << m_MovedGradientImage[iDimension]->GetPointer() << std::endl;
    os << indent << "Fixed Gradient Image,  dimension " << iDimension 
       << ": " << m_FixedGradientImage[iDimension]->GetPointer() << std::endl;
  }
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

    IteratorType iterate( m_MovedGradientImage[iDimension], 
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

  cout << "Moved image range: ";
  for (iDimension=0; iDimension<FixedImageDimension; iDimension++)
    cout << " " << m_MinMovedGradient[iDimension] 
         << ":" << m_MaxMovedGradient[iDimension];
  cout << endl;
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

    IteratorType iterate( m_FixedGradientImage[iDimension], 
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

  cout << "Fixed image range: ";
  for (iDimension=0; iDimension<FixedImageDimension; iDimension++)
    cout << " " << m_MinFixedGradient[iDimension] << ":" << m_MaxFixedGradient[iDimension];
  cout << endl;
  
  cout << "Mean: ";
  for (iDimension=0; iDimension<FixedImageDimension; iDimension++)
    cout << " " << mean[iDimension];
  cout << endl;
  
  cout << "Variance: ";
  for (iDimension=0; iDimension<FixedImageDimension; iDimension++)
    cout << " " << m_Variance[iDimension];
  cout << endl;
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
  MeasureType measure = NumericTraits< MeasureType >::Zero;


  for (iDimension=0; iDimension<FixedImageDimension; iDimension++) 
    {

    // Iterate over the fixed and moving gradient images
    // calculating the similarity measure

    MovedGradientPixelType movedGradient;
    FixedGradientPixelType fixedGradient;

    MovedGradientPixelType diff;

    typedef  itk::ImageRegionConstIteratorWithIndex< FixedGradientImageType > 
                                                                FixedIteratorType;

    FixedIteratorType fixedIterator( m_FixedGradientImage[iDimension], 
                                     this->GetFixedImageRegion() );

    typename FixedImageType::IndexType index;

    m_NumberOfPixelsCounted = 0;

    while ( ! fixedIterator.IsAtEnd() ) 
      {

      index = fixedIterator.GetIndex();
    
      typename Superclass::InputPointType inputPoint;

      m_FixedGradientImage[iDimension]->TransformIndexToPhysicalPoint( index, 
                                                                       inputPoint );

      typename Superclass::OutputPointType transformedPoint 
        = m_Transform->TransformPoint( inputPoint );

      if( m_Interpolator->IsInsideBuffer( transformedPoint ) )
        {

          // Get the moving and fixed image gradients
  
          movedGradient = m_Interpolator->Evaluate( transformedPoint );
          fixedGradient  = fixedIterator.Get();

          m_NumberOfPixelsCounted++;

          // And calculate the gradient difference

          diff = fixedGradient - subtractionFactor[iDimension]*movedGradient; 

          cout << setw(8) << diff << " = " << setw(8) << fixedGradient << " - " << setw(8) << subtractionFactor[iDimension] << "*" << setw(8) << movedGradient << ";" << endl; 

          measure += m_Variance[iDimension] 
                          / ( m_Variance[iDimension] + diff*diff ); 
        }
    
      ++fixedIterator;
      }

    if( !m_NumberOfPixelsCounted )
      {
      itkExceptionMacro(<<"All the points mapped to outside of the moving image");
      }
    else
      {
      measure /= m_NumberOfPixelsCounted;
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
  int iFilter;                        // Index of Sobel filters for each dimension
  bool maximumFound = false;
  bool firstIteration = true;
  unsigned int nIterations = 0;
  unsigned int iDimension;


  // Update the gradient images

  for (iFilter=0; iFilter<MovedImageDimension; iFilter++) 
    {
      m_MovedSobelFilters[iFilter]->Update();
    }

  // Compute the range of the moved image gradients
  // NB: Ideally this should be a filter as the computation is only 
  //     required if the moved gradient image has been updated. 
  //     However for the moment we'll assume that this is the case
  //     whenever GetValue() is called. 

  ComputeMovedGradientRange();

  // Write the gradient images to a files

#if 1
  WriteGradientImagesToFiles();
#endif

  // Compute the scale factor step size

  MovedGradientPixelType stepSize[FixedImageDimension];

  for (iDimension=0; iDimension<FixedImageDimension; iDimension++)
    {
      stepSize[iDimension] = 
        ((m_MaxFixedGradient[iDimension] - m_MinFixedGradient[iDimension])
         / (m_MaxMovedGradient[iDimension] - m_MinMovedGradient[iDimension]))/50.;
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

    nIterations++;

    if (! firstIteration) 
      {
      for (iDimension=0; iDimension<FixedImageDimension; iDimension++)
        {
        subtractionFactor[iDimension] += stepSize[iDimension];
        }
      }



    // Compute the new value of the measure for this subtraction factor

    currentMeasure = ComputeMeasure( parameters, subtractionFactor );

    cout << setw(4) << nIterations << " " << currentMeasure << endl;

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

  cout << "No. of iterations: " << nIterations << endl;

  cout << "Subtraction factor: ";
  for (iDimension=0; iDimension<FixedImageDimension; iDimension++)
    cout << " " << subtractionFactor[iDimension];
  cout << endl;

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

  const double delta = 0.00011;
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
