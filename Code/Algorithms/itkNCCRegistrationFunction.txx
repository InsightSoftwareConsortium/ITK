/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNCCRegistrationFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkNCCRegistrationFunction_txx_
#define _itkNCCRegistrationFunction_txx_

#include "itkNCCRegistrationFunction.h"
#include "itkExceptionObject.h"
#include "vnl/vnl_math.h"
 
namespace itk {

/*
 * Default constructor
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
NCCRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::NCCRegistrationFunction()
{

  RadiusType r;
  unsigned int j;
  for( j = 0; j < ImageDimension; j++ )
    {
    r[j] = 1;
    }
  this->SetRadius(r);
  m_MetricTotal=0.0;

  m_TimeStep = 1.0;
  m_DenominatorThreshold = 1e-9;
  m_IntensityDifferenceThreshold = 0.001;
  m_MovingImage = NULL;
  m_FixedImage = NULL;
  m_FixedImageSpacing = NULL;
  m_FixedImageOrigin = NULL;
  m_FixedImageGradientCalculator = GradientCalculatorType::New();

  typename DefaultInterpolatorType::Pointer interp =
    DefaultInterpolatorType::New();

  m_MovingImageInterpolator = static_cast<InterpolatorType*>(
    interp.GetPointer() );


}


/*
 * Standard "PrintSelf" method.
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
NCCRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::PrintSelf(std::ostream& os, Indent indent) const
{/*
  
  Superclass::PrintSelf(os, indent);

  os << indent << "MovingImageIterpolator: ";
  os << m_MovingImageInterpolator.GetPointer() << std::endl;
  os << indent << "FixedImageGradientCalculator: ";
  os << m_FixedImageGradientCalculator.GetPointer() << std::endl;
  os << indent << "DenominatorThreshold: ";
  os << m_DenominatorThreshold << std::endl;
  os << indent << "IntensityDifferenceThreshold: ";
  os << m_IntensityDifferenceThreshold << std::endl;
*/
}


/*
 * Set the function state values before each iteration
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
NCCRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::InitializeIteration()
{
  if( !m_MovingImage || !m_FixedImage || !m_MovingImageInterpolator )
    {
    itkExceptionMacro( << "MovingImage, FixedImage and/or Interpolator not set" );
    throw ExceptionObject(__FILE__,__LINE__);
    }

  // cache fixed image information
  m_FixedImageSpacing    = m_FixedImage->GetSpacing();
  m_FixedImageOrigin     = m_FixedImage->GetOrigin();

  // setup gradient calculator
  m_FixedImageGradientCalculator->SetInputImage( m_FixedImage );

  // setup moving image interpolator
  m_MovingImageInterpolator->SetInputImage( m_MovingImage );

  std::cout << " total metric " << m_MetricTotal << " field size " <<
    m_DeformationField->GetLargestPossibleRegion().GetSize()<< " image size " <<
    m_FixedImage->GetLargestPossibleRegion().GetSize() << std::endl;
  m_MetricTotal=0.0;

}


/*
 * Compute update at a non boundary neighbourhood
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
typename NCCRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::PixelType
NCCRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::ComputeUpdate(const NeighborhoodType &it, void * globalData,
                const FloatOffsetType& offset) const
{

  PixelType update;
  update.Fill(0.0);
  unsigned int j;

  IndexType oindex = it.GetIndex();

  typename FixedImageType::SizeType hradius=it.GetRadius();
 
  FixedImageType* img =const_cast<FixedImageType *>(m_FixedImage.GetPointer()); 

  typename FixedImageType::SizeType imagesize=img->GetLargestPossibleRegion().GetSize();
  

  NeighborhoodIterator<FixedImageType> 
    hoodIt( hradius , img, img->GetRequestedRegion());
  hoodIt.SetLocation(oindex);

  double sff=0.0;
  double smm=0.0;
  double sfm=0.0;
  double fixedValue=0.0;
  double movingValue=0.0;

  double derivativeF[ImageDimension];
  double derivativeM[ImageDimension];
  for (j=0; j<ImageDimension;j++){
    derivativeF[j]=0;
    derivativeM[j]=0;
  }

  unsigned int indct=0;
  unsigned int hoodlen=hoodIt.Size();

  for(indct=0; indct<hoodlen-1; indct++)
  {

    IndexType index=hoodIt.GetIndex(indct);
 

    bool inimage=true;
    for (unsigned int dd=0; dd<ImageDimension; dd++)
    {
      if ( index[dd] < 0 || index[dd] > imagesize[dd]-1 ) inimage=false;
    }
    
    if (inimage)
    {


        // Get fixed image related information
      fixedValue=0.0;
      CovariantVectorType fixedGradient;
      double fixedGradientSquaredMagnitude = 0;

      // Note: no need to check the index is within
      // fixed image buffer. This is done by the external filter.
      fixedValue = (double) m_FixedImage->GetPixel( index );
      for( j = 0; j < ImageDimension; j++ )
      {
        fixedGradient[j] = m_FixedImageGradientCalculator->EvaluateAtIndex( index, j );
        fixedGradientSquaredMagnitude += vnl_math_sqr( fixedGradient[j] ) * m_FixedImageSpacing[j];
      } 

      // Get moving image related information
      movingValue=0.0;
      PointType mappedPoint;


      unsigned int itsize = it.Size();
      typename TDeformationField::PixelType vec = m_DeformationField->GetPixel(index);

      for( j = 0; j < ImageDimension; j++ )
      {
        mappedPoint[j] = double( index[j] ) * m_FixedImageSpacing[j] + 
        m_FixedImageOrigin[j];
        mappedPoint[j] += vec[j];
      }
      if( m_MovingImageInterpolator->IsInsideBuffer( mappedPoint ) )
      {
        movingValue = m_MovingImageInterpolator->Evaluate( mappedPoint );
      }
      else
      {
        movingValue = 0.0;
      }

      sff+=fixedValue*fixedValue;
      smm+=movingValue*movingValue;
      sfm+=fixedValue*movingValue;

      for(unsigned int dim=0; dim<ImageDimension; dim++)
      {
        double differential = fixedGradient[dim];
        derivativeF[dim]+= fixedValue  * differential;
        derivativeM[dim]+= movingValue * differential;
      }
    } 
   
  }

  double updatenorm=0.0;
  if( (sff*smm) != 0.0)
  {
    double factor = 1.0 / sqrt( sff * smm );
    for(unsigned int i=0; i<ImageDimension; i++)
    {
      update[i] = factor * ( derivativeF[i] - (sfm/smm)*derivativeM[i]);
      updatenorm+=(update[i]*update[i]);
    }
    updatenorm=sqrt(updatenorm);
    m_MetricTotal+=sfm*factor;
    m_Energy+=sfm*factor;
  } 
  else
  {
    for(unsigned int i=0; i<ImageDimension; i++)
    {
      update[i] = 0.0;
    }
    updatenorm=1.0;
  } 

//  if ( fixedValue > 0.40 && movingValue > 0.40)  std::cout << " update norm " << updatenorm;
  
  if (m_NormalizeGradient && updatenorm !=0.0 ) 
  {
    update/=(updatenorm);
  }


  return update/m_GradientStep;
}




} // end namespace itk

#endif
