/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageMapper.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkImageMapper_txx
#define _itkImageMapper_txx

#include "itkImageMapper.h"

namespace itk
{

/**
 * Constructor
 */
template <class TImage, class TTransformation> 
ImageMapper<TImage,TTransformation>
::ImageMapper()
{
}



/**
 * Set the Domain
 */
template <class TImage, class TTransformation> 
void
ImageMapper<TImage,TTransformation>
::SetDomain(DomainType *  domain)
{
  Superclass::SetDomain( domain );

  m_Spacing  = domain->GetSpacing();
  RegionType region = domain->GetRequestedRegion();

  m_Start  = region.GetIndex();
  m_Size   = region.GetSize();

  m_Interpolator = InterpolatorType::New();
  m_Interpolator->SetInputImage( domain );

}




/**
 * Evaluate the image at some point
 */
template <class TImage, class TTransformation> 
double
ImageMapper<TImage,TTransformation>
::Evaluate( PointType & point )
{ 

  typename Superclass::TransformationType::Pointer transformation;

  transformation = GetTransformation();

  PointType mappedPoint = transformation->Transform( point );

  double index[TImage::ImageDimension];

  for( unsigned int j = 0; j < TImage::ImageDimension; j++ )
  {
    index[j] =  mappedPoint[j] / m_Spacing[j] ;
  }
   
   
  // Verify the range of the image
  // Throw and exception if the point is outside the 
  // RequestedRegion of the image
  // in our case just return 0;
  for( unsigned int i = 0; i < TImage::ImageDimension; i++ )
  {
    
    if( index[i] < m_Start[i] )
    { 
      MapperException outOfImage;
      outOfImage.SetLocation("Evaluate()");
      throw outOfImage;
    }
    
    if( index[i] >= m_Start[i] + m_Size[i] ) 
    {
      MapperException outOfImage;
      outOfImage.SetLocation("Evaluate()");
      throw outOfImage;
    }
  }

  const double value = m_Interpolator->Evaluate( index );

  return value;

}


} // end namespace itk

#endif
