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
 * Test whether the point is inside the image domain
 */
template <class TImage, class TTransformation> 
bool
ImageMapper<TImage,TTransformation>
::IsInside( const PointType & point ) 
{ 

  typename Superclass::TransformationType::Pointer transformation;

  transformation = GetTransformation();

  PointType mappedPoint = transformation->Transform( point );


  for( unsigned int j = 0; j < TImage::ImageDimension; j++ )
  {
    m_CurrentPoint[j] =  mappedPoint[j] / m_Spacing[j] ;
  }
   
  bool value = true;
  for( unsigned int i = 0; i < TImage::ImageDimension; i++ )
  {
    
    if( m_CurrentPoint[i] < m_Start[i] )
    { 
      value = false;
      break;
    }
    
    if( m_CurrentPoint[i] >= m_Start[i] + m_Size[i] ) 
    {
      value = false;
      break;
    }
  }

  return value;

}




/**
 * Evaluate the image at some point
 */
template <class TImage, class TTransformation> 
double
ImageMapper<TImage,TTransformation>
::Evaluate( void ) const
{ 

  const double value = m_Interpolator->Evaluate( m_CurrentPoint );

  return value;

}


} // end namespace itk

#endif
