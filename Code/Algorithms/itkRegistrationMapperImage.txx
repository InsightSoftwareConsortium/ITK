/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationMapperImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRegistrationMapperImage_txx
#define _itkRegistrationMapperImage_txx



namespace itk
{

/**
 * Constructor
 */
template <class TImage, class TTransformation> 
RegistrationMapperImage<TImage,TTransformation>
::RegistrationMapperImage()
{
}





/**
 * Test whether the point is inside the image domain
 */
template <class TImage, class TTransformation> 
bool
RegistrationMapperImage<TImage,TTransformation>
::IsInside( const PointType & point ) const
{ 

  typename Superclass::TransformationType::Pointer transformation;

  transformation = GetTransform();

  PointType mappedPoint = transformation->Transform( point );

  double index[TImage::ImageDimension];

  for( unsigned int j = 0; j < TImage::ImageDimension; j++ )
  {
    m_CurrentIndex[j] =  mappedPoint[j] / m_Spacing[j] ;
  }
   
  bool value = true;
  for( unsigned int i = 0; i < TImage::ImageDimension; i++ )
  {
    
    if( m_CurrentIndex[i] < m_Start[i] )
    { 
      value = false;
      break;
    }
    
    if( m_CurrentIndex[i] >= m_Start[i] + static_cast<long>(m_Size[i]) ) 
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
typename RegistrationMapperImage<TImage,TTransformation>::PixelType
RegistrationMapperImage<TImage,TTransformation>
::Evaluate( void ) const
{ 

  const double value = m_Interpolator->Evaluate( m_CurrentIndex );

  return value;

}





} // end namespace itk

#endif
