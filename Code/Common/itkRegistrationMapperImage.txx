/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationMapperImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/


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
 * Evaluate the image at some point
 */
template <class TImage, class TTransformation> 
RegistrationMapperImage<TImage,TTransformation>::PixelType
RegistrationMapperImage<TImage,TTransformation>
::Evaluate( const PointType & point )
{
  PointType mappedPoint = m_Transformation->Transform( point );

  double * spacing  = m_Domain->GetSpacing();
  RegionType region = m_Domain->GetRequestedRegion();
  IndexType  start  = region.GetIndex();
  SizeType   size   = region.GetSize();

  IndexType index;

  for( unsigned int j = 0; j < TImage::ImageDimension; j++ )
  {
    index[j] = vnl_math_rnd( mappedPoint[j] / spacing[j] );
  }

  // Verify the range of the image
  // Throw and exception if the point is outside the 
  // RequestedRegion of the image
  for( unsigned int i = 0; i < TImage::ImageDimension; i++ )
  {

    if( index[i] < start[i] )
    {
      throw MapperException();
    }
    
    if( index[i] >= start[i] + size[i] ) 
    {
      throw MapperException();
    }

  }

  const PixelType value = m_Domain->GetPixel( index );
 
  return value;

}






} // end namespace itk
