/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAffineMutualInformationImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef _itkAffineMutualInformationImageMetric_txx
#define _itkAffineMutualInformationImageMetric_txx


namespace itk
{

/**
 * Default constructor
 */
template <class TRefImage, class TTestImage, class TDerivImage>
AffineMutualInformationImageMetric<TRefImage,TTestImage,TDerivImage>
::AffineMutualInformationImageMetric()
{

  for( int j = 0; j < ImageDimension; j++ )
  {
    m_DerivImages[j] = NULL;
  }

  m_AffineMatrix.set_identity();
  m_AffineVector.fill( 0.0 );
  m_AffineMatrixDerivative.fill( 0.0 );
  m_AffineVectorDerivative.fill( 0.0 );

}

/**
 * PrintSelf
 */
template <class TRefImage, class TTestImage, class TDerivImage>
void
AffineMutualInformationImageMetric<TRefImage,TTestImage,TDerivImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Affine transform Mutual information" << std::endl;

}

/**
 * Set one of the input test image derivatives.
 */
template <class TRefImage, class TTestImage, class TDerivImage>
void
AffineMutualInformationImageMetric<TRefImage,TTestImage,TDerivImage>
::SetTestImageDerivative(
DerivImageType * ptr,
unsigned int idx )
{
  if( idx >= ImageDimension ) return;

  m_DerivImages[idx] = ptr;

}



} // namespace itk

#endif
