/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMutualInformationAffineRegistrator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef _itkMutualInformationAffineRegistrator_txx
#define _itkMutualInformationAffineRegistrator_txx


namespace itk
{

/**
 * Default constructor
 */
template <class TRefImage, class TTestImage, class TDerivImage>
MutualInformationAffineRegistrator<TRefImage,TTestImage,TDerivImage>
::MutualInformationAffineRegistrator()
{
  m_RefImage = NULL;
  m_TestImage = NULL;

  for( int j = 0; j < ImageDimension; j++ )
    {
      m_DerivImages[j] = NULL;
    }

  m_BestAffineMatrix.set_identity();
  m_BestAffineVector.fill( 0.0 );

  m_BestMutualInformationImageMetric = -1.0 * NumericTraits<double>::max();
  
}

/**
 * PrintSelf
 */
template <class TRefImage, class TTestImage, class TDerivImage>
void
MutualInformationAffineRegistrator<TRefImage,TTestImage,TDerivImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Mutual information registration" << std::endl;

}


} // namespace itk

#endif
