/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMutualInformationRigidRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/

namespace itk
{

/**
 * Default constructor
 */
template <class TRefImage, class TTestImage, class TDerivImage>
MutualInformationRigidRegistration<TRefImage,TTestImage,TDerivImage>
::MutualInformationRigidRegistration()
{
  m_RefImage = NULL;
  m_TestImage = NULL;

  for( int j = 0; j < ImageDimension; j++ )
    {
      m_DerivImages[j] = NULL;
    }

  m_BestAffineMatrix.set_identity();
  m_BestAffineVector.fill( 0.0 );

  m_BestMutualInformation = -1.0 * NumericTraits<double>::max();
  
}

/**
 * PrintSelf
 */
template <class TRefImage, class TTestImage, class TDerivImage>
void
MutualInformationRigidRegistration<TRefImage,TTestImage,TDerivImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Mutual information registration" << std::endl;

}


} // namespace itk
