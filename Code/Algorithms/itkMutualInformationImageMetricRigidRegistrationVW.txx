/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMutualInformationImageMetricRigidRegistrationVW.txx
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
MutualInformationImageMetricRigidRegistrationVW<TRefImage,TTestImage,TDerivImage>
::MutualInformationImageMetricRigidRegistrationVW()
{

  m_MutualInformationImageMetricCalculator = dynamic_cast<CalculatorType*>
    ( (DefaultCalculatorType::New()).GetPointer() );
  
  m_LearningRate       = 1.0;
  m_NumberOfIterations = 10;
  m_MatrixScaling.fill( 0.001 );
  m_VectorScaling.fill( 1.0 );

  m_InitialMatrix.set_identity();
  m_InitialVector.fill( 0.0 );

  m_AffineMatrix = m_InitialMatrix;
  m_AffineVector = m_InitialVector;

  m_DebugOn = false;

}


/**
 * PrintSelf method.
 */
template <class TRefImage, class TTestImage, class TDerivImage>
void
MutualInformationImageMetricRigidRegistrationVW<TRefImage,TTestImage,TDerivImage>
::PrintSelf(std::ostream& os, Indent indent)
{

  Superclass::PrintSelf(os,indent);

  os << indent << "Mutual information registration" << std::endl;
  os << indent << "using Viola and Wells optimization algorithm" << std::endl;

}


/**
 * Set the input reference image.
 */
template <class TRefImage, class TTestImage, class TDerivImage>
void
MutualInformationImageMetricRigidRegistrationVW<TRefImage,TTestImage,TDerivImage>
::SetReferenceImage( RefImageType *ptr )
{

  m_RefImage = ptr;
  m_MutualInformationImageMetricCalculator->SetReferenceImage( ptr );
}

/**
 * Set the input test image.
 */
template <class TRefImage, class TTestImage, class TDerivImage>
void
MutualInformationImageMetricRigidRegistrationVW<TRefImage,TTestImage,TDerivImage>
::SetTestImage( TestImageType *ptr )
{

  m_TestImage = ptr;
  m_MutualInformationImageMetricCalculator->SetTestImage( ptr );
}

/**
 * Set one of the test derivative images.
 */
template <class TRefImage, class TTestImage, class TDerivImage>
void
MutualInformationImageMetricRigidRegistrationVW<TRefImage,TTestImage,TDerivImage>
::SetTestImageDerivative( 
DerivImageType *ptr,
unsigned int idx )
{

  if ( idx >= ImageDimension ) return;
  m_DerivImages[idx] = ptr;

  m_MutualInformationImageMetricCalculator->SetTestImageDerivative( ptr, idx );

}

/**
 * Set the mutual information calculator
 */
template <class TRefImage, class TTestImage, class TDerivImage>
void
MutualInformationImageMetricRigidRegistrationVW<TRefImage,TTestImage,TDerivImage>
::SetMutualInformationImageMetricCalculator(
CalculatorType * ptr )
{

  m_MutualInformationImageMetricCalculator = ptr;

  // set inputs for the new calculator
  if( m_RefImage )
    {
      m_MutualInformationImageMetricCalculator->SetReferenceImage( m_RefImage );
    }

  if( m_TestImage )
    {
      m_MutualInformationImageMetricCalculator->SetTestImage( m_TestImage );
    }

  for( int j = 0; j < ImageDimension; j++ )
    {
      if( m_DerivImages[j] )
        {
          m_MutualInformationImageMetricCalculator->SetTestImageDerivative(
            m_DerivImages[j], j );
        }
    }

}


/**
 * Maximize the mutual information using a steepest descent optimization scheme.
 */
template <class TRefImage, class TTestImage, class TDerivImage>
void
MutualInformationImageMetricRigidRegistrationVW<TRefImage,TTestImage,TDerivImage>
::Maximize()
{

  // set up the initial affine parameters
  m_AffineMatrix = m_InitialMatrix;
  m_AffineVector = m_InitialVector;

  double mutualInformationValue;
  MatrixType tempMatrix;

  for( unsigned int k = 0; k < m_NumberOfIterations; k++ )
    {

    // set up the affine parameters
    m_MutualInformationImageMetricCalculator->SetAffineMatrix( m_AffineMatrix );
    m_MutualInformationImageMetricCalculator->SetAffineVector( m_AffineVector );

    // calculate mutual information and gradient
    m_MutualInformationImageMetricCalculator->CalculateMutualInformationImageMetricAndGradient();

    mutualInformationValue = 
      m_MutualInformationImageMetricCalculator->GetMutualInformationImageMetric();

    if ( m_DebugOn ) 
      {
      std::cout << k << " " << mutualInformationValue << " " ;
      std::cout << std::endl;
      }

    // check if this is the best visited
    if( mutualInformationValue >= m_BestMutualInformationImageMetric )
      {
      m_BestMutualInformationImageMetric = mutualInformationValue;
      m_BestAffineMatrix = m_AffineMatrix;
      m_BestAffineVector = m_AffineVector;
      }

    // calculate the next parameters to visit
    m_AffineMatrix = m_AffineMatrix  + m_LearningRate * 
      element_product<double>( m_MatrixScaling, 
        m_MutualInformationImageMetricCalculator->GetAffineMatrixDerivative() );

    m_AffineVector = m_AffineVector + m_LearningRate *
      element_product<double>( m_VectorScaling, 
        m_MutualInformationImageMetricCalculator->GetAffineVectorDerivative() );

    }

}


} // namespace itk
