/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageAffineMutualInformationRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkImageToImageAffineMutualInformationRegistration_txx
#define _itkImageToImageAffineMutualInformationRegistration_txx

#include "itkImageToImageAffineMutualInformationRegistration.h"


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageAffineMutualInformationRegistration<TReference, TTarget>
::ImageToImageAffineMutualInformationRegistration()
{ 
  m_Metric = MetricType::New();
  m_Mapper = MapperType::New(); 
  m_Transformation = TransformationType::New();

  m_Parameters = ParametersType::New();
  m_Parameters->Reserve(TransformationType::ParametersDimension);

  m_ScalingWeights = ScalingWeightsType::New();
  m_ScalingWeights->Reserve(TransformationType::ParametersDimension);

  // initialize the parameter to be the identity transform
  typename ParametersType::Iterator pit = m_Parameters->Begin();
  typename ScalingWeightsType::Iterator sit = m_ScalingWeights->Begin();

  // initialize the linear part 
  for (unsigned int i=0; i<TReference::ImageDimension; i++)
    {
    for (unsigned int j=0; j<TReference::ImageDimension; j++)
      {
      pit.Value() = 0;
      if(i == j) 
	      {
	      pit.Value() = 1;
        }
	    ++pit;

      sit.Value() = 0.001;
      ++sit;
      }
    }

  // initialize the offset part
  for (unsigned int i=0; i<TReference::ImageDimension; i++)
  { 
    pit.Value() = 0;
    ++pit;

    sit.Value() = 1.0;
    ++sit;
  }

  m_NumberOfIterations = 1000;
  m_LearningRate = 1.0;
  m_ScreenDump = true;

}


/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageAffineMutualInformationRegistration<TReference, TTarget>
::ImageToImageAffineMutualInformationRegistration( const Self & other )
{
  m_Reference       =   other.m_Reference;
  m_Target          =   other.m_Target;
  m_Transformation  =   other.m_Transformation;
  m_Metric          =   other.m_Metric;
}



/**
 * Destructor
 */
template <class TReference, class TTarget>
ImageToImageAffineMutualInformationRegistration<TReference,  TTarget>
::~ImageToImageAffineMutualInformationRegistration()
{
}



/**
 * Assignment Operator
 */
template <class TReference, class TTarget>
const ImageToImageAffineMutualInformationRegistration< TReference, TTarget> &
ImageToImageAffineMutualInformationRegistration< TReference, TTarget>
::operator=( const Self & other )
{
  m_Reference       =   other.m_Reference;
  m_Target          =   other.m_Target;
  m_Transformation  =   other.m_Transformation;
  m_Metric          =   other.m_Metric;
  return *this;
}


/**
 * Set Reference 
 */


template <class TReference, class TTarget>
void
ImageToImageAffineMutualInformationRegistration<TReference, TTarget>
::SetReference( ReferenceType * reference )
{
  m_Reference       =   reference;
  m_Mapper->SetDomain( m_Reference );
}


/**
 * Set Target 
 */
template <class TReference, class TTarget>
void
ImageToImageAffineMutualInformationRegistration< TReference, TTarget>
::SetTarget( TargetType * target )
{
  m_Target       =   target;
  m_Metric->SetTarget( m_Target );
}

/**
 * Set the parameters
 */
template <class TReference, class TTarget>
void
ImageToImageAffineMutualInformationRegistration< TReference, TTarget>
::SetParameters( ParametersType * parameters ) 
{
  typename ParametersType::ConstIterator inIt = parameters->Begin();
  typename ParametersType::Iterator outIt = m_Parameters->Begin();

  while( inIt != parameters->End() )
    {
    outIt.Value() = inIt.Value();
    ++outIt;
    ++inIt;
    }
}

/**
 * Set the scaling weights
 */
template <class TReference, class TTarget>
void
ImageToImageAffineMutualInformationRegistration< TReference, TTarget>
::SetScalingWeights( ScalingWeightsType * weights ) 
{
  typename ScalingWeightsType::ConstIterator inIt = weights->Begin();
  typename ScalingWeightsType::Iterator outIt = m_ScalingWeights->Begin();

  while( inIt !=  weights->End() )
    {
    outIt.Value() = inIt.Value();
    ++outIt;
    ++inIt;
    }
}



/**
 * Starts the Registration Process
 */
template <class TReference, class TTarget>
int
ImageToImageAffineMutualInformationRegistration<TReference, TTarget>
::StartRegistration( void )
{ 

  m_Mapper->SetTransformation(m_Transformation);
  m_Metric->SetMapper(m_Mapper);

  //
  // Maximize the mutual information using a stochastic ascent method
  // 
  // FIXME: should separate this part out into an itkOptimizer
  //
  typedef MetricType::DerivativeType  DerivativeType;

  double MIValue;
  typename DerivativeType::Pointer MIDerivatives;

  typename ParametersType::Iterator pit;
  typename ParametersType::ConstIterator pend = m_Parameters->End();

  typename ScalingWeightsType::ConstIterator sit;
  typename DerivativeType::ConstIterator dit;

  for( unsigned int k = 0; k < m_NumberOfIterations; k++ )
    {
    
    // set the affine parameters
    m_Metric->SetParameters( m_Parameters );

    // compute mutual information and derivative
    m_Metric->GetValueAndDerivative( MIValue, MIDerivatives );

    // compute the next set of parameters to visit
    for( pit = m_Parameters->Begin(), sit = m_ScalingWeights->Begin(), 
      dit = MIDerivatives->Begin(); 
      pit != pend;
      ++pit, ++sit, ++dit )
      {
      pit.Value() += m_LearningRate * sit.Value() * dit.Value();
      }

    if( m_ScreenDump )
      {
      std::cout << k << " " << MIValue << std::endl;
      }

    }

  if( m_ScreenDump )
    {
    std::cout << "Last set of parameter visited: " << std::endl;
    for( pit = m_Parameters->Begin(); pit != pend; ++pit )
      {
      std::cout << pit.Value() << " ";
      }
    std::cout << std::endl;
    }
    
  return 0;

}


} // end namespace itk


#endif