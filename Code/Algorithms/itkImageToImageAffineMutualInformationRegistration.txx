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

  // initialize the parameter to be the identity transform
  typename ParametersType::Iterator pit = m_Parameters.Begin();
  typename ScalingWeightsType::Iterator sit = m_ScalingWeights.Begin();

  // initialize the linear part 
  for (unsigned int i=0; i<TReference::ImageDimension; i++)
    {
    for (unsigned int j=0; j<TReference::ImageDimension; j++)
      {
      *pit = 0;
      if(i == j) 
	      {
	      *pit = 1;
        }
	    ++pit;

      *sit = 0.001;
      ++sit;
      }
    }

  // initialize the offset part
  for (unsigned int i=0; i<TReference::ImageDimension; i++)
  { 
    *pit = 0;
    ++pit;

    *sit = 1.0;
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
 * Set Target transformation center 
 */
template <class TReference, class TTarget>
void
ImageToImageAffineMutualInformationRegistration< TReference, TTarget>
::SetTargetTransformationCenter( const PointType& center )
{
  m_TargetTransformationCenter = center;
  m_Transformation->SetDomainTransformationCenter( m_TargetTransformationCenter );

}

/**
 * Set Reference transformation center 
 */
template <class TReference, class TTarget>
void
ImageToImageAffineMutualInformationRegistration< TReference, TTarget>
::SetReferenceTransformationCenter( const PointType& center )
{
  m_ReferenceTransformationCenter = center;
  m_Transformation->SetRangeTransformationCenter( m_ReferenceTransformationCenter );

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
  double MIValue;

  typedef MetricType::DerivativeType  DerivativeType;
  DerivativeType MIDerivatives;
  vnl_vector<double> increment;

  for( unsigned int k = 0; k < m_NumberOfIterations; k++ )
    {
    
    // set the affine parameters
    m_Metric->SetParameters( m_Parameters );

    // compute mutual information and derivative
    m_Metric->GetValueAndDerivative( MIValue, MIDerivatives );

    // compute the next set of parameters to visit
    increment = m_LearningRate * element_product<double>( 
        m_ScalingWeights.Get_vnl_vector(), MIDerivatives.Get_vnl_vector() );

    for( unsigned int j = 0; j < ParametersDimension; j++ )
      {
      m_Parameters[j] += increment[j];
      }


    if( m_ScreenDump )
      {
      std::cout << k << " " << MIValue << std::endl;
      }

/*
  if( m_ScreenDump )
    {
    for( pit = m_Parameters->Begin(); pit != pend; ++pit )
      {
      std::cout << pit.Value() << " ";
      }
    std::cout << std::endl;
    }
*/

    }

  if( m_ScreenDump )
    {
    std::cout << "Last set of parameter visited: " << m_Parameters << std::endl;
    }
    
  return 0;

}


} // end namespace itk


#endif