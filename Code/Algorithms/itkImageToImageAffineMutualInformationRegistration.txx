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
  m_Optimizer = OptimizerType::New();

  // initialize the parameter to be the identity transform
  typename ParametersType::Iterator pit = m_Parameters.Begin();
  typename ParametersType::Iterator sit = m_ScalingWeights.Begin();

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
  m_Transformation->SetDomainTransformationCenter(
   m_TargetTransformationCenter );

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
  m_Transformation->SetRangeTransformationCenter(
   m_ReferenceTransformationCenter );

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
  m_Optimizer->SetCostFunction( m_Metric );

  // setup the optimizer
  m_Optimizer->SetMaximize();
  m_Optimizer->SetScale( m_ScalingWeights );
  m_Optimizer->SetNumberOfIterations( m_NumberOfIterations );
  m_Optimizer->SetInitialPosition( m_Parameters );

  // do the optimization
  m_Optimizer->StartOptimization();

  // get the results
  m_Parameters = m_Optimizer->GetCurrentPosition();
  
  return 0;

}


} // end namespace itk


#endif
