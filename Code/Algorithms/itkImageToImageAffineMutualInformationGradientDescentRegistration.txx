/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageAffineMutualInformationGradientDescentRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkImageToImageAffineMutualInformationGradientDescentRegistration_txx
#define _itkImageToImageAffineMutualInformationGradientDescentRegistration_txx

#include "itkImageToImageAffineMutualInformationGradientDescentRegistration.h"


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageAffineMutualInformationGradientDescentRegistration<TReference, TTarget>
::ImageToImageAffineMutualInformationGradientDescentRegistration()
{

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

  // set default parameters
  m_NumberOfIterations = 1000;
  m_LearningRate = 1.0;
  PointType center;
  center.Fill( 0 );
  this->SetTargetTransformationCenter( center );
  this->SetReferenceTransformationCenter( center );

}


/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageAffineMutualInformationGradientDescentRegistration<TReference, TTarget>
::ImageToImageAffineMutualInformationGradientDescentRegistration( const Self & other )
:Superclass( other )
{
}



/**
 * Destructor
 */
template <class TReference, class TTarget>
ImageToImageAffineMutualInformationGradientDescentRegistration<TReference,  TTarget>
::~ImageToImageAffineMutualInformationGradientDescentRegistration()
{
}



/**
 * Assignment Operator
 */
template <class TReference, class TTarget>
const ImageToImageAffineMutualInformationGradientDescentRegistration< TReference, TTarget> &
ImageToImageAffineMutualInformationGradientDescentRegistration< TReference, TTarget>
::operator=( const Self & other )
{
  Superclass::operator=( other );
  return *this;
}




/**
 * Set Target transformation center
 */
template <class TReference, class TTarget>
void
ImageToImageAffineMutualInformationGradientDescentRegistration< TReference, TTarget>
::SetTargetTransformationCenter( const PointType& center )
{
  m_TargetTransformationCenter = center;

  typename TransformationType::Pointer transformation =
            this->GetMetric()->GetMapper()->GetTransformation();

  transformation->SetDomainTransformationCenter(
                           m_TargetTransformationCenter );

}


/**
 * Set Reference transformation center
 */
template <class TReference, class TTarget>
void
ImageToImageAffineMutualInformationGradientDescentRegistration< TReference, TTarget>
::SetReferenceTransformationCenter( const PointType& center )
{
  m_ReferenceTransformationCenter = center;

  typename TransformationType::Pointer transformation =
            this->GetMetric()->GetMapper()->GetTransformation();

  transformation->SetRangeTransformationCenter(
                           m_ReferenceTransformationCenter );

}


/**
 * Starts the Registration Process
 */
template <class TReference, class TTarget>
int
ImageToImageAffineMutualInformationGradientDescentRegistration<TReference, TTarget>
::StartRegistration( void )
{

  typename OptimizerType::Pointer optimizer;
  optimizer = this->GetOptimizer();

  optimizer->SetCostFunction( this->GetMetric() );

  // setup the optimizer
  optimizer->SetMaximize();
  optimizer->SetLearningRate( m_LearningRate );
  optimizer->SetScale( m_ScalingWeights );
  optimizer->SetNumberOfIterations( m_NumberOfIterations );
  optimizer->SetInitialPosition( m_Parameters );

  // do the optimization
  optimizer->StartOptimization();

  // get the results
  m_Parameters = optimizer->GetCurrentPosition();

  return 0;

}


} // end namespace itk


#endif
