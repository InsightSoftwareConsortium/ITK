/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageAffineNormalizedCorrelationRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkImageToImageAffineNormalizedCorrelationRegistration_txx
#define _itkImageToImageAffineNormalizedCorrelationRegistration_txx

#include "itkImageToImageAffineNormalizedCorrelationRegistration.h"


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageAffineNormalizedCorrelationRegistration<TReference, TTarget>
::ImageToImageAffineNormalizedCorrelationRegistration()
{ 
  m_Parameters = ParametersType::New();
  m_Parameters->Reserve(TransformationType::ParametersDimension);
  m_Metric = MetricType::New();
  m_Mapper = MapperType::New(); 
  m_Optimizer = OptimizerType::New();
  m_Transformation = TransformationType::New();
}


/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageAffineNormalizedCorrelationRegistration<TReference, TTarget>
::ImageToImageAffineNormalizedCorrelationRegistration( const Self & other )
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
ImageToImageAffineNormalizedCorrelationRegistration<TReference,  TTarget>
::~ImageToImageAffineNormalizedCorrelationRegistration()
{
}



/**
 * Assignment Operator
 */
template <class TReference, class TTarget>
const ImageToImageAffineNormalizedCorrelationRegistration< TReference, TTarget> &
ImageToImageAffineNormalizedCorrelationRegistration< TReference, TTarget>
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
ImageToImageAffineNormalizedCorrelationRegistration<TReference, TTarget>
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
ImageToImageAffineNormalizedCorrelationRegistration< TReference, TTarget>
::SetTarget( TargetType * target )
{
  m_Target       =   target;
  m_Metric->SetTarget( m_Target );
}


/**
 * Starts the Registration Process
 */
template <class TReference, class TTarget>
int
ImageToImageAffineNormalizedCorrelationRegistration<TReference, TTarget>
::StartRegistration( void )
{ 
  ParametersType::Iterator it = m_Parameters->Begin();

  /* Initialize Linear part*/ 
  for (unsigned int i=0; i<TReference::ImageDimension; i++)
    for (unsigned int j=0; j<TReference::ImageDimension; j++)
    {
      it.Value() = 0;
      if(i == j) 
	  {
	    it.Value() = 1;
      }
	  it++;
    }

  /* Initialize the Offset */ 
  for (unsigned int i=0; i<TReference::ImageDimension; i++)
  { 
    it.Value() = 0;
    it++;
  }

  m_Mapper->SetTransformation(m_Transformation);
  m_Metric->SetMapper(m_Mapper);
  m_Optimizer->SetCostFunction( m_Metric );

  /*Tolerances for conjugate gradient optimizer */
  /*const double F_Tolerance      = 1e-3;  // Function value tolerance
  const double G_Tolerance      = 1e-7;  // Gradient magnitude tolerance 
  const double X_Tolerance      = 1e-10;  // Search space tolerance
  const double Epsilon_Function = 1e-1; // Step
  const int    Max_Iterations   =   100; // Maximum number of iterations
  */

  /*Tolerances for Levenberg Marquardt  optimizer */
  const double F_Tolerance      = 1e-10;  // Function value tolerance
  const double G_Tolerance      = 1e-10;  // Gradient magnitude tolerance 
  const double X_Tolerance      = 1e-10;  // Search space tolerance
  const double Epsilon_Function = 1e-3;  // Step 
  const int    Max_Iterations   =   100; // Maximum number of iterations

/*
  vnlOptimizerType & vnlOptimizer = m_Optimizer->GetOptimizer();

  vnlOptimizer.set_f_tolerance( F_Tolerance );
  vnlOptimizer.set_g_tolerance( G_Tolerance );
  vnlOptimizer.set_x_tolerance( X_Tolerance ); 
  vnlOptimizer.set_epsilon_function( Epsilon_Function );
  vnlOptimizer.set_max_function_evals( Max_Iterations );

  vnlOptimizer.set_trace( true );   // activate print out per iteration
  vnlOptimizer.set_verbose( true ); // activate verbose mode

  vnlOptimizer.set_check_derivatives( 3 );
*/
  m_Optimizer->StartOptimization(m_Parameters);

 /*
    ERROR_FAILURE              =-1,
    ERROR_DODGY_INPUT          = 0,
    CONVERGED_FTOL     	       = 1,
    CONVERGED_XTOL     	       = 2,
    CONVERGED_XFTOL    	       = 3,
    CONVERGED_GTOL     	       = 4,
    FAILED_TOO_MANY_ITERATIONS = 5,
    FAILED_FTOL_TOO_SMALL      = 6,
    FAILED_XTOL_TOO_SMALL      = 7,
    FAILED_GTOL_TOO_SMALL      = 8
 */

/*  
  std::cout << "End condition   = " << vnlOptimizer.get_failure_code() << std::endl;
  std::cout << "Number of iters = " << vnlOptimizer.get_num_iterations() << std::endl;
  std::cout << "Number of evals = " << vnlOptimizer.get_num_evaluations() << std::endl;    
  std::cout << std::endl;
*/

  std::cout << "The Solution is : " ;
  it = m_Metric->GetParameters()->Begin();
  while( it != m_Metric->GetParameters()->End())
  {
    std::cout << it.Value() << " ";
	it++;
  }
  std::cout << std::endl;

return 0;
}



} // end namespace itk


#endif
