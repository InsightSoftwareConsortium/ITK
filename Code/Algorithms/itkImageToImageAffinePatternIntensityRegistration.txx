/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageAffinePatternIntensityRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkImageToImageAffinePatternIntensityRegistration_txx
#define _itkImageToImageAffinePatternIntensityRegistration_txx

#include "itkImageToImageAffinePatternIntensityRegistration.h"


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageAffinePatternIntensityRegistration<TReference, TTarget>
::ImageToImageAffinePatternIntensityRegistration()
{ 
  m_Metric = MetricType::New();
  m_Mapper = MapperType::New(); 
  m_Optimizer = OptimizerType::New();
  m_Transformation = TransformationType::New();
  m_TranslationScale = 100.0; 
}


/**
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageAffinePatternIntensityRegistration<TReference, TTarget>
::ImageToImageAffinePatternIntensityRegistration( const Self & other )
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
ImageToImageAffinePatternIntensityRegistration<TReference,  TTarget>
::~ImageToImageAffinePatternIntensityRegistration()
{
}



/**
 * Assignment Operator
 */
template <class TReference, class TTarget>
const ImageToImageAffinePatternIntensityRegistration< TReference, TTarget> &
ImageToImageAffinePatternIntensityRegistration< TReference, TTarget>
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
ImageToImageAffinePatternIntensityRegistration<TReference, TTarget>
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
ImageToImageAffinePatternIntensityRegistration< TReference, TTarget>
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
ImageToImageAffinePatternIntensityRegistration<TReference, TTarget>
::StartRegistration( void )
{ 
  /* Initialize the rotation / shear */
  unsigned int k = 0;
  for (unsigned int col=0; col<ImageDimension; col++)
  {
    for (unsigned int row=0; row<ImageDimension; row++)
    { 
      if( col == row ) 
      {
        m_Parameters[ k++ ] = 1.0;
      }
      else 
      {
        m_Parameters[ k++ ] = 0.0;
      }
    }
  }

  /* Initialize the Offset */ 
  for (unsigned int coff=0; coff<ImageDimension; coff++)
  {
    m_Parameters[ k++ ] = 0.0;
  }

  m_Transformation->SetTranslationScale( m_TranslationScale );
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
  /*
  const double F_Tolerance      = 1e-10;  // Function value tolerance
  const double G_Tolerance      = 1e-10;  // Gradient magnitude tolerance 
  const double X_Tolerance      = 1e-10;  // Search space tolerance
  const double Epsilon_Function = 1e-3;  // Step 
  const int    Max_Iterations   =   100; // Maximum number of iterations
  */

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

  ParametersType  parametersScale;
  parametersScale.Fill( 1.0 );
  m_Optimizer->SetMinimize();
  m_Optimizer->SetScale( parametersScale );
  m_Optimizer->SetGradientMagnitudeTolerance( 1e-9 );
  m_Optimizer->SetMaximumStepLength( 1e-3  );
  m_Optimizer->SetMinimumStepLength( 1e-6 );
  m_Optimizer->SetMaximumNumberOfIterations( 100 );

  m_Optimizer->SetInitialPosition( m_Parameters );
  m_Optimizer->StartOptimization();

 /*
    ERROR_FAILURE              =-1,
    ERROR_DODGY_INPUT          = 0,
    CONVERGED_FTOL              = 1,
    CONVERGED_XTOL              = 2,
    CONVERGED_XFTOL             = 3,
    CONVERGED_GTOL              = 4,
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
  m_Parameters = m_Optimizer->GetCurrentPosition();
  const unsigned int offsetStart = ImageDimension * ImageDimension;
  for(unsigned int k=0; k<ImageDimension; k++)
  {
    m_Parameters[ offsetStart + k ] *= m_TranslationScale;
  }
  std::cout << m_Parameters << std::endl;
  std::cout << std::endl;

return 0;
}



} // end namespace itk


#endif
