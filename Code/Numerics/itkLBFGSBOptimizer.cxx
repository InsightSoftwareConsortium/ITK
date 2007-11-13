/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLBFGSBOptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLBFGSBOptimizer_txx
#define __itkLBFGSBOptimizer_txx

#include "itkLBFGSBOptimizer.h"
#include "vnl/algo/vnl_lbfgsb.h"


namespace itk
{


/** \class LBFGSBOptimizerHelper
 * \brief Wrapper helper around vnl_lbfgsb.
 *
 * This class is used to translate iteration events, etc, from
 * vnl_lbfgsb into iteration events in ITK.
 */
class ITK_EXPORT LBFGSBOptimizerHelper :
    public vnl_lbfgsb
{
public:
  typedef LBFGSBOptimizerHelper               Self;
  typedef vnl_lbfgsb                          Superclass;

  /** Create with a reference to the ITK object */
  LBFGSBOptimizerHelper( vnl_cost_function& f,
                         LBFGSBOptimizer* itkObj );

  /** Handle new iteration event */
  virtual bool report_iter();

private:
  LBFGSBOptimizer* m_itkObj;
};

  


/**
 * Constructor
 */
LBFGSBOptimizer
::LBFGSBOptimizer()
{
  m_OptimizerInitialized = false;
  m_VnlOptimizer         = 0;

  m_LowerBound       = InternalBoundValueType(0);
  m_UpperBound       = InternalBoundValueType(0); 
  m_BoundSelection   = InternalBoundSelectionType(0);

  m_CostFunctionConvergenceFactor   = 1e+7;
  m_ProjectedGradientTolerance      = 1e-5;
  m_MaximumNumberOfIterations       = 500;
  m_MaximumNumberOfEvaluations      = 500;
  m_MaximumNumberOfCorrections      = 5;
  m_CurrentIteration                = 0;
  m_Value                           = 0.0;
  m_InfinityNormOfProjectedGradient = 0.0;
 
}


/**
 * Destructor
 */
LBFGSBOptimizer
::~LBFGSBOptimizer()
{
  delete m_VnlOptimizer;
}

/**
 * PrintSelf
 */
void
LBFGSBOptimizer
::PrintSelf( std::ostream& os, Indent indent) const
{  
  Superclass::PrintSelf(os, indent);

  os << indent << "LowerBound: " << m_LowerBound << std::endl;
  os << indent << "UpperBound: " << m_UpperBound << std::endl;
  os << indent << "BoundSelection: " << m_BoundSelection << std::endl;

  os << indent << "CostFunctionConvergenceFactor: " << 
    m_CostFunctionConvergenceFactor << std::endl;

  os << indent << "ProjectedGradientTolerance: " <<
    m_ProjectedGradientTolerance << std::endl;

  os << indent << "MaximumNumberOfIterations: " <<
    m_MaximumNumberOfIterations << std::endl;

  os << indent << "MaximumNumberOfEvaluations: " <<
    m_MaximumNumberOfEvaluations << std::endl;

  os << indent << "MaximumNumberOfCorrections: " << 
    m_MaximumNumberOfCorrections << std::endl;

  os << indent << "CurrentIteration: " << 
    m_CurrentIteration << std::endl;

  os << indent << "Value: " <<
    m_Value << std::endl;

  os << indent << "InfinityNormOfProjectedGradient: " <<
    m_InfinityNormOfProjectedGradient << std::endl;

}

/**
 * Set lower bound
 */
void
LBFGSBOptimizer
::SetLowerBound(
const BoundValueType& value )
{
  m_LowerBound = value;
  if( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_lower_bound( m_LowerBound );
    }

  this->Modified();
}

/**
 * Get lower bound
 */
const
LBFGSBOptimizer 
::BoundValueType &
LBFGSBOptimizer
::GetLowerBound()
{
  return m_LowerBound;
} 

/**
 * Set upper bound
 */
void
LBFGSBOptimizer
::SetUpperBound(
const BoundValueType& value )
{
  m_UpperBound = value;
  if( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_upper_bound( m_UpperBound );
    }

  this->Modified();
}

/**
 * Get upper bound
 */
const
LBFGSBOptimizer 
::BoundValueType &
LBFGSBOptimizer
::GetUpperBound()
{
  return m_UpperBound;
} 


/**
 * Set bound selection array
 */
void
LBFGSBOptimizer
::SetBoundSelection(
const BoundSelectionType& value )
{
  m_BoundSelection = value;
  if( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_bound_selection( m_BoundSelection );
    }
  this->Modified();
}

/**
 * Get bound selection array
 */
const
LBFGSBOptimizer 
::BoundSelectionType &
LBFGSBOptimizer
::GetBoundSelection()
{
  return m_BoundSelection;
} 


/** Set/Get the CostFunctionConvergenceFactor. Algorithm terminates
 * when the reduction in cost function is less than factor * epsmcj
 * where epsmch is the machine precision.
 * Typical values for factor: 1e+12 for low accuracy; 
 * 1e+7 for moderate accuracy and 1e+1 for extremely high accuracy.
 */
void
LBFGSBOptimizer
::SetCostFunctionConvergenceFactor( double value )
{
  m_CostFunctionConvergenceFactor = value;
  if( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_cost_function_convergence_factor(
      m_CostFunctionConvergenceFactor );
    }
  this->Modified();
}


/** Set/Get the ProjectedGradientTolerance. Algorithm terminates
 * when the project gradient is below the tolerance. Default value
 * is 1e-5.
 */
void
LBFGSBOptimizer
::SetProjectedGradientTolerance( double value )
{
  m_ProjectedGradientTolerance = value;
  if( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_projected_gradient_tolerance(
      m_ProjectedGradientTolerance );
    }
  this->Modified();
}


/** Set/Get the MaximumNumberOfIterations. Default is 500 */
void
LBFGSBOptimizer
::SetMaximumNumberOfIterations( unsigned int value )
{
  m_MaximumNumberOfIterations = value;
  this->Modified();
}


/** Set/Get the MaximumNumberOfEvaluations. Default is 500 */
void
LBFGSBOptimizer
::SetMaximumNumberOfEvaluations( unsigned int value )
{
  m_MaximumNumberOfEvaluations = value;
  if( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_max_function_evals( m_MaximumNumberOfEvaluations );
    }
  this->Modified();
}


/** Set/Get the MaximumNumberOfCorrections. Default is 5 */
void
LBFGSBOptimizer
::SetMaximumNumberOfCorrections( unsigned int value )
{
  m_MaximumNumberOfCorrections = value;
  if( m_OptimizerInitialized )
    {
    m_VnlOptimizer->set_max_variable_metric_corrections(
      m_MaximumNumberOfCorrections );
    }
  this->Modified();
}


/**
 * Connect a Cost Function
 */
void
LBFGSBOptimizer::
SetCostFunction( SingleValuedCostFunction * costFunction )
{
  m_CostFunction = costFunction;

  const unsigned int numberOfParameters = 
    costFunction->GetNumberOfParameters();

  CostFunctionAdaptorType * adaptor = 
    new CostFunctionAdaptorType( numberOfParameters );
       
  adaptor->SetCostFunction( costFunction );

  if( m_OptimizerInitialized )
    { 
    delete m_VnlOptimizer;
    }
    
  this->SetCostFunctionAdaptor( adaptor );

  m_VnlOptimizer = new InternalOptimizerType( *adaptor, this );

  // set the optimizer parameters
  m_VnlOptimizer->set_lower_bound( m_LowerBound );
  m_VnlOptimizer->set_upper_bound( m_UpperBound );
  m_VnlOptimizer->set_bound_selection( m_BoundSelection );
  m_VnlOptimizer->set_cost_function_convergence_factor(
    m_CostFunctionConvergenceFactor );
  m_VnlOptimizer->set_projected_gradient_tolerance( 
    m_ProjectedGradientTolerance );
  m_VnlOptimizer->set_max_function_evals( m_MaximumNumberOfEvaluations );
  m_VnlOptimizer->set_max_variable_metric_corrections(
    m_MaximumNumberOfCorrections );

  m_OptimizerInitialized = true;

  this->Modified();
}


/**
 * Start the optimization
 */
void
LBFGSBOptimizer
::StartOptimization( void )
{
  
  // Check if all the bounds parameters are the same size as the initial parameters.
  unsigned int numberOfParameters = m_CostFunction->GetNumberOfParameters();

  if ( this->GetInitialPosition().Size() < numberOfParameters )
    {
    itkExceptionMacro( << "InitialPosition array does not have sufficient number of elements" );
    }

  if ( m_LowerBound.size() < numberOfParameters )
    {
    itkExceptionMacro( << "LowerBound array does not have sufficient number of elements" );
    }

  if ( m_UpperBound.size() < numberOfParameters )
    {
    itkExceptionMacro( << "UppperBound array does not have sufficient number of elements" );
    }

  if ( m_BoundSelection.size() < numberOfParameters )
    {
    itkExceptionMacro( << "BoundSelection array does not have sufficient number of elements" );
    }

  if( this->GetMaximize() )
    {
    this->GetNonConstCostFunctionAdaptor()->NegateCostFunctionOn();
    }

  this->SetCurrentPosition( this->GetInitialPosition() );

  ParametersType parameters( this->GetInitialPosition() );

  // vnl optimizers return the solution by reference 
  // in the variable provided as initial position
  m_VnlOptimizer->minimize( parameters );

  if ( parameters.size() != this->GetInitialPosition().Size() )
    {
    // set current position to initial position and throw an exception
    this->SetCurrentPosition( this->GetInitialPosition() );
    itkExceptionMacro( << "Error occured in optimization" );
    }

  this->SetCurrentPosition( parameters );

  this->InvokeEvent( EndEvent() );

}


/*-------------------------------------------------------------------------
 * helper class
 *-------------------------------------------------------------------------
 */




/** Create with a reference to the ITK object */
LBFGSBOptimizerHelper
::LBFGSBOptimizerHelper( vnl_cost_function& f,
                         LBFGSBOptimizer* itkObj )
  : vnl_lbfgsb( f ),
    m_itkObj( itkObj )
{
}


/** Handle new iteration event */
bool
LBFGSBOptimizerHelper
::report_iter()
{
  Superclass::report_iter();

  m_itkObj->m_InfinityNormOfProjectedGradient =
    this->get_inf_norm_projected_gradient();

  m_itkObj->InvokeEvent( IterationEvent() );

  m_itkObj->m_CurrentIteration = this->num_iterations_;

  // Return true to terminate the optimization loop.
  if( this->num_iterations_ > m_itkObj->m_MaximumNumberOfIterations )
    {
    return true;
    }
  else
    {
    return false;
    }
}


} // end namespace itk

#endif
