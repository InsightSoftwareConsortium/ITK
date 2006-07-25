/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLBFGSBOptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkLBFGSBOptimizer_txx
#define _itkLBFGSBOptimizer_txx

#include "itkLBFGSBOptimizer.h"

#include "v3p_netlib.h"
#include "v3p_f2c_mangle.h"

namespace itk
{

typedef long integer;
typedef double doublereal;
typedef long logical; // not bool
typedef long int ftnlen;

extern "C" void s_copy(char*,const char*,ftnlen,ftnlen);
extern "C" integer s_cmp(char*,const char*,ftnlen,ftnlen);

extern "C" int
setulb_(
integer *n, integer *m,
const doublereal *x, doublereal *l, doublereal *u,
integer *nbd,
doublereal *f, doublereal *g, doublereal *factr, doublereal *pgtol, doublereal *wa,
integer *iwa,
char *task,
integer *iprint,
char *csave,
logical *lsave,
integer *isave,
doublereal *dsave,
ftnlen task_len,
ftnlen csave_len );

/**
 * Constructor
 */
LBFGSBOptimizer
::LBFGSBOptimizer()
{
  m_LowerBound       = BoundValueType(0);
  m_UpperBound       = BoundValueType(0); 
  m_BoundSelection   = BoundSelectionType(0);

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


/**
 * Start the optimization
 */
void
LBFGSBOptimizer
::StartOptimization( void )
{
  
  /**
   * Check if all the bounds parameters are the same size as the initial parameters.
   */
  unsigned int numberOfParameters = m_CostFunction->GetNumberOfParameters();

  if ( this->GetInitialPosition().Size() < numberOfParameters )
    {
    itkExceptionMacro( << "InitialPosition array does not have sufficient number of elements" );
    }

  if ( m_LowerBound.Size() < numberOfParameters )
    {
    itkExceptionMacro( << "LowerBound array does not have sufficient number of elements" );
    }

  if ( m_UpperBound.Size() < numberOfParameters )
    {
    itkExceptionMacro( << "UppperBound array does not have sufficient number of elements" );
    }

  if ( m_BoundSelection.Size() < numberOfParameters )
    {
    itkExceptionMacro( << "BoundSelection array does not have sufficient number of elements" );
    }

  this->SetCurrentPosition( this->GetInitialPosition() );

  /**
   * Allocate memory for gradient and workspaces
   */
  integer n = numberOfParameters;
  integer m = m_MaximumNumberOfCorrections;

  Array<double>  gradient( n );                           // gradient
  Array<double>  wa( (2*m+4)*n + 12*m*m + 12*m );  // double array workspace

  Array<integer> iwa( 3* n );                     // integer array workspace

  /** String indicating current job */
  char task[60];
  s_copy( task, "START", (ftnlen)60, (ftnlen)5);
 
  /**  Control frequency and type of output */
  integer iprint = -1;  // no output

  /** Working string of characters */
  char csave[60];

  /** Logical working array */
  Array<logical> lsave(4);

  /** Integer working array */
  Array<integer> isave(44);

  /** Double working array */
  Array<double> dsave(29);

  // Initialize
  unsigned int numberOfEvaluations = 0;
  m_CurrentIteration = 0;

  this->InvokeEvent( StartEvent() );

  // Iteration looop
  for ( ;; )
    {

    /** Call the L-BFGS-B code */
    setulb_(&n, &m, this->GetCurrentPosition().data_block(), 
           (double *)m_LowerBound.data_block(), (double *)m_UpperBound.data_block(), 
           (long *)m_BoundSelection.data_block(),
           &m_Value, gradient.data_block(), 
           &m_CostFunctionConvergenceFactor, &m_ProjectedGradientTolerance, 
           wa.data_block(), iwa.data_block(),
           task, &iprint, csave, lsave.data_block(), isave.data_block(), 
           dsave.data_block(), (ftnlen)60, (ftnlen)60 ) ;

     /** Check return code.
      * 'FG_*'  = request to evaluate f & g for the current x and continue
      * 'NEW_X' = return with new iterate - continue the iteration w/out evaluation
      * 'ERROR' = error in input arguments
      * 'CONVERGENCE' = convergence has been reached
      */

    if ( s_cmp(task, "FG", (ftnlen)2, (ftnlen)2) == 0 )
      {

      m_CostFunction->GetValueAndDerivative( this->GetCurrentPosition(), m_Value, gradient );
      numberOfEvaluations++;

      }
    else if ( s_cmp( task, "NEW_X", (ftnlen)5, (ftnlen)5) == 0 )
      {

      m_InfinityNormOfProjectedGradient = dsave[12];
      this->InvokeEvent( IterationEvent() );
      m_CurrentIteration++;

      }
    else
      {
        // terminate

      if( s_cmp( task, "CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL", 
        (ftnlen)48, (ftnlen)48) == 0 )
        {
        itkDebugMacro( << "Convergence: gradient tolerance reached." );
        break;
        }

      if( s_cmp( task, "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH", 
        (ftnlen)47, (ftnlen)47) == 0 )
        {
        itkDebugMacro( << "Convergence: function tolerance reached." );
        break;
        }

      if ( s_cmp( task, "ERROR", (ftnlen)5, (ftnlen)5) == 0 )
        {
        itkDebugMacro( << "Error: dodgy input." );
        break;
        }

      // unknown error
      itkDebugMacro( << "Unknown error." );
      break;
        
      }

    /** Check if we have exceeded the maximum number of iterations */
    if ( numberOfEvaluations > m_MaximumNumberOfEvaluations || 
      m_CurrentIteration > m_MaximumNumberOfIterations ) 
      {
      itkDebugMacro( << "Exceeded maximum number of iterations." );
      break;
      }

    }

  this->InvokeEvent( EndEvent() );

}

} // end namespace itk

#endif
