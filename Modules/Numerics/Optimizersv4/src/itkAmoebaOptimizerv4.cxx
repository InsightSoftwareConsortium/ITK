/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkAmoebaOptimizerv4.h"

namespace itk
{


AmoebaOptimizerv4
::AmoebaOptimizerv4() :
  m_InitialSimplexDelta(1)
{
  this->m_NumberOfIterations      = 500;
  this->m_ParametersConvergenceTolerance = 1e-8;
  this->m_FunctionConvergenceTolerance   = 1e-4;
  this->m_AutomaticInitialSimplex        = true;
  this->m_InitialSimplexDelta.Fill( NumericTraits< ParametersType::ValueType >::OneValue() );
  this->m_OptimizeWithRestarts = false;
  this->m_VnlOptimizer = ITK_NULLPTR;
}


AmoebaOptimizerv4
::~AmoebaOptimizerv4()
{
  delete m_VnlOptimizer;
}


const std::string
AmoebaOptimizerv4
::GetStopConditionDescription() const
{
  return this->m_StopConditionDescription.str();
}


void
AmoebaOptimizerv4
::PrintSelf( std::ostream & os, Indent indent ) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "ParametersConvergenceTolerance: "
     << this->m_ParametersConvergenceTolerance << std::endl;
  os << indent << "FunctionConvergenceTolerance: "
     << this->m_FunctionConvergenceTolerance << std::endl;
  os << indent << "AutomaticInitialSimplex: "
     << ( this->m_AutomaticInitialSimplex ? "On" : "Off" ) << std::endl;
  os << indent << "InitialSimplexDelta: "
     << this->m_InitialSimplexDelta << std::endl;
}


vnl_amoeba *
AmoebaOptimizerv4
::GetOptimizer() const
{
  return this->m_VnlOptimizer;
}

void
AmoebaOptimizerv4::
SetInitialSimplexDelta( ParametersType initialSimplexDelta,
                        bool automaticInitialSimplex )
{
  this->m_InitialSimplexDelta = initialSimplexDelta;
  this->m_AutomaticInitialSimplex = automaticInitialSimplex;
  this->Modified();
}


void
AmoebaOptimizerv4
::SetMetric(MetricType *metric)
{
  this->m_Metric = metric;

  //if cost function is ITK_NULLPTR this will throw an exception when the pointer is dereferenced
  const unsigned int numberOfParameters = metric->GetNumberOfParameters();

  class AmoebaCostFunctionAdaptorv4:
    public  SingleValuedVnlCostFunctionAdaptorv4
    {
    public:
      typedef SingleValuedVnlCostFunctionAdaptorv4 Superclass;
      typedef AmoebaOptimizerv4                    ITKOptimizerType;

      AmoebaCostFunctionAdaptorv4(unsigned int spaceDimension, ITKOptimizerType *itkObj)
        : SingleValuedVnlCostFunctionAdaptorv4(spaceDimension),
          m_ItkObj(itkObj)
        {
        }

      Superclass::InternalMeasureType f(const Superclass::InternalParametersType & inparameters)
        {
          const  Superclass::InternalMeasureType &ret = Superclass::f( inparameters );
          ++m_ItkObj->m_CurrentIteration;
          return ret;
        }
    protected:
      Self *m_ItkObj;

  };

  // assign to vnl cost-function adaptor
  CostFunctionAdaptorType *adaptor = new AmoebaCostFunctionAdaptorv4( numberOfParameters, this );
  adaptor->SetCostFunction( metric );
  this->SetCostFunctionAdaptor( adaptor );
  this->Modified();
}


void
AmoebaOptimizerv4
::StartOptimization(bool /* doOnlyInitialization */)
{
  // Perform some verification, check scales,
  // pass settings to cost-function adaptor.
  Superclass::StartOptimization();

  //validate the settings (cost function is initialized, the size of its
  //expected parameter vector matches the one we have etc...)
  this->ValidateSettings();

  ParametersType parameters = this->m_Metric->GetParameters();
  unsigned int n = parameters.GetSize();

  InternalParametersType delta( m_InitialSimplexDelta );

  //start the actual work
  this->InvokeEvent( StartEvent() );

  //configure the vnl optimizer
  CostFunctionAdaptorType *adaptor = GetNonConstCostFunctionAdaptor();
  //get rid of previous instance of the internal optimizer and create a
  //new one
  delete m_VnlOptimizer;
  m_VnlOptimizer = new vnl_amoeba( *adaptor );
  m_VnlOptimizer->set_max_iterations( static_cast< int >( m_NumberOfIterations ) );
  m_VnlOptimizer->set_x_tolerance(m_ParametersConvergenceTolerance);
  m_VnlOptimizer->set_f_tolerance(m_FunctionConvergenceTolerance);

  m_StopConditionDescription.str( "" );
  m_StopConditionDescription << this->GetNameOfClass() << ": Running";

  ParametersType bestPosition = parameters;

  // Scale the initial parameters up if scales are defined.
  // This compensates for later scaling them down in the cost function adaptor
  // and at the end of this function.
  const ScalesType & scales = GetScales();
  if ( !this->GetScalesAreIdentity() )
    {
    for ( unsigned int i = 0; i < n; i++ )
      {
      parameters[i] *= scales[i];
      }
    }
  //copy the automated initialization from vnl so that we have
  //the same control as when the user provides the initial simplex.
  //this also exposes the fact that there is an interaction between
  //the parameter scaling and the initial simplex when using
  //automated initialization - previously hidden inside vnl
  if ( this->m_AutomaticInitialSimplex )
    {
    const double relativeDiameter = 0.05;
    const double zeroTermDelta = 0.00025;
    InternalParametersType automaticDelta(n);
    for( unsigned int i = 0; i < n; i++ )
      {
      if( fabs( parameters[i] ) > zeroTermDelta )
        {
        automaticDelta[i] = relativeDiameter*parameters[i];
        }
      else
        {
        automaticDelta[i] = zeroTermDelta;
        }
      }
    delta = automaticDelta;
    }

  this->m_VnlOptimizer->minimize( parameters, delta );
  bestPosition = parameters;
  double bestValue = adaptor->f( bestPosition );
  //multiple restart heuristic
  if( this->m_OptimizeWithRestarts )
    {
    bool converged = false;
    unsigned int i=1;
    while( !converged && ( this->m_CurrentIteration < m_NumberOfIterations ) )
      {
      this->m_VnlOptimizer->set_max_iterations(
        static_cast< int >( this->m_NumberOfIterations - this->m_CurrentIteration ) );
      parameters = bestPosition;
      delta = delta*( 1.0/pow( 2.0, static_cast<double>(i) ) *
                     (rand() > RAND_MAX/2 ? 1 : -1) );
      m_VnlOptimizer->minimize( parameters, delta );
      double currentValue = adaptor->f( parameters );
      // be consistent with the underlying vnl amoeba implementation
      double maxAbs = 0.0;
      for( unsigned j=0; j<n; j++ )
        {
        if( maxAbs< fabs( bestPosition[j] - parameters[j] ) )
          {
          maxAbs = fabs( bestPosition[j] - parameters[j] );
          }
        }
      converged = fabs( bestValue - currentValue ) <
                  this->m_FunctionConvergenceTolerance &&
                  maxAbs < this->m_ParametersConvergenceTolerance;
               //this comparison is valid because the
               //adaptor is set to always return the function value
               //corresponding to minimization
      if( currentValue < bestValue )
        {
        bestValue = currentValue;
        bestPosition = parameters;
        }
      i++;
      }
    }
       // get the results, we scale the parameters down if scales are defined
  if ( !this->GetScalesAreIdentity() )
    {
    for ( unsigned int i = 0; i < n; ++i )
      {
      bestPosition[i] /= scales[i];
      }
    }

  this->m_Metric->SetParameters( bestPosition );

  this->m_StopConditionDescription.str( "" );
  this->m_StopConditionDescription << this->GetNameOfClass() << ": ";
  if ( static_cast< unsigned int >( this->m_VnlOptimizer->get_num_evaluations() )
       < this->m_NumberOfIterations )
    {
    this->m_StopConditionDescription << "Both parameters convergence tolerance ("
                               << this->m_ParametersConvergenceTolerance
                               << ") and function convergence tolerance ("
                               << this->m_FunctionConvergenceTolerance
                               << ") have been met in "
                               << this->m_VnlOptimizer->get_num_evaluations()
                               << " iterations.";
    }
  else
    {
    this->m_StopConditionDescription << "Maximum number of iterations exceeded."
                                     << " Number of iterations is "
                                     << this->m_NumberOfIterations;
    }
  this->InvokeEvent( EndEvent() );
}


void
AmoebaOptimizerv4
::ValidateSettings()
{
  //if we got here it is safe to get the number of parameters the cost
  //function expects
  ParametersType parameters = this->m_Metric->GetParameters();
  unsigned int n = parameters.GetSize();

  //the user gave us data to use for the initial simplex, check that it
  //matches the number of parameters (simplex dimension is n+1 - the initial
  //position and n vertices defined by adding m_InitialSimplexDelta[i] to
  //the initial position
  if( !m_AutomaticInitialSimplex )
    {
      if( m_InitialSimplexDelta.size() != n )
      {
      itkExceptionMacro(<<"cost function and simplex delta dimensions mismatch")
      }
    }

  //check that the number of scale factors matches
  if ( this->GetScalesInitialized() )
    {
    if( this->GetScales().Size() != n )
      {
      itkExceptionMacro(<<"cost function and scaling information dimensions mismatch")
      }
    }

  //parameters' convergence tolerance has to be positive
  if ( this->m_ParametersConvergenceTolerance < 0 )
    {
    itkExceptionMacro(<<"negative parameters convergence tolerance")
    }
  //function convergence tolerance has to be positive
  if ( this->m_FunctionConvergenceTolerance < 0 )
    {
    itkExceptionMacro(<<"negative function convergence tolerance")
    }
}

} // end namespace itk
