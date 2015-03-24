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

#include "itkAmoebaOptimizer.h"

namespace itk
{


AmoebaOptimizer
::AmoebaOptimizer() :
  m_InitialSimplexDelta(1)
{
  this->m_MaximumNumberOfIterations      = 500;
  this->m_ParametersConvergenceTolerance = 1e-8;
  this->m_FunctionConvergenceTolerance   = 1e-4;
  this->m_AutomaticInitialSimplex        = true;
  this->m_InitialSimplexDelta.Fill( NumericTraits< ParametersType::ValueType >::OneValue() );
  this->m_OptimizeWithRestarts = false;
  this->m_VnlOptimizer = ITK_NULLPTR;
}


AmoebaOptimizer
::~AmoebaOptimizer()
{
  delete m_VnlOptimizer;
}


const std::string
AmoebaOptimizer
::GetStopConditionDescription() const
{
  return this->m_StopConditionDescription.str();
}


void
AmoebaOptimizer
::PrintSelf( std::ostream & os, Indent indent ) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "MaximumNumberOfIterations: "
     << this->m_MaximumNumberOfIterations << std::endl;
  os << indent << "ParametersConvergenceTolerance: "
     << this->m_ParametersConvergenceTolerance << std::endl;
  os << indent << "FunctionConvergenceTolerance: "
     << this->m_FunctionConvergenceTolerance << std::endl;
  os << indent << "AutomaticInitialSimplex: "
     << ( this->m_AutomaticInitialSimplex ? "On" : "Off" ) << std::endl;
  os << indent << "InitialSimplexDelta: "
     << this->m_InitialSimplexDelta << std::endl;
}


AmoebaOptimizer::MeasureType
AmoebaOptimizer
::GetValue() const
{
  ParametersType parameters = this->GetCurrentPosition();
  const unsigned int numberOfParameters = parameters.Size();
  SingleValuedNonLinearVnlOptimizer::CostFunctionAdaptorType *costFunction =
    this->GetNonConstCostFunctionAdaptor();

  if( costFunction != ITK_NULLPTR )
    {
    if( static_cast<unsigned int>(costFunction->get_number_of_unknowns()) !=
        numberOfParameters )
      {
      itkExceptionMacro(<< "cost function and current position dimensions mismatch");
      }
    }
  else
    {
    itkExceptionMacro(<< "cost function not set");
    }

  if ( m_ScalesInitialized )
    {
    const ScalesType & scales = this->GetScales();
    for ( unsigned int i = 0; i < numberOfParameters; i++ )
      {
      parameters[i] *= scales[i];
      }
    }
  return costFunction->f( parameters );
}

/** Get the Optimizer */
vnl_amoeba *
AmoebaOptimizer
::GetOptimizer() const
{
  return this->m_VnlOptimizer;
}

void
AmoebaOptimizer::
SetInitialSimplexDelta( ParametersType initialSimplexDelta,
                        bool automaticInitialSimplex )
{
  this->m_InitialSimplexDelta = initialSimplexDelta;
  this->m_AutomaticInitialSimplex = automaticInitialSimplex;
  this->Modified();
}


void
AmoebaOptimizer
::SetCostFunction(SingleValuedCostFunction *costFunction)
{
       //call our ancestors SetCostFunction, we are overriding it - this would
       //be the correct thing to do so that the GetCostFunction() would work
       //correctly. Unfortunately, there is a side effect to
       //this function call, it also sets the scales to one if they haven't been
       //initialized yet. This causes the optimization to use the scales which
       //only increases the computationaly complexity without any benefit.
       //Right now the result of GetCostFunction() will be a null pointer.
  //SingleValuedNonLinearOptimizer::SetCostFunction( costFunction );

                    //if cost function is ITK_NULLPTR this will throw an exception
                    //when the pointer is dereferenced
  CostFunctionAdaptorType *adaptor =
    new CostFunctionAdaptorType( costFunction->GetNumberOfParameters() );
  adaptor->SetCostFunction( costFunction );
              //our ancestor, SingleValuedNonLinearVnlOptimizer, will release
              //the adaptor's memory in its destructor or if it is set again
  this->SetCostFunctionAdaptor( adaptor );
  this->Modified();
}


void
AmoebaOptimizer
::StartOptimization(void)
{
  const ScalesType & scales = GetScales();
  const ParametersType &initialPosition = GetInitialPosition();
  InternalParametersType delta( m_InitialSimplexDelta );
  SingleValuedNonLinearVnlOptimizer::CostFunctionAdaptorType *costFunction =
    this->GetCostFunctionAdaptor();
  unsigned int n =
    static_cast<unsigned int>( costFunction->get_number_of_unknowns() );

      //validate the settings (cost function is initialized, the size of its
      //expected parameter vector matches the one we have etc...)
  this->ValidateSettings();

           //start the actual work
  this->InvokeEvent( StartEvent() );

              //configure the vnl optimizer
  CostFunctionAdaptorType *adaptor = GetNonConstCostFunctionAdaptor();
       //get rid of previous instance of the internal optimizer and create a
       //new one
  delete m_VnlOptimizer;
  m_VnlOptimizer = new vnl_amoeba( *adaptor );
  m_VnlOptimizer->set_max_iterations( static_cast< int >( m_MaximumNumberOfIterations ) );
  m_VnlOptimizer->set_x_tolerance(m_ParametersConvergenceTolerance);
  m_VnlOptimizer->set_f_tolerance(m_FunctionConvergenceTolerance);

  m_StopConditionDescription.str( "" );
  m_StopConditionDescription << this->GetNameOfClass() << ": Running";

  if( GetMaximize() )
    {
    adaptor->NegateCostFunctionOn();
    }

  this->SetCurrentPosition( initialPosition );

  ParametersType parameters( initialPosition );
  ParametersType bestPosition( initialPosition );

  // If the user provides the scales then we set otherwise we don't
  // for computation speed.
  // We also scale the initial parameters up if scales are defined.
  // This compensates for later scaling them down in the cost function adaptor
  // and at the end of this function.
  if ( m_ScalesInitialized )
    {
    adaptor->SetScales( scales );
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
    double currentValue;
    unsigned int totalEvaluations = static_cast<unsigned int>
      (m_VnlOptimizer->get_num_evaluations());
    bool converged = false;
    unsigned int i=1;
    while( !converged && ( totalEvaluations < m_MaximumNumberOfIterations ) )
      {
      this->m_VnlOptimizer->set_max_iterations(
        static_cast< int >( this->m_MaximumNumberOfIterations - totalEvaluations ) );
      parameters = bestPosition;
      delta = delta*( 1.0/pow( 2.0, static_cast<double>(i) ) *
                     (rand() > RAND_MAX/2 ? 1 : -1) );
      m_VnlOptimizer->minimize( parameters, delta );
      totalEvaluations += static_cast<unsigned int>
                          (m_VnlOptimizer->get_num_evaluations());
      currentValue = adaptor->f( parameters );
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
               //this comparison is valid both for min and max because the
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
  if ( m_ScalesInitialized )
    {
    const ScalesType & invScales = this->GetInverseScales();
    for ( unsigned int i = 0; i < n; ++i )
      {
      bestPosition[i] *= invScales[i];
      }
    }

  this->SetCurrentPosition( bestPosition );

  this->m_StopConditionDescription.str( "" );
  this->m_StopConditionDescription << this->GetNameOfClass() << ": ";
  if ( static_cast< unsigned int >( this->m_VnlOptimizer->get_num_evaluations() )
       < this->m_MaximumNumberOfIterations )
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
                                     << this->m_MaximumNumberOfIterations;
    }
  this->InvokeEvent( EndEvent() );
}


void
AmoebaOptimizer
::ValidateSettings()
{
  //we have to have a cost function
  if( GetCostFunctionAdaptor() == ITK_NULLPTR )
    {
    itkExceptionMacro(<<"ITK_NULLPTR cost function")
    }
  //if we got here it is safe to get the number of parameters the cost
  //function expects
  unsigned int n =
    static_cast<unsigned int>( (GetCostFunctionAdaptor() )->get_number_of_unknowns() );

  //check that the number of parameters match
  if( GetInitialPosition().Size() != n )
    {
    itkExceptionMacro(<<"cost function and initial position dimensions mismatch")
    }

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
  if ( m_ScalesInitialized )
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
