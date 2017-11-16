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
#include "itkLBFGS2Optimizerv4.h"
#include "itkMacro.h"
#include "itkMath.h"

#include "lbfgs.h"

namespace itk
{

class LBFGS2Optimizerv4::PrivateImplementationHolder
{
public:
  lbfgs_parameter_t m_Parameters;
};

LBFGS2Optimizerv4
  ::LBFGS2Optimizerv4()
    :m_Pimpl(new PrivateImplementationHolder, true)
{
  //Initialize to default paramaters
  lbfgs_parameter_init( &m_Pimpl->m_Parameters );
  m_StatusCode = 100;
}


LBFGS2Optimizerv4
::~LBFGS2Optimizerv4()
{
}

void
LBFGS2Optimizerv4
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m: "
     << m_Pimpl->m_Parameters.m << std::endl;
  os << indent << "epsilon: "
     << m_Pimpl->m_Parameters.epsilon << std::endl;
  os << indent << "past: "
     << m_Pimpl->m_Parameters.past << std::endl;
  os << indent << "delta: "
     << m_Pimpl->m_Parameters.delta << std::endl;
  os << indent << "max_iterations: "
     << m_Pimpl->m_Parameters.max_iterations << std::endl;
  os << indent << "linesearch: "
     << m_Pimpl->m_Parameters.linesearch << std::endl;
  os << indent << "max_linesearch: "
     << m_Pimpl->m_Parameters.max_linesearch << std::endl;
  os << indent << "min_step: "
     << m_Pimpl->m_Parameters.min_step << std::endl;
  os << indent << "max_step: "
     << m_Pimpl->m_Parameters.max_step << std::endl;
  os << indent << "ftol: "
     << m_Pimpl->m_Parameters.ftol << std::endl;
  os << indent << "wolfe: "
     << m_Pimpl->m_Parameters.wolfe << std::endl;
  os << indent << "gtol: "
     << m_Pimpl->m_Parameters.gtol << std::endl;
  os << indent << "xtol: "
     << m_Pimpl->m_Parameters.xtol << std::endl;
  os << indent << "orthantwise_c: "
     << m_Pimpl->m_Parameters.orthantwise_c << std::endl;
  os << indent << "orthantwise_start: "
     << m_Pimpl->m_Parameters.orthantwise_start << std::endl;
  os << indent << "orthantwise_end: "
     << m_Pimpl->m_Parameters.orthantwise_end << std::endl;
}


//Register callbacks and call the lbfgs routine
void
LBFGS2Optimizerv4
::StartOptimization(bool doOnlyInitialization)
{

  //Check if everything is setup correctly
  Superclass::StartOptimization( doOnlyInitialization );
  if( this->GetMetric()->HasLocalSupport() )
    {
    itkExceptionMacro("The assigned transform has local-support. This is not supported for this optimizer. See the optimizer documentation.");
    }

  this->InvokeEvent( StartEvent() );

  //Copy paramaters
  const ParametersType &parameters = this->m_Metric->GetParameters();

  int N = parameters.GetSize();
  if( N == 0 )
    {
    itkExceptionMacro(<<"Optimizer parameters are not initialized.");
    }

  //TODO: only needed if SSE is enabled
  LBFGS2Optimizerv4::PrecisionType *x = lbfgs_malloc(N);

  std::memcpy(x, parameters.data_block(), sizeof(LBFGS2Optimizerv4::PrecisionType)*N );

  //Run lbfgs
  m_StatusCode = lbfgs( N, x, &this->m_CurrentMetricValue,
                        LBFGS2Optimizerv4::EvaluateCostCallback,
                        LBFGS2Optimizerv4::UpdateProgressCallback,
                        this, &m_Pimpl->m_Parameters );

  // Match the behavior of other optimizer setting the current
  // iteration to the max when iteration limit is reached
  if (m_StatusCode == LBFGSERR_MAXIMUMITERATION)
    {
    ++this->m_CurrentIteration;
    }


  //Copy results
  ParametersType optimizedParameters(N);
  std::memcpy(optimizedParameters.data_block(), x, sizeof(LBFGS2Optimizerv4::PrecisionType)*N );

  lbfgs_free(x);

  this->m_Metric->SetParameters( optimizedParameters );
}


//LBFGS method callbacks
LBFGS2Optimizerv4::PrecisionType
LBFGS2Optimizerv4::EvaluateCostCallback( void *instance,
                                         const LBFGS2Optimizerv4::PrecisionType *x,
                                         LBFGS2Optimizerv4::PrecisionType *g,
                                         const int n,
                                         const LBFGS2Optimizerv4::PrecisionType step
                                       )
{
  LBFGS2Optimizerv4 *optimizer = reinterpret_cast< LBFGS2Optimizerv4* >(instance);
  return optimizer->EvaluateCost(x, g, n,step);
}

LBFGS2Optimizerv4::PrecisionType
LBFGS2Optimizerv4::EvaluateCost( const LBFGS2Optimizerv4::PrecisionType *x,
                                 LBFGS2Optimizerv4::PrecisionType *g,
                                 const int n,
                                 const LBFGS2Optimizerv4::PrecisionType
                               )
{

  static ParametersType xItk(n);
  std::memcpy(xItk.data_block(), x, n * sizeof(LBFGS2Optimizerv4::PrecisionType) );

  DerivativeType gItk(n);
  gItk.SetData(g, n, false);

  MeasureType value;

  this->m_Metric->SetParameters( xItk );
  this->m_Metric->GetValueAndDerivative(value, gItk);

  gItk *= - 1;

  return value;
}

int
LBFGS2Optimizerv4::UpdateProgressCallback( void *instance,
                                           const LBFGS2Optimizerv4::PrecisionType *x,
                                           const LBFGS2Optimizerv4::PrecisionType *g,
                                           const LBFGS2Optimizerv4::PrecisionType fx,
                                           const LBFGS2Optimizerv4::PrecisionType xnorm,
                                           const LBFGS2Optimizerv4::PrecisionType gnorm,
                                           const LBFGS2Optimizerv4::PrecisionType step,
                                           int n,
                                           int k,
                                           int ls
                                         ){
  LBFGS2Optimizerv4 *optimizer = reinterpret_cast< LBFGS2Optimizerv4* >(instance);
  return optimizer->UpdateProgress(x, g, fx, xnorm, gnorm, step, n, k, ls);
}

int
LBFGS2Optimizerv4::UpdateProgress( const LBFGS2Optimizerv4::PrecisionType *x,
                                   const LBFGS2Optimizerv4::PrecisionType *g,
                                   const LBFGS2Optimizerv4::PrecisionType fx,
                                   const LBFGS2Optimizerv4::PrecisionType xnorm,
                                   const LBFGS2Optimizerv4::PrecisionType gnorm,
                                   const LBFGS2Optimizerv4::PrecisionType step,
                                   int,
                                   int k,
                                   int ls
                                 )
{
  // Convert to 0-based ITK iteration counting
  this->m_CurrentIteration = k - 1;
  this->m_CurrentMetricValue = fx;

  m_CurrentGradient = g;
  m_CurrentParameter = x;
  m_CurrentParameterNorm = xnorm;
  m_CurrentGradientNorm =  gnorm;
  m_CurrentStepSize = step;
  m_CurrentNumberOfEvaluations = ls;

  this->InvokeEvent( IterationEvent() );
  return 0;
}

const LBFGS2Optimizerv4::StopConditionReturnStringType
LBFGS2Optimizerv4::GetStopConditionDescription() const{
  switch(m_StatusCode){
    case 100:
      return "Optimization not started";
    case LBFGS_SUCCESS:
      return "Converged";
    case LBFGS_ALREADY_MINIMIZED:
      return "Already minimized";
    case LBFGSERR_UNKNOWNERROR:
      return "Unknown errer";
    case LBFGSERR_LOGICERROR:
      return "Logic error";
    case LBFGSERR_OUTOFMEMORY:
      return "Out of memmory";
    case LBFGSERR_CANCELED:
      return "Optmization canceled";
    case LBFGSERR_INVALID_N:
      return "Invlaid number of variables";
    case LBFGSERR_INVALID_N_SSE:
      return "Invalid number of variables for SSE";
    case LBFGSERR_INVALID_X_SSE:
      return "Invalid alignment of variables for SSE";
    case LBFGSERR_INVALID_EPSILON:
      return "Invalid solution accuracy paramater";
    case LBFGSERR_INVALID_TESTPERIOD:
      return "Invalid delta convergence distance";
    case LBFGSERR_INVALID_DELTA:
      return "Invalid delta convergence tolerance";
    case LBFGSERR_INVALID_LINESEARCH:
      return "Invalid linesearch specified";
    case LBFGSERR_INVALID_MINSTEP:
      return "Invalid minimum linesearch step";
    case LBFGSERR_INVALID_MAXSTEP:
      return "Invalid maximum linesearch step";
    case LBFGSERR_INVALID_FTOL:
      return "Invalid linesearch accuracy";
    case LBFGSERR_INVALID_WOLFE:
      return "Invalid wolfe coefficient";
    case LBFGSERR_INVALID_GTOL:
      return "Invalid lnesearch gradient accuracy";
    case LBFGSERR_INVALID_XTOL:
      return "Invalid machine precision tolerance";
    case LBFGSERR_INVALID_MAXLINESEARCH:
      return "Invalid maximum linesearch iterastions";
    case LBFGSERR_INVALID_ORTHANTWISE:
      return "Invalid orthantwise coefficient";
    case LBFGSERR_INVALID_ORTHANTWISE_START:
      return "Invalid orthantwise start";
    case LBFGSERR_INVALID_ORTHANTWISE_END:
      return "Invalid orthantwise end";
    case LBFGSERR_OUTOFINTERVAL:
      return "LineSearch went outside of the uncertainty interval";
    case LBFGSERR_INCORRECT_TMINMAX:
      return "Interval of uncretainty beecame too small";
    case LBFGSERR_ROUNDING_ERROR:
      return "A roundng error occured";
    case LBFGSERR_MINIMUMSTEP:
      return "LineSearch became smaller than the minimum linesearch step size";
    case LBFGSERR_MAXIMUMSTEP:
      return "LineSearch became bigger than the maximum linesearch step size";
    case LBFGSERR_MAXIMUMLINESEARCH:
      return "LineSearch reched maximum umber of iterations";
    case LBFGSERR_MAXIMUMITERATION:
      return "Reached maximum number of iterations";
    case LBFGSERR_WIDTHTOOSMALL:
      return "Relative width of the interval became too small (less than machine tolerance)";
    case LBFGSERR_INVALIDPARAMETERS:
      return "Negative line search step occured";
    case LBFGSERR_INCREASEGRADIENT:
      return "Current search direction increases objective funuction";
  }
  return "Unknown status";
}

//Disallow setting scale and weight
void
LBFGS2Optimizerv4
::SetScales(const ScalesType &)
{
  itkWarningMacro( << "LBFGS optimizer does not support scaling. All scales are set to one." )
  m_Scales.SetSize( this->m_Metric->GetNumberOfLocalParameters() );
  m_Scales.Fill( NumericTraits<ScalesType::ValueType>::OneValue() );
  this->m_ScalesAreIdentity = true;
}

void
LBFGS2Optimizerv4
::SetWeights(const ScalesType )
{
  itkWarningMacro( << "LBFGS optimizer does not support weights. All weights are set to one." )
  m_Weights.SetSize( this->m_Metric->GetNumberOfLocalParameters() );
  m_Weights.Fill( NumericTraits<ScalesType::ValueType>::OneValue() );
  this->m_WeightsAreIdentity = true;
}

//A bunch of Set/Get methods for setting lbfgs paramaters
void
LBFGS2Optimizerv4::SetHessianApproximationAccuracy( int m ){
  m_Pimpl->m_Parameters.m = m;
  this->Modified();
}

int
LBFGS2Optimizerv4::GetHessianApproximationAccuracy() const{
  return m_Pimpl->m_Parameters.m;
}

void
LBFGS2Optimizerv4::SetSolutionAccuracy(LBFGS2Optimizerv4::PrecisionType epsilon){
  m_Pimpl->m_Parameters.epsilon = epsilon;
  this->Modified();
}


LBFGS2Optimizerv4::PrecisionType
LBFGS2Optimizerv4::GetSolutionAccuracy() const{
  return m_Pimpl->m_Parameters.epsilon;
}

void
LBFGS2Optimizerv4::SetDeltaConvergenceDistance(int nPast){
  m_Pimpl->m_Parameters.past = nPast;
  this->Modified();
}

int
LBFGS2Optimizerv4::GetDeltaConvergenceDistance() const{
  return m_Pimpl->m_Parameters.past;
}

void
LBFGS2Optimizerv4::SetDeltaConvergenceTolerance(LBFGS2Optimizerv4::PrecisionType tol){
  m_Pimpl->m_Parameters.delta = tol;
  this->Modified();
}

LBFGS2Optimizerv4::PrecisionType
LBFGS2Optimizerv4::GetDeltaConvergenceTolerance() const{
  return m_Pimpl->m_Parameters.delta;
}

void
LBFGS2Optimizerv4::SetMaximumIterations(int maxIterations){
 m_Pimpl->m_Parameters.max_iterations = maxIterations;
 this->Modified();
}

int
LBFGS2Optimizerv4::GetMaximumIterations() const{
  return m_Pimpl->m_Parameters.max_iterations;
}

//translate to lbfgs.h enum
//this translation is kind of annoying and error prone
//but avoids exposing lbfgs.h
void
LBFGS2Optimizerv4::SetLineSearch(const LBFGS2Optimizerv4::LineSearchMethod &linesearch)
{
  //
  int lbfgsLineSearch = LINESEARCH_DEFAULT;
  if( linesearch == LINESEARCH_BACKTRACKING)
    {
    lbfgsLineSearch = LBFGS_LINESEARCH_BACKTRACKING;
    }
  else if( linesearch == LINESEARCH_BACKTRACKING_ARMIJO )
    {
    lbfgsLineSearch = LBFGS_LINESEARCH_BACKTRACKING_ARMIJO;
    }
  else if( linesearch == LINESEARCH_BACKTRACKING_WOLFE )
    {
    lbfgsLineSearch = LBFGS_LINESEARCH_BACKTRACKING_WOLFE;
    }
  else if( linesearch == LINESEARCH_BACKTRACKING_STRONG_WOLFE )
    {
    lbfgsLineSearch = LBFGS_LINESEARCH_BACKTRACKING_STRONG_WOLFE;
    }
  else if( linesearch == LINESEARCH_MORETHUENTE )
    {
    lbfgsLineSearch = LBFGS_LINESEARCH_MORETHUENTE;
    }

  m_Pimpl->m_Parameters.linesearch = lbfgsLineSearch;
  this->Modified();
}

LBFGS2Optimizerv4::LineSearchMethod
LBFGS2Optimizerv4::GetLineSearch() const{
  LineSearchMethod linesearch = LINESEARCH_DEFAULT;
  int lbfgsLineSearch = m_Pimpl->m_Parameters.linesearch;
  if( lbfgsLineSearch == LBFGS_LINESEARCH_BACKTRACKING)
    {
     linesearch = LINESEARCH_BACKTRACKING;
    }
  else if( lbfgsLineSearch == LBFGS_LINESEARCH_BACKTRACKING_ARMIJO )
    {
    linesearch = LINESEARCH_BACKTRACKING_ARMIJO;
    }
  else if( lbfgsLineSearch == LBFGS_LINESEARCH_BACKTRACKING_WOLFE )
    {
    linesearch = LINESEARCH_BACKTRACKING_WOLFE;
    }
  else if( lbfgsLineSearch == LBFGS_LINESEARCH_BACKTRACKING_STRONG_WOLFE )
    {
    linesearch = LINESEARCH_BACKTRACKING_STRONG_WOLFE;
    }
  else if( lbfgsLineSearch == LBFGS_LINESEARCH_MORETHUENTE )
    {
    linesearch = LINESEARCH_MORETHUENTE;
    }

  return linesearch;
}


void
LBFGS2Optimizerv4::SetMaximumLineSearchEvaluations(int n){
  m_Pimpl->m_Parameters.max_linesearch = n;
  this->Modified();
}

int
LBFGS2Optimizerv4::GetMaximumLineSearchEvaluations() const{
  return m_Pimpl->m_Parameters.max_linesearch;
}

void
LBFGS2Optimizerv4::SetMinimumLineSearchStep(LBFGS2Optimizerv4::PrecisionType step){
  m_Pimpl->m_Parameters.min_step = step;
  this->Modified();
}

LBFGS2Optimizerv4::PrecisionType
LBFGS2Optimizerv4::GetMinimumLineSearchStep() const{
  return m_Pimpl->m_Parameters.min_step;
}

void
LBFGS2Optimizerv4::SetMaximumLineSearchStep(LBFGS2Optimizerv4::PrecisionType step){
  m_Pimpl->m_Parameters.max_step = step;

  this->Modified();
}

LBFGS2Optimizerv4::PrecisionType
LBFGS2Optimizerv4::GetMaximumLineSearchStep() const{
  return m_Pimpl->m_Parameters.max_step;
}

void
LBFGS2Optimizerv4::SetLineSearchAccuracy( LBFGS2Optimizerv4::PrecisionType ftol ){
  m_Pimpl->m_Parameters.ftol = ftol;

  this->Modified();
}

LBFGS2Optimizerv4::PrecisionType
LBFGS2Optimizerv4::GetLineSearchAccuracy() const{
  return m_Pimpl->m_Parameters.ftol;
}


void
LBFGS2Optimizerv4::SetWolfeCoefficient( LBFGS2Optimizerv4::PrecisionType wc ){
  m_Pimpl->m_Parameters.wolfe = wc;
  this->Modified();
}

LBFGS2Optimizerv4::PrecisionType
LBFGS2Optimizerv4::GetWolfeCoefficient() const{
  return m_Pimpl->m_Parameters.wolfe;
}

void
LBFGS2Optimizerv4::SetLineSearchGradientAccuracy( LBFGS2Optimizerv4::PrecisionType gtol ){
  m_Pimpl->m_Parameters.gtol = gtol;
  this->Modified();
}

LBFGS2Optimizerv4::PrecisionType
LBFGS2Optimizerv4::GetLineSearchGradientAccuracy() const{
  return m_Pimpl->m_Parameters.gtol;
}

void
LBFGS2Optimizerv4::SetMachinePrecisionTolerance(LBFGS2Optimizerv4::PrecisionType xtol){
  m_Pimpl->m_Parameters.xtol = xtol;
  this->Modified();
}

LBFGS2Optimizerv4::PrecisionType
LBFGS2Optimizerv4::GetMachinePrecisionTolerance() const{
  return m_Pimpl->m_Parameters.xtol;
}

void
LBFGS2Optimizerv4::SetOrthantwiseCoefficient(LBFGS2Optimizerv4::PrecisionType orthant_c){
  m_Pimpl->m_Parameters.orthantwise_c = orthant_c;
  this->Modified();
}

LBFGS2Optimizerv4::PrecisionType
LBFGS2Optimizerv4::GetOrthantwiseCoefficient() const{
  return m_Pimpl->m_Parameters.orthantwise_c;
}

void
LBFGS2Optimizerv4::SetOrthantwiseStart(int start){
  m_Pimpl->m_Parameters.orthantwise_start = start;
  this->Modified();
}

int
LBFGS2Optimizerv4::GetOrthantwiseStart() const{
  return m_Pimpl->m_Parameters.orthantwise_start;
}

void
LBFGS2Optimizerv4::SetOrthantwiseEnd(int end){
  m_Pimpl->m_Parameters.orthantwise_end = end;
  this->Modified();
}

int
LBFGS2Optimizerv4::GetOrthantwiseEnd() const{
  return m_Pimpl->m_Parameters.orthantwise_end;
}


} // end namespace itk
