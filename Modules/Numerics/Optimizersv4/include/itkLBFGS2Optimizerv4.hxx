/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkLBFGS2Optimizerv4_hxx
#define itkLBFGS2Optimizerv4_hxx

#include "itkMacro.h"
#include "itkMath.h"


namespace itk
{


template <typename TInternalComputationValueType>
LBFGS2Optimizerv4Template<TInternalComputationValueType>::LBFGS2Optimizerv4Template()
{
  // Initialize to default parameters
  lbfgs_parameter_init(&m_Parameters);
  m_StatusCode = 100;

  this->m_EstimateScalesAtEachIteration = true;
}


template <typename TInternalComputationValueType>
LBFGS2Optimizerv4Template<TInternalComputationValueType>::~LBFGS2Optimizerv4Template() = default;

template <typename TInternalComputationValueType>
void
LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m: " << m_Parameters.m << std::endl;
  os << indent << "epsilon: " << m_Parameters.epsilon << std::endl;
  os << indent << "past: " << m_Parameters.past << std::endl;
  os << indent << "delta: " << m_Parameters.delta << std::endl;
  os << indent << "max_iterations: " << m_Parameters.max_iterations << std::endl;
  os << indent << "linesearch: " << m_Parameters.linesearch << std::endl;
  os << indent << "max_linesearch: " << m_Parameters.max_linesearch << std::endl;
  os << indent << "min_step: " << m_Parameters.min_step << std::endl;
  os << indent << "max_step: " << m_Parameters.max_step << std::endl;
  os << indent << "ftol: " << m_Parameters.ftol << std::endl;
  os << indent << "wolfe: " << m_Parameters.wolfe << std::endl;
  os << indent << "gtol: " << m_Parameters.gtol << std::endl;
  os << indent << "xtol: " << m_Parameters.xtol << std::endl;
  os << indent << "orthantwise_c: " << m_Parameters.orthantwise_c << std::endl;
  os << indent << "orthantwise_start: " << m_Parameters.orthantwise_start << std::endl;
  os << indent << "orthantwise_end: " << m_Parameters.orthantwise_end << std::endl;
}


template <typename TInternalComputationValueType>
void
LBFGS2Optimizerv4Template<TInternalComputationValueType>::StartOptimization(bool doOnlyInitialization)
{
  Superclass::StartOptimization(doOnlyInitialization);
  if (!doOnlyInitialization)
  {
    this->ResumeOptimization();
  }
}

// Register callbacks and call the lbfgs routine

template <typename TInternalComputationValueType>
void
LBFGS2Optimizerv4Template<TInternalComputationValueType>::ResumeOptimization()
{

  this->InvokeEvent(StartEvent());

  // Copy parameters
  const ParametersType & parameters = this->m_Metric->GetParameters();

  int N = parameters.GetSize();
  if (N == 0)
  {
    itkExceptionMacro(<< "Optimizer parameters are not initialized.");
  }

  // TODO: only needed if SSE is enabled
  PrecisionType * x = lbfgs_malloc(N);
  std::memcpy(x, parameters.data_block(), sizeof(PrecisionType) * N);

  // Run lbfgs
  m_StatusCode = lbfgs(N,
                       x,
                       &this->m_CurrentMetricValue,
                       LBFGS2Optimizerv4Template::EvaluateCostCallback,
                       LBFGS2Optimizerv4Template::UpdateProgressCallback,
                       this,
                       &m_Parameters);

  // Match the behavior of other optimizer setting the current
  // iteration to the max when iteration limit is reached
  if (m_StatusCode == LBFGSERR_MAXIMUMITERATION)
  {
    ++this->m_CurrentIteration;
  }

  // Copy results
  ParametersType optimizedParameters(N);
  std::memcpy(optimizedParameters.data_block(), x, sizeof(PrecisionType) * N);
  this->m_Metric->SetParameters(optimizedParameters);
  lbfgs_free(x);
}

// LBFGS method
template <typename TInternalComputationValueType>
typename LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType
LBFGS2Optimizerv4Template<TInternalComputationValueType>::EvaluateCostCallback(
  void *                                                                          instance,
  const LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType * x,
  LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType *       g,
  const int                                                                       n,
  const LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType   step)
{
  auto * optimizer = static_cast<LBFGS2Optimizerv4Template *>(instance);
  return optimizer->EvaluateCost(x, g, n, step);
}
template <typename TInternalComputationValueType>
typename LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType
LBFGS2Optimizerv4Template<TInternalComputationValueType>::EvaluateCost(
  const LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType * x,
  LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType *       g,
  const int                                                                       n,
  const LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType)
{
  ParametersType xItk(n);
  // TODO: potentially not thread safe since x is modified by lbfgs
  // std::memcpy(xItk.data_block(), x, sizeof(PrecisionType) * n);
  xItk.SetData(const_cast<PrecisionType *>(x), n, false);
  this->m_Metric->SetParameters(xItk);

  this->m_Gradient.SetSize(n);
  this->m_Gradient.SetData(g, n, false);
  PrecisionType value;
  this->m_Metric->GetValueAndDerivative(value, this->m_Gradient);

  this->ModifyGradientByScales();
  this->EstimateLearningRate();
  this->m_LearningRate *= -1;
  this->ModifyGradientByLearningRate();
  this->m_LearningRate *= -1;

  /* Re-estimate the parameter scales if requested. */
  if (this->m_EstimateScalesAtEachIteration && this->m_DoEstimateScales && this->m_ScalesEstimator.IsNotNull())
  {
    ScalesType scales;
    this->m_ScalesEstimator->EstimateScales(scales);
    this->SetScales(scales);
    itkDebugMacro("Estimated scales = " << this->m_Scales);
  }

  return value;
}
template <typename TInternalComputationValueType>
int
LBFGS2Optimizerv4Template<TInternalComputationValueType>::UpdateProgressCallback(
  void *                                           instance,
  const LBFGS2Optimizerv4Template::PrecisionType * x,
  const LBFGS2Optimizerv4Template::PrecisionType * g,
  const LBFGS2Optimizerv4Template::PrecisionType   fx,
  const LBFGS2Optimizerv4Template::PrecisionType   xnorm,
  const LBFGS2Optimizerv4Template::PrecisionType   gnorm,
  const LBFGS2Optimizerv4Template::PrecisionType   step,
  int                                              n,
  int                                              k,
  int                                              ls)
{
  auto * optimizer = static_cast<LBFGS2Optimizerv4Template *>(instance);
  return optimizer->UpdateProgress(x, g, fx, xnorm, gnorm, step, n, k, ls);
}

template <typename TInternalComputationValueType>
int
LBFGS2Optimizerv4Template<TInternalComputationValueType>::UpdateProgress(
  const LBFGS2Optimizerv4Template::PrecisionType *,
  const LBFGS2Optimizerv4Template::PrecisionType *,
  const LBFGS2Optimizerv4Template::PrecisionType fx,
  const LBFGS2Optimizerv4Template::PrecisionType xnorm,
  const LBFGS2Optimizerv4Template::PrecisionType gnorm,
  const LBFGS2Optimizerv4Template::PrecisionType step,
  int,
  int k,
  int ls)
{
  // Convert to 0-based ITK iteration counting
  this->m_CurrentIteration = k - 1;
  this->m_CurrentMetricValue = fx;

  m_CurrentParameterNorm = xnorm;
  m_CurrentGradientNorm = gnorm;
  m_CurrentStepSize = step;
  m_CurrentNumberOfEvaluations = ls;


  this->Modified();
  this->InvokeEvent(IterationEvent());

  return this->m_Stop;
}

template <typename TInternalComputationValueType>
const typename LBFGS2Optimizerv4Template<TInternalComputationValueType>::StopConditionReturnStringType
LBFGS2Optimizerv4Template<TInternalComputationValueType>::GetStopConditionDescription() const
{
  switch (m_StatusCode)
  {
    case 100:
      return "Optimization not started";
    case LBFGS_SUCCESS:
      return "Converged";
    case LBFGS_ALREADY_MINIMIZED:
      return "Already minimized";
    case LBFGSERR_UNKNOWNERROR:
      return "Unknown error";
    case LBFGSERR_LOGICERROR:
      return "Logic error";
    case LBFGSERR_OUTOFMEMORY:
      return "Out of memory";
    case LBFGSERR_CANCELED:
      return "Optimization canceled";
    case LBFGSERR_INVALID_N:
      return "Invalid number of variables";
    case LBFGSERR_INVALID_N_SSE:
      return "Invalid number of variables for SSE";
    case LBFGSERR_INVALID_X_SSE:
      return "Invalid alignment of variables for SSE";
    case LBFGSERR_INVALID_EPSILON:
      return "Invalid solution accuracy parameter";
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
      return "Invalid maximum linesearch iterations";
    case LBFGSERR_INVALID_ORTHANTWISE:
      return "Invalid orthantwise coefficient";
    case LBFGSERR_INVALID_ORTHANTWISE_START:
      return "Invalid orthantwise start";
    case LBFGSERR_INVALID_ORTHANTWISE_END:
      return "Invalid orthantwise end";
    case LBFGSERR_OUTOFINTERVAL:
      return "LineSearch went outside of the uncertainty interval";
    case LBFGSERR_INCORRECT_TMINMAX:
      return "Interval of uncertainty became too small";
    case LBFGSERR_ROUNDING_ERROR:
      return "A rounding error occurred or line-search steps have an insufficient reduction";
    case LBFGSERR_MINIMUMSTEP:
      return "LineSearch became smaller than the minimum linesearch step size";
    case LBFGSERR_MAXIMUMSTEP:
      return "LineSearch became bigger than the maximum linesearch step size";
    case LBFGSERR_MAXIMUMLINESEARCH:
      return "LineSearch reached maximum umber of iterations";
    case LBFGSERR_MAXIMUMITERATION:
      return "Reached maximum number of iterations";
    case LBFGSERR_WIDTHTOOSMALL:
      return "Relative width of the interval became too small (less than machine tolerance)";
    case LBFGSERR_INVALIDPARAMETERS:
      return "Negative line search step occurred";
    case LBFGSERR_INCREASEGRADIENT:
      return "Current search direction increases objective function";
  }
  return "Unknown status";
}

template <typename TInternalComputationValueType>
// A bunch of Set/Get methods for setting lbfgs parameters
void
LBFGS2Optimizerv4Template<TInternalComputationValueType>::SetHessianApproximationAccuracy(int m)
{
  m_Parameters.m = m;
  this->Modified();
}

template <typename TInternalComputationValueType>
int
LBFGS2Optimizerv4Template<TInternalComputationValueType>::GetHessianApproximationAccuracy() const
{
  return m_Parameters.m;
}

template <typename TInternalComputationValueType>
void
LBFGS2Optimizerv4Template<TInternalComputationValueType>::SetSolutionAccuracy(
  LBFGS2Optimizerv4Template::PrecisionType epsilon)
{
  m_Parameters.epsilon = epsilon;
  this->Modified();
}

template <typename TInternalComputationValueType>
typename LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType
LBFGS2Optimizerv4Template<TInternalComputationValueType>::GetSolutionAccuracy() const
{
  return m_Parameters.epsilon;
}

template <typename TInternalComputationValueType>
void
LBFGS2Optimizerv4Template<TInternalComputationValueType>::SetDeltaConvergenceDistance(int nPast)
{
  m_Parameters.past = nPast;
  this->Modified();
}

template <typename TInternalComputationValueType>
int
LBFGS2Optimizerv4Template<TInternalComputationValueType>::GetDeltaConvergenceDistance() const
{
  return m_Parameters.past;
}

template <typename TInternalComputationValueType>
void
LBFGS2Optimizerv4Template<TInternalComputationValueType>::SetDeltaConvergenceTolerance(
  LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType tol)
{
  m_Parameters.delta = tol;
  this->Modified();
}

template <typename TInternalComputationValueType>
typename LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType
LBFGS2Optimizerv4Template<TInternalComputationValueType>::GetDeltaConvergenceTolerance() const
{
  return m_Parameters.delta;
}

template <typename TInternalComputationValueType>
void
LBFGS2Optimizerv4Template<TInternalComputationValueType>::SetMaximumIterations(int maxIterations)
{
  m_Parameters.max_iterations = maxIterations;
  this->Modified();
}


template <typename TInternalComputationValueType>
int
LBFGS2Optimizerv4Template<TInternalComputationValueType>::GetMaximumIterations() const
{
  return m_Parameters.max_iterations;
}

// translate to lbfgs.h enum
// this translation is kind of annoying and error prone
// but avoids exposing lbfgs.h

template <typename TInternalComputationValueType>
void
LBFGS2Optimizerv4Template<TInternalComputationValueType>::SetLineSearch(
  const LBFGS2Optimizerv4Template<TInternalComputationValueType>::LineSearchMethodEnum & linesearch)
{
  //
  int lbfgsLineSearch = LBFGS_LINESEARCH_DEFAULT;
  if (linesearch == LineSearchMethodEnum::LINESEARCH_BACKTRACKING)
  {
    lbfgsLineSearch = LBFGS_LINESEARCH_BACKTRACKING;
  }
  else if (linesearch == LineSearchMethodEnum::LINESEARCH_BACKTRACKING_ARMIJO)
  {
    lbfgsLineSearch = LBFGS_LINESEARCH_BACKTRACKING_ARMIJO;
  }
  else if (linesearch == LineSearchMethodEnum::LINESEARCH_BACKTRACKING_WOLFE)
  {
    lbfgsLineSearch = LBFGS_LINESEARCH_BACKTRACKING_WOLFE;
  }
  else if (linesearch == LineSearchMethodEnum::LINESEARCH_BACKTRACKING_STRONG_WOLFE)
  {
    lbfgsLineSearch = LBFGS_LINESEARCH_BACKTRACKING_STRONG_WOLFE;
  }
  else if (linesearch == LineSearchMethodEnum::LINESEARCH_MORETHUENTE)
  {
    lbfgsLineSearch = LBFGS_LINESEARCH_MORETHUENTE;
  }

  m_Parameters.linesearch = lbfgsLineSearch;
  this->Modified();
}

template <typename TInternalComputationValueType>
typename LBFGS2Optimizerv4Template<TInternalComputationValueType>::LineSearchMethodEnum
LBFGS2Optimizerv4Template<TInternalComputationValueType>::GetLineSearch() const
{
  LineSearchMethodEnum linesearch = LineSearchMethodEnum::LINESEARCH_DEFAULT;
  int                  lbfgsLineSearch = m_Parameters.linesearch;
  if (lbfgsLineSearch == LBFGS_LINESEARCH_BACKTRACKING)
  {
    linesearch = LineSearchMethodEnum::LINESEARCH_BACKTRACKING;
  }
  else if (lbfgsLineSearch == LBFGS_LINESEARCH_BACKTRACKING_ARMIJO)
  {
    linesearch = LineSearchMethodEnum::LINESEARCH_BACKTRACKING_ARMIJO;
  }
  else if (lbfgsLineSearch == LBFGS_LINESEARCH_BACKTRACKING_WOLFE)
  {
    linesearch = LineSearchMethodEnum::LINESEARCH_BACKTRACKING_WOLFE;
  }
  else if (lbfgsLineSearch == LBFGS_LINESEARCH_BACKTRACKING_STRONG_WOLFE)
  {
    linesearch = LineSearchMethodEnum::LINESEARCH_BACKTRACKING_STRONG_WOLFE;
  }
  else if (lbfgsLineSearch == LBFGS_LINESEARCH_MORETHUENTE)
  {
    linesearch = LineSearchMethodEnum::LINESEARCH_MORETHUENTE;
  }

  return linesearch;
}

template <typename TInternalComputationValueType>
void
LBFGS2Optimizerv4Template<TInternalComputationValueType>::SetMaximumLineSearchEvaluations(int n)
{
  m_Parameters.max_linesearch = n;
  this->Modified();
}

template <typename TInternalComputationValueType>
int
LBFGS2Optimizerv4Template<TInternalComputationValueType>::GetMaximumLineSearchEvaluations() const
{
  return m_Parameters.max_linesearch;
}

template <typename TInternalComputationValueType>
void
LBFGS2Optimizerv4Template<TInternalComputationValueType>::SetMinimumLineSearchStep(
  LBFGS2Optimizerv4Template::PrecisionType step)
{
  m_Parameters.min_step = step;
  this->Modified();
}

template <typename TInternalComputationValueType>
typename LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType
LBFGS2Optimizerv4Template<TInternalComputationValueType>::GetMinimumLineSearchStep() const
{
  return m_Parameters.min_step;
}

template <typename TInternalComputationValueType>
void
LBFGS2Optimizerv4Template<TInternalComputationValueType>::SetMaximumLineSearchStep(
  LBFGS2Optimizerv4Template::PrecisionType step)
{
  m_Parameters.max_step = step;

  this->Modified();
}

template <typename TInternalComputationValueType>
typename LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType
LBFGS2Optimizerv4Template<TInternalComputationValueType>::GetMaximumLineSearchStep() const
{
  return m_Parameters.max_step;
}

template <typename TInternalComputationValueType>
void
LBFGS2Optimizerv4Template<TInternalComputationValueType>::SetLineSearchAccuracy(
  LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType ftol)
{
  m_Parameters.ftol = ftol;

  this->Modified();
}
template <typename TInternalComputationValueType>
typename LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType
LBFGS2Optimizerv4Template<TInternalComputationValueType>::GetLineSearchAccuracy() const
{
  return m_Parameters.ftol;
}

template <typename TInternalComputationValueType>
void
LBFGS2Optimizerv4Template<TInternalComputationValueType>::SetWolfeCoefficient(
  LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType wc)
{
  m_Parameters.wolfe = wc;
  this->Modified();
}

template <typename TInternalComputationValueType>
typename LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType
LBFGS2Optimizerv4Template<TInternalComputationValueType>::GetWolfeCoefficient() const
{
  return m_Parameters.wolfe;
}

template <typename TInternalComputationValueType>
void
LBFGS2Optimizerv4Template<TInternalComputationValueType>::SetLineSearchGradientAccuracy(
  LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType gtol)
{
  m_Parameters.gtol = gtol;
  this->Modified();
}

template <typename TInternalComputationValueType>
typename LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType
LBFGS2Optimizerv4Template<TInternalComputationValueType>::GetLineSearchGradientAccuracy() const
{
  return m_Parameters.gtol;
}


template <typename TInternalComputationValueType>
void
LBFGS2Optimizerv4Template<TInternalComputationValueType>::SetMachinePrecisionTolerance(
  LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType xtol)
{
  m_Parameters.xtol = xtol;
  this->Modified();
}

template <typename TInternalComputationValueType>
typename LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType
LBFGS2Optimizerv4Template<TInternalComputationValueType>::GetMachinePrecisionTolerance() const
{
  return m_Parameters.xtol;
}

template <typename TInternalComputationValueType>
void
LBFGS2Optimizerv4Template<TInternalComputationValueType>::SetOrthantwiseCoefficient(
  LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType orthant_c)
{
  m_Parameters.orthantwise_c = orthant_c;
  this->Modified();
}

template <typename TInternalComputationValueType>
typename LBFGS2Optimizerv4Template<TInternalComputationValueType>::PrecisionType
LBFGS2Optimizerv4Template<TInternalComputationValueType>::GetOrthantwiseCoefficient() const
{
  return m_Parameters.orthantwise_c;
}

template <typename TInternalComputationValueType>
void
LBFGS2Optimizerv4Template<TInternalComputationValueType>::SetOrthantwiseStart(int start)
{
  m_Parameters.orthantwise_start = start;
  this->Modified();
}

template <typename TInternalComputationValueType>
int
LBFGS2Optimizerv4Template<TInternalComputationValueType>::GetOrthantwiseStart() const
{
  return m_Parameters.orthantwise_start;
}

template <typename TInternalComputationValueType>
void
LBFGS2Optimizerv4Template<TInternalComputationValueType>::SetOrthantwiseEnd(int end)
{
  m_Parameters.orthantwise_end = end;
  this->Modified();
}

template <typename TInternalComputationValueType>
int
LBFGS2Optimizerv4Template<TInternalComputationValueType>::GetOrthantwiseEnd() const
{
  return m_Parameters.orthantwise_end;
}


} // end namespace itk
#endif
