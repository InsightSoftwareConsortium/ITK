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
#ifndef itkOnePlusOneEvolutionaryOptimizerv4_hxx
#define itkOnePlusOneEvolutionaryOptimizerv4_hxx

#include "itkMath.h"
#include "itkOnePlusOneEvolutionaryOptimizerv4.h"
#include "vnl/vnl_matrix.h"
namespace itk
{
template<typename TInternalComputationValueType>
OnePlusOneEvolutionaryOptimizerv4<TInternalComputationValueType>
::OnePlusOneEvolutionaryOptimizerv4()
{
  m_CatchGetValueException = false;
  m_MetricWorstPossibleValue = 0;

  m_Epsilon = (double)1.5e-4;
  m_RandomGenerator = ITK_NULLPTR;

  m_Initialized = false;
  m_GrowthFactor = 1.05;
  m_ShrinkFactor = std::pow(m_GrowthFactor, -0.25);
  m_InitialRadius = 1.01;
  m_MaximumIteration = 100;
  m_Stop = false;
  m_StopConditionDescription.str("");
  m_CurrentCost = 0;
  m_FrobeniusNorm = 0.0;
}

template<typename TInternalComputationValueType>
OnePlusOneEvolutionaryOptimizerv4<TInternalComputationValueType>
::~OnePlusOneEvolutionaryOptimizerv4()
{}

template<typename TInternalComputationValueType>
void
OnePlusOneEvolutionaryOptimizerv4<TInternalComputationValueType>
::SetNormalVariateGenerator(NormalVariateGeneratorType *generator)
{
  if ( m_RandomGenerator != generator )
    {
    m_RandomGenerator = generator;
    this->Modified();
    }
}

template<typename TInternalComputationValueType>
void
OnePlusOneEvolutionaryOptimizerv4<TInternalComputationValueType>
::Initialize(double initialRadius, double grow, double shrink)
{
  m_InitialRadius = initialRadius;

  if ( Math::AlmostEquals( grow, -1 ) )
    {
    m_GrowthFactor = 1.05;
    }
  else
    {
    m_GrowthFactor = grow;
    }
  if ( Math::AlmostEquals( shrink, -1 ) )
    {
    m_ShrinkFactor = std::pow(m_GrowthFactor, -0.25);
    }
  else
    {
    m_ShrinkFactor = shrink;
    }
}

template<typename TInternalComputationValueType>
void
OnePlusOneEvolutionaryOptimizerv4<TInternalComputationValueType>
::StartOptimization(bool /* doOnlyInitialization */)
{
  if ( this->m_Metric.IsNull() )
    {
    return;
    }

  Superclass::StartOptimization();

  this->InvokeEvent( StartEvent() );
  m_Stop = false;

  unsigned int         spaceDimension = this->m_Metric->GetNumberOfParameters();
  vnl_matrix< double > A(spaceDimension, spaceDimension);
  vnl_vector< double > parent( this->m_Metric->GetParameters() );
  vnl_vector< double > f_norm(spaceDimension);
  vnl_vector< double > child(spaceDimension);
  vnl_vector< double > delta(spaceDimension);

  ParametersType parentPosition(spaceDimension);
  ParametersType childPosition(spaceDimension);

  for ( unsigned int i = 0; i < spaceDimension; i++ )
    {
    parentPosition[i] = parent[i];
    }
  this->m_Metric->SetParameters( parentPosition );

  double pvalue = m_MetricWorstPossibleValue;
  try
    {
    pvalue = this->m_Metric->GetValue();
    }
  catch ( ... )
    {
    if ( m_CatchGetValueException )
      {
      pvalue = m_MetricWorstPossibleValue;
      }
    else
      {
      throw;
      }
    }

  itkDebugMacro(<< ": initial position: " << parentPosition);
  itkDebugMacro(<< ": initial fitness: " << pvalue);

  this->m_Metric->SetParameters(parentPosition);
  const ScalesType & scales = this->GetScales();

  // Make sure the scales have been set properly
  if ( scales.size() != spaceDimension )
    {
    itkExceptionMacro(<< "The size of Scales is "
                      << scales.size()
                      << ", but the NumberOfParameters for the CostFunction is "
                      << spaceDimension
                      << ".");
    }

  A.set_identity();
  for ( unsigned int i = 0; i < spaceDimension; i++ )
    {
    A(i, i) = m_InitialRadius / scales[i];
    }

  for ( this->m_CurrentIteration = 0;
        this->m_CurrentIteration < m_MaximumIteration;
        this->m_CurrentIteration++ )
    {
    if ( m_Stop )
      {
      m_StopConditionDescription.str("");
      m_StopConditionDescription << this->GetNameOfClass() << ": ";
      m_StopConditionDescription << "StopOptimization() called";
      break;
      }

    for ( unsigned int i = 0; i < spaceDimension; i++ )
      {
      if ( !m_RandomGenerator )
        {
        itkExceptionMacro(<< "Random Generator is not set!");
        }
      f_norm[i] = m_RandomGenerator->GetVariate();
      }

    delta  = A * f_norm;
    child  = parent + delta;

    for ( unsigned int i = 0; i < spaceDimension; i++ )
      {
      childPosition[i] = child[i];
      }
    // Update the metric so we can check the metric value in childPosition
    this->m_Metric->SetParameters( childPosition );

    double cvalue = m_MetricWorstPossibleValue;
    try
      {
      cvalue = this->m_Metric->GetValue();
      // While we got the metric value in childPosition,
      // the metric parameteres are set back to parentPosition
      this->m_Metric->SetParameters( parentPosition );
      }
    catch ( ... )
      {
      if ( m_CatchGetValueException )
        {
        cvalue = m_MetricWorstPossibleValue;
        }
      else
        {
        throw;
        }
      }

    itkDebugMacro(<< "iter: " << this->m_CurrentIteration << ": parent position: "
                  << parentPosition);
    itkDebugMacro(<< "iter: " << this->m_CurrentIteration << ": parent fitness: "
                  << pvalue);
    itkDebugMacro(<< "iter: " << this->m_CurrentIteration << ": random vector: " << f_norm);
    itkDebugMacro(<< "iter: " << this->m_CurrentIteration << ": A: " << std::endl << A);
    itkDebugMacro(<< "iter: " << this->m_CurrentIteration << ": delta: " << delta);
    itkDebugMacro(<< "iter: " << this->m_CurrentIteration << ": child position: "
                  << childPosition);
    itkDebugMacro(<< "iter: " << this->m_CurrentIteration << ": child fitness: "
                  << cvalue);

    double adjust = m_ShrinkFactor;

    if ( cvalue < pvalue )
      {
      itkDebugMacro(<< "iter: " << this->m_CurrentIteration << ": increasing search radius");
      pvalue = cvalue;
      parent.swap(child);
      adjust = m_GrowthFactor;
      for ( unsigned int i = 0; i < spaceDimension; i++ )
        {
        parentPosition[i] = parent[i];
        }
      this->m_Metric->SetParameters(parentPosition);
      }
    else
      {
      itkDebugMacro(<< "iter: " << this->m_CurrentIteration << ": decreasing search radius");
      }

    m_CurrentCost = pvalue;
    // convergence criterion: f-norm of A < epsilon_A
    // Compute double precision sum of absolute values of
    // a single precision vector
    m_FrobeniusNorm = A.fro_norm();
    itkDebugMacro(<< "A f-norm:" << m_FrobeniusNorm);
    if ( m_FrobeniusNorm <= m_Epsilon )
      {
      itkDebugMacro(<< "converges at iteration = " << this->m_CurrentIteration);
      m_StopConditionDescription.str("");
      m_StopConditionDescription << this->GetNameOfClass() << ": ";
      m_StopConditionDescription << "Fnorm (" << m_FrobeniusNorm
                                 << ") is less than Epsilon (" << m_Epsilon
                                 << " at iteration #" << this->m_CurrentIteration;
      this->InvokeEvent( EndEvent() );
      return;
      }

    // A += (adjust - 1)/ (f_norm * f_norm) * A * f_norm * f_norm;
    // Blas_R1_Update(A, A * f_norm, f_norm,
    //             ((adjust - 1) / Blas_Dot_Prod(f_norm, f_norm)));
    // = DGER(Fortran)
    //   performs the rank 1 operation
    // A := alpha*x*y' + A,
    // where y' = transpose(y)
    // where alpha is a scalar, x is an m element vector, y is an n element
    // vector and A is an m by n matrix.
    // x = A * f_norm , y = f_norm, alpha = (adjust - 1) / Blas_Dot_Prod(
    // f_norm, f_norm)

    //A = A + (adjust - 1.0) * A;
    double alpha = ( ( adjust - 1.0 ) / dot_product(f_norm, f_norm) );
    for ( unsigned int c = 0; c < spaceDimension; c++ )
      {
      for ( unsigned int r = 0; r < spaceDimension; r++ )
        {
        A(r, c) += alpha * delta[r] * f_norm[c];
        }
      }

    this->InvokeEvent( IterationEvent() );
    itkDebugMacro( << "Current position: " << this->GetCurrentPosition() );
    }
  m_StopConditionDescription.str("");
  m_StopConditionDescription << this->GetNameOfClass() << ": ";
  m_StopConditionDescription << "Maximum number of iterations ("
                             << m_MaximumIteration
                             << ") exceeded. ";
  this->InvokeEvent( EndEvent() );
}

template<typename TInternalComputationValueType>
const std::string
OnePlusOneEvolutionaryOptimizerv4<TInternalComputationValueType>
::GetStopConditionDescription() const
{
  return m_StopConditionDescription.str();
}

template<typename TInternalComputationValueType>
const typename OnePlusOneEvolutionaryOptimizerv4<TInternalComputationValueType>::MeasureType &
OnePlusOneEvolutionaryOptimizerv4<TInternalComputationValueType>
::GetValue() const
{
  return this->GetCurrentCost();
}

template<typename TInternalComputationValueType>
void
OnePlusOneEvolutionaryOptimizerv4<TInternalComputationValueType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  if ( m_RandomGenerator )
    {
    os << indent << "Random Generator  " << m_RandomGenerator.GetPointer()  << std::endl;
    }
  else
    {
    os << indent << "Random Generator  " << "(none)" << std::endl;
    }
  os << indent << "Maximum Iteration " << GetMaximumIteration() << std::endl;
  os << indent << "Epsilon           " << GetEpsilon()          << std::endl;
  os << indent << "Initial Radius    " << GetInitialRadius()    << std::endl;
  os << indent << "Growth Fractor    " << GetGrowthFactor()     << std::endl;
  os << indent << "Shrink Fractor    " << GetShrinkFactor()     << std::endl;
  os << indent << "Initialized       " << GetInitialized()      << std::endl;
  os << indent << "Current Cost      " << GetCurrentCost()      << std::endl;
  os << indent << "Frobenius Norm    " << GetFrobeniusNorm()    << std::endl;
  os << indent << "CatchGetValueException   " << GetCatchGetValueException()
     << std::endl;
  os << indent << "MetricWorstPossibleValue " << GetMetricWorstPossibleValue()
     << std::endl;
}
} // end of namespace itk
#endif
