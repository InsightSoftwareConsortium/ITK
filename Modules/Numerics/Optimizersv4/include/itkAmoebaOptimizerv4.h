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
#ifndef itkAmoebaOptimizerv4_h
#define itkAmoebaOptimizerv4_h

#include "itkSingleValuedNonLinearVnlOptimizerv4.h"
#include "vnl/algo/vnl_amoeba.h"
#include "ITKOptimizersv4Export.h"

namespace itk
{
/** \class AmoebaOptimizerv4
 * \brief Wrap of the vnl_amoeba algorithm
 *
 * AmoebaOptimizerv4 is a wrapper around the vnl_amoeba algorithm which
 * is an implementation of the Nelder-Meade downhill simplex
 * problem. For most problems, it is a few times slower than a
 * Levenberg-Marquardt algorithm but does not require derivatives of
 * its cost function. It works by creating a simplex (n+1 points in
 * ND space). The cost function is evaluated at each corner of the
 * simplex.  The simplex is then modified (by reflecting a corner
 * about the opposite edge, by shrinking the entire simplex, by
 * contracting one edge of the simplex, or by expanding the simplex)
 * in searching for the minimum of the cost function.
 *
 * The methods AutomaticInitialSimplex() and SetInitialSimplexDelta()
 * control whether the optimizer defines the initial simplex
 * automatically (by constructing a very small simplex around the
 * initial position) or uses a user supplied simplex size.
 *
 * The method SetOptimizeWithRestarts() indicates that the amoeabe algorithm
 * should be rerun after if converges. This heuristic increases the chances
 * of escaping from a local optimum. Each time the simplex is initialized with
 * the best solution obtained by the previous runs. The edge length is half of
 * that from the previous iteration. The heuristic is terminated if the total
 * number of iterations is greater-equal than the maximal number of iterations
 * (SetNumberOfIterations) or the difference between the current function
 * value and the best function value is less than a threshold
 * (SetFunctionConvergenceTolerance) and
 * max(|best_parameters_i - current_parameters_i|) is less than a threshold
 * (SetParametersConvergenceTolerance).
 *
 * \ingroup ITKOptimizersv4
 */
class ITKOptimizersv4_EXPORT AmoebaOptimizerv4:
  public SingleValuedNonLinearVnlOptimizerv4
{
public:
  /** Standard "Self" typedef. */
  typedef AmoebaOptimizerv4                   Self;
  typedef SingleValuedNonLinearVnlOptimizerv4 Superclass;
  typedef SmartPointer< Self >                Pointer;
  typedef SmartPointer< const Self >          ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(AmoebaOptimizerv4, SingleValuedNonLinearVnlOptimizerv4);

  /**  Parameters type.
   *  It defines a position in the optimization search space. */
  typedef Superclass::ParametersType ParametersType;

  /** InternalParameters typedef. */
  typedef   vnl_vector< double > InternalParametersType;

  /** Start optimization with an initial value. */
  virtual void StartOptimization(bool doOnlyInitialization = false) ITK_OVERRIDE;

  /** Plug in a Cost Function into the optimizer  */
  virtual void SetMetric(MetricType *metric) ITK_OVERRIDE;

  /** Set/Get the mode which determines how the amoeba algorithm
   * defines the initial simplex.  Default is
   * AutomaticInitialSimplexOn. If AutomaticInitialSimplex is on, the
   * initial simplex is created with a default size. If
   * AutomaticInitialSimplex is off, then InitialSimplexDelta will be
   * used to define the initial simplex, setting the ith corner of the
   * simplex as [x0[0], x0[1], ..., x0[i]+InitialSimplexDelta[i], ...,
   * x0[d-1]]. */
  itkSetMacro(AutomaticInitialSimplex, bool);
  itkBooleanMacro(AutomaticInitialSimplex);
  itkGetConstMacro(AutomaticInitialSimplex, bool);

  /** Set/Get the mode that determines if we want to use multiple runs of the
   * Amoeba optimizer. If true, then the optimizer is rerun after it converges.
   * The additional runs are performed using a simplex initialized with the
   * best solution obtained by the previous runs. The edge length is half of
   * that from the previous iteration.
   */
  itkSetMacro(OptimizeWithRestarts, bool);
  itkBooleanMacro(OptimizeWithRestarts);
  itkGetConstMacro(OptimizeWithRestarts, bool);

  /** Set/Get the deltas that are used to define the initial simplex
   * when AutomaticInitialSimplex is off. */
  void SetInitialSimplexDelta(ParametersType initialSimplexDelta,
                              bool automaticInitialSimplex = false);
  itkGetConstMacro(InitialSimplexDelta, ParametersType);

  /** The optimization algorithm will terminate when the simplex
   * diameter and the difference in cost function values at the corners of
   * the simplex falls below user specified thresholds. The simplex
   * diameter threshold is set via SetParametersConvergenceTolerance().*/
  itkSetMacro(ParametersConvergenceTolerance, double);
  itkGetConstMacro(ParametersConvergenceTolerance, double);

  /** The optimization algorithm will terminate when the simplex
   * diameter and the difference in cost function values at the corners of
   * the simplex falls below user specified thresholds. The cost function
   * convergence threshold is set via SetFunctionConvergenceTolerance().*/
  itkSetMacro(FunctionConvergenceTolerance, double);
  itkGetConstMacro(FunctionConvergenceTolerance, double);

  /** Report the reason for stopping. */
  virtual const std::string GetStopConditionDescription() const ITK_OVERRIDE;

  /** Method for getting access to the internal optimizer. */
  vnl_amoeba * GetOptimizer() const;

protected:
  AmoebaOptimizerv4();
  virtual ~AmoebaOptimizerv4() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  typedef Superclass::CostFunctionAdaptorType CostFunctionAdaptorType;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(AmoebaOptimizerv4);

  /**Check that the settings are valid. If not throw an exception.*/
  void ValidateSettings();

  ParametersType::ValueType       m_ParametersConvergenceTolerance;
  MeasureType                     m_FunctionConvergenceTolerance;
  bool                            m_AutomaticInitialSimplex;
  ParametersType                  m_InitialSimplexDelta;
  bool                            m_OptimizeWithRestarts;
  vnl_amoeba *                    m_VnlOptimizer;

  std::ostringstream              m_StopConditionDescription;
};
} // end namespace itk

#endif
