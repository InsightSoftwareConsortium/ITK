/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkLBFGSBOptimizerv4_h
#define itkLBFGSBOptimizerv4_h

#include "itkLBFGSOptimizerBasev4.h"
#include "vnl/algo/vnl_lbfgsb.h"
#include "ITKOptimizersv4Export.h"

namespace itk
{
/* Necessary forward declaration */
/**
 *\class LBFGSBOptimizerHelperv4
 * \brief Wrapper helper around vnl_lbfgsb.
 *
 * This class is used to translate iteration events, etc, from
 * vnl_lbfgsb into iteration events in ITK.
 *
 * \ingroup ITKOptimizersv4
 */
// Forward reference because of private implementation
class ITK_FORWARD_EXPORT LBFGSBOptimizerHelperv4;

/**
 *\class LBFGSBOptimizerv4
 * \brief Limited memory Broyden Fletcher Goldfarb Shannon minimization with simple bounds.
 *
 * This class is a wrapper for converted Fortran code for performing limited
 * memory Broyden Fletcher Goldfarb Shannon minimization with simple bounds.
 * The algorithm mininizes a nonlinear function f(x) of n variables subject to
 * simple bound constraints of l <= x <= u.
 *
 * See also the documentation in Numerics/lbfgsb.c
 *
 * References:
 *
 * [1] R. H. Byrd, P. Lu and J. Nocedal.
 * A Limited Memory Algorithm for Bound Constrained Optimization, (1995),
 * SIAM Journal on Scientific and Statistical Computing ,
 * 16, 5, pp. 1190-1208.
 *
 * [2] C. Zhu, R. H. Byrd and J. Nocedal.
 * L-BFGS-B: Algorithm 778: L-BFGS-B, FORTRAN routines for large scale
 * bound constrained optimization (1997),
 * ACM Transactions on Mathematical Software,
 * Vol 23, Num. 4, pp. 550 - 560.
 *
 * \ingroup Numerics Optimizersv4
 * \ingroup ITKOptimizersv4
 */
class ITKOptimizersv4_EXPORT LBFGSBOptimizerv4 : public LBFGSOptimizerBasev4<vnl_lbfgsb>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LBFGSBOptimizerv4);

  /** Standard "Self" type alias. */
  using Self = LBFGSBOptimizerv4;
  using Superclass = LBFGSOptimizerBasev4<vnl_lbfgsb>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using MetricType = Superclass::MetricType;
  using ParametersType = Superclass::ParametersType;
  using ScalesType = Superclass::ScalesType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LBFGSBOptimizerv4, Superclass);

  enum BoundSelectionValues
  {
    UNBOUNDED = 0,
    LOWERBOUNDED = 1,
    BOTHBOUNDED = 2,
    UPPERBOUNDED = 3
  };

  /**  BoundValue type.
   *  Use for defining the lower and upper bounds on the variables.
   */
  using BoundValueType = Array<double>;

  /** BoundSelection type
   * Use for defining the boundary condition for each variables.
   */
  using BoundSelectionType = Array<long>;

  /**  Set the position to initialize the optimization. */
  void
  SetInitialPosition(const ParametersType & param);

  /** Get the position to initialize the optimization. */
  ParametersType &
  GetInitialPosition()
  {
    return m_InitialPosition;
  }

  /** Start optimization with an initial value. */
  void
  StartOptimization(bool doOnlyInitialization = false) override;

  /** Plug in a Cost Function into the optimizer  */
  void
  SetMetric(MetricType * metric) override;

  /** Set the lower bound value for each variable. */
  void
  SetLowerBound(const BoundValueType & value);

  itkGetConstReferenceMacro(LowerBound, BoundValueType);

  /** Set the upper bound value for each variable. */
  void
  SetUpperBound(const BoundValueType & value);

  itkGetConstReferenceMacro(UpperBound, BoundValueType);

  /** Set the boundary condition for each variable, where
   * select[i] = 0 if x[i] is unbounded,
   *           = 1 if x[i] has only a lower bound,
   *           = 2 if x[i] has both lower and upper bounds, and
   *           = 3 if x[1] has only an upper bound
   */
  void
  SetBoundSelection(const BoundSelectionType & value);

  itkGetConstReferenceMacro(BoundSelection, BoundSelectionType);

  /** Set/Get the CostFunctionConvergenceFactor. Algorithm terminates
   * when the reduction in cost function is less than factor * epsmcj
   * where epsmch is the machine precision.
   * Typical values for factor: 1e+12 for low accuracy;
   * 1e+7 for moderate accuracy and 1e+1 for extremely high accuracy.
   */
  virtual void
  SetCostFunctionConvergenceFactor(double);

  itkGetConstMacro(CostFunctionConvergenceFactor, double);

  /** Set/Get the MaximumNumberOfCorrections. Default is 5 */
  virtual void
  SetMaximumNumberOfCorrections(unsigned int);

  itkGetConstMacro(MaximumNumberOfCorrections, unsigned int);

  /** This optimizer does not support scaling of the derivatives. */
  void
  SetScales(const ScalesType &) override;

  /** Get the current infinity norm of the project gradient of the cost
   * function. */
  itkGetConstReferenceMacro(InfinityNormOfProjectedGradient, double);

protected:
  LBFGSBOptimizerv4();
  ~LBFGSBOptimizerv4() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  using CostFunctionAdaptorType = Superclass::CostFunctionAdaptorType;

  /** Internal optimizer type. */
  using InternalOptimizerType = LBFGSBOptimizerHelperv4;

  friend class LBFGSBOptimizerHelperv4;

private:
  unsigned int m_MaximumNumberOfCorrections{ 5 };

  ParametersType     m_InitialPosition;
  BoundValueType     m_LowerBound;
  BoundValueType     m_UpperBound;
  BoundSelectionType m_BoundSelection;
};
} // end namespace itk
#endif
