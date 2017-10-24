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
#ifndef itkLBFGSBOptimizer_h
#define itkLBFGSBOptimizer_h

#include "itkIntTypes.h"
#include "itkSingleValuedNonLinearVnlOptimizer.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/* Necessary forward declaration see below for definition */
/** \class LBFGSBOptimizerHelper
 * \brief Wrapper helper around vnl_lbfgsb.
 *
 * This class is used to translate iteration events, etc, from
 * vnl_lbfgsb into iteration events in ITK.
 * \ingroup ITKOptimizers
 */
class ITK_FORWARD_EXPORT LBFGSBOptimizerHelper;

/** \class LBFGSBOptimizer
 * \brief Limited memory Broyden Fletcher Goldfarb Shannon minimization with simple bounds.
 *
 * This class is a wrapper for converted fortan code for performing limited
 * memory Broyden Fletcher Goldfarb Shannon minimization with simple bounds.
 * The algorithm miminizes a nonlinear function f(x) of n variables subject to
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
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */
class ITKOptimizers_EXPORT LBFGSBOptimizer:
  public SingleValuedNonLinearVnlOptimizer
{
public:
  /** Standard "Self" typedef. */
  typedef LBFGSBOptimizer                   Self;
  typedef SingleValuedNonLinearVnlOptimizer Superclass;
  typedef SmartPointer< Self >              Pointer;
  typedef SmartPointer< const Self >        ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LBFGSBOptimizer, SingleValuedNonLinearVnlOptimizer);

  /**  BoundValue type.
   *  Use for defining the lower and upper bounds on the variables.
   */
  typedef Array< double > BoundValueType;

  /** BoundSelection type
   * Use for defining the boundary condition for each variables.
   */
  typedef Array< long > BoundSelectionType;

  /** Internal boundary value storage type */
  typedef vnl_vector< double > InternalBoundValueType;

  /** Internal boundary selection storage type */
  typedef vnl_vector< long > InternalBoundSelectionType;

  /** The vnl optimizer */
  typedef LBFGSBOptimizerHelper InternalOptimizerType;

  /** Start optimization with an initial value. */
  virtual void StartOptimization(void) ITK_OVERRIDE;

  /** Plug in a Cost Function into the optimizer  */
  virtual void SetCostFunction(SingleValuedCostFunction *costFunction) ITK_OVERRIDE;

  /** Set/Get the optimizer trace flag. If set to true, the optimizer
   * prints out information every iteration.
   */
  virtual void SetTrace(bool flag);

  itkGetMacro(Trace, bool);
  itkBooleanMacro(Trace);

  /** Set the lower bound value for each variable. */
  virtual void SetLowerBound(const BoundValueType & value);
  itkGetConstReferenceMacro(LowerBound,BoundValueType);

  /** Set the upper bound value for each variable. */
  virtual void SetUpperBound(const BoundValueType & value);
  itkGetConstReferenceMacro(UpperBound,BoundValueType);

  /** Set the boundary condition for each variable, where
   * select[i] = 0 if x[i] is unbounded,
   *           = 1 if x[i] has only a lower bound,
   *           = 2 if x[i] has both lower and upper bounds, and
   *           = 3 if x[1] has only an upper bound
   */
  virtual void SetBoundSelection(const BoundSelectionType & select);
  itkGetConstReferenceMacro(BoundSelection,BoundSelectionType);

  /** Set/Get the CostFunctionConvergenceFactor. Algorithm terminates
   * when the reduction in cost function is less than factor * epsmcj
   * where epsmch is the machine precision.
   * Typical values for factor: 1e+12 for low accuracy;
   * 1e+7 for moderate accuracy and 1e+1 for extremely high accuracy.
   */
  virtual void SetCostFunctionConvergenceFactor(double);

  itkGetMacro(CostFunctionConvergenceFactor, double);

  /** Set/Get the ProjectedGradientTolerance. Algorithm terminates
   * when the project gradient is below the tolerance. Default value
   * is 1e-5.
   */
  virtual void SetProjectedGradientTolerance(double);

  itkGetMacro(ProjectedGradientTolerance, double);

  /** Set/Get the MaximumNumberOfIterations. Default is 500 */
  virtual void SetMaximumNumberOfIterations(unsigned int);

  itkGetMacro(MaximumNumberOfIterations, unsigned int);

  /** Set/Get the MaximumNumberOfEvaluations. Default is 500 */
  virtual void SetMaximumNumberOfEvaluations(unsigned int);

  itkGetMacro(MaximumNumberOfEvaluations, unsigned int);

  /** Set/Get the MaximumNumberOfCorrections. Default is 5 */
  virtual void SetMaximumNumberOfCorrections(unsigned int);

  itkGetMacro(MaximumNumberOfCorrections, unsigned int);

  /** This optimizer does not support scaling of the derivatives. */
  void SetScales(const ScalesType &)
  {
    itkExceptionMacro(<< "This optimizer does not support scales.");
  }

  /** Get the current iteration number. */
  itkGetConstReferenceMacro(CurrentIteration, unsigned int);

  /** Get the current cost function value. */
  MeasureType GetValue() const;

  /** Get the current infinity norm of the project gradient of the cost
   * function. */
  itkGetConstReferenceMacro(InfinityNormOfProjectedGradient, double);

  /** Get the reason for termination */
  virtual const std::string GetStopConditionDescription() const ITK_OVERRIDE;

protected:
  LBFGSBOptimizer();
  virtual ~LBFGSBOptimizer() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  typedef Superclass::CostFunctionAdaptorType CostFunctionAdaptorType;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LBFGSBOptimizer);

  // give the helper access to member variables, to update iteration
  // counts, etc.
  friend class LBFGSBOptimizerHelper;

  bool         m_Trace;
  bool         m_OptimizerInitialized;
  double       m_CostFunctionConvergenceFactor;
  double       m_ProjectedGradientTolerance;
  unsigned int m_MaximumNumberOfIterations;
  unsigned int m_MaximumNumberOfEvaluations;
  unsigned int m_MaximumNumberOfCorrections;
  unsigned int m_CurrentIteration;
  double       m_InfinityNormOfProjectedGradient;

  InternalOptimizerType * m_VnlOptimizer;
  BoundValueType          m_LowerBound;
  BoundValueType          m_UpperBound;
  BoundSelectionType      m_BoundSelection;
};
} // end namespace itk

#endif
