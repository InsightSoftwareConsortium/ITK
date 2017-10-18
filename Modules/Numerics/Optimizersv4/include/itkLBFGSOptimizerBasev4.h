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
#ifndef itkLBFGSOptimizerBasev4_h
#define itkLBFGSOptimizerBasev4_h

#include "itkSingleValuedNonLinearVnlOptimizerv4.h"
#include "vnl/algo/vnl_lbfgs.h"
#include "vnl/algo/vnl_lbfgsb.h"
#include "itkAutoPointer.h"
#include "ITKOptimizersv4Export.h"

namespace itk
{
/* Necessary forward declaration see below for definition */
/** \class LBFGSOptimizerBaseHelperv4
 * \brief Wrapper helper around vnl optimizer.
 *
 * This class is used to translate iteration events, etc, from
 * vnl_lbfgsb into iteration events in ITK.
 *
 * \ingroup ITKOptimizersv4
 */
// Forward reference because of circular dependencies
template< typename TInternalVnlOptimizerType >
class  ITK_TEMPLATE_EXPORT LBFGSOptimizerBaseHelperv4;

/** \class LBFGSOptimizerBasev4
 * \brief Abstract base for vnl lbfgs algorithm optimizers in ITKv4 registration framework.
 *
 * \note The StopConditionDescription returned by this class is directly from the vnl
 * optimizer by calling <tt> m_VnlOptimizer->get_failure_code() </tt>. This seems to
 * return "Failure" even when no error has occured. The same behavior is observed
 * in the ITKv3 version of this optimizer.
 *
 * \note Local-support (high-density) transforms.
 * Local-support transforms are not supported. To add support for these,
 * the class must be modified thusly:
 *
 * \note 1) Parameter updates:
 * In SingleValuedNonLinearCostFunctionAdaptor, the handling of the gradient
 * must be changed to accomodate the fact that local-support transforms expect
 * a gradient to be added to the transform parameters using the
 * UpdateTransformParameters method of the local support transform. Other optimizers
 * in the v4 framework use this method, but the use of the vnl optimizers here
 * complicates it.
 *
 * \note 2) Efficiency
 * To work efficiently with local-support transforms, this class should be
 * modified to use a single parameter object to avoid the multiple
 * parameter copies that are currently performed. It should work to use
 * the transform parameters pointer.
 *
 * This code has been adapted for the ITKv4 registration framework from the
 * v3 version, itkLBFGSOptimizer.
 *
 * \ingroup ITKOptimizersv4
 */
template< typename TInternalVnlOptimizerType >
class ITK_TEMPLATE_EXPORT  LBFGSOptimizerBasev4:
    public SingleValuedNonLinearVnlOptimizerv4
{
public:
  /** Standard "Self" typedef. */
  typedef LBFGSOptimizerBasev4                Self;
  typedef SingleValuedNonLinearVnlOptimizerv4 Superclass;
  typedef SmartPointer< Self >                Pointer;
  typedef SmartPointer< const Self >          ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(LBFGSOptimizerBasev4, SingleValuedNonLinearVnlOptimizerv4);

  typedef Superclass::MetricType     MetricType;
  typedef Superclass::ParametersType ParametersType;
  typedef Superclass::ScalesType     ScalesType;

  /** Stop condition return string type */
  typedef Superclass::StopConditionReturnStringType StopConditionReturnStringType;

  /** Stop condition internal string type */
  typedef Superclass::StopConditionDescriptionType  StopConditionDescriptionType;

  /** The vnl optimizer */
  typedef LBFGSOptimizerBaseHelperv4<TInternalVnlOptimizerType>   InternalOptimizerType;

  /** Method for getting access to the internal optimizer. */
  InternalOptimizerType * GetOptimizer();

  /** Start optimization with an initial value. */
  virtual void StartOptimization(bool doOnlyInitialization = false) ITK_OVERRIDE;

  /** Plug in a Cost Function into the optimizer  */
  virtual void SetMetric(MetricType *metric) ITK_OVERRIDE;

  /** Set/Get the optimizer trace flag. If set to true, the optimizer
   * prints out information every iteration.
   */
  virtual void SetTrace(bool flag);

  itkGetConstMacro(Trace, bool);
  itkBooleanMacro(Trace);

  /** Set/Get the maximum number of function evaluations allowed. */
  virtual void SetMaximumNumberOfFunctionEvaluations(unsigned int n);

  itkGetConstMacro(MaximumNumberOfFunctionEvaluations, unsigned int);

  /** Set/Get the gradient convergence tolerance. This is a positive
   * real number that determines the accuracy with which the solution is to
   * be found. The optimization terminates when:
   * ||G|| < gtol max(1,||X||) where ||.|| denotes the Euclidean norm.
   */
  virtual void SetGradientConvergenceTolerance(double gtol);

  itkGetConstMacro(GradientConvergenceTolerance, double);

  /** Get the reason for termination */
  virtual const StopConditionReturnStringType GetStopConditionDescription() const ITK_OVERRIDE;

protected:
  LBFGSOptimizerBasev4();
  virtual ~LBFGSOptimizerBasev4() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  typedef Superclass::CostFunctionAdaptorType CostFunctionAdaptorType;

  bool                         m_OptimizerInitialized;

  typedef AutoPointer<InternalOptimizerType>  InternalOptimizerAutoPointer;
  InternalOptimizerAutoPointer  m_VnlOptimizer;

  mutable std::ostringstream    m_StopConditionDescription;

  bool         m_Trace;
  unsigned int m_MaximumNumberOfFunctionEvaluations;
  double       m_GradientConvergenceTolerance;
  double       m_InfinityNormOfProjectedGradient;
  double       m_CostFunctionConvergenceFactor;

  // give the helper access to member variables, to update iteration
  // counts, etc.
  friend class LBFGSOptimizerBaseHelperv4<TInternalVnlOptimizerType>;
  friend class LBFGSBOptimizerHelperv4;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LBFGSOptimizerBasev4);

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLBFGSOptimizerBasev4.hxx"
#endif

#endif
