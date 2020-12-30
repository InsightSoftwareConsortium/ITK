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
#ifndef itkQuasiNewtonOptimizerv4_h
#define itkQuasiNewtonOptimizerv4_h

#include "itkArray2D.h"
#include "itkGradientDescentOptimizerv4.h"

#include "vnl/algo/vnl_matrix_inverse.h"
#include "vnl/algo/vnl_determinant.h"

namespace itk
{
/**
 *\class QuasiNewtonOptimizerv4Template
 * \brief Implement a Quasi-Newton optimizer with BFGS Hessian estimation.
 *
 * Second order approximation of the cost function is usually more efficient
 * since it estimates the descent or ascent direction more precisely. However,
 * computation of Hessian is usually expensive or unavailable. Alternatively
 * Quasi-Newton methods can estimate a Hessian from the gradients in previous
 * steps. Here a specific Quasi-Newton method, BFGS, is used to compute the
 * Quasi-Newton steps.
 *
 * The Quasi-Newton method doesn't produce a valid step sometimes, ex. when
 * the metric function is not a convex locally. In this scenario, the gradient
 * step is used after being scaled properly.
 *
 * A helper member object, m_ScalesEstimator may be set to estimate parameter
 * scales and step scales. A step scale measures the magnitude of a step and
 * is used for learning rate computation.

 * When m_ScalesEstimator is set, SetMaximumNewtonStepSizeInPhysicalUnits()
 * may be called to set the maximum step size. If it is not called,
 * m_MaximumNewtonStepSizeInPhysicalUnits defaults to lambda *
 * OptimizerParameterScalesEstimatorTemplate::EstimateMaximumStepSize(), where lambda is
 * in [1,5].
 *
 * When m_ScalesEstimator is not set, the parameter scales and learning rates
 * defaults to ones, or can be set by users manually.
 *
 * \ingroup ITKOptimizersv4
 */
template <typename TInternalComputationValueType>
class ITK_TEMPLATE_EXPORT QuasiNewtonOptimizerv4Template
  : public GradientDescentOptimizerv4Template<TInternalComputationValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(QuasiNewtonOptimizerv4Template);

  /** Standard class type aliases. */
  using Self = QuasiNewtonOptimizerv4Template;
  using Superclass = GradientDescentOptimizerv4Template<TInternalComputationValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(QuasiNewtonOptimizerv4Template, Superclass);

  /** It should be possible to derive the internal computation type from the class object. */
  using InternalComputationValueType = TInternalComputationValueType;

  using ParametersType = typename Superclass::ParametersType;
  using MeasureType = typename Superclass::MeasureType;
  using DerivativeType = typename Superclass::DerivativeType;
  using IndexRangeType = typename Superclass::IndexRangeType;

  /** Type for Hessian matrix in the Quasi-Newton method */
  using HessianType = itk::Array2D<TInternalComputationValueType>;

  /** Type for an array of Hessian matrix for local support */
  using HessianArrayType = std::vector<HessianType>;

  /** Start and run the optimization */
  void
  StartOptimization(bool doOnlyInitialization = false) override;

  /** Set the maximum tolerable number of iteration without any progress */
  itkSetMacro(MaximumIterationsWithoutProgress, SizeValueType);

  /** Set the maximum step size.
   *
   *  When SetScalesEstimator is called by user, the optimizer will compute
   *  learning rates as
   *      m_MaximumNewtonStepSizeInPhysicalUnits /
   *      m_ScalesEstimator->EstimateStepScale(newtonStep).
   *
   *  If SetMaximumNewtonStepSizeInPhysicalUnits is not called by user,
   *  m_MaximumNewtonStepSizeInPhysicalUnits defaults to
   *      lambda * m_ScalesEstimator->EstimateMaximumStepSize(),
   *
   *  where EstimateMaximumStepSize returns one voxel spacing and
   *  lambda may be in [1,5] according to our experience.
   */
  itkSetMacro(MaximumNewtonStepSizeInPhysicalUnits, TInternalComputationValueType);

  /** Get the most recent Newton step. */
  itkGetConstReferenceMacro(NewtonStep, DerivativeType);

  /**
   * Estimate the quasi-newton step over a given index range.
     This function is used in QuasiNewtonOptimizerv4EstimateNewtonStepThreaderTemplate class.
   */
  virtual void
  EstimateNewtonStepOverSubRange(const IndexRangeType & subrange);

protected:
  /** The maximum tolerable number of iteration without any progress */
  SizeValueType m_MaximumIterationsWithoutProgress{ 30 };

  /** The information about the current step */
  ParametersType m_CurrentPosition;
  ParametersType m_OptimalStep;

  /** The information about the previous step */
  MeasureType    m_PreviousValue;
  ParametersType m_PreviousPosition;

  /** The best value so far and relevant information */
  MeasureType    m_BestValue;
  ParametersType m_BestPosition;
  SizeValueType  m_BestIteration{ 0 };

  /** The Quasi-Newton step */
  DerivativeType m_NewtonStep;

  /** Warning message during Quasi-Newton step estimation */
  std::string m_NewtonStepWarning;

  /** The maximum Quasi-Newton step size to restrict learning rates. */
  TInternalComputationValueType m_MaximumNewtonStepSizeInPhysicalUnits;

  /** The Hessian with local support */
  HessianArrayType m_HessianArray;

  /** Valid flag for the Quasi-Newton steps */
  std::vector<bool> m_NewtonStepValidFlags;

  /** Estimate a Newton step */
  virtual void
  EstimateNewtonStep();

  /** Estimate the next Hessian and step with BFGS method.
   *  The details of the method are described at
   *  http://en.wikipedia.org/wiki/BFGS_method .
   */
  virtual bool
  ComputeHessianAndStepWithBFGS(IndexValueType loc);

  /** Reset the Hessian to identity matrix and the Newton step to zeros. */
  virtual void
  ResetNewtonStep(IndexValueType loc);

  /**
   * Combine a gradient step with a Newton step. The Newton step will be used
   * when it is valid. Otherwise the gradient step will be used.
   */
  void
  CombineGradientNewtonStep();

  /**
   *  Estimate and apply the learning rate(s) for a combined Newton step.
   *  A combined Newton step uses the Newton step by default and the gradient
   *  step when the Newton step is not valid.
   *
   *  The learning rate is less than 1.0 and is restricted by
   *  m_MaximumNewtonStepSizeInPhysicalUnits.
   */
  void
  ModifyCombinedNewtonStep();

  /**
   * Advance one step using the Quasi-Newton step. When the Newton step
   * is invalid, the gradient step will be used.
   */
  void
  AdvanceOneStep() override;

  QuasiNewtonOptimizerv4Template();
  ~QuasiNewtonOptimizerv4Template() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Threader for Newton step estimation. */
  typename DomainThreader<ThreadedIndexedContainerPartitioner, Self>::Pointer m_EstimateNewtonStepThreader;
};

/** This helps to meet backward compatibility */
using QuasiNewtonOptimizerv4 = QuasiNewtonOptimizerv4Template<double>;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkQuasiNewtonOptimizerv4.hxx"
#endif

#endif
