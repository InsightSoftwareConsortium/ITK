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
#ifndef itkGradientDescentLineSearchOptimizerv4_h
#define itkGradientDescentLineSearchOptimizerv4_h

#include "itkGradientDescentOptimizerv4.h"
#include "itkOptimizerParameterScalesEstimator.h"
#include "itkWindowConvergenceMonitoringFunction.h"

namespace itk
{
/**
 *\class GradientDescentLineSearchOptimizerv4Template
 *  \brief Gradient descent optimizer with a golden section line search.
 *
 * GradientDescentLineSearchOptimizer implements a simple gradient descent optimizer
 * that is followed by a line search to find the best value for the learning rate.
 * At each iteration the current position is updated according to
 *
 * \f[
 *        p_{n+1} = p_n
 *                + \mbox{learningRateByGoldenSectionLineSearch}
 \, \frac{\partial f(p_n) }{\partial p_n}
 * \f]
 *
 * Options are identical to the superclass's except for:
 *
 * options Epsilon, LowerLimit and UpperLimit that will guide
 * a golden section line search to find the optimal gradient update
 * within the range :
 *
 *   [ learningRate * LowerLimit , learningRate * UpperLimit ]
 *
 * where Epsilon sets the resolution of the search.  Smaller values
 * lead to additional computation time but better localization of
 * the minimum.
 *
 * By default, this optimizer will return the best value and associated
 * parameters that were calculated during the optimization.
 * See SetReturnBestParametersAndValue().
 *
 * \ingroup ITKOptimizersv4
 */
template <typename TInternalComputationValueType>
class ITK_TEMPLATE_EXPORT GradientDescentLineSearchOptimizerv4Template
  : public GradientDescentOptimizerv4Template<TInternalComputationValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GradientDescentLineSearchOptimizerv4Template);

  /** Standard class type aliases. */
  using Self = GradientDescentLineSearchOptimizerv4Template;
  using Superclass = GradientDescentOptimizerv4Template<TInternalComputationValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(GradientDescentLineSearchOptimizerv4Template, Superclass);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  /** It should be possible to derive the internal computation type from the class object. */
  using InternalComputationValueType = TInternalComputationValueType;

  /** Derivative type */
  using DerivativeType = typename Superclass::DerivativeType;

  /** Metric type over which this class is templated */
  using MeasureType = typename Superclass::MeasureType;
  using ParametersType = typename Superclass::ParametersType;

  /** Type for the convergence checker */
  using ConvergenceMonitoringType = itk::Function::WindowConvergenceMonitoringFunction<TInternalComputationValueType>;

  /** The epsilon determines the accuracy of the line search
   *  i.e. the energy alteration that is considered convergent.
   */
  itkSetMacro(Epsilon, TInternalComputationValueType);
  itkGetMacro(Epsilon, TInternalComputationValueType);

  /** The upper and lower limit below determine the range
   *  of values over which the learning rate can be adjusted
   *  by the golden section line search.  The update can then
   *  occur in the range from the smallest change given by :
   *     NewParams = OldParams + LowerLimit * gradient
   *  to the largest change given by :
   *     NewParams = OldParams + UpperLimit * gradient
   *  Reasonable values might be 0 and 2.
   */
  itkSetMacro(LowerLimit, TInternalComputationValueType);
  itkGetMacro(LowerLimit, TInternalComputationValueType);
  itkSetMacro(UpperLimit, TInternalComputationValueType);
  itkGetMacro(UpperLimit, TInternalComputationValueType);
  itkSetMacro(MaximumLineSearchIterations, unsigned int);
  itkGetMacro(MaximumLineSearchIterations, unsigned int);

protected:
  /** Advance one Step following the gradient direction.
   * Includes transform update. */
  void
  AdvanceOneStep() override;

  /** Default constructor */
  GradientDescentLineSearchOptimizerv4Template();

  /** Destructor */
  ~GradientDescentLineSearchOptimizerv4Template() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  TInternalComputationValueType
  GoldenSectionSearch(TInternalComputationValueType a,
                      TInternalComputationValueType b,
                      TInternalComputationValueType c,
                      TInternalComputationValueType metricb = NumericTraits<TInternalComputationValueType>::max());

  TInternalComputationValueType m_LowerLimit;
  TInternalComputationValueType m_UpperLimit;
  TInternalComputationValueType m_Phi;
  TInternalComputationValueType m_Resphi;
  TInternalComputationValueType m_Epsilon;

  /** Controls the maximum recursion depth for the golden section search */
  unsigned int m_MaximumLineSearchIterations;
  /** Counts the recursion depth for the golden section search */
  unsigned int m_LineSearchIterations;
};

/** This helps to meet backward compatibility */
using GradientDescentLineSearchOptimizerv4 = GradientDescentLineSearchOptimizerv4Template<double>;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGradientDescentLineSearchOptimizerv4.hxx"
#endif

#endif
