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
#ifndef itkRegistrationParameterScalesFromJacobian_h
#define itkRegistrationParameterScalesFromJacobian_h

#include "itkRegistrationParameterScalesEstimator.h"

namespace itk
{

/**
 *\class RegistrationParameterScalesFromJacobian
 *  \brief Implements a registration helper class for estimating scales of
 * transform parameters from Jacobian norms.
 *
 * Its input includes the fixed/moving images and transform objects,
 * which can be obtained from the metric object.
 *
 * The scale of a parameter is estimated from the averaged squared norm of
 * the Jacobian w.r.t the parameter. The averaging is done over a sampling
 * of the image domain. The sampling by default is a uniform random
 * distribution.
 *
 * \ingroup ITKOptimizersv4
 */
template <typename TMetric>
class ITK_TEMPLATE_EXPORT RegistrationParameterScalesFromJacobian : public RegistrationParameterScalesEstimator<TMetric>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(RegistrationParameterScalesFromJacobian);

  /** Standard class type aliases. */
  using Self = RegistrationParameterScalesFromJacobian;
  using Superclass = RegistrationParameterScalesEstimator<TMetric>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegistrationParameterScalesFromJacobian, RegistrationParameterScalesEstimator);

  /** Type of scales */
  using ScalesType = typename Superclass::ScalesType;
  /** Type of parameters of the optimizer */
  using ParametersType = typename Superclass::ParametersType;
  /** Type of float */
  using FloatType = typename Superclass::FloatType;

  using VirtualPointType = typename Superclass::VirtualPointType;
  using VirtualIndexType = typename Superclass::VirtualIndexType;
  using MovingTransformType = typename Superclass::MovingTransformType;
  using FixedTransformType = typename Superclass::FixedTransformType;
  using JacobianType = typename Superclass::JacobianType;
  using VirtualImageConstPointer = typename Superclass::VirtualImageConstPointer;

  /** Estimate parameter scales. */
  void
  EstimateScales(ScalesType & parameterScales) override;

  /**
   *  Estimate the scale for \f$\Delta p\f$, the step of change on parameters.
   *  The step scale describes the impact of \f$\Delta p\f$ on the transform.
   *
   *  Let us denote the transform by
   *  \f[ T(x, p) = T(x, p_0 + t * \Delta p) \f]
   *  where \f$x\f$ is the coordinates of a voxel, \f$p = p_0+t*\Delta p\f$ is
   *  the transform parameters, and \f$t\f$ is the step factor.
   *
   *  At a specific voxel at \f$x\f$, the step scale w.r.t. \f$\Delta p\f$ is
   *  defined here as
   *
   *  \f[ |\frac{dT}{dt}| = |\frac{\partial T}{\partial p} * \Delta p |. \f]
   *
   *  For multiple voxels, we average the above formula to get the overall
   *  step scale.
   */
  FloatType
  EstimateStepScale(const ParametersType & step) override;

  /** Estimate the scales of local steps. */
  void
  EstimateLocalStepScales(const ParametersType & step, ScalesType & localStepScales) override;

protected:
  RegistrationParameterScalesFromJacobian() = default;
  ~RegistrationParameterScalesFromJacobian() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /**
   *  Compute the step scales for samples, i.e. the impacts on each sampled
   *  voxel from a change on the transform.
   */
  void
  ComputeSampleStepScales(const ParametersType & step, ScalesType & sampleScales);
}; // class RegistrationParameterScalesFromJacobian


} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRegistrationParameterScalesFromJacobian.hxx"
#endif

#endif /* itkRegistrationParameterScalesFromJacobian_h */
