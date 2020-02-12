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
#ifndef itkRegistrationParameterScalesFromPhysicalShift_h
#define itkRegistrationParameterScalesFromPhysicalShift_h

#include "itkRegistrationParameterScalesFromShiftBase.h"

namespace itk
{

/**
 *\class RegistrationParameterScalesFromPhysicalShift
 *  \brief Registration helper class for estimating scales of
 * transform parameters a step sizes, from the maximum voxel shift
 * in physical space caused by a parameter change.
 *
 * \ingroup ITKOptimizersv4
 */
template <typename TMetric>
class ITK_TEMPLATE_EXPORT RegistrationParameterScalesFromPhysicalShift
  : public RegistrationParameterScalesFromShiftBase<TMetric>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(RegistrationParameterScalesFromPhysicalShift);

  /** Standard class type aliases. */
  using Self = RegistrationParameterScalesFromPhysicalShift;
  using Superclass = RegistrationParameterScalesFromShiftBase<TMetric>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegistrationParameterScalesFromPhysicalShift, RegistrationParameterScalesFromShiftBase);

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

protected:
  RegistrationParameterScalesFromPhysicalShift() = default;
  ~RegistrationParameterScalesFromPhysicalShift() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  ComputeSampleShifts(const ParametersType & deltaParameters, ScalesType & localShifts) override;

private:
  template <typename TTransform>
  void
  ComputeSampleShiftsInternal(const ParametersType & deltaParameters, ScalesType & localShifts);

}; // class RegistrationParameterScalesFromPhysicalShift

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRegistrationParameterScalesFromPhysicalShift.hxx"
#endif

#endif /* itkRegistrationParameterScalesFromPhysicalShift_h */
