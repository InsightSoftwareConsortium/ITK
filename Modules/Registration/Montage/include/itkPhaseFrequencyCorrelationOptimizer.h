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
#ifndef itkPhaseFrequencyCorrelationOptimizer_h
#define itkPhaseFrequencyCorrelationOptimizer_h

#include "itkSamplePeakCorrelationOptimizer.h"

namespace itk
{
/** \class PhaseFrequencyCorrelationOptimizer
 *  \brief Implements shift estimation based on complex phase zero-crossing.
 *
 *  This class is templated over the type of registration method in which it has
 *  to be plugged in.
 *
 *  Operates on complex correlation surface, so when set to the registration method,
 *  it should be get back by
 *  PhaseCorrelationImageRegistrationMethod::GetComplexOptimizer() method.
 *
 *  The optimizer finds the maximum peak by SamplePeakCorrelationOptimizer.
 *  If interpolation method is None, the shift is estimated with pixel-level
 *  precision. Otherwise the requested interpolation method is used.
 *
 * \ingroup Montage
 */
template <typename TRegistrationMethod>
class ITK_TEMPLATE_EXPORT PhaseFrequencyCorrelationOptimizer
  : public PhaseCorrelationOptimizer<typename TRegistrationMethod::ComplexImageType>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PhaseFrequencyCorrelationOptimizer);

  using Self = PhaseFrequencyCorrelationOptimizer;
  using Superclass = PhaseCorrelationOptimizer<typename TRegistrationMethod::ComplexImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PhaseFrequencyCorrelationOptimizer, PhaseCorrelationOptimizer);

  /**  Type of the input image. */
  using ImageType = typename TRegistrationMethod::RealImageType;
  using ImageConstPointer = typename ImageType::ConstPointer;

  /** Dimensionality of input and output data. */
  static constexpr unsigned int ImageDimension = ImageType::ImageDimension;

  /** Real correlation surface's pixel type. */
  using PixelType = typename ImageType::PixelType;

  /** Type for the output parameters.
   *  It defines a position in the optimization search space. */
  using OffsetType = typename Superclass::OffsetType;
  using OffsetScalarType = typename Superclass::OffsetScalarType;

  using ConfidenceVector = typename Superclass::ConfidenceVector;

  using PeakInterpolationMethodEnum = typename Superclass::PeakInterpolationMethodEnum;

  const ConfidenceVector & GetConfidences() const override
  {
    return this->m_SamplePeakOptimizer->GetConfidences();
  }

  using SamplePeakOptimizerType = SamplePeakCorrelationOptimizer<TRegistrationMethod>;
  itkGetModifiableObjectMacro(SamplePeakOptimizer, SamplePeakOptimizerType);

protected:
  PhaseFrequencyCorrelationOptimizer() = default;
  virtual ~PhaseFrequencyCorrelationOptimizer() = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** This method is executed by superclass to execute the computation. */
  void
  ComputeOffset() override;

private:
  typename SamplePeakOptimizerType::Pointer m_SamplePeakOptimizer = SamplePeakOptimizerType::New();
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPhaseFrequencyCorrelationOptimizer.hxx"
#endif

#endif
