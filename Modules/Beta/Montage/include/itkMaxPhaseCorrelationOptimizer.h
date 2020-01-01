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
#ifndef itkMaxPhaseCorrelationOptimizer_h
#define itkMaxPhaseCorrelationOptimizer_h

#include "itkSamplePeakCorrelationOptimizer.h"

namespace itk
{

/** \class MaxPhaseCorrelationOptimizerEnums
 * \ingroup Montage
 */
class MaxPhaseCorrelationOptimizerEnums
{
public:
  /** \class PeakInterpolationMethod
   *  \brief Different methods of interpolation the phase correlation peak.
   *  \ingroup Montage */
  enum class PeakInterpolationMethod : uint8_t
  {
    None = 0,
    Parabolic,
    Cosine,
    Last = Cosine
  };
};

/** \class MaxPhaseCorrelationOptimizer
 *  \brief Implements basic shift estimation from position of maximum peak.
 *
 *  This class is templated over the type of registration method in which it has
 *  to be plugged in.
 *
 *  Operates on real correlation surface, so when set to the registration method,
 *  it should be get back by
 *  PhaseCorrelationImageRegistrationMethod::GetRealOptimizer() method.
 *
 *  The optimizer finds the maximum peak by SamplePeakCorrelationOptimizer.
 *  If interpolation method is None, the shift is estimated with pixel-level
 *  precision. Otherwise the requested interpolation method is used.
 *
 * \author Jakub Bican, jakub.bican@matfyz.cz, Department of Image Processing,
 *         Institute of Information Theory and Automation,
 *         Academy of Sciences of the Czech Republic.
 *
 * \ingroup Montage
 */
template <typename TRegistrationMethod>
class ITK_TEMPLATE_EXPORT MaxPhaseCorrelationOptimizer
  : public PhaseCorrelationOptimizer<typename TRegistrationMethod::RealImageType>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MaxPhaseCorrelationOptimizer);

  using Self = MaxPhaseCorrelationOptimizer;
  using Superclass = PhaseCorrelationOptimizer<typename TRegistrationMethod::RealImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MaxPhaseCorrelationOptimizer, PhaseCorrelationOptimizer);

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
  MaxPhaseCorrelationOptimizer() = default;
  ~MaxPhaseCorrelationOptimizer() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** This method is executed by superclass to execute the computation. */
  void
  ComputeOffset() override;

  bool
  SupportsPeakInterpolationMethod(PeakInterpolationMethodEnum method) const override;

private:
  typename SamplePeakOptimizerType::Pointer m_SamplePeakOptimizer = SamplePeakOptimizerType::New();
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMaxPhaseCorrelationOptimizer.hxx"
#endif

#endif
