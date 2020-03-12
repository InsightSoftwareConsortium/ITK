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
#ifndef itkPhaseFrequencyCorrelationOptimizer_h
#define itkPhaseFrequencyCorrelationOptimizer_h

#include "itkSamplePeakCorrelationOptimizer.h"
#include "itkMaxPhaseCorrelationOptimizer.h"
#include "itkCyclicShiftImageFilter.h"
#include "itkRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkFFTPadImageFilter.h"

namespace itk
{
/** \class PhaseFrequencyCorrelationOptimizer
 *  \brief Implements shift estimation based on complex phase zero-crossing.
 *
 *  This class is templated over the type of registration method in which it has
 *  to be plugged in.
 *
 *  When set to the registration method,
 *  it should be get back by
 *  PhaseCorrelationImageRegistrationMethod::GetComplexOptimizer() method.
 *
 *  The optimizer finds the maximum peak by SamplePeakCorrelationOptimizer.
 *  If interpolation method is None, the shift is estimated with pixel-level
 *  precision. If the interpolation method is Parabolic or Cosine, the peak is
 *  estimated by fitting these function around the peak. If the interpolation
 *  method is WeightedMeanPhase or PhaseFrequencySlope, for efficiency, these
 *  methods are used the first PhaseInterpolated number of peaks, and Parabolic
 *  interpolation is used for the remaining peaks.
 *
 *  The WeightedMeanPhase and PhaseFrequencySlope are implemented.
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

  const ConfidenceVector &
  GetConfidences() const override
  {
    return this->m_MaxPhaseOptimizer->GetConfidences();
  }

  using MaxPhaseOptimizerType = MaxPhaseCorrelationOptimizer<TRegistrationMethod>;
  itkGetModifiableObjectMacro(MaxPhaseOptimizer, MaxPhaseOptimizerType);

  bool
  SupportsPeakInterpolationMethod(PeakInterpolationMethodEnum method) const override;

  /** Number of peaks to use phase-based sub-sample interpolation with the
   * WeightedMeanPhase or PhaseFrequencySlope methods. */
  itkGetConstMacro(PhaseInterpolated, unsigned int);
  itkSetMacro(PhaseInterpolated, unsigned int);

  SizeValueType GetPixelDistanceTolerance() const
  {
    return this->m_MaxPhaseOptimizer->GetPixelDistanceTolerance();
  }
  void SetPixelDistanceTolerance(SizeValueType tolerance)
  {
    return this->m_MaxPhaseOptimizer->SetPixelDistanceTolerance(tolerance);
  }

protected:
  PhaseFrequencyCorrelationOptimizer();
  virtual ~PhaseFrequencyCorrelationOptimizer() = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** This method is executed by superclass to execute the computation. */
  void
  ComputeOffset() override;

private:
  typename MaxPhaseOptimizerType::Pointer m_MaxPhaseOptimizer = MaxPhaseOptimizerType::New();

  using CyclicShiftFilterType = CyclicShiftImageFilter< typename TRegistrationMethod::RealImageType >;
  typename CyclicShiftFilterType::Pointer m_CyclicShiftFilter = CyclicShiftFilterType::New();

  unsigned int m_PhaseInterpolated {1};

  using PadFilterType = FFTPadImageFilter< typename TRegistrationMethod::RealImageType, typename TRegistrationMethod::RealImageType >;
  typename PadFilterType::Pointer m_PadFilter = PadFilterType::New();

  using FFTFilterType = RealToHalfHermitianForwardFFTImageFilter< typename TRegistrationMethod::RealImageType >;
  typename FFTFilterType::Pointer m_FFTFilter = FFTFilterType::New();
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPhaseFrequencyCorrelationOptimizer.hxx"
#endif

#endif
