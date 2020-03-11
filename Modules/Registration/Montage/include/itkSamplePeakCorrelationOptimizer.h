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
#ifndef itkSamplePeakCorrelationOptimizer_h
#define itkSamplePeakCorrelationOptimizer_h

#include "itkNMinimaMaximaImageCalculator.h"
#include "itkPhaseCorrelationOptimizer.h"

namespace itk
{
/** \class SamplePeakCorrelationOptimizer
 *
 *  \brief Implements integer shift estimation from position of maximum peak.
 *
 *  This class is templated over the type of registration method in which it has
 *  to be plugged in.
 *
 *  Operates on real correlation surface, so when set to the registration method,
 *  it should be get back by
 *  PhaseCorrelationImageRegistrationMethod::GetRealOptimizer() method.
 *
 *  The optimizer finds the maximum peak by MinimumMaximumImageCalculator.
 *
 * \author Jakub Bican, jakub.bican@matfyz.cz, Department of Image Processing,
 *         Institute of Information Theory and Automation,
 *         Academy of Sciences of the Czech Republic.
 *
 * \ingroup Montage
 */
template <typename TRegistrationMethod>
class ITK_TEMPLATE_EXPORT SamplePeakCorrelationOptimizer
  : public PhaseCorrelationOptimizer<typename TRegistrationMethod::RealImageType>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(SamplePeakCorrelationOptimizer);

  using Self = SamplePeakCorrelationOptimizer;
  using Superclass = PhaseCorrelationOptimizer<typename TRegistrationMethod::RealImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SamplePeakCorrelationOptimizer, PhaseCorrelationOptimizer);

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

  using MaxCalculatorType = NMinimaMaximaImageCalculator<ImageType>;
  using IndexContainerType = typename MaxCalculatorType::IndexVector;

  using PeakInterpolationMethodEnum = typename Superclass::PeakInterpolationMethodEnum;

  /** Get/Set maximum city-block distance for peak merging. Zero disables it. */
  itkGetConstMacro(MergePeaks, unsigned);
  itkSetMacro(MergePeaks, unsigned);

  /** Get/Set suppression aggressiveness of trivial [0,0,...] solution. */
  itkGetConstMacro(ZeroSuppression, double);
  itkSetClampMacro(ZeroSuppression, double, 0.0, 100.0);

  /** Get/Set expected maximum linear translation needed, in pixels.
   * Zero (the default) has a special meaning: sigmoid scaling
   * with half-way point at around quarter of image size.
   * Translations can plausibly be up to half an image size. */
  itkGetConstMacro(PixelDistanceTolerance, SizeValueType);
  itkSetMacro(PixelDistanceTolerance, SizeValueType);

  /** Get correlation image biased towards the expected solution. */
  itkGetConstObjectMacro(AdjustedInput, ImageType);

  /** Indices of the maxima. */
  itkGetConstReferenceMacro(MaxIndices, IndexContainerType);

  bool
  SupportsPeakInterpolationMethod(PeakInterpolationMethodEnum method) const override;

protected:
  SamplePeakCorrelationOptimizer();
  virtual ~SamplePeakCorrelationOptimizer() = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** This method is executed by superclass to execute the computation. */
  void
  ComputeOffset() override;

private:
  typename MaxCalculatorType::Pointer m_MaxCalculator = MaxCalculatorType::New();
  unsigned                            m_MergePeaks = 1;
  double                              m_ZeroSuppression = 5;
  SizeValueType                       m_PixelDistanceTolerance = 0;

  typename ImageType::Pointer m_AdjustedInput;
  IndexContainerType          m_MaxIndices;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSamplePeakCorrelationOptimizer.hxx"
#endif

#endif
