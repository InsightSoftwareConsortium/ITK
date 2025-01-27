/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkPhaseCorrelationOptimizer_h
#define itkPhaseCorrelationOptimizer_h

#include "itkImage.h"
#include "itkNumericTraits.h"
#include "itkProcessObject.h"
#include "itkSimpleDataObjectDecorator.h"
#include <vector>
#include "MontageExport.h"
#include "itkNMinimaMaximaImageCalculator.h"
#include "itkCyclicShiftImageFilter.h"
#include "itkRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkFFTPadImageFilter.h"

namespace itk
{

/** \class PhaseCorrelationOptimizerEnums
 * \ingroup Montage
 */
class PhaseCorrelationOptimizerEnums
{
public:
  /** \class PeakInterpolationMethodEnum
   *  \brief Different methods of interpolation the phase correlation peak.
   *  \ingroup Montage */
  enum class PeakInterpolationMethod : uint8_t
  {
    None = 0,
    Parabolic = 1,
    Cosine = 2,
    WeightedMeanPhase = 3,
    // PhaseFrequencySlope = 4,
  };

  // For iteration
  static const std::initializer_list<PeakInterpolationMethod>
  AllPeakInterpolationMethods()
  {
    const std::initializer_list<PeakInterpolationMethod> methods{
      PeakInterpolationMethod::None,
      PeakInterpolationMethod::Parabolic,
      PeakInterpolationMethod::Cosine,
      PeakInterpolationMethod::WeightedMeanPhase,
      // PeakInterpolationMethod::PhaseFrequencySlope
    };
    return methods;
  }
};

/** Define how to print enumerations */
extern Montage_EXPORT std::ostream &
operator<<(std::ostream & out, const PhaseCorrelationOptimizerEnums::PeakInterpolationMethod value);


/** \class PhaseCorrelationOptimizer
 *
 *  \brief Defines common interface for optimizers, that estimates the shift
 *         from correlation surface.
 *
 *  The class is templated over the input phase correlation real pixel type.
 *  As some optimization methods operate on
 *  real correlation surface and some on complex correlation surface, both
 *  input phase correlation images can be set with SetRealInput and SetComplexInput.
 *
 *  The peak interpolation method is defined by SetPeakInterpolationMethod.
 *  More complex peak interpolation methods use simpler methods as preliminary
 *  steps.
 *
 *  For PeakInterpolationMethod::None the integer pixel-precision max peaks
 *  are found.  This step operates on the real correlation surface.
 *  The optimizer finds the maximum peak by NMinimaMaximaImageCalculator, and
 *  applies constraits from the SetMergePeaks, SetZeroSuppression, and
 *  SetPixelDistanceTolerance parameters.
 *
 *  For PeakInterpolationMethod::Parabolic or PeakInterpolationMethod::Cosine,
 *  a sub-pixel peak is found by fitting these functions to the real phase
 *  correlation surface.
 *
 *  For PeakInterpolationMethod::WeightedMeanPhase, for efficiency, the
 *  weighted mean phase method is used the first PhaseInterpolated
 *  number of peaks, and Parabolic interpolation is used for the remaining peaks.
 *  This approach is summarized in:
 *
 *    https://www.ncbi.nlm.nih.gov/pubmed/31352341
 *
 *  Power spectrum weighted mean. (Eqn. 10)
 *
 *  Future work may add support for the
 *  slope of the phase-frequency least squares linear regression. (Eqn. 14)
 *
 *
 * \author Matt McCormick, matt.mccormick@kitware.com, Kitware, Inc
 * \author Dženan Zukić, dzenan.zukic@kitware.com, Kitware, Inc
 * \author Jakub Bican, jakub.bican@matfyz.cz, Department of Image Processing,
 *         Institute of Information Theory and Automation,
 *         Academy of Sciences of the Czech Republic.
 *
 * \ingroup Montage
 */
template <typename TRealPixel, unsigned int VImageDimension>
class ITK_TEMPLATE_EXPORT PhaseCorrelationOptimizer : public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PhaseCorrelationOptimizer);

  using Self = PhaseCorrelationOptimizer;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkOverrideGetNameOfClassMacro(PhaseCorrelationOptimizer);

  /**  Type of the inputs. */
  static constexpr unsigned int ImageDimension = VImageDimension;

  using RealPixelType = TRealPixel;
  using ComplexPixelType = std::complex<RealPixelType>;
  using ImageType = Image<RealPixelType, ImageDimension>;
  using ComplexImageType = Image<ComplexPixelType, ImageDimension>;

  /** Type for the output parameters.
   *  It defines a position in the optimization search space. */
  using OffsetType = typename ImageType::PointType;
  using OffsetScalarType = typename OffsetType::ValueType;

  /** Type for the output: Using Decorator pattern for enabling
   *  the offset to be passed in the data pipeline */
  using OffsetOutputType = SimpleDataObjectDecorator<OffsetType>;
  using OffsetOutputPointer = typename OffsetOutputType::Pointer;
  using OffsetOutputConstPointer = typename OffsetOutputType::ConstPointer;

  /** Smart Pointer type to a DataObject. */
  using DataObjectPointer = typename DataObject::Pointer;

  /** Resulting vector of offsets. */
  using OffsetVector = std::vector<OffsetType>;

  /** Get the computed offsets. */
  itkGetConstReferenceMacro(Offsets, OffsetVector);

  /** Confidences corresponding to offsets. */
  using ConfidenceVector = std::vector<typename NumericTraits<typename ImageType::PixelType>::ValueType>;

  /** Get the confidences corresponding to offsets. */
  itkGetConstReferenceMacro(Confidences, ConfidenceVector);

  /** Sets the fixed image to the optimizer. */
  void
  SetFixedImage(const ImageBase<ImageType::ImageDimension> * image);

  /** Sets the fixed image to the optimizer. */
  void
  SetMovingImage(const ImageBase<ImageType::ImageDimension> * image);

  /** Sets the real phase correlation input image to the optimizer. */
  void
  SetRealInput(const ImageType * image);

  /** Sets the complex phase correlation input image to the optimizer. */
  void
  SetComplexInput(const ComplexImageType * image);

  using PeakInterpolationMethodEnum = PhaseCorrelationOptimizerEnums::PeakInterpolationMethod;
  itkGetConstMacro(PeakInterpolationMethod, PeakInterpolationMethodEnum);
  void
  SetPeakInterpolationMethod(const PeakInterpolationMethodEnum peakInterpolationMethod);

  /** Returns the offset resulting from the registration process  */
  const OffsetOutputType *
  GetOutput(unsigned index) const
  {
    return static_cast<const OffsetOutputType *>(this->ProcessObject::GetOutput(index));
  }

  /** Get/Set number of maximums to be computed.
   * Resulting count could be smaller than requested!
   * After Update is called, check count again. */
  virtual void
  SetOffsetCount(unsigned count);
  virtual unsigned
  GetOffsetCount() const
  {
    return m_Offsets.size();
  }

  using MaxCalculatorType = NMinimaMaximaImageCalculator<ImageType>;
  using IndexContainerType = typename MaxCalculatorType::IndexVector;

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

  /** Number of peaks to use phase-based sub-sample interpolation with the
   * WeightedMeanPhase methods. */
  itkGetConstMacro(PhaseInterpolated, unsigned int);
  itkSetMacro(PhaseInterpolated, unsigned int);

#ifdef ITK_USE_CONCEPT_CHECKING
  static_assert(!std::is_same<TRealPixel, bool>::value, "bool is not supported as RealPixelType");
#endif

protected:
  PhaseCorrelationOptimizer();
  ~PhaseCorrelationOptimizer() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Method invoked by the pipeline in order to trigger the computation of
   * the output values. */
  void
  GenerateData() override;

  void
  ComputeOffset();

  using Superclass::MakeOutput;

  /** Make a DataObject of the correct type to be used as the specified
   *  output. */
  DataObjectPointer
  MakeOutput(DataObjectPointerArraySizeType itkNotUsed(idx)) override
  {
    return static_cast<DataObject *>(OffsetOutputType::New().GetPointer());
  }

private:
  PeakInterpolationMethodEnum m_PeakInterpolationMethod = PeakInterpolationMethodEnum::Parabolic;

  OffsetVector     m_Offsets;
  ConfidenceVector m_Confidences;

  typename MaxCalculatorType::Pointer m_MaxCalculator = MaxCalculatorType::New();
  unsigned                            m_MergePeaks = 1;
  double                              m_ZeroSuppression = 5;
  SizeValueType                       m_PixelDistanceTolerance = 0;

  typename ImageType::Pointer m_AdjustedInput;
  IndexContainerType          m_MaxIndices;

  using CyclicShiftFilterType = CyclicShiftImageFilter<ImageType>;
  typename CyclicShiftFilterType::Pointer m_CyclicShiftFilter = CyclicShiftFilterType::New();

  unsigned int m_PhaseInterpolated{ 1 };

  using PadFilterType = FFTPadImageFilter<ImageType, ImageType>;
  typename PadFilterType::Pointer m_PadFilter = PadFilterType::New();

  using FFTFilterType = RealToHalfHermitianForwardFFTImageFilter<ImageType>;
  typename FFTFilterType::Pointer m_FFTFilter = FFTFilterType::New();
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPhaseCorrelationOptimizer.hxx"
#endif

#endif
