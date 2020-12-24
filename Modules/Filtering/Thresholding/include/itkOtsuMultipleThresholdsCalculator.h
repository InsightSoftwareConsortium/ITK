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
#ifndef itkOtsuMultipleThresholdsCalculator_h
#define itkOtsuMultipleThresholdsCalculator_h

#include "itkHistogramAlgorithmBase.h"
#include "itkHistogram.h"

namespace itk
{
/**
 *\class OtsuMultipleThresholdsCalculator
 * \brief Computes Otsu's multiple thresholds for a histogram.
 *
 * You plug in the target histogram using SetInputHistogram method and
 * specify the number of thresholds you want to be computed. Then call
 * the Compute() method to run the algorithm.
 *
 * The thresholds are computed so that the between-class variance is
 * maximized.
 *
 * This calculator also includes an option to use the valley emphasis algorithm from
 * H.F. Ng, "Automatic thresholding for defect detection", Pattern Recognition Letters, (27): 1644-1649, 2006.
 * The valley emphasis algorithm is particularly effective when the object to be thresholded is small.
 * See the following tests for examples:
 * itkOtsuMultipleThresholdsImageFilterTest3 and itkOtsuMultipleThresholdsImageFilterTest4
 * To use this algorithm, simple call the setter: SetValleyEmphasis(true)
 * It is turned off by default.
 *
 * \ingroup Calculators
 * \ingroup ITKThresholding
 */

template <typename TInputHistogram>
class ITK_TEMPLATE_EXPORT OtsuMultipleThresholdsCalculator : public HistogramAlgorithmBase<TInputHistogram>
{
public:
  /**Standard class type aliases. */
  using Self = OtsuMultipleThresholdsCalculator;
  using Superclass = HistogramAlgorithmBase<TInputHistogram>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using MeasurementType = typename TInputHistogram::MeasurementType;
  using FrequencyType = typename TInputHistogram::AbsoluteFrequencyType;

  using MeanType = typename NumericTraits<MeasurementType>::RealType;
  using VarianceType = typename NumericTraits<MeasurementType>::RealType;
  using WeightType = typename NumericTraits<MeasurementType>::RealType;

  using MeanVectorType = std::vector<MeanType>;
  using FrequencyVectorType = std::vector<FrequencyType>;
  using WeightVectorType = std::vector<WeightType>;

  using InstanceIdentifierType = typename TInputHistogram::InstanceIdentifier;
  using InstanceIdentifierVectorType = std::vector<InstanceIdentifierType>;

  /**Standard Macros */
  itkTypeMacro(OtsuMultipleThresholdsCalculator, HistogramAlgorithmsBase);
  itkNewMacro(Self);

  /** Typedef for the thresholds output */
  using OutputType = std::vector<MeasurementType>;

  /** Returns the thresholds vector */
  const OutputType &
  GetOutput();

  /** Set/Get the number of thresholds. */
  itkSetClampMacro(NumberOfThresholds, SizeValueType, 1, NumericTraits<SizeValueType>::max());
  itkGetConstMacro(NumberOfThresholds, SizeValueType);

  /** Calculates Otsu's thresholds and saves them. */
  void
  Compute() override;

  /** Set/Get the use of valley emphasis. Default is false. */
  itkSetMacro(ValleyEmphasis, bool);
  itkGetConstReferenceMacro(ValleyEmphasis, bool);
  itkBooleanMacro(ValleyEmphasis);

  /** Should the threshold value be mid-point of the bin or the maximum?
   * Default is to return bin maximum. */
  itkSetMacro(ReturnBinMidpoint, bool);
  itkGetConstReferenceMacro(ReturnBinMidpoint, bool);
  itkBooleanMacro(ReturnBinMidpoint);


protected:
  OtsuMultipleThresholdsCalculator();
  ~OtsuMultipleThresholdsCalculator() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Increment the thresholds of one position along the histogram. */
  bool
  IncrementThresholds(InstanceIdentifierVectorType & thresholdIndexes,
                      MeanType                       globalMean,
                      MeanVectorType &               classMean,
                      FrequencyVectorType &          classFrequency);

private:
  SizeValueType m_NumberOfThresholds{ 1 };
  OutputType    m_Output;
  bool          m_ValleyEmphasis{ false };
#if defined(ITKV4_COMPATIBILITY)
  bool m_ReturnBinMidpoint{ true };
#else
  bool m_ReturnBinMidpoint{ false };
#endif
};
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkOtsuMultipleThresholdsCalculator.hxx"
#endif

#endif
