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
#ifndef itkOtsuMultipleThresholdsCalculator_h
#define itkOtsuMultipleThresholdsCalculator_h

#include "itkHistogramAlgorithmBase.h"
#include "itkHistogram.h"

namespace itk
{
/** \class OtsuMultipleThresholdsCalculator
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

template< typename TInputHistogram >
class ITK_TEMPLATE_EXPORT OtsuMultipleThresholdsCalculator:
  public HistogramAlgorithmBase< TInputHistogram >
{
public:
  /**Standard class typedefs. */
  typedef OtsuMultipleThresholdsCalculator          Self;
  typedef HistogramAlgorithmBase< TInputHistogram > Superclass;
  typedef SmartPointer< Self >                      Pointer;
  typedef SmartPointer< const Self >                ConstPointer;

  typedef typename TInputHistogram::MeasurementType       MeasurementType;
  typedef typename TInputHistogram::AbsoluteFrequencyType FrequencyType;

  typedef typename NumericTraits< MeasurementType >::RealType MeanType;
  typedef typename NumericTraits< MeasurementType >::RealType VarianceType;
  typedef typename NumericTraits< MeasurementType >::RealType WeightType;

  typedef std::vector< MeanType >      MeanVectorType;
  typedef std::vector< FrequencyType > FrequencyVectorType;
  typedef std::vector< WeightType >    WeightVectorType;

  typedef typename TInputHistogram::InstanceIdentifier InstanceIdentifierType;
  typedef std::vector< InstanceIdentifierType >        InstanceIdentifierVectorType;

  /**Standard Macros */
  itkTypeMacro(OtsuMultipleThresholdsCalculator, HistogramAlgorithmsBase);
  itkNewMacro(Self);

  /** Typedef for the thresholds output */
  typedef std::vector< MeasurementType > OutputType;

  /** Returns the thresholds vector */
  const OutputType & GetOutput();

  /** Set/Get the number of thresholds. */
  itkSetClampMacro( NumberOfThresholds, SizeValueType, 1, NumericTraits< SizeValueType >::max() );
  itkGetConstMacro(NumberOfThresholds, SizeValueType);

  /** Calculates Otsu's thresholds and saves them. */
  void Compute() ITK_OVERRIDE;

  /** Set/Get the use of valley emphasis. Default is false. */
  itkSetMacro(ValleyEmphasis, bool);
  itkGetConstReferenceMacro(ValleyEmphasis, bool);
  itkBooleanMacro(ValleyEmphasis);

protected:
  OtsuMultipleThresholdsCalculator();
  virtual ~OtsuMultipleThresholdsCalculator() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Increment the thresholds of one position along the histogram. */
  bool IncrementThresholds(InstanceIdentifierVectorType & thresholdIds,
                           MeanType totalMean,
                           MeanVectorType & classMean,
                           FrequencyVectorType & classFrequency);

private:

  SizeValueType m_NumberOfThresholds;
  OutputType    m_Output;
  bool          m_ValleyEmphasis;
};
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOtsuMultipleThresholdsCalculator.hxx"
#endif

#endif
