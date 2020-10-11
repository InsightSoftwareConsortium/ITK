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
#ifndef itkScalarImageToHistogramGenerator_h
#define itkScalarImageToHistogramGenerator_h

#include "itkImageToListSampleAdaptor.h"
#include "itkSampleToHistogramFilter.h"
#include "itkHistogram.h"
#include "itkObject.h"

namespace itk
{
namespace Statistics
{
/**
 *\class ScalarImageToHistogramGenerator
 *
 * \brief TODO
 * \ingroup ITKStatistics
 */
template <typename TImageType>
class ITK_TEMPLATE_EXPORT ScalarImageToHistogramGenerator : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ScalarImageToHistogramGenerator);

  /** Standard type alias */
  using Self = ScalarImageToHistogramGenerator;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScalarImageToHistogramGenerator, Object);

  /** standard New() method support */
  itkNewMacro(Self);

  using ImageType = TImageType;
  using AdaptorType = itk::Statistics::ImageToListSampleAdaptor<ImageType>;
  using AdaptorPointer = typename AdaptorType::Pointer;
  using PixelType = typename ImageType::PixelType;
  using RealPixelType = typename NumericTraits<PixelType>::RealType;

  using HistogramType = itk::Statistics::Histogram<double>;
  using GeneratorType = itk::Statistics::SampleToHistogramFilter<AdaptorType, HistogramType>;

  using GeneratorPointer = typename GeneratorType::Pointer;

  using HistogramPointer = typename HistogramType::Pointer;
  using HistogramConstPointer = typename HistogramType::ConstPointer;

public:
  /** Triggers the Computation of the histogram */
  void
  Compute();

  /** Connects the input image for which the histogram is going to be computed
   */
  void
  SetInput(const ImageType *);

  /** Return the histogram. o
   \warning This output is only valid after the Compute() method has been invoked
   \sa Compute */
  const HistogramType *
  GetOutput() const;

  /** Set number of histogram bins */
  void
  SetNumberOfBins(unsigned int numberOfBins);

  /** Set marginal scale value to be passed to the histogram generator */
  void
  SetMarginalScale(double marginalScale);

  /** Set the minimum value from which the bins will be computed.
   \warning This requires to set the automatic computation of the histogram minimum/maximum to Off.
   \sa SetAutoHistogramMinimumMaximum */
  void
  SetHistogramMin(RealPixelType minimumValue);

  /** Set the maximum value from which the bins will be computed.
   \warning This requires to set the automatic computation of the histogram minimum/maximum to Off.
   \sa SetAutoHistogramMinimumMaximum */
  void
  SetHistogramMax(RealPixelType maximumValue);

  /** Computes the histogram minimum and maximum bin values automatically.
   \warning If set to Off, the values must be manually specified with SetHistogramMin() and SetHistogramMax().
   \sa SetHistogramMin SetHistogramMax */
  void
  SetAutoHistogramMinimumMaximum(bool autoOnOff);

protected:
  ScalarImageToHistogramGenerator();
  ~ScalarImageToHistogramGenerator() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  AdaptorPointer m_ImageToListSampleAdaptor;

  GeneratorPointer m_HistogramGenerator;
};
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkScalarImageToHistogramGenerator.hxx"
#endif

#endif
