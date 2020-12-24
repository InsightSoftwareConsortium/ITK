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
#ifndef itkImageToHistogramFilter_h
#define itkImageToHistogramFilter_h

#include <mutex>

#include "itkHistogram.h"
#include "itkImageSink.h"
#include "itkSimpleDataObjectDecorator.h"
#include "itkProgressReporter.h"

namespace itk
{
namespace Statistics
{
/**
 *\class ImageToHistogramFilter
 *  \brief This class generates a histogram from an image.
 *
 *  The concept of Histogram in ITK is quite generic. It has been designed to
 *  manage multiple components data. This class facilitates the computation of
 *  an histogram from an image.
 *
 * This filter is automatically multi-threaded. When
 * AutoMinimumMaximum is off and the NumberOfStreamDivisions is set to more than
 * one, then this filter streams its input in a series of requested
 * regions. A histogram is computed for each streamed and threaded
 * region then merged.
 *
 * \ingroup ITKStatistics
 */

template <typename TImage>
class ITK_TEMPLATE_EXPORT ImageToHistogramFilter : public ImageSink<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImageToHistogramFilter);

  /** Standard type alias */
  using Self = ImageToHistogramFilter;
  using Superclass = ImageSink<TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToHistogramFilter, ImageSink);

  /** standard New() method support */
  itkNewMacro(Self);

  using ImageType = TImage;
  using PixelType = typename ImageType::PixelType;
  using RegionType = typename ImageType::RegionType;
  using ValueType = typename NumericTraits<PixelType>::ValueType;
  using ValueRealType = typename NumericTraits<ValueType>::RealType;

  using HistogramType = Histogram<ValueRealType>;
  using HistogramPointer = typename HistogramType::Pointer;
  using HistogramConstPointer = typename HistogramType::ConstPointer;
  using HistogramSizeType = typename HistogramType::SizeType;
  using HistogramMeasurementType = typename HistogramType::MeasurementType;
  using HistogramMeasurementVectorType = typename HistogramType::MeasurementVectorType;

public:
  /** Return the output histogram. */
  const HistogramType *
  GetOutput() const;
  HistogramType *
  GetOutput();

  /** Type of DataObjects to use for Size inputs */
  using InputHistogramSizeObjectType = SimpleDataObjectDecorator<HistogramSizeType>;

  /** Type of DataObjects to use for Marginal Scale inputs */
  using InputHistogramMeasurementObjectType = SimpleDataObjectDecorator<HistogramMeasurementType>;

  /** Type of DataObjects to use for Minimum and Maximums values of the
   * histogram bins. */
  using InputHistogramMeasurementVectorObjectType = SimpleDataObjectDecorator<HistogramMeasurementVectorType>;

  /** Type of DataObjects to use for AutoMinimumMaximum input */
  using InputBooleanObjectType = SimpleDataObjectDecorator<bool>;

  /** Methods for setting and getting the histogram size.  The histogram size
   * is encapsulated inside a decorator class. For this reason, it is possible
   * to set and get the decorator class, but it is only possible to set the
   * histogram size by value. This macro declares the methods
   * SetHistogramSize(), SetHistogramSizeInput(), GetHistogramSizeInput().
   */
  itkSetGetDecoratedInputMacro(HistogramSize, HistogramSizeType);

  /** Methods for setting and getting the Marginal scale value.  The marginal
   * scale is used when the type of the measurement vector components are of
   * integer type. */
  itkSetGetDecoratedInputMacro(MarginalScale, HistogramMeasurementType);

  /** Methods for setting and getting the Minimum and Maximum values of the
   * histogram bins. */
  itkSetGetDecoratedInputMacro(HistogramBinMinimum, HistogramMeasurementVectorType);
  itkSetGetDecoratedInputMacro(HistogramBinMaximum, HistogramMeasurementVectorType);

  /** Methods for setting and getting the boolean flag that defines whether the
   * minimum and maximum of the histogram are going to be computed
   * automatically from the values of the sample */
  itkSetGetDecoratedInputMacro(AutoMinimumMaximum, bool);
  itkBooleanMacro(AutoMinimumMaximum);

  /** Method that facilitates the use of this filter in the internal
   * pipeline of another filter. */
  virtual void
  GraftOutput(DataObject * graft);


  // Change the access from protected to public to expose streaming option, a using statement can not be used due to
  // limitations of wrapping.
  void
  SetNumberOfStreamDivisions(const unsigned int n) override
  {
    Superclass::SetNumberOfStreamDivisions(n);
  }
  unsigned int
  GetNumberOfStreamDivisions() const override
  {
    return Superclass::GetNumberOfStreamDivisions();
  }

protected:
  ImageToHistogramFilter();
  ~ImageToHistogramFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  StreamedGenerateData(unsigned int inputRequestedRegionNumber) override;

  void
  InitializeOutputHistogram();
  void
  AfterStreamedGenerateData() override;

  /** Method that construct the outputs */
  using DataObjectPointerArraySizeType = ProcessObject::DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  DataObject::Pointer MakeOutput(DataObjectPointerArraySizeType) override;

  /* Override method to disable streaming when the minimum and
   * maximum need to be computed. */
  unsigned int
  GetNumberOfInputRequestedRegions() override;

  void
  ThreadedStreamedGenerateData(const RegionType &) override;
  virtual void
  ThreadedComputeMinimumAndMaximum(const RegionType & inputRegionForThread);


  virtual void
  ThreadedMergeHistogram(HistogramPointer && histogram);

  std::mutex m_Mutex;

  HistogramPointer m_MergeHistogram;

  HistogramMeasurementVectorType m_Minimum;
  HistogramMeasurementVectorType m_Maximum;

private:
  void
  ApplyMarginalScale(HistogramMeasurementVectorType & min,
                     HistogramMeasurementVectorType & max,
                     HistogramSizeType &              size);
};
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageToHistogramFilter.hxx"
#endif

#endif
