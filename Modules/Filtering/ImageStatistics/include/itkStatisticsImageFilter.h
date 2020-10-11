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
#ifndef itkStatisticsImageFilter_h
#define itkStatisticsImageFilter_h

#include "itkImageSink.h"
#include "itkNumericTraits.h"
#include "itkArray.h"
#include "itkSimpleDataObjectDecorator.h"
#include <mutex>
#include "itkCompensatedSummation.h"

namespace itk
{
/** \class StatisticsImageFilter
 * \brief Compute min, max, variance and mean of an Image.
 *
 * StatisticsImageFilter computes the minimum, maximum, sum, sum of squares, mean, variance
 * sigma of an image.  The filter needs all of its input image.  It
 * behaves as a filter with an input and output. Thus it can be inserted
 * in a pipline with other filters and the statistics will only be
 * recomputed if a downstream filter changes.
 *
 * This filter is automatically multi-threaded and can stream its
 * input when NumberOfStreamDivisions is set to more than
 * one. Statistics are independently computed for each streamed and
 * threaded region then merged.
 *
 * Internally a compensated summation algorithm is used for the
 * accumulation of intensities to improve accuracy for large images.
 *
 * \ingroup MathematicalStatisticsImageFilters
 * \ingroup ITKImageStatistics
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageStatistics/ComputeMinMaxVarianceMeanOfImage,Compute Min, Max, Variance And Mean Of
 * Image} \endsphinx
 */
template <typename TInputImage>
class ITK_TEMPLATE_EXPORT StatisticsImageFilter : public ImageSink<TInputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(StatisticsImageFilter);

  /** Standard Self type alias */
  using Self = StatisticsImageFilter;
  using Superclass = ImageSink<TInputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(StatisticsImageFilter, ImageToImageFilter);

  /** Image related type alias. */
  using InputImagePointer = typename TInputImage::Pointer;

  using RegionType = typename TInputImage::RegionType;
  using SizeType = typename TInputImage::SizeType;
  using IndexType = typename TInputImage::IndexType;
  using PixelType = typename TInputImage::PixelType;

  /** Image related type alias. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Type to use for computations. */
  using RealType = typename NumericTraits<PixelType>::RealType;

  /** Smart Pointer type to a DataObject. */
  using DataObjectPointer = typename DataObject::Pointer;

  /** Type of DataObjects used for scalar outputs */
  using RealObjectType = SimpleDataObjectDecorator<RealType>;
  using PixelObjectType = SimpleDataObjectDecorator<PixelType>;

  /** Return the computed Minimum. */
  itkGetDecoratedOutputMacro(Minimum, PixelType);

  /** Return the computed Maximum. */
  itkGetDecoratedOutputMacro(Maximum, PixelType);

  /** Return the computed Mean. */
  itkGetDecoratedOutputMacro(Mean, RealType);

  /** Return the computed Standard Deviation. */
  itkGetDecoratedOutputMacro(Sigma, RealType);

  /** Return the computed Variance. */
  itkGetDecoratedOutputMacro(Variance, RealType);

  /** Return the compute Sum. */
  itkGetDecoratedOutputMacro(Sum, RealType);

  /** Return the compute Sum of Squares. */
  itkGetDecoratedOutputMacro(SumOfSquares, RealType);

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

  /** Make a DataObject of the correct type to be used as the specified
   * output. */
  using DataObjectIdentifierType = ProcessObject::DataObjectIdentifierType;
  using Superclass::MakeOutput;
  DataObjectPointer
  MakeOutput(const DataObjectIdentifierType & name) override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<PixelType>));
  // End concept checking
#endif

protected:
  StatisticsImageFilter();
  ~StatisticsImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Initialize some accumulators before the threads run. */
  void
  BeforeStreamedGenerateData() override;

  /** Set outputs to computed values from all regions
   */
  void
  AfterStreamedGenerateData() override;

  void
  ThreadedStreamedGenerateData(const RegionType &) override;

  itkSetDecoratedOutputMacro(Minimum, PixelType);
  itkSetDecoratedOutputMacro(Maximum, PixelType);
  itkSetDecoratedOutputMacro(Mean, RealType);
  itkSetDecoratedOutputMacro(Sigma, RealType);
  itkSetDecoratedOutputMacro(Variance, RealType);
  itkSetDecoratedOutputMacro(Sum, RealType);
  itkSetDecoratedOutputMacro(SumOfSquares, RealType);

private:
  CompensatedSummation<RealType> m_ThreadSum{ 1 };
  CompensatedSummation<RealType> m_SumOfSquares{ 1 };

  SizeValueType m_Count{ 1 };
  PixelType     m_ThreadMin{ 1 };
  PixelType     m_ThreadMax{ 1 };

  std::mutex m_Mutex;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkStatisticsImageFilter.hxx"
#endif

#endif
