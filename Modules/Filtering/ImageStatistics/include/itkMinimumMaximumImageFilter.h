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
#ifndef itkMinimumMaximumImageFilter_h
#define itkMinimumMaximumImageFilter_h

#include "itkImageSink.h"
#include "itkSimpleDataObjectDecorator.h"
#include <mutex>

#include <vector>

#include "itkNumericTraits.h"

namespace itk
{
/** \class MinimumMaximumImageFilter
 * \brief Computes the minimum and the maximum intensity values of
 * an image.
 *
 * It is templated over input image type only.
 *
 * This filter is automatically multi-threaded and can stream its
 * input when NumberOfStreamDivisions is set to more than
 * 1. The extrema are independently computed for each streamed and
 * threaded region then merged.
 *
 *
 * \ingroup Operators
 * \sa StatisticsImageFilter
 * \ingroup ITKImageStatistics
 */
template <typename TInputImage>
class ITK_TEMPLATE_EXPORT MinimumMaximumImageFilter : public ImageSink<TInputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MinimumMaximumImageFilter);

  /** Extract dimension from input image. */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;

  /** Standard class type aliases. */
  using Self = MinimumMaximumImageFilter;
  using Superclass = ImageSink<TInputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Image related type alias. */
  using InputImagePointer = typename TInputImage::Pointer;

  using RegionType = typename TInputImage::RegionType;
  using SizeType = typename TInputImage::SizeType;
  using IndexType = typename TInputImage::IndexType;
  using PixelType = typename TInputImage::PixelType;

  /** Smart Pointer type to a DataObject. */
  using DataObjectPointer = typename DataObject::Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MinimumMaximumImageFilter, ImageToImageFilter);

  /** Image type alias support */
  using InputImageType = TInputImage;

  /** Type of DataObjects used for scalar outputs */
  using PixelObjectType = SimpleDataObjectDecorator<PixelType>;

  /** Return the computed Minimum. */
  itkGetDecoratedOutputMacro(Minimum, PixelType);

  /** Return the computed Maximum. */
  itkGetDecoratedOutputMacro(Maximum, PixelType);

  /** Make a DataObject of the correct type to be used as the specified
   * output. */
  using DataObjectIdentifierType = ProcessObject::DataObjectIdentifierType;
  using Superclass::MakeOutput;
  DataObjectPointer
  MakeOutput(const DataObjectIdentifierType & name) override;


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

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(LessThanComparableCheck, (Concept::LessThanComparable<PixelType>));
  itkConceptMacro(GreaterThanComparableCheck, (Concept::GreaterThanComparable<PixelType>));
  itkConceptMacro(OStreamWritableCheck, (Concept::OStreamWritable<PixelType>));
  // End concept checking
#endif

protected:
  MinimumMaximumImageFilter();
  ~MinimumMaximumImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Initialize some accumulators before any chunks are processes */
  void
  BeforeStreamedGenerateData() override;

  /** Do final mean and variance computation from data accumulated in threads.
   */
  void
  AfterStreamedGenerateData() override;

  void
  ThreadedStreamedGenerateData(const RegionType &) override;


  itkSetDecoratedOutputMacro(Minimum, PixelType);
  itkSetDecoratedOutputMacro(Maximum, PixelType);

private:
  PixelType m_ThreadMin;
  PixelType m_ThreadMax;

  std::mutex m_Mutex;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMinimumMaximumImageFilter.hxx"
#endif

#endif
