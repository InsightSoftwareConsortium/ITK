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
#ifndef itkMinimumMaximumImageFilter_h
#define itkMinimumMaximumImageFilter_h

#include "itkImageToImageFilter.h"
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
 * This filter just copies the input image through this output to
 * be included within the pipeline. The implementation uses the
 * StatisticsImageFilter.
 *
 * \ingroup Operators
 * \sa StatisticsImageFilter
 * \ingroup ITKImageStatistics
 */
template< typename TInputImage >
class ITK_TEMPLATE_EXPORT MinimumMaximumImageFilter:
  public ImageToImageFilter< TInputImage, TInputImage >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MinimumMaximumImageFilter);

  /** Extract dimension from input image. */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TInputImage::ImageDimension;

  /** Standard class type aliases. */
  using Self = MinimumMaximumImageFilter;
  using Superclass = ImageToImageFilter< TInputImage, TInputImage >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

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
  using PixelObjectType = SimpleDataObjectDecorator< PixelType >;

  /** Return the computed Minimum. */
  PixelType GetMinimum() const
  { return this->GetMinimumOutput()->Get(); }
  PixelObjectType * GetMinimumOutput();

  const PixelObjectType * GetMinimumOutput() const;

  /** Return the computed Maximum. */
  PixelType GetMaximum() const
  { return this->GetMaximumOutput()->Get(); }
  PixelObjectType * GetMaximumOutput();

  const PixelObjectType * GetMaximumOutput() const;

  /** Make a DataObject of the correct type to be used as the specified
   * output. */
  using DataObjectPointerArraySizeType = ProcessObject::DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  DataObjectPointer MakeOutput(DataObjectPointerArraySizeType idx) override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( LessThanComparableCheck,
                   ( Concept::LessThanComparable< PixelType > ) );
  itkConceptMacro( GreaterThanComparableCheck,
                   ( Concept::GreaterThanComparable< PixelType > ) );
  itkConceptMacro( OStreamWritableCheck,
                   ( Concept::OStreamWritable< PixelType > ) );
  // End concept checking
#endif

protected:
  MinimumMaximumImageFilter();
  ~MinimumMaximumImageFilter() override = default;
  void PrintSelf(std::ostream & os, Indent indent) const override;

  /** Pass the input through unmodified. Do this by Grafting in the
    AllocateOutputs method. */
  void AllocateOutputs() override;

  /** Initialize some accumulators before the threads run. */
  void BeforeThreadedGenerateData() override;

  /** Do final mean and variance computation from data accumulated in threads.
    */
  void AfterThreadedGenerateData() override;

  void DynamicThreadedGenerateData( const RegionType & ) override;

  // Override since the filter needs all the data for the algorithm
  void GenerateInputRequestedRegion() override;

  // Override since the filter produces all of its output
  void EnlargeOutputRequestedRegion(DataObject *data) override;

private:
  PixelType m_ThreadMin;
  PixelType m_ThreadMax;

  std::mutex m_Mutex;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMinimumMaximumImageFilter.hxx"
#endif

#endif
