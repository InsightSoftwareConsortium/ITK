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
#ifndef itkVotingBinaryHoleFillingImageFilter_h
#define itkVotingBinaryHoleFillingImageFilter_h

#include "itkVotingBinaryImageFilter.h"
#include "itkArray.h"

namespace itk
{
/**
 *\class VotingBinaryHoleFillingImageFilter
 * \brief Fills in holes and cavities by applying a voting operation on each pixel.
 *
 *
 * \sa Image
 * \sa VotingBinaryImageFilter
 * \sa VotingBinaryIterativeHoleFillingImageFilter
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 *
 * \ingroup IntensityImageFilters
 * \ingroup ITKLabelVoting
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT VotingBinaryHoleFillingImageFilter : public VotingBinaryImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VotingBinaryHoleFillingImageFilter);

  /** Extract dimension from input and output image. */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Convenient type alias for simplifying declarations. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;

  /** Standard class type aliases. */
  using Self = VotingBinaryHoleFillingImageFilter;
  using Superclass = VotingBinaryImageFilter<InputImageType, OutputImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VotingBinaryHoleFillingImageFilter, VotingBinaryImageFilter);

  /** Image type alias support */
  using InputPixelType = typename InputImageType::PixelType;
  using OutputPixelType = typename OutputImageType::PixelType;

  using InputImageRegionType = typename InputImageType::RegionType;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  using InputSizeType = typename InputImageType::SizeType;
  using SizeValueType = typename InputImageType::SizeValueType;

  /** Majority threshold. It is the number of pixels over 50% that will decide
   * whether an OFF pixel will become ON or not. For example, if the
   * neighborhood of a pixel has 124 pixels (excluding itself), the 50% will be
   * 62, and if you set upd a Majority threshold of 5, that means that the
   * filter will require 67 or more neighbor pixels to be ON in order to switch
   * the current OFF pixel to ON. The default value is 1. */
  itkGetConstReferenceMacro(MajorityThreshold, unsigned int);
  itkSetMacro(MajorityThreshold, unsigned int);

  /** Returns the number of pixels that changed when the filter was executed. */
  itkGetConstReferenceMacro(NumberOfPixelsChanged, SizeValueType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(IntConvertibleToInputCheck, (Concept::Convertible<int, InputPixelType>));
  itkConceptMacro(UnsignedIntConvertibleToInputCheck, (Concept::Convertible<unsigned int, InputPixelType>));
  // End concept checking
#endif

protected:
  VotingBinaryHoleFillingImageFilter();
  ~VotingBinaryHoleFillingImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Make protected the methods SetBirthThreshold() and
   * SetSurvivalThreshold() so users of this filter do not have access to
   * them. */
  void
  SetBirthThreshold(const unsigned int value) override
  {
    this->Superclass::SetBirthThreshold(value);
  }
  void
  SetSurvivalThreshold(const unsigned int value) override
  {
    this->Superclass::SetSurvivalThreshold(value);
  }

  /** VotingBinaryHoleFillingImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData().  ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) override;

  void
  DynamicThreadedGenerateData(const OutputImageRegionType &) override
  {
    itkExceptionMacro("This class requires threadId so it must use classic multi-threading model");
  }

  /** Methods to be called before and after the invokation of
   * ThreadedGenerateData(). */
  void
  BeforeThreadedGenerateData() override;

  void
  AfterThreadedGenerateData() override;

private:
  unsigned int m_MajorityThreshold;

  SizeValueType m_NumberOfPixelsChanged;

  // Auxiliary array for multi-threading
  Array<SizeValueType> m_Count;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVotingBinaryHoleFillingImageFilter.hxx"
#endif

#endif
