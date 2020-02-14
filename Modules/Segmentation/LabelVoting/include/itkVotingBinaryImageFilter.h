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
#ifndef itkVotingBinaryImageFilter_h
#define itkVotingBinaryImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/**
 *\class VotingBinaryImageFilter
 * \brief Applies a voting operation in a neighborhood of each pixel.
 *
 * \note Pixels which are not Foreground or Background will remain unchanged.
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 *
 * \ingroup IntensityImageFilters
 * \ingroup ITKLabelVoting
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT VotingBinaryImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VotingBinaryImageFilter);

  /** Extract dimension from input and output image. */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Convenient type alias for simplifying declarations. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;

  /** Standard class type aliases. */
  using Self = VotingBinaryImageFilter;
  using Superclass = ImageToImageFilter<InputImageType, OutputImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VotingBinaryImageFilter, ImageToImageFilter);

  /** Image type alias support */
  using InputPixelType = typename InputImageType::PixelType;
  using OutputPixelType = typename OutputImageType::PixelType;

  using InputImageRegionType = typename InputImageType::RegionType;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  using InputSizeType = typename InputImageType::SizeType;

  /** Set the radius of the neighborhood used to compute the median. */
  itkSetMacro(Radius, InputSizeType);

  /** Get the radius of the neighborhood used to compute the median */
  itkGetConstReferenceMacro(Radius, InputSizeType);

  /** Set the value associated with the Foreground (or the object) on
      the binary input image and the Background . */
  itkSetMacro(BackgroundValue, InputPixelType);
  itkSetMacro(ForegroundValue, InputPixelType);

  /** Get the value associated with the Foreground (or the object) on the
      binary input image and the Background . */
  itkGetConstReferenceMacro(BackgroundValue, InputPixelType);
  itkGetConstReferenceMacro(ForegroundValue, InputPixelType);

  /** Birth threshold. Pixels that are OFF will turn ON when the number of
   * neighbors ON is larger than the value defined in this threshold. */
  itkGetConstReferenceMacro(BirthThreshold, unsigned int);
  itkSetMacro(BirthThreshold, unsigned int);

  /** Survival threshold. Pixels that are ON will turn OFF when the number of
   * neighbors ON is smaller than the value defined in this survival threshold. */
  itkGetConstReferenceMacro(SurvivalThreshold, unsigned int);
  itkSetMacro(SurvivalThreshold, unsigned int);

  /** VotingBinaryImageFilter needs a larger input requested region than
   * the output requested region.  As such, VotingBinaryImageFilter needs
   * to provide an implementation for GenerateInputRequestedRegion()
   * in order to inform the pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputEqualityComparableCheck, (Concept::EqualityComparable<InputPixelType>));
  itkConceptMacro(IntConvertibleToInputCheck, (Concept::Convertible<int, InputPixelType>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputPixelType, OutputPixelType>));
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<InputImageDimension, OutputImageDimension>));
  itkConceptMacro(InputOStreamWritableCheck, (Concept::OStreamWritable<InputPixelType>));
  // End concept checking
#endif

protected:
  VotingBinaryImageFilter();
  ~VotingBinaryImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** VotingBinaryImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a DynamicThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling DynamicThreadedGenerateData().  DynamicThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

private:
  InputSizeType m_Radius;

  InputPixelType m_ForegroundValue;
  InputPixelType m_BackgroundValue;

  unsigned int m_BirthThreshold;
  unsigned int m_SurvivalThreshold;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVotingBinaryImageFilter.hxx"
#endif

#endif
