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
#ifndef itkZeroCrossingImageFilter_h
#define itkZeroCrossingImageFilter_h

#include "itkImageToImageFilter.h"
namespace itk
{
/**
 *\class ZeroCrossingImageFilter
 * \brief This filter finds the closest pixel to the zero-crossings
 * (sign changes) in a signed itk::Image.
 *
 * Pixels closest to zero-crossings are labeled with a foreground value.
 * All other pixels are marked with a background value. The algorithm works
 * by detecting differences in sign among neighbors using city-block style
 * connectivity (4-neighbors in 2d, 6-neighbors in 3d, etc.).
 *
 *  \par Inputs and Outputs
 *  The input to this filter is an itk::Image of arbitrary dimension.  The
 *  algorithm assumes a signed data type (zero-crossings are not defined for
 *  unsigned data types), and requires that operator>, operator<, operator==,
 *  and operator!= are defined.
 *
 *  \par
 *  The output of the filter is a binary, labeled image of user-specified type.
 *  By default, zero-crossing pixels are labeled with a default "foreground"
 *  value of itk::NumericTraits<OutputDataType>::OneValue(), where OutputDataType is
 *  the data type of the output image.  All other pixels are labeled with a
 *  default "background" value of itk::NumericTraits<OutputDataType>::ZeroValue().
 *
 *  \par Parameters
 *  There are two parameters for this filter.  ForegroundValue is the value
 *  that marks zero-crossing pixels.  The BackgroundValue is the value given to
 *  all other pixels.
 *
 *  \sa Image
 *  \sa Neighborhood
 *  \sa NeighborhoodOperator
 *  \sa NeighborhoodIterator
 *  \ingroup ImageFeatureExtraction
 * \ingroup ITKImageFeature
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageFeature/FindZeroCrossings,Find Zero Crossings In Signed Image}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT ZeroCrossingImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ZeroCrossingImageFilter);

  /** Standard "Self" & Superclass type alias. */
  using Self = ZeroCrossingImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;

  /** Image type alias support   */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;

  /** SmartPointer type alias support  */
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Define pixel types  */
  using InputImagePixelType = typename TInputImage::PixelType;
  using OutputImagePixelType = typename TOutputImage::PixelType;

  /** Method for creation through the object factory.  */
  itkNewMacro(Self);

  /** Typedef to describe the output image region type. */
  using OutputImageRegionType = typename TOutputImage::RegionType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ZeroCrossingImageFilter, ImageToImageFilter);

  /** ImageDimension enumeration   */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** ZeroCrossingImageFilter needs a larger input requested
   * region than the output requested region (larger by the kernel
   * size to do comparisons between the central pixel and ite neighbors).
   * Thus ZeroCrossingImageFilter needs to provide an implementation
   * for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion()   */
  void
  GenerateInputRequestedRegion() override;

  /** Set/Get the label value for zero-crossing pixels. */
  itkSetMacro(ForegroundValue, OutputImagePixelType);
  itkGetConstMacro(ForegroundValue, OutputImagePixelType);

  /** Set/Get the label value for non-zero-crossing pixels. */
  itkSetMacro(BackgroundValue, OutputImagePixelType);
  itkGetConstMacro(BackgroundValue, OutputImagePixelType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputEqualityComparableCheck, (Concept::EqualityComparable<OutputImagePixelType>));
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  itkConceptMacro(InputComparableCheck, (Concept::Comparable<InputImagePixelType>));
  itkConceptMacro(OutputOStreamWritableCheck, (Concept::OStreamWritable<OutputImagePixelType>));
  // End concept checking
#endif

protected:
  ZeroCrossingImageFilter();
  ~ZeroCrossingImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  OutputImagePixelType m_BackgroundValue;
  OutputImagePixelType m_ForegroundValue;

  /**
   * ZeroCrossingImageFilter can be implemented as a multithreaded filter.
   * Therefore,this implementation provides a DynamicThreadedGenerateData() routine which
   * is called for each processing thread. The output image data is allocated
   * automatically by the superclass prior to calling DynamicThreadedGenerateData().
   * DynamicThreadedGenerateData can only write to the portion of the output image
   * specified by the parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()
   */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;
};
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkZeroCrossingImageFilter.hxx"
#endif

#endif
