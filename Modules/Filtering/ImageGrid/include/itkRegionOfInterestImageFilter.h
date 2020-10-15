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
#ifndef itkRegionOfInterestImageFilter_h
#define itkRegionOfInterestImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkSmartPointer.h"

namespace itk
{
/**
 *\class RegionOfInterestImageFilter
 * \brief Extract a region of interest from the input image.
 *
 *  This filter produces an output image of the same dimension as the input
 *  image. The user specifies the region of the input image that will be
 *  contained in the output image. The origin coordinates of the output images
 *  will be computed in such a way that if mapped to physical space, the output
 *  image will overlay the input image with perfect registration. In other
 *  words, a registration process between the output image and the input image
 *  will return an identity transform.
 *
 *  If you are interested in changing the dimension of the image, you may want
 *  to consider the ExtractImageFilter. For example for extracting a 2D image
 *  from a slice of a 3D image.
 *
 *  The region to extract is set using the method SetRegionOfInterest.
 *
 * \sa ExtractImageFilter
 *
 * \ingroup GeometricTransform
 * \ingroup ITKImageGrid
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageGrid/ExtractRegionOfInterestInOneImage,Extract Region Of Interest In One Image}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT RegionOfInterestImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(RegionOfInterestImageFilter);

  /** Standard class type aliases. */
  using Self = RegionOfInterestImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using InputImageRegionType = typename Superclass::InputImageRegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegionOfInterestImageFilter, ImageToImageFilter);

  /** Typedef to describe the input image region types. */
  using RegionType = typename TInputImage::RegionType;
  using IndexType = typename TInputImage::IndexType;
  using SizeType = typename TInputImage::SizeType;

  /** Typedef to describe the type of pixel. */
  using OutputImagePixelType = typename TOutputImage::PixelType;
  using InputImagePixelType = typename TInputImage::PixelType;

  /** Set/Get the output image region. */
  itkSetMacro(RegionOfInterest, RegionType);
  itkGetConstMacro(RegionOfInterest, RegionType);

  /** ImageDimension enumeration */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputImagePixelType, OutputImagePixelType>));
  // End concept checking
#endif

protected:
  RegionOfInterestImageFilter();
  ~RegionOfInterestImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateInputRequestedRegion() override;

  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  /** RegionOfInterestImageFilter can produce an image which is a different
   * size than its input image. As such, RegionOfInterestImageFilter
   * needs to provide an implementation for
   * GenerateOutputInformation() in order to inform the pipeline
   * execution model.
   *
   * \sa ProcessObject::GenerateOutputInformaton()  */
  void
  GenerateOutputInformation() override;

  /** RegionOfInterestImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a DynamicThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling DynamicThreadedGenerateData(). DynamicThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread".
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void
  DynamicThreadedGenerateData(const RegionType & outputRegionForThread) override;

private:
  RegionType m_RegionOfInterest;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRegionOfInterestImageFilter.hxx"
#endif

#endif
