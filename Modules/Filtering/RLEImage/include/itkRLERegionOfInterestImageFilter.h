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
#ifndef itkRLERegionOfInterestImageFilter_h
#define itkRLERegionOfInterestImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkRLEImage.h"
#include "itkRegionOfInterestImageFilter.h"
#include "itkSmartPointer.h"

namespace itk
{
/** \class RegionOfInterestImageFilter
 * \brief Extract a region of interest from the input image or convert
 *  between itk::Image and RLEImage (a custom region can be used).
 *
 *  This filter produces an output image of the same dimension as the input
 *  image. The user specifies the region of the input image that will be
 *  contained in the output image. The origin coordinates of the output images
 *  will be computed in such a way that if mapped to physical space, the output
 *  image will overlay the input image with perfect registration. In other
 *  words, a registration process between the output image and the input image
 *  will return an identity transform.
 *
 *  The region to extract is set using the method SetRegionOfInterest.
 *
 * Specialized for RLEImage.
 *
 *  \ingroup RLEImage
 *  \ingroup ITKCommon
 */
template <typename TPixel, unsigned int VImageDimension, typename CounterType>
class RegionOfInterestImageFilter<RLEImage<TPixel, VImageDimension, CounterType>,
                                  RLEImage<TPixel, VImageDimension, CounterType>>
  : public ImageToImageFilter<RLEImage<TPixel, VImageDimension, CounterType>,
                              RLEImage<TPixel, VImageDimension, CounterType>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(RegionOfInterestImageFilter);

  /** Standard class type alias. */
  using Self = RegionOfInterestImageFilter;
  using RLEImageType = RLEImage<TPixel, VImageDimension, CounterType>;
  using ImageType = RLEImageType;
  using Superclass = ImageToImageFilter<RLEImageType, RLEImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using InputImageRegionType = typename Superclass::InputImageRegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegionOfInterestImageFilter, ImageToImageFilter);

  /** Typedef to describe the input image region types. */
  using RegionType = typename RLEImageType::RegionType;
  using IndexType = typename RLEImageType::IndexType;
  using SizeType = typename RLEImageType::SizeType;

  /** Typedef to describe the type of pixel. */
  using OutputImagePixelType = typename RLEImageType::PixelType;
  using InputImagePixelType = typename RLEImageType::PixelType;

  /** Set/Get the output image region. */
  itkSetMacro(RegionOfInterest, RegionType);
  itkGetConstMacro(RegionOfInterest, RegionType);

  /** ImageDimension enumeration */
  static constexpr unsigned int ImageDimension = VImageDimension;
  static constexpr unsigned int OutputImageDimension = VImageDimension;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputImagePixelType, OutputImagePixelType>));
// End concept checking
#endif

protected:
  RegionOfInterestImageFilter() { this->DynamicMultiThreadingOn(); }
  ~RegionOfInterestImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateInputRequestedRegion() override;

  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  /** RegionOfInterestImageFilter can produce an image which is a different
   * size than its input image.  As such, RegionOfInterestImageFilter
   * needs to provide an implementation for
   * GenerateOutputInformation() in order to inform the pipeline
   * execution model.  The original documentation of this method is
   * below.
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
   * parameter "outputRegionForThread"
   * \sa ImageToImageFilter::DynamicThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void
  DynamicThreadedGenerateData(const RegionType & outputRegionForThread) override;

private:
  RegionType m_RegionOfInterest;
};

template <typename TPixelIn,
          typename TPixelOut,
          unsigned int VImageDimension,
          typename CounterTypeIn,
          typename CounterTypeOut>
class RegionOfInterestImageFilter<RLEImage<TPixelIn, VImageDimension, CounterTypeIn>,
                                  RLEImage<TPixelOut, VImageDimension, CounterTypeOut>>
  : public ImageToImageFilter<RLEImage<TPixelIn, VImageDimension, CounterTypeIn>,
                              RLEImage<TPixelOut, VImageDimension, CounterTypeOut>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(RegionOfInterestImageFilter);

  /** Standard class type alias. */
  using Self = RegionOfInterestImageFilter;
  using RLEImageTypeIn = RLEImage<TPixelIn, VImageDimension, CounterTypeIn>;
  using RLEImageTypeOut = RLEImage<TPixelOut, VImageDimension, CounterTypeOut>;
  using ImageType = RLEImageTypeOut;
  using Superclass = ImageToImageFilter<RLEImageTypeIn, RLEImageTypeOut>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using InputImageRegionType = typename Superclass::InputImageRegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegionOfInterestImageFilter, ImageToImageFilter);

  /** Typedef to describe the input image region types. */
  using RegionType = typename RLEImageTypeIn::RegionType;
  using IndexType = typename RLEImageTypeIn::IndexType;
  using SizeType = typename RLEImageTypeIn::SizeType;

  /** Typedef to describe the type of pixel. */
  using OutputImagePixelType = typename RLEImageTypeOut::PixelType;
  using InputImagePixelType = typename RLEImageTypeIn::PixelType;

  /** Set/Get the output image region. */
  itkSetMacro(RegionOfInterest, RegionType);
  itkGetConstMacro(RegionOfInterest, RegionType);

  /** ImageDimension enumeration */
  static constexpr unsigned int ImageDimension = VImageDimension;
  static constexpr unsigned int OutputImageDimension = VImageDimension;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputImagePixelType, OutputImagePixelType>));
// End concept checking
#endif

protected:
  RegionOfInterestImageFilter() { this->DynamicMultiThreadingOn(); }
  ~RegionOfInterestImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateInputRequestedRegion() override;

  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  /** RegionOfInterestImageFilter can produce an image which is a different
   * size than its input image.  As such, RegionOfInterestImageFilter
   * needs to provide an implementation for
   * GenerateOutputInformation() in order to inform the pipeline
   * execution model.  The original documentation of this method is
   * below.
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
   * parameter "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void
  DynamicThreadedGenerateData(const RegionType & outputRegionForThread) override;

private:
  RegionType m_RegionOfInterest;
};

// not implemented on purpose, so it will produce a meaningful error message
template <unsigned int VImageDimensionIn, unsigned int VImageDimensionOut>
class InputAndOutputImagesMustHaveSameDimension;

// input and output images must have the same dimension (e.g. both 2D or both 3D)
// so disallow this by inheriting from unimplemented base class
template <typename TPixelIn,
          typename TPixelOut,
          unsigned int VImageDimensionIn,
          unsigned int VImageDimensionOut,
          typename CounterTypeIn,
          typename CounterTypeOut>
class RegionOfInterestImageFilter<RLEImage<TPixelIn, VImageDimensionIn, CounterTypeIn>,
                                  RLEImage<TPixelOut, VImageDimensionOut, CounterTypeOut>>
  : InputAndOutputImagesMustHaveSameDimension<VImageDimensionIn, VImageDimensionOut>
{};

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
class RegionOfInterestImageFilter<Image<TPixel, VImageDimension>, RLEImage<TPixel, VImageDimension, CounterType>>
  : public ImageToImageFilter<Image<TPixel, VImageDimension>, RLEImage<TPixel, VImageDimension, CounterType>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(RegionOfInterestImageFilter);

  /** Standard class type alias. */
  using RLEImageType = RLEImage<TPixel, VImageDimension, CounterType>;

  using Self = RegionOfInterestImageFilter;
  using ImageType = Image<TPixel, VImageDimension>;
  using Superclass = ImageToImageFilter<ImageType, RLEImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using InputImageRegionType = typename Superclass::InputImageRegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegionOfInterestImageFilter, ImageToImageFilter);

  /** Typedef to describe the input image region types. */
  using RegionType = typename RLEImageType::RegionType;
  using IndexType = typename RLEImageType::IndexType;
  using SizeType = typename RLEImageType::SizeType;

  /** Typedef to describe the type of pixel. */
  using OutputImagePixelType = typename RLEImageType::PixelType;
  using InputImagePixelType = typename RLEImageType::PixelType;

  /** Set/Get the output image region. */
  itkSetMacro(RegionOfInterest, RegionType);
  itkGetConstMacro(RegionOfInterest, RegionType);

  /** ImageDimension enumeration */
  static constexpr unsigned int ImageDimension = VImageDimension;
  static constexpr unsigned int OutputImageDimension = VImageDimension;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputImagePixelType, OutputImagePixelType>));
// End concept checking
#endif

protected:
  RegionOfInterestImageFilter() { this->DynamicMultiThreadingOn(); }
  ~RegionOfInterestImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateInputRequestedRegion() override;

  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  /** RegionOfInterestImageFilter can produce an image which is a different
   * size than its input image.  As such, RegionOfInterestImageFilter
   * needs to provide an implementation for
   * GenerateOutputInformation() in order to inform the pipeline
   * execution model.  The original documentation of this method is
   * below.
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
   * parameter "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void
  DynamicThreadedGenerateData(const RegionType & outputRegionForThread) override;

private:
  RegionType m_RegionOfInterest;
};

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
class RegionOfInterestImageFilter<RLEImage<TPixel, VImageDimension, CounterType>, Image<TPixel, VImageDimension>>
  : public ImageToImageFilter<RLEImage<TPixel, VImageDimension, CounterType>, Image<TPixel, VImageDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(RegionOfInterestImageFilter);

  /** Standard class type alias. */
  using RLEImageType = RLEImage<TPixel, VImageDimension, CounterType>;

  using Self = RegionOfInterestImageFilter;
  using ImageType = Image<TPixel, VImageDimension>;
  using Superclass = ImageToImageFilter<RLEImageType, ImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using InputImageRegionType = typename Superclass::InputImageRegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegionOfInterestImageFilter, ImageToImageFilter);

  /** Typedef to describe the input image region types. */
  using RegionType = typename RLEImageType::RegionType;
  using IndexType = typename RLEImageType::IndexType;
  using SizeType = typename RLEImageType::SizeType;

  /** Typedef to describe the type of pixel. */
  using OutputImagePixelType = typename RLEImageType::PixelType;
  using InputImagePixelType = typename RLEImageType::PixelType;

  /** Set/Get the output image region. */
  itkSetMacro(RegionOfInterest, RegionType);
  itkGetConstMacro(RegionOfInterest, RegionType);

  /** ImageDimension enumeration */
  static constexpr unsigned int ImageDimension = VImageDimension;
  static constexpr unsigned int OutputImageDimension = VImageDimension;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputImagePixelType, OutputImagePixelType>));
// End concept checking
#endif

protected:
  RegionOfInterestImageFilter() { this->DynamicMultiThreadingOn(); }
  ~RegionOfInterestImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateInputRequestedRegion() override;

  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  /** RegionOfInterestImageFilter can produce an image which is a different
   * size than its input image.  As such, RegionOfInterestImageFilter
   * needs to provide an implementation for
   * GenerateOutputInformation() in order to inform the pipeline
   * execution model.  The original documentation of this method is
   * below.
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
   * parameter "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void
  DynamicThreadedGenerateData(const RegionType & outputRegionForThread) override;

private:
  RegionType m_RegionOfInterest;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRLERegionOfInterestImageFilter.hxx"
#endif

#endif // itkRLERegionOfInterestImageFilter_h
