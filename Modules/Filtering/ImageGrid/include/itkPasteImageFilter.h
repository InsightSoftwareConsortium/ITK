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
#ifndef itkPasteImageFilter_h
#define itkPasteImageFilter_h

#include "itkInPlaceImageFilter.h"
#include "itkSmartPointer.h"
#include "itkSimpleDataObjectDecorator.h"
#include <numeric>

namespace itk
{


/** \class PasteImageFilter
 * \brief Paste an image (or a constant value) into another image.
 *
 * PasteImageFilter allows a region in a destination image to be filled with a source image or a constant pixel value.
 * The SetDestinationIndex() method prescribes where in the destination input to start pasting data from the source
 * input.  The SetSourceRegion method prescribes the section of the second image to paste into the first. When a
 * constant pixel value is set, the SourceRegion describes the size of the region filled. If the output requested
 * region does not include the SourceRegion after it has been repositioned to DestinationIndex, then the output will
 * just be a copy of the input.
 *
 * This filter supports running "InPlace" to efficiently reuse the destination image buffer for the output, removing
 * the need to copy the destination pixels to the output.
 *
 * When the source image has a lower dimension than the destination image then the DestinationSkipAxes parameter
 * specifies which axes in the destination image are set to 1 when copying the region or filling with a constant.
 *
 * The two inputs and output image will have the same pixel type.
 *
 * \ingroup GeometricTransform
 * \ingroup ITKImageGrid
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageGrid/PasteImageIntoAnotherOne,Paste Image Into Another One}
 * \sphinxexample{Filtering/ImageGrid/RunImageFilterOnRegionOfImage,Run Image Filter On Region Of Image}
 * \endsphinx
 */
template <typename TInputImage, typename TSourceImage = TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT PasteImageFilter : public InPlaceImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PasteImageFilter);

  /** Standard class type aliases. */
  using Self = PasteImageFilter;
  using Superclass = InPlaceImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PasteImageFilter, InPlaceImageFilter);

  /** Typedefs from Superclass */
  using InputImagePointer = typename Superclass::InputImagePointer;
  using OutputImagePointer = typename Superclass::OutputImagePointer;

  /** Typedef to describe the output and input image region types. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using SourceImageType = TSourceImage;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using InputImageRegionType = typename InputImageType::RegionType;
  using SourceImageRegionType = typename SourceImageType::RegionType;

  using SourceImagePointer = typename SourceImageType::Pointer;
  using SourceImageConstPointer = typename SourceImageType::ConstPointer;

  /** Typedef to describe the type of pixel. */
  using OutputImagePixelType = typename OutputImageType::PixelType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using SourceImagePixelType = typename SourceImageType::PixelType;

  /** Typedef to describe the output and input image index and size types. */
  using OutputImageIndexType = typename OutputImageType::IndexType;
  using OutputImageSizeType = typename OutputImageType::SizeType;
  using InputImageIndexType = typename InputImageType::IndexType;
  using InputImageSizeType = typename InputImageType::SizeType;
  using SourceImageIndexType = typename SourceImageType::IndexType;
  using SourceImageSizeType = typename SourceImageType::SizeType;
  using DecoratedSourceImagePixelType = SimpleDataObjectDecorator<SourceImagePixelType>;

  using InputSkipAxesArrayType = FixedArray<bool, InputImageType::ImageDimension>;


  /** ImageDimension enumeration */
  static constexpr unsigned int InputImageDimension = InputImageType::ImageDimension;
  static constexpr unsigned int OutputImageDimension = OutputImageType::ImageDimension;
  static constexpr unsigned int SourceImageDimension = SourceImageType::ImageDimension;

  /** Set/Get the destination index (where in the first input the second
   * input will be pasted. */
  itkSetMacro(DestinationIndex, InputImageIndexType);
  itkGetConstMacro(DestinationIndex, InputImageIndexType);

  /** Set/Get the array describing which axes in the destination image to skip
   *
   * The axes with true values are set to 1, to fill the difference between the dimension of the input and source image.
   * The number of true values in DestinationSkipAxes plus the DestinationImageDimension must equal the
   * InputImageDimension.
   *
   * By default this array contains SourceImageDimension false values followed by true values for the remainder.
   */
  itkSetMacro(DestinationSkipAxes, InputSkipAxesArrayType);
  itkGetConstMacro(DestinationSkipAxes, InputSkipAxesArrayType);


  /** Set/Get the source region (what part of the second input will be
   * pasted. */
  itkSetMacro(SourceRegion, SourceImageRegionType);
  itkGetConstMacro(SourceRegion, SourceImageRegionType);

  /** Set/Get the "destination" image.  This is the image that will be
   * obscured by the paste operation. */
  itkSetInputMacro(DestinationImage, InputImageType);
  itkGetInputMacro(DestinationImage, InputImageType);

  /** Set/Get the "source" image.  This is the image that will be
   * pasted over the destination image. */
  itkSetInputMacro(SourceImage, SourceImageType);
  itkGetInputMacro(SourceImage, SourceImageType);

  /** Set/Get a constant value to fill the destination region.
   *
   * This input is an alternative input to the SourceImage.
   */
  itkSetDecoratedInputMacro(Constant, SourceImagePixelType);
  itkGetDecoratedInputMacro(Constant, SourceImagePixelType);


  /** PasteImageFilter needs to set the input requested regions for its
   * inputs.  The first input's requested region will be set to match
   * the output requested region.  The second input's requested region
   * will be set to the value of the m_SourceRegion ivar.  Note that
   * if the output requested region is a portion of the image that
   * is outside the DestinationIndex + size of the source region,
   * then the first input is copied to the output.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;


  /** Override VerifyInputInformation() since this filter's inputs do
   * not need to occupy the same physical space.
   *
   * \sa ProcessObject::VerifyInputInformation
   */
  void
  VerifyInputInformation() ITKv5_CONST override
  {}

  void
  VerifyPreconditions() ITKv5_CONST override;

  bool
  CanRunInPlace() const override;

protected:
  PasteImageFilter();
  ~PasteImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  InputImageSizeType
  GetPresumedDestinationSize() const;


  /** NPasteImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a DynamicThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling DynamicThreadedGenerateData(). DynamicThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


  SourceImageRegionType m_SourceRegion;

  InputImageIndexType    m_DestinationIndex;
  InputSkipAxesArrayType m_DestinationSkipAxes;

  static_assert(InputImageDimension >= SourceImageDimension,
                "The source image dimension is greater than the input image.");
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPasteImageFilter.hxx"
#endif

#endif
