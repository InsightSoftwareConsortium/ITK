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
#ifndef itkSliceImageFilter_h
#define itkSliceImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class SliceImageFilter
 * \brief Slices an image based on a starting index and a stopping
 * index, and a step size.
 *
 * This class is designed to facilitate the implementation of extended
 * sliced based indexing into images.
 *
 * The input and output image must be of the same dimension.
 *
 * The input parameters are a starting and stopping index as well as a
 * stepping size. The starting index indicates the first pixels to be
 * used and for each dimension the index is incremented by the step
 * until the index is equal to or "beyond" the stopping index. If the
 * step is negative then the image will be reversed in the dimension,
 * and the stopping index is expected to be less then the starting
 * index. If the stopping index is already beyond the starting index
 * then an image of size zero will be returned.
 *
 * The output image's starting index is always zero. The origin is the
 * physical location of the starting index. The output directions
 * cosine matrix is that of the input but with sign changes matching
 * that of the step's sign.
 *
 * \note In certain combinations such as with start=1, and step>1 while
 * the physical location of the center of the pixel remains the same,
 * the extent (edge to edge space) of the output image will be beyond the
 * extent of the original image.
 *
 * \ingroup ITKImageGrid
 */
template <class TInputImage, class TOutputImage>
class ITK_TEMPLATE_EXPORT SliceImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SliceImageFilter);

  /** Standard class type aliases. */
  using Self = SliceImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SliceImageFilter, ImageToImageFilter);

  /** Typedef to images */
  using OutputImageType = TOutputImage;
  using InputImageType = TInputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;

  using OutputIndexType = typename TOutputImage::IndexType;
  using InputIndexType = typename TInputImage::IndexType;
  using OutputOffsetType = typename TOutputImage::OffsetType;

  /** Typedef to describe the output image region type. */
  using OutputImageRegionType = typename TOutputImage::RegionType;

  /** ImageDimension enumeration. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;


  using IndexType = typename InputImageType::IndexType;
  using IndexValueType = typename InputIndexType::IndexValueType;
  using ArrayType = FixedArray<int, ImageDimension>;

  /** Set/Get the first index extracted from the input image */
  itkSetMacro(Start, IndexType);
  itkGetConstReferenceMacro(Start, IndexType);
  void
  SetStart(IndexValueType start);

  /** Set/Get the excluded end of the range */
  itkSetMacro(Stop, IndexType);
  itkGetConstReferenceMacro(Stop, IndexType);
  void
  SetStop(IndexValueType stop);

  /** Set/Get the stride of indexes extracted
   *
   * An exception will be generated if 0.
   */
  itkSetMacro(Step, ArrayType);
  itkGetConstReferenceMacro(Step, ArrayType);
  void
  SetStep(int step);


#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputConvertibleToOutputCheck,
                  (Concept::Convertible<typename TInputImage::PixelType, typename TOutputImage::PixelType>));
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  /** End concept checking */
#endif

protected:
  SliceImageFilter();
  ~SliceImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** SliceImageFilter produces an image which is a different
   * resolution and with a different pixel spacing than its input
   * image.
   * \sa ProcessObject::GenerateOutputInformaton() */
  void
  GenerateOutputInformation() override;

  void
  GenerateInputRequestedRegion() override;

  /** SliceImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a DynamicThreadedGenerateData() routine
   * which is called for each processing thread. The output image data is
   * allocated automatically by the superclass prior to calling
   * DynamicThreadedGenerateData().  DynamicThreadedGenerateData can only write to the
   * portion of the output image specified by the parameter
   * "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


  void
  VerifyInputInformation() ITKv5_CONST override;

private:
  IndexType m_Start;
  IndexType m_Stop;
  ArrayType m_Step;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSliceImageFilter.hxx"
#endif

#endif
