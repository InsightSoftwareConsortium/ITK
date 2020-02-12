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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkShrinkImageFilter_h
#define itkShrinkImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/**
 *\class ShrinkImageFilter
 * \brief Reduce the size of an image by an integer factor in each
 * dimension.
 *
 * ShrinkImageFilter reduces the size of an image by an integer factor
 * in each dimension. The algorithm implemented is a simple subsample.
 * The output image size in each dimension is given by:
 *
 * outputSize[j] = max( std::floor(inputSize[j]/shrinkFactor[j]), 1 );
 *
 * NOTE: The physical centers of the input and output will be the
 * same. Because of this, the Origin of the output may not be the same
 * as the Origin of the input.
 * Since this filter produces an image which is a different
 * resolution, origin and with different pixel spacing than its input
 * image, it needs to override several of the methods defined
 * in ProcessObject in order to properly manage the pipeline execution model.
 * In particular, this filter overrides
 * ProcessObject::GenerateInputRequestedRegion() and
 * ProcessObject::GenerateOutputInformation().
 *
 * This filter is implemented as a multithreaded filter.  It provides a
 * DynamicThreadedGenerateData() method for its implementation.
 *
 * \ingroup GeometricTransform Streamed
 * \ingroup ITKImageGrid
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageGrid/ShrinkImage,Shrink Image}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT ShrinkImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ShrinkImageFilter);

  /** Standard class type aliases. */
  using Self = ShrinkImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ShrinkImageFilter, ImageToImageFilter);

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

  using ShrinkFactorsType = FixedArray<unsigned int, ImageDimension>;

  /** Set the shrink factors. Values are clamped to
   * a minimum value of 1. Default is 1 for all dimensions. */
  itkSetMacro(ShrinkFactors, ShrinkFactorsType);
  void
  SetShrinkFactors(unsigned int factor);
  void
  SetShrinkFactor(unsigned int i, unsigned int factor);

  /** Get the shrink factors. */
  itkGetConstReferenceMacro(ShrinkFactors, ShrinkFactorsType);

  /** ShrinkImageFilter produces an image which is a different
   * resolution and with a different pixel spacing than its input
   * image.  As such, ShrinkImageFilter needs to provide an
   * implementation for GenerateOutputInformation() in order to inform
   * the pipeline execution model.  The original documentation of this
   * method is below.
   * \sa ProcessObject::GenerateOutputInformaton() */
  void
  GenerateOutputInformation() override;

  /** ShrinkImageFilter needs a larger input requested region than the output
   * requested region.  As such, ShrinkImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputConvertibleToOutputCheck,
                  (Concept::Convertible<typename TInputImage::PixelType, typename TOutputImage::PixelType>));
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  // End concept checking
#endif

protected:
  ShrinkImageFilter();
  ~ShrinkImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** ShrinkImageFilter can be implemented as a multithreaded filter.
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


private:
  ShrinkFactorsType m_ShrinkFactors;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkShrinkImageFilter.hxx"
#endif

#endif
