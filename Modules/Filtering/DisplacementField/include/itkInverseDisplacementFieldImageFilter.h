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
#ifndef itkInverseDisplacementFieldImageFilter_h
#define itkInverseDisplacementFieldImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkKernelTransform.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{
/** \class InverseDisplacementFieldImageFilter
 * \brief Computes the inverse of a displacement field.
 *
 * InverseDisplacementFieldImageFilter takes a displacement field as input and
 * computes the displacement field that is its inverse. If the input displacement
 * field was mapping coordinates from a space A into a space B, the output of
 * this filter will map coordinates from the space B into the space A.
 *
 * Given that both the input and output displacement field are represented as
 * discrete images with pixel type vector, the inverse will be only an
 * estimation and will probably not correspond to a perfect inverse.  The
 * precision of the inverse can be improved at the price of increasing the
 * computation time and memory consumption in this filter.
 *
 * The method used for computing the inverse displacement field is to subsample
 * the input field using a regular grid and create Kerned-Base Spline in which
 * the reference landmarks are the coordinates of the deformed point and the
 * target landmarks are the negative of the displacement vectors. The
 * kernel-base spline is then used for regularly sampling the output space and
 * recover vector values for every single pixel.
 *
 * The subsampling factor used for the regular grid of the input field will
 * determine the number of landmarks in the KernelBased spline and therefore it
 * will have a dramatic effect on both the precision of output displacement
 * field and the computational time required for the filter to complete the
 * estimation. A large subsampling factor will result in few landmarks in the
 * KernelBased spline, therefore on fast computation and low precision.  A
 * small subsampling factor will result in a large number of landmarks in the
 * KernelBased spline, therefore a large memory consumption, long computation
 * time and high precision for the inverse estimation.
 *
 * This filter expects both the input and output images to be of pixel type
 * Vector.
 *
 * \ingroup ImageToImageFilter
 * \ingroup ITKDisplacementField
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT InverseDisplacementFieldImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(InverseDisplacementFieldImageFilter);

  /** Standard class type aliases. */
  using Self = InverseDisplacementFieldImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(InverseDisplacementFieldImageFilter, ImageToImageFilter);

  /** Number of dimensions. */
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** Transform type alias.
   *
   * \todo Check that input and output images have the same number of
   * dimensions; this is required for consistency.  */
  using KernelTransformType = KernelTransform<double, Self::ImageDimension>;
  using KernelTransformPointerType = typename KernelTransformType::Pointer;

  /** Image size type alias. */
  using SizeType = typename OutputImageType::SizeType;

  /** Image index type alias. */
  using IndexType = typename OutputImageType::IndexType;

  /** Image pixel value type alias. */
  using OutputPixelType = typename TOutputImage::PixelType;
  using OutputPixelComponentType = typename OutputPixelType::ValueType;

  /** Typedef to describe the output image region type. */
  using OutputImageRegionType = typename TOutputImage::RegionType;

  /** Image spacing type alias */
  using SpacingType = typename TOutputImage::SpacingType;
  using OriginPointType = typename TOutputImage::PointType;

  /** Get/Set the coordinate transformation.
   * Set the KernelBase spline used for resampling the displacement grid.
   * */
  itkSetObjectMacro(KernelTransform, KernelTransformType);
  itkGetModifiableObjectMacro(KernelTransform, KernelTransformType);

  /** Set the size of the output image. */
  itkSetMacro(Size, SizeType);

  /** Get the size of the output image. */
  itkGetConstReferenceMacro(Size, SizeType);

  /** Set the output image spacing. */
  itkSetMacro(OutputSpacing, SpacingType);
  virtual void
  SetOutputSpacing(const double * values);

  /** Get the output image spacing. */
  itkGetConstReferenceMacro(OutputSpacing, SpacingType);

  /** Set the output image origin. */
  itkSetMacro(OutputOrigin, OriginPointType);
  virtual void
  SetOutputOrigin(const double * values);

  /** Get the output image origin. */
  itkGetConstReferenceMacro(OutputOrigin, OriginPointType);

  /** Set/Get the factor used for subsampling the input displacement field.  A
   * large value in this factor will produce a fast computation of the inverse
   * field but with low precision. A small value of this factor will produce a
   * precise computation of the inverse field at the price of large memory
   * consumption and long computational time. */
  itkSetMacro(SubsamplingFactor, unsigned int);
  itkGetConstMacro(SubsamplingFactor, unsigned int);

  /** InverseDisplacementFieldImageFilter produces an image which is a different size
   * than its input.  As such, it needs to provide an implementation
   * for GenerateOutputInformation() in order to inform the pipeline
   * execution model.  The original documentation of this method is
   * below. \sa ProcessObject::GenerateOutputInformaton() */
  void
  GenerateOutputInformation() override;

  /** InverseDisplacementFieldImageFilter needs a different input requested region than
   * the output requested region.  As such, InverseDisplacementFieldImageFilter needs
   * to provide an implementation for GenerateInputRequestedRegion()
   * in order to inform the pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

  /** Method Compute the Modified Time based on changed to the components. */
  ModifiedTimeType
  GetMTime() const override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<OutputPixelComponentType>));
  // End concept checking
#endif

protected:
  InverseDisplacementFieldImageFilter();
  ~InverseDisplacementFieldImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /**
   * GenerateData() computes the internal KernelBase spline and resamples
   * the displacement field.
   */
  void
  GenerateData() override;

  /** Subsample the input displacement field and generate the
   *  landmarks for the kernel base spline
   */
  void
  PrepareKernelBaseSpline();

private:
  SizeType                   m_Size;            // Size of the output image
  KernelTransformPointerType m_KernelTransform; // Coordinate transform to
                                                // use
  SpacingType     m_OutputSpacing;              // output image spacing
  OriginPointType m_OutputOrigin;               // output image origin

  unsigned int m_SubsamplingFactor; // factor to subsample the
                                    // input field.
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkInverseDisplacementFieldImageFilter.hxx"
#endif

#endif
