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
#ifndef itkLaplacianImageFilter_h
#define itkLaplacianImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/**
 * \class LaplacianImageFilter
 * \brief This filter computes the Laplacian of a scalar-valued image.
 *
 * The Laplacian
 * is an isotropic measure of the 2nd spatial derivative of an image. The
 * Laplacian of an image highlights regions of rapid intensity change and is
 * therefore often used for edge detection.  Often, the Laplacian is applied to
 * an image that has first been smoothed with a Gaussian filter in order to
 * reduce its sensitivity to noise.
 *
 * \par
 * The Laplacian at each pixel location is computed by convolution with the
 * itk::LaplacianOperator.
 *
 * \par Inputs and Outputs
 * The input to this filter is a scalar-valued itk::Image of arbitrary
 * dimension. The output is a scalar-valued itk::Image.
 *
 * \warning The pixel type of the input and output images must be of real type
 * (float or double). ConceptChecking is used here to enforce the input pixel
 * type. You will get a compilation error if the pixel type of the input and
 * output images is not float or double.
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 * \sa LaplacianOperator
 *
 * \ingroup ImageFeatureExtraction
 * \ingroup ITKImageFeature
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageFeature/ComputeLaplacian,Compute Laplacian}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT LaplacianImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LaplacianImageFilter);

  /** Standard "Self" & Superclass type alias.   */
  using Self = LaplacianImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  using OutputPixelType = typename TOutputImage::PixelType;
  using OutputInternalPixelType = typename TOutputImage::InternalPixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using InputInternalPixelType = typename TInputImage::InternalPixelType;
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** Image type alias support */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using InputImagePointer = typename InputImageType::Pointer;

  /** Smart pointer type alias support   */
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods)  */
  itkTypeMacro(LaplacianImageFilter, ImageToImageFilter);

  /** Method for creation through the object factory.  */
  itkNewMacro(Self);

  /** LaplacianImageFilter needs a larger input requested region than
   * the output requested region (larger in the direction of the
   * derivative).  As such, LaplacianImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to
   * inform the pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion()  */
  void
  GenerateInputRequestedRegion() override;

  /** Enable/Disable using the image spacing information in
   *  calculations. Use this option if you  want derivatives in
   *  physical space. Default  is UseImageSpacingOn. */
  itkBooleanMacro(UseImageSpacing);

  /** Set/Get whether or not the filter will use the spacing of the input
      image in its calculations */
  itkSetMacro(UseImageSpacing, bool);
  itkGetConstMacro(UseImageSpacing, bool);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<InputImageDimension, ImageDimension>));
  itkConceptMacro(InputPixelTypeIsFloatingPointCheck, (Concept::IsFloatingPoint<InputPixelType>));
  itkConceptMacro(OutputPixelTypeIsFloatingPointCheck, (Concept::IsFloatingPoint<OutputPixelType>));
  // End concept checking
#endif

protected:
  LaplacianImageFilter() { m_UseImageSpacing = true; }

  ~LaplacianImageFilter() override = default;

  /** Standard pipeline method. While this class does not implement a
   * ThreadedGenerateData(), its GenerateData() delegates all
   * calculations to an NeighborhoodOperatorImageFilter.  Since the
   * NeighborhoodOperatorImageFilter is multithreaded, this filter is
   * multithreaded by default.   */
  void
  GenerateData() override;

  void
  PrintSelf(std::ostream &, Indent) const override;

private:
  bool m_UseImageSpacing;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLaplacianImageFilter.hxx"
#endif

#endif
