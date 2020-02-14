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
#ifndef itkDerivativeImageFilter_h
#define itkDerivativeImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/**
 *\class DerivativeImageFilter
 * \brief Computes the directional derivative of an image.
 * The directional derivative at each pixel location is computed by convolution
 * with a derivative operator of user-specified order.
 *
 * SetOrder specifies the order of the derivative.
 *
 * SetDirection specifies the direction of the derivative with respect to the
 * coordinate axes of the image.
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 *
 * \ingroup ImageFeatureExtraction
 * \ingroup ITKImageFeature
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageFeature/DerivativeImage,Apply A Filter Only To A Specified Region Of An Image}
 * \sphinxexample{Filtering/ImageFeature/RequesterRegion, Apply A Filter Only To A Specified Region Of An Image}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT DerivativeImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(DerivativeImageFilter);

  /** Standard class type aliases. */
  using Self = DerivativeImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  using OutputPixelType = typename TOutputImage::PixelType;
  using OutputInternalPixelType = typename TOutputImage::InternalPixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using InputInternalPixelType = typename TInputImage::InternalPixelType;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** Image type alias support */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DerivativeImageFilter, ImageToImageFilter);

  /** The output pixel type must be signed. */
#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SignedOutputPixelType, (Concept::Signed<OutputPixelType>));
  // End concept checking
#endif

  /** Standard get/set macros for filter parameters. */
  itkSetMacro(Order, unsigned int);
  itkGetConstMacro(Order, unsigned int);
  itkSetMacro(Direction, unsigned int);
  itkGetConstMacro(Direction, unsigned int);

  /** Use the image spacing information in calculations. Use this option if you
   *  want derivatives in physical space. Default is UseImageSpacingOn. */
  void
  SetUseImageSpacingOn()
  {
    this->SetUseImageSpacing(true);
  }

  /** Ignore the image spacing. Use this option if you want derivatives in
      isotropic pixel space.  Default is UseImageSpacingOn. */
  void
  SetUseImageSpacingOff()
  {
    this->SetUseImageSpacing(false);
  }

  /** Set/Get whether or not the filter will use the spacing of the input
      image in its calculations */
  itkSetMacro(UseImageSpacing, bool);
  itkGetConstMacro(UseImageSpacing, bool);

  /** DerivativeImageFilter needs a larger input requested region than
   * the output requested region (larger in the direction of the
   * derivative).  As such, DerivativeImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to
   * inform the pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

protected:
  DerivativeImageFilter()
  {
    m_Order = 1;
    m_Direction = 0;
    m_UseImageSpacing = true;
  }

  ~DerivativeImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Standard pipeline method. While this class does not implement a
   * ThreadedGenerateData(), its GenerateData() delegates all
   * calculations to an NeighborhoodOperatorImageFilter.  Since the
   * NeighborhoodOperatorImageFilter is multithreaded, this filter is
   * multithreaded by default. */
  void
  GenerateData() override;

private:
  /** The order of the derivative. */
  unsigned int m_Order;

  /** The direction of the derivative. */
  unsigned int m_Direction;

  bool m_UseImageSpacing;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkDerivativeImageFilter.hxx"
#endif

#endif
