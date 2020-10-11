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
#ifndef itkSobelEdgeDetectionImageFilter_h
#define itkSobelEdgeDetectionImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"

namespace itk
{
/**
 * \class SobelEdgeDetectionImageFilter
 * \brief A 2D or 3D edge detection using the Sobel operator.
 *
 * This filter uses the Sobel operator to calculate the image gradient and then
 * finds the magnitude of this gradient vector.  The Sobel gradient magnitude
 * (square-root sum of squares) is an indication of edge strength.
 *
 * \sa ImageToImageFilter
 * \sa SobelOperator
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 *
 * \ingroup ImageFeatureExtraction
 *
 * \ingroup ITKImageFeature
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageFeature/SobelEdgeDetectionImageFilter,Sobel Edge Detection Image Filter}
 * \endsphinx
 */

template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT SobelEdgeDetectionImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SobelEdgeDetectionImageFilter);

  /**
   * Standard "Self" & Superclass type alias.
   */
  using Self = SobelEdgeDetectionImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;

  /**
   * Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.
   */
  using OutputPixelType = typename TOutputImage::PixelType;
  using OutputInternalPixelType = typename TOutputImage::InternalPixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using InputInternalPixelType = typename TInputImage::InternalPixelType;
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;

  /**
   * Image type alias support
   */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using InputImagePointer = typename InputImageType::Pointer;

  /**
   * Smart pointer type alias support
   */
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(SobelEdgeDetectionImageFilter, ImageToImageFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * SobelEdgeDetectionImageFilter needs a larger input requested region than
   * the output requested region (larger in the direction of the
   * derivative).  As such, SobelEdgeDetectionImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to
   * inform the pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion()
   */
  void
  GenerateInputRequestedRegion() override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<InputImageDimension, ImageDimension>));
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<OutputPixelType>));
  itkConceptMacro(OutputPixelIsFloatingPointCheck, (Concept::IsFloatingPoint<OutputPixelType>));
  // End concept checking
#endif

protected:
  SobelEdgeDetectionImageFilter() = default;
  ~SobelEdgeDetectionImageFilter() override = default;

  /**
   * Standard pipeline method. While this class does not implement a
   * ThreadedGenerateData(), its GenerateData() delegates all
   * calculations to an NeighborhoodOperatorImageFilter.  Since the
   * NeighborhoodOperatorImageFilter is multithreaded, this filter is
   * multithreaded by default.
   */
  void
  GenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
  }
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSobelEdgeDetectionImageFilter.hxx"
#endif

#endif
