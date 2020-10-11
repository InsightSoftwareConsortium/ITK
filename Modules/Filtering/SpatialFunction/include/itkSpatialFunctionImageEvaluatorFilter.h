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
#ifndef itkSpatialFunctionImageEvaluatorFilter_h
#define itkSpatialFunctionImageEvaluatorFilter_h

#include "itkImageFunction.h"
#include "itkImageRegionIterator.h"
#include "itkImageToImageFilter.h"
#include "itkSize.h"
#include "itkSpatialFunction.h"

namespace itk
{
/** \class SpatialFunctionImageEvaluatorFilter
 * \brief Evaluates a SpatialFunction onto a source image
 *
 * SpatialFunctionImageEvaluatorFilter walks an input image and evaluates
 * the function at every pixel location. The output of the spatial function
 * and the pixel type of the output image must be compatible.
 *
 * Like its parent ImageToImageFilter, this class functions in the filtering
 * pipeline and produces a unique output image.
 *
 * \ingroup ImageFilters
 * \ingroup ITKSpatialFunction
 */
template <typename TSpatialFunction, typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT SpatialFunctionImageEvaluatorFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SpatialFunctionImageEvaluatorFilter);

  /** Standard class type aliases. */
  using Self = SpatialFunctionImageEvaluatorFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpatialFunctionImageEvaluatorFilter, ImageToImageFilter);

  /** Number of dimensions. */
  static constexpr unsigned int NDimensions = TInputImage::ImageDimension;

  /** Image size type alias. */
  using SizeType = typename TInputImage::SizeType;

  /** Image index type alias. */
  using IndexType = typename TOutputImage::IndexType;

  /** Image pixel value type alias. */
  using PixelType = typename TOutputImage::PixelType;

  /** Typedef to describe the output image region type. */
  using OutputImageRegionType = typename TOutputImage::RegionType;

  /** Type of function. */
  using FunctionType = TSpatialFunction;

  /** Return type of function. */
  using FunctionValueType = typename FunctionType::OutputType;

  /** Typedef describing vector info. */
  using PositionType = typename FunctionType::InputType;

  /** Set the internal spatial function. */
  void
  SetFunction(FunctionType * PixelFunction)
  {
    m_PixelFunction = PixelFunction;
  }

protected:
  SpatialFunctionImageEvaluatorFilter();
  ~SpatialFunctionImageEvaluatorFilter() override = default;

  /** Method for evaluating the implicit function over the image. */
  void
  GenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** The function that will be evaluated over the image */
  FunctionType * m_PixelFunction;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSpatialFunctionImageEvaluatorFilter.hxx"
#endif

#endif
