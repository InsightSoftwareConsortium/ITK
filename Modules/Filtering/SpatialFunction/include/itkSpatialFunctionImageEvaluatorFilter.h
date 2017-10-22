/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
template< typename TSpatialFunction, typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT SpatialFunctionImageEvaluatorFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef SpatialFunctionImageEvaluatorFilter             Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpatialFunctionImageEvaluatorFilter, ImageToImageFilter);

  /** Number of dimensions. */
  itkStaticConstMacro(NDimensions, unsigned int,
                      TInputImage::ImageDimension);

  /** Image size typedef. */
  typedef typename TInputImage::SizeType SizeType;

  /** Image index typedef. */
  typedef typename TOutputImage::IndexType IndexType;

  /** Image pixel value typedef. */
  typedef typename TOutputImage::PixelType PixelType;

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Type of function. */
  typedef TSpatialFunction FunctionType;

  /** Return type of function. */
  typedef typename FunctionType::OutputType FunctionValueType;

  /** Typedef describing vector info. */
  typedef typename FunctionType::InputType PositionType;

  /** Set the internal spatial function. */
  void SetFunction(FunctionType *PixelFunction)
  { m_PixelFunction = PixelFunction; }

protected:
  SpatialFunctionImageEvaluatorFilter();
  virtual ~SpatialFunctionImageEvaluatorFilter() ITK_OVERRIDE {}

  /** Method for evaluating the implicit function over the image. */
  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SpatialFunctionImageEvaluatorFilter);

  /** The function that will be evaluated over the image */
  FunctionType *m_PixelFunction;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialFunctionImageEvaluatorFilter.hxx"
#endif

#endif
