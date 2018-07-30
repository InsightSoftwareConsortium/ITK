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
#ifndef itkBSplineGradientImageFilter_h
#define itkBSplineGradientImageFilter_h

#include "itkBSplineInterpolateImageFunction.h"
#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include "itkCovariantVector.h"

namespace itk
{
/** \class BSplineGradientImageFilter
 *
 * \brief Calculate image gradient from an interpolating B-spline fit.
 *
 * \ingroup GradientFilters
 * \ingroup BSplineGradient
 *
 * \sa ApproximationBSplineGradientImageFilter
 */
template <typename TInputImage,
          typename TOutputValueType = float,
          typename TCoordRep = double,
          typename TCoefficientType = double>
class ITK_TEMPLATE_EXPORT BSplineGradientImageFilter
  : public ImageToImageFilter<
      TInputImage,
      Image<CovariantVector<TOutputValueType, TInputImage::ImageDimension>, TInputImage::ImageDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineGradientImageFilter);

  /** Extract dimension from input image. */
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

  /** Standard class typedefs. */
  typedef BSplineGradientImageFilter Self;

  /** Convenient typedefs for simplifying declarations. */
  typedef TInputImage                                                              InputImageType;
  typedef typename InputImageType::Pointer                                         InputImagePointer;
  typedef Image<CovariantVector<TOutputValueType, ImageDimension>, ImageDimension> OutputImageType;

  /** Standard class typedefs. */
  typedef ImageToImageFilter<InputImageType, OutputImageType> Superclass;
  typedef SmartPointer<Self>                                  Pointer;
  typedef SmartPointer<const Self>                            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineGradientImageFilter, ImageToImageFilter);

  /** Image typedef support. */
  typedef typename InputImageType::PixelType                                             InputPixelType;
  typedef TOutputValueType                                                               OutputValueType;
  typedef CovariantVector<OutputValueType, itkGetStaticConstMacro(OutputImageDimension)> OutputPixelType;
  typedef typename OutputImageType::RegionType                                           OutputImageRegionType;

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputPixelType, OutputValueType>));
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<OutputValueType>));
  /** End concept checking */
#endif

  /** Typedefs for the interpolator. */
  typedef TCoordRep                                                                           CoordRepType;
  typedef TCoefficientType                                                                    CoefficientType;
  typedef itk::BSplineInterpolateImageFunction<InputImageType, CoordRepType, CoefficientType> InterpolatorType;
  typedef typename InterpolatorType::Pointer                                                  InterpolatorPointerType;

protected:
  BSplineGradientImageFilter();
  virtual ~BSplineGradientImageFilter() {}

  /** This filter requires all of the input image for now.  Future
   * implementation may be capable of streaming. */
  void
  GenerateInputRequestedRegion() override;

  /** This filter must produce all of its output at once. */
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

private:
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBSplineGradientImageFilter.hxx"
#endif

#endif
