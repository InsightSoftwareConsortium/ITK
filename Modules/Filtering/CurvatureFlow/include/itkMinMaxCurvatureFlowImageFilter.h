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
#ifndef itkMinMaxCurvatureFlowImageFilter_h
#define itkMinMaxCurvatureFlowImageFilter_h

#include "itkCurvatureFlowImageFilter.h"
#include "itkMinMaxCurvatureFlowFunction.h"

namespace itk
{
/**
 *\class MinMaxCurvatureFlowImageFilter
 * \brief Denoise an image using min/max curvature flow.
 *
 * MinMaxCurvatureFlowImageFilter implements a curvature driven image denoising
 * algorithm. Iso-brightness contours in the grayscale input image are viewed
 * as a level set. The level set is then evolved using a curvature-based speed
 * function:
 *
 * \f[  I_t = F_{\mbox{minmax}} |\nabla I| \f]
 *
 * where \f$ F_{\mbox{minmax}} = \max(\kappa,0) \f$ if
 * \f$ \mbox{Avg}_{\mbox{stencil}}(x) \f$ is less than or equal to \f$ T_{threshold} \f$
 * and \f$ \min(\kappa,0) \f$, otherwise.
 * \f$ \kappa \f$ is the mean curvature of the iso-brightness contour
 * at point \f$ x \f$.
 *
 * In min/max curvature flow, movement is turned on or off depending
 * on the scale of the noise one wants to remove. Switching depends on
 * the average image value of a region of radius \f$ R \f$ around each
 * point. The choice of \f$ R \f$, the stencil radius, governs the scale of
 * the noise to be removed.
 *
 * The threshold value \f$ T_{threshold} \f$ is the average intensity obtained
 * in the direction perpendicular to the gradient at point \f$ x \f$ at the
 * extrema of the local neighborhood.
 *
 * This filter make use of the multi-threaded finite difference solver
 * hierarchy.  Updates are computed using a MinMaxCurvatureFlowFunction object.
 * A zero flux Neumann boundary condition is used when computing derivatives
 * near the data boundary.
 *
 * \warning This filter assumes that the input and output types have the
 * same dimensions. This filter also requires that the output image pixels
 * are of a real type. This filter works for any dimensional images,
 * however for dimensions greater than 3D, an expensive brute-force search
 * is used to compute the local threshold.
 *
 * Reference:
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Chapter 16, Second edition, 1999.
 *
 * \sa MinMaxCurvatureFlowFunction
 * \sa CurvatureFlowImageFilter
 * \sa BinaryMinMaxCurvatureFlowImageFilter
 *
 * \ingroup ImageEnhancement
 * \ingroup MultiThreaded
 *
 * \ingroup ITKCurvatureFlow
 *
 * \sphinx
 * \sphinxexample{Filtering/CurvatureFlow/SmoothImageUsingMinMaxCurvatureFlow,Smooth Image Using Min Max Curvature Flow}
 * \sphinxexample{Filtering/CurvatureFlow/SmoothRGBImageUsingMinMaxCurvatureFlow,SmoothRGBImageUsingMinMaxCurvatureFlow}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT MinMaxCurvatureFlowImageFilter : public CurvatureFlowImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MinMaxCurvatureFlowImageFilter);

  /** Standard class type aliases. */
  using Self = MinMaxCurvatureFlowImageFilter;
  using Superclass = CurvatureFlowImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MinMaxCurvatureFlowImageFilter, CurvatureFlowImageFilter);

  /** Inherit type alias from Superclass. */
  using FiniteDifferenceFunctionType = typename Superclass::FiniteDifferenceFunctionType;
  using OutputImageType = typename Superclass::OutputImageType;

  /** MinMaxCurvatureFlowFunction type. */
  using MinMaxCurvatureFlowFunctionType = MinMaxCurvatureFlowFunction<OutputImageType>;

  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Typedef support for the neighbour radius. */
  using RadiusType = typename FiniteDifferenceFunctionType::RadiusType;
  using RadiusValueType = typename RadiusType::SizeValueType;

  /** Set/Get the stencil radius. */
  itkSetMacro(StencilRadius, RadiusValueType);
  itkGetConstMacro(StencilRadius, RadiusValueType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(UnsignedLongConvertibleToOutputCheck,
                  (Concept::Convertible<unsigned long, typename TOutputImage::PixelType>));
  itkConceptMacro(OutputLessThanComparableCheck, (Concept::LessThanComparable<typename TOutputImage::PixelType>));
  itkConceptMacro(LongConvertibleToOutputCheck, (Concept::Convertible<long, typename TOutputImage::PixelType>));
  itkConceptMacro(OutputDoubleComparableCheck, (Concept::Comparable<typename TOutputImage::PixelType, double>));
  itkConceptMacro(OutputDoubleMultiplyAndAssignOperatorCheck,
                  (Concept::MultiplyAndAssignOperator<typename TOutputImage::PixelType, double>));
  itkConceptMacro(OutputGreaterThanUnsignedLongCheck,
                  (Concept::GreaterThanComparable<typename TOutputImage::PixelType, unsigned long>));
  itkConceptMacro(UnsignedLongOutputAditiveOperatorsCheck,
                  (Concept::AdditiveOperators<unsigned long, typename TOutputImage::PixelType>));
  // End concept checking
#endif

protected:
  MinMaxCurvatureFlowImageFilter();
  ~MinMaxCurvatureFlowImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Initialize the state of filter and equation before each iteration.
   * Progress feedback is implemented as part of this method. */
  void
  InitializeIteration() override;

private:
  RadiusValueType m_StencilRadius;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMinMaxCurvatureFlowImageFilter.hxx"
#endif

#endif
