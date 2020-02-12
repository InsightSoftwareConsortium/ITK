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
#ifndef itkBinaryMinMaxCurvatureFlowImageFilter_h
#define itkBinaryMinMaxCurvatureFlowImageFilter_h

#include "itkMinMaxCurvatureFlowImageFilter.h"
#include "itkBinaryMinMaxCurvatureFlowFunction.h"

namespace itk
{
/**
 *\class BinaryMinMaxCurvatureFlowImageFilter
 * \brief Denoise a binary image using min/max curvature flow.
 *
 * BinaryMinMaxCurvatureFlowImageFilter implements a curvature driven image
 * denoising algorithm. This filter assumes that the image is essentially
 * binary: consisting of two classes. Iso-brightness contours in the input
 * image are viewed as a level set. The level set is then evolved using
 * a curvature-based speed function:
 *
 * \f[  I_t = F_{\mbox{minmax}} |\nabla I| \f]
 *
 * where \f$ F_{\mbox{minmax}} = \min(\kappa,0) \f$ if
 * \f$ \mbox{Avg}_{\mbox{stencil}}(x) \f$
 * is less than or equal to \f$ T_{threshold} \f$
 * and \f$ \max(\kappa,0) \f$, otherwise.
 * \f$ \kappa \f$ is the mean curvature of the iso-brightness contour
 * at point \f$ x \f$.
 *
 * In min/max curvature flow, movement is turned on or off depending
 * on the scale of the noise one wants to remove. Switching depends on
 * the average image value of a region of radius \f$ R \f$ around each
 * point. The choice of \f$ R \f$, the stencil radius, governs the scale of
 * the noise to be removed.
 *
 * The threshold value \f$ T_{threshold} \f$ is a user specified value which
 * discriminates between the two pixel classes.
 *
 * This filter make use of the multi-threaded finite difference solver
 * hierarchy.  Updates are computed using a BinaryMinMaxCurvatureFlowFunction
 * object. A zero flux Neumann boundary condition is used when computing
 * derivatives near the data boundary.
 *
 * \warning This filter assumes that the input and output types have the
 * same dimensions. This filter also requires that the output image pixels
 * are of a real type. This filter works for any dimensional images.
 *
 * Reference:
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Chapter 16, Second edition, 1999.
 *
 * \sa BinaryMinMaxCurvatureFlowFunction
 * \sa CurvatureFlowImageFilter
 * \sa MinMaxCurvatureFlowImageFilter
 *
 * \ingroup ImageEnhancement
 * \ingroup MultiThreaded
 *
 * \ingroup ITKCurvatureFlow
 *
 * \sphinx
 * \sphinxexample{Filtering/CurvatureFlow/BinaryMinMaxCurvatureFlow,Binary Min And Max Curvature Flow Of Binary Image}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT BinaryMinMaxCurvatureFlowImageFilter
  : public MinMaxCurvatureFlowImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryMinMaxCurvatureFlowImageFilter);

  /** Standard class type aliases. */
  using Self = BinaryMinMaxCurvatureFlowImageFilter;
  using Superclass = MinMaxCurvatureFlowImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryMinMaxCurvatureFlowImageFilter, MinMaxCurvatureFlowImageFilter);

  /** Inherit type alias from Superclass. */
  using FiniteDifferenceFunctionType = typename Superclass::FiniteDifferenceFunctionType;
  using OutputImageType = typename Superclass::OutputImageType;

  /** BinaryMinMaxCurvatureFlowFunction type. */
  using BinaryMinMaxCurvatureFlowFunctionType = BinaryMinMaxCurvatureFlowFunction<OutputImageType>;

  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Set/Get the threshold value. */
  itkSetMacro(Threshold, double);
  itkGetConstMacro(Threshold, double);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputConvertibleToOutputCheck,
                  (Concept::Convertible<typename TInputImage::PixelType, typename TOutputImage::PixelType>));
  // End concept checking
#endif

protected:
protected:
  BinaryMinMaxCurvatureFlowImageFilter();
  ~BinaryMinMaxCurvatureFlowImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Initialize the state of filter and equation before each iteration.
   * Progress feedback is implemented as part of this method. */
  void
  InitializeIteration() override;

private:
  double m_Threshold;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinaryMinMaxCurvatureFlowImageFilter.hxx"
#endif

#endif
