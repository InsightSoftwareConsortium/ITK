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
#ifndef itkGradientNDAnisotropicDiffusionFunction_h
#define itkGradientNDAnisotropicDiffusionFunction_h

#include "itkScalarAnisotropicDiffusionFunction.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkDerivativeOperator.h"

namespace itk
{
/** \class GradientNDAnisotropicDiffusionFunction
 *
 * This class implements an N-dimensional version of the classic Perona-Malik
 * anisotropic diffusion equation for scalar-valued images.  See
 * itkAnisotropicDiffusionFunction for an overview of the anisotropic diffusion
 * framework and equation.
 *
 * \par
 * The conductance term for this implementation is chosen as a function of the
 * gradient magnitude of the image at each point, reducing the strength of
 * diffusion at edge pixels.
 *
 * \f[C(\mathbf{x}) = e^{-(\frac{\parallel \nabla U(\mathbf{x}) \parallel}{K})^2}\f].
 *
 * \par
 * The numerical implementation of this equation is similar to that described
 * in the Perona-Malik paper below, but uses a more robust technique
 * for gradient magnitude estimation and has been generalized to N-dimensions.
 *
 * \par References
 * Pietro Perona and Jalhandra Malik, ``Scale-space and edge detection using
 * anisotropic diffusion,'' IEEE Transactions on Pattern Analysis Machine
 * Intelligence, vol. 12, pp. 629-639, 1990.
 *
 * \sa AnisotropicDiffusionFunction
 * \sa VectorAnisotropicDiffusionFunction
 * \sa VectorGradientAnisotropicDiffusionFunction
 * \sa CurvatureNDAnisotropicDiffusionFunction
 * \ingroup FiniteDifferenceFunctions
 * \ingroup ImageEnhancement
 * \ingroup ITKAnisotropicSmoothing
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT GradientNDAnisotropicDiffusionFunction : public ScalarAnisotropicDiffusionFunction<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GradientNDAnisotropicDiffusionFunction);

  /** Standard class type aliases. */
  using Self = GradientNDAnisotropicDiffusionFunction;
  using Superclass = ScalarAnisotropicDiffusionFunction<TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(GradientNDAnisotropicDiffusionFunction, ScalarAnisotropicDiffusionFunction);

  /** Inherit some parameters from the superclass type. */
  using ImageType = typename Superclass::ImageType;
  using PixelType = typename Superclass::PixelType;
  using PixelRealType = typename Superclass::PixelRealType;
  using TimeStepType = typename Superclass::TimeStepType;
  using RadiusType = typename Superclass::RadiusType;
  using NeighborhoodType = typename Superclass::NeighborhoodType;
  using FloatOffsetType = typename Superclass::FloatOffsetType;

  using NeighborhoodSizeValueType = SizeValueType;

  /** Inherit some parameters from the superclass type. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Compute the equation value. */
  PixelType
  ComputeUpdate(const NeighborhoodType & it,
                void *                   globalData,
                const FloatOffsetType &  offset = FloatOffsetType(0.0)) override;

  /** This method is called prior to each iteration of the solver. */
  void
  InitializeIteration() override
  {
    m_K = static_cast<PixelType>(this->GetAverageGradientMagnitudeSquared() * this->GetConductanceParameter() *
                                 this->GetConductanceParameter() * -2.0f);
  }

protected:
  GradientNDAnisotropicDiffusionFunction();
  ~GradientNDAnisotropicDiffusionFunction() override = default;

  /** Inner product function. */
  NeighborhoodInnerProduct<ImageType> m_InnerProduct;

  /** Slices for the ND neighborhood. */
  std::slice x_slice[ImageDimension];
  std::slice xa_slice[ImageDimension][ImageDimension];
  std::slice xd_slice[ImageDimension][ImageDimension];

  /** Derivative operator. */
  DerivativeOperator<PixelType, Self::ImageDimension> m_DerivativeOperator;

  /** Modified global average gradient magnitude term. */
  PixelType m_K;

  NeighborhoodSizeValueType m_Center;
  NeighborhoodSizeValueType m_Stride[ImageDimension];

  static double m_MIN_NORM;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGradientNDAnisotropicDiffusionFunction.hxx"
#endif

#endif
