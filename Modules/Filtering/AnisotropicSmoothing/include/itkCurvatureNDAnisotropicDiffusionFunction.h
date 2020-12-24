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
#ifndef itkCurvatureNDAnisotropicDiffusionFunction_h
#define itkCurvatureNDAnisotropicDiffusionFunction_h

#include "itkScalarAnisotropicDiffusionFunction.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkDerivativeOperator.h"

namespace itk
{
/**
 * \class CurvatureNDAnisotropicDiffusionFunction
 *
 * This class implements a variation on the classic, Perona-Malik anisotropic
 * image diffusion equation as described in
 * itkGradientNDAnisotropicDiffusionFunction.  This object is a level-set
 * analog of that equation and will be referred to below as the \em modified
 * \em curvature \em diffusion \em equation (MCDE).  MCDE does not exhibit
 * the edge enhancing properties of classic anisotropic diffusion, which can under
 * certain conditions undergo a "negative" diffusion,which enhances the
 * contrast of edges.  Equations of the form of MCDE always undergo positive
 * diffusion, with the conductance term only varying the strength of that
 * diffusion.
 *
 * \par
 * Qualitatively, MCDE compares well with other non-linear diffusion
 * techniques.  It is less sensitive to contrast than classic Perona-Malik
 * style diffusion, and preserves finer detailed structures in images.
 * There is a potential speed trade-off for using this function in place of
 * itkGradientNDAnisotropicDiffusionFunction.  Each iteration of the solution
 * takes roughly twice as long.  Fewer iterations, however, may be required to
 * reach an acceptable solution.
 *
 * \par
 * The MCDE equation is given as:
 *
 * \f[ f_t = \mid \nabla f \mid \nabla \cdot c( \mid \nabla f \mid ) \frac{
 * \nabla f }{ \mid \nabla f \mid } \f] ,
 *
 * \par
 * where the conductance modified curvature term is
 *
 * \f[ \nabla \cdot \frac{\nabla f}{\mid \nabla f \mid} \f] .
 *
 * \par References
 *  R. Whitaker and X. Xue. Variable-Conductance, Level-Set Curvature for
 *  Image Denoising, International Conference on Image Processing, 2001
 *  pp. 142-145, Vol.3.
 *
 *
 * \sa AnisotropicDiffusionFunction
 * \ingroup FiniteDifferenceFunctions
 * \ingroup ImageEnhancement
 * \todo References
 * \ingroup ITKAnisotropicSmoothing
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT CurvatureNDAnisotropicDiffusionFunction : public ScalarAnisotropicDiffusionFunction<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(CurvatureNDAnisotropicDiffusionFunction);

  /** Standard class type aliases. */
  using Self = CurvatureNDAnisotropicDiffusionFunction;
  using Superclass = ScalarAnisotropicDiffusionFunction<TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(CurvatureNDAnisotropicDiffusionFunction, ScalarAnisotropicDiffusionFunction);

  /** Inherit some parameters from the superclass type. */
  using ImageType = typename Superclass::ImageType;
  using PixelType = typename Superclass::PixelType;
  using TimeStepType = typename Superclass::TimeStepType;
  using RadiusType = typename Superclass::RadiusType;
  using NeighborhoodType = typename Superclass::NeighborhoodType;
  using FloatOffsetType = typename Superclass::FloatOffsetType;

  using NeighborhoodSizeValueType = typename NeighborhoodType::SizeValueType;

  /** Inherit some parameters from the superclass type. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Compute incremental update. */
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
  CurvatureNDAnisotropicDiffusionFunction();
  ~CurvatureNDAnisotropicDiffusionFunction() override = default;

private:
  /** Inner product function. */
  NeighborhoodInnerProduct<ImageType> m_InnerProduct;

  /** Slices for the ND neighborhood. */
  std::slice x_slice[ImageDimension];
  std::slice xa_slice[ImageDimension][ImageDimension];
  std::slice xd_slice[ImageDimension][ImageDimension];

  /** Derivative operator */
  DerivativeOperator<PixelType, Self::ImageDimension> m_DerivativeOperator;

  /** Modified global average gradient magnitude term. */
  PixelType m_K;

  /** */
  static double m_MIN_NORM;

  NeighborhoodSizeValueType m_Center;
  NeighborhoodSizeValueType m_Stride[ImageDimension];
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCurvatureNDAnisotropicDiffusionFunction.hxx"
#endif

#endif
