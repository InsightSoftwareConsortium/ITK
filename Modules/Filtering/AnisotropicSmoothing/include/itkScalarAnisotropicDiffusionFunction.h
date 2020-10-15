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
#ifndef itkScalarAnisotropicDiffusionFunction_h
#define itkScalarAnisotropicDiffusionFunction_h

#include "itkAnisotropicDiffusionFunction.h"

namespace itk
{
/**
 * \class ScalarAnisotropicDiffusionFunction
 * This class forms the base for any anisotropic diffusion function that
 * operates on scalar data (see itkAnisotropicDiffusionFunction).  It provides
 * some common functionality used in classes like
 * CurvatureNDAnisotropicDiffusionFunction and
 * GradientNDAnisotropicDiffusionFunction.
 *
 * \sa AnisotropicDiffusionFunction
 * \sa AnisotropicDiffusionImageFilter
 * \sa VectorAnisotropicDiffusionFunction
 * \ingroup FiniteDifferenceFunctions
 * \ingroup ImageEnhancement
 * \ingroup ITKAnisotropicSmoothing
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ScalarAnisotropicDiffusionFunction : public AnisotropicDiffusionFunction<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ScalarAnisotropicDiffusionFunction);

  /** Standard class type aliases. */
  using Self = ScalarAnisotropicDiffusionFunction;
  using Superclass = AnisotropicDiffusionFunction<TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Inherit some parameters from the superclass type. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Inherit some parameters from the superclass type. */
  using ImageType = typename Superclass::ImageType;
  using PixelType = typename Superclass::PixelType;
  using PixelRealType = typename Superclass::PixelRealType;
  using RadiusType = typename Superclass::RadiusType;
  using NeighborhoodType = typename Superclass::NeighborhoodType;
  using TimeStepType = typename Superclass::TimeStepType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScalarAnisotropicDiffusionFunction, AnisotropicDiffusionFunction);

  void
  CalculateAverageGradientMagnitudeSquared(TImage *) override;

protected:
  ScalarAnisotropicDiffusionFunction() = default;
  ~ScalarAnisotropicDiffusionFunction() override = default;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkScalarAnisotropicDiffusionFunction.hxx"
#endif

#endif
