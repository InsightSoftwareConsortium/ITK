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
template< typename TImage >
class ITK_TEMPLATE_EXPORT ScalarAnisotropicDiffusionFunction:
  public AnisotropicDiffusionFunction< TImage >
{
public:
  /** Standard class typedefs. */
  typedef ScalarAnisotropicDiffusionFunction     Self;
  typedef AnisotropicDiffusionFunction< TImage > Superclass;
  typedef SmartPointer< Self >                   Pointer;
  typedef SmartPointer< const Self >             ConstPointer;

  /** Inherit some parameters from the superclass type. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** Inherit some parameters from the superclass type. */
  typedef typename Superclass::ImageType        ImageType;
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::PixelRealType    PixelRealType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::TimeStepType     TimeStepType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScalarAnisotropicDiffusionFunction,
               AnisotropicDiffusionFunction);

  virtual void CalculateAverageGradientMagnitudeSquared(TImage *) ITK_OVERRIDE;

protected:
  ScalarAnisotropicDiffusionFunction() {}
  ~ScalarAnisotropicDiffusionFunction() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ScalarAnisotropicDiffusionFunction);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScalarAnisotropicDiffusionFunction.hxx"
#endif

#endif
