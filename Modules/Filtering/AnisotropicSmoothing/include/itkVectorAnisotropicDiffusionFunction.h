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
#ifndef itkVectorAnisotropicDiffusionFunction_h
#define itkVectorAnisotropicDiffusionFunction_h

#include "itkAnisotropicDiffusionFunction.h"
#include "itkVector.h"

namespace itk
{
/** \class VectorAnisotropicDiffusionFunction
 *
 * This class implements a vector-valued version of
 * AnisotropicDiffusionFunction.  Typically in vector-valued diffusion, vector
 * components are diffused independently of one another using a conductance
 * term that is linked across the components. Refer to the the documentation of
 * AnisotropicDiffusionFunction for an overview of anisotropic diffusion.  The
 * way that the conductance term is calculated is specific to the specific type
 * of diffusion function.
 *
 * \par Data type requirements
 * This filter was designed to process itk::Images of itk::Vector type.  The code
 * relies on various typedefs and overloaded operators defined in itk::Vector.
 * It is perfectly reasonable, however, to apply this filter to images of other,
 * user-defined types as long as the appropriate typedefs and operator overloads
 * are in place.  As a general rule, follow the example of itk::Vector in
 * defining your data types.
 *
 *  \ingroup FiniteDifferenceFunctions
 *  \ingroup ImageEnhancement
 *
 * \sa AnisotropicDiffusionFunction
 * \sa ScalarAnisotropicDiffusionFunction
 *
 * \ingroup ITKAnisotropicSmoothing
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT VectorAnisotropicDiffusionFunction:
  public AnisotropicDiffusionFunction< TImage >
{
public:
  /** Standard class typedefs. */
  typedef VectorAnisotropicDiffusionFunction     Self;
  typedef AnisotropicDiffusionFunction< TImage > Superclass;
  typedef SmartPointer< Self >                   Pointer;
  typedef SmartPointer< const Self >             ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(VectorAnisotropicDiffusionFunction,
               AnisotropicDiffusionFunction);

  /** Inherit some parameters from the superclass type */
  typedef typename Superclass::ImageType        ImageType;
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::TimeStepType     TimeStepType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;

  /** Inherit some parameters from the superclass type */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);
  itkStaticConstMacro(VectorDimension, unsigned int, PixelType::Dimension);

  /** Compute the average gradient magnitude squared. */
  virtual void CalculateAverageGradientMagnitudeSquared(TImage *) ITK_OVERRIDE;

protected:
  VectorAnisotropicDiffusionFunction() {}
  ~VectorAnisotropicDiffusionFunction() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  { Superclass::PrintSelf(os, indent); }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VectorAnisotropicDiffusionFunction);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorAnisotropicDiffusionFunction.hxx"
#endif

#endif
