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
#ifndef itkVectorGradientAnisotropicDiffusionImageFilter_h
#define itkVectorGradientAnisotropicDiffusionImageFilter_h

#include "itkMacro.h"
#include "itkAnisotropicDiffusionImageFilter.h"
#include "itkVectorGradientNDAnisotropicDiffusionFunction.h"

namespace itk
{
/** \class VectorGradientAnisotropicDiffusionImageFilter
 *
 * This filter performs anisotropic diffusion on a vector itk::Image using the
 * anisotropic diffusion function implemented implemented in
 * itkVectorGradientNDAnisotropicDiffusionFunction.  For detailed information on
 * anisotropic diffusion see itkAnisotropicDiffusionFunction,
 * itkVectorGradientNDAnisotropicDiffusionFunction, and
 * itkGradientAnisotropicDiffusionFunction.
 *
 * \par Inputs and Outputs
 * The input to this filter must be an itk::Image with pixel
 * type which is either an itk::Vector, or a subclass of an itk::Vector.
 * Additionally, the component type of the vector should be a numerical type
 * (float or double, or a user defined type which correctly defines
 * arithmetic operations with floating point accuracy).  The output image type
 * also has these requirements.
 *
 * \par Parameters
 * Please read all the documentation found in
 * AnisotropicDiffusionImageFilter and AnisotropicDiffusionFunction.  Also see
 * VectorGradientNDAnisotropicDiffusionFunction.
 *
 * The maximum allowable time step for this filter is 1/2^N, where N is the
 * dimensionality of the image.  For 2D images any value below 0.250 is stable,
 * and for 3D images, any value below 0.125 is stable.
 *
 * \ingroup ImageEnhancement
 * \ingroup ITKAnisotropicSmoothing
 *
 * \wiki
 * \wikiexample{Smoothing/VectorGradientAnisotropicDiffusionImageFilter,Smooth an image while preserving edges}
 * \endwiki
 */
template< typename TInputImage, typename TOutputImage >
class VectorGradientAnisotropicDiffusionImageFilter:
  public AnisotropicDiffusionImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef VectorGradientAnisotropicDiffusionImageFilter Self;
  typedef AnisotropicDiffusionImageFilter< TInputImage, TOutputImage >
  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Instantiation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information. */
  itkTypeMacro(VectorGradientAnisotropicDiffusionImageFilter,
               AnisotropicDiffusionImageFilter);

  /** Extract information from the superclass. */
  typedef typename Superclass::UpdateBufferType UpdateBufferType;

  /** Determine the image dimension from the  superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< typename TInputImage::PixelType::ValueType > ) );
  itkConceptMacro( OutputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< typename TOutputImage::PixelType::ValueType > ) );
  // End concept checking
#endif

protected:
  VectorGradientAnisotropicDiffusionImageFilter()
  {
    typename VectorGradientNDAnisotropicDiffusionFunction< UpdateBufferType >::Pointer p =
      VectorGradientNDAnisotropicDiffusionFunction< UpdateBufferType >::New();
    this->SetDifferenceFunction(p);
  }

  ~VectorGradientAnisotropicDiffusionImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VectorGradientAnisotropicDiffusionImageFilter);
};
} // end namspace itk

#endif
