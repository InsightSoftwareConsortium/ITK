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
#ifndef itkVectorCurvatureAnisotropicDiffusionImageFilter_h
#define itkVectorCurvatureAnisotropicDiffusionImageFilter_h

#include "itkMacro.h"
#include "itkAnisotropicDiffusionImageFilter.h"
#include "itkVectorCurvatureNDAnisotropicDiffusionFunction.h"

namespace itk
{
/** \class VectorCurvatureAnisotropicDiffusionImageFilter
 *
 * This filter performs anisotropic diffusion on a vector itk::Image using the
 * modified curvature diffusion equation (MCDE) implemented in
 * itkVectorCurvatureNDAnisotropicDiffusionFunction.  For detailed information on
 * anisotropic diffusion and the MCDE see itkAnisotropicDiffusionFunction,
 * itkVectorCurvatureNDAnisotropicDiffusionFunction, and
 * itkCurvatureNDAnisotropicDiffusionFunction.
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
 * Please first read all the documentation found in
 * AnisotropicDiffusionImageFilter and AnisotropicDiffusionFunction.  Also see
 * VectorCurvatureNDAnisotropicDiffusionFunction.
 *
 * The default time step for this filter is set to the maximum theoretically
 * stable value: 0.5 / 2^N, where N is the dimensionality of the image.  For a
 * 2D image, this means valid time steps are below 0.1250.  For a 3D image,
 * valid time steps are below 0.0625.
 *
 * \sa AnisotropicDiffusionImageFilter
 * \sa AnisotropicDiffusionFunction
 * \sa CurvatureNDAnisotropicDiffusionFunction

 * \ingroup ImageEnhancement
 *
 * \ingroup ITKAnisotropicSmoothing
 */
template< typename TInputImage, typename TOutputImage >
class VectorCurvatureAnisotropicDiffusionImageFilter:
  public AnisotropicDiffusionImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard itk typedefs */
  typedef VectorCurvatureAnisotropicDiffusionImageFilter Self;
  typedef AnisotropicDiffusionImageFilter< TInputImage, TOutputImage >
  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Instantiation through object factory. */
  itkNewMacro(Self);

  /** Run-time type information. */
  itkTypeMacro(VectorCurvatureAnisotropicDiffusionImageFilter,
               AnisotropicDiffusionImageFilter);

  /** Convenient typedef. */
  typedef typename Superclass::UpdateBufferType UpdateBufferType;

  /** Determine the image dimension. */
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
  VectorCurvatureAnisotropicDiffusionImageFilter()
  {
    typename VectorCurvatureNDAnisotropicDiffusionFunction< UpdateBufferType >::Pointer q =
      VectorCurvatureNDAnisotropicDiffusionFunction< UpdateBufferType >::New();
    this->SetDifferenceFunction(q);
  }

  ~VectorCurvatureAnisotropicDiffusionImageFilter() ITK_OVERRIDE {}

  virtual void InitializeIteration() ITK_OVERRIDE
  {
    Superclass::InitializeIteration();
    if ( this->GetTimeStep() >  0.5 / std::pow( 2.0, static_cast< double >( ImageDimension ) ) )
      {
      itkWarningMacro(
        << "Anisotropic diffusion has attempted to use a time step which may introduce instability into the solution.");
      }
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VectorCurvatureAnisotropicDiffusionImageFilter);
};
} // end namspace itk

#endif
