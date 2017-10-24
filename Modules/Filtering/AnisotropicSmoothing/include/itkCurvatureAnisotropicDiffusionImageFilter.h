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
#ifndef itkCurvatureAnisotropicDiffusionImageFilter_h
#define itkCurvatureAnisotropicDiffusionImageFilter_h

#include "itkAnisotropicDiffusionImageFilter.h"
#include "itkCurvatureNDAnisotropicDiffusionFunction.h"
#include "itkMacro.h"
namespace itk
{
/**
 * \class CurvatureAnisotropicDiffusionImageFilter
 *
 * This filter performs anisotropic diffusion on a scalar itk::Image using the
 * modified curvature diffusion equation (MCDE) implemented in
 * itkCurvatureNDAnisotropicDiffusionFunction.  For detailed information on
 * anisotropic diffusion and the MCDE see itkAnisotropicDiffusionFunction and
 * itkCurvatureNDAnisotropicDiffusionFunction.
 *
 * \par Inputs and Outputs
 * The input and output to this filter must be a scalar itk::Image with
 * numerical pixel types (float or double).  A user defined type which
 * correctly defines arithmetic operations with floating point accuracy should
 * also give correct results.
 *
 * \par Parameters
 * Please first read all the documentation found in
 * AnisotropicDiffusionImageFilter and AnisotropicDiffusionFunction.  Also see
 * CurvatureNDAnisotropicDiffusionFunction.
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
 * \ingroup ITKAnisotropicSmoothing
 */
template< typename TInputImage, typename TOutputImage >
class CurvatureAnisotropicDiffusionImageFilter:
  public AnisotropicDiffusionImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef CurvatureAnisotropicDiffusionImageFilter Self;
  typedef AnisotropicDiffusionImageFilter< TInputImage, TOutputImage >
  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Standard method for creation through object factory. */
  itkNewMacro(Self);

  /** Run-time information. */
  itkTypeMacro(CurvatureAnisotropicDiffusionImageFilter,
               AnisotropicDiffusionImageFilter);

  /** Extract superclass information. */
  typedef typename Superclass::UpdateBufferType UpdateBufferType;

  /** Extract superclass image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( OutputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< typename TOutputImage::PixelType > ) );
  // End concept checking
#endif

protected:
  CurvatureAnisotropicDiffusionImageFilter()
  {
    typename CurvatureNDAnisotropicDiffusionFunction< UpdateBufferType >::Pointer q =
      CurvatureNDAnisotropicDiffusionFunction< UpdateBufferType >::New();
    this->SetDifferenceFunction(q);
  }

  ~CurvatureAnisotropicDiffusionImageFilter() ITK_OVERRIDE {}

  virtual void InitializeIteration() ITK_OVERRIDE
  {
    Superclass::InitializeIteration();
    if ( this->GetTimeStep() >  0.5 / std::pow( 2.0, static_cast< double >( ImageDimension ) ) )
      {
      itkWarningMacro(
        << "Anisotropic diffusion is using a time step which may introduce instability into the solution.");
      }
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(CurvatureAnisotropicDiffusionImageFilter);
};
} // end namspace itk

#endif
