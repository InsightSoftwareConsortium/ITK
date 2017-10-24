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
#ifndef itkGPUScalarAnisotropicDiffusionFunction_h
#define itkGPUScalarAnisotropicDiffusionFunction_h

#include "itkGPUAnisotropicDiffusionFunction.h"

namespace itk
{
/**
 * \class GPUScalarAnisotropicDiffusionFunction
 *
 * This class forms the base for any GPU anisotropic diffusion function that
 * operates on scalar data (see itkGPUAnisotropicDiffusionFunction).
 *
 * \ingroup ITKGPUAnisotropicSmoothing
 * */

/** Create a helper GPU Kernel class for GPUScalarAnisotropicDiffusionFunction */
itkGPUKernelClassMacro(GPUScalarAnisotropicDiffusionFunctionKernel);

template< typename TImage >
class ITK_TEMPLATE_EXPORT GPUScalarAnisotropicDiffusionFunction :
  public GPUAnisotropicDiffusionFunction< TImage >
{
public:
  /** Standard class typedefs. */
  typedef GPUScalarAnisotropicDiffusionFunction     Self;
  typedef GPUAnisotropicDiffusionFunction< TImage > Superclass;
  typedef SmartPointer< Self >                      Pointer;
  typedef SmartPointer< const Self >                ConstPointer;

  itkTypeMacro(GPUScalarAnisotropicDiffusionFunction, GPUAnisotropicDiffusionFunction);

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

  /** Get OpenCL Kernel source as a string, creates a GetOpenCLSource method */
  itkGetOpenCLSourceFromKernelMacro(GPUScalarAnisotropicDiffusionFunctionKernel);

  /** Compute average squared gradient of magnitude using the GPU */
  virtual void GPUCalculateAverageGradientMagnitudeSquared(TImage *) ITK_OVERRIDE;

protected:
  GPUScalarAnisotropicDiffusionFunction();
  ~GPUScalarAnisotropicDiffusionFunction() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUScalarAnisotropicDiffusionFunction);

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGPUScalarAnisotropicDiffusionFunction.hxx"
#endif

#endif
