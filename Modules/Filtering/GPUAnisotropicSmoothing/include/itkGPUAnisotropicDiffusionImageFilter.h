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
#ifndef itkGPUAnisotropicDiffusionImageFilter_h
#define itkGPUAnisotropicDiffusionImageFilter_h

#include "itkGPUDenseFiniteDifferenceImageFilter.h"
#include "itkAnisotropicDiffusionImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
/**
 * \class GPUAnisotropicDiffusionImageFilter
 * This filter is the GPU base class for AnisotropicDiffusionImageFilter.
 * InitializeIteration() calls GPUCalculateAverageGradientMagnitudeSquared().
 *
 * \ingroup ITKGPUAnisotropicSmoothing
 */
template< typename TInputImage, typename TOutputImage, typename TParentImageFilter =
            AnisotropicDiffusionImageFilter< TInputImage, TOutputImage > >
class ITK_TEMPLATE_EXPORT GPUAnisotropicDiffusionImageFilter :
  public GPUDenseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TParentImageFilter >
{
public:
  /** Standard class typedefs. */
  typedef GPUAnisotropicDiffusionImageFilter                                                   Self;
  typedef GPUDenseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TParentImageFilter > GPUSuperclass;
  typedef AnisotropicDiffusionImageFilter< TInputImage, TOutputImage >                         CPUSuperclass;
  typedef SmartPointer< Self >                                                                 Pointer;
  typedef SmartPointer< const Self >                                                           ConstPointer;

  /** Run-time type information. */
  itkTypeMacro(GPUAnisotropicDiffusionImageFilter,
               GPUDenseFiniteDifferenceImageFilter);

  /** Capture information from the superclass. */
  typedef typename GPUSuperclass::InputImageType   InputImageType;
  typedef typename GPUSuperclass::OutputImageType  OutputImageType;
  typedef typename GPUSuperclass::UpdateBufferType UpdateBufferType;

  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int, GPUSuperclass::ImageDimension);

  /** The pixel type of the output image will be used in computations.
   * Inherited from the superclass. */
  typedef typename GPUSuperclass::PixelType    PixelType;
  typedef typename GPUSuperclass::TimeStepType TimeStepType;

protected:
  GPUAnisotropicDiffusionImageFilter() {}
  ~GPUAnisotropicDiffusionImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Prepare for the iteration process. */
  virtual void InitializeIteration() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUAnisotropicDiffusionImageFilter);

};
} // end namspace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGPUAnisotropicDiffusionImageFilter.hxx"
#endif

#endif
