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
#ifndef itkGPUCastImageFilter_hxx
#define itkGPUCastImageFilter_hxx

#include "itkGPUCastImageFilter.h"
#include "itkOpenCLUtil.h"

namespace itk
{

template <typename TInputImage, typename TOutputImage>
GPUCastImageFilter<TInputImage, TOutputImage>::GPUCastImageFilter()
{
  std::ostringstream defines;

  if (TInputImage::ImageDimension > 3 || TInputImage::ImageDimension < 1)
  {
    itkExceptionMacro("GPUCastImageFilter supports 1/2/3D image.");
  }

  defines << "#define DIM_" << TInputImage::ImageDimension << "\n";
  defines << "#define INPIXELTYPE ";
  GetTypenameInString(typeid(typename TInputImage::PixelType), defines);
  defines << "#define OUTPIXELTYPE ";
  GetTypenameInString(typeid(typename TOutputImage::PixelType), defines);

  // OpenCL kernel source
  const char * GPUSource = GPUCastImageFilterKernel::GetOpenCLSource();

  // load and build program
  this->m_GPUKernelManager->LoadProgramFromString(GPUSource, defines.str().c_str());

  // create kernel
  this->m_UnaryFunctorImageFilterGPUKernelHandle = this->m_GPUKernelManager->CreateKernel("CastImageFilter");
}


template <typename TInputImage, typename TOutputImage>
void
GPUCastImageFilter<TInputImage, TOutputImage>::GPUGenerateData()
{
  itkDebugMacro(<< "Calling GPUCastImageFilter::GPUGenerateData()");
  GPUSuperclass::GPUGenerateData();
  itkDebugMacro(<< "GPUCastImageFilter::GPUGenerateData() finished");
}


} // end of namespace itk

#endif /* itkGPUCastImageFilter_hxx */
