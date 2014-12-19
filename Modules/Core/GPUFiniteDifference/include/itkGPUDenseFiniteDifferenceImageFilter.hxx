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
#ifndef itkGPUDenseFiniteDifferenceImageFilter_hxx
#define itkGPUDenseFiniteDifferenceImageFilter_hxx

#include "itkGPUDenseFiniteDifferenceImageFilter.h"

#include <list>
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkOpenCLUtil.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TParentImageFilter >
GPUDenseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TParentImageFilter >
::GPUDenseFiniteDifferenceImageFilter()
{
  /**
   * FiniteDifferenceImageFilter requires two GPU kernels
   * 1. CalculateChange
   * 2. ApplyUpdate
   * Kernel for 1 is defined in the specific DifferenceFunction.
   * Kernel for 2 can be used for entire subclass of GPUDense..,
   * it is defined and created here.
   */

  std::ostringstream defines;

  if(TInputImage::ImageDimension > 3 || ImageDimension < 1)
    {
    itkExceptionMacro("GPUDenseFiniteDifferenceImageFilter supports 1/2/3D image.");
    }

  defines << "#define DIM_" << TInputImage::ImageDimension << "\n";

  //PixelType is a Vector
  defines << "#define BUFPIXELTYPE ";
  GetTypenameInString( typeid ( typename UpdateBufferType::PixelType ), defines );

  defines << "#define OUTPIXELTYPE ";
  GetTypenameInString( typeid ( typename TOutputImage::PixelType ), defines );

  // assumes input and output pixel type is same
  defines << "#define PIXELDIM " << GetPixelDimension( typeid ( typename TOutputImage::PixelType ) ) << "\n";
  std::cout << "Defines: " << defines.str() << std::endl;

  const char* GPUSource = GPUDenseFiniteDifferenceImageFilter::GetOpenCLSource();

  // load and build program
  this->m_GPUKernelManager->LoadProgramFromString( GPUSource, defines.str().c_str() );

  // create kernel
  m_ApplyUpdateGPUKernelHandle = this->m_GPUKernelManager->CreateKernel("ApplyUpdate");

}

template< typename TInputImage, typename TOutputImage, typename TParentImageFilter >
void
GPUDenseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TParentImageFilter >
::CopyInputToOutput()
{
  CPUSuperclass::CopyInputToOutput();

  // Marking GPU is dirty by setting CPU as modified
  this->GetOutput()->Modified();
}

template< typename TInputImage, typename TOutputImage, typename TParentImageFilter >
void
GPUDenseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TParentImageFilter >
::AllocateUpdateBuffer()
{
  //CPUSuperclass will call Image::Allocate() which will call GPUImage::Allocate() .
  CPUSuperclass::AllocateUpdateBuffer();
}

template< typename TInputImage, typename TOutputImage, typename TParentImageFilter >
void
GPUDenseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TParentImageFilter >
::ApplyUpdate(const TimeStepType& dt)
{
  this->GPUApplyUpdate( dt );
}

template< typename TInputImage, typename TOutputImage, typename TParentImageFilter >
void
GPUDenseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TParentImageFilter >
::GPUApplyUpdate(const TimeStepType& dt)
{
  // GPU version of ApplyUpdate
  // Single threaded version : Apply entire update buffer to output image
  typedef typename itk::GPUTraits< UpdateBufferType >::Type GPUBufferImage;
  typedef typename itk::GPUTraits< TOutputImage >::Type     GPUOutputImage;

  typename GPUBufferImage::Pointer  bfPtr =  dynamic_cast< GPUBufferImage * >( GetUpdateBuffer() );
  typename GPUOutputImage::Pointer  otPtr =  dynamic_cast< GPUOutputImage * >( this->GetOutput() ); //this->ProcessObject::GetOutput(0)
                                                                                                    // );
  typename GPUOutputImage::SizeType outSize = otPtr->GetLargestPossibleRegion().GetSize();

  int imgSize[3];
  imgSize[0] = imgSize[1] = imgSize[2] = 1;

  float timeStep = dt;

  int ImageDim = (int)TInputImage::ImageDimension;

  for(int i=0; i<ImageDim; i++)
    {
    imgSize[i] = outSize[i];
    }

  size_t localSize[3], globalSize[3];
  localSize[0] = localSize[1] = localSize[2] = OpenCLGetLocalBlockSize(ImageDim);
  for(int i=0; i<ImageDim; i++)
    {
    globalSize[i] = localSize[i]*(unsigned int)ceil( (float)outSize[i]/(float)localSize[i]); //
                                                                                             // total
                                                                                             // #
                                                                                             // of
                                                                                             // threads
    }

  // arguments set up
  int argidx = 0;
  this->m_GPUKernelManager->SetKernelArgWithImage(m_ApplyUpdateGPUKernelHandle, argidx++, bfPtr->GetGPUDataManager() );
  this->m_GPUKernelManager->SetKernelArgWithImage(m_ApplyUpdateGPUKernelHandle, argidx++, otPtr->GetGPUDataManager() );
  this->m_GPUKernelManager->SetKernelArg(m_ApplyUpdateGPUKernelHandle, argidx++, sizeof(float), &(timeStep) );
  for(int i=0; i<ImageDim; i++)
    {
    this->m_GPUKernelManager->SetKernelArg(m_ApplyUpdateGPUKernelHandle, argidx++, sizeof(int), &(imgSize[i]) );
    }

  // launch kernel
  this->m_GPUKernelManager->LaunchKernel(m_ApplyUpdateGPUKernelHandle, (int)TInputImage::ImageDimension, globalSize,
                                         localSize );

  // Explicitely call Modified on GetOutput here. Do we need this?
  //this->GetOutput()->Modified();
}

template< typename TInputImage, typename TOutputImage, typename TParentImageFilter >
typename
GPUDenseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TParentImageFilter >::TimeStepType
GPUDenseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TParentImageFilter >
::GPUCalculateChange()
{
  typename OutputImageType::Pointer output = this->GetOutput();

  TimeStepType timeStep;
  void *       globalData;

  GPUFiniteDifferenceFunction< OutputImageType > *df
    = dynamic_cast< GPUFiniteDifferenceFunction< OutputImageType > * >( this->GetDifferenceFunction().GetPointer() );

  globalData = df->GetGlobalDataPointer();
  this->m_ComputeUpdateTime.Start();
  df->GPUComputeUpdate( output, GetUpdateBuffer(), globalData );
  this->m_ComputeUpdateTime.Stop();

  //// Ask the finite difference function to compute the time step for
  //// this iteration.  We give it the global data pointer to use, then
  //// ask it to free the global data memory.
  timeStep = df->ComputeGlobalTimeStep(globalData);
  df->ReleaseGlobalDataPointer(globalData);

  return timeStep;
}

template< typename TInputImage, typename TOutputImage, typename TParentImageFilter >
void
GPUDenseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TParentImageFilter >
::PrintSelf(std::ostream & os, Indent indent) const
{
  GPUSuperclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
