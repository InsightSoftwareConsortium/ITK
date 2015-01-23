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
#ifndef itkGPUScalarAnisotropicDiffusionFunction_hxx
#define itkGPUScalarAnisotropicDiffusionFunction_hxx

#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkDerivativeOperator.h"
#include "itkGPUScalarAnisotropicDiffusionFunction.h"

namespace itk
{

template< typename TImage >
GPUScalarAnisotropicDiffusionFunction< TImage >
::GPUScalarAnisotropicDiffusionFunction()
{
  this->m_AnisotropicDiffusionFunctionGPUBuffer = GPUDataManager::New();
  this->m_AnisotropicDiffusionFunctionGPUKernelManager = GPUKernelManager::New();

  // load GPU kernel
  std::ostringstream defines;

  if(ImageDimension > 3 || ImageDimension < 1)
    {
    itkExceptionMacro("GPUScalarAnisotropicDiffusionFunction supports 1/2/3D image.");
    }

  defines << "#define DIM_" << ImageDimension << "\n";

  defines << "#define PIXELTYPE ";
  GetTypenameInString( typeid ( typename TImage::PixelType ), defines );
  std::cout << "Defines: " << defines.str() << std::endl;

  const char* GPUSource = GPUScalarAnisotropicDiffusionFunction::GetOpenCLSource();

  // load and build program
  this->m_AnisotropicDiffusionFunctionGPUKernelManager->LoadProgramFromString( GPUSource, defines.str().c_str() );

  // create kernel
  this->m_AverageGradientMagnitudeSquaredGPUKernelHandle =
    this->m_AnisotropicDiffusionFunctionGPUKernelManager->CreateKernel("AverageGradientMagnitudeSquared");
}

template< typename TImage >
void
GPUScalarAnisotropicDiffusionFunction< TImage >
::GPUCalculateAverageGradientMagnitudeSquared(TImage *ip)
{
  // GPU kernel to compute Average Squared Gradient Magnitude
  typedef typename itk::GPUTraits< TImage >::Type GPUImageType;
  typename GPUImageType::Pointer  inPtr =  dynamic_cast< GPUImageType * >( ip );
  typename GPUImageType::SizeType outSize = inPtr->GetLargestPossibleRegion().GetSize();

  int imgSize[3];
  imgSize[0] = imgSize[1] = imgSize[2] = 1;
  float imgScale[3];
  imgScale[0] = imgScale[1] = imgScale[2] = 1.0f;
  int ImageDim = (int)TImage::ImageDimension;

  size_t localSize[3], globalSize[3];
  localSize[0] = localSize[1] = localSize[2] = 1;
  globalSize[0] = globalSize[1] = globalSize[2] = 1;
  unsigned int blockSize = OpenCLGetLocalBlockSize(ImageDim);

  unsigned int numPixel = 1;
  unsigned int bufferSize = 1;
  for(int i=0; i<ImageDim; i++)
    {
    imgSize[i] = outSize[i];
    imgScale[i] = this->m_ScaleCoefficients[i];
    localSize[i] = (blockSize <= outSize[i]) ? blockSize : 1;
    globalSize[i] = localSize[i]*(unsigned int)ceil( (float)outSize[i]/(float)localSize[i]); //
                                                                                             // total
                                                                                             // #
                                                                                             // of
                                                                                             // threads
    bufferSize *= globalSize[i]/localSize[i];
    numPixel *= imgSize[i];
    }

  // Initialize & Allocate GPU Buffer
  if(bufferSize != this->m_AnisotropicDiffusionFunctionGPUBuffer->GetBufferSize() )
    {
    this->m_AnisotropicDiffusionFunctionGPUBuffer->Initialize();
    this->m_AnisotropicDiffusionFunctionGPUBuffer->SetBufferSize( sizeof(float)*bufferSize );
    this->m_AnisotropicDiffusionFunctionGPUBuffer->Allocate();
    }

  typename GPUKernelManager::Pointer kernelManager = this->m_AnisotropicDiffusionFunctionGPUKernelManager;
  int                                kernelHandle = this->m_AverageGradientMagnitudeSquaredGPUKernelHandle;

  // Set arguments
  int argidx = 0;
  kernelManager->SetKernelArgWithImage(kernelHandle, argidx++, inPtr->GetGPUDataManager() );
  kernelManager->SetKernelArgWithImage(kernelHandle, argidx++, this->m_AnisotropicDiffusionFunctionGPUBuffer);

  // Set shared memory args
  if (ImageDim == 2)
  {
    kernelManager->SetKernelArg(kernelHandle, argidx++, sizeof(float) * localSize[0] * localSize[1], ITK_NULLPTR);
    kernelManager->SetKernelArg(kernelHandle, argidx++, sizeof(float) * localSize[0], ITK_NULLPTR);
    kernelManager->SetKernelArg(kernelHandle, argidx++, sizeof(float) * localSize[0], ITK_NULLPTR);
    kernelManager->SetKernelArg(kernelHandle, argidx++, sizeof(float) * localSize[1], ITK_NULLPTR);
    kernelManager->SetKernelArg(kernelHandle, argidx++, sizeof(float) * localSize[1], ITK_NULLPTR);
  }
  else if (ImageDim == 3)
  {
    kernelManager->SetKernelArg(kernelHandle, argidx++, sizeof(float) * localSize[0] * localSize[1] * localSize[2], ITK_NULLPTR);
    kernelManager->SetKernelArg(kernelHandle, argidx++, sizeof(float) * localSize[0] * localSize[1], ITK_NULLPTR);
    kernelManager->SetKernelArg(kernelHandle, argidx++, sizeof(float) * localSize[0] * localSize[1], ITK_NULLPTR);
    kernelManager->SetKernelArg(kernelHandle, argidx++, sizeof(float) * localSize[0] * localSize[2], ITK_NULLPTR);
    kernelManager->SetKernelArg(kernelHandle, argidx++, sizeof(float) * localSize[0] * localSize[2], ITK_NULLPTR);
    kernelManager->SetKernelArg(kernelHandle, argidx++, sizeof(float) * localSize[1] * localSize[2], ITK_NULLPTR);
    kernelManager->SetKernelArg(kernelHandle, argidx++, sizeof(float) * localSize[1] * localSize[2], ITK_NULLPTR);
  }
  else
  {
    // no need to set shared memory args when dimension is not 2 or 3
  }

  // Set filter scale parameter
  for(int i=0; i<ImageDim; i++)
    {
    kernelManager->SetKernelArg(kernelHandle, argidx++, sizeof(float), &(imgScale[i]) );
    }

  // Set image size
  for(int i=0; i<ImageDim; i++)
    {
    kernelManager->SetKernelArg(kernelHandle, argidx++, sizeof(int), &(imgSize[i]) );
    }

  // launch kernel
  kernelManager->LaunchKernel(kernelHandle, ImageDim, globalSize, localSize );

  // Read back intermediate sums from GPU and compute final value
  double sum = 0;
  float *intermSum = new float[bufferSize];

  this->m_AnisotropicDiffusionFunctionGPUBuffer->SetCPUBufferPointer( intermSum );
  this->m_AnisotropicDiffusionFunctionGPUBuffer->SetCPUDirtyFlag( true );   //
                                                                            // CPU
                                                                            // is
                                                                            // dirty
  this->m_AnisotropicDiffusionFunctionGPUBuffer->SetGPUDirtyFlag( false );
  this->m_AnisotropicDiffusionFunctionGPUBuffer->UpdateCPUBuffer();   //
                                                                            // Copy
                                                                            // GPU->CPU

  for(int i=0; i<(int)bufferSize; i++)
    {
    sum += (double)intermSum[i];
    }

  this->SetAverageGradientMagnitudeSquared( (double)( sum / (double)numPixel ) );

  delete[] intermSum;
}

} // end namespace itk

#endif
