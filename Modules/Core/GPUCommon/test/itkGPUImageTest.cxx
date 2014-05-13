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

/**
 * Test program for itkGPUImage class.
 * This program shows how to use GPU image and GPU program.
 */
//#include "pathToOpenCLSourceCode.h"
#include "itkGPUImage.h"
#include "itkGPUKernelManager.h"
#include "itkGPUContextManager.h"
#include "itkGPUImageOps.h"

typedef itk::GPUImage<float, 2> ItkImage1f;


int itkGPUImageTest(int argc, char *argv[])
{
  if (argc > 1)
  {
    std::cout << "received " << argc << " arguments, but didn't expect any."
              << "first ignored argument: " << argv[1] << std::endl;
  }
  unsigned int width, height;

  width  = 256;
  height = 256;

  //
  // create GPUImage
  //

  // set size & region
  ItkImage1f::Pointer srcA, srcB, dest;

  ItkImage1f::IndexType start;
  start[0] = 0;
  start[1] = 0;
  ItkImage1f::SizeType size;
  size[0] = width;
  size[1] = height;
  ItkImage1f::RegionType region;
  region.SetSize( size );
  region.SetIndex( start );

  // create
  srcA = ItkImage1f::New();
  srcA->SetRegions( region );
  srcA->Allocate();
  srcA->FillBuffer( 1.0f );

  srcB = ItkImage1f::New();
  srcB->SetRegions( region );
  srcB->Allocate();
  srcB->FillBuffer( 3.0f );

  dest = ItkImage1f::New();
  dest->SetRegions( region );
  dest->Allocate(true); // initialize buffer to zero

  // check pixel value
  ItkImage1f::IndexType idx;
  idx[0] = 0;
  idx[1] = 0;


  unsigned int nElem = width*height;

  //
  // create GPU program object
  //
  itk::GPUKernelManager::Pointer kernelManager = itk::GPUKernelManager::New();

  // load program and compile

  const char* GPUSource = itk::GPUImageOps::GetOpenCLSource();
  kernelManager->LoadProgramFromString(GPUSource, "#define PIXELTYPE float\n");

  //
  // create addition kernel
  //
  int kernel_add = kernelManager->CreateKernel("ImageAdd");

  srcA->SetCurrentCommandQueue( 0 );
  srcB->SetCurrentCommandQueue( 0 );
  dest->SetCurrentCommandQueue( 0 );
  kernelManager->SetCurrentCommandQueue( 0 );

  std::cout << "Current Command Queue ID : 0 " << std::endl;

  std::cout << "======================" << std::endl;
  std::cout << "Kernel : Addition" << std::endl;
  std::cout << "------------------" << std::endl;
  std::cout << "Before GPU kernel execution" << std::endl;
  std::cout << "SrcA : " << srcA->GetPixel( idx ) << std::endl;
  std::cout << "SrcB : " << srcB->GetPixel( idx ) << std::endl;
  std::cout << "Dest : " << dest->GetPixel( idx ) << std::endl;

  kernelManager->SetKernelArgWithImage(kernel_add, 0, srcA->GetGPUDataManager());
  kernelManager->SetKernelArgWithImage(kernel_add, 1, srcB->GetGPUDataManager());
  kernelManager->SetKernelArgWithImage(kernel_add, 2, dest->GetGPUDataManager());
  kernelManager->SetKernelArg(kernel_add, 3, sizeof(unsigned int), &nElem);
  kernelManager->LaunchKernel2D(kernel_add, width, height, 16, 16);

  std::cout << "------------------" << std::endl;
  std::cout << "After GPU kernel execution" << std::endl;
  std::cout << "SrcA : " << srcA->GetPixel( idx ) << std::endl;
  std::cout << "SrcB : " << srcB->GetPixel( idx ) << std::endl;
  std::cout << "Des  : " << dest->GetPixel( idx ) << std::endl;
  std::cout << "======================" << std::endl;

  //
  // create multiplication kernel
  //
  int kernel_mult = kernelManager->CreateKernel("ImageMult");

  std::cout << "======================" << std::endl;
  std::cout << "Kernel : Multiplication" << std::endl;
  std::cout << "------------------" << std::endl;
  std::cout << "Before GPU kernel execution" << std::endl;
  std::cout << "SrcA : " << srcA->GetPixel( idx ) << std::endl;
  std::cout << "SrcB : " << srcB->GetPixel( idx ) << std::endl;
  std::cout << "Dest : " << dest->GetPixel( idx ) << std::endl;

  kernelManager->SetKernelArgWithImage(kernel_mult, 0, srcA->GetGPUDataManager());
  kernelManager->SetKernelArgWithImage(kernel_mult, 1, srcB->GetGPUDataManager());
  kernelManager->SetKernelArgWithImage(kernel_mult, 2, dest->GetGPUDataManager());
  kernelManager->SetKernelArg(kernel_mult, 3, sizeof(unsigned int), &nElem);
  kernelManager->LaunchKernel2D(kernel_mult, width, height, 16, 16);

  std::cout << "------------------" << std::endl;
  std::cout << "After GPU kernel execution" << std::endl;
  std::cout << "SrcA : " << srcA->GetPixel( idx ) << std::endl;
  std::cout << "SrcB : " << srcB->GetPixel( idx ) << std::endl;
  std::cout << "Des  : " << dest->GetPixel( idx ) << std::endl;
  std::cout << "======================" << std::endl;

  //
  // Change Command Queue if more than one GPU device exists
  // otherwise, use same command queue
  //
  unsigned int queueID = 0;
  itk::GPUContextManager *contextManager = itk::GPUContextManager::GetInstance();
  if(contextManager->GetNumberOfCommandQueues() >= 2)
  {
    queueID = 1;
    std::cout << "More than one GPU device available, switching command queues."
              << std::endl;
  }
  else
  {
    std::cout << "Only one GPU device available, using same command queue."
              << std::endl;
  }

  std::cout << "Current Command Queue ID : " << queueID << std::endl;

  //
  // create subtraction kernel
  //
  int kernel_sub = kernelManager->CreateKernel("ImageSub");

  srcA->FillBuffer( 2.0f );
  srcB->FillBuffer( 4.0f );
  dest->FillBuffer( 1.0f );

  // default queue id was 0
  srcA->SetCurrentCommandQueue( queueID );
  srcB->SetCurrentCommandQueue( queueID );
  dest->SetCurrentCommandQueue( queueID );
  kernelManager->SetCurrentCommandQueue( queueID );

  std::cout << "======================" << std::endl;
  std::cout << "Kernel : Subtraction" << std::endl;
  std::cout << "------------------" << std::endl;
  std::cout << "Before GPU kernel execution" << std::endl;
  std::cout << "SrcA : " << srcA->GetPixel( idx ) << std::endl;
  std::cout << "SrcB : " << srcB->GetPixel( idx ) << std::endl;
  std::cout << "Dest : " << dest->GetPixel( idx ) << std::endl;

  kernelManager->SetKernelArgWithImage(kernel_sub, 0, srcA->GetGPUDataManager());
  kernelManager->SetKernelArgWithImage(kernel_sub, 1, srcB->GetGPUDataManager());
  kernelManager->SetKernelArgWithImage(kernel_sub, 2, dest->GetGPUDataManager());
  kernelManager->SetKernelArg(kernel_sub, 3, sizeof(unsigned int), &nElem);
  kernelManager->LaunchKernel2D(kernel_sub, width, height, 16, 16);

  std::cout << "------------------" << std::endl;
  std::cout << "After GPU kernel execution" << std::endl;
  std::cout << "SrcA : " << srcA->GetPixel( idx ) << std::endl;
  std::cout << "SrcB : " << srcB->GetPixel( idx ) << std::endl;
  std::cout << "Des  : " << dest->GetPixel( idx ) << std::endl;
  std::cout << "======================" << std::endl;

  return EXIT_SUCCESS;
}
