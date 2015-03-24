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
#include "itkGPUReduction.h"

int itkGPUReductionTest(int argc, char *argv[])
{
  if (argc > 2)
  {
    std::cout << "received " << argc << " arguments, but didn't expect any more than 1."
              << "first ignored argument: " << argv[2] << std::endl;
  }
  int numPixels = 256;
  if (argc > 1)
  {
    numPixels  = atoi(argv[1]);
  }

  // create input
  typedef int ElementType;

  itk::GPUReduction<ElementType>::Pointer summer = itk::GPUReduction<ElementType>::New();
  summer->InitializeKernel(numPixels);
  unsigned int bytes = numPixels * sizeof(ElementType);
  ElementType*    h_idata = (ElementType*)malloc(bytes);

  for(int ii=0; ii<numPixels; ii++)
  {
    h_idata[ii] = 1;
  }
  summer->AllocateGPUInputBuffer(h_idata);
  summer->GPUGenerateData();
  int GPUsum = summer->GetGPUResult();
  summer->ReleaseGPUInputBuffer();
  int status = EXIT_FAILURE;
  if (GPUsum == static_cast<int>(numPixels))
  {
    std::cout << "GPU reduction to sum passed, sum = " << GPUsum << ", numPixels = " << numPixels << std::endl;
    status = EXIT_SUCCESS;
  }
  else
  {
    std::cout << "Expected sum to be " << numPixels << ", GPUReduction computed " << GPUsum << " which is wrong." << std::endl;
    status = EXIT_FAILURE;
  }
  int CPUsum = summer->CPUGenerateData(h_idata, numPixels);
  if (CPUsum == static_cast<int>(numPixels))
  {
    std::cout << "CPU reduction to sum passed, sum = " << CPUsum << ", numPixels = " << numPixels << std::endl;
  }
  else
  {
    std::cout << "Expected CPU sum to be " << numPixels << ", GPUReduction computed " << CPUsum << " which is wrong." << std::endl;
    status = EXIT_FAILURE;
  }
  summer = ITK_NULLPTR; // explicit GPU object destruction test
  itk::GPUContextManager::GetInstance()->DestroyInstance(); // GPUContextManager singleton destruction test
  return status;
}
