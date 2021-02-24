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
#ifndef itkGPUImageOps_h
#define itkGPUImageOps_h

#include "itkMacro.h"

namespace itk
{
/** Create a helper GPU Kernel class for GPUImageOps */
itkGPUKernelClassMacro(GPUImageOpsKernel);

/** \class GPUImageOps
 *
 * \brief Provides the kernels for some basic GPU Image Operations
 *
 * \ingroup ITKGPUCommon
 */
class GPUImageOps
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GPUImageOps);
  GPUImageOps() = default;
  virtual ~GPUImageOps() = default;

  /** Standard class type aliases. */
  using Self = GPUImageOps;

  /** Get OpenCL Kernel source as a string, creates a GetOpenCLSource method */
  itkGetOpenCLSourceFromKernelMacro(GPUImageOpsKernel);
};
} // end of namespace itk

#endif
