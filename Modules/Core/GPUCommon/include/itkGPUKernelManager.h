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

//
// GPU Kernel Manager Class
//

#ifndef itkGPUKernelManager_h
#define itkGPUKernelManager_h

#include <vector>
#include <itkLightObject.h>
#include <itkObjectFactory.h>
#include "itkOpenCLUtil.h"
#include "itkGPUImage.h"
#include "itkGPUContextManager.h"
#include "itkGPUDataManager.h"

namespace itk
{
/** \class GPUKernelManager
 * \brief GPU kernel manager implemented using OpenCL.
 *
 * This class is responsible for managing the GPU kernel and
 * command queue.
 *
 * \ingroup ITKGPUCommon
 */
class GPUKernelManager : public LightObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GPUKernelManager);

  struct KernelArgumentList
  {
    bool                    m_IsReady;
    GPUDataManager::Pointer m_GPUDataManager;
  };

  using Self = GPUKernelManager;
  using Superclass = LightObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkNewMacro(Self);
  itkTypeMacro(GPUKernelManager, LightObject);

  bool
  LoadProgramFromFile(const char * filename, const char * cPreamble = "");

  bool
  LoadProgramFromString(const char * cSource, const char * cPreamble = "");

  int
  CreateKernel(const char * kernelName);

  cl_int
  GetKernelWorkGroupInfo(int kernelIdx, cl_kernel_work_group_info paramName, void * value);

  cl_int
  GetDeviceInfo(cl_kernel_work_group_info paramName, size_t argSize, void * argValue);

  bool
  SetKernelArg(int kernelIdx, cl_uint argIdx, size_t argSize, const void * argVal);

  bool
  SetKernelArgWithChar(int kernelIdx, cl_uint argIdx, char argVal);

  bool
  SetKernelArgWithUChar(int kernelIdx, cl_uint argIdx, unsigned char argVal);

  bool
  SetKernelArgWithShort(int kernelIdx, cl_uint argIdx, short argVal);

  bool
  SetKernelArgWithUShort(int kernelIdx, cl_uint argIdx, unsigned short argVal);

  bool
  SetKernelArgWithInt(int kernelIdx, cl_uint argIdx, int argVal);

  bool
  SetKernelArgWithUInt(int kernelIdx, cl_uint argIdx, unsigned int argVal);

  bool
  SetKernelArgWithLongLong(int kernelIdx, cl_uint argIdx, long long argVal);

  bool
  SetKernelArgWithULongLong(int kernelIdx, cl_uint argIdx, unsigned long long argVal);

  bool
  SetKernelArgWithFloat(int kernelIdx, cl_uint argIdx, float argVal);

  bool
  SetKernelArgWithDouble(int kernelIdx, cl_uint argIdx, double argVal);

  bool
  SetKernelArgWithImage(int kernelIdx, cl_uint argIdx, GPUDataManager * manager);

  /** Pass to GPU both the pixel buffer and the buffered region. */
  // template< typename TGPUImageDataManager >
  // bool SetKernelArgWithImageAndBufferedRegion(int kernelIdx, cl_uint &argIdx,
  //  typename TGPUImageDataManager::Pointer manager);
  template <typename TGPUImageDataManager>
  bool
  SetKernelArgWithImageAndBufferedRegion(int kernelIdx, cl_uint & argIdx, TGPUImageDataManager * manager)
  {
    if (kernelIdx < 0 || kernelIdx >= (int)m_KernelContainer.size())
      return false;

    cl_int errid;

    errid = clSetKernelArg(m_KernelContainer[kernelIdx], argIdx, sizeof(cl_mem), manager->GetGPUBufferPointer());
    OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);

    m_KernelArgumentReady[kernelIdx][argIdx].m_IsReady = true;
    m_KernelArgumentReady[kernelIdx][argIdx].m_GPUDataManager = manager;
    argIdx++;

    // this->SetKernelArg(kernelIdx, argIdx++, sizeof(int), &(TGPUImageDataManager::ImageDimension) );

    // the starting index for the buffered region
    errid = clSetKernelArg(m_KernelContainer[kernelIdx],
                           argIdx,
                           sizeof(cl_mem),
                           manager->GetModifiableGPUBufferedRegionIndex()->GetGPUBufferPointer());
    OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);

    m_KernelArgumentReady[kernelIdx][argIdx].m_IsReady = true;
    m_KernelArgumentReady[kernelIdx][argIdx].m_GPUDataManager = manager->GetModifiableGPUBufferedRegionIndex();
    argIdx++;

    // the size for the buffered region
    errid = clSetKernelArg(m_KernelContainer[kernelIdx],
                           argIdx,
                           sizeof(cl_mem),
                           manager->GetModifiableGPUBufferedRegionSize()->GetGPUBufferPointer());
    OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);

    m_KernelArgumentReady[kernelIdx][argIdx].m_IsReady = true;
    m_KernelArgumentReady[kernelIdx][argIdx].m_GPUDataManager = manager->GetModifiableGPUBufferedRegionSize();
    argIdx++;

    return true;
  }

  bool
  LaunchKernel(int kernelIdx, int dim, size_t * globalWorkSize, size_t * localWorkSize);

  bool
  LaunchKernel1D(int kernelIdx, size_t globalWorkSize, size_t localWorkSize);

  bool
  LaunchKernel2D(int    kernelIdx,
                 size_t globalWorkSizeX,
                 size_t globalWorkSizeY,
                 size_t localWorkSizeX,
                 size_t localWorkSizeY);

  bool
  LaunchKernel3D(int    kernelIdx,
                 size_t globalWorkSizeX,
                 size_t globalWorkSizeY,
                 size_t globalWorkSizeZ,
                 size_t localWorkSizeX,
                 size_t localWorkSizeY,
                 size_t localWorkSizeZ);

  void
  SetCurrentCommandQueue(int queueid);

  int
  GetCurrentCommandQueueID() const;

protected:
  GPUKernelManager();
  ~GPUKernelManager() override;

  bool
  CheckArgumentReady(int kernelIdx);

  void
  ResetArguments(int kernelIdx);

private:
  cl_program m_Program;

  GPUContextManager * m_Manager;
  int                 m_CommandQueueId;

  std::vector<cl_kernel>                       m_KernelContainer;
  std::vector<std::vector<KernelArgumentList>> m_KernelArgumentReady;

  template <typename TArg>
  bool
  SetTypedKernelArg(int kernelIdx, cl_uint argIdx, TArg argVal);
};
} // namespace itk

#endif
