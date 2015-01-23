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
#ifndef itkGPUContextManager_h
#define itkGPUContextManager_h

#include "itkOpenCLUtil.h"
#include <itkLightObject.h>

namespace itk
{
/** \class GPUContextManager
 *
 * \brief Singleton class to store the GPU context.
 *
 *  Won-Ki to write more documentation here...
 *
 * \ingroup ITKGPUCommon
 */
class GPUContextManager : public LightObject
{
public:

  static GPUContextManager* GetInstance();

  void DestroyInstance();

  cl_command_queue GetCommandQueue(int i);

  unsigned int GetNumberOfCommandQueues() {
    return m_NumberOfDevices;
  }

  cl_context GetCurrentContext() {
    return m_Context;
  }

  cl_device_id GetDeviceId(int i);

private:

  GPUContextManager();
  ~GPUContextManager();

  cl_platform_id    m_Platform;
  cl_context        m_Context;
  cl_device_id*     m_Devices;
  cl_command_queue* m_CommandQueue;    // one queue per device

  cl_uint m_NumberOfDevices, m_NumberOfPlatforms;

  static GPUContextManager* m_Instance;
};
} // namespace itk

#endif
