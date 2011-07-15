#ifndef __itkGPUContextManager_h
#define __itkGPUContextManager_h

#include "itkOclUtil.h"
#include <itkLightObject.h>


//
// Singleton class for GPUContextManager
//

/** \class GPUContextManager
 *
 * \brief Class to store the GPU context.
 *
 *  Won-Ki to write more documentation here...
 *
 * \ingroup ITKGPUCommon
 */
namespace itk
{
  class ITK_EXPORT GPUContextManager: public LightObject
  {
  public:

    static GPUContextManager* GetInstance();
    void DestroyInstance();

    cl_command_queue GetCommandQueue(int i);

    unsigned int GetNumCommandQueue() { return m_NumDevices; }

    cl_context GetCurrentContext() { return m_Context; }

    cl_device_id GetDeviceId(int i);

  private:

    GPUContextManager();
    ~GPUContextManager();

    cl_platform_id     m_Platform;
    cl_context         m_Context;
    cl_device_id*      m_Devices;
    cl_command_queue*  m_CommandQueue; // one queue per device

    cl_uint m_NumDevices, m_NumPlatforms;

    static GPUContextManager* m_Instance;
  };
} // namespace itk

#endif
