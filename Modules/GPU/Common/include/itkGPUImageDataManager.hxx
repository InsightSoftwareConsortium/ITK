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
#ifndef __itkGPUImageDataManager_hxx
#define __itkGPUImageDataManager_hxx

#include "itkGPUImageDataManager.h"


namespace itk
{
  template < class ImageType >
  void GPUImageDataManager< ImageType >::SetImagePointer( typename ImageType::Pointer img )
  {
    m_Image = img;
  }

  template < class ImageType >
  void GPUImageDataManager< ImageType >::MakeCPUBufferUpToDate()
  {
    if( m_Image.IsNotNull() )
    {
      m_Mutex.Lock();

      unsigned long gpu_time = this->GetMTime();
      TimeStamp cpu_time_stamp = m_Image->GetTimeStamp();
      unsigned long cpu_time = cpu_time_stamp.GetMTime();

      /* Why we check dirty flag and time stamp together?
       * Because existing CPU image filters do not use pixel/buffer
       * access function in GPUImage and therefore dirty flag is not
       * correctly managed. Therefore, we check the time stamp of
       * CPU and GPU data as well
       */
      if( (m_IsCPUBufferDirty || (gpu_time > cpu_time)) && m_GPUBuffer != NULL)
      {
        cl_int errid;

        std::cout << "GPU->CPU data copy" << std::endl;
        errid = clEnqueueReadBuffer(m_Manager->GetCommandQueue(m_CommandQueueId), m_GPUBuffer, CL_TRUE, 0, m_BufferSize, m_CPUBuffer, 0, NULL, NULL);
        OclCheckError(errid);

        m_Image->Modified();
        this->SetTimeStamp( m_Image->GetTimeStamp() );

        m_IsCPUBufferDirty = false;
        m_IsGPUBufferDirty = false;
      }

      m_Mutex.Unlock();
    }
  }

  template < class ImageType >
  void GPUImageDataManager< ImageType >::MakeGPUBufferUpToDate()
  {
    if( m_Image.IsNotNull() )
    {
      m_Mutex.Lock();

      unsigned long gpu_time = this->GetMTime();
      TimeStamp cpu_time_stamp = m_Image->GetTimeStamp();
      unsigned long cpu_time = m_Image->GetMTime();

      /* Why we check dirty flag and time stamp together?
      * Because existing CPU image filters do not use pixel/buffer
      * access function in GPUImage and therefore dirty flag is not
      * correctly managed. Therefore, we check the time stamp of
      * CPU and GPU data as well
      */
      if( (m_IsGPUBufferDirty || (gpu_time < cpu_time)) && m_CPUBuffer != NULL )
      {
        cl_int errid;

        std::cout << "CPU->GPU data copy" << std::endl;
        errid = clEnqueueWriteBuffer(m_Manager->GetCommandQueue(m_CommandQueueId), m_GPUBuffer, CL_TRUE, 0, m_BufferSize, m_CPUBuffer, 0, NULL, NULL);
        OclCheckError(errid);

        this->SetTimeStamp( cpu_time_stamp );

        m_IsCPUBufferDirty = false;
        m_IsGPUBufferDirty = false;
      }

      m_Mutex.Unlock();
    }
  }


} // namespace itk

#endif
