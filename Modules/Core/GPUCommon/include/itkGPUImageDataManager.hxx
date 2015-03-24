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
#ifndef itkGPUImageDataManager_hxx
#define itkGPUImageDataManager_hxx

#include "itkGPUImageDataManager.h"
#include "itkOpenCLUtil.h"
//#define VERBOSE

namespace itk
{
template < typename ImageType >
void GPUImageDataManager< ImageType >::SetImagePointer( typename ImageType::Pointer img )
{
  m_Image = img.GetPointer();

  typedef typename ImageType::RegionType RegionType;
  typedef typename ImageType::IndexType  IndexType;
  typedef typename ImageType::SizeType   SizeType;

  RegionType region = m_Image->GetBufferedRegion();
  IndexType  index  = region.GetIndex();
  SizeType   size   = region.GetSize();

  for (unsigned int d = 0; d < ImageDimension; d++)
    {
    m_BufferedRegionIndex[d] = index[d];
    m_BufferedRegionSize[d] = size[d];
    }

  m_GPUBufferedRegionIndex = GPUDataManager::New();
  m_GPUBufferedRegionIndex->SetBufferSize( sizeof(int) * ImageDimension );
  m_GPUBufferedRegionIndex->SetCPUBufferPointer( m_BufferedRegionIndex );
  m_GPUBufferedRegionIndex->SetBufferFlag( CL_MEM_READ_ONLY );
  m_GPUBufferedRegionIndex->Allocate();
  m_GPUBufferedRegionIndex->SetGPUDirtyFlag(true);

  m_GPUBufferedRegionSize = GPUDataManager::New();
  m_GPUBufferedRegionSize->SetBufferSize( sizeof(int) * ImageDimension );
  m_GPUBufferedRegionSize->SetCPUBufferPointer( m_BufferedRegionSize );
  m_GPUBufferedRegionSize->SetBufferFlag( CL_MEM_READ_ONLY );
  m_GPUBufferedRegionSize->Allocate();
  m_GPUBufferedRegionSize->SetGPUDirtyFlag(true);

}

template < typename ImageType >
void GPUImageDataManager< ImageType >::MakeCPUBufferUpToDate()
{
  if( m_Image.IsNotNull() )
    {
    m_Mutex.Lock();

    ModifiedTimeType gpu_time = this->GetMTime();
    TimeStamp     cpu_time_stamp = m_Image->GetTimeStamp();
    ModifiedTimeType cpu_time = cpu_time_stamp.GetMTime();

    /* Why we check dirty flag and time stamp together?
     * Because existing CPU image filters do not use pixel/buffer
     * access function in GPUImage and therefore dirty flag is not
     * correctly managed. Therefore, we check the time stamp of
     * CPU and GPU data as well
     */
    if( (m_IsCPUBufferDirty || (gpu_time > cpu_time) ) && m_GPUBuffer != ITK_NULLPTR && m_CPUBuffer != ITK_NULLPTR )
      {
      cl_int errid;
      itkDebugMacro(<< "GPU->CPU data copy" );
      errid = clEnqueueReadBuffer(m_ContextManager->GetCommandQueue(
                                    m_CommandQueueId), m_GPUBuffer, CL_TRUE, 0, m_BufferSize, m_CPUBuffer, 0, ITK_NULLPTR,
                                  ITK_NULLPTR);
      OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);

      m_Image->Modified();
      this->SetTimeStamp( m_Image->GetTimeStamp() );

      m_IsCPUBufferDirty = false;
      m_IsGPUBufferDirty = false;
      }

    m_Mutex.Unlock();
    }
}

template < typename ImageType >
void GPUImageDataManager< ImageType >::MakeGPUBufferUpToDate()
{
  if( m_Image.IsNotNull() )
    {
    m_Mutex.Lock();

    ModifiedTimeType gpu_time = this->GetMTime();
    TimeStamp     cpu_time_stamp = m_Image->GetTimeStamp();
    ModifiedTimeType cpu_time = m_Image->GetMTime();

    /* Why we check dirty flag and time stamp together?
    * Because existing CPU image filters do not use pixel/buffer
    * access function in GPUImage and therefore dirty flag is not
    * correctly managed. Therefore, we check the time stamp of
    * CPU and GPU data as well
    */
    if( (m_IsGPUBufferDirty || (gpu_time < cpu_time) ) && m_CPUBuffer != ITK_NULLPTR && m_GPUBuffer != ITK_NULLPTR )
      {
      cl_int errid;
      itkDebugMacro(<< "CPU->GPU data copy");
      errid = clEnqueueWriteBuffer(m_ContextManager->GetCommandQueue(
                                     m_CommandQueueId), m_GPUBuffer, CL_TRUE, 0, m_BufferSize, m_CPUBuffer, 0, ITK_NULLPTR,
                                   ITK_NULLPTR);
      OpenCLCheckError(errid, __FILE__, __LINE__, ITK_LOCATION);

      this->SetTimeStamp( cpu_time_stamp );

      m_IsCPUBufferDirty = false;
      m_IsGPUBufferDirty = false;
      }

    m_Mutex.Unlock();
    }
}

} // namespace itk

#endif
