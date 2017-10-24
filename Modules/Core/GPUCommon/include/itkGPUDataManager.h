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

#ifndef itkGPUDataManager_h
#define itkGPUDataManager_h

#include "itkObject.h"
#include "itkDataObject.h"
#include "itkObjectFactory.h"
#include "itkOpenCLUtil.h"
#include "itkGPUContextManager.h"
#include "itkSimpleFastMutexLock.h"
#include "itkMutexLockHolder.h"

namespace itk
{
/** \class GPUDataManager
 * \brief GPU memory manager implemented using OpenCL. Required by GPUImage class.
 *
 * This class serves as a base class for GPU data container for GPUImage class,
 * which is similar to ImageBase class for Image class. However, all the image-related
 * meta data will be already stored in image class (parent of GPUImage), therefore
 * we did not name it GPUImageBase. Rather, this class is a GPU-specific data manager
 * that provides functionalties for CPU-GPU data synchronization and grafting GPU data.
 *
 * \ingroup ITKGPUCommon
 */
class GPUDataManager : public Object   //DataObject//
{
  /** allow GPUKernelManager to access GPU buffer pointer */
  friend class GPUKernelManager;

public:

  typedef GPUDataManager           Self;
  typedef Object                   Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  itkNewMacro(Self);
  itkTypeMacro(GPUDataManager, Object);

  typedef MutexLockHolder<SimpleFastMutexLock> MutexHolderType;

  /** total buffer size in bytes */
  void SetBufferSize( unsigned int num );

  unsigned int GetBufferSize() {
    return m_BufferSize;
  }

  void SetBufferFlag( cl_mem_flags flags );

  void SetCPUBufferPointer( void* ptr );

  void SetCPUDirtyFlag( bool isDirty );

  void SetGPUDirtyFlag( bool isDirty );

  /** Make GPU up-to-date and mark CPU as dirty.
   * Call this function when you want to modify CPU data */
  void SetCPUBufferDirty();

  /** Make CPU up-to-date and mark GPU as dirty.
   * Call this function when you want to modify GPU data */
  void SetGPUBufferDirty();

  bool IsCPUBufferDirty() {
    return m_IsCPUBufferDirty;
  }

  bool IsGPUBufferDirty() {
    return m_IsGPUBufferDirty;
  }

  /** actual GPU->CPU memory copy takes place here */
  virtual void UpdateCPUBuffer();

  /** actual CPU->GPU memory copy takes place here */
  virtual void UpdateGPUBuffer();

  void Allocate();

  void SetCurrentCommandQueue( int queueid );

  int  GetCurrentCommandQueueID();

  /** Synchronize CPU and GPU buffers (using dirty flags) */
  bool Update();

  /** Method for grafting the content of one GPUDataManager into another one */
  virtual void Graft(const GPUDataManager* data);

  /** Initialize GPUDataManager */
  virtual void Initialize();

  /** Get GPU buffer pointer */
  cl_mem* GetGPUBufferPointer();

  /** Get GPU buffer pointer */
  void* GetCPUBufferPointer();

protected:

  GPUDataManager();
  virtual ~GPUDataManager() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

protected:
  /* NOTE: ivars are protected instead of private to improve performance access in child classes*/

  unsigned int m_BufferSize;   // # of bytes

  GPUContextManager *m_ContextManager;

  int m_CommandQueueId;

  /** buffer type */
  cl_mem_flags m_MemFlags;

  /** buffer pointers */
  cl_mem m_GPUBuffer;
  void*  m_CPUBuffer;

  /** checks if buffer needs to be updated */
  bool m_IsGPUBufferDirty;
  bool m_IsCPUBufferDirty;

  /** Mutex lock to prevent r/w hazard for multithreaded code */
  SimpleFastMutexLock m_Mutex;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUDataManager);
};

} // namespace itk

#endif
