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

#ifndef __itkGPUImageDataManager_h
#define __itkGPUImageDataManager_h

#include <itkObject.h>
#include <itkTimeStamp.h>
#include <itkLightObject.h>
#include <itkObjectFactory.h>
#include "itkOpenCLUtil.h"
#include "itkGPUDataManager.h"
#include "itkGPUContextManager.h"
#include "itkSimpleFastMutexLock.h"

namespace itk
{
template < class TPixel, unsigned int NDimension > class GPUImage;

/**
 * \class GPUImageDataManager
 *
 * DataManager for GPUImage. This class will take care of data synchronization
 * between CPU Image and GPU Image.
 *
 * \ingroup ITKGPUCommon
 */
template < class ImageType >
class ITK_EXPORT GPUImageDataManager : public GPUDataManager
{
  // allow GPUKernelManager to access GPU buffer pointer
  friend class GPUKernelManager;
  friend class GPUImage< typename ImageType::PixelType, ImageType::ImageDimension >;

public:
  typedef GPUImageDataManager      Self;
  typedef GPUDataManager           Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  itkNewMacro(Self);
  itkTypeMacro(GPUImageDataManager, GPUDataManager);

  static const unsigned int        ImageDimension = ImageType::ImageDimension;

  itkGetModifiableObjectMacro(GPUBufferedRegionIndex, GPUDataManager);
  itkGetModifiableObjectMacro(GPUBufferedRegionSize, GPUDataManager);

  void SetImagePointer( typename ImageType::Pointer img );
  ImageType *GetImagePointer()
    {
    return this->m_Image.GetPointer();
    }

  /** actual GPU->CPU memory copy takes place here */
  virtual void MakeCPUBufferUpToDate();

  /** actual CPU->GPU memory copy takes place here */
  virtual void MakeGPUBufferUpToDate();

  /** Grafting GPU Image Data */
  virtual void Graft(const GPUDataManager* data);

protected:
  GPUImageDataManager() {}
  virtual ~GPUImageDataManager() {}

private:
  GPUImageDataManager(const Self&);   //purposely not implemented
  void operator=(const Self&);

  typename ImageType::Pointer       m_Image;
  int                               m_BufferedRegionIndex[ImageType::ImageDimension];
  int                               m_BufferedRegionSize[ImageType::ImageDimension];
  typename GPUDataManager::Pointer  m_GPUBufferedRegionIndex;
  typename GPUDataManager::Pointer  m_GPUBufferedRegionSize;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGPUImageDataManager.hxx"
#endif

#endif
