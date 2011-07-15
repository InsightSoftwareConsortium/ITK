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

//
// GPUImage Data Management
//
// This class will take care of synchronization between CPU Image and GPU Image
//

#ifndef __itkGPUImageDataManager_h
#define __itkGPUImageDataManager_h

#include <itkObject.h>
#include <itkTimeStamp.h>
#include <itkLightObject.h>
#include <itkObjectFactory.h>
#include "itkOclUtil.h"
#include "itkGPUImage.h"
#include "itkGPUDataManager.h"
#include "itkGPUContextManager.h"
#include "itkSimpleFastMutexLock.h"

namespace itk
{
  template < class TPixel, unsigned int NDimension > class GPUImage;

  template < class ImageType >
  class ITK_EXPORT GPUImageDataManager : public GPUDataManager
  {
    // allow GPUKernelManager to access GPU buffer pointer
    friend class GPUKernelManager;
    friend class GPUImage< typename ImageType::PixelType, ImageType::ImageDimension >;

  public:

    typedef GPUImageDataManager        Self;
    typedef GPUDataManager             Superclass;
    typedef SmartPointer<Self>         Pointer;
    typedef SmartPointer<const Self>   ConstPointer;

    itkNewMacro(Self);
    itkTypeMacro(GPUImageDataManager, GPUDataManager);

    void SetImagePointer( typename ImageType::Pointer img );

    // actual GPU->CPU memory copy takes place here
    virtual void MakeCPUBufferUpToDate();

    // actual CPU->GPU memory copy takes place here
    virtual void MakeGPUBufferUpToDate();

  protected:

    GPUImageDataManager() { m_Image = NULL; };
    virtual ~GPUImageDataManager() {};

  private:
    GPUImageDataManager(const Self&); //purposely not implemented
    void operator=(const Self&);

    typename ImageType::Pointer m_Image;

  };

} // namespace itk


#if ITK_TEMPLATE_TXX
#include "itkGPUImageDataManager.hxx"
#endif

#endif
