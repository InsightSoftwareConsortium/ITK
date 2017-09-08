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
#ifndef itkGPUImage_hxx
#define itkGPUImage_hxx

#include "itkGPUImage.h"

namespace itk
{
template <typename TPixel, unsigned int VImageDimension>
GPUImage< TPixel, VImageDimension >::GPUImage()
{
  m_DataManager = GPUImageDataManager< GPUImage< TPixel, VImageDimension > >::New();
  m_DataManager->SetTimeStamp( this->GetTimeStamp() );
}

template <typename TPixel, unsigned int VImageDimension>
GPUImage< TPixel, VImageDimension >::~GPUImage()
{

}

template <typename TPixel, unsigned int VImageDimension>
void GPUImage< TPixel, VImageDimension >::Allocate(bool initialize)
{
  // allocate CPU memory - calling Allocate() in superclass
  Superclass::Allocate(initialize);

  // allocate GPU memory
  this->ComputeOffsetTable();
  unsigned long numPixel = this->GetOffsetTable()[VImageDimension];
  m_DataManager->SetBufferSize( sizeof(TPixel)*numPixel );
  m_DataManager->SetImagePointer( this );
  m_DataManager->SetCPUBufferPointer( Superclass::GetBufferPointer() );
  m_DataManager->Allocate();

  /* prevent unnecessary copy from CPU to GPU at the beginning */
  m_DataManager->SetTimeStamp( this->GetTimeStamp() );
}

template< typename TPixel, unsigned int VImageDimension >
void GPUImage< TPixel, VImageDimension >::Initialize()
{
  // CPU image initialize
  Superclass::Initialize();

  // GPU image initialize
  m_DataManager->Initialize();
  this->ComputeOffsetTable();
  unsigned long numPixel = this->GetOffsetTable()[VImageDimension];
  m_DataManager->SetBufferSize( sizeof(TPixel)*numPixel );
  m_DataManager->SetImagePointer( this );
  m_DataManager->SetCPUBufferPointer( Superclass::GetBufferPointer() );
  m_DataManager->Allocate();

  /* prevent unnecessary copy from CPU to GPU at the beginning */
  m_DataManager->SetTimeStamp( this->GetTimeStamp() );
}

template <typename TPixel, unsigned int VImageDimension>
void GPUImage< TPixel, VImageDimension >::FillBuffer(const TPixel & value)
{
  m_DataManager->SetGPUBufferDirty();
  Superclass::FillBuffer( value );
}

template <typename TPixel, unsigned int VImageDimension>
void GPUImage< TPixel, VImageDimension >::SetPixel(const IndexType & index, const TPixel & value)
{
  m_DataManager->SetGPUBufferDirty();
  Superclass::SetPixel( index, value );
}

template <typename TPixel, unsigned int VImageDimension>
const TPixel & GPUImage< TPixel, VImageDimension >::GetPixel(const IndexType & index) const
{
  m_DataManager->UpdateCPUBuffer();
  return Superclass::GetPixel( index );
}

template <typename TPixel, unsigned int VImageDimension>
TPixel & GPUImage< TPixel, VImageDimension >::GetPixel(const IndexType & index)
{
  /* Original version - very conservative
  m_DataManager->SetGPUBufferDirty();
  return Superclass::GetPixel( index );
  */
  m_DataManager->UpdateCPUBuffer();
  return Superclass::GetPixel( index );
}

template <typename TPixel, unsigned int VImageDimension>
TPixel & GPUImage< TPixel, VImageDimension >::operator[] (const IndexType &index)
  {
  /* Original version - very conservative
  m_DataManager->SetGPUBufferDirty();
  return Superclass::operator[]( index );
  */
  m_DataManager->UpdateCPUBuffer();
  return Superclass::operator[] ( index );
  }

template <typename TPixel, unsigned int VImageDimension>
const TPixel & GPUImage< TPixel, VImageDimension >::operator[] (const IndexType &index) const
  {
  m_DataManager->UpdateCPUBuffer();
  return Superclass::operator[] ( index );
  }

template <typename TPixel, unsigned int VImageDimension>
void GPUImage< TPixel, VImageDimension >::SetPixelContainer(PixelContainer *container)
{
  Superclass::SetPixelContainer( container );
  m_DataManager->SetCPUDirtyFlag( false );
  m_DataManager->SetGPUDirtyFlag( true );
}

template <typename TPixel, unsigned int VImageDimension>
void GPUImage< TPixel, VImageDimension >::UpdateBuffers()
{
  m_DataManager->UpdateCPUBuffer();
  m_DataManager->UpdateGPUBuffer();
}

template <typename TPixel, unsigned int VImageDimension>
TPixel* GPUImage< TPixel, VImageDimension >::GetBufferPointer()
{
  /* Original version - very conservative
   * Always set GPU dirty (even though pixel values are not modified)
  m_DataManager->SetGPUBufferDirty();
  return Superclass::GetBufferPointer();
  */

  /* less conservative version - if you modify pixel value using
   * this pointer then you must set the image as modified manually!!! */
  m_DataManager->UpdateCPUBuffer();
  return Superclass::GetBufferPointer();
}

template <typename TPixel, unsigned int VImageDimension>
const TPixel * GPUImage< TPixel, VImageDimension >::GetBufferPointer() const
{
  // const does not change buffer, but if CPU is dirty then make it up-to-date
  m_DataManager->UpdateCPUBuffer();
  return Superclass::GetBufferPointer();
}

template <typename TPixel, unsigned int VImageDimension>
GPUDataManager *
GPUImage< TPixel, VImageDimension >::GetGPUDataManager()
{
  return m_DataManager.GetPointer();
}

template <typename TPixel, unsigned int VImageDimension>
void
GPUImage< TPixel, VImageDimension >::Graft(const Self *data)
{
  typedef GPUImageDataManager< GPUImage >              GPUImageDataManagerType;

  GPUImageDataManagerType* ptr = const_cast<GPUImageDataManagerType*>( data->GetDataManager() );

  // call the superclass' implementation
  Superclass::Graft(ptr->GetImagePointer());

  // call GPU data graft function
  m_DataManager->SetImagePointer( this );
  m_DataManager->Graft( ptr );

  // Synchronize timestamp of GPUImage and GPUDataManager
  m_DataManager->SetTimeStamp( this->GetTimeStamp() );
}

template <typename TPixel, unsigned int VImageDimension>
void
GPUImage< TPixel, VImageDimension >::Graft(const DataObject *data)
{
  const Self* ptr = dynamic_cast<const Self*>(data);
  if(ptr)
    {
    this->Graft(ptr);
    }
  else
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::GPUImage::Graft() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( const Self * ).name() );
    }
}

} // namespace itk

#endif
