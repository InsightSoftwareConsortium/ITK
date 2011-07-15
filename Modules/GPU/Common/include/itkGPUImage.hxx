#ifndef __itkGPUImage_hxx
#define __itkGPUImage_hxx

#include "itkGPUImage.h"

namespace itk
{
  //
  // Constructor
  //
  template <class TPixel, unsigned int VImageDimension>
  GPUImage< TPixel, VImageDimension >::GPUImage()
  {
    m_GPUManager = GPUImageDataManager< GPUImage< TPixel, VImageDimension > >::New();
    m_GPUManager->SetTimeStamp( this->GetTimeStamp() );
  }

  template <class TPixel, unsigned int VImageDimension>
  GPUImage< TPixel, VImageDimension >::~GPUImage()
  {

  }

  template <class TPixel, unsigned int VImageDimension>
  void GPUImage< TPixel, VImageDimension >::Allocate()
  {
    // allocate CPU memory - calling Allocate() in superclass
    Superclass::Allocate();

    // allocate GPU memory
    this->ComputeOffsetTable();
    unsigned long numPixel = this->GetOffsetTable()[VImageDimension];
    m_GPUManager->SetBufferSize( sizeof(TPixel)*numPixel );
    m_GPUManager->SetImagePointer( this );
    m_GPUManager->SetCPUBufferPointer( Superclass::GetBufferPointer() );
    m_GPUManager->Allocate();

    /* prevent unnecessary copy from CPU to GPU at the beginning */
    m_GPUManager->SetTimeStamp( this->GetTimeStamp() );
  }


  template< class TPixel, unsigned int VImageDimension >
  void GPUImage< TPixel, VImageDimension >::Initialize()
  {
    Superclass::Initialize();
    m_GPUManager->SetCPUDirtyFlag( false );
    m_GPUManager->SetGPUDirtyFlag( true );
  }

  template <class TPixel, unsigned int VImageDimension>
  void GPUImage< TPixel, VImageDimension >::FillBuffer(const TPixel & value)
  {
    m_GPUManager->SetGPUBufferDirty();
    Superclass::FillBuffer( value );
  }

  template <class TPixel, unsigned int VImageDimension>
  void GPUImage< TPixel, VImageDimension >::SetPixel(const IndexType & index, const TPixel & value)
  {
    m_GPUManager->SetGPUBufferDirty();
    Superclass::SetPixel( index, value );
  }

  template <class TPixel, unsigned int VImageDimension>
  TPixel & GPUImage< TPixel, VImageDimension >::GetPixel(const IndexType & index)
  {
    m_GPUManager->SetGPUBufferDirty();
    return Superclass::GetPixel( index );
  }

  template <class TPixel, unsigned int VImageDimension>
  TPixel & GPUImage< TPixel, VImageDimension >::operator[](const IndexType & index)
  {
    m_GPUManager->SetGPUBufferDirty();
    return Superclass::operator[]( index );
  }

  template <class TPixel, unsigned int VImageDimension>
  void GPUImage< TPixel, VImageDimension >::SetPixelContainer(PixelContainer *container)
  {
    Superclass::SetPixelContainer( container );
    m_GPUManager->SetCPUDirtyFlag( false );
    m_GPUManager->SetGPUDirtyFlag( true );
  }

  template <class TPixel, unsigned int VImageDimension>
  TPixel* GPUImage< TPixel, VImageDimension >::GetBufferPointer()
  {
    m_GPUManager->SetGPUBufferDirty();
    return Superclass::GetBufferPointer();
  }

  template <class TPixel, unsigned int VImageDimension>
  const TPixel * GPUImage< TPixel, VImageDimension >::GetBufferPointer() const
  {
    // const does not change buffer, but if CPU is dirty then make it up-to-date
    m_GPUManager->MakeCPUBufferUpToDate();
    return Superclass::GetBufferPointer();
  }

  template <class TPixel, unsigned int VImageDimension>
  GPUDataManager::Pointer
  GPUImage< TPixel, VImageDimension >::GetGPUDataManager()
  {
    typedef typename GPUImageDataManager< GPUImage >::Superclass GPUImageSuperclass;
    typedef typename GPUImageSuperclass::Pointer                 GPUImageSuperclassPointer;

    return static_cast< GPUImageSuperclassPointer >( m_GPUManager.GetPointer() );
  }

} // namespace itk

#endif
