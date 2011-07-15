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

#ifndef __itkGPUImage_h
#define __itkGPUImage_h

#include "itkImage.h"
#include "itkGPUImageDataManager.h"
#include "itkVersion.h"
#include "itkObjectFactoryBase.h"

namespace itk
{
  template <class TPixel, unsigned int VImageDimension = 2>
  class ITK_EXPORT GPUImage : public Image<TPixel,VImageDimension>
  {
  public:
    typedef GPUImage                        Self;
    typedef Image<TPixel,VImageDimension>   Superclass;
    typedef SmartPointer<Self>              Pointer;
    typedef SmartPointer<const Self>        ConstPointer;
    typedef WeakPointer<const Self>         ConstWeakPointer;

    itkNewMacro(Self);

    itkTypeMacro(GPUImage, Image);

    itkStaticConstMacro(ImageDimension, unsigned int, VImageDimension);

    typedef typename Superclass::PixelType           PixelType;
    typedef typename Superclass::ValueType           ValueType;
    typedef typename Superclass::InternalPixelType   InternalPixelType;
    typedef typename Superclass::IOPixelType         IOPixelType;
    typedef typename Superclass::DirectionType       DirectionType;
    typedef typename Superclass::SpacingType         SpacingType;
    typedef typename Superclass::PixelContainer      PixelContainer;
    typedef typename Superclass::SizeType            SizeType;
    typedef typename Superclass::IndexType           IndexType;
    typedef typename Superclass::OffsetType          OffsetType;
    typedef typename Superclass::RegionType          RegionType;
    typedef typename PixelContainer::Pointer         PixelContainerPointer;
    typedef typename PixelContainer::ConstPointer    PixelContainerConstPointer;
    typedef typename Superclass::AccessorType        AccessorType;

    typedef DefaultPixelAccessorFunctor< Self > AccessorFunctorType;

    //typedef NeighborhoodAccessorFunctor< Self > NeighborhoodAccessorFunctorType;
    typedef typename Superclass::NeighborhoodAccessorFunctorType NeighborhoodAccessorFunctorType;

    //
    // Allocate CPU and GPU memory space
    //
    void Allocate();

    virtual void Initialize();

    void FillBuffer(const TPixel & value);

    void SetPixel(const IndexType & index, const TPixel & value);

    TPixel & GetPixel(const IndexType & index);

    const TPixel & operator[](const IndexType & index) const
    { return Superclass::GetPixel(index); }

    TPixel & operator[](const IndexType & index);

    //
    // Get CPU buffer pointer
    //
    TPixel* GetBufferPointer();

    const TPixel * GetBufferPointer() const;

    /** Return the Pixel Accessor object */
    AccessorType GetPixelAccessor(void)
    {
      m_GPUManager->SetGPUBufferDirty();
      return Superclass::GetPixelAccessor();
    }

    /** Return the Pixel Accesor object */
    const AccessorType GetPixelAccessor(void) const
    {
      m_GPUManager->MakeCPUBufferUpToDate();
      return Superclass::GetPixelAccessor();
    }

    /** Return the NeighborhoodAccessor functor */
    NeighborhoodAccessorFunctorType GetNeighborhoodAccessor()
    {
      m_GPUManager->SetGPUBufferDirty();
      return Superclass::GetNeighborhoodAccessor();
    }

    /** Return the NeighborhoodAccessor functor */
    const NeighborhoodAccessorFunctorType GetNeighborhoodAccessor() const
    {
      m_GPUManager->MakeCPUBufferUpToDate();
      return Superclass::GetNeighborhoodAccessor();
    }

    void SetPixelContainer(PixelContainer *container);

     /** Return a pointer to the container. */
    PixelContainer * GetPixelContainer()
    { m_GPUManager->SetGPUBufferDirty(); return Superclass::GetPixelContainer(); }

    const PixelContainer * GetPixelContainer() const
    {
      m_GPUManager->MakeCPUBufferUpToDate();
      return Superclass::GetPixelContainer();
    }

    void SetCurrentCommandQueue( int queueid )
    {
      m_GPUManager->SetCurrentCommandQueue( queueid );
    }

    int  GetCurrentCommandQueueID() { return m_GPUManager->GetCurrentCommandQueueID(); };

    GPUDataManager::Pointer GetGPUDataManager();

    /* Override DataHasBeenGenerated() in DataObject class.
     * We need this because CPU time stamp is always bigger
     * than GPU's. That is because Modified() is called at
     * the end of each filter in the pipeline so although we
     * increment GPU's time stamp in GPUGenerateData() the
     * CPU's time stamp will be increased after that.
     */
    void DataHasBeenGenerated()
    {
      Superclass::DataHasBeenGenerated();
      if( m_GPUManager->IsCPUBufferDirty() )
        {
        m_GPUManager->Modified();
        }
    }


  protected:
    GPUImage();
    virtual ~GPUImage();

  private:

    // functions that are purposely not implemented
    GPUImage(const Self&);
    void operator=(const Self&);

    typename GPUImageDataManager< GPUImage >::Pointer m_GPUManager;
  };


class GPUImageFactory : public itk::ObjectFactoryBase
{
public:
  typedef GPUImageFactory               Self;
  typedef itk::ObjectFactoryBase        Superclass;
  typedef itk::SmartPointer<Self>       Pointer;
  typedef itk::SmartPointer<const Self> ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char* GetITKSourceVersion() const { return ITK_SOURCE_VERSION; }
  const char* GetDescription() const { return "A Factory for GPUImage"; }

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GPUImageFactory, itk::ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    GPUImageFactory::Pointer factory = GPUImageFactory::New();
    itk::ObjectFactoryBase::RegisterFactory(factory);
  }

private:
  GPUImageFactory(const Self&);    //purposely not implemented
  void operator=(const Self&); //purposely not implemented

#define OverrideImageTypeMacro(pt,dm)    this->RegisterOverride(\
        typeid(itk::Image<pt,dm>).name(),\
        typeid(itk::GPUImage<pt,dm>).name(),\
        "GPU Image Override",\
        true,\
        itk::CreateObjectFunction<GPUImage<pt,dm> >::New())

  GPUImageFactory()
    {
      //TODO: we need global function to check GPU availability
      //this->IfGPUISAvailable()
      //{
      // 1/2/3D
      OverrideImageTypeMacro(unsigned char, 1);
      OverrideImageTypeMacro(signed char,  1);
      OverrideImageTypeMacro(int, 1);
      OverrideImageTypeMacro(unsigned int, 1);
      OverrideImageTypeMacro(float, 1);
      OverrideImageTypeMacro(double, 1);

      OverrideImageTypeMacro(unsigned char, 2);
      OverrideImageTypeMacro(signed char, 2);
      OverrideImageTypeMacro(int, 2);
      OverrideImageTypeMacro(unsigned int, 2);
      OverrideImageTypeMacro(float, 2);
      OverrideImageTypeMacro(double, 2);

      OverrideImageTypeMacro(unsigned char, 3);
      OverrideImageTypeMacro(signed char, 3);
      OverrideImageTypeMacro(int, 3);
      OverrideImageTypeMacro(unsigned int, 3);
      OverrideImageTypeMacro(float, 3);
      OverrideImageTypeMacro(double, 3);
      // need more type here!
      //}
    }
};

template <class T>
class GPUTraits
{
public:
  typedef T   Type;
};


template <class TPixelType, unsigned int NDimension>
class GPUTraits< Image< TPixelType, NDimension > >
{
public:
  typedef GPUImage<TPixelType,NDimension>   Type;
};


} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_GPUImage(_, EXPORT, TypeX, TypeY)     \
  namespace itk                                         \
  {                                                     \
  _( 2 ( class EXPORT GPUImage< ITK_TEMPLATE_2 TypeX > ) ) \
  namespace Templates                                   \
  {                                                     \
  typedef GPUImage< ITK_TEMPLATE_2 TypeX > GPUImage##TypeY; \
  }                                                     \
  }

#if ITK_TEMPLATE_EXPLICIT
#include "Templates/itkGPUImage+-.h"
#endif

#if ITK_TEMPLATE_TXX
#include "itkGPUImage.hxx"
#endif

#endif
