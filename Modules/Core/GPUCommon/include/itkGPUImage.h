/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#ifndef itkGPUImage_h
#define itkGPUImage_h

#include "itkImage.h"
#include "itkGPUImageDataManager.h"
#include "itkVersion.h"
#include "itkObjectFactoryBase.h"

namespace itk
{
/** \class GPUImage
 *  \brief Templated n-dimensional image class for the GPU.
 *
 * Derived from itk Image class to use with GPU image filters.
 * This class manages both CPU and GPU memory implicitly, and
 * can be used with non-GPU itk filters as well. Memory transfer
 * between CPU and GPU is done automatically and implicitly.
 *
 * \ingroup ITKGPUCommon
 */
template <typename TPixel, unsigned int VImageDimension = 2>
class ITK_TEMPLATE_EXPORT GPUImage : public Image<TPixel, VImageDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GPUImage);

  using Self = GPUImage;
  using Superclass = Image<TPixel, VImageDimension>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using ConstWeakPointer = WeakPointer<const Self>;

  itkNewMacro(Self);

  itkTypeMacro(GPUImage, Image);

  static constexpr unsigned int ImageDimension = VImageDimension;

  using PixelType = typename Superclass::PixelType;
  using ValueType = typename Superclass::ValueType;
  using InternalPixelType = typename Superclass::InternalPixelType;
  using IOPixelType = typename Superclass::IOPixelType;
  using DirectionType = typename Superclass::DirectionType;
  using SpacingType = typename Superclass::SpacingType;
  using PixelContainer = typename Superclass::PixelContainer;
  using SizeType = typename Superclass::SizeType;
  using IndexType = typename Superclass::IndexType;
  using OffsetType = typename Superclass::OffsetType;
  using RegionType = typename Superclass::RegionType;
  using PixelContainerPointer = typename PixelContainer::Pointer;
  using PixelContainerConstPointer = typename PixelContainer::ConstPointer;
  using AccessorType = typename Superclass::AccessorType;

  using AccessorFunctorType = DefaultPixelAccessorFunctor<Self>;

  using NeighborhoodAccessorFunctorType = NeighborhoodAccessorFunctor<Self>;
  // NeighborhoodAccessorFunctorType;

  //
  // Allocate CPU and GPU memory space
  //
  void
  Allocate(bool initialize = false) override;

  void
  Initialize() override;

  void
  FillBuffer(const TPixel & value);

  void
  SetPixel(const IndexType & index, const TPixel & value);

  const TPixel &
  GetPixel(const IndexType & index) const;

  TPixel &
  GetPixel(const IndexType & index);

  const TPixel & operator[](const IndexType & index) const;

  TPixel & operator[](const IndexType & index);

  /** Explicit synchronize CPU/GPU buffers */
  void
  UpdateBuffers();

  //
  // Get CPU buffer pointer
  //
  TPixel *
  GetBufferPointer() override;

  const TPixel *
  GetBufferPointer() const override;

  /** Return the Pixel Accessor object */
  AccessorType
  GetPixelAccessor()
  {
    m_DataManager->SetGPUBufferDirty();
    return Superclass::GetPixelAccessor();
  }

  /** Return the Pixel Accesor object */
  const AccessorType
  GetPixelAccessor() const
  {
    m_DataManager->UpdateCPUBuffer();
    return Superclass::GetPixelAccessor();
  }

  /** Return the NeighborhoodAccessor functor */
  NeighborhoodAccessorFunctorType
  GetNeighborhoodAccessor()
  {
    m_DataManager->SetGPUBufferDirty();
    // return Superclass::GetNeighborhoodAccessor();
    return NeighborhoodAccessorFunctorType();
  }

  /** Return the NeighborhoodAccessor functor */
  const NeighborhoodAccessorFunctorType
  GetNeighborhoodAccessor() const
  {
    m_DataManager->UpdateCPUBuffer();
    // return Superclass::GetNeighborhoodAccessor();
    return NeighborhoodAccessorFunctorType();
  }

  void
  SetPixelContainer(PixelContainer * container);

  /** Return a pointer to the container. */
  PixelContainer *
  GetPixelContainer()
  {
    m_DataManager->SetGPUBufferDirty();
    return Superclass::GetPixelContainer();
  }

  const PixelContainer *
  GetPixelContainer() const
  {
    m_DataManager->UpdateCPUBuffer();
    return Superclass::GetPixelContainer();
  }

  void
  SetCurrentCommandQueue(int queueid)
  {
    m_DataManager->SetCurrentCommandQueue(queueid);
  }

  int
  GetCurrentCommandQueueID()
  {
    return m_DataManager->GetCurrentCommandQueueID();
  }

  itkGetModifiableObjectMacro(DataManager, GPUImageDataManager<GPUImage>);
  GPUDataManager *
  GetGPUDataManager();

  /* Override DataHasBeenGenerated() in DataObject class.
   * We need this because CPU time stamp is always bigger
   * than GPU's. That is because Modified() is called at
   * the end of each filter in the pipeline so although we
   * increment GPU's time stamp in GPUGenerateData() the
   * CPU's time stamp will be increased after that.
   */
  void
  DataHasBeenGenerated() override
  {
    Superclass::DataHasBeenGenerated();
    if (m_DataManager->IsCPUBufferDirty())
    {
      m_DataManager->Modified();
    }
  }

  /** Graft the data and information from one GPUImage to another. */
  virtual void
  Graft(const Self * data);

protected:
  void
  Graft(const DataObject * data) override;
  GPUImage();
  ~GPUImage() override;
  using Superclass::Graft;

private:
  typename GPUImageDataManager<GPUImage>::Pointer m_DataManager;
};

class ITK_TEMPLATE_EXPORT GPUImageFactory : public itk::ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GPUImageFactory);

  using Self = GPUImageFactory;
  using Superclass = itk::ObjectFactoryBase;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  /** Class methods used to interface with the registered factories. */
  const char *
  GetITKSourceVersion() const override
  {
    return ITK_SOURCE_VERSION;
  }
  const char *
  GetDescription() const override
  {
    return "A Factory for GPUImage";
  }

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GPUImageFactory, itk::ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    GPUImageFactory::Pointer factory = GPUImageFactory::New();

    itk::ObjectFactoryBase::RegisterFactory(factory);
  }

private:
#define OverrideImageTypeMacro(pt, dm)                                                                                 \
  this->RegisterOverride(typeid(itk::Image<pt, dm>).name(),                                                            \
                         typeid(itk::GPUImage<pt, dm>).name(),                                                         \
                         "GPU Image Override",                                                                         \
                         true,                                                                                         \
                         itk::CreateObjectFunction<GPUImage<pt, dm>>::New())

  GPUImageFactory()
  {
    if (IsGPUAvailable())
    {
      // 1/2/3D
      OverrideImageTypeMacro(unsigned char, 1);
      OverrideImageTypeMacro(signed char, 1);
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
    }
  }
};

template <typename T>
class ITK_TEMPLATE_EXPORT GPUTraits
{
public:
  using Type = T;
};

template <typename TPixelType, unsigned int NDimension>
class ITK_TEMPLATE_EXPORT GPUTraits<Image<TPixelType, NDimension>>
{
public:
  using Type = GPUImage<TPixelType, NDimension>;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGPUImage.hxx"
#endif

#endif
