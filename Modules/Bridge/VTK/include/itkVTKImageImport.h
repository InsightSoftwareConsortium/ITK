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
#ifndef itkVTKImageImport_h
#define itkVTKImageImport_h

#include "itkImageSource.h"
#include "itkImportImageContainer.h"

#define itkSetMacro2(name, type)                                                                                       \
  virtual void Set##name(type _arg)                                                                                    \
  {                                                                                                                    \
    itkDebugMacro("setting " #name " to " << _arg);                                                                    \
    if (this->m_##name != _arg)                                                                                        \
    {                                                                                                                  \
      this->m_##name = _arg;                                                                                           \
      this->Modified();                                                                                                \
    }                                                                                                                  \
  }

namespace itk
{
/**
 *\class VTKImageImport
 * \brief Connect the end of an VTK pipeline to an ITK image pipeline.
 *
 * VTKImageImport can be used at the beginning of an ITK image pipeline to
 * connect with a VTK pipeline that ends with vtkImageExport.  Callbacks
 * provided by VTKImageImport are registered with vtkImageExport to connect
 * the pipeline execution together.  Once connected, update requests coming
 * through the ITK pipeline are automatically propagated to the VTK pipeline.
 *
 * Note that the VTK images are assumed to be of 1, 2, or 3 dimensions.
 * Scalar value types can be one of: float, double, char, unsigned char,
 * short, unsigned short, int, unsigned int, long, unsigned long. The
 * images can also have pixel types with more than one component.
 *
 * \ingroup IOFilters
 * \sa VTKImageImport
 * \ingroup ITKVTK
 */
template <typename TOutputImage>
class ITK_TEMPLATE_EXPORT VTKImageImport : public ImageSource<TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VTKImageImport);

  /** Standard class type aliases. */
  using Self = VTKImageImport;
  using Superclass = ImageSource<TOutputImage>;
  using Pointer = SmartPointer<Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKImageImport, ImageSource);

  /** Convenient type alias from the output image. */
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputPixelType = typename OutputImageType::PixelType;
  using OutputSizeType = typename OutputImageType::SizeType;
  using OutputIndexType = typename OutputImageType::IndexType;
  using OutputRegionType = typename OutputImageType::RegionType;

  /** The output image dimension. */
  static constexpr unsigned int OutputImageDimension = OutputImageType::ImageDimension;

  /** These are function pointer types for the pipeline connection
   * callbacks. */
  using UpdateInformationCallbackType = void (*)(void *);
  using PipelineModifiedCallbackType = int (*)(void *);
  using WholeExtentCallbackType = int * (*)(void *);
  using SpacingCallbackType = double * (*)(void *);
  using OriginCallbackType = double * (*)(void *);
  using DirectionCallbackType = double * (*)(void *);
  using ScalarTypeCallbackType = const char * (*)(void *);
  using NumberOfComponentsCallbackType = int (*)(void *);
  using PropagateUpdateExtentCallbackType = void (*)(void *, int *);
  using UpdateDataCallbackType = void (*)(void *);
  using DataExtentCallbackType = int * (*)(void *);
  using BufferPointerCallbackType = void * (*)(void *);

  /** Compatibility for VTK older than 4.4.  */
  using FloatSpacingCallbackType = float * (*)(void *);
  using FloatOriginCallbackType = float * (*)(void *);

  /** What to do when receiving UpdateInformation(). */
  itkSetMacro(UpdateInformationCallback, UpdateInformationCallbackType);
  itkGetConstMacro(UpdateInformationCallback, UpdateInformationCallbackType);

  /** What to do when receiving PipelineModified(). */
  itkSetMacro(PipelineModifiedCallback, PipelineModifiedCallbackType);
  itkGetConstMacro(PipelineModifiedCallback, PipelineModifiedCallbackType);

  /** What to do when receiving SetWholeExtent(). */
  itkSetMacro(WholeExtentCallback, WholeExtentCallbackType);
  itkGetConstMacro(WholeExtentCallback, WholeExtentCallbackType);

  /** What to do when receiving SetSpacing(). */
  itkSetMacro(SpacingCallback, SpacingCallbackType);
  itkGetConstMacro(SpacingCallback, SpacingCallbackType);
  itkSetMacro(FloatSpacingCallback, FloatSpacingCallbackType);
  itkGetConstMacro(FloatSpacingCallback, FloatSpacingCallbackType);
  void
  SetSpacingCallback(FloatSpacingCallbackType f)
  {
    this->SetFloatSpacingCallback(f);
  }

  /** What to do when receiving SetOrigin(). */
  itkSetMacro(OriginCallback, OriginCallbackType);
  itkGetConstMacro(OriginCallback, OriginCallbackType);
  itkSetMacro(FloatOriginCallback, FloatOriginCallbackType);
  itkGetConstMacro(FloatOriginCallback, FloatOriginCallbackType);
  void
  SetOriginCallback(FloatOriginCallbackType f)
  {
    this->SetFloatOriginCallback(f);
  }

  /** What to do when receiving SetDirection(). */
  itkSetMacro(DirectionCallback, DirectionCallbackType);
  itkGetConstMacro(DirectionCallback, DirectionCallbackType);

  /** What to do when receiving UpdateInformation(). */
  itkSetMacro(ScalarTypeCallback, ScalarTypeCallbackType);
  itkGetConstMacro(ScalarTypeCallback, ScalarTypeCallbackType);

  /** What to do when receiving SetNumberOfComponents(). */
  itkSetMacro(NumberOfComponentsCallback, NumberOfComponentsCallbackType);
  itkGetConstMacro(NumberOfComponentsCallback, NumberOfComponentsCallbackType);

  /** What to do when receiving PropagateUpdateExtent(). */
  itkSetMacro(PropagateUpdateExtentCallback, PropagateUpdateExtentCallbackType);
  itkGetConstMacro(PropagateUpdateExtentCallback, PropagateUpdateExtentCallbackType);

  /** What to do when receiving UpdateData(). */
  itkSetMacro(UpdateDataCallback, UpdateDataCallbackType);
  itkGetConstMacro(UpdateDataCallback, UpdateDataCallbackType);

  /** What to do when receiving DataExtent(). */
  itkSetMacro(DataExtentCallback, DataExtentCallbackType);
  itkGetConstMacro(DataExtentCallback, DataExtentCallbackType);

  /** What to do when receiving BufferPointer(). */
  itkSetMacro(BufferPointerCallback, BufferPointerCallbackType);
  itkGetConstMacro(BufferPointerCallback, BufferPointerCallbackType);

  /** Specify callback data. */
  itkSetMacro2(CallbackUserData, void *);
  itkGetConstMacro(CallbackUserData, void *);

protected:
  VTKImageImport();
  ~VTKImageImport() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  PropagateRequestedRegion(DataObject *) override;

  void
  UpdateOutputInformation() override;

  void
  GenerateData() override;

  void
  GenerateOutputInformation() override;

private:
  void *                            m_CallbackUserData;
  UpdateInformationCallbackType     m_UpdateInformationCallback;
  PipelineModifiedCallbackType      m_PipelineModifiedCallback;
  WholeExtentCallbackType           m_WholeExtentCallback;
  SpacingCallbackType               m_SpacingCallback;
  FloatSpacingCallbackType          m_FloatSpacingCallback;
  OriginCallbackType                m_OriginCallback;
  FloatOriginCallbackType           m_FloatOriginCallback;
  DirectionCallbackType             m_DirectionCallback;
  ScalarTypeCallbackType            m_ScalarTypeCallback;
  NumberOfComponentsCallbackType    m_NumberOfComponentsCallback;
  PropagateUpdateExtentCallbackType m_PropagateUpdateExtentCallback;
  UpdateDataCallbackType            m_UpdateDataCallback;
  DataExtentCallbackType            m_DataExtentCallback;
  BufferPointerCallbackType         m_BufferPointerCallback;

  std::string m_ScalarTypeName;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVTKImageImport.hxx"
#endif

#endif // itkVTKImageImport_h
