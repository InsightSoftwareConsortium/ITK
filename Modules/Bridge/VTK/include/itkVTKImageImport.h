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
#ifndef itkVTKImageImport_h
#define itkVTKImageImport_h

#include "itkImageSource.h"
#include "itkImportImageContainer.h"

#define itkSetMacro2(name, type)                     \
  virtual void Set##name (type _arg)               \
    {                                                \
    itkDebugMacro("setting " #name " to " << _arg); \
    if ( this->m_##name != _arg )                  \
      {                                              \
      this->m_##name = _arg;                       \
      this->Modified();                              \
      }                                              \
    }

namespace itk
{
/** \class VTKImageImport
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
template< typename TOutputImage >
class ITK_TEMPLATE_EXPORT VTKImageImport:public ImageSource< TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef VTKImageImport              Self;
  typedef ImageSource< TOutputImage > Superclass;
  typedef SmartPointer< Self >        Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKImageImport, ImageSource);

  /** Convenient typedefs from the output image. */
  typedef TOutputImage                         OutputImageType;
  typedef typename OutputImageType::Pointer    OutputImagePointer;
  typedef typename OutputImageType::PixelType  OutputPixelType;
  typedef typename OutputImageType::SizeType   OutputSizeType;
  typedef typename OutputImageType::IndexType  OutputIndexType;
  typedef typename OutputImageType::RegionType OutputRegionType;

  /** The output image dimension. */
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      OutputImageType::ImageDimension);

  /** These are function pointer types for the pipeline connection
   * callbacks. */
  typedef void ( *        UpdateInformationCallbackType )(void *);
  typedef int ( *         PipelineModifiedCallbackType )(void *);
  typedef int * ( *       WholeExtentCallbackType )(void *);
  typedef double * ( *    SpacingCallbackType )(void *);
  typedef double * ( *    OriginCallbackType )(void *);
  typedef const char * ( *ScalarTypeCallbackType )(void *);
  typedef int ( *         NumberOfComponentsCallbackType )(void *);
  typedef void ( *        PropagateUpdateExtentCallbackType )(void *, int *);
  typedef void ( *        UpdateDataCallbackType )(void *);
  typedef int * ( *       DataExtentCallbackType )(void *);
  typedef void * ( *      BufferPointerCallbackType )(void *);

  /** Compatibility for VTK older than 4.4.  */
  typedef float * ( *FloatSpacingCallbackType )(void *);
  typedef float * ( *FloatOriginCallbackType )(void *);

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
  void SetSpacingCallback(FloatSpacingCallbackType f)
  { this->SetFloatSpacingCallback(f); }

  /** What to do when receiving SetOrigin(). */
  itkSetMacro(OriginCallback, OriginCallbackType);
  itkGetConstMacro(OriginCallback, OriginCallbackType);
  itkSetMacro(FloatOriginCallback, FloatOriginCallbackType);
  itkGetConstMacro(FloatOriginCallback, FloatOriginCallbackType);
  void SetOriginCallback(FloatOriginCallbackType f)
  { this->SetFloatOriginCallback(f); }

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
  ~VTKImageImport() {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void PropagateRequestedRegion(DataObject *) ITK_OVERRIDE;

  virtual void UpdateOutputInformation() ITK_OVERRIDE;

  virtual void GenerateData() ITK_OVERRIDE;

  virtual void GenerateOutputInformation() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VTKImageImport);

  void *                            m_CallbackUserData;
  UpdateInformationCallbackType     m_UpdateInformationCallback;
  PipelineModifiedCallbackType      m_PipelineModifiedCallback;
  WholeExtentCallbackType           m_WholeExtentCallback;
  SpacingCallbackType               m_SpacingCallback;
  FloatSpacingCallbackType          m_FloatSpacingCallback;
  OriginCallbackType                m_OriginCallback;
  FloatOriginCallbackType           m_FloatOriginCallback;
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
#include "itkVTKImageImport.hxx"
#endif

#endif // itkVTKImageImport_h
