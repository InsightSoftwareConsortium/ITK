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
#ifndef itkVTKImageExportBase_h
#define itkVTKImageExportBase_h

#include "itkProcessObject.h"
#include "ITKVTKExport.h"

namespace itk
{
/**
 *\class VTKImageExportBase
 * \brief Superclass for VTKImageExport instantiations.
 *
 * VTKImageExportBase provides the functions that serve as callbacks
 * given to vtkImageImport to connect the end of an ITK pipeline to
 * the beginning of a VTK pipeline.
 *
 * \ingroup IOFilters
 * \sa VTKImageExport
 * \ingroup ITKVTK
 */
class ITKVTK_EXPORT VTKImageExportBase : public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VTKImageExportBase);

  /** Standard class type aliases. */
  using Self = VTKImageExportBase;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKImageExportBase, ProcessObject);

  /** Returns the user data to set for the vtkImageImport callbacks. */
  void *
  GetCallbackUserData();

  /** The function pointer type expected for a callback. */
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

  /**
   *\class CallbackTypeProxy
   * \brief Provide compatibility between VTK 4.4 and earlier versions.
   * \ingroup ITKVTK
   */
  class CallbackTypeProxy
  {
  public:
    using DoubleCallbackType = double * (*)(void *);
    using FloatCallbackType = float * (*)(void *);
    operator DoubleCallbackType() { return m_DoubleCallback; }

    operator FloatCallbackType() { return m_FloatCallback; }
    CallbackTypeProxy(DoubleCallbackType d, FloatCallbackType f)
      : m_DoubleCallback(d)
      , m_FloatCallback(f)
    {}

  private:
    DoubleCallbackType m_DoubleCallback;
    FloatCallbackType  m_FloatCallback;
  };

  /** Get a pointer to function to set as a callback in vtkImageImport. */
  UpdateInformationCallbackType
  GetUpdateInformationCallback() const;

  PipelineModifiedCallbackType
  GetPipelineModifiedCallback() const;

  WholeExtentCallbackType
  GetWholeExtentCallback() const;

  CallbackTypeProxy
  GetSpacingCallback() const;

  CallbackTypeProxy
  GetOriginCallback() const;

  DirectionCallbackType
  GetDirectionCallback() const;

  ScalarTypeCallbackType
  GetScalarTypeCallback() const;

  NumberOfComponentsCallbackType
  GetNumberOfComponentsCallback() const;

  PropagateUpdateExtentCallbackType
  GetPropagateUpdateExtentCallback() const;

  UpdateDataCallbackType
  GetUpdateDataCallback() const;

  DataExtentCallbackType
  GetDataExtentCallback() const;

  BufferPointerCallbackType
  GetBufferPointerCallback() const;

protected:
  VTKImageExportBase();
  ~VTKImageExportBase() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  using DataObjectPointer = DataObject::Pointer;

  virtual void
  UpdateInformationCallback();

  virtual int
  PipelineModifiedCallback();

  virtual void
  UpdateDataCallback();

  /** These callbacks are image-type specific, and are implemented
   * in VTKImageExport. */
  virtual int *
  WholeExtentCallback() = 0;

  virtual double *
  SpacingCallback() = 0;

  virtual double *
  OriginCallback() = 0;

  virtual double *
  DirectionCallback() = 0;

  virtual float *
  FloatSpacingCallback() = 0;

  virtual float *
  FloatOriginCallback() = 0;

  virtual const char *
  ScalarTypeCallback() = 0;

  virtual int
  NumberOfComponentsCallback() = 0;

  virtual void
  PropagateUpdateExtentCallback(int *) = 0;

  virtual int *
  DataExtentCallback() = 0;

  virtual void *
  BufferPointerCallback() = 0;

private:
  /** Actual function sent to VTK as a callback.  Casts the user data
   * to a VTKImageExportBase pointer and invokes the corresponding
   * virtual method in that instance. */
  static void
  UpdateInformationCallbackFunction(void *);

  static int
  PipelineModifiedCallbackFunction(void *);

  static int *
  WholeExtentCallbackFunction(void *);

  static double *
  SpacingCallbackFunction(void *);

  static double *
  OriginCallbackFunction(void *);

  static double *
  DirectionCallbackFunction(void *);

  static float *
  FloatSpacingCallbackFunction(void *);

  static float *
  FloatOriginCallbackFunction(void *);

  static const char *
  ScalarTypeCallbackFunction(void *);

  static int
  NumberOfComponentsCallbackFunction(void *);

  static void
  PropagateUpdateExtentCallbackFunction(void *, int *);

  static void
  UpdateDataCallbackFunction(void *);

  static int *
  DataExtentCallbackFunction(void *);

  static void *
  BufferPointerCallbackFunction(void *);

private:
  /** PipelineMTime from the last call to PipelineModifiedCallback. */
  ModifiedTimeType m_LastPipelineMTime;
};
} // end namespace itk

#endif
