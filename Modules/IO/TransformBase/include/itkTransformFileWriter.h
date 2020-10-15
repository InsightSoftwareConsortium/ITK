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
#ifndef itkTransformFileWriter_h
#define itkTransformFileWriter_h

#include "ITKIOTransformBaseExport.h"

#include "itkTransformIOBase.h"
#include <iostream>
#include <fstream>

namespace itk
{
/** \class TransformFileWriterTemplate
 *
 * \brief TODO
 * \ingroup ITKIOTransformBase
 *
 * \sphinx
 * \sphinxexample{IO/TransformBase/WriteTransformToFile,Write Transform From File}
 * \endsphinx
 */
template <typename TParametersValueType>
class ITKIOTransformBase_TEMPLATE_EXPORT TransformFileWriterTemplate : public LightProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TransformFileWriterTemplate);

  /** SmartPointer type alias support */
  using Self = TransformFileWriterTemplate;
  using Superclass = LightProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using ParametersValueType = TParametersValueType;
  using FixedParametersValueType = double;
  using TransformType = TransformBaseTemplate<ParametersValueType>;
  using TransformIOType = TransformIOBaseTemplate<ParametersValueType>;
  using TransformPointer = typename TransformIOType::TransformPointer;
  using ConstTransformPointer = typename TransformIOType::ConstTransformPointer;
  using ConstTransformListType = typename TransformIOType::ConstTransformListType;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TransformFileWriterTemplate, LightProcessObject);

  /** Set the filename  */
  itkSetStringMacro(FileName);

  /** Get the filename */
  itkGetStringMacro(FileName);

  /** Set/Get the write mode (append/overwrite) for the Filter */
  void
  SetAppendOff();

  void
  SetAppendOn();

  void
  SetAppendMode(bool mode);

  bool
  GetAppendMode();

  /** Set/Get a boolean to use the compression or not. */
  itkSetMacro(UseCompression, bool);
  itkGetConstMacro(UseCompression, bool);
  itkBooleanMacro(UseCompression);

  /** Set/Get the input transform to write */
  void
  SetInput(const Object * transform);

  const TransformType *
  GetInput();

  /** Add a transform to be written */
  void
  AddTransform(const Object * transform);

  /** Write out the transform */
  void
  Update();

  /** Set/Get the TransformIO class used internally to read to transform. */
  itkSetObjectMacro(TransformIO, TransformIOType);
  itkGetConstObjectMacro(TransformIO, TransformIOType);

protected:
  TransformFileWriterTemplate();
  ~TransformFileWriterTemplate() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  void
  PushBackTransformList(const Object * transObj);

  std::string            m_FileName;
  ConstTransformListType m_TransformList;
  bool                   m_AppendMode{ false };
  /** Should we compress the data? */
  bool                              m_UseCompression{ false };
  typename TransformIOType::Pointer m_TransformIO;
};

/** This helps to meet backward compatibility */
using TransformFileWriter = itk::TransformFileWriterTemplate<double>;

ITK_GCC_PRAGMA_DIAG_PUSH()
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

/** Declare specializations */
template <>
void ITKIOTransformBase_TEMPLATE_EXPORT
     TransformFileWriterTemplate<double>::PushBackTransformList(const Object * transObj);
template <>
void ITKIOTransformBase_TEMPLATE_EXPORT
     TransformFileWriterTemplate<float>::PushBackTransformList(const Object * transObj);

ITK_GCC_PRAGMA_DIAG_POP()

} // namespace itk

#ifdef ITK_IO_FACTORY_REGISTER_MANAGER
#  include "itkTransformIOFactoryRegisterManager.h"
#endif

// Note: Explicit instantiation is done in itkTransformFileWriterSpecializations.cxx

#endif // itkTransformFileWriter_h

/** Explicit instantiations */
#ifndef ITK_TEMPLATE_EXPLICIT_TransformFileWriter
// Explicit instantiation is required to ensure correct dynamic_cast
// behavior across shared libraries.
//
// IMPORTANT: Since within the same compilation unit,
//            ITK_TEMPLATE_EXPLICIT_<classname> defined and undefined states
//            need to be considered. This code *MUST* be *OUTSIDE* the header
//            guards.
//
#if defined(ITKIOTransformBase_EXPORTS)
//   We are building this library
#  define ITKIOTransformBase_EXPORT_EXPLICIT ITKIOTransformBase_TEMPLATE_EXPORT
#else
//   We are using this library
#  define ITKIOTransformBase_EXPORT_EXPLICIT ITKIOTransformBase_EXPORT
#endif
namespace itk
{

ITK_GCC_PRAGMA_DIAG_PUSH()
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

extern template class ITKIOTransformBase_EXPORT_EXPLICIT TransformFileWriterTemplate<double>;
extern template class ITKIOTransformBase_EXPORT_EXPLICIT TransformFileWriterTemplate<float>;

ITK_GCC_PRAGMA_DIAG_POP()

} // end namespace itk
#undef ITKIOTransformBase_EXPORT_EXPLICIT
#endif
