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
#ifndef itkTransformFileReader_h
#define itkTransformFileReader_h

#include "ITKIOTransformBaseExport.h"

#include "itkTransformIOBase.h"

namespace itk
{
/** \class TransformFileReaderTemplate
 *
 * \brief TODO
 * \ingroup ITKIOTransformBase
 *
 * \sphinx
 * \sphinxexample{IO/TransformBase/ReadTransformFromFile,Read Transform From File}
 * \endsphinx
 */
template <typename TParametersValueType>
class ITK_TEMPLATE_EXPORT TransformFileReaderTemplate : public LightProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(TransformFileReaderTemplate);

  /** SmartPointer type alias support */
  using Self = TransformFileReaderTemplate;
  using Pointer = SmartPointer<Self>;
  using TransformType = TransformBaseTemplate<TParametersValueType>;

  using ParametersType = typename TransformType::ParametersType;
  using ParametersValueType = typename TransformType::ParametersValueType;
  using FixedParametersType = typename TransformType::FixedParametersType;
  using FixedParametersValueType = typename TransformType::FixedParametersValueType;

  using TransformIOType = TransformIOBaseTemplate<ParametersValueType>;
  using TransformPointer = typename TransformIOType::TransformPointer;
  using TransformListType = typename TransformIOType::TransformListType;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  using Superclass = Object;
  itkTypeMacro(TransformFileReaderTemplate, LightProcessObject);

  /** Set the filename  */
  itkSetStringMacro(FileName);

  /** Get the filename */
  itkGetStringMacro(FileName);

  /** Read the transforms */
  virtual void
  Update();

#if !defined(ITK_LEGACY_REMOVE)
  /** Get the list of transforms.
   * \warning The output is not intended to be modifiable.
   * \deprecated */
  TransformListType *
  GetTransformList()
  {
    return &m_TransformList;
  }
#else
  const TransformListType *
  GetTransformList()
  {
    return &m_TransformList;
  }
#endif
  TransformListType *
  GetModifiableTransformList()
  {
    return &m_TransformList;
  }

  /** Set/Get the TransformIO class used internally to read to transform. */
  itkSetObjectMacro(TransformIO, TransformIOType);
  itkGetConstObjectMacro(TransformIO, TransformIOType);

protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  TransformFileReaderTemplate();
  ~TransformFileReaderTemplate() override;

  TransformListType                 m_TransformList;
  typename TransformIOType::Pointer m_TransformIO;
  std::string                       m_FileName;
};

/** This helps to meet backward compatibility */
using TransformFileReader = itk::TransformFileReaderTemplate<double>;

} // namespace itk

#ifdef ITK_IO_FACTORY_REGISTER_MANAGER
#  include "itkTransformIOFactoryRegisterManager.h"
#endif

// Note: Explicit instantiation is done in itkTransformFileReader.cxx

#endif // itkTransformFileReade_h

/** Explicit instantiations */
#ifndef ITK_TEMPLATE_EXPLICIT_TransformFileReader
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
#  define ITKIOTransformBase_EXPORT_EXPLICIT ITK_FORWARD_EXPORT
#else
//   We are using this library
#  define ITKIOTransformBase_EXPORT_EXPLICIT ITKIOTransformBase_EXPORT
#endif
namespace itk
{

ITK_GCC_PRAGMA_DIAG_PUSH()
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

extern template class ITKIOTransformBase_EXPORT_EXPLICIT TransformFileReaderTemplate<double>;
extern template class ITKIOTransformBase_EXPORT_EXPLICIT TransformFileReaderTemplate<float>;

ITK_GCC_PRAGMA_DIAG_POP()

} // end namespace itk
#undef ITKIOTransformBase_EXPORT_EXPLICIT
#endif
