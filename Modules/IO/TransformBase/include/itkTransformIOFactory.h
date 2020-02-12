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
#ifndef itkTransformIOFactory_h
#define itkTransformIOFactory_h

#include "itkObject.h"
#include "itkTransformIOBase.h"
#include "ITKIOTransformBaseExport.h"
#include "itkCommonEnums.h"

namespace itk
{

#if !defined(ITK_LEGACY_REMOVE)
using TransformIOFactoryFileModeEnum = IOFileModeEnum;
#endif

/** \class TransformIOFactoryTemplate
 * \brief Create instances of TransformIO objects using an object factory.
 * \ingroup ITKIOTransformBase
 */
template <typename TParametersValueType>
class ITK_TEMPLATE_EXPORT TransformIOFactoryTemplate : public Object
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(TransformIOFactoryTemplate);

  /** Standard class type aliases. */
  using Self = TransformIOFactoryTemplate;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Class Methods used to interface with the registered factories */

  /** Run-time type information (and related methods). */
  itkTypeMacro(TransformIOFactoryTemplate, Object);

  /** Convenient type alias. */
  using TransformIOBasePointer = typename TransformIOBaseTemplate<TParametersValueType>::Pointer;

  /** Create the appropriate TransformIO depending on
   *  the particulars of the file.
   */
  static TransformIOBasePointer
  CreateTransformIO(const char * path, IOFileModeEnum mode);

protected:
  TransformIOFactoryTemplate();
  ~TransformIOFactoryTemplate() override;
};

/** This helps to meet backward compatibility */
using TransformIOFactory = TransformIOFactoryTemplate<double>;

} // end namespace itk

// Note: Explicit instantiation is done in itkTransformIOFactory.cxx

#endif

/** Explicit instantiations */
#ifndef ITK_TEMPLATE_EXPLICIT_TransformIOFactory
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

extern template class ITKIOTransformBase_EXPORT_EXPLICIT TransformIOFactoryTemplate<double>;
extern template class ITKIOTransformBase_EXPORT_EXPLICIT TransformIOFactoryTemplate<float>;

ITK_GCC_PRAGMA_DIAG_POP()

} // end namespace itk
#undef ITKIOTransformBase_EXPORT_EXPLICIT
#endif
