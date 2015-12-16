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
#ifndef itkTransformIOFactory_h
#define itkTransformIOFactory_h

#include "ITKIOTransformBaseExport.h"

#include "itkObject.h"
#include "itkTransformIOBase.h"

namespace itk
{

/** Mode in which the files is intended to be used */
typedef enum { ReadMode, WriteMode } TransformIOFactoryFileModeType;

/** \class TransformIOFactoryTemplate
 * \brief Create instances of TransformIO objects using an object factory.
 * \ingroup ITKIOTransformBase
 */
template<typename TParametersValueType>
class ITKIOTransformBase_EXPORT TransformIOFactoryTemplate:public Object
{
public:
  /** Standard class typedefs. */
  typedef TransformIOFactoryTemplate Self;
  typedef Object                     Superclass;
  typedef SmartPointer<Self>         Pointer;
  typedef SmartPointer<const Self>   ConstPointer;

  /** Class Methods used to interface with the registered factories */

  /** Run-time type information (and related methods). */
  itkTypeMacro(TransformIOFactoryTemplate, Object);

  /** Convenient typedefs. */
  typedef typename TransformIOBaseTemplate<TParametersValueType>::Pointer TransformIOBasePointer;

  /** Create the appropriate TransformIO depending on
   *  the particulars of the file.
   */
  static TransformIOBasePointer
  CreateTransformIO(const char *path, TransformIOFactoryFileModeType mode);

protected:
  TransformIOFactoryTemplate();
  virtual ~TransformIOFactoryTemplate();

private:
  TransformIOFactoryTemplate(const Self &) ITK_DELETE_FUNCTION;
  void operator=(const Self &) ITK_DELETE_FUNCTION;
};

/** This helps to meet backward compatibility */
typedef TransformIOFactoryTemplate<double> TransformIOFactory;

/** Explicit instantiations */
#ifndef ITK_TEMPLATE_EXPLICIT_TransformIOFactory
// Explicit instantiation is required to ensure correct dynamic_cast
// behavior across shared libraries.
#  if defined( ITKIOTransformBase_EXPORTS )
//   We are building this library
#    define ITKIOTransformBase_EXPORT_EXPLICIT
#  else
//   We are using this library
#    define ITKIOTransformBase_EXPORT_EXPLICIT ITKIOTransformBase_EXPORT
#  endif
extern template class ITKIOTransformBase_EXPORT_EXPLICIT TransformIOFactoryTemplate< double >;
extern template class ITKIOTransformBase_EXPORT_EXPLICIT TransformIOFactoryTemplate< float >;
#  undef ITKIOTransformBase_EXPORT_EXPLICIT
#endif

} // end namespace itk

// Note: Explicit instantiation is done in itkTransformIOFactory.cxx

#endif
