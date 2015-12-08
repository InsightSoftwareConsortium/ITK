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
class TransformIOFactoryTemplate:public Object
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
  ~TransformIOFactoryTemplate();

private:
  TransformIOFactoryTemplate(const Self &) ITK_DELETE_FUNCTION;
  void operator=(const Self &) ITK_DELETE_FUNCTION;
};

/** This helps to meet backward compatibility */
typedef TransformIOFactoryTemplate<double> TransformIOFactory;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTransformIOFactory.hxx"
#endif

#endif
