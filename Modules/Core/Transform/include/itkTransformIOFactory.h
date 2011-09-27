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
#ifndef __itkTransformIOFactory_h
#define __itkTransformIOFactory_h

#include "itkObject.h"
#include "itkTransformIOBase.h"

namespace itk
{
/** \class TransformIOFactory
 * \brief Create instances of TransformIO objects using an object factory.
 * \ingroup ITKTransform
 */
class ITK_EXPORT TransformIOFactory:public Object
{
public:
  /** Standard class typedefs. */
  typedef TransformIOFactory         Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class Methods used to interface with the registered factories */

  /** Run-time type information (and related methods). */
  itkTypeMacro(TransformIOFactory, Object);

  /** Convenient typedefs. */
  typedef TransformIOBase::Pointer TransformIOBasePointer;

  /** Mode in which the files is intended to be used */
  typedef enum { ReadMode, WriteMode } FileModeType;

  /** Create the appropriate TransformIO depending on
   *  the particulars of the file.
   */
  static TransformIOBasePointer
  CreateTransformIO(const char *path, FileModeType mode);

  /** Register Built-in factories */
  static void RegisterBuiltInFactories();

protected:
  TransformIOFactory();
  ~TransformIOFactory();
private:
  TransformIOFactory(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented
};
} // end namespace itk

#endif
