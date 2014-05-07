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
#ifndef __itkCreateObjectFunction_h
#define __itkCreateObjectFunction_h

#include "itkObject.h"

namespace itk
{
/** \class CreateObjectFunctionBase
 *  \brief Define API for object creation callback functions.
 *
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */
class CreateObjectFunctionBase:public Object
{
public:
  /** Standard typedefs. */
  typedef CreateObjectFunctionBase   Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Create an object and return a pointer to it as an
   * itk::LightObject. */
  virtual SmartPointer< LightObject > CreateObject() = 0;

protected:
  CreateObjectFunctionBase() {}
  ~CreateObjectFunctionBase() {}

private:
  CreateObjectFunctionBase(const Self &); //purposely not implemented
  void operator=(const Self &);           //purposely not implemented
};

/** \class CreateObjectFunction
 * \brief Used to create callback functions that create ITK Objects for
 *        use with the itk::ObjectFactory.
 *
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */
template< typename T >
class CreateObjectFunction:public CreateObjectFunctionBase
{
public:
  /** Standard class typedefs. */
  typedef CreateObjectFunction Self;
  typedef SmartPointer< Self > Pointer;

  /** Methods from itk:LightObject. */
  itkFactorylessNewMacro(Self);
  virtual LightObject::Pointer CreateObject() ITK_OVERRIDE { return T::New().GetPointer(); }

protected:
  CreateObjectFunction() {}
  ~CreateObjectFunction() {}

private:
  CreateObjectFunction(const Self &); //purposely not implemented
  void operator=(const Self &);       //purposely not implemented
};
} // end namespace itk

#endif
