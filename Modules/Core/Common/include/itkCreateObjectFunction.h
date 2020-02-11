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
#ifndef itkCreateObjectFunction_h
#define itkCreateObjectFunction_h

#include "itkObject.h"

namespace itk
{
/** \class CreateObjectFunctionBase
 *  \brief Define API for object creation callback functions.
 *
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT CreateObjectFunctionBase : public Object
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(CreateObjectFunctionBase);

  /** Standard type alias. */
  using Self = CreateObjectFunctionBase;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Create an object and return a pointer to it as an
   * itk::LightObject. */
  virtual SmartPointer<LightObject>
  CreateObject() = 0;

protected:
  CreateObjectFunctionBase();
  ~CreateObjectFunctionBase() override;
};

/** \class CreateObjectFunction
 * \brief Used to create callback functions that create ITK Objects for
 *        use with the itk::ObjectFactory.
 *
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */
template <typename T>
class CreateObjectFunction : public CreateObjectFunctionBase
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(CreateObjectFunction);

  /** Standard class type aliases. */
  using Self = CreateObjectFunction;
  using Pointer = SmartPointer<Self>;

  /** Methods from itk:LightObject. */
  itkFactorylessNewMacro(Self);
  LightObject::Pointer
  CreateObject() override
  {
    return T::New().GetPointer();
  }

protected:
  CreateObjectFunction() = default;
  ~CreateObjectFunction() override = default;
};
} // end namespace itk

#endif
