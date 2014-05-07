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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef __itkTransformFactoryBase_h
#define __itkTransformFactoryBase_h

#include "itkObjectFactoryBase.h"

namespace itk
{
/** \class TransformFactoryBase
 * \brief Create instances of Transforms
 * \ingroup ITKIOTransformBase
 */

class TransformFactoryBase:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef TransformFactoryBase       Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const ITK_OVERRIDE;

  virtual const char * GetDescription(void) const ITK_OVERRIDE;

  /** Run-time type information (and related methods). */
  itkTypeMacro(TransformFactoryBase, ObjectFactoryBase);

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Register all builtin transforms */
  static void RegisterDefaultTransforms();

  /** Register this transform */
  static TransformFactoryBase * GetFactory()
  {
    if ( m_Factory == ITK_NULLPTR )
      {
      // Make and register the factory
      Pointer p = New();
      m_Factory = p.GetPointer();
      ObjectFactoryBase::RegisterFactory (p);
      p->RegisterDefaultTransforms ();
      }
    return m_Factory;
  }

  void RegisterTransform(const char *classOverride,
                         const char *overrideClassName,
                         const char *description,
                         bool enableFlag,
                         CreateObjectFunctionBase *createFunction)
  {
    this->RegisterOverride (classOverride, overrideClassName, description, enableFlag, createFunction);
  }

protected:
  TransformFactoryBase();
  virtual ~TransformFactoryBase();

private:
  TransformFactoryBase(const Self &); //purposely not implemented
  void operator=(const Self &);       //purposely not implemented

  static TransformFactoryBase *m_Factory;
};
} // end namespace itk

#endif
