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
#ifndef itkTransformFactoryBase_h
#define itkTransformFactoryBase_h

#include "itkObjectFactoryBase.h"

namespace itk
{
/** \class TransformFactoryBase
 * \brief Create instances of Transforms
 * \ingroup ITKTransformFactory
 */

class
TransformFactoryBase:public ObjectFactoryBase
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

  /** Get the factory */
  static TransformFactoryBase * GetFactory();

  /**
 * Register transform given its type.
 *
 * Type of a transform can be obtained using itk::Transform::GetTransformTypeAsString()
 *
 * Within ITK, \a classOverride, \a overrideClassName and \a description are all
 * set to the transform type.
 *
 * \note
 * If a transform has already been registered, this function is a no-op. To help
 * debugging issue related to registration, it is possible to enable runtime warnings
 * when compiling in \b Debug mode by setting both the \b Debug flag of this class
 * and \b GetGlobalWarningDisplay to true.
 */
  void RegisterTransform(const char *classOverride,
                         const char *overrideClassName,
                         const char *description,
                         bool enableFlag,
                         CreateObjectFunctionBase *createFunction)
  {

    // Ensure there is only one transform registered by a name, this
    // may happen on windows where this library is static, and the
    // global init flag may not be unique.
    LightObject::Pointer test = this->CreateInstance(classOverride);
    if ( test.IsNotNull() )
      {
      itkDebugMacro("Refusing to register transform \"" << classOverride << "\" again!");
      test->UnRegister();
      }
    else
      {
      this->RegisterOverride (classOverride, overrideClassName, description, enableFlag, createFunction);
      }
  }

protected:
  TransformFactoryBase();
  virtual ~TransformFactoryBase() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(TransformFactoryBase);

  // Sub private functions of RegisterDefaultTransforms
  static void RegisterTransformFactoryDoubleParameters();
  static void RegisterTransformFactoryFloatParameters();


  // Called by the type specific methods
  template <typename TParameterType>
  static void RegisterTransformFactory(void);

  static TransformFactoryBase *m_Factory;
};
} // end namespace itk

#endif
