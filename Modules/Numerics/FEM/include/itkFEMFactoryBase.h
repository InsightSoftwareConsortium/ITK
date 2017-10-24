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
#ifndef itkFEMFactoryBase_h
#define itkFEMFactoryBase_h

#include "itkObjectFactoryBase.h"
#include "ITKFEMExport.h"

namespace itk
{
/** \class FEMFactoryBase
 * \brief Create instances of FEM Objects.
 * This includes Elements, Loads, and Materials
 * \ingroup ITKFEM
 */

class ITKFEM_EXPORT FEMFactoryBase : public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef FEMFactoryBase           Self;
  typedef ObjectFactoryBase        Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const ITK_OVERRIDE;

  virtual const char * GetDescription(void) const ITK_OVERRIDE;

  /** Run-time type information (and related methods). */
  itkTypeMacro(FEMFactoryBase, ObjectFactoryBase);

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Register all builtin transforms */
  static void RegisterDefaultTypes();  //HACK: This should not have a public interface since it does nothing except during instantiation of the class.

  /** Register this transform */
  static FEMFactoryBase * GetFactory()
  {
    if( m_Factory == ITK_NULLPTR )
      {
      m_CreationLock.Lock();
      //Need to make sure that during gaining access
      //to the lock that some other thread did not
      //initialize the singleton.
      if( m_Factory == ITK_NULLPTR )
        {
        // Make and register the factory
        FEMFactoryBase::Pointer p = FEMFactoryBase::New();
        if( p.IsNull() )
          {
          std::ostringstream message;
          message << "itk::ERROR: " << "FEMFactoryBase"
            << " instance not created";
          ::itk::ExceptionObject e_(__FILE__, __LINE__, message.str().c_str(), ITK_LOCATION);
          throw e_; /* Explicit naming to work around for Intel compiler bug. */
          }
        ObjectFactoryBase::RegisterFactory( p );
        m_Factory = p.GetPointer();
        }
      m_CreationLock.Unlock();
      m_Factory->RegisterDefaultTypes(); //Not initialzie all default types.
      }
    return m_Factory;
  }

  void RegisterType(const char* classOverride,
                    const char* overrideClassName,
                    const char* description,
                    bool enableFlag,
                    CreateObjectFunctionBase* createFunction)
  {
    this->RegisterOverride( classOverride, overrideClassName, description, enableFlag, createFunction );
  }

protected:
  FEMFactoryBase();
  virtual ~FEMFactoryBase() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FEMFactoryBase);

  static SimpleFastMutexLock m_CreationLock;
  static FEMFactoryBase*     m_Factory;
};
} // end namespace itk

#endif
