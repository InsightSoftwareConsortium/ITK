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
#ifndef itkSpatialObjectFactoryBase_h
#define itkSpatialObjectFactoryBase_h

#include "itkObjectFactoryBase.h"
#include "itkSpatialObjectExport.h"
#include "ITKSpatialObjectsExport.h"

namespace itk
{
/**
 *\class SpatialObjectFactoryBase
 * \brief Create instances of SpatialObjects
 * \ingroup ITKSpatialObjects
 */

class ITKSpatialObjects_EXPORT SpatialObjectFactoryBase : public ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SpatialObjectFactoryBase);

  /** Standard class type aliases. */
  using Self = SpatialObjectFactoryBase;
  using Superclass = ObjectFactoryBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Class methods used to interface with the registered factories. */
  const char *
  GetITKSourceVersion() const override;

  const char *
  GetDescription() const override;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpatialObjectFactoryBase, ObjectFactoryBase);

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Register all builtin SpatialObjects */
  static void
  RegisterDefaultSpatialObjects();

  /** Register this SpatialObject */
  static SpatialObjectFactoryBase *
  GetFactory()
  {
    if (m_Factory == nullptr)
    {
      // Make and register the factory
      SpatialObjectFactoryBase::Pointer p = SpatialObjectFactoryBase::New();
      m_Factory = p.GetPointer();
      ObjectFactoryBase::RegisterFactory(p);
      p->RegisterDefaultSpatialObjects();
    }
    return m_Factory;
  }

  void
  RegisterSpatialObject(const char *               classOverride,
                        const char *               overrideClassName,
                        const char *               description,
                        bool                       enableFlag,
                        CreateObjectFunctionBase * createFunction)
  {
    this->RegisterOverride(classOverride, overrideClassName, description, enableFlag, createFunction);
  }

protected:
  SpatialObjectFactoryBase();
  ~SpatialObjectFactoryBase() override;

private:
  static SpatialObjectFactoryBase * m_Factory;
};
} // end namespace itk
#endif
