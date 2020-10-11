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
#ifndef itkOFFMeshIOFactory_h
#define itkOFFMeshIOFactory_h
#include "ITKIOMeshOFFExport.h"

#include "itkObjectFactoryBase.h"
#include "itkMeshIOBase.h"

namespace itk
{
/**
 *\class OFFMeshIOFactory
 * \brief Create instances of OFFMeshIO objects using an object factory.
 * \ingroup ITKIOMeshOFF
 */
class ITKIOMeshOFF_EXPORT OFFMeshIOFactory : public ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(OFFMeshIOFactory);

  /** Standard class type aliases. */
  using Self = OFFMeshIOFactory;
  using Superclass = ObjectFactoryBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Class methods used to interface with the registered factories. */
  const char *
  GetITKSourceVersion() const override;

  const char *
  GetDescription() const override;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(OFFMeshIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    OFFMeshIOFactory::Pointer offFactory = OFFMeshIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(offFactory);
  }

protected:
  OFFMeshIOFactory();
  ~OFFMeshIOFactory() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk

#endif
