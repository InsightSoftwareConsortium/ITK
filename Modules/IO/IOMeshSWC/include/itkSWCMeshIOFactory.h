/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkSWCMeshIOFactory_h
#define itkSWCMeshIOFactory_h
#include "IOMeshSWCExport.h"

#include "itkMeshIOBase.h"
#include "itkObjectFactoryBase.h"

namespace itk
{
/**
 *\class SWCMeshIOFactory
 * \brief Create instances of SWCMeshIO objects using an object factory.
 * \ingroup IOMeshSWC
 */
class IOMeshSWC_EXPORT SWCMeshIOFactory : public ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SWCMeshIOFactory);

  /** Standard class type aliases. */
  using Self = SWCMeshIOFactory;
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
  itkTypeMacro(SWCMeshIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    auto swcFactory = SWCMeshIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(swcFactory);
  }

protected:
  SWCMeshIOFactory();
  ~SWCMeshIOFactory() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};

} // end namespace itk

#endif
