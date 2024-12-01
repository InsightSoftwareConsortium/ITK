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
#ifndef itkMZ3MeshIOFactory_h
#define itkMZ3MeshIOFactory_h
#include "IOMeshMZ3Export.h"

#include "itkMeshIOBase.h"
#include "itkObjectFactoryBase.h"

namespace itk
{
/**
 * \class MZ3MeshIOFactory
 * \brief Create instances of MZ3MeshIO objects using an object factory.
 * \ingroup ITKIOMeshMZ3
 */
class IOMeshMZ3_EXPORT MZ3MeshIOFactory : public ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MZ3MeshIOFactory);

  /** Standard class type aliases. */
  using Self = MZ3MeshIOFactory;
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

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(MZ3MeshIOFactory);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    auto byuFactory = MZ3MeshIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(byuFactory);
  }

protected:
  MZ3MeshIOFactory();
  ~MZ3MeshIOFactory() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};

} // end namespace itk

#endif
