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
#ifndef itkFreeSurferAsciiMeshIOFactory_h
#define itkFreeSurferAsciiMeshIOFactory_h
#include "ITKIOMeshFreeSurferExport.h"

#include "itkObjectFactoryBase.h"
#include "itkMeshIOBase.h"

namespace itk
{
/** \class FreeSurferAsciiMeshIOFactory
 * \brief Create instances of FreeSurferAsciiMeshIO objects using an object factory.
 * \ingroup ITKIOMeshFreeSurfer
 */
class ITKIOMeshFreeSurfer_EXPORT FreeSurferAsciiMeshIOFactory : public ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FreeSurferAsciiMeshIOFactory);

  /** Standard class type aliases. */
  using Self = FreeSurferAsciiMeshIOFactory;
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
  itkTypeMacro(FreeSurferAsciiMeshIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    FreeSurferAsciiMeshIOFactory::Pointer freesurferFactory = FreeSurferAsciiMeshIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(freesurferFactory);
  }

protected:
  FreeSurferAsciiMeshIOFactory();
  ~FreeSurferAsciiMeshIOFactory() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk

#endif
