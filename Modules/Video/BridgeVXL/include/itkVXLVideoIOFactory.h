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
#ifndef itkVXLVideoIOFactory_h
#define itkVXLVideoIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkVideoIOBase.h"

namespace itk
{
/**
 * \class VXLVideoIOFactory
 * \brief Create instances of VXLVideoIO objects using an object factory.
 *
 * \ingroup ITKVideoBridgeVXL
 */
class VXLVideoIOFactory : public ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VXLVideoIOFactory);

  /** Standard class type aliases. */
  using Self = VXLVideoIOFactory;
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
  itkTypeMacro(VXLVideoIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    auto VXLFactory = VXLVideoIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(VXLFactory);
  }

protected:
  VXLVideoIOFactory();
  ~VXLVideoIOFactory();
};
} // end namespace itk

#endif
