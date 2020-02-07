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
#ifndef itkFileFreeImageIOFactory_h
#define itkFileFreeImageIOFactory_h

#include "itkObjectFactoryBase.h"

namespace itk
{
/** \class FileFreeImageIOFactory
 * \brief Create instances of FileFreeImageIO objects using an object factory.
 */
class FileFreeImageIOFactory : public ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(FileFreeImageIOFactory);

  /** Standard class type aliases. */
  using Self = FileFreeImageIOFactory;
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
  static FileFreeImageIOFactory *
  FactoryNew()
  {
    return new FileFreeImageIOFactory;
  }

  /** Run-time type information (and related methods). */
  itkTypeMacro(FileFreeImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    FileFreeImageIOFactory::Pointer fileFreeFactory = FileFreeImageIOFactory::New();
    ObjectFactoryBase::RegisterFactory(fileFreeFactory);
  }

protected:
  FileFreeImageIOFactory();
  ~FileFreeImageIOFactory();
};

} // end namespace itk

#endif
