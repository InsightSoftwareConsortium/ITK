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
#ifndef itkPhilipsRECImageIOFactory_h
#define itkPhilipsRECImageIOFactory_h
#include "ITKIOPhilipsRECExport.h"

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class PhilipsRECImageIOFactory
 * \brief Create instances of PhilipsRECImageIO objects using an object factory.
 *
 * \author Don C. Bigler
 *         The Pennsylvania State University 2005
 *
 * This implementation was contributed as a paper to the Insight Journal
 * https://hdl.handle.net/1926/1381
 *
 * \ingroup ITKIOPhilipsREC
 */
class ITKIOPhilipsREC_EXPORT PhilipsRECImageIOFactory : public ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PhilipsRECImageIOFactory);

  /** Standard class type aliases. */
  using Self = PhilipsRECImageIOFactory;
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
  static PhilipsRECImageIOFactory *
  FactoryNew()
  {
    return new PhilipsRECImageIOFactory;
  }

  /** Run-time type information (and related methods). */
  itkTypeMacro(PhilipsRECImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    PhilipsRECImageIOFactory::Pointer factory = PhilipsRECImageIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(factory);
  }

protected:
  PhilipsRECImageIOFactory();
  ~PhilipsRECImageIOFactory() override;
};
} // end namespace itk

#endif
