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
#ifndef itkMINCImageIOFactory_h
#define itkMINCImageIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"
#include "ITKIOMINCExport.h"

namespace itk
{
/**
 *\class MINCImageIOFactory
 * \brief Create instances of MINCImageIO objects using an object factory.
 *
 * \ingroup ITKIOMINC
 *
 * This code was contributed in the Insight Journal paper:
 * "MINC2.0 IO Support for ITK"
 * by Baghdadi L.
 * https://www.insight-journal.org/browse/publication/88
 *
 * And Modified by Vladimir S. FONOV during ITK-MINC Hackathon
 *
 */
class ITKIOMINC_EXPORT MINCImageIOFactory : public ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MINCImageIOFactory);

  /** Standard class type aliases. */
  using Self = MINCImageIOFactory;
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
  itkTypeMacro(MINCImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    MINCImageIOFactory::Pointer MINCFactory = MINCImageIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(MINCFactory);
  }

protected:
  MINCImageIOFactory();
  ~MINCImageIOFactory() override;
};
} // end namespace itk

#endif
