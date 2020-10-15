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
#ifndef itkNrrdImageIOFactory_h
#define itkNrrdImageIOFactory_h
#include "ITKIONRRDExport.h"

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/**
 *\class NrrdImageIOFactory
 * \brief Create instances of NrrdImageIO objects using an object factory.
 * \ingroup ITKIONRRD
 */
class ITKIONRRD_EXPORT NrrdImageIOFactory : public ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(NrrdImageIOFactory);

  /** Standard class type aliases. */
  using Self = NrrdImageIOFactory;
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
  itkTypeMacro(NrrdImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    NrrdImageIOFactory::Pointer nrrdFactory = NrrdImageIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(nrrdFactory);
  }

protected:
  NrrdImageIOFactory();
  ~NrrdImageIOFactory() override;
};
} // end namespace itk

#endif
