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
#ifndef itkScancoImageIOFactory_h
#define itkScancoImageIOFactory_h
#include "IOScancoExport.h"

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class ScancoImageIOFactory
 * \brief Create instances of ScancoImageIO objects using an object factory.
 * \ingroup ITKIOScanco
 */
class IOScanco_EXPORT ScancoImageIOFactory : public ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ScancoImageIOFactory);

  /** Standard class typedefs. */
  using Self = ScancoImageIOFactory;
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
  itkOverrideGetNameOfClassMacro(ScancoImageIOFactory);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    ScancoImageIOFactory::Pointer scancoFactory = ScancoImageIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(scancoFactory);
  }

protected:
  ScancoImageIOFactory();
  ~ScancoImageIOFactory() override = default;
};
} // end namespace itk

#endif
