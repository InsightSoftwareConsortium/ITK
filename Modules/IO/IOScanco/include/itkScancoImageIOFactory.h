/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
  /** Standard class typedefs. */
  typedef ScancoImageIOFactory     Self;
  typedef ObjectFactoryBase        Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char *
  GetITKSourceVersion() const ITK_OVERRIDE;

  virtual const char *
  GetDescription() const ITK_OVERRIDE;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScancoImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    ScancoImageIOFactory::Pointer scancoFactory = ScancoImageIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(scanocFactory);
  }

protected:
  ScancoImageIOFactory();
  ~ScancoImageIOFactory();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ScancoImageIOFactory);
};
} // end namespace itk

#endif
