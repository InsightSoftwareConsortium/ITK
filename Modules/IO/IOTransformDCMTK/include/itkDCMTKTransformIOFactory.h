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
#ifndef itkDCMTKTransformIOFactory_h
#define itkDCMTKTransformIOFactory_h

#include "IOTransformDCMTKExport.h"

#include "itkObjectFactoryBase.h"
#include "itkTransformIOBase.h"

namespace itk
{

/** \class DCMTKTransformIOFactory
 *
 * \brief Create instances of DCMTKTransformIO objects using an object factory.
 *
 * \ingroup IOTransformDCMTK
 */
class IOTransformDCMTK_EXPORT DCMTKTransformIOFactory : public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef DCMTKTransformIOFactory  Self;
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
  itkTypeMacro(DCMTKTransformIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    Self::Pointer metaFactory = DCMTKTransformIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(metaFactory);
  }

protected:
  DCMTKTransformIOFactory();
  ~DCMTKTransformIOFactory();
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  DCMTKTransformIOFactory(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented
};

} // end namespace itk

#endif
