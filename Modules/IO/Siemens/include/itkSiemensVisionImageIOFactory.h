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
#ifndef __itkSiemensVisionImageIOFactory_h
#define __itkSiemensVisionImageIOFactory_h


#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class SiemensVisionImageIOFactory
   * \brief Create instances of SiemensVisionImageIO objects using an object factory.
   * \ingroup ITKIOSiemens
   */
class SiemensVisionImageIOFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef SiemensVisionImageIOFactory Self;
  typedef ObjectFactoryBase           Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const;

  virtual const char * GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SiemensVisionImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    SiemensVisionImageIOFactory::Pointer metaFactory = SiemensVisionImageIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(metaFactory);
  }

protected:
  SiemensVisionImageIOFactory();
  ~SiemensVisionImageIOFactory();
  virtual void PrintSelf(std::ostream & os, Indent indent) const;

private:
  SiemensVisionImageIOFactory(const Self &); //purposely not implemented
  void operator=(const Self &);              //purposely not implemented
};
} // end namespace itk

#endif
