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
class ITKIOPhilipsREC_EXPORT PhilipsRECImageIOFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef PhilipsRECImageIOFactory   Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion() const ITK_OVERRIDE;

  virtual const char * GetDescription() const ITK_OVERRIDE;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);
  static PhilipsRECImageIOFactory * FactoryNew()
  {
    return new PhilipsRECImageIOFactory;
  }

  /** Run-time type information (and related methods). */
  itkTypeMacro(PhilipsRECImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    PhilipsRECImageIOFactory::Pointer factory =
      PhilipsRECImageIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(factory);
  }

protected:
  PhilipsRECImageIOFactory();
  ~PhilipsRECImageIOFactory() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PhilipsRECImageIOFactory);
};
} // end namespace itk

#endif
