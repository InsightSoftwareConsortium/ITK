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
  /** Standard class typedefs. */
  typedef FileFreeImageIOFactory    Self;
  typedef ObjectFactoryBase         Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char* GetITKSourceVersion() const;
  virtual const char* GetDescription() const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);
  static FileFreeImageIOFactory* FactoryNew() { return new FileFreeImageIOFactory;}

  /** Run-time type information (and related methods). */
  itkTypeMacro(FileFreeImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    FileFreeImageIOFactory::Pointer fileFreeFactory = FileFreeImageIOFactory::New();
    ObjectFactoryBase::RegisterFactory(fileFreeFactory);
  }

protected:
  FileFreeImageIOFactory();
  ~FileFreeImageIOFactory();

private:
  FileFreeImageIOFactory(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#endif
