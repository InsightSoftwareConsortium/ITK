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
#ifndef __itkMINC2ImageIOFactory_h
#define __itkMINC2ImageIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class MINC2ImageIOFactory
 * \brief Create instances of MINC2ImageIO objects using an object factory.
 * \ingroup ITK-Review
 */
class ITK_EXPORT MINC2ImageIOFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef MINC2ImageIOFactory        Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const;

  virtual const char * GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);
  static MINC2ImageIOFactory * FactoryNew() { return new MINC2ImageIOFactory; }
  /** Run-time type information (and related methods). */
  itkTypeMacro(MINC2ImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    MINC2ImageIOFactory::Pointer MINC2Factory = MINC2ImageIOFactory::New();

    ObjectFactoryBase::RegisterFactory(MINC2Factory);
  }

protected:
  MINC2ImageIOFactory();
  ~MINC2ImageIOFactory();
private:
  MINC2ImageIOFactory(const Self &); //purposely not implemented
  void operator=(const Self &);      //purposely not implemented
};
} // end namespace itk

#endif
