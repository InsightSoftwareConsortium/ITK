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
#ifndef __itkBruker2DSEQImageIOFactory_h
#define __itkBruker2DSEQImageIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class Bruker2DSEQImageIOFactory
 * \brief Create instances of Bruker2DSEQImageIO objects using an object factory.
 *
 * \author Don C. Bigler
 *         The Pennsylvania State University 2005
 *
 * This implementation was contributed as a paper to the Insight Journal
 * http://hdl.handle.net/1926/1381
 *
 * \ingroup ITKReview
 */
class Bruker2DSEQImageIOFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef Bruker2DSEQImageIOFactory  Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const;

  virtual const char * GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Bruker2DSEQImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    Bruker2DSEQImageIOFactory::Pointer factory =
      Bruker2DSEQImageIOFactory::New();

    ObjectFactoryBase::RegisterFactory(factory);
  }

protected:
  Bruker2DSEQImageIOFactory();
  virtual ~Bruker2DSEQImageIOFactory();

private:
  Bruker2DSEQImageIOFactory(const Self &); //purposely not implemented
  void operator=(const Self &);            //purposely not implemented
};
} // end namespace itk

#endif
