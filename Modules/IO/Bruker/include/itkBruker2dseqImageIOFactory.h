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
#ifndef itkBruker2dseqImageIOFactory_h
#define itkBruker2dseqImageIOFactory_h
#include "ITKIOBrukerExport.h"

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class Bruker2dseqImageIOFactory
 * \brief Create instances of Bruker2dseqImageIO objects using an object factory.
 *
 * \author Don C. Bigler
 *         The Pennsylvania State University 2005
 *
 * This implementation was contributed as a paper to the Insight Journal
 * https://hdl.handle.net/1926/1381
 *
 * \ingroup ITKIOBruker
 */
class ITKIOBruker_EXPORT Bruker2dseqImageIOFactory : public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef Bruker2dseqImageIOFactory  Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const ITK_OVERRIDE;

  virtual const char * GetDescription(void) const ITK_OVERRIDE;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Bruker2dseqImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    Bruker2dseqImageIOFactory::Pointer factory =
      Bruker2dseqImageIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(factory);
  }

protected:
  Bruker2dseqImageIOFactory();
  virtual ~Bruker2dseqImageIOFactory() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(Bruker2dseqImageIOFactory);
};
} // end namespace itk

#endif
