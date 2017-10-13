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
#ifndef itkMINCImageIOFactory_h
#define itkMINCImageIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"
#include "ITKIOMINCExport.h"

namespace itk
{
/** \class MINCImageIOFactory
 * \brief Create instances of MINCImageIO objects using an object factory.
 *
 * \ingroup ITKIOMINC
 *
 * This code was contributed in the Insight Journal paper:
 * "MINC2.0 IO Support for ITK"
 * by Baghdadi L.
 * https://hdl.handle.net/1926/191
 * http://www.insight-journal.org/browse/publication/88
 *
 * And Modified by Vladimir S. FONOV during ITK-MINC Hackathon
 *
 */
class ITKIOMINC_EXPORT MINCImageIOFactory : public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef MINCImageIOFactory         Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion() const ITK_OVERRIDE;

  virtual const char * GetDescription() const ITK_OVERRIDE;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MINCImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory()
  {
    MINCImageIOFactory::Pointer MINCFactory = MINCImageIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(MINCFactory);
  }

protected:
  MINCImageIOFactory();
  ~MINCImageIOFactory() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MINCImageIOFactory);

};
} // end namespace itk

#endif
