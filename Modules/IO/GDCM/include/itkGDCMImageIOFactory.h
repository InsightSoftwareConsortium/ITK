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
#ifndef itkGDCMImageIOFactory_h
#define itkGDCMImageIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"
#include "ITKIOGDCMExport.h"

namespace itk
{
/** \class GDCMImageIOFactory
 * \brief Create instances of GDCMImageIO objects using an object factory.
 * \ingroup ITKIOGDCM
 */
class ITKIOGDCM_EXPORT GDCMImageIOFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef GDCMImageIOFactory         Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion() const ITK_OVERRIDE;

  virtual const char * GetDescription() const ITK_OVERRIDE;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GDCMImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory()
  {
    GDCMImageIOFactory::Pointer gdcmFactory = GDCMImageIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(gdcmFactory);
  }

protected:
  GDCMImageIOFactory();
  ~GDCMImageIOFactory() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GDCMImageIOFactory);
};
} // end namespace itk

#endif
