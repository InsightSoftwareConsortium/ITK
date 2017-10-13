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
#ifndef itkNiftiImageIOFactory_h
#define itkNiftiImageIOFactory_h
#include "ITKIONIFTIExport.h"


#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class NiftiImageIOFactory
   * \brief Create instances of NiftiImageIO objects using an object factory.
   * \ingroup ITKIONIFTI
   */
class ITKIONIFTI_EXPORT NiftiImageIOFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef NiftiImageIOFactory        Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const ITK_OVERRIDE;

  virtual const char * GetDescription(void) const ITK_OVERRIDE;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NiftiImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    NiftiImageIOFactory::Pointer metaFactory = NiftiImageIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(metaFactory);
  }

protected:
  NiftiImageIOFactory();
  ~NiftiImageIOFactory() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(NiftiImageIOFactory);
};
} // end namespace itk

#endif
