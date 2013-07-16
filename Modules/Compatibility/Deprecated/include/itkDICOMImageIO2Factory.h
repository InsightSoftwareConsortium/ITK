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
#ifndef __itkDICOMImageIO2Factory_h
#define __itkDICOMImageIO2Factory_h
#if !defined( ITK_LEGACY_REMOVE )


#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class DICOMImageIO2Factory
 * \brief Create instances of DICOMImageIO2 objects using an object factory.
 * \deprecated
 * \ingroup ITKDeprecated
 */
class DICOMImageIO2Factory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef DICOMImageIO2Factory       Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const;

  virtual const char * GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DICOMImageIO2Factory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    DICOMImageIO2Factory::Pointer DicomFactory = DICOMImageIO2Factory::New();

    ObjectFactoryBase::RegisterFactory(DicomFactory);
  }

protected:
  DICOMImageIO2Factory();
  ~DICOMImageIO2Factory();

private:
  DICOMImageIO2Factory(const Self &); //purposely not implemented
  void operator=(const Self &);       //purposely not implemented
};
} // end namespace itk

#endif //#if !defined( ITK_LEGACY_REMOVE )
#endif
