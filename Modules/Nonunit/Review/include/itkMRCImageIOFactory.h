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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef __itkMRCImageIOFactory_h
#define __itkMRCImageIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class MRCImageIOFactory
 * \brief Create instances of MRCImageIO objects using an object factory.
 *
 *  This code was contributed in the Insight Journal paper:
 *  "A Streaming IO Base Class and Support for Streaming the MRC and VTK File Format"
 *  by Lowekamp B., Chen D.
 *  http://www.insight-journal.org/browse/publication/729
 *  http://hdl.handle.net/10380/3171
 *
 * \ingroup ITKReview
 */
class MRCImageIOFactory
  : public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef MRCImageIOFactory          Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class Methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const;

  virtual const char * GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MRCImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    MRCImageIOFactory::Pointer vtkFactory = MRCImageIOFactory::New();

    ObjectFactoryBase::RegisterFactoryInternal(vtkFactory);
  }

protected:
  MRCImageIOFactory();
  ~MRCImageIOFactory();

private:
  MRCImageIOFactory(const Self &); //purposely not implemented
  void operator=(const Self &);    //purposely not implemented

};
} // end namespace itk

#endif
