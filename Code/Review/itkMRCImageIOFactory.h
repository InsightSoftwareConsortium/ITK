/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMRCImageIOFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMRCImageIOFactory_h
#define __itkMRCImageIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class MRCImageIOFactory
 * \brief Create instances of MRCImageIO objects using an object factory.
 */
class ITK_EXPORT MRCImageIOFactory:public ObjectFactoryBase
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

    ObjectFactoryBase::RegisterFactory(vtkFactory);
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
