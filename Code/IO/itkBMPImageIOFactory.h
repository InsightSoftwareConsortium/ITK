/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBMPImageIOFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBMPImageIOFactory_h
#define __itkBMPImageIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class BMPImageIOFactory
 * \brief Create instances of BMPImageIO objects using an object factory.
 */
class ITK_EXPORT BMPImageIOFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef BMPImageIOFactory          Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const;

  virtual const char * GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BMPImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    BMPImageIOFactory::Pointer BMPFactory = BMPImageIOFactory::New();

    ObjectFactoryBase::RegisterFactory(BMPFactory);
  }

protected:
  BMPImageIOFactory();
  ~BMPImageIOFactory();
private:
  BMPImageIOFactory(const Self &); //purposely not implemented
  void operator=(const Self &);    //purposely not implemented
};
} // end namespace itk

#endif
