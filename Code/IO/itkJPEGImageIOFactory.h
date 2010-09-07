/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkJPEGImageIOFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkJPEGImageIOFactory_h
#define __itkJPEGImageIOFactory_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class JPEGImageIOFactory
 * \brief Create instances of JPEGImageIO objects using an object factory.
 */
class ITK_EXPORT JPEGImageIOFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef JPEGImageIOFactory         Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const;

  virtual const char * GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);
  static JPEGImageIOFactory * FactoryNew() { return new JPEGImageIOFactory; }
  /** Run-time type information (and related methods). */
  itkTypeMacro(JPEGImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    JPEGImageIOFactory::Pointer JPEGFactory = JPEGImageIOFactory::New();

    ObjectFactoryBase::RegisterFactory(JPEGFactory);
  }

protected:
  JPEGImageIOFactory();
  ~JPEGImageIOFactory();
private:
  JPEGImageIOFactory(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented
};
} // end namespace itk

#endif
