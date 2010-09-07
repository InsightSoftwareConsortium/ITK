/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMINC2ImageIOFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMINC2ImageIOFactory_h
#define __itkMINC2ImageIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class MINC2ImageIOFactory
 * \brief Create instances of MINC2ImageIO objects using an object factory.
 */
class ITK_EXPORT MINC2ImageIOFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef MINC2ImageIOFactory        Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const;

  virtual const char * GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);
  static MINC2ImageIOFactory * FactoryNew() { return new MINC2ImageIOFactory; }
  /** Run-time type information (and related methods). */
  itkTypeMacro(MINC2ImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    MINC2ImageIOFactory::Pointer MINC2Factory = MINC2ImageIOFactory::New();

    ObjectFactoryBase::RegisterFactory(MINC2Factory);
  }

protected:
  MINC2ImageIOFactory();
  ~MINC2ImageIOFactory();
private:
  MINC2ImageIOFactory(const Self &); //purposely not implemented
  void operator=(const Self &);      //purposely not implemented
};
} // end namespace itk

#endif
