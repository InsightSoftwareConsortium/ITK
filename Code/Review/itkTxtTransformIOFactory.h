/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkTxtTransformIOFactory.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTxtTransformIOFactory_h
#define __itkTxtTransformIOFactory_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkObjectFactoryBase.h"
#include "itkTransformIOBase.h"

namespace itk
{
/** \class TxtTransformIOFactory
   * \brief Create instances of TxtTransformIO objects using an object factory.
   */
class ITK_EXPORT TxtTransformIOFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef TxtTransformIOFactory      Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const;

  virtual const char * GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TxtTransformIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    TxtTransformIOFactory::Pointer metaFactory = TxtTransformIOFactory::New();

    ObjectFactoryBase::RegisterFactory(metaFactory);
  }

protected:
  TxtTransformIOFactory();
  ~TxtTransformIOFactory();
  virtual void PrintSelf(std::ostream & os, Indent indent) const;

private:
  TxtTransformIOFactory(const Self &); //purposely not implemented
  void operator=(const Self &);        //purposely not implemented
};
} // end namespace itk

#endif
