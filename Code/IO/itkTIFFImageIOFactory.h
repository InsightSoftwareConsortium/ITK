/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTIFFImageIOFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTIFFImageIOFactory_h
#define __itkTIFFImageIOFactory_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class TIFFImageIOFactory
 * \brief Create instances of TIFFImageIO objects using an object factory.
 */
class ITK_EXPORT TIFFImageIOFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef TIFFImageIOFactory         Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const;

  virtual const char * GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);
  static TIFFImageIOFactory * FactoryNew() { return new TIFFImageIOFactory; }
  /** Run-time type information (and related methods). */
  itkTypeMacro(TIFFImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    TIFFImageIOFactory::Pointer TIFFFactory = TIFFImageIOFactory::New();

    ObjectFactoryBase::RegisterFactory(TIFFFactory);
  }

protected:
  TIFFImageIOFactory();
  ~TIFFImageIOFactory();
private:
  TIFFImageIOFactory(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented
};
} // end namespace itk

#endif
