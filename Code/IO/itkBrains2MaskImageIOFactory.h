/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBrains2MaskImageIOFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBrains2MaskImageIOFactory_h
#define __itkBrains2MaskImageIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class Brains2MaskImageIOFactory
 * \brief Create instances of Brains2MaskImageIO objects using an object factory.
 */
class ITK_EXPORT Brains2MaskImageIOFactory : public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef Brains2MaskImageIOFactory   Self;
  typedef ObjectFactoryBase  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char* GetITKSourceVersion(void) const;
  virtual const char* GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Brains2MaskImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    Brains2MaskImageIOFactory::Pointer metaFactory = Brains2MaskImageIOFactory::New();
    ObjectFactoryBase::RegisterFactory(metaFactory);
  }

protected:
  Brains2MaskImageIOFactory();
  ~Brains2MaskImageIOFactory();

private:
  Brains2MaskImageIOFactory(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};
} // end namespace itk

#endif
