/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFileFreeImageIOFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFileFreeImageIOFactory_h
#define __itkFileFreeImageIOFactory_h

#include "itkObjectFactoryBase.h"

namespace itk
{
/** \class FileFreeImageIOFactory
 * \brief Create instances of FileFreeImageIO objects using an object factory.
 */
class ITK_EXPORT FileFreeImageIOFactory : public ObjectFactoryBase
{
public:  
  /** Standard class typedefs. */
  typedef FileFreeImageIOFactory    Self;
  typedef ObjectFactoryBase         Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Class methods used to interface with the registered factories. */
  virtual const char* GetITKSourceVersion(void) const;
  virtual const char* GetDescription(void) const;
  
  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);
  static FileFreeImageIOFactory* FactoryNew() { return new FileFreeImageIOFactory;}

  /** Run-time type information (and related methods). */
  itkTypeMacro(FileFreeImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    FileFreeImageIOFactory::Pointer fileFreeFactory = FileFreeImageIOFactory::New();
    ObjectFactoryBase::RegisterFactory(fileFreeFactory);
  }

protected:
  FileFreeImageIOFactory();
  ~FileFreeImageIOFactory();

private:
  FileFreeImageIOFactory(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#endif  
