/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGDCMImageIOFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGDCMImageIOFactory_h
#define __itkGDCMImageIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class GDCMImageIOFactory
 * \brief Create instances of GDCMImageIO objects using an object factory.
 */
class ITK_EXPORT GDCMImageIOFactory : public ObjectFactoryBase
{
public:  
  /** Standard class typedefs. */
  typedef GDCMImageIOFactory  Self;
  typedef ObjectFactoryBase   Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Class methods used to interface with the registered factories. */
  virtual const char* GetITKSourceVersion() const;
  virtual const char* GetDescription() const;
  
  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GDCMImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory()
  {
    GDCMImageIOFactory::Pointer gdcmFactory = GDCMImageIOFactory::New();
    ObjectFactoryBase::RegisterFactory(gdcmFactory);
  }

protected:
  GDCMImageIOFactory();
  ~GDCMImageIOFactory();

private:
  GDCMImageIOFactory(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};
  
  
} // end namespace itk

#endif
