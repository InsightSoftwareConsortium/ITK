/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPNGImageIOFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPNGImageIOFactory_h
#define __itkPNGImageIOFactory_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class PNGImageIOFactory
 * \brief Create instances of PNGImageIO objects using an object factory.
 */
class ITK_EXPORT PNGImageIOFactory : public ObjectFactoryBase
{
public:  
  /** Standard class typedefs. */
  typedef PNGImageIOFactory   Self;
  typedef ObjectFactoryBase  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Class methods used to interface with the registered factories. */
  virtual const char* GetITKSourceVersion(void) const;
  virtual const char* GetDescription(void) const;
    
  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);
  static PNGImageIOFactory* FactoryNew() { return new PNGImageIOFactory;}
  /** Run-time type information (and related methods). */
  itkTypeMacro(PNGImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
    {
      PNGImageIOFactory::Pointer pngFactory = PNGImageIOFactory::New();
      ObjectFactoryBase::RegisterFactory(pngFactory);
    }
  
protected:
  PNGImageIOFactory();
  ~PNGImageIOFactory();

private:
  PNGImageIOFactory(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};
extern "C"   
#ifdef _WIN32  
__declspec( dllexport ) 
#endif
itk::ObjectFactoryBase* itkLoad();
  
  
} // end namespace itk

#endif
