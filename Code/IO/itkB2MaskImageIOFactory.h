/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkB2MaskImageIOFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkB2MaskImageIOFactory_h
#define __itkB2MaskImageIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class B2MaskImageIOFactory
 * \brief Create instances of B2MaskImageIO objects using an object factory.
 */
class ITK_EXPORT B2MaskImageIOFactory : public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef B2MaskImageIOFactory   Self;
  typedef ObjectFactoryBase  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char* GetITKSourceVersion(void) const;
  virtual const char* GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(B2MaskImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
    {
      B2MaskImageIOFactory::Pointer metaFactory = B2MaskImageIOFactory::New();
      ObjectFactoryBase::RegisterFactory(metaFactory);
    }

protected:
  B2MaskImageIOFactory();
  ~B2MaskImageIOFactory();
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

private:
  B2MaskImageIOFactory(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};
} // end namespace itk

#endif
