/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVOLImageIOFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVOLImageIOFactory_h
#define __itkVOLImageIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class VOLImageIOFactory
 * \brief Create instances of VOLImageIO objects using an object factory.
 */
class ITK_EXPORT VOLImageIOFactory : public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef VOLImageIOFactory   Self;
  typedef ObjectFactoryBase  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VOLImageIOFactory, ObjectFactoryBase);
  
  /** Class Methods used to interface with the registered factories. */
  virtual const char* GetITKSourceVersion(void) const;
  virtual const char* GetDescription(void) const;
  
  /** Register one factory of this type.  */
  static void RegisterOneFactory(void)
  {
    VOLImageIOFactory::Pointer volFactory = VOLImageIOFactory::New();
    ObjectFactoryBase::RegisterFactory(volFactory);
  }

protected:
  VOLImageIOFactory();
  ~VOLImageIOFactory();

private:
  VOLImageIOFactory(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};

} // end namespace itk

#endif



