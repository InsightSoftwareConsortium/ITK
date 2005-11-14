/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBioRadImageIOFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBioRadImageIOFactory_h
#define __itkBioRadImageIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class BioRadImageIOFactory
 * \brief Create instances of BioRadImageIO objects using an object factory.
 */
class ITK_EXPORT BioRadImageIOFactory : public ObjectFactoryBase
{
public:  
  /** Standard class typedefs. */
  typedef BioRadImageIOFactory   Self;
  typedef ObjectFactoryBase  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Class Methods used to interface with the registered factories. */
  virtual const char* GetITKSourceVersion() const;
  virtual const char* GetDescription() const;
    
  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BioRadImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory()
  {
    BioRadImageIOFactory::Pointer bioradFactory = BioRadImageIOFactory::New();
    ObjectFactoryBase::RegisterFactory(bioradFactory);
  }

protected:
  BioRadImageIOFactory();
  ~BioRadImageIOFactory();

private:
  BioRadImageIOFactory(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#endif
