/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDicomImageIOFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDicomImageIOFactory_h
#define __itkDicomImageIOFactory_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class DicomImageIOFactory
 * \brief Create instances of DicomImageIO objects using an object factory.
 */
class ITK_EXPORT DicomImageIOFactory : public ObjectFactoryBase
{
public:  
  /** Standard class typedefs. */
  typedef DicomImageIOFactory   Self;
  typedef ObjectFactoryBase  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Class methods used to interface with the registered factories. */
  virtual const char* GetITKSourceVersion(void) const;
  virtual const char* GetDescription(void) const;
  
  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DicomImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
    {
      DicomImageIOFactory::Pointer DicomFactory = DicomImageIOFactory::New();
      ObjectFactoryBase::RegisterFactory(DicomFactory);
    }

protected:
  DicomImageIOFactory();
  ~DicomImageIOFactory();

private:
  DicomImageIOFactory(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};
  
  
} // end namespace itk

#endif
