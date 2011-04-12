/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVXLReaderFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVXLReaderFactory_h
#define __itkVXLReaderFactory_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkObjectFactoryBase.h"
#include "itkVideoReaderBase.h"

namespace itk
{
/** \class VXLReaderFactory
 * \brief Create instances of VXLReader objects using an object factory.
 */
template <class TImage>
class ITK_EXPORT VXLReaderFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef VXLReaderFactory             Self;
  typedef ObjectFactoryBase           Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const;

  virtual const char * GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  static VXLReaderFactory * FactoryNew() { return new VXLReaderFactory; }

  /** Runtime type information (and related methods). */
  itkTypeMacro(VXLReaderFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    VXLReaderFactory::Pointer VXLFactory = VXLReaderFactory::New();

    ObjectFactoryBase::RegisterFactory(VXLFactory);
  }

protected:
  VXLReaderFactory();
  ~VXLReaderFactory();
private:
  VXLReaderFactory(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVXLReaderFactory.txx"
#endif

#endif