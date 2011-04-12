/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVXLWriterFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVXLWriterFactory_h
#define __itkVXLWriterFactory_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkObjectFactoryBase.h"
#include "itkVideoWriterBase.h"

namespace itk
{
/** \class VXLWriterFactory
 * \brief Create instances of VXLWriter objects using an object factory.
 */
template <class TImage>
class ITK_EXPORT VXLWriterFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef VXLWriterFactory             Self;
  typedef ObjectFactoryBase           Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const;

  virtual const char * GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  static VXLWriterFactory * FactoryNew() { return new VXLWriterFactory; }

  /** Runtime type information (and related methods). */
  itkTypeMacro(VXLWriterFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    VXLWriterFactory::Pointer VXLFactory = VXLWriterFactory::New();

    ObjectFactoryBase::RegisterFactory(VXLFactory);
  }

protected:
  VXLWriterFactory();
  ~VXLWriterFactory();
private:
  VXLWriterFactory(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVXLWriterFactory.txx"
#endif

#endif