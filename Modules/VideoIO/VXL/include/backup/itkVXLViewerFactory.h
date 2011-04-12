/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVXLViewerFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVXLViewerFactory_h
#define __itkVXLViewerFactory_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkObjectFactoryBase.h"
#include "itkVideoViewerBase.h"

namespace itk
{
/** \class VXLViewerFactory
 * \brief Create instances of VXLViewer objects using an object factory.
 */
template <class TImage>
class ITK_EXPORT VXLViewerFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef VXLViewerFactory             Self;
  typedef ObjectFactoryBase           Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const;

  virtual const char * GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  static VXLViewerFactory * FactoryNew() { return new VXLViewerFactory; }

  /** Runtime type information (and related methods). */
  itkTypeMacro(VXLViewerFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    VXLViewerFactory::Pointer VXLFactory = VXLViewerFactory::New();

    ObjectFactoryBase::RegisterFactory(VXLFactory);
  }

protected:
  VXLViewerFactory();
  ~VXLViewerFactory();
private:
  VXLViewerFactory(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVXLViewerFactory.txx"
#endif

#endif