/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOpenCVViewerFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkOpenCVViewerFactory_h
#define __itkOpenCVViewerFactory_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkObjectFactoryBase.h"
#include "itkVideoViewerBase.h"

namespace itk
{
/** \class VideoViewerFactory
 * \brief Create instances of openCVViewer objects using an object factory.
 */
template <class TImage>
class ITK_EXPORT OpenCVViewerFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef OpenCVViewerFactory             Self;
  typedef ObjectFactoryBase           Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const;

  virtual const char * GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  static OpenCVViewerFactory * FactoryNew() { return new OpenCVViewerFactory; }

  /** Runtime type information (and related methods). */
  itkTypeMacro(OpenCVViewerFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    OpenCVViewerFactory::Pointer OpenCVFactory = OpenCVViewerFactory::New();

    ObjectFactoryBase::RegisterFactory(OpenCVFactory);
  }

protected:
  OpenCVViewerFactory();
  ~OpenCVViewerFactory();
private:
  OpenCVViewerFactory(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOpenCVViewerFactory.txx"
#endif

#endif