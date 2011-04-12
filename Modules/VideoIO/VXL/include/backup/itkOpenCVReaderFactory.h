/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOpenCVReaderFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkOpenCVReaderFactory_h
#define __itkOpenCVReaderFactory_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkObjectFactoryBase.h"
#include "itkVideoReaderBase.h"

namespace itk
{
/** \class VideoIOFactory
 * \brief Create instances of openCVIO objects using an object factory.
 */
template <class TImage>
class ITK_EXPORT OpenCVReaderFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef OpenCVReaderFactory             Self;
  typedef ObjectFactoryBase           Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const;

  virtual const char * GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  static OpenCVReaderFactory * FactoryNew() { return new OpenCVReaderFactory; }

  /** Runtime type information (and related methods). */
  itkTypeMacro(OpenCVReaderFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    OpenCVReaderFactory::Pointer OpenCVFactory = OpenCVReaderFactory::New();

    ObjectFactoryBase::RegisterFactory(OpenCVFactory);
  }

protected:
  OpenCVReaderFactory();
  ~OpenCVReaderFactory();
private:
  OpenCVReaderFactory(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOpenCVReaderFactory.txx"
#endif

#endif