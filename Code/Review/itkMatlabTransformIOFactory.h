/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkMatlabTransformIOFactory.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMatlabTransformIOFactory_h
#define __itkMatlabTransformIOFactory_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkObjectFactoryBase.h"
#include "itkTransformIOBase.h"

namespace itk
{
/** \class MatlabTransformIOFactory
 *  \brief Create instances of MatlabTransformIO objects using an
 *  object factory.
 */
class ITK_EXPORT MatlabTransformIOFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef MatlabTransformIOFactory   Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const;

  virtual const char * GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MatlabTransformIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    MatlabTransformIOFactory::Pointer metaFactory =
      MatlabTransformIOFactory::New();

    ObjectFactoryBase::RegisterFactory(metaFactory);
  }

protected:
  MatlabTransformIOFactory();
  ~MatlabTransformIOFactory();
  virtual void PrintSelf(std::ostream & os, Indent indent) const;

private:
  MatlabTransformIOFactory(const Self &); //purposely not implemented
  void operator=(const Self &);           //purposely not implemented
};
} // end namespace itk

#endif
