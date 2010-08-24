/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNrrdImageIOFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNrrdImageIOFactory_h
#define __itkNrrdImageIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class NrrdImageIOFactory
 * \brief Create instances of NrrdImageIO objects using an object factory.
 */
class ITK_EXPORT NrrdImageIOFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef NrrdImageIOFactory         Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const;

  virtual const char * GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NrrdImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    NrrdImageIOFactory::Pointer nrrdFactory = NrrdImageIOFactory::New();

    ObjectFactoryBase::RegisterFactory(nrrdFactory);
  }

protected:
  NrrdImageIOFactory();
  ~NrrdImageIOFactory();
private:
  NrrdImageIOFactory(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented
};
} // end namespace itk

#endif
