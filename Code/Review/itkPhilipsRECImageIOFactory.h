/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPhilipsRECImageIOFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPhilipsRECImageIOFactory_h
#define __itkPhilipsRECImageIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class PhilipsRECImageIOFactory
 * \brief Create instances of PhilipsRECImageIO objects using an object factory.
 *
 * \author Don C. Bigler
 *         The Pennsylvania State University 2005
 *
 * This implementation was contributed as a paper to the Insight Journal
 * http://insight-journal.org/midas/handle.php?handle=1926/1381
 *
 */
class ITK_EXPORT PhilipsRECImageIOFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef PhilipsRECImageIOFactory   Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const;

  virtual const char * GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);
  static PhilipsRECImageIOFactory * FactoryNew()
  {
    return new PhilipsRECImageIOFactory;
  }

  /** Run-time type information (and related methods). */
  itkTypeMacro(PhilipsRECImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    PhilipsRECImageIOFactory::Pointer factory =
      PhilipsRECImageIOFactory::New();

    ObjectFactoryBase::RegisterFactory(factory);
  }

protected:
  PhilipsRECImageIOFactory();
  ~PhilipsRECImageIOFactory();
private:
  PhilipsRECImageIOFactory(const Self &); //purposely not implemented
  void operator=(const Self &);           //purposely not implemented
};
} // end namespace itk

#endif
