/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoxBoCUBImageIOFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVoxBoCUBImageIOFactory_h
#define __itkVoxBoCUBImageIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class VoxBoCUBImageIOFactory
 *
 * \brief Create instances of VoxBoCUBImageIO objects using an object factory.
 *
 * \author Burstein, Pablo D.; Yushkevich, Paul; Gee, James C.
 *
 * This implementation was contributed as a paper to the Insight Journal
 * http://insight-journal.org/midas/handle.php?handle=1926/303
 *
 */
class ITK_EXPORT VoxBoCUBImageIOFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef VoxBoCUBImageIOFactory     Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const;

  virtual const char * GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VoxBoCUBImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    VoxBoCUBImageIOFactory::Pointer VoxBoCUBFactory = VoxBoCUBImageIOFactory::New();

    ObjectFactoryBase::RegisterFactory(VoxBoCUBFactory);
  }

protected:
  VoxBoCUBImageIOFactory();
  ~VoxBoCUBImageIOFactory();
private:
  VoxBoCUBImageIOFactory(const Self &); //purposely not implemented
  void operator=(const Self &);         //purposely not implemented
};
} // end namespace itk

#endif
