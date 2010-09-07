/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkJPEG2000ImageIOFactory.h,v $
  Language:  C++
  Date:      $Date: 2007/03/22 14:28:51 $
  Version:   $Revision: 1.15 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkJPEG2000ImageIOFactory_h
#define __itkJPEG2000ImageIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class JPEG2000ImageIOFactory
 * \brief Supports for the JPEG2000 file format based on openjpeg
 *
 *  JPEG2000 offers a large collection of interesting features including:
 *  compression (lossless and lossy), streaming, multi-channel images.
 *
 */
class ITK_EXPORT JPEG2000ImageIOFactory:public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef JPEG2000ImageIOFactory     Self;
  typedef ObjectFactoryBase          Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion() const;

  virtual const char * GetDescription() const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);
  static JPEG2000ImageIOFactory * FactoryNew() { return new JPEG2000ImageIOFactory; }

  /** Run-time type information (and related methods). */
  itkTypeMacro(JPEG2000ImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory()
  {
    JPEG2000ImageIOFactory::Pointer metaFactory = JPEG2000ImageIOFactory::New();

    ObjectFactoryBase::RegisterFactory(metaFactory);
  }

protected:
  JPEG2000ImageIOFactory();
  ~JPEG2000ImageIOFactory();
private:
  JPEG2000ImageIOFactory(const Self &); //purposely not implemented
  void operator=(const Self &);         //purposely not implemented
};
} // end namespace itk

#endif
