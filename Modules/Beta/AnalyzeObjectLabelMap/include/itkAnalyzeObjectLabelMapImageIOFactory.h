/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    $RCSfile: itkAnalyzeObjectLabelMapImageIOFactory.h,v $
Language:  C++
Date:      $Date: 2007/03/22 14:28:51 $
Version:   $Revision: 1.2 $

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAnalyzeObjectLabelMapImageIOFactory_h
#define __itkAnalyzeObjectLabelMapImageIOFactory_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class AnalyzeObjectLabelMapImageIOFactory
   * \brief Create instances of AnalyzeObjectLabelMapImageIO objects using an object factory.
   */
class ITK_EXPORT AnalyzeObjectLabelMapImageIOFactory : public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef AnalyzeObjectLabelMapImageIOFactory Self;
  typedef ObjectFactoryBase                   Superclass;
  typedef SmartPointer<Self>                  Pointer;
  typedef SmartPointer<const Self>            ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char * GetITKSourceVersion(void) const;

  virtual const char * GetDescription(void) const;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(AnalyzeObjectLabelMapImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    AnalyzeObjectLabelMapImageIOFactory::Pointer metaFactory = AnalyzeObjectLabelMapImageIOFactory::New();
    ObjectFactoryBase::RegisterFactory(metaFactory);
  }

protected:
  AnalyzeObjectLabelMapImageIOFactory();
  ~AnalyzeObjectLabelMapImageIOFactory();
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

private:
  AnalyzeObjectLabelMapImageIOFactory(const Self &); // purposely not implemented
  void operator=(const Self &);                      // purposely not implemented

};

} // end namespace itk

#endif
