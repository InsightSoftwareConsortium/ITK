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
#ifndef itkAnalyzeObjectLabelMapImageIOFactory_h
#define itkAnalyzeObjectLabelMapImageIOFactory_h

#ifdef _MSC_VER
#  pragma warning(disable : 4786)
#endif

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class AnalyzeObjectLabelMapImageIOFactory
 *  \ingroup AnalyzeObjectMapIO
 *  \brief Create instances of AnalyzeObjectLabelMapImageIO objects using an object factory.
 */
class ITK_EXPORT AnalyzeObjectLabelMapImageIOFactory : public ObjectFactoryBase
{
public:
  /** Standard class type alias. */
  using Self = AnalyzeObjectLabelMapImageIOFactory;
  using Superclass = ObjectFactoryBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Class methods used to interface with the registered factories. */
  const char *
  GetITKSourceVersion(void) const override;

  const char *
  GetDescription(void) const override;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(AnalyzeObjectLabelMapImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory(void)
  {
    AnalyzeObjectLabelMapImageIOFactory::Pointer metaFactory = AnalyzeObjectLabelMapImageIOFactory::New();
    ObjectFactoryBase::RegisterFactory(metaFactory);
  }

protected:
  AnalyzeObjectLabelMapImageIOFactory();
  ~AnalyzeObjectLabelMapImageIOFactory() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  AnalyzeObjectLabelMapImageIOFactory(const Self &) = delete; // purposely not implemented
  void
  operator=(const Self &) = delete; // purposely not implemented
};

} // end namespace itk

#endif
