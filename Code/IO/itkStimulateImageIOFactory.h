/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStimulateImageIOFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkStimulateImageIOFactory_h
#define __itkStimulateImageIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class StimulateImageIOFactory
 * \brief Create instances of StimulateImageIO objects using an object factory.
 */
class ITK_EXPORT StimulateImageIOFactory : public ObjectFactoryBase
{
public:  
  /** Standard class typedefs. */
  typedef StimulateImageIOFactory   Self;
  typedef ObjectFactoryBase  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Class Methods used to interface with the registered factories. */
  virtual const char* GetITKSourceVersion(void) const;
  virtual const char* GetDescription(void) const;
    
  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(StimulateImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
    {
      StimulateImageIOFactory::Pointer stimulateFactory = StimulateImageIOFactory::New();
      ObjectFactoryBase::RegisterFactory(stimulateFactory);
    }
  
protected:
  StimulateImageIOFactory();
  ~StimulateImageIOFactory();

private:
  StimulateImageIOFactory(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

  
} // end namespace itk

#endif
