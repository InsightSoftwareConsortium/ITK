/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBruker2DSEQImageIOFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBruker2DSEQImageIOFactory_h
#define __itkBruker2DSEQImageIOFactory_h

#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class Bruker2DSEQImageIOFactory
 * \brief Create instances of Bruker2DSEQImageIO objects using an object factory.
 *
 * \author Don C. Bigler
 *         The Pennsylvania State University 2005
 *
 * This implementation was contributed as a paper to the Insight Journal
 * http://insight-journal.org/midas/handle.php?handle=1926/1381
 *
 */
class ITK_EXPORT Bruker2DSEQImageIOFactory : public ObjectFactoryBase
{
public:  
  /** Standard class typedefs. */
  typedef Bruker2DSEQImageIOFactory   Self;
  typedef ObjectFactoryBase  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Class methods used to interface with the registered factories. */
  virtual const char* GetITKSourceVersion(void) const;
  virtual const char* GetDescription(void) const;
    
  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);
  static Bruker2DSEQImageIOFactory* FactoryNew() { return new Bruker2DSEQImageIOFactory;}
  /** Run-time type information (and related methods). */
  itkTypeMacro(Bruker2DSEQImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    Bruker2DSEQImageIOFactory::Pointer MINC2Factory = Bruker2DSEQImageIOFactory::New();
    ObjectFactoryBase::RegisterFactory(MINC2Factory);
  }
  
protected:
  Bruker2DSEQImageIOFactory();
  ~Bruker2DSEQImageIOFactory();

private:
  Bruker2DSEQImageIOFactory(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};
  
  
} // end namespace itk

#endif
