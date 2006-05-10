/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCreateObjectFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCreateObjectFunction_h
#define __itkCreateObjectFunction_h

#include "itkObject.h"

namespace itk
{

/** \class CreateObjectFunctionBase
 * \brief Define API for object creation callback functions.
 *
 * \ingroup ITKSystemObjects
 */
class CreateObjectFunctionBase: public Object
{
public:
  /** Standard typedefs. */
  typedef CreateObjectFunctionBase  Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Create an object and return a pointer to it as an
   * itk::LightObject. */
  virtual SmartPointer<LightObject> CreateObject() = 0;

protected:
  CreateObjectFunctionBase() {}
  ~CreateObjectFunctionBase() {}
  
private:
  CreateObjectFunctionBase(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented  
};


/** \class CreateObjectFunction
 * \brief CreateObjectFunction is used to create callback functions that
 * create ITK Objects for use with the itk::ObjectFactory.
 * 
 * \ingroup ITKSystemObjects
 */
template <class T>
class CreateObjectFunction : public CreateObjectFunctionBase
{
public:
  /** Standard class typedefs. */
  typedef CreateObjectFunction  Self;
  typedef SmartPointer<Self>    Pointer;
    
  /** Methods from itk:LightObject. */
  itkFactorylessNewMacro(Self);
  LightObject::Pointer CreateObject() { return T::New().GetPointer(); }

protected:
  CreateObjectFunction() {}
  ~CreateObjectFunction() {}
  
private:
  CreateObjectFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#endif


