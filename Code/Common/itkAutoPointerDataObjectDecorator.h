/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAutoPointerDataObjectDecorator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAutoPointerDataObjectDecorator_h
#define __itkAutoPointerDataObjectDecorator_h

#include <memory>
#include "itkDataObject.h"

namespace itk
{


/** \class AutoPointerDataObjectDecorator
 * \brief Decorates any pointer to a simple object with a DataObject API using
 * AutoPointer semantics.
 *
 * AutoPointerDataObjectDecorator decorates a pointer to an object
 * with a DataObject API. This allows a pointer to an object to be
 * encapsulated in a DataObject and passed through the pipeline. This
 * object differs from SimpleDataObjectDecorator in that the decorator
 * takes control of deleting the pointer upon destruction.
 *
 * The decorator provides two methods Set() and Get() to access the
 * decorated object (referred internally as the component).
 *
 * Note that when an instance of SimpleDataObjectDecorator is created,
 * the component is initialized with its default constructor (i.e. a
 * null pointer).
 *
 * \sa SimpleDataObjectDecorator
 * \sa DataObjectDecorator
 * \ingroup ITKSystemObjects
 *
 */
template<class T>
class ITK_EXPORT AutoPointerDataObjectDecorator : public DataObject
{
public:
  /** Standard typedefs. */
  typedef AutoPointerDataObjectDecorator   Self;
  typedef DataObject                       Superclass;
  typedef SmartPointer<Self>               Pointer;
  typedef SmartPointer<const Self>         ConstPointer;

  /** Typedef for the component type (object being decorated) */
  typedef T                ComponentType;
  typedef std::auto_ptr<T> ComponentPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(AutoPointerDataObjectDecorator, DataObject);

  /** Set the contained object */
  virtual void Set(T* val);
  
  /** Get the contained object */
  virtual T* Get() { return m_Component.get(); }
  virtual const T* Get() const { return m_Component.get(); }
  
protected:
  AutoPointerDataObjectDecorator();
  ~AutoPointerDataObjectDecorator();
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

protected:

private:
  AutoPointerDataObjectDecorator(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  ComponentPointer m_Component;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAutoPointerDataObjectDecorator.txx"
#endif

#endif
