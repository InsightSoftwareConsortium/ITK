/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDataObjectDecorator.h
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
#ifndef __itkDataObjectDecorator_h
#define __itkDataObjectDecorator_h

#include "itkDataObject.h"

namespace itk
{
/** \class DataObjectDecorator
 * \brief Decorates any subclass of itkObject with a DataObject API
 *
 * DataObjectDecorator decorates an instance of a subclass of
 * itkObject with a DataObject API. This allows any itkObject to be
 * encapsulated into a DataObject that can be passed down the
 * pipeline. To decorate simple types (float, int, std::vector) see
 * SimpleDataObjectDecorator.
 *
 * The decorator provides two methods Set() and Get() to access the
 * decorated object (referred internally as the component).
 *
 * Note that when an instance of DataObjectDecorator is created, the
 * component is initialized with its default constructor (in this case
 * a null pointer).
 *
 * DataObjectDecorator can decorate any subclass of itkObject. Two
 * other decorators are provided. SimpleDataObjectDecorator can
 * encapsulate simple types (float, int, std::vector).
 * AutoPointerDataObjectDecorator will decorate any pointer type (for
 * objects other than subclasses of itkObject) and manage the memory
 * deallocationg of the component.
 *
 * \sa SimpleDataObjectDecorator
 * \sa AutoPointerDataObjectDecorator
 * \ingroup ITKSystemObjects
 *
 */
template< class T >
class ITK_EXPORT DataObjectDecorator:public DataObject
{
public:
  /** Standard typedefs. */
  typedef DataObjectDecorator        Self;
  typedef DataObject                 Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Typedef for the component type (object being decorated) */
  typedef T                        ComponentType;
  typedef typename T::ConstPointer ComponentConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DataObjectDecorator, DataObject);

  /** Set the contained object */
  virtual void Set(const T *val);

  /** Get the contained object */
  virtual const T * Get() const;

protected:
  DataObjectDecorator();
  ~DataObjectDecorator();
  virtual void PrintSelf(std::ostream & os, Indent indent) const;

protected:
private:
  DataObjectDecorator(const Self &); //purposely not implemented
  void operator=(const Self &);      //purposely not implemented

  ComponentConstPointer m_Component;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDataObjectDecorator.txx"
#endif

#endif
