/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef __itkDataObjectDecorator_h
#define __itkDataObjectDecorator_h

#include "itkDataObject.h"
#include "itkObjectFactory.h"

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
 * \ingroup ITKCommon
 */
template< typename T >
class DataObjectDecorator:public DataObject
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
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

protected:

private:
  DataObjectDecorator(const Self &); //purposely not implemented
  void operator=(const Self &);      //purposely not implemented

  ComponentConstPointer m_Component;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDataObjectDecorator.hxx"
#endif

#endif
