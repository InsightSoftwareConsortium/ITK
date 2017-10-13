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
#ifndef itkSimpleDataObjectDecorator_h
#define itkSimpleDataObjectDecorator_h

#include "itkDataObject.h"
#include "itkObjectFactory.h"

namespace itk
{
/** \class SimpleDataObjectDecorator
 * \brief Decorates any "simple" data type (data types without smart pointers) with a DataObject API
 *
 * SimpleDataObjectDecorator decorates an object with a DataObject
 * API. This allows simple objects to be encapsulated into objects
 * that can be passed as down the pipeline. This decorator is intended
 * to be used on native types (float, int, etc.) or any objects not
 * derived from itkObject.  To decorate a subclass of itkObject, see
 * DataObjectDecorator.
 *
 * The decorator provides two methods Set() and Get() to access the
 * decorated object (referred internally as the component).
 *
 * Note that when an instance of SimpleDataObjectDecorator is created,
 * the component is initialized with its default constructor.
 *
 * SimpleDataObjectDecorator can decorate any simple data type. Two
 * other decorators are provided for decorating pointers.
 * DataObjectDecorator will decorate pointers to subclasses of
 * itkObject (internally storing the pointer in a
 * SmartPointer). AutoPointerDataObjectDecorator will decorate any
 * other pointer and manage the memory deallocationg of the component.
 *
 * \sa DataObjectDecorator
 * \sa AutoPointerDataObjectDecorator
 * \ingroup ITKSystemObjects
 *
 * \ingroup ITKCommon
 */
template< typename T >
class ITK_TEMPLATE_EXPORT SimpleDataObjectDecorator:public DataObject
{
public:
  /** Standard typedefs. */
  typedef SimpleDataObjectDecorator  Self;
  typedef DataObject                 Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Typedef for the component type (object being decorated) */
  typedef T ComponentType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SimpleDataObjectDecorator, DataObject);

  /** Set the contained object */
  virtual void Set(const T & val);

  /** Get the contained object */
  virtual T &       Get() { return m_Component; }
  virtual const T & Get() const { return m_Component; }

protected:
  SimpleDataObjectDecorator();
  ~SimpleDataObjectDecorator() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

protected:

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SimpleDataObjectDecorator);

  ComponentType m_Component;
  bool          m_Initialized;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimpleDataObjectDecorator.hxx"
#endif

#endif
