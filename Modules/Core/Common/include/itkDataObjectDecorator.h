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
#ifndef itkDataObjectDecorator_h
#define itkDataObjectDecorator_h

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
class ITK_TEMPLATE_EXPORT DataObjectDecorator:public DataObject
{
public:
  /** Standard typedefs. */
  typedef DataObjectDecorator        Self;
  typedef DataObject                 Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Typedef for the component type (object being decorated) */
  typedef T                        ComponentType;
  typedef typename T::Pointer      ComponentPointer;
  typedef typename T::ConstPointer ComponentConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DataObjectDecorator, DataObject);

  /** Set the contained object */
  virtual void Set( const ComponentType *val);

  /** Get the contained object */
  virtual const ComponentType * Get() const;
  virtual ComponentType * GetModifiable();

  /** The most recent MTime of this object and the held component */
  virtual ModifiedTimeType GetMTime() const ITK_OVERRIDE;

  /** Restore the data object to its initial state. This means
   *  releasing the help component.
   */
  virtual void Initialize() ITK_OVERRIDE;

  /** \brief Graft the content of one decorator onto another
   *
   * The DataObject is dynamically_cast to this type, if successful
   * then the component pointer is copies to that both decorators
   * refer to the same object.
   */
  virtual void Graft( const DataObject * ) ITK_OVERRIDE;
  void Graft( const Self * decorator );

  /** Method to aid in dynamic Graft of polymorphic types.
   *
   * To this method by default a raw pointer must be used or explicit
   * template parameter must be provided.
   */
  template <typename TOther>
  void Graft( const DataObjectDecorator<TOther> * decorator )
    {
      ComponentType *component = const_cast< ComponentType * >( dynamic_cast< const ComponentType * >( decorator->Get() ) );
      if ( !component  )
        {
        return;
        }
      this->Set( component );
    }

protected:
  DataObjectDecorator();
  ~DataObjectDecorator() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

protected:

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DataObjectDecorator);

  ComponentPointer m_Component;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDataObjectDecorator.hxx"
#endif

#endif
