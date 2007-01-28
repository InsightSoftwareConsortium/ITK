/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObjectDuplicator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSpatialObjectDuplicator_h
#define __itkSpatialObjectDuplicator_h

#include "itkObject.h"
#include "itkSpatialObject.h"

namespace itk
{

/** \class SpatialObjectDuplicator
 *  This helper class create an SpatialObject which is perfect 
 *  copy of the input SpatialObject */
template <class TInputSpatialObject>
class ITK_EXPORT SpatialObjectDuplicator : public Object 
{
public:
  /** Standard class typedefs. */
  typedef SpatialObjectDuplicator   Self;
  typedef Object                    Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpatialObjectDuplicator, Object);

  /** Type definitions for the input SpatialObject. */
  typedef TInputSpatialObject                        SpatialObjectType;
  typedef typename TInputSpatialObject::Pointer      SpatialObjectPointer;
  typedef typename TInputSpatialObject::ConstPointer SpatialObjectConstPointer;

  itkStaticConstMacro(ObjectDimension, unsigned int,
                      SpatialObjectType::ObjectDimension);

  typedef SpatialObject<itkGetStaticConstMacro(ObjectDimension)> 
                                                     InternalSpatialObjectType;

  /** Set the input SpatialObject. */
  itkSetConstObjectMacro(Input,SpatialObjectType);
  
  /** Get the output SpatialObject. */
  itkGetObjectMacro(Output,SpatialObjectType);

  /** Compute of the input SpatialObject. */
  void Update(void);

protected:
  SpatialObjectDuplicator();
  virtual ~SpatialObjectDuplicator() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  void CopyObject(const InternalSpatialObjectType* source,
                  InternalSpatialObjectType* destination);

private:
  SpatialObjectDuplicator(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  SpatialObjectConstPointer   m_Input;
  SpatialObjectPointer        m_Output;
  unsigned long               m_InternalSpatialObjectTime;
  
};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialObjectDuplicator.txx"
#endif

#endif /* __itkSpatialObjectDuplicator_h */
