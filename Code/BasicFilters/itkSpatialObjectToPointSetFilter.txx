/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkSpatialObjectToPointSetFilter.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSpatialObjectToPointSetFilter_txx
#define _itkSpatialObjectToPointSetFilter_txx

#include "itkSpatialObjectToPointSetFilter.h"

namespace itk
{

/** Constructor */
template <class TInputSpatialObject, class TOutputPointSet>
SpatialObjectToPointSetFilter<TInputSpatialObject,TOutputPointSet>
::SpatialObjectToPointSetFilter()
{
  this->SetNumberOfRequiredInputs(1);
  m_ChildrenDepth = 0;
  m_SamplingFactor = 1;
}

/** Destructor */
template <class TInputSpatialObject, class TOutputPointSet>
SpatialObjectToPointSetFilter<TInputSpatialObject,TOutputPointSet>
::~SpatialObjectToPointSetFilter()
{
}
  

/** Set the Input SpatialObject */
template <class TInputSpatialObject, class TOutputPointSet>
void 
SpatialObjectToPointSetFilter<TInputSpatialObject,TOutputPointSet>
::SetInput(const InputSpatialObjectType *input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(0, 
                                   const_cast< InputSpatialObjectType * >( input ) );
}


/** Connect one of the operands  */
template <class TInputSpatialObject, class TOutputPointSet>
void
SpatialObjectToPointSetFilter<TInputSpatialObject,TOutputPointSet>
::SetInput( unsigned int index, const TInputSpatialObject * object ) 
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(index, 
                                   const_cast< TInputSpatialObject *>( object ) );
}


/** Get the input Spatial Object */
template <class TInputSpatialObject, class TOutputPointSet>
const typename SpatialObjectToPointSetFilter<TInputSpatialObject,TOutputPointSet>::InputSpatialObjectType *
SpatialObjectToPointSetFilter<TInputSpatialObject,TOutputPointSet>
::GetInput(void) 
{
  if (this->GetNumberOfInputs() < 1)
    {
    return 0;
    }
  
  return static_cast<const TInputSpatialObject * >
    (this->ProcessObject::GetInput(0) );
}
  
/** Get the input Spatial Object */
template <class TInputSpatialObject, class TOutputPointSet>
const typename SpatialObjectToPointSetFilter<TInputSpatialObject,TOutputPointSet>::InputSpatialObjectType *
SpatialObjectToPointSetFilter<TInputSpatialObject,TOutputPointSet>
::GetInput(unsigned int idx)
{
  return static_cast< const TInputSpatialObject * >
    (this->ProcessObject::GetInput(idx));
}


/** Update */
template <class TInputSpatialObject, class TOutputPointSet>
void
SpatialObjectToPointSetFilter<TInputSpatialObject,TOutputPointSet>
::GenerateData(void)
{
  itkDebugMacro(<< "SpatialObjectToPointSetFilter::Update() called");

  // Get the input and output pointers 
  const InputSpatialObjectType * inputObject  = this->GetInput();
  typename OutputPointSetType::Pointer  outputPointSet = this->GetOutput();                                             
 
  // Look for the number of points to allocate
  unsigned long numberOfPoints = 0;
  if(dynamic_cast<const PointBasedSpatialObjectType*>(inputObject))
    {
    numberOfPoints = dynamic_cast<const PointBasedSpatialObjectType*>(inputObject)->GetNumberOfPoints()/m_SamplingFactor;
    }

  ChildrenListType* children = inputObject->GetChildren(m_ChildrenDepth);
  typename ChildrenListType::const_iterator it = children->begin();

  for(;it!=children->end();it++)
    {
    if(dynamic_cast<const PointBasedSpatialObjectType*>((*it).GetPointer()))
      {
      numberOfPoints += dynamic_cast<const PointBasedSpatialObjectType*>((*it).GetPointer())->GetNumberOfPoints()/m_SamplingFactor;
      }
    }
  
  typedef typename OutputPointSetType::PointDataContainer DataContainer;
  outputPointSet->SetPointData( DataContainer::New());

  outputPointSet->GetPoints()->Reserve( numberOfPoints );
  outputPointSet->GetPointData()->Reserve( numberOfPoints );

  typename OutputPointSetType::PointIdentifier  pointId = 0;
  typename OutputPointSetType::PointType  point;

  // add the object it itself
  unsigned long n = 0;
  if(dynamic_cast<const PointBasedSpatialObjectType*>(inputObject))
    {
    n = dynamic_cast<const PointBasedSpatialObjectType*>(inputObject)->GetNumberOfPoints();
    for(unsigned int i=0;i<n;i+=m_SamplingFactor)
      {
      typename InputSpatialObjectType::PointType transformedPoint
        =  inputObject->GetIndexToWorldTransform()->TransformPoint(
            dynamic_cast<const PointBasedSpatialObjectType*>(inputObject)->GetPoint(i)->GetPosition());

      for(unsigned int j=0;j< itkGetStaticConstMacro(ObjectDimension) ;j++)
        {
        point[j] = transformedPoint[j];
        }
      outputPointSet->SetPoint(pointId++, point );
      }
    }

  // then add children points
  it = children->begin();

  for(;it!=children->end();it++)
    {
    if(dynamic_cast<const PointBasedSpatialObjectType*>((*it).GetPointer()))
      {
      unsigned long n = dynamic_cast<const PointBasedSpatialObjectType*>((*it).GetPointer())->GetNumberOfPoints();
      for(unsigned int i=0;i<n;i+=m_SamplingFactor)
        {
        typename InputSpatialObjectType::PointType transformedPoint
        =  inputObject->GetIndexToWorldTransform()->TransformPoint(dynamic_cast<const PointBasedSpatialObjectType*>((*it).GetPointer())->GetPoint(i)->GetPosition());

        for(unsigned int j=0;j< itkGetStaticConstMacro(ObjectDimension) ;j++)
          {
          point[j] = transformedPoint[j];
          }
        outputPointSet->SetPoint(pointId++, point );
        } 
      }
    }

  delete children;

  itkDebugMacro(<< "SpatialObjectToPointSetFilter::Update() finished");

} // end update function  


template<class TInputSpatialObject, class TOutputPointSet>
void 
SpatialObjectToPointSetFilter<TInputSpatialObject,TOutputPointSet>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Children depth : " << m_ChildrenDepth << std::endl;
  os << indent << "Sampling Factor : " << m_SamplingFactor << std::endl;
}



} // end namespace itk

#endif
