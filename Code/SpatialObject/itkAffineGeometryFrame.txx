/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAffineGeometryFrame.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAffineGeometryFrame_txx
#define __itkAffineGeometryFrame_txx

#include "itkAffineGeometryFrame.h"

namespace itk
{

/** Constructor */
template<class TScalarType, unsigned int NDimensions>
AffineGeometryFrame<TScalarType, NDimensions>
::AffineGeometryFrame()
{
  m_BoundingBox = NULL;
  m_IndexToObjectTransform = TransformType::New();
  m_IndexToObjectTransform->SetIdentity();
  m_ObjectToNodeTransform = TransformType::New();
  m_ObjectToNodeTransform->SetIdentity();
  m_IndexToWorldTransform = 0;
}

/** Destructor */
template<class TScalarType, unsigned int NDimensions>
AffineGeometryFrame<TScalarType, NDimensions>
::~AffineGeometryFrame()
{

}


/** Initialize the transform */
template<class TScalarType, unsigned int NDimensions>
void AffineGeometryFrame<TScalarType, NDimensions>
::Initialize()
{
  TScalarType b[2*NDimensions];
  unsigned int i;
  for(i=0; i<2*NDimensions; ++i)
    {
    b[i]=i%2-1;
    }
  SetBounds(b); 
  m_IndexToObjectTransform = TransformType::New();
  m_IndexToObjectTransform->SetIdentity();
  m_ObjectToNodeTransform = TransformType::New();
  m_ObjectToNodeTransform->SetIdentity();  
}

/** Set the bounds */
template<class TScalarType, unsigned int NDimensions>
void AffineGeometryFrame<TScalarType, NDimensions>
::SetBounds(const BoundsArrayType& bounds)
{
  SetBoundsArray(bounds, m_BoundingBox);
}

/** Set the bounds array */
template<class TScalarType, unsigned int NDimensions>
void AffineGeometryFrame<TScalarType, NDimensions>
::SetBoundsArray(const BoundsArrayType& bounds, BoundingBoxPointer& boundingBox)
{
  boundingBox = BoundingBoxType::New();
  
  typename BoundingBoxType::PointsContainer::Pointer pointscontainer =
           BoundingBoxType::PointsContainer::New();
  typename BoundingBoxType::PointType p; 
  typename BoundingBoxType::PointIdentifier pointid;
  
  for(pointid=0; pointid<2;++pointid)
    {
    unsigned int i;
    for(i=0; i<NDimensions; ++i)
      {
      p[i] = bounds[2*i+pointid];
      }
    pointscontainer->InsertElement(pointid, p);
    }

  boundingBox->SetPoints(pointscontainer);
  boundingBox->ComputeBoundingBox();
  this->Modified();
}

/** Clone the geometry */
template<class TScalarType, unsigned int NDimensions>
typename AffineGeometryFrame<TScalarType, NDimensions>::Pointer
AffineGeometryFrame<TScalarType, NDimensions>
::Clone() const
{
  typename Self::Pointer newGeometry = Self::New();
  newGeometry->Initialize();
  InitializeGeometry(newGeometry);
  return newGeometry;
}

/** Initialize the geometry */
template<class TScalarType, unsigned int NDimensions>
void 
AffineGeometryFrame<TScalarType, NDimensions>
::InitializeGeometry(AffineGeometryFrame * newGeometry) const
{
  newGeometry->SetBounds(m_BoundingBox->GetBounds());
  // we have to create a new transform!!
  typename TransformType::Pointer indexToObjecttransform = TransformType::New();
  indexToObjecttransform->SetCenter( m_IndexToObjectTransform->GetCenter() );
  indexToObjecttransform->SetMatrix( m_IndexToObjectTransform->GetMatrix() );
  indexToObjecttransform->SetOffset( m_IndexToObjectTransform->GetOffset() );
  newGeometry->SetIndexToObjectTransform(indexToObjecttransform);
  
  typename TransformType::Pointer objectToNodeTransform = TransformType::New();
  objectToNodeTransform->SetCenter( m_ObjectToNodeTransform->GetCenter() );
  objectToNodeTransform->SetMatrix( m_ObjectToNodeTransform->GetMatrix() );
  objectToNodeTransform->SetOffset( m_ObjectToNodeTransform->GetOffset() );
  newGeometry->SetObjectToNodeTransform(objectToNodeTransform);

  if(m_IndexToWorldTransform)
    {
    typename TransformType::Pointer indexToWorldTransform =
             TransformType::New();
    indexToWorldTransform->SetCenter( m_IndexToWorldTransform->GetCenter() );
    indexToWorldTransform->SetMatrix( m_IndexToWorldTransform->GetMatrix() );
    indexToWorldTransform->SetOffset( m_IndexToWorldTransform->GetOffset() );
    newGeometry->SetIndexToWorldTransform(indexToWorldTransform);
    }
}

} //namespace

#endif
