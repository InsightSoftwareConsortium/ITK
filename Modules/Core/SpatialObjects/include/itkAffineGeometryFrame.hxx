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
#ifndef __itkAffineGeometryFrame_hxx
#define __itkAffineGeometryFrame_hxx

#include "itkAffineGeometryFrame.h"

namespace itk
{
/** Constructor */
template< class TScalarType, unsigned int NDimensions >
AffineGeometryFrame< TScalarType, NDimensions >
::AffineGeometryFrame()
{
  m_BoundingBox = NULL;
  m_IndexToObjectTransform = TransformType::New();
  m_IndexToObjectTransform->SetIdentity();
  m_ObjectToNodeTransform = TransformType::New();
  m_ObjectToNodeTransform->SetIdentity();
  m_IndexToNodeTransform = TransformType::New();
  m_IndexToNodeTransform->SetIdentity();
  m_IndexToWorldTransform = 0;
}

/** Destructor */
template< class TScalarType, unsigned int NDimensions >
AffineGeometryFrame< TScalarType, NDimensions >
::~AffineGeometryFrame()
{}

/** Initialize the transform */
template< class TScalarType, unsigned int NDimensions >
void AffineGeometryFrame< TScalarType, NDimensions >
::Initialize()
{
  TScalarType  b[2 * NDimensions];
  unsigned int i;

  for ( i = 0; i < 2 * NDimensions; ++i )
    {
    b[i] = i % 2 - 1;
    }
  SetBounds(b);
  m_IndexToObjectTransform = TransformType::New();
  m_IndexToObjectTransform->SetIdentity();
  m_ObjectToNodeTransform = TransformType::New();
  m_ObjectToNodeTransform->SetIdentity();
}

/** Set the bounds */
template< class TScalarType, unsigned int NDimensions >
void AffineGeometryFrame< TScalarType, NDimensions >
::SetBounds(const BoundsArrayType & bounds)
{
  SetBoundsArray(bounds, m_BoundingBox);
}

/** Set the bounds array */
template< class TScalarType, unsigned int NDimensions >
void AffineGeometryFrame< TScalarType, NDimensions >
::SetBoundsArray(const BoundsArrayType & bounds, BoundingBoxPointer & boundingBox)
{
  boundingBox = BoundingBoxType::New();

  typename BoundingBoxType::PointsContainer::Pointer pointscontainer =
    BoundingBoxType::PointsContainer::New();
  typename BoundingBoxType::PointType p;
  typename BoundingBoxType::PointIdentifier pointid;

  for ( pointid = 0; pointid < 2; ++pointid )
    {
    unsigned int i;
    for ( i = 0; i < NDimensions; ++i )
      {
      p[i] = bounds[2 * i + pointid];
      }
    pointscontainer->InsertElement(pointid, p);
    }

  boundingBox->SetPoints(pointscontainer);
  boundingBox->ComputeBoundingBox();
  this->Modified();
}

/** Clone the geometry */
template< class TScalarType, unsigned int NDimensions >
typename AffineGeometryFrame< TScalarType, NDimensions >::Pointer
AffineGeometryFrame< TScalarType, NDimensions >
::Clone() const
{
  typename Self::Pointer newGeometry = Self::New();
  newGeometry->Initialize();
  InitializeGeometry(newGeometry);
  return newGeometry;
}

/** Initialize the geometry */
template< class TScalarType, unsigned int NDimensions >
void
AffineGeometryFrame< TScalarType, NDimensions >
::InitializeGeometry(AffineGeometryFrame *newGeometry) const
{
  newGeometry->SetBounds( m_BoundingBox->GetBounds() );
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

  if ( m_IndexToWorldTransform )
    {
    typename TransformType::Pointer indexToWorldTransform =
      TransformType::New();
    indexToWorldTransform->SetCenter( m_IndexToWorldTransform->GetCenter() );
    indexToWorldTransform->SetMatrix( m_IndexToWorldTransform->GetMatrix() );
    indexToWorldTransform->SetOffset( m_IndexToWorldTransform->GetOffset() );
    newGeometry->SetIndexToWorldTransform(indexToWorldTransform);
    }
}

/** Print the object */
template< class TScalarType, unsigned int NDimensions >
void
AffineGeometryFrame< TScalarType, NDimensions >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  if ( m_BoundingBox )
    {
    os << indent << "BoundingBox: "
       << m_BoundingBox << std::endl;
    }
  os << indent << "IndexToObjectTransform: "
     << m_IndexToObjectTransform << std::endl;
  os << indent << "ObjectToNodeTransform: "
     << m_ObjectToNodeTransform << std::endl;
  os << indent << "IndexToNodeTransform: "
     << m_IndexToNodeTransform << std::endl;
  if ( m_IndexToWorldTransform )
    {
    os << indent << "IndexToWorldTransform: "
       << m_IndexToWorldTransform << std::endl;
    }
}
} //namespace

#endif
