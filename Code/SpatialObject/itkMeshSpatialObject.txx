/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeshSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __MeshSpatialObject_txx
#define __MeshSpatialObject_txx


#include "itkMeshSpatialObject.h"
#include "itkSize.h"

namespace itk
{

/** Constructor */
template <class TMesh>
MeshSpatialObject< TMesh >
::MeshSpatialObject()
{
  this->SetTypeName("MeshSpatialObject");
  m_Mesh = MeshType::New();
  this->ComputeBoundingBox();
  m_PixelType = typeid(typename TMesh::PixelType).name();
  m_IsInsidePrecision = 1;
}

/** Destructor */
template <class TMesh>
MeshSpatialObject< TMesh >
::~MeshSpatialObject()
{
}

/** Return true if the given point is inside the Mesh */
template <class TMesh>
bool
MeshSpatialObject< TMesh >
::IsEvaluableAt( const PointType & point, unsigned int depth, char * name ) const
{
  return IsInside(point, depth, name);
}

/** Test whether a point is inside or outside the object 
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */ 
template <class TMesh>
bool
MeshSpatialObject< TMesh >
::IsInside( const PointType & point) const
{
  if(!this->GetIndexToWorldTransform()->GetInverse(const_cast<TransformType *>(this->GetInternalInverseTransform())))
    {
    return false;
    }

  PointType transformedPoint = this->GetInternalInverseTransform()->TransformPoint(point);

  if( this->GetBounds()->IsInside(transformedPoint) )
    {     
    typename MeshType::CellsContainerPointer cells =  m_Mesh->GetCells();
    typename MeshType::CellsContainer::ConstIterator it = cells->Begin();
    while(it!=cells->End())
      {
      typename MeshType::CoordRepType position[itkGetStaticConstMacro(Dimension)];
      for(unsigned int i=0;i<itkGetStaticConstMacro(Dimension);i++)
        {
        position[i] = transformedPoint[i];
        }

      typename MeshType::CoordRepType closestPoint[itkGetStaticConstMacro(Dimension)];
      double minDist;

      if(it.Value()->EvaluatePosition(position,m_Mesh->GetPoints(),closestPoint,NULL,&minDist,NULL))
        {
        // If this is a triangle cell we need to check the distance
        if(it.Value()->GetNumberOfPoints() == 3)
          {
          if(minDist <= m_IsInsidePrecision)
            {
            return true;
            }
          }
        else
          {
          return true;
          }
        }
      it++;
      }
    }
  return false;
}


/** Return true if the given point is inside the Mesh */
template <class TMesh>
bool
MeshSpatialObject< TMesh >
::IsInside( const PointType & point, unsigned int depth, char * name ) const
{
  if(name == NULL)
    {
    if(IsInside(point))
      {
      return true;
      }
    }
  else if(strstr(typeid(Self).name(), name))
    {
    if(IsInside(point))
      {
      return true;
      }
    }
  return Superclass::IsInside(point, depth, name);
}

/** Return the value of the Mesh at a specified point 
 *  The value returned is always of type double */
template <class TMesh>
bool 
MeshSpatialObject< TMesh >
::ValueAt( const PointType & point, double & value, unsigned int depth,
           char * name ) const
{
  if( IsEvaluableAt( point, 0, name ) )
    {
    value = this->GetDefaultInsideValue();
    return true;
    }
  else
    {
    if( Superclass::IsEvaluableAt(point, depth, name) )
      {
      Superclass::ValueAt(point, value, depth, name);
      return true;
      }
    else
      {
      value = this->GetDefaultOutsideValue();
      return false;
      }
    }
  return false;
}

/** Compute the bounds of the object which is the same as the internal mesh */
template <class TMesh>
bool
MeshSpatialObject< TMesh >
::ComputeLocalBoundingBox() const
{
  if( this->GetBoundingBoxChildrenName().empty() 
      || strstr(typeid(Self).name(), this->GetBoundingBoxChildrenName().c_str()) )
    {
    PointType pnt;
    PointType pnt2;

    for(unsigned int i=0;i<itkGetStaticConstMacro(Dimension);i++)
      {
      pnt[i] = m_Mesh->GetBoundingBox()->GetBounds()[2*i];
      pnt2[i] = m_Mesh->GetBoundingBox()->GetBounds()[2*i+1];
      }
      
    pnt = this->GetIndexToWorldTransform()->TransformPoint(pnt);
    pnt2 = this->GetIndexToWorldTransform()->TransformPoint(pnt2);
         
    const_cast<BoundingBoxType *>(this->GetBounds())->SetMinimum(pnt);
    const_cast<BoundingBoxType *>(this->GetBounds())->SetMaximum(pnt2);
    }
  return true;
}

/** Set the Mesh in the spatial object */
template <class TMesh>
void
MeshSpatialObject< TMesh >
::SetMesh( MeshType * mesh )
{
  m_Mesh = mesh;
  m_Mesh->Modified();
  this->ComputeBoundingBox();
}

/** Get the Mesh inside the spatial object */
template <class TMesh>
typename MeshSpatialObject< TMesh >::MeshType *
MeshSpatialObject< TMesh >
::GetMesh( void )
{
  return m_Mesh.GetPointer();
}

/** Print the object */
template <class TMesh>
void
MeshSpatialObject< TMesh >
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);
  os << "Mesh: " << std::endl;
  os << "m_IsInsidePrecision: " << m_IsInsidePrecision << std::endl; 
  os << indent << m_Mesh << std::endl;
}

/** Get the modification time */
template <class TMesh>
unsigned long 
MeshSpatialObject< TMesh >
::GetMTime( void ) const
{
  unsigned long latestMTime = Superclass::GetMTime();
  unsigned long MeshMTime = m_Mesh->GetMTime();
    
  if( MeshMTime > latestMTime )
    {
    latestMTime = MeshMTime;
    }

  return latestMTime; 
}


} // end namespace itk

#endif //__MeshSpatialObject_txx
