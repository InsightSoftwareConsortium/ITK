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
  m_TypeName = "MeshSpatialObject";
  m_Mesh = MeshType::New();
  ComputeBoundingBox();
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

/** Return true if the given point is inside the Mesh */
template <class TMesh>
bool
MeshSpatialObject< TMesh >
::IsInside( const PointType & point, unsigned int depth, char * name ) const
{
  if( name == NULL || strstr(typeid(Self).name(), name) )
    {
    const TransformType * giT = GetWorldToIndexTransform();
    PointType p = giT->TransformPoint(point);
    if(m_Bounds->IsInside( p))
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
    value = 1;
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
      value = 0;
      return false;
      }
    }
  return false;
}

/** Compute the bounds of the object which is the same as the internal mesh */
template <class TMesh>
bool
MeshSpatialObject< TMesh >
::ComputeBoundingBox() const
{
  itkDebugMacro( "Computing tube bounding box" );

  if( this->GetMTime() > m_BoundsMTime )
    { 
    bool ret = Superclass::ComputeBoundingBox();

    if( m_BoundingBoxChildrenName.empty() 
        || strstr(typeid(Self).name(), m_BoundingBoxChildrenName.c_str()) )
      {
      PointType pnt;
      PointType pnt2;

      for(unsigned int i=0;i<itkGetStaticConstMacro(Dimension);i++)
        {
        pnt[i] = m_Mesh->GetBoundingBox()->GetBounds()[2*i];
        pnt2[i] = m_Mesh->GetBoundingBox()->GetBounds()[2*i+1];
        }
      
      if(!ret)
        {
        m_Bounds->SetMinimum(pnt);
        m_Bounds->SetMaximum(pnt2);
        }
      else
        {
        m_Bounds->ConsiderPoint(pnt);
        m_Bounds->ConsiderPoint(pnt2);
        }
      }

    m_BoundsMTime = this->GetMTime();
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
  ComputeBoundingBox();
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
