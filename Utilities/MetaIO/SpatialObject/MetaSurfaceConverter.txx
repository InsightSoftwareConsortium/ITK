/*=========================================================================

  Program:   itkUNC
  Module:    MetaSurfaceConverter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
  Author:    Julien Jomier (julien@jomier.com)

  Copyright (c) 2002 CADDLab @ UNC. All rights reserved.
  See itkUNCCopyright.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __MetaSurfaceConverter__txx
#define __MetaSurfaceConverter__txx

#include "MetaSurfaceConverter.h"

/** Constructor */ 
template <unsigned int NDimensions>                                          
MetaSurfaceConverter<NDimensions>
::MetaSurfaceConverter()
{
  
}


/** Convert a metaSurface into an Surface SpatialObject  */
template <unsigned int NDimensions>       
typename MetaSurfaceConverter<NDimensions>::SpatialObjectPointer
MetaSurfaceConverter<NDimensions>
::MetaSurfaceToSurfaceSpatialObject(MetaSurface * Surface)
{ 

  typedef itk::SurfaceSpatialObject<NDimensions> SurfaceSpatialObjectType;
  typename SurfaceSpatialObjectType::Pointer surface = SurfaceSpatialObjectType::New();

  double spacing[NDimensions];
  surface->SetReferenceCount(2);
 
  unsigned int ndims = Surface->NDims();
  for(unsigned int i=0;i<ndims;i++)
  {
    spacing[i]=Surface->ElementSpacing()[i];
  }
  surface->SetSpacing(spacing);
  surface->GetProperty()->SetName((char*)Surface->Name());
  surface->SetParentId(Surface->ParentID());
  surface->SetId(Surface->ID());
  surface->GetProperty()->SetRed(Surface->Color()[0]);
  surface->GetProperty()->SetGreen(Surface->Color()[1]);
  surface->GetProperty()->SetBlue(Surface->Color()[2]);
  surface->GetProperty()->SetAlpha(Surface->Color()[3]);

  typedef typename SurfaceSpatialObjectType::SurfacePointType SurfacePointType;
  typedef SurfacePointType* SurfacePointPointer;

  
  typedef MetaSurface::PointListType ListType;
  ListType::iterator it2 = Surface->GetPoints().begin();
    
  vnl_vector<double> v(ndims);
  
  for(unsigned int id=0;id< Surface->GetPoints().size();id++)
  {
    SurfacePointType pnt;
    
    typedef typename SurfacePointType::PointType PointType;
    PointType point;
    typedef typename SurfacePointType::VectorType VectorType;
    VectorType normal;

    for(unsigned int i=0;i<ndims;i++)
    {
      point[i]=(*it2)->m_X[i];
    }
   
    for(unsigned int i=0;i<ndims;i++)
    {
      normal[i]=(*it2)->m_V[i];
    }


    pnt.SetRed((*it2)->m_Color[0]);
    pnt.SetGreen((*it2)->m_Color[1]);
    pnt.SetBlue((*it2)->m_Color[2]);
    pnt.SetAlpha((*it2)->m_Color[3]);

    pnt.SetPosition(point);
    pnt.SetNormal(normal);

    surface->GetPoints().push_back(pnt);
    it2++;
  }
 
  return surface;
}

/** Convert an Surface SpatialObject into a metaSurface */
template <unsigned int NDimensions>       
MetaSurface*
MetaSurfaceConverter<NDimensions>
::SurfaceSpatialObjectToMetaSurface(SpatialObjectType * spatialObject)
{ 
  MetaSurface* Surface = new MetaSurface(NDimensions);

  // fill in the Surface information
   
  typename SpatialObjectType::PointListType::const_iterator i;
  for(i = dynamic_cast<SpatialObjectType*>(spatialObject)->GetPoints().begin(); i != dynamic_cast<SpatialObjectType*>(spatialObject)->GetPoints().end(); i++)
  {
    SurfacePnt* pnt = new SurfacePnt(NDimensions);
   
    for(unsigned int d=0;d<NDimensions;d++)
    {
      pnt->m_X[d]=(*i).GetPosition()[d];
    }  

    for(unsigned int d=0;d<NDimensions;d++)
    {
      pnt->m_V[d]=(*i).GetNormal()[d];
    }  

    pnt->m_Color[0] = (*i).GetRed();
    pnt->m_Color[1] = (*i).GetGreen();
    pnt->m_Color[2] = (*i).GetBlue();
    pnt->m_Color[3] = (*i).GetAlpha();

    Surface->GetPoints().push_back(pnt); 
  }
    
  if(NDimensions == 2)
  {
    Surface->PointDim("x y v1 v2 red green blue alpha");
  }
  else if (NDimensions == 3)
  {
    Surface->PointDim("x y z v1 v2 v3 red green blue alpha");
  }

  float color[4];
  for(unsigned int i=0;i<4;i++)
  {
    color[i]=spatialObject->GetProperty()->GetColor()[i];
  }

  Surface->Color(color);
  Surface->ID( spatialObject->GetId());
  Surface->ParentID(spatialObject->GetParentId());
  Surface->NPoints(Surface->GetPoints().size());

  return Surface;
}


/** Read a meta file give the type */
template <unsigned int NDimensions>       
typename MetaSurfaceConverter<NDimensions>::SpatialObjectPointer
MetaSurfaceConverter<NDimensions>
::ReadMeta(const char* name)
{
  SpatialObjectPointer spatialObject;
  MetaSurface* Surface = new MetaSurface();
  Surface->Read(name);
  spatialObject = MetaSurfaceToSurfaceSpatialObject(Surface);

  return spatialObject;
}


/** Write a meta Surface file */
template <unsigned int NDimensions>
bool
MetaSurfaceConverter<NDimensions>
::WriteMeta(SpatialObjectType* spatialObject,const char* name)
{
  MetaSurface* Surface = SurfaceSpatialObjectToMetaSurface(spatialObject);
  Surface->Write(name);
  return true;
}

#endif
