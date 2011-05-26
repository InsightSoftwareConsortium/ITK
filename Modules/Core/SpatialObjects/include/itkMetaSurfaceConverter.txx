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
#ifndef __itkMetaSurfaceConverter_txx
#define __itkMetaSurfaceConverter_txx

#include "itkMetaSurfaceConverter.h"

namespace itk
{
/** Constructor */
template< unsigned int NDimensions >
MetaSurfaceConverter< NDimensions >
::MetaSurfaceConverter()
{}

/** Convert a metaSurface into an Surface SpatialObject */
template< unsigned int NDimensions >
typename MetaSurfaceConverter< NDimensions >::SpatialObjectPointer
MetaSurfaceConverter< NDimensions >
::MetaSurfaceToSurfaceSpatialObject(MetaSurface *Surface)
{
  typedef itk::SurfaceSpatialObject< NDimensions > SurfaceSpatialObjectType;
  typename SurfaceSpatialObjectType::Pointer
  surface = SurfaceSpatialObjectType::New();

  double spacing[NDimensions];

  unsigned int ndims = Surface->NDims();
  for ( unsigned int ii = 0; ii < ndims; ii++ )
    {
    spacing[ii] = Surface->ElementSpacing()[ii];
    }
  surface->GetIndexToObjectTransform()->SetScaleComponent(spacing);
  surface->GetProperty()->SetName( Surface->Name() );
  surface->SetId( Surface->ID() );
  surface->SetParentId( Surface->ParentID() );
  surface->GetProperty()->SetRed(Surface->Color()[0]);
  surface->GetProperty()->SetGreen(Surface->Color()[1]);
  surface->GetProperty()->SetBlue(Surface->Color()[2]);
  surface->GetProperty()->SetAlpha(Surface->Color()[3]);

  typedef typename SurfaceSpatialObjectType::SurfacePointType SurfacePointType;
  typedef SurfacePointType *                                  SurfacePointPointer;

  typedef MetaSurface::PointListType ListType;
  ListType::iterator it2 = Surface->GetPoints().begin();

  vnl_vector< double > v(ndims);

  for ( unsigned int identifier = 0; identifier < Surface->GetPoints().size(); identifier++ )
    {
    SurfacePointType pnt;

    typedef typename SurfacePointType::PointType PointType;
    PointType point;
    typedef typename SurfacePointType::VectorType VectorType;
    VectorType normal;

    for ( unsigned int ii = 0; ii < ndims; ii++ )
      {
      point[ii] = ( *it2 )->m_X[ii];
      }

    for ( unsigned int ii = 0; ii < ndims; ii++ )
      {
      normal[ii] = ( *it2 )->m_V[ii];
      }

    pnt.SetRed( ( *it2 )->m_Color[0] );
    pnt.SetGreen( ( *it2 )->m_Color[1] );
    pnt.SetBlue( ( *it2 )->m_Color[2] );
    pnt.SetAlpha( ( *it2 )->m_Color[3] );

    pnt.SetPosition(point);
    pnt.SetNormal(normal);

    surface->GetPoints().push_back(pnt);
    it2++;
    }

  return surface;
}

/** Convert an Surface SpatialObject into a metaSurface */
template< unsigned int NDimensions >
MetaSurface *
MetaSurfaceConverter< NDimensions >
::SurfaceSpatialObjectToMetaSurface(SpatialObjectType *spatialObject)
{
  MetaSurface *Surface = new MetaSurface(NDimensions);

  // fill in the Surface information

  typename SpatialObjectType::PointListType::const_iterator i;
  for ( i = dynamic_cast< SpatialObjectType * >( spatialObject )->GetPoints().begin();
        i != dynamic_cast< SpatialObjectType * >( spatialObject )->GetPoints().end();
        i++ )
    {
    SurfacePnt *pnt = new SurfacePnt(NDimensions);

    for ( unsigned int d = 0; d < NDimensions; d++ )
      {
      pnt->m_X[d] = ( *i ).GetPosition()[d];
      }

    for ( unsigned int d = 0; d < NDimensions; d++ )
      {
      pnt->m_V[d] = ( *i ).GetNormal()[d];
      }

    pnt->m_Color[0] = ( *i ).GetRed();
    pnt->m_Color[1] = ( *i ).GetGreen();
    pnt->m_Color[2] = ( *i ).GetBlue();
    pnt->m_Color[3] = ( *i ).GetAlpha();

    Surface->GetPoints().push_back(pnt);
    }

  if ( NDimensions == 2 )
    {
    Surface->PointDim("x y v1 v2 red green blue alpha");
    }
  else if ( NDimensions == 3 )
    {
    Surface->PointDim("x y z v1 v2 v3 red green blue alpha");
    }

  float color[4];
  for ( unsigned int ii = 0; ii < 4; ii++ )
    {
    color[ii] = spatialObject->GetProperty()->GetColor()[ii];
    }

  Surface->Color(color);
  Surface->ID( spatialObject->GetId() );
  if ( spatialObject->GetParent() )
    {
    Surface->ParentID( spatialObject->GetParent()->GetId() );
    }
  Surface->NPoints( Surface->GetPoints().size() );

  for ( unsigned int ii = 0; ii < NDimensions; ii++ )
    {
    Surface->ElementSpacing(ii, spatialObject->GetIndexToObjectTransform()
                            ->GetScaleComponent()[ii]);
    }

  return Surface;
}

/** Read a meta file give the type */
template< unsigned int NDimensions >
typename MetaSurfaceConverter< NDimensions >::SpatialObjectPointer
MetaSurfaceConverter< NDimensions >
::ReadMeta(const char *name)
{
  SpatialObjectPointer spatialObject;
  MetaSurface *        Surface = new MetaSurface();

  Surface->Read(name);
  spatialObject = MetaSurfaceToSurfaceSpatialObject(Surface);

  return spatialObject;
}

/** Write a meta Surface file */
template< unsigned int NDimensions >
bool
MetaSurfaceConverter< NDimensions >
::WriteMeta(SpatialObjectType *spatialObject, const char *name)
{
  MetaSurface *Surface = SurfaceSpatialObjectToMetaSurface(spatialObject);

  Surface->Write(name);
  return true;
}
} // end namespace itk

#endif
