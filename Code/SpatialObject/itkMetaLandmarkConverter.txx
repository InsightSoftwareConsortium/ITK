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
#ifndef __itkMetaLandmarkConverter_txx
#define __itkMetaLandmarkConverter_txx

#include "itkMetaLandmarkConverter.h"

namespace itk
{
/** Constructor */
template< unsigned int NDimensions >
MetaLandmarkConverter< NDimensions >
::MetaLandmarkConverter()
{}

/** Convert a metaLandmark into an Landmark SpatialObject  */
template< unsigned int NDimensions >
typename MetaLandmarkConverter< NDimensions >::SpatialObjectPointer
MetaLandmarkConverter< NDimensions >
::MetaLandmarkToLandmarkSpatialObject(MetaLandmark *Landmark)
{
  typedef itk::LandmarkSpatialObject< NDimensions > LandmarkSpatialObjectType;
  typename LandmarkSpatialObjectType::Pointer landmark =
    LandmarkSpatialObjectType::New();

  //typedef LandmarkSpatialObjectType::VectorType VectorType;
  typedef vnl_vector< double > VectorType;

  unsigned int ndims = Landmark->NDims();
  double       spacing[NDimensions];
  for ( unsigned int ii = 0; ii < ndims; ii++ )
    {
    spacing[ii] = Landmark->ElementSpacing()[ii];
    }
  landmark->GetIndexToObjectTransform()->SetScaleComponent(spacing);
  landmark->GetProperty()->SetName( Landmark->Name() );
  landmark->SetId( Landmark->ID() );
  landmark->SetParentId( Landmark->ParentID() );
  landmark->GetProperty()->SetRed(Landmark->Color()[0]);
  landmark->GetProperty()->SetGreen(Landmark->Color()[1]);
  landmark->GetProperty()->SetBlue(Landmark->Color()[2]);
  landmark->GetProperty()->SetAlpha(Landmark->Color()[3]);

  typedef itk::SpatialObjectPoint< NDimensions > LandmarkPointType;

  typedef MetaLandmark::PointListType ListType;
  ListType::iterator it2 = Landmark->GetPoints().begin();

  vnl_vector< double > v(ndims);

  for ( unsigned int identifier = 0; identifier < Landmark->GetPoints().size(); identifier++ )
    {
    LandmarkPointType pnt;

    typedef typename LandmarkSpatialObjectType::PointType PointType;
    PointType point;

    for ( unsigned int ii = 0; ii < ndims; ii++ )
      {
      point[ii] = ( *it2 )->m_X[ii];
      }

    pnt.SetPosition(point);

    pnt.SetRed( ( *it2 )->m_Color[0] );
    pnt.SetGreen( ( *it2 )->m_Color[1] );
    pnt.SetBlue( ( *it2 )->m_Color[2] );
    pnt.SetAlpha( ( *it2 )->m_Color[3] );

    landmark->GetPoints().push_back(pnt);
    it2++;
    }

  return landmark;
}

/** Convert an Landmark SpatialObject into a metaLandmark */
template< unsigned int NDimensions >
MetaLandmark *
MetaLandmarkConverter< NDimensions >
::LandmarkSpatialObjectToMetaLandmark(SpatialObjectType *spatialObject)
{
  MetaLandmark *Landmark = new MetaLandmark(NDimensions);

  // fill in the Landmark information

  typename SpatialObjectType::PointListType::const_iterator i;
  for ( i = dynamic_cast< SpatialObjectType * >( spatialObject )->GetPoints().begin();
        i != dynamic_cast< SpatialObjectType * >( spatialObject )->GetPoints().end();
        i++ )
    {
    LandmarkPnt *pnt = new LandmarkPnt(NDimensions);

    for ( unsigned int d = 0; d < NDimensions; d++ )
      {
      pnt->m_X[d] = ( *i ).GetPosition()[d];
      }

    pnt->m_Color[0] = ( *i ).GetRed();
    pnt->m_Color[1] = ( *i ).GetGreen();
    pnt->m_Color[2] = ( *i ).GetBlue();
    pnt->m_Color[3] = ( *i ).GetAlpha();
    Landmark->GetPoints().push_back(pnt);
    }

  if ( NDimensions == 2 )
    {
    Landmark->PointDim("x y red green blue alpha");
    }
  else
    {
    Landmark->PointDim("x y z red green blue alpha");
    }

  float color[4];
  for ( unsigned int ii = 0; ii < 4; ii++ )
    {
    color[ii] = spatialObject->GetProperty()->GetColor()[ii];
    }

  Landmark->Color(color);
  Landmark->ID( spatialObject->GetId() );
  if ( spatialObject->GetParent() )
    {
    Landmark->ParentID( spatialObject->GetParent()->GetId() );
    }
  Landmark->NPoints( Landmark->GetPoints().size() );

  return Landmark;
}

/** Read a meta file give the type */
template< unsigned int NDimensions >
typename MetaLandmarkConverter< NDimensions >::SpatialObjectPointer
MetaLandmarkConverter< NDimensions >
::ReadMeta(const char *name)
{
  SpatialObjectPointer spatialObject;
  MetaLandmark *       Landmark = new MetaLandmark();

  Landmark->Read(name);
  spatialObject = MetaLandmarkToLandmarkSpatialObject(Landmark);

  return spatialObject;
}

/** Write a meta Landmark file */
template< unsigned int NDimensions >
bool
MetaLandmarkConverter< NDimensions >
::WriteMeta(SpatialObjectType *spatialObject, const char *name)
{
  MetaLandmark *Landmark = LandmarkSpatialObjectToMetaLandmark(spatialObject);

  Landmark->BinaryData(true);
  Landmark->Write(name);
  return true;
}
} // end namespace itk

#endif
