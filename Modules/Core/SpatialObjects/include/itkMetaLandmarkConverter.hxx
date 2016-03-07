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
#ifndef itkMetaLandmarkConverter_hxx
#define itkMetaLandmarkConverter_hxx

#include "itkMetaLandmarkConverter.h"

namespace itk
{
/** Constructor */
template< unsigned int NDimensions >
MetaLandmarkConverter< NDimensions >
::MetaLandmarkConverter()
{}

template< unsigned int NDimensions >
typename MetaLandmarkConverter< NDimensions >::MetaObjectType *
MetaLandmarkConverter< NDimensions>
::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new LandmarkMetaObjectType);
}

/** Convert a metaLandmark into an Landmark SpatialObject  */
template< unsigned int NDimensions >
typename MetaLandmarkConverter< NDimensions >::SpatialObjectPointer
MetaLandmarkConverter< NDimensions >
::MetaObjectToSpatialObject(const MetaObjectType *mo)
{
  const LandmarkMetaObjectType *landmarkMO =
    dynamic_cast<const LandmarkMetaObjectType *>(mo);
  if(landmarkMO == ITK_NULLPTR)
    {
    itkExceptionMacro(<< "Can't convert MetaObject to MetaLandmark");
    }

  LandmarkSpatialObjectPointer landmarkSO =
    LandmarkSpatialObjectType::New();

  unsigned int ndims = landmarkMO->NDims();
  double       spacing[NDimensions];
  for ( unsigned int ii = 0; ii < ndims; ii++ )
    {
    spacing[ii] = landmarkMO->ElementSpacing()[ii];
    }
  landmarkSO->GetIndexToObjectTransform()->SetScaleComponent(spacing);
  landmarkSO->GetProperty()->SetName( landmarkMO->Name() );
  landmarkSO->SetId( landmarkMO->ID() );
  landmarkSO->SetParentId( landmarkMO->ParentID() );
  landmarkSO->GetProperty()->SetRed(landmarkMO->Color()[0]);
  landmarkSO->GetProperty()->SetGreen(landmarkMO->Color()[1]);
  landmarkSO->GetProperty()->SetBlue(landmarkMO->Color()[2]);
  landmarkSO->GetProperty()->SetAlpha(landmarkMO->Color()[3]);

  typedef itk::SpatialObjectPoint< NDimensions > LandmarkPointType;

  typename LandmarkMetaObjectType::PointListType::const_iterator it2
    = landmarkMO->GetPoints().begin();

  for ( unsigned int identifier = 0; identifier < landmarkMO->GetPoints().size(); identifier++ )
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

    landmarkSO->GetPoints().push_back(pnt);
    it2++;
    }

  return landmarkSO.GetPointer();
}

/** Convert a Landmark SpatialObject into a metaLandmark */
template< unsigned int NDimensions >
typename MetaLandmarkConverter< NDimensions >::MetaObjectType *
MetaLandmarkConverter< NDimensions >
::SpatialObjectToMetaObject(const SpatialObjectType *so)
{
  const LandmarkSpatialObjectConstPointer landmarkSO =
    dynamic_cast<const LandmarkSpatialObjectType *>(so);

  if(landmarkSO.IsNull())
    {
    itkExceptionMacro(<< "Can't downcast SpatialObject to LandmarkSpatialObject");
    }

  MetaLandmark *landmarkMO = new MetaLandmark(NDimensions);

  // fill in the Landmark information
  typename LandmarkSpatialObjectType::PointListType::const_iterator it;
  for ( it = landmarkSO->GetPoints().begin(); it != landmarkSO->GetPoints().end(); ++it )
    {
    LandmarkPnt *pnt = new LandmarkPnt(NDimensions);

    for ( unsigned int d = 0; d < NDimensions; d++ )
      {
      pnt->m_X[d] = ( *it ).GetPosition()[d];
      }

    pnt->m_Color[0] = ( *it ).GetRed();
    pnt->m_Color[1] = ( *it ).GetGreen();
    pnt->m_Color[2] = ( *it ).GetBlue();
    pnt->m_Color[3] = ( *it ).GetAlpha();
    landmarkMO->GetPoints().push_back(pnt);
    }

  if ( NDimensions == 2 )
    {
    landmarkMO->PointDim("x y red green blue alpha");
    }
  else
    {
    landmarkMO->PointDim("x y z red green blue alpha");
    }

  float color[4];
  for ( unsigned int ii = 0; ii < 4; ii++ )
    {
    color[ii] = landmarkSO->GetProperty()->GetColor()[ii];
    }

  landmarkMO->Color(color);
  landmarkMO->ID( landmarkSO->GetId() );
  if ( landmarkSO->GetParent() )
    {
    landmarkMO->ParentID( landmarkSO->GetParent()->GetId() );
    }
  landmarkMO->NPoints(static_cast<int>( landmarkMO->GetPoints().size() ) );
  landmarkMO->BinaryData(true);

  return landmarkMO;
}

} // end namespace itk

#endif
