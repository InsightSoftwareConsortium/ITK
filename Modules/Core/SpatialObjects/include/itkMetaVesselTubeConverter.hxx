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
#ifndef itkMetaVesselTubeConverter_hxx
#define itkMetaVesselTubeConverter_hxx

#include "itkMetaVesselTubeConverter.h"

namespace itk
{
/** Constructor */
template< unsigned int NDimensions >
MetaVesselTubeConverter< NDimensions >
::MetaVesselTubeConverter()
{}

template< unsigned int NDimensions >
typename MetaVesselTubeConverter< NDimensions >::MetaObjectType *
MetaVesselTubeConverter< NDimensions>
::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new VesselTubeMetaObjectType);
}

/** Convert a MetaVesselTube into an Tube SpatialObject  */
template< unsigned int NDimensions >
typename MetaVesselTubeConverter< NDimensions >::SpatialObjectPointer
MetaVesselTubeConverter< NDimensions >
::MetaObjectToSpatialObject(const MetaObjectType *mo)
{
  const VesselTubeMetaObjectType *vesselTubeMO =
    dynamic_cast<const VesselTubeMetaObjectType *>(mo);
  if(vesselTubeMO == ITK_NULLPTR)
    {
    itkExceptionMacro(<< "Can't convert MetaObject to MetaVesselTube" );
    }

  VesselTubeSpatialObjectPointer
    vesselTubeSO = VesselTubeSpatialObjectType::New();
  double spacing[NDimensions];

  unsigned int ndims = vesselTubeMO->NDims();
  for ( unsigned int ii = 0; ii < ndims; ii++ )
    {
    spacing[ii] = vesselTubeMO->ElementSpacing()[ii];
    }

  vesselTubeSO->GetIndexToObjectTransform()->SetScaleComponent(spacing);
  vesselTubeSO->GetProperty()->SetName( vesselTubeMO->Name() );
  vesselTubeSO->SetParentPoint( vesselTubeMO->ParentPoint() );
  vesselTubeSO->SetId( vesselTubeMO->ID() );
  vesselTubeSO->SetRoot( vesselTubeMO->Root() );
  vesselTubeSO->SetArtery( vesselTubeMO->Artery() );
  vesselTubeSO->SetParentId( vesselTubeMO->ParentID() );
  vesselTubeSO->GetProperty()->SetRed(vesselTubeMO->Color()[0]);
  vesselTubeSO->GetProperty()->SetGreen(vesselTubeMO->Color()[1]);
  vesselTubeSO->GetProperty()->SetBlue(vesselTubeMO->Color()[2]);
  vesselTubeSO->GetProperty()->SetAlpha(vesselTubeMO->Color()[3]);

  typedef itk::VesselTubeSpatialObjectPoint< NDimensions > VesselTubePointType;

  typedef VesselTubeMetaObjectType::PointListType ListType;
  ListType::const_iterator it2 = vesselTubeMO->GetPoints().begin();

  itk::CovariantVector< double, NDimensions > v;
  itk::Vector< double, NDimensions >          t;

  for ( unsigned int identifier = 0; identifier < vesselTubeMO->GetPoints().size(); identifier++ )
    {
    VesselTubePointType pnt;

    typedef typename VesselTubeSpatialObjectType::PointType SOPointType;
    SOPointType point;

    for ( unsigned int ii = 0; ii < ndims; ii++ )
      {
      point[ii] = ( *it2 )->m_X[ii];
      }

    pnt.SetPosition(point);
    pnt.SetRadius( ( *it2 )->m_R );
    pnt.SetMedialness( ( *it2 )->m_Medialness );
    pnt.SetRidgeness( ( *it2 )->m_Ridgeness );
    pnt.SetBranchness( ( *it2 )->m_Branchness );
    pnt.SetMark( ( *it2 )->m_Mark );

    for ( unsigned int ii = 0; ii < ndims; ii++ )
      {
      v[ii] = ( *it2 )->m_V1[ii];
      }
    pnt.SetNormal1(v);

    for ( unsigned int ii = 0; ii < ndims; ii++ )
      {
      v[ii] = ( *it2 )->m_V2[ii];
      }
    pnt.SetNormal2(v);

    for ( unsigned int ii = 0; ii < ndims; ii++ )
      {
      t[ii] = ( *it2 )->m_T[ii];
      }
    pnt.SetTangent(t);

    pnt.SetAlpha1( ( *it2 )->m_Alpha1 );
    pnt.SetAlpha2( ( *it2 )->m_Alpha2 );
    pnt.SetAlpha3( ( *it2 )->m_Alpha3 );

    pnt.SetRed( ( *it2 )->m_Color[0] );
    pnt.SetGreen( ( *it2 )->m_Color[1] );
    pnt.SetBlue( ( *it2 )->m_Color[2] );
    pnt.SetAlpha( ( *it2 )->m_Color[3] );

    pnt.SetID( ( *it2 )->m_ID );

    vesselTubeSO->GetPoints().push_back(pnt);

    it2++;
    }

  return vesselTubeSO.GetPointer();
}

/** Convert a Tube SpatialObject into a MetaVesselTube */
template< unsigned int NDimensions >
typename MetaVesselTubeConverter< NDimensions >::MetaObjectType *
MetaVesselTubeConverter< NDimensions >
::SpatialObjectToMetaObject(const SpatialObjectType *so)
{
  const VesselTubeSpatialObjectConstPointer vesselTubeSO =
    dynamic_cast<const VesselTubeSpatialObjectType *>(so);

  if(vesselTubeSO.IsNull())
    {
    itkExceptionMacro(<< "Can't downcast SpatialObject to VesselTubeSpatialObject");
    }
  MetaVesselTube *vesselTubeMO = new MetaVesselTube(NDimensions);

  // fill in the tube information

  typename VesselTubeSpatialObjectType::PointListType::const_iterator i;
  for ( i = vesselTubeSO->GetPoints().begin();
        i != vesselTubeSO->GetPoints().end();
        i++ )
    {
    VesselTubePnt *pnt = new VesselTubePnt(NDimensions);

    for ( unsigned int d = 0; d < NDimensions; d++ )
      {
      pnt->m_X[d] = ( *i ).GetPosition()[d];
      }

    pnt->m_ID = ( *i ).GetID();
    pnt->m_R = ( *i ).GetRadius();
    pnt->m_Alpha1 = ( *i ).GetAlpha1();
    pnt->m_Alpha2 = ( *i ).GetAlpha2();
    pnt->m_Alpha3 = ( *i ).GetAlpha3();
    pnt->m_Medialness = ( *i ).GetMedialness();
    pnt->m_Ridgeness = ( *i ).GetRidgeness();
    pnt->m_Branchness = ( *i ).GetBranchness();
    pnt->m_Mark = ( *i ).GetMark();

    for ( unsigned int d = 0; d < NDimensions; d++ )
      {
      pnt->m_V1[d] = ( *i ).GetNormal1()[d];
      }

    for ( unsigned int d = 0; d < NDimensions; d++ )
      {
      pnt->m_V2[d] = ( *i ).GetNormal2()[d];
      }

    for ( unsigned int d = 0; d < NDimensions; d++ )
      {
      pnt->m_T[d] = ( *i ).GetTangent()[d];
      }

    pnt->m_Color[0] = ( *i ).GetRed();
    pnt->m_Color[1] = ( *i ).GetGreen();
    pnt->m_Color[2] = ( *i ).GetBlue();
    pnt->m_Color[3] = ( *i ).GetAlpha();

    vesselTubeMO->GetPoints().push_back(pnt);
    }

  if ( NDimensions == 2 )
    {
    vesselTubeMO->PointDim("x y r rn mn bn mk v1x v1y tx ty a1 a2 red green blue alpha id");
    }
  else
    {
    vesselTubeMO->PointDim("x y z r rn mn bn mk v1x v1y v1z v2x v2y v2z tx ty tz a1 a2 a3 red green blue alpha id");
    }

  float color[4];
  for ( unsigned int ii = 0; ii < 4; ii++ )
    {
    color[ii] = vesselTubeSO->GetProperty()->GetColor()[ii];
    }

  vesselTubeMO->Color(color);
  vesselTubeMO->ID( vesselTubeSO->GetId() );
  vesselTubeMO->Root( vesselTubeSO->GetRoot() );
  vesselTubeMO->Artery( vesselTubeSO->GetArtery() );

  if ( vesselTubeSO->GetParent() )
    {
    vesselTubeMO->ParentID( vesselTubeSO->GetParent()->GetId() );
    }
  vesselTubeMO->ParentPoint( vesselTubeSO->GetParentPoint() );
  vesselTubeMO->NPoints(static_cast<int>( vesselTubeMO->GetPoints().size() ) );

  for ( unsigned int ii = 0; ii < NDimensions; ii++ )
    {
    vesselTubeMO->ElementSpacing(ii, vesselTubeSO->GetIndexToObjectTransform()
                         ->GetScaleComponent()[ii]);
    }
  return vesselTubeMO;
}

} // end namespace itk

#endif
