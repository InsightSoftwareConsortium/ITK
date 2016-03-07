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
#ifndef itkMetaTubeConverter_hxx
#define itkMetaTubeConverter_hxx

#include "itkMetaTubeConverter.h"

namespace itk
{
/** Constructor */
template< unsigned int NDimensions >
MetaTubeConverter< NDimensions >
::MetaTubeConverter()
{}

template< unsigned int NDimensions >
typename MetaTubeConverter< NDimensions >::MetaObjectType *
MetaTubeConverter< NDimensions>
::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new TubeMetaObjectType);
}

/** Convert a metaTube into an Tube SpatialObject  */
template< unsigned int NDimensions >
typename MetaTubeConverter< NDimensions >::SpatialObjectPointer
MetaTubeConverter< NDimensions >
::MetaObjectToSpatialObject(const MetaObjectType *mo)
{
  const TubeMetaObjectType *tubeMO =
    dynamic_cast<const TubeMetaObjectType *>(mo);
  if(tubeMO == ITK_NULLPTR)
    {
    itkExceptionMacro(<< "Can't convert MetaObject to MetaTube" );
    }

  typename TubeSpatialObjectType::Pointer tubeSO = TubeSpatialObjectType::New();
  double spacing[NDimensions];

  unsigned int ndims = tubeMO->NDims();
  for ( unsigned int i = 0; i < ndims; i++ )
    {
    spacing[i] = tubeMO->ElementSpacing()[i];
    }

  tubeSO->GetIndexToObjectTransform()->SetScaleComponent(spacing);
  tubeSO->GetProperty()->SetName( tubeMO->Name() );
  tubeSO->SetParentPoint( tubeMO->ParentPoint() );
  tubeSO->SetId( tubeMO->ID() );
  tubeSO->SetParentId( tubeMO->ParentID() );
  tubeSO->GetProperty()->SetRed(tubeMO->Color()[0]);
  tubeSO->GetProperty()->SetGreen(tubeMO->Color()[1]);
  tubeSO->GetProperty()->SetBlue(tubeMO->Color()[2]);
  tubeSO->GetProperty()->SetAlpha(tubeMO->Color()[3]);

  typedef itk::TubeSpatialObjectPoint< NDimensions > TubePointType;

  typedef MetaTube::PointListType ListType;
  ListType::const_iterator it2 = tubeMO->GetPoints().begin();

  itk::CovariantVector< double, NDimensions > v;
  itk::Vector< double, NDimensions >          t;

  for ( unsigned int identifier = 0; identifier < tubeMO->GetPoints().size(); identifier++ )
    {
    TubePointType pnt;

    typedef typename TubeSpatialObjectType::PointType PointType;
    PointType point;

    for ( unsigned int i = 0; i < ndims; i++ )
      {
      point[i] = ( *it2 )->m_X[i];
      }

    pnt.SetPosition(point);
    pnt.SetRadius( ( *it2 )->m_R );

    for ( unsigned int i = 0; i < ndims; i++ )
      {
      v[i] = ( *it2 )->m_V1[i];
      }
    pnt.SetNormal1(v);

    for ( unsigned int i = 0; i < ndims; i++ )
      {
      v[i] = ( *it2 )->m_V2[i];
      }
    pnt.SetNormal2(v);

    for ( unsigned int i = 0; i < ndims; i++ )
      {
      t[i] = ( *it2 )->m_T[i];
      }
    pnt.SetTangent(t);

    pnt.SetRed( ( *it2 )->m_Color[0] );
    pnt.SetGreen( ( *it2 )->m_Color[1] );
    pnt.SetBlue( ( *it2 )->m_Color[2] );
    pnt.SetAlpha( ( *it2 )->m_Color[3] );

    pnt.SetID( ( *it2 )->m_ID );

    tubeSO->GetPoints().push_back(pnt);

    it2++;
    }

  return tubeSO.GetPointer();
}

/** Convert a Tube SpatialObject into a metaTube */
template< unsigned int NDimensions >
typename MetaTubeConverter< NDimensions >::MetaObjectType *
MetaTubeConverter< NDimensions >
::SpatialObjectToMetaObject(const SpatialObjectType *spatialObject)
{
  TubeSpatialObjectConstPointer tubeSO =
    dynamic_cast<const TubeSpatialObjectType *>(spatialObject);
  if(tubeSO.IsNull())
    {
    itkExceptionMacro(<< "Can't downcast SpatialObject to TubeSpatialObject");
    }

  MetaTube *tubeMO = new MetaTube(NDimensions);

  // fill in the tube information
  typename TubeSpatialObjectType::PointListType::const_iterator it;
  for ( it = tubeSO->GetPoints().begin();
        it != tubeSO->GetPoints().end();
        it++ )
    {
    TubePnt *pnt = new TubePnt(NDimensions);

    for ( unsigned int d = 0; d < NDimensions; d++ )
      {
      pnt->m_X[d] = ( *it ).GetPosition()[d];
      }

    pnt->m_ID = ( *it ).GetID();
    pnt->m_R = ( *it ).GetRadius();

    for ( unsigned int d = 0; d < NDimensions; d++ )
      {
      pnt->m_V1[d] = ( *it ).GetNormal1()[d];
      }

    for ( unsigned int d = 0; d < NDimensions; d++ )
      {
      pnt->m_V2[d] = ( *it ).GetNormal2()[d];
      }

    for ( unsigned int d = 0; d < NDimensions; d++ )
      {
      pnt->m_T[d] = ( *it ).GetTangent()[d];
      }

    pnt->m_Color[0] = ( *it ).GetRed();
    pnt->m_Color[1] = ( *it ).GetGreen();
    pnt->m_Color[2] = ( *it ).GetBlue();
    pnt->m_Color[3] = ( *it ).GetAlpha();

    tubeMO->GetPoints().push_back(pnt);
    }

  if ( NDimensions == 2 )
    {
    tubeMO->PointDim("x y r v1x v1y tx ty red green blue alpha id");
    }
  else
    {
    tubeMO->PointDim("x y z r v1x v1y v1z v2x v2y v2z tx ty tz red green blue alpha id");
    }

  float color[4];
  for ( unsigned int i = 0; i < 4; i++ )
    {
    color[i] = tubeSO->GetProperty()->GetColor()[i];
    }

  tubeMO->Color(color);
  tubeMO->ID( tubeSO->GetId() );

  if ( tubeSO->GetParent() )
    {
    tubeMO->ParentID( tubeSO->GetParent()->GetId() );
    }
  tubeMO->ParentPoint( tubeSO->GetParentPoint() );
  tubeMO->NPoints(static_cast<int>( tubeMO->GetPoints().size() ) );

  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    tubeMO->ElementSpacing(i, tubeSO->GetIndexToObjectTransform()
                         ->GetScaleComponent()[i]);
    }
  return tubeMO;
}

} // end namespace itk

#endif
