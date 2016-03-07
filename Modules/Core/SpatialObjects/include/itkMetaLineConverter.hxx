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
#ifndef itkMetaLineConverter_hxx
#define itkMetaLineConverter_hxx

#include "itkMetaLineConverter.h"

namespace itk
{
/** Constructor */
template< unsigned int NDimensions >
MetaLineConverter< NDimensions >
::MetaLineConverter()
{}

template< unsigned int NDimensions >
typename MetaLineConverter< NDimensions >::MetaObjectType *
MetaLineConverter< NDimensions>
::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new LineMetaObjectType);
}

/** Convert a metaLine into an Line SpatialObject  */
template< unsigned int NDimensions >
typename MetaLineConverter< NDimensions >::SpatialObjectPointer
MetaLineConverter< NDimensions >
::MetaObjectToSpatialObject(const MetaObjectType *mo)
{
  const LineMetaObjectType *lineMO =
    dynamic_cast<const LineMetaObjectType *>(mo);
  if(lineMO == ITK_NULLPTR)
    {
    itkExceptionMacro(<< "Can't convert MetaObject to MetaLine" );
    }

  LineSpatialObjectPointer lineSO = LineSpatialObjectType::New();

  double spacing[NDimensions];

  unsigned int ndims = lineMO->NDims();
  for ( unsigned int ii = 0; ii < ndims; ii++ )
    {
    spacing[ii] = lineMO->ElementSpacing()[ii];
    }
  lineSO->GetIndexToObjectTransform()->SetScaleComponent(spacing);
  lineSO->GetProperty()->SetName( lineMO->Name() );
  lineSO->SetId( lineMO->ID() );
  lineSO->SetParentId( lineMO->ParentID() );
  lineSO->GetProperty()->SetRed(lineMO->Color()[0]);
  lineSO->GetProperty()->SetGreen(lineMO->Color()[1]);
  lineSO->GetProperty()->SetBlue(lineMO->Color()[2]);
  lineSO->GetProperty()->SetAlpha(lineMO->Color()[3]);

  typedef itk::LineSpatialObjectPoint< NDimensions > LinePointType;

  typedef MetaLine::PointListType ListType;
  ListType::const_iterator it2 = lineMO->GetPoints().begin();

  for ( unsigned int identifier = 0; identifier < lineMO->GetPoints().size(); identifier++ )
    {
    LinePointType pnt;

    typedef typename LinePointType::PointType PointType;
    PointType point;
    typedef typename LinePointType::VectorType NormalType;

    for ( unsigned int ii = 0; ii < ndims; ii++ )
      {
      point[ii] = ( *it2 )->m_X[ii];
      }

    pnt.SetPosition(point);

    for ( unsigned int ii = 0; ii < ndims - 1; ii++ )
      {
      NormalType normal;
      for ( unsigned int jj = 0; jj < ndims; jj++ )
        {
        normal[jj] = ( *it2 )->m_V[ii][jj];
        }
      pnt.SetNormal(normal, ii);
      }

    pnt.SetRed( ( *it2 )->m_Color[0] );
    pnt.SetGreen( ( *it2 )->m_Color[1] );
    pnt.SetBlue( ( *it2 )->m_Color[2] );
    pnt.SetAlpha( ( *it2 )->m_Color[3] );

    lineSO->GetPoints().push_back(pnt);
    it2++;
    }
  return lineSO.GetPointer();
}

/** Convert a Line SpatialObject into a metaLine */
template< unsigned int NDimensions >
typename MetaLineConverter< NDimensions >::MetaObjectType *
MetaLineConverter< NDimensions >
::SpatialObjectToMetaObject(const SpatialObjectType *spatialObject)
{
  LineSpatialObjectConstPointer lineSO =
    dynamic_cast<const LineSpatialObjectType *>(spatialObject);
  if(lineSO.IsNull())
    {
    itkExceptionMacro(<< "Can't downcast SpatialObject to LineSpatialObject");
    }

  MetaLine *lineMO = new MetaLine(NDimensions);

  // due to a Visual Studio stupidity, can't seem to define
  // a const method to return the points list.
  const typename LineSpatialObjectType::PointListType &linePoints =
    lineSO->GetPoints();

  // fill in the Line information
  typename LineSpatialObjectType::PointListType::const_iterator it;
  for ( it = linePoints.begin();
        it != linePoints.end();
        ++it )
    {
    LinePnt *pnt = new LinePnt(NDimensions);

    for ( unsigned int d = 0; d < NDimensions; d++ )
      {
      pnt->m_X[d] = ( *it ).GetPosition()[d];
      }

    for ( unsigned int n = 0; n < NDimensions - 1; n++ )
      {
      for ( unsigned int d = 0; d < NDimensions; d++ )
        {
        pnt->m_V[n][d] = ( ( *it ).GetNormal(n) )[d];
        }
      }

    pnt->m_Color[0] = ( *it ).GetRed();
    pnt->m_Color[1] = ( *it ).GetGreen();
    pnt->m_Color[2] = ( *it ).GetBlue();
    pnt->m_Color[3] = ( *it ).GetAlpha();

    lineMO->GetPoints().push_back(pnt);
    }

  if ( NDimensions == 2 )
    {
    lineMO->PointDim("x y v1x v1y v2x v2y red green blue alpha");
    }
  else if ( NDimensions == 3 )
    {
    lineMO->PointDim("x y z v1x v1y v1z v2x v2y v2z red green blue alpha");
    }

  float color[4];
  for ( unsigned int ii = 0; ii < 4; ii++ )
    {
    color[ii] = lineSO->GetProperty()->GetColor()[ii];
    }

  lineMO->Color(color);
  lineMO->ID( lineSO->GetId() );
  if ( lineSO->GetParent() )
    {
    lineMO->ParentID( lineSO->GetParent()->GetId() );
    }
  lineMO->NPoints(static_cast<int>( linePoints.size() ) );
  lineMO->BinaryData(true);
  return lineMO;
}

} // end namespace itk

#endif
