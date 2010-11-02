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
#ifndef __itkMetaLineConverter_txx
#define __itkMetaLineConverter_txx

#include "itkMetaLineConverter.h"

namespace itk
{
/** Constructor */
template< unsigned int NDimensions >
MetaLineConverter< NDimensions >
::MetaLineConverter()
{}

/** Convert a metaLine into an Line SpatialObject  */
template< unsigned int NDimensions >
typename MetaLineConverter< NDimensions >::SpatialObjectPointer
MetaLineConverter< NDimensions >
::MetaLineToLineSpatialObject(MetaLine *Line)
{
  typedef itk::LineSpatialObject< NDimensions > LineSpatialObjectType;
  typename LineSpatialObjectType::Pointer line = LineSpatialObjectType::New();

  double spacing[NDimensions];

  unsigned int ndims = Line->NDims();
  for ( unsigned int ii = 0; ii < ndims; ii++ )
    {
    spacing[ii] = Line->ElementSpacing()[ii];
    }
  line->GetIndexToObjectTransform()->SetScaleComponent(spacing);
  line->GetProperty()->SetName( Line->Name() );
  line->SetId( Line->ID() );
  line->SetParentId( Line->ParentID() );
  line->GetProperty()->SetRed(Line->Color()[0]);
  line->GetProperty()->SetGreen(Line->Color()[1]);
  line->GetProperty()->SetBlue(Line->Color()[2]);
  line->GetProperty()->SetAlpha(Line->Color()[3]);

  typedef itk::LineSpatialObjectPoint< NDimensions > LinePointType;
  typedef LinePointType *                            LinePointPointer;

  typedef MetaLine::PointListType ListType;
  ListType::iterator it2 = Line->GetPoints().begin();

  vnl_vector< double > v(ndims);

  for ( unsigned int identifier = 0; identifier < Line->GetPoints().size(); identifier++ )
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

    line->GetPoints().push_back(pnt);
    it2++;
    }
  return line;
}

/** Convert an Line SpatialObject into a metaLine */
template< unsigned int NDimensions >
MetaLine *
MetaLineConverter< NDimensions >
::LineSpatialObjectToMetaLine(SpatialObjectType *spatialObject)
{
  MetaLine *Line = new MetaLine(NDimensions);

  // fill in the Line information

  typename SpatialObjectType::PointListType::const_iterator i;
  for ( i = dynamic_cast< SpatialObjectType * >( spatialObject )->GetPoints().begin();
        i != dynamic_cast< SpatialObjectType * >( spatialObject )->GetPoints().end();
        i++ )
    {
    LinePnt *pnt = new LinePnt(NDimensions);

    for ( unsigned int d = 0; d < NDimensions; d++ )
      {
      pnt->m_X[d] = ( *i ).GetPosition()[d];
      }

    for ( unsigned int n = 0; n < NDimensions - 1; n++ )
      {
      for ( unsigned int d = 0; d < NDimensions; d++ )
        {
        pnt->m_V[n][d] = ( ( *i ).GetNormal(n) )[d];
        }
      }

    pnt->m_Color[0] = ( *i ).GetRed();
    pnt->m_Color[1] = ( *i ).GetGreen();
    pnt->m_Color[2] = ( *i ).GetBlue();
    pnt->m_Color[3] = ( *i ).GetAlpha();

    Line->GetPoints().push_back(pnt);
    }

  if ( NDimensions == 2 )
    {
    Line->PointDim("x y v1x v1y v2x v2y red green blue alpha");
    }
  else if ( NDimensions == 3 )
    {
    Line->PointDim("x y z v1x v1y v1z v2x v2y v2z red green blue alpha");
    }

  float color[4];
  for ( unsigned int ii = 0; ii < 4; ii++ )
    {
    color[ii] = spatialObject->GetProperty()->GetColor()[ii];
    }

  Line->Color(color);
  Line->ID( spatialObject->GetId() );
  if ( spatialObject->GetParent() )
    {
    Line->ParentID( spatialObject->GetParent()->GetId() );
    }
  Line->NPoints( Line->GetPoints().size() );

  return Line;
}

/** Read a meta file give the type */
template< unsigned int NDimensions >
typename MetaLineConverter< NDimensions >::SpatialObjectPointer
MetaLineConverter< NDimensions >
::ReadMeta(const char *name)
{
  SpatialObjectPointer spatialObject;
  MetaLine *           Line = new MetaLine();

  Line->Read(name);
  spatialObject = MetaLineToLineSpatialObject(Line);

  return spatialObject;
}

/** Write a meta Line file */
template< unsigned int NDimensions >
bool
MetaLineConverter< NDimensions >
::WriteMeta(SpatialObjectType *spatialObject, const char *name)
{
  MetaLine *Line = LineSpatialObjectToMetaLine(spatialObject);

  Line->Write(name);
  return true;
}
} // end namespace itk

#endif
