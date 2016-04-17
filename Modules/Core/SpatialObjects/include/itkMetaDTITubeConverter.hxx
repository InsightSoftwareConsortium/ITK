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
#ifndef itkMetaDTITubeConverter_hxx
#define itkMetaDTITubeConverter_hxx

#include "itkMetaDTITubeConverter.h"
#include "itkMath.h"

namespace itk
{
/** Constructor */
template< unsigned int NDimensions >
MetaDTITubeConverter< NDimensions >
::MetaDTITubeConverter()
{}

template< unsigned int NDimensions >
typename MetaDTITubeConverter< NDimensions >::MetaObjectType *
MetaDTITubeConverter< NDimensions>
::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new DTITubeMetaObjectType);
}

/** Convert a MetaDTITube into an Tube SpatialObject  */
template< unsigned int NDimensions >
typename MetaDTITubeConverter< NDimensions >::SpatialObjectPointer
MetaDTITubeConverter< NDimensions >
::MetaObjectToSpatialObject(const MetaObjectType *mo)
{
  const MetaDTITube *tube = dynamic_cast<const MetaDTITube *>(mo);
  if(tube == ITK_NULLPTR)
    {
    itkExceptionMacro(<< "Can't downcast MetaObject to MetaDTITube");
    }
  DTITubeSpatialObjectPointer tubeSO =
    DTITubeSpatialObjectType::New();

  double spacing[NDimensions];

  unsigned int ndims = tube->NDims();
  for ( unsigned int ii = 0; ii < ndims; ii++ )
    {
    spacing[ii] = tube->ElementSpacing()[ii];
    }

  tubeSO->GetIndexToObjectTransform()->SetScaleComponent(spacing);
  tubeSO->GetProperty()->SetName( tube->Name() );
  tubeSO->SetParentPoint( tube->ParentPoint() );
  tubeSO->SetId( tube->ID() );
  tubeSO->SetParentId( tube->ParentID() );
  tubeSO->GetProperty()->SetRed(tube->Color()[0]);
  tubeSO->GetProperty()->SetGreen(tube->Color()[1]);
  tubeSO->GetProperty()->SetBlue(tube->Color()[2]);
  tubeSO->GetProperty()->SetAlpha(tube->Color()[3]);

  typedef itk::DTITubeSpatialObjectPoint< NDimensions > TubePointType;

  MetaDTITube::PointListType::const_iterator it2 = tube->GetPoints().begin();

  itk::CovariantVector< double, NDimensions > v;
  v.Fill(0.0);
  itk::Vector< double, NDimensions > t;
  t.Fill(0.0);

  for ( unsigned int identifier = 0; identifier < tube->GetPoints().size(); identifier++ )
    {
    TubePointType pnt;

    typedef typename DTITubeSpatialObjectType::PointType PointType;
    PointType point;

    for ( unsigned int ii = 0; ii < ndims; ii++ )
      {
      point[ii] = ( *it2 )->m_X[ii];
      }

    // Get the fields from the metaIO
    const DTITubePnt::FieldListType &         metaFields = ( *it2 )->GetExtraFields();
    DTITubePnt::FieldListType::const_iterator extraIt = metaFields.begin();
    while ( extraIt != metaFields.end() )
      {
      // Do not add the optional fields
      if ( ( ( *extraIt ).first != "r" )
           && ( ( *extraIt ).first != "v1x" )
           && ( ( *extraIt ).first != "v1y" )
           && ( ( *extraIt ).first != "v1z" )
           && ( ( *extraIt ).first != "v2x" )
           && ( ( *extraIt ).first != "v2y" )
           && ( ( *extraIt ).first != "v2z" )
           && ( ( *extraIt ).first != "tx" )
           && ( ( *extraIt ).first != "ty" )
           && ( ( *extraIt ).first != "tz" )
           && ( ( *extraIt ).first != "red" )
           && ( ( *extraIt ).first != "green" )
           && ( ( *extraIt ).first != "blue" )
           && ( ( *extraIt ).first != "alpha" )
           && ( ( *extraIt ).first != "id" )
            )
        {
        pnt.AddField( ( *extraIt ).first.c_str(), ( *extraIt ).second );
        }
      extraIt++;
      }

    pnt.SetPosition(point);

    float *tensor = new float[6];

    for ( unsigned int ii = 0; ii < 6; ii++ )
      {
      tensor[ii] = ( *it2 )->m_TensorMatrix[ii];
      }
    pnt.SetTensorMatrix(tensor);

    delete[] tensor;

    // This attribute is optional
    if ( Math::NotExactlyEquals(( *it2 )->GetField("r"), -1) )
      {
      pnt.SetRadius( ( *it2 )->GetField("r") );
      }

    char vnd[] = "v1x";
    if ( Math::NotExactlyEquals(( *it2 )->GetField( vnd ), -1) )
      {
      v[0]  = ( *it2 )->GetField( vnd );
      for( unsigned int ii = 1; ii < ndims; ++ii )
        {
        ++(vnd[2]); // x -> y -> z
        v[ii]  = ( *it2 )->GetField( vnd );
        }
      pnt.SetNormal1(v);
      }

    vnd[1] = '2';
    vnd[2] = 'x';
    if ( Math::NotExactlyEquals(( *it2 )->GetField( vnd ), -1) )
      {
      v[0]  = ( *it2 )->GetField( vnd );
      for( unsigned int ii = 1; ii < ndims; ++ii )
        {
        ++(vnd[2]); // x -> y -> z
        v[ii]  = ( *it2 )->GetField( vnd );
        }
      pnt.SetNormal1(v);
      }

    char td[] = "tx";
    if ( Math::NotExactlyEquals(( *it2 )->GetField( td ), -1) )
      {
      t[0] = ( *it2 )->GetField( td );
      for( unsigned int ii = 1; ii < ndims; ++ii )
        {
        ++(td[1]); // x -> y -> z
        t[ii] = ( *it2 )->GetField( td );
        }
      pnt.SetTangent(t);
      }

    if ( Math::NotExactlyEquals(( *it2 )->GetField("red"), -1) )
      {
      pnt.SetRed( ( *it2 )->GetField("red") );
      }

    if ( Math::NotExactlyEquals(( *it2 )->GetField("green"), -1) )
      {
      pnt.SetGreen( ( *it2 )->GetField("green") );
      }

    if ( Math::NotExactlyEquals(( *it2 )->GetField("blue"), -1) )
      {
      pnt.SetBlue( ( *it2 )->GetField("blue") );
      }

    if ( Math::NotExactlyEquals(( *it2 )->GetField("alpha"), -1) )
      {
      pnt.SetAlpha( ( *it2 )->GetField("alpha") );
      }

    if ( Math::NotExactlyEquals(( *it2 )->GetField("id"), -1) )
      {
      pnt.SetID( (int)( ( *it2 )->GetField("id") ) );
      }

    tubeSO->GetPoints().push_back(pnt);

    it2++;
    }
  return tubeSO.GetPointer();
}

/** Convert a Tube SpatialObject into a MetaDTITube */
template< unsigned int NDimensions >
typename MetaDTITubeConverter<NDimensions>::MetaObjectType *
MetaDTITubeConverter< NDimensions >
::SpatialObjectToMetaObject(const SpatialObjectType *spatialObject)
{
  DTITubeSpatialObjectConstPointer DTITubeSO =
    dynamic_cast<const DTITubeSpatialObjectType *>(spatialObject);
  if(DTITubeSO.IsNull())
    {
    itkExceptionMacro(<< "Can't downcast SpatialObject to DTITubeSpatialObject");
    }

  DTITubeMetaObjectType *tube = new MetaDTITube(NDimensions);

  // Check what are the fields to be written
  bool writeNormal1 = false;
  bool writeNormal2 = false;
  bool writeTangent = false;
  bool writeRadius = false;
  bool writeColor = false;
  bool writeAlpha = false;
  bool writeID = false;

  typename DTITubeSpatialObjectType::PointListType::const_iterator it;
  for ( it = DTITubeSO->GetPoints().begin(); it != DTITubeSO->GetPoints().end(); ++it )
    {
    // Optional fields (written only if not default values)
    if ( ( *it ).GetID() != -1 )
      {
      writeID = true;
      }

    if ( ( *it ).GetRadius() != 0.0f )
      {
      writeRadius = true;
      }

    unsigned int d;
    for ( d = 0; d < NDimensions; d++ )
      {
      if ( Math::NotExactlyEquals(( *it ).GetNormal1()[d], 0) )
        {
        writeNormal1 = true;
        }
      if ( Math::NotExactlyEquals(( *it ).GetNormal2()[d], 0) )
        {
        writeNormal2 = true;
        }
      if ( Math::NotExactlyEquals(( *it ).GetTangent()[d], 0) )
        {
        writeTangent = true;
        }
      }

    // write the color if changed
    if ( ( ( *it ).GetRed() != 1.0 )
         || ( ( *it ).GetGreen() != 0.0 )
         || ( ( *it ).GetBlue() != 0.0 )
          )
      {
      writeColor = true;
      }

    if ( ( *it ).GetAlpha() != 1.0 )
      {
      writeAlpha = true;
      }
    }

  // fill in the tube information
  for ( it = DTITubeSO->GetPoints().begin(); it != DTITubeSO->GetPoints().end(); ++it )
    {
    DTITubePnt *pnt = new DTITubePnt(NDimensions);

    for ( unsigned int d = 0; d < NDimensions; d++ )
      {
      pnt->m_X[d] = ( *it ).GetPosition()[d];
      }

    const DTITubePnt::FieldListType &         metaFields = ( *it ).GetFields();
    DTITubePnt::FieldListType::const_iterator extraIt = metaFields.begin();
    while ( extraIt != metaFields.end() )
      {
      pnt->AddField( ( *extraIt ).first.c_str(), ( *extraIt ).second );
      extraIt++;
      }

    for ( unsigned int d = 0; d < 6; d++ )
      {
      pnt->m_TensorMatrix[d] = ( *it ).GetTensorMatrix()[d];
      }

    // Optional fields (written only if not default values)
    if ( writeID )
      {
      pnt->AddField( "id", ( *it ).GetID() );
      }

    if ( writeRadius )
      {
      pnt->AddField( "r", ( *it ).GetRadius() );
      }

    if ( writeNormal1 )
      {
      pnt->AddField("v1x", ( *it ).GetNormal1()[0]);
      pnt->AddField("v1y", ( *it ).GetNormal1()[1]);
      if ( NDimensions == 3 )
        {
        pnt->AddField("v1z", ( *it ).GetNormal1()[2]);
        }
      }

    if ( writeNormal2 )
      {
      pnt->AddField("v2x", ( *it ).GetNormal2()[0]);
      pnt->AddField("v2y", ( *it ).GetNormal2()[1]);
      if ( NDimensions == 3 )
        {
        pnt->AddField("v2z", ( *it ).GetNormal2()[2]);
        }
      }

    if ( writeTangent )
      {
      pnt->AddField("tx", ( *it ).GetTangent()[0]);
      pnt->AddField("ty", ( *it ).GetTangent()[1]);
      if ( NDimensions == 3 )
        {
        pnt->AddField("tz", ( *it ).GetTangent()[2]);
        }
      }

    // write the color if changed
    if ( writeColor )
      {
      pnt->AddField( "red", ( *it ).GetRed() );
      pnt->AddField( "green", ( *it ).GetGreen() );
      pnt->AddField( "blue", ( *it ).GetBlue() );
      }

    if ( writeAlpha )
      {
      pnt->AddField( "alpha", ( *it ).GetAlpha() );
      }

    tube->GetPoints().push_back(pnt);
    }

  tube->PointDim("x y z tensor1 tensor2 tensor3 tensor4 tensor5 tensor6");

  float color[4];
  for ( unsigned int ii = 0; ii < 4; ii++ )
    {
    color[ii] = DTITubeSO->GetProperty()->GetColor()[ii];
    }

  tube->Color(color);
  tube->ID( DTITubeSO->GetId() );

  if ( DTITubeSO->GetParent() )
    {
    tube->ParentID( DTITubeSO->GetParent()->GetId() );
    }
  tube->ParentPoint( DTITubeSO->GetParentPoint() );
  tube->NPoints(static_cast<int>( tube->GetPoints().size() ) );

  for ( unsigned int ii = 0; ii < NDimensions; ii++ )
    {
    tube->ElementSpacing(ii, DTITubeSO->GetIndexToObjectTransform()
                         ->GetScaleComponent()[ii]);
    }
  return tube;
}

} // end namespace itk

#endif
