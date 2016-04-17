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
#ifndef itkMetaBlobConverter_hxx
#define itkMetaBlobConverter_hxx

#include "itkMetaBlobConverter.h"

namespace itk
{
/** Constructor */
template< unsigned int NDimensions >
MetaBlobConverter< NDimensions >
::MetaBlobConverter()
{}

template< unsigned int NDimensions >
typename MetaBlobConverter< NDimensions >::MetaObjectType *
MetaBlobConverter< NDimensions>
::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new BlobMetaObjectType);
}

/** Convert a metaBlob into an Blob SpatialObject  */
template< unsigned int NDimensions >
typename MetaBlobConverter< NDimensions >::SpatialObjectPointer
MetaBlobConverter< NDimensions >
::MetaObjectToSpatialObject(const MetaObjectType *mo)
{
  const BlobMetaObjectType *Blob = dynamic_cast<const BlobMetaObjectType *>(mo);
  if(Blob == ITK_NULLPTR)
    {
    itkExceptionMacro(<< "Can't downcast MetaObject to BlobMetaObject");
    }

  typename BlobSpatialObjectType::Pointer blob = BlobSpatialObjectType::New();

  unsigned int ndims = Blob->NDims();
  double       spacing[NDimensions];
  for ( unsigned int ii = 0; ii < ndims; ii++ )
    {
    spacing[ii] = Blob->ElementSpacing()[ii];
    }

  blob->GetIndexToObjectTransform()->SetScaleComponent(spacing);
  blob->GetProperty()->SetName( Blob->Name() );
  blob->SetId( Blob->ID() );
  blob->SetParentId( Blob->ParentID() );
  blob->GetProperty()->SetRed(Blob->Color()[0]);
  blob->GetProperty()->SetGreen(Blob->Color()[1]);
  blob->GetProperty()->SetBlue(Blob->Color()[2]);
  blob->GetProperty()->SetAlpha(Blob->Color()[3]);

  typedef itk::SpatialObjectPoint< NDimensions > BlobPointType;

  MetaBlob::PointListType::const_iterator it2 = Blob->GetPoints().begin();

  vnl_vector< double > v(ndims);

  for ( unsigned int identifier = 0; identifier < Blob->GetPoints().size(); identifier++ )
    {
    BlobPointType pnt;

    typedef typename BlobSpatialObjectType::PointType PointType;
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

    blob->GetPoints().push_back(pnt);
    it2++;
    }

  return SpatialObjectPointer(blob.GetPointer());
}

/** Convert a Blob SpatialObject into a metaBlob */
template< unsigned int NDimensions >
typename MetaBlobConverter<NDimensions>::MetaObjectType *
MetaBlobConverter< NDimensions >
::SpatialObjectToMetaObject(const SpatialObjectType *spatialObject)
{
  BlobSpatialObjectConstPointer blobSO = dynamic_cast<const BlobSpatialObjectType *>(spatialObject);
  if(blobSO.IsNull())
    {
    itkExceptionMacro(<< "Can't downcast SpatialObject to BlobSpatialObject");
    }

  BlobMetaObjectType *Blob = new MetaBlob(NDimensions);

  // fill in the Blob information
  typename BlobSpatialObjectType::PointListType::const_iterator i;
  for ( i = blobSO->GetPoints().begin();
        i != blobSO->GetPoints().end();
        i++ )
    {
    BlobPnt *pnt = new BlobPnt(NDimensions);

    for ( unsigned int d = 0; d < NDimensions; d++ )
      {
      pnt->m_X[d] = ( *i ).GetPosition()[d];
      }

    pnt->m_Color[0] = ( *i ).GetRed();
    pnt->m_Color[1] = ( *i ).GetGreen();
    pnt->m_Color[2] = ( *i ).GetBlue();
    pnt->m_Color[3] = ( *i ).GetAlpha();

    Blob->GetPoints().push_back(pnt);
    }

  if ( NDimensions == 2 )
    {
    Blob->PointDim("x y red green blue alpha");
    }
  else
    {
    Blob->PointDim("x y z red green blue alpha");
    }

  float color[4];
  for ( unsigned int ii = 0; ii < 4; ii++ )
    {
    color[ii] = spatialObject->GetProperty()->GetColor()[ii];
    }

  Blob->Color(color);
  Blob->ID( spatialObject->GetId() );
  if ( spatialObject->GetParent() )
    {
    Blob->ParentID( spatialObject->GetParent()->GetId() );
    }
  Blob->NPoints(Blob->GetPoints().size());

  for ( unsigned int ii = 0; ii < NDimensions; ii++ )
    {
    Blob->ElementSpacing(ii, spatialObject->GetIndexToObjectTransform()
                         ->GetScaleComponent()[ii]);
    }
  Blob->BinaryData(true);
  return Blob;
}

} // end namespace itk

#endif
