/*=========================================================================
 *
 *  Copyright NumFOCUS
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

template <unsigned int NDimensions>
typename MetaBlobConverter<NDimensions>::MetaObjectType *
MetaBlobConverter<NDimensions>::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new BlobMetaObjectType);
}

/** Convert a metaBlob into an Blob SpatialObject  */
template <unsigned int NDimensions>
typename MetaBlobConverter<NDimensions>::SpatialObjectPointer
MetaBlobConverter<NDimensions>::MetaObjectToSpatialObject(const MetaObjectType * mo)
{
  const auto * Blob = dynamic_cast<const BlobMetaObjectType *>(mo);
  if (Blob == nullptr)
  {
    itkExceptionMacro(<< "Can't downcast MetaObject to BlobMetaObject");
  }

  typename BlobSpatialObjectType::Pointer blob = BlobSpatialObjectType::New();

  blob->GetProperty().SetName(Blob->Name());
  blob->SetId(Blob->ID());
  blob->SetParentId(Blob->ParentID());
  blob->GetProperty().SetRed(Blob->Color()[0]);
  blob->GetProperty().SetGreen(Blob->Color()[1]);
  blob->GetProperty().SetBlue(Blob->Color()[2]);
  blob->GetProperty().SetAlpha(Blob->Color()[3]);

  using BlobPointType = itk::SpatialObjectPoint<NDimensions>;

  auto it2 = Blob->GetPoints().begin();

  vnl_vector<double> v(NDimensions);

  for (unsigned int identifier = 0; identifier < Blob->GetPoints().size(); identifier++)
  {
    BlobPointType pnt;

    using PointType = typename BlobSpatialObjectType::PointType;
    PointType point;

    for (unsigned int ii = 0; ii < NDimensions; ii++)
    {
      point[ii] = (*it2)->m_X[ii] * Blob->ElementSpacing(ii);
    }

    pnt.SetPositionInObjectSpace(point);

    pnt.SetRed((*it2)->m_Color[0]);
    pnt.SetGreen((*it2)->m_Color[1]);
    pnt.SetBlue((*it2)->m_Color[2]);
    pnt.SetAlpha((*it2)->m_Color[3]);

    blob->AddPoint(pnt);
    it2++;
  }

  return SpatialObjectPointer(blob);
}

/** Convert a Blob SpatialObject into a metaBlob */
template <unsigned int NDimensions>
typename MetaBlobConverter<NDimensions>::MetaObjectType *
MetaBlobConverter<NDimensions>::SpatialObjectToMetaObject(const SpatialObjectType * spatialObject)
{
  BlobSpatialObjectConstPointer blobSO = dynamic_cast<const BlobSpatialObjectType *>(spatialObject);
  if (blobSO.IsNull())
  {
    itkExceptionMacro(<< "Can't downcast SpatialObject to BlobSpatialObject");
  }

  auto * Blob = new MetaBlob(NDimensions);

  // fill in the Blob information
  typename BlobSpatialObjectType::BlobPointListType::const_iterator it;
  for (it = blobSO->GetPoints().begin(); it != blobSO->GetPoints().end(); it++)
  {
    auto * pnt = new BlobPnt(NDimensions);

    for (unsigned int d = 0; d < NDimensions; d++)
    {
      pnt->m_X[d] = (*it).GetPositionInObjectSpace()[d];
    }

    pnt->m_Color[0] = (*it).GetRed();
    pnt->m_Color[1] = (*it).GetGreen();
    pnt->m_Color[2] = (*it).GetBlue();
    pnt->m_Color[3] = (*it).GetAlpha();

    Blob->GetPoints().push_back(pnt);
  }

  if (NDimensions == 2)
  {
    Blob->PointDim("x y red green blue alpha");
  }
  else
  {
    Blob->PointDim("x y z red green blue alpha");
  }

  float color[4];
  for (unsigned int ii = 0; ii < 4; ii++)
  {
    color[ii] = spatialObject->GetProperty().GetColor()[ii];
  }

  Blob->Color(color);
  Blob->ID(spatialObject->GetId());
  if (spatialObject->GetParent())
  {
    Blob->ParentID(spatialObject->GetParent()->GetId());
  }
  Blob->NPoints(Blob->GetPoints().size());

  Blob->BinaryData(true);
  return Blob;
}

} // end namespace itk

#endif
