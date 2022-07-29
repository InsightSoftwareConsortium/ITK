/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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


namespace itk
{

template <unsigned int VDimension>
auto
MetaLandmarkConverter<VDimension>::CreateMetaObject() -> MetaObjectType *
{
  return dynamic_cast<MetaObjectType *>(new LandmarkMetaObjectType);
}

/** Convert a metaLandmark into an Landmark SpatialObject  */
template <unsigned int VDimension>
auto
MetaLandmarkConverter<VDimension>::MetaObjectToSpatialObject(const MetaObjectType * mo) -> SpatialObjectPointer
{
  const auto * landmarkMO = dynamic_cast<const LandmarkMetaObjectType *>(mo);
  if (landmarkMO == nullptr)
  {
    itkExceptionMacro(<< "Can't convert MetaObject to MetaLandmark");
  }

  LandmarkSpatialObjectPointer landmarkSO = LandmarkSpatialObjectType::New();

  landmarkSO->GetProperty().SetName(landmarkMO->Name());
  landmarkSO->SetId(landmarkMO->ID());
  landmarkSO->SetParentId(landmarkMO->ParentID());
  landmarkSO->GetProperty().SetRed(landmarkMO->Color()[0]);
  landmarkSO->GetProperty().SetGreen(landmarkMO->Color()[1]);
  landmarkSO->GetProperty().SetBlue(landmarkMO->Color()[2]);
  landmarkSO->GetProperty().SetAlpha(landmarkMO->Color()[3]);

  using LandmarkPointType = itk::SpatialObjectPoint<VDimension>;

  auto it2 = landmarkMO->GetPoints().begin();

  for (unsigned int identifier = 0; identifier < landmarkMO->GetPoints().size(); ++identifier)
  {
    LandmarkPointType pnt;

    using PointType = typename LandmarkSpatialObjectType::PointType;
    PointType point;

    for (unsigned int ii = 0; ii < VDimension; ++ii)
    {
      point[ii] = (*it2)->m_X[ii] * landmarkMO->ElementSpacing(ii);
    }

    pnt.SetPositionInObjectSpace(point);

    pnt.SetRed((*it2)->m_Color[0]);
    pnt.SetGreen((*it2)->m_Color[1]);
    pnt.SetBlue((*it2)->m_Color[2]);
    pnt.SetAlpha((*it2)->m_Color[3]);

    landmarkSO->AddPoint(pnt);
    ++it2;
  }

  return landmarkSO.GetPointer();
}

/** Convert a Landmark SpatialObject into a metaLandmark */
template <unsigned int VDimension>
auto
MetaLandmarkConverter<VDimension>::SpatialObjectToMetaObject(const SpatialObjectType * so) -> MetaObjectType *
{
  const LandmarkSpatialObjectConstPointer landmarkSO = dynamic_cast<const LandmarkSpatialObjectType *>(so);

  if (landmarkSO.IsNull())
  {
    itkExceptionMacro(<< "Can't downcast SpatialObject to LandmarkSpatialObject");
  }

  auto * landmarkMO = new MetaLandmark(VDimension);

  // fill in the Landmark information
  typename LandmarkSpatialObjectType::LandmarkPointListType::const_iterator it;
  for (it = landmarkSO->GetPoints().begin(); it != landmarkSO->GetPoints().end(); ++it)
  {
    auto * pnt = new LandmarkPnt(VDimension);

    for (unsigned int d = 0; d < VDimension; ++d)
    {
      pnt->m_X[d] = it->GetPositionInObjectSpace()[d];
    }

    pnt->m_Color[0] = it->GetRed();
    pnt->m_Color[1] = it->GetGreen();
    pnt->m_Color[2] = it->GetBlue();
    pnt->m_Color[3] = it->GetAlpha();
    landmarkMO->GetPoints().push_back(pnt);
  }

  if (VDimension == 2)
  {
    landmarkMO->PointDim("x y red green blue alpha");
  }
  else
  {
    landmarkMO->PointDim("x y z red green blue alpha");
  }

  float color[4];
  for (unsigned int ii = 0; ii < 4; ++ii)
  {
    color[ii] = landmarkSO->GetProperty().GetColor()[ii];
  }

  landmarkMO->Color(color);
  landmarkMO->ID(landmarkSO->GetId());
  if (landmarkSO->GetParent())
  {
    landmarkMO->ParentID(landmarkSO->GetParent()->GetId());
  }
  landmarkMO->NPoints(static_cast<int>(landmarkMO->GetPoints().size()));
  landmarkMO->BinaryData(true);

  return landmarkMO;
}

} // end namespace itk

#endif
