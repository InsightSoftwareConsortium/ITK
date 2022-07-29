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
#ifndef itkMetaSurfaceConverter_hxx
#define itkMetaSurfaceConverter_hxx


namespace itk
{
template <unsigned int VDimension>
auto
MetaSurfaceConverter<VDimension>::CreateMetaObject() -> MetaObjectType *
{
  return dynamic_cast<MetaObjectType *>(new SurfaceMetaObjectType);
}

/** Convert a metaSurface into an Surface SpatialObject */
template <unsigned int VDimension>
auto
MetaSurfaceConverter<VDimension>::MetaObjectToSpatialObject(const MetaObjectType * mo) -> SpatialObjectPointer
{
  const auto * surfaceMO = dynamic_cast<const SurfaceMetaObjectType *>(mo);
  if (surfaceMO == nullptr)
  {
    itkExceptionMacro(<< "Can't convert MetaObject to MetaSurface");
  }
  auto surfaceSO = SurfaceSpatialObjectType::New();

  surfaceSO->GetProperty().SetName(surfaceMO->Name());
  surfaceSO->SetId(surfaceMO->ID());
  surfaceSO->SetParentId(surfaceMO->ParentID());
  surfaceSO->GetProperty().SetRed(surfaceMO->Color()[0]);
  surfaceSO->GetProperty().SetGreen(surfaceMO->Color()[1]);
  surfaceSO->GetProperty().SetBlue(surfaceMO->Color()[2]);
  surfaceSO->GetProperty().SetAlpha(surfaceMO->Color()[3]);

  using SurfacePointType = typename SurfaceSpatialObjectType::SurfacePointType;

  auto it2 = surfaceMO->GetPoints().begin();

  for (unsigned int identifier = 0; identifier < surfaceMO->GetPoints().size(); ++identifier)
  {
    SurfacePointType pnt;

    using PointType = typename SurfacePointType::PointType;
    PointType point;
    using CovariantVectorType = typename SurfacePointType::CovariantVectorType;
    CovariantVectorType normal;

    for (unsigned int ii = 0; ii < VDimension; ++ii)
    {
      point[ii] = (*it2)->m_X[ii] * surfaceMO->ElementSpacing(ii);
    }

    for (unsigned int ii = 0; ii < VDimension; ++ii)
    {
      normal[ii] = (*it2)->m_V[ii];
    }

    pnt.SetRed((*it2)->m_Color[0]);
    pnt.SetGreen((*it2)->m_Color[1]);
    pnt.SetBlue((*it2)->m_Color[2]);
    pnt.SetAlpha((*it2)->m_Color[3]);

    pnt.SetPositionInObjectSpace(point);
    pnt.SetNormalInObjectSpace(normal);

    surfaceSO->AddPoint(pnt);
    ++it2;
  }

  return surfaceSO.GetPointer();
}

/** Convert a Surface SpatialObject into a metaSurface */
template <unsigned int VDimension>
auto
MetaSurfaceConverter<VDimension>::SpatialObjectToMetaObject(const SpatialObjectType * so) -> MetaObjectType *
{
  SurfaceSpatialObjectConstPointer surfaceSO = dynamic_cast<const SurfaceSpatialObjectType *>(so);

  if (surfaceSO.IsNull())
  {
    itkExceptionMacro(<< "Can't downcast SpatialObject to SurfaceSpatialObject");
  }
  auto * surfaceMO = new MetaSurface(VDimension);

  // fill in the Surface information
  typename SurfaceSpatialObjectType::SurfacePointListType::const_iterator it;
  for (it = surfaceSO->GetPoints().begin(); it != surfaceSO->GetPoints().end(); ++it)
  {
    auto * pnt = new SurfacePnt(VDimension);

    for (unsigned int d = 0; d < VDimension; ++d)
    {
      pnt->m_X[d] = it->GetPositionInObjectSpace()[d];
    }

    for (unsigned int d = 0; d < VDimension; ++d)
    {
      pnt->m_V[d] = it->GetNormalInObjectSpace()[d];
    }

    pnt->m_Color[0] = it->GetRed();
    pnt->m_Color[1] = it->GetGreen();
    pnt->m_Color[2] = it->GetBlue();
    pnt->m_Color[3] = it->GetAlpha();

    surfaceMO->GetPoints().push_back(pnt);
  }

  if (VDimension == 2)
  {
    surfaceMO->PointDim("x y v1 v2 red green blue alpha");
  }
  else if (VDimension == 3)
  {
    surfaceMO->PointDim("x y z v1 v2 v3 red green blue alpha");
  }

  float color[4];
  for (unsigned int ii = 0; ii < 4; ++ii)
  {
    color[ii] = surfaceSO->GetProperty().GetColor()[ii];
  }

  surfaceMO->Color(color);
  surfaceMO->ID(surfaceSO->GetId());
  if (surfaceSO->GetParent())
  {
    surfaceMO->ParentID(surfaceSO->GetParent()->GetId());
  }
  surfaceMO->NPoints(static_cast<int>(surfaceMO->GetPoints().size()));

  return surfaceMO;
}

} // end namespace itk

#endif
