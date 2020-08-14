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
#ifndef itkMetaTubeConverter_hxx
#define itkMetaTubeConverter_hxx

#include "itkMetaTubeConverter.h"

namespace itk
{
template <unsigned int NDimensions>
typename MetaTubeConverter<NDimensions>::MetaObjectType *
MetaTubeConverter<NDimensions>::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new TubeMetaObjectType);
}

/** Convert a metaTube into an Tube SpatialObject  */
template <unsigned int NDimensions>
typename MetaTubeConverter<NDimensions>::SpatialObjectPointer
MetaTubeConverter<NDimensions>::MetaObjectToSpatialObject(const MetaObjectType * mo)
{
  const auto * tubeMO = dynamic_cast<const TubeMetaObjectType *>(mo);
  if (tubeMO == nullptr)
  {
    itkExceptionMacro(<< "Can't convert MetaObject to MetaTube");
  }

  typename TubeSpatialObjectType::Pointer tubeSO = TubeSpatialObjectType::New();

  tubeSO->GetProperty().SetName(tubeMO->Name());
  tubeSO->SetParentPoint(tubeMO->ParentPoint());
  tubeSO->SetId(tubeMO->ID());
  tubeSO->SetParentId(tubeMO->ParentID());
  tubeSO->GetProperty().SetRed(tubeMO->Color()[0]);
  tubeSO->GetProperty().SetGreen(tubeMO->Color()[1]);
  tubeSO->GetProperty().SetBlue(tubeMO->Color()[2]);
  tubeSO->GetProperty().SetAlpha(tubeMO->Color()[3]);

  using TubePointType = itk::TubeSpatialObjectPoint<NDimensions>;

  auto it2 = tubeMO->GetPoints().begin();

  itk::CovariantVector<double, NDimensions> v;
  itk::Vector<double, NDimensions>          t;

  for (unsigned int identifier = 0; identifier < tubeMO->GetPoints().size(); identifier++)
  {
    TubePointType pnt;

    typename TubePointType::PointType pos;
    for (unsigned int d = 0; d < NDimensions; ++d)
    {
      pos[d] = (*it2)->m_X[d] * tubeMO->ElementSpacing(d);
    }
    pnt.SetPositionInObjectSpace(pos);
    pnt.SetRadiusInObjectSpace((*it2)->m_R * tubeMO->ElementSpacing(0));
    pnt.SetMedialness((*it2)->m_Medialness);
    pnt.SetBranchness((*it2)->m_Branchness);
    pnt.SetRidgeness((*it2)->m_Ridgeness);
    pnt.SetCurvature((*it2)->m_Curvature);
    pnt.SetLevelness((*it2)->m_Levelness);
    pnt.SetRoundness((*it2)->m_Roundness);
    pnt.SetIntensity((*it2)->m_Intensity);

    for (unsigned int i = 0; i < NDimensions; i++)
    {
      v[i] = (*it2)->m_V1[i];
    }
    pnt.SetNormal1InObjectSpace(v);

    for (unsigned int i = 0; i < NDimensions; i++)
    {
      v[i] = (*it2)->m_V2[i];
    }
    pnt.SetNormal2InObjectSpace(v);

    for (unsigned int i = 0; i < NDimensions; i++)
    {
      t[i] = (*it2)->m_T[i];
    }
    pnt.SetTangentInObjectSpace(t);

    pnt.SetAlpha1((*it2)->m_Alpha1);
    pnt.SetAlpha2((*it2)->m_Alpha2);
    pnt.SetAlpha3((*it2)->m_Alpha3);

    pnt.SetRed((*it2)->m_Color[0]);
    pnt.SetGreen((*it2)->m_Color[1]);
    pnt.SetBlue((*it2)->m_Color[2]);
    pnt.SetAlpha((*it2)->m_Color[3]);

    pnt.SetId((*it2)->m_ID);

    auto iter = (*it2)->m_ExtraFields.begin();
    while (iter != (*it2)->m_ExtraFields.end())
    {
      pnt.SetTagScalarValue(iter->first.c_str(), iter->second);
      ++iter;
    }

    tubeSO->AddPoint(pnt);

    it2++;
  }

  return tubeSO.GetPointer();
}

/** Convert a Tube SpatialObject into a metaTube */
template <unsigned int NDimensions>
typename MetaTubeConverter<NDimensions>::MetaObjectType *
MetaTubeConverter<NDimensions>::SpatialObjectToMetaObject(const SpatialObjectType * spatialObject)
{
  TubeSpatialObjectConstPointer tubeSO = dynamic_cast<const TubeSpatialObjectType *>(spatialObject);
  if (tubeSO.IsNull())
  {
    itkExceptionMacro(<< "Can't downcast SpatialObject to TubeSpatialObject");
  }

  auto * tubeMO = new MetaTube(NDimensions);

  // fill in the tube information
  typename TubeSpatialObjectType::TubePointListType::const_iterator it;
  for (it = tubeSO->GetPoints().begin(); it != tubeSO->GetPoints().end(); it++)
  {
    auto * pnt = new TubePnt(NDimensions);

    for (unsigned int d = 0; d < NDimensions; d++)
    {
      pnt->m_X[d] = (*it).GetPositionInObjectSpace()[d];
    }

    pnt->m_ID = (*it).GetId();
    pnt->m_R = (*it).GetRadiusInObjectSpace();
    pnt->m_Alpha1 = (*it).GetAlpha1();
    pnt->m_Alpha2 = (*it).GetAlpha2();
    pnt->m_Alpha3 = (*it).GetAlpha3();
    pnt->m_Medialness = (*it).GetMedialness();
    pnt->m_Branchness = (*it).GetBranchness();
    pnt->m_Ridgeness = (*it).GetRidgeness();
    pnt->m_Curvature = (*it).GetCurvature();
    pnt->m_Levelness = (*it).GetLevelness();
    pnt->m_Roundness = (*it).GetRoundness();
    pnt->m_Intensity = (*it).GetIntensity();

    auto iter = (*it).GetTagScalarDictionary().begin();
    while (iter != (*it).GetTagScalarDictionary().end())
    {
      pnt->AddField(iter->first.c_str(), iter->second);
      ++iter;
    }

    for (unsigned int d = 0; d < NDimensions; d++)
    {
      pnt->m_V1[d] = (*it).GetNormal1InObjectSpace()[d];
    }

    for (unsigned int d = 0; d < NDimensions; d++)
    {
      pnt->m_V2[d] = (*it).GetNormal2InObjectSpace()[d];
    }

    for (unsigned int d = 0; d < NDimensions; d++)
    {
      pnt->m_T[d] = (*it).GetTangentInObjectSpace()[d];
    }

    pnt->m_Color[0] = (*it).GetRed();
    pnt->m_Color[1] = (*it).GetGreen();
    pnt->m_Color[2] = (*it).GetBlue();
    pnt->m_Color[3] = (*it).GetAlpha();

    tubeMO->GetPoints().push_back(pnt);
  }

  float color[4];
  for (unsigned int i = 0; i < 4; i++)
  {
    color[i] = tubeSO->GetProperty().GetColor()[i];
  }

  tubeMO->Color(color);
  tubeMO->ID(tubeSO->GetId());

  if (tubeSO->GetParent())
  {
    tubeMO->ParentID(tubeSO->GetParent()->GetId());
  }
  tubeMO->ParentPoint(tubeSO->GetParentPoint());
  tubeMO->NPoints(static_cast<int>(tubeMO->GetPoints().size()));

  return tubeMO;
}

} // end namespace itk

#endif
