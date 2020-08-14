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
#ifndef itkMetaVesselTubeConverter_hxx
#define itkMetaVesselTubeConverter_hxx

#include "itkMetaVesselTubeConverter.h"

namespace itk
{

template <unsigned int NDimensions>
typename MetaVesselTubeConverter<NDimensions>::MetaObjectType *
MetaVesselTubeConverter<NDimensions>::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new VesselTubeMetaObjectType);
}

/** Convert a MetaVesselTube into an Tube SpatialObject  */
template <unsigned int NDimensions>
typename MetaVesselTubeConverter<NDimensions>::SpatialObjectPointer
MetaVesselTubeConverter<NDimensions>::MetaObjectToSpatialObject(const MetaObjectType * mo)
{
  const auto * vesselTubeMO = dynamic_cast<const VesselTubeMetaObjectType *>(mo);
  if (vesselTubeMO == nullptr)
  {
    itkExceptionMacro(<< "Can't convert MetaObject to MetaVesselTube");
  }

  typename VesselTubeSpatialObjectType::Pointer vesselTubeSO = VesselTubeSpatialObjectType::New();

  vesselTubeSO->SetTypeName("VesselTubeSpatialObject");
  vesselTubeSO->GetProperty().SetName(vesselTubeMO->Name());
  vesselTubeSO->SetParentPoint(vesselTubeMO->ParentPoint());
  vesselTubeSO->SetId(vesselTubeMO->ID());
  vesselTubeSO->SetRoot(vesselTubeMO->Root());
  vesselTubeSO->SetParentId(vesselTubeMO->ParentID());
  vesselTubeSO->GetProperty().SetRed(vesselTubeMO->Color()[0]);
  vesselTubeSO->GetProperty().SetGreen(vesselTubeMO->Color()[1]);
  vesselTubeSO->GetProperty().SetBlue(vesselTubeMO->Color()[2]);
  vesselTubeSO->GetProperty().SetAlpha(vesselTubeMO->Color()[3]);
  if (vesselTubeMO->Artery())
  {
    vesselTubeSO->GetProperty().SetTagStringValue("Artery", "true");
  }
  else
  {
    vesselTubeSO->GetProperty().SetTagStringValue("Artery", "false");
  }

  using VesselTubePointType = itk::TubeSpatialObjectPoint<NDimensions>;

  auto it2 = vesselTubeMO->GetPoints().begin();

  itk::CovariantVector<double, NDimensions> v;
  itk::Vector<double, NDimensions>          t;

  for (unsigned int identifier = 0; identifier < vesselTubeMO->GetPoints().size(); identifier++)
  {
    VesselTubePointType pnt;

    typename VesselTubePointType::PointType pos;
    for (unsigned int d = 0; d < NDimensions; ++d)
    {
      pos[d] = (*it2)->m_X[d] * vesselTubeMO->ElementSpacing(d);
    }
    pnt.SetPositionInObjectSpace(pos);
    pnt.SetRadiusInObjectSpace((*it2)->m_R * vesselTubeMO->ElementSpacing(0));
    pnt.SetMedialness((*it2)->m_Medialness);
    pnt.SetBranchness((*it2)->m_Branchness);
    pnt.SetRidgeness((*it2)->m_Ridgeness);
    pnt.SetCurvature((*it2)->m_Curvature);
    pnt.SetLevelness((*it2)->m_Levelness);
    pnt.SetRoundness((*it2)->m_Roundness);
    pnt.SetIntensity((*it2)->m_Intensity);

    for (unsigned int ii = 0; ii < NDimensions; ii++)
    {
      v[ii] = (*it2)->m_V1[ii];
    }
    pnt.SetNormal1InObjectSpace(v);

    for (unsigned int ii = 0; ii < NDimensions; ii++)
    {
      v[ii] = (*it2)->m_V2[ii];
    }
    pnt.SetNormal2InObjectSpace(v);

    for (unsigned int ii = 0; ii < NDimensions; ii++)
    {
      t[ii] = (*it2)->m_T[ii];
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

    vesselTubeSO->AddPoint(pnt);

    it2++;
  }

  return vesselTubeSO.GetPointer();
}

/** Convert a Tube SpatialObject into a MetaVesselTube */
template <unsigned int NDimensions>
typename MetaVesselTubeConverter<NDimensions>::MetaObjectType *
MetaVesselTubeConverter<NDimensions>::SpatialObjectToMetaObject(const SpatialObjectType * so)
{
  const typename VesselTubeSpatialObjectType::ConstPointer vesselTubeSO =
    dynamic_cast<const VesselTubeSpatialObjectType *>(so);

  if (vesselTubeSO.IsNull())
  {
    itkExceptionMacro(<< "Can't downcast SpatialObject to VesselTubeSpatialObject");
  }
  auto * vesselTubeMO = new MetaVesselTube(NDimensions);

  // fill in the tube information

  typename VesselTubeSpatialObjectType::TubePointListType::const_iterator i;
  for (i = vesselTubeSO->GetPoints().begin(); i != vesselTubeSO->GetPoints().end(); i++)
  {
    auto * pnt = new VesselTubePnt(NDimensions);

    for (unsigned int d = 0; d < NDimensions; d++)
    {
      pnt->m_X[d] = (*i).GetPositionInObjectSpace()[d];
    }

    pnt->m_ID = (*i).GetId();
    pnt->m_R = (*i).GetRadiusInObjectSpace();
    pnt->m_Alpha1 = (*i).GetAlpha1();
    pnt->m_Alpha2 = (*i).GetAlpha2();
    pnt->m_Alpha3 = (*i).GetAlpha3();
    pnt->m_Medialness = (*i).GetMedialness();
    pnt->m_Branchness = (*i).GetBranchness();
    pnt->m_Ridgeness = (*i).GetRidgeness();
    pnt->m_Curvature = (*i).GetCurvature();
    pnt->m_Levelness = (*i).GetLevelness();
    pnt->m_Roundness = (*i).GetRoundness();
    pnt->m_Intensity = (*i).GetIntensity();

    auto iter = (*i).GetTagScalarDictionary().begin();
    while (iter != (*i).GetTagScalarDictionary().end())
    {
      pnt->AddField(iter->first.c_str(), iter->second);
      ++iter;
    }

    for (unsigned int d = 0; d < NDimensions; d++)
    {
      pnt->m_V1[d] = (*i).GetNormal1InObjectSpace()[d];
    }

    for (unsigned int d = 0; d < NDimensions; d++)
    {
      pnt->m_V2[d] = (*i).GetNormal2InObjectSpace()[d];
    }

    for (unsigned int d = 0; d < NDimensions; d++)
    {
      pnt->m_T[d] = (*i).GetTangentInObjectSpace()[d];
    }

    pnt->m_Color[0] = (*i).GetRed();
    pnt->m_Color[1] = (*i).GetGreen();
    pnt->m_Color[2] = (*i).GetBlue();
    pnt->m_Color[3] = (*i).GetAlpha();

    vesselTubeMO->GetPoints().push_back(pnt);
  }

  float color[4];
  for (unsigned int ii = 0; ii < 4; ii++)
  {
    color[ii] = vesselTubeSO->GetProperty().GetColor()[ii];
  }

  vesselTubeMO->Color(color);
  vesselTubeMO->ID(vesselTubeSO->GetId());
  vesselTubeMO->Root(vesselTubeSO->GetRoot());
  std::string str;
  if (vesselTubeSO->GetProperty().GetTagStringValue("Artery", str) && str == "True")
  {
    vesselTubeMO->Artery(true);
  }
  else
  {
    vesselTubeMO->Artery(false);
  }


  if (vesselTubeSO->GetParent())
  {
    vesselTubeMO->ParentID(vesselTubeSO->GetParent()->GetId());
  }
  vesselTubeMO->ParentPoint(vesselTubeSO->GetParentPoint());
  vesselTubeMO->NPoints(static_cast<int>(vesselTubeMO->GetPoints().size()));

  for (unsigned int ii = 0; ii < NDimensions; ii++)
  {
    vesselTubeMO->ElementSpacing(ii, 1);
    // Spacing is no longer used
    // vesselTubeSO->GetObjectToParentTransform()->GetScaleComponent()[ii]);
  }
  return vesselTubeMO;
}

} // end namespace itk

#endif
