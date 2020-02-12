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
#ifndef itkMetaContourConverter_hxx
#define itkMetaContourConverter_hxx

#include "itkMetaContourConverter.h"

namespace itk
{

template <unsigned int NDimensions>
typename MetaContourConverter<NDimensions>::MetaObjectType *
MetaContourConverter<NDimensions>::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new ContourMetaObjectType);
}

/** Convert a metaContour into an Contour SpatialObject  */
template <unsigned int NDimensions>
typename MetaContourConverter<NDimensions>::SpatialObjectPointer
MetaContourConverter<NDimensions>::MetaObjectToSpatialObject(const MetaObjectType * mo)
{
  const auto * contourMO = dynamic_cast<const MetaContour *>(mo);
  if (contourMO == nullptr)
  {
    itkExceptionMacro(<< "Can't downcast MetaObject to MetaContour");
  }

  ContourSpatialObjectPointer contourSO = ContourSpatialObjectType::New();

  contourSO->GetProperty().SetName(contourMO->Name());
  contourSO->SetId(contourMO->ID());
  contourSO->SetParentId(contourMO->ParentID());
  contourSO->GetProperty().SetRed(contourMO->Color()[0]);
  contourSO->GetProperty().SetGreen(contourMO->Color()[1]);
  contourSO->GetProperty().SetBlue(contourMO->Color()[2]);
  contourSO->GetProperty().SetAlpha(contourMO->Color()[3]);
  contourSO->SetIsClosed(const_cast<ContourMetaObjectType *>(contourMO)->Closed());
  contourSO->SetAttachedToSlice(const_cast<ContourMetaObjectType *>(contourMO)->AttachedToSlice());

  // First the control points
  using ControlPointType = typename ContourSpatialObjectType::ContourPointType;

  auto itCP = contourMO->GetControlPoints().begin();

  for (unsigned int identifier = 0; identifier < contourMO->GetControlPoints().size(); identifier++)
  {
    ControlPointType pnt;

    using PointType = typename ControlPointType::PointType;
    PointType point;
    PointType pickedPoint;

    using CovariantVectorType = typename ControlPointType::CovariantVectorType;
    CovariantVectorType normal;

    for (unsigned int i = 0; i < NDimensions; i++)
    {
      point[i] = (*itCP)->m_X[i] * contourMO->ElementSpacing(i);
    }

    for (unsigned int i = 0; i < NDimensions; i++)
    {
      pickedPoint[i] = (*itCP)->m_XPicked[i] * contourMO->ElementSpacing(i);
    }

    for (unsigned int i = 0; i < NDimensions; i++)
    {
      normal[i] = (*itCP)->m_V[i];
    }

    pnt.SetId((*itCP)->m_Id);
    pnt.SetRed((*itCP)->m_Color[0]);
    pnt.SetGreen((*itCP)->m_Color[1]);
    pnt.SetBlue((*itCP)->m_Color[2]);
    pnt.SetAlpha((*itCP)->m_Color[3]);

    pnt.SetPositionInObjectSpace(point);
    pnt.SetPickedPointInObjectSpace(pickedPoint);
    pnt.SetNormalInObjectSpace(normal);

    contourSO->GetControlPoints().push_back(pnt);
    itCP++;
  }

  // Then the interpolated points
  using InterpolatedPointType = typename ContourSpatialObjectType::ContourPointType;
  auto itI = contourMO->GetInterpolatedPoints().begin();

  for (unsigned int identifier = 0; identifier < contourMO->GetInterpolatedPoints().size(); identifier++)
  {
    InterpolatedPointType pnt;

    using PointType = typename ControlPointType::PointType;
    PointType point;

    for (unsigned int i = 0; i < NDimensions; i++)
    {
      point[i] = (*itI)->m_X[i];
    }

    pnt.SetId((*itI)->m_Id);
    pnt.SetRed((*itI)->m_Color[0]);
    pnt.SetGreen((*itI)->m_Color[1]);
    pnt.SetBlue((*itI)->m_Color[2]);
    pnt.SetAlpha((*itI)->m_Color[3]);

    pnt.SetPositionInObjectSpace(point);
    contourSO->AddPoint(pnt);
    itI++;
  }

  return contourSO.GetPointer();
}

/** Convert a Contour SpatialObject into a metaContour */
template <unsigned int NDimensions>
typename MetaContourConverter<NDimensions>::MetaObjectType *
MetaContourConverter<NDimensions>::SpatialObjectToMetaObject(const SpatialObjectType * so)
{
  ContourSpatialObjectConstPointer contourSO = dynamic_cast<const ContourSpatialObjectType *>(so);
  if (contourSO.IsNull())
  {
    itkExceptionMacro(<< "Can't downcast SpatialObject to ContourSpatialObject");
  }
  auto * contourMO = new MetaContour(NDimensions);


  // fill in the control points information
  typename ContourSpatialObjectType::ContourPointListType::const_iterator itCP;

  for (itCP = contourSO->GetControlPoints().begin(); itCP != contourSO->GetControlPoints().end(); itCP++)
  {
    auto * pnt = new ContourControlPnt(NDimensions);

    pnt->m_Id = (*itCP).GetId();

    for (unsigned int d = 0; d < NDimensions; d++)
    {
      pnt->m_X[d] = (*itCP).GetPositionInObjectSpace()[d];
    }

    for (unsigned int d = 0; d < NDimensions; d++)
    {
      pnt->m_XPicked[d] = (*itCP).GetPickedPointInObjectSpace()[d];
    }

    for (unsigned int d = 0; d < NDimensions; d++)
    {
      pnt->m_V[d] = (*itCP).GetNormalInObjectSpace()[d];
    }

    pnt->m_Color[0] = (*itCP).GetRed();
    pnt->m_Color[1] = (*itCP).GetGreen();
    pnt->m_Color[2] = (*itCP).GetBlue();
    pnt->m_Color[3] = (*itCP).GetAlpha();

    contourMO->GetControlPoints().push_back(pnt);
  }

  if (NDimensions == 2)
  {
    contourMO->ControlPointDim("id x y xp yp v1 v2 r g b a");
  }
  else if (NDimensions == 3)
  {
    contourMO->ControlPointDim("id x y z xp yp zp v1 v2 v3 r gn be a");
  }

  // fill in the interpolated points information
  typename ContourSpatialObjectType::ContourPointListType::const_iterator itI;
  for (itI = contourSO->GetPoints().begin(); itI != contourSO->GetPoints().end(); itI++)
  {
    auto * pnt = new ContourInterpolatedPnt(NDimensions);

    pnt->m_Id = (*itI).GetId();
    for (unsigned int d = 0; d < NDimensions; d++)
    {
      pnt->m_X[d] = (*itI).GetPositionInObjectSpace()[d];
    }

    pnt->m_Color[0] = (*itI).GetRed();
    pnt->m_Color[1] = (*itI).GetGreen();
    pnt->m_Color[2] = (*itI).GetBlue();
    pnt->m_Color[3] = (*itI).GetAlpha();

    contourMO->GetInterpolatedPoints().push_back(pnt);
  }

  if (NDimensions == 2)
  {
    contourMO->InterpolatedPointDim("id x y r g b a");
  }
  else if (NDimensions == 3)
  {
    contourMO->InterpolatedPointDim("id x y z r g b a");
  }

  // Set the interpolation type
  switch (contourSO->GetInterpolationMethod())
  {
    case ContourSpatialObjectType::InterpolationMethodEnum::EXPLICIT_INTERPOLATION:
      contourMO->Interpolation(MET_EXPLICIT_INTERPOLATION);
      break;
    case ContourSpatialObjectType::InterpolationMethodEnum::LINEAR_INTERPOLATION:
      contourMO->Interpolation(MET_LINEAR_INTERPOLATION);
      break;
    case ContourSpatialObjectType::InterpolationMethodEnum::BEZIER_INTERPOLATION:
      contourMO->Interpolation(MET_BEZIER_INTERPOLATION);
      break;
    default:
      contourMO->Interpolation(MET_NO_INTERPOLATION);
  }

  float color[4];
  for (unsigned int i = 0; i < 4; i++)
  {
    color[i] = contourSO->GetProperty().GetColor()[i];
  }
  contourMO->Color(color);
  contourMO->ID(contourSO->GetId());
  contourMO->Closed(contourSO->GetIsClosed());
  contourMO->AttachedToSlice(contourSO->GetAttachedToSlice());
  contourMO->DisplayOrientation(contourSO->GetOrientationInObjectSpace());

  if (contourSO->GetParent())
  {
    contourMO->ParentID(contourSO->GetParent()->GetId());
  }

  contourMO->BinaryData(true);
  return contourMO;
}

} // end namespace itk

#endif
