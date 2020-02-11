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
#ifndef itkMetaLineConverter_hxx
#define itkMetaLineConverter_hxx

#include "itkMetaLineConverter.h"

namespace itk
{

template <unsigned int NDimensions>
typename MetaLineConverter<NDimensions>::MetaObjectType *
MetaLineConverter<NDimensions>::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new LineMetaObjectType);
}

/** Convert a metaLine into an Line SpatialObject  */
template <unsigned int NDimensions>
typename MetaLineConverter<NDimensions>::SpatialObjectPointer
MetaLineConverter<NDimensions>::MetaObjectToSpatialObject(const MetaObjectType * mo)
{
  const auto * lineMO = dynamic_cast<const LineMetaObjectType *>(mo);
  if (lineMO == nullptr)
  {
    itkExceptionMacro(<< "Can't convert MetaObject to MetaLine");
  }

  LineSpatialObjectPointer lineSO = LineSpatialObjectType::New();

  lineSO->GetProperty().SetName(lineMO->Name());
  lineSO->SetId(lineMO->ID());
  lineSO->SetParentId(lineMO->ParentID());
  lineSO->GetProperty().SetRed(lineMO->Color()[0]);
  lineSO->GetProperty().SetGreen(lineMO->Color()[1]);
  lineSO->GetProperty().SetBlue(lineMO->Color()[2]);
  lineSO->GetProperty().SetAlpha(lineMO->Color()[3]);

  using LinePointType = itk::LineSpatialObjectPoint<NDimensions>;

  auto it2 = lineMO->GetPoints().begin();

  for (unsigned int identifier = 0; identifier < lineMO->GetPoints().size(); identifier++)
  {
    LinePointType pnt;

    using PointType = typename LinePointType::PointType;
    PointType point;
    using NormalType = typename LinePointType::CovariantVectorType;

    for (unsigned int ii = 0; ii < NDimensions; ii++)
    {
      point[ii] = (*it2)->m_X[ii] * lineMO->ElementSpacing(ii);
    }

    pnt.SetPositionInObjectSpace(point);

    for (unsigned int ii = 0; ii < NDimensions - 1; ii++)
    {
      NormalType normal;
      for (unsigned int jj = 0; jj < NDimensions; jj++)
      {
        normal[jj] = (*it2)->m_V[ii][jj];
      }
      pnt.SetNormalInObjectSpace(normal, ii);
    }

    pnt.SetRed((*it2)->m_Color[0]);
    pnt.SetGreen((*it2)->m_Color[1]);
    pnt.SetBlue((*it2)->m_Color[2]);
    pnt.SetAlpha((*it2)->m_Color[3]);

    lineSO->AddPoint(pnt);
    it2++;
  }
  return lineSO.GetPointer();
}

/** Convert a Line SpatialObject into a metaLine */
template <unsigned int NDimensions>
typename MetaLineConverter<NDimensions>::MetaObjectType *
MetaLineConverter<NDimensions>::SpatialObjectToMetaObject(const SpatialObjectType * spatialObject)
{
  LineSpatialObjectConstPointer lineSO = dynamic_cast<const LineSpatialObjectType *>(spatialObject);
  if (lineSO.IsNull())
  {
    itkExceptionMacro(<< "Can't downcast SpatialObject to LineSpatialObject");
  }

  auto * lineMO = new MetaLine(NDimensions);

  // due to a Visual Studio stupidity, can't seem to define
  // a const method to return the points list.
  const typename LineSpatialObjectType::LinePointListType & linePoints = lineSO->GetPoints();

  // fill in the Line information
  typename LineSpatialObjectType::LinePointListType::const_iterator it;
  for (it = linePoints.begin(); it != linePoints.end(); ++it)
  {
    auto * pnt = new LinePnt(NDimensions);

    for (unsigned int d = 0; d < NDimensions; d++)
    {
      pnt->m_X[d] = (*it).GetPositionInObjectSpace()[d];
    }

    for (unsigned int n = 0; n < NDimensions - 1; n++)
    {
      for (unsigned int d = 0; d < NDimensions; d++)
      {
        pnt->m_V[n][d] = ((*it).GetNormalInObjectSpace(n))[d];
      }
    }

    pnt->m_Color[0] = (*it).GetRed();
    pnt->m_Color[1] = (*it).GetGreen();
    pnt->m_Color[2] = (*it).GetBlue();
    pnt->m_Color[3] = (*it).GetAlpha();

    lineMO->GetPoints().push_back(pnt);
  }

  if (NDimensions == 2)
  {
    lineMO->PointDim("x y v1x v1y v2x v2y red green blue alpha");
  }
  else if (NDimensions == 3)
  {
    lineMO->PointDim("x y z v1x v1y v1z v2x v2y v2z red green blue alpha");
  }

  float color[4];
  for (unsigned int ii = 0; ii < 4; ii++)
  {
    color[ii] = lineSO->GetProperty().GetColor()[ii];
  }

  lineMO->Color(color);
  lineMO->ID(lineSO->GetId());
  if (lineSO->GetParent())
  {
    lineMO->ParentID(lineSO->GetParent()->GetId());
  }
  lineMO->NPoints(static_cast<int>(linePoints.size()));
  lineMO->BinaryData(true);
  return lineMO;
}

} // end namespace itk

#endif
