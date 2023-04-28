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
#ifndef itkShapePriorSegmentationLevelSetFunction_hxx
#define itkShapePriorSegmentationLevelSetFunction_hxx

#include "itkMath.h"

namespace itk
{

template <typename TImageType, typename TFeatureImageType>
ShapePriorSegmentationLevelSetFunction<TImageType, TFeatureImageType>::ShapePriorSegmentationLevelSetFunction()
{
  m_ShapeFunction = nullptr;
  m_ShapePriorWeight = NumericTraits<ScalarValueType>::ZeroValue();
}

template <typename TImageType, typename TFeatureImageType>
void
ShapePriorSegmentationLevelSetFunction<TImageType, TFeatureImageType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfObjectMacro(ShapeFunction);

  os << indent
     << "ShapePriorWeight: " << static_cast<typename NumericTraits<ScalarValueType>::PrintType>(m_ShapePriorWeight)
     << std::endl;
}

template <typename TImageType, typename TFeatureImageType>
auto
ShapePriorSegmentationLevelSetFunction<TImageType, TFeatureImageType>::ComputeUpdate(
  const NeighborhoodType & neighborhood,
  void *                   gd,
  const FloatOffsetType &  offset) -> PixelType
{
  // Compute the generic level set update using superclass
  PixelType value = this->Superclass::ComputeUpdate(neighborhood, gd, offset);

  // Add the shape prior term
  if (m_ShapeFunction && Math::NotExactlyEquals(m_ShapePriorWeight, NumericTraits<ScalarValueType>::ZeroValue()))
  {
    IndexType                               idx = neighborhood.GetIndex();
    ContinuousIndex<double, ImageDimension> cdx;
    for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      cdx[i] = static_cast<double>(idx[i]) - offset[i];
    }
    typename ShapeFunctionType::PointType point;
    this->GetFeatureImage()->TransformContinuousIndexToPhysicalPoint(cdx, point);

    ScalarValueType shape_term =
      m_ShapePriorWeight * (m_ShapeFunction->Evaluate(point) - neighborhood.GetCenterPixel());

    value += shape_term;

    // collect max change to be used for calculating the time step
    auto * globalData = (ShapePriorGlobalDataStruct *)gd;
    globalData->m_MaxShapePriorChange = std::max(globalData->m_MaxShapePriorChange, itk::Math::abs(shape_term));
  }

  return value;
}

template <typename TImageType, typename TFeatureImageType>
auto
ShapePriorSegmentationLevelSetFunction<TImageType, TFeatureImageType>::ComputeGlobalTimeStep(void * gd) const
  -> TimeStepType
{
  TimeStepType dt;

  auto * d = (ShapePriorGlobalDataStruct *)gd;

  d->m_MaxAdvectionChange += d->m_MaxPropagationChange + d->m_MaxShapePriorChange;

  if (itk::Math::abs(d->m_MaxCurvatureChange) > 0.0)
  {
    if (d->m_MaxAdvectionChange > 0.0)
    {
      dt = std::min((this->m_WaveDT / d->m_MaxAdvectionChange), (this->m_DT / d->m_MaxCurvatureChange));
    }
    else
    {
      dt = this->m_DT / d->m_MaxCurvatureChange;
    }
  }
  else
  {
    if (d->m_MaxAdvectionChange > 0.0)
    {
      dt = this->m_WaveDT / d->m_MaxAdvectionChange;
    }
    else
    {
      dt = 0.0;
    }
  }

  double maxScaleCoefficient = 0.0;
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    maxScaleCoefficient = std::max(this->m_ScaleCoefficients[i], maxScaleCoefficient);
  }
  dt /= maxScaleCoefficient;

  // reset the values
  d->m_MaxAdvectionChange = 0;
  d->m_MaxPropagationChange = 0;
  d->m_MaxCurvatureChange = 0;
  d->m_MaxShapePriorChange = 0;

  return dt;
}
} // end namespace itk

#endif
