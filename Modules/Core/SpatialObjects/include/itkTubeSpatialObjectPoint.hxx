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

#ifndef itkTubeSpatialObjectPoint_hxx
#define itkTubeSpatialObjectPoint_hxx

#include "itkTubeSpatialObjectPoint.h"

namespace itk
{
/** Constructor */
template <unsigned int TPointDimension>
TubeSpatialObjectPoint<TPointDimension>::TubeSpatialObjectPoint()
{
  m_TangentInObjectSpace.Fill(0);
  m_Normal1InObjectSpace.Fill(0);
  m_Normal2InObjectSpace.Fill(0);
  m_RadiusInObjectSpace = 0;
  m_Medialness = 0;
  m_Ridgeness = 0;
  m_Branchness = 0;
  m_Curvature = 0;
  m_Levelness = 0;
  m_Roundness = 0;
  m_Intensity = 0;
  m_Alpha1 = 0;
  m_Alpha2 = 0;
  m_Alpha3 = 0;
}

/** Copy Constructor */
template <unsigned int TPointDimension>
TubeSpatialObjectPoint<TPointDimension>::TubeSpatialObjectPoint(const TubeSpatialObjectPoint & other)
  : Superclass(other)
{
  this->SetRadiusInObjectSpace(other.GetRadiusInObjectSpace());
  this->SetTangentInObjectSpace(other.GetTangentInObjectSpace());
  this->SetNormal1InObjectSpace(other.GetNormal1InObjectSpace());
  this->SetNormal2InObjectSpace(other.GetNormal2InObjectSpace());

  this->SetRidgeness(other.GetRidgeness());
  this->SetMedialness(other.GetMedialness());
  this->SetBranchness(other.GetBranchness());
  this->SetCurvature(other.GetCurvature());
  this->SetLevelness(other.GetLevelness());
  this->SetRoundness(other.GetRoundness());
  this->SetIntensity(other.GetIntensity());
  this->SetAlpha1(other.GetAlpha1());
  this->SetAlpha2(other.GetAlpha2());
  this->SetAlpha3(other.GetAlpha3());
}

/** Get the radius */
template <unsigned int TPointDimension>
double
TubeSpatialObjectPoint<TPointDimension>::GetRadiusInWorldSpace() const
{
  if (this->m_SpatialObject == nullptr)
  {
    itkExceptionMacro(<< "The SpatialObject must be set prior to calling.");
  }

  CovariantVectorType cVect;
  cVect.Fill(m_RadiusInObjectSpace);
  cVect = Superclass::m_SpatialObject->GetObjectToWorldTransform()->TransformCovariantVector(cVect);
  double worldR = 0;
  for (unsigned int d = 0; d < TPointDimension; ++d)
  {
    worldR += cVect[d];
  }
  worldR /= TPointDimension;
  return worldR;
}

/** Get the radius */
template <unsigned int TPointDimension>
void
TubeSpatialObjectPoint<TPointDimension>::SetRadiusInWorldSpace(double newR)
{
  if (this->m_SpatialObject == nullptr)
  {
    itkExceptionMacro(<< "The SpatialObject must be set prior to calling.");
  }

  CovariantVectorType cVect;
  cVect.Fill(newR);
  cVect =
    Superclass::m_SpatialObject->GetObjectToWorldTransform()->GetInverseTransform()->TransformCovariantVector(cVect);
  m_RadiusInObjectSpace = 0;
  for (unsigned int d = 0; d < TPointDimension; ++d)
  {
    m_RadiusInObjectSpace += cVect[d];
  }
  m_RadiusInObjectSpace /= TPointDimension;
}

template <unsigned int TPointDimension>
const typename TubeSpatialObjectPoint<TPointDimension>::VectorType
TubeSpatialObjectPoint<TPointDimension>::GetTangentInWorldSpace() const
{
  if (this->m_SpatialObject == nullptr)
  {
    itkExceptionMacro(<< "The SpatialObject must be set prior to calling.");
  }

  return Superclass::m_SpatialObject->GetObjectToWorldTransform()->TransformVector(m_TangentInObjectSpace);
}

template <unsigned int TPointDimension>
void
TubeSpatialObjectPoint<TPointDimension>::SetTangentInWorldSpace(const VectorType & newT)
{
  if (this->m_SpatialObject == nullptr)
  {
    itkExceptionMacro(<< "The SpatialObject must be set prior to calling.");
  }

  m_TangentInObjectSpace =
    Superclass::m_SpatialObject->GetObjectToWorldTransform()->GetInverseTransform()->TransformVector(newT);
}

template <unsigned int TPointDimension>
const typename TubeSpatialObjectPoint<TPointDimension>::CovariantVectorType
TubeSpatialObjectPoint<TPointDimension>::GetNormal1InWorldSpace() const
{
  if (this->m_SpatialObject == nullptr)
  {
    itkExceptionMacro(<< "The SpatialObject must be set prior to calling.");
  }

  return Superclass::m_SpatialObject->GetObjectToWorldTransform()->TransformCovariantVector(m_Normal1InObjectSpace);
}

template <unsigned int TPointDimension>
void
TubeSpatialObjectPoint<TPointDimension>::SetNormal1InWorldSpace(const CovariantVectorType & newV1)
{
  if (this->m_SpatialObject == nullptr)
  {
    itkExceptionMacro(<< "The SpatialObject must be set prior to calling.");
  }

  m_Normal1InObjectSpace =
    Superclass::m_SpatialObject->GetObjectToWorldTransform()->GetInverseTransform()->TransformCovariantVector(newV1);
}

template <unsigned int TPointDimension>
const typename TubeSpatialObjectPoint<TPointDimension>::CovariantVectorType
TubeSpatialObjectPoint<TPointDimension>::GetNormal2InWorldSpace() const
{
  if (this->m_SpatialObject == nullptr)
  {
    itkExceptionMacro(<< "The SpatialObject must be set prior to calling.");
  }

  return Superclass::m_SpatialObject->GetObjectToWorldTransform()->TransformCovariantVector(m_Normal2InObjectSpace);
}

template <unsigned int TPointDimension>
void
TubeSpatialObjectPoint<TPointDimension>::SetNormal2InWorldSpace(const CovariantVectorType & newV2)
{
  if (this->m_SpatialObject == nullptr)
  {
    itkExceptionMacro(<< "The SpatialObject must be set prior to calling.");
  }

  m_Normal2InObjectSpace =
    Superclass::m_SpatialObject->GetObjectToWorldTransform()->GetInverseTransform()->TransformCovariantVector(newV2);
}

template <unsigned int TPointDimension>
void
TubeSpatialObjectPoint<TPointDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Radius In Object Space: " << m_RadiusInObjectSpace << std::endl;
  os << indent << "Tangent In Object Space: " << m_TangentInObjectSpace << std::endl;
  os << indent << "Normal1 In Object Space: " << m_Normal1InObjectSpace << std::endl;
  os << indent << "Normal2 In Object Space: " << m_Normal2InObjectSpace << std::endl;
  os << indent << "Medialness: " << m_Medialness << std::endl;
  os << indent << "Branchness: " << m_Branchness << std::endl;
  os << indent << "Ridgeness: " << m_Ridgeness << std::endl;
  os << indent << "Curvature: " << m_Curvature << std::endl;
  os << indent << "Levelness: " << m_Levelness << std::endl;
  os << indent << "Roundness: " << m_Roundness << std::endl;
  os << indent << "Intensity: " << m_Intensity << std::endl;
  os << indent << "Alpha1: " << m_Alpha1 << std::endl;
  os << indent << "Alpha2: " << m_Alpha2 << std::endl;
  os << indent << "Alpha3: " << m_Alpha3 << std::endl;
}

template <unsigned int TPointDimension>
typename TubeSpatialObjectPoint<TPointDimension>::Self &
TubeSpatialObjectPoint<TPointDimension>::operator=(const TubeSpatialObjectPoint & rhs)
{
  if (this != &rhs)
  {
    Superclass::operator=(rhs);

    this->SetRadiusInObjectSpace(rhs.GetRadiusInObjectSpace());
    this->SetTangentInObjectSpace(rhs.GetTangentInObjectSpace());
    this->SetNormal1InObjectSpace(rhs.GetNormal1InObjectSpace());
    this->SetNormal2InObjectSpace(rhs.GetNormal2InObjectSpace());

    this->SetRidgeness(rhs.GetRidgeness());
    this->SetMedialness(rhs.GetMedialness());
    this->SetBranchness(rhs.GetBranchness());
    this->SetCurvature(rhs.GetCurvature());
    this->SetLevelness(rhs.GetLevelness());
    this->SetRoundness(rhs.GetRoundness());
    this->SetIntensity(rhs.GetIntensity());
    this->SetAlpha1(rhs.GetAlpha1());
    this->SetAlpha2(rhs.GetAlpha2());
    this->SetAlpha3(rhs.GetAlpha3());
  }
  return *this;
}
} // end namespace itk

#endif
