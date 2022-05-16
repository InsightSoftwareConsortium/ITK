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
#ifndef itkSurfaceSpatialObjectPoint_hxx
#define itkSurfaceSpatialObjectPoint_hxx


namespace itk
{
/** Constructor */
template <unsigned int TPointDimension>
SurfaceSpatialObjectPoint<TPointDimension>::SurfaceSpatialObjectPoint()
{
  m_NormalInObjectSpace.Fill(0);
}

/** Copy Constructor */
template <unsigned int TPointDimension>
SurfaceSpatialObjectPoint<TPointDimension>::SurfaceSpatialObjectPoint(const SurfaceSpatialObjectPoint & other)
  : Superclass(other)
{
  this->m_NormalInObjectSpace = other.m_NormalInObjectSpace;
}

/** Set the normal : N-D case */
template <unsigned int TPointDimension>
void
SurfaceSpatialObjectPoint<TPointDimension>::SetNormalInObjectSpace(const CovariantVectorType & normal)
{
  m_NormalInObjectSpace = normal;
}

/** Set the normal : N-D case */
template <unsigned int TPointDimension>
void
SurfaceSpatialObjectPoint<TPointDimension>::SetNormalInWorldSpace(const CovariantVectorType & normal)
{
  if (this->m_SpatialObject == nullptr)
  {
    itkExceptionMacro(<< "The SpatialObject must be set prior to calling.");
  }

  m_NormalInObjectSpace =
    Superclass::m_SpatialObject->GetObjectToWorldTransform()->GetInverseTransform()->TransformCovariantVector(normal);
}

/** Get the normal at one point */
template <unsigned int TPointDimension>
auto
SurfaceSpatialObjectPoint<TPointDimension>::GetNormalInObjectSpace() const -> const CovariantVectorType &
{
  return m_NormalInObjectSpace;
}

/** Get the normal at one point */
template <unsigned int TPointDimension>
auto
SurfaceSpatialObjectPoint<TPointDimension>::GetNormalInWorldSpace() const -> const CovariantVectorType
{
  if (this->m_SpatialObject == nullptr)
  {
    itkExceptionMacro(<< "The SpatialObject must be set prior to calling.");
  }

  return Superclass::m_SpatialObject->GetObjectToWorldTransform()->TransformCovariantVector(m_NormalInObjectSpace);
}

/** Print the object */
template <unsigned int TPointDimension>
void
SurfaceSpatialObjectPoint<TPointDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "SurfaceSpatialObjectPoint(" << this << ")" << std::endl;
  os << indent << "Normal definition: ";
  os << indent << m_NormalInObjectSpace << std::endl;
}

/** Copy a surface point to another */
template <unsigned int TPointDimension>
auto
SurfaceSpatialObjectPoint<TPointDimension>::operator=(const SurfaceSpatialObjectPoint & rhs) -> Self &
{
  if (this != &rhs)
  {
    Superclass::operator=(rhs);
    this->SetNormalInObjectSpace(rhs.GetNormalInObjectSpace());
  }
  return *this;
}
} // end namespace itk

#endif
