/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#include "itkArrowSpatialObject.h"

namespace itk
{
// This is the 3D implementation of the ArrowSpatialObject.

/** Update the local transform from the position and the direction */
// Parial specialization for TDimension = 3
template< >
void
ArrowSpatialObject< 3 >
::UpdateTransform()
{
  VectorType offset;

  for ( unsigned int i = 0; i < 3; i++ )
    {
    offset[i] = m_Position[i];
    }
  this->GetObjectToParentTransform()->SetOffset(offset);

  // If the given direction is not normalized we set the length of the vector
  // as the length of the arrow
  m_Length = m_Direction.GetSquaredNorm();

  if ( m_Length != 0.0 )
    {
    m_Length = std::sqrt(m_Length);
    }
  else
    {
    this->Modified();
    return;
    }

  m_Direction.Normalize();

  double anglez = 0;
  if ( m_Direction[0] == 0.0 )
    {
    if ( m_Direction[1] > 0.0 )
      {
      anglez = itk::Math::pi / 2;
      }
    else if ( m_Direction[1] < 0.0 )
      {
      anglez = -itk::Math::pi / 2;
      }
    //NOTE: else if m_Direction[1] == 0, anglez = 0;
    }
  else
    {
    if ( m_Direction[0] < 0.0 )
      {
      anglez = itk::Math::pi + std::atan(m_Direction[1] / m_Direction[0]);
      }
    else
      {
      anglez = std::atan(m_Direction[1] / m_Direction[0]);
      }
    }
  const double angley = -asin(m_Direction[2]);
  typedef itk::Euler3DTransform< double > EulerTransformType;
  EulerTransformType::Pointer euler = EulerTransformType::New();

  euler->SetRotation(0, angley, anglez);
  this->GetObjectToParentTransform()->SetMatrix( euler->GetMatrix() );
  this->Modified();
}

}
