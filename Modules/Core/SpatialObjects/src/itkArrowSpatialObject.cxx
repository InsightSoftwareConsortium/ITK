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
    m_Length = vcl_sqrt(m_Length);
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
      anglez = vnl_math::pi / 2;
      }
    else if ( m_Direction[1] < 0.0 )
      {
      anglez = -vnl_math::pi / 2;
      }
    //NOTE: else if m_Direction[1] == 0, anglez = 0;
    }
  else
    {
    if ( m_Direction[0] < 0.0 )
      {
      anglez = vnl_math::pi + vcl_atan(m_Direction[1] / m_Direction[0]);
      }
    else
      {
      anglez = vcl_atan(m_Direction[1] / m_Direction[0]);
      }
    }
  const double angley = -asin(m_Direction[2]);
  typedef itk::Euler3DTransform< double > EulerTransformType;
  EulerTransformType::Pointer euler = EulerTransformType::New();

  euler->SetRotation(0, angley, anglez);
  this->GetObjectToParentTransform()->SetMatrix( euler->GetRotationMatrix() );
  this->Modified();
}

}
