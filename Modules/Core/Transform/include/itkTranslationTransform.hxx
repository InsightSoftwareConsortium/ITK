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
#ifndef __itkTranslationTransform_hxx
#define __itkTranslationTransform_hxx

#include "itkTranslationTransform.h"
#include "itkMath.h"

namespace itk
{
// Constructor with default arguments
template <class TScalarType, unsigned int NDimensions>
TranslationTransform<TScalarType, NDimensions>::TranslationTransform() : Superclass(ParametersDimension),
  m_IdentityJacobian(NDimensions, NDimensions)
{
  m_Offset.Fill(0);

  // The Jacobian of this transform is constant.
  // Therefore the m_IdentityJacobian variable can be
  // initialized here and be shared among all the threads.
  this->m_IdentityJacobian.Fill(0.0);
  for( unsigned int i = 0; i < NDimensions; i++ )
    {
    this->m_IdentityJacobian(i, i) = 1.0;
    }
}

// Destructor
template <class TScalarType, unsigned int NDimensions>
TranslationTransform<TScalarType, NDimensions>::
~TranslationTransform()
{
}

// Set the parameters
template <class TScalarType, unsigned int NDimensions>
void
TranslationTransform<TScalarType, NDimensions>
::SetParameters(const ParametersType & parameters)
{
  // Save parameters. Needed for proper operation of TransformUpdateParameters.
  if( &parameters != &(this->m_Parameters) )
    {
    this->m_Parameters = parameters;
    }

  typedef typename ParametersType::ValueType ParameterValueType;
  bool modified = false;
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    if( m_Offset[i] != parameters[i] )
      {
      m_Offset[i] = parameters[i];
      modified = true;
      }
    }
  if( modified )
    {
    this->Modified();
    }
}

// Get the parameters
template <class TScalarType, unsigned int NDimensions>
const typename TranslationTransform<TScalarType, NDimensions>::ParametersType
& TranslationTransform<TScalarType, NDimensions>
::GetParameters(void) const
  {
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    this->m_Parameters[i] = this->m_Offset[i];
    }
  return this->m_Parameters;
  }

// Print self
template <class TScalarType, unsigned int NDimensions>
void
TranslationTransform<TScalarType, NDimensions>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Offset: " << m_Offset << std::endl;
}

// Compose with another affine transformation
template <class TScalarType, unsigned int NDimensions>
void
TranslationTransform<TScalarType, NDimensions>::Compose(const Self *other, bool)
{
  this->Translate(other->m_Offset);
}

// Compose with a translation
template <class TScalarType, unsigned int NDimensions>
void
TranslationTransform<TScalarType, NDimensions>::Translate(const OutputVectorType & offset, bool)
{
  ParametersType newOffset(SpaceDimension);

  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    newOffset[i] = m_Offset[i] + offset[i];
    }
  this->SetParameters(newOffset);
}

// Transform a point
template <class TScalarType, unsigned int NDimensions>
typename TranslationTransform<TScalarType, NDimensions>::OutputPointType
TranslationTransform<TScalarType, NDimensions>::TransformPoint(const InputPointType & point) const
{
  return point + m_Offset;
}

// Transform a vector
template <class TScalarType, unsigned int NDimensions>
typename TranslationTransform<TScalarType, NDimensions>::OutputVectorType
TranslationTransform<TScalarType, NDimensions>::TransformVector(const InputVectorType & vect) const
{
  return vect;
}

// Transform a vnl_vector_fixed
template <class TScalarType, unsigned int NDimensions>
typename TranslationTransform<TScalarType, NDimensions>::OutputVnlVectorType
TranslationTransform<TScalarType, NDimensions>::TransformVector(const InputVnlVectorType & vect) const
{
  return vect;
}

// Transform a CovariantVector
template <class TScalarType, unsigned int NDimensions>
typename TranslationTransform<TScalarType, NDimensions>::OutputCovariantVectorType
TranslationTransform<TScalarType, NDimensions>::TransformCovariantVector(const InputCovariantVectorType & vect) const
{
  return vect;
}

// return an inverse transformation
template <class TScalarType, unsigned int NDimensions>
bool
TranslationTransform<TScalarType, NDimensions>::GetInverse(Self *inverse) const
{
  if( !inverse )
    {
    return false;
    }

  inverse->m_Offset   = -m_Offset;
  return true;
}

// Return an inverse of this transform
template <class TScalarType, unsigned int NDimensions>
typename TranslationTransform<TScalarType, NDimensions>::InverseTransformBasePointer
TranslationTransform<TScalarType, NDimensions>
::GetInverseTransform() const
{
  Pointer inv = New();

  return GetInverse(inv) ? inv.GetPointer() : NULL;
}

// Compute the Jacobian in one position
template <class TScalarType, unsigned int NDimensions>
void
TranslationTransform<TScalarType, NDimensions>::ComputeJacobianWithRespectToParameters(
  const InputPointType &,
  JacobianType & jacobian) const
{
  // the Jacobian is constant for this transform, and it has already been
  // initialized in the constructor, so we just need to return it here.
  jacobian = this->m_IdentityJacobian;
}

// Compute jacobian with respect to position
template <class TScalarType, unsigned int NDimensions>
void
TranslationTransform<TScalarType, NDimensions>
::ComputeJacobianWithRespectToPosition(const InputPointType &,
                                       JacobianType & jac) const
{
  jac.SetSize( NDimensions, NDimensions );
  jac.Fill(0.0);
  for( unsigned int dim = 0; dim < NDimensions; dim++ )
    {
    jac[dim][dim] = 1.0;
    }
}

// Set the parameters for an Identity transform of this class
template <class TScalarType, unsigned int NDimensions>
void
TranslationTransform<TScalarType, NDimensions>::SetIdentity()
{
  m_Offset.Fill(0.0);
}

} // namespace

#endif
