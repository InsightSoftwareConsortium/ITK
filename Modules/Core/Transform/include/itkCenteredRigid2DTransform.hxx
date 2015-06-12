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
#ifndef itkCenteredRigid2DTransform_hxx
#define itkCenteredRigid2DTransform_hxx

#include "itkCenteredRigid2DTransform.h"

namespace itk
{

template<typename TParametersValueType>
CenteredRigid2DTransform<TParametersValueType>
::CenteredRigid2DTransform() :
  Superclass(ParametersDimension)
{
}


template<typename TParametersValueType>
CenteredRigid2DTransform<TParametersValueType>::CenteredRigid2DTransform(unsigned int spaceDimension,
                                                                unsigned int parametersDimension) :
  Superclass(spaceDimension, parametersDimension)
{
}


template<typename TParametersValueType>
void
CenteredRigid2DTransform<TParametersValueType>
::SetParameters(const ParametersType & parameters)
{
  itkDebugMacro(<< "Setting parameters " << parameters);
  // Parameters are ordered as:
  //
  // p[0]   = angle
  // p[1:2} = center of rotation coordinates
  // p[3:4} = translation components
  //
  //

  // Save parameters
  if( &parameters != &(this->m_Parameters) )
    {
    this->m_Parameters = parameters;
    }

  // Set the angle
  const TParametersValueType angle = parameters[0];
  this->SetVarAngle(angle);
  // Set the center
  InputPointType center;
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    center[i] = parameters[i + 1];
    }
  this->SetVarCenter(center);

  // Set the translation
  OutputVectorType translation;
  for( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    translation[j] = parameters[j + 1 + SpaceDimension];
    }
  this->SetVarTranslation(translation);

  // Update matrix and offset
  this->ComputeMatrix();
  this->ComputeOffset();

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();

  itkDebugMacro(<< "After setting parameters ");
}


template<typename TParametersValueType>
const typename CenteredRigid2DTransform<TParametersValueType>::ParametersType
& CenteredRigid2DTransform<TParametersValueType>
::GetParameters() const
{
  itkDebugMacro(<< "Getting parameters ");
  // Parameters are ordered as:
  //
  // p[0]   = angle
  // p[1:2} = center of rotation coordinates
  // p[3:4} = translation components
  //

  // Get the angle
  this->m_Parameters[0] = this->GetAngle();
  // Get the center
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    this->m_Parameters[i + 1] = this->GetCenter()[i];
    }
  // Get the translation
  for( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    this->m_Parameters[j + 1 + SpaceDimension] = this->GetTranslation()[j];
    }

  itkDebugMacro(<< "After getting parameters " << this->m_Parameters);

  return this->m_Parameters;
}


template<typename TParametersValueType>
void
CenteredRigid2DTransform<TParametersValueType>
::ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const
{
  const double ca = std::cos( this->GetAngle() );
  const double sa = std::sin( this->GetAngle() );

  jacobian.SetSize( 2, this->GetNumberOfLocalParameters() );
  jacobian.Fill(0.0);

  const double cx = this->GetCenter()[0];
  const double cy = this->GetCenter()[1];

  // derivatives with respect to the angle
  jacobian[0][0] = -sa * ( p[0] - cx ) - ca * ( p[1] - cy );
  jacobian[1][0] =  ca * ( p[0] - cx ) - sa * ( p[1] - cy );

  // compute derivatives with respect to the center part
  // first with respect to cx
  jacobian[0][1] = 1.0 - ca;
  jacobian[1][1] =     -sa;
  // then with respect to cy
  jacobian[0][2] =       sa;
  jacobian[1][2] = 1.0 - ca;

  // compute derivatives with respect to the translation part
  // first with respect to tx
  jacobian[0][3] = 1.0;
  jacobian[1][3] = 0.0;
  // first with respect to ty
  jacobian[0][4] = 0.0;
  jacobian[1][4] = 1.0;
}


template<typename TParametersValueType>
void
CenteredRigid2DTransform<TParametersValueType>
::SetFixedParameters( const FixedParametersType & itkNotUsed(parameters) )
{
  // no fixed parameters
}


template<typename TParametersValueType>
const typename CenteredRigid2DTransform<TParametersValueType>::FixedParametersType &
CenteredRigid2DTransform<TParametersValueType>
::GetFixedParameters() const
{
  // return dummy parameters
  return this->m_FixedParameters;
}


template<typename TParametersValueType>
void
CenteredRigid2DTransform<TParametersValueType>
::CloneInverseTo(Pointer & result) const
{
  result = New();
  this->GetInverse( result.GetPointer() );
}


template<typename TParametersValueType>
bool
CenteredRigid2DTransform<TParametersValueType>
::GetInverse(Self *inverse) const
{
  if( !inverse )
    {
    return false;
    }

  inverse->SetFixedParameters( this->GetFixedParameters() );
  inverse->SetCenter( this->GetCenter() );  // inverse have the same center
  inverse->SetAngle( -this->GetAngle() );
  inverse->SetTranslation( -( this->GetInverseMatrix()
                              * this->GetTranslation() ) );
  return true;
}


template<typename TParametersValueType>
typename CenteredRigid2DTransform<TParametersValueType>::InverseTransformBasePointer
CenteredRigid2DTransform<TParametersValueType>
::GetInverseTransform() const
{
  Pointer inv = New();

  return GetInverse(inv) ? inv.GetPointer() : ITK_NULLPTR;
}


template<typename TParametersValueType>
void
CenteredRigid2DTransform<TParametersValueType>
::CloneTo(Pointer & result) const
{
  result = New();
  result->SetCenter( this->GetCenter() );
  result->SetAngle( this->GetAngle() );
  result->SetTranslation( this->GetTranslation() );
}


template<typename TParametersValueType>
void
CenteredRigid2DTransform<TParametersValueType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
