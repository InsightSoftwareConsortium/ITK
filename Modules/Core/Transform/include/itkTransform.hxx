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
#ifndef __itkTransform_hxx
#define __itkTransform_hxx

#include "itkTransform.h"

namespace itk
{
/**
 * Constructor
 */
template <class TScalarType,
          unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
Transform<TScalarType, NInputDimensions, NOutputDimensions>
::Transform() :
  m_Parameters(1),
  m_FixedParameters(1)
#ifdef ITKV3_COMPATIBILITY
  , m_SharedLocalJacobian(NOutputDimensions, 1)
#endif
{
  m_DirectionChange.SetIdentity();

  itkWarningMacro(
    << "Using default transform constructor.  Should specify NOutputDims and NParameters as args to constructor.");
}

/**
 * Constructor
 */
template <class TScalarType,
          unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
Transform<TScalarType, NInputDimensions, NOutputDimensions>
::Transform(NumberOfParametersType numberOfParameters) :
  m_Parameters(numberOfParameters),
  m_FixedParameters(numberOfParameters)
#ifdef ITKV3_COMPATIBILITY
  , m_SharedLocalJacobian(NOutputDimensions, numberOfParameters)
#endif
{
  m_DirectionChange.SetIdentity();
}

/**
 * GenerateName
 */
template <class TScalarType,
          unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
std::string Transform<TScalarType, NInputDimensions, NOutputDimensions>
::GetTransformTypeAsString() const
{
  std::ostringstream n;

  n << GetNameOfClass();
  n << "_";
  n << this->GetTransformTypeAsString(static_cast<TScalarType *>(0) );
  n << "_" << this->GetInputSpaceDimension() << "_" << this->GetOutputSpaceDimension();
  return n.str();
}

#if 0
/**
 * SetDirectionChange
 */
template <class TScalarType,
          unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
void
Transform<TScalarType, NInputDimensions, NOutputDimensions>
::SetDirectionChange( const OutputDirectionMatrix fixedDir,
                      const InputDirectionMatrix movingDir )
{

  OutputDirectionMatrix movingDir2;

  movingDir2.SetIdentity();
  for( unsigned int i = 0; i < NOutputDimensions; i++ )
    {
    for( unsigned int j = 0; j < NOutputDimensions; j++ )
      {
      if( ( i < NInputDimensions ) && ( j < NInputDimensions ) )
        {
        movingDir2(i, j) = movingDir(i, j);
        }
      }
    }
  // TODO: this line can't work when NInputDimensions and NOutputDimensions are not the same
  // m_DirectionChange = movingDir2 * fixedDir;
  this->Modified();
}
#endif

/**
 * UpdateTransformParameters
 */
template <class TScalarType,
          unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
void
Transform<TScalarType, NInputDimensions, NOutputDimensions>
::UpdateTransformParameters( DerivativeType & update,
                             TScalarType factor )
{
  NumberOfParametersType numberOfParameters = this->GetNumberOfParameters();

  if( update.Size() != numberOfParameters )
    {
    itkExceptionMacro("Parameter update size, " << update.Size() << ", must "
                      " be same as transform parameter size, "
                                                << numberOfParameters << std::endl);
    }

  /* Make sure m_Parameters is updated to reflect the current values in
   * the transform's other parameter-related variables. This is effective for
   * managing the parallel variables used for storing parameter data,
   * but inefficient. However for small global transforms, shouldn't be
   * too bad. Dense-field transform will want to make sure m_Parameters
   * is always updated whenever the transform is changed, so GetParameters
   * can be skipped in their implementations of UpdateTransformParameters. */
  this->GetParameters();

  if( factor == 1.0 )
    {
    for( NumberOfParametersType k = 0; k < numberOfParameters; k++ )
      {
      this->m_Parameters[k] += update[k];
      }
    }
  else
    {
    for( NumberOfParametersType k = 0; k < numberOfParameters; k++ )
      {
      this->m_Parameters[k] += update[k] * factor;
      }
    }

  /* Call SetParameters with the updated parameters.
   * SetParameters in most transforms is used to assign the input params
   * to member variables, possibly with some processing. The member variables
   * are then used in TransformPoint.
   * In the case of dense-field transforms that are updated in blocks from
   * a threaded implementation, SetParameters doesn't do this, and is
   * optimized to not copy the input parameters when == m_Parameters.
   */
  this->SetParameters( this->m_Parameters );

  /* Call Modified, following behavior of other transform when their
   * parameters change, e.g. MatrixOffsetTransformBase */
  this->Modified();
}

} // end namespace itk

#endif
