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
#ifndef __itkMultiTransform_hxx
#define __itkMultiTransform_hxx

#include "itkMultiTransform.h"

namespace itk
{

/**
 * Constructor
 */
template
<typename TScalar, unsigned int NDimensions, unsigned int NSubDimensions>
MultiTransform<TScalar, NDimensions, NSubDimensions>::MultiTransform() : Superclass( 0 )
{
  this->m_NumberOfLocalParameters = NumericTraits< NumberOfParametersType >::Zero;
  this->m_LocalParametersUpdateTime = NumericTraits< ModifiedTimeType >::Zero;
  this->m_TransformQueue.clear();
}

/**
 * Destructor
 */
template
<typename TScalar, unsigned int NDimensions, unsigned int NSubDimensions>
MultiTransform<TScalar, NDimensions, NSubDimensions>::
~MultiTransform()
{
}

/**
 * Get transform category
 */
template
<typename TScalar, unsigned int NDimensions, unsigned int NSubDimensions>
typename MultiTransform<TScalar, NDimensions, NSubDimensions>::TransformCategoryType
MultiTransform<TScalar, NDimensions, NSubDimensions>
::GetTransformCategory() const
{
  // If all sub-transforms are the same, return that type. Otherwise
  // return Unknown.
  TransformCategoryType result = Self::UnknownTransformCategory;

  for( SizeValueType tind = 0; tind < this->GetNumberOfTransforms(); tind++ )
    {
    TransformCategoryType type = this->GetNthTransform( tind ).GetPointer()->GetTransformCategory();
    if( tind == 0 )
      {
      result = type;
      }
    else
      {
      if( type != result )
        {
        result = Self::UnknownTransformCategory;
        break;
        }
      }
    }

  return result;
}

/**
 * Are all the transforms linear?
 */
template
<typename TScalar, unsigned int NDimensions, unsigned int NSubDimensions>
bool
MultiTransform<TScalar, NDimensions, NSubDimensions>
::IsLinear() const
{
  // If all sub-transforms are linear, return true.
  for( SizeValueType tind = 0; tind < this->GetNumberOfTransforms(); tind++ )
    {
    if( ! this->GetNthTransform( tind ).GetPointer()->IsLinear() )
      {
      return false;
      }
    }
  return true;
}

template
<typename TScalar, unsigned int NDimensions, unsigned int NSubDimensions>
const typename MultiTransform<TScalar, NDimensions, NSubDimensions>::ParametersType
& MultiTransform<TScalar, NDimensions, NSubDimensions>
::GetParameters() const
{
  /* Resize destructively. But if it's already this size, nothing is done so
   * it's efficient. */
  this->m_Parameters.SetSize( this->GetNumberOfParameters() );
  NumberOfParametersType offset = NumericTraits< NumberOfParametersType >::Zero;
  TransformQueueType transforms = this->GetTransformQueue();
  typename TransformQueueType::const_iterator it;
  it = transforms.begin();

  do
    {
    const ParametersType & subParameters = (*it)->GetParameters();
    /* use vnl_vector data_block() to get data ptr */
    std::copy(subParameters.data_block(),
              subParameters.data_block()+subParameters.Size(),
              &(this->m_Parameters.data_block() )[offset]);
    offset += subParameters.Size();
    ++it;
    }
  while( it != transforms.end() );

  return this->m_Parameters;
}

template
<typename TScalar, unsigned int NDimensions, unsigned int NSubDimensions>
void
MultiTransform<TScalar, NDimensions, NSubDimensions>
::SetParameters(const ParametersType & inputParameters)
{
  /* We do not copy inputParameters into m_Parameters,
   * to avoid unnecessary copying. */

  /* We assume input params are concatenation of the parameters of the
     sub-transforms, in order of the queue from begin() to end(). */

  /* Verify proper input size. */
  if( inputParameters.Size() != this->GetNumberOfParameters() )
    {
    itkExceptionMacro(<< "Input parameter list size is not expected size. " << inputParameters.Size() << " instead of " << this->GetNumberOfParameters() << ".");
    }

  TransformQueueType transforms = this->GetTransformQueue();
  NumberOfParametersType offset = NumericTraits< NumberOfParametersType >::Zero;
  typename TransformQueueType::const_iterator it;
  it = transforms.begin();

  do
    {
    /* If inputParams is same object as m_Parameters, we just pass
     * each sub-transforms own m_Parameters in. This is needed to
     * avoid unnecessary copying of parameters in the sub-transforms,
     * while still allowing SetParameters to do any oprations on the
     * parameters to update member variable states. A hack. */
    ParametersType & subParameters = const_cast<ParametersType &>( (*it)->GetParameters() );
    if( &inputParameters != &this->m_Parameters )
      {
      /* New parameter data, so copy it in */
      /* Use vnl_vector data_block() to get data ptr */
      std::copy(&(inputParameters.data_block() )[offset],
                &(inputParameters.data_block() )[offset]+subParameters.Size(),
                subParameters.data_block());
      offset += subParameters.Size();
      }
      /* Call SetParameters explicitly to include anything extra it does */
    (*it)->SetParameters(subParameters);
    ++it;
    }
  while( it != transforms.end() );
}

template
<typename TScalar, unsigned int NDimensions, unsigned int NSubDimensions>
const typename MultiTransform<TScalar, NDimensions, NSubDimensions>::ParametersType
& MultiTransform<TScalar, NDimensions, NSubDimensions>
::GetFixedParameters(void) const
{
  /* Resize destructively. But if it's already this size, nothing is done so
   * it's efficient. */
  this->m_FixedParameters.SetSize( this->GetNumberOfFixedParameters() );

  NumberOfParametersType offset = NumericTraits< NumberOfParametersType >::Zero;
  typename TransformQueueType::const_iterator it;
  TransformQueueType transforms = this->GetTransformQueue();
  it = transforms.begin();

  do
    {
    const ParametersType & subFixedParameters = (*it)->GetFixedParameters();
    /* use vnl_vector data_block() to get data ptr */
    std::copy(subFixedParameters.data_block(),
              subFixedParameters.data_block()+subFixedParameters.Size(),
              &(this->m_FixedParameters.data_block() )[offset]);
    offset += subFixedParameters.Size();
    ++it;
    }
  while( it != transforms.end() );

  return this->m_FixedParameters;
}

template
<typename TScalar, unsigned int NDimensions, unsigned int NSubDimensions>
void
MultiTransform<TScalar, NDimensions, NSubDimensions>
::SetFixedParameters(const ParametersType & inputParameters)
{
  /* Verify proper input size. */
  if( inputParameters.Size() != this->GetNumberOfFixedParameters() )
    {
    itkExceptionMacro(<< "Input parameter list size is not expected size. "
                      << inputParameters.Size() << " instead of "
                      << this->GetNumberOfFixedParameters() << ".");
    }

  /* Assumes input params are concatenation of the parameters of the
   * sub transforms. */
  TransformQueueType transforms = this->GetTransformQueue();
  NumberOfParametersType offset = NumericTraits< NumberOfParametersType >::Zero;
  typename TransformQueueType::const_iterator it;

  /* Why is this done? Seems unnecessary. */
  this->m_FixedParameters = inputParameters;

  it = transforms.begin();
  do
    {
    ParametersType & subFixedParameters = const_cast<ParametersType &>( (*it)->GetFixedParameters() );
    /* Use vnl_vector data_block() to get data ptr */
    std::copy(&(this->m_FixedParameters.data_block() )[offset],
              &(this->m_FixedParameters.data_block() )[offset]+subFixedParameters.Size(),
              subFixedParameters.data_block());
    /* Call SetParameters explicitly to include anything extra it does */
    (*it)->SetFixedParameters(subFixedParameters);
    offset += subFixedParameters.Size();
    ++it;
    }
  while( it != transforms.end() );
}

template<typename TScalar, unsigned int NDimensions, unsigned int NSubDimensions>
typename MultiTransform<TScalar, NDimensions, NSubDimensions>::NumberOfParametersType
MultiTransform<TScalar, NDimensions, NSubDimensions>
::GetNumberOfParameters(void) const
{
  /* Returns to total number of params in all transforms currently
   * set to be used for optimized.
   * NOTE: Ideally we'd optimize this to store the result and
   * only re-calc when the composite object has been modified.
   * However, it seems that number of parameter might change for dense
   * field transfroms (deformation, bspline) during processing and
   * we wouldn't know that in this class, so this is safest. */
  NumberOfParametersType result = NumericTraits< NumberOfParametersType >::Zero;

  const TransformType * transform;

  for( SizeValueType tind = 0; tind < this->GetNumberOfTransforms(); tind++ )
    {
    /* Use raw pointer for efficiency */
    transform = this->GetNthTransform( tind ).GetPointer();
    result += transform->GetNumberOfParameters();
    }
  return result;
}

template<typename TScalar, unsigned int NDimensions, unsigned int NSubDimensions>
typename MultiTransform<TScalar, NDimensions, NSubDimensions>::NumberOfParametersType
MultiTransform<TScalar, NDimensions, NSubDimensions>
::GetNumberOfLocalParameters(void) const
{
  if ( this->GetMTime() == this->m_LocalParametersUpdateTime )
   {
   return this->m_NumberOfLocalParameters;
   }

  this->m_LocalParametersUpdateTime = this->GetMTime();

  /* Note that unlike in GetNumberOfParameters(), we don't expect the
   * number of local parameters to possibly change, so we can cache
   * the value. */
  NumberOfParametersType result = NumericTraits< NumberOfParametersType >::Zero;
  const TransformType * transform;

  for( SizeValueType tind = 0; tind < this->GetNumberOfTransforms(); tind++ )
    {
    transform = this->GetNthTransform( tind ).GetPointer();
    result += transform->GetNumberOfLocalParameters();
    }
  this->m_NumberOfLocalParameters = result;
  return result;
}

template
<typename TScalar, unsigned int NDimensions, unsigned int NSubDimensions>
typename MultiTransform<TScalar, NDimensions, NSubDimensions>::NumberOfParametersType
MultiTransform<TScalar, NDimensions, NSubDimensions>
::GetNumberOfFixedParameters(void) const
{
  NumberOfParametersType result = NumericTraits< NumberOfParametersType >::Zero;
  const TransformType * transform;

  for( SizeValueType tind = 0; tind < this->GetNumberOfTransforms(); tind++ )
    {
    transform = this->GetNthTransform( tind ).GetPointer();
    result += transform->GetFixedParameters().Size();
    }
  return result;
}

template
<typename TScalar, unsigned int NDimensions, unsigned int NSubDimensions>
void
MultiTransform<TScalar, NDimensions, NSubDimensions>
::UpdateTransformParameters(  const DerivativeType & update, ScalarType  factor )
{
  /* Update parameters within the sub-transforms. */
  /* NOTE: We might want to thread this over each sub-transform, if we
   * find we're working with longer lists of sub-transforms that do
   * not implement any threading of their own for UpdateTransformParameters.
   * Since the plan is for a UpdateTransformParameters functor that is
   * user-assignable, we would need a method in the
   * functor to return whether or not it does therading. If all sub-transforms
   * return that they don't thread, we could do each sub-transform in its
   * own thread from here. */
  NumberOfParametersType numberOfParameters = this->GetNumberOfParameters();

  if( update.Size() != numberOfParameters )
    {
    itkExceptionMacro("Parameter update size, " << update.Size() << ", must "
                      " be same as transform parameter size, " << numberOfParameters << std::endl);
    }

  NumberOfParametersType offset = NumericTraits< NumberOfParametersType >::Zero;
  TransformType * subtransform;

  for( SizeValueType tind = 0; tind < this->GetNumberOfTransforms(); tind++ )
    {
    subtransform = const_cast<TransformType*>( this->GetNthTransform( tind ).GetPointer() );
    /* The input values are in a monolithic block, so we have to point
     * to the subregion corresponding to the individual subtransform.
     * This simply creates an Array object with data pointer, no
     * memory is allocated or copied. */
    DerivativeType subUpdate( &( (update.data_block() )[offset]), subtransform->GetNumberOfParameters(), false );
    /* This call will also call SetParameters, so don't need to call it
     * expliclity here. */
    subtransform->UpdateTransformParameters( subUpdate, factor );
    offset += subtransform->GetNumberOfParameters();
    }
  this->Modified();
}

/**
 * return an inverse transformation
 */
template
<typename TScalar, unsigned int NDimensions, unsigned int NSubDimensions>
bool
MultiTransform<TScalar, NDimensions, NSubDimensions>
::GetInverse( Self *inverse ) const
{
  typename TransformQueueType::const_iterator it;

  inverse->ClearTransformQueue();
  for( it = this->m_TransformQueue.begin(); it != this->m_TransformQueue.end(); ++it )
    {
    TransformTypePointer inverseTransform = dynamic_cast<TransformType *>( ( ( *it )->GetInverseTransform() ).GetPointer() );
    if( !inverseTransform )
      {
      inverse->ClearTransformQueue();
      return false;
      }
    else
      {
      /* Add to end of queue to preserve transform order */
      inverse->PushBackTransform( inverseTransform );
      }
    }

  return true;
}

template
<typename TScalar, unsigned int NDimensions, unsigned int NSubDimensions>
void
MultiTransform<TScalar, NDimensions, NSubDimensions>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  if( this->m_TransformQueue.empty() )
    {
    os << indent << "Transform queue is empty." << std::endl;
    return;
    }

  os << indent <<  "Transforms in queue, from begin to end:" << std::endl;
  typename TransformQueueType::const_iterator cit;
  for( cit = this->m_TransformQueue.begin(); cit != this->m_TransformQueue.end(); ++cit )
    {
    os << indent << ">>>>>>>>>" << std::endl;
    (*cit)->Print( os, indent );
    }

  os << indent <<  "End of MultiTransform." << std::endl << "<<<<<<<<<<" << std::endl;
}

} // namespace itk

#endif
