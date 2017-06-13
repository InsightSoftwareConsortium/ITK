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
#ifndef itkMultiTransform_hxx
#define itkMultiTransform_hxx

#include "itkMultiTransform.h"

namespace itk
{


template<typename TParametersValueType, unsigned int NDimensions, unsigned int NSubDimensions>
MultiTransform<TParametersValueType, NDimensions, NSubDimensions>::MultiTransform() : Superclass( 0 )
{
  this->m_NumberOfLocalParameters = NumericTraits< NumberOfParametersType >::ZeroValue();
  this->m_LocalParametersUpdateTime = NumericTraits< ModifiedTimeType >::ZeroValue();
  this->m_TransformQueue.clear();
}


template<typename TParametersValueType, unsigned int NDimensions, unsigned int NSubDimensions>
MultiTransform<TParametersValueType, NDimensions, NSubDimensions>::
~MultiTransform()
{
}


template<typename TParametersValueType, unsigned int NDimensions, unsigned int NSubDimensions>
typename MultiTransform<TParametersValueType, NDimensions, NSubDimensions>::TransformCategoryType
MultiTransform<TParametersValueType, NDimensions, NSubDimensions>
::GetTransformCategory() const
{
  // If all sub-transforms are the same, return that type. Otherwise
  // return Unknown.
  TransformCategoryType result = Self::UnknownTransformCategory;

  for( SizeValueType tind = 0; tind < this->GetNumberOfTransforms(); tind++ )
    {
    const TransformCategoryType type = this->GetNthTransformConstPointer( tind )->GetTransformCategory();
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


template<typename TParametersValueType, unsigned int NDimensions, unsigned int NSubDimensions>
bool
MultiTransform<TParametersValueType, NDimensions, NSubDimensions>
::IsLinear() const
{
  // If all sub-transforms are linear, return true.
  for( SizeValueType tind = 0; tind < this->GetNumberOfTransforms(); tind++ )
    {
    if( ! this->GetNthTransformConstPointer( tind )->IsLinear() )
      {
      return false;
      }
    }
  return true;
}


template<typename TParametersValueType, unsigned int NDimensions, unsigned int NSubDimensions>
const typename MultiTransform<TParametersValueType, NDimensions, NSubDimensions>::ParametersType &
MultiTransform<TParametersValueType, NDimensions, NSubDimensions>
::GetParameters() const
{
  /* Resize destructively. But if it's already this size, nothing is done so
   * it's efficient. */
  this->m_Parameters.SetSize( this->GetNumberOfParameters() );
  NumberOfParametersType offset = NumericTraits< NumberOfParametersType >::ZeroValue();
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


template<typename TParametersValueType, unsigned int NDimensions, unsigned int NSubDimensions>
void
MultiTransform<TParametersValueType, NDimensions, NSubDimensions>
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
  NumberOfParametersType offset = NumericTraits< NumberOfParametersType >::ZeroValue();
  typename TransformQueueType::iterator it = transforms.begin();

  do
    {
    /* If inputParams is same object as m_Parameters, we just pass
     * each sub-transforms own m_Parameters in. This is needed to
     * avoid unnecessary copying of parameters in the sub-transforms,
     * while still allowing SetParameters to do any oprations on the
     * parameters to update member variable states. A hack. */
    if( &inputParameters == &this->m_Parameters )
      {
      (*it)->SetParameters( (*it)->GetParameters() );
      }
    else
      {
      const size_t parameterSize = (*it)->GetParameters().Size();
      (*it)->CopyInParameters( &( inputParameters.data_block() )[offset],
                              &( inputParameters.data_block() )[offset] + parameterSize );
      offset += static_cast<NumberOfParametersType>(parameterSize);
      }
    ++it;
    }
  while( it != transforms.end() );
}


template<typename TParametersValueType, unsigned int NDimensions, unsigned int NSubDimensions>
const typename MultiTransform<TParametersValueType, NDimensions, NSubDimensions>::FixedParametersType &
MultiTransform<TParametersValueType, NDimensions, NSubDimensions>
::GetFixedParameters() const
{
  /* Resize destructively. But if it's already this size, nothing is done so
   * it's efficient. */
  this->m_FixedParameters.SetSize( this->GetNumberOfFixedParameters() );

  NumberOfParametersType offset = NumericTraits< NumberOfParametersType >::ZeroValue();
  typename TransformQueueType::const_iterator it;
  TransformQueueType transforms = this->GetTransformQueue();
  it = transforms.begin();

  do
    {
    const FixedParametersType & subFixedParameters = (*it)->GetFixedParameters();
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


template<typename TParametersValueType, unsigned int NDimensions, unsigned int NSubDimensions>
void
MultiTransform<TParametersValueType, NDimensions, NSubDimensions>
::SetFixedParameters(const FixedParametersType & inputParameters)
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
  NumberOfParametersType offset = NumericTraits< NumberOfParametersType >::ZeroValue();

  /* Why is this done? Seems unnecessary. */
  this->m_FixedParameters = inputParameters;

  typename TransformQueueType::iterator it = transforms.begin();
  do
    {
    const size_t fixedParameterSize = (*it)->GetFixedParameters().Size();
    (*it)->CopyInFixedParameters( &( this->m_FixedParameters.data_block() )[offset],
              &( this->m_FixedParameters.data_block() )[offset] + fixedParameterSize );
    offset += static_cast<NumberOfParametersType>(fixedParameterSize);
    ++it;
    }
  while( it != transforms.end() );
}


template<typename TParametersValueType, unsigned int NDimensions, unsigned int NSubDimensions>
typename MultiTransform<TParametersValueType, NDimensions, NSubDimensions>::NumberOfParametersType
MultiTransform<TParametersValueType, NDimensions, NSubDimensions>
::GetNumberOfParameters() const
{
  /* Returns to total number of params in all transforms currently
   * set to be used for optimized.
   * NOTE: Ideally we'd optimize this to store the result and
   * only re-calc when the composite object has been modified.
   * However, it seems that number of parameter might change for dense
   * field transforms (deformation, bspline) during processing and
   * we wouldn't know that in this class, so this is safest. */
  NumberOfParametersType result = NumericTraits< NumberOfParametersType >::ZeroValue();


  for( SizeValueType tind = 0; tind < this->GetNumberOfTransforms(); tind++ )
    {
    /* Use raw pointer for efficiency */
    const TransformType * transform = this->GetNthTransformConstPointer( tind );
    result += transform->GetNumberOfParameters();
    }
  return result;
}


template<typename TParametersValueType, unsigned int NDimensions, unsigned int NSubDimensions>
typename MultiTransform<TParametersValueType, NDimensions, NSubDimensions>::NumberOfParametersType
MultiTransform<TParametersValueType, NDimensions, NSubDimensions>
::GetNumberOfLocalParameters() const
{
  if ( this->GetMTime() == this->m_LocalParametersUpdateTime )
   {
   return this->m_NumberOfLocalParameters;
   }

  this->m_LocalParametersUpdateTime = this->GetMTime();

  /* Note that unlike in GetNumberOfParameters(), we don't expect the
   * number of local parameters to possibly change, so we can cache
   * the value. */
  NumberOfParametersType result = NumericTraits< NumberOfParametersType >::ZeroValue();

  for( SizeValueType tind = 0; tind < this->GetNumberOfTransforms(); tind++ )
    {
    const TransformType * transform = this->GetNthTransformConstPointer( tind );
    result += transform->GetNumberOfLocalParameters();
    }
  this->m_NumberOfLocalParameters = result;
  return result;
}


template<typename TParametersValueType, unsigned int NDimensions, unsigned int NSubDimensions>
typename MultiTransform<TParametersValueType, NDimensions, NSubDimensions>::NumberOfParametersType
MultiTransform<TParametersValueType, NDimensions, NSubDimensions>
::GetNumberOfFixedParameters() const
{
  NumberOfParametersType result = NumericTraits< NumberOfParametersType >::ZeroValue();

  for( SizeValueType tind = 0; tind < this->GetNumberOfTransforms(); tind++ )
    {
    const TransformType * transform = this->GetNthTransformConstPointer( tind );
    result += transform->GetFixedParameters().Size();
    }
  return result;
}


template<typename TParametersValueType, unsigned int NDimensions, unsigned int NSubDimensions>
void
MultiTransform<TParametersValueType, NDimensions, NSubDimensions>
::UpdateTransformParameters( const DerivativeType & update, ScalarType factor )
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

  NumberOfParametersType offset = NumericTraits< NumberOfParametersType >::ZeroValue();

  for( SizeValueType tind = 0; tind < this->GetNumberOfTransforms(); tind++ )
    {
    // HACK:  The following line looks wrong.  We should not need to const_cast
    TransformType * subtransform = this->GetNthTransformModifiablePointer( tind );
    /* The input values are in a monolithic block, so we have to point
     * to the subregion corresponding to the individual subtransform.
     * This simply creates an Array object with data pointer, no
     * memory is allocated or copied.
     * NOTE: the use of const_cast is used to avoid a deep copy in the underlying vnl_vector
     * by using LetArrayManageMemory=false, and being very careful here we can
     * ensure that casting away consteness does not result in memory corruption. */
    typename DerivativeType::ValueType * nonConstDataRefForPerformance =
      const_cast< typename DerivativeType::ValueType * >( &( (update.data_block() )[offset]) );
    const DerivativeType subUpdate( nonConstDataRefForPerformance, subtransform->GetNumberOfParameters(), false );
    /* This call will also call SetParameters, so don't need to call it
     * expliclity here. */
    subtransform->UpdateTransformParameters( subUpdate, factor );
    offset += subtransform->GetNumberOfParameters();
    }
  this->Modified();
}


template<typename TParametersValueType, unsigned int NDimensions, unsigned int NSubDimensions>
bool
MultiTransform<TParametersValueType, NDimensions, NSubDimensions>
::GetInverse( Self *inverse ) const
{
  typename TransformQueueType::const_iterator it;

  //NOTE: MultiTransform delegagtes to
  //      individual transform for setting FixedParameters
  //      inverse->SetFixedParameters( this->GetFixedParameters() );
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


template<typename TParametersValueType, unsigned int NDimensions, unsigned int NSubDimensions>
void
MultiTransform<TParametersValueType, NDimensions, NSubDimensions>
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

} // end namespace itk

#endif
