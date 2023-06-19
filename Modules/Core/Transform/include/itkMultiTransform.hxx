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
#ifndef itkMultiTransform_hxx
#define itkMultiTransform_hxx


namespace itk
{

template <typename TParametersValueType, unsigned int VDimension, unsigned int VSubDimensions>
auto
MultiTransform<TParametersValueType, VDimension, VSubDimensions>::GetTransformCategory() const -> TransformCategoryEnum
{
  // If all sub-transforms are the same, return that type. Otherwise
  // return Unknown.
  TransformCategoryEnum result = Self::TransformCategoryEnum::UnknownTransformCategory;

  for (SizeValueType tind = 0; tind < this->GetNumberOfTransforms(); ++tind)
  {
    const TransformCategoryEnum type = this->GetNthTransformConstPointer(tind)->GetTransformCategory();
    if (tind == 0)
    {
      result = type;
    }
    else
    {
      if (type != result)
      {
        result = Self::TransformCategoryEnum::UnknownTransformCategory;
        break;
      }
    }
  }

  return result;
}


template <typename TParametersValueType, unsigned int VDimension, unsigned int VSubDimensions>
bool
MultiTransform<TParametersValueType, VDimension, VSubDimensions>::IsLinear() const
{
  // If all sub-transforms are linear, return true.
  for (SizeValueType tind = 0; tind < this->GetNumberOfTransforms(); ++tind)
  {
    if (!this->GetNthTransformConstPointer(tind)->IsLinear())
    {
      return false;
    }
  }
  return true;
}


template <typename TParametersValueType, unsigned int VDimension, unsigned int VSubDimensions>
auto
MultiTransform<TParametersValueType, VDimension, VSubDimensions>::GetParameters() const -> const ParametersType &
{
  /* Resize destructively. But if it's already this size, nothing is done so
   * it's efficient. */
  this->m_Parameters.SetSize(this->GetNumberOfParameters());
  NumberOfParametersType offset{};

  for (const TransformType * const transform : m_TransformQueue)
  {
    const ParametersType & subParameters = transform->GetParameters();
    /* use vnl_vector data_block() to get data ptr */
    std::copy_n(subParameters.data_block(), subParameters.Size(), &(this->m_Parameters.data_block())[offset]);
    offset += subParameters.Size();
  }

  return this->m_Parameters;
}


template <typename TParametersValueType, unsigned int VDimension, unsigned int VSubDimensions>
void
MultiTransform<TParametersValueType, VDimension, VSubDimensions>::SetParameters(const ParametersType & inputParameters)
{
  /* We do not copy inputParameters into m_Parameters,
   * to avoid unnecessary copying. */

  /* We assume input params are concatenation of the parameters of the
     sub-transforms, in order of the queue from begin() to end(). */

  /* Verify proper input size. */
  if (inputParameters.Size() != this->GetNumberOfParameters())
  {
    itkExceptionMacro("Input parameter list size is not expected size. " << inputParameters.Size() << " instead of "
                                                                         << this->GetNumberOfParameters() << '.');
  }

  NumberOfParametersType offset{};

  for (TransformType * const transform : m_TransformQueue)
  {
    /* If inputParams is same object as m_Parameters, we just pass
     * each sub-transforms own m_Parameters in. This is needed to
     * avoid unnecessary copying of parameters in the sub-transforms,
     * while still allowing SetParameters to do any operations on the
     * parameters to update member variable states. A hack. */
    if (&inputParameters == &this->m_Parameters)
    {
      transform->SetParameters(transform->GetParameters());
    }
    else
    {
      const size_t parameterSize = transform->GetParameters().Size();
      transform->CopyInParameters(&(inputParameters.data_block())[offset],
                                  &(inputParameters.data_block())[offset] + parameterSize);
      offset += static_cast<NumberOfParametersType>(parameterSize);
    }
  }
}


template <typename TParametersValueType, unsigned int VDimension, unsigned int VSubDimensions>
auto
MultiTransform<TParametersValueType, VDimension, VSubDimensions>::GetFixedParameters() const
  -> const FixedParametersType &
{
  /* Resize destructively. But if it's already this size, nothing is done so
   * it's efficient. */
  this->m_FixedParameters.SetSize(this->GetNumberOfFixedParameters());

  NumberOfParametersType offset{};

  for (const TransformType * const transform : m_TransformQueue)
  {
    const FixedParametersType & subFixedParameters = transform->GetFixedParameters();
    /* use vnl_vector data_block() to get data ptr */
    std::copy_n(
      subFixedParameters.data_block(), subFixedParameters.Size(), &(this->m_FixedParameters.data_block())[offset]);
    offset += subFixedParameters.Size();
  }

  return this->m_FixedParameters;
}


template <typename TParametersValueType, unsigned int VDimension, unsigned int VSubDimensions>
void
MultiTransform<TParametersValueType, VDimension, VSubDimensions>::SetFixedParameters(
  const FixedParametersType & inputParameters)
{
  /* Verify proper input size. */
  if (inputParameters.Size() != this->GetNumberOfFixedParameters())
  {
    itkExceptionMacro("Input parameter list size is not expected size. " << inputParameters.Size() << " instead of "
                                                                         << this->GetNumberOfFixedParameters() << '.');
  }

  /* Assumes input params are concatenation of the parameters of the
   * sub transforms. */
  NumberOfParametersType offset{};

  /* Why is this done? Seems unnecessary. */
  this->m_FixedParameters = inputParameters;

  for (TransformType * const transform : m_TransformQueue)
  {
    const size_t fixedParameterSize = transform->GetFixedParameters().Size();
    transform->CopyInFixedParameters(&(this->m_FixedParameters.data_block())[offset],
                                     &(this->m_FixedParameters.data_block())[offset] + fixedParameterSize);
    offset += static_cast<NumberOfParametersType>(fixedParameterSize);
  }
}


template <typename TParametersValueType, unsigned int VDimension, unsigned int VSubDimensions>
auto
MultiTransform<TParametersValueType, VDimension, VSubDimensions>::GetNumberOfParameters() const
  -> NumberOfParametersType
{
  /* Returns to total number of params in all transforms currently
   * set to be used for optimized.
   * NOTE: Ideally we'd optimize this to store the result and
   * only re-calc when the composite object has been modified.
   * However, it seems that number of parameter might change for dense
   * field transforms (deformation, bspline) during processing and
   * we wouldn't know that in this class, so this is safest. */
  NumberOfParametersType result{};


  for (SizeValueType tind = 0; tind < this->GetNumberOfTransforms(); ++tind)
  {
    /* Use raw pointer for efficiency */
    const TransformType * transform = this->GetNthTransformConstPointer(tind);
    result += transform->GetNumberOfParameters();
  }
  return result;
}


template <typename TParametersValueType, unsigned int VDimension, unsigned int VSubDimensions>
auto
MultiTransform<TParametersValueType, VDimension, VSubDimensions>::GetNumberOfLocalParameters() const
  -> NumberOfParametersType
{
  if (this->GetMTime() == this->m_LocalParametersUpdateTime)
  {
    return this->m_NumberOfLocalParameters;
  }

  this->m_LocalParametersUpdateTime = this->GetMTime();

  /* Note that unlike in GetNumberOfParameters(), we don't expect the
   * number of local parameters to possibly change, so we can cache
   * the value. */
  NumberOfParametersType result{};

  for (SizeValueType tind = 0; tind < this->GetNumberOfTransforms(); ++tind)
  {
    const TransformType * transform = this->GetNthTransformConstPointer(tind);
    result += transform->GetNumberOfLocalParameters();
  }
  this->m_NumberOfLocalParameters = result;
  return result;
}


template <typename TParametersValueType, unsigned int VDimension, unsigned int VSubDimensions>
auto
MultiTransform<TParametersValueType, VDimension, VSubDimensions>::GetNumberOfFixedParameters() const
  -> NumberOfParametersType
{
  NumberOfParametersType result{};

  for (SizeValueType tind = 0; tind < this->GetNumberOfTransforms(); ++tind)
  {
    const TransformType * transform = this->GetNthTransformConstPointer(tind);
    result += transform->GetFixedParameters().Size();
  }
  return result;
}


template <typename TParametersValueType, unsigned int VDimension, unsigned int VSubDimensions>
void
MultiTransform<TParametersValueType, VDimension, VSubDimensions>::UpdateTransformParameters(
  const DerivativeType & update,
  ScalarType             factor)
{
  /* Update parameters within the sub-transforms. */
  /* NOTE: We might want to thread this over each sub-transform, if we
   * find we're working with longer lists of sub-transforms that do
   * not implement any threading of their own for UpdateTransformParameters.
   * Since the plan is for an UpdateTransformParameters functor that is
   * user-assignable, we would need a method in the
   * functor to return whether or not it does threading. If all sub-transforms
   * return that they don't thread, we could do each sub-transform in its
   * own thread from here. */
  NumberOfParametersType numberOfParameters = this->GetNumberOfParameters();

  if (update.Size() != numberOfParameters)
  {
    itkExceptionMacro("Parameter update size, " << update.Size()
                                                << ", must "
                                                   " be same as transform parameter size, "
                                                << numberOfParameters << std::endl);
  }

  NumberOfParametersType offset{};

  for (SizeValueType tind = 0; tind < this->GetNumberOfTransforms(); ++tind)
  {
    // HACK:  The following line looks wrong.  We should not need to const_cast
    TransformType * subtransform = this->GetNthTransformModifiablePointer(tind);
    /* The input values are in a monolithic block, so we have to point
     * to the subregion corresponding to the individual subtransform.
     * This simply creates an Array object with data pointer, no
     * memory is allocated or copied.
     * NOTE: the use of const_cast is used to avoid a deep copy in the underlying vnl_vector
     * by using LetArrayManageMemory=false, and being very careful here we can
     * ensure that casting away const-ness does not result in memory corruption. */
    auto * nonConstDataRefForPerformance =
      const_cast<typename DerivativeType::ValueType *>(&((update.data_block())[offset]));
    const DerivativeType subUpdate(nonConstDataRefForPerformance, subtransform->GetNumberOfParameters(), false);
    /* This call will also call SetParameters, so don't need to call it
     * explicitly here. */
    subtransform->UpdateTransformParameters(subUpdate, factor);
    offset += subtransform->GetNumberOfParameters();
  }
  this->Modified();
}


template <typename TParametersValueType, unsigned int VDimension, unsigned int VSubDimensions>
bool
MultiTransform<TParametersValueType, VDimension, VSubDimensions>::GetInverse(Self * inverse) const
{
  // NOTE: MultiTransform delegates to
  //      individual transform for setting FixedParameters
  //      inverse->SetFixedParameters( this->GetFixedParameters() );
  inverse->ClearTransformQueue();
  for (const TransformType * const transform : m_TransformQueue)
  {
    TransformTypePointer inverseTransform = (transform->GetInverseTransform()).GetPointer();
    if (!inverseTransform)
    {
      inverse->ClearTransformQueue();
      return false;
    }
    else
    {
      /* Add to end of queue to preserve transform order */
      inverse->PushBackTransform(inverseTransform);
    }
  }

  return true;
}


template <typename TParametersValueType, unsigned int VDimension, unsigned int VSubDimensions>
void
MultiTransform<TParametersValueType, VDimension, VSubDimensions>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "TransformQueue: " << std::endl;
  for (const TransformType * const transform : m_TransformQueue)
  {
    os << indent << ">>>>>>>>>" << std::endl;
    transform->Print(os, indent);
  }
}

} // end namespace itk

#endif
