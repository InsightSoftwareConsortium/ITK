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
#ifndef itkCompositeTransform_hxx
#define itkCompositeTransform_hxx

#include "itkCompositeTransform.h"

namespace itk
{


template
<typename TParametersValueType, unsigned int NDimensions>
CompositeTransform<TParametersValueType, NDimensions>::CompositeTransform()
{
  this->m_TransformsToOptimizeFlags.clear();
  this->m_TransformsToOptimizeQueue.clear();
  this->m_PreviousTransformsToOptimizeUpdateTime = 0;
}


template
<typename TParametersValueType, unsigned int NDimensions>
CompositeTransform<TParametersValueType, NDimensions>::
~CompositeTransform()
{
}


template
<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>::TransformCategoryType
CompositeTransform<TParametersValueType, NDimensions>
::GetTransformCategory() const
{
  // Check if linear
  bool isLinearTransform = this->IsLinear();
  if( isLinearTransform )
    {
    return Self::Linear;
    }

  // Check if displacement field
  bool isDisplacementFieldTransform = true;
  for( signed long tind = static_cast<signed long>( this->GetNumberOfTransforms() ) - 1; tind >= 0; tind-- )
    {
    if( this->GetNthTransformToOptimize( tind ) &&
      ( this->GetNthTransformConstPointer( tind )->GetTransformCategory() != Self::DisplacementField ) )
      {
      isDisplacementFieldTransform = false;
      break;
      }
    }

  if( isDisplacementFieldTransform )
    {
    return Self::DisplacementField;
    }
  else
    {
    return Self::UnknownTransformCategory;
    }
}


template
<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>
::OutputPointType
CompositeTransform<TParametersValueType, NDimensions>
::TransformPoint( const InputPointType& inputPoint ) const
{

  /* Apply in reverse queue order.  */
  typename TransformQueueType::const_iterator it( this->m_TransformQueue.end() );
  const typename TransformQueueType::const_iterator beginit( this->m_TransformQueue.begin() );
  OutputPointType outputPoint( inputPoint );
  do
    {
    it--;
    outputPoint = (*it)->TransformPoint( outputPoint );
    }
  while( it != beginit );
  return outputPoint;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>
::OutputVectorType
CompositeTransform<TParametersValueType, NDimensions>
::TransformVector( const InputVectorType & inputVector ) const
{
  OutputVectorType outputVector( inputVector );

  typename TransformQueueType::const_iterator it;
  /* Apply in reverse queue order.  */
  it = this->m_TransformQueue.end();

  do
    {
    it--;
    outputVector = (*it)->TransformVector( outputVector );
    }
  while( it != this->m_TransformQueue.begin() );

  return outputVector;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>
::OutputVectorType
CompositeTransform<TParametersValueType, NDimensions>
::TransformVector( const InputVectorType & inputVector, const InputPointType & inputPoint ) const
{
  OutputVectorType outputVector( inputVector );
  OutputPointType outputPoint( inputPoint );

  typename TransformQueueType::const_iterator it;
  /* Apply in reverse queue order.  */
  it = this->m_TransformQueue.end();

  do
    {
    it--;
    outputVector = (*it)->TransformVector( outputVector, outputPoint );
    outputPoint = (*it)->TransformPoint( outputPoint );
    }
  while( it != this->m_TransformQueue.begin() );

  return outputVector;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>
::OutputVnlVectorType
CompositeTransform<TParametersValueType, NDimensions>
::TransformVector( const InputVnlVectorType & inputVector, const InputPointType & inputPoint ) const
{
  OutputVnlVectorType outputVector( inputVector );
  OutputPointType outputPoint( inputPoint );

  typename TransformQueueType::const_iterator it;
  /* Apply in reverse queue order.  */
  it = this->m_TransformQueue.end();

  do
    {
    it--;
    outputVector = (*it)->TransformVector( outputVector, outputPoint );
    outputPoint = (*it)->TransformPoint( outputPoint );
    }
  while( it != this->m_TransformQueue.begin() );

  return outputVector;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>
::OutputVnlVectorType
CompositeTransform<TParametersValueType, NDimensions>
::TransformVector( const InputVnlVectorType & inputVector) const
{
  OutputVnlVectorType outputVector( inputVector );

  typename TransformQueueType::const_iterator it;
  /* Apply in reverse queue order.  */
  it = this->m_TransformQueue.end();

  do
    {
    it--;
    outputVector = (*it)->TransformVector( outputVector );
    }
  while( it != this->m_TransformQueue.begin() );

  return outputVector;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>
::OutputVectorPixelType
CompositeTransform<TParametersValueType, NDimensions>
::TransformVector( const InputVectorPixelType & inputVector ) const
{
  OutputVectorPixelType outputVector( inputVector );

  typename TransformQueueType::const_iterator it;
  /* Apply in reverse queue order.  */
  it = this->m_TransformQueue.end();

  do
    {
    it--;
    outputVector = (*it)->TransformVector( outputVector );
    }
  while( it != this->m_TransformQueue.begin() );

  return outputVector;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>
::OutputVectorPixelType
CompositeTransform<TParametersValueType, NDimensions>
::TransformVector( const InputVectorPixelType & inputVector, const InputPointType & inputPoint ) const
{
  OutputVectorPixelType outputVector( inputVector );
  OutputPointType outputPoint( inputPoint );

  typename TransformQueueType::const_iterator it;
  /* Apply in reverse queue order.  */
  it = this->m_TransformQueue.end();

  do
    {
    it--;
    outputVector = (*it)->TransformVector( outputVector, outputPoint );
    outputPoint = (*it)->TransformPoint( outputPoint );
    }
  while( it != this->m_TransformQueue.begin() );

  return outputVector;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>
::OutputCovariantVectorType
CompositeTransform<TParametersValueType, NDimensions>
::TransformCovariantVector( const InputCovariantVectorType & inputVector ) const
{
  OutputCovariantVectorType outputVector( inputVector );

  typename TransformQueueType::const_iterator it;
  /* Apply in reverse queue order.  */
  it = this->m_TransformQueue.end();

  do
    {
    it--;
    outputVector = (*it)->TransformCovariantVector( outputVector );
    }
  while( it != this->m_TransformQueue.begin() );

  return outputVector;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>
::OutputCovariantVectorType
CompositeTransform<TParametersValueType, NDimensions>
::TransformCovariantVector( const InputCovariantVectorType & inputVector, const InputPointType & inputPoint ) const
{
  OutputCovariantVectorType outputVector( inputVector );
  OutputPointType outputPoint( inputPoint );

  typename TransformQueueType::const_iterator it;
  /* Apply in reverse queue order.  */
  it = this->m_TransformQueue.end();

  do
    {
    it--;
    outputVector = (*it)->TransformCovariantVector( outputVector, outputPoint );
    outputPoint = (*it)->TransformPoint( outputPoint );
    }
  while( it != this->m_TransformQueue.begin() );

  return outputVector;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>
::OutputVectorPixelType
CompositeTransform<TParametersValueType, NDimensions>
::TransformCovariantVector( const InputVectorPixelType & inputVector ) const
{
  OutputVectorPixelType outputVector( inputVector );

  typename TransformQueueType::const_iterator it;
  /* Apply in reverse queue order.  */
  it = this->m_TransformQueue.end();

  do
    {
    it--;
    outputVector = (*it)->TransformCovariantVector( outputVector );
    }
  while( it != this->m_TransformQueue.begin() );

  return outputVector;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>
::OutputVectorPixelType
CompositeTransform<TParametersValueType, NDimensions>
::TransformCovariantVector( const InputVectorPixelType & inputVector, const InputPointType & inputPoint ) const
{
  OutputVectorPixelType outputVector( inputVector );
  OutputPointType outputPoint( inputPoint );

  typename TransformQueueType::const_iterator it;
  /* Apply in reverse queue order.  */
  it = this->m_TransformQueue.end();

  do
    {
    it--;
    outputVector = (*it)->TransformCovariantVector( outputVector, outputPoint );
    outputPoint = (*it)->TransformPoint( outputPoint );
    }
  while( it != this->m_TransformQueue.begin() );

  return outputVector;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>
::OutputDiffusionTensor3DType
CompositeTransform<TParametersValueType, NDimensions>
::TransformDiffusionTensor3D( const InputDiffusionTensor3DType & inputTensor, const InputPointType & inputPoint ) const
{
  OutputDiffusionTensor3DType outputTensor( inputTensor );
  OutputPointType outputPoint( inputPoint );

  typename TransformQueueType::const_iterator it;
  /* Apply in reverse queue order.  */
  it = this->m_TransformQueue.end();

  do
    {
    it--;
    outputTensor = (*it)->TransformDiffusionTensor3D( outputTensor, outputPoint );
    outputPoint = (*it)->TransformPoint( outputPoint );
    }
  while( it != this->m_TransformQueue.begin() );

  return outputTensor;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>
::OutputVectorPixelType
CompositeTransform<TParametersValueType, NDimensions>
::TransformDiffusionTensor3D( const InputVectorPixelType & inputTensor, const InputPointType & inputPoint ) const
{
  OutputVectorPixelType outputTensor( inputTensor );
  OutputPointType outputPoint( inputPoint );

  typename TransformQueueType::const_iterator it;
  /* Apply in reverse queue order.  */
  it = this->m_TransformQueue.end();

  do
    {
    it--;
    outputTensor = (*it)->TransformDiffusionTensor3D( outputTensor, outputPoint );
    outputPoint = (*it)->TransformPoint( outputPoint );
    }
  while( it != this->m_TransformQueue.begin() );

  return outputTensor;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>
::OutputDiffusionTensor3DType
CompositeTransform<TParametersValueType, NDimensions>
::TransformDiffusionTensor3D( const InputDiffusionTensor3DType & inputTensor ) const
{
  OutputDiffusionTensor3DType outputTensor( inputTensor );

  typename TransformQueueType::const_iterator it;
  /* Apply in reverse queue order.  */
  it = this->m_TransformQueue.end();

  do
    {
    it--;
    outputTensor = (*it)->TransformDiffusionTensor3D( outputTensor );
    }
  while( it != this->m_TransformQueue.begin() );

  return outputTensor;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>
::OutputVectorPixelType
CompositeTransform<TParametersValueType, NDimensions>
::TransformDiffusionTensor3D( const InputVectorPixelType & inputTensor ) const
{
  OutputVectorPixelType outputTensor( inputTensor );

  typename TransformQueueType::const_iterator it;
  /* Apply in reverse queue order.  */
  it = this->m_TransformQueue.end();

  do
    {
    it--;
    outputTensor = (*it)->TransformDiffusionTensor3D( outputTensor );
    }
  while( it != this->m_TransformQueue.begin() );

  return outputTensor;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>
::OutputSymmetricSecondRankTensorType
CompositeTransform<TParametersValueType, NDimensions>
::TransformSymmetricSecondRankTensor( const InputSymmetricSecondRankTensorType & inputTensor, const InputPointType & inputPoint ) const
{
  OutputSymmetricSecondRankTensorType outputTensor( inputTensor );
  OutputPointType outputPoint( inputPoint );

  typename TransformQueueType::const_iterator it;
  /* Apply in reverse queue order.  */
  it = this->m_TransformQueue.end();

  do
    {
    it--;
    outputTensor = (*it)->TransformSymmetricSecondRankTensor( outputTensor, outputPoint );
    outputPoint = (*it)->TransformPoint( outputPoint );
    }
  while( it != this->m_TransformQueue.begin() );

  return outputTensor;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>
::OutputVectorPixelType
CompositeTransform<TParametersValueType, NDimensions>
::TransformSymmetricSecondRankTensor( const InputVectorPixelType & inputTensor, const InputPointType & inputPoint ) const
{
  OutputVectorPixelType outputTensor( inputTensor );
  OutputPointType outputPoint( inputPoint );

  typename TransformQueueType::const_iterator it;
  /* Apply in reverse queue order.  */
  it = this->m_TransformQueue.end();

  do
    {
    it--;
    outputTensor = (*it)->TransformSymmetricSecondRankTensor( outputTensor, outputPoint );
    outputPoint = (*it)->TransformPoint( outputPoint );
    }
  while( it != this->m_TransformQueue.begin() );

  return outputTensor;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>
::OutputSymmetricSecondRankTensorType
CompositeTransform<TParametersValueType, NDimensions>
::TransformSymmetricSecondRankTensor( const InputSymmetricSecondRankTensorType & inputTensor ) const
{
  OutputSymmetricSecondRankTensorType outputTensor( inputTensor );

  typename TransformQueueType::const_iterator it;
  /* Apply in reverse queue order.  */
  it = this->m_TransformQueue.end();

  do
    {
    it--;
    outputTensor = (*it)->TransformSymmetricSecondRankTensor( outputTensor );
    }
  while( it != this->m_TransformQueue.begin() );

  return outputTensor;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>
::OutputVectorPixelType
CompositeTransform<TParametersValueType, NDimensions>
::TransformSymmetricSecondRankTensor( const InputVectorPixelType & inputTensor ) const
{
  OutputVectorPixelType outputTensor( inputTensor );

  typename TransformQueueType::const_iterator it;
  /* Apply in reverse queue order.  */
  it = this->m_TransformQueue.end();

  do
    {
    it--;
    outputTensor = (*it)->TransformSymmetricSecondRankTensor( outputTensor );
    }
  while( it != this->m_TransformQueue.begin() );

  return outputTensor;
}


template<typename TParametersValueType, unsigned int NDimensions>
bool
CompositeTransform<TParametersValueType, NDimensions>
::GetInverse( Self *inverse ) const
{
  typename TransformQueueType::const_iterator it;

  //NOTE: CompositeTransform delegagtes to
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
      /* Push to front to reverse the transform order */
      inverse->PushFrontTransform( inverseTransform );
      }
    }

  /* Copy the optimization flags */
  inverse->m_TransformsToOptimizeFlags.clear();
  for( TransformsToOptimizeFlagsType::iterator ofit = this->m_TransformsToOptimizeFlags.begin(); ofit != this->m_TransformsToOptimizeFlags.end(); ofit++ )
    {
    inverse->m_TransformsToOptimizeFlags.push_front( *ofit );
    }

  return true;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>
::InverseTransformBasePointer
CompositeTransform<TParametersValueType, NDimensions>
::GetInverseTransform() const
{
  /* This method can't be defined in Superclass because of the call to New() */
  Pointer inverseTransform = New();

  if( this->GetInverse( inverseTransform ) )
    {
    return inverseTransform.GetPointer();
    }
  else
    {
    return ITK_NULLPTR;
    }
}


template<typename TParametersValueType, unsigned int NDimensions>
void
CompositeTransform<TParametersValueType, NDimensions>
::ComputeJacobianWithRespectToParameters( const InputPointType & p, JacobianType & outJacobian ) const
{
  /* Returns a concatenated MxN array, holding the Jacobian of each sub
   * transform that is selected for optimization. The order is the same
   * as that in which they're applied, i.e. reverse order.
   * M rows = dimensionality of the transforms
   * N cols = total number of parameters in the selected sub transforms. */
  outJacobian.SetSize( NDimensions, this->GetNumberOfLocalParameters() );
  JacobianType jacobianWithRespectToPosition(NDimensions, NDimensions);
  this->ComputeJacobianWithRespectToParametersCachedTemporaries( p, outJacobian, jacobianWithRespectToPosition );
}

template<typename TParametersValueType, unsigned int NDimensions>
void
CompositeTransform<TParametersValueType, NDimensions>
::ComputeJacobianWithRespectToParametersCachedTemporaries( const InputPointType & p, JacobianType & outJacobian, JacobianType & jacobianWithRespectToPosition ) const
{
  //NOTE: This must have been done outside of outJacobian.SetSize( NDimensions, this->GetNumberOfLocalParameters() );
  //NOTE: assert( outJacobian.GetSize == ( NDimensions, this->GetNumberOfLocalParameters() ) )
  //NOTE: assert( jacobianWithRespectToPosition.GetSize == (NDimensions, NDimensions) )

  NumberOfParametersType offset = NumericTraits< NumberOfParametersType >::ZeroValue();

  OutputPointType transformedPoint( p );

  /*
   * Composite transform $T is composed of $T0(p0,x), $T1(p1,x) and $T2(p2, x) as:
   *
   * T(p0, p1, p2, x)
   * = T0(p0, T1(p1, T2(p2, x)))
   *
   * p0, p1, p2 are the transform parameters for transform T0, T1, T2
   * respectively.
   *
   * Let p = (p0, p1, p2).
   *  x2 = T2(p2, x).
   *  x1 = T1(p1, x2).
   *
   *
   * The following loop computes dT/dp:
   *
   * dT/dp
   * = (dT/dp0, dT/dp1, dT/dp2)
   * = ( dT0/dp0 | x1 ),
   *   ( dT0/dT1 | x1 ) * ( dT1/dp1 | x2 ),
   *   ( ( dT0/dT1 | x1 ) * ( dT1/dT2 | x2 ) * ( dT2/dp2 | x )
   *
   * In the first iteration, it computes
   *   dT2/dp2 | x
   *
   * In the second iteration, it computes
   *   dT1/dp1 | x2
   *
   *  and it computes
   *   dT1/dT2 | x2, and left multiplying to  dT2/dp2 | x
   *
   * In the third iteration, it computes
   *   dT0/dp0 | x1,
   *
   *  and it computes
   *   dT0/dT1 | x1, and left multiplying to
   *    ( dT1/dT2 | x2 ) * ( dT2/dp2 | x )
   *    and ( dT1/dp1 | x2 )
   *
   */
  for( signed long tind = (signed long) this->GetNumberOfTransforms() - 1;
       tind >= 0; --tind )
    {
    /* Get a raw pointer for efficiency, avoiding SmartPointer register/unregister */
    const TransformType * const transform = this->GetNthTransformConstPointer( tind );

    const NumberOfParametersType offsetLast = offset;

    if( this->GetNthTransformToOptimize( tind ) )
      {
      /* Copy from another matrix, element-by-element */
      /* The matrices are row-major, so block copy is less obviously
       * better */

      const NumberOfParametersType numberOfLocalParameters = transform->GetNumberOfLocalParameters();

      typename TransformType::JacobianType current_jacobian( NDimensions, numberOfLocalParameters );
      transform->ComputeJacobianWithRespectToParameters( transformedPoint, current_jacobian );
      outJacobian.update( current_jacobian, 0, offset );
      offset += numberOfLocalParameters;
      }

    /** The composite transform needs to compose previous jacobians
     *  (those closer to the originating point) with the current
     *  transform's jacobian.  We therefore update the previous
     *  jacobian by multiplying the current matrix jumping over the
     *  first transform. The matrix here refers to  dT/dx at the point.
     *  For example, in the affine transform, this is the affine matrix.
     *
     *  TODO: for general transform, there should be something like
     *  GetPartialDerivativeOfPointCoordinates
     *
     *  Also, noted the multiplication contains all the affine matrix from
     *  all transforms no matter they are going to be optimized or not
     */

    // update every old term by left multiplying dTk / dT{k-1}
    // do this before computing the transformedPoint for the next iteration
    if( offsetLast > 0 )
      {
      transform->ComputeJacobianWithRespectToPosition(transformedPoint, jacobianWithRespectToPosition);

      const JacobianType & old_j = outJacobian.extract(NDimensions, offsetLast, 0, 0);
      const JacobianType & update_j = jacobianWithRespectToPosition * old_j;

      outJacobian.update(update_j, 0, 0);

      // itkExceptionMacro(" To sort out with new ComputeJacobianWithRespectToPosition prototype ");
      }

    /* Transform the point so it's ready for next transform's Jacobian */
    transformedPoint = transform->TransformPoint( transformedPoint );
    }
}


template<typename TParametersValueType, unsigned int NDimensions>
const typename CompositeTransform<TParametersValueType, NDimensions>::ParametersType &
CompositeTransform<TParametersValueType, NDimensions>
::GetParameters() const
{
  const TransformQueueType & transforms = this->GetTransformsToOptimizeQueue();
  if( transforms.size() == 1 )
    {
    // Return directly to avoid copying. Most often we'll have only a single
    // active transform, so we'll end up here.
    return transforms[0]->GetParameters();
    }
  else
    {
    /* Resize destructively. But if it's already this size, nothing is done so
         * it's efficient. */
    this->m_Parameters.SetSize( this->GetNumberOfParameters() );

    NumberOfParametersType offset = NumericTraits< NumberOfParametersType >::ZeroValue();

    typename TransformQueueType::const_iterator it = transforms.end();

    do
      {
      it--;
      const ParametersType & subParameters = (*it)->GetParameters();
      /* use vnl_vector data_block() to get data ptr */
      std::copy(subParameters.data_block(),
                subParameters.data_block()+subParameters.Size(),
                &(this->m_Parameters.data_block() )[offset]);
      offset += subParameters.Size();

      }
    while( it != transforms.begin() );
    }

  return this->m_Parameters;
}


template<typename TParametersValueType, unsigned int NDimensions>
void
CompositeTransform<TParametersValueType, NDimensions>
::SetParameters(const ParametersType & inputParameters)
{
  /* We do not copy inputParameters into m_Parameters,
     * to avoid unnecessary copying. */

  /* Assumes input params are concatenation of the parameters of the
     sub transforms currently selected for optimization, in
     the order of the queue from begin() to end(). */
  TransformQueueType transforms = this->GetTransformsToOptimizeQueue();

  /* Verify proper input size. */
  if( inputParameters.Size() != this->GetNumberOfParameters() )
    {
    itkExceptionMacro(<< "Input parameter list size is not expected size. "
                      << inputParameters.Size() << " instead of "
                      << this->GetNumberOfParameters() << ".");
    }

  if( transforms.size() == 1 )
    {
    /* Avoid unnecessary copying. See comments below */
    if( &inputParameters == &this->m_Parameters )
      {
      transforms[0]->SetParameters( transforms[0]->GetParameters() );
      }
    else
      {
      transforms[0]->SetParameters(inputParameters);
      }
    }
  else
    {
    NumberOfParametersType offset = NumericTraits< NumberOfParametersType >::ZeroValue();
    typename TransformQueueType::iterator it = transforms.end();

    do
      {
      it--;
      /* If inputParams is same object as m_Parameters, we just pass
       * each sub-transforms own m_Parameters in. This is needed to
       * avoid unnecessary copying of parameters in the sub-transforms,
       * while still allowing SetParameters to do any oeprations on the
       * parameters to update member variable states. A hack. */
      if( &inputParameters == &this->m_Parameters )
        {
        (*it)->SetParameters( (*it)->GetParameters() );
        }
      else
        {
        const size_t parameterSize = (*it)->GetParameters().Size();
        (*it)->CopyInParameters(&(inputParameters.data_block() )[offset],
                                &(inputParameters.data_block() )[offset]+parameterSize );
        offset += static_cast<NumberOfParametersType>(parameterSize);
        }

      }
    while( it != transforms.begin() );
    }
}


template<typename TParametersValueType, unsigned int NDimensions>
const typename CompositeTransform<TParametersValueType, NDimensions>::FixedParametersType &
CompositeTransform<TParametersValueType, NDimensions>
::GetFixedParameters() const
  {
  TransformQueueType transforms = this->GetTransformsToOptimizeQueue();
  /* Resize destructively. But if it's already this size, nothing is done so
   * it's efficient. */
  this->m_FixedParameters.SetSize( this->GetNumberOfFixedParameters() );

  NumberOfParametersType offset = NumericTraits< NumberOfParametersType >::ZeroValue();
  typename TransformQueueType::const_iterator it;

  it = transforms.end();

  do
    {
    it--;
    const FixedParametersType & subFixedParameters = (*it)->GetFixedParameters();
    /* use vnl_vector data_block() to get data ptr */
    std::copy(subFixedParameters.data_block(),
              subFixedParameters.data_block()+subFixedParameters.Size(),
              &(this->m_FixedParameters.data_block() )[offset]);
    offset += subFixedParameters.Size();
    }
  while( it != transforms.begin() );

  return this->m_FixedParameters;
  }

template<typename TParametersValueType, unsigned int NDimensions>
void
CompositeTransform<TParametersValueType, NDimensions>
::SetFixedParameters(const FixedParametersType & inputParameters)
{
  /* Assumes input params are concatenation of the parameters of the
   * sub transforms currently selected for optimization. */
  TransformQueueType transforms = this->GetTransformsToOptimizeQueue();

  NumberOfParametersType offset = NumericTraits< NumberOfParametersType >::ZeroValue();


  /* Verify proper input size. */
  if( inputParameters.Size() != this->GetNumberOfFixedParameters() )
    {
    itkExceptionMacro(<< "Input parameter list size is not expected size. "
                      << inputParameters.Size() << " instead of "
                      << this->GetNumberOfFixedParameters() << ".");
    }
  this->m_FixedParameters = inputParameters;

  typename TransformQueueType::const_iterator it = transforms.end();

  do
    {
    it--;
    const size_t fixedParameterSize=(*it)->GetFixedParameters().Size();
    (*it)->CopyInFixedParameters(&(this->m_FixedParameters.data_block() )[offset],
              &(this->m_FixedParameters.data_block() )[offset]+fixedParameterSize);
    offset += static_cast<NumberOfParametersType>(fixedParameterSize);
    }
  while( it != transforms.begin() );
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>::NumberOfParametersType
CompositeTransform<TParametersValueType, NDimensions>
::GetNumberOfParameters(void) const
{
  /* Returns to total number of params in all transforms currently
   * set to be used for optimized.
   * NOTE: We might want to optimize this only to store the result and
   * only re-calc when the composite object has been modified.
   * However, it seems that number of parameter might change for dense
   * field transforms (deformation, bspline) during processing and
   * we wouldn't know that in this class, so this is safest. */
  NumberOfParametersType result = NumericTraits< NumberOfParametersType >::ZeroValue();


  for( signed long tind = (signed long) this->GetNumberOfTransforms() - 1; tind >= 0; tind-- )
    {
    if( this->GetNthTransformToOptimize( tind ) )
      {
      const TransformType * transform = this->GetNthTransformConstPointer( tind );
      result += transform->GetNumberOfParameters();
      }
    }
  return result;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>::NumberOfParametersType
CompositeTransform<TParametersValueType, NDimensions>
::GetNumberOfLocalParameters() const
{
  if ( this->GetMTime() == this->m_LocalParametersUpdateTime )
   {
   return this->m_NumberOfLocalParameters;
   }

  this->m_LocalParametersUpdateTime = this->GetMTime();

  /* Returns to total number of *local* params in all transforms currently
   * set to be used for optimized.
   * Note that unlike in GetNumberOfParameters(), we don't expect the
   * number of local parameters to possibly change. */
  NumberOfParametersType result = NumericTraits< NumberOfParametersType >::ZeroValue();

  for( signed long tind = (signed long) this->GetNumberOfTransforms() - 1; tind >= 0; tind-- )
    {
    if( this->GetNthTransformToOptimize( tind ) )
      {
      const TransformType * transform = this->GetNthTransformConstPointer( tind );
      result += transform->GetNumberOfLocalParameters();
      }
    }
  this->m_NumberOfLocalParameters = result;
  return result;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>::NumberOfParametersType
CompositeTransform<TParametersValueType, NDimensions>
::GetNumberOfFixedParameters() const
{
  /* Returns to total number of params in all transforms currently
   * set to be used for optimized.
   * NOTE: We might want to optimize this only to store the result and
   * only re-calc when the composite object has been modified. */
  NumberOfParametersType result = NumericTraits< NumberOfParametersType >::ZeroValue();

  for( signed long tind = (signed long) this->GetNumberOfTransforms() - 1;
       tind >= 0; tind-- )
    {
    if( this->GetNthTransformToOptimize( tind ) )
      {
      const TransformType * transform = this->GetNthTransformConstPointer( tind );
      result += transform->GetFixedParameters().Size();
      }
    }
  return result;
}


template<typename TParametersValueType, unsigned int NDimensions>
void
CompositeTransform<TParametersValueType, NDimensions>
::UpdateTransformParameters( const DerivativeType & update, ScalarType  factor )
{
  /* Update parameters within the sub-transforms set to be optimized. */
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


  for( signed long tind = (signed long) this->GetNumberOfTransforms() - 1;
       tind >= 0; tind-- )
    {
    if( this->GetNthTransformToOptimize( tind ) )
      {
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
      const DerivativeType subUpdate( nonConstDataRefForPerformance,
                                subtransform->GetNumberOfParameters(), false );
      /* This call will also call SetParameters, so don't need to call it
       * expliclity here. */
      subtransform->UpdateTransformParameters( subUpdate, factor );
      offset += subtransform->GetNumberOfParameters();
      }
    }
  this->Modified();
}


template<typename TParametersValueType, unsigned int NDimensions>
typename CompositeTransform<TParametersValueType, NDimensions>::TransformQueueType &
CompositeTransform<TParametersValueType, NDimensions>
::GetTransformsToOptimizeQueue() const
{
  /* Update the list of transforms to use for optimization only if
   the selection of transforms to optimize may have changed */
  if( this->GetMTime() > this->m_PreviousTransformsToOptimizeUpdateTime )
    {
    this->m_TransformsToOptimizeQueue.clear();
    for( size_t n = 0; n < this->m_TransformQueue.size(); n++ )
      {
      /* Return them in the same order as they're found in the main list */
      if( this->GetNthTransformToOptimize(static_cast<SizeValueType>( n ) ) )
        {
        this->m_TransformsToOptimizeQueue.push_back( this->GetNthTransformModifiablePointer(static_cast<SizeValueType>( n ) ) );
        }
      }
    this->m_PreviousTransformsToOptimizeUpdateTime = this->GetMTime();
    }
  return this->m_TransformsToOptimizeQueue;
}


template<typename TParametersValueType, unsigned int NDimensions>
void
CompositeTransform<TParametersValueType, NDimensions>
::FlattenTransformQueue()
{
  TransformQueueType             transformQueue;
  TransformQueueType             transformsToOptimizeQueue;
  TransformsToOptimizeFlagsType  transformsToOptimizeFlags;

  for( SizeValueType m = 0; m < this->GetNumberOfTransforms(); m++ )
    {
    Self * nestedCompositeTransform = dynamic_cast<Self *>( this->m_TransformQueue[m].GetPointer() );
    if( nestedCompositeTransform )
      {
      nestedCompositeTransform->FlattenTransformQueue();
      for( SizeValueType n = 0; n < nestedCompositeTransform->GetNumberOfTransforms(); n++ )
        {
        transformQueue.push_back( nestedCompositeTransform->GetNthTransformModifiablePointer( n ) );
        if( nestedCompositeTransform->GetNthTransformToOptimize( n ) )
          {
          transformsToOptimizeFlags.push_back( true );
          transformsToOptimizeQueue.push_back( nestedCompositeTransform->GetNthTransformModifiablePointer( n ) );
          }
        else
          {
          transformsToOptimizeFlags.push_back( false );
          }
        }
      }
    else
      {
      transformQueue.push_back( this->m_TransformQueue[m] );
      if( this->m_TransformsToOptimizeFlags[m] )
        {
        transformsToOptimizeFlags.push_back( true );
        transformsToOptimizeQueue.push_back( this->m_TransformQueue[m] );
        }
      else
        {
        transformsToOptimizeFlags.push_back( false );
        }
      }
    }

  this->m_TransformQueue = transformQueue;
  this->m_TransformsToOptimizeQueue = transformsToOptimizeQueue;
  this->m_TransformsToOptimizeFlags = transformsToOptimizeFlags;
}


template<typename TParametersValueType, unsigned int NDimensions>
void
CompositeTransform<TParametersValueType, NDimensions>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  if( this->GetNumberOfTransforms() == 0 )
    {
    return;
    }

  os << indent << "TransformsToOptimizeFlags, begin() to end(): " << std::endl << indent << indent;
  for(  TransformsToOptimizeFlagsType::iterator
        it = this->m_TransformsToOptimizeFlags.begin();
        it != this->m_TransformsToOptimizeFlags.end(); it++ )
    {
    os << *it << " ";
    }
  os << std::endl;

  os << indent <<  "TransformsToOptimize in queue, from begin to end:" << std::endl;
  typename TransformQueueType::const_iterator cit;
  for( cit = this->m_TransformsToOptimizeQueue.begin();
       cit != this->m_TransformsToOptimizeQueue.end(); ++cit )
    {
    os << indent << ">>>>>>>>>" << std::endl;
    (*cit)->Print( os, indent );
    }
  os << indent <<  "End of TransformsToOptimizeQueue." << std::endl << "<<<<<<<<<<" << std::endl;

  os << indent <<  "End of CompositeTransform." << std::endl << "<<<<<<<<<<" << std::endl;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename LightObject::Pointer
CompositeTransform<TParametersValueType, NDimensions>
::InternalClone() const
{
  // This class doesn't use its superclass implementation
  // TODO: is it really the right behavior?
  // LightObject::Pointer loPtr = Superclass::InternalClone();

  LightObject::Pointer loPtr = CreateAnother();
  typename Self::Pointer clone =
    dynamic_cast<Self *>(loPtr.GetPointer());
  if(clone.IsNull())
    {
    itkExceptionMacro(<< "downcast to type " << this->GetNameOfClass() << " failed.");
    }

  typename TransformQueueType::iterator tqIt =
    this->m_TransformQueue.begin();

  typename TransformsToOptimizeFlagsType::iterator tfIt =
    this->m_TransformsToOptimizeFlags.begin();

  for(int i = 0; tqIt != this->m_TransformQueue.end() &&
        tfIt != this->m_TransformsToOptimizeFlags.end();
      ++tqIt, ++tfIt, ++i)
    {
    clone->AddTransform((*tqIt)->Clone().GetPointer());
    clone->SetNthTransformToOptimize(i,(*tfIt));
    }
  return loPtr;
}

} // end namespace itk

#endif
