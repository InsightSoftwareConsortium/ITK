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
#ifndef itkCompositeTransform_h
#define itkCompositeTransform_h

#include "itkMultiTransform.h"

#include <deque>

namespace itk
{

/** \class CompositeTransform
 * \brief This class contains a list of transforms and concatenates them by composition.
 *
 * This class concatenates transforms in \b reverse \b queue order by means of composition:
 *    \f$ T_0 o T_1 = T_0(T_1(x)) \f$
 * Transforms are stored in a container (queue), in the following order:
 *    \f$ T_0, T_1, ... , T_N-1 \f$
 * Transforms are added via a single method, AddTransform(). This adds the
 * transforms to the back of the queue. A single method for adding transforms
 * is meant to simplify the interface and prevent errors.
 * One use of the class is to optimize only a subset of included transforms.
 *
 * The sub transforms are the same dimensionality as this class.
 *
 * Example:
 * A user wants to optimize two Affine transforms together, then add a
 * Deformation Field (DF) transform, and optimize it separately.
 * He first adds the two Affines, then runs the optimization and both Affines
 * transforms are optimized. Next, he adds the DF transform and calls
 * SetOnlyMostRecentTransformToOptimizeOn, which clears the optimization flags
 * for both of the affine transforms, and leaves the flag set only for the DF
 * transform, since it was the last transform added. Now he runs the
 * optimization and only the DF transform is optimized, but the affines are
 * included in the transformation during the optimization.
 *
 * Optimization Flags:
 * The m_TransformsToOptimize flags hold one flag for each transform in the
 * queue, designating if each transform is to be used for optimization. Note
 * that all transforms in the queue are applied in TransformPoint, regardless
 * of these flags states'. The methods GetParameters, SetParameters,
 * ComputeJacobianWithRespectToParameters, GetTransformCategory,
 * GetFixedParameters, and SetFixedParameters all query these
 * flags and include only those transforms whose corresponding flag is set.
 * Their input or output is a concatenated array of all transforms set for use
 * in optimization. The goal is to be able to optimize multiple transforms at
 * once, while leaving other transforms fixed. See the above example.
 *
 * Setting Optimization Flags:
 * A transform's optimization flag is set when it is added to the queue, and
 * remains set as other transforms are added. The methods
 * SetNthTransformToOptimize* and SetAllTransformToOptimize* are used to
 * set and clear flags arbitrarily. SetOnlyMostRecentTransformToOptimizeOn is
 * a convenience method for setting only the most recently added transform
 * for optimization, with the idea that this will be a common practice.
 *
 * Indexing:
 * The index values used in GetNthTransform and
 * SetNthTransformToOptimize* and SetAllTransformToOptimize* follow the
 * order in which transforms were added. Thus, the first transform added is at
 * index 0, the next at index 1, etc.
 *
 * Inverse:
 * The inverse transform is created by retrieving the inverse from each
 * sub transform and adding them to a composite transform in reverse order.
 * The m_TransformsToOptimizeFlags is copied in reverse for the inverse.
 *
 * \ingroup ITKTransform
 */
template<typename TParametersValueType=double, unsigned int NDimensions = 3>
class ITK_TEMPLATE_EXPORT CompositeTransform :
  public MultiTransform<TParametersValueType, NDimensions, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef CompositeTransform                                              Self;
  typedef MultiTransform<TParametersValueType, NDimensions, NDimensions> Superclass;
  typedef SmartPointer<Self>                                              Pointer;
  typedef SmartPointer<const Self>                                        ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( CompositeTransform, Transform );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** Sub transform type **/
  typedef typename Superclass::TransformType                TransformType;
  typedef typename Superclass::TransformTypePointer         TransformTypePointer;
  /** InverseTransform type. */
  typedef typename Superclass::InverseTransformBasePointer  InverseTransformBasePointer;
  /** Scalar type. */
  typedef typename Superclass::ScalarType                 ScalarType;
  /** Parameters type. */
  typedef typename Superclass::FixedParametersType        FixedParametersType;
  typedef typename Superclass::FixedParametersValueType   FixedParametersValueType;
  typedef typename Superclass::ParametersType             ParametersType;
  typedef typename Superclass::ParametersValueType        ParametersValueType;
  /** Derivative type */
  typedef typename Superclass::DerivativeType             DerivativeType;
  /** Jacobian type. */
  typedef typename Superclass::JacobianType               JacobianType;
  /** Transform category type. */
  typedef typename Superclass::TransformCategoryType      TransformCategoryType;
  /** Standard coordinate point type for this class. */
  typedef typename Superclass::InputPointType             InputPointType;
  typedef typename Superclass::OutputPointType            OutputPointType;
  /** Standard vector type for this class. */
  typedef typename Superclass::InputVectorType            InputVectorType;
  typedef typename Superclass::OutputVectorType           OutputVectorType;
  /** Standard covariant vector type for this class */
  typedef typename Superclass::InputCovariantVectorType   InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType  OutputCovariantVectorType;
  /** Standard vnl_vector type for this class. */
  typedef typename Superclass::InputVnlVectorType         InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType        OutputVnlVectorType;
  /** Standard Vectorpixel type for this class */
  typedef typename Superclass::InputVectorPixelType       InputVectorPixelType;
  typedef typename Superclass::OutputVectorPixelType      OutputVectorPixelType;
  /** Standard DiffusionTensor3D typedef for this class */
  typedef typename Superclass::InputDiffusionTensor3DType  InputDiffusionTensor3DType;
  typedef typename Superclass::OutputDiffusionTensor3DType OutputDiffusionTensor3DType;
  /** Standard SymmetricSecondRankTensor typedef for this class */
  typedef typename Superclass::InputSymmetricSecondRankTensorType   InputSymmetricSecondRankTensorType;
  typedef typename Superclass::OutputSymmetricSecondRankTensorType  OutputSymmetricSecondRankTensorType;

  /** Transform queue type */
  typedef typename Superclass::TransformQueueType         TransformQueueType;

  /** The number of parameters defininig this transform. */
  typedef typename Superclass::NumberOfParametersType     NumberOfParametersType;

  /** Optimization flags queue type */
  typedef std::deque<bool>                                TransformsToOptimizeFlagsType;

  /** Dimension of the domain spaces. */
  itkStaticConstMacro( InputDimension, unsigned int, NDimensions );
  itkStaticConstMacro( OutputDimension, unsigned int, NDimensions );

  /** Active Transform state manipulation */

  virtual void SetNthTransformToOptimize( SizeValueType i, bool state )
  {
    this->m_TransformsToOptimizeFlags.at(i) = state;
    this->Modified();
  }

  virtual void SetNthTransformToOptimizeOn( SizeValueType i )
  {
    this->SetNthTransformToOptimize( i, true );
  }

  virtual void SetNthTransformToOptimizeOff( SizeValueType i )
  {
    this->SetNthTransformToOptimize( i, false );
  }

  virtual void SetAllTransformsToOptimize( bool state )
  {
    this->m_TransformsToOptimizeFlags.assign(
      this->m_TransformsToOptimizeFlags.size(), state );
    this->Modified();
  }

  virtual void SetAllTransformsToOptimizeOn()
  {
    this->SetAllTransformsToOptimize( true );
  }

  virtual void SetAllTransformsToOptimizeOff()
  {
    this->SetAllTransformsToOptimize( false );
  }

  /* With AddTransform() as the only way to add a transform, we
   * can have this method to easily allow user to optimize only
   * the transform added most recenlty. */
  virtual void SetOnlyMostRecentTransformToOptimizeOn()
  {
    this->SetAllTransformsToOptimize( false );
    this->SetNthTransformToOptimizeOn( this->GetNumberOfTransforms() - 1 );
  }

  /* Get whether the Nth transform is set to be optimzied */
  /* NOTE: ambiguous function name here - are we getting if the Nth transform
      is set to be optimized, or the Nth of the transforms that are set to be
      optimized? */
  virtual bool GetNthTransformToOptimize( SizeValueType i ) const
  {
    return this->m_TransformsToOptimizeFlags.at(i);
  }

  /** Access optimize flags */
  virtual const TransformsToOptimizeFlagsType & GetTransformsToOptimizeFlags() const
  {
    return this->m_TransformsToOptimizeFlags;
  }

  virtual void ClearTransformQueue() ITK_OVERRIDE
  {
    Superclass::ClearTransformQueue();
    this->m_TransformsToOptimizeFlags.clear();
  }

  /** Returns a boolean indicating whether it is possible or not to compute the
   * inverse of this current Transform. If it is possible, then the inverse of
   * the transform is returned in the inverseTransform variable passed by the user.
   * The inverse consists of the inverse of each sub-transform, in the \b reverse order
   * of the forward transforms. */
  bool GetInverse( Self *inverse ) const;

  virtual InverseTransformBasePointer GetInverseTransform() const ITK_OVERRIDE;

  /** Compute the position of point in the new space.
  *
  * Transforms are applied starting from the *back* of the
  * queue. That is, in reverse order of which they were added, in order
  * to work properly with ResampleFilter.
  *
  * Imagine a user wants to apply an Affine transform followed by a Deformation
  * Field (DF) transform. He adds the Affine, then the DF. Because the user
  * typically conceptualizes a transformation as being applied from the Moving
  * image to the Fixed image, this makes intuitive sense. But since the
  * ResampleFilter expects to transform from the Fixed image to the Moving
  * image, the transforms are applied in reverse order of addition, i.e. from
  * the back of the queue, and thus, DF then Affine.
  */
  virtual OutputPointType TransformPoint( const InputPointType & inputPoint ) const ITK_OVERRIDE;

  /**  Method to transform a vector. */
  using Superclass::TransformVector;
  virtual OutputVectorType TransformVector(const InputVectorType &) const ITK_OVERRIDE;

  virtual OutputVnlVectorType TransformVector(const InputVnlVectorType & inputVector) const ITK_OVERRIDE;

  virtual OutputVectorPixelType TransformVector(const InputVectorPixelType & inputVector ) const ITK_OVERRIDE;

  virtual OutputVectorType TransformVector(const InputVectorType & inputVector,
                                           const InputPointType & inputPoint ) const ITK_OVERRIDE;

  virtual OutputVnlVectorType TransformVector(const InputVnlVectorType & inputVector,
                                              const InputPointType & inputPoint ) const ITK_OVERRIDE;

  virtual OutputVectorPixelType TransformVector(const InputVectorPixelType & inputVector,
                                                const InputPointType & inputPoint ) const ITK_OVERRIDE;

  /**  Method to transform a CovariantVector. */
  using Superclass::TransformCovariantVector;
  virtual OutputCovariantVectorType TransformCovariantVector(const InputCovariantVectorType &) const ITK_OVERRIDE;

  virtual OutputVectorPixelType TransformCovariantVector(const InputVectorPixelType &) const ITK_OVERRIDE;

  virtual OutputCovariantVectorType TransformCovariantVector(const InputCovariantVectorType & inputVector,
                                                             const InputPointType & inputPoint ) const ITK_OVERRIDE;

  virtual OutputVectorPixelType TransformCovariantVector(const InputVectorPixelType & inputVector,
                                                         const InputPointType & inputPoint ) const ITK_OVERRIDE;

  /** Method to transform a DiffusionTensor3D */
  using Superclass::TransformDiffusionTensor3D;
  virtual OutputDiffusionTensor3DType TransformDiffusionTensor3D(
    const InputDiffusionTensor3DType & inputTensor) const ITK_OVERRIDE;

  virtual OutputVectorPixelType TransformDiffusionTensor3D(
    const InputVectorPixelType & inputTensor) const ITK_OVERRIDE;

  virtual OutputDiffusionTensor3DType TransformDiffusionTensor3D(
    const InputDiffusionTensor3DType & inputTensor,
    const InputPointType & inputPoint ) const ITK_OVERRIDE;

  virtual OutputVectorPixelType TransformDiffusionTensor3D(
    const InputVectorPixelType & inputTensor,
    const InputPointType & inputPoint ) const ITK_OVERRIDE;

  /** Method to transform a SymmetricSecondRankTensor */
  using Superclass::TransformSymmetricSecondRankTensor;
  virtual OutputSymmetricSecondRankTensorType TransformSymmetricSecondRankTensor(
    const InputSymmetricSecondRankTensorType & inputTensor) const ITK_OVERRIDE;

  virtual OutputVectorPixelType TransformSymmetricSecondRankTensor(
    const InputVectorPixelType & inputTensor) const ITK_OVERRIDE;

  virtual OutputSymmetricSecondRankTensorType TransformSymmetricSecondRankTensor(
    const InputSymmetricSecondRankTensorType & inputTensor,
    const InputPointType & inputPoint ) const ITK_OVERRIDE;

  virtual OutputVectorPixelType TransformSymmetricSecondRankTensor(
    const InputVectorPixelType & inputTensor,
    const InputPointType & inputPoint ) const ITK_OVERRIDE;

  /** Special handling for composite transform. If all transforms
   * are linear, then return category Linear. Otherwise if all
   * transforms set to optimize are DisplacementFields, then
   * return DisplacementField category. */
  virtual TransformCategoryType GetTransformCategory() const ITK_OVERRIDE;

  /** Get/Set Parameter functions work on the current list of transforms
      that are set to be optimized (active) using the
      'Set[Nth|All]TransformToOptimze' routines.
      The parameter data from each active transform is
      concatenated into a single ParametersType object.
      \note The sub-transforms are read in \b reverse queue order,
      so the returned array is ordered in the same way. That is,
      the last sub-transform to be added is returned first in the
      parameter array. This is the opposite of what's done in the
      parent MultiTransform class. */
  virtual const ParametersType & GetParameters() const ITK_OVERRIDE;

  /* SetParameters only for transforms that are set to be optimized
   * See GetParameters() for parameter ordering. */
  virtual void  SetParameters(const ParametersType & p) ITK_OVERRIDE;

  /* GetFixedParameters only for transforms that are set to be optimized
   * See GetParameters() for parameter ordering. */
  virtual const FixedParametersType & GetFixedParameters() const ITK_OVERRIDE;

  /* SetFixedParameters only for transforms that are set to be optimized.
   * See GetParameters() for parameter ordering. */
  virtual void  SetFixedParameters(const FixedParametersType & fixedParameters) ITK_OVERRIDE;

  /* Get total number of parameters for transforms that are set to be
   * optimized */
  virtual NumberOfParametersType GetNumberOfParameters(void) const ITK_OVERRIDE;

  /* Get total number of local parameters for transforms that are set
   * to be optimized */
  virtual NumberOfParametersType GetNumberOfLocalParameters(void) const ITK_OVERRIDE;

  /* Get total number of fixed parameters for transforms that are set
   * to be optimized */
  virtual NumberOfParametersType GetNumberOfFixedParameters(void) const ITK_OVERRIDE;

  /** Update the transform's parameters by the values in \c update.
   * See GetParameters() for parameter ordering. */
  virtual void UpdateTransformParameters( const DerivativeType & update, ScalarType factor = 1.0 ) ITK_OVERRIDE;

  /**
   * Flatten the transform queue such that there are no nested composite transforms.
   */
  virtual void FlattenTransformQueue();

  /**
   * Compute the Jacobian with respect to the parameters for the compositie
   * transform using Jacobian rule. See comments in the implementation.
   */
  virtual void ComputeJacobianWithRespectToParameters(const InputPointType  & p, JacobianType & j) const ITK_OVERRIDE;

  /**
   * Expanded interface to Compute the Jacobian with respect to the parameters for the compositie
   * transform using Jacobian rule. This version takes in temporary
   * variables to avoid excessive constructions.
   * NOTE: outJacobian and jacobianWithRespectToPosition MUST be sized
   * prior to the call; outJacobian's size should be
   * [NDimensions, this->GetNumberOfLocalParameters() ]
   * jacobianWithRespectToPosition size == [ NDimensions, NDimensions ]
   */
  virtual void ComputeJacobianWithRespectToParametersCachedTemporaries( const InputPointType & p, JacobianType & outJacobian, JacobianType & jacobianWithRespectToPosition ) const ITK_OVERRIDE;

protected:
  CompositeTransform();
  virtual ~CompositeTransform() ITK_OVERRIDE;
  virtual void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE;

  /** Clone the current transform */
  virtual typename LightObject::Pointer InternalClone() const ITK_OVERRIDE;

  virtual void PushFrontTransform( TransformTypePointer t  ) ITK_OVERRIDE
  {
    Superclass::PushFrontTransform( t );
    /* Add element to list of flags, and set true by default */
    this->m_TransformsToOptimizeFlags.push_front( true );
  }

  virtual void PushBackTransform( TransformTypePointer t  ) ITK_OVERRIDE
  {
    Superclass::PushBackTransform( t );
    /* Add element to list of flags, and set true by default */
    this->m_TransformsToOptimizeFlags.push_back( true );
  }

  virtual void PopFrontTransform() ITK_OVERRIDE
  {
    Superclass::PopFrontTransform();
    this->m_TransformsToOptimizeFlags.pop_front();
  }

  virtual void PopBackTransform() ITK_OVERRIDE
  {
    Superclass::PopBackTransform();
    this->m_TransformsToOptimizeFlags.pop_back();
  }

  /** Get a list of transforms to optimize. Helper function. */
  TransformQueueType & GetTransformsToOptimizeQueue() const;

  mutable TransformQueueType            m_TransformsToOptimizeQueue;
  mutable TransformsToOptimizeFlagsType m_TransformsToOptimizeFlags;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(CompositeTransform);

  mutable ModifiedTimeType m_PreviousTransformsToOptimizeUpdateTime;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCompositeTransform.hxx"
#endif

#endif // itkCompositeTransform_h
