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
#ifndef itkMultiTransform_h
#define itkMultiTransform_h

#include "itkTransform.h"

#include <deque>

namespace itk
{

/** \class MultiTransform
 * \brief This abstract class contains a list of transforms and provides basic methods.
 *
 * This abstract base class is used by classes that operate on a list of
 * sub-transforms. The sub-transforms can have a different dimensionality
 * than the container transform.
 *
 * Transforms are stored in a container (queue), in the following order:
 *    \f$ T_0, T_1, ... , T_N-1 \f$
 *
 * Transforms are added via a single method, AddTransform(). This adds the
 * transforms to the back of the queue. A single method for adding transforms
 * is meant to simplify the interface and prevent errors.
 *
 * Inverse
 *  todo
 *
 * TODO
 *
 * Interface Issues/Comments
 * x The PushFrontTransform and PushBackTransform methods are protected to
 *   force the user to use the AddTransform method, forcing the order of
 *   transforms. Are there use cases where the user would *need* to insert
 *   transforms at the front of the queue? Or at arbitrary positions?
 *
 * GetParameters efficiency optimization
 *  Can we optimize this to only query the sub-transforms when the params
 *  in the sub transforms have changed since the previous call? Can't use
 *  Modified time b/c that will get updated in sub-transforms with every
 *  call to SetParameters. Is this worth worrying about? i.e. how much time
 *  will it take in the overall registration process? Probably very little.
 *
 * \ingroup ITKTransform
 */
template <typename TParametersValueType = double, unsigned int VDimension = 3, unsigned int VSubDimensions = VDimension>
class ITK_TEMPLATE_EXPORT MultiTransform : public Transform<TParametersValueType, VDimension, VSubDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MultiTransform);

  /** Standard class type aliases. */
  using Self = MultiTransform;
  using Superclass = Transform<TParametersValueType, VDimension, VSubDimensions>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MultiTransform, Transform);

  /** Sub transform type **/
  using TransformType = Transform<TParametersValueType, VSubDimensions, VSubDimensions>;
  using TransformTypePointer = typename TransformType::Pointer;

  /* Types common to both container and sub transforms */

  /** Parameters type. */
  using typename Superclass::ParametersType;
  using typename Superclass::ParametersValueType;
  using typename Superclass::FixedParametersType;
  using typename Superclass::FixedParametersValueType;
  using ScalarType = ParametersValueType;
  /** Derivative type */
  using typename Superclass::DerivativeType;
  /** Jacobian type. */
  using typename Superclass::JacobianType;
  using typename Superclass::JacobianPositionType;
  using typename Superclass::InverseJacobianPositionType;
  /** Transform category type. */
  using typename Superclass::TransformCategoryEnum;

  /* Types relative to the container transform. */

  /** InverseTransform type. */
  using typename Superclass::InverseTransformBasePointer;

  /** Standard coordinate point type for this class. */
  using typename Superclass::InputPointType;
  using typename Superclass::OutputPointType;
  /** Standard vector type for this class. */
  using typename Superclass::InputVectorType;
  using typename Superclass::OutputVectorType;
  /** Standard covariant vector type for this class */
  using typename Superclass::InputCovariantVectorType;
  using typename Superclass::OutputCovariantVectorType;
  /** Standard vnl_vector type for this class. */
  using typename Superclass::InputVnlVectorType;
  using typename Superclass::OutputVnlVectorType;
  /** Standard Vectorpixel type for this class */
  using typename Superclass::InputVectorPixelType;
  using typename Superclass::OutputVectorPixelType;
  /** Standard DiffusionTensor3D type alias for this class */
  using typename Superclass::InputDiffusionTensor3DType;
  using typename Superclass::OutputDiffusionTensor3DType;
  /** Standard SymmetricSecondRankTensor type alias for this class */
  using typename Superclass::InputSymmetricSecondRankTensorType;
  using typename Superclass::OutputSymmetricSecondRankTensorType;

  /* Types relative to the sub transform type. */

  /** InverseTransform type. */
  using SubTransformInverseTransformBasePointer = typename TransformType::InverseTransformBasePointer;

  /** Transform queue type */
  using TransformQueueType = std::deque<TransformTypePointer>;

  /** The number of parameters defining this transform. */
  using typename Superclass::NumberOfParametersType;

  /** Dimension of the domain spaces. */
  static constexpr unsigned int InputDimension = VDimension;
  static constexpr unsigned int OutputDimension = VDimension;

  static constexpr unsigned int SubInputDimension = VSubDimensions;
  static constexpr unsigned int SubOutputDimension = VSubDimensions;

  /** Functionality for sub transforms */

  /** Add transforms to the queue, as stack.
   *  Most-recently added transform is always at back of queue, index N-1.
   */
  virtual void
  AddTransform(TransformType * t)
  {
    this->PushBackTransform(t);
  }

  /** Same as AddTransform */
  virtual void
  AppendTransform(TransformType * t)
  {
    this->AddTransform(t);
  }

  /** Add transform to the front of the stack */
  virtual void
  PrependTransform(TransformType * t)
  {
    this->PushFrontTransform(t);
  }

  /** Remove transform from the back of the queue, index N-1 */
  virtual void
  RemoveTransform()
  {
    this->PopBackTransform();
  }

  /** Get transforms at the front and the back of the queue */
  virtual const TransformType *
  GetFrontTransform() const
  {
    return this->m_TransformQueue.front().GetPointer();
  }

  virtual const TransformType *
  GetBackTransform() const
  {
    return this->m_TransformQueue.back().GetPointer();
  }

  virtual const TransformTypePointer
  GetNthTransform(SizeValueType n) const
  {
    // NOTE: By returning a smart pointer type, the use of this function can
    //      be a significant bottleneck in multithreaded applications.
    return this->m_TransformQueue[n];
  }

  /** Get the Nth transform.
   * \warning No bounds checking is performed. */
  virtual TransformType *
  GetNthTransformModifiablePointer(const SizeValueType n) const
  {
    return this->m_TransformQueue[n].GetPointer();
  }

  virtual const TransformType *
  GetNthTransformConstPointer(const SizeValueType n) const
  {
    return this->m_TransformQueue[n].GetPointer();
  }

  /** Access transform queue */
  virtual const TransformQueueType &
  GetTransformQueue() const
  {
    return this->m_TransformQueue;
  }

  /** Misc. functionality */
  virtual bool
  IsTransformQueueEmpty() const
  {
    return this->m_TransformQueue.empty();
  }

  /** Return the number of sub-transforms. */
  virtual SizeValueType
  GetNumberOfTransforms() const
  {
    return static_cast<SizeValueType>(this->m_TransformQueue.size());
  }

  /** Clear the transform queue. */
  virtual void
  ClearTransformQueue()
  {
    this->m_TransformQueue.clear();
    this->Modified();
  }

  /** If all sub-transforms are linear, then the multi-transform is linear. */
  bool
  IsLinear() const override;

  /** If all sub-transforms are of the same category, return that category.
   * Otherwise return UnknownTransformCategory. */
  TransformCategoryEnum
  GetTransformCategory() const override;

  /** Get/Set Parameter functions work on all sub-transforms.
      The parameter data from each sub-transform is
      concatenated into a single ParametersType object.
      \note The sub-transforms are read in forward queue order,
      so the returned array is ordered in the same way. That is,
      first sub-transform to be added is returned first in the
      parameter array.*/
  const ParametersType &
  GetParameters() const override;

  /* SetParameters for all sub-transforms.
   * See GetParameters() for parameter ordering. */
  void
  SetParameters(const ParametersType & inputParameters) override;

  /* GetFixedParameters for all sub-transforms.
   * See GetParameters() for parameter ordering. */
  const FixedParametersType &
  GetFixedParameters() const override;

  /* SetFixedParameters for all sub-transforms.
   * See GetParameters() for parameter ordering. */
  void
  SetFixedParameters(const FixedParametersType & inputParameters) override;

  /* Get total number of parameters. Sum of all sub-transforms. */
  NumberOfParametersType
  GetNumberOfParameters() const override;

  /* Get total number of local parameters, the sum of all sub-transforms. */
  NumberOfParametersType
  GetNumberOfLocalParameters() const override;

  /* Get total number of fixed parameters, the sum of all sub-transforms. */
  NumberOfParametersType
  GetNumberOfFixedParameters() const override;

  /** Update the transform's parameters by the values in \c update.
   * See GetParameters() for parameter ordering. */
  void
  UpdateTransformParameters(const DerivativeType & update, ScalarType factor = 1.0) override;

  /** Returns a boolean indicating whether it is possible or not to compute the
   * inverse of this current Transform. If it is possible, then the inverse of
   * the transform is returned in the inverseTransform variable passed by the user.
   * The inverse consists of the inverse of each sub-transform, in the same order
   * as the forward transforms. */
  bool
  GetInverse(Self * inverse) const;

  /** Flatten the transform queue such that there are no nested composite transforms. */
  // TODO - what do we need here?
  //  virtual void FlattenTransformQueue();

protected:
  MultiTransform();
  ~MultiTransform() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  virtual void
  PushFrontTransform(TransformTypePointer t)
  {
    this->m_TransformQueue.push_front(t);
    this->Modified();
  }

  virtual void
  PushBackTransform(TransformTypePointer t)
  {
    this->m_TransformQueue.push_back(t);
    this->Modified();
  }

  virtual void
  PopFrontTransform()
  {
    this->m_TransformQueue.pop_front();
    this->Modified();
  }

  virtual void
  PopBackTransform()
  {
    this->m_TransformQueue.pop_back();
    this->Modified();
  }

  /** Transform container object. */
  mutable TransformQueueType m_TransformQueue;

  /** Cache to save time returning the number of local parameters */
  mutable NumberOfParametersType m_NumberOfLocalParameters;
  mutable ModifiedTimeType       m_LocalParametersUpdateTime;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMultiTransform.hxx"
#endif

#endif // itkMultiTransform_h
