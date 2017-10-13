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
template
<typename TParametersValueType=double, unsigned int NDimensions=3, unsigned int NSubDimensions=NDimensions>
class ITK_TEMPLATE_EXPORT MultiTransform :
  public Transform<TParametersValueType, NDimensions, NSubDimensions>
{
public:
  /** Standard class typedefs. */
  typedef MultiTransform                                               Self;
  typedef Transform<TParametersValueType, NDimensions, NSubDimensions> Superclass;
  typedef SmartPointer<Self>                                           Pointer;
  typedef SmartPointer<const Self>                                     ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( MultiTransform, Transform );

  /** Sub transform type **/
  typedef Transform<TParametersValueType, NSubDimensions, NSubDimensions > TransformType;
  typedef typename TransformType::Pointer                     TransformTypePointer;

  /* Types common to both container and sub transforms */

  /** Parameters type. */
  typedef typename Superclass::ParametersType           ParametersType;
  typedef typename Superclass::ParametersValueType      ParametersValueType;
  typedef typename Superclass::FixedParametersType      FixedParametersType;
  typedef typename Superclass::FixedParametersValueType FixedParametersValueType;
  typedef ParametersValueType                           ScalarType;
  /** Derivative type */
  typedef typename Superclass::DerivativeType           DerivativeType;
  /** Jacobian type. */
  typedef typename Superclass::JacobianType             JacobianType;
  /** Transform category type. */
  typedef typename Superclass::TransformCategoryType    TransformCategoryType;

  /* Types relative to the container transform. */

  /** InverseTransform type. */
  typedef typename Superclass::InverseTransformBasePointer InverseTransformBasePointer;

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
  typedef typename Superclass::InputSymmetricSecondRankTensorType InputSymmetricSecondRankTensorType;
  typedef typename Superclass::OutputSymmetricSecondRankTensorType  OutputSymmetricSecondRankTensorType;

  /* Types relative to the sub transform type. */

  /** InverseTransform type. */
  typedef typename TransformType::InverseTransformBasePointer    SubTransformInverseTransformBasePointer;

  /** Transform queue type */
  typedef std::deque<TransformTypePointer>  TransformQueueType;

  /** The number of parameters defininig this transform. */
  typedef typename Superclass::NumberOfParametersType NumberOfParametersType;

  /** Dimension of the domain spaces. */
  itkStaticConstMacro( InputDimension, unsigned int, NDimensions );
  itkStaticConstMacro( OutputDimension, unsigned int, NDimensions );

  itkStaticConstMacro( SubInputDimension, unsigned int, NSubDimensions );
  itkStaticConstMacro( SubOutputDimension, unsigned int, NSubDimensions );

  /** Functionality for sub transforms */

  /** Add transforms to the queue, as stack.
   *  Most-recently added transform is always at back of queue, index N-1.
   */
  virtual void AddTransform( TransformType *t  )
  {
    this->PushBackTransform( t );
  }

  /** Same as AddTransform */
  virtual void AppendTransform( TransformType *t  )
  {
    this->AddTransform( t );
  }

  /** Add transform to the front of the stack */
  virtual void PrependTransform( TransformType *t  )
  {
    this->PushFrontTransform( t );
  }

  /** Remove transform from the back of the queue, index N-1 */
  virtual void RemoveTransform()
  {
    this->PopBackTransform();
  }

  /** Get transforms at the front and the back of the queue */
  virtual const
  TransformType * GetFrontTransform() const
  {
    return this->m_TransformQueue.front().GetPointer();
  }

  virtual const
  TransformType * GetBackTransform() const
  {
    return this->m_TransformQueue.back().GetPointer();
  }

  virtual const
  TransformTypePointer GetNthTransform( SizeValueType n ) const
  {
    //NOTE: By returning a smart pointer type, the use of this function can
    //      be a significant bottleneck in multithreaded applications.
    return this->m_TransformQueue[n];
  }

  /** Get the Nth transform.
   * \warning No bounds checking is performed. */
  virtual
  TransformType * GetNthTransformModifiablePointer( const SizeValueType n ) const
  {
    return this->m_TransformQueue[n].GetPointer();
  }

  virtual const
  TransformType * GetNthTransformConstPointer( const SizeValueType n ) const
  {
    return this->m_TransformQueue[n].GetPointer();
  }

  /** Access transform queue */
  virtual const TransformQueueType & GetTransformQueue() const
  {
    return this->m_TransformQueue;
  }

  /** Misc. functionality */
  virtual bool IsTransformQueueEmpty() const
  {
    return this->m_TransformQueue.empty();
  }

  /** Return the number of sub-transforms. */
  virtual SizeValueType GetNumberOfTransforms() const
  {
    return static_cast<SizeValueType>(this->m_TransformQueue.size());
  }

  /** Clear the transform queue. */
  virtual void ClearTransformQueue()
  {
    this->m_TransformQueue.clear();
    this->Modified();
  }

  /** If all sub-transforms are linear, then the multi-transform is linear. */
  virtual bool IsLinear() const ITK_OVERRIDE;

  /** If all sub-transforms are of the same category, return that category.
   * Otherwise return UnknownTransformCategory. */
  virtual TransformCategoryType GetTransformCategory() const ITK_OVERRIDE;

  /** Get/Set Parameter functions work on all sub-transforms.
      The parameter data from each sub-transform is
      concatenated into a single ParametersType object.
      \note The sub-transforms are read in forward queue order,
      so the returned array is ordered in the same way. That is,
      first sub-transform to be added is returned first in the
      parameter array.*/
  virtual const ParametersType & GetParameters() const ITK_OVERRIDE;

  /* SetParameters for all sub-transforms.
   * See GetParameters() for parameter ordering. */
  virtual void  SetParameters(const ParametersType & p) ITK_OVERRIDE;

  /* GetFixedParameters for all sub-transforms.
   * See GetParameters() for parameter ordering. */
  virtual const FixedParametersType & GetFixedParameters() const ITK_OVERRIDE;

  /* SetFixedParameters for all sub-transforms.
   * See GetParameters() for parameter ordering. */
  virtual void SetFixedParameters(const FixedParametersType & fixedParameters) ITK_OVERRIDE;

  /* Get total number of parameters. Sum of all sub-transforms. */
  virtual NumberOfParametersType GetNumberOfParameters() const ITK_OVERRIDE;

  /* Get total number of local parameters, the sum of all sub-transforms. */
  virtual NumberOfParametersType GetNumberOfLocalParameters() const ITK_OVERRIDE;

  /* Get total number of fixed parameters, the sum of all sub-transforms. */
  virtual NumberOfParametersType GetNumberOfFixedParameters() const ITK_OVERRIDE;

  /** Update the transform's parameters by the values in \c update.
   * See GetParameters() for parameter ordering. */
  virtual void UpdateTransformParameters( const DerivativeType & update, ScalarType  factor = 1.0 ) ITK_OVERRIDE;

  /** Returns a boolean indicating whether it is possible or not to compute the
   * inverse of this current Transform. If it is possible, then the inverse of
   * the transform is returned in the inverseTransform variable passed by the user.
   * The inverse consists of the inverse of each sub-transform, in the same order
   * as the forward transforms. */
  bool GetInverse( Self *inverse ) const;

  /** Flatten the transform queue such that there are no nested composite transforms. */
//TODO - what do we need here?
//  virtual void FlattenTransformQueue();

protected:
  MultiTransform();
  virtual ~MultiTransform() ITK_OVERRIDE;
  virtual void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE;

  virtual void PushFrontTransform( TransformTypePointer t  )
  {
    this->m_TransformQueue.push_front( t );
    this->Modified();
  }

  virtual void PushBackTransform( TransformTypePointer t  )
  {
    this->m_TransformQueue.push_back( t );
    this->Modified();
  }

  virtual void PopFrontTransform()
  {
    this->m_TransformQueue.pop_front();
    this->Modified();
  }

  virtual void PopBackTransform()
  {
    this->m_TransformQueue.pop_back();
    this->Modified();
  }

  /** Transform container object. */
  mutable TransformQueueType m_TransformQueue;

  /** Cache to save time returning the number of local parameters */
  mutable NumberOfParametersType  m_NumberOfLocalParameters;
  mutable ModifiedTimeType        m_LocalParametersUpdateTime;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MultiTransform);

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMultiTransform.hxx"
#endif

#endif // itkMultiTransform_h
