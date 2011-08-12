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
#ifndef __itkCompositeTransform_h
#define __itkCompositeTransform_h

#include "itkTransform.h"

#include <deque>

namespace itk
{

/** \class CompositeTransform
 * \brief This class contains a list of transforms and concatenates them by composition.
 *
 * This class concatenates transforms by means of composition:
 *    \f$ T_1 o T_0 = T_1(T_0(x)) \f$
 * Transforms are stored in a container (queue), in the following order:
 *    \f$ T_0, T_1, ... , T_N-1 \f$
 * Transforms are added via a single method, AddTransform(). This adds the
 * transforms to the back of the queue. A single method for adding transforms
 * is meant to simplify the interface and prevent errors.
 * One use of the class is to optimize only a subset of included transforms.
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
 * GetJacobian, GetFixedParameters, and SetFixedParameters all query these
 * flags and include only those transforms whose corresponsing flag is set.
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
 * \ingroup Transforms
 *
 * \ingroup ITKTransform
 */
template
<class TScalar = double, unsigned int NDimensions = 3>
class ITK_EXPORT CompositeTransform :
  public Transform<TScalar, NDimensions, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef CompositeTransform                           Self;
  typedef Transform<TScalar, NDimensions, NDimensions> Superclass;
  typedef SmartPointer<Self>                           Pointer;
  typedef SmartPointer<const Self>                     ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( CompositeTransform, Transform );

  /** New macro for creation of through a Smart Pointer */
 itkSimpleNewMacro( Self );

  /** Leave CreateAnother undefined. To fully implement here, it must be
   * sure to copy all members. It may be called from transform-cloning
   * that only copies parameters, so override here to prevent
   * its use without copying full members. */
  virtual::itk::LightObject::Pointer CreateAnother(void) const
    {
    itkExceptionMacro("CreateAnother unimplemented. See source comments.");
    }

  /** Component transform type **/
  typedef Superclass                    TransformType;
  typedef typename Superclass::Pointer  TransformTypePointer;
  /** InverseTransform type. */
  typedef typename Superclass::InverseTransformBasePointer
                                        InverseTransformBasePointer;
  /** Scalar type. */
  typedef typename Superclass::ScalarType           ScalarType;
  /** Parameters type. */
  typedef typename Superclass::ParametersType       ParametersType;
  typedef typename Superclass::ParametersValueType  ParametersValueType;
  /** Derivative type */
  typedef typename Superclass::DerivativeType       DerivativeType;
  /** Jacobian type. */
  typedef typename Superclass::JacobianType         JacobianType;
  /** Standard coordinate point type for this class. */
  typedef typename Superclass::InputPointType       InputPointType;
  typedef typename Superclass::OutputPointType      OutputPointType;
  /** Standard vector type for this class. */
  typedef typename Superclass::InputVectorType      InputVectorType;
  typedef typename Superclass::OutputVectorType     OutputVectorType;
  /** Standard covariant vector type for this class */
  typedef typename Superclass::InputCovariantVectorType
                                                    InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType
                                                    OutputCovariantVectorType;
  /** Standard vnl_vector type for this class. */
  typedef typename Superclass::InputVnlVectorType   InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType  OutputVnlVectorType;
  /** Transform queue type */
  typedef std::deque<TransformTypePointer>          TransformQueueType;
  /** Optimization flags queue type */
  typedef std::deque<bool>                       TransformsToOptimizeFlagsType;

  /** Dimension of the domain spaces. */
  itkStaticConstMacro( InputDimension, unsigned int, NDimensions );
  itkStaticConstMacro( OutputDimension, unsigned int, NDimensions );

  /** Functionality for sub transforms */

  /** Add transforms to the queue, as stack. Only allow one method for simplicity.
   *  Most-recently added transform is always at back of queue, index N-1.
   */
  void AddTransform( TransformType *t  )
  {
    this->PushBackTransform( t ); /* Also adds to TransformsToOptimize list */
  }

  /** See transforms at the front and the back of the queue */
  const
  TransformTypePointer GetFrontTransform()
    {
    return this->m_TransformQueue.front();
    }
  const
  TransformTypePointer GetBackTransform()
    {
    return this->m_TransformQueue.back();
    }

  const
  TransformTypePointer GetNthTransform( size_t n ) const
  {
    return this->m_TransformQueue[n];
  }

  /** Active Transform state manipulation */

  void SetNthTransformToOptimize( size_t i, bool state )
  {
    this->m_TransformsToOptimizeFlags.at(i)=state;
    this->Modified();
  }

  void SetNthTransformToOptimizeOn( size_t i )
  {
    this->SetNthTransformToOptimize( i, true );
  }

  void SetNthTransformToOptimizeOff( size_t i )
  {
    this->SetNthTransformToOptimize( i, false );
  }

  void SetAllTransformsToOptimize( bool state )
  {
    this->m_TransformsToOptimizeFlags.assign(
      this->m_TransformsToOptimizeFlags.size(), state );
    this->Modified();
  }

  void SetAllTransformsToOptimizeOn()
  {
    this->SetAllTransformsToOptimize( true );
  }

  void SetAllTransformsToOptimizeOff()
  {
    this->SetAllTransformsToOptimize( false );
  }

  /* With AddTransform() as the only way to add a transform, we
   * can have this method to easily allow user to optimize only
   * the transform added most recenlty. */
  void SetOnlyMostRecentTransformToOptimizeOn()
  {
    this->SetAllTransformsToOptimize( false );
    this->SetNthTransformToOptimizeOn( this->GetNumberOfTransforms()-1 );
  }

  /* Get whether the Nth transform is set to be optimzied */
  /* NOTE: ambiguous function name here - are we getting if the Nth transform
      is set to be optimized, or the Nth of the transforms that are set to be
      optimized? */
  bool GetNthTransformToOptimize( size_t i ) const
  {
    return this->m_TransformsToOptimizeFlags.at(i);
  }

  /** Access transform queue */
  const TransformQueueType & GetTransformQueue() const
  { return this->m_TransformQueue; }


  /** Access optimize flags */
  const TransformsToOptimizeFlagsType & GetTransformsToOptimizeFlags() const
  { return this->m_TransformsToOptimizeFlags; }

  /** Misc. functionality */
  bool IsTransformQueueEmpty()
  {
    return this->m_TransformQueue.empty();
  }

  size_t GetNumberOfTransforms() const
  {
    return this->m_TransformQueue.size();
  }

  void ClearTransformQueue()
  {
    this->m_TransformQueue.clear();
    this->m_TransformsToOptimizeFlags.clear();
    this->Modified();
  }

  /** Return an inverse of this transform. */
  bool GetInverse( Self *inverse ) const;

  virtual InverseTransformBasePointer GetInverseTransform() const;

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
  virtual OutputPointType
    TransformPoint( const InputPointType &inputPoint ) const;
  /* Note: why was the 'isInsideTransformRegion' flag used below?
  {
    bool isInside = true;

    return this->TransformPoint( inputPoint, isInside );
  }
  virtual OutputPointType TransformPoint( const InputPointType& thisPoint,
                                          bool &isInsideTransformRegion ) const;
  */
  /**  Method to transform a vector. */
  virtual OutputVectorType TransformVector(const InputVectorType &) const
  {
    itkExceptionMacro( "TransformVector unimplemented" );
  }

  /**  Method to transform a vnl_vector. */
  virtual OutputVnlVectorType TransformVector(const InputVnlVectorType &) const
  {
    itkExceptionMacro( "TransformVector unimplemented" );
  }

  /**  Method to transform a CovariantVector. */
  virtual OutputCovariantVectorType
  TransformCovariantVector(const InputCovariantVectorType &) const
  {
    itkExceptionMacro( "TransformCovariantVector unimplemented" );
  }

  virtual bool IsLinear() const;

  /**
   * Compute the jacobian with respect to the parameters.
   */
  virtual const JacobianType & GetJacobian(const InputPointType  &) const;

  /**
   * Compute the Jacobian with respect to the parameters for the compositie
   * transform using Jacobian rule. See comments in the implementation.
   */
  virtual void GetJacobianWithRespectToParameters(const InputPointType  &p,
                                                  JacobianType &j) const;

  virtual void GetJacobianWithRespectToPosition(const InputPointType &,
                                                  JacobianType &) const
  {
    itkExceptionMacro( "GetJacobianWithRespectToPosition not yet implemented "
                       "for " << this->GetNameOfClass() );
  }

  /** Get/Set Parameter functions work on the current list of transforms
      that are set to be optimized (active) using the
      'Set[Nth|All]TransformToOptimze' routines.
      The parameter data from each active transform is
      concatenated into a single ParametersType object. */
  virtual const ParametersType & GetParameters(void) const;

  /* SetParameters only for transforms that are set to be optimized */
  virtual void  SetParameters(const ParametersType & p);

  /* GetFixedParameters only for transforms that are set to be optimized */
  virtual const ParametersType & GetFixedParameters(void) const;

  /* SetFixedParameters only for transforms that are set to be optimized */
  virtual void  SetFixedParameters(const ParametersType & fixedParameters);

  /* Get total number of parameters for transforms that are set to be
   * optimized */
  virtual unsigned int GetNumberOfParameters(void) const;

  /* Get total number of local parameters for transforms that are set
   * to be optimized */
  virtual unsigned int GetNumberOfLocalParameters(void) const;

  /* Get total number of fixed parameters for transforms that are set
   * to be optimized */
  virtual unsigned int GetNumberOfFixedParameters(void) const;

  /* Prepare the transform for use, e.g. in registration framework.
   * Must be called before registration to optimize parameter storage
   * for more efficient operation, particularly with high-dimensionality
   * sub-transforms. */
   //virtual void PrepareForUse(void);

  /** Update the transform's parameters by the values in \c update.
   * We assume \c update is of the same length as Parameters. Throw
   * exception otherwise.
   * \c factor is a scalar multiplier for each value in update.
   * SetParameters is called on each sub-transform, to allow transforms
   * to perform any required operations on the update parameters, typically
   * a converion to member variables for use in TransformPoint.
   */
  virtual void UpdateTransformParameters( DerivativeType & update,
                                          ScalarType  factor = 1.0 );

  /** Indicates if this transform is a "global" transform
   *  e.g. an affine transform or a local one, e.g. a deformation field.
   *  Returns true if only all sub-transforms that are set to be
   *  optimized return true.
   */
  virtual bool HasLocalSupport() const;

protected:
  CompositeTransform();
  virtual ~CompositeTransform();
  void PrintSelf( std::ostream& os, Indent indent ) const;

  void PushFrontTransform( TransformTypePointer t  )
  {
    this->m_TransformQueue.push_front( t );
    /* Add element to list of flags, and set true by default */
    this->m_TransformsToOptimizeFlags.push_front( true );
    this->Modified();
  }

  void PushBackTransform( TransformTypePointer t  )
  {
    this->m_TransformQueue.push_back( t );
    /* Add element to list of flags, and set true by default */
    this->m_TransformsToOptimizeFlags.push_back( true );
    this->Modified();
  }

  /** Unify the parameter memory be copying all sub-transform parameters
   * into a single memory block, and redirecting sub-transform's parameter
   * memory to point within this block.
   * \warning This will temporarily use twice the memory of all
   * sub-transform. */
  //void UnifyParameterMemory(void);

  /** Transform container object. */
  mutable TransformQueueType m_TransformQueue;

  /** Get a list of transforms to optimize. Helper function. */
  TransformQueueType& GetTransformsToOptimizeQueue() const;

  mutable TransformQueueType            m_TransformsToOptimizeQueue;
  mutable TransformsToOptimizeFlagsType m_TransformsToOptimizeFlags;
private:
  CompositeTransform( const Self & ); //purposely not implemented
  void operator=( const Self& );      //purposely not implemented

  mutable unsigned long m_PreviousTransformsToOptimizeUpdateTime;
};

} // end namespace itk

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkCompositeTransform+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkCompositeTransform.hxx"
#endif

#endif // __itkCompositeTransform_h
