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
#ifndef itkMatrixOffsetTransformBase_h
#define itkMatrixOffsetTransformBase_h


#include "itkMacro.h"
#include "itkMatrix.h"
#include "itkTransform.h"

#include <iostream>

namespace itk
{

/* MatrixOrthogonalityTolerance is a utility to
 * allow setting the tolerance limits used for
 * checking if a matrix meet the orthogonality
 * constraints of being a rigid rotation matrix.
 * The tolerance needs to be different for
 * matricies of type float vs. double.
 */
template<typename T>
class MatrixOrthogonalityTolerance;

template <>
class ITK_TEMPLATE_EXPORT MatrixOrthogonalityTolerance<double>
{
public:
  static double GetTolerance() { return 1e-10; }
};

template <>
class ITK_TEMPLATE_EXPORT MatrixOrthogonalityTolerance<float>
{
public:
  static float GetTolerance() { return 1e-5f; }
};

/** \class MatrixOffsetTransformBase
 * \brief Matrix and Offset transformation of a vector space (e.g. space coordinates)
 *
 * This class serves as a base class for transforms that can be expressed
 * as a linear transformation plus a constant offset (e.g., affine, similarity
 * and rigid transforms).   This base class also provides the concept of
 * using a center of rotation and a translation instead of an offset.
 *
 * As derived instances of this class are specializations of an affine
 * transform, any two of these transformations may be composed and the result
 * is an affine transformation.  However, the order is important.
 * Given two affine transformations T1 and T2, we will say that
 * "precomposing T1 with T2" yields the transformation which applies
 * T1 to the source, and then applies T2 to that result to obtain the
 * target.  Conversely, we will say that "postcomposing T1 with T2"
 * yields the transformation which applies T2 to the source, and then
 * applies T1 to that result to obtain the target.  (Whether T1 or T2
 * comes first lexicographically depends on whether you choose to
 * write mappings from right-to-left or vice versa; we avoid the whole
 * problem by referring to the order of application rather than the
 * textual order.)
 *
 * \tparam TParametersValueType The type to be used for scalar numeric values.  Either
 *    float or double.
 *
 * \tparam NInputDimensions   The number of dimensions of the input vector space.
 *
 * \tparam NOutputDimensions  The number of dimensions of the output vector space.
 *
 * This class provides several methods for setting the matrix and offset
 * defining the transform. To support the registration framework, the
 * transform parameters can also be set as an Array<TParametersValueType> of size
 * (NInputDimension + 1) * NOutputDimension using method SetParameters().
 * The first (NOutputDimension x NInputDimension) parameters defines the
 * matrix in row-major order (where the column index varies the fastest).
 * The last NOutputDimension parameters defines the translation
 * in each dimensions.
 *
 * \ingroup ITKTransform
 */

template<typename TParametersValueType=double,
         unsigned int NInputDimensions = 3,
         unsigned int NOutputDimensions = 3>
class ITK_TEMPLATE_EXPORT MatrixOffsetTransformBase :
  public Transform<TParametersValueType, NInputDimensions, NOutputDimensions>
{
public:
  /** Standard typedefs   */
  typedef MatrixOffsetTransformBase Self;
  typedef Transform<TParametersValueType,
                    NInputDimensions,
                    NOutputDimensions>        Superclass;

  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(MatrixOffsetTransformBase, Transform);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  /** Dimension of the domain space. */
  itkStaticConstMacro(InputSpaceDimension, unsigned int, NInputDimensions);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, NOutputDimensions);
  itkStaticConstMacro( ParametersDimension, unsigned int,
                       NOutputDimensions * ( NInputDimensions + 1 ) );

  /** Parameters Type   */
  typedef typename Superclass::FixedParametersType      FixedParametersType;
  typedef typename Superclass::FixedParametersValueType FixedParametersValueType;
  typedef typename Superclass::ParametersType           ParametersType;
  typedef typename Superclass::ParametersValueType      ParametersValueType;

  /** Jacobian Type   */
  typedef typename Superclass::JacobianType JacobianType;

  /** Transform category type. */
  typedef typename Superclass::TransformCategoryType TransformCategoryType;

  /** Standard scalar type for this class */
  typedef typename Superclass::ScalarType ScalarType;

  /** Standard vector type for this class   */
  typedef Vector<TParametersValueType,
                 itkGetStaticConstMacro(InputSpaceDimension)>  InputVectorType;
  typedef Vector<TParametersValueType,
                 itkGetStaticConstMacro(OutputSpaceDimension)> OutputVectorType;
  typedef typename OutputVectorType::ValueType OutputVectorValueType;

  /** Standard covariant vector type for this class   */
  typedef CovariantVector<TParametersValueType,
                          itkGetStaticConstMacro(InputSpaceDimension)>
  InputCovariantVectorType;
  typedef CovariantVector<TParametersValueType,
                          itkGetStaticConstMacro(OutputSpaceDimension)>
  OutputCovariantVectorType;

  typedef typename Superclass::InputVectorPixelType  InputVectorPixelType;
  typedef typename Superclass::OutputVectorPixelType OutputVectorPixelType;

  /** Standard diffusion tensor type for this class */
  typedef typename Superclass::InputDiffusionTensor3DType
  InputDiffusionTensor3DType;
  typedef typename Superclass::OutputDiffusionTensor3DType
  OutputDiffusionTensor3DType;

  /** Standard tensor type for this class */
  typedef typename Superclass::InputSymmetricSecondRankTensorType
  InputSymmetricSecondRankTensorType;
  typedef typename Superclass::OutputSymmetricSecondRankTensorType
  OutputSymmetricSecondRankTensorType;

  typedef CovariantVector<TParametersValueType, InputDiffusionTensor3DType::Dimension>
  InputTensorEigenVectorType;

  /** Standard vnl_vector type for this class   */
  typedef vnl_vector_fixed<TParametersValueType,
                           itkGetStaticConstMacro(InputSpaceDimension)>
  InputVnlVectorType;
  typedef vnl_vector_fixed<TParametersValueType,
                           itkGetStaticConstMacro(OutputSpaceDimension)>
  OutputVnlVectorType;

  /** Standard coordinate point type for this class   */
  typedef Point<TParametersValueType,
                itkGetStaticConstMacro(InputSpaceDimension)>
  InputPointType;
  typedef typename InputPointType::ValueType InputPointValueType;
  typedef Point<TParametersValueType,
                itkGetStaticConstMacro(OutputSpaceDimension)>
  OutputPointType;
  typedef typename OutputPointType::ValueType OutputPointValueType;

  /** Standard matrix type for this class   */
  typedef Matrix<TParametersValueType, itkGetStaticConstMacro(OutputSpaceDimension),
                 itkGetStaticConstMacro(InputSpaceDimension)>
  MatrixType;
  typedef typename MatrixType::ValueType MatrixValueType;

  /** Standard inverse matrix type for this class   */
  typedef Matrix<TParametersValueType, itkGetStaticConstMacro(InputSpaceDimension),
                 itkGetStaticConstMacro(OutputSpaceDimension)>
  InverseMatrixType;

  typedef InputPointType CenterType;

  typedef OutputVectorType               OffsetType;
  typedef typename OffsetType::ValueType OffsetValueType;

  typedef OutputVectorType TranslationType;

  typedef typename TranslationType::ValueType TranslationValueType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost. */
  typedef typename Superclass::InverseTransformBaseType InverseTransformBaseType;
  typedef typename InverseTransformBaseType::Pointer    InverseTransformBasePointer;

  /** Set the transformation to an Identity
   *
   * This sets the matrix to identity and the Offset to null. */
  virtual void SetIdentity();

  /** Indicates the category transform.
   *  e.g. an affine transform, or a local one, e.g. a deformation field.
   */
  virtual TransformCategoryType GetTransformCategory() const ITK_OVERRIDE
  {
    return Self::Linear;
  }

  /** Set matrix of an MatrixOffsetTransformBase
   *
   * This method sets the matrix of an MatrixOffsetTransformBase to a
   * value specified by the user.
   *
   * This updates the Offset wrt to current translation
   * and center.  See the warning regarding offset-versus-translation
   * in the documentation for SetCenter.
   *
   * To define an affine transform, you must set the matrix,
   * center, and translation OR the matrix and offset */
  virtual void SetMatrix(const MatrixType & matrix)
  {
    m_Matrix = matrix; this->ComputeOffset();
    this->ComputeMatrixParameters();
    m_MatrixMTime.Modified(); this->Modified(); return;
  }

  /** Get matrix of an MatrixOffsetTransformBase
   *
   * This method returns the value of the matrix of the
   * MatrixOffsetTransformBase.
   * To define an affine transform, you must set the matrix,
   * center, and translation OR the matrix and offset */

  virtual const MatrixType & GetMatrix() const
  {
    return m_Matrix;
  }

  /** Set offset (origin) of an MatrixOffset TransformBase.
   *
   * This method sets the offset of an MatrixOffsetTransformBase to a
   * value specified by the user.
   * This updates Translation wrt current center.  See the warning regarding
   * offset-versus-translation in the documentation for SetCenter.
   * To define an affine transform, you must set the matrix,
   * center, and translation OR the matrix and offset */
  void SetOffset(const OutputVectorType & offset)
  {
    m_Offset = offset; this->ComputeTranslation();
    this->Modified(); return;
  }

  /** Get offset of an MatrixOffsetTransformBase
   *
   * This method returns the offset value of the MatrixOffsetTransformBase.
   * To define an affine transform, you must set the matrix,
   * center, and translation OR the matrix and offset */
  const OutputVectorType & GetOffset() const
  {
    return m_Offset;
  }

  /** Set center of rotation of an MatrixOffsetTransformBase
   *
   * This method sets the center of rotation of an MatrixOffsetTransformBase
   * to a fixed point - for most transforms derived from this class,
   * this point is not a "parameter" of the transform - the exception is that
   * "centered" transforms have center as a parameter during optimization.
   *
   * This method updates offset wrt to current translation and matrix.
   * That is, changing the center changes the transform!
   *
   * WARNING: When using the Center, we strongly recommend only changing the
   * matrix and translation to define a transform.   Changing a transform's
   * center, changes the mapping between spaces - specifically, translation is
   * not changed with respect to that new center, and so the offset is updated
   * to * maintain the consistency with translation.   If a center is not used,
   * or is set before the matrix and the offset, then it is safe to change the
   * offset directly.
   *        As a rule of thumb, if you wish to set the center explicitly, set
   * before Offset computations are done.
   *
   * To define an affine transform, you must set the matrix,
   * center, and translation OR the matrix and offset */
  void SetCenter(const InputPointType & center)
  {
    m_Center = center; this->ComputeOffset();
    this->Modified(); return;
  }

  /** Get center of rotation of the MatrixOffsetTransformBase
   *
   * This method returns the point used as the fixed
   * center of rotation for the MatrixOffsetTransformBase.
   * To define an affine transform, you must set the matrix,
   * center, and translation OR the matrix and offset */
  const InputPointType & GetCenter() const
  {
    return m_Center;
  }

  /** Set translation of an MatrixOffsetTransformBase
   *
   * This method sets the translation of an MatrixOffsetTransformBase.
   * This updates Offset to reflect current translation.
   * To define an affine transform, you must set the matrix,
   * center, and translation OR the matrix and offset */
  void SetTranslation(const OutputVectorType & translation)
  {
    m_Translation = translation; this->ComputeOffset();
    this->Modified(); return;
  }

  /** Get translation component of the MatrixOffsetTransformBase
   *
   * This method returns the translation used after rotation
   * about the center point.
   * To define an affine transform, you must set the matrix,
   * center, and translation OR the matrix and offset */
  const OutputVectorType & GetTranslation() const
  {
    return m_Translation;
  }

  /** Set the transformation from a container of parameters.
   * The first (NOutputDimension x NInputDimension) parameters define the
   * matrix and the last NOutputDimension parameters the translation.
   * Offset is updated based on current center. */
  void SetParameters(const ParametersType & parameters) ITK_OVERRIDE;

  /** Get the Transformation Parameters. */
  const ParametersType & GetParameters() const ITK_OVERRIDE;

  /** Set the fixed parameters and update internal transformation. */
  virtual void SetFixedParameters(const FixedParametersType &) ITK_OVERRIDE;

  /** Get the Fixed Parameters. */
  virtual const FixedParametersType & GetFixedParameters() const ITK_OVERRIDE;

  /** Compose with another MatrixOffsetTransformBase
   *
   * This method composes self with another MatrixOffsetTransformBase of the
   * same dimension, modifying self to be the composition of self
   * and other.  If the argument pre is true, then other is
   * precomposed with self; that is, the resulting transformation
   * consists of first applying other to the source, followed by
   * self.  If pre is false or omitted, then other is post-composed
   * with self; that is the resulting transformation consists of
   * first applying self to the source, followed by other.
   * This updates the Translation based on current center. */
  void Compose(const Self *other, bool pre = 0);

  /** Transform by an affine transformation
   *
   * This method applies the affine transform given by self to a
   * given point or vector, returning the transformed point or
   * vector.  The TransformPoint method transforms its argument as
   * an affine point, whereas the TransformVector method transforms
   * its argument as a vector. */

  OutputPointType       TransformPoint(const InputPointType & point) const ITK_OVERRIDE;

  using Superclass::TransformVector;

  OutputVectorType      TransformVector(const InputVectorType & vector) const ITK_OVERRIDE;

  OutputVnlVectorType   TransformVector(const InputVnlVectorType & vector) const ITK_OVERRIDE;

  OutputVectorPixelType TransformVector(const InputVectorPixelType & vector) const ITK_OVERRIDE;

  using Superclass::TransformCovariantVector;

  OutputCovariantVectorType TransformCovariantVector(const InputCovariantVectorType & vector) const ITK_OVERRIDE;

  OutputVectorPixelType TransformCovariantVector(const InputVectorPixelType & vector) const ITK_OVERRIDE;

  using Superclass::TransformDiffusionTensor3D;

  OutputDiffusionTensor3DType TransformDiffusionTensor3D(const InputDiffusionTensor3DType & tensor) const ITK_OVERRIDE;

  OutputVectorPixelType TransformDiffusionTensor3D(const InputVectorPixelType & tensor ) const ITK_OVERRIDE;

  using Superclass::TransformSymmetricSecondRankTensor;
  OutputSymmetricSecondRankTensorType TransformSymmetricSecondRankTensor( const InputSymmetricSecondRankTensorType & tensor ) const ITK_OVERRIDE;

  OutputVectorPixelType TransformSymmetricSecondRankTensor( const InputVectorPixelType & tensor ) const ITK_OVERRIDE;

  /** Compute the Jacobian of the transformation
   *
   * This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the transform
   * is invertible at this point.
   * Get local Jacobian for the given point
   * \c j will sized properly as needed.
   */
  virtual void ComputeJacobianWithRespectToParameters(const InputPointType  & x, JacobianType & j) const ITK_OVERRIDE;

  /** Get the jacobian with respect to position. This simply returns
   * the current Matrix. jac will be resized as needed, but it's
   * more efficient if it's already properly sized. */
  virtual void ComputeJacobianWithRespectToPosition(const InputPointType  & x, JacobianType & jac) const ITK_OVERRIDE;

  /** Get the jacobian with respect to position. This simply returns
   * the inverse of the current Matrix. jac will be resized as needed, but it's
   * more efficient if it's already properly sized. */
  virtual void ComputeInverseJacobianWithRespectToPosition(const InputPointType  & x, JacobianType & jac) const ITK_OVERRIDE;

  /** Create inverse of an affine transformation
   *
   * This populates the parameters an affine transform such that
   * the transform is the inverse of self. If self is not invertible,
   * an exception is thrown.
   * Note that by default the inverese transform is centered at
   * the origin. If you need to compute the inverse centered at a point, p,
   *
   * \code
   * transform2->SetCenter( p );
   * transform1->GetInverse( transform2 );
   * \endcode
   *
   * transform2 will now contain the inverse of transform1 and will
   * with its center set to p. Flipping the two statements will produce an
   * incorrect transform.
   *
   */
  bool GetInverse(Self *inverse) const;

  /** Return an inverse of this transform. */
  virtual InverseTransformBasePointer GetInverseTransform() const ITK_OVERRIDE;

  /** Indicates that this transform is linear. That is, given two
   * points P and Q, and scalar coefficients a and b, then
   *
   *           T( a*P + b*Q ) = a * T(P) + b * T(Q)
   */
  virtual bool IsLinear() const ITK_OVERRIDE
  {
    return true;
  }

#if !defined(ITK_LEGACY_REMOVE)

public:
#else

protected:
#endif
  /** \deprecated Use GetInverse for public API instead.
   * Method will eventually be made a protected member function */
  const InverseMatrixType & GetInverseMatrix() const;

protected:
  /** Construct an MatrixOffsetTransformBase object
   *
   * This method constructs a new MatrixOffsetTransformBase object and
   * initializes the matrix and offset parts of the transformation
   * to values specified by the caller.  If the arguments are
   * omitted, then the MatrixOffsetTransformBase is initialized to an identity
   * transformation in the appropriate number of dimensions. */
  MatrixOffsetTransformBase(const MatrixType & matrix, const OutputVectorType & offset);
  MatrixOffsetTransformBase(unsigned int paramDims);
  MatrixOffsetTransformBase();

  /** Destroy an MatrixOffsetTransformBase object */
  virtual ~MatrixOffsetTransformBase() ITK_OVERRIDE;

  /** Print contents of an MatrixOffsetTransformBase */
  virtual void PrintSelf(std::ostream & s, Indent indent) const ITK_OVERRIDE;

  const InverseMatrixType & GetVarInverseMatrix() const
  {
    return m_InverseMatrix;
  }
  void SetVarInverseMatrix(const InverseMatrixType & matrix) const
  {
    m_InverseMatrix = matrix; m_InverseMatrixMTime.Modified();
  }
  bool InverseMatrixIsOld() const
  {
    if( m_MatrixMTime != m_InverseMatrixMTime )
      {
      return true;
      }
    else
      {
      return false;
      }
  }

  virtual void ComputeMatrixParameters();

  virtual void ComputeMatrix();

  void SetVarMatrix(const MatrixType & matrix)
  {
    m_Matrix = matrix; m_MatrixMTime.Modified();
  }

  virtual void ComputeTranslation();

  void SetVarTranslation(const OutputVectorType & translation)
  {
    m_Translation = translation;
  }

  virtual void ComputeOffset();

  void SetVarOffset(const OutputVectorType & offset)
  {
    m_Offset = offset;
  }

  void SetVarCenter(const InputPointType & center)
  {
    m_Center = center;
  }

  itkGetConstMacro(Singular, bool);
private:

  MatrixOffsetTransformBase(const Self & other);
  const Self & operator=(const Self &);

  MatrixType                m_Matrix;           // Matrix of the transformation
  OutputVectorType          m_Offset;           // Offset of the transformation
  mutable InverseMatrixType m_InverseMatrix;    // Inverse of the matrix
  mutable bool              m_Singular;         // Is m_Inverse singular?

  InputPointType   m_Center;
  OutputVectorType m_Translation;

  /** To avoid recomputation of the inverse if not needed */
  TimeStamp         m_MatrixMTime;
  mutable TimeStamp m_InverseMatrixMTime;
}; // class MatrixOffsetTransformBase
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMatrixOffsetTransformBase.hxx"
#endif

#endif /* itkMatrixOffsetTransformBase_h */
