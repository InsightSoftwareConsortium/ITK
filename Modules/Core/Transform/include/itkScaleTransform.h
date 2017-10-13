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
#ifndef itkScaleTransform_h
#define itkScaleTransform_h

#include "itkMatrixOffsetTransformBase.h"
#include "itkMacro.h"
#include "itkMatrix.h"

namespace itk
{
/** \class ScaleTransform
 * \brief Scale transformation of a vector space (e.g. space coordinates)
 *
 * The same functionality could be obtained by using the Affine transform,
 * but with a large difference in performace since the affine transform will
 * use a matrix multiplication using a diagonal matrix.
 *
 * \ingroup ITKTransform
 *
 * \wiki
 * \wikiexample{ImageProcessing/ScaleTransform,Scale an image}
 * \endwiki
 */
template<typename TParametersValueType=float,
          unsigned int NDimensions=3>
class ITK_TEMPLATE_EXPORT ScaleTransform : public MatrixOffsetTransformBase<TParametersValueType,
                                                        NDimensions,
                                                        NDimensions>
{
public:
  /** Standard class typedefs.   */
  typedef ScaleTransform                                                            Self;
  typedef MatrixOffsetTransformBase<TParametersValueType, NDimensions, NDimensions> Superclass;
  typedef SmartPointer<Self>                                                        Pointer;
  typedef SmartPointer<const Self>                                                  ConstPointer;

  /** New macro for creation of through a smart pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScaleTransform, Transform);

  /** Dimension of the domain space. */
  itkStaticConstMacro(SpaceDimension, unsigned int, NDimensions);
  itkStaticConstMacro(ParametersDimension, unsigned int, NDimensions);

  /** Scalar type. */
  typedef typename Superclass::ScalarType ScalarType;

  /** Parameters type. */
  typedef typename Superclass::FixedParametersType FixedParametersType;
  typedef typename Superclass::ParametersType      ParametersType;

  /** Jacobian type. */
  typedef typename Superclass::JacobianType JacobianType;

  /** Standard vector type for this class. */
  typedef FixedArray<TParametersValueType, NDimensions> ScaleType;

  /** Standard vector type for this class. */
  typedef Vector<TParametersValueType, NDimensions> InputVectorType;
  typedef Vector<TParametersValueType, NDimensions> OutputVectorType;

  /** Standard covariant vector type for this class. */
  typedef CovariantVector<TParametersValueType, NDimensions> InputCovariantVectorType;
  typedef CovariantVector<TParametersValueType, NDimensions> OutputCovariantVectorType;

  /** Standard vnl_vector type for this class. */
  typedef vnl_vector_fixed<TParametersValueType, NDimensions> InputVnlVectorType;
  typedef vnl_vector_fixed<TParametersValueType, NDimensions> OutputVnlVectorType;

  /** Standard coordinate point type for this class. */
  typedef Point<TParametersValueType, NDimensions> InputPointType;
  typedef Point<TParametersValueType, NDimensions> OutputPointType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost.*/
  typedef typename Superclass::InverseTransformBaseType InverseTransformBaseType;
  typedef typename InverseTransformBaseType::Pointer    InverseTransformBasePointer;

  typedef typename Superclass::MatrixType MatrixType;

  /** Set parameters.  This method sets the parameters for the transform value
   *  specified by the user. The parameters are organized as scale[i] =
   *  parameter[i]. That means that in 3D the scale parameters for the coordinates
   *  {x,y,z} are {parameter[0], parameter[1], parameter[2]} respectively */
  virtual void SetParameters(const ParametersType & parameters) ITK_OVERRIDE;

  /** Get the parameters that uniquely define the transform This is typically
   * used by optimizers during the process of image registration.  The parameters
   * are organized as {scale X, scale Y, scale Z } = { parameter[0],
   * parameter[1], parameter[2] } respectively */
  virtual const ParametersType & GetParameters() const ITK_OVERRIDE;


#if !defined( ITK_LEGACY_REMOVE )
  /** Set the fixed parameters and update internal
   * transformation. This transform has the center point as it's fixed
   * parameters.
   */
  virtual void SetFixedParameters(const FixedParametersType &params) ITK_OVERRIDE
    {
      if (params.GetSize() != NDimensions)
        {
        itkWarningMacro(<< "The ScaleTransform now has " << NDimensions << " fixed parameters for the Center. Ignoring fixed parameters provided.");
        return;
        }
      Superclass::SetFixedParameters(params);
    }
#endif

  /** Get the Jacobian matrix. */
  virtual void ComputeJacobianWithRespectToParameters(const InputPointType & point, JacobianType & j) const ITK_OVERRIDE;

  /** Get the jacobian with respect to position, which simply is the
   *  matrix because the transform is position-invariant.
   *  jac will be resized as needed, but it will be more efficient if
   *  it is already properly sized. */
  virtual void ComputeJacobianWithRespectToPosition(const InputPointType  & x, JacobianType & jac) const ITK_OVERRIDE;

  /** Set the factors of an Scale Transform
   * This method sets the factors of an ScaleTransform to a
   * value specified by the user.
   * This method cannot be done with SetMacro because itk::Array has not an
   * operator== defined. The array of scales correspond in order to the factors
   * to be applied to each one of the coordinaates. For example, in 3D,
   * scale[0] corresponds to X, scale[1] corresponds to Y and scale[2]
   * corresponds to Z. */
  void SetScale(const ScaleType & scale);

  virtual void ComputeMatrix(void) ITK_OVERRIDE;

  /** Compose with another ScaleTransform. */
  void Compose(const Self *other, bool pre = false);

  /** Compose this transform transformation with another scaling.
   * The pre argument is irrelevant here since scale transforms are commutative,
   * pre and postcomposition are therefore equivalent. */
  void Scale(const ScaleType & scale, bool pre = false);

  /** Transform by a scale transformation
   * This method applies the scale transform given by self to a
   * given point or vector, returning the transformed point or
   * vector. */
  OutputPointType     TransformPoint(const InputPointType  & point) const ITK_OVERRIDE;

  using Superclass::TransformVector;
  OutputVectorType    TransformVector(const InputVectorType & vector) const ITK_OVERRIDE;

  OutputVnlVectorType TransformVector(const InputVnlVectorType & vector) const ITK_OVERRIDE;

  using Superclass::TransformCovariantVector;
  OutputCovariantVectorType TransformCovariantVector(const InputCovariantVectorType & vector) const ITK_OVERRIDE;

  /** Back transform by a scale transformation
   * This method finds the point or vector that maps to a given
   * point or vector under the scale transformation defined by
   * self.  If no such point exists, an exception is thrown. */
  inline InputPointType     BackTransform(const OutputPointType  & point) const;

  inline InputVectorType    BackTransform(const OutputVectorType & vector) const;

  inline InputVnlVectorType BackTransform(const OutputVnlVectorType & vector) const;

  inline InputCovariantVectorType BackTransform(const OutputCovariantVectorType & vector) const;

  /** Find inverse of a scale transformation
   * This method creates and returns a new ScaleTransform object
   * which is the inverse of self.  If self is not invertible,
   * false is returned. */
  bool GetInverse(Self *inverse) const;

  /** Return an inverse of this transform. */
  virtual InverseTransformBasePointer GetInverseTransform() const ITK_OVERRIDE;

  /** Set the transformation to an Identity
   *
   * This sets all the scales to 1.0 */
  void SetIdentity() ITK_OVERRIDE;

  /** Get access to scale values */
  itkGetConstReferenceMacro(Scale, ScaleType);

protected:
  /** Construct an ScaleTransform object. */
  ScaleTransform();

  /** Destroy an ScaleTransform object. */
  ~ScaleTransform() ITK_OVERRIDE;

  /** Print contents of an ScaleTransform */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ScaleTransform);

  ScaleType m_Scale;    // Scales of the transformation

};                         // class ScaleTransform

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScaleTransform.hxx"
#endif

#endif /* itkScaleTransform_h */
