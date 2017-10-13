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
#ifndef itkKernelTransform_h
#define itkKernelTransform_h

#include "itkTransform.h"
#include "itkMatrix.h"
#include "itkPointSet.h"
#include <deque>
#include <cmath>
#include "vnl/vnl_matrix_fixed.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"
#include "vnl/vnl_vector_fixed.h"
#include "vnl/algo/vnl_svd.h"
#include "vnl/vnl_sample.h"

namespace itk
{
/** \class KernelTransform
 * Intended to be a base class for elastic body spline and thin plate spline.
 * This is implemented in as straightforward a manner as possible from the
 * IEEE TMI paper by Davis, Khotanzad, Flamig, and Harms, Vol. 16,
 * No. 3 June 1997. Notation closely follows their paper, so if you have it
 * in front of you, this code will make a lot more sense.
 *
 * KernelTransform:
 *  Provides support for defining source and target landmarks
 *  Defines a number of data types used in the computations
 *  Defines the mathematical framework used to compute all splines,
 *    so that subclasses need only provide a kernel specific to
 *    that spline
 *
 * This formulation allows the stiffness of the spline to
 * be adjusted, allowing the spline to vary from interpolating the
 * landmarks to approximating the landmarks.  This part of the
 * formulation is based on the short paper by R. Sprengel, K. Rohr,
 * H. Stiehl. "Thin-Plate Spline Approximation for Image
 * Registration". In 18th International Conference of the IEEE
 * Engineering in Medicine and Biology Society. 1996.
 *
 *
 * \ingroup ITKTransform
 */
template<typename TParametersValueType,
          unsigned int NDimensions>
class ITK_TEMPLATE_EXPORT KernelTransform :
  public Transform<TParametersValueType, NDimensions, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef KernelTransform                                             Self;
  typedef Transform<TParametersValueType, NDimensions, NDimensions> Superclass;
  typedef SmartPointer<Self>                                        Pointer;
  typedef SmartPointer<const Self>                                  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(KernelTransform, Transform);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** Dimension of the domain space. */
  itkStaticConstMacro(SpaceDimension, unsigned int, NDimensions);

  /** Scalar type. */
  typedef typename Superclass::ScalarType ScalarType;

  /** Parameters type. */
  typedef typename Superclass::FixedParametersType FixedParametersType;
  typedef typename Superclass::ParametersType      ParametersType;

  /** Jacobian type. */
  typedef typename Superclass::JacobianType JacobianType;

  /** Transform category type. */
  typedef typename Superclass::TransformCategoryType TransformCategoryType;

  /** Standard coordinate point type for this class. */
  typedef typename Superclass::InputPointType  InputPointType;
  typedef typename Superclass::OutputPointType OutputPointType;

  /** Standard vector type for this class. */
  typedef typename Superclass::InputVectorType  InputVectorType;
  typedef typename Superclass::OutputVectorType OutputVectorType;

  /** Standard covariant vector type for this class */
  typedef typename Superclass::InputCovariantVectorType  InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType OutputCovariantVectorType;

  /** Standard vnl_vector type for this class. */
  typedef typename Superclass::InputVnlVectorType  InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType OutputVnlVectorType;

   /** The number of parameters defininig this transform. */
  typedef typename Superclass::NumberOfParametersType NumberOfParametersType;

  /** PointList typedef. This type is used for maintaining lists of points,
   * specifically, the source and target landmark lists. */
  typedef DefaultStaticMeshTraits<TParametersValueType, NDimensions, NDimensions, TParametersValueType, TParametersValueType> PointSetTraitsType;
  typedef PointSet<InputPointType, NDimensions, PointSetTraitsType>                                PointSetType;

  typedef typename PointSetType::Pointer                      PointSetPointer;
  typedef typename PointSetType::PointsContainer              PointsContainer;
  typedef typename PointSetType::PointsContainerIterator      PointsIterator;
  typedef typename PointSetType::PointsContainerConstIterator PointsConstIterator;
  typedef typename PointSetType::PointIdentifier              PointIdentifier;

  /** VectorSet typedef. */
  typedef itk::VectorContainer<SizeValueType, InputVectorType> VectorSetType;
  typedef typename VectorSetType::Pointer                      VectorSetPointer;

  /** Get/Set the source landmarks list, which we will denote \f$ p \f$. */
  itkGetModifiableObjectMacro(SourceLandmarks, PointSetType); //NOTE: This is used to circumvent the SetTargetLandmarks
  virtual void SetSourceLandmarks(PointSetType *);

  /** Get the target landmarks list, which we will denote  \f$ q \f$. */
  itkGetModifiableObjectMacro(TargetLandmarks, PointSetType); //NOTE: This is used to circumvent the SetTargetLandmarks
  virtual void SetTargetLandmarks(PointSetType *);

  /** Get the displacements list, which we will denote \f$ d \f$,
   * where \f$ d_i = q_i - p_i \f$. */
  itkGetModifiableObjectMacro(Displacements, VectorSetType);

  /** Compute W matrix. */
  void ComputeWMatrix();

  /** Compute the position of point in the new space */
  virtual OutputPointType TransformPoint(const InputPointType & thisPoint) const ITK_OVERRIDE;

  /** These vector transforms are not implemented for this transform */
  using Superclass::TransformVector;
  virtual OutputVectorType TransformVector(const InputVectorType &) const ITK_OVERRIDE
  {
    itkExceptionMacro( << "TransformVector(const InputVectorType &) is not implemented for KernelTransform");
  }

  virtual OutputVnlVectorType TransformVector(const InputVnlVectorType &) const ITK_OVERRIDE
  {
    itkExceptionMacro( << "TransformVector(const InputVnlVectorType &) is not implemented for KernelTransform");
  }

  /**  Method to transform a CovariantVector. */
  using Superclass::TransformCovariantVector;
  virtual OutputCovariantVectorType TransformCovariantVector(const InputCovariantVectorType &) const ITK_OVERRIDE
  {
    itkExceptionMacro( << "TransformCovariantVector(const InputCovariantVectorType &) is not implemented for KernelTransform");
  }

  /** 'I' (identity) matrix typedef. */
  typedef vnl_matrix_fixed<TParametersValueType, NDimensions, NDimensions> IMatrixType;

  /** Compute the Jacobian Matrix of the transformation at one point */
  virtual void ComputeJacobianWithRespectToParameters( const InputPointType  & p, JacobianType & jacobian) const ITK_OVERRIDE;

  virtual void ComputeJacobianWithRespectToPosition(const InputPointType &,
                                                    JacobianType &) const ITK_OVERRIDE
  {
    itkExceptionMacro( "ComputeJacobianWithRespectToPosition not yet implemented "
                       "for " << this->GetNameOfClass() );
  }

  /** Set the Transformation Parameters and update the internal transformation.
   * The parameters represent the source landmarks. Each landmark point is
   * represented by NDimensions doubles. All the landmarks are concatenated to
   * form one flat Array<double>. */
  virtual void SetParameters(const ParametersType &) ITK_OVERRIDE;

  /** Set Transform Fixed Parameters:
   *     To support the transform file writer this function was
   *     added to set the target landmarks similar to the
   *     SetParameters function setting the source landmarks
   */
  virtual void SetFixedParameters(const FixedParametersType &) ITK_OVERRIDE;

  /** Update the Parameters array from the landmarks corrdinates. */
  virtual void UpdateParameters() const;

  /** Get the Transformation Parameters - Gets the Source Landmarks */
  virtual const ParametersType & GetParameters() const ITK_OVERRIDE;

  /** Get Transform Fixed Parameters - Gets the Target Landmarks */
  virtual const FixedParametersType & GetFixedParameters() const ITK_OVERRIDE;

  /** This transform is not linear, because the transformation of a linear
   * combination of points is not equal to the linear combination of the
   * transformations of individual points */
  virtual TransformCategoryType GetTransformCategory() const ITK_OVERRIDE
  {
    return Self::Spline;
  }

  /** Stiffness of the spline.  A stiffness of zero results in the
   * standard interpolating spline.  A non-zero stiffness allows the
   * spline to approximate rather than interpolate the landmarks.
   * Stiffness values are usually rather small, typically in the range
   * of 0.001 to 0.1. The approximating spline formulation is based on
   * the short paper by R. Sprengel, K. Rohr, H. Stiehl. "Thin-Plate
   * Spline Approximation for Image Registration". In 18th
   * International Conference of the IEEE Engineering in Medicine and
   * Biology Society. 1996.
   */
  itkSetClampMacro( Stiffness, double, 0.0, NumericTraits<double>::max() );
  itkGetConstMacro(Stiffness, double);

protected:
  KernelTransform();
  virtual ~KernelTransform() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

public:
  /** 'G' matrix typedef. */
  typedef vnl_matrix_fixed<TParametersValueType, NDimensions, NDimensions> GMatrixType;

  /** 'L' matrix typedef. */
  typedef vnl_matrix<TParametersValueType> LMatrixType;

  /** 'K' matrix typedef. */
  typedef vnl_matrix<TParametersValueType> KMatrixType;

  /** 'P' matrix typedef. */
  typedef vnl_matrix<TParametersValueType> PMatrixType;

  /** 'Y' matrix typedef. */
  typedef vnl_matrix<TParametersValueType> YMatrixType;

  /** 'W' matrix typedef. */
  typedef vnl_matrix<TParametersValueType> WMatrixType;

  /** 'D' matrix typedef. Deformation component */
  typedef vnl_matrix<TParametersValueType> DMatrixType;

  /** 'A' matrix typedef. Rotational part of the Affine component */
  typedef vnl_matrix_fixed<TParametersValueType, NDimensions, NDimensions> AMatrixType;

  /** 'B' matrix typedef. Translational part of the Affine component */
  typedef vnl_vector_fixed<TParametersValueType, NDimensions> BMatrixType;

  /** Row matrix typedef. */
  typedef vnl_matrix_fixed<TParametersValueType, 1, NDimensions> RowMatrixType;

  /** Column matrix typedef. */
  typedef vnl_matrix_fixed<TParametersValueType, NDimensions, 1> ColumnMatrixType;

protected:
  /** Compute G(x)
   * This is essentially the kernel of the transform.
   * By overriding this method, we can obtain (among others):
   *    Elastic body spline
   *    Thin plate spline
   *    Volume spline */
  virtual void ComputeG(const InputVectorType & landmarkVector, GMatrixType & gmatrix) const;

  /** Compute a G(x) for a point to itself (i.e. for the block diagonal
   * elements of the matrix K. Parameter indicates for which landmark
   * the reflexive G is to be computed. The default implementation for
   * the reflexive contribution is a diagonal matrix where the diagonal
   * elements are the stiffness of the spline.
   *
   * \warning this method is not thread-safe. However this method is called
   * only through ComputeWMatrix() that is itself normally called from a single
   * thread during the initialization of the Transform. */
  virtual const GMatrixType & ComputeReflexiveG(PointsIterator) const;

  /** Compute the contribution of the landmarks weighted by the kernel funcion
      to the global deformation of the space  */
  virtual void ComputeDeformationContribution(const InputPointType & inputPoint, OutputPointType & result) const;

  /** Compute K matrix. */
  void ComputeK();

  /** Compute L matrix. */
  void ComputeL();

  /** Compute P matrix. */
  void ComputeP();

  /** Compute Y matrix. */
  void ComputeY();

  /** Compute displacements \f$ q_i - p_i \f$. */
  void ComputeD();

  /** Reorganize the components of W into
    D (deformable), A (rotation part of affine)
    and B (translational part of affine ) components.
    \warning This method release the memory of the W Matrix  */
  void ReorganizeW();

  /** Stiffness parameter */
  double m_Stiffness;

  /** The list of displacements.
   * d[i] = q[i] - p[i]; */
  VectorSetPointer m_Displacements;

  /** The L matrix. */
  LMatrixType m_LMatrix;

  /** The K matrix. */
  KMatrixType m_KMatrix;

  /** The P matrix. */
  PMatrixType m_PMatrix;

  /** The Y matrix. */
  YMatrixType m_YMatrix;

  /** The W matrix. */
  WMatrixType m_WMatrix;

  /** The Deformation matrix.
      This is an auxiliary matrix that will hold the
      Deformation (non-affine) part of the transform.
      Those are the coefficients that will multiply the
      Kernel function */
  DMatrixType m_DMatrix;

  /** Rotatinoal/Shearing part of the Affine component of the Transformation */
  AMatrixType m_AMatrix;

  /** Translational part of the Affine component of the Transformation */
  BMatrixType m_BVector;

  /** The G matrix.
   *  It is made mutable because m_GMatrix was made an ivar
   *  only to avoid copying the matrix at return time */
  mutable GMatrixType m_GMatrix;

  /** Has the W matrix been computed? */
  bool m_WMatrixComputed;

  /** Identity matrix. */
  IMatrixType m_I;

  /** The list of source landmarks, denoted 'p'. */
  PointSetPointer m_SourceLandmarks;

  /** The list of target landmarks, denoted 'q'. */
  PointSetPointer m_TargetLandmarks;

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(KernelTransform);

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKernelTransform.hxx"
#endif

#endif // itkKernelTransform_h
