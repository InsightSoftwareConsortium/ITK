#ifndef __itkKernelTransform_h
#define __itkKernelTransform_h

#include "itkTransformation.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkMatrix.h"
#include <deque>
#include <math.h>
#include "vnl/vnl_matrix_fixed.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"
#include "vnl/vnl_vector_fixed.h"
#include "vnl/algo/vnl_svd.h"

namespace itk
{

/** \class KernelTransform
 * Intended to be a base class for elastic body spline and thin plate spline.
 * This is implemented in as straightforward a manner as possible from the
 * IEEE TMI paper by Davis, Khotanzad, Flamig, and Harms,
 * Vol. 16 No. 3 June 1997
 * Notation closely follows their paper, so if you have it in front of you,
 * this code will make a lot more sense.
 *
 * KernelTransform:
 *  Provides support for defining source and target landmarks
 *  Defines a number of data types used in the computations
 *  Defines the mathematical framework used to compute all splines,
 *    so that subclasses need only provide a kernel specific to
 *    that spline
 */
template <class TScalarType,         // Only float and double make sense
          int NDimensions = 3>       // Number of dimensions
class ITK_EXPORT KernelTransform : public Transformation<TScalarType, NDimensions>
{
public:
  /**
   * Standard Self typedef
   */
  typedef KernelTransform<TScalarType, NDimensions> Self;
  /**
   * Standard Superclass typedef
   */
  typedef Transformation<TScalarType, NDimensions> Superclass;
  /**
   * Standard coordinate point type for this class
   */
	typedef typename Superclass::PointType PointType;
  /**
   * Standard vector type for this class
   */
	typedef typename Superclass::VectorType VectorType;
  /**
   * PointList typedef. This type is used for maintaining lists of points,
   * specifically, the source and target landmark lists.
   */
  typedef std::deque<PointType*> PointListType;
  /**
   * VectorList typedef
   */
  typedef std::deque<VectorType*> VectorListType;
  /**
   * Get the source landmarks list, which we will denote p
   */
	PointListType* Getp();
  /**
   * Get the target landmarks list, which we will denote q
   */
	PointListType* Getq();
  /**
   * Get the displacements list, which we will denote d,
   * where d_i = q_i - p_i
   */
	VectorListType* Getd();
  /**
   * Compute W matrix
   */
  void ComputeW();
  /**
   * Compute the position of point in the new space
   */
  virtual PointType Transform(const PointType& thisPoint) const;
  /**
   * Compute the position of vector in the new space
   */
  virtual VectorType Transform(const VectorType& thisVector) const;
  /**
   * 'I' (identity) matrix typedef
   */
  typedef vnl_matrix_fixed<TScalarType, NDimensions, NDimensions> IMatrixType;
  /**
   * Default constructor
   */
  KernelTransform();
  /**
   * Destructor
   */
  virtual ~KernelTransform();

protected:
  /**
   * 'G' matrix typedef
   */
  typedef vnl_matrix_fixed<TScalarType, NDimensions, NDimensions> GMatrixType;
  /**
   * 'L' matrix typedef
   */
  typedef vnl_matrix<TScalarType> LMatrixType;
  /**
   * 'K' matrix typedef
   */
  typedef vnl_matrix<TScalarType> KMatrixType;
  /**
   * 'P' matrix typedef
   */
  typedef vnl_matrix<TScalarType> PMatrixType;
  /**
   * 'Y' matrix typedef
   */
  typedef vnl_matrix<TScalarType> YMatrixType;
  /**
   * 'W' matrix typedef
   */
  typedef vnl_matrix<TScalarType> WMatrixType;
  /**
   * Row matrix typedef
   */
  typedef vnl_matrix_fixed<TScalarType, 1, NDimensions> RowMatrixType;
  /**
   * Column matrix typedef
   */
  typedef vnl_matrix_fixed<TScalarType, NDimensions, 1> ColumnMatrixType;

  /**
   * Compute G(x)
   * This is essentially the kernel of the transform.
   * By overriding this method, we can obtain (among others):
   *    Elastic body spline
   *    Thin plate spline
   *    Volume spline
   */
  virtual GMatrixType ComputeG(VectorType& x) const = 0;
  /**
   * Compute K matrix
   */
  void ComputeK();
  /**
   * Compute L matrix
   */
  void ComputeL();
  /**
   * Compute P matrix
   */
  void ComputeP();
  /**
   * Compute Y matrix
   */
  void ComputeY();
  /**
   * Compute displacements q_i - p_i
   */
  void ComputeD();
  /**
   * The list of source landmarks, denoted 'p'
   */
  PointListType* m_p;
  /**
   * The list of target landmarks, denoted 'q'
   */
  PointListType* m_q;
  /**
   * The list of displacements.
   * d[i] = q[i] - p[i];
   */
  VectorListType* m_d;
  /**
   * The L matrix
   */
  LMatrixType* m_LMatrix;
  /**
   * The K matrix
   */
  KMatrixType* m_KMatrix;
  /**
   * The P matrix
   */
  PMatrixType* m_PMatrix;
  /**
   * The Y matrix
   */
  YMatrixType* m_YMatrix;
  /**
   * The W matrix
   */
  WMatrixType* m_WMatrix;
  /**
   * Has the W matrix been computed?
   */
  bool m_WMatrixComputed;
  /**
   * Identity matrix
   */
  static IMatrixType m_I;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKernelTransform.txx"
#endif

#endif // __itkKernelTransform_h
