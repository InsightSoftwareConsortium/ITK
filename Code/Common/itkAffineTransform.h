/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAffineTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkAffineTransform_h
#define __itkAffineTransform_h

#include <iostream>

#include "itkMatrix.h"
#include "itkTransform.h"
#include "itkExceptionObject.h"
#include "itkMacro.h"

namespace itk
{


/**
 * Affine transformation of a vector space (e.g. space coordinates)
 *
 * This class allows the definition and manipulation of affine
 * transformations of an n-dimensional affine space (and its
 * associated vector space) onto itself.  One common use is to define
 * and manipulate Euclidean coordinate transformations in two and
 * three dimensions, but other uses are possible as well.
 *
 * An affine transformation is defined mathematically as a linear
 * transformation plus a constant offset.  If A is a constant n x n
 * matrix and b is a constant n-vector, then y = Ax+b defines an
 * affine transformation from the n-vector x to the n-vector y.
 *
 * The difference between two points is a vector and transforms
 * linearly, using the matrix only.  That is, (y1-y2) = A*(x1-x2).
 *
 * The AffineTransform class determines whether to transform an object
 * as a point or a vector by examining its type.  An object of type
 * Point transforms as a point; an object of type Vector transforms as
 * a vector.
 *
 * One common use of affine transformations is to define coordinate
 * conversions in two- and three-dimensional space.  In this
 * application, x is a two- or three-dimensional vector containing the
 * "source" coordinates of a point, y is a vector containing the
 * "target" coordinates, the matrix A defines the scaling and rotation
 * of the coordinate systems from the source to the target, and b
 * defines the translation of the origin from the source to the
 * target.  More generally, A can also define anisotropic scaling and
 * shearing transformations.  Any good textbook on computer graphics
 * will discuss coordinate transformations in more detail.  Several of
 * the methods in this class are designed for this purpose and use the
 * language appropriate to coordinate conversions.
 *
 * Any two affine transformations may be composed and the result is
 * another affine transformation.  However, the order is important.
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
 * There are two template parameters for this class:
 *
 * ScalarT       The type to be used for scalar numeric values.  Either
 *               float or double.
 *
 * NDimensions   The number of dimensions of the vector space.
 *
 * This class provides several methods for setting the matrix and vector
 * defining the transform. To support the registration framework, the 
 * transform parameters can also be set as an Array<double> of size
 * (NDimension + 1) * NDimension using method SetParameters(). 
 * The first (NDimension x NDimension) parameters defines the matrix in 
 * column-major order (where the column index) varies the fastest). 
 * The last NDimension parameters defines the translation or offest 
 * in each dimensions.
 *
 * \ingroup Transforms
 *
 *
 * \todo Is there any real value in allowing the user to template
 * over the scalar type?  Perhaps it should always be double, unless
 * there's a compatibility problem with the Point class.
 *
 * \todo Add methods to transform (or back transform)
 *   many points or vectors at once?
 *
 * \todo  Add reflection?  **/

template <
 class TScalarType=double,         // Data type for scalars (e.g. float or double)
 unsigned int NDimensions=3>       // Number of dimensions in the input space
class AffineTransform : public Transform< TScalarType,
                                          NDimensions, 
                                          NDimensions >
{
public:
  /** Standard typedefs   */
  typedef AffineTransform  Self;
  typedef Transform< TScalarType, NDimensions, NDimensions >  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods).   */
  itkTypeMacro( AffineTransform, Transform );

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro( Self );

  /** Dimension of the domain space. */
  itkStaticConstMacro(SpaceDimension, unsigned int, NDimensions);
  itkStaticConstMacro(ParametersDimension, unsigned int,
                      NDimensions*(NDimensions+1));

  
  /** Parameters Type   */
  typedef typename Superclass::ParametersType  ParametersType;

  /** Jacobian Type   */
  typedef typename Superclass::JacobianType  JacobianType;

  /** Standard scalar type for this class */
  typedef typename Superclass::ScalarType ScalarType;

  /** Standard vector type for this class   */
  typedef Vector<TScalarType,
                 itkGetStaticConstMacro(SpaceDimension)>  InputVectorType;
  typedef Vector<TScalarType,
                 itkGetStaticConstMacro(SpaceDimension)>  OutputVectorType;
  
  /** Standard covariant vector type for this class   */
  typedef CovariantVector<TScalarType,
                          itkGetStaticConstMacro(SpaceDimension)>  InputCovariantVectorType;
  typedef CovariantVector<TScalarType,
                          itkGetStaticConstMacro(SpaceDimension)>  OutputCovariantVectorType;
  
  /** Standard vnl_vector type for this class   */
  typedef vnl_vector_fixed<TScalarType,
                           itkGetStaticConstMacro(SpaceDimension)> InputVnlVectorType;
  typedef vnl_vector_fixed<TScalarType,
                           itkGetStaticConstMacro(SpaceDimension)> OutputVnlVectorType;
  
  /** Standard coordinate point type for this class   */
  typedef Point<TScalarType,
                itkGetStaticConstMacro(SpaceDimension)> InputPointType;
  typedef Point<TScalarType,
                itkGetStaticConstMacro(SpaceDimension)> OutputPointType;
  
  /** Standard matrix type for this class   */
  typedef Matrix<TScalarType, itkGetStaticConstMacro(SpaceDimension),
                 itkGetStaticConstMacro(SpaceDimension)> MatrixType;

  /** Standard offset type for this class   */
  typedef     OutputVectorType    OffsetType;

  /** Get offset of an AffineTransform
   *
   * This method returns the offset value of the AffineTransform. **/
  const OffsetType & GetOffset(void) const
      { return m_Offset; }

  /** Get matrix of an AffineTransform
   *
   * This method returns the value of the matrix of the
   * AffineTransform. */
  const MatrixType & GetMatrix() const
      { return m_Matrix; }

  /** Set the transformation to an Identity
   *
   * This sets the matrix to identity and the Offset to null. */
  void SetIdentity( void )
    { m_Matrix.SetIdentity();
      m_Offset.Fill( 0.0 );
      this->Modified();  
    }

  /** Get inverse matrix of an AffineTransform
   *
   * This method returns the value of the inverse matrix of
   * the AffineTransform.  It's not clear that this is useful
   * except for debugging the class itself.
   *
   * \todo Do something reasonable if the transform is singular.  */
  const MatrixType & GetInverse() const
    { if( m_Singular ) { throw ExceptionObject(__FILE__, __LINE__); }
    return m_Inverse; 
    }

  /** Set offset (origin) of an Affine Transform.
   *
   * This method sets the offset of an AffineTransform to a
   * value specified by the user.  The offset is ...?
   */
  void SetOffset(const OffsetType &offset)
      { m_Offset = offset; this->Modified(); return; }

  /** Set matrix of an AffineTransform
   *
   * This method sets the matrix of an AffineTransform to a
   * value specified by the user. */
  void SetMatrix(const MatrixType &matrix)
      { m_Matrix = matrix; RecomputeInverse(); this->Modified(); return; }

  /** Set the transformation from a container of parameters.
   * The first (NDimension x NDimension) parameters define the
   * matrix and the last NDimension parameters the translation. */
  void SetParameters( const ParametersType & parameters );

  /** Get the Transformation Parameters. */
  const ParametersType& GetParameters(void) const;

  /** Compose with another AffineTransform
   *
   * This method composes self with another AffineTransform of the
   * same dimension, modifying self to be the composition of self
   * and other.  If the argument pre is true, then other is
   * precomposed with self; that is, the resulting transformation
   * consists of first applying other to the source, followed by
   * self.  If pre is false or omitted, then other is post-composed
   * with self; that is the resulting transformation consists of
   * first applying self to the source, followed by other. */
  void Compose(const Self * other, bool pre=0);

  /** Compose affine transformation with a translation
   *
   * This method modifies self to include a translation of the
   * origin.  The translation is precomposed with self if pre is
   * true, and postcomposed otherwise. */
  void Translate(const OutputVectorType &offset, bool pre=0);

  /** Compose affine transformation with a scaling
   *
   * This method modifies self to magnify the source by a given
   * factor along each axis.  If all factors are the same, or only a
   * single factor is given, then the scaling is isotropic;
   * otherwise it is anisotropic.  If an odd number of factors are
   * negative, then the parity of the image changes.  If any of the
   * factors is zero, then the transformation becomes a projection
   * and is not invertible.  The scaling is precomposed with self if
   * pre is true, and postcomposed otherwise. */
  void Scale(const OutputVectorType &factor, bool pre=0);
  void Scale(const TScalarType &factor, bool pre=0);

  /** Compose affine transformation with an elementary rotation
   *
   * This method composes self with a rotation that affects two
   * specified axes, replacing the current value of self.  The
   * rotation angle is in radians.  The axis of rotation goes
   * through the origin.  The transformation is given by
   *
   * y[axis1] =  cos(angle)*x[axis1] + sin(angle)*x[axis2]
   * y[axis2] = -sin(angle)*x[axis1] + cos(angle)*x[axis2].
   *
   * All coordinates other than axis1 and axis2 are unchanged;
   * a rotation of pi/2 radians will carry +axis1 into +axis2.
   * The rotation is precomposed with self if pre is true, and
   * postcomposed otherwise. */
  void Rotate(int axis1, int axis2, TScalarType angle, bool pre=0);

  /** Compose 2D affine transformation with a rotation
   *
   * This method composes self, which must be a 2D affine
   * transformation, with a clockwise rotation through a given angle
   * in radians.  The center of rotation is the origin.  The
   * rotation is precomposed with self if pre is true, and
   * postcomposed otherwise.
   *
   * \warning Only to be use in two dimensions
   *
   * \todo Find a way to generate a compile-time error
   *       is this is used with NDimensions != 2. */
  void Rotate2D(TScalarType angle, bool pre=0);

  /** Compose 3D affine transformation with a rotation
   *
   * This method composes self, which must be a 3D affine
   * transformation, with a clockwise rotation around a specified
   * axis.  The rotation angle is in radians; the axis of rotation
   * goes through the origin.  The rotation is precomposed with self
   * if pre is true, and postcomposed otherwise.
   *
   * \warning Only to be used in dimension 3
   *
   * \todo Find a way to generate a compile-time error
   * is this is used with NDimensions != 3. */
  void Rotate3D(const OutputVectorType &axis, TScalarType angle, bool pre=0);

  /** Compose affine transformation with a shear
   *
   * This method composes self with a shear transformation,
   * replacing the original contents of self.  The shear is
   * precomposed with self if pre is true, and postcomposed
   * otherwise.  The transformation is given by
   *
   * y[axis1] = x[axis1] + coef*x[axis2]
   * y[axis2] =                 x[axis2]. **/
  void Shear(int axis1, int axis2, TScalarType coef, bool pre=0);

  /** Transform by an affine transformation
   *
   * This method applies the affine transform given by self to a
   * given point or vector, returning the transformed point or
   * vector.  The TransformPoint method transforms its argument as
   * an affine point, whereas the TransformVector method transforms
   * its argument as a vector. */
  OutputPointType     TransformPoint (const InputPointType  &point ) const;
  OutputVectorType    TransformVector(const InputVectorType &vector) const;
  OutputVnlVectorType TransformVector(const InputVnlVectorType &vector) const;
  OutputCovariantVectorType TransformCovariantVector(
                             const InputCovariantVectorType &vector) const;
  
  /** Back transform by an affine transformation
   *
   * This method finds the point or vector that maps to a given
   * point or vector under the affine transformation defined by
   * self.  If no such point exists, an exception is thrown.   **/
  inline InputPointType   BackTransform(const OutputPointType  &point ) const;
  inline InputVectorType  BackTransform(const OutputVectorType &vector) const;
  inline InputVnlVectorType BackTransform(const OutputVnlVectorType &vector) const;

  inline InputCovariantVectorType BackTransform(
                              const OutputCovariantVectorType &vector) const;

  /** Back transform a point by an affine transform
   *
   * This method finds the point that maps to a given point under
   * the affine transformation defined by self.  If no such point
   * exists, an exception is thrown.  The returned value is (a
   * pointer to) a brand new point created with new. */
  InputPointType  BackTransformPoint(const OutputPointType  &point) const;

  /** Find inverse of an affine transformation
   *
   * This method creates and returns a new AffineTransform object
   * which is the inverse of self.  If self is not invertible,
   * an exception is thrown.   **/
  AffineTransform::Pointer Inverse(void) const;

  /** Compute distance between two affine transformations
   *
   * This method computes a ``distance'' between two affine
   * transformations.  This distance is guaranteed to be a metric,
   * but not any particular metric.  (At the moment, the algorithm
   * is to collect all the elements of the matrix and offset into a
   * vector, and compute the euclidean (L2) norm of that vector.
   * Some metric which could be used to estimate the distance between
   * two points transformed by the affine transformation would be
   * more useful, but I don't have time right now to work out the
   * mathematical details.) */
  ScalarType Metric(const Self * other) const;

  /** This method computes the distance from self to the identity
   * transformation, using the same metric as the one-argument form
   * of the Metric() method. **/
  ScalarType Metric(void) const;

  /** Print contents of an AffineTransform */
  void PrintSelf(std::ostream &s, Indent indent) const;

  /** Compute the Jacobian of the transformation
   *
   * This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the transform
   * is invertible at this point. */
  const JacobianType & GetJacobian(const InputPointType  &point ) const;

protected:
  /** Construct an AffineTransform object
   *
   * This method constructs a new AffineTransform object and
   * initializes the matrix and offset parts of the transformation
   * to values specified by the caller.  If the arguments are
   * omitted, then the AffineTransform is initialized to an identity
   * transformation in the appropriate number of dimensions.   **/
  AffineTransform(const MatrixType &matrix, const OutputVectorType &offset);
  AffineTransform();      
  
  /** Destroy an AffineTransform object   **/
  virtual ~AffineTransform();

  /** Recompute inverse of the transformation matrix   **/
  void RecomputeInverse();

private:
  AffineTransform(const Self & other);
  const Self & operator=( const Self & );

  MatrixType         m_Matrix;       // Matrix of the transformation
  OffsetType         m_Offset;       // Offset of the transformation
  MatrixType         m_Inverse;      // Inverse of the matrix
  bool               m_Singular;     // Is m_Inverse singular?


}; //class AffineTransform

}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAffineTransform.txx"
#endif

#endif /* __itkAffineTransform_h */





