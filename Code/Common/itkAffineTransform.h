/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAffineTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
 * \ingroup Transforms
 *
 *
 * \todo Is there any real value in allowing the user to template
 * over the scalar type?  Perhaps it should always be double, unless
 * there's a compatibility problem with the Point class.
 *
 *
 * \todo Add methods to transform (or back transform)
 *   many points or vectors at once?
 *
 * \todo  Add reflection?
 *
 *
 **/

template <
 class TScalarType=double,         // Data type for scalars (e.g. float or double)
 unsigned int NDimensions=3,       // Number of dimensions in the input space
 class TParameters = Point< double, NDimensions*(NDimensions+1) >,
 class TJacobianType = Matrix<double,NDimensions,NDimensions*(NDimensions+1) > 
      >  
class AffineTransform : public Transform< TScalarType,
                                          NDimensions, 
                                          NDimensions,
                                          TParameters,
                                          TJacobianType >
{
public:

    /**
     * Standard Self Typedef
     */
    typedef AffineTransform  Self;

    /// Standard scalar type for this class
    typedef TScalarType ScalarType;

    /// Dimension of the domain space
    enum { SpaceDimension = NDimensions };


    /**
     * Standard "Superclass" typedef.
     */
    typedef Transform< TScalarType, NDimensions,
                       NDimensions, TParameters, 
                       TJacobianType >             Superclass;


    /** 
     * Smart pointer typedef support 
     */
    typedef SmartPointer<Self>        Pointer;
    typedef SmartPointer<const Self>  ConstPointer;


    /** 
     * Run-time type information (and related methods).
     */
    itkTypeMacro( AffineTransform, Transform );


    /** 
     * New macro for creation of through a Smart Pointer
     */
    itkNewMacro( Self );



    /**
     * Standard vector type for this class
     */
    typedef Vector<TScalarType, SpaceDimension>  InputVectorType;
    typedef Vector<TScalarType, SpaceDimension>  OutputVectorType;


    /**
     * Standard covariant vector type for this class
     */
    typedef CovariantVector<TScalarType, SpaceDimension>  InputCovariantVectorType;
    typedef CovariantVector<TScalarType, SpaceDimension>  OutputCovariantVectorType;


    /**
     * Standard vnl_vector type for this class
     */
    typedef vnl_vector_fixed<TScalarType, SpaceDimension>  InputVnlVectorType;
    typedef vnl_vector_fixed<TScalarType, SpaceDimension>  OutputVnlVectorType;


    /**
     * Standard coordinate point type for this class
     */
    typedef Point<TScalarType, SpaceDimension> InputPointType;
    typedef Point<TScalarType, SpaceDimension> OutputPointType;

    
    /**
     * Standard matrix type for this class
     */
    typedef Matrix<TScalarType, SpaceDimension, SpaceDimension> MatrixType;

    /**
     * Standard offset type for this class
     */
    typedef     OutputVectorType    OffsetType;


    /**
     * Get offset of an AffineTransform
     *
     * This method returns the offset value of the AffineTransform.
     *
     **/
    const OffsetType & GetOffset(void) const
        { return m_Offset; }


    /**
     * Get matrix of an AffineTransform
     *
     * This method returns the value of the matrix of the
     * AffineTransform.
     **/
    const MatrixType & GetMatrix() const
        { return m_Matrix; }


    /**
     * Set the transformation to an Identity
     *
     * This sets the matrix to identity and the Offset to null
     *
     **/
    void SetIdentity( void )
    { m_Matrix.SetIdentity();
      m_Offset.Fill( 0.0 ); }


    /**
     * Get inverse matrix of an AffineTransform
     *
     * This method returns the value of the inverse matrix of
     * the AffineTransform.  It's not clear that this is useful
     * except for debugging the class itself.
     *
     * \todo Do something reasonable if the transform is singular.
     **/
    const MatrixType & GetInverse() const
        { if( m_Singular )
          { 
            throw ExceptionObject();
          }
          return m_Inverse; }


    /**
     * Set offset of an Affine Transform
     *
     * This method sets the offset of an AffineTransform to a
     * value specified by the user.
     **/
    void SetOffset(const OffsetType &offset)
        { m_Offset = offset; return; }



    /**
     * Set matrix of an AffineTransform
     *
     * This method sets the matrix of an AffineTransform to a
     * value specified by the user.
     **/
    void SetMatrix(const MatrixType &matrix)
        { m_Matrix = matrix; RecomputeInverse(); return; }


    
    /**
     * Compose with another AffineTransform
     *
     * This method composes self with another AffineTransform of the
     * same dimension, modifying self to be the composition of self
     * and other.  If the argument pre is true, then other is
     * precomposed with self; that is, the resulting transformation
     * consists of first applying other to the source, followed by
     * self.  If pre is false or omitted, then other is post-composed
     * with self; that is the resulting transformation consists of
     * first applying self to the source, followed by other.
     *
     **/
    void Compose(const Self * other, bool pre=0);



    /**
     * Compose affine transformation with a translation
     *
     * This method modifies self to include a translation of the
     * origin.  The translation is precomposed with self if pre is
     * true, and postcomposed otherwise.
     * 
     **/
    void Translate(const OutputVectorType &offset, bool pre=0);



    /**
     * Compose affine transformation with a scaling
     *
     * This method modifies self to magnify the source by a given
     * factor along each axis.  If all factors are the same, or only a
     * single factor is given, then the scaling is isotropic;
     * otherwise it is anisotropic.  If an odd number of factors are
     * negative, then the parity of the image changes.  If any of the
     * factors is zero, then the transformation becomes a projection
     * and is not invertible.  The scaling is precomposed with self if
     * pre is true, and postcomposed otherwise.
     *
     *
     **/
    void Scale(const OutputVectorType &factor, bool pre=0);
    void Scale(const ScalarType &factor, bool pre=0);



    /**
     * Compose affine transformation with an elementary rotation
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
     * postcomposed otherwise.
     **/
    void Rotate(int axis1, int axis2, double angle, bool pre=0);



    /**
     * Compose 2D affine transformation with a rotation
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
     *       is this is used with NDimensions != 2.
     **/
    void Rotate2D(double angle, bool pre=0);



    /**
     * Compose 3D affine transformation with a rotation
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
     * is this is used with NDimensions != 3.
     **/
    void Rotate3D(const OutputVectorType &axis, double angle, bool pre=0);



    /**
     * Compose affine transformation with a shear
     *
     * This method composes self with a shear transformation,
     * replacing the original contents of self.  The shear is
     * precomposed with self if pre is true, and postcomposed
     * otherwise.  The transformation is given by
     *
     * y[axis1] = x[axis1] + coef*x[axis2]
     * y[axis2] =                 x[axis2].
     *
     **/
    void Shear(int axis1, int axis2, double coef, bool pre=0);



    /**
     * Transform by an affine transformation
     *
     * This method applies the affine transform given by self to a
     * given point or vector, returning the transformed point or
     * vector.  The TransformPoint method transforms its argument as
     * an affine point, whereas the TransformVector method transforms
     * its argument as a vector.
     **/
    OutputPointType     TransformPoint (const InputPointType  &point ) const;
    OutputVectorType    TransformVector(const InputVectorType &vector) const;
    OutputVnlVectorType TransformVector(const InputVnlVectorType &vector) const;

    OutputCovariantVectorType TransformCovariantVector(
                               const InputCovariantVectorType &vector) const;



    /**
     * Back transform by an affine transformation
     *
     * This method finds the point or vector that maps to a given
     * point or vector under the affine transformation defined by
     * self.  If no such point exists, an exception is thrown.
     **/
    inline InputPointType      BackTransform(const OutputPointType  &point ) const;
    inline InputVectorType     BackTransform(const OutputVectorType &vector) const;
    inline InputVnlVectorType  BackTransform(const OutputVnlVectorType &vector) const;

    inline InputCovariantVectorType BackTransform(
                                 const OutputCovariantVectorType &vector) const;


    /**
     * Back transform a point by an affine transform
     *
     * This method finds the point that maps to a given point under
     * the affine transformation defined by self.  If no such point
     * exists, an exception is thrown.  The returned value is (a
     * pointer to) a brand new point created with new.
     **/
    InputPointType  BackTransformPoint(const OutputPointType  &point) const;

    /**
     * \todo The validity of this method has to be verified
     */
    OutputVectorType  BackTransformPoint(const InputVectorType & point) const;




    /**
     * Find inverse of an affine transformation
     *
     * This method creates and returns a new AffineTransform object
     * which is the inverse of self.  If self is not invertible,
     * an exception is thrown.
     **/
    AffineTransform::Pointer Inverse(void) const;




    /**
     * Compute distance between two affine transformations
     *
     * This method computes a ``distance'' between two affine
     * transformations.  This distance is guaranteed to be a metric,
     * but not any particular metric.  (At the moment, the algorithm
     * is to collect all the elements of the matrix and offset into a
     * vector, and compute the euclidean (L2) norm of that vector.
     * Some metric which could be used to estimate the distance between
     * two points transformed by the affine transformation would be
     * more useful, but I don't have time right now to work out the
     * mathematical details.)
     *
     **/
    double Metric(const Self &other) const;




    /** Compute distance to the identity transform
     *
     * This method computes the distance from self to the identity
     * transformation, using the same metric as the one-argument form
     * of the Metric() method.
     *
     **/
    double Metric(void) const;




    /**
     * Print contents of an AffineTransform
     **/
    std::ostream & PrintSelf(std::ostream &s) const;
    


protected:

    /**
     * Construct an AffineTransform object
     *
     * This method constructs a new AffineTransform object and
     * initializes the matrix and offset parts of the transformation
     * to values specified by the caller.  If the arguments are
     * omitted, then the AffineTransform is initialized to an identity
     * transformation in the appropriate number of dimensions.
     **/
    AffineTransform(const MatrixType &matrix, const OutputVectorType &offset);
    AffineTransform();      


    /**
     * Copy an AffineTransform object
     *
     * This method creates a new AffineTransform object and 
     * initializes it to be a copy of an existing AffineTransform.
     **/
    AffineTransform(const Self & other);

    /**
     * Destroy an AffineTransform object
     **/
    virtual ~AffineTransform();

    /**
     * Assignment operator
     **/
    const Self & operator=( const Self & );


    /**
     * Recompute inverse of the transformation matrix
     **/
    void RecomputeInverse();


private:

    MatrixType         m_Matrix;       // Matrix of the transformation
    
    OffsetType         m_Offset;       // Offset of the transformation
    
    MatrixType         m_Inverse;      // Inverse of the matrix
    
    bool               m_Singular;     // Is m_Inverse singular?


}; //class AffineTransform


/**
 * Print the matrix and offset of an AffineTransform
 *
 * This method prints the matrix and offset of the
 * AffineTransform as an n x (n+1) matrix, with the
 * offset as the last column.  This is the same as
 * the conventional homogeneous coordinate representation,
 * except that the last row is omitted.
 **/
template<class TScalarType, unsigned int NDimensions,
 class TParameters, class TJacobianType >
inline
std::ostream &
operator<< (std::ostream &s, AffineTransform< TScalarType,
                                              NDimensions, 
                                              TParameters,
                                              TJacobianType > &affine)
{
    return affine.PrintSelf(s);
}


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAffineTransform.txx"
#endif

#endif /* __itkAffineTransform_h */





