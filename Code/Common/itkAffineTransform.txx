/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAffineTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

NOTE: The code in this file is tested partly by itkAffineTransformTest.cxx
and partly by itkImageMomentsTest.cxx.
=========================================================================*/


#include "itkPoint.h"

#include "vnl/vnl_vector_fixed.h"
#include "vnl/vnl_matrix_fixed.h"
#include "vnl/algo/vnl_matrix_inverse.h"


namespace itk
{

    // Constructor with default arguments
    template<class ScalarType, int NDimensions>
    AffineTransform<ScalarType, NDimensions>::
    AffineTransform(void)
    {
        int i, j;
        for (i = 0; i < NDimensions; i++) {
            for (j = 0; j < NDimensions; j++) {
                m_Matrix [i][j] = 0.0;
                m_Inverse[i][j] = 0.0;
            }
            m_Matrix [i][i] = 1.0;
            m_Inverse[i][i] = 1.0;
            m_Offset [i] = 0.0;
        }
        m_Singular = false;
    }
 
    // Constructor with explicit arguments
    template<class ScalarType, int NDimensions>
    AffineTransform<ScalarType, NDimensions>::
    AffineTransform(const MatrixType &matrix, const VectorType &offset)
    {
        m_Matrix = matrix;
        m_Offset = offset;
        RecomputeInverse();
    }

    
    // Copy Constructor
    template <class ScalarType, int NDimensions>
    AffineTransform<ScalarType, NDimensions>
    ::AffineTransform( const AffineTransform<ScalarType, NDimensions> & other )
    {
        m_Matrix    = other.m_Matrix;
        m_Offset    = other.m_Offset;
        m_Inverse   = other.m_Inverse;
        m_Singular  = other.m_Singular;
    }

    // Destructor
    template<class ScalarType, int NDimensions>
    AffineTransform<ScalarType, NDimensions>::
    ~AffineTransform()
    {
        return;
    }


    // Assignment Operator
    template <class ScalarType, int NDimensions>
    const AffineTransform<ScalarType, NDimensions> &
    AffineTransform<ScalarType, NDimensions>
    ::operator=( const Self & other )
    {
        m_Matrix   = other.m_Matrix;
        m_Offset   = other.m_Offset;
        m_Inverse  = other.m_Inverse;
        m_Singular = other.m_Singular;
        return *this;
    }


    // Print self
    template<class ScalarType, int NDimensions>
    std::ostream &
    AffineTransform<ScalarType, NDimensions>::
    PrintSelf(std::ostream &s) const
    {
        int i, j;
        for (i = 0; i < NDimensions; i++) {
            for (j = 0; j < NDimensions; j++)
                s << m_Matrix[i][j] << " ";
            s << m_Offset[i] << std::endl;
        }
        return s;
    }


    // Compose with another affine transformation
    template<class ScalarType, int NDimensions>
    void
    AffineTransform<ScalarType, NDimensions>::
    Compose(const Self &other, bool pre)
    {
        if (pre) {
            m_Offset = m_Matrix * other.m_Offset + m_Offset;
            m_Matrix = m_Matrix * other.m_Matrix;
        }
        else {
            m_Offset = other.m_Matrix * m_Offset + other.m_Offset;
            m_Matrix = other.m_Matrix * m_Matrix;
        }
        RecomputeInverse();

        return;
    }


    // Compose with a translation
    template<class ScalarType, int NDimensions>
    void
    AffineTransform<ScalarType, NDimensions>::
    Translate(const VectorType &offset, bool pre)
    {
        if (pre) {
            m_Offset += m_Matrix * offset;
        }
        else {
            m_Offset += offset;
        }
        RecomputeInverse();

        return;
    }


    // Compose with isotropic scaling
    template<class ScalarType, int NDimensions>
    void
    AffineTransform<ScalarType, NDimensions>::
    Scale(const ScalarType &factor, bool pre) {
        if (pre) {
            m_Matrix *= factor;
        }
        else {
            m_Matrix *= factor;
            m_Offset *= factor;
        }
        RecomputeInverse();
        return;
    }


    // Compose with anisotropic scaling
    template<class ScalarType, int NDimensions>
    void
    AffineTransform<ScalarType, NDimensions>::
    Scale(const VectorType &factor, bool pre) {
        MatrixType trans;
        int i, j;

        for (i = 0; i < NDimensions; i++) {
            for (j = 0; j < NDimensions; j++) {
                trans[i][j] = 0.0;
            }
            trans[i][i] = factor[i];
        }
        if (pre) {
            m_Matrix = m_Matrix * trans;
        }
        else {
            m_Matrix = trans * m_Matrix;
            m_Offset = trans * m_Offset;
        }
        RecomputeInverse();
        return;
    }

    // Compose with elementary rotation
    template<class ScalarType, int NDimensions>
    void
    AffineTransform<ScalarType, NDimensions>::
    Rotate(int axis1, int axis2, double angle, bool pre) {
        MatrixType trans;
        int i, j;

        for (i = 0; i < NDimensions; i++) {
            for (j = 0; j < NDimensions; j++) {
                trans[i][j] = 0.0;
            }
            trans[i][i] = 1.0;
        }
        trans[axis1][axis1] =  cos(angle);
        trans[axis1][axis2] =  sin(angle);
        trans[axis2][axis1] = -sin(angle);
        trans[axis2][axis2] =  cos(angle);
        if (pre) {
            m_Matrix = m_Matrix * trans;
        }
        else {
            m_Matrix = trans * m_Matrix;
            m_Offset = trans * m_Offset;
        }
        RecomputeInverse();
        return;
    }


    // Compose with 2D rotation
    // FIXME: Find a way to generate a compile-time error
    // is this is used with NDimensions != 2.
    template<class ScalarType, int NDimensions>
    void
    AffineTransform<ScalarType, NDimensions>::
    Rotate2D(double angle, bool pre)
    {
        vnl_matrix_fixed<ScalarType, 2, 2> trans;

        trans[0][0] =  cos(angle);
        trans[0][1] =  sin(angle);
        trans[1][0] = -sin(angle);
        trans[1][1] =  cos(angle);
        if (pre) {
            m_Matrix = m_Matrix * trans;
        }
        else {
            m_Matrix = trans * m_Matrix;
            m_Offset = trans * m_Offset;
        }
        RecomputeInverse();
        return;
    }


    // Compose with 3D rotation
    // FIXME: Find a way to generate a compile-time error
    // is this is used with NDimensions != 3.
    template<class ScalarType, int NDimensions>
    void
    AffineTransform<ScalarType, NDimensions>::
    Rotate3D(const VectorType &axis, double angle, bool pre)
    {
        vnl_matrix_fixed<ScalarType, 3, 3> trans;
        ScalarType r, x1, x2, x3;
        ScalarType q0, q1, q2, q3;

        // Convert the axis to a unit vector
        r = sqrt(axis[0]*axis[0] + axis[1]*axis[1] + axis[2]*axis[2]);
        x1 = axis[0] / r;
        x2 = axis[1] / r;
        x3 = axis[2] / r;

        // Compute quaternion elements
        q0 = cos(angle/2.0);
        q1 = x1 * sin(angle/2.0);
        q2 = x2 * sin(angle/2.0);
        q3 = x3 * sin(angle/2.0);
        
        // Compute elements of the rotation matrix
        trans[0][0] = q0*q0 + q1*q1 - q2*q2 - q3*q3;
        trans[0][1] = 2.0*(q1*q2 - q0*q3);
        trans[0][2] = 2.0*(q1*q3 + q0*q2);
        trans[1][0] = 2.0*(q1*q2 + q0*q3);
        trans[1][1] = q0*q0 + q2*q2 - q1*q1 - q3*q3;
        trans[1][2] = 2.0*(q2*q3 - q0*q1);
        trans[2][0] = 2.0*(q1*q3 - q0*q2);
        trans[2][1] = 2.0*(q2*q3 + q0*q1);
        trans[2][2] = q0*q0 + q3*q3 - q1*q1 - q2*q2;

        // Compose rotation matrix with the existing matrix
        if (pre) {
            m_Matrix = m_Matrix * trans;
        }
        else {
            m_Matrix = trans * m_Matrix;
            m_Offset = trans * m_Offset;
        }
        RecomputeInverse();
        return;
    }


    // Compose with elementary rotation
    template<class ScalarType, int NDimensions>
    void
    AffineTransform<ScalarType, NDimensions>::
    Shear(int axis1, int axis2, double coef, bool pre)
    {
        MatrixType trans;
        int i, j;

        for (i = 0; i < NDimensions; i++) {
            for (j = 0; j < NDimensions; j++) {
                trans[i][j] = 0.0;
            }
            trans[i][i] = 1.0;
        }
        trans[axis1][axis2] =  coef;
        if (pre) {
            m_Matrix = m_Matrix * trans;
        }
        else {
            m_Matrix = trans * m_Matrix;
            m_Offset = trans * m_Offset;
        }
        RecomputeInverse();
        return;
    }


    // Transform a point
    template<class ScalarType, int NDimensions>
    AffineTransform<ScalarType, NDimensions>::PointType
    AffineTransform<ScalarType, NDimensions>::
    Transform(const PointType &point) const {
        PointType  result;    // Converted point
        int i, j;
        
        for (i = 0; i < NDimensions; i++) {
            result[i] = m_Offset[i];
            for (j = 0; j < NDimensions; j++) {
                result[i] += m_Matrix[i][j]*point[j];
            }
        }
        return result;
    }


    // Transform a vector
    template<class ScalarType, int NDimensions>
    AffineTransform<ScalarType, NDimensions>::VectorType
    AffineTransform<ScalarType, NDimensions>::
    Transform(const VectorType &vect) const {
        return m_Matrix * vect;
    }


    // Transform a Vector
    template<class ScalarType, int NDimensions>
    Vector<ScalarType,NDimensions>  
    AffineTransform<ScalarType, NDimensions>::
    Transform(const Vector<ScalarType,NDimensions> &vec) const {
        Vector<ScalarType,NDimensions>  result;    // Converted vector
        int i, j;
        
        for (i = 0; i < NDimensions; i++) {
            result[i] = 0;    // should use a numeric trait ?
            for (j = 0; j < NDimensions; j++) {
                result[i] += m_Matrix[i][j]*vec[j];
            }
        }
        return result;
    }

   // Transform a CovariantVector
    template<class ScalarType, int NDimensions>
    CovariantVector<ScalarType,NDimensions>  
    AffineTransform<ScalarType, NDimensions>::
    Transform(const CovariantVector<ScalarType,NDimensions> &vec) const {
        CovariantVector<ScalarType,NDimensions>  result;    // Converted vector
        int i, j;
        
        for (i = 0; i < NDimensions; i++) {
            result[i] = 0;    // should use a numeric trait ?
            for (j = 0; j < NDimensions; j++) {
                result[i] += m_Inverse[j][i]*vec[j]; // Inverse transposed
            }
        }
        return result;
    }




    // Transform a given point which is represented as type VectorType
    // FIXME: Deprecated on 2001-01-02
    template<class ScalarType, int NDimensions>
    AffineTransform<ScalarType, NDimensions>::VectorType
    AffineTransform<ScalarType, NDimensions>::
    TransformPoint(const VectorType &point) {
        return m_Matrix * point + m_Offset;
    }


    // Transform a given point which is represented as type PointType
    // FIXME: Deprecated on 2001-01-02
    template<class ScalarType, int NDimensions>
    AffineTransform<ScalarType, NDimensions>::PointType
    AffineTransform<ScalarType, NDimensions>::
    TransformPoint(const PointType &point) {
        PointType result;                   // Converted point
        int i, j;

        for (i = 0; i < NDimensions; i++) {
            result[i] = m_Offset[i];
            for (j = 0; j < NDimensions; j++) {
                result[i] += m_Matrix[i][j] * point[j];
            }
        }
        return result;
    }

    // Back transform a point
    template<class ScalarType, int NDimensions>
    AffineTransform<ScalarType, NDimensions>::PointType
    AffineTransform<ScalarType, NDimensions>::
    BackTransform(const PointType &point) const {
        PointType result;       // Converted point
        ScalarType temp[NDimensions];
        int i, j;

        for (j = 0; j < NDimensions; j++) {
            temp[j] = point[j] - m_Offset[j];
        }

        for (i = 0; i < NDimensions; i++) {
            result[i] = 0.0;
            for (j = 0; j < NDimensions; j++) {
                result[i] += m_Inverse[i][j]*temp[j];
            }
        }
        return result;
    }

    // Back transform a vector
    template<class ScalarType, int NDimensions>
    AffineTransform<ScalarType, NDimensions>::VectorType
    AffineTransform<ScalarType, NDimensions>::
    BackTransform(const VectorType &vect ) const {
        return m_Inverse * vect;
    }


    // Back transform a given point which is represented as type VectorType
    // FIXME: deprecated on 2001-01-02
    template<class ScalarType, int NDimensions>
    AffineTransform<ScalarType, NDimensions>::VectorType
    AffineTransform<ScalarType, NDimensions>::
    BackTransformPoint(const VectorType &point) {
        return m_Inverse * (point - m_Offset);
    }

    // Back Transform a Vector
    template<class ScalarType, int NDimensions>
    Vector<ScalarType,NDimensions>  
    AffineTransform<ScalarType, NDimensions>::
    BackTransform(const Vector<ScalarType,NDimensions> &vec) const {
        Vector<ScalarType,NDimensions>  result;    // Converted vector
        int i, j;
        
        for (i = 0; i < NDimensions; i++) {
            result[i] = 0;    // should use a numeric trait ?
            for (j = 0; j < NDimensions; j++) {
                result[i] += m_Inverse[i][j]*vec[j];
            }
        }
        return result;
    }

    // Back Transform a CovariantVector
    template<class ScalarType, int NDimensions>
    CovariantVector<ScalarType,NDimensions>  
    AffineTransform<ScalarType, NDimensions>::
    BackTransform(const CovariantVector<ScalarType,NDimensions> &vec) const {
        CovariantVector<ScalarType,NDimensions>  result;    // Converted vector
        int i, j;
        
        for (i = 0; i < NDimensions; i++) {
            result[i] = 0;    // should use a numeric trait ?
            for (j = 0; j < NDimensions; j++) {
                result[i] += m_Matrix[j][i]*vec[j]; // Direct matrix transposed
            }
        }
        return result;
    }


    // Back transform a given point which is represented as type PointType
    // FIXME: deprecated on 2001-01-02
    template<class ScalarType, int NDimensions>
    AffineTransform<ScalarType, NDimensions>::PointType
    AffineTransform<ScalarType, NDimensions>::
    BackTransformPoint(const PointType &point) {
        PointType result;       // Converted point
        ScalarType temp[NDimensions];
        int i, j;

        for (j = 0; j < NDimensions; j++) {
            temp[j] = point[j] - m_Offset[j];
        }

        for (i = 0; i < NDimensions; i++) {
            res[i] = 0.0;
            for (j = 0; j < NDimensions; j++) {
                result[i] += m_Inverse[i][j]*temp[j];
            }
        }
        return result;
    }


    // Create and return an inverse transformation
    template<class ScalarType, int NDimensions>
    AffineTransform<ScalarType, NDimensions>
    AffineTransform<ScalarType, NDimensions>::
    Inverse()
    {
        Self result;
        result.m_Matrix   = m_Inverse;
        result.m_Inverse  = m_Matrix;
        result.m_Offset   = -m_Inverse*m_Offset;
        result.m_Singular = false;
        return result;
    }

    // Compute a distance between two affine transforms
    template<class ScalarType, int NDimensions>
    double
    AffineTransform<ScalarType, NDimensions>::
    Metric(const Self &other) const
    {
        double result = 0.0, term;

        for (int i = 0; i < NDimensions; i++) {
            for (int j = 0; j < NDimensions; j++) {
                term = m_Matrix[i][j] - other.m_Matrix[i][j];
                result += term * term;
            }
            term = m_Offset[i] - other.m_Offset[i];
            result += term * term;
        }
        return sqrt(result);
    }

    // Compute a distance between self and the identity transform
    template<class ScalarType, int NDimensions>
    double
    AffineTransform<ScalarType, NDimensions>::
    Metric(void) const
    {
        double result = 0.0, term;

        for (int i = 0; i < NDimensions; i++) {
            for (int j = 0; j < NDimensions; j++) {
                if (i == j)
                    term = m_Matrix[i][j] - 1.0;
                else
                    term = m_Matrix[i][j];
                result += term * term;
            }
            term = m_Offset[i];
            result += term * term;
        }
        return sqrt(result);
    }

    // Recompute the inverse matrix (internal)
    template<class ScalarType, int NDimensions>
    void
    AffineTransform<ScalarType, NDimensions>::
    RecomputeInverse()
    {
        // FIXME: the matrix inversion function appears not
        // to give any useful indication of a singular matrix
        m_Inverse  = vnl_matrix_inverse<ScalarType>(m_Matrix);
        m_Singular = false;
        return;
    }

} // namespace
