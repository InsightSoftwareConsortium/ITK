/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSFile: itkRigid3DTransform.h.v $
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

#ifndef __itkRigid3DTransform_h
#define __itkRigid3DTransform_h

#include <iostream>
#include "itkTransformation.h"
#include "itkExceptionObject.h"
#include "vnl/vnl_quaternion.h"



namespace itk
{

/**
 *
 * Rigid3DTramsform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation and translation to the space
 *
 **/

template <
    class TScalarType=double,    // Data type for scalars (float or double)
    unsigned int NDimensions=3 > // Number of dimensions
class ITK_EXPORT Rigid3DTransform : 
            public Transformation< TScalarType, NDimensions >
{
public:

    /**
     * Standard Self Typedef
     */
    typedef Rigid3DTransform Self;

    /// Standard scalar type for this class
    typedef TScalarType ScalarType;

    /// Dimension of the domain space
    enum { SpaceDimension     = NDimensions };

    /// Standard matrix type for this class
    typedef Matrix<TScalarType, SpaceDimension, SpaceDimension> MatrixType;

    /// Standard vector type for this class
    typedef Vector<TScalarType, SpaceDimension> VectorType;

    /// Standard covariant vector type for this class
    typedef CovariantVector<TScalarType, SpaceDimension> CovariantVectorType;

    /// Standard vnl_vector type for this class
    typedef vnl_vector_fixed<TScalarType, SpaceDimension> VnlVectorType;

    /// Standard coordinate point type for this class
    typedef Point<TScalarType, SpaceDimension>    PointType;

    /// Standard vnl_quaternion type
    typedef vnl_quaternion<TScalarType>           VnlQuaternionType;

    /**
     * Construct an Rigid3DTransform object
     *
     **/
    Rigid3DTransform(
              const VectorType &offset, 
              const VnlQuaternionType & rotation);
    Rigid3DTransform();

    /**
     * Copy a Rigid3DTransform object
     *
     * This method creates a new Rigid3DTransform object and
     * initializes it to be a copy of an existing Rigid3DTransform.
     **/
    Rigid3DTransform(const Self & other);

    /**
     * Destroy an Rigid3DTransform object
     **/
    ~Rigid3DTransform();

    /**
     * Get offset of an Rigid3DTransform
     *
     * This method returns the value of the offset of the
     * Rigid3DTransform.
     **/
    const VectorType & GetOffset() const
        { return m_Offset; }


    /**
     * Get rotation from an Rigid3DTransform
     *
     * This method returns the value of the rotation of the
     * Rigid3DTransform.
     **/
    const VnlQuaternionType & GetRotation() const
        { return m_Rotation; }


    /**
     * Assignment operator
     **/
    const Self & operator=( const Self & );


    /**
     * Set offset of an Translation Transform
     *
     * This method sets the offset of an Rigid3DTransform to a
     * value specified by the user.
     **/
    void SetOffset(const VectorType &offset)
        { m_Offset = offset; return; }


    /**
     * Set Rotatoin of an Translation Transform
     *
     * This method sets the rotation of an Rigid3DTransform to a
     * value specified by the user.
     **/
    void SetRotation(const VnlQuaternionType &rotation)
        { m_Rotation = rotation; return; }


    /**
     * Compose with another Rigid3DTransform
     *
     **/
    void Compose(const Self &other, bool pre=0);

    /**
     * Compose the transformation with a translation
     *
     * This method modifies self to include a translation of the
     * origin.  The translation is precomposed with self if pre is
     * true, and postcomposed otherwise.
     **/
    void Translate(const VectorType &offset, bool pre=0);

    /**
     * Compose the transformation with a rotation
     *
     * This method modifies self to include a translation of the
     * origin.  The translation is precomposed with self if pre is
     * true, and postcomposed otherwise.
     **/
    void Rotate(const VnlQuaternionType &offset, bool pre=0);
    void Rotate(const VectorType & axis, TScalarType angle );

    /**
     * Transform by an affine transformation
     *
     * This method applies the affine transform given by self to a
     * given point or vector, returning the transformed point or
     * vector.
     **/
    PointType           Transform(const PointType  &point ) const;
    VectorType          Transform(const VectorType &vector) const;
    VnlVectorType       Transform(const VnlVectorType &vector) const;

    CovariantVectorType Transform(
                                   const CovariantVectorType &vector) const;

    /**
     * Back transform by an affine transformation
     *
     * This method finds the point or vector that maps to a given
     * point or vector under the affine transformation defined by
     * self.  If no such point exists, an exception is thrown.
     **/
    inline PointType           BackTransform(const PointType  &point ) const;
    inline VectorType          BackTransform(const VectorType &vector) const;
    inline VnlVectorType       BackTransform(const VnlVectorType &vector) const;

    inline CovariantVectorType BackTransform(
                                       const CovariantVectorType &vector) const;
    /**
     * Print contents of an Rigid3DTransform
     **/
    std::ostream & PrintSelf(std::ostream &s) const;

    /**
     * Find inverse of an affine transformation
     *
     * This method creates and returns a new Rigid3DTransform object
     * which is the inverse of self.  If self is not invertible,
     * an exception is thrown.
     **/
    Rigid3DTransform Inverse();


private:

    // Offset of the transformation
    VectorType          m_Offset;   

    // Rotation of the transformation
    VnlQuaternionType   m_Rotation; 

    // matrix representation of the rotation
    MatrixType          m_DirectMatrix;   

    // representation of the inverse rottion
    MatrixType          m_InverseMatrix;  

}; //class Rigid3DTransform


/**
 * Print the offset of a Rigid3DTransform
 * This method prints the offset of the
 * Rigid3DTransform as n vector.
 **/
                                                              
 template<class ScalarType, unsigned int NDimensions>
 inline
 std::ostream &
 operator<< (std::ostream &s, Rigid3DTransform<ScalarType, NDimensions> &affine)
 {
     s << m_Offset << std::endl;
     s << m_Rotation << std::endl;
     return s;
 }


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRigid3DTransform.txx"
#endif

#endif /* __itkRigid3DTransform_h */
