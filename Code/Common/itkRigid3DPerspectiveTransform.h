/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRigid3DPerspectiveTransform.h
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

#ifndef __itkRigid3DPerspectiveTransform_h
#define __itkRigid3DPerspectiveTransform_h

#include "itkExceptionObject.h"
#include "vnl/vnl_quaternion.h"
#include <iostream>



namespace itk
{

/**
 *
 * Rigid3DTramsform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation and translation to the 3D space
 * followed by a projection to 2D space along the Z axis.
 *
 * \ingroup Transforms
 *
 **/

template <
    class TScalarType=double>    // Data type for scalars (float or double)
class ITK_EXPORT Rigid3DPerspectiveTransform  

{
public:

    /**
     * Standard Self Typedef
     */
    typedef Rigid3DPerspectiveTransform Self;

    /// Standard scalar type for this class
    typedef TScalarType ScalarType;

    /// Dimension of the domain space
    enum 
    { 
      InputSpaceDimension      = 3,
      OutputSpaceDimension     = 2
    };

    /// Standard matrix type for this class
    typedef Matrix<TScalarType, InputSpaceDimension, InputSpaceDimension> MatrixType;

    /// Standard vector type for this class
    typedef Vector<TScalarType, InputSpaceDimension> VectorType;

    /// Standard coordinate point type for this class
    typedef Point<TScalarType, InputSpaceDimension>    InputPointType;

    /// Standard coordinate point type for this class
    typedef Point<TScalarType, OutputSpaceDimension>    OutputPointType;

    /// Standard vnl_quaternion type
    typedef vnl_quaternion<TScalarType>           VnlQuaternionType;

    /**
     * Construct an Rigid3DPerspectiveTransform object
     *
     **/
    Rigid3DPerspectiveTransform();

    /**
     * Copy a Rigid3DPerspectiveTransform object
     *
     * This method creates a new Rigid3DPerspectiveTransform object and
     * initializes it to be a copy of an existing Rigid3DPerspectiveTransform.
     **/
    Rigid3DPerspectiveTransform(const Self & other);

    /**
     * Destroy an Rigid3DPerspectiveTransform object
     **/
    ~Rigid3DPerspectiveTransform();

    /**
     * Get offset of an Rigid3DPerspectiveTransform
     *
     * This method returns the value of the offset of the
     * Rigid3DPerspectiveTransform.
     **/
    const VectorType & GetOffset() const
        { return m_Offset; }


    /**
     * Get rotation from an Rigid3DPerspectiveTransform
     *
     * This method returns the value of the rotation of the
     * Rigid3DPerspectiveTransform.
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
     * This method sets the offset of an Rigid3DPerspectiveTransform to a
     * value specified by the user.
     **/
    void SetOffset(const VectorType &offset)
        { m_Offset = offset; return; }


    /**
     * Set Rotation
     *
     * This method sets the rotation of an Rigid3DPerspectiveTransform to a
     * value specified by the user.
     **/
    void SetRotation(const VnlQuaternionType &rotation);

    /**
     * Set the Focal Distance of the projection
     *
     * This method sets the focal distance for the perspective
     * projection to a value specified by the user.
     **/
    void SetFocalDistance( TScalarType focalDistance )
        { m_FocalDistance = focalDistance; }

    /**
     * Set the Height of the output plan
     *
     * This method sets the height of the output plan
     * to a value specified by the user.
     * This value is used to scale and center the points 
     * at the output
     **/
    void SetHeight( TScalarType height )
        { m_Height = height; }


    /**
     * Set the Width of the output plan
     *
     * This method sets the width of the output plan
     * to a value specified by the user.
     * This value is used to scale and center the points 
     * at the output
     **/
    void SetWidth( TScalarType width )
        { m_Width = width; }


    /**
     * Transform by a Rigid3DPerspectiveTransform
     *
     * This method applies the affine transform given by self to a
     * given point or vector, returning the transformed point or
     * vector.
     **/
    OutputPointType  Transform(const InputPointType  &point ) const;

    /**
     * Print contents of an Rigid3DPerspectiveTransform
     **/
    std::ostream & PrintSelf(std::ostream &s) const;

private:

    // Offset of the transformation
    VectorType          m_Offset;   

    // Rotation of the transformation
    VnlQuaternionType   m_Rotation; 

    // Set Focal distance of the projection
    TScalarType         m_FocalDistance;  

    // Set the height of the output plan
    TScalarType         m_Height;  

    // Set the width of the output plan
    TScalarType         m_Width;  

    // matrix representation of the rotation
    MatrixType          m_DirectMatrix;   


}; //class Rigid3DPerspectiveTransform:


/**
 * Print the offset of a Rigid3DPerspectiveTransform
 * This method prints the offset of the
 * Rigid3DPerspectiveTransform as n vector.
 **/
                                                              
 template<class ScalarType>
 inline
 std::ostream &
 operator<< (std::ostream &s, Rigid3DPerspectiveTransform<ScalarType> &transf)
 {
     s << transf.GetOffset() << std::endl;
     s << transf.GetRotation() << std::endl;
     return s;
 }


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRigid3DPerspectiveTransform.txx"
#endif

#endif /* __itkRigid3DPerspectiveTransform_h */
