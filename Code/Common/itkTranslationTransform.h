/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTranslationTransform.h
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

#ifndef __itkTranslationTransform_h
#define __itkTranslationTransform_h

#include <iostream>
#include "itkTransform.h"
#include "itkExceptionObject.h"
#include "itkMatrix.h"




namespace itk
{

/**
 *
 * Translation transformation of a vector space (e.g. space coordinates)
 *
 * The same functionality could be obtained by using the Affine tranform,
 * but with a large difference in performace
 *
 * \ingroup Transforms
 **/

template <
    class TScalarType=double,          // Data type for scalars (float or double)
    unsigned int NDimensions=3,
    class TParameters = Point< double, NDimensions >,
    class TJacobianType = Matrix<double,NDimensions,NDimensions > 
    >  // Number of dimensions
class TranslationTransform : public Transform< TScalarType, 
                                               NDimensions,
                                               NDimensions,
                                               TParameters,
                                               TJacobianType >
{
public:

    /**
     * Standard Self Typedef
     */
    typedef TranslationTransform Self;

    /// Standard scalar type for this class
    typedef TScalarType ScalarType;

    /// Dimension of the domain space
    enum { SpaceDimension      = NDimensions,
           ParametersDimension = NDimensions };

    /// Standard parameters container
    typedef TParameters ParametersType;


    /// Standard "Superclass" typedef.
    typedef Transform< TScalarType, NDimensions,
                       NDimensions, TParameters, 
                       TJacobianType >             Superclass;


    /// Smart pointer typedef support 
    typedef SmartPointer<Self>        Pointer;
    typedef SmartPointer<const Self>  ConstPointer;


    /// Run-time type information (and related methods).
    itkTypeMacro( AffineTransform, Transform );


    /// New macro for creation of through a Smart Pointer
    itkNewMacro( Self );


    /// Standard vector type for this class
    typedef Vector<TScalarType, SpaceDimension> InputVectorType;
    typedef Vector<TScalarType, SpaceDimension> OutputVectorType;

    /// Standard covariant vector type for this class
    typedef CovariantVector<TScalarType, SpaceDimension> InputCovariantVectorType;
    typedef CovariantVector<TScalarType, SpaceDimension> OutputCovariantVectorType;

    /// Standard vnl_vector type for this class
    typedef vnl_vector_fixed<TScalarType, SpaceDimension> InputVnlVectorType;
    typedef vnl_vector_fixed<TScalarType, SpaceDimension> OutputVnlVectorType;

    /// Standard coordinate point type for this class
    typedef Point<TScalarType, SpaceDimension> InputPointType;
    typedef Point<TScalarType, SpaceDimension> OutputPointType;


    /**
     * Get offset of an TranslationTransform
     *
     * This method returns the value of the offset of the
     * TranslationTransform.
     **/
    const OutputVectorType & GetOffset(void) const
        { return m_Offset; }

    /**
     * Set Parameters
     *
     * This method sets the parameters for the transform
     * value specified by the user.
     **/
    void SetParameters(const ParametersType & parameters);


    /**
     * Set offset of an Translation Transform
     *
     * This method sets the offset of an TranslationTransform to a
     * value specified by the user.
     **/
    void SetOffset(const OutputVectorType &offset)
        { m_Offset = offset; return; }

    /**
     * Compose with another TranslationTransform
     *
     **/
    void Compose(const Self * other, bool pre=0);

    /**
     * Compose affine transformation with a translation
     *
     * This method modifies self to include a translation of the
     * origin.  The translation is precomposed with self if pre is
     * true, and postcomposed otherwise.
     **/
    void Translate(const OutputVectorType &offset, bool pre=0);

    /**
     * Transform by an affine transformation
     *
     * This method applies the affine transform given by self to a
     * given point or vector, returning the transformed point or
     * vector.
     **/
    OutputPointType     TransformPoint(const InputPointType  &point ) const;
    OutputVectorType    TransformVector(const InputVectorType &vector) const;
    OutputVnlVectorType TransformVnlVector(const InputVnlVectorType &vector) const;

    OutputCovariantVectorType TransformCovariantVector(
                                   const InputCovariantVectorType &vector) const;

    /**
     * Back transform by an affine transformation
     *
     * This method finds the point or vector that maps to a given
     * point or vector under the affine transformation defined by
     * self.  If no such point exists, an exception is thrown.
     **/
    inline InputPointType     BackTransform(const OutputPointType  &point ) const;
    inline InputVectorType    BackTransform(const OutputVectorType &vector) const;
    inline InputVnlVectorType BackTransform(const OutputVnlVectorType &vector) const;

    inline InputCovariantVectorType BackTransform(
                                       const OutputCovariantVectorType &vector) const;
    /**
     * Print contents of an TranslationTransform
     **/
    std::ostream & PrintSelf(std::ostream &s) const;

    /**
     * Find inverse of an affine transformation
     *
     * This method creates and returns a new TranslationTransform object
     * which is the inverse of self.  If self is not invertible,
     * an exception is thrown.
     **/
    TranslationTransform::Pointer Inverse(void) const;

protected:
    /**
     * Construct an TranslationTransform object
     *
     **/
    TranslationTransform();

    /**
     * Copy an TranslationTransform object
     *
     * This method creates a new TranslationTransform object and
     * initializes it to be a copy of an existing TranslationTransform.
     **/
    TranslationTransform(const Self & other);

    /**
     * Destroy an TranslationTransform object
     **/
    ~TranslationTransform();

    /**
     * Assignment operator
     **/
    const Self & operator=( const Self & );



private:
    OutputVectorType   m_Offset;       // Offset of the transformation


}; //class TranslationTransform


/**
 * Print the offset of a TranslationTransform
 * This method prints the offset of the
 * TranslationTransform as n vector.
 **/
                                                              
 template<class ScalarType, unsigned int NDimensions,
          class TParameters, class TJacobianType>
 inline
 std::ostream &
 operator<< (std::ostream &s, TranslationTransform<ScalarType, 
                                 NDimensions, TParameters, 
                                 TJacobianType> &affine)
 {
     return affine.PrintSelf(s);
 }


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTranslationTransform.txx"
#endif

#endif /* __itkTranslationTransform_h */
