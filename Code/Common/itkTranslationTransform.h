/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTranslationTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkTranslationTransform_h
#define __itkTranslationTransform_h

#include <iostream>
#include "itkTransform.h"
#include "itkExceptionObject.h"
#include "itkMatrix.h"




namespace itk
{

/** \brief Translation transformation of a vector space (e.g. space coordinates)
 *
 * The same functionality could be obtained by using the Affine tranform,
 * but with a large difference in performace.
 *
 * \ingroup Transforms
 */
template <
    class TScalarType=double,          // Data type for scalars (float or double)
    unsigned int NDimensions=3>        // Number of dimensions
class ITK_EXPORT TranslationTransform : 
          public Transform< TScalarType, NDimensions, NDimensions >
{
public:
  /** Standard class typedefs. */
  typedef TranslationTransform Self;
  typedef Transform< TScalarType, NDimensions, NDimensions > Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
      
  /** New macro for creation of through the object factory.*/
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( TranslationTransform, Transform );

  /** Dimension of the domain space. */
  itkStaticConstMacro(SpaceDimension, unsigned int, NDimensions);
  itkStaticConstMacro(ParametersDimension, unsigned int, NDimensions);

  /** Standard scalar type for this class. */
  typedef typename Superclass::ScalarType ScalarType;

  /** Standard parameters container. */
  typedef typename Superclass::ParametersType ParametersType;

  /** Standard Jacobian container. */
  typedef typename Superclass::JacobianType JacobianType;

  /** Standard vector type for this class. */
  typedef Vector<TScalarType, itkGetStaticConstMacro(SpaceDimension)> InputVectorType;
  typedef Vector<TScalarType, itkGetStaticConstMacro(SpaceDimension)> OutputVectorType;

  /** Standard covariant vector type for this class. */
  typedef CovariantVector<TScalarType, itkGetStaticConstMacro(SpaceDimension)> InputCovariantVectorType;
  typedef CovariantVector<TScalarType, itkGetStaticConstMacro(SpaceDimension)> OutputCovariantVectorType;
  
  /** Standard vnl_vector type for this class. */
  typedef vnl_vector_fixed<TScalarType, itkGetStaticConstMacro(SpaceDimension)> InputVnlVectorType;
  typedef vnl_vector_fixed<TScalarType, itkGetStaticConstMacro(SpaceDimension)> OutputVnlVectorType;
  
  /** Standard coordinate point type for this class. */
  typedef Point<TScalarType, itkGetStaticConstMacro(SpaceDimension)> InputPointType;
  typedef Point<TScalarType, itkGetStaticConstMacro(SpaceDimension)> OutputPointType;
  
  /** This method returns the value of the offset of the
   * TranslationTransform. */
  const OutputVectorType & GetOffset(void) const
    { return m_Offset; }

  /** This method sets the parameters for the transform
   * value specified by the user. */
  void SetParameters(const ParametersType & parameters);

  /** Get the Transformation Parameters. */
  virtual const ParametersType& GetParameters(void) const;

  /** Set offset of an Translation Transform.
   * This method sets the offset of an TranslationTransform to a
   * value specified by the user. */
  void SetOffset(const OutputVectorType &offset)
    { m_Offset = offset; return; }

  /** Compose with another TranslationTransform. */
  void Compose(const Self * other, bool pre=0);

  /** Compose affine transformation with a translation.
   * This method modifies self to include a translation of the
   * origin.  The translation is precomposed with self if pre is
   * true, and postcomposed otherwise. */
  void Translate(const OutputVectorType &offset, bool pre=0);

  /** Transform by an affine transformation.
   * This method applies the affine transform given by self to a
   * given point or vector, returning the transformed point or
   * vector. */
  OutputPointType     TransformPoint(const InputPointType  &point ) const;
  OutputVectorType    TransformVector(const InputVectorType &vector) const;
  OutputVnlVectorType TransformVector(const InputVnlVectorType &vector) const;
  OutputCovariantVectorType TransformCovariantVector(
    const InputCovariantVectorType &vector) const;
  
  /** This method finds the point or vector that maps to a given
   * point or vector under the affine transformation defined by
   * self.  If no such point exists, an exception is thrown. */
  inline InputPointType    BackTransform(const OutputPointType  &point ) const;
  inline InputVectorType   BackTransform(const OutputVectorType &vector) const;
  inline InputVnlVectorType BackTransform(const OutputVnlVectorType &vector) const;
  inline InputCovariantVectorType BackTransform(
    const OutputCovariantVectorType &vector) const;
  
  /** Find inverse of an affine transformation.
   * This method creates and returns a new TranslationTransform object
   * which is the inverse of self.  If self is not invertible,
   * an exception is thrown.  */
  TranslationTransform::Pointer Inverse(void) const;

  /** Compute the Jacobian Matrix of the transformation at one point */
  virtual const JacobianType & GetJacobian(const InputPointType  &point ) const;

  /** Set the parameters to the IdentityTransform */
  void SetIdentity(void);

  /** Return the number of parameters that completely define the Transfom  */
  virtual unsigned int GetNumberOfParameters(void) const 
                      { return NDimensions; }


protected:
  TranslationTransform();
  ~TranslationTransform();
  /** Print contents of an TranslationTransform. */
  void PrintSelf(std::ostream &os, Indent indent) const;

private:
  TranslationTransform(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  OutputVectorType   m_Offset;       // Offset of the transformation


}; //class TranslationTransform


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTranslationTransform.txx"
#endif

#endif /* __itkTranslationTransform_h */
