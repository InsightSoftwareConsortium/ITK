/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAffineRegistrationTransform.h
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
#ifndef __itkAffineRegistrationTransform_h
#define __itkAffineRegistrationTransform_h

#include "itkObject.h"
#include "itkTransform.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkAffineTransform.h"

namespace itk
{
  
/** \class AffineRegistrationTransform
 * \brief Generic Affine Transformation for a registration method
 *
 * This Class define the generic interface for an Affine Transformation 
 * \ingroup Transforms
 *
 */

template <class TScalarType,unsigned int NDimensions, class TParameters>
class ITK_EXPORT  AffineRegistrationTransform : public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef AffineRegistrationTransform  Self;


  /**
   * Integer constants
   */
  enum 
  { 
    SpaceDimension = NDimensions,
    ParametersDimension = NDimensions * (NDimensions + 1)
  };


  /**
   * Standard "Superclass" typedef.
   */
  typedef Transform<TScalarType,NDimensions> Superclass;


  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /** 
   * Type of the input parameters
   */
  typedef  TParameters     ParametersType;


  /** 
   * Affine Transform Type
   */
  typedef  AffineTransform<TScalarType,NDimensions>    AffineTransformType;


  /** 
   * Point Type
   */
  typedef  typename AffineTransformType::InputPointType     InputPointType;
  typedef  typename AffineTransformType::OutputPointType    OutputPointType;


  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(AffineRegistrationTransform, Transform);


  /** 
   * Run-time type information (and related methods).
   */
  typedef Matrix<TScalarType, SpaceDimension, 
                              ParametersDimension > JacobianType;


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Transform a Point using the Affine transformation
   */
  OutputPointType Transform( const InputPointType & point ) const;

  /**
   * Set the Transformation Parameters
   * and update the internal transformation
   */
  void SetParameters(const ParametersType &);


  /**
   *  Set the Scale for translations
   */
  itkSetMacro( TranslationScale , TScalarType );
   
  /**
   *  Get the Scale for translations
   */
  itkGetMacro( TranslationScale , TScalarType );
 
  /**
   * Compute the Jacobian of the transformation
   *
   * This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector.
   **/
  const JacobianType & GetJacobian(const InputPointType  &point ) const;

protected:

  AffineRegistrationTransform();
  virtual ~AffineRegistrationTransform() {};
  AffineRegistrationTransform(const Self&);
  const Self & operator=(const Self&);


private:

  /**
   *  Internal transformation
   */
  AffineTransformType                 m_AffineTransform;
  
  /**
   *  List of parameters that unambiguosly define the transformation
   */  
  ParametersType                      m_Parameters;

  /**
   *  Scale of the translations. It is used to bring translations
   *  and rotations to a similar scale. It should be set to the 
   *  value of the maximum expected translation.
   */  
  TScalarType                         m_TranslationScale;

  /**
   * Jacobian matrix of the transformation. It is used to compute
   * derivatives by using the chain rule.
   */
  mutable JacobianType                m_Jacobian;     

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAffineRegistrationTransform.txx"
#endif

#endif



