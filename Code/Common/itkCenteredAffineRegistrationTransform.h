/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCenteredAffineRegistrationTransform.h
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
#ifndef __itkCenteredAffineRegistrationTransform_h
#define __itkCenteredAffineRegistrationTransform_h

#include "itkObject.h"
#include "itkTransformation.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkAffineTransform.h"

namespace itk
{

/** \class CenteredAffineRegistrationTransform
 * \brief Affine Transformation used for registration.
 *
 * CenteredAffineRegistrationTransform maps a point
 * from one space to another for a given set of affine
 * parameters.
 *
 * The affine parameters is specifed as an itkPoint of size
 * ImageDimension * (ImageDimension + 1 ). This Point is formed
 * by concatenating each row of the matrix (linear) component with
 * the offset (translation) component.
 *
 * In addition the user can specify the transformation center
 * for both the domain and range spaces. For example, setting the
 * centers to the center of mass or image stack center can
 * greatly improve the registration optimization process.
 *
 * \sa AffineRegistrationTransform
 *
 */
template <class TScalarType,unsigned int NDimensions, class TParameters>
class ITK_EXPORT  CenteredAffineRegistrationTransform : public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef CenteredAffineRegistrationTransform  Self;

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
  typedef Object Superclass;

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
  typedef typename AffineTransformType::PointType     PointType;

  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(CenteredAffineRegistrationTransform, Object);

  /**
   * Type of the Jacobian matrix
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
  PointType Transform( const PointType & point ) const;

  /**
   * Set the domain transformation center
   */
  void SetDomainTransformationCenter( const PointType & center )
    {  m_DomainTransformationCenter = center; }

  /**
   * Get the domain transformation center
   */
  const PointType& GetDomainTransformationCenter( void )
    { return m_DomainTransformationCenter; }

  /**
   * Set the range transformation center
   */
  void SetRangeTransformationCenter( const PointType & center )
    {  m_RangeTransformationCenter = center; }

  /**
   * Get the range transformation center
   */
  const PointType& GetRangeTransformationCenter( void )
    { return m_RangeTransformationCenter; }

  /**
   * Set the Transformation Parameters
   * and update the internal transformation
   */
  void SetParameters(const ParametersType &);

  /**
   * Compute the Jacobian of the transformation
   *
   * This method computes the Jacobian matrix of the transformation
   * at a given point.
   **/
  const JacobianType & GetJacobian(const PointType  &point ) const;

protected:

  CenteredAffineRegistrationTransform();
  virtual ~CenteredAffineRegistrationTransform() {};
  CenteredAffineRegistrationTransform(const Self&);
  const Self & operator=(const Self&);

private:

  AffineTransformType                 m_AffineTransform;
  ParametersType                      m_Parameters;

  mutable JacobianType                m_Jacobian;

  PointType                           m_DomainTransformationCenter;
  PointType                           m_RangeTransformationCenter;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCenteredAffineRegistrationTransform.txx"
#endif

#endif



