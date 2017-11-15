/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkPolarToCartesianTransform_h
#define itkPolarToCartesianTransform_h

#include <iostream>
#include "itkTransform.h"
#include "itkExceptionObject.h"
#include "itkMatrix.h"

namespace itk
{

/** \brief Polar transformation of a vector space (e.g. space coordinates).
 *
 * Transforms first two coordinates form polar space <alpha,radius> to cartesian
 * coordinates. Other dimensions are left unchanges. In fact this is generalized
 * cylindric transform:
 * \f[			x_1 = r cos( \alpha ) \f]
 * \f[			x_2 = r sin( \alpha ) \f]
 * \f[			x_n = x_n, \mbox{ n>=2 } \f]
 *
 *
 * \par
 * Center of the polar transform is a center of coordinate system < 0, 0 >.
 *
 * Dimension must be at least 2 or an exception is thrown during transform.
 *
 * Extent of input in first dimension (alpha) should be only < 0, 2*pi ).
 *
 * \author Jakub Bican, Department of Image Processing, Institute of Information Theory and Automation, Academy of
 * Sciences of the Czech Republic.
 *
 * \ingroup Transforms
 * \ingroup PolarTransform
 */
template <typename TParametersValueType = double, // Data type for scalars (float or double)
          unsigned int NDimensions = 3>           // Number of dimensions
class ITK_TEMPLATE_EXPORT PolarToCartesianTransform : public Transform<TParametersValueType, NDimensions, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef PolarToCartesianTransform                                 Self;
  typedef Transform<TParametersValueType, NDimensions, NDimensions> Superclass;
  typedef SmartPointer<Self>                                        Pointer;
  typedef SmartPointer<const Self>                                  ConstPointer;

  /** New macro for creation of through the object factory.*/
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PolarToCartesianTransform, Transform);

  /** Dimension of the domain space. */
  itkStaticConstMacro(SpaceDimension, unsigned int, NDimensions);
  itkStaticConstMacro(ParametersDimension, unsigned int, 0);

  /** Standard scalar type for this class. */
  typedef typename Superclass::ScalarType ScalarType;

  /** Standard Jacobian container. */
  typedef typename Superclass::JacobianType JacobianType;

  /** Standard parameters container. */
  typedef typename Superclass::ParametersType ParametersType;

  /** Standard vector type for this class. */
  typedef Vector<TParametersValueType, itkGetStaticConstMacro(SpaceDimension)> InputVectorType;
  typedef Vector<TParametersValueType, itkGetStaticConstMacro(SpaceDimension)> OutputVectorType;

  /** Standard covariant vector type for this class. */
  typedef CovariantVector<TParametersValueType, itkGetStaticConstMacro(SpaceDimension)> InputCovariantVectorType;
  typedef CovariantVector<TParametersValueType, itkGetStaticConstMacro(SpaceDimension)> OutputCovariantVectorType;

  /** Standard vnl_vector type for this class. */
  typedef vnl_vector_fixed<TParametersValueType, itkGetStaticConstMacro(SpaceDimension)> InputVnlVectorType;
  typedef vnl_vector_fixed<TParametersValueType, itkGetStaticConstMacro(SpaceDimension)> OutputVnlVectorType;

  /** Standard coordinate point type for this class. */
  typedef Point<TParametersValueType, itkGetStaticConstMacro(SpaceDimension)> InputPointType;
  typedef Point<TParametersValueType, itkGetStaticConstMacro(SpaceDimension)> OutputPointType;

  /** Method to transform a point.
   * This method transforms first two dimensions of a point from polar
   * coordinates <alpha,radius> to cartesian coordinates.
   */
  OutputPointType
  TransformPoint(const InputPointType & point) const ITK_OVERRIDE;

  /** Method to transform a vector - not applicable for this type of
      transform. */
  virtual OutputVectorType
  TransformVector(const InputVectorType &) const ITK_OVERRIDE
  {
    itkExceptionMacro(<< "Method not implemented yet.");
    return OutputVectorType();
  }

  /** Method to transform a vnl_vector - not applicable for this type of
      transform. */
  virtual OutputVnlVectorType
  TransformVector(const InputVnlVectorType &) const ITK_OVERRIDE
  {
    itkExceptionMacro(<< "Method not implemented yet.");
    return OutputVnlVectorType();
  }

  /** Method to transform a CovariantVector - not applicable for this type of
      transform */
  virtual OutputCovariantVectorType
  TransformCovariantVector(const InputCovariantVectorType &) const ITK_OVERRIDE
  {
    itkExceptionMacro(<< "Method not implemented yet.");
    return OutputCovariantVectorType();
  }

  /** Compute the Jacobian Matrix of the transformation at one point - not
      applicable for this type of transform */
  virtual const JacobianType &
  GetJacobian(const InputPointType & point) const ITK_OVERRIDE
  {
    itkExceptionMacro(<< "Method not implemented yet.");
  }

  virtual void
  ComputeJacobianWithRespectToParameters(const InputPointType &, JacobianType &) const ITK_OVERRIDE
  {
    itkExceptionMacro(<< "Method not implemented yet.");
  }

  void
  SetParameters(const ParametersType & parameters) ITK_OVERRIDE
  {}

  void
  SetFixedParameters(const ParametersType &) ITK_OVERRIDE
  {}

protected:
  PolarToCartesianTransform();
  virtual ~PolarToCartesianTransform() ITK_OVERRIDE;

  /** Print contents of an PolarToCartesianTransform. */
  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PolarToCartesianTransform);

}; // class PolarToCartesianTransform


} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPolarToCartesianTransform.hxx"
#endif

#endif
