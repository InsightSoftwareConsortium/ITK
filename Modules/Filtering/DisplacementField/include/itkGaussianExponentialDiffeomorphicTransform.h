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
#ifndef __itkGaussianExponentialDiffeomorphicTransform_h
#define __itkGaussianExponentialDiffeomorphicTransform_h

#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.h"

namespace itk
{

/** \class GaussianExponentialDiffeomorphicTransform
 * \brief Modifies the UpdateTransformParameters method
 * to peform a Gaussian smoothing of the constant
 * velocity field after adding the update array.
 *
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup ITKDisplacementField
 */
template
  <class TScalar, unsigned int NDimensions>
class ITK_EXPORT GaussianExponentialDiffeomorphicTransform :
  public GaussianSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef GaussianExponentialDiffeomorphicTransform                                                           Self;
  typedef GaussianSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>      Superclass;
  typedef typename Superclass::Superclass                                                SuperSuperclass;
  typedef SmartPointer<Self>                                                             Pointer;
  typedef SmartPointer<const Self>                                                       ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( GaussianExponentialDiffeomorphicTransform, GaussianSmoothingOnUpdateDisplacementFieldTransform );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** Types from superclass */
  typedef typename Superclass::ScalarType               ScalarType;
  typedef typename Superclass::DerivativeType           DerivativeType;
  typedef typename DerivativeType::ValueType            DerivativeValueType;
  typedef typename Superclass::DisplacementFieldType    ConstantVelocityFieldType;
  typedef typename Superclass::DisplacementFieldPointer ConstantVelocityFieldPointer;
  typedef typename Superclass::DisplacementFieldType    DisplacementFieldType;
  typedef typename Superclass::DisplacementFieldPointer DisplacementFieldPointer;
  typedef typename Superclass::DisplacementVectorType   DisplacementVectorType;

  typedef typename Transform<TScalar,NDimensions,NDimensions>::Pointer TransformPointer;

  /** Update the transform's parameters by the values in \c update.
   * We assume \c update is of the same length as Parameters. Throw
   * exception otherwise.
   * \c factor is a scalar multiplier for each value in update.
   * \c GaussianSmoothConstantVelocityField is called after the update is
   * added to the field.
   * See base class for more details.
   */
  virtual void UpdateTransformParameters( const DerivativeType & update, ScalarType factor = 1.0 );

  // Set/get compute inverse
  itkSetMacro( ComputeInverse, bool );
  itkGetConstMacro( ComputeInverse, bool );
  itkBooleanMacro( ComputeInverse );

  // Set/get compute number of exp. iterations automatically
  itkSetMacro( CalculateNumberOfIterationsAutomatically, bool );
  itkGetConstMacro( CalculateNumberOfIterationsAutomatically, bool );
  itkBooleanMacro( CalculateNumberOfIterationsAutomatically );

  // Set/get the number of iterations (valid only if CalculateNumberOfIterationsAutomatically = false)
  itkSetMacro( MaximumNumberOfIterations, unsigned int );
  itkGetConstMacro( MaximumNumberOfIterations, unsigned int );

protected:
  GaussianExponentialDiffeomorphicTransform();
  virtual ~GaussianExponentialDiffeomorphicTransform();

  void PrintSelf( std::ostream &, Indent ) const;

private:
  GaussianExponentialDiffeomorphicTransform( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

  bool                                    m_CalculateNumberOfIterationsAutomatically;
  unsigned int                            m_MaximumNumberOfIterations;

  bool                                    m_ComputeInverse;
};

} // end namespace itk

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkGaussianExponentialDiffeomorphicTransform+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkGaussianExponentialDiffeomorphicTransform.hxx"
#endif

#endif // __itkGaussianExponentialDiffeomorphicTransform_h
