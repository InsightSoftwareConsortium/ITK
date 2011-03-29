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
#ifndef __itkGaussianSmoothingOnUpdateDisplacementFieldTransform_h
#define __itkGaussianSmoothingOnUpdateDisplacementFieldTransform_h

#include "itkDisplacementFieldTransform.h"
#include "itkGaussianOperator.h"
#include "itkVectorNeighborhoodOperatorImageFilter.h"

namespace itk
{

/** \class GaussianSmoothingOnUpdateDisplacementFieldTransform
 * \brief Modifies the UpdateTransformParameters method
 * to peform a Gaussian smoothing of the
 * displacement field after adding the update array.
 *
 * This class is the same as \c DisplacementFieldTransform, except
 * for the changes to UpdateTransformParameters. The method smooths
 * the result of the addition of the update array and the displacement
 * field, using a \c GaussianOperator filter.
 *
 * To free the memory allocated and cached in \c GaussianSmoothDisplacementField
 * on demand, see \c FreeGaussianSmoothingTempField.
 *
 * \ingroup Transforms
 *
 * \ingroup ITKTransform
 */
template
  <class TScalar, unsigned int NDimensions>
class ITK_EXPORT GaussianSmoothingOnUpdateDisplacementFieldTransform :
  public DisplacementFieldTransform<TScalar, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef GaussianSmoothingOnUpdateDisplacementFieldTransform                        Self;
  typedef DisplacementFieldTransform<TScalar, NDimensions>  Superclass;
  typedef SmartPointer<Self>                                Pointer;
  typedef SmartPointer<const Self>                          ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( GaussianSmoothingOnUpdateDisplacementFieldTransform,
                                                DisplacementFieldTransform );

  /** New macro for creation of through a Smart Pointer */
  itkSimpleNewMacro( Self );

  /** Types from superclass */
  typedef typename Superclass::ScalarType               ScalarType;
  typedef typename Superclass::DerivativeType           DerivativeType;
  typedef typename Superclass::DisplacementFieldType    DisplacementFieldType;

  /** Get/Set the GaussianOperator variance */
  itkSetMacro( GaussianSmoothingSigma, ScalarType );
  itkGetConstReferenceMacro( GaussianSmoothingSigma, ScalarType );

  /** Update the transform's parameters by the values in \c update.
   * We assume \c update is of the same length as Parameters. Throw
   * exception otherwise.
   * \c factor is a scalar multiplier for each value in update.
   * \c GaussianSmoothDisplacementField is called after the update is
   * added to the field.
   * See base class for more details.
   */
  virtual void UpdateTransformParameters( DerivativeType & update,
                                          ScalarType factor = 1.0 );

  /** Smooth the displacement field in-place.
   * Uses m_GaussSmoothSigma to change the variance for the GaussianOperator,
   * the default is 3. Other parameters MaximumError and MaximumKernelWidth
   * are hardcoded.
   * \warning Not thread safe. Does its own threading. */
  virtual void GaussianSmoothDisplacementField();

  /** Free the \c m_GaussianSmoothingTempField object to conserve memory.
   * This object is allocated during the first call to
   * GaussianSmoothDisplacementField and cached to avoid re-allocation
   * during multiple calls to GaussianSmoothDisplacementField during
   * the lifetime of this object. Otherwise it will be deleted when
   * this object is destroyed. */
  virtual void FreeGaussianSmoothingTempField()
  {
    if( !m_GaussianSmoothingTempField.IsNull() )
      {
      m_GaussianSmoothingTempField = NULL;
      }
  }

protected:
  GaussianSmoothingOnUpdateDisplacementFieldTransform();
  virtual ~GaussianSmoothingOnUpdateDisplacementFieldTransform();
  void PrintSelf( std::ostream& os, Indent indent ) const;

  /** Used in GaussianSmoothDisplacementField as variance for the
   * GaussianOperator */
  ScalarType                                  m_GaussianSmoothingSigma;

private:
  GaussianSmoothingOnUpdateDisplacementFieldTransform( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

  /** Used to hold temporary displacement field during smoothing.
   * Use member variable to avoid allocation on stack and allow
   * cache'ing betwen calls. */
  typename DisplacementFieldType::Pointer    m_GaussianSmoothingTempField;

  /** Track when the temporary displacement field used during smoothing
   * was last modified/initialized. We only want to change it if the
   * main displacement field is also changed, i.e. assigned to a new object */
  unsigned long                       m_GaussianSmoothingTempFieldModifiedTime;

  /** Type of Gaussian Operator used during smoothing. Define here
   * so we can use a member var during the operation. */
  typedef GaussianOperator<ScalarType, Superclass::Dimension>
                                                  GaussianSmoothingOperatorType;
  typedef VectorNeighborhoodOperatorImageFilter< DisplacementFieldType,
                                                 DisplacementFieldType >
                                                  GaussianSmoothingSmootherType;
  GaussianSmoothingOperatorType                    m_GaussianSmoothingOperator;
  typename GaussianSmoothingSmootherType::Pointer  m_GaussianSmoothingSmoother;
};

} // end namespace itk

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkGaussianSmoothingOnUpdateDisplacementFieldTransform+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.hxx"
#endif

#endif // __itkGaussianSmoothingOnUpdateDisplacementFieldTransform_h
