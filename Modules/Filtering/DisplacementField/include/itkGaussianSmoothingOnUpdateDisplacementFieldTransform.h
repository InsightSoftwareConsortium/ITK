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
#ifndef itkGaussianSmoothingOnUpdateDisplacementFieldTransform_h
#define itkGaussianSmoothingOnUpdateDisplacementFieldTransform_h

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
 *
 * \ingroup ITKDisplacementField
 */
template<typename TParametersValueType, unsigned int NDimensions>
class ITK_TEMPLATE_EXPORT GaussianSmoothingOnUpdateDisplacementFieldTransform :
  public DisplacementFieldTransform<TParametersValueType, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef GaussianSmoothingOnUpdateDisplacementFieldTransform           Self;
  typedef DisplacementFieldTransform<TParametersValueType, NDimensions> Superclass;
  typedef SmartPointer<Self>                                            Pointer;
  typedef SmartPointer<const Self>                                      ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( GaussianSmoothingOnUpdateDisplacementFieldTransform,
                                                DisplacementFieldTransform );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** Types from superclass */
  typedef typename Superclass::ScalarType               ScalarType;
  typedef typename Superclass::DerivativeType           DerivativeType;
  typedef typename DerivativeType::ValueType            DerivativeValueType;
  typedef typename Superclass::DisplacementFieldType    DisplacementFieldType;
  typedef typename Superclass::DisplacementFieldPointer DisplacementFieldPointer;
  typedef typename DisplacementFieldType::PixelType     DisplacementVectorType;

  typedef typename Transform<TParametersValueType,NDimensions, NDimensions>::Pointer TransformPointer;

  /**
   * Get/Set the Gaussian smoothing standard deviation for the update field.
   * Default = 1.75.
   */
  itkSetMacro( GaussianSmoothingVarianceForTheUpdateField, ScalarType );
  itkGetConstReferenceMacro( GaussianSmoothingVarianceForTheUpdateField, ScalarType );

  /**
   * Get/Set the Gaussian smoothing standard deviation for the total field.
   * Default = 0.5.
   */
  itkSetMacro( GaussianSmoothingVarianceForTheTotalField, ScalarType );
  itkGetConstReferenceMacro( GaussianSmoothingVarianceForTheTotalField, ScalarType );

  /** Update the transform's parameters by the values in \c update.
   * We assume \c update is of the same length as Parameters. Throw
   * exception otherwise.
   * \c factor is a scalar multiplier for each value in update.
   * \c GaussianSmoothDisplacementField is called after the update is
   * added to the field.
   * See base class for more details.
   */
  virtual void UpdateTransformParameters( const DerivativeType & update, ScalarType factor = 1.0 ) ITK_OVERRIDE;

  /** Smooth the displacement field in-place.
   * Uses m_GaussSmoothSigma to change the variance for the GaussianOperator.
   * \warning Not thread safe. Does its own threading.
   */
  virtual DisplacementFieldPointer GaussianSmoothDisplacementField( DisplacementFieldType *, ScalarType );

protected:
  GaussianSmoothingOnUpdateDisplacementFieldTransform();
  virtual ~GaussianSmoothingOnUpdateDisplacementFieldTransform() ITK_OVERRIDE;
  void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE;

  /** Clone the current transform */
  virtual typename LightObject::Pointer InternalClone() const ITK_OVERRIDE;

  /** Used in GaussianSmoothDisplacementField as variance for the
   * GaussianOperator */
  ScalarType                        m_GaussianSmoothingVarianceForTheUpdateField;
  ScalarType                        m_GaussianSmoothingVarianceForTheTotalField;

  /** Type of Gaussian Operator used during smoothing. Define here
   * so we can use a member var during the operation. */
  typedef GaussianOperator<ScalarType, Superclass::Dimension>
                                                  GaussianSmoothingOperatorType;
  typedef VectorNeighborhoodOperatorImageFilter< DisplacementFieldType,
                                                 DisplacementFieldType >
                                                  GaussianSmoothingSmootherType;
  GaussianSmoothingOperatorType                    m_GaussianSmoothingOperator;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GaussianSmoothingOnUpdateDisplacementFieldTransform);

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
# include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.hxx"
#endif

#endif // itkGaussianSmoothingOnUpdateDisplacementFieldTransform_h
