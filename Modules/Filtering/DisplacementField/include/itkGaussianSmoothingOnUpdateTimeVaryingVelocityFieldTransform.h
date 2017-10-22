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
#ifndef itkGaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform_h
#define itkGaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform_h

#include "itkTimeVaryingVelocityFieldTransform.h"

namespace itk
{

/** \class GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform
 * \brief Modifies the UpdateTransformParameters method
 * to peform a Gaussian smoothing of the
 * velocity field after adding the update array.
 *
 * This class is the same as \c TimeVaryingVelocityFieldTransform, except
 * for the changes to UpdateTransformParameters. The method smooths
 * the result of the addition of the update array and the displacement
 * field, using a \c GaussianOperator filter.
 *
 * \ingroup ITKDisplacementField
 */
template<typename TParametersValueType, unsigned int NDimensions>
class ITK_TEMPLATE_EXPORT GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform
: public TimeVaryingVelocityFieldTransform<TParametersValueType, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform           Self;
  typedef TimeVaryingVelocityFieldTransform<TParametersValueType, NDimensions> Superclass;
  typedef SmartPointer<Self>                                                   Pointer;
  typedef SmartPointer<const Self>                                             ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform,
                                                TimeVaryingVelocityFieldTransform );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** Dimension of the time varying velocity field. */
  itkStaticConstMacro( TimeVaryingVelocityFieldDimension, unsigned int, NDimensions+1 );

  /** Types from superclass */
  typedef typename Superclass::ScalarType                         ScalarType;
  typedef typename Superclass::DerivativeType                     DerivativeType;
  typedef typename DerivativeType::ValueType                      DerivativeValueType;
  typedef typename Superclass::VelocityFieldType                  VelocityFieldType;

  typedef typename Superclass::TimeVaryingVelocityFieldType       TimeVaryingVelocityFieldType;
  typedef typename Superclass::TimeVaryingVelocityFieldPointer    TimeVaryingVelocityFieldPointer;

  typedef typename VelocityFieldType::PixelType                   DisplacementVectorType;
  typedef typename DisplacementVectorType::ValueType              DisplacementVectorValueType;


  /**
   * Get/Set the Gaussian spatial smoothing variance for the update field.
   * Default = 3.
   */
  itkSetMacro( GaussianSpatialSmoothingVarianceForTheUpdateField, ScalarType );
  itkGetConstReferenceMacro( GaussianSpatialSmoothingVarianceForTheUpdateField, ScalarType );

  /**
   * Get/Set the Gaussian temporal smoothing variance for the update field.
   * Default = 1.0.
   */
  itkSetMacro( GaussianTemporalSmoothingVarianceForTheUpdateField, ScalarType );
  itkGetConstReferenceMacro( GaussianTemporalSmoothingVarianceForTheUpdateField, ScalarType );

  /**
   * Get/Set the Gaussian spatial smoothing variance for the total field.
   * Default = 0.5.
   */
  itkSetMacro( GaussianSpatialSmoothingVarianceForTheTotalField, ScalarType );
  itkGetConstReferenceMacro( GaussianSpatialSmoothingVarianceForTheTotalField, ScalarType );

  /**
   * Get/Set the Gaussian temporal smoothing variance for the total field.
   * Default = 0.
   */
  itkSetMacro( GaussianTemporalSmoothingVarianceForTheTotalField, ScalarType );
  itkGetConstReferenceMacro( GaussianTemporalSmoothingVarianceForTheTotalField, ScalarType );

  /** Update the transform's parameters by the values in \c update.
   * We assume \c update is of the same length as Parameters. Throw
   * exception otherwise.
   * \c factor is a scalar multiplier for each value in update.
   * \c GaussianSmoothTimeVaryingVelocityField is called after the update is
   * added to the field.
   * See base class for more details.
   */
  virtual void UpdateTransformParameters( const DerivativeType & update, ScalarType factor = 1.0 ) ITK_OVERRIDE;

  /** Smooth the displacement field in-place.
   * Uses m_GaussSmoothSigma to change the variance for the GaussianOperator.
   * \warning Not thread safe. Does its own threading.
   */
  virtual TimeVaryingVelocityFieldPointer GaussianSmoothTimeVaryingVelocityField( VelocityFieldType *, ScalarType, ScalarType );

protected:
  GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform();
  virtual ~GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform() ITK_OVERRIDE;
  virtual void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE;

  /** Track when the temporary displacement field used during smoothing
   * was last modified/initialized. We only want to change it if the
   * main displacement field is also changed, i.e. assigned to a new object */
  ModifiedTimeType                  m_GaussianSmoothingTempFieldModifiedTime;

  /** Used in GaussianSmoothTimeVaryingVelocityField as variance for the
   * GaussianOperator
   */
  ScalarType                        m_GaussianSpatialSmoothingVarianceForTheUpdateField;
  ScalarType                        m_GaussianSpatialSmoothingVarianceForTheTotalField;
  ScalarType                        m_GaussianTemporalSmoothingVarianceForTheUpdateField;
  ScalarType                        m_GaussianTemporalSmoothingVarianceForTheTotalField;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform);

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
# include "itkGaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform.hxx"
#endif

#endif // itkGaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform_h
