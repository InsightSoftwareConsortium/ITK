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
#ifndef itkGaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor_h
#define itkGaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor_h

#include "itkDisplacementFieldTransformParametersAdaptor.h"

namespace itk
{
/** \class GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor
 * \brief Helper class for multiresolution image registration
 *
 * \author Nick Tustison
 *
 * \ingroup ITKRegistrationCommon
 */
template<typename TTransform>
class ITK_TEMPLATE_EXPORT GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor
: public DisplacementFieldTransformParametersAdaptor<TTransform>
{
public:

  /** Standard class typedefs. */
  typedef GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor          Self;
  typedef DisplacementFieldTransformParametersAdaptor<TTransform>                       Superclass;
  typedef SmartPointer<Self>                                                            Pointer;
  typedef SmartPointer<const Self>                                                      ConstPointer;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor,
    DisplacementFieldTransformParametersAdaptor );

  typedef TTransform                               TransformType;
  typedef typename TransformType::ScalarType       ScalarType;

  /**
   * Get/Set the Gaussian smoothing standard deviation for the update field.
   */
  virtual void SetGaussianSmoothingVarianceForTheUpdateField( ScalarType );
  itkGetConstReferenceMacro( GaussianSmoothingVarianceForTheUpdateField, ScalarType );

  /**
   * Get/Set the Gaussian smoothing standard deviation for the total field.
   */
  virtual void SetGaussianSmoothingVarianceForTheTotalField( const ScalarType );
  itkGetConstReferenceMacro( GaussianSmoothingVarianceForTheTotalField, ScalarType );

  virtual void AdaptTransformParameters() ITK_OVERRIDE;

protected:
  GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor();
  ~GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor() ITK_OVERRIDE;

  void PrintSelf( std::ostream & os, Indent indent ) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor);

  ScalarType          m_GaussianSmoothingVarianceForTheUpdateField;
  ScalarType          m_GaussianSmoothingVarianceForTheTotalField;

  ModifiedTimeType    m_GaussianSmoothingVarianceForTheUpdateFieldSetTime;
  ModifiedTimeType    m_GaussianSmoothingVarianceForTheTotalFieldSetTime;

}; //class GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor.hxx"
#endif

#endif /* itkGaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor_h */
