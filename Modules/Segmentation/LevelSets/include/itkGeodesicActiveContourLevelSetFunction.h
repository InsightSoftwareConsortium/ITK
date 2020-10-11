/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkGeodesicActiveContourLevelSetFunction_h
#define itkGeodesicActiveContourLevelSetFunction_h

#include "itkSegmentationLevelSetFunction.h"

namespace itk
{
/** \class GeodesicActiveContourLevelSetFunction
 *
 * \brief This function is used in GeodesicActiveContourLevelSetImageFilter to
 * segment structures in an image based on a user supplied edge potential map.
 *
 * \par IMPORTANT
 * The LevelSetFunction class contain additional information necessary
 * to gain full understanding of how to use this function.
 *
 * GeodesicActiveContourLevelSetFunction is a subclass of the generic LevelSetFunction.
 * It is used to segment structures in an image based on a user supplied
 * edge potential map \f$ g(I) \f$, which
 * has values close to zero in regions near edges (or high image gradient) and values
 * close to one in regions with relatively constant intensity. Typically, the edge
 * potential map is a function of the gradient, for example:
 *
 * \f[ g(I) = 1 / ( 1 + | (\nabla * G)(I)| ) \f]
 * \f[ g(I) = \exp^{-|(\nabla * G)(I)|} \f]
 *
 * where \f$ I \f$ is image intensity and
 * \f$ (\nabla * G) \f$ is the derivative of Gaussian operator.
 *
 * The edge potential image is set via the SetFeatureImage() method.
 *
 * In this function both the propagation term \f$ P(\mathbf{x}) \f$
 * and the curvature spatial modifier term \f$ Z(\mathbf{x}) \f$ are taken directly
 * from the edge potential image such that:
 *
 * \f[ P(\mathbf{x}) = g(\mathbf{x}) \f]
 * \f[ Z(\mathbf{x}) = g(\mathbf{x}) \f]
 *
 * An advection term \f$ \mathbf{A}(\mathbf{x}) \f$ is constructed
 * from the negative gradient of the edge potential image.
 *
 * \f[ \mathbf{A}(\mathbf{x}) = -\nabla g(\mathbf{x}) \f]
 *
 * This term behaves like a doublet attracting the contour to the edges.
 *
 * This implementation is based on:
 * "Geodesic Active Contours",
 * V. Caselles, R. Kimmel and G. Sapiro.
 * International Journal on Computer Vision,
 * Vol 22, No. 1, pp 61-97, 1997
 *
 * \sa LevelSetFunction
 * \sa SegmentationLevelSetImageFunction
 * \sa GeodesicActiveContourLevelSetImageFilter
 *
 * \ingroup FiniteDifferenceFunctions
 * \ingroup ITKLevelSets
 */
template <typename TImageType, typename TFeatureImageType = TImageType>
class ITK_TEMPLATE_EXPORT GeodesicActiveContourLevelSetFunction
  : public SegmentationLevelSetFunction<TImageType, TFeatureImageType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GeodesicActiveContourLevelSetFunction);

  /** Standard class type aliases. */
  using Self = GeodesicActiveContourLevelSetFunction;
  using Superclass = SegmentationLevelSetFunction<TImageType, TFeatureImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using FeatureImageType = TFeatureImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GeodesicActiveContourLevelSetFunction, SegmentationLevelSetFunction);

  /** Extract some parameters from the superclass. */
  using ImageType = typename Superclass::ImageType;
  using NeighborhoodType = typename Superclass::NeighborhoodType;
  using ScalarValueType = typename Superclass::ScalarValueType;
  using FeatureScalarType = typename Superclass::FeatureScalarType;
  using RadiusType = typename Superclass::RadiusType;
  using FloatOffsetType = typename Superclass::FloatOffsetType;
  using VectorImageType = typename Superclass::VectorImageType;
  using GlobalDataStruct = typename Superclass::GlobalDataStruct;

  /** Extract some parameters from the superclass. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Compute speed image from feature image. */
  void
  CalculateSpeedImage() override;

  /** Compute the advection field from feature image. */
  void
  CalculateAdvectionImage() override;

  /** The curvature speed is same as the propagation speed. */
  ScalarValueType
  CurvatureSpeed(const NeighborhoodType & neighborhood,
                 const FloatOffsetType &  offset,
                 GlobalDataStruct *       gd) const override
  {
    return this->PropagationSpeed(neighborhood, offset, gd);
  }

  /** Set/Get the sigma for the Gaussian kernel used to compute the gradient
   * of the feature image needed for the advection term of the equation. */
  void
  SetDerivativeSigma(const double v)
  {
    m_DerivativeSigma = v;
  }
  double
  GetDerivativeSigma()
  {
    return m_DerivativeSigma;
  }

  void
  Initialize(const RadiusType & r) override
  {
    Superclass::Initialize(r);

    this->SetAdvectionWeight(NumericTraits<ScalarValueType>::OneValue());
    this->SetPropagationWeight(NumericTraits<ScalarValueType>::OneValue());
    this->SetCurvatureWeight(NumericTraits<ScalarValueType>::OneValue());
  }

protected:
  GeodesicActiveContourLevelSetFunction()
  {
    this->SetAdvectionWeight(NumericTraits<ScalarValueType>::OneValue());
    this->SetPropagationWeight(NumericTraits<ScalarValueType>::OneValue());
    this->SetCurvatureWeight(NumericTraits<ScalarValueType>::OneValue());

    m_DerivativeSigma = 1.0;
  }

  ~GeodesicActiveContourLevelSetFunction() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  double m_DerivativeSigma;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGeodesicActiveContourLevelSetFunction.hxx"
#endif

#endif
