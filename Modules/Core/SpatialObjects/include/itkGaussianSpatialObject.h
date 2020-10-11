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
#ifndef itkGaussianSpatialObject_h
#define itkGaussianSpatialObject_h

#include "itkEllipseSpatialObject.h"

namespace itk
{
/**
 *\class GaussianSpatialObject
 *
 * \brief Represents a multivariate Gaussian function.
 *
 * The Gaussian function G(x) is given by
 * \f[
 * G(\vec{x}) = m e^{-\|\S^{-1} \vec{x}\|^2 / 2},
 * \f]
 * where m is a scaling factor set by SetMaximum(), and \f$\S\f$ is the
 * (invertible) matrix associated to the IndexToObjectTransform of the object
 * multiplied by the Sigma parameter.  If \f$\S\f$ is symmetric and positive
 * definite, and m is chosen so that the integral of G(x) is 1, then G will
 * denote a normal distribution with mean 0 and covariance matrix \f$\S \times
 * Sigma\f$.
 * \ingroup ITKSpatialObjects
 */

template <unsigned int TDimension = 3>
class ITK_TEMPLATE_EXPORT GaussianSpatialObject : public SpatialObject<TDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GaussianSpatialObject);

  using Self = GaussianSpatialObject;
  using ScalarType = double;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = SpatialObject<TDimension>;
  using SuperclassPointer = SmartPointer<Superclass>;
  using PointType = typename Superclass::PointType;
  using TransformType = typename Superclass::TransformType;
  using BoundingBoxType = typename Superclass::BoundingBoxType;

  static constexpr unsigned int ObjectDimensions = TDimension;

  itkNewMacro(Self);
  itkTypeMacro(GaussianSpatialObject, SpatialObject);

  /** Reset the spatial object to its initial condition, yet preserves
   *   Id, Parent, and Child information */
  void
  Clear() override;

  /** The Radius determines the bounding box, and which points are
   * considered to be inside the SpatialObject.  All points with
   * z-score less than the radius are in the object.  */
  itkSetMacro(RadiusInObjectSpace, ScalarType);
  itkGetConstReferenceMacro(RadiusInObjectSpace, ScalarType);

  /** The Sigma parameter determines the fallout of the Gaussian inside of the
   * region defined by the Radius parameter. */
  itkSetMacro(SigmaInObjectSpace, ScalarType);
  itkGetConstReferenceMacro(SigmaInObjectSpace, ScalarType);

  itkSetMacro(CenterInObjectSpace, PointType);
  itkGetConstReferenceMacro(CenterInObjectSpace, PointType);

  /** The maximum value of the Gaussian (its value at the origin of
   * the spatial object coordinate system). */
  itkSetMacro(Maximum, ScalarType);
  itkGetConstReferenceMacro(Maximum, ScalarType);

  ScalarType
  SquaredZScoreInObjectSpace(const PointType & point) const;

  ScalarType
  SquaredZScoreInWorldSpace(const PointType & point) const;

  /** Test whether a point is inside or outside the object */
  bool
  IsInsideInObjectSpace(const PointType & point) const override;

  /* Avoid hiding the overload that supports depth and name arguments */
  using Superclass::IsInsideInObjectSpace;

  /** Returns the value of the Gaussian at the given point.  */
  bool
  ValueAtInObjectSpace(const PointType &   point,
                       double &            value,
                       unsigned int        depth = 0,
                       const std::string & name = "") const override;

  /** Returns the sigma=m_Radius level set of the Gaussian function, as an
   * EllipseSpatialObject.  */
  typename EllipseSpatialObject<TDimension>::Pointer
  GetEllipsoid() const;

#if !defined(ITK_LEGACY_REMOVE)
  itkLegacyMacro(void SetSigma(double sigma)) { return this->SetSigmaInObjectSpace(sigma); }

  itkLegacyMacro(double GetSigma() const) { return this->GetSigmaInObjectSpace(); }
#endif
protected:
  /** This function needs to be called every time one of the object's
   *  components is changed. */
  void
  ComputeMyBoundingBox() override;

  GaussianSpatialObject();
  ~GaussianSpatialObject() override = default;

  /** Print the object information in a stream. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  typename LightObject::Pointer
  InternalClone() const override;

private:
  ScalarType m_Maximum;
  ScalarType m_RadiusInObjectSpace;
  ScalarType m_SigmaInObjectSpace;
  PointType  m_CenterInObjectSpace;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGaussianSpatialObject.hxx"
#endif

#endif // itkGaussianSpatialObject_h
