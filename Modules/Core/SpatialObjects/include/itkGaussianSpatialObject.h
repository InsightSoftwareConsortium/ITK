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
#ifndef itkGaussianSpatialObject_h
#define itkGaussianSpatialObject_h

#include "itkEllipseSpatialObject.h"

namespace itk
{
/** \class GaussianSpatialObject
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

template< unsigned int TDimension = 3 >
class ITK_TEMPLATE_EXPORT GaussianSpatialObject:
  public SpatialObject< TDimension >
{
public:

  typedef GaussianSpatialObject                Self;
  typedef double                               ScalarType;
  typedef SmartPointer< Self >                 Pointer;
  typedef SmartPointer< const Self >           ConstPointer;
  typedef SpatialObject< TDimension >          Superclass;
  typedef SmartPointer< Superclass >           SuperclassPointer;
  typedef typename Superclass::PointType       PointType;
  typedef typename Superclass::TransformType   TransformType;
  typedef typename Superclass::BoundingBoxType BoundingBoxType;

  itkStaticConstMacro(NumberOfDimensions, unsigned int,
                      TDimension);

  itkNewMacro(Self);
  itkTypeMacro(GaussianSpatialObject, SpatialObject);

  /** The Radius determines the bounding box, and which points are
   * considered to be inside the SpatialObject.  All points with
   * z-score less than the radius are in the object.  */
  itkSetMacro(Radius, ScalarType);
  itkGetConstReferenceMacro(Radius, ScalarType);

  /** The Sigma parameter determines the fallout of the Gaussian inside of the
   * region defined by the Radius parameter. */
  itkSetMacro(Sigma, ScalarType);
  itkGetConstReferenceMacro(Sigma, ScalarType);

  /** The maximum value of the Gaussian (its value at the origin of
   * the spatial object coordinate system). */
  itkSetMacro(Maximum, ScalarType);
  itkGetConstReferenceMacro(Maximum, ScalarType);

  /** If the matrix S is returned by
   * this->GetIndexToObjectTransform()->GetMatrix(), then SquaredZScore(x)
   * returns |Sx| squared.  */
  ScalarType SquaredZScore(const PointType & point) const;

  /** Returns the value of the Gaussian at the given point.  */
  virtual bool ValueAt(const PointType & point, ScalarType & value,
                       unsigned int depth = 0,
                       char *name = ITK_NULLPTR) const ITK_OVERRIDE;

  /** Return true if the object provides a method to evaluate the value
   * at the specified point, false otherwise. */
  virtual bool IsEvaluableAt(const PointType & point,
                             unsigned int depth = 0,
                             char *name = ITK_NULLPTR) const ITK_OVERRIDE;

  /** Test whether a point is inside or outside the object */
  virtual bool IsInside(const PointType & point,
                        unsigned int depth,
                        char *name) const ITK_OVERRIDE;

  /** Test whether a point is inside or outside the object
   *  For computational speed purposes, it is faster if the method does not
   *  check the name of the class and the current depth */
  virtual bool IsInside(const PointType & point) const;

  /** This function needs to be called every time one of the object's
   *  components is changed. */
  virtual bool ComputeLocalBoundingBox() const ITK_OVERRIDE;

  /** Returns the sigma=m_Radius level set of the Gaussian function, as an
   * EllipseSpatialObject.  */
  typename EllipseSpatialObject< TDimension >::Pointer GetEllipsoid() const;

protected:
  ITK_DISALLOW_COPY_AND_ASSIGN(GaussianSpatialObject);

  GaussianSpatialObject();
  ~GaussianSpatialObject() ITK_OVERRIDE;

  ScalarType m_Maximum;
  ScalarType m_Radius;
  ScalarType m_Sigma;

  /** Print the object information in a stream. */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGaussianSpatialObject.hxx"
#endif

#endif // itkGaussianSpatialObject_h
