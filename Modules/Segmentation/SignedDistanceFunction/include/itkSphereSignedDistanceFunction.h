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
#ifndef itkSphereSignedDistanceFunction_h
#define itkSphereSignedDistanceFunction_h

#include "itkShapeSignedDistanceFunction.h"
#include "itkVector.h"

namespace itk
{
/** \class SphereSignedDistanceFunction
 * \brief Compute the signed distance from a N-dimensional sphere.
 *
 * A instance of sphere is defined by a set parameters. The first parameter
 * is the radius and the next SpaceDimension parameters represent the center.
 * The first parameter forms the set of ShapeParameters and the remaining
 * parameters the set of PoseParameters.
 *
 * This class is templated over the coordinate representation type
 * (e.g. float or double) and the space dimension.
 *
 * \sa ShapeSignedDistanceFunction
 * \ingroup ImageFunctions
 *
 *
 * \ingroup ITKSignedDistanceFunction
 */
template< typename TCoordRep, unsigned int VSpaceDimension >
class ITK_TEMPLATE_EXPORT SphereSignedDistanceFunction:
  public ShapeSignedDistanceFunction< TCoordRep, VSpaceDimension >
{
public:
  /** Standard class typedefs. */
  typedef SphereSignedDistanceFunction                              Self;
  typedef ShapeSignedDistanceFunction< TCoordRep, VSpaceDimension > Superclass;
  typedef SmartPointer< Self >                                      Pointer;
  typedef SmartPointer< const Self >                                ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SphereSignedDistanceFunction, ShapeSignedDistancFunction);

  /** New macro for creation of through the object factory. */
  itkNewMacro(Self);

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** InputeType typedef support. */
  typedef typename Superclass::InputType InputType;

  /** Dimension underlying input image. */
  itkStaticConstMacro(SpaceDimension, unsigned int, Superclass::SpaceDimension);

  /** CoordRep typedef support. */
  typedef typename Superclass::CoordRepType CoordRepType;

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Type of the shape parameters. */
  typedef typename Superclass::ParametersType ParametersType;

  /** A sphere is defined by a set of shape parameters. The first parameter
   * is the radius and the next SpaceDimension parameters represent the center. */
  virtual void SetParameters(const ParametersType &) ITK_OVERRIDE;

  virtual unsigned int GetNumberOfShapeParameters(void) const ITK_OVERRIDE
  { return 1; }
  virtual unsigned int GetNumberOfPoseParameters(void) const ITK_OVERRIDE
  { return SpaceDimension; }

  /** Evaluate the signed distance from a shape at a given position. */
  virtual OutputType Evaluate(const PointType & point) const ITK_OVERRIDE;

protected:
  SphereSignedDistanceFunction();
  ~SphereSignedDistanceFunction() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SphereSignedDistanceFunction);

  typedef Vector< CoordRepType, itkGetStaticConstMacro(SpaceDimension) > VectorType;

  VectorType m_Translation;
  double     m_Radius;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSphereSignedDistanceFunction.hxx"
#endif

#endif
