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
#ifndef itkShapeSignedDistanceFunction_h
#define itkShapeSignedDistanceFunction_h

#include "itkSpatialFunction.h"
#include "itkOptimizerParameters.h"

namespace itk
{
/**
 *\class ShapeSignedDistanceFunction
 * \brief Base class for functions which evaluates the signed distance
 * from a shape.
 *
 * ShapeSignedDistanceFunction is the base class for functions which
 * returns the signed distance from a shape at an arbitrary point.
 * A shape assumed to be defined by a set of shape and pose parameters.
 *
 * Note that Initialize() must be called before use.
 * This allows the class an opportunity to validate any inputs.
 *
 * This class is templated over the coordinate representation type
 * (e.g. float or double) and the space dimension.
 *
 * ShapeSignedDistanceFunction is used to encapsulate the shape prior
 * in ShapePriorSegmentationLevelSetFunctions.
 *
 * \sa SpatialFunction
 * \sa ShapePriorSegmentationLevelSetFunction
 *
 * \ingroup ImageFunctions
 *
 *
 * \ingroup ITKSignedDistanceFunction
 */
template <typename TCoordRep, unsigned int VSpaceDimension>
class ShapeSignedDistanceFunction : public SpatialFunction<double, VSpaceDimension, Point<TCoordRep, VSpaceDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ShapeSignedDistanceFunction);

  /** Standard class type aliases. */
  using Self = ShapeSignedDistanceFunction;
  using Superclass = SpatialFunction<double, VSpaceDimension, Point<TCoordRep, VSpaceDimension>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ShapeSignedDistanceFunction, SpatialFunction);

  /** OutputType type alias support */
  using OutputType = typename Superclass::OutputType;

  /** InputType type alias support */
  using InputType = typename Superclass::InputType;

  /** Dimension underlying input image. */
  static constexpr unsigned int SpaceDimension = VSpaceDimension;

  /** CoordRep type alias support */
  using CoordRepType = TCoordRep;

  /** Point type alias support */
  using PointType = InputType;

  /** Type of the shape parameters. */
  using ParametersType = OptimizerParameters<double>;

  /** A shape is defined by a set of shape parameters. */
  virtual void
  SetParameters(const ParametersType &) = 0;

  virtual ParametersType &
  GetParameters()
  {
    return m_Parameters;
  }
  virtual unsigned int
  GetNumberOfShapeParameters() const = 0;

  virtual unsigned int
  GetNumberOfPoseParameters() const = 0;

  virtual unsigned int
  GetNumberOfParameters() const
  {
    return this->GetNumberOfShapeParameters() + this->GetNumberOfPoseParameters();
  }

  /** Evaluate the signed distance from a shape at a given position. */
  OutputType
  Evaluate(const PointType & point) const override = 0;

  /** Initialize must be called before the first call of SetParameters() or
   Evaluate() to allow the class to validate any inputs. */
  virtual void
  Initialize()
  {}

protected:
  ShapeSignedDistanceFunction() = default;

  ~ShapeSignedDistanceFunction() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
    // FIX    os << indent << "Parameters: " << m_Parameters << std::endl;
  }

  ParametersType m_Parameters;
};
} // end namespace itk

#endif
