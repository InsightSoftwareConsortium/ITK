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
#ifndef itkConicShellInteriorExteriorSpatialFunction_h
#define itkConicShellInteriorExteriorSpatialFunction_h

#include "vnl/vnl_vector.h"
#include "itkInteriorExteriorSpatialFunction.h"
#include "itkCovariantVector.h"

namespace itk
{
/**
 * \class ConicShellInteriorExteriorSpatialFunction
 * \brief Spatial function implementation of a conic shell
 *
 * We are creating search areas from BoundaryPoint1 in which to look for
 * candidate BoundaryPoint2's with which to form core atoms.  Assume the
 * "worst case" that BoundaryPoint2 is somewhere in that search area pointing
 * directly at BoundaryPoint1.
 *
 * The search area (ConicShell?) from each BoundaryPoint1 has the following
 * parameters:
 *
 * DistanceMax and DistanceMin from the location of the BoundaryPoint
 *
 * AngleMax from the line along the gradient at the boundary point.
 * This is determined in n dimensions by taking the dot product of two vectors,
 * (1) the normalized gradient at BoundaryPoint1 and
 * (2) the normalized vector from BoundaryPoint1 to BoundaryPoint2.
 *
 * If the absolute value of that dot product is greater than (1 - epsilon)
 * then you are in the ConicShell. This epsilon is the same one determining
 * face-to-faceness in the IEEE TMI paper.
 *
 * The polarity indicates which direction along the gradient of BoundaryPoint1
 * the function is to be evaluated.
 *
 * \ingroup SpatialFunctions
 *
 *
 * \ingroup ITKCommon
 */

template <unsigned int VDimension = 3, typename TInput = Point<double, VDimension>>
class ITK_TEMPLATE_EXPORT ConicShellInteriorExteriorSpatialFunction
  : public InteriorExteriorSpatialFunction<VDimension, TInput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ConicShellInteriorExteriorSpatialFunction);

  /** Standard class type aliases. */
  using Self = ConicShellInteriorExteriorSpatialFunction;
  using Superclass = InteriorExteriorSpatialFunction<VDimension, TInput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run time information. */
  itkTypeMacro(ConicShellInteriorExteriorSpatialFunction, InteriorExteriorSpatialFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Input type for the function. */
  using InputType = typename Superclass::InputType;

  /** Output type for the function. */
  using OutputType = typename Superclass::OutputType;

  /** The type of vector used to store the gradient info. */
  using GradientType = CovariantVector<double, VDimension>;

  /** Evaluates the function at a given position. */
  OutputType
  Evaluate(const InputType & position) const override;

  /** Set/Get the origin of the function. */
  itkGetConstMacro(Origin, InputType);
  itkSetMacro(Origin, InputType);

  /** Set/Get the gradient at the origin of the function. */
  GradientType
  GetOriginGradient()
  {
    return m_OriginGradient;
  }
  void
  SetOriginGradient(GradientType grad);

  /** Set/Get the minimum search distance. */
  itkGetConstMacro(DistanceMin, double);
  itkSetMacro(DistanceMin, double);

  /** Set/Get the maximum search distance. */
  itkGetConstMacro(DistanceMax, double);
  itkSetMacro(DistanceMax, double);

  /** Set/Get the tolerance of the in/out comparison. */
  itkGetConstMacro(Epsilon, double);
  itkSetMacro(Epsilon, double);

  /** Set/Get direction along the gradient to search.
   * Set to true to use the direction that the gradient is pointing;
   * set to false for the opposite direction. Default is Off. */
  itkGetConstMacro(Polarity, bool);
  itkSetMacro(Polarity, bool);
  itkBooleanMacro(Polarity);

protected:
  ConicShellInteriorExteriorSpatialFunction();
  ~ConicShellInteriorExteriorSpatialFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  InputType    m_Origin;
  GradientType m_OriginGradient;
  double       m_DistanceMin{ 0.0 };
  double       m_DistanceMax{ 0.0 };
  double       m_Epsilon{ 0.0 };
  bool         m_Polarity{ false };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkConicShellInteriorExteriorSpatialFunction.hxx"
#endif

#endif
