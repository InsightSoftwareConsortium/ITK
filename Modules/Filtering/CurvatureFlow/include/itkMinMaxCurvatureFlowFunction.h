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
#ifndef itkMinMaxCurvatureFlowFunction_h
#define itkMinMaxCurvatureFlowFunction_h

#include "itkCurvatureFlowFunction.h"
#include "itkNeighborhoodOperator.h"

namespace itk
{
/**
 *\class MinMaxCurvatureFlowFunction
 *
 * This class encapsulate the finite difference equation which drives a
 * min/max curvature flow denoising algorithm.
 *
 * This class uses a zero flux Neumann boundary condition when computing
 * derivatives near the data boundary.
 *
 * This class operates as part of the finite difference solver hierarchy.
 *
 * \sa MinMaxCurvatureFlowImageFilter
 * \sa ZeroFluxNeumannBoundaryCondition
 * \ingroup FiniteDifferenceFunctions
 * \ingroup ITKCurvatureFlow
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT MinMaxCurvatureFlowFunction : public CurvatureFlowFunction<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MinMaxCurvatureFlowFunction);

  /**  Standard class type aliases. */
  using Self = MinMaxCurvatureFlowFunction;
  using Superclass = CurvatureFlowFunction<TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(MinMaxCurvatureFlowFunction, CurvatureFlowFunction);

  /** Inherit some parameters from the superclass type. */
  using ImageType = typename Superclass::ImageType;
  using PixelType = typename Superclass::PixelType;
  using RadiusType = typename Superclass::RadiusType;
  using NeighborhoodType = typename Superclass::NeighborhoodType;
  using FloatOffsetType = typename Superclass::FloatOffsetType;

  /** Extract superclass dimension. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Typedef support for the stencil radius. */
  using RadiusValueType = typename RadiusType::SizeValueType;

  /** Set/Get the stencil radius. */
  void
  SetStencilRadius(const RadiusValueType value);

  const RadiusValueType &
  GetRadiusValueType() const
  {
    return m_StencilRadius;
  }

  /** Convenience function for symmetry with SetStencilRadius. */
  const RadiusValueType &
  GetStencilRadius() const
  {
    return GetRadiusValueType();
  }

  /** This method computes the solution update for each pixel that does not
   * lie on a the data set boundary. */
  PixelType
  ComputeUpdate(const NeighborhoodType & it,
                void *                   globalData,
                const FloatOffsetType &  offset = FloatOffsetType(0.0)) override;

protected:
  MinMaxCurvatureFlowFunction();
  ~MinMaxCurvatureFlowFunction() override = default;

  using StencilOperatorType = Neighborhood<PixelType, Self::ImageDimension>;
  StencilOperatorType m_StencilOperator;

  /** Initialize the stencil operator to be an N-Dimensional sphere
   * of radius m_StencilRadius. */
  void
  InitializeStencilOperator();

private:
  RadiusValueType m_StencilRadius;

  // To control overloaded versions of ComputeThreshold
  struct DispatchBase
  {};
  template <signed int VDimension>
  struct Dispatch : public DispatchBase
  {};

  /** This method computes the threshold by averaging the intensity
   *  in direction perpendicular to the image gradient. */
  PixelType
  ComputeThreshold(const Dispatch<2> &, const NeighborhoodType & it) const;

  PixelType
  ComputeThreshold(const Dispatch<3> &, const NeighborhoodType & it) const;

  PixelType
  ComputeThreshold(const DispatchBase &, const NeighborhoodType & it) const;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMinMaxCurvatureFlowFunction.hxx"
#endif

#endif
