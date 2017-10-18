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
#ifndef itkMinMaxCurvatureFlowFunction_h
#define itkMinMaxCurvatureFlowFunction_h

#include "itkCurvatureFlowFunction.h"
#include "itkNeighborhoodOperator.h"

namespace itk
{
/** \class MinMaxCurvatureFlowFunction
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
template< typename TImage >
class ITK_TEMPLATE_EXPORT MinMaxCurvatureFlowFunction:
  public CurvatureFlowFunction< TImage >
{
public:
  /**  Standard class typedefs. */
  typedef MinMaxCurvatureFlowFunction     Self;
  typedef CurvatureFlowFunction< TImage > Superclass;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(MinMaxCurvatureFlowFunction,
               CurvatureFlowFunction);

  /** Inherit some parameters from the superclass type. */
  typedef typename Superclass::ImageType        ImageType;
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::FloatOffsetType  FloatOffsetType;

  /** Extract superclass dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** Typedef support for the stencil radius. */
  typedef typename RadiusType::SizeValueType RadiusValueType;

  /** Set/Get the stencil radius. */
  void SetStencilRadius(const RadiusValueType radius);

  const RadiusValueType & GetRadiusValueType() const
  { return m_StencilRadius; }

  /** Convenience function for symmetry with SetStencilRadius. */
  const RadiusValueType & GetStencilRadius() const
  { return GetRadiusValueType(); }

  /** This method computes the solution update for each pixel that does not
   * lie on a the data set boundary. */
  virtual PixelType ComputeUpdate(const NeighborhoodType & neighborhood,
                                  void *globalData,
                                  const FloatOffsetType & offset = FloatOffsetType(0.0)
                                  ) ITK_OVERRIDE;

protected:
  MinMaxCurvatureFlowFunction();
  ~MinMaxCurvatureFlowFunction() ITK_OVERRIDE {}

  typedef Neighborhood< PixelType, itkGetStaticConstMacro(ImageDimension) > StencilOperatorType;
  StencilOperatorType m_StencilOperator;

  /** Initialize the stencil opearator to be an N-Dimensional sphere
   * of radius m_StencilRadius. */
  void InitializeStencilOperator();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MinMaxCurvatureFlowFunction);

  RadiusValueType m_StencilRadius;

  // To control overloaded versions of ComputeThreshold
  struct DispatchBase {};
  template< signed int VDimension >
  struct Dispatch: public DispatchBase {};

  /** This method computes the threshold by averaging the intensity
   *  in direction perpendicular to the image gradient. */
  PixelType ComputeThreshold(const Dispatch< 2 > &,
                             const NeighborhoodType & neighborhood) const;

  PixelType ComputeThreshold(const Dispatch< 3 > &,
                             const NeighborhoodType & neighborhood) const;

  PixelType ComputeThreshold(const DispatchBase &,
                             const NeighborhoodType & neighborhood) const;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMinMaxCurvatureFlowFunction.hxx"
#endif

#endif
