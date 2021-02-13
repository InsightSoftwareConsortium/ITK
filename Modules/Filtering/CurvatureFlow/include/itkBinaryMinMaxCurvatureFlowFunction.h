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
#ifndef itkBinaryMinMaxCurvatureFlowFunction_h
#define itkBinaryMinMaxCurvatureFlowFunction_h

#include "itkMinMaxCurvatureFlowFunction.h"
#include "itkMacro.h"

namespace itk
{
/**
 *\class BinaryMinMaxCurvatureFlowFunction
 *
 * This class encapsulate the finite difference equation which drives a
 * min/max curvature flow algorithm for denoising binary images.
 *
 * This class uses a zero flux Neumann boundary condition when computing
 * derivatives near the data boundary.
 *
 * This class operates as part of the finite difference solver hierarchy.
 *
 * \sa BinaryMinMaxCurvatureFlowImageFilter
 * \sa ZeroFluxNeumannBoundaryCondition
 * \ingroup FiniteDifferenceFunctions
 * \ingroup ITKCurvatureFlow
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT BinaryMinMaxCurvatureFlowFunction : public MinMaxCurvatureFlowFunction<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BinaryMinMaxCurvatureFlowFunction);

  /**  Standard class type aliases. */
  using Self = BinaryMinMaxCurvatureFlowFunction;
  using Superclass = MinMaxCurvatureFlowFunction<TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(BinaryMinMaxCurvatureFlowFunction, MinMaxCurvatureFlowFunction);

  /** Inherit some parameters from the superclass type. */
  using PixelType = typename Superclass::PixelType;
  using RadiusType = typename Superclass::RadiusType;
  using NeighborhoodType = typename Superclass::NeighborhoodType;
  using FloatOffsetType = typename Superclass::FloatOffsetType;
  using ImageType = typename Superclass::ImageType;

  /** Extract superclass dimension. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Set/Get the threshold value. */
  void
  SetThreshold(const double thresh)
  {
    m_Threshold = thresh;
  }
  const double &
  GetThreshold() const
  {
    return m_Threshold;
  }

  /** This method computes the solution update for each pixel that does not
   * lie on a the data set boundary. */
  PixelType
  ComputeUpdate(const NeighborhoodType & it,
                void *                   globalData,
                const FloatOffsetType &  offset = FloatOffsetType(0.0)) override;

protected:
  BinaryMinMaxCurvatureFlowFunction();
  ~BinaryMinMaxCurvatureFlowFunction() override = default;

private:
  double m_Threshold;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinaryMinMaxCurvatureFlowFunction.hxx"
#endif

#endif
