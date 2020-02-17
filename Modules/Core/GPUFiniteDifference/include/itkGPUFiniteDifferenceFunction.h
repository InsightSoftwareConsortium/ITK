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
#ifndef itkGPUFiniteDifferenceFunction_h
#define itkGPUFiniteDifferenceFunction_h

#include "itkFiniteDifferenceFunction.h"
#include "itkLightObject.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkVector.h"

#include "itkGPUDataManager.h"
#include "itkGPUKernelManager.h"

namespace itk
{
/**
 * \class GPUFiniteDifferenceFunction
 *
 * This is a base class of GPU finite difference function.
 * Note that unlike most GPU classes, derived class of GPUFiniteDifferenceFunction
 * does not have corresponding CPU class as its parent but only has CPU class
 * FiniteDifferenceFunction as its base class. Therefore, only members of
 * FiniteDifferenceFunction are reused by its derived GPU classes.
 *
 * \par How to use this class
 * GPUFiniteDifferenceFunction must be subclassed to add functionality for
 * GPUComputeUpdate.
 *
 * \ingroup ITKGPUFiniteDifference
 **/
template <typename TImageType>
class GPUFiniteDifferenceFunction : public FiniteDifferenceFunction<TImageType>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUFiniteDifferenceFunction);

  /** Standard class type aliases. */
  using Self = GPUFiniteDifferenceFunction;
  using Superclass = FiniteDifferenceFunction<TImageType>;
  using DifferenceFunctionType = Superclass;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(GPUFiniteDifferenceFunction, FiniteDifferenceFunction);

  /** Extract some parameters from the image type */
  using ImageType = typename Superclass::ImageType;
  using PixelType = typename Superclass::PixelType;
  using PixelRealType = typename Superclass::PixelRealType;

  /** Save image dimension. */
  static constexpr unsigned int ImageDimension = ImageType::ImageDimension;

  /** Define the TimeStepType to always be double. */
  using TimeStepType = typename Superclass::TimeStepType;

  /** The default boundary condition for finite difference
   * functions that is used unless overridden in the Evaluate() method. */
  using DefaultBoundaryConditionType = typename Superclass::DefaultBoundaryConditionType;

  /** Neighborhood radius type */
  using RadiusType = typename Superclass::RadiusType;

  /** Neighborhood type */
  using NeighborhoodType = typename Superclass::NeighborhoodType;

  /** A floating point offset from an image grid location. Used for
   * interpolation among grid values in a neighborhood. */
  using FloatOffsetType = typename Superclass::FloatOffsetType;

#if !defined(ITK_WRAPPING_PARSER)
  /** Empty implementation - this will not be used by GPU filters */
  PixelType
  ComputeUpdate(const NeighborhoodType & itkNotUsed(neighborhood),
                void *                   itkNotUsed(globalData),
                const FloatOffsetType &  itkNotUsed(offset = FloatOffsetType(0.0))) override
  {
    PixelType pix = itk::NumericTraits<PixelType>::ZeroValue();
    return pix;
  }
#endif

  /** GPU function to compute update buffer */
  virtual void
  GPUComputeUpdate(const typename TImageType::Pointer output, typename TImageType::Pointer update, void * gd) = 0;

  /** Allocate GPU buffers for computing metric statistics
   * */
  virtual void
  GPUAllocateMetricData(unsigned int itkNotUsed(numPixels))
  {}

  /** Release GPU buffers for computing metric statistics
   * */
  virtual void
  GPUReleaseMetricData()
  {}

protected:
  GPUFiniteDifferenceFunction() { m_GPUKernelManager = GPUKernelManager::New(); }
  ~GPUFiniteDifferenceFunction() override = default;

  /** GPU kernel manager for GPUFiniteDifferenceFunction class */
  typename GPUKernelManager::Pointer m_GPUKernelManager;

  /** GPU kernel handle for GPUComputeUpdate() */
  int m_ComputeUpdateGPUKernelHandle;
};
} // end namespace itk

#endif
