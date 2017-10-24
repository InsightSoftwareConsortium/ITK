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
template< typename TImageType >
class GPUFiniteDifferenceFunction : public FiniteDifferenceFunction< TImageType >
{
public:

  /** Standard class typedefs. */
  typedef GPUFiniteDifferenceFunction            Self;
  typedef FiniteDifferenceFunction< TImageType > Superclass;
  typedef Superclass                             DifferenceFunctionType;
  typedef SmartPointer< Self >                   Pointer;
  typedef SmartPointer< const Self >             ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(GPUFiniteDifferenceFunction, FiniteDifferenceFunction);

  /** Extract some parameters from the image type */
  typedef typename Superclass::ImageType     ImageType;
  typedef typename Superclass::PixelType     PixelType;
  typedef typename Superclass::PixelRealType PixelRealType;

  /** Save image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, ImageType::ImageDimension);

  /** Define the TimeStepType to always be double. */
  typedef typename Superclass::TimeStepType TimeStepType;

  /** The default boundary condition for finite difference
   * functions that is used unless overridden in the Evaluate() method. */
  typedef typename Superclass::DefaultBoundaryConditionType DefaultBoundaryConditionType;

  /** Neighborhood radius type */
  typedef typename Superclass::RadiusType RadiusType;

  /** Neighborhood type */
  typedef typename Superclass::NeighborhoodType NeighborhoodType;

  /** A floating point offset from an image grid location. Used for
    * interpolation among grid values in a neighborhood. */
  typedef typename Superclass::FloatOffsetType FloatOffsetType;

#if !defined( ITK_WRAPPING_PARSER )
  /** Empty implementation - this will not be used by GPU filters */
  virtual PixelType  ComputeUpdate( const NeighborhoodType & itkNotUsed(neighborhood),
                                    void *itkNotUsed(globalData),
                                    const FloatOffsetType & itkNotUsed(offset = FloatOffsetType(0.0)) )
    ITK_OVERRIDE
  {
    PixelType pix = itk::NumericTraits<PixelType>::ZeroValue();
    return pix;
  }
#endif

  /** GPU function to compute update buffer */
  virtual void GPUComputeUpdate( const typename TImageType::Pointer output,
                                 typename TImageType::Pointer update,
                                 void *gd) = 0;

  /** Allocate GPU buffers for computing metric statitics
   * */
  virtual void GPUAllocateMetricData(unsigned int itkNotUsed(numPixels)) {}

  /** Release GPU buffers for computing metric statitics
   * */
  virtual void GPUReleaseMetricData() {}

protected:
  GPUFiniteDifferenceFunction() {
    m_GPUKernelManager = GPUKernelManager::New();
  }
  ~GPUFiniteDifferenceFunction() ITK_OVERRIDE {}

  /** GPU kernel manager for GPUFiniteDifferenceFunction class */
  typename GPUKernelManager::Pointer m_GPUKernelManager;

  /** GPU kernel handle for GPUComputeUpdate() */
  int m_ComputeUpdateGPUKernelHandle;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUFiniteDifferenceFunction);

};
} // end namespace itk

#endif
