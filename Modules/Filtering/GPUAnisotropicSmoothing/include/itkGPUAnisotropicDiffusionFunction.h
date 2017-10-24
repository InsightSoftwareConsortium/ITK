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
#ifndef itkGPUAnisotropicDiffusionFunction_h
#define itkGPUAnisotropicDiffusionFunction_h

#include "itkGPUFiniteDifferenceFunction.h"

namespace itk
{
/**
 * \class GPUAnisotropicDiffusionFunction
 * This is the GPU version of AnisotropicDiffusionFunction class.
 *
 * \par How to use this class
 * This class must be subclassed to provide the GPUCalculateUpdate() methods of
 * GPUFiniteDifferenceFunction and the function
 * CalculateAverageGradientMagnitudeSquared(), which is called before each
 * iteration to recalibrate the conductance term.
 *
 * \ingroup ITKGPUAnisotropicSmoothing
 */
template< typename TImage >
class GPUAnisotropicDiffusionFunction :
  public GPUFiniteDifferenceFunction< TImage >
{
public:
  /** Standard class typedefs. */
  typedef GPUAnisotropicDiffusionFunction       Self;
  typedef GPUFiniteDifferenceFunction< TImage > Superclass;
  typedef SmartPointer< Self >                  Pointer;
  typedef SmartPointer< const Self >            ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(GPUAnisotropicDiffusionFunction, GPUFiniteDifferenceFunction);

  /** Inherit some parameters from the superclass type */
  typedef typename Superclass::ImageType        ImageType;
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::PixelRealType    PixelrealType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::TimeStepType     TimeStepType;
  typedef typename Superclass::FloatOffsetType  FloatOffsetType;

  /** Inherit some parameters from the superclass type */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** This method is called before each iteration.  It calculates a scalar
      value that is the average of the gradient magnitude squared at each pixel
      in the output image (intermediate solution). The average gradient magnitude
      value is typically used in the anisotropic diffusion equations to
      calibrate the conductance term. */
  virtual void GPUCalculateAverageGradientMagnitudeSquared(ImageType *) = 0;

  /** Set/Get the time step. For this class of anisotropic diffusion filters,
      the time-step is supplied by the user and remains fixed for all
      updates. */
  void SetTimeStep(const TimeStepType & t)
  {
    m_TimeStep = t;
  }

  const TimeStepType & GetTimeStep() const
  {
    return m_TimeStep;
  }

  /** Set/Get the conductance parameter.  The conductance parameter. */
  void SetConductanceParameter(const double & c)
  {
    m_ConductanceParameter = c;
  }

  const double & GetConductanceParameter() const
  {
    return m_ConductanceParameter;
  }

  /** Set/Get the average gradient magnitude squared. */
  const double & GetAverageGradientMagnitudeSquared() const
  {
    return m_AverageGradientMagnitudeSquared;
  }

  void SetAverageGradientMagnitudeSquared(const double & c)
  {
    m_AverageGradientMagnitudeSquared = c;
  }

  /** Returns the time step supplied by the user.  We don't need to use the
   * global data supplied since we are returning a fixed value.  */
  virtual TimeStepType ComputeGlobalTimeStep( void *itkNotUsed(GlobalData) ) const ITK_OVERRIDE
  {
    return this->GetTimeStep();
  }

  /** The anisotropic diffusion classes don't use this particular parameter
   * so it's safe to return a null value. */
  virtual void * GetGlobalDataPointer() const ITK_OVERRIDE
  {
    return ITK_NULLPTR;
  }

  /** Does nothing.  No global data is used in this class of equations.   */
  virtual void ReleaseGlobalDataPointer( void *itkNotUsed(GlobalData) ) const ITK_OVERRIDE
  {
    /* do nothing */
  }

protected:
  GPUAnisotropicDiffusionFunction()
  {
    m_AverageGradientMagnitudeSquared = 0.0;
    m_ConductanceParameter     = 1.0;     // default value
    m_TimeStep                 = 0.125f;  // default value
  }

  ~GPUAnisotropicDiffusionFunction() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "TimeStep: " << m_TimeStep << std::endl;
    os << indent << "ConductanceParameter: " << m_ConductanceParameter
       << std::endl;
  }

  // GPU buffer for Computing Average Squared Gradient Magnitude
  typename GPUDataManager::Pointer   m_AnisotropicDiffusionFunctionGPUBuffer;
  typename GPUKernelManager::Pointer m_AnisotropicDiffusionFunctionGPUKernelManager;

  // GPU Kernel Handles
  int m_AverageGradientMagnitudeSquaredGPUKernelHandle;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUAnisotropicDiffusionFunction);

  double       m_AverageGradientMagnitudeSquared;
  double       m_ConductanceParameter;
  TimeStepType m_TimeStep;
};
} // end namespace itk

#endif
