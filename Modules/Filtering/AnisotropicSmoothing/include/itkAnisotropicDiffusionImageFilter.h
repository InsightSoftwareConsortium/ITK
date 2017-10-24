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
#ifndef itkAnisotropicDiffusionImageFilter_h
#define itkAnisotropicDiffusionImageFilter_h

#include "itkDenseFiniteDifferenceImageFilter.h"
#include "itkAnisotropicDiffusionFunction.h"
#include "itkNumericTraits.h"

namespace itk
{
/** \class AnisotropicDiffusionImageFilter
 *  This filter is the base class for a set of filters that iteratively diffuse
 *  images by solving non-linear partial differential equations based on the
 *  classical heat equation.  For an overview of the diffusion algorithm, see
 *  AnisotropicImageFunction.  AnisotropicDiffusionImageFilter is a subclass
 *  of itkFiniteDifferenceImageFilter and is part of the finite difference
 *  solver hierarchy.
 *
 *  \par Inputs and Outputs
 *  This is an image-to-image filter.  The requirements for data types and
 *  dimensionality of the input and output are defined by subclasses.  In
 *  general, these filters expect images of real-valued types.  This means
 *  pixel types of floats, doubles, or a user-defined type with floating point
 *  accuracy and arithmetic operations.
 *
 *  \par Parameters
 *  Set/GetNumberOfIterations specifies the number of iterations (time-step updates)
 *  that the solver will perform to produce a solution image.  The appropriate
 *  number of iterations is dependent on the application and the image being
 *  processed.  As a general rule, the more iterations performed, the more
 *  diffused the image will become.
 *
 *  \par
 *  Set/GetTimeStep sets the time step to be used for each iteration (update).
 *  This parameter is described in detail in itkAnisotropicDiffusionFunction.
 *  The time step is constrained at run-time to keep the solution stable.  In
 *  general, the time step should be at or below \f$(PixelSpacing)/2^{N+1}\f$,
 *  where \f$N\f$ is the dimensionality of the image.
 *
 *  \par
 *  Set/GetConductanceParameter set a common parameter used by subclasses of
 *  itkAnisotropicDiffusionFunction.   See itkAnisotropicDiffusionFunction for
 *  detailed information.
 *
 *  \par How to use this filter
 *  AnisotropicDiffusionImageFilter must be subclassed to be used.  This class
 *  implements a generic framework for other diffusion filters.
 *
 *  \sa GradientAnisotropicDiffusionImageFilter
 *  \sa VectorGradientAnisotropicDiffusionImageFilter
 *  \sa CurvatureAnisotropicDiffusionImageFilter
 *  \sa VectorCurvatureAnisotropicDiffusionImageFilter
 * \ingroup ImageEnhancement
 * \ingroup ITKAnisotropicSmoothing
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT AnisotropicDiffusionImageFilter:
  public DenseFiniteDifferenceImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef AnisotropicDiffusionImageFilter                               Self;
  typedef DenseFiniteDifferenceImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                          Pointer;
  typedef SmartPointer< const Self >                                    ConstPointer;

  /** Run-time type information. */
  itkTypeMacro(AnisotropicDiffusionImageFilter,
               DenseFiniteDifferenceImageFilter);

  /** Capture information from the superclass. */
  typedef typename Superclass::InputImageType   InputImageType;
  typedef typename Superclass::OutputImageType  OutputImageType;
  typedef typename Superclass::UpdateBufferType UpdateBufferType;

  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** The pixel type of the output image will be used in computations.
   * Inherited from the superclass. */
  typedef typename Superclass::PixelType    PixelType;
  typedef typename Superclass::TimeStepType TimeStepType;

  /** Set/Get the time step for each iteration */
  itkSetMacro(TimeStep, TimeStepType);
  itkGetConstMacro(TimeStep, TimeStepType);

  /** Set/Get the conductance parameter governing sensitivity of the
      conductance equation. */
  itkSetMacro(ConductanceParameter, double);
  itkGetConstMacro(ConductanceParameter, double);

  /** Set/Get the interval at which a new scaling for the conductance term is
      calculated.  */
  itkSetMacro(ConductanceScalingUpdateInterval, unsigned int);
  itkGetConstMacro(ConductanceScalingUpdateInterval, unsigned int);

  /** The following parameters are not used at this time.  Setting them will
      have no effect on the output */
  itkSetMacro(ConductanceScalingParameter, double);
  itkGetConstMacro(ConductanceScalingParameter, double);

  /** Supplies a fixed value for the average gradient magnitude of the image to
      the AnisotropicDiffusionFunction at each iteration.  The average gradient
      magnitude is normally calculated over the entire image before each
      iteration and is used as a scaling factor in the calculations of change
      at a pixel.  This method is  useful in streaming applications to avoid
      block artifacts by overriding the normal gradient magnitude calculation
      (i.e. all image chunks are scaled uniformly). */
  void SetFixedAverageGradientMagnitude(double a)
  {
    m_FixedAverageGradientMagnitude = a;
    this->Modified();
    m_GradientMagnitudeIsFixed = true;
  }

  itkGetConstMacro(FixedAverageGradientMagnitude, double);

protected:
  AnisotropicDiffusionImageFilter();
  ~AnisotropicDiffusionImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Supplies the halting criteria for this class of filters.  The
   * algorithm will stop after a user-specified number of iterations. */
  //  virtual bool Halt();

  /** Prepare for the iteration process. */
  virtual void InitializeIteration() ITK_OVERRIDE;

  bool m_GradientMagnitudeIsFixed;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(AnisotropicDiffusionImageFilter);

  double       m_ConductanceParameter;
  double       m_ConductanceScalingParameter;
  unsigned int m_ConductanceScalingUpdateInterval;
  double       m_FixedAverageGradientMagnitude;

  TimeStepType m_TimeStep;
};
} // end namspace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAnisotropicDiffusionImageFilter.hxx"
#endif

#endif
