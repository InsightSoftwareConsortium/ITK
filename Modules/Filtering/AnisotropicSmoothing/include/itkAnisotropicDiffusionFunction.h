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
#ifndef itkAnisotropicDiffusionFunction_h
#define itkAnisotropicDiffusionFunction_h

#include "itkFiniteDifferenceFunction.h"

namespace itk
{
/**
 * \class AnisotropicDiffusionFunction
 * This class is a virtual base for anisotropic diffusion function objects.  It
 * is a component object in the finite difference solver hierarchy (see
 * itkFiniteDifferenceImageFilter for an overview).
 * AnisotropicDiffusionFunction objects are used by
 * AnisotropicDiffusionImageFilter objects to perform non-linear diffusion on
 * itk::Images.
 *
 * \par Overview of anisotropic diffusion
 *
 * Anisotropic diffusion methods are formulated to reduce noise (or unwanted
 * detail) in images while preserving specific image features.  For many
 * applications, there is an assumption that light-dark transitions
 * (edges) are interesting.  Standard isotropic diffusion methods move and blur
 * light-dark boundaries.  Anisotropic diffusion methods are formulated to
 * specifically preserve edges.
 *
 * Anisotropic diffusion methods can be thought of as tools for calculating
 * multi-scale descriptions of images. Embed an image \f$U(\mathbf{x})\f$ in a
 * higher dimensional function of derived images, \f$U(\mathbf{x}, t)\f$.  This
 * higher dimensional function represents the solution of the heat diffusion
 * equation,
 *
 * \par
 * \f[\frac{d U(\mathbf{x})}{d t} = \nabla \cdot c \nabla U(\mathbf{x})\f]
 *
 * \par
 * with constant \f$c\f$ and initial condition
 * \f$U(\mathbf{x}, 0) = U_0(\mathbf{x})\f$, the original image.
 *
 * \par
 * Extending to the case where \f$c\f$ is not a constant, but a function of
 * \f$\mathbf{x}\f$, gives
 *
 * \par
 * \f[\frac{d U(\mathbf{x})}{d t} = C(\mathbf{x})\Delta U(\mathbf{x})
 * + \nabla C(\mathbf{x}) \nabla U(\mathbf{x})\f]
 *
 * \par
 * Our choice of \f$C\f$ now varies the strength of diffusion anisotropically.
 * Typically, \f$C\f$ is chosen as some function of image features to
 * selectively preserve or remove those features.  For example, edges tend to
 * be preserved over smoother regions where \f$C\f$ is inversely scaled
 * according to gradient magnitude as in
 *
 * \par
 * \f[C(\mathbf{x}) = e^{-(\frac{\parallel \nabla U(\mathbf{x}) \parallel}{K})^2}\f].
 *
 * \par
 * Several variations on the scheme presented above are implemented in Itk as
 * subclasses of this equation.  The equations are solved using an iterative,
 * finite forward difference technique (see the FiniteDifferenceImageFilter
 * class).
 *
 * \par How to use this class
 * This class must be subclassed to provide the CalculateUpdate() methods of
 * FiniteDifferenceFunction and the function
 * CalculateAverageGradientMagnitudeSquared(), which is called before each
 * iteration to recalibrate the conductance term.
 *
 * \par Parameters
 *  The parameters defined in this class apply to the basic anisotropic
 *  diffusion equation described in AnisotropicDiffusionFunction.  Variations
 *  on the basic equation will be more or less sensitive to these parameters.
 *  For example, functions that perform higher-order derivative calculations
 *  may require smaller time-steps than those that only do first-derivative
 *  calculations. Wherever possibe, reasonable parameters settings are
 *  suggested in the documentation of a specific equation implementation.
 *
 * \par TimeStep
 * In the anisotropic diffusion filter hierarchy, the time step is set
 * explicitly by the user.  The time step referred to here corresponds exactly
 * to \f$ \Delta t \f$ in the finite difference update equation described in
 * FiniteDifferenceImageFilter (see itkFiniteDifferenceImageFilter for more
 * information).  Appropriate time steps for solving this type of p.d.e. depend
 * on the dimensionality of the image and the order of the equation.
 * Stable values for most 2D and 3D functions are
 * 0.125 and 0.0625, respectively, when the pixel spacing is unity or is turned off.
 * In general, you should keep the time step below
 * \f$(PixelSpacing)/2^{N+1}\f$, where \f$N\f$ is the number of image
 * dimensions.  A filter will automatically attempt to constrain its time step
 * to a stable value and generate a run-time warning if the time step is set
 * too high.
 *
 * \par Conductance Parameter
 * The conductance parameter controls the sensitivity of the conductance term
 * in the basic anisotropic diffusion equation.  It affects the conductance term
 * in different ways depending on the particular variation on the basic
 * equation. As a general rule, the lower the value, the more strongly the
 * diffusion equation preserves image features (such as high gradients or
 * curvature).  A high value for conductance will cause the filter to diffuse
 * image features more readily.  Typical values range from 0.5 to 2.0 for data
 * like the Visible Human color data, but the correct value for your
 * application is wholly dependent on the results you want from a specific data
 * set and the number or iterations you perform.
 *
 * \par References
 * Pietro Perona and Jitendra Malik, ``Scale-space and edge detection using
 * anisotropic diffusion,'' IEEE Transactions on Pattern Analysis Machine
 * Intelligence, vol. 12, pp. 629-639, 1990.
 *
 * \sa VectorAnisotropicDiffusionFunction
 * \sa ScalarAnisotropicDiffusionFunction
 * \sa GradientAnisotropicDiffusionFunction
 * \sa CurvatureAnisotropicDiffusionFunction
 * \sa VectorGradientAnisotropicDiffusionFunction
 * \ingroup FiniteDifferenceFunctions
 * \ingroup ImageEnhancement
 * \todo Automatically generate the time step value from image dimensionality
 *  and order of the equations
 * \ingroup ITKAnisotropicSmoothing
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT AnisotropicDiffusionFunction:
  public FiniteDifferenceFunction< TImage >
{
public:
  /** Standard class typedefs. */
  typedef AnisotropicDiffusionFunction       Self;
  typedef FiniteDifferenceFunction< TImage > Superclass;
  typedef SmartPointer< Self >               Pointer;
  typedef SmartPointer< const Self >         ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(AnisotropicDiffusionFunction, FiniteDifferenceFunction);

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
  virtual void CalculateAverageGradientMagnitudeSquared(ImageType *) = 0;

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
  AnisotropicDiffusionFunction()
  {
    m_AverageGradientMagnitudeSquared = 0.0;
    m_ConductanceParameter     = 1.0;     // default value
    m_TimeStep                 = 0.125f;  // default value
  }

  ~AnisotropicDiffusionFunction() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "TimeStep: " << m_TimeStep << std::endl;
    os << indent << "ConductanceParameter: " << m_ConductanceParameter
       << std::endl;
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(AnisotropicDiffusionFunction);

  double       m_AverageGradientMagnitudeSquared;
  double       m_ConductanceParameter;
  TimeStepType m_TimeStep;
};
} // end namespace itk

#endif
