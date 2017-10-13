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
#ifndef itkFiniteDifferenceFunction_h
#define itkFiniteDifferenceFunction_h

#include "itkLightObject.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkVector.h"

namespace itk
{
/**
 * \class FiniteDifferenceFunction
 *
 * This class is a component object of the finite difference solver hierarchy
 * (see FiniteDifferenceImageFilter).  It defines a generic interface for a
 * function object that computes a single scalar value from a neighborhood of
 * values.  Examples of the application of this class are the various flavors
 * of AnisotropicDiffusionFunction and LevelSetFunction objects.
 *
 * These functions calculate the incremental change at a pixel in the solution
 * image from one iteration of the Partial Differential Equation (PDE) solver to
 * the next.
 *
 * \par
 * Subclasses of FiniteDifferenceImageFilter (solvers) call the
 * ComputeUpdate() method of this class to compute \f$ \Delta u^n_{\mathbf{i}}
 * \f$ at each \f$ \mathbf{i} \f$  in \f$ u \f$.  Because the size of the time
 * step for each iteration of the p.d.e. solution depends on the particular
 * calculations done, this function object is also responsible for computing
 * that time step (see ComputeGlobalTimeStep() ).
 *
 * \par How to use this class
 * FiniteDifferenceFunction must be subclassed to add functionality for
 * ComputeUpdate, ComputeGlobalTimeStep, and Get/ReleaseGlobalDataPointer.
 *
 * \par A note on thread safety.
 * The ComputeUpdate() methods of this filter are declared as const to enforce
 * thread-safety during execution of FiniteDifferenceImageFilter solver
 * algorithms.  The InitializeIteration() method is intended to provide a safe
 * way to modify the state of the object between threaded calculations of
 * solvers.
 *
 * \todo Possibly subclass this object from Function.  Stumbling blocks here
 * are the specialized api of FiniteDifferenceFunction.
 *
 * \ingroup Functions
 * \ingroup ITKFiniteDifference
 */
template< typename TImageType >
class ITK_TEMPLATE_EXPORT FiniteDifferenceFunction:public LightObject
{
public:
  /** Standard class typedefs. */
  typedef FiniteDifferenceFunction   Self;
  typedef LightObject                Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(FiniteDifferenceFunction, LightObject);

  /** Extract some parameters from the image type */
  typedef TImageType                    ImageType;
  typedef typename ImageType::PixelType PixelType;
  typedef double                        PixelRealType;

  /** Save image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, ImageType::ImageDimension);

  /** Define the TimeStepType to always be double. */
  typedef double TimeStepType;

  /** The default boundary condition for finite difference
   * functions that is used unless overridden in the Evaluate() method. */
  typedef ZeroFluxNeumannBoundaryCondition< ImageType >
  DefaultBoundaryConditionType;

  /** Neighborhood radius type */
  typedef typename ConstNeighborhoodIterator< TImageType >::RadiusType RadiusType;

  /** The type of data structure that is passed to this function object
   * to evaluate at a pixel that does not lie on a data set boundary. */
  typedef ConstNeighborhoodIterator< TImageType, DefaultBoundaryConditionType > NeighborhoodType;

  /** The type of data structure that holds the scales with which the
   * neighborhood is weighted to properly account for spacing and neighborhood radius. */
  typedef Vector< PixelRealType, itkGetStaticConstMacro(ImageDimension) > NeighborhoodScalesType;

  /** A floating point offset from an image grid location. Used for
   * interpolation among grid values in a neighborhood. */
  typedef Vector< float, itkGetStaticConstMacro(ImageDimension) > FloatOffsetType;

  /** This method allows the function to set its state before each iteration
   *  of the finite difference solver (image filter) that uses it.  This is
   *  a thread-safe time to manipulate the object's state.
   *
   * An example of how this can be used: the AnisotropicDiffusionFunction class
   * of FiniteDifferenceFunction use this method to pre-calculate an average
   * gradient magnitude across the entire image region. This value is set in
   * the function object and used by the ComputeUpdate() methods that are called
   * at each pixel as a constant. */
  virtual void InitializeIteration() {}

  /** This method is called by a finite difference solver image filter at
   * each pixel that does not lie on a data set boundary.  The width of the
   * data set boundary is defined by the width of the neighborhood being
   * evaluated.
   *
   * The neighborhood argument is the neighborhood data.
   * The globalData argument is a pointer to a data structure that holds
   * values that need to be persistent across calls to this function, but
   * cannot be stored in the function object itself for thread-safety reasons.
   * Examples are values needed to compute the time-step for an iteration
   * of the solver.
   * \sa InitializeIteration
   * \sa ComputeGlobalTimeStep */
  virtual PixelType  ComputeUpdate( const NeighborhoodType & neighborhood,
                                    void *globalData,
                                    const FloatOffsetType & offset = FloatOffsetType(0.0) ) = 0;


  /** Sets the radius of the neighborhood this FiniteDifferenceFunction
   * needs to perform its calculations. */
  void SetRadius(const RadiusType & r);

  /** Returns the radius of the neighborhood this FiniteDifferenceFunction
   * needs to perform its calculations. */
  const RadiusType & GetRadius() const;

  /** Set the ScaleCoefficients for the difference
   * operators. The defaults a 1.0. These can be set to take the image
   * spacing into account. */
  void SetScaleCoefficients(PixelRealType vals[ImageDimension]);

  /** Returns the current scale coefficients. */
  void GetScaleCoefficients(PixelRealType vals[ImageDimension]) const;

  /** Compute the scales that weight the neighborhood during difference operations
   * to properly account for spacing and neighborhood radius
   */
  const NeighborhoodScalesType ComputeNeighborhoodScales() const;

  /** Computes the time step for an update given a global data structure.
   * The data used in the computation may take different forms depending on
   * the nature of the equations.  This global data cannot be kept in the
   * instance of the equation object itself since the equation object must
   * remain stateless for thread safety.  The global data is therefore managed
   * for each thread by the finite difference solver filters. */
  virtual TimeStepType ComputeGlobalTimeStep(void *GlobalData) const = 0;

  /** Returns a pointer to a global data structure that is passed to this
   * object from the solver at each calculation.  The idea is that the
   * solver holds the state of any global values needed to calculate the
   * time step, while the equation object performs the actual calculations.
   *
   * The global data should also be initialized in this method.
   * */
  virtual void * GetGlobalDataPointer() const = 0;

  /** When the finite difference solver filter has finished using a global
   * data pointer, it passes it to this method, which frees the memory.
   *
   * The solver cannot free the memory because it does not know the type
   * to which the pointer points. */
  virtual void ReleaseGlobalDataPointer(void *GlobalData) const = 0;

protected:
  FiniteDifferenceFunction();
  ~FiniteDifferenceFunction() ITK_OVERRIDE {}

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  RadiusType m_Radius;
  PixelRealType m_ScaleCoefficients[ImageDimension];

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FiniteDifferenceFunction);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFiniteDifferenceFunction.hxx"
#endif

#endif
