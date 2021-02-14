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
#ifndef itkCurvatureFlowFunction_h
#define itkCurvatureFlowFunction_h

#include "itkFiniteDifferenceFunction.h"
#include "itkMacro.h"

namespace itk
{
/**
 *\class CurvatureFlowFunction
 *
 * \brief
 * This class encapsulate the finite difference equation which drives a
 * curvature flow denoising algorithm.
 *
 * This class uses a zero flux Neumann boundary condition when computing
 * derivatives near the data boundary.
 *
 * This class operates as part of the finite difference solver hierarchy.
 *
 * \sa CurvatureFlowImageFilter
 * \sa ZeroFluxNeumannBoundaryCondition
 * \ingroup FiniteDifferenceFunctions
 * \ingroup ITKCurvatureFlow
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT CurvatureFlowFunction : public FiniteDifferenceFunction<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(CurvatureFlowFunction);

  /**  Standard class type aliases. */
  using Self = CurvatureFlowFunction;
  using Superclass = FiniteDifferenceFunction<TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(CurvatureFlowFunction, FiniteDifferenceFunction);

  /** Inherit some parameters from the superclass type. */
  using ImageType = typename Superclass::ImageType;
  using PixelType = typename Superclass::PixelType;
  using RadiusType = typename Superclass::RadiusType;
  using ScalarValueType = PixelType;
  using PixelRealType = typename Superclass::PixelRealType;
  using NeighborhoodType = typename Superclass::NeighborhoodType;
  using NeighborhoodScalesType = typename Superclass::NeighborhoodScalesType;
  using FloatOffsetType = typename Superclass::FloatOffsetType;
  using TimeStepType = typename Superclass::TimeStepType;

  /** Extract superclass dimension. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Computes the time step for an update given a global data structure.
   * The data used in the computation may take different forms depending on
   * the nature of the equations.  This global data cannot be kept in the
   * instance of the equation object itself since the equation object must
   * remain stateless for thread safety.  The global data is therefore managed
   * for each thread by the finite difference solver filters.
   *
   * Currently, this function returns the user specified constant time step.
   * \todo compute timestep based on CFL condition.
   */
  TimeStepType
  ComputeGlobalTimeStep(void * GlobalData) const override;

  /** Returns a pointer to a global data structure that is passed to this
   * object from the solver at each calculation.  The idea is that the solver
   * holds the state of any global values needed to calculate the time step,
   * while the equation object performs the actual calculations.  The global
   * data should also be initialized in this method. */
  void *
  GetGlobalDataPointer() const override
  {
    auto * ans = new GlobalDataStruct();

    ans->m_MaxChange = NumericTraits<ScalarValueType>::ZeroValue();
    return ans;
  }

  /** When the finite difference solver filter has finished using a global
   * data pointer, it passes it to this method, which frees the memory.
   * The solver cannot free the memory because it does not know the type
   * to which the pointer points. */
  void
  ReleaseGlobalDataPointer(void * GlobalData) const override
  {
    delete (GlobalDataStruct *)GlobalData;
  }

  /** Set the time step parameter */
  void
  SetTimeStep(const TimeStepType & t)
  {
    m_TimeStep = t;
  }

  /** Get the time step parameter */
  const TimeStepType &
  GetTimeStep() const
  {
    return m_TimeStep;
  }

  /** This method computes the solution update for each pixel that does not
   * lie on a the data set boundary. */
  PixelType
  ComputeUpdate(const NeighborhoodType & it,
                void *                   globalData,
                const FloatOffsetType &  offset = FloatOffsetType(0.0)) override;

protected:
  /// \cond HIDE_STRUCTURE

  /** A global data type for this class of equations.  Used to store
   * values that are needed in calculating the time step. */
  struct GlobalDataStruct
  {
    GlobalDataStruct() { m_MaxChange = NumericTraits<ScalarValueType>::ZeroValue(); }

    ~GlobalDataStruct() = default;

    ScalarValueType m_MaxChange;
  };
  /// \endcond

  CurvatureFlowFunction();
  ~CurvatureFlowFunction() override = default;

private:
  TimeStepType m_TimeStep;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCurvatureFlowFunction.hxx"
#endif

#endif
