/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkArrivalFunctionToPathFilter_h
#define itkArrivalFunctionToPathFilter_h

#include "itkImage.h"
#include "itkCommand.h"
#include "itkImageToPathFilter.h"
#include "itkSingleImageCostFunction.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkSingleValuedNonLinearOptimizer.h"
#include "itkPolyLineParametricPath.h"

namespace itk
{
/**
 * \class ArrivalFunctionToPathCommand
 * \brief A command to listen for Optimizer Iteration events.
 * \author Dan Mueller, Queensland University of Technology,
 * dan.muel[at]gmail.com
 *
 *
 * \ingroup MinimalPathExtraction
 *
 */

template <typename TFilter>
class ITK_TEMPLATE_EXPORT ArrivalFunctionToPathCommand : public itk::Command
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ArrivalFunctionToPathCommand);

  /** Standard class type alias. */
  using Self = ArrivalFunctionToPathCommand;
  using Superclass = itk::Command;
  using Pointer = itk::SmartPointer<Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Some useful type alias. */
  using FilterType = TFilter;

  /** Get/set the Filter. */
  itkSetObjectMacro(Filter, FilterType);
  itkGetConstObjectMacro(Filter, FilterType);

  /** Execute */
  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    const auto * constCaller = const_cast<const itk::Object *>(caller);
    Execute(constCaller, event);
  }

  /** Execute */
  void
  Execute(const itk::Object * object, const itk::EventObject & event) override
  {
    if (!itk::IterationEvent().CheckEvent(&event))
    {
      return;
    }
    // Pass event to Filter
    if (m_Filter.IsNotNull())
    {
      m_Filter->Execute(object, event);
    }
  }

protected:
  ArrivalFunctionToPathCommand() = default;
  ~ArrivalFunctionToPathCommand() override = default;

private:
  typename FilterType::Pointer m_Filter;
};


/** \class ArrivalFunctionToPathFilter
 * \brief Extracts a path from a Fast Marching arrival function.
 *
 * This filter extracts the geodesic (minimal) path between the given
 * end-point and a start-point (which is implicitly embedded in the
 * given arrival function). The path is extracted by back-propagating
 * perpendicular to the Fast Marching front from the end-point to the
 * global minimum of the arrival function (ie. the start-point).
 * A step-wise optimizer is used to perform the back-propagation.
 *
 * The user must provide the following:
 *    1. A real-valued arrival function as the filter input
 *    2. At least one path end point.
 * The arrival function must be a real-valued (float or double) image
 * in the range [0,inf). If multiple end points are given, multiple
 * paths are extracted and saved to separate filter outputs.
 *
 * A cost function optimizer may also be provided. If an optimizer
 * is not given, a RegularStepGradientDescentOptimizer is created
 * with default settings. The optimizer is responsible for
 * tracking from the given end-point to the embedded start-point.
 * This filter listens for the optimizer Iteration event and
 * stores the current position as a point in the current path.
 * Therefore, only step-wise optimizers (which report their
 * intermediate position at each iteration) are suitable for
 * extracting the path. Current suitable optimizers include:
 * RegularStepGradientDescentOptimizer, GradientDescentOptimizer,
 * and IterateNeighborhoodOptimizer.
 *
 * The TerminationValue parameter prevents unwanted oscillations
 * when closing in on the start-point. The optimizer is terminated
 * when the current arrival value is less than TerminationValue;
 * the smaller the value, the closer the path will get to the
 * start-point. The default is 1.0. It is recommended that your
 * optimizer has a small step size when TerminationValue is small.
 *
 * This filter is based on the methods described in:
 * [1] J. Sethian. Level Set Methods and Fast Marching Methods, chapter 20.
 *     Cambridge Press, 2nd edition, 1999.
 * [2] J. Andrews and J. Sethian. Fast marching methods for the continuous
 *     traveling salesman problem. Proceedings of the National Academy of
 *     Sciences (PNAS), 104(4):1118/1123, 2007.
 *
 * \author Dan Mueller, Queensland University of Technology, dan.muel[at]gmail.com
 *
 * \ingroup ImageToPathFilters
 *
 * \ingroup MinimalPathExtraction
 */
template <typename TInputImage, typename TOutputPath = PolyLineParametricPath<TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT ArrivalFunctionToPathFilter : public ImageToPathFilter<TInputImage, TOutputPath>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ArrivalFunctionToPathFilter);

  /** Standard class type alias. */
  using Self = ArrivalFunctionToPathFilter;
  using Superclass = ImageToPathFilter<TInputImage, TOutputPath>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkOverrideGetNameOfClassMacro(ArrivalFunctionToPathFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Some image type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;

  /** Some path type alias. */
  using OutputPathType = TOutputPath;
  using OutputPathPointer = typename OutputPathType::Pointer;
  using OutputPathConstPointer = typename OutputPathType::ConstPointer;

  /** ImageDimension constants. */
  static constexpr unsigned int InputImageDimension = InputImageType::ImageDimension;

  /** Some convenient type alias. */
  using IndexType = Index<InputImageDimension>;
  using ContinuousIndexType = ContinuousIndex<double, InputImageDimension>;
  using PointType = Point<double, InputImageDimension>;
  using CommandType = ArrivalFunctionToPathCommand<Self>;
  using CostFunctionType = SingleImageCostFunction<InputImageType>;
  using OptimizerType = SingleValuedNonLinearOptimizer;
  using DefaultOptimizerType = RegularStepGradientDescentOptimizer;

  /** The points are in vectors to support extended "nodes" */
  using PointsContainerType = std::vector<PointType>;

  /** Get/set the Optimizer. */
  itkSetObjectMacro(Optimizer, OptimizerType);
  itkGetConstObjectMacro(Optimizer, OptimizerType);

  /** Get/set the cost function. The filter (not the user) is responsible
   *  for connecting the arrival function to the cost function. */
  itkSetObjectMacro(CostFunction, CostFunctionType);
  itkGetConstObjectMacro(CostFunction, CostFunctionType);

  /** Clears the list of end points and adds the given point to the list. */
  virtual void
  SetPathEndPoint(const PointType & point);

  /** Adds the given point to the list. */
  virtual void
  AddPathEndPoint(const PointType & point);

  /** Clear the list of end points. */
  virtual void
  ClearPathEndPoints();

  /** Get/set the termination. Once the current optimizer value falls below
   *  TerminationValue, no further points will be appended to the path.
   *  The default value is 1.0. */
  itkSetMacro(TerminationValue, typename OptimizerType::MeasureType);
  itkGetMacro(TerminationValue, typename OptimizerType::MeasureType);

  /** Handle optimizer iteration events. */
  virtual void
  Execute(const itk::Object * object, const itk::EventObject & event);

protected:
  ArrivalFunctionToPathFilter();
  ~ArrivalFunctionToPathFilter() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Override since the filter needs all the data for the algorithm */
  void
  GenerateInputRequestedRegion() override;

  /** Implemention of algorithm */
  void
  GenerateData() override;

  /** Get the arrival function from which to extract the path. */
  virtual unsigned int
  GetNumberOfPathsToExtract() const;

  /** Compute the arrival function from which to extract the path.
   *  In this case it is simply the filter input. */
  virtual InputImageType *
  ComputeArrivalFunction();

  /** Get the next end point from which to back propagate. */
  virtual const PointsContainerType &
  GetNextEndPoint();

  typename CostFunctionType::Pointer  m_CostFunction;
  typename OptimizerType::Pointer     m_Optimizer;
  typename OptimizerType::MeasureType m_TerminationValue;
  std::vector<PointsContainerType>    m_PointList;
  unsigned int                        m_CurrentOutput;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkArrivalFunctionToPathFilter.hxx"
#endif

#endif
