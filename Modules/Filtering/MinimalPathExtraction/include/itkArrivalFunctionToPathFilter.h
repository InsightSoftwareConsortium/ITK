/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkArrivalFunctionToPathFilter.h,v $
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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

template <class TFilter>
class ArrivalFunctionToPathCommand : public itk::Command
{
public:
  /** Standard class typedefs. */
  typedef ArrivalFunctionToPathCommand Self;
  typedef itk::Command                 Superclass;
  typedef itk::SmartPointer<Self>      Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Some useful typedefs. */
  typedef TFilter FilterType;

  /** Get/set the Filter. */
  itkSetObjectMacro(Filter, FilterType);
  itkGetConstObjectMacro(Filter, FilterType);

  /** Execute */
  void
  Execute(itk::Object * caller, const itk::EventObject & event) ITK_OVERRIDE
  {
    Execute((const itk::Object *)caller, event);
  }

  /** Execute */
  void
  Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
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
  ArrivalFunctionToPathCommand() {}
  ~ArrivalFunctionToPathCommand() {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ArrivalFunctionToPathCommand);

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
template <class TInputImage, class TOutputPath = PolyLineParametricPath<TInputImage::ImageDimension>>
class ITK_EXPORT ArrivalFunctionToPathFilter : public ImageToPathFilter<TInputImage, TOutputPath>
{
public:
  /** Standard class typedefs. */
  typedef ArrivalFunctionToPathFilter                 Self;
  typedef ImageToPathFilter<TInputImage, TOutputPath> Superclass;
  typedef SmartPointer<Self>                          Pointer;
  typedef SmartPointer<const Self>                    ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ArrivalFunctionToPathFilter, ImageToPathFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Some image typedefs. */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;

  /** Some path typedefs. */
  typedef TOutputPath                           OutputPathType;
  typedef typename OutputPathType::Pointer      OutputPathPointer;
  typedef typename OutputPathType::ConstPointer OutputPathConstPointer;

  /** ImageDimension constants. */
  itkStaticConstMacro(InputImageDimension, unsigned int, InputImageType::ImageDimension);

  /** Some convenient typedefs. */
  typedef Index<InputImageDimension>                   IndexType;
  typedef ContinuousIndex<double, InputImageDimension> ContinuousIndexType;
  typedef Point<double, InputImageDimension>           PointType;
  typedef ArrivalFunctionToPathCommand<Self>           CommandType;
  typedef SingleImageCostFunction<InputImageType>      CostFunctionType;
  typedef SingleValuedNonLinearOptimizer               OptimizerType;
  typedef RegularStepGradientDescentOptimizer          DefaultOptimizerType;

  /** The points are in vectors to support extended "nodes" */
  typedef std::vector<PointType> PointsContainerType;

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
   *  The default value is 0.0. */
  itkSetMacro(TerminationValue, typename OptimizerType::MeasureType);
  itkGetMacro(TerminationValue, typename OptimizerType::MeasureType);

  /** Handle optimizer iteration events. */
  virtual void
  Execute(const itk::Object * object, const itk::EventObject & event);

protected:
  ArrivalFunctionToPathFilter();
  ~ArrivalFunctionToPathFilter();
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Override since the filter needs all the data for the algorithm */
  void
  GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** Implemention of algorithm */
  void
  GenerateData(void) ITK_OVERRIDE;

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

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ArrivalFunctionToPathFilter);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkArrivalFunctionToPathFilter.hxx"
#endif

#endif
