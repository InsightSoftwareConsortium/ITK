/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkSpeedFunctionToPathFilter.h,v $
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSpeedFunctionToPathFilter_h
#define __itkSpeedFunctionToPathFilter_h

#include "itkArrivalFunctionToPathFilter.h"

namespace itk
{

/** \class SpeedFunctionToPathFilter
 * \brief Extracts a path from a speed function between a start point
 *       and end point, which also passes near the given way points.
 *
 * This filter propagates a number of Fast Marching fronts using the
 * given speed image to facilitate the path extraction.
 * For each end- and way-point, a Fast Marching arrival function is
 * computed. The minimal path is extracted for each segment and
 * concatenated to obtain the total path between the start- and
 * end-points, while passing near the given way-points.
 *
 * The user must provide the following:
 *    1. A real-valued speed function as the filter input
 *    2. At least one PathInfo object using AddPathInfo().
 * The speed function must be a real-valued (float or double) image
 * in the range [0,1]. If multiple PathInfo objects are added,
 * multiple paths are extracted and saved to separate filter outputs.

 * A cost function optimizer may also be provided. If an optimizer
 * is not given, a RegularStepGradientDescentOptimizer is created
 * with default settings. Other suitable optimizers include
 * GradientDescentOptimizer and IterateNeighborhoodOptimizer.
 * See itkArrivalFunctionToPathFilter.h for more details.
 *
 * This filter is based on the methods described in:
 * [1] J. Sethian. Level Set Methods and Fast Marching Methods, chapter 20.
 *     Cambridge Press, 2nd edition, 1999.
 * [2] J. Andrews and J. Sethian. Fast marching methods for the continuous
 *     traveling salesman problem. Proceedings of the National Academy of
 *     Sciences (PNAS), 104(4):11181123, 2007.
 *
 * \author Dan Mueller, Queensland University of Technology, dan.muel[at]gmail.com
 *
 * \sa itkArrivalFunctionToPathFilter
 * \ingroup ImageToPathFilters
 */
template <class TInputImage, class TOutputPath = PolyLineParametricPath<TInputImage::ImageDimension>>
class ITK_EXPORT SpeedFunctionToPathFilter : public ArrivalFunctionToPathFilter<TInputImage, TOutputPath>
{
public:
  /** Standard class typedefs. */
  typedef SpeedFunctionToPathFilter                             Self;
  typedef ArrivalFunctionToPathFilter<TInputImage, TOutputPath> Superclass;
  typedef SmartPointer<Self>                                    Pointer;
  typedef SmartPointer<const Self>                              ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpeedFunctionToPathFilter, ArrivalFunctionToPathFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int, TInputImage::ImageDimension);

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

  /** Some convenient typedefs. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;
  typedef typename Superclass::IndexType           IndexType;
  typedef typename Superclass::PointType           PointType;
  typedef typename Superclass::CostFunctionType    CostFunctionType;
  typedef typename Superclass::OptimizerType       OptimizerType;

  /** Override superclass behaviour.
   *  Warning: SetPathEndPoint() is not valid for this filter.
   *  This method is provided by the superclass, however it is not
   *  used by this subclass. Use AddPathInfo() instead.*/
  void
  SetPathEndPoint(const PointType &)
  {
    itkWarningMacro("SetPathEndPoint() is not valid for this filter. Use AddPathInfo() instead.");
  }

  /** Override superclass behaviour.
   *  Warning: AddPathEndPoint() is not valid for this filter.
   *  This method is provided by the superclass, however it is not
   *  used by this subclass. Use AddPathInfo() instead.*/
  void
  AddPathEndPoint(const PointType &)
  {
    itkWarningMacro("AddPathEndPoint() is not valid for this filter. Use AddPathInfo() instead.");
  }

  /** Override superclass behaviour.
   *  Warning: ClearPathEndPoints() is not valid for this filter.
   *  This method is provided by the superclass, however it is not
   *  used by this subclass. Use ClearPathInfo() instead.*/
  void
  ClearPathEndPoints()
  {
    itkWarningMacro("ClearPathEndPoints() is not valid for this filter. Use ClearPathInfo() instead.");
  }

  /** PathInfo class for encapsulating information about a path.
   *  The points are stored as follows: end, start, way0, way1, way2, ..., wayN.
   *  Fronts are propagated in reverse order: wayN, ..., way2, way1, way0, start.
   *  (NOTE: a front is never propagated from end). */
  class PathInfo
  {
  public:
    PathInfo() { this->ClearInfo(); }
    void
    ClearInfo()
    {
      m_Info.clear();
      m_Info.resize(2);
      m_Front = 1;
    }
    void
    SetStartPoint(const PointType & start)
    {
      m_Info[1] = start;
    }
    void
    SetEndPoint(const PointType & end)
    {
      m_Info[0] = end;
    }
    void
    AddWayPoint(const PointType & way)
    {
      m_Info.push_back(way);
      m_Front++;
    }
    unsigned int
    GetNumberOfPoints() const
    {
      return m_Info.size();
    }
    const PointType &
    GetStartPoint() const
    {
      return m_Info[1];
    }
    const PointType &
    GetEndPoint() const
    {
      return m_Info[0];
    }
    const PointType &
    GetWayPoint(unsigned int i) const
    {
      return m_Info[2 + i];
    }
    bool
    HasNextFront() const
    {
      return m_Front >= 1;
    }
    const PointType &
    GetCurrentFrontAndAdvance()
    {
      return m_Info[m_Front--];
    }
    const PointType &
    PeekCurrentFront()
    {
      return m_Info[m_Front];
    }
    const PointType &
    PeekNextFront()
    {
      if (m_Front <= 1)
        return m_Info[1];
      else
        return m_Info[m_Front - 1];
    }
    const PointType &
    PeekPreviousFront()
    {
      if (m_Front == m_Info.size() - 1)
        return m_Info[0];
      else
        return m_Info[m_Front + 1];
    }

  private:
    std::vector<PointType> m_Info;
    unsigned int           m_Front;
  };

  /** Add a path information object to process.
   *  At least one PathInfo object must be added before processing. */
  void
  AddPathInfo(const PathInfo & info)
  {
    m_Info.push_back(info);
  }

  /** Clear the list of path information objects. */
  void
  ClearPathInfo()
  {
    m_Info.clear();
  }

  /** Handle optimizer iteration events. */
  virtual void
  Execute(const itk::Object * object, const itk::EventObject & event);

protected:
  SpeedFunctionToPathFilter();
  ~SpeedFunctionToPathFilter();
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const;

  /** Implemention of algorithm. */
  void
  GenerateData(void);

  /** Get the number of paths which the user has instructed to extracted. */
  virtual unsigned int
  GetNumberOfPathsToExtract() const;

  /** Compute the arrival function from which to extract the path. */
  virtual InputImageType *
  ComputeArrivalFunction();

  /** Override handling of optimizer iteration events to accomodate way points. */
  virtual const PointType &
  GetNextEndPoint();

  std::vector<PathInfo> m_Info;
  InputImagePointer     m_CurrentArrivalFunction;

private:
  SpeedFunctionToPathFilter(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSpeedFunctionToPathFilter.hxx"
#endif

#endif
