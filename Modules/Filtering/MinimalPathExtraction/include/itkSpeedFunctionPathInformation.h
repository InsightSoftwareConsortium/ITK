/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkSpeedFunctionPathInformation.h,v $
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef itkSpeedFunctionPathInformation_h
#define itkSpeedFunctionPathInformation_h

#include "itkLightObject.h"
#include "itkObjectFactory.h"

#include <vector>

namespace itk
{

/** \class SpeedFunctionPathInformation
 * \brief  PathInfo class for encapsulating information about a path
 * for a SpeedFunctionToPathFilter Object.
 * The points are stored as follows: end, start, way0, way1, ..., wayN.
 * Each element of a path can be a set of points - e.g. "end" may be
 * derived from a segmentation mask. Alternatively start and end may
 * be the left and right sides of a 2d image, producing a minimal
 * path across the image.
 * Fronts are propagated in reverse order: wayN, ..., way1, way0, start.
 * (NOTE: a front is never propagated from end).
 *
 * The user must provide at least one PathInfo object using
 * SpeedFunctionToPathFilter::AddPathInfo().
 * If multiple PathInfo objects are added,
 * multiple paths are extracted and saved to separate filter outputs.
 *
 * \author Dan Mueller,
 * Queensland University of Technology, dan.muel[at]gmail.com
 *
 * \sa LightObject
 *
 * \ingroup MinimalPathExtraction
 */
template <typename TPoint>
class SpeedFunctionPathInformation : public LightObject
{
public:
  /** Standard class typedefs. */
  typedef SpeedFunctionPathInformation Self;
  typedef LightObject                  Superclass;
  typedef SmartPointer<Self>           Pointer;
  typedef SmartPointer<const Self>     ConstPointer;


  /** Run-time type information (and related methods). */
  itkTypeMacro(SpeedFunctionPathInformation, LightObject);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Some point typedefs. */
  typedef TPoint                 PointType;
  typedef std::vector<PointType> PointsContainerType;

  void
  ClearInfo();

  void
  SetStartPoint(const PointType & start);

  void
  SetEndPoint(const PointType & end);

  void
  AddWayPoint(const PointType & way);


  /** Methods for adding extended path components **/
  void
  SetStartPoint(const PointsContainerType & start);

  void
  SetEndPoint(const PointsContainerType & end);

  void
  AddWayPoint(const PointsContainerType & way);

  // methods for modifying path seeds - needed when using
  // an extended seed.
  void
  SetCurrent(const PointsContainerType & newcurrent);
  void
  SetPrevious(const PointsContainerType & newprevious);
  void
  SetNext(const PointsContainerType & newnext);

  void
  SetCurrent(const PointType & current);
  void
  SetPrevious(const PointType & newprevious);
  void
  SetNext(const PointType & newnext);

  void
  Advance();

  unsigned int
  GetNumberOfPoints() const;

  const PointsContainerType &
  GetStartPoint() const;

  const PointsContainerType &
  GetEndPoint() const;

  const PointsContainerType &
  GetWayPoint(SizeValueType i) const;

  bool
  HasNextFront() const;

  const PointsContainerType &
  GetCurrentFrontAndAdvance();

  const PointsContainerType &
  PeekCurrentFront() const;

  const PointsContainerType &
  PeekNextFront() const;

  const PointsContainerType &
  PeekPreviousFront() const;


protected:
  SpeedFunctionPathInformation();
  ~SpeedFunctionPathInformation() ITK_OVERRIDE;
  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  std::vector<PointsContainerType> m_Information;
  SizeValueType                    m_Front;


  PointsContainerType
  PtoPVec(const PointType & P)
  {
    PointsContainerType V(1);
    V[0] = P;
    return (V);
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SpeedFunctionPathInformation);
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSpeedFunctionPathInformation.hxx"
#endif

#endif
