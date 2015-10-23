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
  typedef TPoint PointType;

  void
  ClearInfo();

  void
  SetStartPoint(const PointType & start);

  void
  SetEndPoint(const PointType & end);

  void
  AddWayPoint(const PointType & way);

  unsigned int
  GetNumberOfPoints() const;

  const PointType &
  GetStartPoint() const;

  const PointType &
  GetEndPoint() const;

  const PointType &
  GetWayPoint(SizeValueType i) const;

  bool
  HasNextFront() const;

  const PointType &
  GetCurrentFrontAndAdvance();

  const PointType &
  PeekCurrentFront() const;

  const PointType &
  PeekNextFront() const;

  const PointType &
  PeekPreviousFront() const;


protected:
  SpeedFunctionPathInformation();
  virtual ~SpeedFunctionPathInformation();
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const;

  std::vector<PointType> m_Information;
  SizeValueType          m_Front;


private:
  SpeedFunctionPathInformation(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSpeedFunctionPathInformation.hxx"
#endif

#endif
