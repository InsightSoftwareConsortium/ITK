/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSphereSignedDistanceFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSphereSignedDistanceFunction_h
#define _itkSphereSignedDistanceFunction_h

#include "itkShapeSignedDistanceFunction.h"
#include "itkVector.h"

namespace itk
{

/** \class SphereSignedDistanceFunction
 * \brief Compute the signed distance from a N-dimensional sphere
 *
 * This class is templated over the coordinate representation type 
 * (e.g. float or double) and the space dimension.
 *
 * \sa ShapeSignedDistanceFunction
 * \ingroup ImageFunctions
 * 
 * */
template <typename TCoordRep, unsigned int VSpaceDimension>
class ITK_EXPORT SphereSignedDistanceFunction : 
  public ShapeSignedDistanceFunction< TCoordRep, VSpaceDimension >
{
public:
  /** Standard class typedefs. */
  typedef SphereSignedDistanceFunction Self;
  typedef ShapeSignedDistanceFunction< TCoordRep, VSpaceDimension > Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(SphereSignedDistanceFunction, ShapeSignedDistancFunction);

  /** New macro for creation of through the object factory.*/
  itkNewMacro( Self );

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** InputeType typedef support. */
  typedef typename Superclass::InputType InputType;
  
  /** Dimension underlying input image. */
  itkStaticConstMacro(SpaceDimension, unsigned int, Superclass::SpaceDimension);

  /** CoordRep typedef support. */
  typedef typename Superclass::CoordRepType CoordRepType;

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Type of the shape parameters. */
  typedef typename Superclass::ParametersType ParametersType;

  /** A sphere is defined by a set of shape parameters. The first parameter 
   * is the radius and the next SpaceDimension parameters represent the center. */
  virtual void SetParameters( const ParametersType & );
  virtual const ParametersType& GetParameters(void) const;
  virtual unsigned int GetNumberOfShapeParameters(void) const
    { return 1; }
  virtual unsigned int GetNumberOfPoseParameters(void) const
    { return SpaceDimension; }

  /** Evaluate the signed distance from a shape at a given position. */
  virtual OutputType Evaluate( const PointType& point ) const;

protected:
  SphereSignedDistanceFunction();
  ~SphereSignedDistanceFunction(){};

  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  SphereSignedDistanceFunction( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

  typedef Vector<CoordRepType,SpaceDimension> VectorType;

  VectorType   m_Translation;
  double       m_Radius;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSphereSignedDistanceFunction.txx"
#endif

#endif
