/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapeSignedDistanceFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkShapeSignedDistanceFunction_h
#define _itkShapeSignedDistanceFunction_h

#include "itkSpatialFunction.h"
#include "itkArray.h"

namespace itk
{

/** \class ShapeSignedDistanceFunction
 * \brief Base class for functions which evaluates the signed distance
 * from a shape.
 *
 * ShapeSignedDistanceFunction is the base class for functions which
 * returns the signed distance from a shape at an arbitrary point.
 * A shape assumed to be defined by a set of shape and pose parameters.
 *
 * Note that Initialize() must be called before use.
 * This allows the class an opportunity to validate any inputs.
 *
 * This class is templated over the coordinate representation type 
 * (e.g. float or double) and the space dimension.
 * 
 * ShapeSignedDistanceFunction is used to encapsulate the shape prior
 * in ShapePriorSegmentationLevelSetFunctions.
 *
 * \sa SpatialFunction
 * \sa ShapePriorSegmentationLevelSetFunction
 *
 * \ingroup ImageFunctions
 * 
 * */
template <typename TCoordRep, unsigned int VSpaceDimension>
class ITK_EXPORT ShapeSignedDistanceFunction : 
  public SpatialFunction< double, VSpaceDimension, Point<TCoordRep,VSpaceDimension> >
{
public:
  /** Standard class typedefs. */
  typedef ShapeSignedDistanceFunction Self;
  typedef SpatialFunction< double, VSpaceDimension, 
                                   Point<TCoordRep,VSpaceDimension> > Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ShapeSignedDistanceFunction, SpatialFunction);

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** InputType typedef support. */
  typedef typename Superclass::InputType InputType;
  
  /** Dimension underlying input image. */
  itkStaticConstMacro(SpaceDimension, unsigned int, VSpaceDimension);

  /** CoordRep typedef support. */
  typedef TCoordRep CoordRepType;

  /** Point typedef support. */
  typedef InputType PointType;

  /** Type of the shape parameters. */
  typedef Array<double>      ParametersType;

  /** A shape is defined by a set of shape parameters. */
  virtual void SetParameters( const ParametersType & ) = 0;
  virtual const ParametersType& GetParameters(void) const = 0;
  virtual unsigned int GetNumberOfShapeParameters(void) const = 0;
  virtual unsigned int GetNumberOfPoseParameters(void) const = 0;
  virtual unsigned int GetNumberOfParameters(void) const
    { return this->GetNumberOfShapeParameters() + this->GetNumberOfPoseParameters(); }

  /** Evaluate the signed distance from a shape at a given position. */
  virtual OutputType Evaluate( const PointType& point ) const = 0;

  /** Initialize must be called before the first call of SetParameters() or 
   Evaluate() to allow the class to validate any inputs. */
  virtual void Initialize() throw ( ExceptionObject ) {};

protected:

  ShapeSignedDistanceFunction()
    {
    // memory initialization
    m_Parameters = ParametersType(1);
    m_Parameters.Fill( 0.0 );
    };

  ~ShapeSignedDistanceFunction(){};

  void PrintSelf(std::ostream& os, Indent indent) const
    { 
    Superclass::PrintSelf( os, indent );
    os << indent << "Parameters: " << m_Parameters << std::endl;
    }

  ParametersType  m_Parameters;

private:
  ShapeSignedDistanceFunction( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

};

} // namespace itk

#endif
