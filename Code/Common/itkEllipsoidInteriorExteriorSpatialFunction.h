/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEllipsoidInteriorExteriorSpatialFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkEllipsoidInteriorExteriorSpatialFunction_h
#define __itkEllipsoidInteriorExteriorSpatialFunction_h

#include "itkInteriorExteriorSpatialFunction.h"

namespace itk
{

/**
 * \class EllipsoidSpatialFunction
 * \brief Function implementation of an ellipsoid
 *
 * Implements a function that returns 1 for points inside or on the surface
 * of a ellipsoid and 0 for points outside the ellipsoid. The orientation of the 
 * n-dimensional ellipsoid axes are defined by n orthogonal vectors.
 * See Examples/EllipsoidInteriorExteriorSpatialFunction/README for an example
 * of creating an Ellipsoid in an image.
 *
 **/
template <unsigned int VDimension = 3,
          typename TInput = Point<double, VDimension> >
class ITK_EXPORT EllipsoidInteriorExteriorSpatialFunction
: public InteriorExteriorSpatialFunction<VDimension, TInput>
{
public:
  /** Standard class typedefs. */
  typedef EllipsoidInteriorExteriorSpatialFunction Self;
  typedef InteriorExteriorSpatialFunction<VDimension, TInput> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer; 
      
  /** Run-time type information (and related methods). */
  itkTypeMacro(EllipsoidInteriorExteriorSpatialFunction,InteriorExteriorSpatialFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Input type for the function */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function */
  typedef typename Superclass::OutputType OutputType;
   
  /** Set/Get and set the center of the ellipsoid. */
  itkGetMacro(Center, InputType);
  itkSetMacro(Center, InputType);
  
  /** Get and set the axes lengths of the ellipsoid. */
  itkGetMacro(Axes, InputType);
  itkSetMacro(Axes, InputType);
  
  /** Set the orientation vectors (must be orthogonal) of the ellipsoid axes.
   * Must be normalized!!!!! */
  void SetOrientations(vnl_matrix<double>);

  /** Evaluates the function at a given position. */
  OutputType Evaluate(const InputType& position) const;
     
protected:
  EllipsoidInteriorExteriorSpatialFunction();
  virtual ~EllipsoidInteriorExteriorSpatialFunction();

  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  EllipsoidInteriorExteriorSpatialFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** The center of the ellipsoid. */
  InputType m_Center;

  /** The axes lenths of the ellipsoid. */
  InputType m_Axes;

  /** The orientation vectors (must be orthogonal) of the ellipsoid axes. */  
  double ** m_Orientations;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEllipsoidInteriorExteriorSpatialFunction.txx"
#endif

#endif
