/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFiniteCylinderSpatialFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFiniteCylinderSpatialFunction_h
#define __itkFiniteCylinderSpatialFunction_h

#include "itkInteriorExteriorSpatialFunction.h"

namespace itk
{

/**
 * \class FiniteCylinderSpatialFunction
 * \brief Function implementation of an finite cylinder
 *
 * Implements a function that returns 1 for points inside or on the surface
 * of a cylinder and 0 for points outside the cylinder.
 *
 **/

template <unsigned int VDimension = 3,
          typename TInput = Point<double, VDimension> >
class ITK_EXPORT FiniteCylinderSpatialFunction
: public InteriorExteriorSpatialFunction<VDimension, TInput>
{
public:

  /** Standard class typedefs. */
  typedef FiniteCylinderSpatialFunction Self;
  typedef InteriorExteriorSpatialFunction<VDimension, TInput> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer; 
      
  /** Run-time type information (and related methods). */
  itkTypeMacro(FiniteCylinderSpatialFunction,InteriorExteriorSpatialFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Input type for the function */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function */
  typedef typename Superclass::OutputType OutputType;
   
  /** Set/Get and set the center of the cylinder. */
  itkGetMacro(Center, InputType);
  itkSetMacro(Center, InputType);
  
  /** Get and set the medial axis length of the cylinder. */
  itkGetMacro(AxisLength, double);
  itkSetMacro(AxisLength, double);

  /** Get and set the radius length of the cylinder. */
  itkGetMacro(Radius, double);
  itkSetMacro(Radius, double);
  
  /** Set the orientation vectors (must be orthogonal) of the ellipsoid axes.
   * Must be normalized!!!!! */
  itkGetMacro(Orientation, InputType);
  itkSetMacro(Orientation, InputType);

  /** Evaluates the function at a given position. */
  OutputType Evaluate(const InputType& position) const;
  
protected:

  FiniteCylinderSpatialFunction();
  virtual ~FiniteCylinderSpatialFunction();

  void PrintSelf(std::ostream& os, Indent indent) const;

private:

  FiniteCylinderSpatialFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** The center of the cylinder. */
  InputType m_Center;

  /** The medial axis length of the cylinder. */
  double m_AxisLength;

  /** The radius length of the cylinder. */
  double m_Radius;

  /** The orientation vectors (must be orthogonal) of the ellipsoid axes. */  
  InputType m_Orientation;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFiniteCylinderSpatialFunction.txx"
#endif

#endif
