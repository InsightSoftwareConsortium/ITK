/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFrustumSpatialFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFrustumSpatialFunction_h
#define __itkFrustumSpatialFunction_h

#include "itkInteriorExteriorSpatialFunction.h"

namespace itk
{

/**
 * \class FrustumSpatialFunction
 * \brief Spatial function implementation of a truncated pyramid.
 *
 * Implements a function that returns 0 for points inside or on the surface
 * of a truncated pyrami, 1 for points outside the truncated pyramid
 * 
 * \ingroup SpatialFunctions
 *
 * */

template <unsigned int VImageDimension=3,typename TInput=Point<double,3> >
class ITK_EXPORT FrustumSpatialFunction : 
            public InteriorExteriorSpatialFunction<VImageDimension,TInput>
{
public:

  /** Standard class typedefs. */
  typedef FrustumSpatialFunction<VImageDimension,TInput> Self;
  typedef InteriorExteriorSpatialFunction<VImageDimension,TInput> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
    
  /** Run-time type information (and related methods). */
  itkTypeMacro(FrustumSpatialFunction,InteriorExteriorSpatialFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Input type for the function */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function */
  typedef typename Superclass::OutputType OutputType;
  
  /** Rotate the frustum in the XZ or the YZ plane */
  typedef enum{ 
    RotateInXZPlane=1,
    RotateInYZPlane
  } FrustumRotationPlaneType;
 
  /** Evaluates the function at a given position */
  OutputType Evaluate(const InputType& position) const;

  /** Get and set the center of the sphere */
  itkGetMacro( Apex, InputType);
  itkSetMacro( Apex, InputType);
  
  /** Get and set the angle of the pyramid axis 
   * with respect to the Z axis */
  itkGetMacro( AngleZ, double);
  itkSetMacro( AngleZ, double);
       
  /** Get and set the aperture angle in X */
  itkGetMacro( ApertureAngleX, double);
  itkSetMacro( ApertureAngleX, double);
       
  /** Get and set the aperture angle in Y */
  itkGetMacro( ApertureAngleY, double);
  itkSetMacro( ApertureAngleY, double);
       
  /** Get and set the top plane distance to the Apex */
  itkGetMacro( TopPlane, double);
  itkSetMacro( TopPlane, double);
       
  /** Get and set the bottom plane distance to the Apex */
  itkGetMacro( BottomPlane, double);
  itkSetMacro( BottomPlane, double);
  
 /** Set macro to set the plane in which the frustum should rotate */
  itkSetMacro( RotationPlane, FrustumRotationPlaneType );
     
protected:
  FrustumSpatialFunction();
  virtual ~FrustumSpatialFunction();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  FrustumSpatialFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** The apex of the pyramid (of the same type as Input) */
  InputType m_Apex;

  /** Angle between the pyramid axis and the Z axis */
  double m_AngleZ;

  /** Aperture Angle in X direction */
  double m_ApertureAngleX;

  /** Aperture Angle in Y direction */
  double m_ApertureAngleY;

  /** Distance from Apex to top plane */
  double m_TopPlane;

  /** Distance from Apex to bottom plane */
  double m_BottomPlane;

  /** Plane in which to the frustum is being rotated */
  FrustumRotationPlaneType m_RotationPlane;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFrustumSpatialFunction.txx"
#endif

#endif




