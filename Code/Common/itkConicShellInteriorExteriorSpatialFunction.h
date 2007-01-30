/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConicShellInteriorExteriorSpatialFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConicShellInteriorExteriorSpatialFunction_h
#define __itkConicShellInteriorExteriorSpatialFunction_h

#include "vnl/vnl_vector.h"
#include "itkInteriorExteriorSpatialFunction.h"
#include "itkCovariantVector.h"

namespace itk
{

/**
 * \class ConicShellInteriorExteriorSpatialFunction
 * \brief Spatial function implementation of a conic shell
 *
 * We are creating search areas from BoundaryPoint1 in which to look for 
 * candidate BoundaryPoint2's with which to form core atoms.  Assume the 
 * "worst case" that BoundaryPoint2 is somewhere in that search area pointing
 * directly at BoundaryPoint1. 
 *
 * The search area (ConicShell?) from each BoundaryPoint1 has the following 
 * parameters: 
 *
 * DistanceMax and DistanceMin from the location of the BoundaryPoint 
 *
 * AngleMax from the line along the gradient at the boundary point.
 * This is determined in n dimensions by taking the dot product of two vectors,
 * (1) the normalized gradient at BoundaryPoint1 and
 * (2) the normalized vector from BoundaryPoint1 to BoundaryPoint2.
 *
 * If the absolute value of that dot product is greater than (1 - epsilon)
 * then you are in the ConicShell.  This epsilon is the same one determining
 * face-to-faceness in the IEEE TMI paper. 
 *
 * Polarity, i.e. which direction along the gradient of BoundaryPoint1 
 * you want to look.
 * 
 * \ingroup SpatialFunctions
 *
 * */

template <unsigned int VDimension=3, typename TInput=Point<double,3> >
class ITK_EXPORT ConicShellInteriorExteriorSpatialFunction:
    public InteriorExteriorSpatialFunction<VDimension, TInput>
{
public:

  /** Standard class typedefs. */
  typedef ConicShellInteriorExteriorSpatialFunction   Self;
  typedef InteriorExteriorSpatialFunction<VDimension> Superclass;
  typedef SmartPointer<Self>                          Pointer;
  typedef SmartPointer<const Self>                    ConstPointer;
    
  /** Run time information. */
  itkTypeMacro(ConicShellInteriorExteriorSpatialFunction,
               InteriorExteriorSpatialFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Input type for the function. */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function. */
  typedef typename Superclass::OutputType OutputType;

  /** The type of vector used to store the gradient info. */
  typedef CovariantVector<double, VDimension> GradientType;
  
  /** Evaluates the function at a given position */
  OutputType Evaluate(const InputType& position) const;

  /** Set/Get the origin of the function. */
  itkGetMacro( Origin, InputType);
  itkSetMacro( Origin, InputType);
  
  /** Set/Get the gradient at the origin of the function. */
  GradientType GetOriginGradient() {return m_OriginGradient;}
  void SetOriginGradient(GradientType grad);
  
  /** Set/Get the minimum search distance. */
  itkGetMacro( DistanceMin, double);
  itkSetMacro( DistanceMin, double);
  
  /** Set/Get the maximum search distance. */
  itkGetMacro( DistanceMax, double);
  itkSetMacro( DistanceMax, double);
  
  /** Set/Get the tolerance of the in/out comparison. */
  itkGetMacro( Epsilon, double);
  itkSetMacro( Epsilon, double);
  
  /** Set/Get direction along the gradient to search. */
  itkGetMacro( Polarity, bool);
  itkSetMacro( Polarity, bool);
       
protected:
  ConicShellInteriorExteriorSpatialFunction();
  virtual ~ConicShellInteriorExteriorSpatialFunction();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  ConicShellInteriorExteriorSpatialFunction(const Self&); //not implemented
  void operator=(const Self&); //purposely not implemented

  /** The origin of the conic shell */
  InputType m_Origin;

  /** The gradient at the origin */
  GradientType m_OriginGradient;

  double m_DistanceMin;
  double m_DistanceMax;
  double m_Epsilon;
  bool   m_Polarity;

};

} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_ConicShellInteriorExteriorSpatialFunction(_, EXPORT, x, y) namespace itk { \
  _(2(class EXPORT ConicShellInteriorExteriorSpatialFunction< ITK_TEMPLATE_2 x >)) \
  namespace Templates { typedef ConicShellInteriorExteriorSpatialFunction< ITK_TEMPLATE_2 x > \
                                                  ConicShellInteriorExteriorSpatialFunction##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkConicShellInteriorExteriorSpatialFunction+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkConicShellInteriorExteriorSpatialFunction.txx"
#endif

#endif
