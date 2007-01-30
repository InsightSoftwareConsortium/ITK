
/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSphereSpatialFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSphereSpatialFunction_h
#define __itkSphereSpatialFunction_h

#include "itkInteriorExteriorSpatialFunction.h"

namespace itk
{

/** \class SphereSpatialFunction
 * \brief Spatial function implementation of a sphere
 *
 * Implements a function that returns 0 for points inside or on the surface
 * of a sphere, 1 for points outside the sphere
 * 
 * \ingroup SpatialFunctions
 */
template <unsigned int VImageDimension=3,typename TInput=Point<double,VImageDimension> >
class ITK_EXPORT SphereSpatialFunction
: public InteriorExteriorSpatialFunction<VImageDimension,TInput>
{
public:
  /** Standard class typedefs. */
  typedef SphereSpatialFunction<VImageDimension,TInput> Self;
  typedef InteriorExteriorSpatialFunction<VImageDimension,TInput> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
    
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SphereSpatialFunction,InteriorExteriorSpatialFunction);

  /** Input type for the function. */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function. */
  typedef typename Superclass::OutputType OutputType;

  /** Evaluates the function at a given position */
  OutputType Evaluate(const InputType& position) const;

  /** Get and set the center of the sphere. */
  itkGetMacro( Center, InputType);
  itkSetMacro( Center, InputType);
  
  /** Get and set the radius of the sphere */
  itkGetMacro( Radius, double);
  itkSetMacro( Radius, double);
       
protected:
  SphereSpatialFunction();
  virtual ~SphereSpatialFunction();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  SphereSpatialFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** The center of the sphere (of the same type as Input). */
  InputType m_Center;

  /** The radius of the sphere. */
  double m_Radius;

};

} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_SphereSpatialFunction(_, EXPORT, x, y) namespace itk { \
  _(2(class EXPORT SphereSpatialFunction< ITK_TEMPLATE_2 x >)) \
  namespace Templates { typedef SphereSpatialFunction< ITK_TEMPLATE_2 x > \
                                           SphereSpatialFunction##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkSphereSpatialFunction+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkSphereSpatialFunction.txx"
#endif

#endif
