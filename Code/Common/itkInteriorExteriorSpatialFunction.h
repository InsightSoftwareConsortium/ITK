/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInteriorExteriorSpatialFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkInteriorExteriorSpatialFunction_h
#define __itkInteriorExteriorSpatialFunction_h

#include "itkSpatialFunction.h"

namespace itk
{

/** \class InteriorExteriorSpatialFunction
 * \brief Returns whether or not a location is "inside" or "outside" a function
 *
 * InteriorExteriorSpatialFunction is a specialized version of SpatialFunction
 * where the output type is a boolean. In particular, the return type is understood
 * to mean the following:
 *
 * A return of 1 means inside or on the surface of the function,
 * 0 means outside the function
 *
 * There is no implied meaning in the terms "inside" or "outside"; although
 * the standard assumption is that "inside" means "bounded by a closed surface",
 * alternative definitions are also fine. For example, inside might be one side
 * of a plane, outside the other side.
 *
 * A typical use for an InteriorExteriorSpatialFunction is to generate test
 * primitives of arbitrary dimensionality, in conjunction with
 * itk::SpatialFunctionImageEvaluatorFilter or itk::FloodFilledSpatialFunctionConditionalIterator
 *
 * \ingroup SpatialFunctions
 */

template <unsigned int VDimension=3,typename TInput=Point<double,VDimension> >
class ITK_EXPORT InteriorExteriorSpatialFunction : public
  SpatialFunction<bool, VDimension, TInput >
{
public:
  /** Standard class typedefs. */
  typedef InteriorExteriorSpatialFunction Self;
  typedef SpatialFunction<bool, VDimension,TInput> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(InteriorExteriorSpatialFunction, SpatialFunction);

  /** Input type for the function */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function */
  typedef typename Superclass::OutputType OutputType;
  
 /** Evaluate the function at a given position.
  * A return of 1 means inside or on the surface of the function,
  * 0 means outside the function
  * The actual definition of inside/outside is left up to the subclass */
  virtual OutputType Evaluate( const InputType& input ) const = 0;

protected:
  InteriorExteriorSpatialFunction();
  virtual ~InteriorExteriorSpatialFunction();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  InteriorExteriorSpatialFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};

} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_InteriorExteriorSpatialFunction(_, EXPORT, x, y) namespace itk { \
  _(2(class EXPORT InteriorExteriorSpatialFunction< ITK_TEMPLATE_2 x >)) \
  namespace Templates { typedef InteriorExteriorSpatialFunction< ITK_TEMPLATE_2 x > \
                                  InteriorExteriorSpatialFunction##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkInteriorExteriorSpatialFunction+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkInteriorExteriorSpatialFunction.txx"
#endif

#endif
