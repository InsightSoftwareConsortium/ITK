
/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTorusInteriorExteriorSpatialFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTorusInteriorExteriorSpatialFunction_h
#define __itkTorusInteriorExteriorSpatialFunction_h

#include "vnl/vnl_vector.h"
#include "itkInteriorExteriorSpatialFunction.h"
#include "itkCovariantVector.h"

namespace itk
{

/**
 * \class TorusInteriorExteriorSpatialFunction
 * \brief Spatial function implementation of torus symmetric about the z-axis in 3D
 *
 * Handle with care! May behave in strange ways when used with dimensions other than 3
 *
 * \ingroup SpatialFunctions
 *
 * */

template <unsigned int VDimension=3, typename TInput=Point<double,3> >
class ITK_EXPORT TorusInteriorExteriorSpatialFunction:
    public InteriorExteriorSpatialFunction<VDimension, TInput>
{
public:

  /** Standard class typedefs. */
  typedef TorusInteriorExteriorSpatialFunction Self;
  typedef InteriorExteriorSpatialFunction<VDimension> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
    
  /** Run time information. */
  itkTypeMacro(TorusInteriorExteriorSpatialFunction,InteriorExteriorSpatialFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Input type for the function. */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function. */
  typedef typename Superclass::OutputType OutputType;
  
  /** Evaluates the function at a given position */
  OutputType Evaluate(const InputType& position) const;

  /** Set/Get the origin of the torus (the point from which the major
   * radius is measured). */
  itkGetMacro( Origin, InputType);
  itkSetMacro( Origin, InputType);
  
  /** Set/Get the major radius of the torus. */
  itkGetMacro( MajorRadius, double);
  itkSetMacro( MajorRadius, double);
  
  /** Set/Get the major radius of the torus. */
  itkGetMacro( MinorRadius, double);
  itkSetMacro( MinorRadius, double);
       
protected:
  TorusInteriorExteriorSpatialFunction();
  virtual ~TorusInteriorExteriorSpatialFunction();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  TorusInteriorExteriorSpatialFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** The origin of the torus (the point from which the major
   * radius is measured) */
  InputType m_Origin;

  /** The distance from the origin of the torus to the center of the "tube" */
  double m_MajorRadius;

  /** The diameter of the "tube" */
  double m_MinorRadius;

};

} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_TorusInteriorExteriorSpatialFunction(_, EXPORT, x, y) namespace itk { \
  _(2(class EXPORT TorusInteriorExteriorSpatialFunction< ITK_TEMPLATE_2 x >)) \
  namespace Templates { typedef TorusInteriorExteriorSpatialFunction< ITK_TEMPLATE_2 x > \
                                           TorusInteriorExteriorSpatialFunction##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkTorusInteriorExteriorSpatialFunction+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkTorusInteriorExteriorSpatialFunction.txx"
#endif

#endif
