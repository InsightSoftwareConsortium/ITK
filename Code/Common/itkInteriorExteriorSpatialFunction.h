/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInteriorExteriorSpatialFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkInteriorExteriorSpatialFunction_h
#define __itkInteriorExteriorSpatialFunction_h

#include "itkSpatialFunction.h"
#include "vnl/vnl_vector_fixed.h"

namespace itk
{

/**
 * \class InteriorExteriorSpatialFunction
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
 * */

template <unsigned int VImageDimension=3>
class ITK_EXPORT InteriorExteriorSpatialFunction : public
  SpatialFunction<bool, VImageDimension>
{
  public:

  /**
   * Standard "Self" typedef.
   */
  typedef InteriorExteriorSpatialFunction Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef SpatialFunction<bool, VImageDimension> Superclass;

  /**
   * Input type for the function
   */
  typedef typename Superclass::InputType InputType;

  /**
   * Output type for the function
   */
  typedef typename Superclass::OutputType OutputType;
  
  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(InteriorExteriorSpatialFunction, SpatialFunction);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
  * Evaluate the function at a given position.
  * A return of 1 means inside or on the surface of the function,
  * 0 means outside the function
  * The actual definition of inside/outside is left up to the subclass
  */
  virtual OutputType Evaluate( const InputType& input ) const = 0;

protected:
  InteriorExteriorSpatialFunction();
  virtual ~InteriorExteriorSpatialFunction();
  InteriorExteriorSpatialFunction(const InteriorExteriorSpatialFunction&) {};
  void operator=(const InteriorExteriorSpatialFunction&) {};
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkInteriorExteriorSpatialFunction.txx"
#endif

#endif
