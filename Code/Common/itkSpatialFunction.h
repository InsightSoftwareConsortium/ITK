/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialFunction.h
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
#ifndef __itkSpatialFunction_h
#define __itkSpatialFunction_h

#include "itkFunctionBase.h"
#include "itkPoint.h"

namespace itk
{

/**
 * \class SpatialFunction
 * \brief N-dimensional spatial function class
 *
 * itk::SpatialFunction provides the ability to define functions that can
 * be evluated at an arbitrary point in space (physical or otherwise). The return
 * type is specified by the derived class, and the input to the function
 * is an n-dimensional itk::Point.
 *
 * Although itk::ImageFunction and itk::SpatialFunction are quite similar,
 * itk::SpatialFunction derived classes exist without reference to an Image
 * type.
 *
 * SpatialFunction is templated over output type (the data type
 * returned by an evaluate() call) and dimensionality.
 *
 * \ingroup SpatialFunctions
 */

template <typename TOutput, unsigned int VImageDimension=3>
class ITK_EXPORT SpatialFunction : public FunctionBase<Point<double, VImageDimension>, TOutput>
{
  public:

  /**
   * Standard "Self" typedef.
   */
  typedef SpatialFunction Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef FunctionBase<Point<double, VImageDimension>, TOutput> Superclass;

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
  itkTypeMacro(SpatialFunction, FunctionBase);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
  * Evaluate the function at a given position. Remember, position is
  * represented by an n-d itk::Point object with data type double
  */
  virtual OutputType Evaluate( const InputType& input ) const = 0;

protected:
  SpatialFunction();
  virtual ~SpatialFunction();
  SpatialFunction(const SpatialFunction&) {};
  void operator=(const SpatialFunction&) {};
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialFunction.txx"
#endif

#endif
