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

#include "itkObject.h"
#include "VNL/vnl_vector_fixed.h"

namespace itk
{

/**
 * \class SpatialFunction
 * \brief N-dimensional spatial function class
 *
 * SpatialFunction is based on its implicit VTK counterpart. It provides
 * the ability to evaluate a function and its gradient at an arbitrary
 * point in space, defined by an n-dimensional VNL vector of doubles. Spatial
 * functions are defined at all points in n-dimensional space; evaluation
 * of spatial functions occurs irrespective of images, meshes, or other
 * data storage types.
 *
 * Functions are scalar valued, gradients are vector valued, with the
 * same number of dimensions as the function itself. All vector and
 * scalar values are doubles.
 *
 * This class was designed with the creation of test/validation images in mind
 * (in conjunction with itkSpatialFunctionImageEvaluatorFilter)
 * */

template <unsigned int VImageDimension=3>
class ITK_EXPORT SpatialFunction : public Object
{
  public:

  /**
   * Vector typedef - use this type to get the gradient info from the
   * function, and to pass an evaluation location to the function
   */
  typedef vnl_vector_fixed<double, VImageDimension> TVectorType;

  /**
   * Standard "Self" typedef.
   */
  typedef SpatialFunction Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Object Superclass;
  
  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** 
   * Run-time type information (and related methods).
   */
  TypeMacro(SpatialFunction,Object);

  /**
   * Method for creation through the object factory.
   */
  NewMacro(Self);

  /**
  * Evaluate the function at a given position. Remember, position is
  * represented by a TVectorType object, which uses VNL vector syntax
  */
  virtual double EvaluateFunctionValue(TVectorType* position) = 0;

  /**
  * Evaluate the gradient at a given position
  */
  virtual void EvaluateFunctionGradient(TVectorType* position,
    TVectorType* gradient) = 0;

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
