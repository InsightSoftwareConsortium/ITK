/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConeSpatialFunction.h
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
#ifndef __itkConeSpatialFunction_h
#define __itkConeSpatialFunction_h

#include "VNL/vnl_vector_fixed.h"
#include "itkSpatialFunction.h"

namespace itk
{

/**
 * \class ConeSpatialFunction
 * \brief Spatial function implementation of a Cone
 *
 * Implements a function that returns < 0 for points inside a Cone, 0 for points
 * on the surface of a Cone, and > 0 for points outside a Cone.
 *
 * */

template <unsigned int VImageDimension=3>
class ITK_EXPORT ConeSpatialFunction : public SpatialFunction<VImageDimension>
{
public:

  /**
   * Standard "Self" typedef.
   */
  typedef ConeSpatialFunction Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef SpatialFunction<VImageDimension> Superclass;
  
  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  itkTypeMacro(ConeSpatialFunction,SpatialFunction);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Evaluates the function at a given position
   */
  double EvaluateFunctionValue(TVectorType* position);

  /**
   * Evaluates the gradient at a given position
   */

  void EvaluateFunctionGradient(TVectorType* position,
    TVectorType* gradient);

  /**
   * Get and set the angle of the Cone
   */
  itkGetMacro( Angle, double);
  itkSetMacro( Angle, double);
     
protected:

  ConeSpatialFunction();
  virtual ~ConeSpatialFunction();

  ConeSpatialFunction(const ConeSpatialFunction&) {};
  void operator=(const ConeSpatialFunction&) {};

private:

  /**
   * The radius of the Cone
   */
  double m_Angle;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConeSpatialFunction.txx"
#endif

#endif
