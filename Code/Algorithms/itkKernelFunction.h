/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKernelFunction.h
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
#ifndef _itkKernelFunction_h
#define _itkKernelFunction_h

#include "vnl/vnl_math.h"

namespace itk
{

/**
 * \class KernelFunction
 * \brief Kernel used for kernel function/density estimation.
 * 
 * \ingroup Operators
 */
class ITK_EXPORT KernelFunction : public Object
{
public:  
  /**
   * Standard "Self" typedef.
   */
  typedef KernelFunction Self;

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
   * Evaluate the function.
   */
  virtual double Evaluate (const double u) = 0;

protected:  
  KernelFunction(){};  
  ~KernelFunction(){};

};

/**
 * \class GaussianKernelFunction
 * \brief Gaussian kernel used for kernel function/density estimation.
 *
 * \ingroup Operators
 *
 */
class ITK_EXPORT GaussianKernelFunction : public KernelFunction
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef GaussianKernelFunction Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef KernelFunction Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  

  /**
   * Evaluate the function.
   */
  inline double Evaluate (const double u)
  {
    return ( exp( -0.5 * vnl_math_sqr( u ) ) * m_Factor );
  }

protected:

  GaussianKernelFunction()
  { 
    m_Factor = 1.0 / vnl_math_sqrt( 2.0 * vnl_math::pi ); 
  };

  ~GaussianKernelFunction(){};

private:
  double m_Factor;

};

} // namespace itk

#endif
