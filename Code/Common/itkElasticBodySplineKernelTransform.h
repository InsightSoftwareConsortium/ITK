/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkElasticBodySplineKernelTransform.h
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
#ifndef __itkElasticBodySplineKernelTransform_h
#define __itkElasticBodySplineKernelTransform_h

#include "itkKernelTransform.h"

namespace itk
{

/** \class ElasticBodySplineKernelTransform
 * This class defines the elastic body spline (EBS) transformation.
 * It is implemented in as straightforward a manner as possible from
 * the IEEE TMI paper by Davis, Khotanzad, Flamig, and Harms,
 * Vol. 16 No. 3 June 1997
 * Taken from the paper:
 * The EBS "is based on a physical model of a homogeneous, isotropic,
 * three-dimensional elastic body. The model can approximate the way
 * that some physical objects deform".
 */
template <class TScalarType,         // Data type for scalars (float or double)
          int NDimensions = 3>          // Number of dimensions
class ElasticBodySplineKernelTransform : public KernelTransform<TScalarType, NDimensions>
{
public:
  /**
   * Standard Self typedef
   */
  typedef ElasticBodySplineKernelTransform<TScalarType, NDimensions> Self;
  /**
   * Standard Superclass typedef
   */
  typedef KernelTransform<TScalarType, NDimensions> Superclass;
  /**
   * Set alpha
   */
	void SetAlpha(const TScalarType newAlpha);
  /**
   * Get alpha
   */
  TScalarType GetAlpha() const;
  /**
   * These (rather redundant) typedefs are needed because on SGI, typedefs
   * are not inherited
   */
  typedef typename Superclass::VectorType VectorType;
  /**
   * Default constructor
   */
  ElasticBodySplineKernelTransform();
  /**
   * Destructor
   */
  virtual ~ElasticBodySplineKernelTransform();

protected:
  /**
   * These (rather redundant) typedefs are needed because on SGI, typedefs
   * are not inherited
   */
  typedef typename Superclass::GMatrixType GMatrixType;

  /**
   * Compute G(x)
   * For the elastic body spline, this is:
   * G(x) = [alpha*r(x)^2*I - 3*x*x']*r(x)
   * where
   * alpha is a constant
   * r(x) = Euclidean norm = sqrt[x1^2 + x2^2 + x3^2]
   * I = identity matrix
   */
  GMatrixType ComputeG(VectorType& x) const;
  /**
   * alpha, Poisson's ratio
   */
  TScalarType m_Alpha;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkElasticBodySplineKernelTransform.txx"
#endif

#endif // __itkElasticBodySplineKernelTransform_h
