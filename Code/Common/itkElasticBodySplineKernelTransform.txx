/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkElasticBodySplineKernelTransform.txx
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
#include "itkElasticBodySplineKernelTransform.h"

namespace itk
{

template <class TScalarType, int NDimensions>
ElasticBodySplineKernelTransform<TScalarType, NDimensions>::
ElasticBodySplineKernelTransform() : KernelTransform<TScalarType, NDimensions>()
{
}

template <class TScalarType, int NDimensions>
ElasticBodySplineKernelTransform<TScalarType, NDimensions>::
~ElasticBodySplineKernelTransform()
{
}

template <class TScalarType, int NDimensions>
TScalarType
ElasticBodySplineKernelTransform<TScalarType, NDimensions>::GetAlpha() const
{
	return m_Alpha;
}

template <class TScalarType, int NDimensions>
void ElasticBodySplineKernelTransform<TScalarType, NDimensions>::
SetAlpha(const TScalarType newAlpha)
{
	m_Alpha = newAlpha;
}

template <class TScalarType, int NDimensions>
ElasticBodySplineKernelTransform<TScalarType, NDimensions>::GMatrixType
ElasticBodySplineKernelTransform<TScalarType, NDimensions>::ComputeG(VectorType& x) const
{
  RowMatrixType xRV; // row vector rep. of x
  ColumnMatrixType xCV; // column vector rep. of x
  TScalarType r; // Euclidean norm of x

  xRV.set_row(0, x.Get_vnl_vector());
  xCV = xRV.transpose();
  r = (xCV.get_column(0)).magnitude();
  return ((m_Alpha * ((TScalarType) pow(r, 2)) * m_I) - (xCV*xRV*3)) * r;
}

} // namespace itk
