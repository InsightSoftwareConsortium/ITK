/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKernelTransform.txx
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
#include "itkKernelTransform.h"

namespace itk
{

/**
 *
 */
template <class TScalarType, int NDimensions>
KernelTransform<TScalarType, NDimensions>::IMatrixType
KernelTransform<TScalarType, NDimensions>::m_I;

/**
 *
 */
template <class TScalarType, int NDimensions>
KernelTransform<TScalarType, NDimensions>::KernelTransform() : 
Transform<TScalarType, NDimensions>()
{
  static bool IMatrixInitialized = false;

  if (!IMatrixInitialized)
  {
    m_I.set_identity();
    IMatrixInitialized = true;
  }
  m_p = PointSetType::New();
  m_q = PointSetType::New();
  m_d = new VectorListType;
  m_LMatrix = NULL;
  m_KMatrix = NULL;
  m_PMatrix = NULL;
  m_YMatrix = NULL;
  m_WMatrix = NULL;
  m_WMatrixComputed = false;
}

/**
 *
 */
template <class TScalarType, int NDimensions>
KernelTransform<TScalarType, NDimensions>::~KernelTransform()
{
  if (m_d != NULL)
    {
    delete m_d;
    }
  if (m_LMatrix != NULL)
    {
    delete m_LMatrix;
    }
  if (m_KMatrix != NULL)
    {
    delete m_KMatrix;
    }
  if (m_PMatrix != NULL)
    {
    delete m_PMatrix;
    }
  if (m_YMatrix != NULL)
    {
    delete m_YMatrix;
    }
  if (m_WMatrix != NULL)
    {
    delete m_WMatrix;
    }
}

/**
 *
 */
template <class TScalarType, int NDimensions>
KernelTransform<TScalarType, NDimensions>::PointSetPointer
KernelTransform<TScalarType, NDimensions>::Getp()
{
  return m_p;
}

/**
 *
 */
template <class TScalarType, int NDimensions>
KernelTransform<TScalarType, NDimensions>::PointSetPointer
KernelTransform<TScalarType, NDimensions>::Getq()
{
  return m_q;
}

/**
 *
 */
template <class TScalarType, int NDimensions>
void 
KernelTransform<TScalarType, NDimensions>::Setp(const PointSetPointer p)
{
  m_p = p;
}

/**
 *
 */
template <class TScalarType, int NDimensions>
void 
KernelTransform<TScalarType, NDimensions>::Setq(const PointSetPointer q)
{
  m_q = q;
}

/**
 *
 */
template <class TScalarType, int NDimensions>
KernelTransform<TScalarType, NDimensions>::VectorListType*
KernelTransform<TScalarType, NDimensions>::Getd()
{
  return m_d;
}

/**
 *
 */
template <class TScalarType, int NDimensions>
void KernelTransform<TScalarType, NDimensions>::ComputeD()
{
  int numLandmarks = m_p->GetNumberOfPoints();
  int i;
  PointType p;
  PointType q;
  VectorType* d;

  m_d->clear();
  for (i = 0; i < numLandmarks; i++) 
    {
    m_p->GetPoint(i, &p);
    m_q->GetPoint(i, &q);
    d = new VectorType(q - p);
    m_d->push_back(d);
    }
}

/**
 *
 */
template <class TScalarType, int NDimensions>
void KernelTransform<TScalarType, NDimensions>::ComputeW()
{
        int numLandmarks = m_p->GetNumberOfPoints();
  vnl_svd<TScalarType>* svd;

  ComputeL();
  ComputeY();
  if (m_WMatrix != NULL)
    {
    delete m_WMatrix;
    }
  m_WMatrix = new WMatrixType(NDimensions*(numLandmarks+NDimensions+1), 1);
  svd = new vnl_svd<TScalarType>(*m_LMatrix, 1e-8);
  *m_WMatrix = svd->solve(*m_YMatrix);
}

/**
 *
 */
template <class TScalarType, int NDimensions>
void KernelTransform<TScalarType, NDimensions>::ComputeL()
{
  int numLandmarks = m_p->GetNumberOfPoints();
  vnl_matrix<TScalarType> O2(NDimensions*(NDimensions+1),
                             NDimensions*(NDimensions+1), 0);

  ComputeP();
  ComputeK();
  if (m_LMatrix != NULL)
    {
    delete m_LMatrix;
    }
  m_LMatrix = new LMatrixType(NDimensions*(numLandmarks+NDimensions+1),
                              NDimensions*(numLandmarks+NDimensions+1));
  m_LMatrix->update(*m_KMatrix, 0, 0);
  m_LMatrix->update(*m_PMatrix, 0, m_KMatrix->columns());
  m_LMatrix->update(m_PMatrix->transpose(), m_KMatrix->rows(), 0);
  m_LMatrix->update(O2, m_KMatrix->rows(), m_KMatrix->columns());
}

/**
 *
 */
template <class TScalarType, int NDimensions>
void KernelTransform<TScalarType, NDimensions>::ComputeK()
{
  int numLandmarks = m_p->GetNumberOfPoints();
  GMatrixType G;
  VectorType s;
  int i, j;
  PointType p1;
  PointType p2;

  ComputeD();
  if (m_KMatrix != NULL)
    {
    delete m_KMatrix;
    }
  m_KMatrix = new KMatrixType(NDimensions*numLandmarks,
                              NDimensions*numLandmarks);
  for (i = 0; i < numLandmarks; i++)
    {
    for (j = 0; j < numLandmarks; j++)
      {
      m_p->GetPoint(i, &p1);
      m_p->GetPoint(j, &p2);
      s = p1 - p2;
      G = ComputeG(s);
      m_KMatrix->update(G, i*NDimensions, j*NDimensions);
    }
  }
}

/**
 *
 */
template <class TScalarType, int NDimensions>
void KernelTransform<TScalarType, NDimensions>::ComputeP()
{
  int numLandmarks = m_p->GetNumberOfPoints();
  IMatrixType I;
  IMatrixType temp;
  int i, j;
  PointType p;

  I.set_identity();
  if (m_PMatrix != NULL)
    {
    delete m_PMatrix;
    }
  m_PMatrix = new PMatrixType(NDimensions*numLandmarks,
                              NDimensions*(NDimensions+1));
  for (i = 0; i < numLandmarks; i++)
    {
    m_p->GetPoint(i, &p);
    for (j = 0; j < NDimensions; j++)
      {
      temp = I * p.Get_vnl_vector()[j];
      m_PMatrix->update(temp, i*NDimensions, j*NDimensions);
      }
    m_PMatrix->update(I, i*NDimensions, j*NDimensions);
  }
}

/**
 *
 */
template <class TScalarType, int NDimensions>
void KernelTransform<TScalarType, NDimensions>::ComputeY()
{
  int i, j;
  int numLandmarks = m_p->GetNumberOfPoints();

  if (m_YMatrix != NULL)
    {
    delete m_YMatrix;
    }

  m_YMatrix = new YMatrixType(NDimensions*(numLandmarks+NDimensions+1), 1);
  for (i = 0; i < numLandmarks; i++)
    {
    for (j = 0; j < NDimensions; j++)
      {
      m_YMatrix->put(i*NDimensions+j, 0, (*m_d)[i]->Get_vnl_vector()[j]);
      }
    }
  for (i = 0; i < NDimensions*(NDimensions+1); i++) 
    {
    m_YMatrix->put(numLandmarks*NDimensions+i, 0, 0);
    }
}

/**
 *
 */
template <class TScalarType, int NDimensions>
KernelTransform<TScalarType, NDimensions>::PointType
KernelTransform<TScalarType, NDimensions>
::TransformPoint(const PointType& thisPoint) const
{
  int numLandmarks = m_p->GetNumberOfPoints();
  int i, j;
  ColumnMatrixType b, c, d, Ax;
  vnl_matrix_fixed<TScalarType, NDimensions, NDimensions> A;
  PointType result;
  VectorType argumentG;
  PointType p;

  d = d*0;
  for (i = 0; i < numLandmarks; i++)
    {
    c.update(m_WMatrix->extract(NDimensions, 1, i*NDimensions, 0), 0, 0);
    m_p->GetPoint(i, &p);
    argumentG = thisPoint - p;
    d = d + ComputeG(argumentG) * c;
    }
  for (i = 0; i < NDimensions; i++)
    {
    A.update(m_WMatrix->extract(NDimensions, 1,
                                (numLandmarks+i)*NDimensions, 0), 0, i);
    }
  b.update(m_WMatrix->extract(NDimensions, 1,
                              (numLandmarks+NDimensions)*NDimensions, 0),
           0, 0);
  for (j = 0; j < NDimensions; j++)
    {
    Ax.put(j, 0, thisPoint[j]);
    }
  Ax = A*Ax;
  d = d + Ax + b;
  for (j = 0; j < NDimensions; j++)
    {
    result[j] = thisPoint[j] + d.get(j, 0);
    }
  return result;
}

/**
 *
 */
template <class TScalarType, int NDimensions>
KernelTransform<TScalarType, NDimensions>::VectorType
KernelTransform<TScalarType, NDimensions>
::TransformVector(const VectorType& thisVector) const
{
  int numLandmarks = m_p->GetNumberOfPoints();
  int i, j;
  ColumnMatrixType c, d, Ax;
  VectorType result;
  VectorType GArgument;
  vnl_matrix_fixed<TScalarType, NDimensions, NDimensions> A;
  VectorType thisVectorCopy = thisVector;
  PointType p;

  d = d*0;
  for (i = 0; i < numLandmarks; i++)
    {
    m_p->GetPoint(i, &p);
    c.update(m_WMatrix->extract(NDimensions, 1, i*NDimensions, 0), 0, 0);
    for (j = 0; j < NDimensions; j++)
      {
      GArgument[j] = thisVectorCopy.Get_vnl_vector()[j] -
        p.Get_vnl_vector()[j];
      }
    d = d + ComputeG(GArgument) * c;
  }
  A.update(m_WMatrix->extract(NDimensions, 1,
                              numLandmarks*NDimensions, 0), 0, 0);
  A.update(m_WMatrix->extract(NDimensions, 1,
                              (numLandmarks+1)*NDimensions, 0), 0, 1);
  A.update(m_WMatrix->extract(NDimensions, 1,
                              (numLandmarks+2)*NDimensions, 0), 0, 2);
  for (j = 0; j < NDimensions; j++)
    {
    Ax.put(j, 0, thisVector[j]);
    }
  Ax = A*Ax;
  d = d + Ax;
  for (i = 0; i < NDimensions; i++)
    {
    result[i] = thisVector[i] + d.get(i, 0);
    }

  return result;
}

/**
 *
 */
template<class TScalarType, int NDimensions>
inline
std::ostream&
operator<< (std::ostream &s,
            const KernelTransform<TScalarType, NDimensions>& transform)
{
  return transform.PrintSelf(s);
}

} // namespace itk

