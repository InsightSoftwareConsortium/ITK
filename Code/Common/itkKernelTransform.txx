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
template <class TScalarType, int NDimensions,
          class TParameters, class TJacobianType>
KernelTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
KernelTransform()  
{
  static bool IMatrixInitialized = false;

  if (!IMatrixInitialized)
  {
    m_I.set_identity();
    IMatrixInitialized = true;
  }
  m_SourceLandmarks = PointSetType::New();
  m_TargetLandmarks = PointSetType::New();
  m_Displacements   = VectorSetType::New();
  m_WMatrixComputed = false;
}

/**
 *
 */
template <class TScalarType, int NDimensions,
          class TParameters, class TJacobianType>
KernelTransform<TScalarType, NDimensions,
                TParameters,TJacobianType>::
~KernelTransform()
{
}


/**
 *
 */
template <class TScalarType, int NDimensions,
          class TParameters, class TJacobianType>
KernelTransform<TScalarType, NDimensions,TParameters,TJacobianType>::PointSetPointer
KernelTransform<TScalarType, NDimensions,TParameters,TJacobianType>
::GetSourceLandmarks(void) const
{
  return m_SourceLandmarks;
}

/**
 *
 */
template <class TScalarType, int NDimensions,
          class TParameters, class TJacobianType>
KernelTransform<TScalarType, NDimensions,TParameters,TJacobianType>::PointSetPointer
KernelTransform<TScalarType, NDimensions,TParameters,TJacobianType>
::GetTargetLandmarks(void) const
{
  return m_TargetLandmarks;
}

/**
 *
 */
template <class TScalarType, int NDimensions,
          class TParameters, class TJacobianType>
void 
KernelTransform<TScalarType, NDimensions,TParameters,TJacobianType>
::SetSourceLandmarks(const PointSetType * p)
{
  m_SourceLandmarks = p;
}

/**
 *
 */
template <class TScalarType, int NDimensions,
          class TParameters, class TJacobianType>
void 
KernelTransform<TScalarType, NDimensions,TParameters,TJacobianType>
::SetTargetLandmarks(const PointSetType * q)
{
  m_TargetLandmarks = q;
}

/**
 *
 */
template <class TScalarType, int NDimensions,
          class TParameters, class TJacobianType>
KernelTransform<TScalarType, NDimensions,TParameters,TJacobianType>::VectorSetPointer
KernelTransform<TScalarType, NDimensions,TParameters,TJacobianType>
::GetDisplacements(void) const
{
  return m_Displacements;
}


/**
 *
 */
template <class TScalarType, int NDimensions,
          class TParameters, class TJacobianType>
KernelTransform<TScalarType, NDimensions,TParameters,TJacobianType>::GMatrixType
KernelTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
ComputeG( const InputVectorType & vect ) const
{
  //
  // Should an Exception be thrown here  ?
  //
  std::cerr << "WARNING: ComputeG() should be reimplemented in the subclass !!";
  GMatrixType matrix;
  return matrix;
}



/**
 *
 */
template <class TScalarType, int NDimensions,
          class TParameters,class TJacobianType>
void KernelTransform<TScalarType, NDimensions,TParameters,TJacobianType>
::ComputeD(void)
{
  int numLandmarks = m_SourceLandmarks->GetNumberOfPoints();
  
  PointsIterator sp  = m_SourceLandmarks->GetPoints()->Begin();
  PointsIterator tp  = m_TargetLandmarks->GetPoints()->Begin();
  PointsIterator end = m_SourceLandmarks->GetPoints()->End();

  m_Displacements->Reserve( numLandmarks );
  VectorSetType::Iterator vt = m_Displacements->Begin();

  while( sp != end )
  {
    vt->Value() = tp->Value() - sp->Value();
    vt++;
    sp++;
    tp++;
  }
}

/**
 *
 */
template <class TScalarType, int NDimensions,
          class TParameters, class TJacobianType>
void KernelTransform<TScalarType, NDimensions,TParameters,TJacobianType>
::ComputeWMatrix(void)
{
  typedef vnl_svd<TScalarType>  SVDSolverType;

  ComputeL();
  ComputeY();
  SVDSolverType svd( m_LMatrix, 1e-8 );
  m_WMatrix = svd.solve( m_YMatrix );
}

/**
 *
 */
template <class TScalarType, int NDimensions,
          class TParameters, class TJacobianType>
void KernelTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
ComputeL(void)
{
  int numLandmarks = m_SourceLandmarks->GetNumberOfPoints();
  vnl_matrix<TScalarType> O2(NDimensions*(NDimensions+1),
                             NDimensions*(NDimensions+1), 0);

  ComputeP();
  ComputeK();
  m_LMatrix.resize( NDimensions*(numLandmarks+NDimensions+1),
                    NDimensions*(numLandmarks+NDimensions+1) );
  m_LMatrix.update( m_KMatrix, 0, 0 );
  m_LMatrix.update( m_PMatrix, 0, m_KMatrix.columns() );
  m_LMatrix.update( m_PMatrix.transpose(), m_KMatrix.rows(), 0);
  m_LMatrix.update(O2, m_KMatrix.rows(), m_KMatrix.columns());
}


/**
 *
 */
template <class TScalarType, int NDimensions,
          class TParameters, class TJacobianType>
void KernelTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
ComputeK(void)
{
  int numLandmarks = m_SourceLandmarks->GetNumberOfPoints();
  GMatrixType G;

  ComputeD();

  m_KMatrix.resize( NDimensions * numLandmarks,
                    NDimensions * numLandmarks );

  PointsIterator p1  = m_SourceLandmarks->GetPoints()->Begin();
  PointsIterator end = m_SourceLandmarks->GetPoints()->End();

  unsigned int i = 0;
  while( p1 != end )
  {
    PointsIterator p2  = m_SourceLandmarks->GetPoints()->Begin();
    unsigned int j = 0;
    while( p2 != end ) 
    {
      const InputVectorType s = p1.Value() - p2.Value();
      G = ComputeG(s);
      m_KMatrix.update(G, i*NDimensions, j*NDimensions);
      p2++;
      j++;
    }
    p1++;
    i++;
  }
}



/**
 *
 */
template <class TScalarType, int NDimensions,
          class TParameters, class TJacobianType>
void KernelTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
ComputeP()
{
  int numLandmarks = m_SourceLandmarks->GetNumberOfPoints();
  IMatrixType I;
  IMatrixType temp;
  int i, j;
  InputPointType p;

  I.set_identity();
  m_PMatrix.resize( NDimensions*numLandmarks,
                    NDimensions*(NDimensions+1) );
  for (i = 0; i < numLandmarks; i++)
  {
    m_SourceLandmarks->GetPoint(i, &p);
    for (j = 0; j < NDimensions; j++)
      {
      temp = I * p.Get_vnl_vector()[j];
      m_PMatrix.update(temp, i*NDimensions, j*NDimensions);
      }
    m_PMatrix.update(I, i*NDimensions, j*NDimensions);
  }
}



/**
 *
 */
template <class TScalarType, int NDimensions,
          class TParameters, class TJacobianType>
void KernelTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
ComputeY(void)
{
  int i, j;
  int numLandmarks = m_SourceLandmarks->GetNumberOfPoints();

  VectorSetType::ConstIterator displacement = m_Displacements->Begin();

  m_YMatrix.resize( NDimensions*(numLandmarks+NDimensions+1), 1);

  for (i = 0; i < numLandmarks; i++)
  {
    for (j = 0; j < NDimensions; j++)
    {
      m_YMatrix.put(i*NDimensions+j, 0, displacement.Value()[j]);
    }
    displacement++;
  }

  for (i = 0; i < NDimensions*(NDimensions+1); i++) 
  {
    m_YMatrix.put(numLandmarks*NDimensions+i, 0, 0);
  }
}


/**
 *
 */
template <class TScalarType, int NDimensions,
          class TParameters, class TJacobianType>
KernelTransform<TScalarType, NDimensions,TParameters,TJacobianType>::OutputPointType
KernelTransform<TScalarType, NDimensions,TParameters,TJacobianType>
::TransformPoint(const InputPointType& thisPoint) const
{
  int numLandmarks = m_SourceLandmarks->GetNumberOfPoints();
  int i, j;
  ColumnMatrixType b, c, d, Ax;
  vnl_matrix_fixed<TScalarType, NDimensions, NDimensions> A;
  OutputPointType result;
  InputVectorType argumentG;
  InputPointType p;

  d = d*0;
  for (i = 0; i < numLandmarks; i++)
    {
    c.update(m_WMatrix.extract(NDimensions, 1, i*NDimensions, 0), 0, 0);
    m_SourceLandmarks->GetPoint(i, &p);
    argumentG = thisPoint - p;
    d = d + ComputeG(argumentG) * c;
    }
  for (i = 0; i < NDimensions; i++)
    {
    A.update(m_WMatrix.extract(NDimensions, 1,
                                (numLandmarks+i)*NDimensions, 0), 0, i);
    }
  b.update(m_WMatrix.extract(NDimensions, 1,
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
template <class TScalarType, int NDimensions,
          class TParameters, class TJacobianType>
KernelTransform<TScalarType, NDimensions,TParameters,TJacobianType>::OutputVectorType
KernelTransform<TScalarType, NDimensions,TParameters,TJacobianType>
::TransformVector(const InputVectorType& thisVector) const
{
  int numLandmarks = m_SourceLandmarks->GetNumberOfPoints();
  int i, j;
  ColumnMatrixType c, d, Ax;
  InputVectorType result;
  InputVectorType GArgument;
  vnl_matrix_fixed<TScalarType, NDimensions, NDimensions> A;
  InputVectorType thisVectorCopy = thisVector;
  InputPointType p;

  d = d*0;
  for (i = 0; i < numLandmarks; i++)
    {
    m_SourceLandmarks->GetPoint(i, &p);
    c.update(m_WMatrix.extract(NDimensions, 1, i*NDimensions, 0), 0, 0);
    for (j = 0; j < NDimensions; j++)
      {
      GArgument[j] = thisVectorCopy.Get_vnl_vector()[j] -
        p.Get_vnl_vector()[j];
      }
    d = d + ComputeG(GArgument) * c;
  }
  A.update(m_WMatrix.extract(NDimensions, 1,
                              numLandmarks*NDimensions, 0), 0, 0);
  A.update(m_WMatrix.extract(NDimensions, 1,
                              (numLandmarks+1)*NDimensions, 0), 0, 1);
  A.update(m_WMatrix.extract(NDimensions, 1,
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
template<class TScalarType, int NDimensions, class TParameters, class TJacobianType>
inline
std::ostream&
operator<< ( std::ostream &s,
             const KernelTransform<TScalarType, NDimensions, 
                                  TParameters, TJacobianType>& transform )
{
  return transform.PrintSelf(s);
}

} // namespace itk

