/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKernelTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkKernelTransform_txx
#define _itkKernelTransform_txx
#include "itkKernelTransform.h"

namespace itk
{

#define MAX_NUM_OF_LANDMARKS 100

/**
 *
 */
template <class TScalarType, int NDimensions>
KernelTransform<TScalarType, NDimensions>::
KernelTransform():Superclass(
                      NDimensions,
                      NDimensions * MAX_NUM_OF_LANDMARKS)    
{

  m_I.set_identity();
  m_SourceLandmarks = PointSetType::New();
  m_TargetLandmarks = PointSetType::New();
  m_Displacements   = VectorSetType::New();
  m_WMatrixComputed = false;
}

/**
 *
 */
template <class TScalarType, int NDimensions>
KernelTransform<TScalarType, NDimensions>::
~KernelTransform()
{
}



/**
 *
 */
template <class TScalarType, int NDimensions>
const KernelTransform<TScalarType, NDimensions>::GMatrixType &
KernelTransform<TScalarType, NDimensions>::
ComputeG( const InputVectorType & vect ) const
{
  //
  // Should an Exception be thrown here  ?
  //
  itkWarningMacro(<< "ComputeG() should be reimplemented in the subclass !!");
  return m_GMatrix;
}



/**
 *
 */
template <class TScalarType, int NDimensions>
void KernelTransform<TScalarType, NDimensions>
::ComputeD(void)
{
  unsigned long numLandmarks = m_SourceLandmarks->GetNumberOfPoints();
  
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
template <class TScalarType, int NDimensions>
void KernelTransform<TScalarType, NDimensions>
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
template <class TScalarType, int NDimensions>
void KernelTransform<TScalarType, NDimensions>::
ComputeL(void)
{
  unsigned long numLandmarks = m_SourceLandmarks->GetNumberOfPoints();
  vnl_matrix<TScalarType> O2(NDimensions*(NDimensions+1),
                             NDimensions*(NDimensions+1), 0);

  ComputeP();
  ComputeK();

  m_LMatrix.resize( NDimensions*(numLandmarks+NDimensions+1),
                    NDimensions*(numLandmarks+NDimensions+1) );
  m_LMatrix.fill( 0.0 );

  m_LMatrix.update( m_KMatrix, 0, 0 );
  m_LMatrix.update( m_PMatrix, 0, m_KMatrix.columns() );
  m_LMatrix.update( m_PMatrix.transpose(), m_KMatrix.rows(), 0);
  m_LMatrix.update( O2, m_KMatrix.rows(), m_KMatrix.columns());

}


/**
 *
 */
template <class TScalarType, int NDimensions>
void KernelTransform<TScalarType, NDimensions>::
ComputeK(void)
{
  unsigned long numLandmarks = m_SourceLandmarks->GetNumberOfPoints();
  GMatrixType G;

  ComputeD();

  m_KMatrix.resize( NDimensions * numLandmarks,
                    NDimensions * numLandmarks );

  m_KMatrix.fill( 0.0 );

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
template <class TScalarType, int NDimensions>
void KernelTransform<TScalarType, NDimensions>::
ComputeP()
{
  unsigned long numLandmarks = m_SourceLandmarks->GetNumberOfPoints();
  IMatrixType I;
  IMatrixType temp;
  InputPointType p;

  I.set_identity();
  m_PMatrix.resize( NDimensions*numLandmarks,
                    NDimensions*(NDimensions+1) );
  m_PMatrix.fill( 0.0 );
  for (unsigned int i = 0; i < numLandmarks; i++)
  {
    m_SourceLandmarks->GetPoint(i, &p);
    for (unsigned int j = 0; j < NDimensions; j++)
      {
      temp = I * p[j];
      m_PMatrix.update(temp, i*NDimensions, j*NDimensions);
      }
    m_PMatrix.update(I, i*NDimensions, NDimensions*NDimensions);
  }
}



/**
 *
 */
template <class TScalarType, int NDimensions>
void KernelTransform<TScalarType, NDimensions>::
ComputeY(void)
{
  unsigned long numLandmarks = m_SourceLandmarks->GetNumberOfPoints();

  VectorSetType::ConstIterator displacement = m_Displacements->Begin();

  m_YMatrix.resize( NDimensions*(numLandmarks+NDimensions+1), 1);

  m_YMatrix.fill( 0.0 );
    
  for (unsigned int i = 0; i < numLandmarks; i++)
  {
    for (unsigned int j = 0; j < NDimensions; j++)
    {
      m_YMatrix.put(i*NDimensions+j, 0, displacement.Value()[j]);
    }
    displacement++;
  }

  for (unsigned int i = 0; i < NDimensions*(NDimensions+1); i++) 
  {
    m_YMatrix.put(numLandmarks*NDimensions+i, 0, 0);
  }
}


/**
 *
 */
template <class TScalarType, int NDimensions>
KernelTransform<TScalarType, NDimensions>::OutputPointType
KernelTransform<TScalarType, NDimensions>
::TransformPoint(const InputPointType& thisPoint) const
{
  unsigned long numLandmarks = m_SourceLandmarks->GetNumberOfPoints();
  ColumnMatrixType b, c, d, Ax;
  vnl_matrix_fixed<TScalarType, NDimensions, NDimensions> A;
  OutputPointType result;
  InputVectorType argumentG;
  InputPointType p;

  b.fill(0.0);
  c.fill(0.0);
  d.fill(0.0);
  A.fill(0.0);
  Ax.fill(0.0);

  for (unsigned int i = 0; i < numLandmarks; i++)
    {
    c.update(m_WMatrix.extract(NDimensions, 1, i*NDimensions, 0), 0, 0);
    m_SourceLandmarks->GetPoint(i, &p);
    argumentG = thisPoint - p;
    d = d + ComputeG(argumentG) * c;
    }
  for (unsigned int i = 0; i < NDimensions; i++)
    {
    A.update(m_WMatrix.extract(NDimensions, 1,
                                (numLandmarks+i)*NDimensions, 0), 0, i);
    }
  b.update(m_WMatrix.extract(NDimensions, 1,
                              (numLandmarks+NDimensions)*NDimensions, 0),
           0, 0);
  for (unsigned int j = 0; j < NDimensions; j++)
    {
    Ax.put(j, 0, thisPoint[j]);
    }
  Ax = A*Ax;
  d = d + Ax + b;
  for (unsigned int j = 0; j < NDimensions; j++)
    {
    result[j] = thisPoint[j] + d.get(j, 0);
    }
  return result;
}




// Compute the Jacobian in one position 
template <class TScalarType, int NDimensions>
const KernelTransform<TScalarType,NDimensions>::JacobianType & 
KernelTransform< TScalarType,NDimensions>::
GetJacobian( const InputPointType & p ) const
{
  

  m_Jacobian.Fill( 0.0 );

  // TODO
  // The Jacobian should be computable in terms of the matrices
  // used to Transform points...

  return m_Jacobian;

}


template <class TScalarType, int NDimensions>
void
KernelTransform<TScalarType, NDimensions>::
PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  if (m_SourceLandmarks)
    {
    os << indent << "SourceLandmarks: " << m_SourceLandmarks << std::endl;
    }
  if (m_TargetLandmarks)
    {
    os << indent << "TargetLandmarks: " << m_TargetLandmarks << std::endl;
    }
  if (m_Displacements)
    {
    os << indent << "Displacements: " << m_Displacements << std::endl;
    }

}
} // namespace itk

#endif
