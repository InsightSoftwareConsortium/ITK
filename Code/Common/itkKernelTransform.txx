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


/**
 *
 */
template <class TScalarType, unsigned int NDimensions>
KernelTransform<TScalarType, NDimensions>::
KernelTransform():Superclass(
                      NDimensions,
                      NDimensions ) 
// the second NDimensions is associated is provided as
// a tentative number for initializing the Jacobian.
// The matrix can be resized at run time so this number
// here is irrelevant. The correct size of the Jacobian
// will be NDimension X NDimension.NumberOfLandMarks.
{

  m_I.set_identity();
  m_SourceLandmarks = PointSetType::New();
  m_TargetLandmarks = PointSetType::New();
  m_Displacements   = VectorSetType::New();
  m_WMatrixComputed = false;

  m_Stiffness = 0.0;
}

/**
 *
 */
template <class TScalarType, unsigned int NDimensions>
KernelTransform<TScalarType, NDimensions>::
~KernelTransform()
{
}



/**
 *
 */
template <class TScalarType, unsigned int NDimensions>
const typename KernelTransform<TScalarType, NDimensions>::GMatrixType &
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
template <class TScalarType, unsigned int NDimensions>
const typename KernelTransform<TScalarType, NDimensions>::GMatrixType &
KernelTransform<TScalarType, NDimensions>::
ComputeReflexiveG( PointsIterator ) const
{
  m_GMatrix.fill( NumericTraits< TScalarType >::Zero );
  m_GMatrix.fill_diagonal( m_Stiffness );

  return m_GMatrix;
}




/**
 * Default implementation of the the method. This can be overloaded
 * in transforms whose kernel produce diagonal G matrices.
 */
template <class TScalarType, unsigned int NDimensions>
void
KernelTransform<TScalarType, NDimensions>::
ComputeDeformationContribution( const InputPointType  & thisPoint,
                                      OutputPointType & result     ) const
{

  unsigned long numberOfLandmarks = m_SourceLandmarks->GetNumberOfPoints();

  PointsIterator sp  = m_SourceLandmarks->GetPoints()->Begin();

  for(unsigned int lnd=0; lnd < numberOfLandmarks; lnd++ )
    {
    const GMatrixType & Gmatrix = ComputeG( thisPoint - sp->Value() );
    for(unsigned int dim=0; dim < NDimensions; dim++ )
      {
      for(unsigned int odim=0; odim < NDimensions; odim++ )
        {
        result[ odim ] += Gmatrix(dim, odim ) * m_DMatrix(dim,lnd);
        }
      }
    ++sp;
    }

}


/**
 *
 */
template <class TScalarType, unsigned int NDimensions>
void KernelTransform<TScalarType, NDimensions>
::ComputeD(void)
{
  unsigned long numberOfLandmarks = m_SourceLandmarks->GetNumberOfPoints();
  
  PointsIterator sp  = m_SourceLandmarks->GetPoints()->Begin();
  PointsIterator tp  = m_TargetLandmarks->GetPoints()->Begin();
  PointsIterator end = m_SourceLandmarks->GetPoints()->End();

  m_Displacements->Reserve( numberOfLandmarks );
  typename VectorSetType::Iterator vt = m_Displacements->Begin();

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
template <class TScalarType, unsigned int NDimensions>
void KernelTransform<TScalarType, NDimensions>
::ComputeWMatrix(void)
{

  typedef vnl_svd<TScalarType>  SVDSolverType;

  this->ComputeL();
  this->ComputeY();
  SVDSolverType svd( m_LMatrix, 1e-8 );
  m_WMatrix = svd.solve( m_YMatrix );

  this->ReorganizeW();

}

/**
 *
 */
template <class TScalarType, unsigned int NDimensions>
void KernelTransform<TScalarType, NDimensions>::
ComputeL(void)
{
  unsigned long numberOfLandmarks = m_SourceLandmarks->GetNumberOfPoints();
  vnl_matrix<TScalarType> O2(NDimensions*(NDimensions+1),
                             NDimensions*(NDimensions+1), 0);

  this->ComputeP();
  this->ComputeK();

  m_LMatrix.resize( NDimensions*(numberOfLandmarks+NDimensions+1),
                    NDimensions*(numberOfLandmarks+NDimensions+1) );
  m_LMatrix.fill( 0.0 );

  m_LMatrix.update( m_KMatrix, 0, 0 );
  m_LMatrix.update( m_PMatrix, 0, m_KMatrix.columns() );
  m_LMatrix.update( m_PMatrix.transpose(), m_KMatrix.rows(), 0);
  m_LMatrix.update( O2, m_KMatrix.rows(), m_KMatrix.columns());

}


/**
 *
 */
template <class TScalarType, unsigned int NDimensions>
void KernelTransform<TScalarType, NDimensions>::
ComputeK(void)
{
  unsigned long numberOfLandmarks = m_SourceLandmarks->GetNumberOfPoints();
  GMatrixType G;

  this->ComputeD();

  m_KMatrix.resize( NDimensions * numberOfLandmarks,
                    NDimensions * numberOfLandmarks );

  m_KMatrix.fill( 0.0 );

  PointsIterator p1  = m_SourceLandmarks->GetPoints()->Begin();
  PointsIterator end = m_SourceLandmarks->GetPoints()->End();

  // K matrix is symmetric, so only evaluate the upper triangle and
  // store the values in bot the upper and lower triangle
  unsigned int i = 0;
  while( p1 != end )
  {
    PointsIterator p2 = p1; // start at the diagonal element
    unsigned int j = i;

    // Compute the block diagonal element, i.e. kernel for pi->pi
    G = ComputeReflexiveG(p1);
    m_KMatrix.update(G, i*NDimensions, i*NDimensions);
    p2++;
    j++;
    
    // Compute the upper (and copy into lower) triangular part of K
    while( p2 != end ) 
    {
      const InputVectorType s = p1.Value() - p2.Value();
      G = ComputeG(s);
      // write value in upper and lower triangle of matrix
      m_KMatrix.update(G, i*NDimensions, j*NDimensions);
      m_KMatrix.update(G, j*NDimensions, i*NDimensions);  
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
template <class TScalarType, unsigned int NDimensions>
void KernelTransform<TScalarType, NDimensions>::
ComputeP()
{
  unsigned long numberOfLandmarks = m_SourceLandmarks->GetNumberOfPoints();
  IMatrixType I;
  IMatrixType temp;
  InputPointType p;

  I.set_identity();
  m_PMatrix.resize( NDimensions*numberOfLandmarks,
                    NDimensions*(NDimensions+1) );
  m_PMatrix.fill( 0.0 );
  for (unsigned int i = 0; i < numberOfLandmarks; i++)
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
template <class TScalarType, unsigned int NDimensions>
void KernelTransform<TScalarType, NDimensions>::
ComputeY(void)
{
  unsigned long numberOfLandmarks = m_SourceLandmarks->GetNumberOfPoints();

  typename VectorSetType::ConstIterator displacement =
    m_Displacements->Begin();

  m_YMatrix.resize( NDimensions*(numberOfLandmarks+NDimensions+1), 1);

  m_YMatrix.fill( 0.0 );
    
  for (unsigned int i = 0; i < numberOfLandmarks; i++)
  {
    for (unsigned int j = 0; j < NDimensions; j++)
    {
      m_YMatrix.put(i*NDimensions+j, 0, displacement.Value()[j]);
    }
    displacement++;
  }

  for (unsigned int i = 0; i < NDimensions*(NDimensions+1); i++) 
  {
    m_YMatrix.put(numberOfLandmarks*NDimensions+i, 0, 0);
  }
}


/**
 *
 */
template <class TScalarType, unsigned int NDimensions>
void
KernelTransform<TScalarType, NDimensions>
::ReorganizeW(void) 
{
  unsigned long numberOfLandmarks = m_SourceLandmarks->GetNumberOfPoints();

  // The deformable (non-affine) part of the registration goes here
  m_DMatrix.resize(NDimensions,numberOfLandmarks);
  unsigned int ci = 0;
  for(unsigned int lnd=0; lnd < numberOfLandmarks; lnd++ )
    {
    for(unsigned int dim=0; dim < NDimensions; dim++ )
      {
      m_DMatrix(dim,lnd) = m_WMatrix(ci++,0);
      }
    }

  // This matrix holds the rotational part of the Affine component
  for(unsigned int j=0; j < NDimensions; j++ )
    {
    for(unsigned int i=0; i < NDimensions; i++ )
      {
      m_AMatrix(i,j) = m_WMatrix(ci++,0);
      }
    }

  // This vector holds the translational part of the Affine component
  for(unsigned int k=0; k < NDimensions; k++ )
    {
    m_BVector(k) = m_WMatrix(ci++,0);
    }

  // release WMatrix memory by assigning a small one.
  m_WMatrix = WMatrixType(1,1);   

}



/**
 *
 */
template <class TScalarType, unsigned int NDimensions>
typename KernelTransform<TScalarType, NDimensions>::OutputPointType
KernelTransform<TScalarType, NDimensions>
::TransformPoint(const InputPointType& thisPoint) const
{

  OutputPointType result;

  typedef typename OutputPointType::ValueType ValueType;

  result.Fill( NumericTraits< ValueType >::Zero );

  this->ComputeDeformationContribution( thisPoint, result );

  // Add the rotational part of the Affine component
  for(unsigned int j=0; j < NDimensions; j++ )
    {
    for(unsigned int i=0; i < NDimensions; i++ )
      {
      result[i] += m_AMatrix(i,j) * thisPoint[j];
      }
    }

  
 
  // This vector holds the translational part of the Affine component
  for(unsigned int k=0; k < NDimensions; k++ )
    {
    result[k] += m_BVector(k) + thisPoint[k];
    }

  return result;

}




// Compute the Jacobian in one position 
template <class TScalarType, unsigned int NDimensions>
const typename KernelTransform<TScalarType,NDimensions>::JacobianType & 
KernelTransform< TScalarType,NDimensions>::
GetJacobian( const InputPointType & p ) const
{
  

  m_Jacobian.Fill( 0.0 );

  // TODO
  // The Jacobian should be computable in terms of the matrices
  // used to Transform points...

  return m_Jacobian;

}


// Set the parameters
// NOTE that in this transformation both the Source and Target
// landmarks could be considered as parameters. It is assumed
// here that the Target landmarks are provided by the user and
// are not changed during the optimization process required for
// registration.
template <class TScalarType, unsigned int NDimensions>
void
KernelTransform<TScalarType, NDimensions>::
SetParameters( const ParametersType & parameters )
{
  Superclass::SetParameters( parameters );

  typename PointsContainer::Pointer landmarks = PointsContainer::New();
  const unsigned int numberOfLandmarks =  parameters.Size() / NDimensions; 
  landmarks->Reserve( numberOfLandmarks );

  PointsIterator itr = landmarks->Begin();
  PointsIterator end = landmarks->End();

  InputPointType  landMark; 

  unsigned int pcounter = 0;
  while( itr != end )
    {
    for(unsigned int dim=0; dim<NDimensions; dim++)
      {
      landMark[ dim ] = m_Parameters[ pcounter ];
      pcounter++;
      }  
    itr.Value() = landMark;
    itr++;
    }

  m_SourceLandmarks->SetPoints( landmarks );
}




// Get the parameters
// They are the components of all the landmarks in the source space
template <class TScalarType, unsigned int NDimensions>
const typename KernelTransform<TScalarType, NDimensions>::ParametersType &
KernelTransform<TScalarType, NDimensions>::
GetParameters( void ) const
{

  m_Parameters = ParametersType( m_SourceLandmarks->GetNumberOfPoints() * NDimensions );

  PointsIterator itr = m_SourceLandmarks->GetPoints()->Begin();
  PointsIterator end = m_SourceLandmarks->GetPoints()->End();

  unsigned int pcounter = 0;
  while( itr != end )
    {
    InputPointType  landmark = itr.Value();
    for(unsigned int dim=0; dim<NDimensions; dim++)
      {
      m_Parameters[ pcounter ] = landmark[ dim ];
      pcounter++;
      }  
    itr++;
    }

  return m_Parameters;

}



template <class TScalarType, unsigned int NDimensions>
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
