/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThreadSafeTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkThreadSafeTransform_txx
#define _itkThreadSafeTransform_txx

#include "itkThreadSafeTransform.h"

namespace itk
{


/**
 * Constructor
 */
template < class TScalarType,
           unsigned int NInputDimensions,
           unsigned int NOutputDimensions >
Transform< TScalarType,NInputDimensions,NOutputDimensions>
::Transform():
  m_Parameters(1),
  m_FixedParameters(1),
  m_JacobianDimensions(NOutputDimensions),
  m_JacobianNumberOfParameters(1),
  m_Jacobian(NOutputDimensions,1),
  m_NumberOfThreads(1)
{
  itkWarningMacro(<< "Using default transform constructor.  Should specify NOutputDims and NParameters as args to constructor.");

  // threadId=0 is stored directly in m_Jacobian so we only need (num trhreads - 1) variables here.
  this->m_ThreaderJacobian = new JacobianType[ this->m_NumberOfThreads-1 ]; 
  for( unsigned int i=0; i < this->m_NumberOfThreads-1; i++ )
    {
    this->m_ThreaderJacobian[i].SetSize( this->m_JacobianDimensions, this->m_JacobianNumberOfParameters );
    }
}


/**
 * Constructor
 */
template < class TScalarType,
           unsigned int NInputDimensions,
           unsigned int NOutputDimensions >
Transform< TScalarType,NInputDimensions,NOutputDimensions>
::Transform(unsigned int dimension,unsigned int numberOfParameters):
  m_Parameters(numberOfParameters),
  m_FixedParameters(numberOfParameters),
  m_JacobianDimensions(dimension),
  m_JacobianNumberOfParameters(numberOfParameters),
  m_Jacobian(dimension,numberOfParameters),
  m_NumberOfThreads(1)
{
  // threadId=0 is stored directly in m_Jacobian so we only need (num trhreads - 1) variables here.
  this->m_ThreaderJacobian = new JacobianType[ this->m_NumberOfThreads-1 ]; 
  for( unsigned int i=0; i < this->m_NumberOfThreads-1; i++ )
    {
    this->m_ThreaderJacobian[i].SetSize( this->m_JacobianDimensions, this->m_JacobianNumberOfParameters );
    }
}


/**
 * Set the number of threads. This method is const,
 * since it only affects mutable parts of the Transform.
 */
template < class TScalarType,
           unsigned int NInputDimensions,
           unsigned int NOutputDimensions >
void
Transform< TScalarType,NInputDimensions,NOutputDimensions>
::SetNumberOfThreads( unsigned int numberOfThreads ) const
{
  itkDebugMacro("setting NumberOfThreads to " << numberOfThreads);
  if (this->m_NumberOfThreads != numberOfThreads )
    { 
    this->m_NumberOfThreads = numberOfThreads;

    if( this->m_ThreaderJacobian == NULL )
      {
      delete [] this->m_ThreaderJacobian;
      } 
    this->m_ThreaderJacobian = new JacobianType[ this->m_NumberOfThreads-1 ]; 
    for( unsigned int i=0; i < this->m_NumberOfThreads-1; i++ )
      {
      this->m_ThreaderJacobian[i].SetSize( this->m_JacobianDimensions, this->m_JacobianNumberOfParameters );
      }

    this->Modified();
    }
}
 
// Get the storage space for the Jacobian corresponding to a particular thread.
template<class TScalarType, unsigned int NDimensions, unsigned int VSplineOrder>
typename Transform<TScalarType, NDimensions,VSplineOrder>::JacobianType * 
Transform<TScalarType, NDimensions,VSplineOrder>
::GetJacobianVariableForThread( unsigned int threadId ) const
{
  JacobianType * jacobian = &(this->m_Jacobian);
  if( threadId > 0 )
    {
    jacobian = &(this->m_ThreaderJacobian[threadId-1]);
    }
  return jacobian;
}

/**
 * GenerateName
 */
template < class TScalarType,
           unsigned int NInputDimensions,
           unsigned int NOutputDimensions >
std::string Transform< TScalarType,NInputDimensions,NOutputDimensions>
::GetTransformTypeAsString () const
{
  OStringStream n;
  n << GetNameOfClass();
  n << "_";
  if ( typeid ( TScalarType ) == typeid ( float ) )
    {
    n << "float";
    }
  else if ( typeid ( TScalarType ) == typeid ( double ) )
    {
      n << "double";
    }
  else
    {
      n << "other";
    }
  n << "_" << InputSpaceDimension << "_" << OutputSpaceDimension;
  return n.str();
}


template < class TScalarType,
           unsigned int NInputDimensions,
           unsigned int NOutputDimensions >
void
Transform< TScalarType,NInputDimensions,NOutputDimensions>::
PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "NumberOfThreads: " << this->m_NumberOfThreads << std::endl;
}

} // end namespace itk


#endif
