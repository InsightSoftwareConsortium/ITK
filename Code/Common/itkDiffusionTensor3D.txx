/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDiffusionTensor3D.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkDiffusionTensor3D_txx
#define _itkDiffusionTensor3D_txx

#include "itkDiffusionTensor3D.h"
#include "itkNumericTraits.h"

namespace itk
{


/*
 * Default Constructor 
 */
template<class T>
DiffusionTensor3D<T>
::DiffusionTensor3D()
{
}


/*
 * Constructor with initialization
 */
template<class T>
DiffusionTensor3D<T>
::DiffusionTensor3D( const Self & r ):SymmetricSecondRankTensor<T,3>(r)
{
}



/*
 * Constructor with initialization
 */
template<class T>
DiffusionTensor3D<T>
::DiffusionTensor3D( const Superclass & r ):SymmetricSecondRankTensor<T,3>(r)
{
}



/*
 * Constructor with initialization
 */
template<class T>
DiffusionTensor3D<T>
::DiffusionTensor3D( const ComponentType & r ):SymmetricSecondRankTensor<T,3>(r)
{
}




/*
 * Constructor with initialization
 */
template<class T>
DiffusionTensor3D<T>
::DiffusionTensor3D( const ComponentArrayType r ):SymmetricSecondRankTensor<T,3>(r)
{
}


/*
 * Assignment Operator
 */
template<class T>
DiffusionTensor3D<T>&
DiffusionTensor3D<T>
::operator= (const Self& r)
{
  Superclass::operator=(r);
  return *this;
}



/*
 * Assignment Operator
 */
template<class T>
DiffusionTensor3D<T>&
DiffusionTensor3D<T>
::operator= (const ComponentType & r)
{
  Superclass::operator=(r);
  return *this;
}



/*
 * Assignment Operator
 */
template<class T>
DiffusionTensor3D<T>&
DiffusionTensor3D<T>
::operator= (const ComponentArrayType r)
{
  Superclass::operator=(r);
  return *this;
}



/*
 * Assignment Operator
 */
template<class T>
DiffusionTensor3D<T>&
DiffusionTensor3D<T>
::operator= (const Superclass & r)
{
  Superclass::operator=(r);
  return *this;
}



/*
 * Get the Trace, specialized version for 3D.
 * 
 * Note that the indices are related to the fact 
 * that we store only the upper-right triangle of
 * the matrix. Like
 *
 *       | 0  1  2  |
 *       | X  3  4  |
 *       | X  X  5  |
 *
 * The trace is therefore the sum of the components
 * M[0], M[3] and M[5].
 *
 */
template<class T>
typename DiffusionTensor3D<T>::AccumulateValueType
DiffusionTensor3D<T>
::GetTrace() const
{
  AccumulateValueType trace = (*this)[0];
  trace += (*this)[3];
  trace += (*this)[5];
  return trace;
}


/**
 *  Compute the value of fractional anisotropy
 */
template<class T>
typename DiffusionTensor3D<T>::RealValueType
DiffusionTensor3D<T>
::GetFractionalAnisotropy() const
{
  EigenValuesArrayType eigenValues;
  this->ComputeEigenValues( eigenValues );

  // trace of the diagonal eigenvalue matrix
  const RealValueType lambdaMean = 
    ( eigenValues[0] + eigenValues[1] + eigenValues[2] ) / 3.0;

  const RealValueType tmp0 = eigenValues[0] - lambdaMean;
  const RealValueType tmp1 = eigenValues[1] - lambdaMean;
  const RealValueType tmp2 = eigenValues[2] - lambdaMean;
    
  const RealValueType faSquare =
    (3 * (tmp0*tmp0 + tmp1*tmp1 + tmp2*tmp2)) / 
    (2 * (eigenValues[0]*eigenValues[0] + 
          eigenValues[1]*eigenValues[1] + 
          eigenValues[2]*eigenValues[2]));
  
  return sqrt( faSquare );
}






} // end namespace itk

#endif
