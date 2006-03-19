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
  // Computed as 
  // FA = vcl_sqrt(1.5*sum(sum(N.*N))/sum((sum(D.*D))))
  // where N = D - ((1/3)*trace(D)*eye(3,3))
  // equation (28) in http://lmi.bwh.harvard.edu/papers/pdfs/2002/westinMEDIA02.pdf
  const RealValueType isp   = this->GetInnerScalarProduct();

  if( isp > 0.0 )
    {
    const RealValueType trace = this->GetTrace();
    const RealValueType anisotropy = 3.0 * isp - trace * trace;
    const RealValueType fractionalAnisotropy =
        static_cast< RealValueType >( vcl_sqrt(anisotropy / ( 2.0 * isp ) ) );
    return fractionalAnisotropy;
    }
 
   return 0.0 ;

   /*
   // Computed as 
   // FA = vcl_sqrt(1.5 * ( \sum_i ( lambda_i - lambda_mean )^2 ) / \sum_i ( lambda_i^2 ) )
   // as in http://splweb.bwh.harvard.edu:8000/pages/papers/martha/DTI_Tech354.pdf
   // [lambda = eig(A)].
   EigenValuesArrayType eigenValues;
   ComputeEigenValues( eigenValues );
   eigenValues[0] = vnl_math_abs(eigenValues[0]);
   eigenValues[1] = vnl_math_abs(eigenValues[1]);
   eigenValues[2] = vnl_math_abs(eigenValues[2]);
   const RealValueType norm_E = vnl_math_sqr(eigenValues[0]) 
                              + vnl_math_sqr(eigenValues[1]) 
                              + vnl_math_sqr(eigenValues[2]);

   if( norm_E > 0.0 )
     {
     const RealValueType anisotropy = 
                      vnl_math_sqr(eigenValues[0] - eigenValues[1]) +
                      vnl_math_sqr(eigenValues[1] - eigenValues[2]) +
                      vnl_math_sqr(eigenValues[2] - eigenValues[0]);
     const RealValueType fractionalAnisotropy = vcl_sqrt( 0.5 * anisotropy/norm_E);
     return fractionalAnisotropy;
     }

   return 0.0;
   */                 
   
}


/**
 *  Compute the value of relative anisotropy
 */
template<class T>
typename DiffusionTensor3D<T>::RealValueType
DiffusionTensor3D<T>
::GetRelativeAnisotropy() const
{
  const RealValueType trace = this->GetTrace();
  const RealValueType isp   = this->GetInnerScalarProduct();

  // Avoid negative trace and traces small enough to look like a division by
  // zero.
  if( trace < NumericTraits< RealValueType >::min() )
    {
    return NumericTraits<RealValueType>::Zero;
    }

  const RealValueType anisotropy = 3.0 * isp - trace * trace;

  if( anisotropy  < NumericTraits< RealValueType >::Zero )
    {
    return NumericTraits<RealValueType>::Zero;
    }

  const RealValueType relativeAnisotropySquared =
        static_cast< RealValueType >( anisotropy / ( vcl_sqrt(3.0 ) * trace ) ); 

  const RealValueType relativeAnisotropy =
        static_cast< RealValueType >( vcl_sqrt(relativeAnisotropySquared ) );

  return relativeAnisotropy;
}



/**
 *  Compute the inner scalar product
 */
template<class T>
typename DiffusionTensor3D<T>::RealValueType
DiffusionTensor3D<T>
::GetInnerScalarProduct() const
{

  const RealValueType xx = (*this)[0];
  const RealValueType xy = (*this)[1];
  const RealValueType xz = (*this)[2];
  const RealValueType yy = (*this)[3];
  const RealValueType yz = (*this)[4];
  const RealValueType zz = (*this)[5];

  return ( xx*xx + yy*yy + zz*zz + 2.0*(xy*xy + xz*xz + yz*yz) );

}



} // end namespace itk

#endif
