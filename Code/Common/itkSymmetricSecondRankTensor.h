/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSymmetricSecondRankTensor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSymmetricSecondRankTensor_h
#define __itkSymmetricSecondRankTensor_h

// Undefine an eventual SymmetricSecondRankTensor macro
#ifdef SymmetricSecondRankTensor
#undef SymmetricSecondRankTensor
#endif

#include <itkIndent.h>
#include <itkFixedArray.h>
#include <itkMatrix.h>
#include "vnl/vnl_math.h"

namespace itk
{

/** \class SymmetricSecondRankTensor
 * \brief Represent a symmetric tensor of second rank.
 *
 * This class implements a ND symmetric tensor of second rank.
 *
 * Since SymmetricSecondRankTensor is a subclass of FixedArray, 
 * you can access its components as:
 *
 * typedef itk::SymmetricSecondRankTensor< float >    TensorPixelType;
 * TensorPixelType tensor;
 * 
 *   tensor[0] = 1.233;
 *   tensor[1] = 1.456;
 *
 * for convenience the indexed access is also available as
 *
 *   tensor(0,0) = 1.233;
 *   tensor(2,0) = 1.233;
 *
 * The Tensor in principle represents a NxN matrix, but given that it is always
 * symmetric the representation can be compacted into a N*(N-1)/2 elements
 * array that derives from the itk::FixedArray<T>
 *
 * \author Torsten Rohlfing from SRI International Neuroscience Program.
 *
 * This class was mostly based on files that Torsten Rohlfing contributed to
 * the ITK users list during a discussion on support for DiffusionTensorImages.
 *
 * \ingroup ImageObjects
 */

template < typename TComponent = float, unsigned int NDimension=3 >
class SymmetricSecondRankTensor: public 
        FixedArray<TComponent,NDimension*(NDimension+1)/2>
{
public:
  /** Standard class typedefs. */
  typedef SymmetricSecondRankTensor  Self;
  typedef FixedArray<TComponent,NDimension*(NDimension+1)/2> SuperClass;
  
  /** Dimension of the vector space. */
  itkStaticConstMacro(Dimension, unsigned int, NDimension);
  itkStaticConstMacro(InternalDimension, unsigned int, NDimension*(NDimension+1)/2);

  /** Convenience typedefs. */
  typedef FixedArray<TComponent,InternalDimension> BaseArray;
  
  /** Array of eigen-values. */
  typedef FixedArray<TComponent, NDimension> EigenValuesArrayType;
  
  /** Matrix of eigen-vectors. */
  typedef Matrix<TComponent, NDimension, NDimension> MatrixType;
  typedef Matrix<TComponent, NDimension, NDimension> EigenVectorsMatrixType;
  
  /**  Define the component type. */
  typedef TComponent ComponentType;
  typedef typename SuperClass::ValueType ValueType;
  
  /** Default constructor has nothing to do. */
  SymmetricSecondRankTensor() {this->Fill(0);}
  SymmetricSecondRankTensor (const ComponentType& r) { this->Fill(r); }
  
  /** Pass-through constructor for the Array base class. */
  SymmetricSecondRankTensor(const Self& r): BaseArray(r) {}
  SymmetricSecondRankTensor(const ComponentType  r[InternalDimension]): BaseArray(r) {}  
    
  /** Pass-through assignment operator for the Array base class. */
  Self& operator= (const Self& r);
  Self& operator= (const ComponentType r[6]);

  /** Aritmetic operations between pixels. Return a new SymmetricSecondRankTensor. */
  Self operator+(const Self &vec) const;
  Self operator-(const Self &vec) const;
  const Self & operator+=(const Self &vec);
  const Self & operator-=(const Self &vec);
  Self operator*(const ComponentType &f) const;

 
  /** Return the number of components. */
  static int GetNumberOfComponents(){ return InternalDimension;}

  /** Return the value for the Nth component. */
  ComponentType GetNthComponent(int c) const
    { return this->operator[](c); }

  /** Set the Nth component to v. */
  void SetNthComponent(int c, const ComponentType& v)  
    {  this->operator[](c) = v; }

  /** Matrix notation, in const and non-const forms. */
  ValueType & operator()( unsigned int row, unsigned int col );
  const ValueType & operator()( unsigned int row, unsigned int col ) const;

  /** Return an array containing EigenValues, and a matrix containing eigen
   * vectors. */
  void ComputeEigenAnalysis( EigenValuesArrayType & eigenValues,
                             EigenVectorsMatrixType & eigenVectors ) const;
  
private:

  
};


template< typename TComponent, unsigned int NDimension  >  
ITK_EXPORT std::ostream& operator<<(std::ostream& os, 
              const SymmetricSecondRankTensor<TComponent,NDimension> & c); 
template< typename TComponent, unsigned int NDimension  >  
ITK_EXPORT std::istream& operator>>(std::istream& is, 
                    SymmetricSecondRankTensor<TComponent,NDimension> & c); 

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSymmetricSecondRankTensor.txx"
#endif

#endif
