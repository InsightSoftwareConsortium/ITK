/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSymmetricEigenSystem.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSymmetricEigenSystem_h
#define __itkSymmetricEigenSystem_h

#include "vnl/algo/vnl_symmetric_eigensystem.h"
#include "itkFixedArray.h"
#include "itkMatrix.h"

namespace itk
{
  
/** \class SymmetricEigenSystem
 * \brief wrapper of the vnl_symmetric_eigensystem algorithm
 *
 * \ingroup Numerics
 */

template< class TMatrixElement, int VNumberOfRows >
class ITK_EXPORT SymmetricEigenSystem : public Object
{
public:
  /** Standard "Self" typedef. */
  typedef SymmetricEigenSystem Self;
  typedef Object Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro( SymmetricEigenSystem, Object );

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** 1D array typedef */
  typedef FixedArray< TMatrixElement, VNumberOfRows > ArrayType;

  /** 2D array typedef */
  typedef FixedArray< ArrayType, VNumberOfRows > Array2DType ;

  /** Array for eigen vectors */
  typedef Array2DType EigenVectorArrayType ;

  /** Array type for eigen values */
  typedef ArrayType EigenValueArrayType ;

  /** Matrix Type */
  typedef Matrix< TMatrixElement, VNumberOfRows, VNumberOfRows > MatrixType ;

  /** Internal eigen sytem type. */
  typedef vnl_symmetric_eigensystem< TMatrixElement > InternalEigenSystemType;

  /** Set/Get the target matrix for the eigen analysis */
  itkSetObjectMacro(Matrix, MatrixType) ;
  MatrixType* GetMatrix()
  { return m_Matrix ; }

  /** Set/Get the absolute order flag.
   * By setting this flag true, after the calculation of eigen vectors 
   * and values, if the absolute eigen value of eigen vector[j > i] is
   * greater that of eigen vector[i], reorder the eigen vectors so that
   * every absolute eigen values of eigen vector[j < i] is always greater than or
   * equal to that of the eigen vectors[i] */
  itkSetMacro(UseAbsoluteOrder, bool) ;
  itkGetMacro(UseAbsoluteOrder, bool) ;

  /** returns the eigen vectors in a 2D array */
  EigenVectorArrayType* GetEigenVectors()
  { return &m_EigenVectors ; } 

  /** returns the eigen values in an 1D array */
  EigenValueArrayType* GetEigenValues()
  { return &m_EigenValues ; }

  /** dummy method that calls the GenerateData method to 
   * produce the eigen vectors and values. */
  void Update()
  { this->GenerateData() ; }

protected:
  SymmetricEigenSystem();
  virtual ~SymmetricEigenSystem();
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Produces the eigen vectors and values. */
  void GenerateData() ;

private:
  SymmetricEigenSystem(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** the target matrix */
  MatrixType* m_Matrix ;

  /** eigen vectors output */
  EigenVectorArrayType m_EigenVectors ;
  
  /** eigen values output */
  EigenValueArrayType m_EigenValues ;

  /** flag for absolute ordering of eigen vectors and
   * eigen values */
  bool m_UseAbsoluteOrder ;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSymmetricEigenSystem.txx"
#endif

#endif



