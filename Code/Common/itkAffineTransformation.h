/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAffineTransformation.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkAffineTransformation_h
#define __itkAffineTransformation_h

#include "itkObject.h"
#include "itkTransformation.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkVectorContainer.h"
#include "vnl/vnl_vector_fixed.h"
#include "vnl/vnl_matrix_fixed.h"

namespace itk
{
  
/** \class AffineTransformation derivated from the Super Class Transformation
 *  This class define a simple affine transform.
 * 
 *
 */

template <class TScalarType,int NDimensions>
class ITK_EXPORT  AffineTransformation : 
public Transformation<TScalarType, NDimensions >
{
public:

  /**
   * Standard "Self" typedef.
   */
  typedef AffineTransformation  Self;


  /**
   * Standard "Superclass" typedef.
   */
  typedef Transformation< TScalarType,NDimensions > Superclass;


  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   *  Vector type
   */
  typedef vnl_vector_fixed<TScalarType, NDimensions> VectorType;

  /**
   *  Matrix types
   */
  typedef vnl_matrix_fixed<TScalarType, NDimensions+1, NDimensions> MatrixType;

  typedef vnl_matrix_fixed<TScalarType, NDimensions, NDimensions>   LinearType;

  /**
   *  Point type
   */
	typedef itk::Point<TScalarType, NDimensions> PointType;

  /** 
   *  Run-time type information (and related methods).
   */
  itkTypeMacro(Self, Superclass);

  /**
   *  Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   *  Set the Matrix
   */
  void SetMatrix(const MatrixType &matrix);

  /**
   *  Set the Linear Matrix
   */
  void SetLinear(const LinearType &linear);

  /**
   *  Set the Offset Vector
   */
  void SetOffset(const VectorType &offset);
   
  /**
   *  Get the Matrix
   */
  itkGetMacro (Matrix, MatrixType);
  
  /**
   *  Get the Linear Matrix
   */
  itkGetMacro (Offset, VectorType);
  
  /**
   *  Get the Offset Vector
   */
  itkGetMacro (Linear, LinearType);

  /**
   *  Get the Matrix state
   */
  itkGetMacro (SetMatrix, bool);

   /**
   *  Get the Linear state
   */
  itkGetMacro (SetLinear, bool);

   /**
   *  Get the Offset state
   */
  itkGetMacro (SetOffset, bool);
  
  /**
   *  Perform the transformation
   */ 
  PointType Transform( PointType &initial_point);

  /**
   *  Constructor
   */
  AffineTransformation();

  /**
   *  Destructor
   */
  virtual ~AffineTransformation() {};

protected:

  AffineTransformation(const Self&);
  const Self & operator=(const Self&);

private:
	
  /**
	 * Matrix = Linear[n*n] + Offset[n]
	 */
  MatrixType   m_Matrix;       // Matrix of the transformation
  VectorType   m_Offset;       // Offset of the transformation
  LinearType   m_Linear;       // Linear of the transformation

  bool         m_SetMatrix;
  bool         m_SetOffset;
  bool         m_SetLinear;

};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAffineTransformation.txx"
#endif

#endif



