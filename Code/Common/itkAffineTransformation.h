/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAffineTransformation.h
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
#ifndef __itkAffineTransformation_h
#define __itkAffineTransformation_h

#include "itkObject.h"
#include "itkTransform.h"
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
public Transform<TScalarType, NDimensions >
{
public:

  /**
   * Standard "Self" typedef.
   */
  typedef AffineTransformation  Self;


  /**
   * Standard "Superclass" typedef.
   */
  typedef Transform< TScalarType,NDimensions > Superclass;


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



