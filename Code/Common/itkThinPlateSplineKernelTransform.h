/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThinPlateSplineKernelTransform.h
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
#ifndef __itkThinPlateSplineKernelTransform_h
#define __itkThinPlateSplineKernelTransform_h

#include "itkKernelTransform.h"

namespace itk
{

/** \class ThinPlateSplineKernelTransform
 * This class defines the thin plate spline (TPS) transformation.
 * It is implemented in as straightforward a manner as possible from
 * the IEEE TMI paper by Davis, Khotanzad, Flamig, and Harms,
 * Vol. 16 No. 3 June 1997
 *
 * \ingroup Transforms
 */

template <class TScalarType,         // Data type for scalars (float or double)
          int NDimensions = 3>          // Number of dimensions
class ThinPlateSplineKernelTransform : 
    public KernelTransform<   TScalarType, 
                              NDimensions,
                              vnl_vector<TScalarType>, 
                              vnl_matrix<TScalarType>    >
{
public:
  /**
   * Standard Self typedef
   */
  typedef ThinPlateSplineKernelTransform Self;


  /**
   * Scalar Type
   */
  typedef typename Superclass::ScalarType  ScalarType;


  /**
   * Parameters Type
   */
  typedef typename Superclass::ParametersType  ParametersType;


  /**
   * Jacobian Type
   */
  typedef typename Superclass::JacobianType  JacobianType;



  /**
   * Standard Superclass typedef
   */
  typedef KernelTransform<    TScalarType,
                              NDimensions,
                              vnl_vector< TScalarType >,
                              vnl_matrix< TScalarType >    > Superclass;


  /// Dimension of the domain space
  enum { SpaceDimension = Superclass::SpaceDimension };



  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( ThinPlateSplineKernelTransform, KernelTransform );


  /** 
   * New macro for creation of through a Smart Pointer
   */
  itkNewMacro( Self );


                              
  /**
   * These (rather redundant) typedefs are needed because on SGI, typedefs
   * are not inherited
   */
  typedef typename Superclass::InputPointType  InputPointType;
  typedef typename Superclass::OutputPointType  OutputPointType;
  typedef typename Superclass::InputVectorType InputVectorType;
  typedef typename Superclass::OutputVectorType OutputVectorType;
  typedef typename Superclass::InputCovariantVectorType InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType OutputCovariantVectorType;
 

  
protected:
  
  
  /**
   * Default constructor
   */
  ThinPlateSplineKernelTransform();

  
  /**
   * Destructor
   */
  virtual ~ThinPlateSplineKernelTransform();

  
  /**
   * These (rather redundant) typedefs are needed because on SGI, typedefs
   * are not inherited
   */
  typedef typename Superclass::GMatrixType GMatrixType;

  
  /**
   * Compute G(x)
   * For the thin plate spline, this is:
   * G(x) = r(x)*I
   * where
   * r(x) = Euclidean norm = sqrt[x1^2 + x2^2 + x3^2]
   * I = identity matrix
   */
  GMatrixType ComputeG(const InputVectorType & x) const;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkThinPlateSplineKernelTransform.txx"
#endif

#endif // __itkThinPlateSplineKernelTransform_h
