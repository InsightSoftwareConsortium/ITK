/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOrthogonallyCorrected2DParametricPath.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkOrthogonallyCorrected2DParametricPath_h
#define __itkOrthogonallyCorrected2DParametricPath_h

#include "itkParametricPath.h"
#include "itkVectorContainer.h"
#include "itkContinuousIndex.h"
#include "itkIndex.h"
#include "itkOffset.h"
#include "itkVector.h"

namespace itk
{

/** \class OrthogonallyCorrected2DParametricPath
 * \brief  Represent an orthogonally corrected 2D parametric path
 *
 * Description
 *
 * \sa EllipseParametricPath
 * \sa PolyLineParametricPath
 * \sa ParametricPath
 * \sa Path
 * \sa ContinuousIndex
 * \sa Index
 * \sa Offset
 * \sa Vector
 *
 * \ingroup PathObjects
 */
class ITKCommon_EXPORT OrthogonallyCorrected2DParametricPath : public
ParametricPath<2>
{
public:
  /** Standard class typedefs. */
  typedef OrthogonallyCorrected2DParametricPath   Self;
  typedef ParametricPath<2>                       Superclass;
  typedef SmartPointer<Self>                      Pointer;
  typedef SmartPointer<const Self>                ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(OrthogonallyCorrected2DParametricPath, ParametricPath);
  
  /** Input type */
  typedef Superclass::InputType  InputType;
  
  /** Output type */
  typedef Superclass::OutputType OutputType;
  
  
  /** Basic data-structure types used */
  typedef ContinuousIndex<double,2>             ContinuousIndexType; 
  typedef Index<2>                              IndexType;           
  typedef Offset<2>                             OffsetType;          
  typedef VectorContainer<unsigned, double>     OrthogonalCorrectionTableType;
  typedef OrthogonalCorrectionTableType::Pointer OrthogonalCorrectionTablePointer;


  /** Return the location of the parametric path at the specified location. */
  virtual OutputType Evaluate( const InputType & input ) const;
  
  /** Evaluate the first derivative of the ND output with respect to the 1D
    * input.  This is an exact, algebraic function. */
  virtual VectorType EvaluateDerivative(const InputType & input) const;
  
  /** Add another harmonic's frequency coefficients. */
  void SetOrthogonalCorrectionTable( const OrthogonalCorrectionTablePointer
                                      orthogonalCorrectionTable );
  
  /** Clear all frequency coefficients (including the "DC" coefficients). */
  void Clear()
    {
    m_OrthogonalCorrectionTable->Initialize();
    this->Modified();
    }
  
  /** New() method for dynamic construction */
  itkNewMacro( Self );
  
    /** Needed for Pipelining */
  virtual void Initialize(void)
    {
    this->Clear();
    }
  
protected:
  OrthogonallyCorrected2DParametricPath();
  ~OrthogonallyCorrected2DParametricPath(){}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  OrthogonallyCorrected2DParametricPath(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  OrthogonalCorrectionTablePointer m_OrthogonalCorrectionTable;
};

} // namespace itk

#endif
