/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFourierSeriesPath.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkFourierSeriesPath_h
#define _itkFourierSeriesPath_h

#include "itkParametricPath.h"
#include "itkVectorContainer.h"
#include "itkContinuousIndex.h"
#include "itkIndex.h"
#include "itkOffset.h"
#include "itkVector.h"

namespace itk
{


/** \class FourierSeriesPath
 * \brief  Represent a closed path through ND Space by its frequency components
 *
 * This class is intended to represent closed parametric paths through an image
 * which are defined by their Fourier coeficients (frequency components).  The
 * paths must be closed and defined over the interval [0,1], where the paths' 
 * values at input 0 and input 1 are identical.  The user can control how many
 * harmonics (how high of frequency components) are represented by a given
 * instantiation of this class.  Classic applications of this class include
 * smoothing other closed paths (by finding and using only the first n harmonics
 * of their frequency components) and interpolating exact derivatives of other
 * closed paths by first converting the other paths to FourierSeriesPaths, which
 * have an exactly defined algebraic derivative.  (As many harmonics as are
 * necessary to adequately approximate the original path should be used when
 * approximating derivatives.)
 *
 * \sa OrthogonallyCorrectedParametricPath
 * \sa EllipseParametricPath
 * \sa PolyLineParametricPath
 * \sa ParametricPath
 * \sa ChainCodePath
 * \sa Path
 * \sa ContinuousIndex
 * \sa Index
 * \sa Offset
 * \sa Vector
 *
 * \ingroup Paths
 */
template <unsigned int VDimension>
class ITK_EXPORT FourierSeriesPath : public
ParametricPath< VDimension >
{
public:
  /** Standard class typedefs. */
  typedef FourierSeriesPath           Self;
  typedef ParametricPath<VDimension>  Superclass;
  typedef SmartPointer<Self>          Pointer;
  typedef SmartPointer<const Self>    ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(FourierSeriesPath, ParametricPath);
  
  /** Input type */
  typedef typename Superclass::InputType  InputType;
  
  /** Output type */
  typedef typename Superclass::OutputType OutputType;
  
  
  /** Basic data-structure types used */
  typedef ContinuousIndex<double,VDimension>    ContinuousIndexType; 
  typedef Index<  VDimension >                  IndexType;           
  typedef Offset< VDimension >                  OffsetType;          
  typedef Vector<double,VDimension>             VectorType;          
  typedef VectorContainer<unsigned, VectorType> CoefficientsType;
  typedef typename CoefficientsType::Pointer    CoefficientsPointer;


  /** Return the location of the parametric path at the specified location. */
  virtual OutputType Evaluate( const InputType & input ) const;
  
  /** Evaluate the first derivative of the ND output with respect to the 1D
    * input.  This is an exact, algebraic function. */
  virtual VectorType EvaluateDerivative(const InputType & input) const;
  
  /** Add another harmonic's frequency coefficients. */
  void AddHarmonic( const VectorType & CosCoefficients,
                    const VectorType & SinCoefficients );
  
  /** Clear all frequency coefficients (including the "DC" coefficients). */
  void Clear()
    {
    m_CosCoefficients->Initialize();
    m_SinCoefficients->Initialize();
    this->Modified();
    }
  
  /** New() method for dynamic construction */
  itkNewMacro( Self );
  
  
protected:
  FourierSeriesPath();
  ~FourierSeriesPath(){}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  FourierSeriesPath(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  CoefficientsPointer m_CosCoefficients;
  CoefficientsPointer m_SinCoefficients;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFourierSeriesPath.txx"
#endif

#endif
