/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkUpwindDerivativeImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkUpwindDerivativeImageFunction_h
#define _itkUpwindDerivativeImageFunction_h

#include "itkImageFunction.h"

namespace itk
{

/** \class UpwindDerivativeImageFunction
 * \brief Calculate the derivative using only upwind neighbors.
 *
 * UpwindDerivativeImageFunction calculates a derivative using only upwind
 * neighbors. The flow direction can be specified by the sign of
 * the speed value set via the SetSpeed() method.
 *
 * If the speed is positive, the backward difference operator is
 * used. If speed is negative, the forward difference operator is
 * used.
 *
 * This class is templated over the input image type and the
 * coordinate representation type (e.g. float or double).
 *
 * Reference:
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Chapter 6, Second edition, 1999.
 *
 * Possible improvements:
 * - the use of Neighborhood operators may improve efficiency.
 *
 * \ingroup ImageFunctions
 */
template <class TInputImage, class TCoordRep = float >
class ITK_EXPORT UpwindDerivativeImageFunction :
  public ImageFunction< TInputImage, double, TCoordRep >
{
public:
  /** Standard class typedefs. */
  typedef UpwindDerivativeImageFunction Self;
  typedef ImageFunction<TInputImage, double,TCoordRep> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(UpwindDerivativeImageFunction, ImageFunction);

  /** InputImageType typedef support. */
  typedef TInputImage InputImageType;

  /** Dimension of the underlying image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      InputImageType::ImageDimension);

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;
 
  /** Set the input image. */
  virtual void SetInputImage( const InputImageType * ptr );

  /** Set the speed parameter. */
  virtual void SetSpeed( double value )
    { m_Speed = value; }

  /** Get the speed parameter. */
  virtual double GetSpeed() const
    { return m_Speed; }

  /** Evalulate the function at specified index. */
  virtual double EvaluateAtIndex( const IndexType& index ) const
    { return ( this->EvaluateNthDerivativeAtIndex( index, 0 ) ); }

  /** Evaluate the function at non-integer positions. */
  virtual double Evaluate( const PointType& point ) const
    { 
      IndexType index;
      this->ConvertPointToNearestIndex( point, index );
      return this->EvaluateAtIndex( index ); 
    }
  virtual double EvaluateAtContinuousIndex( 
    const ContinuousIndexType& cindex ) const
    { 
      IndexType index;
      this->ConvertContinuousIndexToNearestIndex( cindex, index );
      return this->EvaluateAtIndex( index ) ; 
    }
  
  /** Evalulate the Nth derivative at specified index. */
  virtual double EvaluateNthDerivativeAtIndex( const IndexType& index, 
                           unsigned int dim = 0 ) const;

  /** Get the derivative from last evaluation */
  virtual double GetDerivative() const
    { return m_Derivative; }

protected:
  UpwindDerivativeImageFunction(){};
  ~UpwindDerivativeImageFunction(){};
  void PrintSelf(std::ostream& os, Indent indent) const;

  double                  m_Speed;
  mutable double          m_Derivative;

private:
  UpwindDerivativeImageFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  signed long             m_ImageSize[ImageDimension];
  bool                    m_ImageSizeOK;

  mutable IndexType       m_NeighIndex;
  mutable double          m_CenterValue;
  mutable double          m_DiffValue;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkUpwindDerivativeImageFunction.txx"
#endif

#endif

