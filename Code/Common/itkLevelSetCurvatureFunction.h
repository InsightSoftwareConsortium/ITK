/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetCurvatureFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkLevelSetCurvatureFunction_h
#define _itkLevelSetCurvatureFunction_h

#include "itkImageFunction.h"

#include "vnl/vnl_vector_fixed.h"
#include "vnl/vnl_matrix_fixed.h"

namespace itk
{

/** \class LevelSetCurvatureFunction
 * \brief Calculate the mean curvature of a level set at a specified index.
 *
 * LevelSetCurvatureFunction calculates the mean curvature of a level set
 * at a specified index. This class is templated over the input
 * image type and the coordinate representation type (e.g. float or double).
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
class ITK_EXPORT LevelSetCurvatureFunction :
  public ImageFunction<TInputImage,double,TCoordRep>
{
public:
  /** Standard class typedefs. */
  typedef LevelSetCurvatureFunction Self;
  typedef ImageFunction<TInputImage,double,TCoordRep> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(LevelSetCurvatureFunction, ImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

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

  /** Set the magnitude epsilon parameter. This parameter is to guard
   * against division by zero. If the magnitude is smaller than
   * this threshold value, the curvature is assumed to be zero.
   * The default value is 1e-9. */
  void SetEpsilonMagnitude( double value )
    { m_EpsilonMagnitude = value; }

  /** Get the magnitude epsilon parameter. */
  double GetEpsilonMagnitude() const
    { return m_EpsilonMagnitude; }

  /** Evaluate the function at specified index */
  virtual double EvaluateAtIndex( const IndexType& index ) const;

  /** Evaluate the function at non-integer positions */
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
  
  /** Get the curvature from last evaluation */
  double GetCurvature() const
    { return m_Curvature; }

  /** Get the gradient magnitude from last evaluation */
  double GetMagnitude() const
    { return m_Magnitude; }

protected:
  LevelSetCurvatureFunction(){}
  ~LevelSetCurvatureFunction(){}
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  LevelSetCurvatureFunction( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

  signed long                                 m_ImageSize[ImageDimension];
  bool                                        m_ImageSizeOK;

  mutable double                              m_Curvature;
  mutable double                              m_Magnitude;
  double                                      m_EpsilonMagnitude;
  mutable bool                                m_BorderPixel;

  mutable vnl_vector_fixed<double,itkGetStaticConstMacro(ImageDimension)>     
                                              m_FirstDerivative;
  mutable vnl_matrix_fixed<double,itkGetStaticConstMacro(ImageDimension),
                           itkGetStaticConstMacro(ImageDimension)>
                                              m_SecondDerivative;

  mutable IndexType                           m_NeighIndex;
  mutable IndexType                           m_RightIndex;
  mutable IndexType                           m_LeftIndex;
  mutable double                              m_CenterValue;
  mutable double                              m_DiffValue;

  mutable vnl_matrix_fixed<unsigned int,itkGetStaticConstMacro(ImageDimension),
                           itkGetStaticConstMacro(ImageDimension)>
                                                            m_Variable;

  void CalculateDerivatives( const IndexType& index ) const;
  void CalculateCurvature() const;
  void CalculateCurvature2D() const;
  void CalculateCurvature3D() const;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetCurvatureFunction.txx"
#endif

#endif
