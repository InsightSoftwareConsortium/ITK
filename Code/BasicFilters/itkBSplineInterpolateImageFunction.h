/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineInterpolateImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkBSplineInterpolateImageFunction_h
#define __itkBSplineInterpolateImageFunction_h

#include <vector>

#include "itkImageLinearIteratorWithIndex.h"
#include "itkInterpolateImageFunction.h"
#include "vnl/vnl_matrix.h"

#include "itkBSplineDecompositionImageFilter.h"

namespace itk
{
/** \class BSplineInterpolateImageFunction
 * \brief Evaluates the B-Spline interpolation of an image.  Spline order may be from 0 to 5.
 *
 * This class defines N-Dimension B-Spline transformation.
 * It is based on: 
 *    [1] M. Unser,
 *       "Splines: A Perfect Fit for Signal and Image Processing,"
 *        IEEE Signal Processing Magazine, vol. 16, no. 6, pp. 22-38,
 *        November 1999.
 *    [2] M. Unser, A. Aldroubi and M. Eden,
 *        "B-Spline Signal Processing: Part I--Theory,"
 *        IEEE Transactions on Signal Processing, vol. 41, no. 2, pp. 821-832,
 *        February 1993.
 *    [3] M. Unser, A. Aldroubi and M. Eden,
 *        "B-Spline Signal Processing: Part II--Efficient Design and Applications,"
 *        IEEE Transactions on Signal Processing, vol. 41, no. 2, pp. 834-848,
 *        February 1993.
 * And code obtained from bigwww.epfl.ch by Philippe Thevenaz
 *
 * The B spline coefficients are calculated through the 
 * BSplineDecompositionImageFilter
 * 
 * Limitations:  Spline order must be between 0 and 5.
 *               Spline order must be set before setting the image.
 *               Uses mirror boundary conditions.
 *               Requires the same order of Spline for each dimension.
 *               Spline is determined in all dimensions, cannot selectively
 *                  pick dimension for calculating spline.
 *
 * \sa BSplineDecompositionImageFilter
 *
 * \ingroup 
 */
template <class TImageType, class TCoordRep = float>
class ITK_EXPORT BSplineInterpolateImageFunction : 
  public InterpolateImageFunction<TImageType,TCoordRep> 
{
public:
  /** Standard class typedefs. */
  typedef BSplineInterpolateImageFunction       Self;
  typedef InterpolateImageFunction<TImageType,TCoordRep>  Superclass;
  typedef SmartPointer<Self>                    Pointer;
  typedef SmartPointer<const Self>              ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineInterpolateImageFunction, InterpolateImageFunction);

 
  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** PointType typedef support */
  typedef typename Superclass::PointType PointType;

  /** Iterator typedef support */
  typedef itk::ImageLinearIteratorWithIndex<TImageType> Iterator;

  /** Internal Coeffient typedef support */
  typedef double CoeffientDataType;
  typedef itk::Image<CoeffientDataType, ImageDimension> CoeffientImageType;

  /** Define filter for calculating the BSpline coefficients */
  typedef itk::BSplineDecompositionImageFilter<TImageType, CoeffientImageType> 
    CoefficientFilter;
  typedef typename CoefficientFilter::Pointer CoefficientFilterPointer;

   /** Evaluate the function at a ContinuousIndex position.
   *
   * Returns the B-Spline interpolated image intensity at a 
   * specified point position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual OutputType EvaluateAtContinuousIndex( 
      const ContinuousIndexType & index ) const; 

  /** Derivative typedef support */
  typedef CovariantVector<OutputType,itkGetStaticConstMacro(ImageDimension)>
      CovariantVectorType;

  CovariantVectorType EvaluateDerivative( const PointType & point ) const
    {    
    ContinuousIndexType index;
    this->ConvertPointToContinuousIndex( point, index );
    return ( this->EvaluateDerivativeAtContinuousIndex( index ) );
    } 

  CovariantVectorType EvaluateDerivativeAtContinuousIndex( 
    const ContinuousIndexType & x ) const;


  /** Get/Sets the Spline Order, supports 0th - 5th order splines. The default
   *  is a 3rd order spline. */
  void SetSplineOrder(unsigned int SplineOrder);
  itkGetMacro(SplineOrder, int);


  /** Set the input image.  This must be set by the user. */
  virtual void SetInputImage(const TImageType * inputData);

protected:
  BSplineInterpolateImageFunction();
  virtual ~BSplineInterpolateImageFunction() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

    // These are needed by the smoothing spline routine.
  std::vector<CoeffientDataType>    m_Scratch;        // temp storage for processing of Coefficients
  typename TImageType::SizeType      m_DataLength;  // Image size
  unsigned int                       m_SplineOrder; // User specified spline order (3rd or cubic is the default)

  typename CoeffientImageType::Pointer       m_Coefficients; // Spline coefficients  

private:
  /** Determines the weights for interpolation of the value x */
  void SetInterpolationWeights( const ContinuousIndexType & x, 
    const vnl_matrix<long> & EvaluateIndex, 
    vnl_matrix<double> & weights, 
    unsigned int splineOrder ) const;

  /** Determines the weights for the derivative portion of the value x */
  void SetDerivativeWeights( const ContinuousIndexType & x, 
    const vnl_matrix<long> & EvaluateIndex, 
    vnl_matrix<double> & weights, 
    unsigned int splineOrder ) const;

  /** Precomputation for converting the 1D index of the interpolation neighborhood 
    * to an N-dimensional index. */
  void GeneratePointsToIndex(  );

  /** Determines the indicies to use give the splines region of support */
  void DetermineRegionOfSupport( vnl_matrix<long> & evaluateIndex, 
    const ContinuousIndexType & x, 
    unsigned int splineOrder ) const;

  /** Set the indicies in evaluateIndex at the boundaries based on mirror 
    * boundary conditions. */
  void ApplyMirrorBoundaryConditions(vnl_matrix<long> & evaluateIndex, 
    unsigned int splineOrder) const;


  Iterator                  m_CIterator;    // Iterator for traversing spline coefficients.
  unsigned long             m_MaxNumberInterpolationPoints; // number of neighborhood points used for interpolation
  std::vector<IndexType>    m_PointsToIndex;  // Preallocation of interpolation neighborhood indicies

  BSplineInterpolateImageFunction( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented
  CoefficientFilterPointer     m_CoefficientFilter;
  
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBSplineInterpolateImageFunction.txx"
#endif

#endif

