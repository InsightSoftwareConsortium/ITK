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

//#include "itkKernelTransform.h"
#include <vector>
//#include <math.h>

#include "itkImageLinearIteratorWithIndex.h"
#include "itkInterpolateImageFunction.h"
#include "vnl/vnl_matrix.h"

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
 * Limitations:  Spline order must be between 0 and 5.
 *               Spline order must be set before setting the image.
 *               Uses mirror boundary conditions.
 *               Requires the same order of Spline for each dimension.
 *
 * \ingroup 
 */
template <class TImageType>
class ITK_EXPORT BSplineInterpolateImageFunction : 
  public InterpolateImageFunction<TImageType> 
{
public:
  /** Standard class typedefs. */
  typedef BSplineInterpolateImageFunction       Self;
  typedef InterpolateImageFunction<TImageType>  Superclass;
  typedef SmartPointer<Self>                    Pointer;
  typedef SmartPointer<const Self>              ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineInterpolateImageFunction, InterpolateImageFunction);

 
  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** Dimension underlying input image. */
  enum { ImageDimension = Superclass::ImageDimension };

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Iterator typedef support */
  typedef itk::ImageLinearIteratorWithIndex<TImageType> Iterator;

   /** Evaluate the function at a ContinuousIndex position.
   *
   * Returns the B-Spline interpolated image intensity at a 
   * specified point position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual double EvaluateAtContinuousIndex( 
      const ContinuousIndexType & index ) const; 

  /** Get/Sets the Spline Order, supports 0th - 5th order splines. The default
   *  is a 3rd order spline. */
  void SetSplineOrder(int SplineOrder);
  itkGetMacro(SplineOrder, int);


  /** Set the input image.  This must be set by the user. */
  virtual void SetInputImage(const TImageType * inputData);

protected:
  BSplineInterpolateImageFunction();
  virtual ~BSplineInterpolateImageFunction() {};
  void PrintSelf(std::ostream& os, Indent indent) const;
    
private:
  /** Determines the poles given the Spline Order. */
  virtual void SetPoles();

  /** Converts a vector of data to a vector of Spline coefficients. */
  virtual bool DataToCoefficients1D();

  /** Converts an N-dimension image of data to an equivalent sized image
   *    of spline coefficients. */
  void DataToCoefficientsND();

  /** Determines the first coefficient for the causal filtering of the data. */
  virtual void SetInitialCausalCoefficient(double z);

  /** Determines the first coefficient for the anti-causal filtering of the data. */
  virtual void SetInitialAntiCausalCoefficient(double z);

  /** Determines the weights for interpolation of the value x */
  void SetInterpolationWeights( const ContinuousIndexType & x, const vnl_matrix<long> & EvaluateIndex, vnl_matrix<double> & weights ) const;

  /** Used to initialize the Coefficients image before calculation. */
  void CopyImageToImage(const TImageType * input, TImageType * output );

  /** Copies a vector of data from the Coefficients image to the m_Scratch vector. */
  void CopyCoefficientsToScratch( Iterator & );

  /** Copies a vector of data from m_Scratch to the Coefficients image. */
  void CopyScratchToCoefficients( Iterator & );

  /** Precomputation for converting the 1D index of the interpolation neighborhood 
    * to an N-dimensional index. */
  void GeneratePointsToIndex(  );

  // These are needed by the smoothing spline routine.
protected:
  std::vector<double>    m_Scratch;        // temp storage for processing of Coefficients
  typename TImageType::SizeType      m_DataLength;  // Image size
  int                       m_SplineOrder; // User specified spline order (3rd or cubic is the default)
  double                    m_SplinePoles[3];  // Poles calculated for a given spline order
  int                       m_NumberOfPoles;   // number of poles
  double                    m_Tolerance;   // Tolerance used for determining initial causal coefficient
  unsigned int              m_IteratorDirection; // Direction for iterator incrementing
  typename TImageType::Pointer       m_Coefficients; // Spline coefficients

private:
  Iterator                  m_CIterator;    // Iterator for traversing spline coefficients.
  unsigned long             m_MaxNumberInterpolationPoints; // number of neighborhood points used for interpolation
  std::vector<IndexType>    m_PointsToIndex;  // Preallocation of interpolation neighborhood indicies

private:
  BSplineInterpolateImageFunction( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented
  
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBSplineInterpolateImageFunction.txx"
#endif

#endif

