/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineDecompositionImageFilter.h
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

#ifndef __itkBSplineDecompositionImageFilter_h
#define __itkBSplineDecompositionImageFilter_h

#include <vector>

#include "itkImageLinearIteratorWithIndex.h"
#include "vnl/vnl_matrix.h"

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class itkBSplineDecompositionImageFilter
 * \brief Calculates the B-Spline coefficients of an image. Spline order may be from 0 to 5.
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
 *               Can only process LargestPossibleRegion
 *
 * \sa itkBSplineInterpolateImageFunction
 *
 *  ***TODO: Is this an ImageFilter?  or does it belong to another group?
 * \ingroup ImageFilters
 * \ingroup SingleThreaded
 * \ingroup CannotBeStreamed 
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT BSplineDecompositionImageFilter : 
  public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef BSplineDecompositionImageFilter       Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>                    Pointer;
  typedef SmartPointer<const Self>              ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineDecompositionImageFilter, ImageToImageFilter);

 
  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** Output and Input ImagePointer typedef support. */
  typedef typename TOutputImage::Pointer OutputImagePointer;
  typedef typename TInputImage::Pointer InputImagePointer;

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** Dimension underlying input image. */
  enum { ImageDimension = TInputImage::ImageDimension };

  /** Iterator typedef support */
  typedef itk::ImageLinearIteratorWithIndex<TOutputImage> OutputLinearIterator;


  /** Get/Sets the Spline Order, supports 0th - 5th order splines. The default
   *  is a 3rd order spline. */
  void SetSplineOrder(unsigned int SplineOrder);

  itkGetMacro(SplineOrder, int);

  /** Set the input image.  This must be set by the user. */
  virtual void SetInput(const TInputImage * inputData);
  virtual void SetInput( unsigned int, const TInputImage * image);

  /** This filter requires all of the input image */
  void GenerateInputRequestedRegion();

protected:
  BSplineDecompositionImageFilter();
  virtual ~BSplineDecompositionImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateData( );
  void EnlargeOutputRequestedRegion( DataObject *output ); 
  void GenerateOutputInformation();
  
private:
  /** Determines the poles given the Spline Order. */
  virtual void SetPoles();

  /** Converts a vector of data to a vector of Spline coefficients. */
  virtual bool DataToCoefficients1D();

  /** Converts an N-dimension image of data to an equivalent sized image
   *    of spline coefficients. */
  void DataToCoefficientsND(InputImagePointer inPtr, OutputImagePointer outPtr);

  /** Determines the first coefficient for the causal filtering of the data. */
  virtual void SetInitialCausalCoefficient(double z);

  /** Determines the first coefficient for the anti-causal filtering of the data. */
  virtual void SetInitialAntiCausalCoefficient(double z);

  /** Used to initialize the Coefficients image before calculation. */
  void CopyImageToImage(const TInputImage * input, TOutputImage * output );

  /** Copies a vector of data from the Coefficients image to the m_Scratch vector. */
  void CopyCoefficientsToScratch( OutputLinearIterator & );

  /** Copies a vector of data from m_Scratch to the Coefficients image. */
  void CopyScratchToCoefficients( OutputLinearIterator & );

  // These are needed by the smoothing spline routine.
protected:
  std::vector<double>       m_Scratch;       // temp storage for processing of Coefficients
  typename TInputImage::SizeType   m_DataLength;  // Image size
  unsigned int              m_SplineOrder;   // User specified spline order (3rd or cubic is the default)
  double                    m_SplinePoles[3];// Poles calculated for a given spline order
  int                       m_NumberOfPoles; // number of poles
  double                    m_Tolerance;     // Tolerance used for determining initial causal coefficient
  unsigned int              m_IteratorDirection; // Direction for iterator incrementing

private:
  BSplineDecompositionImageFilter( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented
  
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBSplineDecompositionImageFilter.txx"
#endif

#endif

