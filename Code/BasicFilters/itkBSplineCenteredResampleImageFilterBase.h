/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineCenteredResampleImageFilterBase.h
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

#ifndef __itkBSplineCenteredResampleImageFilterBase_h
#define __itkBSplineCenteredResampleImageFilterBase_h

#include "itkBSplineResampleImageFilterBase.h"
#include <vector>


namespace itk
{
/** \class BSplineCenteredResampleImageFilterBase
 * \brief Evaluates the Centered B-Spline interpolation of an image.  Spline order may be from 0 to 5.
 *
 * This class defines N-Dimension CenteredB-Spline transformation.
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
class ProgressReporter;

template <class TInputImage, class TOutputImage>
class ITK_EXPORT BSplineCenteredResampleImageFilterBase : 
  public BSplineResampleImageFilterBase<TInputImage, TOutputImage>  
{
public:
  /** Standard class typedefs. */
  typedef BSplineCenteredResampleImageFilterBase       Self;
  typedef BSplineResampleImageFilterBase<TInputImage, TOutputImage>  Superclass;
  typedef SmartPointer<Self>                    Pointer;
  typedef SmartPointer<const Self>              ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineCenteredResampleImageFilterBase, BSplineResampleImageFilterBase);

 
  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** OutputImagePixelType typedef support. */
  typedef typename Superclass::OutputImagePixelType OutputImagePixelType;

  /** OutputImageIterator typedef support. */
  typedef typename Superclass::OutputImageIterator OutputImageIterator;

protected:

  virtual void InitializePyramidSplineFilter(int SplineOrder);
  virtual void Reduce1DImage( 
        const std::vector<double> & In, 
        OutputImageIterator & Iter, 
        unsigned int traverseSize,
        ProgressReporter &progress
        );

  virtual void Expand1DImage( 
        const std::vector<double> & In, 
        OutputImageIterator & Iter, 
        unsigned int traverseSize,
        ProgressReporter &progress
        );

protected:
  BSplineCenteredResampleImageFilterBase();
  virtual ~BSplineCenteredResampleImageFilterBase() {};
  void PrintSelf(std::ostream& os, Indent indent) const;
    

private:
  BSplineCenteredResampleImageFilterBase( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented
  
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBSplineCenteredResampleImageFilterBase.txx"
#endif

#endif
