/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkLabelImageGaussianInterpolateImageFunction.h,v $
  Language:  C++
  Date:      $Date: $
  Version:   $Revision: $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLabelImageGaussianInterpolateImageFunction_h
#define __itkLabelImageGaussianInterpolateImageFunction_h

#include "itkGaussianInterpolateImageFunction.h"

namespace itk
{

/** \class LabelImageGaussianInterpolateImageFunction
 * \brief Interpolation function for multi-label images that implicitly smooths each
 * unique value in the image corresponding to each label set element and returns the
 * corresponding label set element with the largest wieght.
 *
 * This filter is an alternative to nearest neighbor interpolation for multi-label
 * images. Given a multi-label image \c I with label set \c L, this function returns a
 * label at the non-voxel position \c I(x), based on the following rule
 *
 * \f[
 * I(x) = \arg\max_{l \in L} (G_\sigma * I_l)(x)
 * \f]
 *
 * Where \f$ I_l \f$ is the \c l-th binary component of the multilabel image. In other words,
 * each label in the multi-label image is convolved with a Gaussian, and the label
 * for which the response is largest is returned. For sigma=0, this is just nearest
 * neighbor interpolation.
 *
 * This class defines an N-dimensional Gaussian interpolation function for label
 * using the vnl error function.  The two parameters associated with this function
 * are:
 * \li \c Sigma - a scalar array of size ImageDimension determining the width
 *      of the interpolation function.
 * \li \c Alpha - a scalar specifying the cutoff distance over which the function
 *      is calculated.
 *
 * \note The input image can be of any type, but the number of unique intensity values
 * in the image will determine the amount of memory needed to complete each interpolation.
 *
 *
 * \author Paul Yushkevich
 * \author Nick Tustison
 *
 * \ingroup ITKImageFunction
 */

template <typename TInputImage, typename TCoordRep = double,
          typename TPixelCompare = std::less<typename itk::NumericTraits<typename TInputImage::PixelType>::RealType> >
class LabelImageGaussianInterpolateImageFunction :
  public GaussianInterpolateImageFunction<TInputImage, TCoordRep>
{
public:
  /** Standard class typedefs. */
  typedef LabelImageGaussianInterpolateImageFunction                Self;
  typedef GaussianInterpolateImageFunction<TInputImage, TCoordRep>  Superclass;
  typedef SmartPointer<Self>                                        Pointer;
  typedef SmartPointer<const Self>                                  ConstPointer;
  typedef typename TInputImage::PixelType                           InputPixelType;

  /** Run-time type information (and related methods). */
  itkTypeMacro( LabelImageGaussianInterpolateImageFunction, GaussianInterpolateImageFunction );

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** ImageDimension constant */
  itkStaticConstMacro( ImageDimension, unsigned int, TInputImage::ImageDimension );

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** RealType typedef support. */
  typedef typename Superclass::RealType RealType;

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Array typedef support */
  typedef typename Superclass::ArrayType ArrayType;

  /**
   * Evaluate at the given index
   */
  virtual OutputType EvaluateAtContinuousIndex(
    const ContinuousIndexType & cindex ) const
    {
    return this->EvaluateAtContinuousIndex( cindex, ITK_NULLPTR );
    }

protected:
  LabelImageGaussianInterpolateImageFunction();
  ~LabelImageGaussianInterpolateImageFunction(){};

private:
  LabelImageGaussianInterpolateImageFunction( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

  /**
   * Evaluate function value at the given index
   */
  virtual OutputType EvaluateAtContinuousIndex(
    const ContinuousIndexType &, OutputType * ) const;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelImageGaussianInterpolateImageFunction.hxx"
#endif

#endif
