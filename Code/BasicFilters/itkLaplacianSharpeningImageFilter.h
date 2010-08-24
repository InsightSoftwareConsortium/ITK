/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLaplacianSharpeningImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLaplacianSharpeningImageFilter_h
#define __itkLaplacianSharpeningImageFilter_h

#include "itkNumericTraits.h"
#include "itkImageToImageFilter.h"
#include "itkImage.h"

namespace itk
{
/**
 * \class LaplacianSharpeningImageFilter
 *
 * This filter sharpens an image using a Laplacian. LaplacianSharpening
 * highlights regions of rapid intensity change and therefore
 * highlights or enhances the edges.  The result is an image that
 * appears more in focus.
 *
 * \par The LaplacianSharpening at each pixel location is computed by
 * convolution with the itk::LaplacianOperator.
 *
 * \par Inputs and Outputs
 * The input to this filter is a scalar-valued itk::Image of arbitrary
 * dimension. The output is a scalar-valued itk::Image.
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 * \sa LaplacianOperator
 *
 * \ingroup ImageFeatureExtraction */
template< class TInputImage, class TOutputImage >
class ITK_EXPORT LaplacianSharpeningImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard "Self" & Superclass typedef.   */
  typedef LaplacianSharpeningImageFilter                  Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  typedef typename TOutputImage::PixelType                    OutputPixelType;
  typedef typename TOutputImage::InternalPixelType            OutputInternalPixelType;
  typedef typename NumericTraits< OutputPixelType >::RealType RealType;
  typedef typename TInputImage::PixelType                     InputPixelType;
  typedef typename TInputImage::InternalPixelType             InputInternalPixelType;
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Image typedef support. */
  typedef TInputImage                      InputImageType;
  typedef TOutputImage                     OutputImageType;
  typedef typename InputImageType::Pointer InputImagePointer;

  /** Smart pointer typedef support.   */
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods)  */
  itkTypeMacro(LaplacianSharpeningImageFilter, ImageToImageFilter);

  /** Method for creation through the object factory.  */
  itkNewMacro(Self);

  /** LaplacianSharpeningImageFilter needs a larger input requested
   * region than the output requested region (larger in the direction
   * of the derivative).  As such, LaplacianSharpeningImageFilter
   * needs to provide an implementation for
   * GenerateInputRequestedRegion() in order to inform the pipeline
   * execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion()  */
  virtual void GenerateInputRequestedRegion()
  throw( InvalidRequestedRegionError );

  /** Use the image spacing information in calculations. Use this option if you
   *  want derivatives in physical space. Default is UseImageSpacingOn. */
  void SetUseImageSpacingOn()
  { this->SetUseImageSpacing(true); }

  /** Ignore the image spacing. Use this option if you want derivatives in
      isotropic pixel space.  Default is UseImageSpacingOn. */
  void SetUseImageSpacingOff()
  { this->SetUseImageSpacing(false); }

  /** Set/Get whether or not the filter will use the spacing of the input
      image in its calculations */
  itkSetMacro(UseImageSpacing, bool);
  itkGetConstMacro(UseImageSpacing, bool);
protected:
  LaplacianSharpeningImageFilter()
  {
    m_UseImageSpacing = true;
  }

  virtual ~LaplacianSharpeningImageFilter()  {}

  /** Standard pipeline method. While this class does not implement a
   * ThreadedGenerateData(), its GenerateData() delegates all
   * calculations to an NeighborhoodOperatorImageFilter.  Since the
   * NeighborhoodOperatorImageFilter is multithreaded, this filter is
   * multithreaded by default.   */
  void GenerateData();

  void PrintSelf(std::ostream &, Indent) const;
private:
  LaplacianSharpeningImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);                 //purposely not implemented

  bool m_UseImageSpacing;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLaplacianSharpeningImageFilter.txx"
#endif

#endif
