/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientImageToBloxBoundaryPointImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGradientImageToBloxBoundaryPointImageFilter_h
#define __itkGradientImageToBloxBoundaryPointImageFilter_h

#include "itkBloxBoundaryPointImage.h"
#include "itkImageToImageFilter.h"
#include "itkSize.h"

namespace itk
{

/** \class GradientImageToBloxBoundaryPointImageFilter
 * \brief Converts a gradient image to an BloxImage of BloxBoundaryPoints
 *
 * Thresholds the magnitude of a gradient image to produce
 * a BloxBoundaryPointImage
 * 
 * \ingroup ImageEnhancement
 */
template<typename TInputImage>
class ITK_EXPORT GradientImageToBloxBoundaryPointImageFilter :
    public ImageToImageFilter<TInputImage, BloxBoundaryPointImage< ::itk::GetImageDimension<TInputImage>::ImageDimension> >
{
public:
  /** Number of dimensions */
  itkStaticConstMacro(NDimensions, unsigned int, TInputImage::ImageDimension);

  /** Standard class typedefs. */
  typedef GradientImageToBloxBoundaryPointImageFilter Self;
  typedef ImageToImageFilter<TInputImage,
                             BloxBoundaryPointImage<itkGetStaticConstMacro(NDimensions)> > Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( GradientImageToBloxBoundaryPointImageFilter, ImageToImageFilter );

  /** typedef for images */
  typedef TInputImage                             InputImageType;
  typedef BloxBoundaryPointImage<itkGetStaticConstMacro(NDimensions)>     TOutputImage;
  typedef BloxBoundaryPointImage<itkGetStaticConstMacro(NDimensions)>     OutputImageType;
  typedef typename OutputImageType::Pointer       OutputImagePointer;
  typedef typename InputImageType::Pointer        InputImagePointer;
  typedef typename InputImageType::ConstPointer   InputImageConstPointer;

  /** Image size typedef */
  typedef Size<itkGetStaticConstMacro(NDimensions)> SizeType;

  /** Image index typedef */
  typedef typename TOutputImage::IndexType IndexType;

  /** Image pixel value typedef */
  typedef typename TOutputImage::PixelType PixelType;

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** The type of vector used to convert between physical and blox space */
  typedef Point<double, itkGetStaticConstMacro(NDimensions)> TPositionType;

  /** Get and set the number of times to repeat the filter. */
  itkSetMacro(Threshold, double);
  itkGetMacro(Threshold, double);

  /** Get and set the resolution of the blox
   *  This is the number of input pixels "contained" within
   *  each blox pixel
   */
  void SetBloxResolution( float bloxResolution[] );
  void SetBloxResolution( float bloxResolution );

  void GenerateInputRequestedRegion();

  virtual void GenerateOutputInformation();

protected:
  GradientImageToBloxBoundaryPointImageFilter();
  virtual ~GradientImageToBloxBoundaryPointImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Method for forming the BloxBoundaryPointImage. */
  void GenerateData();

private:
  GradientImageToBloxBoundaryPointImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** The threshold used to decide whether or not a gradient indicates
    * a boundary point that should be included */
  double m_Threshold;

  /** The resolution of the blox in each dimension */
  float m_BloxResolution[NDimensions];
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientImageToBloxBoundaryPointImageFilter.txx"
#endif

#endif
