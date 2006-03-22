/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPolylineMask2DImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPolylineMask2DImageFilter_h
#define _itkPolylineMask2DImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
  
/** \class PolylineMask2DImageFilter
 * \brief Implements 2D image masking operation constrained by a contour.
 *
 * This class is parameterized over input image type, contour defined by a polyline,
 * and output image type. If the input image is three dimensional, the masking operation is 
 * performed on each slice (2D image). The output image will have two regions demarcated
 * by the contour i.e inside(masked) and outside region.  The pixels in the
 * masked region will keep their original intensity values. Whereas, intensity 
 * value of pixels outside the masked region will be set to zero.
 * 
 * 
 * \ingroup ImageToImageFilter
 * \sa  PolylineMaskImageFilter
 */
  template <class TInputImage, class TPolyline,
          class TOutputImage>
class ITK_EXPORT PolylineMask2DImageFilter:public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef PolylineMask2DImageFilter  Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( PolylineMask2DImageFilter, ImageToImageFilter );

  /** Number of dimensions. */
  itkStaticConstMacro(NDimensions, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(NOutputDimensions, unsigned int,
                      TOutputImage::ImageDimension);
  
  /** Some convenient typedefs for input image */
  typedef TInputImage InputImageType;
  typedef typename InputImageType::ConstPointer InputImagePointer;
  typedef typename InputImageType::RegionType InputImageRegionType; 
  typedef typename InputImageType::PixelType InputImagePixelType; 

  /* typedef for the polyline type */
  typedef TPolyline    PolylineType;
  
  /* typedef for the output image */
  typedef TOutputImage OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  typedef typename OutputImageType::PixelType OutputImagePixelType;

  /** Read in image and polyline inputs */
  void SetInput1( const InputImageType * image);
  void SetInput2( const PolylineType * polyline);

  /* Generate Data */
  void GenerateData(void);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(SameDimensionCheck,
    (Concept::SameDimension<NDimensions, NOutputDimensions>));
  itkConceptMacro(IntConvertibleOutputCheck,
    (Concept::Convertible<int, OutputImagePixelType>));
  itkConceptMacro(OutputEqualityComparableCheck,
    (Concept::EqualityComparable<OutputImagePixelType>));
  itkConceptMacro(InputConvertibleToOutputCheck,
    (Concept::Convertible<InputImagePixelType, OutputImagePixelType>));
  /** End concept checking */
#endif

protected:
  PolylineMask2DImageFilter();
  virtual ~PolylineMask2DImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  PolylineMask2DImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPolylineMask2DImageFilter.txx"
#endif

#endif
