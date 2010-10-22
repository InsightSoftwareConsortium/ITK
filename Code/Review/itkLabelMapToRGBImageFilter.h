/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkLabelMapToRGBImageFilter.h,v $
  Language:  C++
  Date:      $Date: 2005/08/23 15:09:03 $
  Version:   $Revision: 1.4 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLabelMapToRGBImageFilter_h
#define __itkLabelMapToRGBImageFilter_h

#include "itkLabelMapFilter.h"
#include "itkLabelToRGBFunctor.h"
#include "itkImage.h"
#include "itkRGBPixel.h"


namespace itk {

/** \class LabelMapToRGBImageFilter
 * \brief Convert a LabelMap to a colored image
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa LabelMapToBinaryImageFilter, LabelMapMaskImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template<class TInputImage, class TOutputImage=Image< RGBPixel<unsigned char>, TInputImage::ImageDimension > >
class ITK_EXPORT LabelMapToRGBImageFilter :
    public LabelMapFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef LabelMapToRGBImageFilter                   Self;
  typedef LabelMapFilter<TInputImage, TOutputImage>  Superclass;
  typedef SmartPointer<Self>                         Pointer;
  typedef SmartPointer<const Self>                   ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage                              InputImageType;
  typedef TOutputImage                             OutputImageType;
  typedef typename InputImageType::Pointer         InputImagePointer;
  typedef typename InputImageType::ConstPointer    InputImageConstPointer;
  typedef typename InputImageType::RegionType      InputImageRegionType;
  typedef typename InputImageType::PixelType       InputImagePixelType;
  typedef typename InputImageType::LabelObjectType LabelObjectType;

  typedef typename OutputImageType::Pointer        OutputImagePointer;
  typedef typename OutputImageType::ConstPointer   OutputImageConstPointer;
  typedef typename OutputImageType::RegionType     OutputImageRegionType;
  typedef typename OutputImageType::PixelType      OutputImagePixelType;
  typedef typename OutputImageType::IndexType      IndexType;

  typedef typename Functor::LabelToRGBFunctor< InputImagePixelType, OutputImagePixelType > FunctorType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelMapToRGBImageFilter,
               ImageToImageFilter);

protected:
  LabelMapToRGBImageFilter();
  ~LabelMapToRGBImageFilter() {};

  virtual void BeforeThreadedGenerateData();

  virtual void ThreadedProcessLabelObject( LabelObjectType * labelObject );

private:
  LabelMapToRGBImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelMapToRGBImageFilter.txx"
#endif

#endif
