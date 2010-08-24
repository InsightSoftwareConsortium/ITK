/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelImageToLabelMapFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLabelImageToLabelMapFilter_h
#define __itkLabelImageToLabelMapFilter_h

#include "itkImageToImageFilter.h"
#include "itkLabelMap.h"
#include "itkLabelObject.h"

namespace itk
{
/** \class LabelImageToLabelMapFilter
 * \brief convert a labeled image to a label collection image
 *
 * LabelImageToLabelMapFilter converts a label image to a label collection image.
 * The labels are the same in the input and the output image.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa BinaryImageToLabelMapFilter, LabelMapToLabelImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template< class TInputImage, class TOutputImage =
            LabelMap< LabelObject< typename TInputImage::PixelType,
                                   ::itk::GetImageDimension< TInputImage >::ImageDimension > > >
class ITK_EXPORT LabelImageToLabelMapFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef LabelImageToLabelMapFilter                      Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage                           InputImageType;
  typedef TOutputImage                          OutputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;
  typedef typename InputImageType::IndexType    IndexType;

  typedef typename OutputImageType::Pointer         OutputImagePointer;
  typedef typename OutputImageType::ConstPointer    OutputImageConstPointer;
  typedef typename OutputImageType::RegionType      OutputImageRegionType;
  typedef typename OutputImageType::PixelType       OutputImagePixelType;
  typedef typename OutputImageType::LabelObjectType LabelObjectType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelImageToLabelMapFilter,
               ImageToImageFilter);

  /**
   * Set/Get the value used as "background" in the output image.
   * Defaults to NumericTraits<PixelType>::NonpositiveMin().
   */
  itkSetMacro(BackgroundValue, OutputImagePixelType);
  itkGetConstMacro(BackgroundValue, OutputImagePixelType);
protected:
  LabelImageToLabelMapFilter();
  ~LabelImageToLabelMapFilter() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** LabelImageToLabelMapFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion();

  /** LabelImageToLabelMapFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion( DataObject *itkNotUsed(output) );

  virtual void BeforeThreadedGenerateData();

  virtual void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, int threadId);

  virtual void AfterThreadedGenerateData();

private:
  LabelImageToLabelMapFilter(const Self &); //purposely not implemented
  void operator=(const Self &);             //purposely not implemented

  OutputImagePixelType m_BackgroundValue;

  typename std::vector< OutputImagePointer > m_TemporaryImages;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelImageToLabelMapFilter.txx"
#endif

#endif
