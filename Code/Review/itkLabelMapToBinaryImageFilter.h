/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelMapToBinaryImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLabelMapToBinaryImageFilter_h
#define __itkLabelMapToBinaryImageFilter_h

#include "itkLabelMapFilter.h"
#include "itkBarrier.h"

namespace itk
{
/** \class LabelMapToBinaryImageFilter
 * \brief Convert a LabelMap to a binary image.
 *
 * LabelMapToBinaryImageFilter to a binary image. All the objects in the image
 * are used as foreground.  The background values of the original binary image
 * can be restored by passing this image to the filter with the
 * SetBackgroundImage() method.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction,
 * INRA de Jouy-en-Josas, France.
 *
 * \sa LabelMapToLabelImageFilter, LabelMapMaskImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template< class TInputImage, class TOutputImage >
class ITK_EXPORT LabelMapToBinaryImageFilter:
  public LabelMapFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef LabelMapToBinaryImageFilter                 Self;
  typedef LabelMapFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage                              InputImageType;
  typedef TOutputImage                             OutputImageType;
  typedef typename InputImageType::Pointer         InputImagePointer;
  typedef typename InputImageType::ConstPointer    InputImageConstPointer;
  typedef typename InputImageType::RegionType      InputImageRegionType;
  typedef typename InputImageType::PixelType       InputImagePixelType;
  typedef typename InputImageType::LabelObjectType LabelObjectType;

  typedef typename OutputImageType::Pointer      OutputImagePointer;
  typedef typename OutputImageType::ConstPointer OutputImageConstPointer;
  typedef typename OutputImageType::RegionType   OutputImageRegionType;
  typedef typename OutputImageType::PixelType    OutputImagePixelType;
  typedef typename OutputImageType::IndexType    IndexType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int, TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelMapToBinaryImageFilter, ImageToImageFilter);

  /**
   * Set/Get the value used as "background" in the output image.
   * Defaults to NumericTraits<PixelType>::NonpositiveMin().
   */
  itkSetMacro(BackgroundValue, OutputImagePixelType);
  itkGetConstMacro(BackgroundValue, OutputImagePixelType);

  /**
   * Set/Get the value used as "foreground" in the output image.
   * Defaults to NumericTraits<PixelType>::max().
   */
  itkSetMacro(ForegroundValue, OutputImagePixelType);
  itkGetConstMacro(ForegroundValue, OutputImagePixelType);

  /** Set/Get the background image top be used to restore the background values
    */
  void SetBackgroundImage(const OutputImageType *input)
  {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput( 1, const_cast< OutputImageType * >( input ) );
  }

  OutputImageType * GetBackgroundImage()
  {
    return static_cast< OutputImageType * >( const_cast< DataObject * >( this->ProcessObject::GetInput(1) ) );
  }

  /** Set the input image */
  void SetInput1(const InputImageType *input)
  {
    this->SetInput(input);
  }

  /** Set the marker image */
  void SetInput2(const OutputImageType *input)
  {
    this->SetBackgroundImage(input);
  }

protected:
  LabelMapToBinaryImageFilter();
  ~LabelMapToBinaryImageFilter() {}

  /** LabelMapToBinaryImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion();

  /** LabelMapToBinaryImageFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion( DataObject *itkNotUsed(output) );

  virtual void BeforeThreadedGenerateData();

  virtual void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, int threadId);

  virtual void ThreadedProcessLabelObject(LabelObjectType *labelObject);

  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  LabelMapToBinaryImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);              //purposely not implemented

  OutputImagePixelType m_BackgroundValue;
  OutputImagePixelType m_ForegroundValue;

  typename Barrier::Pointer m_Barrier;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelMapToBinaryImageFilter.txx"
#endif

#endif
