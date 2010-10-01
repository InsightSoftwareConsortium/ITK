/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkLabelMapMaskImageFilter.h,v $
  Language:  C++
  Date:      $Date: 2005/08/23 15:09:03 $
  Version:   $Revision: 1.4 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLabelMapMaskImageFilter_h
#define __itkLabelMapMaskImageFilter_h

#include "itkLabelMapFilter.h"
#include "itkBarrier.h"

namespace itk {

/** \class LabelMapMaskImageFilter
 * \brief Mask and image with a LabelMap
 *
 * LabelMapMaskImageFilter mask the content of an input image according
 * to the content of the input LabelMap. The masked pixel of the input image
 * are set to the BackgroundValue.
 * LabelMapMaskImageFilter can keep the input image for one label only, with
 * Negated = false (the default) or it can mask the input image for a single label, when
 * Negated equals true. In Both cases, the label is set with SetLabel().
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa LabelMapToBinaryImageFilter, LabelMapToLabelImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template<class TInputImage, class TOutputImage>
class ITK_EXPORT LabelMapMaskImageFilter :
    public LabelMapFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef LabelMapMaskImageFilter                   Self;
  typedef LabelMapFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                        Pointer;
  typedef SmartPointer<const Self>                  ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage                              InputImageType;
  typedef TOutputImage                             OutputImageType;
  typedef typename InputImageType::Pointer         InputImagePointer;
  typedef typename InputImageType::ConstPointer    InputImageConstPointer;
  typedef typename InputImageType::RegionType      InputImageRegionType;
  typedef typename InputImageType::PixelType       InputImagePixelType;
  typedef typename InputImageType::LabelObjectType LabelObjectType;
  typedef typename LabelObjectType::LabelType      LabelType;

  typedef typename OutputImageType::Pointer        OutputImagePointer;
  typedef typename OutputImageType::ConstPointer   OutputImageConstPointer;
  typedef typename OutputImageType::RegionType     OutputImageRegionType;
  typedef typename OutputImageType::PixelType      OutputImagePixelType;
  typedef typename OutputImageType::IndexType      IndexType;
  typedef typename OutputImageType::SizeType       SizeType;
  typedef typename OutputImageType::RegionType     RegionType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelMapMaskImageFilter,
               ImageToImageFilter);

   /** Set the feature image */
  void SetFeatureImage(const TOutputImage *input)
    {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput( 1, const_cast<TOutputImage *>(input) );
    }

  /** Get the feature image */
  OutputImageType * GetFeatureImage()
    {
    return static_cast<OutputImageType*>(const_cast<DataObject *>(this->ProcessObject::GetInput(1)));
    }

  /** Set the input image */
  void SetInput1(TInputImage *input)
    {
    this->SetInput( input );
    }

  /** Set the feature image */
  void SetInput2(TOutputImage *input)
    {
    this->SetFeatureImage( input );
    }

  /**
   * Set/Get the value used as "background" in the output image.
   * Defaults to NumericTraits<PixelType>::Zero.
   */
  itkSetMacro(BackgroundValue, OutputImagePixelType);
  itkGetConstMacro(BackgroundValue, OutputImagePixelType);

  /**
   * The label to mask or to not mask, depending on the value of the Negated ivar.
   */
  itkSetMacro(Label, InputImagePixelType);
  itkGetConstMacro(Label, InputImagePixelType);

  /**
   * Set/Get whether the Label should be masked or not.
   */
  itkSetMacro(Negated, bool);
  itkGetConstReferenceMacro(Negated, bool);
  itkBooleanMacro(Negated);

  /**
   * Set/Get whether the image size should be adjusted to the masked image or not.
   */
  itkSetMacro(Crop, bool);
  itkGetConstReferenceMacro(Crop, bool);
  itkBooleanMacro(Crop);

  /**
   * Set/Get the boder added to the mask before the crop. The default is 0 on
   * all the axes.
   */
  itkSetMacro(CropBorder, SizeType);
  itkGetConstReferenceMacro(CropBorder, SizeType);

protected:
  LabelMapMaskImageFilter();
  ~LabelMapMaskImageFilter() {};

  /** LabelMapMaskImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion();

  /** LabelMapMaskImageFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion(DataObject *itkNotUsed(output));

  virtual void GenerateOutputInformation();

  virtual void BeforeThreadedGenerateData();

  virtual void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread, int threadId );

  virtual void ThreadedProcessLabelObject( LabelObjectType * labelObject );

  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  LabelMapMaskImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  InputImagePixelType       m_Label;
  OutputImagePixelType      m_BackgroundValue;
  bool                      m_Negated;
  bool                      m_Crop;
  SizeType                  m_CropBorder;

  TimeStamp                 m_CropTimeStamp;

  typename Barrier::Pointer m_Barrier;

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelMapMaskImageFilter.txx"
#endif

#endif
