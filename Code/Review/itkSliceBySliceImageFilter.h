/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSliceBySliceImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkSliceBySliceImageFilter_h
#define __itkSliceBySliceImageFilter_h

#include "itkImageToImageFilter.h"


namespace itk {

/**
 * \class SliceBySliceImageFilter
 * \brief
 *
 *
 * \author Gaetan Lehmann
 *
 * This class was taken from the Insight Journal paper:
 * http://insight-journal.org/midas/handle.php?handle=1926/368
 *
 */

template<class TInputImage,
  class TOutputImage,
  class TInputFilter=ImageToImageFilter< 
    Image< ITK_TYPENAME TInputImage::PixelType,  ::itk::GetImageDimension<TInputImage >::ImageDimension - 1 >,
    Image< ITK_TYPENAME TOutputImage::PixelType, ::itk::GetImageDimension<TOutputImage>::ImageDimension - 1 > >,
  class TOutputFilter=TInputFilter,
  class TInternalInputImage=ITK_TYPENAME TInputFilter::InputImageType,
  class TInternalOutputImage=ITK_TYPENAME TOutputFilter::OutputImageType >
class ITK_EXPORT SliceBySliceImageFilter : 
public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef SliceBySliceImageFilter                         Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>    Superclass;
  typedef SmartPointer<Self>                              Pointer;
  typedef SmartPointer<const Self>                        ConstPointer;
  
  /** Superclass typedefs. */
  typedef typename Superclass::InputImagePointer InputImagePointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(SliceBySliceImageFilter, ImageToImageFilter);
 
  /** Image related typedefs. */
  typedef TInputImage                           InputImageType;
  typedef typename TInputImage::RegionType      RegionType;
  typedef typename TInputImage::SizeType        SizeType;
  typedef typename TInputImage::IndexType       IndexType;
  typedef typename TInputImage::PixelType       PixelType;
  typedef typename TInputImage::OffsetType      OffsetType;
  
  typedef TOutputImage                          OutputImageType;
  typedef typename TOutputImage::PixelType      OutputPixelType;

  typedef TInputFilter                          InputFilterType;
  typedef TOutputFilter                         OutputFilterType;
  
  typedef TInternalInputImage                         InternalInputImageType;
  typedef typename InternalInputImageType::RegionType InternalRegionType;
  typedef typename InternalInputImageType::SizeType   InternalSizeType;
  typedef typename InternalInputImageType::IndexType  InternalIndexType;
  typedef typename InternalInputImageType::OffsetType InternalOffsetType;
  typedef typename InternalInputImageType::PixelType  InternalInputPixelType;

  typedef TInternalOutputImage                        InternalOutputImageType;
  typedef typename InternalOutputImageType::PixelType InternalOutputPixelType;

  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  itkStaticConstMacro(InternalImageDimension, unsigned int,
                      InternalInputImageType::ImageDimension);

  itkSetMacro(Dimension, unsigned int);
  itkGetMacro(Dimension, unsigned int);

  void SetFilter(InputFilterType * filter);
  InputFilterType * GetFilter()
    {
    return this->m_InputFilter;
    }

  const InputFilterType * GetFilter() const
    {
    return this->m_InputFilter;
    }

  void SetInputFilter( InputFilterType * filter );
  itkGetObjectMacro( InputFilter, InputFilterType );

  void SetOutputFilter( OutputFilterType * filter );
  itkGetObjectMacro( OutputFilter, OutputFilterType );

protected:
  SliceBySliceImageFilter();
  ~SliceBySliceImageFilter() {};

  void GenerateData();

  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateInputRequestedRegion();
  void EnlargeOutputRequestedRegion(DataObject *itkNotUsed(output));

private:
  SliceBySliceImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  unsigned int                            m_Dimension;
  typename InputFilterType::Pointer       m_InputFilter;
  typename OutputFilterType::Pointer      m_OutputFilter;

};

}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSliceBySliceImageFilter.txx"
#endif

#endif
