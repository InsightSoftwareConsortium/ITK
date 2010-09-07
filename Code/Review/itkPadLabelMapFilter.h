/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPadLabelMapFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPadLabelMapFilter_h
#define __itkPadLabelMapFilter_h

#include "itkChangeRegionLabelMapFilter.h"

namespace itk
{
/** \class PadLabelMapFilter
 * \brief Pad a LabelMap image
 *
 * This filter pads a label map.
 *
 * The SetPadSize() method can be used to set the pad size of the lower and
 * the upper boundaries in a single call.
 * By default, the filter doesn't pad anything.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa CropLabelMapFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template< class TInputImage >
class ITK_EXPORT PadLabelMapFilter:public ChangeRegionLabelMapFilter< TInputImage >
{
public:
  /** Standard class typedefs. */
  typedef PadLabelMapFilter                         Self;
  typedef ChangeRegionLabelMapFilter< TInputImage > Superclass;
  typedef SmartPointer< Self >                      Pointer;
  typedef SmartPointer< const Self >                ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PadLabelMapFilter, ChangeRegionImageFilter);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageType       OutputImageType;
  typedef typename Superclass::OutputImagePointer    OutputImagePointer;
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;
  typedef typename Superclass::OutputImagePixelType  OutputImagePixelType;

  /** Some convenient typedefs. */
  typedef TInputImage                              InputImageType;
  typedef typename InputImageType::Pointer         InputImagePointer;
  typedef typename InputImageType::ConstPointer    InputImageConstPointer;
  typedef typename InputImageType::RegionType      InputImageRegionType;
  typedef typename InputImageType::PixelType       InputImagePixelType;
  typedef typename InputImageType::LabelObjectType LabelObjectType;

  typedef typename InputImageType::PixelType  PixelType;
  typedef typename InputImageType::IndexType  IndexType;
  typedef typename InputImageType::SizeType   SizeType;
  typedef typename InputImageType::RegionType RegionType;

  typedef TInputImage TOutputImage;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int, TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, TOutputImage::ImageDimension);
  itkStaticConstMacro(ImageDimension, unsigned int, TOutputImage::ImageDimension);

  /** Set/Get the cropping sizes for the upper and lower boundaries. */
  itkSetMacro(UpperBoundaryPadSize, SizeType);
  itkGetMacro(UpperBoundaryPadSize, SizeType);
  itkSetMacro(LowerBoundaryPadSize, SizeType);
  itkGetMacro(LowerBoundaryPadSize, SizeType);

  void SetPadSize(const SizeType & size)
  {
    this->SetUpperBoundaryPadSize(size);
    this->SetLowerBoundaryPadSize(size);
  }

protected:
  PadLabelMapFilter()
  {
    m_UpperBoundaryPadSize.Fill(0);
    m_LowerBoundaryPadSize.Fill(0);
  }

  ~PadLabelMapFilter() {}

  virtual void GenerateOutputInformation();

  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  PadLabelMapFilter(const Self &); //purposely not implemented
  void operator=(const Self &);    //purposely not implemented

  SizeType m_UpperBoundaryPadSize;
  SizeType m_LowerBoundaryPadSize;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPadLabelMapFilter.txx"
#endif

#endif
