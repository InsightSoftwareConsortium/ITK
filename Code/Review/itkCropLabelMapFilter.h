/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCropLabelMapFilter.h
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
#ifndef __itkCropLabelMapFilter_h
#define __itkCropLabelMapFilter_h

#include "itkChangeRegionLabelMapFilter.h"

namespace itk
{
/** \class CropLabelMapFilter
 * \brief Crop a LabelMap image
 *
 * Crop a label map. If the output cannot contain some lines of the objects, they are truncated
 * or removed. All objects fully outside the output region are removed.
 *
 * The SetCropSize() method can be used to set the crop size of the lower and the upper
 * boundaries in a single call. By default, the filter does not crop anything.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa PadLabelMapFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template< class TInputImage >
class ITK_EXPORT CropLabelMapFilter:public ChangeRegionLabelMapFilter< TInputImage >
{
public:
  /** Standard class typedefs. */
  typedef CropLabelMapFilter                        Self;
  typedef ChangeRegionLabelMapFilter< TInputImage > Superclass;
  typedef SmartPointer< Self >                      Pointer;
  typedef SmartPointer< const Self >                ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(CropLabelMapFilter, ChangeRegionImageFilter);

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
  itkSetMacro(UpperBoundaryCropSize, SizeType);
  itkGetMacro(UpperBoundaryCropSize, SizeType);
  itkSetMacro(LowerBoundaryCropSize, SizeType);
  itkGetMacro(LowerBoundaryCropSize, SizeType);

  void SetCropSize(const SizeType & size)
  {
    this->SetUpperBoundaryCropSize(size);
    this->SetLowerBoundaryCropSize(size);
  }

protected:
  CropLabelMapFilter()
  {
    m_UpperBoundaryCropSize.Fill(0);
    m_LowerBoundaryCropSize.Fill(0);
  }

  ~CropLabelMapFilter() {}

  virtual void GenerateOutputInformation();

  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  CropLabelMapFilter(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented

  SizeType m_UpperBoundaryCropSize;
  SizeType m_LowerBoundaryCropSize;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCropLabelMapFilter.txx"
#endif

#endif
