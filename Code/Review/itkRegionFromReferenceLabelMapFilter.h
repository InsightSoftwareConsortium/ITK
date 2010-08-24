/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionFromReferenceLabelMapFilter.h
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
#ifndef __itkRegionFromReferenceLabelMapFilter_h
#define __itkRegionFromReferenceLabelMapFilter_h

#include "itkChangeRegionLabelMapFilter.h"

namespace itk
{
/** \class RegionFromReferenceLabelMapFilter
 * \brief Set the region from a reference image
 *
 * Change the region of a label map to be the same as one of a reference image.
 * This filter implements the same feature as its superclass, but with the input region
 * well integrated in the pipeline architecture.
 * If the output cannot contain some of the objects' lines, they are truncated or removed.
 * All objects fully outside the output region are removed.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template< class TInputImage >
class ITK_EXPORT RegionFromReferenceLabelMapFilter:public ChangeRegionLabelMapFilter< TInputImage >
{
public:
  /** Standard class typedefs. */
  typedef RegionFromReferenceLabelMapFilter         Self;
  typedef ChangeRegionLabelMapFilter< TInputImage > Superclass;
  typedef SmartPointer< Self >                      Pointer;
  typedef SmartPointer< const Self >                ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegionFromReferenceLabelMapFilter, ChangeRegionImageFilter);

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

  typedef ImageBase< itkGetStaticConstMacro(ImageDimension) > ReferenceImageType;

  /** Copy the output information from another Image. */
  void SetReferenceImage(const ReferenceImageType *image);

  const ReferenceImageType * GetReferenceImage() const;

  /** Set the input image */
  void SetInput1(const TInputImage *input)
  {
    this->SetInput(input);
  }

  /** Set the reference image */
  void SetInput2(const ReferenceImageType *input)
  {
    this->SetReferenceImage(input);
  }

protected:
  RegionFromReferenceLabelMapFilter()
  {
    this->SetNumberOfRequiredInputs(2);
  }

  ~RegionFromReferenceLabelMapFilter() {}

  virtual void GenerateOutputInformation();

  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  RegionFromReferenceLabelMapFilter(const Self &); //purposely not implemented
  void operator=(const Self &);                    //purposely not implemented
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegionFromReferenceLabelMapFilter.txx"
#endif

#endif
