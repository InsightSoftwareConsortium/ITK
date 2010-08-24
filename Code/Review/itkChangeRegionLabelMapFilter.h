/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkChangeRegionLabelMapFilter.h
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
#ifndef __itkChangeRegionLabelMapFilter_h
#define __itkChangeRegionLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"

namespace itk
{
/** \class ChangeRegionLabelMapFilter
 * \brief Change the region of a label map.
 *
 * Change the region of a label map. If the output can't contain some of the objects' lines
 * they are truncated or removed. All objects fully outside the output region are removed.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa LabelMapMaskImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template< class TInputImage >
class ITK_EXPORT ChangeRegionLabelMapFilter:public InPlaceLabelMapFilter< TInputImage >
{
public:
  /** Standard class typedefs. */
  typedef ChangeRegionLabelMapFilter           Self;
  typedef InPlaceLabelMapFilter< TInputImage > Superclass;
  typedef SmartPointer< Self >                 Pointer;
  typedef SmartPointer< const Self >           ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ChangeRegionLabelMapFilter, InPlaceImageFilter);

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

  typedef typename InputImageType::PixelType      PixelType;
  typedef typename InputImageType::IndexType      IndexType;
  typedef typename InputImageType::IndexValueType IndexValueType;
  typedef typename InputImageType::SizeType       SizeType;
  typedef typename InputImageType::RegionType     RegionType;

  typedef TInputImage TOutputImage;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int, TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, TOutputImage::ImageDimension);
  itkStaticConstMacro(ImageDimension, unsigned int, TOutputImage::ImageDimension);

  /** The output region to use */
  itkSetMacro(Region, OutputImageRegionType);
  itkGetConstReferenceMacro(Region, OutputImageRegionType);
protected:
  ChangeRegionLabelMapFilter() {}
  ~ChangeRegionLabelMapFilter() {}

  virtual void PrintSelf(std::ostream & os, Indent indent) const;

  virtual void ThreadedProcessLabelObject(LabelObjectType *labelObject);

  void GenerateInputRequestedRegion();

  void EnlargeOutputRequestedRegion( DataObject *itkNotUsed(output) );

  virtual void GenerateOutputInformation();

  void GenerateData();

private:
  ChangeRegionLabelMapFilter(const Self &); //purposely not implemented
  void operator=(const Self &);             //purposely not implemented

  OutputImageRegionType m_Region;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkChangeRegionLabelMapFilter.txx"
#endif

#endif
