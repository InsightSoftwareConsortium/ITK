/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkPadLabelMapFilter_h
#define itkPadLabelMapFilter_h

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
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa CropLabelMapFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template< typename TInputImage >
class ITK_TEMPLATE_EXPORT PadLabelMapFilter:public ChangeRegionLabelMapFilter< TInputImage >
{
public:
  /** Standard class typedefs. */
  typedef PadLabelMapFilter                         Self;
  typedef ChangeRegionLabelMapFilter< TInputImage > Superclass;
  typedef SmartPointer< Self >                      Pointer;
  typedef SmartPointer< const Self >                ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PadLabelMapFilter, ChangeRegionLabelMapFilter);

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

  ~PadLabelMapFilter() ITK_OVERRIDE {}

  virtual void GenerateOutputInformation() ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PadLabelMapFilter);

  SizeType m_UpperBoundaryPadSize;
  SizeType m_LowerBoundaryPadSize;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPadLabelMapFilter.hxx"
#endif

#endif
