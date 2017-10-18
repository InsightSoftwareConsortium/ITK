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

#ifndef itkIntermodesThresholdImageFilter_h
#define itkIntermodesThresholdImageFilter_h

#include "itkHistogramThresholdImageFilter.h"
#include "itkIntermodesThresholdCalculator.h"

namespace itk
{

/** \class IntermodesThresholdImageFilter
 * \brief Threshold an image using the Intermodes Threshold
 *
 * This filter creates a binary thresholded image that separates an
 * image into foreground and background components. The filter
 * computes the threshold using the IntermodesThresholdCalculator and
 * applies that threshold to the input image using the
 * BinaryThresholdImageFilter.
 *
 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/10380/3279  or
 * http://www.insight-journal.org/browse/publication/811
 *
 * \sa HistogramThresholdImageFilter
 *
 * \ingroup Multithreaded
 * \ingroup ITKThresholding
 */

template<typename TInputImage, typename TOutputImage, typename TMaskImage=TOutputImage>
class IntermodesThresholdImageFilter :
    public HistogramThresholdImageFilter<TInputImage, TOutputImage, TMaskImage>
{
public:
  /** Standard Self typedef */
  typedef IntermodesThresholdImageFilter                              Self;
  typedef HistogramThresholdImageFilter<TInputImage,TOutputImage,TMaskImage>     Superclass;
  typedef SmartPointer<Self>                                          Pointer;
  typedef SmartPointer<const Self>                                    ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(IntermodesThresholdImageFilter, HistogramThresholdImageFilter);

  typedef TInputImage                       InputImageType;
  typedef TOutputImage                      OutputImageType;
  typedef TMaskImage                        MaskImageType;

  /** Image pixel value typedef. */
  typedef typename InputImageType::PixelType   InputPixelType;
  typedef typename OutputImageType::PixelType  OutputPixelType;
  typedef typename MaskImageType::PixelType    MaskPixelType;

  /** Image related typedefs. */
  typedef typename InputImageType::Pointer  InputImagePointer;
  typedef typename OutputImageType::Pointer OutputImagePointer;

  typedef typename InputImageType::SizeType    InputSizeType;
  typedef typename InputImageType::IndexType   InputIndexType;
  typedef typename InputImageType::RegionType  InputImageRegionType;
  typedef typename OutputImageType::SizeType   OutputSizeType;
  typedef typename OutputImageType::IndexType  OutputIndexType;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  typedef typename MaskImageType::SizeType     MaskSizeType;
  typedef typename MaskImageType::IndexType    MaskIndexType;
  typedef typename MaskImageType::RegionType   MaskImageRegionType;

  typedef typename Superclass::HistogramType                             HistogramType;
  typedef IntermodesThresholdCalculator< HistogramType, InputPixelType > CalculatorType;

  void SetMaximumSmoothingIterations(SizeValueType maxSmoothingIterations)
  {
    m_IntermodesCalculator->SetMaximumSmoothingIterations(maxSmoothingIterations);
  }

  SizeValueType GetMaximumSmoothingIterations()
  {
    return(m_IntermodesCalculator->GetMaximumSmoothingIterations());
  }

  /** Select whether midpoint (intermode=true) or minimum between
     peaks is used. */
  void SetUseInterMode(bool useIntermode)
  {
    m_IntermodesCalculator->SetUseInterMode(useIntermode);
  }

  bool GetUseInterMode()
  {
    return(m_IntermodesCalculator->GetUseInterMode());
  }

  /** Image related typedefs. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      InputImageType::ImageDimension );
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      OutputImageType::ImageDimension );

protected:

  IntermodesThresholdImageFilter()
    {
    m_IntermodesCalculator = CalculatorType::New();
    this->SetCalculator( m_IntermodesCalculator );
    m_IntermodesCalculator->SetMaximumSmoothingIterations(10000);
    m_IntermodesCalculator->SetUseInterMode(true);
    }
  ~IntermodesThresholdImageFilter() ITK_OVERRIDE {};

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
    {
    Superclass::PrintSelf(os, indent);

    itkPrintSelfObjectMacro( IntermodesCalculator );
    }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(IntermodesThresholdImageFilter);

  typename CalculatorType::Pointer m_IntermodesCalculator;

};

} // end namespace itk

#endif
