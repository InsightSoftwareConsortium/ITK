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
#ifndef itkMorphologicalSharpeningImageFilter_h
#define itkMorphologicalSharpeningImageFilter_h

#include "itkImageToImageFilter.h"
// #include "itkProgressReporter.h"
#include "itkCastImageFilter.h"
#include "itkParabolicErodeImageFilter.h"
#include "itkParabolicDilateImageFilter.h"
#include "itkSharpenOpImageFilter.h"

namespace itk
{
/**
 * \class MorphologicalSharpeningImageFilter
 * \brief Image sharpening using methods based on parabolic
 * structuring elements.
 *
 * This is an implemtentation of the method of Schavemaker for testing
 * the parabolic morphology routines. No particular efforts have been
 * made to minimize memory consumption.
 *
 *
 * \@article{Schavemaker2000,
 *  author    = {Schavemaker, J. and Reinders, M. and Gerbrands, J. and Backer, E.
},
 * title     = {Image sharpening by morphological filtering},
 * journal   = {Pattern Recognition},
 * volume    = {33},
 * number    = {6},
 * year      = {2000},
 * pages     = {997-1012},
 * ee        = {http://dx.doi.org/10.1016/S0031-3203(99)00160-0},
 * bibsource = {DBLP, http://dblp.uni-trier.de}
}

 * Core methods described in the InsightJournal article:
 * "Morphology with parabolic structuring elements"
 *
 * http://hdl.handle.net/1926/1370
 *
 *
 * \ingroup ParabolicMorphology
 *
 * \author Richard Beare, Monash University, Department of Medicine,
 * Melbourne, Australia. <Richard.Beare@monash.edu>
 *
**/

template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_EXPORT MorphologicalSharpeningImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef MorphologicalSharpeningImageFilter            Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MorphologicalSharpeningImageFilter, ImageToImageFilter);

  /** Pixel Type of the input image */
  typedef TInputImage                                            InputImageType;
  typedef TOutputImage                                           OutputImageType;
  typedef typename TInputImage::PixelType                        InputPixelType;
  typedef typename NumericTraits<InputPixelType>::RealType       RealType;
  typedef typename NumericTraits<InputPixelType>::ScalarRealType ScalarRealType;
  typedef typename TOutputImage::PixelType                       OutputPixelType;

  /** Smart pointer typedef support.  */
  typedef typename TInputImage::Pointer      InputImagePointer;
  typedef typename TInputImage::ConstPointer InputImageConstPointer;

  /** Image related typedefs. */
  itkStaticConstMacro(OutputImageDimension, unsigned int, TOutputImage::ImageDimension);
  itkStaticConstMacro(InputImageDimension, unsigned int, TInputImage::ImageDimension);
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

  /** a type to represent the "kernel radius" */
  typedef typename itk::FixedArray<ScalarRealType, TInputImage::ImageDimension> RadiusType;

  itkSetMacro(Iterations, int);
  itkGetConstReferenceMacro(Iterations, int);

  void
  SetScale(ScalarRealType scale)
  {
    m_Erode->SetScale(scale);
    m_Dilate->SetScale(scale);
  }

  void
  SetScale(RadiusType scale)
  {
    m_Erode->SetScale(scale);
    m_Dilate->SetScale(scale);
  }

  void
  SetUseImageSpacing(bool uis)
  {
    m_Erode->SetUseImageSpacing(uis);
    m_Dilate->SetUseImageSpacing(uis);
  }

  // need to include the Get methods
  const RadiusType &
  GetScale()
  {
    return m_Erode->GetScale();
  }

  const bool &
  GetUseImageSpacing()
  {
    return m_Erode->GetUseImageSpacing();
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(SameDimension,
                  (Concept::SameDimension<itkGetStaticConstMacro(InputImageDimension),
                                          itkGetStaticConstMacro(OutputImageDimension)>));

  itkConceptMacro(Comparable, (Concept::Comparable<InputPixelType>));

  /** End concept checking */
#endif
protected:
  MorphologicalSharpeningImageFilter();
  virtual ~MorphologicalSharpeningImageFilter() {}
  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Generate Data */
  void
  GenerateData(void) ITK_OVERRIDE;

  // do everything in the output image type, which should have high precision
  typedef typename itk::ParabolicErodeImageFilter<OutputImageType, OutputImageType>  ErodeType;
  typedef typename itk::ParabolicDilateImageFilter<OutputImageType, OutputImageType> DilateType;
  typedef typename itk::CastImageFilter<InputImageType, OutputImageType>             CastType;

  typedef typename itk::SharpenOpImageFilter<OutputImageType, OutputImageType, OutputImageType, OutputImageType>
    SharpenOpType;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MorphologicalSharpeningImageFilter);

  int m_Iterations;

  typename ErodeType::Pointer     m_Erode;
  typename DilateType::Pointer    m_Dilate;
  typename CastType::Pointer      m_Cast;
  typename SharpenOpType::Pointer m_SharpenOp;
};
} // namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMorphologicalSharpeningImageFilter.hxx"
#endif

#endif
