/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkLabelMapContourOverlayImageFilter_hxx
#define itkLabelMapContourOverlayImageFilter_hxx

#include "itkLabelMapContourOverlayImageFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkImageScanlineIterator.h"
#include "itkObjectByObjectLabelMapFilter.h"
#include "itkFlatStructuringElement.h"
#include "itkBinaryDilateImageFilter.h"
#include "itkBinaryErodeImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkBinaryContourImageFilter.h"
#include "itkSliceBySliceImageFilter.h"
#include "itkLabelUniqueLabelMapFilter.h"
#include "itkProgressTransformer.h"


namespace itk
{

template <typename TLabelMap, typename TFeatureImage, typename TOutputImage>
LabelMapContourOverlayImageFilter<TLabelMap, TFeatureImage, TOutputImage>::LabelMapContourOverlayImageFilter()
{
  this->SetNumberOfRequiredInputs(2);
  m_Opacity = 0.5;
  m_Type = CONTOUR;
  m_Priority = HIGH_LABEL_ON_TOP;
  SizeType s;
  s.Fill(1);
  m_ContourThickness = SizeType(s);
  s.Fill(0);
  m_DilationRadius = SizeType(s);
  m_SliceDimension = ImageDimension - 1;
  this->DynamicMultiThreadingOn();
}

template <typename TLabelMap, typename TFeatureImage, typename TOutputImage>
void
LabelMapContourOverlayImageFilter<TLabelMap, TFeatureImage, TOutputImage>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  LabelMapPointer input = const_cast<LabelMapType *>(this->GetInput());
  if (!input)
  {
    return;
  }
  input->SetRequestedRegion(input->GetLargestPossibleRegion());
}

template <typename TLabelMap, typename TFeatureImage, typename TOutputImage>
void
LabelMapContourOverlayImageFilter<TLabelMap, TFeatureImage, TOutputImage>::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()->SetRequestedRegion(this->GetOutput()->GetLargestPossibleRegion());
}

template <typename TLabelMap, typename TFeatureImage, typename TOutputImage>
void
LabelMapContourOverlayImageFilter<TLabelMap, TFeatureImage, TOutputImage>::GenerateData()
{
  this->UpdateProgress(0.0f);
  this->AllocateOutputs();
  this->BeforeThreadedGenerateData();

  ProgressTransformer pt(0.05f, 0.5f, this);
  this->GetMultiThreader()->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
  this->GetMultiThreader()->template ParallelizeImageRegion<OutputImageDimension>(
    this->GetOutput()->GetRequestedRegion(),
    [this](const OutputImageRegionType & outputRegionForThread) {
      this->DynamicThreadedGenerateData(outputRegionForThread);
    },
    pt.GetProcessObject());

  ProgressTransformer pt2(0.5f, 0.99f, this);
  // delegate to the superclass implementation to use the thread support for the label objects
  this->GetMultiThreader()->template ParallelizeImageRegion<OutputImageDimension>(
    this->GetOutput()->GetRequestedRegion(),
    [this](const OutputImageRegionType & outputRegionForThread) { this->SuperclassDynamicTGD(outputRegionForThread); },
    pt2.GetProcessObject());

  this->AfterThreadedGenerateData();
  this->UpdateProgress(1.0f);
}

template <typename TLabelMap, typename TFeatureImage, typename TOutputImage>
void
LabelMapContourOverlayImageFilter<TLabelMap, TFeatureImage, TOutputImage>::BeforeThreadedGenerateData()
{
  using OBOType = ObjectByObjectLabelMapFilter<LabelMapType, LabelMapType>;
  typename OBOType::Pointer obo = OBOType::New();
  obo->SetInput(this->GetInput());
  SizeType rad = m_DilationRadius;
  for (unsigned int i = 0; i < ImageDimension; i++)
  {
    rad[i]++;
  }
  obo->SetPadSize(rad);
  // obo->SetInPlace( false );

  // dilate the image
  using InternalImageType = typename OBOType::InternalInputImageType;
  using KernelType = FlatStructuringElement<ImageDimension>;
  using DilateType = BinaryDilateImageFilter<InternalImageType, InternalImageType, KernelType>;
  typename DilateType::Pointer dilate = DilateType::New();
  dilate->SetKernel(KernelType::Ball(m_DilationRadius));
  obo->SetInputFilter(dilate);

  //   using CastType = typename CastImageFilter< InternalImageType, InternalImageType, KernelType >;
  //   typename CastType::Pointer cast = CastType::New();
  //   cast->SetInPlace( false );

  using ErodeType = BinaryErodeImageFilter<InternalImageType, InternalImageType, KernelType>;
  typename ErodeType::Pointer erode = ErodeType::New();
  erode->SetKernel(KernelType::Ball(m_ContourThickness));
  erode->SetInput(dilate->GetOutput());

  using SubtractType = SubtractImageFilter<InternalImageType, InternalImageType>;
  typename SubtractType::Pointer sub = SubtractType::New();
  sub->SetInput(0, dilate->GetOutput());
  sub->SetInput(1, erode->GetOutput());


  using SliceType = SliceBySliceImageFilter<InternalImageType, InternalImageType>;
  using SliceInternalImageType = typename SliceType::InternalInputImageType;
  typename SliceType::Pointer slice = SliceType::New();

  using SliceCastType = CastImageFilter<SliceInternalImageType, SliceInternalImageType>;
  typename SliceCastType::Pointer scast = SliceCastType::New();
  scast->SetInPlace(false);
  slice->SetInputFilter(scast);

  using SliceKernelType = FlatStructuringElement<ImageDimension - 1>;
  using SliceErodeType = BinaryErodeImageFilter<SliceInternalImageType, SliceInternalImageType, SliceKernelType>;
  typename SliceErodeType::Pointer serode = SliceErodeType::New();
  using RadiusType = typename SliceKernelType::RadiusType;
  RadiusType srad;
  srad.Fill(NumericTraits<typename RadiusType::SizeValueType>::ZeroValue());
  for (unsigned int i = 0, j = 0; i < ImageDimension; i++)
  {
    if (j != static_cast<unsigned int>(m_SliceDimension))
    {
      srad[j] = m_ContourThickness[i];
      j++;
    }
  }
  serode->SetKernel(SliceKernelType::Ball(srad));
  serode->SetInput(scast->GetOutput());

  using SliceSubtractType = SubtractImageFilter<SliceInternalImageType, SliceInternalImageType>;
  typename SliceSubtractType::Pointer ssub = SliceSubtractType::New();
  ssub->SetInput(0, scast->GetOutput());
  ssub->SetInput(1, serode->GetOutput());
  slice->SetOutputFilter(ssub);

  // search the contour, or not
  if (m_Type == PLAIN)
  {
    // nothing to do
    obo->SetOutputFilter(dilate);
  }
  else if (m_Type == CONTOUR)
  {
    //     using ContourType = BinaryContourImageFilter< InternalImageType, InternalImageType >;
    //     typename ContourType::Pointer contour = ContourType::New();
    //     contour->SetInput( dilate->GetOutput() );
    //     obo->SetOutputFilter( contour );
    obo->SetOutputFilter(sub);
  }
  else if (m_Type == SLICE_CONTOUR)
  {
    slice->SetInput(dilate->GetOutput());
    slice->SetDimension(m_SliceDimension);
    obo->SetOutputFilter(slice);

    //     using SliceInternalType = typename SliceType::InternalInputImageType;
    //     using SliceContourType = BinaryContourImageFilter< SliceInternalType, SliceInternalType >;
    //     typename SliceContourType::Pointer slice_contour = SliceContourType::New();
    //     slice->SetFilter( slice_contour );
  }
  else
  {
    itkExceptionMacro(<< "Unsupported Type: " << m_Type);
  }

  // choose which labels will be on top of the oters
  using UniqueType = LabelUniqueLabelMapFilter<LabelMapType>;
  typename UniqueType::Pointer uniq = UniqueType::New();
  uniq->SetInput(obo->GetOutput());
  uniq->SetReverseOrdering(m_Priority == LOW_LABEL_ON_TOP);

  m_TempImage = uniq->GetOutput();
  m_TempImage->Update();
  m_TempImage->DisconnectPipeline();

  Superclass::BeforeThreadedGenerateData();
}


template <typename TLabelMap, typename TFeatureImage, typename TOutputImage>
void
LabelMapContourOverlayImageFilter<TLabelMap, TFeatureImage, TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  OutputImageType *        output = this->GetOutput();
  auto *                   input = const_cast<LabelMapType *>(this->GetInput());
  const FeatureImageType * input2 = this->GetFeatureImage();

  FunctorType function(m_Functor);
  function.SetBackgroundValue(input->GetBackgroundValue());
  function.SetOpacity(m_Opacity);

  ImageScanlineConstIterator<FeatureImageType> featureIt(input2, outputRegionForThread);
  ImageScanlineIterator<OutputImageType>       outputIt(output, outputRegionForThread);

  while (!featureIt.IsAtEnd())
  {
    while (!featureIt.IsAtEndOfLine())
    {
      outputIt.Set(function(featureIt.Get(), input->GetBackgroundValue()));
      ++featureIt;
      ++outputIt;
    }
    featureIt.NextLine();
    outputIt.NextLine();
  }
}


template <typename TLabelMap, typename TFeatureImage, typename TOutputImage>
void
LabelMapContourOverlayImageFilter<TLabelMap, TFeatureImage, TOutputImage>::ThreadedProcessLabelObject(
  LabelObjectType * labelObject)
{
  OutputImageType *        output = this->GetOutput();
  auto *                   input = const_cast<LabelMapType *>(this->GetInput());
  const FeatureImageType * input2 = this->GetFeatureImage();

  FunctorType function(m_Functor);
  function.SetBackgroundValue(input->GetBackgroundValue());
  function.SetOpacity(m_Opacity);

  const typename LabelObjectType::LabelType & label = labelObject->GetLabel();

  // the user want the mask to be the background of the label collection image
  typename LabelObjectType::ConstIndexIterator it(labelObject);
  while (!it.IsAtEnd())
  {
    const IndexType idx = it.GetIndex();
    output->SetPixel(idx, function(input2->GetPixel(idx), label));
    ++it;
  }
}


template <typename TLabelMap, typename TFeatureImage, typename TOutputImage>
void
LabelMapContourOverlayImageFilter<TLabelMap, TFeatureImage, TOutputImage>::GenerateOutputInformation()
{
  // this methods is overloaded so that if the output image is a
  // VectorImage then the correct number of components are set.

  Superclass::GenerateOutputInformation();
  OutputImageType * output = this->GetOutput();

  if (!output)
  {
    return;
  }
  if (output->GetNumberOfComponentsPerPixel() != 3)
  {
    output->SetNumberOfComponentsPerPixel(3);
  }
}

template <typename TLabelMap, typename TFeatureImage, typename TOutputImage>
void
LabelMapContourOverlayImageFilter<TLabelMap, TFeatureImage, TOutputImage>::PrintSelf(std::ostream & os,
                                                                                     Indent         indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Opacity: " << m_Opacity << std::endl;
  os << indent << "Type: " << m_Type << std::endl;
  os << indent << "Priority: " << m_Priority << std::endl;
  os << indent << "ContourThickness: " << m_ContourThickness << std::endl;
  os << indent << "DilationRadius: " << m_DilationRadius << std::endl;
  os << indent << "SliceDimension: " << m_SliceDimension << std::endl;
}


} // end namespace itk
#endif
