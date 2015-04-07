#ifndef __itkMorphologicalSharpeningImageFilter_hxx
#define __itkMorphologicalSharpeningImageFilter_hxx

#include "itkMorphologicalSharpeningImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk

{
template <typename TInputImage, typename TOutputImage>
MorphologicalSharpeningImageFilter<TInputImage, TOutputImage>::MorphologicalSharpeningImageFilter()
{
  this->SetNumberOfRequiredOutputs(1);
  this->SetNumberOfRequiredInputs(1);

  m_Erode = ErodeType::New();
  m_Dilate = DilateType::New();
  m_Cast = CastType::New();
  m_SharpenOp = SharpenOpType::New();
  m_Iterations = 1;
  this->SetScale(1);
  this->SetUseImageSpacing(false);
}


template <typename TInputImage, typename TOutputImage>
void
MorphologicalSharpeningImageFilter<TInputImage, TOutputImage>::GenerateData(void)
{

  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  InputImageConstPointer inputImage = this->GetInput();
  m_Cast->SetInput(inputImage);

  // set the input to the morph operations
  m_Erode->SetInput(m_Cast->GetOutput());
  m_Dilate->SetInput(m_Cast->GetOutput());
  m_SharpenOp->SetInput(m_Dilate->GetOutput());
  m_SharpenOp->SetInput2(m_Cast->GetOutput());
  m_SharpenOp->SetInput3(m_Erode->GetOutput());

  progress->RegisterInternalFilter(m_Erode, 1.0f);
  progress->RegisterInternalFilter(m_Dilate, 1.0f);
  progress->RegisterInternalFilter(m_SharpenOp, 1.0f);


  // set up the progrss monitor
  // WatershedMiniPipelineProgressCommand::Pointer c =
  //   WatershedMiniPipelineProgressCommand::New();
  // c->SetFilter(this);
  // c->SetNumberOfFilters(3 * m_Iterations);

  // m_Erode->AddObserver(ProgressEvent(), c);
  // m_Dilate->AddObserver(ProgressEvent(), c);
  // m_SharpenOp->AddObserver(ProgressEvent(), c);


  for (int i = 0; i < m_Iterations; i++)
  {
    if (i != 0)
    {
      m_Erode->SetInput(this->GetOutput());
      m_Dilate->SetInput(this->GetOutput());
      m_SharpenOp->SetInput2(this->GetOutput());
      m_Erode->Modified();
      m_Dilate->Modified();
    }

    m_SharpenOp->GraftOutput(this->GetOutput());
    m_SharpenOp->Update();
    this->GraftOutput(m_SharpenOp->GetOutput());
  }
}

template <typename TInputImage, typename TOutputImage>
void
MorphologicalSharpeningImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "Iterations = " << m_Iterations << std::endl;
}


} // end namespace itk


#endif
