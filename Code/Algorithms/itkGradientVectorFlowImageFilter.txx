#ifndef _itkGradientVectorFlowImageFilter_txx
#define _itkGradientVectorFlowImageFilter_txx

namespace itk
{
template <class TInputImage, class TOutputImage>
GradientVectorFlowImageFilter<TInputImage, TOutputImage>
::GradientVectorFlowImageFilter()
{
  for (int i=0; i<ImageDimension; i++) m_Steps[i] = 1.0;
  OutputImagePointer m_OutputImage = OutputImageType::New();
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, m_OutputImage.GetPointer());
}

template <class TInputImage, class TOutputImage>
GradientVectorFlowImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  TOutputImage output = this->GetOutput();

  output->SetLargestPossibleRegion( this->GetInput()->GetLargestPossibleRegion() );
  output->SetBufferedRegion( this->GetInput()->GetLargestPossibleRegion() );

  output->Allocate();

  m_TimeStep = 0.2/m_NoiseLevel; 

  int i=0;

  while ( i < 100 ) {
    PixelUpdate();
    i++;
  }
}

template <class TInputImage, class TOutputImage>
GradientVectorFlowImageFilter<TInputImage, TOutputImage>
::InitInterImage()
{
  m_IntermediateImage = TInputImage::New() ;
  m_IntermediateImage->SetLargestPossibleRegion( this->GetInput()->GetLargestPossibleRegion() );
  m_IntermediateImage->SetRequestedRegionToLargestPossibleRegion();
  m_IntermediateImage->SetBufferedRegion( m_IntermediateImage->GetRequestedRegion() );
  m_IntermediateImage->Allocate();

  InputImageIterator  inputIt(this->GetInput(), 
                                   this->GetInput()->GetBufferedRegion() );

  InputImageIterator  interIt(m_IntermediateImage, 
                                    m_IntermediateImage->GetBufferedRegion() );

  inputIt.GoTOBegin();
  interIt.GoToBegin();

  while ( !inputIt.IsAtEnd() ) {
    interIt.Set(inputIt.Get());
    ++interIt;
    ++inputIt;
  }

}

template <class TInputImage, class TOutputImage>
GradientVectorFlowImageFilter<TInputImage, TOutputImage>
::UpdateInterImage()
{
  OutputImageIterator  outputIt(this->GetOutput(), 
                                   this->GetOutput()->GetBufferedRegion() );

  InputImageIterator  interIt(m_IntermediateImage, 
                                    m_IntermediateImage->GetBufferedRegion() );

  outputIt.GoTOBegin();
  interIt.GoToBegin();

  while ( !outputIt.IsAtEnd() ) {
    interIt.Set(outputIt.Get());
    ++interIt;
    ++outputIt;
  }

}

template <class TInputImage, class TOutputImage>
GradientVectorFlowImageFilter<TInputImage, TOutputImage>
::PixelUpdate()
{
  OutputImageIterator  outputIt(this->GetOutput(), 
                                   this->GetOutput()->GetBufferedRegion() );

  InputImageIterator  interIt(m_IntermediateImage, 
                                    m_IntermediateImage->GetBufferedRegion() );

  InputImageIterator  inputIt(this->GetInput(), 
                                    this->GetInput()->GetBufferedRegion() );

  GradientType m_vec, m_vec1, m_vec2, m_vec3, m_vec4;

  int i, j;

  double b, c[ImageDimension], r;

  outputIt.GoTOBegin();
  interIt.GoToBegin();
  inputIt.GoToBegin();

  while ( !outputIt.IsAtEnd() ) {
    b = 0.0;
    for (i=0; i<ImageDimension; i++) {
      b += inputIt.Get[i]*inputIt.Get[i];
      c[i] = inputIt.Get[i];
    }
    for (i=0; i<ImageDimension; i++) {
      outputIt.Set()[i] = (1 - b*m_TimeStep)*interIt.Get()[i] + c[i]*m_TimeStep;
    }
    ++interIt;
    ++outputIt;
    ++inputIt;
  }
  
  m_LapacianFilter->SetInput(m_IntermediateImage);
  m_LapacianFilter->Update();

  InputImageIterator  intIt(m_LapacianFilter->GetOutput(), 
                                    m_LapacianFilter->GetOutput()->GetBufferedRegion() );

  outputIt.GoToBegin();
  intIt.GoToBegin();
  
  r = m_NoiseLevel*m_TimeStep;
  for (i=0; i<ImageDimension; i++) r /= m_Steps[i];

  while ( !outputIt.IsAtEnd() ) {
    for (i=0; i<ImageDimension; i++) outputIt.Set()[i] += r*intIt.Get()[i];
    ++intIt;
    ++outputIt;
  } 
}

} // namespace itk

#endif

