#ifndef _itkGradientVectorFlowImageFilter_txx
#define _itkGradientVectorFlowImageFilter_txx

namespace itk
{
template <class TInputImage, class TOutputImage>
GradientVectorFlowImageFilter<TInputImage, TOutputImage>
::GradientVectorFlowImageFilter()
{
  for (int i=0; i<ImageDimension; i++) m_Steps[i] = 1.0;
}

template <class TInputImage, class TOutputImage>
void
GradientVectorFlowImageFilter<TInputImage, TOutputImage>
::GenerateData()
{

  typename TOutputImage::Pointer output = this->GetOutput(0);

  output->SetLargestPossibleRegion( this->GetInput()->GetLargestPossibleRegion() );
  output->SetBufferedRegion( this->GetInput()->GetLargestPossibleRegion() );

  output->Allocate();

  m_TimeStep = 0.2/m_NoiseLevel; 

  int i=0;

  while ( i < 100 ) {
    UpdatePixels();
    i++;
  }

}

template <class TInputImage, class TOutputImage>
void
GradientVectorFlowImageFilter<TInputImage, TOutputImage>
::InitInterImage()
{

  m_IntermediateImage = TInputImage::New();
  m_IntermediateImage->SetLargestPossibleRegion( this->GetInput()->GetLargestPossibleRegion() );
  m_IntermediateImage->SetRequestedRegionToLargestPossibleRegion();
  m_IntermediateImage->SetBufferedRegion( m_IntermediateImage->GetRequestedRegion() );
  m_IntermediateImage->Allocate();

  for ( i=0; i<ImageDimension; i++ ) {
    m_InternalImage[i] = InternalImageType::New() ;
    m_InternalImage[i]->SetLargestPossibleRegion( this->GetInput()->GetLargestPossibleRegion() );
    m_InternalImage[i]->SetRequestedRegionToLargestPossibleRegion();
    m_InternalImage[i]->SetBufferedRegion( InternalImage[i]->GetRequestedRegion() );
    m_InternalImage[i]->Allocate();
  }

  InputImageIterator  inputIt(this->GetInput(), 
                                   this->GetInput()->GetBufferedRegion() );

  InputImageIterator  interIt(m_IntermediateImage, 
                                    m_IntermediateImage->GetBufferedRegion() );

  for (i=0; i<ImageDimension; i++) {
    InternalImageIterator  intIt[i](m_InternalImage[i], 
                                    m_InternalImage[i]->GetBufferedRegion() );
    intIt[i].GoToBegin();
  }

  inputIt.GoTOBegin();
  interIt.GoToBegin();

  while ( !inputIt.IsAtEnd() ) {
    interIt.Set(inputIt.Get());
    for (i=0; i<ImageDimension; i++) {
      intIt[i].Set(inputIt.Get()[i]);
      ++intIt[i];
    }
    ++interIt;
    ++inputIt;
  }

}

template <class TInputImage, class TOutputImage>
void
GradientVectorFlowImageFilter<TInputImage, TOutputImage>
::UpdateInterImage()
{

  OutputImageIterator  outputIt(this->GetOutput(), 
                                   this->GetOutput()->GetBufferedRegion() );

  InputImageIterator  interIt(m_IntermediateImage, 
                                    m_IntermediateImage->GetBufferedRegion() );

  for (i=0; i<ImageDimension; i++) {
    InternalImageIterator  intIt[i](m_InternalImage[i], 
                                    m_InternalImage[i]->GetBufferedRegion() );
    intIt[i].GoToBegin();
  }

  outputIt.GoTOBegin();
  interIt.GoToBegin();

  while ( !outputIt.IsAtEnd() ) {
    interIt.Set(outputIt.Get());
    for (i=0; i<ImageDimension; i++) {
      intIt[i].Set(inputIt.Get()[i]);
      ++intIt[i];
    }
    ++interIt;
    ++outputIt;
  }

}

template <class TInputImage, class TOutputImage>
void
GradientVectorFlowImageFilter<TInputImage, TOutputImage>
::UpdatePixels()
{

  OutputImageIterator  outputIt(this->GetOutput(), 
                                   this->GetOutput()->GetBufferedRegion() );

  InputImageIterator  interIt(m_IntermediateImage, 
                                    m_IntermediateImage->GetBufferedRegion() );

  InputImageIterator  inputIt(this->GetInput(), 
                                    this->GetInput()->GetBufferedRegion() );

  InternalImageIterator  intIt(m_LaplacianFilter->GetOutput(), 
                                    m_LaplacianFilter->GetOutput()->GetBufferedRegion() );

  PixelType m_vec;

  int i;

  double b, c[ImageDimension], r;

  outputIt.GoToBegin();
  interIt.GoToBegin();
  inputIt.GoToBegin();

  while ( !outputIt.IsAtEnd() ) {
    b = 0.0;
    for (i=0; i<ImageDimension; i++) {
      b = b + inputIt.Get()[i]*inputIt.Get()[i];
      c[i] = inputIt.Get()[i];
    }
    for (i=0; i<ImageDimension; i++) {
      m_vec[i] = (1 - b*m_TimeStep)*interIt.Get()[i] + c[i]*m_TimeStep;
    }
    outputIt.Set(m_vec);
    ++interIt;
    ++outputIt;
    ++inputIt;
  }
  
  for ( i=0; i<ImageDimension; i++ ) {
    m_LaplacianFilter->SetInput(m_InternalImages[i]);
    m_LaplacianFilter->Update();

    intIt.GoToBegin();
    outputIt.GoToBegin();
    intIt.GoToBegin();

    r = m_NoiseLevel*m_TimeStep;
    for (i=0; i<ImageDimension; i++) r = r/m_Steps[i];

    while ( !outputIt.IsAtEnd() ) {
      m_vec = outputIt.Get();
      m_vec[i] = m_vec[i] + r*intIt.Get();
      outputIt.Set(m_vec);
      ++intIt;
      ++outputIt;
    } 
  }
}

} // namespace itk

#endif

