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
#ifndef itkGradientVectorFlowImageFilter_hxx
#define itkGradientVectorFlowImageFilter_hxx
#include "itkGradientVectorFlowImageFilter.h"
#include "itkImageAlgorithm.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TInternalPixel >
GradientVectorFlowImageFilter< TInputImage, TOutputImage, TInternalPixel >
::GradientVectorFlowImageFilter()
{
  m_TimeStep = 0.001;
  m_NoiseLevel = 200;
  m_IterationNum = 2;
  m_LaplacianFilter = LaplacianFilterType::New();
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    m_Steps[i] = 1.0;
    }
}

template< typename TInputImage, typename TOutputImage, typename TInternalPixel >
void
GradientVectorFlowImageFilter< TInputImage, TOutputImage, TInternalPixel >
::GenerateData()
{
  typename TOutputImage::Pointer output = this->GetOutput();

  output->SetLargestPossibleRegion( this->GetInput()->GetLargestPossibleRegion() );
  output->SetBufferedRegion( this->GetInput()->GetLargestPossibleRegion() );

  output->Allocate();

  this->InitInterImage();

/*
 * According to Courant-Friedrichs-Lewy restriction (Eqn 18). However, the paper
 * assumes 2D images which leads to a too-large timestep for 3D images.
 *
 * CMax appears to be 2 * Dim since we are using laplacian operator which expands the front in
 * both directions of each dimension.
 *
 * See wikipedia article on Courant-Friedrichs-Lewy for more information.
 */
  m_TimeStep = 1.0/((1 << ImageDimension) * m_NoiseLevel);

  int i = 0;

  while ( i < m_IterationNum )
    {
    this->UpdatePixels();
    this->UpdateInterImage();
    i++;
    }
}


/*
 *  Precompute B(x, y), C1(x, y), C2(x, y).. etc. These images do not
 *  change throughout the course of the computation.
 *
 *  The intermediate image represents the image of the current time step.
 *
 *  The internal images are simply the intermediate image split into its
 *  component images. (Useful for calculating laplacian image in each direction
 *  later)
 */
template< typename TInputImage, typename TOutputImage, typename TInternalPixel >
void
GradientVectorFlowImageFilter< TInputImage, TOutputImage, TInternalPixel >
::InitInterImage()
{
  unsigned int i;
  double       b;
  PixelType    c_vec, m_vec;

  m_IntermediateImage = TInputImage::New();
  m_IntermediateImage->SetLargestPossibleRegion( this->GetInput()->GetLargestPossibleRegion() );
  m_IntermediateImage->SetRequestedRegionToLargestPossibleRegion();
  m_IntermediateImage->SetBufferedRegion( m_IntermediateImage->GetRequestedRegion() );
  m_IntermediateImage->Allocate();

  for ( i = 0; i < ImageDimension; i++ )
    {
    m_InternalImages[i] = InternalImageType::New();
    m_InternalImages[i]->SetLargestPossibleRegion( this->GetInput()->GetLargestPossibleRegion() );
    m_InternalImages[i]->SetRequestedRegionToLargestPossibleRegion();
    m_InternalImages[i]->SetBufferedRegion( m_InternalImages[i]->GetRequestedRegion() );
    m_InternalImages[i]->Allocate();
    }

  m_BImage = InternalImageType::New();
  m_BImage->SetLargestPossibleRegion( this->GetInput()->GetLargestPossibleRegion() );
  m_BImage->SetRequestedRegionToLargestPossibleRegion();
  m_BImage->SetBufferedRegion( m_BImage->GetRequestedRegion() );
  m_BImage->Allocate();

  m_CImage = InputImageType::New();
  m_CImage->SetLargestPossibleRegion( this->GetInput()->GetLargestPossibleRegion() );
  m_CImage->SetRequestedRegionToLargestPossibleRegion();
  m_CImage->SetBufferedRegion( m_BImage->GetRequestedRegion() );
  m_CImage->Allocate();

  InputImageConstIterator inputIt( this->GetInput(),
                                   this->GetInput()->GetBufferedRegion() );

  InputImageIterator intermediateIt( m_IntermediateImage,
                                     m_IntermediateImage->GetBufferedRegion() );

  itk::ImageAlgorithm::Copy(this->GetInput(),
                            m_IntermediateImage.GetPointer(),
                            this->GetInput()->GetLargestPossibleRegion(),
                            m_IntermediateImage->GetLargestPossibleRegion() );

  UpdateInterImage();

  InternalImageIterator BIt( m_BImage,
                             m_BImage->GetBufferedRegion() );

  InputImageIterator CIt( m_CImage,
                          m_CImage->GetBufferedRegion() );

  BIt.GoToBegin();
  CIt.GoToBegin();
  inputIt.GoToBegin();

/* Calculate b(x, y), c1(x, y), c2(x, y), etc.... (eqn 15) */
  while ( !inputIt.IsAtEnd() )
    {
    b = 0.0;
    m_vec = inputIt.Get();
    for ( i = 0; i < ImageDimension; i++ )
      {
      b = b + m_vec[i] * m_vec[i]; /*  b = fx^2 + fy^2 ... */
      }
    for ( i = 0; i < ImageDimension; i++ )
      {
      c_vec[i] =  b * m_vec[i]; /* c1 = b * fx, c2 = b * fy ... */
      }
    BIt.Set(b);
    CIt.Set(c_vec);

    ++CIt;
    ++BIt;
    ++inputIt;
    }
}

/*
 * Splits the intermediate image (image of vectors) into multiple
 * internal images (image of pixels)
 */
template< typename TInputImage, typename TOutputImage, typename TInternalPixel >
void
GradientVectorFlowImageFilter< TInputImage, TOutputImage, TInternalPixel >
::UpdateInterImage()
{
  unsigned int       i;
  InputImageIterator intermediateIt( m_IntermediateImage,
                                     m_IntermediateImage->GetBufferedRegion() );

  for ( i = 0; i < ImageDimension; i++ )
    {
    InternalImageIterator internalIt( m_InternalImages[i],
                                      m_InternalImages[i]->GetBufferedRegion() );

    internalIt.GoToBegin();
    intermediateIt.GoToBegin();

    while ( !intermediateIt.IsAtEnd() )
      {
      internalIt.Set(intermediateIt.Get()[i]);

      ++internalIt;
      ++intermediateIt;
      }
    }
}

/*
 * Calculates the next timestep
 */
template< typename TInputImage, typename TOutputImage, typename TInternalPixel >
void
GradientVectorFlowImageFilter< TInputImage, TOutputImage, TInternalPixel >
::UpdatePixels()
{
  OutputImageIterator outputIt( this->GetOutput(),
                                this->GetOutput()->GetBufferedRegion() );

  InputImageIterator intermediateIt( m_IntermediateImage,
                                     m_IntermediateImage->GetBufferedRegion() );

  InputImageConstIterator CIt( m_CImage,
                          m_CImage->GetBufferedRegion() );

  InternalImageConstIterator BIt( m_BImage,
                             m_BImage->GetBufferedRegion() );

  outputIt.GoToBegin();
  intermediateIt.GoToBegin();
  BIt.GoToBegin();
  CIt.GoToBegin();

  while ( !outputIt.IsAtEnd() )
    {
    const double b = BIt.Get();
    PixelType c_vec = CIt.Get();

    const double alpha = 1 - b * m_TimeStep; //first part of term 1, eqn 16

    PixelType m_vec;
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      const double first_term = alpha * intermediateIt.Get()[i]; //term1, eqn 16
      const double third_term = c_vec[i] * m_TimeStep;           //term3, eqn 16
      m_vec[i] = first_term + third_term;                        //we will add 2nd term later
      }
    outputIt.Set(m_vec);
    ++intermediateIt;
    ++outputIt;
    ++CIt;
    ++BIt;
    }

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    m_LaplacianFilter->SetInput(m_InternalImages[i]);
    m_LaplacianFilter->UpdateLargestPossibleRegion();

    InternalImageIterator laplacianIt( m_LaplacianFilter->GetOutput(),
                                      m_LaplacianFilter->GetOutput()->GetBufferedRegion() );

    laplacianIt.GoToBegin();
    outputIt.GoToBegin();
    intermediateIt.GoToBegin();

    double r = m_NoiseLevel * m_TimeStep; /** eqn 17, dx and dy are assumed to be 1 */
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      r = r / m_Steps[j];
      }

    while ( !outputIt.IsAtEnd() )
      {
      PixelType m_vec = outputIt.Get();
      m_vec[i] = m_vec[i] + r * laplacianIt.Get(); /** 2nd term of eqn 16 */
      outputIt.Set(m_vec);
      intermediateIt.Set(m_vec);
      ++intermediateIt;
      ++laplacianIt;
      ++outputIt;
      }
    }
}

template< typename TInputImage, typename TOutputImage, typename TInternalPixel >
void
GradientVectorFlowImageFilter< TInputImage, TOutputImage, TInternalPixel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "NoiseLevel: " << m_NoiseLevel << std::endl;
  os << indent << "IterationNum: " << m_IterationNum << std::endl;
  os << indent << "TimeStep: " << m_TimeStep << std::endl;
  if ( m_LaplacianFilter )
    {
    os << indent << "LaplacianFilter: " << m_LaplacianFilter << std::endl;
    }
  else
    {
    os << indent << "LaplacianFilter: (None)" << std::endl;
    }
}
} // namespace itk

#endif
