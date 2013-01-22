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
#ifndef __itkGradientVectorFlowImageFilter_hxx
#define __itkGradientVectorFlowImageFilter_hxx
#include "itkGradientVectorFlowImageFilter.h"

namespace itk
{
template< class TInputImage, class TOutputImage, class TInternalPixel >
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

template< class TInputImage, class TOutputImage, class TInternalPixel >
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
  m_TimeStep = 1/(std::pow(2.0, ImageDimension) * m_NoiseLevel);

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
template< class TInputImage, class TOutputImage, class TInternalPixel >
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

  for ( i = 0; i < ImageDimension; i++ )
    {
    InternalImageIterator internalIt( m_InternalImages[i],
                                      m_InternalImages[i]->GetBufferedRegion() );
    internalIt.GoToBegin();

    inputIt.GoToBegin();
    intermediateIt.GoToBegin();

    while ( !inputIt.IsAtEnd() )
      {
      intermediateIt.Set( inputIt.Get() ); /*  Set the intermediate image to the
                                            *  input image (gradient image) initially
                                            */
      internalIt.Set(inputIt.Get()[i]);    /*  Set the internal images to the
                                            *  respective direction of the input
                                            *  image initially
                                            */
      ++internalIt;
      ++intermediateIt;
      ++inputIt;
      }
    }

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
template< class TInputImage, class TOutputImage, class TInternalPixel >
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
template< class TInputImage, class TOutputImage, class TInternalPixel >
void
GradientVectorFlowImageFilter< TInputImage, TOutputImage, TInternalPixel >
::UpdatePixels()
{
  OutputImageIterator outputIt( this->GetOutput(),
                                this->GetOutput()->GetBufferedRegion() );

  InputImageIterator intermediateIt( m_IntermediateImage,
                                     m_IntermediateImage->GetBufferedRegion() );

  InputImageIterator CIt( m_CImage,
                          m_CImage->GetBufferedRegion() );

  InternalImageIterator BIt( m_BImage,
                             m_BImage->GetBufferedRegion() );

  PixelType m_vec, c_vec;

  unsigned int i;
  unsigned int j;

  double b;
  double r;

  outputIt.GoToBegin();
  intermediateIt.GoToBegin();
  BIt.GoToBegin();
  CIt.GoToBegin();

  while ( !outputIt.IsAtEnd() )
    {
    b = BIt.Get();
    c_vec = CIt.Get();

    for ( i = 0; i < ImageDimension; i++ )
      {
      m_vec[i] = ( 1 - b * m_TimeStep ) * intermediateIt.Get()[i] + c_vec[i] *
      m_TimeStep; // first and third term of eqn 16
      }
    outputIt.Set(m_vec);
    ++intermediateIt;
    ++outputIt;
    ++CIt;
    ++BIt;
    }

  for ( i = 0; i < ImageDimension; i++ )
    {
    m_LaplacianFilter->SetInput(m_InternalImages[i]);
    m_LaplacianFilter->UpdateLargestPossibleRegion();

    InternalImageIterator internalIt( m_LaplacianFilter->GetOutput(),
                                      m_LaplacianFilter->GetOutput()->GetBufferedRegion() );

    internalIt.GoToBegin();
    outputIt.GoToBegin();
    intermediateIt.GoToBegin();

    r = m_NoiseLevel * m_TimeStep;
    for ( j = 0; j < ImageDimension; j++ )
      {
      r = r / m_Steps[j];
      }

    while ( !outputIt.IsAtEnd() )
      {
      m_vec = outputIt.Get();
      m_vec[i] = m_vec[i] + r *internalIt. Get();
      outputIt.Set(m_vec);
      intermediateIt.Set(m_vec);
      ++intermediateIt;
      ++internalIt;
      ++outputIt;
      }
    }
}

template< class TInputImage, class TOutputImage, class TInternalPixel >
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
