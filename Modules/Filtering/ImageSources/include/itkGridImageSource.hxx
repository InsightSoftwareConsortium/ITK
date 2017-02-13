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
#ifndef itkGridImageSource_hxx
#define itkGridImageSource_hxx

#include "itkGaussianKernelFunction.h"
#include "itkGridImageSource.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkProgressReporter.h"

namespace itk
{
template< typename TOutputImage >
GridImageSource< TOutputImage >
::GridImageSource() :
  m_Scale( 255.0 )
{
  this->m_Sigma.Fill(0.5);
  this->m_GridSpacing.Fill(4.0);
  this->m_GridOffset.Fill(0.0);
  this->m_WhichDimensions.Fill(true);

  this->m_KernelFunction = dynamic_cast< KernelFunctionType * >(
    GaussianKernelFunction<double>::New().GetPointer() );
}

template< typename TOutputImage >
void
GridImageSource< TOutputImage >
::BeforeThreadedGenerateData()
{
  ImageType* output = this->GetOutput(0);

  this->m_PixelArrays = PixelArrayContainerType::New();
  m_PixelArrays->Initialize();

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( this->m_GridOffset[i] > this->m_GridSpacing[i] )
      {
      this->m_GridOffset[i] = this->m_GridSpacing[i];
      }
    PixelArrayType pixels = m_PixelArrays->CreateElementAt(i);
    pixels.set_size(this->GetSize()[i]);
    pixels.fill(1);
    if ( this->m_WhichDimensions[i] )
      {
      ImageLinearIteratorWithIndex< ImageType > It( output, output->GetRequestedRegion() );
      It.SetDirection(i);

      // Add two extra functions in the front and one in the back to ensure
      // coverage.
      unsigned int numberOfGaussians = Math::Ceil< unsigned int >(
        this->GetSize()[i] * output->GetSpacing()[i] / this->m_GridSpacing[i]) + 4u;
      for ( It.GoToBegin(); !It.IsAtEndOfLine(); ++It )
        {
        typename ImageType::IndexType index = It.GetIndex();
        typename ImageType::PointType point;
        output->TransformIndexToPhysicalPoint(index, point);

        RealType val = 0;
        for ( unsigned int j = 0; j < numberOfGaussians; j++ )
          {
          RealType num = point[i] - static_cast< RealType >( j - 2 ) * this->m_GridSpacing[i]
            - output->GetOrigin()[i] - this->m_GridOffset[i];
          val += this->m_KernelFunction->Evaluate(num / this->m_Sigma[i]);
          }
        pixels[index[i]] = val;
        }
      pixels = 1.0 - pixels / pixels.max_value();
      }
    this->m_PixelArrays->SetElement(i, pixels);
    }
}

template< typename TOutputImage >
void
GridImageSource< TOutputImage >
::ThreadedGenerateData(const ImageRegionType & outputRegionForThread, ThreadIdType threadId)
{
  // Support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  ImageType* output = this->GetOutput(0);

  ImageRegionIteratorWithIndex< ImageType > It(output, outputRegionForThread);

  for ( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    RealType val = 1.0;
    typename ImageType::IndexType index = It.GetIndex();
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      val *= this->m_PixelArrays->GetElement(i)[index[i]];
      }
    It.Set( static_cast<PixelType>(m_Scale * val) );
    progress.CompletedPixel();
    }
}

template< typename TOutputImage >
void
GridImageSource< TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Output image information: " << std::endl;
  os << indent << "   Scale      : " << this->GetScale() << std::endl;

  os << indent << "Grid information: " << std::endl;
  os << indent << "   WhichDimensions : " << this->GetWhichDimensions() << std::endl;
  os << indent << "   Kernel          : " << this->GetKernelFunction() << std::endl;
  os << indent << "   Sigma           : " << this->GetSigma() << std::endl;
  os << indent << "   Grid spacing    : " << this->GetGridSpacing() << std::endl;
  os << indent << "   Grid offset     : " << this->GetGridOffset() << std::endl;

  os << indent << "Pixel arrays: " << m_PixelArrays << std::endl;
}
} // end namespace itk

#endif
