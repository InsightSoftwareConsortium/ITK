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
#ifndef __itkLabelMapMaskImageFilter_hxx
#define __itkLabelMapMaskImageFilter_hxx

#include "itkLabelMapMaskImageFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkImageAlgorithm.h"

namespace itk {

template <typename TInputImage, typename TOutputImage>
LabelMapMaskImageFilter<TInputImage, TOutputImage>
::LabelMapMaskImageFilter()
{
  this->SetNumberOfRequiredInputs(2);
  m_Label = NumericTraits< InputImagePixelType >::One;
  m_BackgroundValue = NumericTraits< OutputImagePixelType >::Zero;
  m_Negated = false;
  m_Crop = false;
  m_CropBorder.Fill( 0 );
}

template <typename TInputImage, typename TOutputImage>
void
LabelMapMaskImageFilter<TInputImage, TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
  if ( !input )
    { return; }
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}

template <typename TInputImage, typename TOutputImage>
void
LabelMapMaskImageFilter<TInputImage, TOutputImage>
::GenerateOutputInformation()
{

  if( m_Crop )
    {
    const InputImageType * input = this->GetInput();

    if( !(input->GetMTime() > m_CropTimeStamp) && !(this->GetMTime() > m_CropTimeStamp) )
      {
      // early exit, crop sizes already computed
      // std::cout << "Don't recompute the output size again." << std::endl;
      // std::cout << "LargestPossibleRegion: " << this->GetOutput()->GetLargestPossibleRegion() << std::endl;
      // std::cout << "BufferedRegion: " << this->GetOutput()->GetBufferedRegion() << std::endl;
      // std::cout << "RequestedRegion: " << this->GetOutput()->GetRequestedRegion() << std::endl;
      return;
      }

    // first, call the default implementation, to be sure to forgot nothing
    Superclass::GenerateOutputInformation();

    // update the input if needed
    if( input->GetSource())
      {
      ProcessObject * upstream = input->GetSource();
      if (upstream)
        {
        // this->SetInput(NULL);
        // std::cout << "Update the input (again?)." << std::endl;
        upstream->Update();
        // this->SetInput(input);
        }
      }

    // Prefetch image region and size
    InputImageRegionType cropRegion = input->GetLargestPossibleRegion();

    // now the output image size can be computed
    if( m_Negated )
      {
      if( input->GetBackgroundValue() != m_Label )
        {
        // the "bad" case - the zone outside the object is at least partially
        // covered by the background, which is not explicitely defined.

        // simply do nothing for now
        // TODO: implement that part
        itkWarningMacro( << "Cropping according to background label is no yet implemented. The full image will be used." );

        }
      else
        {
        // compute the bounding box of all the objects which don't have that label
        IndexType mins;
        mins.Fill( NumericTraits< IndexValueType >::max() );
        IndexType maxs;
        maxs.Fill( NumericTraits< IndexValueType >::NonpositiveMin() );
        for( typename InputImageType::ConstIterator loit( this->GetInput() );
             ! loit.IsAtEnd();
             ++loit )
          {
          if( loit.GetLabel() != m_Label )
            {
            // iterate over all the lines
            typename LabelObjectType::ConstLineIterator lit( loit.GetLabelObject() );
            while( ! lit.IsAtEnd() )
              {
              const IndexType & idx = lit.GetLine().GetIndex();
              LengthType length = lit.GetLine().GetLength();

              // update the mins and maxs
              for( int i=0; i<ImageDimension; i++)
                {
                if( idx[i] < mins[i] )
                  {
                  mins[i] = idx[i];
                  }
                if( idx[i] > maxs[i] )
                  {
                  maxs[i] = idx[i];
                  }
                }
              // must fix the max for the axis 0
              if( idx[0] + (OffsetValueType)length > maxs[0] )
                {
                maxs[0] = idx[0] + length - 1;
                }
              ++lit;
              }
            }
          }

          // final computation
          SizeType regionSize;
          for( int i=0; i<ImageDimension; i++ )
            {
            regionSize[i] = maxs[i] - mins[i] + 1;
            }
          cropRegion.SetIndex( mins );
          cropRegion.SetSize( regionSize );

        }
      }
    else
      {
      if( input->GetBackgroundValue() == m_Label )
        {
        // the other "bad" case - the label we want is not defined as a label object,
        // but implicitely, in the zones not defined.

        // simply do nothing for now
        // TODO: implement that part
        itkWarningMacro( << "Cropping according to background label is no yet implemented. The full image will be used." );

        }
      else
        {
        // just find the bounding box of the object with that label

        const LabelObjectType * labelObject = input->GetLabelObject( m_Label );
        IndexType mins;
        mins.Fill( NumericTraits< IndexValueType >::max() );
        IndexType maxs;
        maxs.Fill( NumericTraits< IndexValueType >::NonpositiveMin() );
        // iterate over all the lines
        typename LabelObjectType::ConstLineIterator lit( labelObject );
        while( ! lit.IsAtEnd() )
          {
          const IndexType & idx = lit.GetLine().GetIndex();
          LengthType length = lit.GetLine().GetLength();

          // update the mins and maxs
          for( int i=0; i<ImageDimension; i++)
            {
            if( idx[i] < mins[i] )
              {
              mins[i] = idx[i];
              }
            if( idx[i] > maxs[i] )
              {
              maxs[i] = idx[i];
              }
            }
          // must fix the max for the axis 0
          if( idx[0] + (OffsetValueType)length > maxs[0] )
            {
            maxs[0] = idx[0] + length - 1;
            }
          ++lit;
          }
          // final computation
          SizeType regionSize;
          for( int i=0; i<ImageDimension; i++ )
            {
            regionSize[i] = maxs[i] - mins[i] + 1;
            }
          cropRegion.SetIndex( mins );
          cropRegion.SetSize( regionSize );

        }
      }

    // pad by the crop border, but take care to not be larger than the largest
    // possible region of the input image
    cropRegion.PadByRadius( m_CropBorder );
    cropRegion.Crop( input->GetLargestPossibleRegion() );

    // finally set that region as the largest output region
    this->GetOutput()->SetLargestPossibleRegion( cropRegion );

    m_CropTimeStamp.Modified();

    }
  else
    {
    // no crop -> use the default implementation
    Superclass::GenerateOutputInformation();
    }

  // std::cout << "LargestPossibleRegion: " << this->GetOutput()->GetLargestPossibleRegion() << std::endl;
  // std::cout << "BufferedRegion: " << this->GetOutput()->GetBufferedRegion() << std::endl;
  // std::cout << "RequestedRegion: " << this->GetOutput()->GetRequestedRegion() << std::endl;

}

template <typename TInputImage, typename TOutputImage>
void
LabelMapMaskImageFilter<TInputImage, TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template <typename TInputImage, typename TOutputImage>
void
LabelMapMaskImageFilter<TInputImage, TOutputImage>
::BeforeThreadedGenerateData()
{
  ThreadIdType nbOfThreads = this->GetNumberOfThreads();
  if( itk::MultiThreader::GetGlobalMaximumNumberOfThreads() != 0 )
    {
    nbOfThreads = std::min( this->GetNumberOfThreads(), itk::MultiThreader::GetGlobalMaximumNumberOfThreads() );
    }
  // number of threads can be constrained by the region size, so call the SplitRequestedRegion
  // to get the real number of threads which will be used
  typename TOutputImage::RegionType splitRegion;  // dummy region - just to call the following method
  nbOfThreads = this->SplitRequestedRegion(0, nbOfThreads, splitRegion);
  // std::cout << "nbOfThreads: " << nbOfThreads << std::endl;

  m_Barrier = Barrier::New();
  m_Barrier->Initialize( nbOfThreads );

  Superclass::BeforeThreadedGenerateData();

}


template <typename TInputImage, typename TOutputImage>
void
LabelMapMaskImageFilter<TInputImage, TOutputImage>
::ThreadedGenerateData( const OutputImageRegionType& outputRegionForThread, ThreadIdType threadId )
{
  ProgressReporter progress( this, threadId, 1 );
  OutputImageType * output = this->GetOutput();
  InputImageType * input = const_cast<InputImageType *>(this->GetInput());
  const OutputImageType * input2 = this->GetFeatureImage();

  // we will keep the values from the feature image if the same pixel in the label image
  // equals the label given by the user. The other pixels are set to the background value.
  if( ( input->GetBackgroundValue() == m_Label ) ^ m_Negated )
    {
    // the user want the mask to be the background of the label collection image
    // copy the feature image to the output image

    // copy input2 region to output
    ImageAlgorithm::Copy( input2, output, outputRegionForThread, outputRegionForThread );

    }
  else
    {
    ImageRegionIterator< OutputImageType > outputIt( output, outputRegionForThread );

    for ( outputIt.GoToBegin(); !outputIt.IsAtEnd(); ++outputIt )
      {
      outputIt.Set( m_BackgroundValue );
      }
    }

  // wait for the other threads to complete that part
  m_Barrier->Wait();

  if( input->GetBackgroundValue() == m_Label )
    {
    // and delegate to the superclass implementation to use the thread support for the label objects
    Superclass::ThreadedGenerateData( outputRegionForThread, threadId );
    }
  else
    {
    // need only one thread - take the first one
    if( threadId == 0 )
      {
      const LabelObjectType * labelObject = this->GetLabelMap()->GetLabelObject( m_Label );

      if( !m_Negated )
        {
        typename LabelObjectType::ConstIndexIterator it( labelObject );
        while( ! it.IsAtEnd() )
          {
          const IndexType & idx = it.GetIndex();
          output->SetPixel( idx, input2->GetPixel( idx ) );
          ++it;
          }
        }
      else
        {
        // and mark the label object as background

        // should we take care to not write outside the image ?
        bool testIdxIsInside = m_Crop && ( input->GetBackgroundValue() == m_Label ) ^ m_Negated;
        RegionType outputRegion = output->GetLargestPossibleRegion();

        typename LabelObjectType::ConstIndexIterator it( labelObject );
        while( ! it.IsAtEnd() )
          {
          const IndexType & idx = it.GetIndex();
          if( !testIdxIsInside || outputRegion.IsInside( idx ) )
            {
            output->SetPixel( idx, m_BackgroundValue );
            }
          ++it;
          }
        }
      }
    }
}


template<typename TInputImage, typename TOutputImage>
void
LabelMapMaskImageFilter<TInputImage, TOutputImage>
::ThreadedProcessLabelObject( LabelObjectType * labelObject )
{
  OutputImageType * output = this->GetOutput();
  InputImageType * input = const_cast<InputImageType *>(this->GetInput());
  const OutputImageType * input2 = this->GetFeatureImage();

  if( !m_Negated )
    {
    // we will keep the values from the feature image if the same pixel in the label image
    // equals the label given by the user. The other pixels are set to the background value.

    // should we take care to not write outside the image ?
    bool testIdxIsInside = m_Crop && ( input->GetBackgroundValue() == m_Label ) ^ m_Negated;
    RegionType outputRegion = output->GetLargestPossibleRegion();

    // the user want the mask to be the background of the label collection image
    typename LabelObjectType::ConstIndexIterator it( labelObject );
    while( ! it.IsAtEnd() )
      {
      const IndexType & idx = it.GetIndex();
      if( !testIdxIsInside || outputRegion.IsInside( idx ) )
        {
        output->SetPixel( idx, m_BackgroundValue );
        }
      ++it;
      }
    }
  else
    {
    // we will keep the pixels from the feature image if the same pixel from the label image
    // is not equal to the label provided by the user. The pixels with the label provided by the
    // user are set to the background value

    // and copy the feature image where the label objects are
    typename LabelObjectType::ConstIndexIterator it( labelObject );
    while( ! it.IsAtEnd() )
      {
      const IndexType & idx = it.GetIndex();
      output->SetPixel( idx, input2->GetPixel( idx ) );
      ++it;
      }
    }
}


template<typename TInputImage, typename TOutputImage>
void
LabelMapMaskImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Label: " << static_cast<typename NumericTraits<LabelType>::PrintType>(m_Label) << std::endl;
  os << indent << "BackgroundValue: " << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_BackgroundValue) << std::endl;
  os << indent << "Negated: " << m_Negated << std::endl;
  os << indent << "Crop: " << m_Crop << std::endl;
  os << indent << "CropBorder: " << m_CropBorder << std::endl;
}


}// end namespace itk
#endif
