#ifndef _itkContourDirectedMeanDistanceImageFilter_txx
#define _itkContourDirectedMeanDistanceImageFilter_txx

#include "itkContourDirectedMeanDistanceImageFilter.h"

#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkOffset.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkNumericTraits.h"
#include "itkDanielssonDistanceMapImageFilter.h"
#include "itkProgressReporter.h"

namespace itk {


template<class TInputImage1, class TInputImage2>
ContourDirectedMeanDistanceImageFilter<TInputImage1, TInputImage2>
::ContourDirectedMeanDistanceImageFilter(): m_MeanDistance(1)
{

  // this filter requires two input images
  this->SetNumberOfRequiredInputs( 2 );

  m_DistanceMap = NULL;
  m_ContourDirectedMeanDistance = NumericTraits<RealType>::Zero;      
}


template<class TInputImage1, class TInputImage2>
void
ContourDirectedMeanDistanceImageFilter<TInputImage1, TInputImage2>
::SetInput2( const TInputImage2 * image )
{
  this->SetNthInput(1, const_cast<TInputImage2 *>( image ) );      
}


template<class TInputImage1, class TInputImage2>
const typename ContourDirectedMeanDistanceImageFilter<TInputImage1, TInputImage2>
::InputImage2Type *
ContourDirectedMeanDistanceImageFilter<TInputImage1, TInputImage2>
::GetInput2()
{
  return static_cast< const TInputImage2 * >
    (this->ProcessObject::GetInput(1));
}



template<class TInputImage1, class TInputImage2>
void
ContourDirectedMeanDistanceImageFilter<TInputImage1, TInputImage2>
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  // this filter requires:
  // - the largeset possible region of the first image
  // - the corresponding region of the second image
  if ( this->GetInput1() )
    {
    InputImage1Pointer image =
      const_cast< InputImage1Type * >( this->GetInput1() );
    image->SetRequestedRegionToLargestPossibleRegion();

    if ( this->GetInput2() )
      {
      InputImage2Pointer image =
        const_cast< InputImage2Type * >( this->GetInput2() );
      image->SetRequestedRegion( 
        this->GetInput1()->GetRequestedRegion() );
      }

    }
}


template<class TInputImage1, class TInputImage2>
void
ContourDirectedMeanDistanceImageFilter<TInputImage1, TInputImage2>
::EnlargeOutputRequestedRegion(DataObject *data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}


template<class TInputImage1, class TInputImage2>
void
ContourDirectedMeanDistanceImageFilter<TInputImage1, TInputImage2>
::AllocateOutputs()
{
  // Pass the first input through as the output
  InputImage1Pointer image =
    const_cast< TInputImage1 * >( this->GetInput1() );
  this->GraftOutput( image );
}


template<class TInputImage1, class TInputImage2>
void
ContourDirectedMeanDistanceImageFilter<TInputImage1, TInputImage2>
::BeforeThreadedGenerateData()
{
  int numberOfThreads = this->GetNumberOfThreads();

  // Resize the thread temporaries
  m_MeanDistance.SetSize(numberOfThreads); 
  
  // Initialize the temporaries
  m_MeanDistance.Fill(NumericTraits<RealType>::Zero);

  // Compute Danielsson distance from non-zero pixels in the second image
  typedef itk::DanielssonDistanceMapImageFilter<InputImage2Type,DistanceMapType>
    FilterType;

  typename FilterType::Pointer filter = FilterType::New();

  filter->SetInput( this->GetInput2() );
  filter->Update();

  m_DistanceMap = filter->GetOutput();

}


template<class TInputImage1, class TInputImage2>
void
ContourDirectedMeanDistanceImageFilter<TInputImage1, TInputImage2>
::AfterThreadedGenerateData()
{
  int i;
    
  int numberOfThreads = this->GetNumberOfThreads();

  m_ContourDirectedMeanDistance = NumericTraits<RealType>::Zero;

 // find mean over all threads
  for( i = 0; i < numberOfThreads; i++)
    {
     m_ContourDirectedMeanDistance += m_MeanDistance[i];   
     
    }
  m_ContourDirectedMeanDistance= m_ContourDirectedMeanDistance/numberOfThreads;

}



template<class TInputImage1, class TInputImage2>
void
ContourDirectedMeanDistanceImageFilter<TInputImage1, TInputImage2>
::ThreadedGenerateData(const RegionType& outputRegionForThread,
                       int threadId) 
{
  double countOfRegion = NumericTraits<double>::Zero;

  unsigned int i;
  ZeroFluxNeumannBoundaryCondition<InputImage1Type> nbc;
    
  ConstNeighborhoodIterator<InputImage1Type> bit;
    
  typename  InputImage1Type::ConstPointer input  = this->GetInput();
    
  // Find the data-set boundary "faces"
  typedef typename InputImage1Type::SizeType InputSizeType;
  InputSizeType radius;
  radius.Fill(1);
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImage1Type>::FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImage1Type> bC;
  faceList = bC(input, outputRegionForThread, radius);
    
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImage1Type>::FaceListType::iterator fit;
    
  typedef typename InputImage1Type::PixelType   InputPixelType;

  // support progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());
    

  ImageRegionConstIterator<DistanceMapType> it2 (m_DistanceMap, outputRegionForThread);
        
  // Process each of the boundary faces.  These are N-d regions which border
  // the edge of the buffer.
  for (fit=faceList.begin(); fit != faceList.end(); ++fit)
    { 
    bit = ConstNeighborhoodIterator<InputImage1Type>(radius, input, *fit);
    unsigned int neighborhoodSize = bit.Size();

    bit.OverrideBoundaryCondition(&nbc);
    bit.GoToBegin();

    bool bIsOnContour;
      
    while ( ! bit.IsAtEnd() )
      {
        
      // first test
      // if current pixel is not on, let's continue
      if( bit.GetCenterPixel() != itk::NumericTraits< InputPixelType >::Zero )
        {
          
        bIsOnContour = false;
          
        for (i = 0; i < neighborhoodSize; ++i)
          {
          // second test if at least one neighbour pixel is off
          // the center pixel belongs to contour
          if( bit.GetPixel(i) ==  itk::NumericTraits< InputPixelType >::Zero )
            {
            bIsOnContour = true;
            break;
            }
          }
          
        // set pixel center pixel value weither it is or not on contour
        if( bIsOnContour )
          {
          countOfRegion++;
          m_MeanDistance[threadId] += it2.Get();
          }
        }
        
      ++bit;
      ++it2;
      progress.CompletedPixel();
      }
      
      
    }
    m_MeanDistance[ threadId ] = m_MeanDistance[ threadId ] / countOfRegion;
}


template<class TInputImage1, class TInputImage2>
void 
ContourDirectedMeanDistanceImageFilter<TInputImage1, TInputImage2>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "ContourDirectedMeanDistance: "  
     << m_ContourDirectedMeanDistance << std::endl;
}


}// end namespace itk
#endif
