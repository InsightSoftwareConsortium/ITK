/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSparseFieldLevelSetImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkSparseFieldLevelSetImageFilter_txx_
#define __itkSparseFieldLevelSetImageFilter_txx_

#include "itkSparseFieldLevelSetImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "itkZeroCrossingImageFilter.h"
#include "itkRandomAccessNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
#include "itkOffset.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkVector.h"
#include "itkSubtractImageFilter.h"


#include <list>

namespace itk {

  // Initialize the static members
  template<class TInputImage, class TOutputImage>
   int SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>::ACTIVE_STATUS = 10;
  template<class TInputImage, class TOutputImage>
    int SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>::CHANGING_STATUS = 200;
  template<class TInputImage, class TOutputImage>
    int SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>::UP_STATUS = 300;
  template<class TInputImage, class TOutputImage>
    int SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>::DOWN_STATUS = 400;
  template<class TInputImage, class TOutputImage>
    int SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>::INSIDE = 1;
  template<class TInputImage, class TOutputImage>
    int SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>::OUTSIDE = -1;
  template<class TInputImage, class TOutputImage>
    int SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>::ACTIVE = 0;
  template<class TInputImage, class TOutputImage>
    float SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>::DIFFERENCE_FACTOR = 1.0;
 template<class TInputImage, class TOutputImage>
    float SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>:: CHANGE_FACTOR = 0.5;


template<class TInputImage, class TOutputImage>
void SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::Initialize()
{


  /* Initialize the lists  */

  int ThreadNum = this->GetNumberOfThreads();

  OUTSIDE_VALUE  = CHANGE_FACTOR * (2 * m_NumberOfLayers +1);

  m_InStatus = new int[m_NumberOfLayers];
  m_OutStatus = new int[m_NumberOfLayers];

  for(int i = 0; i < m_NumberOfLayers; i++)
    {
      m_InStatus[i] = GetStatus(INSIDE, i);
      m_OutStatus[i] = GetStatus(OUTSIDE,i);
    }

  m_ActiveLists = new LevelSetNodeListType[ThreadNum];
  //  m_BoundaryActiveLists = new LevelSetNodeListType[ThreadNum];

  m_InsideLists = new LevelSetNodeListType *[ThreadNum];
  m_OutsideLists = new LevelSetNodeListType *[ThreadNum];

  m_StatusUpLists = new LevelSetNodeListType *[ThreadNum];
  m_StatusDownLists = new LevelSetNodeListType *[ThreadNum];

  m_FreeLists = new LevelSetNodeListType[ThreadNum];

  m_ActiveList = new  LevelSetNodeListType;
  m_InsideList = new  LevelSetNodeListType[m_NumberOfLayers];
  m_OutsideList = new  LevelSetNodeListType[m_NumberOfLayers];

  typename OutputImageType::Pointer output = this->GetOutput();
  
  
  //This is pre-allocate nodes that possible use. The FreeLists are only used
  // in ThreadedApplyUpdate(). New memory is only possibly needed in updating
  // the innermost and outermost layers.


  if(m_MaxPreAllocateNodes == 0)
    for(int i = 0; i < ImageDimension; i++)
      m_MaxPreAllocateNodes +=( (output->GetRequestedRegion()).GetSize())[i]; 

  m_MaxPreAllocateNodes =  (m_MaxPreAllocateNodes/ImageDimension) *
    (m_MaxPreAllocateNodes/ImageDimension)/ThreadNum;

  for(int i = 0; i < ThreadNum; i ++)
    {
      m_StatusUpLists[i] = new LevelSetNodeListType[2];
      m_StatusDownLists[i] = new LevelSetNodeListType[2];
      m_InsideLists[i] = new LevelSetNodeListType[m_NumberOfLayers];
      m_OutsideLists[i] = new LevelSetNodeListType[m_NumberOfLayers];
     

      /* PreAllocate Memory */
      for(int j = 0; j < m_MaxPreAllocateNodes; j ++)
        m_FreeLists[i].push_back(new LevelSetNodeType());
    }


  //If the iso-surface value is set different from zero, then subtract 
  // it from the image, making it convenient to ccnstruct lists.

  ImageRegionIterator<InputImageType>  sit(this->GetInput(),
                                            this->GetInput()->GetRequestedRegion());  
  ImageRegionIterator<InputImageType>  oit(this->GetOutput(),
                                            this->GetOutput()->GetRequestedRegion());  


  if(m_IsoValue != 0)

    {
      while(!sit.IsAtEnd()) 
        { 
          sit.Value() = sit.Value() - m_IsoValue;
          oit.Value() = sit.Value();
          ++sit;
          ++oit;
        } 
    } 

  //allocate buffer for m_StatusImage

  m_StatusImage->SetLargestPossibleRegion(output->GetLargestPossibleRegion());
  m_StatusImage->SetRequestedRegion(output->GetRequestedRegion());
  m_StatusImage->SetBufferedRegion(output->GetBufferedRegion());
  m_StatusImage->Allocate();

  cout<<"constructing lists"<<endl;
  ConstructLists();
  cout<<"finished constructing lists"<<endl;
}


//When the split method changes, this will also change.

template<class TInputImage, class TOutputImage>
int
SparseFieldLevelSetImageFilter<TInputImage, TOutputImage> 
::GetThreadNum(IndexType Index)
{
  int splitAxis = ImageDimension - 1;
  int threadCount = this->GetNumberOfThreads();
  typename OutputImageType::Pointer output = this->GetOutput();

   typename OutputImageType::SizeType requestedRegionSize
    = output->GetRequestedRegion().GetSize();
  

  int threadNum =  Index[splitAxis]/(requestedRegionSize[splitAxis]/threadCount);

   return threadNum;
}
 

template<class TInputImage, class TOutputImage>
void
SparseFieldLevelSetImageFilter<TInputImage, TOutputImage> 
::ConstructLists()
{

  typedef typename OutputImageType::PixelType PixelType;
  typedef typename OutputImageType::RegionType RegionType;
  typedef typename OutputImageType::SizeType   SizeType;
  typedef typename OutputImageType::SizeValueType   SizeValueType;
  //  typedef typename OutputImageType::IndexType  IndexType;
  typedef typename OutputImageType::IndexValueType  IndexValueType;
  typedef typename FiniteDifferenceFunctionType::BoundaryNeighborhoodType
    BoundaryIteratorType;
  typedef typename FiniteDifferenceFunctionType::NeighborhoodType
    NeighborhoodIteratorType;

  //typename InputImageType::Pointer  m_StatusImage;

  // calculate the zeroCrossing of the input image

  ZeroCrossingImageFilter<InputImageType, InputImageType>::Pointer zeroCrossingFilter = ZeroCrossingImageFilter<InputImageType, InputImageType>::New();
  zeroCrossingFilter->SetInput(this->GetInput());
  zeroCrossingFilter->Update();

  ImageRegionIterator<OutputImageType> ait;
  ImageRegionIterator<OutputImageType> it;
    
  ait = ImageRegionIterator<OutputImageType>(m_StatusImage, m_StatusImage->GetRequestedRegion());
  it  = ImageRegionIterator<OutputImageType>(zeroCrossingFilter->GetOutput(),zeroCrossingFilter->GetOutput()->GetRequestedRegion());

  while(!ait.IsAtEnd())
    {
      ait.Value() = it.Value();
      ++ait;
      ++it;
    }


  // Any problem here ?
  //  m_StatusImage = zeroCrossingFilter->GetOutput();
  
  
  typename OutputImageType::Pointer output = this->GetOutput();
  typename  InputImageType::Pointer input  = this->GetInput();


  SizeType radius;
  for(int i = 0; i < ImageDimension; i++)  radius[i] = 1;

  //  ZeroFluxNeumannBoundaryCondition<OutputImageType> nbc;
  RandomAccessNeighborhoodIterator<OutputImageType> nit;
  RandomAccessNeighborhoodIterator<OutputImageType> oit;
  RandomAccessNeighborhoodIterator<OutputImageType> bit;

  
  PixelType zero = NumericTraits<PixelType>::Zero;

  // Construct the active lists

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<OutputImageType>::
    FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<OutputImageType> bC;
  faceList = bC(m_StatusImage, m_StatusImage->GetRequestedRegion(),  radius);

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<OutputImageType>::
    FaceListType::iterator fit;
  

  fit = faceList.begin();
  

  nit = RandomAccessNeighborhoodIterator<OutputImageType>(radius, m_StatusImage, *fit);
  oit = RandomAccessNeighborhoodIterator<OutputImageType>(radius, output, *fit);
  
  ait = ImageRegionIterator<OutputImageType>(m_StatusImage, *fit);
  
  it = ImageRegionIterator<OutputImageType>(output, *fit);

  nit.GoToBegin();
  oit.GoToBegin();
  ait.GoToBegin();
  it.GoToBegin();

  int idx;
  IndexType pixelIndex;
  LevelSetNodeType  *newItem;

  //construct the activeList from the non-boundary region;
  while(!ait.IsAtEnd())
    {
      if(ait.Value() != zero || it.Value() == zero)
        {
          pixelIndex = ait.GetIndex();
          idx = GetThreadNum(pixelIndex);
          newItem = new LevelSetNodeType(pixelIndex, zero);
          m_ActiveLists[idx].push_back(newItem);
          m_ActiveList->push_back(newItem);

          ait.Value() = ACTIVE_STATUS;
          it.Value() = zero;

        }
      else if(it.Value() > zero)
        it.Value() = OUTSIDE_VALUE;
      else
        it.Value() = -OUTSIDE_VALUE;

      ++ait;
      ++it;
    }


  //Add active pixels on the boundary to the activeList
  for(++fit; fit!=faceList.end(); ++fit)
    {
      
      ait = ImageRegionIterator<OutputImageType>(m_StatusImage, *fit);
      it  = ImageRegionIterator<OutputImageType>(output, *fit);
      
      while(!ait.IsAtEnd())
        {
          if(ait.Value() || it.Value() == zero)
            {
              pixelIndex = ait.GetIndex();
              idx = GetThreadNum(pixelIndex);
              //newItem = new LevelSetNodeType(pixelIndex, zero);
              //m_ActiveLists[idx].push_back(newItem);
              //m_ActiveList->push_back(newItem);
              ait.Value() = ACTIVE_STATUS;
              it.Value() = zero;
            }
          else if(it.Value() > zero)
            it.Value() = OUTSIDE_VALUE;
          else
            it.Value() = -OUTSIDE_VALUE;

          ++ait;
          ++it;
        }
    }
  
  

  int displacement[2 * ImageDimension];
  Offset<ImageDimension> idxOffset[2 * ImageDimension];

  //set offset of neighbors to the center pixel
  
  for(int i = 0; i < 2 * ImageDimension; i++)
    for( int j = 0; j < ImageDimension; j++)
      idxOffset[i][j] = 0;

  for ( int i = 0 ; i < 2 * ImageDimension; i+= 2)
    {
      
      displacement[i] = - nit.GetStride(i/2);
      displacement[i+1] = nit.GetStride(i/2);
      
      idxOffset[i][i/2] = - 1;
      idxOffset[i+1][i/2] = 1;
 
    }

  // Now construct the insideLayer[0] & outsideLayer[0]

  Offset<ImageDimension> offset; 
  IndexType startIndex = nit.GetIndex();
  int center = nit.Size()/2;
  LevelSetNodeType * currentItem;


  //initialize the value of active layer pixel/voxels
  LevelSetNodeListIteratorType iit;
  PixelType dpx[ImageDimension], dmx[ImageDimension], vec[ImageDimension];
  float MIN_NORM = 1.0e-6;
  float len = 0;
  PixelType dist;

  iit = m_ActiveList->begin();
  while(iit!= m_ActiveList->end())
    {

      nit.GoToBegin();
      oit.GoToBegin();
      currentItem = *iit;
      pixelIndex = currentItem->GetIndex();
      offset = pixelIndex - startIndex;
      nit +=  offset;
      oit +=  offset;
      
      for(int i = 0; i < ImageDimension; i++)
        {
          dpx[i] = nit.GetPixel(center + nit.GetStride(i)) -
            nit.GetCenterPixel();

          dmx[i] = nit.GetCenterPixel() - nit.GetPixel(center -
                                                       nit.GetStride(i));
          if(vnl_math_abs(dpx[i]) > vnl_math_abs(dmx[i]))
            vec[i] = dpx[i];
          else
            vec[i] = dmx[i];
          len += vec[i] * vec[i];
        }

      len = vnl_math_sqrt(len) + MIN_NORM;
      dist = oit.GetCenterPixel()/len;
      dist = vnl_math_min(vnl_math_max(-CHANGE_FACTOR,dist), CHANGE_FACTOR);
      oit.SetCenterPixel(dist);
      ++iit;

    }

  while(!m_ActiveList->empty())
    {

      nit.GoToBegin();
      oit.GoToBegin();

      currentItem = m_ActiveList->front();
      pixelIndex = currentItem->GetIndex();
      offset = pixelIndex - startIndex;
      nit +=  offset;
      oit +=  offset;



      for (int i = 0; i < 2 * ImageDimension; i ++)
        {
          int neighbor = center + displacement[i];
          PixelType n_status = nit.GetPixel(neighbor);
          
          if(n_status == zero)
            {
              if(oit.GetPixel(neighbor) > zero )
                {
                  idx = GetThreadNum(pixelIndex+ idxOffset[i]);


                  newItem = new LevelSetNodeType(pixelIndex + idxOffset[i], zero);

                  m_InsideLists[idx][0].push_back(newItem);
                  m_InsideList[0].push_back(newItem);
                  
                  nit.SetPixel(neighbor, m_InStatus[0]);
                  oit.SetPixel(neighbor, DIFFERENCE_FACTOR);
                }
              else if(oit.GetPixel(neighbor) < zero )
                {
                  
                  idx = GetThreadNum(pixelIndex+ idxOffset[i]);
                  newItem = new LevelSetNodeType(pixelIndex + idxOffset[i], zero);
                  
                  m_OutsideLists[idx][0].push_back(newItem);
                  m_OutsideList[0].push_back(newItem);


                  nit.SetPixel(neighbor, m_OutStatus[0]);
                  oit.SetPixel(neighbor, -DIFFERENCE_FACTOR);
                }
            }
        }

      m_ActiveList->pop_front();
    }
  



  //construct other inside/outside lists

  for(int k = 1; k < m_NumberOfLayers; k ++)
    {

      while(! m_InsideList[k -1].empty())
        {
          nit.GoToBegin();
          oit.GoToBegin();
          currentItem = m_InsideList[k-1].front();
          pixelIndex = currentItem->GetIndex();

          
          offset = pixelIndex - startIndex;
          nit +=  offset;
          oit +=  offset;
          
          if(!IsOnBoundary(pixelIndex))
            for (int i = 0; i < 2 * ImageDimension; i ++)
              {
                int neighbor = center + displacement[i];
                
                if( nit.GetPixel(neighbor) == zero )
                  {
                    idx = GetThreadNum(pixelIndex + idxOffset[i]);
                    newItem = new LevelSetNodeType(pixelIndex + idxOffset[i], zero);
                    
                    m_InsideLists[idx][k].push_back(newItem);
                    m_InsideList[k].push_back(newItem);
                    
                    nit.SetPixel(neighbor, m_InStatus[k]);
                    oit.SetPixel(neighbor, DIFFERENCE_FACTOR*(k+1) );
                  }
              }
          m_InsideList[k -1].pop_front();


        }

      
      while(! m_OutsideList[k -1].empty())
        {
          nit.GoToBegin();
          oit.GoToBegin();
          currentItem = m_OutsideList[k-1].front();
          pixelIndex = currentItem->GetIndex();
          offset = pixelIndex - startIndex;
          nit +=  offset;
          oit +=  offset;


          if(!IsOnBoundary(pixelIndex))
            for (int i = 0; i < 2 * ImageDimension; i ++)
              {
                int neighbor = center + displacement[i];
 
                if( nit.GetPixel(neighbor) == zero )
                  {
                    idx = GetThreadNum(pixelIndex+ idxOffset[i]);
                    newItem = new LevelSetNodeType(pixelIndex + idxOffset[i], zero);

                    
                    m_OutsideLists[idx][k].push_back(newItem);
                    m_OutsideList[k].push_back(newItem);
                    
                    nit.SetPixel(neighbor, m_OutStatus[k]);
                    oit.SetPixel(neighbor, -1.0*DIFFERENCE_FACTOR*(k+1) );
                  }
              }
          m_OutsideList[k -1].pop_front();
         

        }

    }

  ApplyUpdate(0.0);

#ifdef DEBUG
  
  for (int threadId =0; threadId< this->GetNumberOfThreads();threadId++)
    {
      for(int i= 0; i < m_NumberOfLayers; i++)
        {
          cout<<"thread "<<threadId<<" outside "<<i<<": "<<m_OutsideLists[threadId][i].size()<<endl;
          cout<<"thread "<<threadId<<" inside "<<i<<": "<<m_InsideLists[threadId][i].size()<<endl;
        }
      cout<<"thread "<<threadId<<" active list "<<m_ActiveLists[threadId].size()<<endl;
    }
  
#endif



}


template<class TInputImage, class TOutputImage>
void
SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::ApplyUpdate(TimeStepType dt)
{
  // Set up for multithreaded processing.
  SparseFieldLevelSetThreadStruct str;
  str.Filter = this;
  str.TimeStep = dt;
  this->GetMultiThreader()->SetNumberOfThreads(this->GetNumberOfThreads());
  this->GetMultiThreader()->SetSingleMethod(this->ApplyUpdateThreaderCallback,
                                            &str);
  // Multithread the execution
  this->GetMultiThreader()->SingleMethodExecute();
  
}

template<class TInputImage, class TOutputImage>
ITK_THREAD_RETURN_TYPE
SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::ApplyUpdateThreaderCallback( void * arg )
{
  SparseFieldLevelSetThreadStruct * str;
  int total, threadId, threadCount;

  threadId = ((MultiThreader::ThreadInfoStruct *)(arg))->ThreadID;
  threadCount = ((MultiThreader::ThreadInfoStruct *)(arg))->NumberOfThreads;

  str = (SparseFieldLevelSetThreadStruct *)(((MultiThreader::ThreadInfoStruct *)(arg))->UserData);

  // Execute the actual method with appropriate output region
  // first find out how many pieces extent can be split into.
  // Using the SplitRequestedRegion method from itk::ImageSource.
  ThreadRegionType splitRegion;
  total = str->Filter->SplitRequestedRegion(threadId, threadCount,
                                            splitRegion);
  
  if (threadId < total)
    {
    str->Filter->ThreadedApplyUpdate(str->TimeStep, splitRegion, threadId);
    }

  return ITK_THREAD_RETURN_VALUE;
}

template <class TInputImage, class TOutputImage>
typename
SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>::TimeStepType
SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::CalculateChange()
{
  int threadCount;
  TimeStepType dt;
  
  // Set up for multithreaded processing.
  SparseFieldLevelSetThreadStruct str;
  str.Filter = this;
  str.TimeStep = NumericTraits<TimeStepType>::Zero;  // Not used during the
                                                  // calculate change step.
  this->GetMultiThreader()->SetNumberOfThreads(this->GetNumberOfThreads());
  this->GetMultiThreader()->SetSingleMethod(this->CalculateChangeThreaderCallback,
                                            &str);

  // Initialize the list of time step values that will be generated by the
  // various threads.  There is one distinct slot for each possible thread,
  // so this data structure is thread-safe.
  threadCount = this->GetMultiThreader()->GetNumberOfThreads();  
  str.TimeStepList = new TimeStepType[threadCount];                 
  str.ValidTimeStepList = new bool[threadCount];
  for (int i =0; i < threadCount; ++i)
    {      str.ValidTimeStepList[i] = false;    } 

  // Multithread the execution
  this->GetMultiThreader()->SingleMethodExecute();

  // Resolve the single value time step to return
  dt = this->ResolveTimeStep(str.TimeStepList, str.ValidTimeStepList, threadCount);
  delete [] str.TimeStepList;
  delete [] str.ValidTimeStepList;

  return  dt;
}

template <class TInputImage, class TOutputImage>
ITK_THREAD_RETURN_TYPE
SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::CalculateChangeThreaderCallback( void * arg )
{
  SparseFieldLevelSetThreadStruct * str;
  int total, threadId, threadCount;

  threadId = ((MultiThreader::ThreadInfoStruct *)(arg))->ThreadID;
  threadCount = ((MultiThreader::ThreadInfoStruct *)(arg))->NumberOfThreads;

  str = (SparseFieldLevelSetThreadStruct *)(((MultiThreader::ThreadInfoStruct *)(arg))->UserData);

  // Execute the actual method with appropriate output region
  // first find out how many pieces extent can be split into.
  // Using the SplitRequestedRegion method from itk::ImageSource.
  ThreadRegionType splitRegion;

  total = str->Filter->SplitRequestedRegion(threadId, threadCount,
                                            splitRegion);

  if (threadId < total)
    { 
      str->TimeStepList[threadId]
        = str->Filter->ThreadedCalculateChange(splitRegion, threadId);
      str->ValidTimeStepList[threadId] = true;
    }
  
  return ITK_THREAD_RETURN_VALUE;  
}

template <class TInputImage, class TOutputImage>
void
SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::ThreadedApplyUpdate(TimeStepType dt, const ThreadRegionType &regionToProcess,
                           int threadId)
{

  OutputImageType * output = this->GetOutput();
  LevelSetNodeType * currentItem;
  LevelSetNodeType * tempItem;
  IndexType         pixelIndex;
  LevelSetNodeListIteratorType it;
  LevelSetNodeListIteratorType tempIt;
  PixelType new_value, this_value, change;
  float scale, min_scale,diff1, diff2, total_change;



  // Define radius of neighborhood
  typename OutputImageType::SizeType radius;
  for(int i = 0; i < ImageDimension; i++)  radius[i] = 1;

  //define neighborhood iterators
  RandomAccessNeighborhoodIterator<OutputImageType> ait(radius, m_StatusImage, regionToProcess);

  RandomAccessNeighborhoodIterator<OutputImageType> oit(radius, output, regionToProcess);

  //define offset and displacements
  Offset<ImageDimension> offset; 
  IndexType startIndex = ait.GetIndex();
  int center = ait.Size()/2;
  
  int  displacement[2 * ImageDimension];
  Offset<ImageDimension>   idxOffset[2 * ImageDimension];
  //set offset of neighbors to the center pixel

  for(int i = 0; i < 2 * ImageDimension; i++)
    {
      for( int j = 0; j < ImageDimension; j++)
        idxOffset[i][j] = 0;
    }
 
  for ( int i = 0 ; i < 2 * ImageDimension; i+= 2)
    {
      int tmp = ait.GetStride(i/2);
      displacement[i] =  -1 * tmp;
       displacement[i+1] = tmp;
      
      idxOffset[i][i/2] = - 1;
      idxOffset[i+1][i/2] = 1;
 
    }

  //CALCULATE status up & down lists for the active layer

  int flag;

  it = m_ActiveLists[threadId].begin();
  while( it != m_ActiveLists[threadId].end())
    {
      
      flag = 0;

      ait.GoToBegin();
      oit.GoToBegin();
          
      currentItem = *it;
      pixelIndex = currentItem->GetIndex();
      offset = pixelIndex - startIndex;
      ait += offset;
      oit += offset;
      
      change = currentItem->GetValue();
      new_value = oit.GetCenterPixel() + dt * change;

      // oit.SetCenterPixel(new_value);      
      
      if(new_value > CHANGE_FACTOR)
        {
              
          // judge if there is a neighbored active pixel that would move in
          // opposite direction. If yes, then do nothing; otherwise, change the
          // status and value of the neighbored outside pixels  

          for(int i = 0; i < ImageDimension * 2; i++)
            {
              if(  ait.GetPixel(center + displacement[i]) == DOWN_STATUS)
                {
                  flag = 1;
                  break;
                }
            }

          if(!flag)
            {

              oit.SetCenterPixel(new_value);  
              ait.SetCenterPixel(UP_STATUS);

              tempIt = it;
              ++it;
              m_ActiveLists[threadId].erase(tempIt);
              m_StatusUpLists[threadId][0].push_back(currentItem);

            }
          else
            ++it;  // anything wrong here?  how to assgn its value ?
          
        }                  
      else if(new_value <= (CHANGE_FACTOR* -1.0))
        {

          // judge if there is a neighbored active pixel that would move in
          // opposite direction. If yes, then do nothing; otherwise, change the
          // status and value of the neighbored outside pixels  


          for(int i = 0; i < ImageDimension * 2; i++)
            {
              if( ait.GetPixel(center + displacement[i]) == UP_STATUS)
                {
                  flag = 1;
                  break;
                }
            }

          if(!flag)
            {

              oit.SetCenterPixel(new_value);  
              ait.SetCenterPixel(DOWN_STATUS);

              tempIt = it;
              ++it;
              m_ActiveLists[threadId].erase(tempIt);

              m_StatusDownLists[threadId][0].push_back(currentItem);
              
            }
          else
            ++it;
        }
      
      else 
        {
          oit.SetCenterPixel(new_value);  
          ++it;
        }
    }

  // cout<<"active status up"<< m_StatusUpLists[threadId][0].size()<<endl;
  //cout<<"active status down"<< m_StatusDownLists[threadId][0].size()<<endl;

  //Process the status lists for active layer: update the 1st inside layer,
  //create status up list for 1st outside layer and update the value

  while(! m_StatusUpLists[threadId][0].empty())
    {
      currentItem = m_StatusUpLists[threadId][0].front();

      ait.GoToBegin();
      oit.GoToBegin();
      pixelIndex = currentItem->GetIndex();
      offset = pixelIndex - startIndex;
      ait += offset;
      oit += offset;

      m_StatusImage->SetPixel(pixelIndex, m_InStatus[0]);
      m_InsideLists[threadId][0].push_back(currentItem);

      new_value = oit.GetCenterPixel();
      
      for(int i = 0; i < ImageDimension * 2; i++)
        {
          if( ait.GetPixel(center + displacement[i]) ==
              m_OutStatus[0] )
            
            {
              ait.SetPixel(center + displacement[i], CHANGING_STATUS);
              this_value = new_value -1.0;
              oit.SetPixel(center + displacement[i], this_value);
              //append to the status up list of the first outside layer
              tempItem = GetFreeItem(threadId); // to be written;
              tempItem->SetIndex(pixelIndex + idxOffset[i]); 
              m_StatusUpLists[threadId][1].push_back(tempItem);
              
            }
          else if( ait.GetPixel(center + displacement[i]) ==
                   CHANGING_STATUS )
            {
              this_value = new_value -1.0;
              if(this_value > oit.GetPixel(center + displacement[i]) ) 
                oit.SetPixel(center + displacement[i], this_value);
             
            }
          
        }
      
      
      m_StatusUpLists[threadId][0].pop_front();
    }




  //Process the status down list for active layer: update the 1st outside layer,
  //create status up list for 1st inside layer and update the value

  while(! m_StatusDownLists[threadId][0].empty())
    {
      ait.GoToBegin();
      oit.GoToBegin();

      currentItem = m_StatusDownLists[threadId][0].front();
      pixelIndex = currentItem->GetIndex();
      offset = pixelIndex - startIndex;
      ait += offset;
      oit += offset;

      m_StatusImage->SetPixel(pixelIndex, m_OutStatus[0]);
      m_OutsideLists[threadId][0].push_back(currentItem);

      new_value = oit.GetCenterPixel();

      for(int i = 0; i < ImageDimension * 2; i++)
        {
          if( ait.GetPixel(center + displacement[i]) ==
              m_InStatus[0] )
            {
              ait.SetPixel(center + displacement[i], CHANGING_STATUS);
              this_value = new_value + 1.0;
              oit.SetPixel(center + displacement[i],this_value);

              //append to the status down list of the first inside layer
              tempItem = GetFreeItem(threadId); // to be written;
              tempItem->SetIndex(pixelIndex + idxOffset[i]); 
              m_StatusDownLists[threadId][1].push_back(tempItem);
              
            }
          else if ( ait.GetPixel(center + displacement[i]) ==
                    CHANGING_STATUS )
            {
              
              this_value = new_value + 1.0;
              if(this_value < oit.GetPixel(center + displacement[i]) ) 
                oit.SetPixel(center + displacement[i], this_value);
            }
          
        }
      m_StatusDownLists[threadId][0].pop_front();
    }
  
  //create status up lists for outside layers and 
  //update the previous lists;

  int tmp_status, current_status;
  LevelSetNodeListType * status;
  int j;
  int k = 1;
  status = & m_StatusUpLists[threadId][1];
  
  for( j = 0; j < m_NumberOfLayers-1 ; j++)
    {

      //      cout<<"outside "<<j <<" up: "<<status->size()<<endl; 

      //This is to re-use the status list
      if( k == 0) 
        k = 1;
      else
        k = 0;

      while(! status->empty())
        {
          
          
          ait.GoToBegin();
          
          currentItem = status->front();
          pixelIndex = currentItem->GetIndex();
          offset = pixelIndex - startIndex;
          ait += offset;

          //build the status list for next outside layer
          
          if(!IsOnBoundary(pixelIndex))
          for(int i = 0; i < ImageDimension * 2; i++)
            {

              if( ait.GetPixel(center + displacement[i]) ==
                  m_OutStatus[j+1] )
                {
                  ait.SetPixel(center + displacement[i], CHANGING_STATUS);

                  //append to the status up list of the first outside layer
                  tempItem = GetFreeItem(threadId);
                  tempItem->SetIndex(pixelIndex + idxOffset[i]); 
                  m_StatusUpLists[threadId][k].push_back(tempItem);

                }
            }
          
          if( j == 0)
            {
              m_ActiveLists[threadId].push_back(currentItem);
              ait.SetCenterPixel(ACTIVE_STATUS);
            }

          else
            {
              m_OutsideLists[threadId][j-1].push_back(currentItem);
              ait.SetCenterPixel(m_OutStatus[j-1]);
            }

          status->pop_front();
        }
  
      status = & m_StatusUpLists[threadId][k];
    }

  // now j = m_NumberOfLayers-1
 
  while(!status->empty())
    {
      ait.GoToBegin();
      oit.GoToBegin();
      currentItem = status->front();
      pixelIndex = currentItem->GetIndex();
      offset = pixelIndex - startIndex;
      ait += offset;
      oit += offset;

      ait.SetCenterPixel(m_OutStatus[j-1]);
      m_OutsideLists[threadId][j-1].push_back(currentItem);
      
      if(!IsOnBoundary(pixelIndex))
      for(int i = 0; i < ImageDimension * 2; i++)
            {

              if( ait.GetPixel(center + displacement[i]) == 0)
                {
                  ait.SetPixel(center + displacement[i], m_OutStatus[j]);

                  //append to the status up list of the first outside layer
                  tempItem = GetFreeItem(threadId);
                  tempItem->SetIndex(pixelIndex + idxOffset[i]); 
                  m_OutsideLists[threadId][j].push_back(tempItem);

                }
            }

      status->pop_front();
    }

  
    //create status up lists for outside layers and 
  //update the previous lists;

  k = 1;
  status = &m_StatusDownLists[threadId][1];


  for( j = 0; j < m_NumberOfLayers -1; j++)
    {

      // cout<<"inside "<<j <<" down: "<<status->size()<<endl; 
      //This is to re-use the status list
      if( k == 0) 
        k = 1;
      else
        k = 0;
      
      while(! status->empty())
        {

          ait.GoToBegin();
          
          currentItem = status->front();
          pixelIndex = currentItem->GetIndex();
          offset = pixelIndex - startIndex;
          ait += offset;

          //build the status list for next outside layer

          if(!IsOnBoundary(pixelIndex))
          for(int i = 0; i < ImageDimension * 2; i++)
            {

              if( ait.GetPixel(center + displacement[i]) == m_InStatus[j+1])
                {
                  ait.SetPixel(center + displacement[i], CHANGING_STATUS);

                  //append to the status up list of the first outside layer
                  tempItem = GetFreeItem(threadId);
                  tempItem->SetIndex(pixelIndex + idxOffset[i]); 
                  m_StatusDownLists[threadId][k].push_back(tempItem);

                }
            }

          if( j == 0)
            {
              m_ActiveLists[threadId].push_back(currentItem);
              ait.SetCenterPixel(ACTIVE_STATUS);
            }

          else
            {
              m_InsideLists[threadId][j-1].push_back(currentItem);
              ait.SetCenterPixel(m_InStatus[j -1]);
            }

          status->pop_front();
        }
  
      status = & m_StatusDownLists[threadId][k];
    }
  

  while(!status->empty())
    {
      ait.GoToBegin();
      oit.GoToBegin();
      currentItem = status->front();
      pixelIndex = currentItem->GetIndex();
      offset = pixelIndex - startIndex;
      ait += offset;
      oit += offset;

      ait.SetCenterPixel(m_InStatus[j-1]);
      m_InsideLists[threadId][j-1].push_back(currentItem);
      
      if(!IsOnBoundary(pixelIndex))
      for(int i = 0; i < ImageDimension * 2; i++)
            {

              if( ait.GetPixel(center + displacement[i]) == 0 )
                {
                  ait.SetPixel(center + displacement[i], m_InStatus[j]);

                  //append to the status up list of the first outside layer
                  tempItem = GetFreeItem(threadId);
                  tempItem->SetIndex(pixelIndex + idxOffset[i]); 
                  m_InsideLists[threadId][j].push_back(tempItem);

                }
            }

      status->pop_front();
    }


  // Now we need to update value of the outside/inside lists


  int total_active_neighbors;
  PixelType neighbor_value;

  PixelType zero = NumericTraits<PixelType>::Zero;


  //statusUpLists[0] --outsideLists[m_NumberOfLayers-1];
  //   ...
  //statusUpLists[m_NumberOfLayers] --activeList
  //   ...
  //statusUpLists[2 * m_NumberOfLayers]--insideLists[m_NumberOfLayers-1]

  //Deal with outside lists

  // ait = RandomAccessNeighborhoodIterator<OutputImageType>(radius, m_StatusImage, regionToProcess);

  //oit = RandomAccessNeighborhoodIterator<OutputImageType>(radius, output, regionToProcess);


  // update the outside lists and its value;
  for( int j = 0; j < m_NumberOfLayers; j ++)
    {
      it = m_OutsideLists[threadId][j].begin();


      while(it!= m_OutsideLists[threadId][j].end())
        {

          ait.GoToBegin();
          oit.GoToBegin();

          currentItem = *it;
          pixelIndex = currentItem->GetIndex();
          offset = pixelIndex - startIndex;
          ait += offset;
          oit += offset;

          //if a pixel was marked a different status, then we just remove it
          // from the current list


          if(ait.GetCenterPixel() != m_OutStatus[j])
            {
              tempIt = it;
              ++it;
              m_OutsideLists[threadId][j].erase(tempIt);

              m_FreeLists[threadId].push_back(currentItem);
              continue;
            }

         if(IsOnBoundary(pixelIndex))
             {
               ++it;
               continue;
             }

          total_active_neighbors = 0;
          neighbor_value = -NumericTraits<PixelType>::max();
          
          for(int k = 0; k < ImageDimension *2; k ++)
            {

              int temp_status;
              if(j == 0)
                temp_status = ACTIVE_STATUS;
              else
                temp_status = m_OutStatus[j-1];

              if(ait.GetPixel(center + displacement[k]) == temp_status)
                {
                  this_value = oit.GetPixel(center + displacement[k]);
                  if(this_value > neighbor_value)
                    neighbor_value = this_value;
                  total_active_neighbors++;
                }
              
            } 

          if(total_active_neighbors == 0)
            {

              //if we can not find an active neighbor for this pixel, we just
              //demote it to next outward layer 
   
              if(j ==m_NumberOfLayers -1 )
                {
                  ait.SetCenterPixel(0);
                  oit.SetCenterPixel(-OUTSIDE_VALUE);
                  tempIt = it;
                  ++it;
                  m_OutsideLists[threadId][j].erase(tempIt);
                  m_FreeLists[threadId].push_back(currentItem);
                }
              else
                {
                  ait.SetCenterPixel(m_OutStatus[(j+1)]);
                  tempIt = it;
                  ++it;
                  m_OutsideLists[threadId][j].erase(tempIt);
                  m_OutsideLists[threadId][j+1].push_back(currentItem);

                }
            }
          else
            {
              
              oit.SetCenterPixel(neighbor_value - DIFFERENCE_FACTOR);
              ++it;
                           
            }//total_active_neighbors != 0
        } // end of dealing with outside[i]
    
      
  //update the  inside lists and update its value
      it = m_InsideLists[threadId][j].begin();
  
      while(it!= m_InsideLists[threadId][j].end())
        {

          ait.GoToBegin();
          oit.GoToBegin();

          currentItem = *it;
          pixelIndex = currentItem->GetIndex();
          offset = pixelIndex - startIndex;
          ait += offset;
          oit += offset;
          
          if(ait.GetCenterPixel() != m_InStatus[j])
            {
              tempIt = it;
              ++it;
              m_InsideLists[threadId][j].erase(tempIt);
              
              m_FreeLists[threadId].push_back(currentItem);
         
              continue;
            }
          
          if(IsOnBoundary(pixelIndex))
             {
               ++it;
               continue;
             }
          total_active_neighbors = 0;
          neighbor_value = NumericTraits<PixelType>::max();
          
          for(int k = 0; k < ImageDimension *2; k ++)
            {
              int temp_status;
              if(j == 0)
                temp_status = ACTIVE_STATUS;
              else
                temp_status = m_InStatus[j-1];

              if(ait.GetPixel(center + displacement[k]) == temp_status)
                {
                  this_value = oit.GetPixel(center + displacement[k]);
                  if(this_value < neighbor_value)
                    neighbor_value = this_value;
                  total_active_neighbors++;
                }

            } 

          if(total_active_neighbors == 0)
            {

             if(j ==m_NumberOfLayers -1 )
                {
                  ait.SetCenterPixel(zero);
                  oit.SetCenterPixel(OUTSIDE_VALUE);
                  tempIt = it;
                  ++it;
                  m_InsideLists[threadId][j].erase(tempIt);
                  m_FreeLists[threadId].push_back(currentItem);
                }
              else
                {
              
                  ait.SetCenterPixel(m_InStatus[j+1]);
                  tempIt = it;
                  ++it;
                  m_InsideLists[threadId][j].erase(tempIt);
                  m_InsideLists[threadId][j+1].push_back(currentItem);

                }
            }
          else
            {
               oit.SetCenterPixel(neighbor_value + DIFFERENCE_FACTOR);
               ++it;
            }
              
        } // end of dealing with inside[j]
    }

#ifdef DEBUG  
  cout<<"active layer"<<endl;

  it = m_ActiveLists[threadId].begin();
  while(it != m_ActiveLists[threadId].end())
    {
      oit.GoToBegin();
      
      currentItem = *it;
      pixelIndex = currentItem->GetIndex();
      offset = pixelIndex - startIndex;
      
      oit += offset;
      
      cout<<oit.GetCenterPixel()<<" ";
      ++it;

    }
  
  for(int i = 0; i < m_NumberOfLayers; i++)
    {
      cout<<"outside layer "<<i<<endl;
      it = m_OutsideLists[threadId][i].begin();
      while(it != m_OutsideLists[threadId][i].end())
        {
          oit.GoToBegin();
          
          currentItem = *it;
          pixelIndex = currentItem->GetIndex();
          offset = pixelIndex - startIndex;
          
          oit += offset;
           ++it;
          cout<<oit.GetCenterPixel()<<" ";
        }
    }

  for(int i = 0; i < m_NumberOfLayers; i++)
    {
      cout<<"Inside layer "<<i<<endl;
      it = m_InsideLists[threadId][i].begin();
      while(it != m_InsideLists[threadId][i].end())
        {
          oit.GoToBegin();
          
          currentItem = *it;
          pixelIndex = currentItem->GetIndex();
          offset = pixelIndex - startIndex;
          
          oit += offset;
           ++it;
          cout<<oit.GetCenterPixel()<<" ";
        }
    }
  
 for (int threadId =0; threadId< this->GetNumberOfThreads();threadId++)
   {
     for(int i= 0; i < m_NumberOfLayers; i++)
       {
         cout<<"thread "<<threadId<<" outside "<<i<<": "<<m_OutsideLists[threadId][i].size()<<endl;
         cout<<"thread "<<threadId<<" inside "<<i<<": "<<m_InsideLists[threadId][i].size()<<endl;
       }
     cout<<"thread "<<threadId<<" active list "<<m_ActiveLists[threadId].size()<<endl;
   }
  
#endif

}

template <class TInputImage, class TOutputImage>
typename
SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>::TimeStepType
SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::ThreadedCalculateChange(const ThreadRegionType &regionToProcess, int
                          threadId)
{
  typedef typename OutputImageType::RegionType RegionType;
  typedef typename OutputImageType::SizeType   SizeType;
  typedef typename OutputImageType::SizeValueType   SizeValueType;
  typedef typename OutputImageType::IndexType  IndexType;
  typedef typename OutputImageType::IndexValueType  IndexValueType;
  typedef typename FiniteDifferenceFunctionType::NeighborhoodType
    NeighborhoodIteratorType;

  typename OutputImageType::Pointer output = this->GetOutput();
  unsigned int i, j;
  TimeStepType timeStep;
  void *globalData;

  // First we analyze the regionToProcess to determine if any of its faces are
  // along a buffer boundary (we have no data in the buffer for pixels
  // that are outside the boundary and within the neighborhood radius so will
  // have to treat them differently).  We also determine the size of the non-
  // boundary region that will be processed.
  const typename FiniteDifferenceFunctionType::Pointer df
    = this->GetDifferenceFunction();
  const SizeType  radius = df->GetRadius();

  
  /*  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage>::
    FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage> bC;
  faceList = bC(input, regionToProcess, radius);

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage>::
    FaceListType::iterator fit;
  fit = faceList.begin();
  */

  RandomAccessNeighborhoodIterator<OutputImageType> nit(radius, output, regionToProcess);

    
  LevelSetNodeType * currentItem;
  IndexType  pixelIndex;
  IndexType  startIndex;
  Offset<ImageDimension> offset;


  // Here use the smart iterator to avoid the boundary conditions
 
  nit.GoToBegin();
  startIndex = nit.GetIndex();
  
  LevelSetNodeListIteratorType lit;
  
  lit = m_ActiveLists[threadId].begin();
  globalData =  globalData = df->GetGlobalDataPointer();

  PixelType tmpV;
  while( lit != m_ActiveLists[threadId].end()  && !nit.IsAtEnd() )
    {
      nit.GoToBegin();
      currentItem = *lit;
      pixelIndex = currentItem->GetIndex();

      if(!IsOnBoundary(pixelIndex))
        {
          offset = pixelIndex - startIndex;
          nit += offset;
          tmpV = df->ComputeUpdate(nit, globalData);
        }
      else
        tmpV = 0;

      currentItem->SetValue(tmpV);
      ++lit;
    }


  // Ask the finite difference function to compute the time step for
  // this iteration.  We give it the global data pointer to use, then
  // ask it to free the global data memory.
  timeStep = df->ComputeGlobalTimeStep(globalData);

  df->ReleaseGlobalDataPointer(globalData);
  
  return timeStep;
}

template <class TInputImage, class TOutputImage>
void
SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  os << indent << "SparseFieldLevelSetImageFilter";
  Superclass::PrintSelf(os, indent.GetNextIndent());
}



template <class TInputImage, class TOutputImage>
int
SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::IsOnBoundary(IndexType index)
{
  typename TInputImage::Pointer input = this->GetInput();
  typename InputImageType::SizeType  size = (input->GetBufferedRegion()).GetSize();
  
  for( int i = 0; i< ImageDimension; i++)
    {
      if(index[i] == 0 || index[i] == size[i] - 1)
        return 1;
    }
  return 0;

}


template <class TInputImage, class TOutputImage>
SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>::LevelSetNodeType *
SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::GetFreeItem(int threadId)
{
  LevelSetNodeType* tempItem;
  
  if(!m_FreeLists[threadId].empty())
    {
      tempItem = m_FreeLists[threadId].front();
      m_FreeLists[threadId].pop_front();
      return tempItem;
    }
  cout<<"allocating new memory"<<endl;
  for(int j = 0; j < m_MaxPreAllocateNodes; j ++)
    m_FreeLists[threadId].push_back(new LevelSetNodeType());
      
  
  tempItem = m_FreeLists[threadId].front();
  m_FreeLists[threadId].pop_front();
  
  return tempItem;
  
}


}// end namespace itk






#endif
