/*======================================================================
  
Program:   Insight Segmentation & Registration Toolkit
Module:    itkParallelSparseFieldLevelSetImageFilter.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

======================================================================*/
#ifndef __itkParallelSparseFieldLevelSetImageFilter_txx_
#define __itkParallelSparseFieldLevelSetImageFilter_txx_

#include "itkParallelSparseFieldLevelSetImageFilter.h"
#include "itkZeroCrossingImageFilter.h"
#include "itkShiftScaleImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkNumericTraits.h"
#include "itkNeighborhoodAlgorithm.h"
#include <iostream>
#include <fstream>

namespace itk {

template <class TNeighborhoodType>
ParallelSparseFieldCityBlockNeighborList<TNeighborhoodType>
::ParallelSparseFieldCityBlockNeighborList()
{
  typedef typename NeighborhoodType::ImageType ImageType;
  typename ImageType::Pointer dummy_image = ImageType::New();
  
  unsigned int i, nCenter;
  int d;
  OffsetType zero_offset;
  
  for (i = 0; i < Dimension; ++i)
    {
    m_Radius[i] = 1;
    zero_offset[i] = 0;
    }
  NeighborhoodType it(m_Radius, dummy_image, dummy_image->GetRequestedRegion());
  nCenter = it.Size() / 2;
  
  m_Size = 2 * Dimension;
  m_ArrayIndex.reserve(m_Size);
  m_NeighborhoodOffset.reserve(m_Size);
  
  for (i = 0; i < m_Size; ++i)
    {
    m_NeighborhoodOffset.push_back(zero_offset);
    }
  
  for (d = Dimension - 1, i = 0; d >= 0; --d, ++i)
    {
    m_ArrayIndex.push_back( nCenter - it.GetStride(d) );
    m_NeighborhoodOffset[i][d] = -1;
    }
  for (d = 0; d < Dimension; ++d, ++i)
    {
    m_ArrayIndex.push_back( nCenter + it.GetStride(d) );
    m_NeighborhoodOffset[i][d] = 1;
    }
}

template <class TNeighborhoodType>
void
ParallelSparseFieldCityBlockNeighborList<TNeighborhoodType>
::Print(std::ostream &os) const
{
  os << "ParallelSparseFieldCityBlockNeighborList: " << std::endl;
  for (unsigned i = 0; i < this->GetSize(); ++i)
    {
    os << "m_ArrayIndex[" << i << "]: " << m_ArrayIndex[i] << std::endl
       << "m_NeighborhoodOffset[" << i << "]: " << m_NeighborhoodOffset[i] << std::endl;
    }
}

template<class TInputImage, class TOutputImage>
double ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::m_ConstantGradientValue = 1.0;

template<class TInputImage, class TOutputImage>
typename ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>::ValueType
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::m_ValueOne = NumericTraits<ParallelSparseFieldLevelSetImageFilter<TInputImage,
                                      TOutputImage>::ValueType >::One;

template<class TInputImage, class TOutputImage>
typename ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>::ValueType
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::m_ValueZero = NumericTraits<ParallelSparseFieldLevelSetImageFilter<TInputImage,
                                             TOutputImage>::ValueType >::Zero;

template<class TInputImage, class TOutputImage>
typename ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>::StatusType
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::m_StatusNull = NumericTraits<ParallelSparseFieldLevelSetImageFilter<TInputImage,
                                        TOutputImage>::StatusType >::NonpositiveMin();

template<class TInputImage, class TOutputImage>
typename ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>::StatusType
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::m_StatusChanging = -1;

template<class TInputImage, class TOutputImage>
typename ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>::StatusType
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::m_StatusActiveChangingUp = -2;

template<class TInputImage, class TOutputImage>
typename ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>::StatusType
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::m_StatusActiveChangingDown = -3;

template<class TInputImage, class TOutputImage>
typename ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>::StatusType
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::m_StatusBoundaryPixel = -4;

template<class TInputImage, class TOutputImage>
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::ParallelSparseFieldLevelSetImageFilter()
{
  m_IsoSurfaceValue = m_ValueZero;
  m_NumberOfLayers = 1;
  m_RMSChange = m_ValueOne;
  m_BoundsCheckingActive = false;
}

template<class TInputImage, class TOutputImage>
void 
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  // Allocate the output image
  m_OutputImage= this->GetOutput();
  m_OutputImage->SetBufferedRegion(m_OutputImage->GetRequestedRegion());
  m_OutputImage->Allocate();
  
  // Copy the input image to the output image.  Algorithms will operate
  // directly on the output image
  this->CopyInputToOutput();
  
  // Perform any other necessary pre-iteration initialization. 
  this->Initialize();
  
  // Evolve the surface
  this->Iterate();

  // Clean up
  this->DeallocateData();
}

template<class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::CopyInputToOutput()
{
  // This method is the first step in initializing the level-set image, which
  // is also the output of the filter.  The input is passed through a
  // zero crossing filter, which produces zero's at pixels closest to the zero
  // level set and one's elsewhere.  The actual zero level set values will be
  // adjusted in the Initialize() step to more accurately represent the
  // position of the zero level set.
  
  // First need to subtract the iso-surface value from the input image.
  typedef ShiftScaleImageFilter<InputImageType, OutputImageType> ShiftScaleFilterType;
  typename ShiftScaleFilterType::Pointer shiftScaleFilter = ShiftScaleFilterType::New();
  shiftScaleFilter->SetInput( this->GetInput()  );
  shiftScaleFilter->SetShift( - m_IsoSurfaceValue );
  // keep a handle to the shifted output
  m_ShiftedImage = shiftScaleFilter->GetOutput();
  
  typename ZeroCrossingImageFilter<OutputImageType, OutputImageType>::Pointer
    zeroCrossingFilter = ZeroCrossingImageFilter<OutputImageType,OutputImageType>::New();
  zeroCrossingFilter->SetInput(m_ShiftedImage);
  zeroCrossingFilter->GraftOutput(m_OutputImage);
  zeroCrossingFilter->SetBackgroundValue(m_ValueOne);
  zeroCrossingFilter->SetForegroundValue(m_ValueZero);
  zeroCrossingFilter->SetNumberOfThreads(1);
  zeroCrossingFilter->Update();
  
  // Here the output is the result of zerocrossings
  this->GraftOutput(zeroCrossingFilter->GetOutput());
}

template<class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::Initialize()
{
  int i;

  // A node pool used during initialization of the level set.
  m_LayerNodeStore = LayerNodeStorageType::New();
  m_LayerNodeStore->SetGrowthStrategyToExponential();
  
  // Allocate the status image.
  m_StatusImage = StatusImageType::New();
  m_StatusImage->SetRegions(m_OutputImage->GetRequestedRegion());
  m_StatusImage->Allocate();
  
  // Initialize the status image to contain all m_StatusNull values.
  ImageRegionIterator<StatusImageType> statusIt(m_StatusImage,
                                 m_StatusImage->GetRequestedRegion());
  for (statusIt = statusIt.Begin(); ! statusIt.IsAtEnd(); ++statusIt)
    {
    statusIt.Set( m_StatusNull );
    }
  
  // Initialize the boundary pixels in the status images to
  // m_StatusBoundaryPixel values.  Uses the face calculator to find all of the
  // region faces.
  typedef NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<StatusImageType>
    BFCType;
  
  BFCType faceCalculator;
  typename BFCType::FaceListType faceList;
  typename BFCType::SizeType sz;
  typename BFCType::FaceListType::iterator fit;
  
  sz.Fill(1);
  faceList = faceCalculator(m_StatusImage, m_StatusImage->GetRequestedRegion(),
                            sz);
  fit = faceList.begin();
  
  for (++fit; fit != faceList.end(); ++fit) // skip the first (nonboundary) region
    {
    statusIt = ImageRegionIterator<StatusImageType>(m_StatusImage, *fit);
    for (statusIt.GoToBegin(); ! statusIt.IsAtEnd(); ++statusIt)
      {
      statusIt.Set( m_StatusBoundaryPixel );
      }
    }
  
  // Allocate the layers of the sparse field.
  m_Layers.reserve(2 * m_NumberOfLayers + 1);
  for (i = 0; i < m_Layers.capacity(); ++i)
    {
    m_Layers.push_back( LayerType::New() );
    }
  
  m_SplitAxis  = m_OutputImage->GetImageDimension() - 1; // always the "Z" dimension
  
  typename OutputImageType::SizeType requestedRegionSize
    = m_OutputImage->GetRequestedRegion().GetSize();
  m_ZSize = requestedRegionSize[m_SplitAxis];

  // Histogram of number of pixels in each Z plane for the entire 3D volume
  m_GlobalZHistogram = new int[m_ZSize];
  for (i = 0; i < m_ZSize; i++)
    {
    m_GlobalZHistogram[i] = 0;
    }
  
  // Construct the active layer and initialize the first layers inside and
  // outside of the active layer
  this->ConstructActiveLayer();
  
  // Construct the rest of the non active set layers using the first two
  // layers. Inside layers are odd numbers, outside layers are even numbers.
  for (i = 1; i < m_Layers.size() - 2; ++i)
    {
    this->ConstructLayer(i, i+2);
    }
  
  // Set the values in the output image for the active layer.
  this->InitializeActiveLayerValues();
  
  // Initialize layer values using the active layer as seeds.
  this->PropagateAllLayerValues();
  
  // Initialize pixels inside and outside the sparse field layers to positive
  // and negative values, respectively.  This is not necessary for the
  // calculations, but is useful for presenting a more intuitive output to the
  // filter.  See PostProcessOutput method for more information.
  this->InitializeBackgroundPixels();
  
  m_NumOfThreads = this->GetNumberOfThreads();

  if (m_SplitAxis < 0)
    {
      // cannot split
      itkDebugMacro ("Unable to choose an axis for workload distribution among threads");
      return;
    }
  
  // Cumulative frequency of number of pixels in each Z plane for the entire 3D
  // volume 
  m_ZCumulativeFrequency = new int[m_ZSize];
  for (i = 0; i < m_ZSize; i++)
    {
    m_ZCumulativeFrequency[i] = 0;
    }

  // The mapping from a z-value to the thread in whose region the z-value lies 
  m_MapZToThreadNumber = new int[m_ZSize];
  for (i = 0; i < m_ZSize; i++)
    {
    m_MapZToThreadNumber[i] = 0;
    }
  
  // The boundaries defining thread regions
  m_Boundary = new int[m_NumOfThreads];
  for (i = 0; i < m_NumOfThreads; i++)
    {
    m_Boundary[i] = 0;
    }

  // A boolean variable stating if the boundaries had been changed during
  // CheckLoadBalance()
  m_BoundaryChanged = false;
  
  // A global barrier for all threads.
  m_Barrier = Barrier::New();
  m_Barrier->Initialize(m_NumOfThreads);
  
  // Allocate data for each thread.
  m_Data = new ThreadData[m_NumOfThreads];
}

template <class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::ConstructActiveLayer()
{
  // We find the active layer by searching for 0's in the zero crossing image
  // (output image).  The first inside and outside layers are also constructed
  // by searching the neighbors of the active layer in the (shifted) input image.
  // Negative neighbors not in the active set are assigned to the inside,
  // positive neighbors are assigned to the outside.
  
  NeighborhoodIterator<OutputImageType> shiftedIt(m_NeighborList.GetRadius(),
                          m_ShiftedImage, m_OutputImage->GetRequestedRegion());
  NeighborhoodIterator<OutputImageType> outputIt (m_NeighborList.GetRadius(),
                          m_OutputImage, m_OutputImage->GetRequestedRegion());
  NeighborhoodIterator<StatusImageType> statusIt (m_NeighborList.GetRadius(),
                          m_StatusImage, m_OutputImage->GetRequestedRegion());
  
  IndexType center_index, offset_index;
  LayerNodeType *node = 0;
  bool bounds_status = true;
  ValueType value;
  StatusType layer_number;
  
  typename OutputImageType::SizeType regionSize
    = m_OutputImage->GetRequestedRegion().GetSize();
  typename OutputImageType::IndexType startIndex
    = m_OutputImage->GetRequestedRegion().GetIndex();;
  
  for (outputIt.GoToBegin(); !outputIt.IsAtEnd(); ++outputIt)
    {
    bounds_status = true;
    if ( outputIt.GetCenterPixel() == m_ValueZero )
      {
      // Grab the neighborhood in the status image.
      center_index = outputIt.GetIndex();
      statusIt.SetLocation( center_index );
      
      for(int j = 0; j < ImageDimension; j++)
        {
        if(center_index[j] <= startIndex[j] || center_index[j]
           >= startIndex[j]+ regionSize[j]-1)
          {
          bounds_status = false;
          break;
          }
        }
      if(bounds_status == true)
        {
        // Here record the hisgram information
        m_GlobalZHistogram[ center_index[m_SplitAxis] ]++;
        
        // Borrow a node from the store and set its value.
        node = m_LayerNodeStore->Borrow();
        node->m_Index = center_index;
        
        // Add the node to the active list and set the status in the status
        // image.
        m_Layers[0]->PushFront( node );
        statusIt.SetCenterPixel( 0 );
        
        // Grab the neighborhood in the image of shifted input values.
        shiftedIt.SetLocation( center_index );
        
        // Search the neighborhood pixels for first inside & outside layer
        // members.  Construct these lists and set status list values. 
        for (int i = 0; i < m_NeighborList.GetSize(); ++i)
          {
          offset_index = center_index + m_NeighborList.GetNeighborhoodOffset(i);
          
          if ( outputIt.GetPixel(m_NeighborList.GetArrayIndex(i)) != m_ValueZero &&
               statusIt.GetPixel(m_NeighborList.GetArrayIndex(i)) == m_StatusNull)
            {
            value = shiftedIt.GetPixel(m_NeighborList.GetArrayIndex(i));
            
            if ( value < m_ValueZero ) // Assign to first outside layer.
              {
              layer_number = 1;
              }
            else // Assign to first inside layer
              {
              layer_number = 2;
              }
            
            statusIt.SetPixel( m_NeighborList.GetArrayIndex(i), layer_number, bounds_status );
            if ( bounds_status == true ) // In bounds
              {
              node = m_LayerNodeStore->Borrow();
              node->m_Index = offset_index;
              m_Layers[layer_number]->PushFront( node );
              } // else do nothing.
            }
          }
        }
      }
    }
}

template<class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::ConstructLayer(StatusType from, StatusType to)
{
  LayerNodeType *node= 0;
  bool boundary_status;
  typename LayerType::ConstIterator fromIt;
  NeighborhoodIterator<StatusImageType> statusIt(m_NeighborList.GetRadius(), m_StatusImage,
                                                 m_OutputImage->GetRequestedRegion() );
  
  // For all indicies in the "from" layer...
  for (fromIt = m_Layers[from]->Begin(); fromIt != m_Layers[from]->End(); ++fromIt)
    {
    // Search the neighborhood of this index in the status image for
    // unassigned indicies. Push those indicies onto the "to" layer and
    // assign them values in the status image.  Status pixels outside the
    // boundary will be ignored.
    statusIt.SetLocation( fromIt->m_Index );
    
    for (int i = 0; i < m_NeighborList.GetSize(); ++i)
      {
      if ( statusIt.GetPixel( m_NeighborList.GetArrayIndex(i) ) == m_StatusNull )
        {
        statusIt.SetPixel(m_NeighborList.GetArrayIndex(i), to, boundary_status);
        
        if (boundary_status == true) // in bounds
          {
          node = m_LayerNodeStore->Borrow();
          node->m_Index = statusIt.GetIndex() + m_NeighborList.GetNeighborhoodOffset(i);
          m_Layers[to]->PushFront( node );
          }
        }
      }
    }
}

template <class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::InitializeActiveLayerValues()
{
  const ValueType CHANGE_FACTOR = m_ConstantGradientValue / 2.0;
  const ValueType MIN_NORM      = 1.0e-6;
  
  typename LayerType::ConstIterator activeIt;
  ConstNeighborhoodIterator<OutputImageType>shiftedIt (m_NeighborList.GetRadius(), m_ShiftedImage,
                                                       m_OutputImage->GetRequestedRegion());
  
  unsigned int center = shiftedIt.Size() /2;
  unsigned int stride;
  
  ValueType dx_forward, dx_backward, length, distance;
  
  // For all indicies in the active layer...
  for (activeIt = m_Layers[0]->Begin(); activeIt != m_Layers[0]->End(); ++activeIt)
    {
    // Interpolate on the (shifted) input image values at this index to
    // assign an active layer value in the output image.
    shiftedIt.SetLocation( activeIt->m_Index );
    
    length = m_ValueZero;
    for (int i = 0; i < ImageDimension; ++i)
      {
      stride = shiftedIt.GetStride(i);
      
      dx_forward  = shiftedIt.GetPixel(center + stride) - shiftedIt.GetCenterPixel();
      dx_backward = shiftedIt.GetCenterPixel()          - shiftedIt.GetPixel(center - stride);
      
      if ( vnl_math_abs(dx_forward) > vnl_math_abs(dx_backward) )
        {
        length += dx_forward  * dx_forward;
        }
      else
        {
        length += dx_backward * dx_backward;
        }
      }
    length = vcl_sqrt(length) + MIN_NORM;
    distance = shiftedIt.GetCenterPixel() / length;
    
    m_OutputImage->SetPixel( activeIt->m_Index ,
        vnl_math_min(vnl_math_max(-CHANGE_FACTOR, distance), CHANGE_FACTOR));
    }
}

template <class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::PropagateAllLayerValues()
{
  // Update values in the first inside and first outside layers using the
  // active layer as a seed. Inside layers are odd numbers, outside layers are
  // even numbers. 
  this->PropagateLayerValues (0, 1, 3, 1); // first inside
  this->PropagateLayerValues (0, 2, 4, 0); // first outside
  
  // Update the rest of the layers.
  for (unsigned int i = 1; i < m_Layers.size() - 2; ++i)
    {
    this->PropagateLayerValues (i, i+2, i+4, (i+2)%2);
    }
}

template <class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::PropagateLayerValues(StatusType from, StatusType to, StatusType promote, int InOrOut)
{
  int i;
  ValueType value, value_temp, delta;
  bool found_neighbor_flag;
  LayerNodeType* node= 0;
  StatusType past_end = static_cast<StatusType>( m_Layers.size() ) - 1;
  
  // Are we propagating values inward (more negative) or outward (more positive)?
  if (InOrOut == 1)
    {
    delta = - m_ConstantGradientValue; // inward
    }
  else
    {
    delta =   m_ConstantGradientValue;
    }
  
  NeighborhoodIterator<OutputImageType> outputIt (m_NeighborList.GetRadius(), m_OutputImage,
                                                  m_OutputImage->GetRequestedRegion());
  NeighborhoodIterator<StatusImageType> statusIt (m_NeighborList.GetRadius(), m_StatusImage,
                                                  m_OutputImage->GetRequestedRegion());
  
  typename LayerType::Iterator toIt = m_Layers[to]->Begin();
  while ( toIt != m_Layers[to]->End() )
    {
    statusIt.SetLocation( toIt->m_Index );
    // Is this index marked for deletion? If the status image has
    // been marked with another layer's value, we need to delete this node
    // from the current list then skip to the next iteration.
    if (statusIt.GetCenterPixel() != to)
      {
      node = toIt.GetPointer();
      ++toIt;
      m_Layers[to]->Unlink( node );
      m_LayerNodeStore->Return( node );
      continue;
      }
    
    outputIt.SetLocation( toIt->m_Index );
    
    value = m_ValueZero;
    found_neighbor_flag = false;
    for (i = 0; i < m_NeighborList.GetSize(); ++i)
      {
      // If this neighbor is in the "from" list, compare its absolute value
      // to any previous values found in the "from" list.  Keep the value
      // that will cause the next layer to be closest to the zero level set.
      if ( statusIt.GetPixel( m_NeighborList.GetArrayIndex(i) ) == from )
        {
        value_temp = outputIt.GetPixel( m_NeighborList.GetArrayIndex(i) );
        
        if (found_neighbor_flag == false)
          {
          value = value_temp;
          }
        else
          {
          if (InOrOut == 1)
            {
            // Find the largest (least negative) neighbor
            if ( value_temp > value )
              { value = value_temp; }
            }
          else
            {
            // Find the smallest (least positive) neighbor
            if (value_temp < value)
              { value = value_temp; }
            }
          }
        found_neighbor_flag = true;
        }
      }
    if (found_neighbor_flag == true)
      {
      // Set the new value using the largest magnitude
      // found in our "from" neighbors.
      outputIt.SetCenterPixel( value + delta );
      ++toIt;
      }
    else
      {
      // Did not find any neighbors on the "from" list, then promote this
      // node.  A "promote" value past the end of my sparse field size
      // means delete the node instead.  Change the status value in the
      // status image accordingly.
      node  = toIt.GetPointer();
      ++toIt;
      m_Layers[to]->Unlink( node );
      if ( promote > past_end )
        {
        m_LayerNodeStore->Return( node );
        statusIt.SetCenterPixel(m_StatusNull);
        }
      else
        {
        m_Layers[promote]->PushFront( node );
        statusIt.SetCenterPixel(promote);
        }
      }
    }
}

template <class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::InitializeBackgroundPixels()
{
  // Assign background pixels INSIDE the sparse field layers to a new level set
  // with value greater than the innermost layer.  Assign background pixels
  // OUTSIDE the sparse field layers to a new level set with value less than
  // the outermost layer.
  const ValueType max_layer = static_cast<ValueType>(m_NumberOfLayers);
  
  const ValueType inside_value  =   max_layer + m_ConstantGradientValue;
  const ValueType outside_value = -(max_layer + m_ConstantGradientValue);
  
  ImageRegionConstIterator<StatusImageType>
    statusIt(m_StatusImage,  m_OutputImage->GetRequestedRegion());
  ImageRegionIterator<OutputImageType>
    outputIt(m_OutputImage,  m_OutputImage->GetRequestedRegion());
  ImageRegionConstIterator<OutputImageType>
    shiftedIt(m_ShiftedImage, m_OutputImage->GetRequestedRegion());
  
  for (outputIt = outputIt.Begin(), statusIt = statusIt.Begin(),
         shiftedIt = shiftedIt.Begin(); ! outputIt.IsAtEnd(); ++outputIt,
         ++statusIt, ++shiftedIt)
    {
    if (statusIt.Get() == m_StatusNull)
      {
      if (shiftedIt.Get() > m_ValueZero)
        {
        outputIt.Set(inside_value);
        }
      else
        {
        outputIt.Set(outside_value);
        }
      }
    }
  
  // deallocate the shifted-image
  m_ShiftedImage = 0;
}

template<class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::ComputeInitialThreadBoundaries()
{
  // NOTE: Properties of the boundary computation algorithm
  //       1. Thread-0 always has something to work on.
  //       2. If a particular thread numbered i has the m_Boundary = (mZSize -
  //          1) then ALL threads numbered > i do NOT have anything to work on.
  
  // Compute the cumulative frequency distribution using the global histogram.
  int i, j;
  m_ZCumulativeFrequency[0] = m_GlobalZHistogram[0];
  for (i= 1; i < m_ZSize; i++)
    {
    m_ZCumulativeFrequency[i] = m_ZCumulativeFrequency[i-1] + m_GlobalZHistogram[i];
    }
  
  // Now define the regions that each thread will process and the corresponding
  // boundaries. 
  m_Boundary[m_NumOfThreads - 1] = m_ZSize - 1; // special case: the upper
                                                // bound for the last thread
  for (i= 0; i < m_NumOfThreads - 1; i++)
    {
    // compute m_Boundary[i]
    
    float cutOff = 1.0 * (i+1) * m_ZCumulativeFrequency[m_ZSize-1] / m_NumOfThreads;
    
    // find the position in the cumulative freq dist where this cutoff is met
    for (j = (i == 0 ? 0 : m_Boundary[i-1]); j < m_ZSize; j++)
      {
      if (cutOff > m_ZCumulativeFrequency[j])
        {
        continue;
        }
      else
        {
        // Optimize a little.
        // Go further FORWARD and find the first index (k) in the cumulative
        // freq distribution s.t. m_ZCumulativeFrequency[k] !=
        // m_ZCumulativeFrequency[j] This is to be done because if we have a
        // flat patch in the cumulative freq. dist. then we can choose
        // a bound midway in that flat patch .
        int k;
        for (k= 1; j+k < m_ZSize; k++)
          {
          if (m_ZCumulativeFrequency[j+k] != m_ZCumulativeFrequency[j])
            {
            break;
            }
          }
        
        // 
        m_Boundary[i]= static_cast<int>( (j + k / 2) );
        break;
        }
      }
    }
  
  // Initialize the local histograms using the global one and the boundaries
  // Also initialize the mapping from the Z value --> the thread number
  // i.e. m_MapZToThreadNumber[] 
  // Also divide the lists up according to the boundaries
  for (i = 0; i <= m_Boundary[0]; i++)
    {
    // this Z belongs to the region associated with thread-0
    m_MapZToThreadNumber[i]= 0;
    }
  
  for (int t= 1; t < m_NumOfThreads; t++)
    {
    for (i = m_Boundary[t-1]+1; i <= m_Boundary[t]; i++)
      {
      // this Z belongs to the region associated with thread-0
      m_MapZToThreadNumber[i]= t;
      }
    }
}

template<class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::ThreadedAllocateData (int ThreadId)
{
  static const float SAFETY_FACTOR = 4.0;
  int i, j;
  // create semaphores
  m_Data[ThreadId].m_Semaphore[0] = Semaphore::New ();
  m_Data[ThreadId].m_Semaphore[1] = Semaphore::New ();
  m_Data[ThreadId].m_Semaphore[0]->Initialize(0);
  m_Data[ThreadId].m_Semaphore[1]->Initialize(0);
  
  // Allocate the layers for the sparse field.
  m_Data[ThreadId].m_Layers.reserve(2 * m_NumberOfLayers + 1);
  for (i = 0; i< m_Data[ThreadId].m_Layers.capacity(); ++i)
    {
      m_Data[ThreadId].m_Layers.push_back( LayerType::New() );
    }
  // Throw an exception if we don't have enough layers.
  if (m_Data[ThreadId].m_Layers.size() < 3)
    {
    itkExceptionMacro( << "Not enough layers have been allocated for the sparse" 
                       << "field. Requires at least one layer." ); 
    }
  
  // Layers used as buffers for transfering pixels during load balancing
  m_Data[ThreadId].m_LoadTransferBufferLayers
    = new LayerListType[2*m_NumberOfLayers+1];
  for (i = 0; i < 2 * m_NumberOfLayers + 1; i++)
    {
    m_Data[ThreadId].m_LoadTransferBufferLayers[i].reserve( m_NumOfThreads );
    
    for (j = 0; j < m_Data[ThreadId].m_LoadTransferBufferLayers[i].capacity();
         j++)
      {
      m_Data[ThreadId].m_LoadTransferBufferLayers[i].push_back(LayerType::New());
      }
    }
  
  // Every thread allocates a local node pool (improving memory locality)
  m_Data[ThreadId].m_LayerNodeStore = LayerNodeStorageType::New();
  m_Data[ThreadId].m_LayerNodeStore->SetGrowthStrategyToExponential();

  // The SAFETY_FACTOR simple ensures that the number of nodes created
  // is larger than those required to start with for each thread.
  int nodeNum= static_cast<int>(SAFETY_FACTOR * m_Layers[0]->Size()
                                * (2*m_NumberOfLayers+1) / m_NumOfThreads);

  m_Data[ThreadId].m_LayerNodeStore->Reserve(nodeNum);
  m_Data[ThreadId].m_RMSChange = m_ValueZero;
  
  // UpLists and Downlists
  for (i = 0; i < 2; ++i)
    {
      m_Data[ThreadId].UpList[i] = LayerType::New();
      m_Data[ThreadId].DownList[i] = LayerType::New();
    }
  
  // Used during the time when status lists are being processed (in ThreadedApplyUpdate() )
  // for the Uplists
  m_Data[ThreadId].m_InterNeighborNodeTransferBufferLayers[0]
    = new LayerPointerType * [m_NumberOfLayers + 1];

  // for the Downlists
  m_Data[ThreadId].m_InterNeighborNodeTransferBufferLayers[1]
    = new LayerPointerType * [m_NumberOfLayers + 1];
  
  for (i= 0; i < m_NumberOfLayers + 1; i++)
    {
    m_Data[ThreadId].m_InterNeighborNodeTransferBufferLayers[0][i] =
      new LayerPointerType[m_NumOfThreads];
    m_Data[ThreadId].m_InterNeighborNodeTransferBufferLayers[1][i] =
      new LayerPointerType[m_NumOfThreads];
    }
  
  for (i= 0; i < m_NumberOfLayers + 1; i++)
    {
    for (j= 0; j < m_NumOfThreads; j++)
      {
      m_Data[ThreadId].m_InterNeighborNodeTransferBufferLayers[0][i][j]
        = LayerType::New();
      m_Data[ThreadId].m_InterNeighborNodeTransferBufferLayers[1][i][j]
        = LayerType::New();
      }
    }
  
  // Local histogram for every thread (used during Iterate() )
  m_Data[ThreadId].m_ZHistogram = new int[m_ZSize];
  for (i = 0; i < m_ZSize; i++)
    {
    m_Data[ThreadId].m_ZHistogram[i] = 0;
    }
  
  // Every thread must have its own copy of the the GlobalData struct.
  m_Data[ThreadId].globalData
    = this->GetDifferenceFunction()->GetGlobalDataPointer();
  
  //
  m_Data[ThreadId].m_SemaphoreArrayNumber = 0;
}

template<class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::ThreadedInitializeData(int ThreadId, const ThreadRegionType & ThreadRegion)
{
  // divide the lists based on the boundaries
  
  LayerNodeType * nodePtr= 0, * nodeTempPtr= 0;
  
  for (int i = 0; i < 2 * m_NumberOfLayers + 1; i++)
    {
    typename LayerType::Iterator layerIt = m_Layers[i]->Begin();
    typename LayerType::Iterator layerEnd= m_Layers[i]->End();
    
    while (layerIt != layerEnd)
      {
      nodePtr = layerIt.GetPointer();
      ++layerIt;
      
      int k = this->GetThreadNumber(nodePtr->m_Index[m_SplitAxis]);
      if (k != ThreadId)
        {
        continue; // some other thread's node => ignore
        }
      
      // Borrow a node from the specific thread's layer so that MEMORY LOCALITY
      // is maintained.
      // NOTE : We already pre-allocated more than enough
      // nodes for each thread implying no new nodes are created here.
      nodeTempPtr= m_Data[ThreadId].m_LayerNodeStore->Borrow ();
      nodeTempPtr->m_Index= nodePtr->m_Index;
      // push the node on the approproate layer
      m_Data[ThreadId].m_Layers[i]->PushFront(nodeTempPtr);
      
      // for the active layer (layer-0) build the histogram for each thread
      if (i == 0)
        {
        // this Z histogram value should be given to thread-0
        m_Data[ThreadId].m_ZHistogram[ (nodePtr->m_Index)[m_SplitAxis] ]
          = m_Data[ThreadId].m_ZHistogram[ (nodePtr->m_Index)[m_SplitAxis] ] + 1;
        }
      }
    }
  
  // Make use of the SGI default "first-touch" memory placement policy
  // Copy from the current status/output images to the new ones and let each
  // thread do the copy of its own region.
  // This will make each thread be the FIRST to write to "it's" data in the new
  // images and hence the memory will get allocated 
  // in the corresponding thread's memory-node.
  ImageRegionConstIterator<StatusImageType> statusIt(m_StatusImage, ThreadRegion);
  ImageRegionIterator<StatusImageType> statusItNew (m_StatusImageTemp, ThreadRegion);
  ImageRegionConstIterator<OutputImageType> outputIt(m_OutputImage, ThreadRegion);
  ImageRegionIterator<OutputImageType> outputItNew(m_OutputImageTemp, ThreadRegion);
  
  for (outputIt = outputIt.Begin(), statusIt = statusIt.Begin(),
         outputItNew = outputItNew.Begin(), statusItNew = statusItNew.Begin();
       ! outputIt.IsAtEnd();  ++outputIt, ++statusIt, ++outputItNew, ++statusItNew)
    {
    statusItNew.Set (statusIt.Get());
    outputItNew.Set (outputIt.Get());
    }
}

template <class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::DeallocateData()
{
  int i, j;
  // Delete data structures used for load distribution and balancing.
  delete [] m_GlobalZHistogram;
  delete [] m_ZCumulativeFrequency;
  delete [] m_MapZToThreadNumber;
  delete [] m_Boundary;
  
  // Deallocate the status image.
  m_StatusImage= 0;
  
  // Remove the barrier from the system.
  //  m_Barrier->Remove ();
  
  // Delete initial nodes, the node pool, the layers.
  for (i = 0; i < 2*m_NumberOfLayers+1; i++)
    {
    // return all the nodes in layer i to the main node pool
    LayerNodeType * nodePtr= 0;
    LayerPointerType layerPtr= m_Layers[i];
    while (! layerPtr->Empty())
      {
      nodePtr= layerPtr->Front();
      layerPtr->PopFront();
      m_LayerNodeStore->Return (nodePtr);
      }
    }
  m_LayerNodeStore->Clear();
  m_Layers.clear();
  
  // Deallocate the thread local data structures.
  for (int ThreadId= 0; ThreadId < m_NumOfThreads; ThreadId++)
    {
    // Remove semaphores from the system.
    m_Data[ThreadId].m_Semaphore[0]->Remove();
    m_Data[ThreadId].m_Semaphore[1]->Remove();
    
    delete [] m_Data[ThreadId].m_ZHistogram;
    
    // 1. delete nodes on the thread layers
    for (i = 0; i < 2*m_NumberOfLayers+1; i++)
      {
      // return all the nodes in layer i to thread-i's node pool
      LayerNodeType * nodePtr= 0;
      LayerPointerType layerPtr= m_Data[ThreadId].m_Layers[i];
      while (! layerPtr->Empty())
        {
        nodePtr= layerPtr->Front();
        layerPtr->PopFront();
        m_Data[ThreadId].m_LayerNodeStore->Return(nodePtr);
        }
      }
    m_Data[ThreadId].m_Layers.clear();
    
    // 2. cleanup the LoadTransferBufferLayers: empty all and return the nodes
    // to the pool 
    for (i = 0; i < 2 * m_NumberOfLayers + 1; i++)
      {
      for (j= 0; j < m_NumOfThreads; j++)
        {
        if (j == ThreadId)
          {
          // a thread does NOT pass nodes to istelf
          continue;
          }
        
        LayerNodeType * nodePtr= 0;
        LayerPointerType layerPtr= m_Data[ThreadId].m_LoadTransferBufferLayers[i][j];
        
        while (! layerPtr->Empty())
          {
          nodePtr= layerPtr->Front();
          layerPtr->PopFront();
          m_Data[ThreadId].m_LayerNodeStore->Return (nodePtr);
          }
        }
      m_Data[ThreadId].m_LoadTransferBufferLayers[i].clear();
      }
    delete [] m_Data[ThreadId].m_LoadTransferBufferLayers;
    
    // 3. clear up the nodes in the last layer of m_InterNeighborNodeTransferBufferLayers (if any)
    for (i= 0; i < m_NumOfThreads; i++)
      {
      LayerNodeType* nodePtr= 0;
      for (int InOrOut= 0; InOrOut < 2; InOrOut++)
        {
        LayerPointerType layerPtr
          = m_Data[ThreadId].m_InterNeighborNodeTransferBufferLayers[InOrOut][m_NumberOfLayers][i];

        while (! layerPtr->Empty())
          {
          nodePtr= layerPtr->Front();
          layerPtr->PopFront();
          m_Data[ThreadId].m_LayerNodeStore->Return(nodePtr);
          }
        }
      }
    
    // check if all last layers are empty and then delete them
    for (i = 0; i < m_NumberOfLayers + 1; i++)
      {
      delete [] m_Data[ThreadId].m_InterNeighborNodeTransferBufferLayers[0][i];
      delete [] m_Data[ThreadId].m_InterNeighborNodeTransferBufferLayers[1][i];
      }
    delete [] m_Data[ThreadId].m_InterNeighborNodeTransferBufferLayers[0];
    delete [] m_Data[ThreadId].m_InterNeighborNodeTransferBufferLayers[1];
    
    // 4. check if all the uplists and downlists are empty
    
    // 5. delete all nodes in the node pool
    m_Data[ThreadId].m_LayerNodeStore->Clear();
    }
  
  delete [] m_Data;
}

template<class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::Iterate()
{
  // Set up for multithreaded processing
  ParallelSparseFieldLevelSetThreadStruct str;
  str.Filter = this;
  str.TimeStep = NumericTraits<TimeStepType>::Zero;
  
  this->GetMultiThreader()->SetNumberOfThreads (m_NumOfThreads);
  
  // Initialize the list of time step values that will be generated by the
  // various threads.  There is one distinct slot for each possible thread,
  // so this data structure is thread-safe.
  str.TimeStepList      = new TimeStepType[m_NumOfThreads];
  str.ValidTimeStepList = new bool        [m_NumOfThreads];
  
  for (int i =0; i < m_NumOfThreads; ++i)
    {
    str.ValidTimeStepList[i] = true;
    }
  
  // Multithread the execution
  this->GetMultiThreader()->SetSingleMethod(this->IterateThreaderCallback, &str);
  // It is this method that will results in the creation of the threads
  this->GetMultiThreader()->SingleMethodExecute ();
  
  delete [] str.TimeStepList;
  delete [] str.ValidTimeStepList;
}

template<class TInputImage, class TOutputImage>
ITK_THREAD_RETURN_TYPE
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::IterateThreaderCallback(void * arg)
{
  // Controls how often we check for balance of the load among the treads and perform
  // load balancing (if needed) by redistributing the load.
  const int LOAD_BALANCE_ITERATION_FREQUENCY = 30;
  
  int i;
#ifdef ITK_USE_SPROC
  // Every thread should 'usadd' itself to the arena as the very first thing so
  // as to detect errors (if any) early.
  if (MultiThreader::GetThreadArena() != 0)
    {
    int code= usadd (MultiThreader::GetThreadArena());
    }
#endif

  int ThreadId = ((MultiThreader::ThreadInfoStruct *)(arg))->ThreadID;
  
  ParallelSparseFieldLevelSetThreadStruct * str
    = (ParallelSparseFieldLevelSetThreadStruct *)
    (((MultiThreader::ThreadInfoStruct *)(arg))->UserData);
  
  // allocate thread data: every thread allocates its own data
  // We do NOT assume here that malloc is thread safe: hence make threads
  // allocate data serially
  if (ThreadId == 0)
    {
    str->Filter->ComputeInitialThreadBoundaries ();
    
    // Create the temporary status image
    str->Filter->m_StatusImageTemp = StatusImageType::New();
    str->Filter->m_StatusImageTemp->SetRegions(str->Filter->m_OutputImage->GetRequestedRegion()); 
    str->Filter->m_StatusImageTemp->Allocate();
    
    // Create the temporary output image
    str->Filter->m_OutputImageTemp = OutputImageType::New();
    str->Filter->m_OutputImageTemp->SetRegions(str->Filter->m_OutputImage->GetRequestedRegion());
    str->Filter->m_OutputImageTemp->Allocate();
    }
  str->Filter->WaitForAll();
  
  // Data allocation performed serially.
  for (i= 0; i < str->Filter->m_NumOfThreads; i++)
    {
    if (ThreadId == i)
      {
      str->Filter->ThreadedAllocateData   (ThreadId);
      }
    str->Filter->WaitForAll();
    }
  
  // Data initialization performed in parallel.
  // Make use of the SGI default first-touch memory placement policy
  str->Filter->GetThreadRegionSplitByBoundary(ThreadId,
                                              str->Filter->m_Data[ThreadId].ThreadRegion);
  str->Filter->ThreadedInitializeData(ThreadId,
                                      str->Filter->m_Data[ThreadId].ThreadRegion);
  str->Filter->WaitForAll();
  
  if (ThreadId == 0)
    {
    str->Filter->m_StatusImage = 0;
    str->Filter->m_StatusImage = str->Filter->m_StatusImageTemp;
    str->Filter->m_StatusImageTemp = 0;
    
    str->Filter->m_OutputImage = 0;
    str->Filter->m_OutputImage = str->Filter->m_OutputImageTemp;
    str->Filter->m_OutputImageTemp = 0;
    //
    str->Filter->GraftOutput(str->Filter->m_OutputImage);
    }
  str->Filter->WaitForAll();
  
  int iter = 0;
  while (! (str->Filter->Halt()) )
    {
    // Threaded Calculate Change
    str->Filter->m_Data[ThreadId].TimeStep
      = str->Filter->ThreadedCalculateChange(ThreadId);
    
    str->Filter->WaitForAll();
    
    // Calcualte the timestep (no need to do this when there is just 1 thread)
    if (str->Filter->m_NumOfThreads == 1)
      {
      if (iter != 0)
        {
        // Update the RMS difference here
        str->Filter->m_RMSChange = str->Filter->m_Data[0].m_RMSChange;
        int count                = str->Filter->m_Data[0].m_Count;
        if (count != 0)
          {
          str->Filter->m_RMSChange = vcl_sqrt(
                ((float) str->Filter->m_RMSChange) / count);
          }
        }
      
      // this is done by the thread0
      str->Filter->InvokeEvent( IterationEvent() );
      str->Filter->InvokeEvent( ProgressEvent () );
      str->Filter->SetElapsedIterations(++iter);
      
      str->TimeStep = str->Filter->m_Data[0].TimeStep; // (works for the 1-thread
                                                      // case else redefined below)
      }
    else
      {
      if (ThreadId == 0)
        {
        if (iter != 0)
          {
          // Update the RMS difference here
          int count = 0;
          str->Filter->m_RMSChange = m_ValueZero;
          for (i = 0; i < str->Filter->m_NumOfThreads; i++)
            {
            str->Filter->m_RMSChange += str->Filter->m_Data[i].m_RMSChange;
            count                    += str->Filter->m_Data[i].m_Count;
            }
          if (count != 0)
            {
            str->Filter->m_RMSChange
              = vcl_sqrt(((float) str->Filter->m_RMSChange) / count);
            }
          }
        
        // Should we stop iterating ? (in case there are too few pixels to
        // process for every thread)
        str->Filter->m_Stop= true;
        for (i= 0; i < str->Filter->m_NumOfThreads; i++) 
          {
          if (str->Filter->m_Data[i].m_Layers[0]->Size() > 10)
            {
            str->Filter->m_Stop= false;
            break;
            }
          }
        
        str->Filter->InvokeEvent ( IterationEvent() );
        str->Filter->InvokeEvent ( ProgressEvent () );
        str->Filter->SetElapsedIterations (++iter);
        }
    
      if (ThreadId == 1)
        {
        for (i= 0; i < str->Filter->m_NumOfThreads; i++)
          {
          str->TimeStepList[i]= str->Filter->m_Data[i].TimeStep;
          }
        str->TimeStep = str->Filter->ResolveTimeStep(str->TimeStepList,
                       str->ValidTimeStepList, str->Filter->m_NumOfThreads);
        }
      }
    
    str->Filter->WaitForAll();
    
    // The active layer is too small => stop iterating
    if (str->Filter->m_Stop == true)
      {
      return ITK_THREAD_RETURN_VALUE;
      }
    
    // Threaded Apply Update
    str->Filter->ThreadedApplyUpdate(str->TimeStep, ThreadId);
    
    // We only need to wait for neighbors because ThreadedCalculateChange
    // requires information only from the neighbors.
    str->Filter->SignalNeighborsAndWait(ThreadId);
    
    if (str->Filter->GetElapsedIterations()
        % LOAD_BALANCE_ITERATION_FREQUENCY == 0)
      {
      str->Filter->WaitForAll();
      // change boundaries if needed
      if (ThreadId == 1)
        {
        str->Filter->CheckLoadBalance();
        }
      str->Filter->WaitForAll();
      
      if (str->Filter->m_BoundaryChanged == true)
        {
        str->Filter->ThreadedLoadBalance (ThreadId);
        str->Filter->WaitForAll();
        }
      }
    }
  
  // post-process output
  str->Filter->GetThreadRegionSplitUniformly(ThreadId,
                        str->Filter->m_Data[ThreadId].ThreadRegion);
  str->Filter->ThreadedPostProcessOutput(ThreadId,
                        str->Filter->m_Data[ThreadId].ThreadRegion);
  
  return ITK_THREAD_RETURN_VALUE;
}

template <class TInputImage, class TOutputImage>
typename
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>::TimeStepType
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::ThreadedCalculateChange(int ThreadId)
{
  typename FiniteDifferenceFunctionType::Pointer df = this->GetDifferenceFunction();

  ConstNeighborhoodIterator<OutputImageType> outputIt (df->GetRadius(), m_OutputImage,
                                                       m_OutputImage->GetRequestedRegion());
  if ( m_BoundsCheckingActive == false )
    {
    outputIt.NeedToUseBoundaryConditionOff();
    }
  
  // Calculates the update values for the active layer indicies in this
  // iteration.  Iterates through the active layer index list, applying 
  // the level set function to the output image (level set image) at each
  // index.
  
  typename LayerType::Iterator layerIt  = m_Data[ThreadId].m_Layers[0]->Begin();
  typename LayerType::Iterator layerEnd = m_Data[ThreadId].m_Layers[0]->End();
  
  for ( ; layerIt != layerEnd; ++layerIt)
    {
    outputIt.SetLocation(layerIt->m_Index);
    layerIt->m_Value = df->ComputeUpdate (outputIt, (void *) m_Data[ThreadId].globalData);
    }
  
  TimeStepType timeStep= df->ComputeGlobalTimeStep ((void*) m_Data[ThreadId].globalData);
  
  return timeStep;         
}

template <class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::ThreadedApplyUpdate (TimeStepType dt, int ThreadId)
{
  this->ThreadedUpdateActiveLayerValues(dt, m_Data[ThreadId].UpList[0],
                                        m_Data[ThreadId].DownList[0], ThreadId);
  
  // We need to update histogram information (because some pixels are LEAVING
  // layer-0 (the active layer)
  
  this->SignalNeighborsAndWait(ThreadId);
  
  // Process status lists and update value for first inside/outside layers
  // int InputLayerNumber, int OutputLayerNumber,
  this->ThreadedProcessStatusList( 0, 1, 2, 1, 1, 0, ThreadId);
  this->ThreadedProcessStatusList( 0, 1, 1, 2, 0, 0, ThreadId);
  
  this->SignalNeighborsAndWait(ThreadId);
  
  // Update first layer value, process first layer/
    this->ThreadedProcessFirstLayerStatusLists( 1, 0, 3, 1, 1, ThreadId);
  this->ThreadedProcessFirstLayerStatusLists( 1, 0, 4, 0, 1, ThreadId);

  // We need to update histogram information (because some pixels are ENTERING
  // layer-0
  
  this->SignalNeighborsAndWait(ThreadId);
  
  StatusType   up_to= 1,   up_search= 5;
  StatusType down_to= 2, down_search= 6;
  unsigned char j= 0, k= 1;
  
  // The 3D case: this loop is executed at least once
  while ( down_search < 2 * m_NumberOfLayers + 1 )
    {
    this->ThreadedProcessStatusList(j, k, up_to, up_search, 1,
                                    (up_search - 1) / 2, ThreadId);
    this->ThreadedProcessStatusList(j, k, down_to, down_search, 0,
                                     (up_search - 1) / 2, ThreadId); 
      
    this->SignalNeighborsAndWait(ThreadId);
      
    up_to   += 2;
    down_to += 2;
    up_search   += 2;
    down_search += 2;
    
    // Swap the lists so we can re-use the empty one.
    j= k;
    k= 1 - j;
    }
  // now up_search   = 2 * m_NumberOfLayers + 1 (= 7 if m_NumberOfLayers = 3)
  // now down_search = 2 * m_NumberOfLayers + 2 (= 8 if m_NumberOfLayers = 3)
  
  // Process the outermost inside/outside layers in the sparse field
  this->ThreadedProcessStatusList(j, k, up_to, m_StatusNull, 1,
                                   (up_search - 1) / 2, ThreadId);
  this->ThreadedProcessStatusList(j, k, down_to, m_StatusNull, 0,
                                  (up_search - 1) / 2, ThreadId);
  
  this->SignalNeighborsAndWait(ThreadId);
  
  this->ThreadedProcessOutsideList(k,(2 * m_NumberOfLayers + 1) - 2, 1,
                                   (up_search + 1) / 2, ThreadId);
  this->ThreadedProcessOutsideList(k,(2 * m_NumberOfLayers + 1) - 1, 0,
                                   (up_search + 1) / 2, ThreadId);
  
  if (m_OutputImage->GetImageDimension() < 3)
    {
    this->SignalNeighborsAndWait(ThreadId);
    }

  // A synchronize is NOT required here because in 3D case we have at least 7
  // layers, thus ThreadedProcessOutsideList() works on layers 5 & 6 while
  // ThreadedPropagateLayerValues() works on 0, 1, 2, 3, 4 only. => There can
  // NOT be any dependencies amoing different threads.
  
  // Finally, we update all of the layer VALUES (excluding the active layer,
  // which has already been updated)
  this->ThreadedPropagateLayerValues(0, 1, 3, 1, ThreadId); // first inside
  this->ThreadedPropagateLayerValues(0, 2, 4, 0, ThreadId); // first outside
  
  this->SignalNeighborsAndWait (ThreadId);
  
  // Update the rest of the layer values
  int i;
  for (i = 1; i < (2 * m_NumberOfLayers + 1) - 2; i += 2)
    {
    j = i+1;
    this->ThreadedPropagateLayerValues(i, i+2,   i+4,   1, ThreadId);
    this->ThreadedPropagateLayerValues(j, j+2,   j+4,   0, ThreadId);    
    this->SignalNeighborsAndWait (ThreadId);
    }
}

template <class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::ThreadedUpdateActiveLayerValues (TimeStepType dt, LayerType * UpList,
                                   LayerType * DownList, int ThreadId)
{
  // This method scales the update buffer values by the time step and adds
  // them to the active layer pixels.  New values at an index which fall
  // outside of the active layer range trigger that index to be placed on the
  // "up" or "down" status list.  The neighbors of any such index are then
  // assigned new values if they are determined to be part of the active list
  // for the next iteration (i.e. their values will be raised or lowered into
  // the active range).
  ValueType LOWER_ACTIVE_THRESHOLD = - (m_ConstantGradientValue / 2.0);
  ValueType UPPER_ACTIVE_THRESHOLD =    m_ConstantGradientValue / 2.0 ;
  
  LayerNodeType *release_node= 0;
  bool flag;
  
  IndexType  centerIndex;
  PixelType  centerValue;
  
  unsigned long int counter =0;
  float new_value;
  float rms_change_accumulator = m_ValueZero;
  
  int Neighbor_Size = m_NeighborList.GetSize();
  
  typename LayerType::Iterator layerIt
    = m_Data[ThreadId].m_Layers[0]->Begin();
  typename LayerType::Iterator layerEnd
    = m_Data[ThreadId].m_Layers[0]->End();
  
  while (layerIt != layerEnd )
    {
      centerIndex = layerIt->m_Index;
      centerValue = m_OutputImage->GetPixel(centerIndex);
      
      new_value = this->CalculateUpdateValue(centerIndex, dt,
                                             centerValue, layerIt->m_Value);
      
      // If this index needs to be moved to another layer, then search its
      // neighborhood for indicies that need to be pulled up/down into the
      // active layer. Set those new active layer values appropriately,
      // checking first to make sure they have not been set by a more
      // influential neighbor.
      
      //   ...But first make sure any neighbors in the active layer are not
      // moving to a layer in the opposite direction.  This step is necessary
      // to avoid the creation of holes in the active layer.  The fix is simply
      // to not change this value and leave the index in the active set.
      
      if (new_value > UPPER_ACTIVE_THRESHOLD)
        {
        // This index will move UP into a positive (outside) layer
        // First check for active layer neighbors moving in the opposite
        // direction
        flag = false;
        for (int i = 0; i < Neighbor_Size; ++i)
          {
          if (m_StatusImage->GetPixel(centerIndex + m_NeighborList.GetNeighborhoodOffset(i))
              == m_StatusActiveChangingDown)
            {
            flag = true;
            break;
            }
          }
        if (flag == true)
          {
          ++layerIt;
          continue;
          }
        
        rms_change_accumulator += vnl_math_sqr((float)(new_value - centerValue ));
        // update the value of the pixel
        m_OutputImage->SetPixel (centerIndex, new_value);
        
        // Now remove this index from the active list.
        release_node = layerIt.GetPointer();
        ++layerIt;
        
        m_Data[ThreadId].m_Layers[0]->Unlink(release_node);
        m_Data[ThreadId].m_ZHistogram[ release_node->m_Index[m_SplitAxis] ]
          = m_Data[ThreadId].m_ZHistogram[ release_node->m_Index[m_SplitAxis] ] - 1;
        
        // add the release_node to status up list
        UpList->PushFront(release_node);
        
        //
        m_StatusImage->SetPixel(centerIndex, m_StatusActiveChangingUp);
        }
      else if (new_value < LOWER_ACTIVE_THRESHOLD)
        {
        // This index will move DOWN into a negative (inside) layer.
        // First check for active layer neighbors moving in the opposite direction
        flag = false;
        for (int i = 0; i < Neighbor_Size; ++i)
          {
          if (m_StatusImage->GetPixel(centerIndex+m_NeighborList.GetNeighborhoodOffset(i))
              == m_StatusActiveChangingUp)
            {
            flag = true;
            break;
            }
          }
        if (flag == true)
          {
          ++layerIt;
          continue;              
          }
        
        rms_change_accumulator += vnl_math_sqr((float)(new_value - centerValue));
        // update the value of the pixel
        m_OutputImage->SetPixel(centerIndex, new_value );
        
        // Now remove this index from the active list.
        release_node = layerIt.GetPointer();
        ++layerIt;
        
        m_Data[ThreadId].m_Layers[0]->Unlink(release_node);
        m_Data[ThreadId].m_ZHistogram[ release_node->m_Index[m_SplitAxis] ]
          = m_Data[ThreadId].m_ZHistogram[ release_node->m_Index[m_SplitAxis] ] - 1;
        
        // now add release_node to status down list
        DownList->PushFront(release_node);
        
        m_StatusImage->SetPixel(centerIndex, m_StatusActiveChangingDown);
        }
      else
        {
        rms_change_accumulator += vnl_math_sqr((float)(new_value - centerValue));
        // update the value of the pixel
        m_OutputImage->SetPixel(centerIndex, new_value );
        ++layerIt;
        }
      ++counter;
    }
  
  // Determine the average change during this iteration
  if (counter == 0)
    {
    m_Data[ThreadId].m_RMSChange = m_ValueZero;
    }
  else
    {
    m_Data[ThreadId].m_RMSChange = rms_change_accumulator;
    }
  
  m_Data[ThreadId].m_Count = counter;
}

template <class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::CopyInsertList (int ThreadId, LayerPointerType FromListPtr,
                  LayerPointerType ToListPtr)
{
  typename LayerType::Iterator layerIt = FromListPtr->Begin();
  typename LayerType::Iterator layerEnd= FromListPtr->End();
  
  LayerNodeType * nodePtr = 0;
  LayerNodeType * nodeTempPtr= 0;
  
  while (layerIt != layerEnd)
    {
    // copy the node
    nodePtr= layerIt.GetPointer();
    ++layerIt;
    
    nodeTempPtr= m_Data[ThreadId].m_LayerNodeStore->Borrow();
    nodeTempPtr->m_Index= nodePtr->m_Index;
    
    // insert
    ToListPtr->PushFront (nodeTempPtr);
    }
}

template <class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::ClearList (int ThreadId, LayerPointerType ListPtr)
{
  LayerNodeType * nodePtr = 0;
  
  while (! ListPtr->Empty())
    {
    nodePtr= ListPtr->Front();
    // remove node from layer
    ListPtr->PopFront();
    // return node to node-pool
    m_Data[ThreadId].m_LayerNodeStore->Return (nodePtr);
    }
}

template <class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::CopyInsertInterNeighborNodeTransferBufferLayers (int ThreadId, LayerPointerType List,
                                                   int InOrOut, int BufferLayerNumber)
{
  if (ThreadId != 0)
    {
    CopyInsertList(ThreadId,
                   m_Data[this->GetThreadNumber(m_Boundary[ThreadId-1])].m_InterNeighborNodeTransferBufferLayers[InOrOut][BufferLayerNumber][ThreadId],
                   List);
    }
  
  if (m_Boundary[ThreadId] != m_ZSize - 1)
    {
    CopyInsertList(ThreadId,
                   m_Data[this->GetThreadNumber (m_Boundary[ThreadId] + 1)].m_InterNeighborNodeTransferBufferLayers[InOrOut][BufferLayerNumber][ThreadId],
                   List);
    }
}

template <class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::ClearInterNeighborNodeTransferBufferLayers (int ThreadId, int InOrOut,
                                              int BufferLayerNumber)
{
  for (int i= 0; i < m_NumOfThreads; i++)
    {
    ClearList(ThreadId, m_Data[ThreadId].m_InterNeighborNodeTransferBufferLayers[InOrOut][BufferLayerNumber][i]);
    }
}

template <class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::ThreadedProcessFirstLayerStatusLists (int InputLayerNumber, int OutputLayerNumber,
                                        StatusType SearchForStatus,
                                        int InOrOut, int BufferLayerNumber, int ThreadId)
{
  LayerNodeType* nodePtr = 0;
  StatusType from, neighbor_status;
  ValueType value, value_temp, delta;
  bool found_neighbor_flag;
  
  IndexType center_index, n_index;
  
  int neighbor_Size = m_NeighborList.GetSize();
  LayerPointerType InputList, OutputList;
  
  //InOrOut == 1, inside, more negative, uplist
  //InOrOut == 0, outside
  if (InOrOut == 1)
    {
    delta =  - m_ConstantGradientValue;
    from = 1;
    InputList  = m_Data[ThreadId].UpList[InputLayerNumber];
    OutputList = m_Data[ThreadId].UpList[OutputLayerNumber];
    }
  else
    {
    delta = m_ConstantGradientValue;
    from = 2;
    InputList  = m_Data[ThreadId].DownList[InputLayerNumber];
    OutputList = m_Data[ThreadId].DownList[OutputLayerNumber];
    }
  
  // 1. nothing to clear
  // 2. make a copy of the node on the
  //    m_InterNeighborNodeTransferBufferLayers[InOrOut][BufferLayerNumber - 1][i]
  //    for all neighbors i ... and insert it in one's own InputList
  CopyInsertInterNeighborNodeTransferBufferLayers(ThreadId, InputList, InOrOut,
                                                  BufferLayerNumber - 1);
  
  typename LayerType::Iterator layerIt  = InputList->Begin();
  typename LayerType::Iterator layerEnd = InputList->End();
  while (layerIt != layerEnd)
    {
    nodePtr = layerIt.GetPointer();
    ++layerIt;
    
    center_index = nodePtr->m_Index;
    
    // remove node from InputList
    InputList->Unlink(nodePtr);
    
    // check if this is not a duplicate pixel in the InputList
    // In the case when the thread boundaries differ by just 1 pixel some
    // nodes may get added twice in the InputLists Even if the boundaries are
    // more than 1 pixel wide the *_shape_* of the layer may allow this to
    // happen. Solution: If a pixel comes multiple times than we would find
    // that the Status image would already be reflecting the new status after
    // the pixel was encountered the first time 
    if (m_StatusImage->GetPixel(center_index) == 0)
      {
      // duplicate node => return it to the node pool
      m_Data[ThreadId].m_LayerNodeStore->Return (nodePtr);      
      continue;
      }
    
    // set status to zero
    m_StatusImage->SetPixel(center_index, 0);
    // add node to the layer-0
    m_Data[ThreadId].m_Layers[0]->PushFront(nodePtr);

    m_Data[ThreadId].m_ZHistogram[ nodePtr->m_Index[m_SplitAxis] ]
      = m_Data[ThreadId].m_ZHistogram[ nodePtr->m_Index[m_SplitAxis] ] + 1;
      
    value = m_OutputImage->GetPixel(center_index);
    found_neighbor_flag = false;
    for (int i = 0; i < neighbor_Size; ++i)
      {
      n_index = center_index + m_NeighborList.GetNeighborhoodOffset(i);
      neighbor_status = m_StatusImage->GetPixel(n_index);
      
      // Have we bumped up against the boundary?  If so, turn on bounds checking.
      if ( neighbor_status == m_StatusBoundaryPixel )
        {
        m_BoundsCheckingActive = true;
        }
      
      if (neighbor_status ==  from)
        {
        value_temp = m_OutputImage->GetPixel(n_index);
        
        if (found_neighbor_flag == false)
          {
          value = value_temp;
          }
        else
          {
          if (InOrOut == 1)
            {
            // Find the largest (least negative) neighbor
            if ( value_temp > value )
              {
              value = value_temp;
              }
            }
          else
            {
            // Find the smallest (least positive) neighbor
            if (value_temp < value)
              { value = value_temp; }
            }
          }
        found_neighbor_flag = true;
        }
      
      if (neighbor_status == SearchForStatus)
        { 
        // mark this pixel so we MAY NOT add it twice
        // This STILL DOES NOT GUARANTEE RACE CONDITIONS BETWEEN THREADS. This
        // is handled at the next stage
        m_StatusImage->SetPixel(n_index, m_StatusChanging);
        
        int tmpId = this->GetThreadNumber(n_index[m_SplitAxis]);
        
        nodePtr = m_Data[ThreadId].m_LayerNodeStore->Borrow();
        nodePtr->m_Index = n_index;
           
        if (tmpId != ThreadId)
          {
          m_Data[ThreadId].m_InterNeighborNodeTransferBufferLayers[InOrOut][BufferLayerNumber][tmpId]->PushFront(nodePtr);
          }
        else
          {
          OutputList->PushFront(nodePtr);
          }
        }
      }
    m_OutputImage->SetPixel(center_index, value + delta );
    }
}

template <class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::ThreadedProcessStatusList (int InputLayerNumber, int OutputLayerNumber,
                             StatusType ChangeToStatus, StatusType SearchForStatus,
                             int InOrOut, int BufferLayerNumber, int ThreadId)
{
  int i;
  LayerNodeType* nodePtr= 0;
  StatusType neighbor_status;
  
  IndexType   center_index, n_index;
  
  LayerPointerType InputList, OutputList;
  
  // Push each index in the input list into its appropriate status layer
  // (ChangeToStatus) and update the status image value at that index.
  // Also examine the neighbors of the index to determine which need to go onto
  // the output list (search for SearchForStatus).
  if (InOrOut == 1)
    {
    InputList  = m_Data[ThreadId].UpList[InputLayerNumber];
    OutputList = m_Data[ThreadId].UpList[OutputLayerNumber];
    }
  else
    {
    InputList  = m_Data[ThreadId].DownList[InputLayerNumber];
    OutputList = m_Data[ThreadId].DownList[OutputLayerNumber];
    }
  
  // 1. clear one's own
  // m_InterNeighborNodeTransferBufferLayers[InOrOut][BufferLayerNumber - 2][i]
  // for all threads i.
  if (BufferLayerNumber >= 2)
    {
    ClearInterNeighborNodeTransferBufferLayers(ThreadId, InOrOut,
                                               BufferLayerNumber - 2);
    }
  // SPECIAL CASE: clear one's own
  // m_InterNeighborNodeTransferBufferLayers[InOrOut][m_NumberOfLayers][i] for
  // all threads i
  if (BufferLayerNumber == 0)
    {
    ClearInterNeighborNodeTransferBufferLayers(ThreadId, InOrOut, m_NumberOfLayers);
    }
  // obtain the pixels (from last iteration) that were given to you from other
  // (neighboring) threads 2. make a copy of the node on the
  // m_InterNeighborNodeTransferBufferLayers[InOrOut][LastLayer - 1][i] for all
  // thread neighbors i ... ... and insert it in one's own InoutList
  if (BufferLayerNumber > 0)
    {
    CopyInsertInterNeighborNodeTransferBufferLayers(ThreadId, InputList,
                                                    InOrOut, BufferLayerNumber - 1);
    }
  
  int neighbor_size = m_NeighborList.GetSize();
  while ( ! InputList->Empty() )
    {
    nodePtr = InputList->Front();
    center_index = nodePtr->m_Index;
      
    InputList->PopFront();
      
    // Check if this is not a duplicate pixel in the InputList.
    // Solution: If a pixel comes multiple times than we would find that the
    // Status image would already be reflecting 
    // the new status after the pixel was encountered the first time 
    if ((BufferLayerNumber != 0)
        && (m_StatusImage->GetPixel(center_index) == ChangeToStatus)) 
      {
      // duplicate node => return it to the node pool
      m_Data[ThreadId].m_LayerNodeStore->Return (nodePtr);
      
      continue;
      }
    
    // add to layer
    m_Data[ThreadId].m_Layers[ChangeToStatus]->PushFront (nodePtr);
    // change the status
    m_StatusImage->SetPixel(center_index, ChangeToStatus);
    
    for (i = 0; i < neighbor_size; ++i)
      {
      n_index = center_index + m_NeighborList.GetNeighborhoodOffset(i);
      
      neighbor_status = m_StatusImage->GetPixel(n_index);
      
      // Have we bumped up against the boundary?  If so, turn on bounds checking.
      if ( neighbor_status == m_StatusBoundaryPixel )
        {
        m_BoundsCheckingActive = true;
        }
      
      if (neighbor_status == SearchForStatus)
        {
        // mark this pixel so we MAY NOT add it twice
        // This STILL DOES NOT AVOID RACE CONDITIONS BETWEEN THREADS (This is
        // handled at the next stage) 
        m_StatusImage->SetPixel(n_index, m_StatusChanging);
        
        int tmpId = this->GetThreadNumber (n_index[m_SplitAxis]);
        
        nodePtr = m_Data[ThreadId].m_LayerNodeStore->Borrow();
        nodePtr->m_Index = n_index;
        
        if (tmpId != ThreadId)
          {
          m_Data[ThreadId].m_InterNeighborNodeTransferBufferLayers[InOrOut][BufferLayerNumber][tmpId]->PushFront(nodePtr);
          }
        else
          {
          OutputList->PushFront(nodePtr);
          }
        }
      }
    }
}

template <class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::ThreadedProcessOutsideList (int InputLayerNumber, StatusType ChangeToStatus,
                              int InOrOut, int BufferLayerNumber, int ThreadId)
{
  LayerPointerType OutsideList;
  if (InOrOut == 1)
    {
    OutsideList= m_Data[ThreadId].UpList  [InputLayerNumber];
    }
  else
    {
    OutsideList= m_Data[ThreadId].DownList[InputLayerNumber];
    }
  
  // obtain the pixels (from last iteration of ThreadedProcessStatusList() )
  // that were given to you from other (neighboring) threads
  // 1. clear one's own
  //    m_InterNeighborNodeTransferBufferLayers[InOrOut][BufferLayerNumber - 2][i]
  //    for all threads i. 
  ClearInterNeighborNodeTransferBufferLayers(ThreadId, InOrOut, BufferLayerNumber - 2);
  
  // 2. make a copy of the node on the
  //    m_InterNeighborNodeTransferBufferLayers[InOrOut][LastLayer - 1][i] for
  //    all thread neighbors i ... ... and insert it in one's own InoutList
  CopyInsertInterNeighborNodeTransferBufferLayers(ThreadId, OutsideList, InOrOut,
                                                  BufferLayerNumber - 1);
  
  // Push each index in the input list into its appropriate status layer
  // (ChangeToStatus) and ... ... update the status image value at that index 
  LayerNodeType* nodePtr= 0;
  while ( ! OutsideList->Empty() )
    {
    nodePtr = OutsideList->Front();
    OutsideList->PopFront();
      
    m_StatusImage->SetPixel(nodePtr->m_Index, ChangeToStatus); 
    m_Data[ThreadId].m_Layers[ChangeToStatus]->PushFront (nodePtr);
    }
}

template <class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::ThreadedPropagateLayerValues(StatusType from, StatusType to, StatusType promote,
                               int InOrOut, int ThreadId)
{
  ValueType value, value_temp, delta;
  bool found_neighbor_flag;
  typename LayerType::Iterator toIt;
  typename LayerType::Iterator toEnd;
  LayerNodeType* nodePtr;
  StatusType past_end = static_cast<StatusType>( m_Layers.size() ) - 1;
  
  // Are we propagating values inward (more negative) or outward (more positive)?
  if (InOrOut == 1)
    {
    delta = - m_ConstantGradientValue;
    }
  else
    {
    delta =   m_ConstantGradientValue;
    }
  
  int Neighbor_Size = m_NeighborList.GetSize();
  toIt  =  m_Data[ThreadId].m_Layers[to]->Begin();
  toEnd =  m_Data[ThreadId].m_Layers[to]->End();
  
  IndexType  centerIndex, nIndex;
  StatusType centerStatus, nStatus;
  
  while ( toIt != toEnd )
    {
      centerIndex = toIt->m_Index;
      
      centerStatus = m_StatusImage->GetPixel(centerIndex);
      
      if (centerStatus != to)
        {
        // delete nodes NOT deleted earlier          
        nodePtr = toIt.GetPointer();
        ++toIt;
        
        // remove the node from the layer
        m_Data[ThreadId].m_Layers[to]->Unlink( nodePtr );
        m_Data[ThreadId].m_LayerNodeStore->Return( nodePtr );
        continue;
        }
      
      value = m_ValueZero;
      found_neighbor_flag = false;
      for (int i = 0; i < Neighbor_Size ; ++i)
        {
        nIndex = centerIndex + m_NeighborList.GetNeighborhoodOffset(i);
        nStatus = m_StatusImage->GetPixel(nIndex);
        // If this neighbor is in the "from" list, compare its absolute value
        // to any previous values found in the "from" list.  Keep only the
        // value with the greatest magnitude.
        
        if (nStatus == from)
          {
          value_temp = m_OutputImage->GetPixel(nIndex);
          
          if (found_neighbor_flag == false)
            {
            value = value_temp;
            }
          else
            {
            if (InOrOut == 1)
              {
              // Find the largest (least negative) neighbor
              if ( value_temp > value )
                { value = value_temp; }
              }
            else
              {
              // Find the smallest (least positive) neighbor
              if (value_temp < value)
                { value = value_temp; }
              }
            }
          found_neighbor_flag = true;
          }
        }
      if (found_neighbor_flag == true)
        {
        // Set the new value using the largest magnitude found in our "from"
        // neighbors
        m_OutputImage->SetPixel (centerIndex, value + delta);
        ++toIt;
        }
      else
        {
        // Did not find any neighbors on the "from" list, then promote this
        // node.  A "promote" value past the end of my sparse field size
        // means delete the node instead.  Change the status value in the
        // status image accordingly.
        nodePtr  = toIt.GetPointer();
        ++toIt;
        m_Data[ThreadId].m_Layers[to]->Unlink( nodePtr );
        
        if ( promote > past_end )
          {
          m_Data[ThreadId].m_LayerNodeStore->Return( nodePtr );
          m_StatusImage->SetPixel(centerIndex, m_StatusNull);
          }
        else
          {
          m_Data[ThreadId].m_Layers[promote]->PushFront( nodePtr );
          m_StatusImage->SetPixel(centerIndex, promote);
          }
        }
    }
}

template<class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::CheckLoadBalance()
{
  int i, j;
  
  // This parameter defines a degree of unbalancedness of the load among threads.
  const float MAX_PIXEL_DIFFERENCE_PERCENT = 0.025;
  m_BoundaryChanged = false;
  
  // work load division based on the nodes on the active layer (layer-0)
  long int min = NumericTraits<long int>::max();
  long int max = 0;
  long int total= 0; // the total nodes in the active layer of the surface

  for (i = 0; i < m_NumOfThreads; i++)
    {
    long int count = m_Data[i].m_Layers[0]->Size();
    total += count;
    if (min > count) min = count;
    if (max < count) max = count;
    }
  
  if (max - min < MAX_PIXEL_DIFFERENCE_PERCENT * total / m_NumOfThreads)
    {
    // if the difference between max and min is NOT even x% of the average
    // nodes in the thread layers then no need to change the boundaries next
    return;
    }
  
  // Change the boundaries --------------------------
  
  // compute the global histogram from the individual histograms
  for (i= 0; i < m_NumOfThreads; i++)
    {
    for (j= (i == 0 ? 0 : m_Boundary[i-1] + 1); j <= m_Boundary[i]; j++)
      {
      m_GlobalZHistogram[j] = m_Data[i].m_ZHistogram[j];      
      }
    }
  
  // compute the cumulative frequency distribution using the histogram
  m_ZCumulativeFrequency[0] = m_GlobalZHistogram[0];
  for (i= 1; i < m_ZSize; i++)
    {
    m_ZCumulativeFrequency[i] = m_ZCumulativeFrequency[i-1] + m_GlobalZHistogram[i];
    }
  
  // now define the boundaries
  m_Boundary[m_NumOfThreads - 1] = m_ZSize - 1; // special case: the last bound
  
  for (i= 0; i < m_NumOfThreads - 1; i++)
    {
    // compute m_Boundary[i]    
    float cutOff= 1.0f * (i+1) * m_ZCumulativeFrequency[m_ZSize-1] / m_NumOfThreads;
    
    // find the position in the cumulative freq dist where this cutoff is met
    for (j= (i == 0 ? 0 : m_Boundary[i-1]); j < m_ZSize; j++) 
      {
      if (cutOff > m_ZCumulativeFrequency[j])
        {
        continue;
        }
      else
        {
        // do some optimization !
        // go further FORWARD and find the first index (k) in the cumulative
        // freq distribution s.t. m_ZCumulativeFrequency[k] !=
        // m_ZCumulativeFrequency[j]. This is to be done because if we have a
        // flat patch in the cum freq dist then ... . we can choose a bound
        // midway in that flat patch 
        int k;
        for (k= 1; j+k < m_ZSize; k++)
          {
          if (m_ZCumulativeFrequency[j+k] != m_ZCumulativeFrequency[j])
            {
            break;
            }
          }
        
        // if ALL new boundaries same as the original then NO NEED TO DO
        // ThreadedLoadBalance() next !!! 
        int newBoundary= (int) ((j + (j+k)) / 2);
        if (newBoundary != m_Boundary[i])
          {
          //
          m_BoundaryChanged= true;
          m_Boundary[i]= newBoundary;
          }
        break;
        }
      }
    }
  
  if (m_BoundaryChanged == false)
    {
    return;
    }
  
  // Reset the individual histograms to reflect the new distrbution
  // Also reset the mapping from the Z value --> the thread number i.e. m_MapZToThreadNumber[]
  for (i= 0; i < m_NumOfThreads; i++)
    {
    for (j= 0; j <= (i == 0 ? -1 : m_Boundary[i-1]); j++)
      {
      m_Data[i].m_ZHistogram[j] = 0;
      }
    
    for (j= (i == 0 ? 0 : m_Boundary[i-1] + 1); j <= m_Boundary[i]; j++)
      {
      // this Z histogram value should be given to thread-i
      m_Data[i].m_ZHistogram[j] = m_GlobalZHistogram[j];
      
      // this Z belongs to the region associated with thread-i
      m_MapZToThreadNumber[j]= i;      
      }
      
    for (j= m_Boundary[i] + 1; j < m_ZSize; j++)
      {
      m_Data[i].m_ZHistogram[j] = 0;
      }
    }  
}

template<class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::ThreadedLoadBalance(int ThreadId)
{
  // the situation at this point in time::
  // the OPTIMAL boundaries (that divide work equally) have changed but ...
  // the thread data lags behind the boundaries (it is still following the old
  // boundaries) the m_ZHistogram[], however, reflects the latest optimal
  // boundaries
  
  // The task:
  // 1. Every thread checks for pixels with itself that should NOT be with
  //    itself anymore (because of the changed boundaries).
  //    These pixels are now put in extra "buckets" for other threads to grab
  // 2. WaitForAll ().
  // 3. Every thread grabs those pixels, from every other thread, that come
  //    within its boundaries (from the extra buckets).
  
  ////////////////////////////////////////////////////
  // 1.

  int i, j;
  // cleanup the layers first
  for (i = 0; i < 2 * m_NumberOfLayers + 1; i++)
    {
    for (j= 0; j < m_NumOfThreads; j++)
      {
      if (j == ThreadId)
        {
        // a thread does NOT pass nodes to istelf
        continue;
        }
      
      ClearList(ThreadId, m_Data[ThreadId].m_LoadTransferBufferLayers[i][j]);
      }
    }
  
  LayerNodeType * nodePtr= 0;
  for (i = 0; i < 2 * m_NumberOfLayers + 1; i++) // for all layers
    {
    typename LayerType::Iterator layerIt  = m_Data[ThreadId].m_Layers[i]->Begin();
    typename LayerType::Iterator layerEnd = m_Data[ThreadId].m_Layers[i]->End();
    
    while (layerIt != layerEnd)
      {
      nodePtr= layerIt.GetPointer();
      ++layerIt;
      
      // use the latest (just updated in CheckLoadBalance) boundaries to
      // determine to which thread region does the pixel now belong
      int tmpId = this->GetThreadNumber(nodePtr->m_Index[m_SplitAxis]);
      
      if (tmpId != ThreadId) // this pixel no longer belongs to this thread
        {
        // remove from the layer
        m_Data[ThreadId].m_Layers[i]->Unlink(nodePtr);
        
        // insert temporarily into the special-layers TO BE LATER taken by the
        // other thread
        // NOTE: What is pushed is a node belonging to the LayerNodeStore of
        // ThreadId. This is deleted later (during the start of the next
        // SpecialIteration).  What is taken by the other thread is NOT this
        // node BUT a copy of it.
        m_Data[ThreadId].m_LoadTransferBufferLayers[i][tmpId]->PushFront(nodePtr);
        }
      }
    }
  
  ////////////////////////////////////////////////////
  // 2.
  this->WaitForAll();
  
  ////////////////////////////////////////////////////
  // 3.
  for (i = 0; i < 2 * m_NumberOfLayers + 1; i++)
    {
    // check all other threads
    for (int j= 0; j < m_NumOfThreads; j++)
      {
      if (j == ThreadId)
        {
        continue; // exclude oneself
        }
      
      CopyInsertList(ThreadId, m_Data[j].m_LoadTransferBufferLayers[i][ThreadId],
                     m_Data[ThreadId].m_Layers[i]);
      }
    }
}

template<class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::GetThreadRegionSplitByBoundary(int ThreadId, ThreadRegionType & ThreadRegion)
{
  // Initialize the ThreadRegion to the output's requested region
  ThreadRegion = m_OutputImage->GetRequestedRegion();
  
  // compute lower bound on the index
  typename TOutputImage::IndexType threadRegionIndex = ThreadRegion.GetIndex();
  threadRegionIndex[m_SplitAxis] += (ThreadId == 0 ? 0 : m_Boundary[ThreadId-1]);
  ThreadRegion.SetIndex (threadRegionIndex);
  
  // compute the size of the region
  typename TOutputImage::SizeType threadRegionSize = ThreadRegion.GetSize();
  threadRegionSize[m_SplitAxis] = (ThreadId == 0
                                   ? (m_Boundary[0] + 1)
                                   : m_Boundary[ThreadId] - m_Boundary[ThreadId-1]);
  ThreadRegion.SetSize(threadRegionSize);
}

template<class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::GetThreadRegionSplitUniformly(int ThreadId, ThreadRegionType & ThreadRegion)
{
  // Initialize the ThreadRegion to the output's requested region
  ThreadRegion = m_OutputImage->GetRequestedRegion();
  
  typename TOutputImage::IndexType threadRegionIndex = ThreadRegion.GetIndex();
  threadRegionIndex[m_SplitAxis]
    += (int) (1.0 * ThreadId * m_ZSize / m_NumOfThreads);
  ThreadRegion.SetIndex(threadRegionIndex);
  
  typename TOutputImage::SizeType  threadRegionSize = ThreadRegion.GetSize();

  // compute lower bound on the index and the size of the region
  if (ThreadId < m_NumOfThreads - 1) // this is NOT the last thread
    {
    threadRegionSize [m_SplitAxis]  = (int) (1.0 * (ThreadId+1) * m_ZSize / m_NumOfThreads)
      - (int) (1.0 * ThreadId * m_ZSize / m_NumOfThreads);
    }
  else
    {
    threadRegionSize [m_SplitAxis]  = m_ZSize
      - (int) (1.0 * ThreadId * m_ZSize / m_NumOfThreads);
    }
  ThreadRegion.SetSize(threadRegionSize);
}

template<class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::ThreadedPostProcessOutput(int ThreadId, const ThreadRegionType & regionToProcess)
{
  // Assign background pixels INSIDE the sparse field layers to a new level set
  // with value greater than the innermost layer.  Assign background pixels
  // OUTSIDE the sparse field layers to a new level set with value less than
  // the outermost layer.  
  const ValueType max_layer = static_cast<ValueType>(m_NumberOfLayers);
  const ValueType inside_value  =   max_layer + m_ConstantGradientValue;
  const ValueType outside_value = -(max_layer + m_ConstantGradientValue);
  
  ImageRegionConstIterator <StatusImageType> statusIt(m_StatusImage, regionToProcess);
  ImageRegionIterator      <OutputImageType> outputIt(m_OutputImage, regionToProcess);
  
  for (outputIt = outputIt.Begin(), statusIt = statusIt.Begin();
       ! outputIt.IsAtEnd(); ++outputIt, ++statusIt)
    {
    if (statusIt.Get() == m_StatusNull)
      {
      if (outputIt.Get() > m_ValueZero)
        {
        outputIt.Set (inside_value);
        }
      else
        {
        outputIt.Set (outside_value);
        }
      }
    }
}

template<class TInputImage, class TOutputImage>
int
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::GetThreadNumber (int splitAxisValue)
{
  return ( m_MapZToThreadNumber[splitAxisValue] );
}

template<class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::SignalNeighborsAndWait (int ThreadId)
{
  // This is the case when a thread has no pixels to process
  // This case is analogous to NOT using that thread at all
  // Hence this thread does not need to signal / wait for any other neighbor
  // thread during the iteration 
  if (ThreadId != 0)
    {
    if (m_Boundary[ThreadId-1] == m_Boundary[ThreadId])
      {
      m_Data[ThreadId].m_SemaphoreArrayNumber=  1
        - m_Data[ThreadId].m_SemaphoreArrayNumber;
      return;
      }
    }
  
  int lastThreadId = m_NumOfThreads - 1;
  if (lastThreadId == 0)
    {
    return; // only 1 thread => no need to wait
    }
  
  // signal neighbors that work is done
  if (ThreadId != 0) // not the first thread
    {
    this->SignalNeighbor(m_Data[ThreadId].m_SemaphoreArrayNumber,
                         this->GetThreadNumber ( m_Boundary[ThreadId-1] ));
    }
  if (m_Boundary[ThreadId] != m_ZSize - 1) // not the last thread
    {
    this->SignalNeighbor (m_Data[ThreadId].m_SemaphoreArrayNumber,
                          this->GetThreadNumber ( m_Boundary[ThreadId] + 1 ));
    }
  
  // wait for signal from neighbors signifying that their work is done
  if ((ThreadId == 0) || (m_Boundary[ThreadId] == m_ZSize - 1))
    {
    // do it just once for the first and the last threads because they share
    // just 1 boundary (just 1 neighbor) 
    this->WaitForNeighbor (m_Data[ThreadId].m_SemaphoreArrayNumber, ThreadId);
    m_Data[ThreadId].m_SemaphoreArrayNumber= 1 - m_Data[ThreadId].m_SemaphoreArrayNumber;
    }
  else
    {
    // do twice because share 2 boundaries with neighbors
    this->WaitForNeighbor (m_Data[ThreadId].m_SemaphoreArrayNumber, ThreadId);
    this->WaitForNeighbor (m_Data[ThreadId].m_SemaphoreArrayNumber, ThreadId);
    
    m_Data[ThreadId].m_SemaphoreArrayNumber=  1 - m_Data[ThreadId].m_SemaphoreArrayNumber;
    }
}

template<class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::SignalNeighbor(int SemaphoreArrayNumber, int ThreadId)
{
  m_Data[ThreadId].m_Semaphore[SemaphoreArrayNumber]->Up();
}

template<class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::WaitForNeighbor(int SemaphoreArrayNumber, int ThreadId)
{
  m_Data[ThreadId].m_Semaphore[SemaphoreArrayNumber]->Down();
}

template<class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::WaitForAll ()
{
  m_Barrier->Wait();
}

template<class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  
  unsigned int i;
  os << indent << "m_IsoSurfaceValue" << m_IsoSurfaceValue << std::endl;
  os << indent << "m_LayerNodeStore: " << m_LayerNodeStore;
  for (i=0; i < m_Layers.size(); i++)
    {
    os << indent << "m_Layers[" << i << "]: size="
       << m_Layers[i]->Size() << std::endl;
    os << indent << m_Layers[i];
    }
}

/**
template <class TInputImage, class TOutputImage>
void
ParallelSparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
::WriteActivePointsToFile()
{
  std::cout << "WriteActivePointsToFile called ..." << std::endl << std::flush;
  
  typename LayerType::Iterator layerIt, end;
  
  FILE* out;
  out= fopen("surf.pts", "wt");
  if(!out) std::cout<<"Can not open surf.pts for write"<<std::endl << std::flush;
  
  for(int i = 0; i < m_NumOfThreads; i++)
    {
      layerIt = m_Data[i].m_Layers[0]->Begin(); 
      end     = m_Data[i].m_Layers[0]->End(); 
      
      while(layerIt != end)
        {
          for(int j = 0; j < ImageDimension; j++)
            {
              fprintf (out,"%d ", (int)layerIt->m_Index[j]);
            }
          fprintf (out, "\n");
    
          ++layerIt;
        }
    }
  fclose(out);
}
*/
} // end namespace itk

#endif
