/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedImageFilter.txx
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
#ifndef _itkWatershedImageFilter_txx
#define _itkWatershedImageFilter_txx

#include "itkConstNeighborhoodIterator.h"
#include "itkConstRandomAccessNeighborhoodIterator.h"
#include "itkNeighborhoodIterator.h"
#include "itkSmartNeighborhoodIterator.h"
#include "itkRandomAccessNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
#include "itkOffset.h"
#include "itkNumericTraits.h"
#include <algorithm>
namespace itk
{

template< class TInputImage, class TOutputImage>
void
WatershedImageFilter<TInputImage, TOutputImage>
::PrintSegmentTable(const SegmentTableType &table)
{
  for (SegmentTableType::const_iterator it = table.begin();
       it != table.end(); ++it)
    {
      std::cout << it->first << " Minimum = " << it->second.Minimum
                << " Depth= " << it->second.Depth
                << " MEV= " << it->second.MinimumEdgeValue
                << " MTL= " << it->second.MergedToLabel 
                << " EdgeTable={ ";
      for (EdgeTableType::const_iterator eit = it->second.EdgeTable.begin();
           eit != it->second.EdgeTable.end(); ++eit)
        {
          std::cout << eit->first <<  "(" << eit->second << ") ";
        }
      std::cout << "}" << std::endl;
    }
}

template< class TInputImage, class TOutputImage>
void
WatershedImageFilter<TInputImage, TOutputImage>
::PrintMergeHeap(const MergeListType &table)
{
  for (typename MergeListType::const_iterator it = table.begin();
       it != table.end(); ++it)
    {
      std::cout << (*it).FromLabel << " -> " << (*it).ToLabel
                << " : " << (*it).Saliency << std::endl;
    }
}
  
template< class TInputImage, class TOutputImage >
void
WatershedImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  WatershedSegmentBasicOutput<TInputImage, TOutputImage>::Pointer basic_output
    = this->GetBasicOutput();
  typename TInputImage::Pointer  thresholded_input = TInputImage::New();
  typename TInputImage::Pointer  input             = this->GetInput();
  typename TOutputImage::Pointer output            = this->GetOutput();
  InputScalarType floodLevel;
  ImageRegion<ImageDimension> expandedOutputRegion;
  Size<ImageDimension>        expandedOutputSize;
  Index<ImageDimension>       expandedOutputIndex;
  InputScalarType             minImageValue;
  InputScalarType             maxImageValue;
  
  for (int i = 0; i < ImageDimension; ++i)
    {
      expandedOutputIndex[i] = output->GetRequestedRegion().GetIndex()[i] -1;
      expandedOutputSize[i]  = output->GetRequestedRegion().GetSize()[i] + 2;
    }
  expandedOutputRegion.SetSize(expandedOutputSize);
  expandedOutputRegion.SetIndex(expandedOutputIndex);

  thresholded_input->SetLargestPossibleRegion(expandedOutputRegion);
  thresholded_input->SetRequestedRegion(output->GetRequestedRegion());
  thresholded_input->SetBufferedRegion(expandedOutputRegion);
  thresholded_input->Allocate();

  output->SetBufferedRegion(expandedOutputRegion);
  output->Allocate();

  basic_output->SetLargestPossibleRegion(output->GetLargestPossibleRegion());
  basic_output->SetRequestedRegion(output->GetRequestedRegion());
  basic_output->SetBufferedRegion(output->GetBufferedRegion());
  basic_output->Allocate();
  
  Self::FindMinMax(input, minImageValue, maxImageValue);
  floodLevel = m_Level * (maxImageValue - minImageValue);
  InputScalarType thresholdValue = minImageValue
    + ((maxImageValue - minImageValue) *  m_Threshold);

  Self::MinimumThresholdImage(
   static_cast<InputScalarType>(thresholdValue), input, thresholded_input );
  
  const ImageRegion<ImageDimension> tempRegion =
    thresholded_input->GetRequestedRegion();
  thresholded_input->SetRequestedRegion(thresholded_input->GetBufferedRegion());

  maxImageValue += NumericTraits<InputScalarType>::One;

  this->FillBorderPixels(thresholded_input, expandedOutputRegion,
                         maxImageValue);
  
  thresholded_input->SetRequestedRegion(tempRegion);
 
  Self::CreateBasicSegmentation2D(thresholded_input, basic_output,
                                  NumericTraits<OutputScalarType>::Zero);
  
  Self::CreateSegmentTable(m_BaseSegmentTable, thresholded_input,
                           basic_output,
                           NumericTraits<OutputScalarType>::Zero );

  Self::CreateMergeHeap(m_BaseSegmentTable, m_MergeHeap);
  
  if (m_MergeHeap.empty()) // Unexpected error, sanity check.
    {
      throw ExceptionObject();
    }

  Self::ExtractMergeHeirarchy(m_BaseSegmentTable, m_MergeHeap,
                              m_MergeHeirarchy, floodLevel,
                              NumericTraits<OutputScalarType>::Zero);
  
  sort_comp comp;
  std::sort(m_MergeHeirarchy.begin(), m_MergeHeirarchy.end(), comp);
 
  basic_output->SetMergeList(m_MergeHeirarchy);
  basic_output->SetMaxDepth(maxImageValue-minImageValue);

  //  Copy basic_output labeled image to output
  Self::CopyOutputToOutput(output, basic_output);
  Self::RelabelImage(output,
                     Self::ExtractEquivalencyTable(m_MergeHeirarchy,
                                                   floodLevel));
}

template <class TInputImage, class TOutputImage>
void
WatershedImageFilter<TInputImage, TOutputImage>
::FillBorderPixels(TInputImage *img, const RegionType &region,
                   OutputScalarType val)
{
  unsigned int i;
  typename SmartNeighborhoodIterator<TInputImage>::RadiusType
    unaryRadius;
  for (i=0; i < ImageDimension; ++i) unaryRadius[i] = 1;
  
  SmartNeighborhoodIterator<TInputImage> it(unaryRadius, img, region);
  it.GoToBegin();

  while ( ! it.IsAtEnd() )
    {
      if (! it.InBounds() ) it.SetCenterPixel(val);
      ++it;
    }
}

template< class TInputImage, class TOutputImage>
void
WatershedImageFilter<TInputImage, TOutputImage>
::CopyOutputToOutput(OutputImageType *output, OutputImageType *input)
{
  typedef typename TOutputImage::PixelType PixelType;
  ImageRegionIterator<TInputImage>
    in_it(input, output->GetRequestedRegion());
  ImageRegionIterator<TOutputImage>
    out_it(output, output->GetRequestedRegion());

  for ( ; !in_it.IsAtEnd(); ++in_it, ++out_it)
    {
    out_it.Set( in_it.Get() );
    }
}

template< class TInputImage, class TOutputImage>
WatershedImageFilter<TInputImage, TOutputImage>::LabelTableType
WatershedImageFilter<TInputImage, TOutputImage>
::ExtractEquivalencyTable(const MergeListType &list, const InputScalarType&
                          floodLevel)
{
  std::stack<LabelPairType> pairStack;
  LabelPairType temp;

  for (typename MergeListType::const_iterator it = list.begin();
       ((*it).Saliency <= floodLevel) &&( it != list.end()); ++it)
    {
      temp.first = (*it).FromLabel;
      temp.second = (*it).ToLabel;
      pairStack.push(temp);
    }
  return Self::MergeLabelPairs(pairStack);
}

template< class TInputImage, class TOutputImage>
void
WatershedImageFilter<TInputImage, TOutputImage>
::ExtractMergeHeirarchy(SegmentTableType& segments, MergeListType &heap,
                        MergeListType &list, const InputScalarType floodLevel,
                        const OutputScalarType UNLABELED_PIXEL)
{
  // Merges segments up to a specified floodlevel according to the information
  // in the heap of merges.  As two segments are merged, calculates any
  // new possible merges and pushes them onto the heap.
  
  merge_comp comp;
  SegmentTableType::iterator toSeg, fromSeg;
  EdgeTableType::iterator edge_it, retVal;
  SegmentTableType::data_type neighbSeg;
  typename MergeListType::value_type tempMerge;
  OutputScalarType toSegLabel, tempLabel, fromSegLabel;
  InputScalarType tempEdgeVal;
  std::stack<OutputScalarType> purgeStack;

  if (heap.empty()) return;
  
  typename MergeListType::value_type topMerge = heap.front();
  while( (! heap.empty()) && (topMerge.Saliency <= floodLevel) )
    {
      std::pop_heap(heap.begin(), heap.end(), comp);
      heap.pop_back();  // Popping the heap moves the top element to the end
                        // of the container structure, so we delete that here.

      // Recursively find the segments we are about to merge
      // (the labels identified here may have merged already)
      fromSegLabel = Self::ResolveLabel(segments,topMerge.FromLabel,
                                        UNLABELED_PIXEL);
      fromSeg = segments.find( fromSegLabel );
      toSegLabel =  Self::ResolveLabel(segments,topMerge.ToLabel,
                                       UNLABELED_PIXEL);
      toSeg = segments.find( toSegLabel );

      // If the two segments do not resolve to the same segment (already
      // merged), then merge their edge information and recalculate
      // Minimum, Minimum Edge Value, and Depth.  Segment B (fromSeg) is being
      // flooded (absorbed) by Segment A (toSeg).
      if (fromSegLabel != toSegLabel)
        {
          topMerge.FromLabel = fromSegLabel;
          topMerge.ToLabel = toSegLabel;
          list.push_back(topMerge);  // Record this merge for posterity
          
          (*fromSeg).second.MergedToLabel = toSegLabel;
          
          // Update A's edge table with that of B.  Leave out any edges between 
          // A and B.
          for (edge_it = (*fromSeg).second.EdgeTable.begin();
               edge_it != (*fromSeg).second.EdgeTable.end(); ++edge_it)
            {
              // Is this going to be a duplicate value?  If so get the minimum
              // edge value
              tempLabel = Self::ResolveLabel(segments, (*edge_it).first,
                                       UNLABELED_PIXEL);
              tempEdgeVal = (*edge_it).second;
              retVal = (*toSeg).second.EdgeTable.find(tempLabel);
              if (retVal != (*toSeg).second.EdgeTable.end())
                {
                  if ((*retVal).second < tempEdgeVal)
                    tempEdgeVal = (*retVal).second;
                }
              
              // Recursively resolve label
              tempLabel =  Self::ResolveLabel(segments, (*edge_it).first,
                                              UNLABELED_PIXEL);
              if (tempLabel != toSegLabel)
                {
                  (*toSeg).second.EdgeTable.insert(
                     std::pair<OutputScalarType,InputScalarType>( tempLabel,
                                                           tempEdgeVal)); 
                }
            }

          // Purge A's new edge table of any references to B or A (again we
          // need to look recursively).
          purgeStack.push(fromSegLabel);
          for (edge_it = (*toSeg).second.EdgeTable.begin();
               edge_it != (*toSeg).second.EdgeTable.end();
               ++edge_it)
            {
              tempLabel = Self::ResolveLabel(segments, (*edge_it).first,
                                             UNLABELED_PIXEL);
              if (tempLabel==fromSegLabel) purgeStack.push((*edge_it).first);
              else if (tempLabel==toSegLabel) purgeStack.push((*edge_it).first);
            }
          while ( ! purgeStack.empty())
            {
              (*toSeg).second.EdgeTable.erase(purgeStack.top());
              purgeStack.pop();
            }
          
          // New minimum?
          if ((*fromSeg).second.Minimum < (*toSeg).second.Minimum)
            (*toSeg).second.Minimum = (*fromSeg).second.Minimum;
          
          // Find the new A.MinimumEdgeValue
          bool flag = true;
          for (edge_it = (*toSeg).second.EdgeTable.begin();
               edge_it != (*toSeg).second.EdgeTable.end();
               ++edge_it)
            {
              if (flag) // first time
                {
                  flag = false;
                  (*toSeg).second.MinimumEdgeValue = (*edge_it).second;
                }
              else if ( (*edge_it).second < (*toSeg).second.MinimumEdgeValue)
                {
                  (*toSeg).second.MinimumEdgeValue = (*edge_it).second;
                }  
            }

          // New depth.
          (*toSeg).second.Depth = (*toSeg).second.MinimumEdgeValue -
            (*toSeg).second.Minimum;
              
          // Now check for new possible merges in A.
          for (edge_it = (*toSeg).second.EdgeTable.begin(); edge_it !=
                 (*toSeg).second.EdgeTable.end(); ++edge_it)
            {
              neighbSeg = (*(segments.find((*edge_it).first))).second;
              
              if ( ( (*toSeg).second.Depth >= neighbSeg.Depth)
                   &&
                   ( (*toSeg).second.MinimumEdgeValue == (*edge_it).second)
                   &&
                   ( (*edge_it).second == (*toSeg).second.MinimumEdgeValue)
                   )
                {
                  tempMerge.FromLabel = (*edge_it).first;
                  tempMerge.ToLabel   = toSegLabel;
                  tempMerge.Saliency  = (*toSeg).second.Depth;
                  heap.push_back(tempMerge);
                  std::push_heap(heap.begin(), heap.end(), comp);
                }
            }          
        }
      if( !heap.empty() )
        {
        topMerge = heap.front();
        }
    }
}

template< class TInputImage, class TOutputImage>
void
WatershedImageFilter<TInputImage, TOutputImage>
::CreateMergeHeap(const SegmentTableType &segments, MergeListType &mergeHeap)
{
  // Region A will flood Region B (B will merge with A) at a flood level l
  // when:
  //  1) Depth of A >= Depth of B
  //  2) Border pixel of A with B is the Minimum border pixel of B.
  //  3) Border pixel of A with B is the Minimum border pixel of A.
  //  4) l > Depth of A
  //
  SegmentTableType::data_type neighbSeg;
  typename MergeListType::value_type tempMerge;
  for (SegmentTableType::const_iterator segment_ptr = segments.begin();
       segment_ptr != segments.end(); ++segment_ptr)
    {
      for (EdgeTableType::const_iterator edge_ptr =
             (*segment_ptr).second.EdgeTable.begin(); edge_ptr !=
             (*segment_ptr).second.EdgeTable.end(); ++edge_ptr)
        {
          neighbSeg = (*segments.find((*edge_ptr).first)).second;

          if ( ((*segment_ptr).second.Depth >= neighbSeg.Depth)
               &&
               ( neighbSeg.MinimumEdgeValue == (*edge_ptr).second)
               &&
               ( (*edge_ptr).second == (*segment_ptr).second.MinimumEdgeValue)
               )
            {
              tempMerge.FromLabel = (*edge_ptr).first;
              tempMerge.ToLabel   = (*segment_ptr).first;
              tempMerge.Saliency  = (*segment_ptr).second.Depth;
              mergeHeap.push_back(tempMerge); 
            }
        }
    }

  merge_comp comp;
  std::make_heap(mergeHeap.begin(), mergeHeap.end(), comp);  
}


template< class TInputImage, class TOutputImage>
void
WatershedImageFilter<TInputImage, TOutputImage>
::CreateSegmentTable(SegmentTableType &segments, InputImageType *input,
                     OutputImageType *output, const OutputScalarType
                     UNLABELED_PIXEL) 
{
  unsigned int i;
  Size<ImageDimension> hoodRadius;
  SegmentTableType::iterator segment_ptr;
  SegmentType temp_segment;
  OutputScalarType segment_label;
  EdgeTableType::iterator edge_ptr;
  InputScalarType lowest_edge;
  
  for (i = 0; i < ImageDimension; ++i)
    {                   
      hoodRadius[i]  = 1;
    }
  ConstNeighborhoodIterator<OutputImageType>
    searchIt(hoodRadius, input, output->GetRequestedRegion());
  NeighborhoodIterator<OutputImageType>
    labelIt(hoodRadius, output, output->GetRequestedRegion());

  unsigned long hoodCenter = searchIt.Size() >> 1;
  unsigned long hoodSize   = searchIt.Size();

  for (searchIt.GoToBegin(), labelIt.GoToBegin(); ! searchIt.IsAtEnd();
       ++searchIt, ++labelIt)
    {
      segment_label = *labelIt[hoodCenter];
 
      // Find the segment corresponding to this label and update its minimum
      // value if necessary.
      segment_ptr = segments.find(segment_label);
      if (segment_ptr == segments.end())  // This segment label not found.
        {                                 // Create a new one and initialize.
          temp_segment.Minimum = *searchIt[hoodCenter];
          temp_segment.MergedToLabel = UNLABELED_PIXEL;
          segment_ptr
            = (segments.insert(std::pair<OutputScalarType,
                              SegmentType>(segment_label,
                                           temp_segment))).first;
        }
      else if (*searchIt[hoodCenter] < (*segment_ptr).second.Minimum)
        {      
          (*segment_ptr).second.Minimum = *searchIt[hoodCenter];
        }

      // Look up each neighboring segment in this segment's edge table.
      // If an edge exists, compare (and reset) the minimum edge value.
      // Note that edges are located *between* two adjacent pixels and
      // the value is taken to be the maximum of the two adjacent pixel
      // values.
      for (i =0; i < hoodSize; ++i)
        {
          if (*labelIt[i] != segment_label && *labelIt[i] != UNLABELED_PIXEL)
            {
              if (*searchIt[i] < *searchIt[hoodCenter])// We want the
                lowest_edge = *searchIt[hoodCenter];   // max of the two
              else lowest_edge = *searchIt[i];         // adjacent pixels
              
              edge_ptr = (*segment_ptr).second.EdgeTable.find(*labelIt[i]);
              if (edge_ptr == (*segment_ptr).second.EdgeTable.end())
                { // This edge has not been identified yet.
                  (*segment_ptr).second.EdgeTable.insert(
                  std::pair<OutputScalarType, InputScalarType>(
                  *labelIt[i], lowest_edge)); 
                }
              else if (lowest_edge < (*edge_ptr).second)
                {
                  (*edge_ptr).second = lowest_edge;
                }
            }
        }

    }
  // Find the depth of each segment. Depth is defined as the difference between
  // the minimum of a region and the minimum edge value.  Also record the
  // minimum edge value.

    for (segment_ptr = segments.begin(); segment_ptr != segments.end();
       ++segment_ptr)
      { 
        edge_ptr = (*segment_ptr).second.EdgeTable.begin();        
        lowest_edge = (*edge_ptr).second;

        for (++edge_ptr; edge_ptr != (*segment_ptr).second.EdgeTable.end();
             ++edge_ptr)
          {
            if ((*edge_ptr).second < lowest_edge)
              lowest_edge = (*edge_ptr).second;
          }

        (*segment_ptr).second.Depth
          = lowest_edge - (*segment_ptr).second.Minimum;

        (*segment_ptr).second.MinimumEdgeValue = lowest_edge;

      }
}
  
template< class TInputImage, class TOutputImage >  
void
WatershedImageFilter<TInputImage, TOutputImage>
::FindMinMax(InputImageType* img, InputScalarType &minImageValue,
             InputScalarType &maxImageValue)
{
  ImageRegionIterator<InputImageType>
    it(img, img->GetRequestedRegion());

  minImageValue = NumericTraits<InputScalarType>::Zero;
  maxImageValue = NumericTraits<InputScalarType>::Zero;
  for (; !it.IsAtEnd(); ++it)
    {
    if (it.Get() < minImageValue) minImageValue = it.Get();
    if (it.Get() > maxImageValue) maxImageValue = it.Get();
    }  
}

template< class TInputImage, class TOutputImage >  
void
WatershedImageFilter<TInputImage, TOutputImage>
::MinimumThresholdImage(const InputScalarType minimumValue,
                   InputImageType *in, InputImageType *out)
{
  ImageRegionIterator<InputImageType>
    it_in(in, out->GetRequestedRegion());
  ImageRegionIterator<InputImageType>
    it_out(out, out->GetRequestedRegion());

  for (; !it_in.IsAtEnd(); ++it_in, ++it_out)
    {
    if (it_in.Get() < minimumValue) it_out.Set( minimumValue );
    else it_out.Set( it_in.Get() );
    }
}


template< class TInputImage, class TOutputImage >
void
WatershedImageFilter<TInputImage, TOutputImage>
::CreateBasicSegmentation2D( InputImageType *input, OutputImageType *output,
                      const OutputScalarType UNLABELED_PIXEL)
{
  FlatRegionTableType flatRegionTable;
  Self::LabelSPMandFlatPixels(input, output, UNLABELED_PIXEL, flatRegionTable);
  Self::TraceUnlabeledPixels2D(input, output, UNLABELED_PIXEL);
  Self::ConnectPlateausWithBasins(input, output, flatRegionTable);
} 

template< class TInputImage, class TOutputImage >
void
WatershedImageFilter<TInputImage, TOutputImage>
::ConnectPlateausWithBasins( InputImageType *input, OutputImageType *output,
                             const FlatRegionTableType &flatRegionTable)
{
  // Assumes all pixels are labeled in the image.  Steps through the
  // flat regions and equates each one with the label at its
  // lowest boundary point. Flat maxima and flat basins are preserved as
  // their own regions.
  // The label equivalencies are merged and the output image is relabeled to
  // reflect these equivalencies.
  std::stack<LabelPairType> equivLabelStack;
  for (FlatRegionTableType::const_iterator region = flatRegionTable.begin();
       region != flatRegionTable.end(); ++region)
    {
      if ( ((*region).second.BoundaryMaxValue > (*region).second.RegionValue)
           && ((*region).second.BoundaryMinValue < (*region).second.RegionValue) )
        {
          equivLabelStack.push(LabelPairType((*region).first,
                              *((*region).second.BoundaryMinLabelPointer)));
        }
    }
  LabelTableType equivalentLabels  = Self::MergeLabelPairs(equivLabelStack);
  Self::RelabelImage(output, equivalentLabels);
}

template< class TInputImage, class TOutputImage >
void
WatershedImageFilter<TInputImage, TOutputImage>
::TraceUnlabeledPixels2D( InputImageType *input, OutputImageType *output,
                      const OutputScalarType UNLABELED_PIXEL)
{
  OutputScalarType newLabel;
  newLabel = UNLABELED_PIXEL;
  Offset<ImageDimension> moveIndex;
  Size<ImageDimension> hoodRadius;
  Size<ImageDimension> zeroRadius;
  InputScalarType minVal;
  for (int i = 0; i < ImageDimension; ++i)
    {                   
      hoodRadius[i]  = 1; // Radius of 1 gives the 8-neighbors in 2D case
      zeroRadius[i] = 0;
    }

  ConstRandomAccessNeighborhoodIterator<InputImageType>
    valueIt(hoodRadius, input, output->GetRequestedRegion());
  RandomAccessNeighborhoodIterator<OutputImageType>
    labelIt(zeroRadius, output, output->GetRequestedRegion());
  
  ImageRegionIterator<OutputImageType>
    it(output, output->GetRequestedRegion());

  std::stack< OutputScalarType * > updateStack;

  unsigned int hoodSize   = valueIt.Size();
  unsigned int hoodCenter = hoodSize >> 1;

  // Set up a table of directional movement indicies for each 8-neighbor
  // index.  This could be generalized for an N-d algorithm or arbitrary
  // connectedness schemes.
  // 0 1 2
  // 3 4 5
  // 6 7 8
  Offset<ImageDimension> mT[9];
  mT[0][0] = -1;  mT[0][1] = -1;
  mT[1][0] =  0;  mT[1][1] = -1;
  mT[2][0] =  1;  mT[2][1] = -1;
  mT[3][0] = -1;  mT[3][1] =  0;
  mT[4][0] =  0;  mT[4][1] =  0;
  mT[5][0] =  1;  mT[5][1] =  0;
  mT[6][0] = -1;  mT[6][1] =  1;
  mT[7][0] =  0;  mT[7][1] =  1;
  mT[8][0] =  1;  mT[8][1] =  1;

  // Search the image and trace the unlabeled pixels to a labeled region.
  for (; !it.IsAtEnd(); ++it)
    {
      if (it.Get() == UNLABELED_PIXEL) // Not part of a flat region or single
        {                         // pixel minimum.
          valueIt.SetLocation(it.GetIndex());
          labelIt.SetLocation(it.GetIndex());
          newLabel = UNLABELED_PIXEL;          // Follow the path of steep-
          while( newLabel == UNLABELED_PIXEL ) // est descent until a label
            {                                  // is found.
              updateStack.push(labelIt.GetCenterPointer());
              minVal = *valueIt[0];
              moveIndex = mT[0];
              for (unsigned int i= 1; i < hoodSize; ++i)
                {
                  if ( (i!=hoodCenter) && (*valueIt[i] < minVal) )
                    {
                      minVal = *valueIt[i];
                      moveIndex = mT[i];
                    }
                }
              valueIt += moveIndex;
              labelIt += moveIndex;
              newLabel = *(labelIt.GetCenterPointer());
            }
        }

      while( ! updateStack.empty() ) // Update all the pixels we've traversed
        {
          *(updateStack.top()) = newLabel;
          updateStack.pop();
        }
    }
}

template< class TInputImage, class TOutputImage >
void
WatershedImageFilter<TInputImage, TOutputImage>
::LabelSPMandFlatPixels( InputImageType *input,
         OutputImageType *output,
         const OutputScalarType UNLABELED_PIXEL,
         FlatRegionTableType &flatRegionTable)
{
  OutputScalarType labelCounter = NumericTraits<OutputScalarType>::One;
  
  register unsigned int i;
  bool foundSinglePixelMinimum;
  bool foundFlatRegion;
  FlatRegion tempFlatRegion;
  InputScalarType currentValue;
  std::stack<LabelPairType> equivalentLabelStack;
  
  // Set up "square" neighborhood iterator. No bounds checking. Either
  // our image is set up properly with an appropriate sized buffer and
  // boundary, or our iterator must take care of this for us.
  Size<ImageDimension> hoodRadius;
  for (int i = 0; i < ImageDimension; ++i)
    {                   
      hoodRadius[i]  = 1; // Radius of 1 gives the 8-neighbors in 2D case
    }                   
  ConstNeighborhoodIterator<InputImageType>
    searchIt ( hoodRadius, input, input->GetRequestedRegion());
  NeighborhoodIterator<OutputImageType>
    labelIt ( hoodRadius, output, input->GetRequestedRegion());
  
  unsigned int hoodSize   = searchIt.Size();
  unsigned int hoodCenter = hoodSize >> 1;
  
  for (searchIt.GoToBegin(), labelIt.GoToBegin(); ! searchIt.IsAtEnd();
       ++searchIt, ++labelIt)
    {
      foundSinglePixelMinimum = true;
      foundFlatRegion = false;

      // Compare current pixel value with its neighbors.
      currentValue = *searchIt[hoodCenter];
      for (i = 0; i < hoodSize; ++i)
        {
          if (i != hoodCenter)
            {
              foundSinglePixelMinimum
                = foundSinglePixelMinimum && (currentValue < *searchIt[i]);
              if ( currentValue == *searchIt[i] )
                {
                  foundFlatRegion  = true;
                  break;
                }
            }
        }
      if (foundFlatRegion)
        {
          if ( *labelIt[i] != UNLABELED_PIXEL )// If the flat region is already
            {                                  // labeled, label this to match.
              *labelIt[hoodCenter] = *labelIt[i];
            }
          else // Add a new flat region to the table. Initialize its
            {  // max, min external boundary values with the first one we see.
              *labelIt[hoodCenter] = labelCounter;
              tempFlatRegion.BoundaryMaxValue        = *searchIt[0];
              tempFlatRegion.BoundaryMinValue        = *searchIt[0];
              tempFlatRegion.BoundaryMinLabelPointer = labelIt[0];
              tempFlatRegion.RegionValue             = currentValue;
              flatRegionTable[labelCounter]  = tempFlatRegion;
              labelCounter
                = labelCounter + NumericTraits<OutputScalarType>::One;
            }
          // While we're at it, check to see if we have just linked two flat
          // regions with the same height value.  Save that info for later.
          for (++i; i <hoodSize; ++i)
            {
              if (   currentValue==*searchIt[i]
                     && *labelIt[i]!=UNLABELED_PIXEL
                     && *labelIt[i]!=*labelIt[hoodCenter]
                     )
                equivalentLabelStack.push
                  ( std::pair<OutputScalarType,OutputScalarType>
                    (*labelIt[hoodCenter], *labelIt[i]) );
            }
          
          // Update the min and max boundary pixel for this flat region.
          tempFlatRegion = flatRegionTable[*labelIt[hoodCenter]];
          for (i = 0; i < hoodSize; ++i)
            {
              if (i!=hoodCenter)
                {
                  if  (*searchIt[i] < tempFlatRegion.BoundaryMinValue)
                    {
                      tempFlatRegion.BoundaryMinValue = *searchIt[i];
                      tempFlatRegion.BoundaryMinLabelPointer = labelIt[i];
                    }
                  else if (*searchIt[i] > tempFlatRegion.BoundaryMaxValue)
                    {
                      tempFlatRegion.BoundaryMaxValue = *searchIt[i];
                    }
                }
            }
          flatRegionTable[*labelIt[hoodCenter]] = tempFlatRegion;
        }
      else if (foundSinglePixelMinimum)
        {
          *(labelIt.GetCenterPointer()) = labelCounter;
          labelCounter = labelCounter + NumericTraits<OutputScalarType>::One;
        } 
    }
  
  // Merge identical flat regions and relabel the image.  This step is
  // necessary because the image was labeled in a single pass from top
  // to bottom and some flat regions may not have been linked.
  
  LabelTableType equivalentLabels
    =  Self::MergeLabelPairs(equivalentLabelStack);
  Self::MergeFlatRegions(equivalentLabels, flatRegionTable);
  Self::RelabelImage(output, equivalentLabels);
}

template< class TInputImage, class TOutputImage >
void
WatershedImageFilter<TInputImage, TOutputImage>
::RelabelImage(OutputImageType *img, const LabelTableType &map)
{
  LabelTableType::const_iterator temp;
  ImageRegionIterator<OutputImageType>
    it(img, img->GetRequestedRegion());
  for ( ; !it.IsAtEnd(); ++it)
    {
    temp = map.find(it.Get());
    if (temp != map.end()) { it.Set( (*temp).second ); }
    }
  
}

template< class TInputImage, class TOutputImage >
WatershedImageFilter<TInputImage, TOutputImage>::LabelTableType
WatershedImageFilter<TInputImage, TOutputImage>
::MergeLabelPairs( std::stack<LabelPairType> s)
{
  LabelTableType labels;
  OutputScalarType first, second, temp;
  std::pair<LabelTableType::iterator, bool> result;
  LabelTableType::iterator recurse;

  while ( ! s.empty() )
    {
      if (s.top().first > s.top().second)  // Make sure first > second
        {
          first = s.top().first;
          second = s.top().second;
        }
      else
        {
          first = s.top().second;
          second = s.top().first;
        }
      s.pop();

      // Find dependencies among equivalent labels.
      // Is this key in the table?  If it is and matches a different label,
      // put that new correspondence on the stack for processing.
      result = labels.insert(LabelPairType(first, second));
      if ( (result.second == false) && ( (*(result.first)).second != second) )
        {
          temp = (*(result.first)).second;
          s.push(LabelPairType(second, (*(result.first)).second));
        }
    }

  // Now recursively resolve dependencies among labels.  We must
  // make sure that every key maps to a value that is not itself
  // a key in the table.
  for (LabelTableType::iterator it = labels.begin(); it != labels.end();
       ++it)
    {
      recurse = labels.find((*it).second);
      if (recurse != labels.end() )
        {
          while (recurse != labels.end())
            {
              temp = (*recurse).second;
              recurse = labels.find(temp);
            }
          (*it).second = temp;
        }
    }
  return labels;
}


template< class TInputImage, class TOutputImage >
void
WatershedImageFilter<TInputImage, TOutputImage>
::MergeFlatRegions(const LabelTableType &labels, FlatRegionTableType &regions)
{
  // Note that the labels must have no interdependencies.  That is,
  // every key must map to a value that is not itself a key in the
  // table. This means that you must always merge label->first with
  // label->second (a to b).
  FlatRegionTableType::iterator a, b;
  for (LabelTableType::const_iterator it = labels.begin();
       it != labels.end(); ++it)
    {
      if ( ((a = regions.find((*it).first)) == regions.end())
           || ((b = regions.find((*it).second)) == regions.end()) )
        throw ExceptionObject();
      if ((*a).second.BoundaryMinValue < (*b).second.BoundaryMinValue)
        {
          (*b).second.BoundaryMinValue = (*a).second.BoundaryMinValue;
          (*b).second.BoundaryMinLabelPointer
            = (*a).second.BoundaryMinLabelPointer;
        }
      if ((*b).second.BoundaryMaxValue < (*a).second.BoundaryMaxValue)
        {
          (*b).second.BoundaryMaxValue = (*a).second.BoundaryMaxValue;
        }
      regions.erase(a);
    }
}


} // end namespace itk

#endif
