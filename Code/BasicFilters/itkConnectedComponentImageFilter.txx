/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConnectedComponentImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkConnectedComponentImageFilter_txx
#define _itkConnectedComponentImageFilter_txx

#include "itkConnectedComponentImageFilter.h"
#include "itkNumericTraits.h"

// don't think we need the indexed version as we only compute the
// index at the start of each run, but there isn't a choice
#include "itkImageLinearConstIteratorWithIndex.h"  
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
#include "itkMaskImageFilter.h"

namespace itk
{
template< class TInputImage, class TOutputImage, class TMaskImage >
void
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
  if( !input )
    {
    return;
    }
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );

  MaskImagePointer mask = const_cast<MaskImageType *>(this->GetMaskImage());
  if (mask)
    {
    mask->SetRequestedRegion( input->GetLargestPossibleRegion() );
    }
}

template< class TInputImage, class TOutputImage, class TMaskImage >
void 
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template< class TInputImage, class TOutputImage, class TMaskImage >
void
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage>
::GenerateData()
{
  // create a line iterator
  typedef itk::ImageLinearConstIteratorWithIndex<InputImageType> InputLineIteratorType;

  typename TOutputImage::Pointer output = this->GetOutput();
  typename TInputImage::ConstPointer input = this->GetInput();
  typename TMaskImage::ConstPointer mask = this->GetMaskImage();

  typedef MaskImageFilter< TInputImage, TMaskImage, TInputImage > MaskFilterType;
  typename MaskFilterType::Pointer maskFilter = MaskFilterType::New();
  if( mask )
    {
    maskFilter->SetInput( input );
    maskFilter->SetInput2( mask );
    maskFilter->Update();
    input = maskFilter->GetOutput();
    }

  long LineIdx = 0;
  InputLineIteratorType inLineIt(input, output->GetRequestedRegion());
  inLineIt.SetDirection(0);
  LineMapType LineMap;
  // Allocate the output
  this->AllocateOutputs();

  long int lab = NumericTraits<long int>::Zero;
  OffsetVec LineOffsets;

  // set the progress reporter to deal with the number of lines
  long pixelcount = this->GetOutput()->GetRequestedRegion().GetNumberOfPixels();
  long xsize = this->GetOutput()->GetRequestedRegion().GetSize()[0];
  long linecount = pixelcount/xsize;
  ProgressReporter progress(this, 0, linecount/2);

  SetupLineOffsets(LineOffsets);

  for (inLineIt.GoToBegin(); ! inLineIt.IsAtEnd(); inLineIt.NextLine(), ++LineIdx)
    {
    inLineIt.GoToBeginOfLine();
    lineEncoding ThisLine;
    while (! inLineIt.IsAtEndOfLine())
      {
      InputPixelType PVal = inLineIt.Get();
      //std::cout << inLineIt.GetIndex() << std::endl;
      if (PVal != NumericTraits<InputPixelType>::Zero)
        {
        // We've hit the start of a run
        runLength thisRun;
        long length=0;
        typename InputImageType::IndexType thisIndex;
        ++lab;
        thisIndex = inLineIt.GetIndex();
        //std::cout << thisIndex << std::endl;
        while ((PVal != NumericTraits<InputPixelType>::Zero) &&
               (! inLineIt.IsAtEndOfLine()))
          {
          ++length;
          ++inLineIt;
          PVal = inLineIt.Get();
          }
        // create the run length object to go in the vector
        thisRun.length=length;
        thisRun.label=lab;
        thisRun.where = thisIndex;
        ThisLine.push_back(thisRun);
        //std::cout << thisIndex[0] << " " << thisIndex[1] << " " << length << std::endl;
        }
      else 
        {
        ++inLineIt;
        }
      }
    if (ThisLine.size() != 0)
      {
      // There are some runs on this line, so insert it into the map
      LineMap[LineIdx] = ThisLine;
      }
    progress.CompletedPixel();
    }
  
  // set up the union find structure
  InitUnion(lab);
  // insert all the labels into the structure -- an extra loop but
  // saves complicating the ones that come later
  for (long pp = 1; pp <= lab; pp++)
    {
    InsertSet(pp);
    }
  // now process the map and make appropriate entries in an equivalence
  // table
  

  typename LineMapType::iterator MapBegin, MapEnd, LineIt;

  MapBegin = LineMap.begin();
  MapEnd = LineMap.end(); 
  LineIt = MapBegin;

  //while( LineIt != MapEnd)
  for (LineIt = MapBegin; LineIt != MapEnd; ++LineIt)
    {
    //lineEncoding L = LineIt->second;
    long ThisIdx = LineIt->first;
    //std::cout << "Line number = " << LineIt->first << std::endl;
    for (OffsetVec::const_iterator I = LineOffsets.begin();
         I != LineOffsets.end(); ++I)
      {
      long NeighIdx = ThisIdx + (*I);
      // check if the neighbor is in the map
      typename LineMapType::const_iterator NN = LineMap.find(NeighIdx);
      if (NN != MapEnd) 
        {
        // Compare the two lines
        CompareLines(LineIt->second, NN->second);
        }
      }
    }
  
  unsigned long int totalLabs = CreateConsecutive();
  m_ObjectCount = totalLabs;
  // check for overflow exception here
  if (totalLabs > static_cast<long int>(NumericTraits<OutputPixelType>::max())) 
    {
    itkExceptionMacro( << "Number of objects greater than maximum of output pixel type " );
    }
  // create the output
  // A more complex version that is intended to minimize the number of
  // visits to the output image which should improve cache
  // performance on large images. We also want to optimize the
  // performance of the map by being able to iterate through it,
  // rather than do lots of look ups. Don't know whether that will
  // make much difference in practice.
  // Note - this is unnecessary if AllocateOutputs initalizes to zero

  FillOutput(LineMap, progress);

}


template< class TInputImage, class TOutputImage, class TMaskImage >
void
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage>
::SetupLineOffsets(OffsetVec &LineOffsets)
{
  // Create a neighborhood so that we can generate a table of offsets
  // to "previous" line indexes
  // We are going to mis-use the neighborhood iterators to compute the
  // offset for us. All this messing around produces an array of
  // offsets that will be used to index the map
  typename TOutputImage::Pointer output = this->GetOutput();
  typedef Image<long, TOutputImage::ImageDimension - 1> PretendImageType;
  typedef typename PretendImageType::RegionType::SizeType PretendSizeType;
  typedef ConstShapedNeighborhoodIterator<PretendImageType> LineNeighborhoodType;

  typename PretendImageType::Pointer fakeImage;
  fakeImage = PretendImageType::New();

  typename PretendImageType::RegionType LineRegion;
  //LineRegion = PretendImageType::RegionType::New();

  OutSizeType OutSize;
  OutSize = output->GetRequestedRegion().GetSize();

  PretendSizeType PretendSize;
  PretendSize = fakeImage->GetRequestedRegion().GetSize();
  // The first dimension has been collapsed
  for (unsigned int i = 0; i<PretendSize.GetSizeDimension(); i++)
    {
    PretendSize[i] = OutSize[i+1];
    }

  LineRegion.SetSize(PretendSize);
  PretendSizeType kernelRadius;
  kernelRadius.Fill(1);
  LineNeighborhoodType lnit(kernelRadius, fakeImage, LineRegion);

  // only activate the indices that are "previous" to the current
  // pixel and face connected (exclude the center pixel from the
  // neighborhood)
  //
  unsigned int d;
  typename LineNeighborhoodType::OffsetType offset;

  if (!m_FullyConnected)
    {
    // only activate the "previous" neighbors that are face connected
    // to the current pixel. do not include the center pixel
    offset.Fill(0);
    for (d=0; d < PretendImageType::ImageDimension; ++d)
      {
      offset[d] = -1;
      lnit.ActivateOffset(offset);
      offset[d] = 0;
      }
    }
  else
    {
    // activate all "previous" neighbors that are face+edge+vertex
    // connected to the current pixel. do not include the center pixel
    unsigned int centerIndex = lnit.GetCenterNeighborhoodIndex();
    for (d=0; d < centerIndex; d++)
      {
      offset = lnit.GetOffset(d);
      lnit.ActivateOffset(offset);
      }
    }

  typename LineNeighborhoodType::IndexListType ActiveIndexes;
  ActiveIndexes = lnit.GetActiveIndexList();

  typedef std::vector<long> OffsetVec;

  typename LineNeighborhoodType::IndexListType::const_iterator LI;
  
  PretendSizeType SizeBuff;
  SizeBuff[0] = 1;
  // should do this with a copy
  for (int i =0; i < PretendImageType::ImageDimension - 1; i++)
    {
    SizeBuff[i+1] = PretendSize[i];
    }
  unsigned int pos = 0;
  for (LI=ActiveIndexes.begin(); LI != ActiveIndexes.end(); LI++, pos++)
    {
    unsigned int idx = *LI;
    offset = lnit.GetOffset(idx);
    //std::cout << offset << std::endl;
    int vv = 0;
    for (int J = 0; J < PretendImageType::ImageDimension;J++)
      {
      vv += offset[J] * SizeBuff[J];
      }
    //std::cout << vv << std::endl;
    LineOffsets.push_back( vv);
    }
  
  // LineOffsets is the thing we wanted.
}
template< class TInputImage, class TOutputImage, class TMaskImage >
void
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage>
::CompareLines(lineEncoding &current, const lineEncoding &Neighbour)
{
  long offset = 0;
  if (m_FullyConnected)
    offset = 1;

  typename lineEncoding::const_iterator nIt, mIt;
  typename lineEncoding::iterator cIt;

  mIt = Neighbour.begin(); // out marker iterator

  for (cIt = current.begin();cIt != current.end();++cIt)
    {
    //runLength cL = *cIt;
    long cStart = cIt->where[0];  // the start x position
    long cLast = cStart + cIt->length - 1;

    for (nIt=mIt; nIt != Neighbour.end(); ++nIt)
      {
      //runLength nL = *nIt;
      long nStart = nIt->where[0];
      long nLast = nStart + nIt->length - 1;
      // there are a few ways that neighbouring lines might overlap
      //   neighbor      S                  E
      //   current    S                        E
      //------------------------------------------
      //   neighbor      S                  E
      //   current    S                E
      //------------------------------------------
      //   neighbor      S                  E
      //   current             S                  E
      //------------------------------------------
      //   neighbor      S                  E
      //   current             S       E
      //------------------------------------------
      long ss1 = nStart - offset;
      // long ss2 = nStart + offset;
      long ee1 = nLast - offset;
      long ee2 = nLast + offset;
      bool eq = false;
      // the logic here can probably be improved a lot
      if ((ss1 >= cStart) && (ee2 <= cLast))
        {
        // case 1
        eq = true;
        } 
      else 
        {
        if ((ss1 <= cLast) && (ee2 >= cLast))
          {
          // case 2
          eq = true;
          }
        else 
          {
          if ((ss1 <= cStart) && (ee2 >= cStart))
            {
            // case 3 
            eq = true;
            }                                        
          else 
            {
            if ((ss1 <= cStart) && (ee2 >= cLast))
              {
              // case 4
              eq = true;
              }
            }
          }
        }
      if (eq) 
        {
        LinkLabels(nIt->label, cIt->label);
        } 

      if (ee1 >= cLast)
        {
        // No point looking for more overlaps with the current run
        // because the neighbor run is either case 2 or 4
        mIt = nIt;
        break;
        }
      }
    }

}

template< class TInputImage, class TOutputImage, class TMaskImage >
void
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage>
::FillOutput(const LineMapType &LineMap,
             ProgressReporter &progress)
{

  typename LineMapType::const_iterator MapBegin, MapEnd, LineIt;
  typename TOutputImage::Pointer output = this->GetOutput();
  MapBegin = LineMap.begin();
  MapEnd = LineMap.end(); 
  LineIt = MapBegin;

  ImageRegionIterator<OutputImageType> oit(output,
                                           output->GetRequestedRegion());

  ImageRegionIterator<OutputImageType> fstart=oit, fend=oit;
  fstart.GoToBegin();
  fend.GoToEnd();

  for (LineIt = MapBegin; LineIt != MapEnd; ++LineIt)
    {
    // now fill the labelled sections
    typename lineEncoding::const_iterator cIt;

    //std::cout << LineIt->first << std::endl;

    for (cIt = LineIt->second.begin();cIt != LineIt->second.end();++cIt)
      {
      unsigned long Ilab = LookupSet( cIt->label);
      OutputPixelType lab = m_Consecutive[Ilab];
      oit.SetIndex(cIt->where);
      // initialize the non labelled pixels
      for (; fstart != oit; ++fstart)
        {
        fstart.Set(NumericTraits<OutputPixelType>::Zero );
        }
      for (long i = 0; i < cIt->length; ++i, ++oit)
        {
        oit.Set(lab);
        }
      fstart = oit;
      //++fstart;
      }
    progress.CompletedPixel();
    }
  // fill the rest of the image with zeros
  for (; fstart != fend; ++fstart)
    {
    fstart.Set(NumericTraits<OutputPixelType>::Zero );
    }

}

// union find related functions
template< class TInputImage, class TOutputImage, class TMaskImage >
void
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage>
::InsertSet(const unsigned long int label)
{
  m_UnionFind[label]=label;
}

template< class TInputImage, class TOutputImage, class TMaskImage >
unsigned long int
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage>
::CreateConsecutive()
{
  m_Consecutive = UnionFindType(m_UnionFind.size());
  m_Consecutive[0] = 0;
  unsigned long int CLab = 0;
  for (unsigned long int I = 1; I < m_UnionFind.size(); I++)
    {
    unsigned long int L = m_UnionFind[I];
    if (L == I) 
      {
      ++CLab;
      m_Consecutive[L] = CLab;
      }
    }
  return(CLab);
}

template< class TInputImage, class TOutputImage, class TMaskImage >
unsigned long int
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage>
::LookupSet(const unsigned long int label)
{
  // recursively set the equivalence if necessary
  if (label != m_UnionFind[label])
    {
    m_UnionFind[label] = this->LookupSet(m_UnionFind[label]);
    }
  return(m_UnionFind[label]);
}

template< class TInputImage, class TOutputImage, class TMaskImage >
void
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage>
::LinkLabels(const unsigned long int lab1, const unsigned long int lab2)
{
  unsigned long E1 = this->LookupSet(lab1);
  unsigned long E2 = this->LookupSet(lab2);

  if (E1 < E2)
    {
    m_UnionFind[E2] = E1;
    }
  else
    {
    m_UnionFind[E1] = E2;
    }

}

template< class TInputImage, class TOutputImage, class TMaskImage >
void
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
  os << indent << "ObjectCount: "  << m_ObjectCount << std::endl;
}

} // end namespace itk

#endif
