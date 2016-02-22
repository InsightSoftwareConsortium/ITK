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
#ifndef itkAttributeMorphologyBaseImageFilter_hxx
#define itkAttributeMorphologyBaseImageFilter_hxx

#include "itkAttributeMorphologyBaseImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkNumericTraits.h"
#include "itkConnectedComponentAlgorithm.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkCastImageFilter.h"

/*
 * This code was contributed in the Insight Journal paper
 *
 * "Grayscale morphological attribute operations"
 * by Beare R.
 * https://hdl.handle.net/1926/1316
 * http://www.insight-journal.org/browse/publication/203
 *
 */

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TAttribute, typename TFunction >
void
AttributeMorphologyBaseImageFilter< TInputImage, TOutputImage, TAttribute, TFunction >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast< InputImageType * >( this->GetInput() );

  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}

template< typename TInputImage, typename TOutputImage, typename TAttribute, typename TFunction >
void
AttributeMorphologyBaseImageFilter< TInputImage, TOutputImage, TAttribute, TFunction >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< typename TInputImage, typename TOutputImage, typename TAttribute, typename TFunction >
void
AttributeMorphologyBaseImageFilter< TInputImage, TOutputImage, TAttribute, TFunction >
::GenerateData()
{
  if( m_Lambda <= 0.0 )
    {
    // save some time - simply copy the input in the output
    typedef CastImageFilter< TInputImage, TOutputImage> CastType;
    typename CastType::Pointer cast = CastType::New();
    cast->SetInput( this->GetInput() );
    cast->SetNumberOfThreads( this->GetNumberOfThreads() );
    cast->SetInPlace( false );
    cast->GraftOutput( this->GetOutput() );
    cast->Update();
    this->GraftOutput( cast->GetOutput() );
    return;
    }

  // the real stuff, for useful lambda values
  typename TOutputImage::Pointer output = this->GetOutput();
  typename TInputImage::ConstPointer input = this->GetInput();
  // Allocate the output
  this->AllocateOutputs();

  TFunction compare;

  SizeValueType buffsize = output->GetRequestedRegion().GetNumberOfPixels();

  SizeType kernelRadius;
  kernelRadius.Fill(1);
  typedef itk::NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage >
  FaceCalculatorType;
  FaceCalculatorType faceCalculator;
  typename FaceCalculatorType::FaceListType faceList;
  faceList = faceCalculator(input, output->GetRequestedRegion(), kernelRadius);
  typename FaceCalculatorType::FaceListType::iterator fit;
  ProgressReporter progress(this, 0, buffsize * 4);  // pretend we have 4 steps

  fit = faceList.begin();

  m_SortPixels = new GreyAndPos[buffsize];
  m_Parent = new OffsetValueType[buffsize];
#ifndef PAMI
  m_Processed = new bool[buffsize];
#endif
  // This is a bit ugly, but I can't see an easy way around
  m_Raw = new InputPixelType[buffsize];
  m_AuxData = new AttributeType[buffsize];

  // copy the pixels to the sort buffer
  typedef ImageRegionConstIteratorWithIndex< TInputImage > CRegionIteratorType;
  CRegionIteratorType RegIt( input, output->GetRequestedRegion() );
  //IndexType Origin = RegIt.GetIndex();
  OffsetValueType pos = 0;

  for ( RegIt.GoToBegin(); !RegIt.IsAtEnd(); ++RegIt, ++pos )
    {
    GreyAndPos P;
    P.Val = RegIt.Get();
    P.Pos = pos;
    m_SortPixels[pos] = P;
    m_Raw[pos] = P.Val;
#ifndef PAMI
    m_Processed[pos] = false;
#endif
    m_Parent[pos] = INACTIVE;
    m_AuxData[pos] = -1;  // invalid value;
    progress.CompletedPixel();
    }
  progress.CompletedPixel();
  std::sort( &( m_SortPixels[0] ), &( m_SortPixels[buffsize - 1] ), ComparePixStruct() );
  progress.CompletedPixel();

  // set up the offset vector
  OffsetVecType       TheseOffsets;
  OffsetDirectVecType TheseDirectOffsets;
  SetupOffsetVec(TheseDirectOffsets, TheseOffsets);

  // the core algorithm
  // process first pixel
#ifdef PAMI
  MakeSet(m_SortPixels[0].Pos);
  //m_Processed[0] = true;
  for ( SizeValueType k = 1; k < buffsize; k++ )
    {
    OffsetValueType   ThisPos = m_SortPixels[k].Pos;
    IndexType         ThisWhere = input->ComputeIndex(ThisPos);
    InputPixelType    ThisPix = m_SortPixels[k].Val;
    MakeSet(ThisPos);
    // Some optimization of bounds check
    if ( fit->IsInside(ThisWhere) )
      {
      // no need for bounds check on neighbours
      for ( unsigned i = 0; i < TheseDirectOffsets.size(); i++ )
        {
        OffsetValueType           NeighInd = ThisPos + TheseDirectOffsets[i];
        InputPixelType NeighPix = m_Raw[NeighInd];
        if ( compare(NeighPix, ThisPix) || ( ( ThisPix == NeighPix ) && ( NeighInd < ThisPos ) ) )
          {
          Union(NeighInd, ThisPos);
          }
        }
      }
    else
      {
      // need a bounds check for each neighbour
      for ( unsigned i = 0; i < TheseOffsets.size(); i++ )
        {
        if ( output->GetRequestedRegion().IsInside(ThisWhere + TheseOffsets[i]) )
          {
          OffsetValueType           NeighInd = ThisPos + TheseDirectOffsets[i];
          InputPixelType NeighPix = m_Raw[NeighInd];
          if ( compare(NeighPix, ThisPix) || ( ( ThisPix == NeighPix ) && ( NeighInd < ThisPos ) ) )
            {
            Union(NeighInd, ThisPos);
            }
          }
        }
      }
    progress.CompletedPixel();
    }
#else
  MakeSet(m_SortPixels[0].Pos);
  m_Processed[0] = true;
  for ( SizeValueType k = 1; k < buffsize; k++ )
    {
    OffsetValueType ThisPos = m_SortPixels[k].Pos;
    OffsetValueType PrevPos = m_SortPixels[k - 1].Pos;
    InputPixelType ThisPix = m_Raw[ThisPos];
    InputPixelType PrevPix = m_Raw[PrevPos];
    IndexType ThisWhere = input->ComputeIndex(ThisPos);
    if ( ThisPix != PrevPix )
      {
      for ( OffsetValueType QPos = k - 1; QPos >= 0; --QPos )
        {
        OffsetValueType QLoc = m_SortPixels[QPos].Pos;
        if ( m_Raw[QLoc] != PrevPix )
          {
          break;
          }
        if ( ( m_Parent[QLoc] == ACTIVE )
             && ( m_AuxData[QLoc] >= m_Lambda ) )
          {
          m_Parent[QLoc] = INACTIVE;
          m_AuxData[QLoc] = -1;
          // dispose auxdata[QLoc]
          }
        }
      }
    MakeSet(ThisPos);
    if ( fit->IsInside(ThisWhere) )
      {
      // no need for neighbor bounds check
      for ( unsigned i = 0; i < TheseDirectOffsets.size(); i++ )
        {
        OffsetValueType NeighInd = ThisPos + TheseDirectOffsets[i];
        if ( m_Processed[NeighInd] )
          {
          Union(NeighInd, ThisPos);
          }
        }
      }
    else
      {
      for ( unsigned i = 0; i < TheseOffsets.size(); i++ )
        {
        if ( output->GetRequestedRegion().IsInside(ThisWhere + TheseOffsets[i]) )
          {
          OffsetValueType NeighInd = ThisPos + TheseDirectOffsets[i];
          if ( m_Processed[NeighInd] )
            {
            Union(NeighInd, ThisPos);
            }
          }
        }
      }
    m_Processed[ThisPos] = true;
    progress.CompletedPixel();
    }

#endif

  // resolving phase
  // copy pixels back
  typedef ImageRegionIterator< TOutputImage > RegionIteratorType;
  RegionIteratorType ORegIt( output, output->GetRequestedRegion() );
  ORegIt.GoToBegin();

  // fill Raw - worry about iteration details later.
  // We aren't filling m_Parent, as suggested in the paper, because it
  // is an integer array. We want this to work with float types
#ifdef PAMI
  // write the new image to Raw - note that we aren't putting the
  // result in parent
  for ( pos = buffsize - 1; pos >= 0; --pos )
    {
    OffsetValueType RPos = m_SortPixels[pos].Pos;
    if ( m_Parent[RPos] >= 0 )
      {
      m_Raw[RPos] = m_Raw[m_Parent[RPos]];
      }
    progress.CompletedPixel();
    }
  for ( SizeValueType ppos = 0; ppos < buffsize; ++ppos, ++ORegIt )
    {
    ORegIt.Set( static_cast< OutputPixelType >( m_Raw[ppos] ) );
    progress.CompletedPixel();
    }

#else
  // the version from the paper
  for ( pos = buffsize - 1; pos >= 0; --pos )
    {
    OffsetValueType RPos = m_SortPixels[pos].Pos;
    if ( m_Parent[RPos] < 0 )
      {
      m_Parent[RPos] = (OffsetValueType)m_Raw[RPos];
      }
    else
      {
      m_Parent[RPos] = m_Parent[m_Parent[RPos]];
      }
    progress.CompletedPixel();
    }
  for ( pos = 0; pos < buffsize; ++pos, ++ORegIt )
    {
    ORegIt.Set( static_cast< OutputPixelType >( m_Parent[pos] ) );
    progress.CompletedPixel();
    }
#endif

  delete[] m_Raw;
  delete[] m_SortPixels;
  delete[] m_Parent;
#ifndef PAMI
  delete[] m_Processed;
#endif
  delete[] m_AuxData;
}

template< typename TInputImage, typename TOutputImage, typename TAttribute, typename TFunction >
void
AttributeMorphologyBaseImageFilter< TInputImage, TOutputImage, TAttribute, TFunction >
::SetupOffsetVec(OffsetDirectVecType & PosOffsets, OffsetVecType & Offsets)
{
  typedef ConstShapedNeighborhoodIterator< TOutputImage > NeighType;
  SizeType KernRad;
  KernRad.Fill(1);
  NeighType It( KernRad,
                this->GetOutput(), this->GetOutput()->GetRequestedRegion() );
  setConnectivity(&It, m_FullyConnected);
  typename NeighType::IndexListType OffsetList;
  typename NeighType::IndexListType::const_iterator LIt;

  OffsetList = It.GetActiveIndexList();
  IndexType idx = this->GetOutput()->GetRequestedRegion().GetIndex();
  OffsetValueType offset = this->GetOutput()->ComputeOffset(idx);

  for ( LIt = OffsetList.begin(); LIt != OffsetList.end(); LIt++ )
    {
    OffsetType O = It.GetOffset(*LIt);
    PosOffsets.push_back(this->GetOutput()->ComputeOffset(idx + O) - offset);
    Offsets.push_back(O);
    }
}

template< typename TInputImage, typename TOutputImage, typename TAttribute, typename TFunction >
void
AttributeMorphologyBaseImageFilter< TInputImage, TOutputImage, TAttribute, TFunction >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
  os << indent << "Lambda: "  << static_cast< typename NumericTraits< AttributeType >::PrintType >( m_Lambda )
     << std::endl;
}
} // end namespace itk

#endif
