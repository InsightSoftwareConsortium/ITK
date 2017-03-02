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
#ifndef itkWatershedSegmenter_hxx
#define itkWatershedSegmenter_hxx

#include "itkMath.h"
#include "itkWatershedSegmenter.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkImageRegionIterator.h"
#include <stack>
#include <list>

namespace itk
{
namespace watershed
{

/*
  ----------------------------------------------------------------------------
  Algorithm methods
  ----------------------------------------------------------------------------
*/

template< typename TInputImage >
Segmenter< TInputImage >::~Segmenter()
{
  delete[] m_Connectivity.index;
  delete[] m_Connectivity.direction;
}

template< typename TInputImage >
void Segmenter< TInputImage >::GenerateData()
{
  //
  // Allocate all the necessary temporary data structures and variables that
  // will be used in this algorithm.  Also re-initialize some temporary data
  // structures that may have been used in previous updates of this filter.
  //
  unsigned int i;

  this->UpdateProgress(0.0);
  if ( m_DoBoundaryAnalysis == false )
    {
    this->GetSegmentTable()->Clear();
    this->SetCurrentLabel(1);
    }

  flat_region_table_t flatRegions;

  typename InputImageType::Pointer input   = this->GetInputImage();
  typename OutputImageType::Pointer output = this->GetOutputImage();
  typename BoundaryType::Pointer boundary  = this->GetBoundary();

  // ------------------------------------------------------------------------
  //
  // HERE ARE THE ASSUMPTIONS ABOUT REGION SIZES FOR NOW.  WHEN THE PIPELINE
  // FULLY SUPPORTS STREAMING, THESE WILL NEED TO BE CHANGED ACCORDINGLY.
  //
  // 1) All region sizes are equivalent.  There is no distinction among
  // regions.  The region size is assumed to be padded one pixel out along each
  // chunk face unless that face touches an actual data set boundary.
  //
  // 2) The ivar m_LargestPossibleRegion represents the actual size of the data
  // set.  This has to be set by the user since the pipeline sometimes clobbers
  // the actual LargestPossibleRegion (?).
  //
  // -------------------------------------------------------------------------

  //
  // Generate the "face" regions A that constitute our shared boundary with
  // another chunk.  Also determine which face regions B lie on a the true
  // dataset boundary.  The faces corresponding to B will need to be padded
  // out a pixel when we threshold so that we can construct the retaining wall
  // along those faces.
  //
  ImageRegionType regionToProcess       = output->GetRequestedRegion();
  ImageRegionType largestPossibleRegion = this->GetLargestPossibleRegion();
  ImageRegionType thresholdImageRegion  = regionToProcess;
  ImageRegionType thresholdLargestPossibleRegion =
    this->GetLargestPossibleRegion();

  // First we have to find the boundaries and adjust the threshold image size
  typename ImageRegionType::IndexType tidx = thresholdImageRegion.GetIndex();
  typename ImageRegionType::SizeType tsz = thresholdImageRegion.GetSize();
  typename ImageRegionType::IndexType tlidx = thresholdLargestPossibleRegion.GetIndex();
  typename ImageRegionType::SizeType tlsz = thresholdLargestPossibleRegion.GetSize();
  for ( i = 0; i < ImageDimension; ++i )
    {
    ImageRegionType reg;
    typename ImageRegionType::IndexType idx = regionToProcess.GetIndex();
    typename ImageRegionType::SizeType sz  = regionToProcess.GetSize();

    // Set LOW face
    idx[i] = regionToProcess.GetIndex()[i];
    sz[i]  = 1;
    reg.SetSize(sz);
    reg.SetIndex(idx);

    if ( reg.GetIndex()[i] == largestPossibleRegion.GetIndex()[i] )
      {
      // This is facing a true data set boundary
      tsz[i] += 1; // we need to pad our threshold image on this face
      tidx[i] -= 1;
      tlsz[i] += 1; // we need to pad our threshold image on this face
      tlidx[i] -= 1;

      boundary->SetValid(false, i, 0);
      }
    else
      {
      // This is an overlap with another data chunk in the data set
      // Mark this boundary face as valid.
      boundary->SetValid(true, i, 0);
      }

    // Set HIGH face
    idx[i] = ( regionToProcess.GetIndex()[i] + regionToProcess.GetSize()[i] ) - 1;
    reg.SetSize(sz);
    reg.SetIndex(idx);
    if ( ( reg.GetIndex()[i] + reg.GetSize()[i] )
         == ( largestPossibleRegion.GetIndex()[i]
              + largestPossibleRegion.GetSize()[i] ) )
      {
      // This is facing a true data set boundary
      tsz[i] += 1;  // we need to pad our threshold image on this face
      tlsz[i] += 1; // we need to pad our threshold image on this face
      boundary->SetValid(false, i, 1);
      }
    else
      {
      // This is an overlap with another data chunk in the data set
      // Mark this face as valid in the boundary.
      boundary->SetValid(true, i, 1);
      }
    }
  thresholdImageRegion.SetSize(tsz);
  thresholdImageRegion.SetIndex(tidx);
  thresholdLargestPossibleRegion.SetSize(tlsz);
  thresholdLargestPossibleRegion.SetIndex(tlidx);

  // Now create and allocate the threshold image.  We need a single pixel
  // border around the NxM region we are segmenting.  This means that for faces
  // that have no overlap into another chunk, we have to pad the image.
  typename InputImageType::Pointer thresholdImage = InputImageType::New();

  thresholdImage->SetLargestPossibleRegion(thresholdLargestPossibleRegion);
  thresholdImage->SetBufferedRegion(thresholdImageRegion);
  thresholdImage->SetRequestedRegion(thresholdImageRegion);
  thresholdImage->Allocate();

  // Now threshold the image. First we calculate the dynamic range of
  // the input.  Then, the threshold operation clamps the lower
  // intensity values at the prescribed threshold.  If the data is
  // integral, then any intensity at NumericTraits<>::max() is reduced
  // by one intensity value.  This allows the watershed algorithm to
  // build a barrier around the image with values above the maximum
  // intensity value which trivially stop the steepest descent search
  // for local minima without requiring expensive boundary conditions.
  //
  //
  InputPixelType minimum, maximum;
  Self::MinMax(input, regionToProcess, minimum, maximum);
  // cap the maximum in the image so that we can always define a pixel
  // value that is one greater than the maximum value in the image.
  if ( NumericTraits< InputPixelType >::IsInteger
CLANG_PRAGMA_PUSH
CLANG_SUPPRESS_Wfloat_equal
       && maximum == NumericTraits< InputPixelType >::max() )
CLANG_PRAGMA_POP
    {
    maximum -= NumericTraits< InputPixelType >::OneValue();
    }
  // threshold the image.
  Self::Threshold( thresholdImage, input, regionToProcess, regionToProcess,
                   static_cast< InputPixelType >( ( m_Threshold * ( maximum - minimum ) ) + minimum ) );

  //
  // Redefine the regionToProcess in terms of the threshold image.  The region
  // to  process represents all the pixels contained within the 1 pixel padded
  // boundary of the threshold image.
  //
  typename ImageRegionType::SizeType irsz;
  typename ImageRegionType::IndexType iridx;
  for ( i = 0; i < ImageDimension; ++i )
    {
    irsz[i]  = thresholdImageRegion.GetSize()[i] - 2;
    iridx[i] = thresholdImageRegion.GetIndex()[i] + 1;
    }
  regionToProcess.SetIndex(iridx);
  regionToProcess.SetSize(irsz);

  //
  // Initialize the connectivity information that will be used by the
  // segmentation algorithm.
  //
  this->GenerateConnectivity();

  //
  // Store the regionToProcess in the RequestedRegion of the threshold image.
  // We are now completely done with the input image.  The input image memory
  // can be released at this point if need be.
  //
  thresholdImage->SetRequestedRegion(regionToProcess);
  this->ReleaseInputs();

  //
  // At this point we are ready to define the output
  // buffer and allocate memory for the output image.
  //
  output->SetBufferedRegion( thresholdImage->GetBufferedRegion() );
  output->Allocate();
  Self::SetOutputImageValues(output, output->GetBufferedRegion(), Self::NULL_LABEL);

  //
  // Now we can create appropriate boundary regions for analyzing the
  // flow at the boundaries from the requested region of the threshold
  // image.
  //
  typename BoundaryType::IndexType b_idx;
  ImageRegionType reg_b;
  typename ImageRegionType::IndexType idx_b;
  typename ImageRegionType::SizeType sz_b;

  for ( b_idx.first = 0; b_idx.first < ImageDimension; ++b_idx.first )
    {
    for ( b_idx.second = 0; b_idx.second < 2; ++b_idx.second )
      {
      if ( boundary->GetValid(b_idx) == false ) { continue; }
      idx_b = thresholdImage->GetRequestedRegion().GetIndex();
      sz_b = thresholdImage->GetRequestedRegion().GetSize();

      if ( b_idx.second == 1 ) // HIGH face must adjust start index
        {
        idx_b[b_idx.first] += sz_b[b_idx.first] - 1;
        }

      sz_b[b_idx.first] = 1;

      reg_b.SetIndex(idx_b);
      reg_b.SetSize(sz_b);

      boundary->GetFace(b_idx)->SetLargestPossibleRegion(reg_b);
      boundary->GetFace(b_idx)->SetRequestedRegion(reg_b);
      boundary->GetFace(b_idx)->SetBufferedRegion(reg_b);
      boundary->GetFace(b_idx)->Allocate();
      }
    }
  this->UpdateProgress(0.1);

  //
  // Analyze the flow at the boundaries.  This method labels all the boundary
  // pixels that flow out of this chunk (either through gradient descent or
  // flat-region connectivity) and constructs the appropriate Boundary
  // data structures.
  //
  if ( m_DoBoundaryAnalysis == true )
    {
    this->InitializeBoundary();
    this->AnalyzeBoundaryFlow(thresholdImage, flatRegions, maximum
                              + NumericTraits< InputPixelType >::OneValue());
    }

  this->UpdateProgress(0.2);

  //
  // Build a "retaining wall" around the image so that gradient descent
  // analysis can be done without worrying about boundaries.
  //
  // All overlap boundary information will be overwritten, but is no longer
  // needed now.
  //
  this->BuildRetainingWall(thresholdImage,
                           thresholdImage->GetBufferedRegion(),
                           maximum + NumericTraits< InputPixelType >::OneValue());

  //
  // Label all the local minima pixels in the image.  This function also
  // labels flat regions, defined as regions where connected pixels all have
  // the same value.
  //
  this->LabelMinima(thresholdImage, thresholdImage->GetRequestedRegion(),
                    flatRegions, maximum + NumericTraits< InputPixelType >::OneValue());
  this->UpdateProgress(0.3);

  this->GradientDescent( thresholdImage, thresholdImage->GetRequestedRegion() );
  this->UpdateProgress(0.4);

  this->DescendFlatRegions( flatRegions, thresholdImage->GetRequestedRegion() );
  this->UpdateProgress(0.5);

  this->UpdateSegmentTable( thresholdImage, thresholdImage->GetRequestedRegion() );
  this->UpdateProgress(0.6);

  if ( m_DoBoundaryAnalysis == true )
          {  this->CollectBoundaryInformation(flatRegions); }
  this->UpdateProgress(0.7);

  if ( m_SortEdgeLists == true )
          {  this->GetSegmentTable()->SortEdgeLists(); }
  this->UpdateProgress(0.8);

  this->GetSegmentTable()->SetMaximumDepth(maximum - minimum);
  this->UpdateProgress(1.0);
}

template< typename TInputImage >
void Segmenter< TInputImage >
::CollectBoundaryInformation(flat_region_table_t & flatRegions)
{
  typename OutputImageType::Pointer output = this->GetOutputImage();
  typename BoundaryType::Pointer boundary  = this->GetBoundary();

  ImageRegionIterator< typename BoundaryType::face_t > faceIt;
  ImageRegionIterator< OutputImageType >                   labelIt;

  typename BoundaryType::face_t::Pointer face;
  typename BoundaryType::flat_hash_t           * flats;
  typename BoundaryType::flat_hash_t::iterator flats_it;
  typename BoundaryType::flat_region_t flr;
  typename flat_region_table_t::iterator flrt_it;

  typename BoundaryType::IndexType idx;
  ImageRegionType region;

  for ( idx.first = 0; idx.first < ImageDimension; ( idx.first )++ )
    {
    for ( idx.second = 0; idx.second < 2; ( idx.second )++ )
      {
      if ( boundary->GetValid(idx) == false ) { continue; }

      face  = boundary->GetFace(idx);
      flats = boundary->GetFlatHash(idx);
      region = face->GetRequestedRegion();

      // Grab all the labels of the boundary pixels.
      faceIt = ImageRegionIterator< typename BoundaryType::face_t >(face,
                                                                        region);
      labelIt = ImageRegionIterator< OutputImageType >(output, region);
      faceIt.GoToBegin();
      labelIt.GoToBegin();
      while ( !faceIt.IsAtEnd() )
        {
        faceIt.Value(). label = labelIt.Get();

        // Is this a flat region that flows out?
        flrt_it = flatRegions.find( labelIt.Get() );
        if ( faceIt.Get().flow != NULL_FLOW
             && flrt_it != flatRegions.end() )
          {
          // Have we already entered this
          // flat region into the boundary?
          flats_it = flats->find( labelIt.Get() );
          if ( flats_it == flats->end() )  // NO
            {
            flr.bounds_min = ( *flrt_it ).second.bounds_min;
            flr.min_label  = *( ( *flrt_it ).second.min_label_ptr );
            flr.value      = ( *flrt_it ).second.value;
            flr.offset_list.push_back(
              face->ComputeOffset( faceIt.GetIndex() ) );
            flats->insert(
              BoundaryFlatHashValueType(labelIt.Get(), flr) );
            flr.offset_list.clear();
            }
          else // YES
            {
            ( *flats_it ).second.offset_list.push_back( face->ComputeOffset( faceIt.GetIndex() ) );
            }
          }

        ++faceIt;
        ++labelIt;
        }
      }
    }
}

template< typename TInputImage >
void Segmenter< TInputImage >
::InitializeBoundary()
{
  ImageRegionIterator< typename BoundaryType::face_t > faceIt;
  typename BoundaryType::face_t::Pointer face;
  typename BoundaryType::face_pixel_t fps;
  BoundaryIndexType idx;

  fps.flow  = NULL_FLOW;
  fps.label = NULL_LABEL;

  for ( idx.first = 0; idx.first < ImageDimension; ++( idx.first ) )
    {
    for ( idx.second = 0; idx.second < 2; ++( idx.second ) )
      {
      if ( this->GetBoundary()->GetValid(idx) == false ) { continue; }
      this->GetBoundary()->GetFlatHash(idx)->clear();
      face = this->GetBoundary()->GetFace(idx);
      faceIt = ImageRegionIterator< typename BoundaryType::face_t >
                 ( face, face->GetBufferedRegion() );
      for ( faceIt.GoToBegin(); !faceIt.IsAtEnd(); ++faceIt )
        {
        faceIt.Set(fps);
        }
      }
    }
}

template< typename TInputImage >
void Segmenter< TInputImage >
::AnalyzeBoundaryFlow(InputImageTypePointer thresholdImage,
                      flat_region_table_t & flatRegions,
                      InputPixelType max)
{
  //
  // NOTE: For ease of initial implementation, this method does
  // not support arbitrary connectivity across boundaries (yet). 10-8-01 jc
  //
  unsigned int nCenter, i, nPos, cPos;
  bool         isSteepest;

  ConstNeighborhoodIterator< InputImageType >              searchIt;
  NeighborhoodIterator< OutputImageType >                  labelIt;
  ImageRegionIterator< typename BoundaryType::face_t > faceIt;

  BoundaryIndexType idx;
  ImageRegionType   region;
  typename ConstNeighborhoodIterator< InputImageType >::RadiusType rad;

  typename BoundaryType::face_pixel_t fps;
  flat_region_t tempFlatRegion;

  typename OutputImageType::Pointer output   = this->GetOutputImage();
  typename BoundaryType::Pointer boundary = this->GetBoundary();

  for ( i = 0; i < ImageDimension; ++i )
    {
    rad[i] = 1;
    }
  fps.label = NULL_LABEL;

  EquivalencyTable::Pointer eqTable = EquivalencyTable::New();

  // Process each boundary region.
  for ( idx.first = 0; idx.first < ImageDimension; ++( idx.first ) )
    {
    for ( idx.second = 0; idx.second < 2; ++( idx.second ) )
      {
      // Skip irrelevant boundaries
      if ( boundary->GetValid(idx) == false ) { continue; }

      typename BoundaryType::face_t::Pointer face = boundary->GetFace(idx);
      region = face->GetRequestedRegion();

      searchIt =
        ConstNeighborhoodIterator< InputImageType >(rad, thresholdImage, region);
      labelIt = NeighborhoodIterator< OutputImageType >(rad, output, region);
      faceIt  = ImageRegionIterator< typename BoundaryType::face_t >(face, region);

      nCenter = searchIt.Size() / 2;
      searchIt.GoToBegin();
      labelIt.GoToBegin();

      if ( ( idx ).second == 0 )
        {
        // Low face
        cPos = m_Connectivity.index[( idx ).first];
        }
      else
        {
        // High face
        cPos = m_Connectivity.index[( ImageDimension - 1 )
                                    + ( ImageDimension - ( idx ).first )];
        }

      while ( !searchIt.IsAtEnd() )
        {
        // Is this a flat connection?
        if ( Math::AlmostEquals( searchIt.GetPixel(nCenter), searchIt.GetPixel(cPos) ) )
          {
          // Fill in the boundary flow information.
          // Labels will be collected later.
          fps.flow   = static_cast< short >( cPos );
          faceIt.Set(fps);

          // Are we touching flat regions
          // that have already been labeled?
          bool _labeled    = false;
          bool _connected  = false;
          for ( i = 0; i < m_Connectivity.size; i++ )
            {
            nPos = m_Connectivity.index[i];
            if (   Math::AlmostEquals( searchIt.GetPixel(nCenter), searchIt.GetPixel(nPos) )
                   && labelIt.GetPixel(nPos) != Self::NULL_LABEL
                   && labelIt.GetPixel(nPos) != labelIt.GetPixel(nCenter)
                   )
              {
              _connected = true;
              if ( _labeled == false )
                {
                labelIt.SetPixel( nCenter,
                                  labelIt.GetPixel(nPos) );
                _labeled = true;
                }
              else
                {
                eqTable->Add( labelIt.GetPixel(nCenter), labelIt.GetPixel(nPos) );
                }
              }
            }
          if ( _connected == false ) // Add a new flat region.
            {
            labelIt.SetPixel(nCenter, m_CurrentLabel);

            // Add a flat region to the (global) flat region table
            tempFlatRegion.bounds_min    = max;
            tempFlatRegion.min_label_ptr = output->GetBufferPointer()
                                           + output->ComputeOffset( labelIt.GetIndex() );
            tempFlatRegion.value         = searchIt.GetPixel(nCenter);
            tempFlatRegion.is_on_boundary = true;
            flatRegions[m_CurrentLabel]  = tempFlatRegion;

            m_CurrentLabel++;
            }
          }
        else  // Is cPos the path of steepest descent?
          {
          if ( searchIt.GetPixel(cPos) < searchIt.GetPixel(nCenter) )
            {
            isSteepest = true;
            for ( i = 0; i < m_Connectivity.size; i++ )
              {
              nPos = m_Connectivity.index[i];
              if ( searchIt.GetPixel(nPos) < searchIt.GetPixel(cPos) )
                {
                isSteepest = false;
                break;
                }
              }
            }
          else { isSteepest = false; }

          if ( isSteepest == true )
            {
            // Label this pixel. It will be safely treated as a local
            // minimum by the rest of the segmentation algorithm.
            labelIt.SetPixel(nCenter, m_CurrentLabel);

            // Add the connectivity information
            // to the boundary data structure.
            fps.flow  = static_cast< short >( cPos );
            faceIt.Set(fps);

            // Since we've labeled this pixel, we need to check to
            // make sure this is not also a flat region.  If it is,
            // then it must be entered into the flat region table
            // or we could have problems later on.
            for ( i = 0; i < m_Connectivity.size; i++ )
              {
              nPos = m_Connectivity.index[i];
              if ( Math::AlmostEquals( searchIt.GetPixel(nPos),
                   searchIt.GetPixel(nCenter) ) )
                {
                tempFlatRegion.bounds_min = max;
                tempFlatRegion.min_label_ptr =
                  output->GetBufferPointer()
                  + output->ComputeOffset( labelIt.GetIndex() );
                tempFlatRegion.value =
                  searchIt.GetPixel(nCenter);
                tempFlatRegion.is_on_boundary = false;
                flatRegions[m_CurrentLabel] = tempFlatRegion;
                break;
                }
              }
            m_CurrentLabel++;
            }
          }

        ++searchIt;
        ++labelIt;
        ++faceIt;
        }
      }
    }

  eqTable->Flatten();

  // Now relabel any equivalent regions in the boundaries.
  for ( idx.first = 0; idx.first < ImageDimension; ++( idx.first ) )
    {
    for ( idx.second = 0; idx.second < 2; ++( idx.second ) )
      {
      // Skip irrelevant boundaries
      if ( boundary->GetValid(idx) == false ) { continue; }

      typename BoundaryType::face_t::Pointer face = boundary->GetFace(idx);
      region = face->GetRequestedRegion();

      Self::RelabelImage(output, region, eqTable);
      }
    }

  // Merge the flat regions in the table
  Self::MergeFlatRegions(flatRegions, eqTable);
}

template< typename TInputImage >
void Segmenter< TInputImage >
::GenerateConnectivity()
{
  unsigned int i, j, nSize, nCenter, stride;
  int          d;

  //
  // Creates city-block style connectivity.  4-Neighbors in 2D.  6-Neighbors in
  // 3D, etc...  Order of creation MUST be lowest index to highest index in the
  // neighborhood.  I.e. for 4 connectivity,
  //
  //     * 1 *
  //     2 * 3
  //     * 4 *
  //
  // Algorithms assume this order to the connectivity.
  //
  typename ConstNeighborhoodIterator< InputImageType >::RadiusType rad;
  for ( i = 0; i < ImageDimension; ++i )
    {
    rad[i] = 1;
    }
  ConstNeighborhoodIterator< InputImageType > it( rad, this->GetInputImage(),
                                                  this->GetInputImage()->GetRequestedRegion() );
  nSize   = it.Size();
  nCenter = nSize >> 1;

  for ( i = 0; i < m_Connectivity.size; i++ ) // initialize move list
    {
    for ( j = 0; j < ImageDimension; j++ )
      {
      m_Connectivity.direction[i][j] = 0;
      }
    }
  i = 0;
  for ( d = ImageDimension - 1; d >= 0; d-- )
    {
    stride = it.GetStride(d);
    m_Connectivity.index[i] = nCenter - stride;
    m_Connectivity.direction[i][d] = -1;
    i++;
    }
  for ( d = 0; d < static_cast< int >( ImageDimension ); d++ )
    {
    stride = it.GetStride(d);
    m_Connectivity.index[i] = nCenter + stride;
    m_Connectivity.direction[i][d] = 1;
    i++;
    }
}

template< typename TInputImage >
void Segmenter< TInputImage >
::LabelMinima(InputImageTypePointer img, ImageRegionType region,
              typename Self::flat_region_table_t & flatRegions, InputPixelType Max)
{
  unsigned int   i, nSize, nCenter, nPos = 0;
  bool           foundSinglePixelMinimum, foundFlatRegion;
  InputPixelType maxValue = Max;

  flat_region_t tempFlatRegion;

  typename flat_region_table_t::iterator flatPtr;
  InputPixelType            currentValue;
  EquivalencyTable::Pointer equivalentLabels = EquivalencyTable::New();

  typename OutputImageType::Pointer output = this->GetOutputImage();

  // Set up the iterators.
  typename ConstNeighborhoodIterator< InputImageType >::RadiusType rad;
  for ( i = 0; i < ImageDimension; ++i )
    {
    rad[i] = 1;
    }
  ConstNeighborhoodIterator< InputImageType > searchIt(rad, img, region);
  NeighborhoodIterator< OutputImageType >     labelIt(rad, output, region);
  nSize   = searchIt.Size();
  nCenter = nSize >> 1;

  // Sweep through the images.  Label all local minima
  // and record information for all the flat regions.
  for ( searchIt.GoToBegin(), labelIt.GoToBegin();
        !searchIt.IsAtEnd(); ++searchIt, ++labelIt )
    {
    foundSinglePixelMinimum = true;
    foundFlatRegion = false;

    // If this pixel has been labeled already,
    // skip directly to the next iteration.
    if ( labelIt.GetPixel(nCenter) != Self::NULL_LABEL ) { continue; }

    // Compare current pixel value with its neighbors.
    currentValue = searchIt.GetPixel(nCenter);

    for ( i = 0; i < m_Connectivity.size; ++i )
      {
      nPos = m_Connectivity.index[i];
      if ( Math::AlmostEquals( currentValue, searchIt.GetPixel(nPos) ) )
        {
        foundFlatRegion  = true;
        break;
        }
      else if ( currentValue > searchIt.GetPixel(nPos) )
        {
        foundSinglePixelMinimum = false;
        }
      }

    if ( foundFlatRegion )
      {
      if ( labelIt.GetPixel(nPos) != Self::NULL_LABEL ) // If the flat region is
                                                        // already
        {                                               // labeled, label this
                                                        // to match.
        labelIt.SetPixel( nCenter, labelIt.GetPixel(nPos) );
        }
      else // Add a new flat region to the table.
        {  // Initialize its contents.
        labelIt.SetPixel(nCenter,  m_CurrentLabel);
        nPos = m_Connectivity.index[0];

        tempFlatRegion.bounds_min        = maxValue;
        tempFlatRegion.min_label_ptr     = labelIt[nPos];
        tempFlatRegion.value             = currentValue;
        flatRegions[m_CurrentLabel]      = tempFlatRegion;
        m_CurrentLabel = m_CurrentLabel + 1;
        }

      // While we're at it, check to see if we have just linked two flat
      // regions with the same height value.  Save that info for later.
      for ( i++; i < m_Connectivity.size; ++i )
        {
        nPos = m_Connectivity.index[i];
        if (   Math::AlmostEquals( searchIt.GetPixel(nCenter), searchIt.GetPixel(nPos) )
               && labelIt.GetPixel(nPos) != Self::NULL_LABEL
               && labelIt.GetPixel(nPos) != labelIt.GetPixel(nCenter)
               )
          {
          equivalentLabels->Add( labelIt.GetPixel(nCenter),
                                 labelIt.GetPixel(nPos) );
          }
        }
      }
    else if ( foundSinglePixelMinimum )
      {
      labelIt.SetPixel(nCenter,  m_CurrentLabel);
      m_CurrentLabel = m_CurrentLabel + 1;
      }
    }

  // Merge the flat regions that we identified as connected components.
  Self::MergeFlatRegions(flatRegions, equivalentLabels);

  // Relabel the image with the merged regions.
  Self::RelabelImage(output, region, equivalentLabels);

  equivalentLabels->Clear();

  // Now make another pass to establish the
  // boundary values for the flat regions.
  for ( searchIt.GoToBegin(), labelIt.GoToBegin();
        !searchIt.IsAtEnd(); ++searchIt, ++labelIt )
    {
    flatPtr = flatRegions.find( labelIt.GetPixel(nCenter) );
    if ( flatPtr != flatRegions.end() ) // If we are in a flat region
      {                                 // Search the connectivity neighborhood
                                        // for lesser boundary pixels.
      for ( i = 0; i < m_Connectivity.size; ++i )
        {
        nPos = m_Connectivity.index[i];

        if  ( labelIt.GetPixel(nPos) != labelIt.GetPixel(nCenter)
              && searchIt.GetPixel(nPos) < ( *flatPtr ).second.bounds_min )
          { // If this is a boundary pixel && has a lesser value than
            // the currently recorded value...
          ( *flatPtr ).second.bounds_min = searchIt.GetPixel(nPos);
          ( *flatPtr ).second.min_label_ptr = labelIt[nPos];
          }
        if ( Math::AlmostEquals( searchIt.GetPixel(nCenter), searchIt.GetPixel(nPos) ) )
          {
          if ( labelIt.GetPixel(nPos) != NULL_LABEL )
            {
            // Pick up any equivalencies we missed before.
            equivalentLabels->Add( labelIt.GetPixel(nCenter),
                                   labelIt.GetPixel(nPos) );
            }
          // If the following is encountered, it means that there is a
          // logic flaw in the first pass of this algorithm where flat
          // regions are initially detected and linked.
#ifndef NDEBUG
          else { itkDebugMacro("An unexpected but non-fatal error has occurred."); }
#endif
          }
        }
      }
    }

  // Merge the flat regions that we identified as connected components.
  Self::MergeFlatRegions(flatRegions, equivalentLabels);

  // Relabel the image with the merged regions.
  Self::RelabelImage(output, region, equivalentLabels);
}

template< typename TInputImage >
void Segmenter< TInputImage >
::GradientDescent(InputImageTypePointer img,
                  ImageRegionType region)
{
  typename OutputImageType::Pointer output = this->GetOutputImage();

  InputPixelType minVal;
  unsigned int   i, nPos;
  typename InputImageType::OffsetType moveIndex;
  IdentifierType                 newLabel;
  std::stack< IdentifierType * > updateStack;

  //
  // Set up our iterators.
  //
  typename ConstNeighborhoodIterator< InputImageType >::RadiusType rad;
  typename NeighborhoodIterator< OutputImageType >::RadiusType zeroRad;
  for ( i = 0; i < ImageDimension; ++i )
    {
    rad[i] = 1;
    zeroRad[i] = 0;
    }
  ConstNeighborhoodIterator< InputImageType >
  valueIt(rad, img, region);
  NeighborhoodIterator< OutputImageType >
                                         labelIt(zeroRad, output, region);
  ImageRegionIterator< OutputImageType > it(output, region);

  //
  // Sweep through the image and trace all unlabeled
  // pixels to a labeled region
  //
  for ( it.GoToBegin(); !it.IsAtEnd(); ++it )
    {
    if ( it.Get() == NULL_LABEL )
      {
      valueIt.SetLocation( it.GetIndex() );
      labelIt.SetLocation( it.GetIndex() );
      newLabel = NULL_LABEL;               // Follow the path of steep-
      while ( newLabel == NULL_LABEL )     // est descent until a label
        {                                  // is found.
        updateStack.push( labelIt.GetCenterPointer() );
        minVal = valueIt.GetPixel(m_Connectivity.index[0]);
        moveIndex = m_Connectivity.direction[0];
        for ( unsigned int ii = 1; ii < m_Connectivity.size; ++ii )
          {
          nPos = m_Connectivity.index[ii];
          if ( valueIt.GetPixel(nPos) < minVal )
            {
            minVal = valueIt.GetPixel(nPos);
            moveIndex = m_Connectivity.direction[ii];
            }
          }
        valueIt += moveIndex;
        labelIt += moveIndex;
        newLabel = labelIt.GetPixel(0);
        }

      while ( !updateStack.empty() ) // Update all the pixels we've traversed
        {
        *( updateStack.top() ) = newLabel;
        updateStack.pop();
        }
      }
    }
}

template< typename TInputImage >
void Segmenter< TInputImage >
::DescendFlatRegions(flat_region_table_t & flatRegionTable,
                     ImageRegionType imageRegion)
{
  typename OutputImageType::Pointer output = this->GetOutputImage();
  // Assumes all pixels are labeled in the image.  Steps through the flat
  // regions and equates each one with the label at its lowest boundary
  // point. Flat basins are preserved as their own regions. The output image is
  // relabeled to reflect these equivalencies.
  EquivalencyTable::Pointer equivalentLabels = EquivalencyTable::New();

  for ( typename flat_region_table_t::const_iterator region = flatRegionTable.begin();
        region != flatRegionTable.end(); ++region )
    {
    if ( ( ( *region ).second.bounds_min < ( *region ).second.value )
         && ( !( *region ).second.is_on_boundary ) )
      {
      equivalentLabels->Add( ( *region ).first, *( ( *region ).second.min_label_ptr ) );
      }
    }

  equivalentLabels->Flatten();
  Self::RelabelImage(output, imageRegion, equivalentLabels);
}

template< typename TInputImage >
void Segmenter< TInputImage >
::UpdateSegmentTable(InputImageTypePointer input, ImageRegionType region)
{
  edge_table_hash_t edgeHash;
  edge_table_t      tempEdgeTable;

  typename edge_table_hash_t::iterator edge_table_entry_ptr;
  typename edge_table_t::iterator edge_ptr;

  unsigned int i, nPos;
  typename NeighborhoodIterator< OutputImageType >::RadiusType hoodRadius;
  typename SegmentTableType::segment_t * segment_ptr;
  typename SegmentTableType::segment_t temp_segment;
  IdentifierType segment_label;

  InputPixelType lowest_edge;

  // Grab the data we need.
  typename OutputImageType::Pointer output    = this->GetOutputImage();
  typename SegmentTableType::Pointer segments = this->GetSegmentTable();

  // Set up some iterators.
  for ( i = 0; i < ImageDimension; i++ )
    {
    hoodRadius[i] = 1;
    }
  ConstNeighborhoodIterator< InputImageType > searchIt(hoodRadius, input, region);
  NeighborhoodIterator< OutputImageType >     labelIt(hoodRadius, output, region);

  IdentifierType hoodCenter = searchIt.Size() >> 1;

  for ( searchIt.GoToBegin(), labelIt.GoToBegin(); !searchIt.IsAtEnd();
        ++searchIt, ++labelIt )
    {
    segment_label = labelIt.GetPixel(hoodCenter);

    // Find the segment corresponding to this label
    // and update its minimum value if necessary.
    segment_ptr = segments->Lookup(segment_label);
    edge_table_entry_ptr = edgeHash.find(segment_label);
    if ( segment_ptr == ITK_NULLPTR ) // This segment not yet identified.
      {                     // So add it to the table.
      temp_segment.min = searchIt.GetPixel(hoodCenter);
      segments->Add(segment_label, temp_segment);
      typedef typename edge_table_hash_t::value_type ValueType;
      edgeHash.insert( ValueType(segment_label,
                                 tempEdgeTable) );

      edge_table_entry_ptr = edgeHash.find(segment_label);
      }
    else if ( searchIt.GetPixel(hoodCenter) < segment_ptr->min )
      {
      segment_ptr->min = searchIt.GetPixel(hoodCenter);
      }

    // Look up each neighboring segment in this segment's edge table.
    // If an edge exists, compare (and reset) the minimum edge value.
    // Note that edges are located *between* two adjacent pixels and
    // the value is taken to be the maximum of the two adjacent pixel
    // values.
    for ( i = 0; i < m_Connectivity.size; ++i )
      {
      nPos = m_Connectivity.index[i];
      if ( labelIt.GetPixel(nPos) != segment_label
           && labelIt.GetPixel(nPos) != NULL_LABEL )
        {
        if ( searchIt.GetPixel(nPos) < searchIt.GetPixel(hoodCenter) )
          {
          lowest_edge = searchIt.GetPixel(hoodCenter); // We want the
          }
        else
          {
          lowest_edge = searchIt.GetPixel(nPos);       // max of the
          }
        // adjacent pixels

        edge_ptr = ( *edge_table_entry_ptr ).second.find( labelIt.GetPixel(nPos) );
        if ( edge_ptr == ( *edge_table_entry_ptr ).second.end() )
          {     // This edge has not been identified yet.
          typedef typename edge_table_t::value_type ValueType;
          ( *edge_table_entry_ptr ).second.insert(
            ValueType(labelIt.GetPixel(nPos), lowest_edge) );
          }
        else if ( lowest_edge < ( *edge_ptr ).second )
          {
          ( *edge_ptr ).second = lowest_edge;
          }
        }
      }
    }

  //
  // Copy all of the edge tables into the edge lists of the
  // segment table.
  //
  IdentifierType listsz;
  typename SegmentTableType::edge_list_t::iterator list_ptr;
  for ( edge_table_entry_ptr = edgeHash.begin();
        edge_table_entry_ptr != edgeHash.end();
        edge_table_entry_ptr++ )
    {
    // Lookup the corresponding segment entry
    segment_ptr = segments->Lookup( ( *edge_table_entry_ptr ).first );
    if ( segment_ptr == ITK_NULLPTR )
      {
      itkGenericExceptionMacro (<< "UpdateSegmentTable:: An unexpected and fatal error has occurred.");
      }

    // Copy into the segment list
    listsz = static_cast< IdentifierType >( ( *edge_table_entry_ptr ).second.size() );
    segment_ptr->edge_list.resize(listsz);
    edge_ptr = ( *edge_table_entry_ptr ).second.begin();
    list_ptr = segment_ptr->edge_list.begin();
    while ( edge_ptr != ( *edge_table_entry_ptr ).second.end() )
      {
      list_ptr->label = ( *edge_ptr ).first;
      list_ptr->height = ( *edge_ptr ).second;
      edge_ptr++;
      list_ptr++;
      }

    // Clean up memory as we go
    ( *edge_table_entry_ptr ).second.clear();
    }
}

template< typename TInputImage >
void Segmenter< TInputImage >
::BuildRetainingWall(InputImageTypePointer img,
                     ImageRegionType region,
                     InputPixelType value)
{
  unsigned int i;

  typename ImageRegionType::SizeType sz;
  typename ImageRegionType::IndexType idx;
  ImageRegionType reg;

  // Loop through the dimensions and populate the LOW and HIGH faces regions.
  for ( i = 0; i < ImageDimension; ++i )
    {
    idx = region.GetIndex();       // LOW face
    sz  = region.GetSize();
    sz[i] = 1;
    reg.SetIndex(idx);
    reg.SetSize(sz);
    Segmenter::SetInputImageValues(img, reg, value);
    idx[i] = region.GetSize()[i] + region.GetIndex()[i] - 1;  // HIGH face
    reg.SetIndex(idx);
    Segmenter::SetInputImageValues(img, reg, value);
    }
}

/*
  ----------------------------------------------------------------------------
  Algorithm helper methods and debugging methods
  ----------------------------------------------------------------------------
*/
template< typename TInputImage >
void Segmenter< TInputImage >
::SetInputImageValues(InputImageTypePointer img,
                      ImageRegionType region,
                      InputPixelType value)
{
  ImageRegionIterator< InputImageType > it(img, region);
  it.GoToBegin();
  while ( !it.IsAtEnd() )
    {
    it.Set(value);
    ++it;
    }
}

template< typename TInputImage >
void Segmenter< TInputImage >
::SetOutputImageValues(OutputImageTypePointer img,
                       ImageRegionType region,
                       IdentifierType value)
{
  ImageRegionIterator< OutputImageType > it(img, region);
  it.GoToBegin();
  while ( !it.IsAtEnd() )
    {
    it.Set(value);
    ++it;
    }
}

template< typename TInputImage >
void Segmenter< TInputImage >
::MinMax(InputImageTypePointer img, ImageRegionType region,
         InputPixelType & min, InputPixelType & max)
{
  ImageRegionIterator< InputImageType > it(img, region);
  it.GoToBegin();
  min = it.Value();
  max = it.Value();
  while ( !it.IsAtEnd() )
    {
    if ( it.Get() > max ) { max = it.Get(); }
    if ( it.Get() < min ) { min = it.Get(); }
    ++it;
    }
}

template< typename TInputImage >
void Segmenter< TInputImage >
::MergeFlatRegions(flat_region_table_t & regions,
                   EquivalencyTable::Pointer eqTable)
{
  // Note that the labels must have no interdependencies.  That is,
  // every key must map to a value that is not itself a key in the
  // table. This means that you must always merge label->first with
  // label->second (a to b). EquivalencyTable can be converted to this
  // format with its Flatten() method.
  eqTable->Flatten();

  typename flat_region_table_t::iterator a, b;
  for ( EquivalencyTable::ConstIterator it = eqTable->Begin();
        it != eqTable->End(); ++it )
    {
    if ( ( ( a = regions.find( ( *it ).first ) ) == regions.end() )
         || ( ( b = regions.find( ( *it ).second ) ) == regions.end() ) )
      {
      itkGenericExceptionMacro (<< "MergeFlatRegions:: An unexpected and fatal error has occurred.");
      }

    if ( ( *a ).second.bounds_min < ( *b ).second.bounds_min )
      {
      ( *b ).second.bounds_min = ( *a ).second.bounds_min;
      ( *b ).second.min_label_ptr = ( *a ).second.min_label_ptr;
      }

    regions.erase(a);
    }
}

template< typename TInputImage >
void Segmenter< TInputImage >
::RelabelImage(OutputImageTypePointer img,
               ImageRegionType region,
               EquivalencyTable::Pointer eqTable)
{
  eqTable->Flatten();

  IdentifierType                         temp;
  ImageRegionIterator< OutputImageType > it(img, region);

  it.GoToBegin();
  while ( !it.IsAtEnd() )
    {
    temp = eqTable->Lookup( it.Get() );
    if ( temp != it.Get() )  { it.Set(temp); }
    ++it;
    }
}

template< typename TInputImage >
void Segmenter< TInputImage >::Threshold(InputImageTypePointer destination,
                                         InputImageTypePointer source,
                                         const ImageRegionType source_region,
                                         const ImageRegionType destination_region,
                                         InputPixelType threshold)
{
  ImageRegionIterator< InputImageType > dIt(destination, destination_region);
  ImageRegionIterator< InputImageType > sIt(source, source_region);

  dIt.GoToBegin();
  sIt.GoToBegin();

  // Assumes that source_region and destination region are the same size.  Does
  // no checking!!
  if ( NumericTraits< InputPixelType >::is_integer )
    {
    // integral data type, if any pixel is at the maximum possible
    // value for the data type, then drop the value by one intensity
    // value. This the watershed algorithm to construct a "barrier" or
    // "wall" around the image that will stop the watershed without
    // requiring a expensive boundary condition checks.
    while ( !dIt.IsAtEnd() )
      {
      InputPixelType tmp = sIt.Get();
      if ( tmp < threshold )
        {
        dIt.Set(threshold);
        }
CLANG_PRAGMA_PUSH
CLANG_SUPPRESS_Wfloat_equal
      else if ( tmp == NumericTraits< InputPixelType >::max() )
CLANG_PRAGMA_POP
        {
        dIt.Set(tmp - NumericTraits< InputPixelType >::OneValue());
        }
      else
        {
        dIt.Set(tmp);
        }
      ++dIt;
      ++sIt;
      }
    }
  else
    {
    // floating point data, no need to worry about overflow
    while ( !dIt.IsAtEnd() )
      {
      if ( sIt.Get() < threshold )
        {
        dIt.Set(threshold);
        }
      else
        {
        dIt.Set( sIt.Get() );
        }
      ++dIt;
      ++sIt;
      }
    }
}

/*
  ----------------------------------------------------------------------------
  Pipeline methods
  ----------------------------------------------------------------------------
*/
template< typename TInputImage >
typename Segmenter< TInputImage >::DataObjectPointer
Segmenter< TInputImage >
::MakeOutput(DataObjectPointerArraySizeType idx)
{
  if ( idx == 0 )
    {
    return OutputImageType::New().GetPointer();
    }
  else if ( idx == 1 )
    {
    return SegmentTableType::New().GetPointer();
    }
  else if ( idx == 2 )
    {
    return BoundaryType::New().GetPointer();
    }
  else { return ITK_NULLPTR; }
}

template< typename TInputImage >
void
Segmenter< TInputImage >::UpdateOutputInformation()
{
  unsigned int i;

  // call the superclass' implementation of this method
  Superclass::UpdateOutputInformation();

  // get pointers to the input and output
  typename InputImageType::Pointer inputPtr = this->GetInputImage();
  typename OutputImageType::Pointer outputPtr = this->GetOutputImage();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }
  // we need to compute the output spacing, the output image size, and the
  // output image start index
  const typename InputImageType::SizeType &   inputSize =
    inputPtr->GetLargestPossibleRegion().GetSize();
  const typename InputImageType::IndexType &  inputStartIndex =
    inputPtr->GetLargestPossibleRegion().GetIndex();

  typename OutputImageType::SizeType outputSize;
  typename OutputImageType::IndexType outputStartIndex;

  for ( i = 0; i < OutputImageType::ImageDimension; i++ )
    {
    outputSize[i] = inputSize[i];
    outputStartIndex[i]  = inputStartIndex[i];
    }

  typename OutputImageType::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize(outputSize);
  outputLargestPossibleRegion.SetIndex(outputStartIndex);

  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);
}

template< typename TInputImage >
void Segmenter< TInputImage >::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  typename InputImageType::Pointer inputPtr  = this->GetInputImage();
  typename OutputImageType::Pointer outputPtr = this->GetOutputImage();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  //
  // FOR NOW WE'LL JUST SET THE INPUT REGION TO THE OUTPUT REGION
  // AND OVERRIDE THIS LATER
  //
  inputPtr->SetRequestedRegion( outputPtr->GetRequestedRegion() );
}

template< typename TInputImage >
void
Segmenter< TInputImage >
::GenerateOutputRequestedRegion(DataObject *output)
{
  // Only the Image output need to be propagated through.
  // No choice but to use RTTI here.
  ImageBase< ImageDimension > *imgData;
  ImageBase< ImageDimension > *op;
  imgData = dynamic_cast< ImageBase< ImageDimension > * >( output );
  typename TInputImage::RegionType c_reg;

  if ( imgData )
    {
    std::vector< ProcessObject::DataObjectPointer >::size_type idx;
    for ( idx = 0; idx < this->GetNumberOfIndexedOutputs(); ++idx )
      {
      if ( this->GetOutput(idx) && this->GetOutput(idx) != output )
        {
        op = dynamic_cast< ImageBase< ImageDimension >
                           * >( this->GetOutput(idx) );

        if ( op ) { this->GetOutput(idx)->SetRequestedRegion(output); }
        }
      }
    }
}

template< typename TInputImage >
Segmenter< TInputImage >
::Segmenter()
{
  m_Threshold = 0.0;
  m_MaximumFloodLevel = 1.0;
  m_CurrentLabel = 1;
  m_DoBoundaryAnalysis = false;
  m_SortEdgeLists = true;
  m_Connectivity.direction = ITK_NULLPTR;
  m_Connectivity.index = ITK_NULLPTR;
  typename OutputImageType::Pointer img =
    static_cast< OutputImageType * >( this->MakeOutput(0).GetPointer() );
  typename SegmentTableType::Pointer st =
    static_cast< SegmentTableType * >( this->MakeOutput(1).GetPointer() );
  typename BoundaryType::Pointer bd =
    static_cast< BoundaryType * >( this->MakeOutput(2).GetPointer() );
  this->SetNumberOfRequiredOutputs(3);
  this->ProcessObject::SetNthOutput( 0, img.GetPointer() );
  this->ProcessObject::SetNthOutput( 1, st.GetPointer() );
  this->ProcessObject::SetNthOutput( 2, bd.GetPointer() );

  // Allocate memory for connectivity
  m_Connectivity.size = 2 * ImageDimension;
  m_Connectivity.index = new unsigned int[m_Connectivity.size];
  m_Connectivity.direction =
    new typename InputImageType::OffsetType[m_Connectivity.size];
}

template< typename TInputImage >
void
Segmenter< TInputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "SortEdgeLists: " << m_SortEdgeLists << std::endl;
  os << indent << "DoBoundaryAnalysis: " << m_DoBoundaryAnalysis << std::endl;
  os << indent << "Threshold: " << m_Threshold << std::endl;
  os << indent << "MaximumFloodLevel: " << m_MaximumFloodLevel << std::endl;
  os << indent << "CurrentLabel: " << m_CurrentLabel << std::endl;
}
} // end namespace watershed
} // end namespace itk

#endif
