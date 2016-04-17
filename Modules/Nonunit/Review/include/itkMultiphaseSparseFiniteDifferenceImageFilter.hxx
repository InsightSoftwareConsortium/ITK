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
#ifndef itkMultiphaseSparseFiniteDifferenceImageFilter_hxx
#define itkMultiphaseSparseFiniteDifferenceImageFilter_hxx

#include "itkMultiphaseSparseFiniteDifferenceImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
double MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage, TFunction, TIdCell >
::m_ConstantGradientValue = 1.0;

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
const typename MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                                                TOutputImage, TFunction, TIdCell >::ValueType
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage, TFunction, TIdCell >
::m_ValueOne = NumericTraits< typename
                              MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage,
                                                                           TFunction, TIdCell >
                              ::ValueType >::OneValue();

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
const typename MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                                                TOutputImage, TFunction, TIdCell >::ValueType
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage, TFunction, TIdCell >
::m_ValueZero = NumericTraits< typename
                               MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage,
                                                                            TFunction, TIdCell >::
                               ValueType >::ZeroValue();

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
const typename MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                                                TOutputImage, TFunction, TIdCell >::StatusType
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage, TFunction, TIdCell >
::m_StatusNull = NumericTraits< typename
                                MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage,
                                                                             TFunction, TIdCell >::
                                StatusType >::NonpositiveMin();

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
const typename MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                                                TOutputImage, TFunction, TIdCell >::StatusType
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage, TFunction, TIdCell >
::m_StatusChanging = -1;

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
const typename MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                                                TOutputImage, TFunction, TIdCell >::StatusType
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage, TFunction, TIdCell >
::m_StatusActiveChangingUp = -2;

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
const typename MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                                                TOutputImage, TFunction, TIdCell >::StatusType
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage, TFunction, TIdCell >
::m_StatusActiveChangingDown = -3;

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
const typename MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                                                TOutputImage, TFunction, TIdCell >::StatusType
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage, TFunction, TIdCell >
::m_StatusBoundaryPixel = -4;

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage, TFunction, TIdCell >
::MultiphaseSparseFiniteDifferenceImageFilter()
{
  this->m_CurrentFunctionIndex = 0;
  this->m_IsoSurfaceValue = m_ValueZero;
  this->m_BackgroundValue  = NumericTraits< ValueType >::max();
  this->m_NumberOfLayers = ImageDimension;
  this->m_InterpolateSurfaceLocation = true;
  this->m_BoundsCheckingActive = false;
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage, TFunction, TIdCell >
::CopyInputToOutput()
{
  for ( IdCellType i = 0; i < this->m_FunctionCount; i++ )
    {
    InputImagePointer input = this->m_LevelSet[i];

    // This is used as a temporary buffer
    InputImagePointer tempImage = InputImageType::New();
    tempImage->SetRegions( input->GetRequestedRegion() );
    tempImage->CopyInformation(input);
    tempImage->Allocate();

    // Compute Heaviside of input image
    // Copy input to temp
    InputRegionType                       region = input->GetRequestedRegion();
    ImageRegionIterator< InputImageType > lIt(input, region);
    ImageRegionIterator< InputImageType > tIt(tempImage, region);

    lIt.GoToBegin();
    tIt.GoToBegin();

    while ( !lIt.IsAtEnd() )
      {
      tIt.Set( lIt.Get() );
      ++tIt;
      ++lIt;
      }

    // TODO: Can the zeroCrossingFilter have the same input and output?
    ZeroCrossingFilterPointer zeroCrossingFilter = ZeroCrossingFilterType::New();
    zeroCrossingFilter->SetInput(tempImage);
    zeroCrossingFilter->SetBackgroundValue(m_ValueOne);
    zeroCrossingFilter->SetForegroundValue(m_ValueZero);
    zeroCrossingFilter->Update();

    // The levelset image has a 0 where the zero contour exists and + outside
    // and - inside
    ImageRegionIterator< InputImageType > zIt(zeroCrossingFilter->GetOutput(), region);

    lIt.GoToBegin();
    zIt.GoToBegin();

    while ( !lIt.IsAtEnd() )
      {
      if ( zIt.Get() == 0 )
        {
        lIt.Set(0);
        }
      ++zIt;
      ++lIt;
      }
    }
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
typename MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                                      TOutputImage, TFunction, TIdCell >::TimeStepType
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage, TFunction, TIdCell >
::CalculateChange()
{
  // Initialize to the maximum possible value
  TimeStepType     minTimeStep = NumericTraits< TimeStepType >::max();
  TimeStepType     timeStep;
  InputSpacingType spacing = this->m_LevelSet[0]->GetSpacing();

  // Calculate change across all the level-set functions
  for ( IdCellType fId = 0; fId < this->m_FunctionCount; ++fId )
    {
    this->m_CurrentFunctionIndex = fId;

    const FiniteDifferenceFunctionPointer df = this->m_DifferenceFunctions[fId];

    SparseDataStruct *sparsePtr = this->m_SparseData[fId];

    FiniteDifferenceFunctionFloatOffsetType offset;
    ValueType                               gradientMagnitudeSqr,
                                            forward, backward, current;
    const ValueType MIN_NORM      = 1.0e-6;

    void *globalData = df->GetGlobalDataPointer();

    NeighborhoodIterator< InputImageType > outputIt ( df->GetRadius(),
                                                      this->m_LevelSet[fId], this->m_LevelSet[fId]->GetRequestedRegion() );

    if ( m_BoundsCheckingActive == false )
      {
      outputIt.NeedToUseBoundaryConditionOff();
      }

    sparsePtr->m_UpdateBuffer.clear();
    sparsePtr->m_UpdateBuffer.reserve ( sparsePtr->m_Layers[0]->Size() );

    // Calculates the update values for the active layer indices in this
    // iteration.  Iterates through the active layer index list by evaluating
    // the update to the output image (level set image) at each
    // index.  Update values are stored in the update buffer.
    LayerConstIterator layerIt = sparsePtr->m_Layers[0]->Begin();

    unsigned int j;

    while ( layerIt != sparsePtr->m_Layers[0]->End() )
      {
      outputIt.SetLocation (layerIt->m_Value);

      current = outputIt.GetCenterPixel();

      // Calculate the offset to the surface from the current of this
      // neighborhood.  This is used by some level set functions in sampling a
      // speed, advection, or curvature term.
      if ( this->GetInterpolateSurfaceLocation() && current != 0.0 )
        {
        // Surface is at the zero crossing, so distance to surface is:
        // phi(x) / norm(grad(phi)), where phi(x) is the current of the
        // neighborhood.  The location is therefore
        // (i,j,k) - ( phi(x)/ norm(grad(phi))) * (grad(phi(x)) /
        // norm(grad(phi)) )
        gradientMagnitudeSqr = 0.0;

        for ( j = 0; j < ImageDimension; ++j )
          {
          forward  = outputIt.GetNext (j);
          backward = outputIt.GetPrevious (j);

          if ( forward * backward >= 0 )
            {
            //  Neighbors are same sign OR at least one neighbor is zero.
            // Pick the larger magnitude derivative.
            if ( itk::Math::abs (forward - current) > itk::Math::abs(current - backward) )
              {
              offset[j] = ( forward - current ) / spacing[j];
              }
            else
              {
              offset[j] = ( current - backward ) / spacing[j];
              }
            }
          else //Neighbors are opposite sign, pick the direction of 0 surface.
            {
            if ( forward * current < 0 )
              {
              offset[j] = ( forward - current ) / spacing[j];
              }
            else
              {
              offset[j] = ( current - backward ) / spacing[j];
              }
            }

          gradientMagnitudeSqr += offset[j] * offset[j];
          }

        // Adding sqrt imagedimension "extends the reach" of the
        // interpolation
        // to surfaces that pass close to the current of cells.  This is a
        // heuristic fudge factor that improves interpolation and reduces
        // "wiggling" at convergence.
        ValueType coeff = current * std::sqrt(ImageDimension
                                             + 0.5) / ( gradientMagnitudeSqr + MIN_NORM );

        for ( j = 0; j < ImageDimension; ++j )
          {
          offset[j] *= coeff;
          }

        sparsePtr->m_UpdateBuffer.push_back ( df->ComputeUpdate (outputIt, globalData, offset) );
        }
      else // Don't do interpolation
        {
        sparsePtr->m_UpdateBuffer.push_back ( df->ComputeUpdate (outputIt, globalData) );
        }

      ++layerIt;
      }

    // Ask the finite difference function to compute the time step for
    // this iteration.  We give it the global data pointer to use, then
    // ask it to free the global data memory.
    timeStep = df->ComputeGlobalTimeStep (globalData);
    df->ReleaseGlobalDataPointer (globalData);

    if ( timeStep < minTimeStep )
      {
      minTimeStep = timeStep;
      }
    }

  minTimeStep = 0.2; //FIXME finally assigned to a constant

  return minTimeStep;
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                             TOutputImage,
                                             TFunction, TIdCell >
::ApplyUpdate(TimeStepType dt)
{
  unsigned int j, k;

  for ( IdCellType fId = 0; fId < this->m_FunctionCount; ++fId )
    {
    this->m_CurrentFunctionIndex = fId;

    SparseDataStruct *sparsePtr = this->m_SparseData[fId];
    unsigned int      t;

    StatusType up_to, up_search;
    StatusType down_to, down_search;

    LayerPointerType UpList[2];
    LayerPointerType DownList[2];

    for ( j = 0; j < 2; ++j )
      {
      UpList[j]   = LayerType::New();
      DownList[j] = LayerType::New();
      }

    // Process the active layer. This step will update the values in the
    // active layer as well as the values at indices that *will* become part
    // of the active layer when they are promoted/demoted. Also records
    // promotions, demotions in the UpList[0] and DownList[0] for current
    // active layer indices (i.e. those indices which will move inside or
    // outside
    // the active layers).
    this->UpdateActiveLayerValues(dt, UpList[0], DownList[0]);

    // Process the up/down lists. This is an iterative process which
    // proceeds outwards from the active layer. Each iteration generates the
    // list for the next iteration.

    // First process the status lists generated on the active layer.
    this->ProcessStatusList (UpList[0], UpList[1], 2, 1);
    this->ProcessStatusList (DownList[0], DownList[1], 1, 2);

    down_to = up_to = 0;
    up_search       = 3;
    down_search     = 4;
    j = 1;
    k = 0;
    while ( down_search < static_cast< StatusType >( sparsePtr->m_Layers.size() ) )
      {
      this->ProcessStatusList(UpList[j], UpList[k], up_to, up_search);
      this->ProcessStatusList(DownList[j], DownList[k], down_to, down_search);

      if ( up_to == 0 )
        {
        up_to += 1;
        }
      else
        {
        up_to += 2;
        }

      down_to += 2;

      up_search += 2;
      down_search += 2;

      // Swap the lists so we can re-use the empty one.
      t = j;
      j = k;
      k = t;
      }

    // Process the outermost inside/outside layers in the sparse field.
    this->ProcessStatusList(UpList[j], UpList[k], up_to, m_StatusNull);
    this->ProcessStatusList(DownList[j], DownList[k], down_to, m_StatusNull);

    // Now we are left with the lists of indices which must be
    // brought into the outermost layers.  Bring UpList into last inside layer
    // and DownList into last outside layer.
    this->ProcessOutsideList (UpList[k], static_cast< signed char >(
                                sparsePtr->m_Layers.size() ) - 2);
    this->ProcessOutsideList (DownList[k], static_cast< signed char >(
                                sparsePtr->m_Layers.size() ) - 1);

    // Finally, we update all of the layer values (excluding the active layer,
    // which has already been updated).
    this->PropagateAllLayerValues();
    }

  this->m_CurrentFunctionIndex = 0;
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage,
          typename TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                             TOutputImage,
                                             TFunction, TIdCell >
::ProcessOutsideList(LayerType *OutsideList, StatusType ChangeToStatus)
{
  SparseDataStruct *sparsePtr = this->m_SparseData[this->m_CurrentFunctionIndex];
  LayerNodeType *   node;

  // Push each index in the input list into its appropriate status layer
  // (ChangeToStatus) and update the status image value at that index.
  while ( !OutsideList->Empty() )
    {
    sparsePtr->m_StatusImage->SetPixel (OutsideList->Front()->m_Value, ChangeToStatus);
    node = OutsideList->Front();
    OutsideList->PopFront();
    sparsePtr->m_Layers[ChangeToStatus]->PushFront (node);
    }
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                             TOutputImage,
                                             TFunction, TIdCell >
::ProcessStatusList(LayerType *InputList, LayerType *OutputList,
                    StatusType ChangeToStatus, StatusType SearchForStatus)
{
  SparseDataStruct *sparsePtr = this->m_SparseData[this->m_CurrentFunctionIndex];

  unsigned int   i;
  bool           bounds_status;
  LayerNodeType *node;
  StatusType     neighbor_status;

  NeighborhoodIterator< StatusImageType > statusIt (
    m_NeighborList.GetRadius(), sparsePtr->m_StatusImage,
    this->m_LevelSet[this->m_CurrentFunctionIndex]->GetRequestedRegion() );

  if ( !m_BoundsCheckingActive )
    {
    statusIt.NeedToUseBoundaryConditionOff();
    }

  // Push each index in the input list into its appropriate status layer
  // (ChangeToStatus) and update the status image value at that index.
  // Also examine the neighbors of the index to determine which need to go
  // onto the output list (search for SearchForStatus).
  while ( !InputList->Empty() )
    {
    statusIt.SetLocation (InputList->Front()->m_Value);
    statusIt.SetCenterPixel (ChangeToStatus);

    node = InputList->Front();  // Must unlink from the input list
    InputList->PopFront();      // before transferring to another list.
    sparsePtr->m_Layers[ChangeToStatus]->PushFront (node);

    // Iterate through the neighbors of this status-changed node
    for ( i = 0; i < m_NeighborList.GetSize(); ++i )
      {
      neighbor_status = statusIt.GetPixel (
        m_NeighborList.GetArrayIndex (i) );

      // Have we bumped up against the boundary?  If so, turn on bounds
      // checking.
      if ( neighbor_status == m_StatusBoundaryPixel )
        {
        m_BoundsCheckingActive = true;
        }

      // Find neighbors that move into the list prior to the ChangeToStatus
      if ( neighbor_status == SearchForStatus )
        {
        // mark this pixel so we don't add it twice.
        statusIt.SetPixel (m_NeighborList.GetArrayIndex (i),
                           m_StatusChanging, bounds_status);
        if ( bounds_status == true )
          {
          node = sparsePtr->m_LayerNodeStore->Borrow();
          node->m_Value = statusIt.GetIndex()
                          + m_NeighborList.GetNeighborhoodOffset (i);
          OutputList->PushFront (node);
          } // else this index was out of bounds.
        }
      }
    }
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                             TOutputImage,
                                             TFunction, TIdCell >
::UpdateActiveLayerValues(TimeStepType dt, LayerType *UpList, LayerType
                          *DownList)
{
  SparseDataStruct *sparsePtr = this->m_SparseData[this->m_CurrentFunctionIndex];

  // This method scales the update buffer values by the time step and adds
  // them to the active layer pixels.  New values at an index which fall
  // outside of the active layer range trigger that index to be placed on the
  // "up" or "down" status list.  The neighbors of any such index are then
  // assigned new values if they are determined to be part of the active list
  // for the next iteration (i.e. their values will be raised or lowered into
  // the active range).

  // These need to take into account the spacing ??
  const ValueType LOWER_ACTIVE_THRESHOLD = -( m_ConstantGradientValue / 2.0 );
  const ValueType UPPER_ACTIVE_THRESHOLD =  m_ConstantGradientValue / 2.0;

  ValueType      new_value, temp_value;
  LayerNodeType *node, *release_node;
  StatusType     neighbor_status;
  unsigned int   i, idx;
  bool           bounds_status, flag;

  LayerIterator             layerIt;
  UpdateBufferConstIterator updateIt;

  NeighborhoodIterator< InputImageType >
  outputIt ( m_NeighborList.GetRadius(),
             this->m_LevelSet[this->m_CurrentFunctionIndex],
             this->m_LevelSet[this->m_CurrentFunctionIndex]->GetRequestedRegion() );

  NeighborhoodIterator< StatusImageType >
  statusIt ( m_NeighborList.GetRadius(),
             sparsePtr->m_StatusImage,
             this->m_LevelSet[this->m_CurrentFunctionIndex]->GetRequestedRegion() );

  // If bounds checking is turned on
  if ( !m_BoundsCheckingActive )
    {
    outputIt.NeedToUseBoundaryConditionOff();
    statusIt.NeedToUseBoundaryConditionOff();
    }

  // Iterate over the update buffer and active layer
  // Both are the same size
  layerIt = sparsePtr->m_Layers[0]->Begin();
  updateIt = sparsePtr->m_UpdateBuffer.begin();

  while ( layerIt != sparsePtr->m_Layers[0]->End() )
    {
    outputIt.SetLocation (layerIt->m_Value);
    statusIt.SetLocation (layerIt->m_Value);

    new_value = this->CalculateUpdateValue (layerIt->m_Value,
                                            dt, outputIt.GetCenterPixel(), *updateIt);

    // If this index needs to be moved to another layer, then search its
    // neighborhood for indices that need to be pulled up/down into the
    // active layer. Set those new active layer values appropriately,
    // checking first to make sure they have not been set by a more
    // influential neighbor.

    //   ...But first make sure any neighbors in the active layer are not
    // moving to a layer in the opposite direction.  This step is necessary
    // to avoid the creation of holes in the active layer.  The fix is simply
    // to not change this value and leave the index in the active set.

    if ( new_value >= UPPER_ACTIVE_THRESHOLD )
      {
      // This index will move UP into a positive (outside) layer. Contour is
      // shrinking
      // into the negative layers.

      // First check for neighbors that belong to the active layer and moving
      // in the opposite direction.
      flag = false;
      for ( i = 0; i < m_NeighborList.GetSize(); ++i )
        {
        if ( statusIt.GetPixel( m_NeighborList.GetArrayIndex(i) )
             == m_StatusActiveChangingDown )
          {
          flag = true;
          break;
          }
        }

      if ( flag == true )
        {
        ++layerIt;
        ++updateIt;
        continue;
        }

      // Search the neighborhood for inside indices.
      for ( i = 0; i < m_NeighborList.GetSize(); ++i )
        {
        temp_value = new_value - m_ConstantGradientValue * m_PixelDistance[i];
        idx = m_NeighborList.GetArrayIndex (i);
        neighbor_status = statusIt.GetPixel (idx);
        // 1 is first negative layer that will come into the active layer
        if ( neighbor_status == 1 )
          {
          // Keep the smallest possible value for the new active node.  This
          // places the new active layer node closest to the zero level-set.
          if ( outputIt.GetPixel (idx) < LOWER_ACTIVE_THRESHOLD
               || itk::Math::abs (temp_value) < itk::Math::abs (
                 outputIt.GetPixel (idx) ) )
            {
            UpdatePixel (this->m_CurrentFunctionIndex, idx, outputIt, temp_value, bounds_status);
            }
          }
        }
      // Push current active layer pixel into the uplist
      node = sparsePtr->m_LayerNodeStore->Borrow();
      node->m_Value = layerIt->m_Value;
      UpList->PushFront (node);
      statusIt.SetCenterPixel (m_StatusActiveChangingUp);

      // Now remove this pixel from the active list.
      release_node = layerIt.GetPointer();
      sparsePtr->m_Layers[0]->Unlink (release_node);
      sparsePtr->m_LayerNodeStore->Return (release_node);
      }

    else if ( new_value < LOWER_ACTIVE_THRESHOLD )
      {
      // This index will move DOWN into a negative (inside) layer. 2 in the
      // positive sparse field will come in

      // First check for active layer neighbors moving in the opposite
      // direction.
      flag = false;
      for ( i = 0; i < m_NeighborList.GetSize(); ++i )
        {
        if ( statusIt.GetPixel( m_NeighborList.GetArrayIndex(i) )
             == m_StatusActiveChangingUp )
          {
          flag = true;
          break;
          }
        }

      if ( flag == true )
        {
        ++layerIt;
        ++updateIt;
        continue;
        }

      // Search the neighborhood for outside indices.
      for ( i = 0; i < m_NeighborList.GetSize(); ++i )
        {
        temp_value = new_value + m_ConstantGradientValue * m_PixelDistance[i];
        idx = m_NeighborList.GetArrayIndex (i);
        neighbor_status = statusIt.GetPixel (idx);
        if ( neighbor_status == 2 )
          {
          // Keep the smallest magnitude value for this active set node.  This
          // places the node closest to the active layer.
          if ( outputIt.GetPixel (idx) >= UPPER_ACTIVE_THRESHOLD
               || itk::Math::abs (temp_value) < itk::Math::abs (
                 outputIt.GetPixel (idx) ) )
            {
            UpdatePixel (this->m_CurrentFunctionIndex, idx, outputIt, temp_value, bounds_status);
            }
          }
        }
      // Push current active layer pixel into the downlist
      node = sparsePtr->m_LayerNodeStore->Borrow();
      node->m_Value = layerIt->m_Value;
      DownList->PushFront (node);
      statusIt.SetCenterPixel (m_StatusActiveChangingDown);

      // Now remove this index from the active list.
      release_node = layerIt.GetPointer();
      sparsePtr->m_Layers[0]->Unlink (release_node);
      sparsePtr->m_LayerNodeStore->Return (release_node);
      }
    else
      {
      UpdatePixel(this->m_CurrentFunctionIndex, outputIt.Size() / 2, outputIt, new_value, bounds_status);
      }

    // Move to the next active layer pixel
    ++layerIt;
    ++updateIt;
    }
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                             TOutputImage,
                                             TFunction, TIdCell >
::InitializeActiveLayerValues()
{
  // Initialize all active layer pixels to values computed as distance
  // to the 0 contour. Similar to the fast marching initial seeds.

  const ValueType  MIN_NORM      = 1.0e-6;
  InputSpacingType spacing = this->m_LevelSet[0]->GetSpacing();

  double temp;

  for ( IdCellType i = 0; i < this->m_FunctionCount; i++ )
    {
    SparseDataStruct *sparsePtr = this->m_SparseData[i];

    InputImagePointer levelset = this->m_LevelSet[i];

    typename LayerType::ConstIterator activeIt;
    ConstNeighborhoodIterator< InputImageType > outputIt (
      m_NeighborList.GetRadius(),
      levelset, levelset->GetRequestedRegion() );

    sparsePtr->m_UpdateBuffer.clear();
    sparsePtr->m_UpdateBuffer.reserve ( sparsePtr->m_Layers[0]->Size() );

    unsigned int center; // index to active layer pixel
    center = outputIt.Size() / 2;
    ValueType dx, gradientMagnitude, gradientMagnitudeSqr,
              distance, forward, current, backward;

    // For all indices in the active layer...
    activeIt = sparsePtr->m_Layers[0]->Begin();
    while ( activeIt != sparsePtr->m_Layers[0]->End() )
      {
      // Interpolate on the (shifted) input image values at this index to
      // assign an active layer value in the output image.
      outputIt.SetLocation (activeIt->m_Value);

      gradientMagnitudeSqr = m_ValueZero;

      for ( unsigned int j = 0; j < ImageDimension; ++j )
        {
        // Compute forward and backward pixel values
        forward = outputIt.GetPixel ( center + m_NeighborList.GetStride(j) );
        current = outputIt.GetCenterPixel();
        backward = outputIt.GetPixel ( center - m_NeighborList.GetStride (j) );

        if ( forward * backward >= 0 )
          {
          //  Neighbors are same sign OR at least one neighbor is zero.
          // Pick the larger magnitude derivative.
          if ( ::itk::Math::abs (forward - center) > ::itk::Math::abs(center - backward) )
            {
            dx = ( forward - current ) / spacing[j];
            }
          else
            {
            dx = ( current - backward ) / spacing[j];
            }
          }
        else
          {
          // Choose the derivative closest to the 0 contour
          if ( itk::Math::sgn(current * forward) == -1 )
            {
            dx = ( forward - current ) / spacing[j];
            }
          else
            {
            dx = ( current - backward ) / spacing[j];
            }
          }
        gradientMagnitudeSqr += dx * dx;
        }
      gradientMagnitude = std::sqrt (gradientMagnitudeSqr) + MIN_NORM;

      // Compute the correct distance as phi(x)/gradientMagnitude
      distance = outputIt.GetCenterPixel() / gradientMagnitude;

      // Insert in the update buffer
      sparsePtr->m_UpdateBuffer.push_back(
        std::min (std::max (-MIN_NORM, distance),
                      MIN_NORM) );
      ++activeIt;
      }

    // Update the level-set image using the update buffer
    activeIt = sparsePtr->m_Layers[0]->Begin();
    while ( activeIt != sparsePtr->m_Layers[0]->End() )
      {
      // Update the accumulator value using the update buffer
      temp = static_cast< double >( sparsePtr->m_UpdateBuffer.front()
                                    - levelset->GetPixel (activeIt->m_Value) );
      m_RMSSum += temp * temp;
      m_RMSCounter++;

      levelset->SetPixel ( activeIt->m_Value, sparsePtr->m_UpdateBuffer.front() );
      ++activeIt;
      }
    }
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                             TOutputImage,
                                             TFunction, TIdCell >
::PropagateAllLayerValues()
{
  for ( IdCellType i = 0; i < this->m_FunctionCount; i++ )
    {
    // Calls the UpdatePixel(...) function inside
    PropagateFunctionLayerValues (i);
    }
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                             TOutputImage,
                                             TFunction, TIdCell >
::PropagateFunctionLayerValues(unsigned int fId)
{
  SparseDataStruct *sparsePtr = this->m_SparseData[fId];

  // Update values in the first inside and first outside layers using the
  // active layer as a seed. Inside layers are odd numbers, outside layers are
  // even numbers.

  this->PropagateLayerValues (sparsePtr, 0, 1, 3, 1);   // first inside
  this->PropagateLayerValues (sparsePtr, 0, 2, 4, 2);   // first outside

  // Update the rest of the layers.
  for ( unsigned int i = 1; i < sparsePtr->m_Layers.size() - 2; ++i )
    {
    this->PropagateLayerValues (sparsePtr, i, i + 2, i + 4, ( i + 2 ) % 2);
    }
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                             TOutputImage,
                                             TFunction, TIdCell >
::PropagateLayerValues(SparseDataStruct *sparsePtr, StatusType from,
                       StatusType to, StatusType promote, int InOrOut)
{
  // InOrOut indicates whether we are propagating in the negative/positive
  // region
  // of the level-set function

  unsigned int   i;
  ValueType      value_temp, delta;
  ValueType      value = NumericTraits< ValueType >::ZeroValue(); // warnings
  bool           found_neighbor_flag;
  LayerIterator  toIt;
  LayerNodeType *node;

  StatusType past_end = static_cast< StatusType >( sparsePtr->m_Layers.size() ) - 1;

  // Are we propagating values inward (-1, more negative) or outward (1, more
  // positive)?
  delta = ( InOrOut == 1 ) ? -1 : 1;

  NeighborhoodIterator< InputImageType >
  outputIt ( m_NeighborList.GetRadius(),
             this->m_LevelSet[sparsePtr->m_Index],
             this->m_LevelSet[sparsePtr->m_Index]->GetRequestedRegion() );
  NeighborhoodIterator< StatusImageType >
  statusIt ( m_NeighborList.GetRadius(), sparsePtr->m_StatusImage,
             this->m_LevelSet[sparsePtr->m_Index]->GetRequestedRegion() );

  if ( !m_BoundsCheckingActive )
    {
    outputIt.NeedToUseBoundaryConditionOff();
    statusIt.NeedToUseBoundaryConditionOff();
    }

  // Iterate over the to-layer to fill the values in the output image
  toIt  = sparsePtr->m_Layers[to]->Begin();
  while ( toIt != sparsePtr->m_Layers[to]->End() )
    {
    // Set the iterator location in the status image
    OutputIndexType indexCurrent = toIt->m_Value;
    statusIt.SetLocation (indexCurrent);

    // Is this index marked for deletion? If the status image has
    // been marked with another layer's value, we need to delete this node
    // from the current list then skip to the next iteration.
    if ( statusIt.GetCenterPixel() != to )
      {
      node = toIt.GetPointer();
      ++toIt;
      sparsePtr->m_Layers[to]->Unlink (node);
      sparsePtr->m_LayerNodeStore->Return (node);
      continue;
      }

    // Set the iterator location in the level-set image
    outputIt.SetLocation (toIt->m_Value);

    // We explore all neighbors to identify the closest from-layer node
    found_neighbor_flag = false;
    unsigned int indexNeighbor;
    for ( i = 0; i < m_NeighborList.GetSize(); ++i )
      {
      // If this neighbor is in the "from" list, compare its absolute value
      // to any previous values found in the "from" list.  Keep the value
      // that will cause the to-layer to be closest to the zero level set.
      indexNeighbor = m_NeighborList.GetArrayIndex (i);  // Get index
      if ( statusIt.GetPixel (indexNeighbor) == from )   // if belongs to
                                                         // from-layer
        {
        // This value should be a distance in terms of spacing with neighbors
        // plus its current value

        InputPointType p1, p2;
        ValueType      dist = 0; // compute the distance between neighbors
        this->m_LevelSet[sparsePtr->m_Index]->TransformIndexToPhysicalPoint(
          statusIt.GetIndex(indexNeighbor), p1);
        this->m_LevelSet[sparsePtr->m_Index]->TransformIndexToPhysicalPoint(
          indexCurrent, p2);
        for ( unsigned int j = 0; j < ImageDimension; j++ )
          {
          dist += ( p1[j] - p2[j] ) * ( p1[j] - p2[j] );
          }
        dist = delta * std::sqrt(dist);

        value_temp = dist + outputIt.GetPixel (indexNeighbor);   // grab its
                                                                 // value

        if ( !found_neighbor_flag )
          {
          value = value_temp;
          }
        else
          {
          // Irrespective of negative/positive region, select the lowest
          // absolute minimum
          //value = delta * std::min( itk::Math::abs( value_temp ),
          // itk::Math::abs( value ) );
          if ( InOrOut == 1 ) // inward
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
            if ( value_temp < value )
              {
              value = value_temp;
              }
            }
          }
        found_neighbor_flag = true;
        }
      }

    if ( found_neighbor_flag )
      {
      // Set the new value using the smallest distance
      // found in our "from" neighbors.
      bool         bounds_status;
      unsigned int center = outputIt.Size() / 2;

      UpdatePixel(sparsePtr->m_Index, center, outputIt, value, bounds_status);

      // Update the rms change
      m_RMSSum += ( value - outputIt.GetCenterPixel() ) * ( value - outputIt.GetCenterPixel() );
      m_RMSCounter++;

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
      sparsePtr->m_Layers[to]->Unlink (node);
      if ( promote > past_end )
        {
        sparsePtr->m_LayerNodeStore->Return (node);
        // Reset the pixel status to null -- does not belong to sparse layer
        statusIt.SetCenterPixel (m_StatusNull);
        // Set the pixel to its default background value
        this->m_LevelSet[sparsePtr->m_Index]->SetPixel(indexCurrent,
                                                       delta * this->m_BackgroundValue);
        }
      else
        {
        sparsePtr->m_Layers[promote]->PushFront (node);
        statusIt.SetCenterPixel (promote);
        }
      }
    }
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage, TFunction, TIdCell >
::InitializeIteration()
{
  Superclass::InitializeIteration();

  m_RMSSum = 0.;
  m_RMSCounter = 0; // counter

  // Set the values in the output image for the active layer.
  this->InitializeActiveLayerValues();

  // Initialize layer values using the active layer as seeds
  this->PropagateAllLayerValues();

  // Determine the average RMS of change during this iteration
  if ( m_RMSCounter == 0 )
    {
    this->SetRMSChange ( static_cast< double >( 0. ) );
    }
  else
    {
    this->SetRMSChange ( std::sqrt (m_RMSSum / m_RMSCounter) );
    }
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage, TFunction, TIdCell >
::Initialize()
{
  // Initialize m_PixelDistance values for the corresponding neighborhood list
  // This stores the distance between neighbors. Usually same as 1 except when
  // the image spacing is different.
  InputSpacingType spacing = this->m_LevelSet[0]->GetSpacing();
  OffsetType       offset;

  this->m_PixelDistance.clear();
  this->m_PixelDistance.resize ( m_NeighborList.GetSize() );
  for ( unsigned int i = 0; i < m_NeighborList.GetSize(); ++i )
    {
    offset = m_NeighborList.GetNeighborhoodOffset (i);
    m_PixelDistance[i] = 0;
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      m_PixelDistance[i] += offset[j] * spacing[j] * offset[j] * spacing[j];
      }
    m_PixelDistance[i] = std::sqrt(m_PixelDistance[i]);
    }

  for ( IdCellType fId = 0; fId < this->m_FunctionCount; ++fId )
    {
    SparseDataStruct *sparsePtr = this->m_SparseData[fId];

    // Allocate the status image.
    sparsePtr->m_StatusImage = StatusImageType::New();
    sparsePtr->m_StatusImage->SetRegions (
      this->m_LevelSet[fId]->GetRequestedRegion() );
    sparsePtr->m_StatusImage->CopyInformation(this->m_LevelSet[fId]);
    sparsePtr->m_StatusImage->Allocate();
    sparsePtr->m_StatusImage->FillBuffer(m_StatusNull);  //NonpositiveMin

    // Initialize the boundary pixels in the status image to
    // m_StatusBoundaryPixel values.  Uses the face calculator to find all of
    // the region faces.
    BFCType faceCalculator;
    typename BFCType::FaceListType faceList;

    // Set the difference function radius here
    typename BFCType::SizeType sz = this->m_DifferenceFunctions[fId]->GetRadius();
    typename BFCType::FaceListType::iterator fit;

    // Compute the boundary pixel regions set in a container
    faceList = faceCalculator (sparsePtr->m_StatusImage,
                               sparsePtr->m_StatusImage->GetRequestedRegion(), sz);

    // Iterate over the boundary region sets
    fit = faceList.begin();
    for ( ++fit; fit != faceList.end(); ++fit )
      {
      // For each region, set the pixel in m_StatusImage to
      // m_StatusBoundaryPixel
      ImageRegionIterator< StatusImageType > statusIt(sparsePtr->m_StatusImage, *fit);

      statusIt.GoToBegin();
      while ( !statusIt.IsAtEnd() )
        {
        statusIt.Set (m_StatusBoundaryPixel);
        ++statusIt;
        }
      }

    // Erase all existing layer lists -- element by element
    for ( unsigned int i = 0; i < sparsePtr->m_Layers.size(); ++i )
      {
      while ( !sparsePtr->m_Layers[i]->Empty() )
        {
        sparsePtr->m_LayerNodeStore->Return( sparsePtr->m_Layers[i]->Front() );
        sparsePtr->m_Layers[i]->PopFront();
        }
      }

    // Allocate the layers for the sparse field.
    sparsePtr->m_Layers.clear();
    sparsePtr->m_Layers.reserve(2 * this->m_NumberOfLayers + 1);

    while ( sparsePtr->m_Layers.size() < ( 2 * this->m_NumberOfLayers + 1 ) )
      {
      sparsePtr->m_Layers.push_back( LayerType::New() );
      }

    // Throw an exception if we don't have enough layers.
    if ( sparsePtr->m_Layers.size() < 3 )
      {
      itkExceptionMacro (<< "Not enough layers have been allocated for the"
                            "sparse field.  Requires at least one layer.");
      }
    }

  // Set the background constants required to be set outside the sparse layer
  this->InitializeBackgroundConstants();

  // Construct the active layer and initialize the first layers inside and
  // outside of the active layer for all level-set functions.
  this->ConstructActiveLayer();

  for ( IdCellType fId = 0; fId < this->m_FunctionCount; ++fId )
    {
    SparseDataStruct *sparsePtr = this->m_SparseData[fId];

    // Construct the rest of the non-active set layers using the first two
    // layers. Inside layers are odd numbers, outside layers are even numbers.
    // We need to loop from i = 1 to m_Layers.size()-2 since the last two layers
    // are constructed in the previous iteration
    for ( unsigned int i = 1; i < sparsePtr->m_Layers.size() - 2; ++i )
      {
      // Construct layer i+2 from layer i. Note that layer i+1 is on the other
      // side
      this->ConstructLayer(sparsePtr, i, i + 2);
      }
    }

  // Set the values in the output image for the active layer.
  this->InitializeActiveLayerValues();

  // Initialize layer values using the active layer as seeds
  this->PropagateAllLayerValues();

  // Initialize pixels outside the sparse field layers to positive
  // and negative values, respectively. This is not necessary for the
  // calculations, but is useful for presenting a more intuitive output to the
  // filter.  See PostProcessOutput method for more information.
  this->InitializeBackgroundPixels();
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage, TFunction, TIdCell >
::InitializeBackgroundConstants()
{
  // Determine the maximum spacing to set the background pixel values
  // outside the sparse field
  float            maxSpacing = NumericTraits< float >::min();
  InputSpacingType spacing = this->m_LevelSet[0]->GetSpacing();

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    maxSpacing = std::max( maxSpacing, static_cast< float >( spacing[i] ) );
    }

  // Assign background pixels OUTSIDE the sparse field layers to a new level
  // set with value greater than the outermost layer.  Assign background pixels
  // INSIDE the sparse field layers to a new level set with value less than
  // the innermost layer.
  const ValueType max_layer = static_cast< ValueType >( this->m_NumberOfLayers );

  this->m_BackgroundValue  = ( max_layer + 1 ) * maxSpacing;
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage, TFunction, TIdCell >
::InitializeBackgroundPixels()
{
  for ( IdCellType fId = 0; fId < this->m_FunctionCount; fId++ )
    {
    SparseDataStruct *sparsePtr = this->m_SparseData[fId];

    ImageRegionConstIterator< StatusImageType > statusIt (
      sparsePtr->m_StatusImage,
      this->m_LevelSet[fId]->GetRequestedRegion() );

    ImageRegionIterator< InputImageType > outputIt (
      this->m_LevelSet[fId],
      this->m_LevelSet[fId]->GetRequestedRegion() );

    outputIt.GoToBegin();
    statusIt.GoToBegin();

    while ( !outputIt.IsAtEnd() )
      {
      if ( statusIt.Get() == m_StatusNull || statusIt.Get() ==
           m_StatusBoundaryPixel )
        {
        if ( outputIt.Get() > 0 )
          {
          outputIt.Set (this->m_BackgroundValue);
          }
        if ( outputIt.Get() < 0 )
          {
          outputIt.Set (-this->m_BackgroundValue);
          }
        }
      ++outputIt;
      ++statusIt;
      }
    }
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage, TFunction, TIdCell >
::ConstructActiveLayer()
{
  // We construct active layers for all level-set functions
  for ( IdCellType fId = 0; fId < this->m_FunctionCount; fId++ )
    {
    SparseDataStruct *sparsePtr = this->m_SparseData[fId];

    //  We find the active layer by searching for 0's in the zero crossing
    //  image (output image).  The first inside and outside layers are also
    //  constructed by searching the neighbors of the active layer in the
    //  (shifted) input image. Negative neighbors not in the active set are
    //  assigned to the inside, positive neighbors are assigned to the outside.
    //
    //  During construction we also check whether any of the layers of the
    //  active set (or the active set itself) is sitting on a boundary pixel
    //  location. If this is the case, then we need to do active bounds
    //  checking in the solver.

    NeighborhoodIterator< InputImageType >
    outputIt ( m_NeighborList.GetRadius(),
               this->m_LevelSet[fId],
               this->m_LevelSet[fId]->GetRequestedRegion() );

    NeighborhoodIterator< StatusImageType >
    statusIt ( m_NeighborList.GetRadius(),
               sparsePtr->m_StatusImage,
               this->m_LevelSet[fId]->GetRequestedRegion() );

    InputIndexType center_index, offset_index;
    LayerNodeType *node;
    bool           bounds_status;
    StatusType     layer_number;

    // Determine image bounds for checking if sparse layers touch boundaries
    InputIndexType lowerBounds;
    InputSizeType  upperBounds;
    lowerBounds = this->m_LevelSet[fId]->GetRequestedRegion().GetIndex();
    upperBounds = this->m_LevelSet[fId]->GetRequestedRegion().GetSize();

    // Iterate over the output image
    outputIt.GoToBegin();
    while ( !outputIt.IsAtEnd() )
      {
      // Check if the center pixel has a value 0. The zeroCrossingFilter has
      // already placed 0s on the active layer pixels and 1 everywhere else.
      if ( outputIt.GetCenterPixel() == m_ValueZero )
        {
        // Grab the neighborhood in the status image.
        center_index = outputIt.GetIndex();

        statusIt.SetLocation (center_index);

        // Check to see if any of the sparse field touches a boundary.  If so,
        // then activate bounds checking.
        for ( unsigned int i = 0; i < ImageDimension; i++ )
          {
          if ( ( center_index[i] + static_cast< InputOffsetValueType >(
                   this->m_NumberOfLayers ) >= ( static_cast< InputIndexValueType >( upperBounds[i] ) - 1 ) )
               || center_index[i] - static_cast< InputOffsetValueType >(
                 this->m_NumberOfLayers ) <= static_cast< InputIndexValueType >( lowerBounds[i] ) )
            {
            m_BoundsCheckingActive = true;
            }
          }

        // Borrow a node from the store and set its value.
        node = sparsePtr->m_LayerNodeStore->Borrow();
        node->m_Value = center_index;

        // Add the node to the active list and set the status in the status
        // image.
        sparsePtr->m_Layers[0]->PushFront (node);
        statusIt.SetCenterPixel (0);

        // Search the neighborhood pixels for first inside & outside layer
        // members.  Construct these lists and set status list values.
        for ( unsigned int i = 0; i < m_NeighborList.GetSize(); ++i )
          {
          // If the neighborhood pixel is not on the active layer
          // determine its sign to assign to outside or inside layers
          unsigned int neighborIndex = m_NeighborList.GetArrayIndex(i);
          if ( outputIt.GetPixel(neighborIndex) != m_ValueZero )
            {
            // Determine if the neighbor belongs to layer 1 (inside) or 2
            // (outside)
            layer_number = ( outputIt.GetPixel (neighborIndex) > 0 ) ? 2 : 1;

            // This check is to prevent the same pixel from being included more
            // than once
            // in the list
            if ( statusIt.GetPixel(neighborIndex) == m_StatusNull )
              {
              statusIt.SetPixel (neighborIndex, layer_number, bounds_status);

              if ( bounds_status ) // In bounds.
                {
                offset_index = center_index + m_NeighborList.GetNeighborhoodOffset (i);
                node = sparsePtr->m_LayerNodeStore->Borrow();
                node->m_Value = offset_index;
                sparsePtr->m_Layers[layer_number]->PushFront (node);
                } // else do nothing.
              }
            }
          }
        }
      ++outputIt;
      }
    }
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage, TFunction, TIdCell >
::ConstructLayer(SparseDataStruct *sparsePtr, StatusType from, StatusType to)
{
  LayerNodeType *node;
  bool           boundary_status;

  NeighborhoodIterator< StatusImageType > statusIt (
    m_NeighborList.GetRadius(), sparsePtr->m_StatusImage,
    this->m_LevelSet[sparsePtr->m_Index]->GetRequestedRegion() );

  typename LayerType::ConstIterator fromIt;
  fromIt = sparsePtr->m_Layers[from]->Begin();

  // For all indices in the "from" layer...
  while ( fromIt != sparsePtr->m_Layers[from]->End() )
    {
    // Search the neighborhood of this index in the status image for
    // unassigned indices. Push those indices onto the "to" layer and
    // assign them values in the status image.  Status pixels outside the
    // boundary will be ignored.
    statusIt.SetLocation (fromIt->m_Value);
    for ( unsigned int i = 0; i < m_NeighborList.GetSize(); ++i )
      {
      // If the pixel is not a boundary pixel or belongs to another layer
      unsigned int neighborIndex = m_NeighborList.GetArrayIndex (i);
      if ( statusIt.GetPixel (neighborIndex) == m_StatusNull )
        {
        statusIt.SetPixel (neighborIndex, to, boundary_status);
        if ( boundary_status == true ) // in bounds
          {
          node = sparsePtr->m_LayerNodeStore->Borrow();
          node->m_Value = statusIt.GetIndex() + m_NeighborList.GetNeighborhoodOffset (i);
          sparsePtr->m_Layers[to]->PushFront (node);
          }
        }
      }
    ++fromIt;
    }
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage, TFunction, TIdCell >
::PostProcessOutput()
{
  // Get the output pointer and clear its contents
  OutputImagePointer output = this->GetOutput();

  output->FillBuffer(NumericTraits< OutputPixelType >::ZeroValue());

  // Set the values in the levelset image for the active layer.
  this->InitializeActiveLayerValues();
  // Initialize layer values using the active layer as seeds.
  this->PropagateAllLayerValues();

  // Initialize pixels outside the sparse field layers to positive
  // and negative values, respectively. This is not necessary for the
  // calculations, but is useful for presenting a more intuitive output to the
  // filter.
  this->InitializeBackgroundPixels();

  for ( IdCellType fId = 0; fId < this->m_FunctionCount; fId++ )
    {
    InputImagePointer input = this->m_LevelSet[fId];
    InputPointType    origin = input->GetOrigin();

    // Local iterator
    ImageRegionIterator< InputImageType > inIt ( this->m_LevelSet[fId],
                                                 this->m_LevelSet[fId]->GetRequestedRegion() );

    // In the context of the global coordinates
    OutputIndexType start;
    output->TransformPhysicalPointToIndex(origin, start);

    // Defining sub-region in the global coordinates
    OutputRegionType region;
    region.SetSize( input->GetRequestedRegion().GetSize() );
    region.SetIndex(start);

    if ( !input || !output )
      {
      itkExceptionMacro (<< "Either input and/or output is ITK_NULLPTR.");
      }

    ImageRegionIterator< OutputImageType > outIt (output, region);

    OutputPixelType p = static_cast< OutputPixelType >( this->m_Lookup[fId] );

    inIt.GoToBegin();
    outIt.GoToBegin();

    while ( !outIt.IsAtEnd() )
      {
      if ( inIt.Get() < 0 )
        {
        outIt.Value() =  p;
        }
      ++inIt;
      ++outIt;
      }
    }
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage, TFunction, TIdCell >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "m_IsoSurfaceValue: " << this->m_IsoSurfaceValue << std::endl;
  os << indent << "m_BoundsCheckingActive: " << m_BoundsCheckingActive;

  for ( IdCellType i = 0; i < this->m_FunctionCount; i++ )
    {
    SparseDataStruct *sparsePtr = this->m_SparseData[i];
    os << indent << "m_LayerNodeStore: " << std::endl;
    sparsePtr->m_LayerNodeStore->Print ( os, indent.GetNextIndent() );
    for ( i = 0; i < sparsePtr->m_Layers.size(); i++ )
      {
      os << indent << "m_Layers[" << i << "]: size="
         << sparsePtr->m_Layers[i]->Size() << std::endl;
      os << indent << sparsePtr->m_Layers[i];
      }

    os << indent << "m_UpdateBuffer: size="
       << static_cast< InputSizeValueType >( sparsePtr->m_UpdateBuffer.size() )
       << " capacity = "
       << static_cast< InputSizeValueType >( sparsePtr->m_UpdateBuffer.capacity() )
       << std::endl;
    }

  os << indent << "Interpolate Surface Location " <<  m_InterpolateSurfaceLocation << std::endl;
  os << indent << "Number of Layers " << m_NumberOfLayers << std::endl;
  os << indent << "Value Zero "
     << static_cast< typename NumericTraits< ValueType >::PrintType >( m_ValueZero ) << std::endl;
  os << indent << "Value One  "
     << static_cast< typename NumericTraits< ValueType >::PrintType >( m_ValueOne ) << std::endl;
}
} // end namespace itk

#endif
