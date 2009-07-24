/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiphaseSparseFiniteDifferenceImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkMultiphaseSparseFiniteDifferenceImageFilter_txx
#define __itkMultiphaseSparseFiniteDifferenceImageFilter_txx

#include "itkMultiphaseSparseFiniteDifferenceImageFilter.h"

namespace itk
{
template < class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
typename MultiphaseSparseFiniteDifferenceImageFilter< TInputImage,
TOutputImage, TFunction, TIdCell >::TimeStepType
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TFunction, TIdCell >
::CalculateChange()
{
  TimeStepType minTimeStep = NumericTraits< TimeStepType >::max();

  for ( IdCellType fId = 0; fId < this->m_FunctionCount; ++fId )
    {
    this->m_CurrentFunctionIndex = fId;

    const FiniteDifferenceFunctionPointer df = this->m_DifferenceFunctions[fId];

    SparseDataStruct *sparsePtr = this->m_SparseData[fId];

    FiniteDifferenceFunctionFloatOffsetType offset;
    ValueType norm_grad_phi_squared, dx_forward, dx_backward, forwardValue,
      backwardValue, centerValue;
    const ValueType MIN_NORM      = 1.0e-6;

    void *globalData = df->GetGlobalDataPointer();

    NeighborhoodIterator<OutputImageType> outputIt ( df->GetRadius(),
      this->m_LevelSet[fId], this->m_LevelSet[fId]->GetRequestedRegion() );

    TimeStepType timeStep;

    if( m_BoundsCheckingActive == false )
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

    while( layerIt != sparsePtr->m_Layers[0]->End() )
      {
      outputIt.SetLocation ( layerIt->m_Value );

      centerValue = outputIt.GetCenterPixel();

      // Calculate the offset to the surface from the center of this
      // neighborhood.  This is used by some level set functions in sampling a
      // speed, advection, or curvature term.
      if ( this->GetInterpolateSurfaceLocation()
        && centerValue != 0.0 )
        {
        // Surface is at the zero crossing, so distance to surface is:
        // phi(x) / norm(grad(phi)), where phi(x) is the center of the
        // neighborhood.  The location is therefore
        // (i,j,k) - ( phi(x) * grad(phi(x)) ) / norm(grad(phi))^2
        norm_grad_phi_squared = 0.0;

        for ( j = 0; j < ImageDimension; ++j )
          {
          forwardValue  = outputIt.GetNext ( j );
          backwardValue = outputIt.GetPrevious ( j );

          if ( forwardValue * backwardValue >= 0 )
            {
            //  Neighbors are same sign OR at least one neighbor is zero.
            dx_forward  = forwardValue - centerValue;
            dx_backward = centerValue - backwardValue;

            // Pick the larger magnitude derivative.
            if ( ::vnl_math_abs ( dx_forward ) >::vnl_math_abs( dx_backward ) )
              {
              offset[j] = dx_forward;
              }
            else
              {
              offset[j] = dx_backward;
              }
            }
          else //Neighbors are opposite sign, pick the direction of 0 surface.
            {
            if ( forwardValue * centerValue < 0 )
              {
              offset[j] = forwardValue - centerValue;
              }
            else
              {
              offset[j] = centerValue - backwardValue;
              }
            }

          norm_grad_phi_squared += offset[j] * offset[j];
          }

        // Adding sqrt imagedimension "extends the reach" of the
        // interpolation
        // to surfaces that pass close to the center of cells.  This is a
        // heuristic fudge factor that improves interpolation and reduces
        // "wiggling" at convergence.
        ValueType coeff = centerValue * vcl_sqrt( ImageDimension
            + 0.5 )/ ( norm_grad_phi_squared + MIN_NORM );

        for ( j = 0; j < ImageDimension; ++j )
          {
          offset[j] *= coeff;
          }

        sparsePtr->m_UpdateBuffer.push_back ( df->ComputeUpdate ( outputIt, globalData, offset ) );
        }
      else // Don't do interpolation
        {
        sparsePtr->m_UpdateBuffer.push_back ( df->ComputeUpdate ( outputIt, globalData ) );
        }

      ++layerIt;
      }

    // Ask the finite difference function to compute the time step for
    // this iteration.  We give it the global data pointer to use, then
    // ask it to free the global data memory.
    timeStep = df->ComputeGlobalTimeStep ( globalData );
    df->ReleaseGlobalDataPointer ( globalData );

    if ( timeStep < minTimeStep )
      {
      minTimeStep = timeStep;
      }
    }

  minTimeStep = 0.2; //FIXME finally assigned to a constant

  return minTimeStep;
}


template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage,
  TOutputImage,
  TFunction, TIdCell >
::ApplyUpdate ( TimeStepType dt )
{
  unsigned int j, k;

  for ( IdCellType fId = 0; fId < this->m_FunctionCount; ++fId )
    {
    this->m_CurrentFunctionIndex = fId;

    SparseDataStruct *sparsePtr = this->m_SparseData[fId];
    unsigned int t;

    StatusType up_to, up_search;
    StatusType down_to, down_search;

    LayerPointerType UpList[2];
    LayerPointerType DownList[2];

    for ( j = 0; j < 2; ++j )
      {
      UpList[j]   = LayerType::New();
      DownList[j] = LayerType::New();
      }

    // Process the active layer.  This step will update the values in the
    // active
    // layer as well as the values at indices that *will* become part of the
    // active layer when they are promoted/demoted.  Also records promotions,
    // demotions in the m_StatusLayer for current active layer indices
    // (i.e. those indices which will move inside or outside the active
    // layers).
    this->UpdateActiveLayerValues( dt, UpList[0], DownList[0] );

    // Process the status up/down lists.  This is an iterative process which
    // proceeds outwards from the active layer.  Each iteration generates the
    // list for the next iteration.

    // First process the status lists generated on the active layer.
    this->ProcessStatusList ( UpList[0], UpList[1], 2, 1 );
    this->ProcessStatusList ( DownList[0], DownList[1], 1, 2 );

    down_to = up_to = 0;
    up_search       = 3;
    down_search     = 4;
    j = 1;
    k = 0;
    while ( down_search < static_cast<StatusType>(sparsePtr->m_Layers.size()))
      {
      this->ProcessStatusList(UpList[j], UpList[k], up_to, up_search );
      this->ProcessStatusList(DownList[j], DownList[k], down_to, down_search);

      if( up_to == 0 )
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
    this->ProcessStatusList(UpList[j], UpList[k], up_to, m_StatusNull );
    this->ProcessStatusList(DownList[j], DownList[k], down_to, m_StatusNull);

    // Now we are left with the lists of indicies which must be
    // brought into the outermost layers.  Bring UpList into last inside layer
    // and DownList into last outside layer.
    this->ProcessOutsideList ( UpList[k], static_cast<signed char> (
      sparsePtr->m_Layers.size() ) -2 );
    this->ProcessOutsideList ( DownList[k], static_cast<signed char> (
      sparsePtr->m_Layers.size() ) -1 );

    // Finally, we update all of the layer values (excluding the active layer,
    // which has already been updated).
    this->PropagateAllLayerValues();
    }

  this->m_CurrentFunctionIndex = 0;
}

template < class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage,
  TOutputImage,
  TFunction, TIdCell >
::ProcessOutsideList ( LayerType *OutsideList, StatusType ChangeToStatus )
{
  SparseDataStruct *sparsePtr = this->m_SparseData[this->m_CurrentFunctionIndex];
  LayerNodeType *node;

  // Push each index in the input list into its appropriate status layer
  // (ChangeToStatus) and update the status image value at that index.
  while ( ! OutsideList->Empty() )
    {
    sparsePtr->m_StatusImage->SetPixel ( OutsideList->Front()->m_Value, ChangeToStatus );
    node = OutsideList->Front();
    OutsideList->PopFront();
    sparsePtr->m_Layers[ChangeToStatus]->PushFront ( node );
    }
}

template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage,
  TOutputImage,
  TFunction, TIdCell >
::ProcessStatusList ( LayerType *InputList, LayerType *OutputList,
                      StatusType ChangeToStatus, StatusType SearchForStatus )
{
  SparseDataStruct *sparsePtr = this->m_SparseData[this->m_CurrentFunctionIndex];

  unsigned int i;
  bool bounds_status;
  LayerNodeType *node;
  StatusType neighbor_status;
  NeighborhoodIterator<StatusImageType>
  statusIt ( sparsePtr->m_NeighborList.GetRadius(), sparsePtr->m_StatusImage,
             this->m_LevelSet[this->m_CurrentFunctionIndex]->GetRequestedRegion() );

  if ( !m_BoundsCheckingActive )
    {
    statusIt.NeedToUseBoundaryConditionOff();
    }

  // Push each index in the input list into its appropriate status layer
  // (ChangeToStatus) and update the status image value at that index.
  // Also examine the neighbors of the index to determine which need to go
  // onto
  // the output list (search for SearchForStatus).
  while ( ! InputList->Empty() )
    {
    statusIt.SetLocation ( InputList->Front()->m_Value );
    statusIt.SetCenterPixel ( ChangeToStatus );

    node = InputList->Front();  // Must unlink from the input list
    InputList->PopFront();      // _before_ transferring to another list.
    sparsePtr->m_Layers[ChangeToStatus]->PushFront ( node );

    for ( i = 0; i < sparsePtr->m_NeighborList.GetSize(); ++i )
      {
      neighbor_status = statusIt.GetPixel (
        sparsePtr->m_NeighborList.GetArrayIndex ( i ) );

      // Have we bumped up against the boundary?  If so, turn on bounds
      // checking.
      if ( neighbor_status == m_StatusBoundaryPixel )
        {
        m_BoundsCheckingActive = true;
        }

      if ( neighbor_status == SearchForStatus )
        {
        // mark this pixel so we don't add it twice.
        statusIt.SetPixel ( sparsePtr->m_NeighborList.GetArrayIndex ( i ),
                            m_StatusChanging, bounds_status );
        if ( bounds_status == true )
          {
          node = sparsePtr->m_LayerNodeStore->Borrow();
          node->m_Value = statusIt.GetIndex() +
            sparsePtr->m_NeighborList.GetNeighborhoodOffset ( i );
          OutputList->PushFront ( node );
          } // else this index was out of bounds.
        }
      }
    }
}

template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage,
  TOutputImage,
  TFunction, TIdCell >
::UpdateActiveLayerValues ( TimeStepType dt, LayerType *UpList, LayerType
*DownList )
{
  SparseDataStruct *sparsePtr = this->m_SparseData[this->m_CurrentFunctionIndex];

  // This method scales the update buffer values by the time step and adds
  // them to the active layer pixels.  New values at an index which fall
  // outside of the active layer range trigger that index to be placed on the
  // "up" or "down" status list.  The neighbors of any such index are then
  // assigned new values if they are determined to be part of the active list
  // for the next iteration (i.e. their values will be raised or lowered into
  // the active range).
  const ValueType LOWER_ACTIVE_THRESHOLD = -( m_ConstantGradientValue/2.0 );
  const ValueType UPPER_ACTIVE_THRESHOLD =  m_ConstantGradientValue / 2.0;

  ValueType new_value, temp_value, rms_change_accumulator;
  LayerNodeType *node, *release_node;
  StatusType neighbor_status;
  unsigned int i, idx, counter;
  bool bounds_status, flag;

  LayerIterator layerIt;
  UpdateBufferConstIterator updateIt;

  NeighborhoodIterator< OutputImageType >
    outputIt ( sparsePtr->m_NeighborList.GetRadius(),
    this->m_LevelSet[this->m_CurrentFunctionIndex],
    this->m_LevelSet[this->m_CurrentFunctionIndex]->GetRequestedRegion() );

  NeighborhoodIterator< StatusImageType >
    statusIt ( sparsePtr->m_NeighborList.GetRadius(),
    sparsePtr->m_StatusImage,
    this->m_LevelSet[this->m_CurrentFunctionIndex]->GetRequestedRegion() );

  if ( !m_BoundsCheckingActive )
    {
    outputIt.NeedToUseBoundaryConditionOff();
    statusIt.NeedToUseBoundaryConditionOff();
    }

  counter = 0;
  rms_change_accumulator = m_ValueZero;

  layerIt = sparsePtr->m_Layers[0]->Begin();
  updateIt = sparsePtr->m_UpdateBuffer.begin();

  while( layerIt != sparsePtr->m_Layers[0]->End() )
    {
    outputIt.SetLocation ( layerIt->m_Value );
    statusIt.SetLocation ( layerIt->m_Value );

    new_value = this->CalculateUpdateValue ( layerIt->m_Value,
      dt, outputIt.GetCenterPixel(), *updateIt );

    // If this index needs to be moved to another layer, then search its
    // neighborhood for indicies that need to be pulled up/down into the
    // active layer. Set those new active layer values appropriately,
    // checking first to make sure they have not been set by a more
    // influential neighbor.

    //   ...But first make sure any neighbors in the active layer are not
    // moving to a layer in the opposite direction.  This step is necessary
    // to avoid the creation of holes in the active layer.  The fix is simply
    // to not change this value and leave the index in the active set.

    if ( new_value >= UPPER_ACTIVE_THRESHOLD )
      {
      // This index will move UP into a positive (outside) layer.

      // First check for active layer neighbors moving in the opposite
      // direction.
      flag = false;
      for ( i = 0; i < sparsePtr->m_NeighborList.GetSize(); ++i )
        {
        if ( statusIt.GetPixel( sparsePtr->m_NeighborList.GetArrayIndex( i ) )
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

      rms_change_accumulator += vnl_math_sqr ( new_value-outputIt.GetCenterPixel() );

      // Search the neighborhood for inside indicies.
      temp_value = new_value - m_ConstantGradientValue;
      for ( i = 0; i < sparsePtr->m_NeighborList.GetSize(); ++i )
        {
        idx = sparsePtr->m_NeighborList.GetArrayIndex ( i );
        neighbor_status = statusIt.GetPixel ( idx );
        if ( neighbor_status == 1 )
          {
          // Keep the smallest possible value for the new active node.  This
          // places the new active layer node closest to the zero level-set.
          if ( outputIt.GetPixel ( idx ) < LOWER_ACTIVE_THRESHOLD ||
              ::vnl_math_abs ( temp_value ) < ::vnl_math_abs (
              outputIt.GetPixel ( idx ) ) )
            {
            UpdatePixel ( this->m_CurrentFunctionIndex, idx, outputIt, temp_value, bounds_status );
            }
          }
        }
      // Push it into the uplist
      node = sparsePtr->m_LayerNodeStore->Borrow();
      node->m_Value = layerIt->m_Value;
      UpList->PushFront ( node );
      statusIt.SetCenterPixel ( m_StatusActiveChangingUp );

      // Now remove this index from the active list.
      release_node = layerIt.GetPointer();
      ++layerIt;
      sparsePtr->m_Layers[0]->Unlink ( release_node );
      sparsePtr->m_LayerNodeStore->Return ( release_node );
      }

    else if ( new_value < LOWER_ACTIVE_THRESHOLD )
      {
      // This index will move DOWN into a negative (inside) layer.

      // First check for active layer neighbors moving in the opposite
      // direction.
      flag = false;
      for ( i = 0; i < sparsePtr->m_NeighborList.GetSize(); ++i )
        {

        if ( statusIt.GetPixel( sparsePtr->m_NeighborList.GetArrayIndex( i ) )
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

      rms_change_accumulator += vnl_math_sqr ( new_value - outputIt.GetCenterPixel() );

      // Search the neighborhood for outside indicies.
      temp_value = new_value + m_ConstantGradientValue;
      for ( i = 0; i < sparsePtr->m_NeighborList.GetSize(); ++i )
        {
        idx = sparsePtr->m_NeighborList.GetArrayIndex ( i );
        neighbor_status = statusIt.GetPixel ( idx );
        if ( neighbor_status == 2 )
          {
          // Keep the smallest magnitude value for this active set node.  This
          // places the node closest to the active layer.
          if ( outputIt.GetPixel ( idx ) >= UPPER_ACTIVE_THRESHOLD ||
            ::vnl_math_abs ( temp_value ) < ::vnl_math_abs (
            outputIt.GetPixel ( idx ) ) )
            {
            UpdatePixel ( this->m_CurrentFunctionIndex, idx, outputIt, temp_value, bounds_status );
            }
          }
        }
      node = sparsePtr->m_LayerNodeStore->Borrow();
      node->m_Value = layerIt->m_Value;
      DownList->PushFront ( node );
      statusIt.SetCenterPixel ( m_StatusActiveChangingDown );

      // Now remove this index from the active list.
      release_node = layerIt.GetPointer();
      ++layerIt;
      sparsePtr->m_Layers[0]->Unlink ( release_node );
      sparsePtr->m_LayerNodeStore->Return ( release_node );
      }
    else
      {
      rms_change_accumulator += vnl_math_sqr ( new_value - outputIt.GetCenterPixel() );

      UpdatePixel ( this->m_CurrentFunctionIndex, outputIt.Size() >> 1, outputIt, new_value, bounds_status );

      ++layerIt;
      }
    ++updateIt;
    ++counter;
    }

  // Determine the average change during this iteration.
  if ( counter == 0 )
    {
    this->SetRMSChange ( static_cast<double> ( m_ValueZero ) );
    }
  else
    {
    this->SetRMSChange (
      vcl_sqrt ( static_cast< double >( rms_change_accumulator /
      static_cast<double> ( counter ) ) ) );
    }
}

template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage,
  TOutputImage,
  TFunction, TIdCell >
::InitializeActiveLayerValues()
{
  const ValueType MIN_NORM      = 1.0e-6;
  OutputSpacingType spacing = this->m_LevelSet[0]->GetSpacing();

  for ( IdCellType i = 0; i < this->m_FunctionCount; i++ )
    {
    SparseDataStruct *sparsePtr = this->m_SparseData[i];

    typename OutputImageType::Pointer output = this->m_LevelSet[i];

    typename LayerType::ConstIterator activeIt;
    ConstNeighborhoodIterator<OutputImageType> outputIt (
      sparsePtr->m_NeighborList.GetRadius(),
      output, output->GetRequestedRegion() );

    sparsePtr->m_UpdateBuffer.clear();
    sparsePtr->m_UpdateBuffer.reserve ( sparsePtr->m_Layers[0]->Size() );

    unsigned int center; // index to active layer pixel
    center = outputIt.Size()/2;

    ValueType dx, gradientMagnitude, gradientMagnitudeSqr,
      distance, forward, current, backward;

    // For all indicies in the active layer...
    activeIt = sparsePtr->m_Layers[0]->Begin();
    while( activeIt != sparsePtr->m_Layers[0]->End() )
      {
      // Interpolate on the (shifted) input image values at this index to
      // assign an active layer value in the output image.
      outputIt.SetLocation ( activeIt->m_Value );

      gradientMagnitudeSqr = m_ValueZero;

      for ( unsigned int j = 0; j < ImageDimension; ++j )
        {
        // Compute forward and backward pixel values
        forward = outputIt.GetPixel ( center + sparsePtr->m_NeighborList.GetStride( j ) );
        current = outputIt.GetCenterPixel();
        backward = outputIt.GetPixel ( center - sparsePtr->m_NeighborList.GetStride ( j ) );

        // Choose the derivative closest to the 0 contour
        if ( vnl_math_sgn( current*forward ) == -1 )
          {
          dx = ( forward - current ) / spacing[j];
          }
        else
          {
          dx = ( current - backward ) / spacing[j];
          }

        gradientMagnitudeSqr += dx * dx;
        }
      gradientMagnitude = vcl_sqrt ( gradientMagnitudeSqr ) + MIN_NORM;

      // Compute the correct distance as phi(x)/gradientMagnitude
      distance = outputIt.GetCenterPixel() / gradientMagnitude;

      // Insert in the update buffer
      sparsePtr->m_UpdateBuffer.push_back(
        vnl_math_min ( vnl_math_max ( -MIN_NORM, distance ),
        MIN_NORM ) );
      ++activeIt;
      }

    // Update the level-set image using the update buffer
    activeIt = sparsePtr->m_Layers[0]->Begin();
    while( activeIt != sparsePtr->m_Layers[0]->End() )
      {
      output->SetPixel ( activeIt->m_Value, sparsePtr->m_UpdateBuffer.front() );
      ++activeIt;
      }
    }
}


template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage,
  TOutputImage,
  TFunction, TIdCell >
::PropagateAllLayerValues()
{
  for ( IdCellType i = 0; i < this->m_FunctionCount; i++ )
    {
    PropagateFunctionLayerValues ( i );
    }
}

template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage,
  TOutputImage,
  TFunction, TIdCell >
::PropagateFunctionLayerValues ( unsigned int functionIndex )
{
  SparseDataStruct *sparsePtr = this->m_SparseData[functionIndex];

  // Update values in the first inside and first outside layers using the
  // active layer as a seed. Inside layers are odd numbers, outside layers are
  // even numbers.
  this->PropagateLayerValues ( sparsePtr, 0, 1, 3, 1 ); // first inside
  this->PropagateLayerValues ( sparsePtr, 0, 2, 4, 2 ); // first outside

  // Update the rest of the layers.
  for ( unsigned int i = 1; i < sparsePtr->m_Layers.size() - 2; ++i )
    {
    this->PropagateLayerValues ( sparsePtr, i, i+2, i+4, ( i+2 ) %2 );
    }
}

template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage,
  TOutputImage,
  TFunction, TIdCell >
::PropagateLayerValues ( SparseDataStruct *sparsePtr, StatusType from,
                        StatusType to, StatusType promote, int InOrOut )
{
  // InOrOut indicates whether we are propagating in the negative/positive region
  // of the level-set function

  unsigned int i;
  ValueType value_temp, delta;
  ValueType value = NumericTraits<ValueType>::Zero; // warnings
  bool found_neighbor_flag;
  LayerIterator toIt;
  LayerNodeType *node;

  StatusType past_end = static_cast<StatusType>( sparsePtr->m_Layers.size() )-1;

  // Are we propagating values inward (more negative) or outward (more
  // positive)?
  if ( InOrOut == 1 )
    {
    delta = - 1;
    }
  else
    {
    delta = 1;
    }

  NeighborhoodIterator<OutputImageType>
  outputIt ( sparsePtr->m_NeighborList.GetRadius(),
    this->m_LevelSet[sparsePtr->m_Index],
    this->m_LevelSet[sparsePtr->m_Index]->GetRequestedRegion() );
  NeighborhoodIterator<StatusImageType>
  statusIt ( sparsePtr->m_NeighborList.GetRadius(), sparsePtr->m_StatusImage,
    this->m_LevelSet[sparsePtr->m_Index]->GetRequestedRegion() );

  if ( !m_BoundsCheckingActive )
    {
    outputIt.NeedToUseBoundaryConditionOff();
    statusIt.NeedToUseBoundaryConditionOff();
    }

  toIt  = sparsePtr->m_Layers[to]->Begin();
  while ( toIt != sparsePtr->m_Layers[to]->End() )
    {
    // Set the iterator location in the status image
    OutputIndexType indexCurrent = toIt->m_Value;
    statusIt.SetLocation ( indexCurrent );

    // Is this index marked for deletion? If the status image has
    // been marked with another layer's value, we need to delete this node
    // from the current list then skip to the next iteration.
    if ( statusIt.GetCenterPixel() != to )
      {
      node = toIt.GetPointer();
      ++toIt;
      sparsePtr->m_Layers[to]->Unlink ( node );
      sparsePtr->m_LayerNodeStore->Return ( node );
      continue;
      }

    // Set the iterator location in the level-set image
    outputIt.SetLocation ( toIt->m_Value );

    // We explore all neighbors to identify the closest from-layer node
    found_neighbor_flag = false;
    unsigned int indexNeighbor;
    for ( i = 0; i < sparsePtr->m_NeighborList.GetSize(); ++i )
      {
      // If this neighbor is in the "from" list, compare its absolute value
      // to any previous values found in the "from" list.  Keep the value
      // that will cause the to-layer to be closest to the zero level set.
      indexNeighbor = sparsePtr->m_NeighborList.GetArrayIndex ( i ); // Get index
      if ( statusIt.GetPixel ( indexNeighbor ) == from ) // if belongs to from-layer
        {
        // This value should be a distance in terms of spacing with neighbors
        // plus its current value

        OutputPointType p1, p2;
        ValueType dist = 0; // compute the distance between neighbors
        this->m_LevelSet[sparsePtr->m_Index]->TransformIndexToPhysicalPoint(
          statusIt.GetIndex( indexNeighbor ), p1 );
        this->m_LevelSet[sparsePtr->m_Index]->TransformIndexToPhysicalPoint(
          indexCurrent, p2);
        for( unsigned int j = 0; j < ImageDimension; j++ )
          dist += (p1[j] - p2[j]) * (p1[j] - p2[j]);
        dist = delta * vcl_sqrt( dist );

        value_temp = dist + outputIt.GetPixel ( indexNeighbor ); // grab its value

        if ( !found_neighbor_flag )
          {
          value = value_temp;
          }
        else
          {
          // Irrespective of negative/positive region, select the lowest absolute minimum
          //value = delta * vnl_math_min( vnl_math_abs( value_temp ), vnl_math_abs( value ) );
          if ( InOrOut == 1 )
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
      bool bounds_status;
      UpdatePixel ( sparsePtr->m_Index, outputIt.Size() >>1, outputIt, value, bounds_status );
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
      sparsePtr->m_Layers[to]->Unlink ( node );
      if ( promote > past_end )
        {
        sparsePtr->m_LayerNodeStore->Return ( node );
        statusIt.SetCenterPixel ( m_StatusNull );

        this->m_LevelSet[sparsePtr->m_Index]->SetPixel( indexCurrent ,
          delta * this->m_BackgroundValue );
        }
      else
        {
        sparsePtr->m_Layers[promote]->PushFront ( node );
        statusIt.SetCenterPixel ( promote );
        }
      }
    }
}


template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
double MultiphaseSparseFiniteDifferenceImageFilter<TInputImage, TOutputImage, TFunction, TIdCell >
::m_ConstantGradientValue = 1.0;

template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
const ITK_TYPENAME MultiphaseSparseFiniteDifferenceImageFilter<TInputImage,
TOutputImage, TFunction, TIdCell >::ValueType
MultiphaseSparseFiniteDifferenceImageFilter<TInputImage, TOutputImage, TFunction, TIdCell >
::m_ValueOne = NumericTraits<ITK_TYPENAME
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TFunction, TIdCell >
::ValueType >::One;

template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
const ITK_TYPENAME MultiphaseSparseFiniteDifferenceImageFilter< TInputImage,
TOutputImage, TFunction, TIdCell >::ValueType
MultiphaseSparseFiniteDifferenceImageFilter<TInputImage, TOutputImage, TFunction, TIdCell >
::m_ValueZero = NumericTraits<ITK_TYPENAME
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TFunction, TIdCell >::
ValueType>::Zero;

template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
const ITK_TYPENAME MultiphaseSparseFiniteDifferenceImageFilter< TInputImage,
TOutputImage, TFunction, TIdCell >::StatusType
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TFunction, TIdCell >
::m_StatusNull = NumericTraits< ITK_TYPENAME
MultiphaseSparseFiniteDifferenceImageFilter<TInputImage, TOutputImage, TFunction, TIdCell >::
StatusType >::NonpositiveMin();

template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
const ITK_TYPENAME MultiphaseSparseFiniteDifferenceImageFilter< TInputImage,
TOutputImage, TFunction, TIdCell >::StatusType
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TFunction, TIdCell >
::m_StatusChanging = -1;

template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
const ITK_TYPENAME MultiphaseSparseFiniteDifferenceImageFilter< TInputImage,
TOutputImage, TFunction, TIdCell >::StatusType
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TFunction, TIdCell >
::m_StatusActiveChangingUp = -2;

template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
const ITK_TYPENAME MultiphaseSparseFiniteDifferenceImageFilter< TInputImage,
TOutputImage, TFunction, TIdCell >::StatusType
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TFunction, TIdCell >
::m_StatusActiveChangingDown = -3;

template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
const ITK_TYPENAME MultiphaseSparseFiniteDifferenceImageFilter< TInputImage,
TOutputImage, TFunction, TIdCell >::StatusType
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TFunction, TIdCell >
::m_StatusBoundaryPixel = -4;

template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TFunction, TIdCell >
::MultiphaseSparseFiniteDifferenceImageFilter()
{
  this->m_CurrentFunctionIndex = 0;
  this->m_IsoSurfaceValue = m_ValueZero;
  this->m_BackgroundValue  = NumericTraits<ValueType>::max();
  this->m_NumberOfLayers = ImageDimension;
  this->m_InterpolateSurfaceLocation = true;
  this->m_BoundsCheckingActive = false;
}

template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter<TInputImage, TOutputImage, TFunction, TIdCell >
::CopyInputToOutput()
{
  for( IdCellType i = 0; i < this->m_FunctionCount; i++ )
    {
    SparseDataStruct * sparsePtr = this->m_SparseData[i];
    OutputImagePointer input = this->m_LevelSet[i];

    // This is used as a temporary buffer in this specific instance
    // However, we need to use it later on. Therefore, its instantiation
    // is important. DO NOT DELETE.
    sparsePtr->m_ShiftedImage = OutputImageType::New();
    sparsePtr->m_ShiftedImage->SetRegions( input->GetRequestedRegion() );
    sparsePtr->m_ShiftedImage->CopyInformation( input );
    sparsePtr->m_ShiftedImage->Allocate();

    // Copy input to ShiftedImage
    //
    OutputRegionType region = input->GetRequestedRegion();
    ImageRegionIterator<OutputImageType> lIt( input, region );
    ImageRegionIterator<OutputImageType> sIt( sparsePtr->m_ShiftedImage, region );

    lIt.GoToBegin();
    sIt.GoToBegin();

    while(  !lIt.IsAtEnd() )
      {
      sIt.Set( lIt.Get() );
      ++sIt;
      ++lIt;
      }

    // TODO: Can the zeroCrossingFilter have the same input and output?
    ZeroCrossingFilterPointer zeroCrossingFilter = ZeroCrossingFilterType::New();
    zeroCrossingFilter->SetInput( sparsePtr->m_ShiftedImage );
    zeroCrossingFilter->SetBackgroundValue( m_ValueOne );
    zeroCrossingFilter->SetForegroundValue( m_ValueZero );
    zeroCrossingFilter->Update();

    this->m_LevelSet[i] = zeroCrossingFilter->GetOutput();
    this->m_LevelSet[i]->DisconnectPipeline();
    }
}

template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TFunction, TIdCell >
::InitializeIteration()
{
  Superclass::InitializeIteration();

  // Set the values in the output image for the active layer.
  this->InitializeActiveLayerValues();

  // Initialize layer values using the active layer as seeds.
  this->PropagateAllLayerValues();
}


template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TFunction, TIdCell >
::Initialize()
{
  for ( IdCellType  fId = 0; fId < this->m_FunctionCount; ++fId )
    {
    SparseDataStruct *sparsePtr = this->m_SparseData[fId];

    // Allocate the status image.
    sparsePtr->m_StatusImage = StatusImageType::New();
    sparsePtr->m_StatusImage->SetRegions (
      this->m_LevelSet[fId]->GetRequestedRegion() );
    sparsePtr->m_StatusImage->CopyInformation( this->m_LevelSet[fId] );
    sparsePtr->m_StatusImage->Allocate();
    sparsePtr->m_StatusImage->FillBuffer( m_StatusNull );//NonpositiveMin

    // Initialize the boundary pixels in the status image to
    // m_StatusBoundaryPixel values.  Uses the face calculator to find all of
    // the region faces.
    BFCType faceCalculator;
    typename BFCType::FaceListType faceList;

    // Set the difference function radius here
    typename BFCType::SizeType sz = this->m_DifferenceFunctions[fId]->GetRadius();
    typename BFCType::FaceListType::iterator fit;

    // Compute the boundary pixel regions set in a container
    faceList = faceCalculator ( sparsePtr->m_StatusImage,
      sparsePtr->m_StatusImage->GetRequestedRegion(), sz );

    // Iterate over the boundary region sets
    fit = faceList.begin();
    for( ++fit; fit != faceList.end(); ++fit )
      {
      // For each region, set the pixel in m_StatusImage to m_StatusBoundaryPixel
      ImageRegionIterator<StatusImageType> statusIt( sparsePtr->m_StatusImage, *fit );

      statusIt.GoToBegin();
      while( ! statusIt.IsAtEnd() )
        {
        statusIt.Set ( m_StatusBoundaryPixel );
        ++statusIt;
        }
      }

    // Erase all existing layer lists -- element by element
    for ( unsigned int i = 0; i < sparsePtr->m_Layers.size(); ++i )
      {
      while ( ! sparsePtr->m_Layers[i]->Empty() )
        {
        sparsePtr->m_LayerNodeStore->Return( sparsePtr->m_Layers[i]->Front());
        sparsePtr->m_Layers[i]->PopFront();
        }
      }

    // Allocate the layers for the sparse field.
    sparsePtr->m_Layers.clear();
    sparsePtr->m_Layers.reserve( 2 * this->m_NumberOfLayers + 1 );

    while( sparsePtr->m_Layers.size() < ( 2 * this->m_NumberOfLayers+1 ) )
      {
      sparsePtr->m_Layers.push_back( LayerType::New() );
      }

    // Throw an exception if we don't have enough layers.
    if ( sparsePtr->m_Layers.size() < 3 )
      {
      itkExceptionMacro ( << "Not enough layers have been allocated for the"
        "sparse field.  Requires at least one layer." );
      }
    }

  // Set the background constants required to be set outside the sparse layer
  this->InitializeBackgroundConstants();

  // Construct the active layer and initialize the first layers inside and
  // outside of the active layer
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
      // Construct layer i+2 from layer i. Note that layer i+1 is on the other side
      this->ConstructLayer( sparsePtr, i, i+2 );
      }
    }

  // Set the values in the output image for the active layer.
  this->InitializeActiveLayerValues();

  // Initialize layer values using the active layer as seeds.
  this->PropagateAllLayerValues();

  // Initialize pixels inside and outside the sparse field layers to positive
  // and negative values, respectively?? This is not necessary for the
  // calculations, but is useful for presenting a more intuitive output to the
  // filter.  See PostProcessOutput method for more information.
  this->InitializeBackgroundPixels();
}


template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TFunction, TIdCell >
::InitializeBackgroundConstants()
{
  // Determine the maximum spacing to set the background pixel values
  // outside the sparse field
  float maxSpacing = NumericTraits<float>::min();
  OutputSpacingType spacing = this->m_LevelSet[0]->GetSpacing();
  for( unsigned int i = 0; i < ImageDimension; i++ )
    maxSpacing = vnl_math_max( maxSpacing, static_cast<float>( spacing[i] ) );

  // Assign background pixels OUTSIDE the sparse field layers to a new level
  // set with value greater than the outermost layer.  Assign background pixels
  // INSIDE the sparse field layers to a new level set with value less than
  // the innermost layer.
  const ValueType max_layer = static_cast<ValueType> ( this->m_NumberOfLayers );

  this->m_BackgroundValue  = ( max_layer + 1 ) * maxSpacing;
}


template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TFunction, TIdCell >
::InitializeBackgroundPixels()
{
  for ( IdCellType fId = 0; fId < this->m_FunctionCount; fId++ )
    {
    SparseDataStruct *sparsePtr = this->m_SparseData[fId];

    ImageRegionConstIterator<StatusImageType> statusIt (
      sparsePtr->m_StatusImage,
      this->m_LevelSet[fId]->GetRequestedRegion() );

    ImageRegionIterator<OutputImageType> outputIt (
      this->m_LevelSet[fId],
      this->m_LevelSet[fId]->GetRequestedRegion() );

    ImageRegionIterator<OutputImageType> shiftedIt (
      sparsePtr->m_ShiftedImage,
      this->m_LevelSet[fId]->GetRequestedRegion() );

    outputIt.GoToBegin();
    shiftedIt.GoToBegin();
    statusIt.GoToBegin();

    while(  !outputIt.IsAtEnd() )
      {
      if( statusIt.Get() == m_StatusNull || statusIt.Get() ==
        m_StatusBoundaryPixel )
        {
        if( shiftedIt.Get() > m_ValueZero )
          {
          outputIt.Set ( this->m_BackgroundValue );
          }
        else
          {
          outputIt.Set ( - this->m_BackgroundValue );
          }
        }
      ++shiftedIt;
      ++outputIt;
      ++statusIt;
      }
    }
}


template< class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TFunction, TIdCell >
::ConstructActiveLayer()
{
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

    NeighborhoodIterator< OutputImageType >
      outputIt ( sparsePtr->m_NeighborList.GetRadius(),
      this->m_LevelSet[fId],
      this->m_LevelSet[fId]->GetRequestedRegion() );

    NeighborhoodIterator< StatusImageType >
      statusIt ( sparsePtr->m_NeighborList.GetRadius(),
      sparsePtr->m_StatusImage,
      this->m_LevelSet[fId]->GetRequestedRegion() );

    NeighborhoodIterator< OutputImageType >
      shiftedIt ( sparsePtr->m_NeighborList.GetRadius(),
      sparsePtr->m_ShiftedImage,
      this->m_LevelSet[fId]->GetRequestedRegion() );

    OutputIndexType center_index, offset_index;
    LayerNodeType *node;
    bool bounds_status;
    ValueType value;
    StatusType layer_number;

    OutputIndexType lowerBounds;
    OutputSizeType upperBounds;
    lowerBounds = this->m_LevelSet[fId]->GetRequestedRegion().GetIndex();
    upperBounds = this->m_LevelSet[fId]->GetRequestedRegion().GetSize();

    // Iterate over the output image
    outputIt.GoToBegin();
    while( !outputIt.IsAtEnd() )
      {
      // Check if the center pixel has a value 0. This is usually rare since
      // the surface mostly will not pass through the pixel.
      if ( outputIt.GetCenterPixel() == m_ValueZero )
        {
        // Grab the neighborhood in the status image.
        center_index = outputIt.GetIndex();
        statusIt.SetLocation ( center_index );

        // Check to see if any of the sparse field touches a boundary.  If so,
        // then activate bounds checking.
        for ( unsigned int i = 0; i < ImageDimension; i++ )
          {
          if ( ( center_index[i] + static_cast< long > (
            this->m_NumberOfLayers ) >= static_cast< long>( upperBounds[i] - 1 ) )
            || center_index[i] - static_cast< long >(
            this->m_NumberOfLayers ) <= static_cast< long >(lowerBounds[i]) )
            {
            m_BoundsCheckingActive = true;
            }
          }

        // Borrow a node from the store and set its value.
        node = sparsePtr->m_LayerNodeStore->Borrow();
        node->m_Value = center_index;

        // Add the node to the active list and set the status in the status
        // image.
        sparsePtr->m_Layers[0]->PushFront ( node );
        statusIt.SetCenterPixel ( 0 );

        // Grab the neighborhood in the image of shifted input values.
        shiftedIt.SetLocation ( center_index );

        // Search the neighborhood pixels for first inside & outside layer
        // members.  Construct these lists and set status list values.
        for ( unsigned int i = 0; i < sparsePtr->m_NeighborList.GetSize(); ++i )
          {
          offset_index = center_index + sparsePtr->m_NeighborList.GetNeighborhoodOffset ( i );

          // If the neighborhood pixel is not on the active layer
          // determine its sign to assign to outside or inside layers
          if ( outputIt.GetPixel( sparsePtr->m_NeighborList.GetArrayIndex( i ) ) != m_ValueZero )
            {
            value = shiftedIt.GetPixel ( sparsePtr->m_NeighborList.GetArrayIndex ( i ) );

            if ( value < m_ValueZero ) // Assign to first inside layer.
              {
              layer_number = 1;
              }
            else // Assign to first outside layer
              {
              layer_number = 2;
              }

            statusIt.SetPixel ( sparsePtr->m_NeighborList.GetArrayIndex ( i ),
              layer_number, bounds_status );

            if ( bounds_status ) // In bounds.
              {
              node = sparsePtr->m_LayerNodeStore->Borrow();
              node->m_Value = offset_index;
              sparsePtr->m_Layers[layer_number]->PushFront ( node );
              } // else do nothing.
            }
          }
        }
      ++outputIt;
      }
    }
}

template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TFunction, TIdCell >
::ConstructLayer ( SparseDataStruct *sparsePtr, StatusType from, StatusType to )
{
  LayerNodeType *node;
  bool boundary_status;

  typename LayerType::ConstIterator fromIt;
  NeighborhoodIterator<StatusImageType> statusIt (
    sparsePtr->m_NeighborList.GetRadius(), sparsePtr->m_StatusImage,
    this->m_LevelSet[sparsePtr->m_Index]->GetRequestedRegion() );

  // For all indices in the "from" layer...
  fromIt = sparsePtr->m_Layers[from]->Begin();
  while( fromIt != sparsePtr->m_Layers[from]->End() )
    {
    // Search the neighborhood of this index in the status image for
    // unassigned indicies. Push those indicies onto the "to" layer and
    // assign them values in the status image.  Status pixels outside the
    // boundary will be ignored.
    statusIt.SetLocation ( fromIt->m_Value );
    for ( unsigned int i = 0; i < sparsePtr->m_NeighborList.GetSize(); ++i )
      {
      // If the pixel is not a boundary pixel or belongs to another layer
      if ( statusIt.GetPixel ( sparsePtr->m_NeighborList.GetArrayIndex ( i ) ) == m_StatusNull )
        {
        statusIt.SetPixel ( sparsePtr->m_NeighborList.GetArrayIndex ( i ), to, boundary_status );
        if ( boundary_status == true ) // in bounds
          {
          node = sparsePtr->m_LayerNodeStore->Borrow();
          node->m_Value = statusIt.GetIndex() + sparsePtr->m_NeighborList.GetNeighborhoodOffset ( i );
          sparsePtr->m_Layers[to]->PushFront ( node );
          }
        }
      }
    ++fromIt;
    }
}

template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter< TInputImage, TOutputImage, TFunction, TIdCell >
::AllocateUpdateBuffer()
{
  for ( IdCellType fId = 0; fId < this->m_FunctionCount; fId++ )
    {
    SparseDataStruct *sparsePtr = this->m_SparseData[fId];

    // Preallocate the update buffer.  NOTE: There is currently no way to
    // downsize a std::vector. This means that the update buffer will grow
    // dynamically but not shrink.  In newer implementations there may be a
    // squeeze method which can do this.  Alternately, we can implement our
    // own strategy for downsizing.
    sparsePtr->m_UpdateBuffer.clear();
    sparsePtr->m_UpdateBuffer.reserve ( sparsePtr->m_Layers[0]->Size() );
    }
}

template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter<TInputImage, TOutputImage, TFunction, TIdCell >
::PostProcessOutput()
{
  // Get the output pointer and clear its contents
  OutputImagePointer output = this->GetOutput();
  output->FillBuffer( NumericTraits<OutputPixelType>::Zero );

  // Set the values in the levelset image for the active layer.
  this->InitializeActiveLayerValues();
  // Initialize layer values using the active layer as seeds.
  this->PropagateAllLayerValues();

  for ( IdCellType fId = 0; fId < this->m_FunctionCount; fId++ )
    {
    InputImagePointer input = this->m_LevelSet[fId];
    InputPointType origin = input->GetOrigin();
    InputSpacingType spacing = input->GetSpacing();

    // Local iterator
    ImageRegionIterator< InputImageType > inIt ( this->m_LevelSet[fId],
      this->m_LevelSet[fId]->GetRequestedRegion() );

    // In the context of the global coordinates
    OutputIndexType start;
    output->TransformPhysicalPointToIndex( origin, start );

    // Defining sub-region in the global coordinates
    OutputRegionType region;
    region.SetSize( input->GetRequestedRegion().GetSize() );
    region.SetIndex( start );

    if ( !input || !output )
      {
      itkExceptionMacro ( << "Either input and/or output is NULL." );
      }

    ImageRegionIterator< OutputImageType > outIt ( output, region );

    OutputPixelType p = static_cast<OutputPixelType> ( this->m_Lookup[fId] );

    inIt.GoToBegin();
    outIt.GoToBegin();

    while( !outIt.IsAtEnd() )
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

template<class TInputImage, class TOutputImage, class TFunction, typename TIdCell >
void
MultiphaseSparseFiniteDifferenceImageFilter<TInputImage, TOutputImage, TFunction, TIdCell >
::PrintSelf ( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "m_IsoSurfaceValue: " << this->m_IsoSurfaceValue << std::endl;
  os << indent << "m_BoundsCheckingActive: " << m_BoundsCheckingActive;

  for( IdCellType i = 0; i < this->m_FunctionCount; i++ )
    {
    SparseDataStruct *sparsePtr = this->m_SparseData[i];
    os << indent << "m_LayerNodeStore: " << std::endl;
    sparsePtr->m_LayerNodeStore->Print ( os,indent.GetNextIndent() );
    for ( i = 0; i < sparsePtr->m_Layers.size(); i++ )
      {
      os << indent << "m_Layers[" << i << "]: size="
        << sparsePtr->m_Layers[i]->Size() << std::endl;
      os << indent << sparsePtr->m_Layers[i];
      }

    os << indent << "m_UpdateBuffer: size=" <<
      static_cast< unsigned long > ( sparsePtr->m_UpdateBuffer.size() )
      << " capacity = " <<
      static_cast<unsigned long> ( sparsePtr->m_UpdateBuffer.capacity() ) <<
      std::endl;
    }

  os << indent << "Interpolate Surface Location " <<  m_InterpolateSurfaceLocation << std::endl;
  os << indent << "Number of Layers " << m_NumberOfLayers << std::endl;
  os << indent << "Value Zero " <<
    static_cast< typename NumericTraits<ValueType>::PrintType >( m_ValueZero ) << std::endl;
  os << indent << "Value One  " <<
    static_cast< typename NumericTraits<ValueType>::PrintType >( m_ValueOne ) << std::endl;
}

} // end namespace itk

#endif
