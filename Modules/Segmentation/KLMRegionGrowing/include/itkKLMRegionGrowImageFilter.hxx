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
#ifndef itkKLMRegionGrowImageFilter_hxx
#define itkKLMRegionGrowImageFilter_hxx
#include "itkKLMRegionGrowImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
KLMRegionGrowImageFilter< TInputImage, TOutputImage >
::KLMRegionGrowImageFilter(void):
  m_MaximumLambda(1000),
  m_NumberOfRegions(0),
  m_InternalLambda(0),
  m_InitialNumberOfRegions(0),
  m_TotalBorderLength(0.0),
  m_BorderCandidate(ITK_NULLPTR),
  m_InitialRegionArea(0)
{
  m_InitialRegionMean.set_size(InputImageVectorDimension);
  m_InitialRegionMean.fill(0);
  this->SetMaximumNumberOfRegions(2);
}

template< typename TInputImage, typename TOutputImage >
KLMRegionGrowImageFilter< TInputImage, TOutputImage >
::~KLMRegionGrowImageFilter()
{}

/**
 * PrintSelf
 */
template< typename TInputImage, typename TOutputImage >
void
KLMRegionGrowImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "KLM Region grow segmentation object" << std::endl;
  os << indent << "KLM Region grow image filter object" << std::endl;
  os << indent << "Maximum value of lambda parameter: " << m_MaximumLambda << std::endl;
  os << indent << "Current internal value of lambda parameter: " << m_InternalLambda << std::endl;
  os << indent << "Initial number of regions: " << m_InitialNumberOfRegions << std::endl;
  os << indent << "Current number of regions: " << m_NumberOfRegions << std::endl;
} // end PrintSelf

/*
 * GenerateInputRequestedRegion method.
 */
template< typename TInputImage, typename TOutputImage >
void
KLMRegionGrowImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // This filter requires all of the input image to be in the buffer
  InputImagePointer inputPtr =
    const_cast< InputImageType * >( this->GetInput() );

  if ( inputPtr )
    {
    inputPtr->SetRequestedRegionToLargestPossibleRegion();
    }
}

/**
 * EnlargeOutputRequestedRegion method.
 */
template< typename TInputImage, typename TOutputImage >
void
KLMRegionGrowImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(
  DataObject *output)
{
  // This filter requires all of the output image to be in the buffer
  TOutputImage *imgData;

  imgData = dynamic_cast< TOutputImage * >( output );
  imgData->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage, typename TOutputImage >
void
KLMRegionGrowImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Run the KLM algorithm
  this->ApplyRegionGrowImageFilter();

  // Set the output labelled and allocate the memory
  OutputImagePointer outputPtr = this->GetOutput();

  // Allocate the output buffer memory
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  GenerateOutputImage();
} // end GenerateData

template< typename TInputImage, typename TOutputImage >
void
KLMRegionGrowImageFilter< TInputImage, TOutputImage >
::GenerateOutputImage()
{
  InputImageConstPointer inputImage     = this->GetInput();
  InputImageSizeType     inputImageSize = inputImage->GetBufferedRegion().GetSize();

  GridSizeType gridSize = this->GetGridSize();

  InputImageSizeType numRegionsAlongDim;

  for ( unsigned int idim = 0; idim < InputImageDimension; idim++ )
    {
    numRegionsAlongDim[idim] = inputImageSize[idim] / gridSize[idim];
    }

  // Walk through each atomic block and get the approximation image.
  // This is needed as the region labels associated each atomic
  // block (as it was for during the initialization stage), have changed.
  // The new region label points to the region that has the up-to-date
  // region intensity. After the updated region intensity is found,
  // each pixel is updated with the mean approximation value.

  OutputImagePointer outputImage = this->GetOutput();
  typedef typename TOutputImage::RegionType OutputRegionType;
  OutputRegionType region;
  region.SetSize(gridSize);   // Constant grid size

  OutputImageIndexType tmpIndex;
  tmpIndex.Fill(0);

  for ( unsigned int iregion = 0; iregion < m_InitialNumberOfRegions; iregion++ )
    {
    region.SetIndex(tmpIndex * gridSize);

    // Convert the mean region value to the correct output format

    MeanRegionIntensityType tmpMeanValue;
    OutputImageVectorType   outMeanValue;
    typedef typename OutputImagePixelType::ValueType OutputValueType;

    tmpMeanValue = m_RegionsPointer[iregion]->GetMeanRegionIntensity();

    for ( unsigned int ivecdim = 0; ivecdim < InputImageVectorDimension; ivecdim++ )
      {
      outMeanValue[ivecdim] =
        static_cast< OutputValueType >( tmpMeanValue[ivecdim] );
      }

    // Fill the region with the mean value

    OutputImageIterator outputIt(outputImage, region);

    while ( !outputIt.IsAtEnd() )
      {
      outputIt.Set(outMeanValue);
      ++outputIt;
      }

    // Calculate next grid index
    IndexValueType tmpVal = 1;
    for ( unsigned int idim = 0; idim < InputImageDimension; idim++ )
      {
      tmpIndex[idim]++;
      tmpVal *= numRegionsAlongDim[idim];
      if ( ( iregion + 1 ) % tmpVal != 0 ) { break; }
      tmpIndex[idim] = 0;
      }
    } // end iregion loop
}     // end GenerateOutputImage()

template< typename TInputImage, typename TOutputImage >
typename KLMRegionGrowImageFilter< TInputImage, TOutputImage >::LabelImagePointer
KLMRegionGrowImageFilter< TInputImage, TOutputImage >
::GetLabelledImage()
{
  // Allocate the memory for the labelled image

  LabelImagePointer labelImagePtr = LabelImageType::New();

  typename LabelImageType::SizeType labelImageSize =
    this->GetInput()->GetBufferedRegion().GetSize();

  LabelImageIndexType labelImageIndex;
  labelImageIndex.Fill(0);

  typename LabelImageType::RegionType labelImageRegion;

  labelImageRegion.SetSize(labelImageSize);
  labelImageRegion.SetIndex(labelImageIndex);

  labelImagePtr->SetLargestPossibleRegion(labelImageRegion);
  labelImagePtr->SetBufferedRegion(labelImageRegion);
  labelImagePtr->Allocate();

  labelImagePtr = GenerateLabelledImage(labelImagePtr);

  return labelImagePtr;
} // end GetLabelledImage()

template< typename TInputImage, typename TOutputImage >
typename KLMRegionGrowImageFilter< TInputImage, TOutputImage >::LabelImagePointer
KLMRegionGrowImageFilter< TInputImage, TOutputImage >
::GenerateLabelledImage(LabelImageType *labelImagePtr)
{
  InputImageConstPointer inputImage     = this->GetInput();
  InputImageSizeType     inputImageSize = inputImage->GetBufferedRegion().GetSize();

  GridSizeType gridSize = this->GetGridSize();

  InputImageSizeType numRegionsAlongDim;

  for ( unsigned int idim = 0; idim < InputImageDimension; idim++ )
    {
    numRegionsAlongDim[idim] = inputImageSize[idim] / gridSize[idim];
    }

  // Walk through each atomic block and get the new unique labels
  // representing the final segmentation.
  // This is needed as the region labels associated each atomic
  // block (as it was during the initialization stage), have changed.

  InputRegionType region;
  region.SetSize(gridSize);   // Constant grid size

  InputImageIndexType tmpIndex;
  tmpIndex.Fill(0);

  for ( unsigned int iregion = 0; iregion < m_InitialNumberOfRegions; iregion++ )
    {
    region.SetIndex(tmpIndex * gridSize);

    // Fill the region with the label

    RegionLabelType newRegionLabel = m_RegionsPointer[iregion]->GetRegionLabel();

    LabelImageIterator labelIt(labelImagePtr, region);
    while ( !labelIt.IsAtEnd() )
      {
      labelIt.Set(newRegionLabel);
      ++labelIt;
      }

    // Calculate next grid index
    IndexValueType tmpVal = 1;
    for ( unsigned int idim = 0; idim < InputImageDimension; idim++ )
      {
      tmpIndex[idim]++;
      tmpVal *= numRegionsAlongDim[idim];
      if ( ( iregion + 1 ) % tmpVal != 0 ) { break; }
      tmpIndex[idim] = 0;
      }
    }

  // Return the reference to the labelled image
  return labelImagePtr;
} // end GenerateLabelledImage()

template< typename TInputImage, typename TOutputImage >
void
KLMRegionGrowImageFilter< TInputImage, TOutputImage >
::ApplyRegionGrowImageFilter()
{
  this->ApplyKLM();
} // end ApplyRegionGrowImageFilter()

template< typename TInputImage, typename TOutputImage >
void
KLMRegionGrowImageFilter< TInputImage, TOutputImage >
::ApplyKLM()
{
  // Region merging based on the minimum value of the current
  // lambdas associated with the borders. The border with the
  // smallest lambda value will be taken out for region merging.
  // This growing process is repeated until the number of current
  // different regions is not bigger than the desired and the
  // current minimum scale parameter is not less than the desired scale.

  this->InitializeKLM();

  while ( ( m_NumberOfRegions > this->GetMaximumNumberOfRegions() )
          && ( m_InternalLambda  < m_MaximumLambda ) )
    {
    this->MergeRegions();
    }

  this->ResolveRegions();
} // end ApplyKLM()

template< typename TInputImage, typename TOutputImage >
void
KLMRegionGrowImageFilter< TInputImage, TOutputImage >
::InitializeKLM()
{
  // Maximum number of regions requested must be greater than 0
  if ( this->GetMaximumNumberOfRegions() <= 1 )
    {
    itkExceptionMacro(<< "Number of requested regions must be 2 or more");
    }

  // This implementation requires the image dimensions to be
  // multiples of the user specified grid sizes.

  InputImageConstPointer inputImage     = this->GetInput();
  InputImageSizeType     inputImageSize = inputImage->GetBufferedRegion().GetSize();
  GridSizeType           gridSize       = this->GetGridSize();
  typename TInputImage::SpacingType
  spacing        = inputImage->GetSpacing();

  for ( unsigned int idim = 0; idim < InputImageDimension; idim++ )
    {
    if ( gridSize[idim] == 0
         || inputImageSize[idim] % gridSize[idim] != 0 )
      {
      itkExceptionMacro(<< "Invalid grid size");
      }
    }

  // Determine the regions first and intialize them

  InputImageSizeType numRegionsAlongDim;
  for ( unsigned int idim = 0; idim < InputImageDimension; idim++ )
    {
    numRegionsAlongDim[idim] = inputImageSize[idim] / gridSize[idim];
    }

  // Calculate the initial number of regions

  m_InitialNumberOfRegions = 1;
  for ( unsigned int idim = 0; idim < InputImageDimension; idim++ )
    {
    m_InitialNumberOfRegions *= numRegionsAlongDim[idim];
    }

  if ( m_InitialNumberOfRegions < this->GetMaximumNumberOfRegions() )
    {
    itkWarningMacro(<< "Number of initial image regions is less than requested: reduce granularity of the grid");
    }

  // Set current number of regions

  m_NumberOfRegions = m_InitialNumberOfRegions;

  // Allocate and intialize memory to the regions in initial image block

  m_RegionsPointer.resize(m_InitialNumberOfRegions);
  for ( unsigned int iregion = 0; iregion < m_InitialNumberOfRegions; iregion++ )
    {
    m_RegionsPointer[iregion] = KLMSegmentationRegion::New();
    }

  // Label each region

  InputRegionType region;
  region.SetSize(gridSize);   // Constant grid size

  InputImageIndexType tmpIndex;
  tmpIndex.Fill(0);

  for ( RegionLabelType iregion = 0; iregion < m_InitialNumberOfRegions; iregion++ )
    {
    region.SetIndex(tmpIndex * gridSize);

    InitializeRegionParameters(region);
    m_RegionsPointer[iregion]->SetRegionParameters(m_InitialRegionMean,
                                                   m_InitialRegionArea, iregion + 1);

    // Calculate next grid index
    IndexValueType tmpVal = 1;
    for ( unsigned int idim = 0; idim < InputImageDimension; idim++ )
      {
      tmpIndex[idim]++;
      tmpVal *= numRegionsAlongDim[idim];
      if ( ( iregion + 1 ) % tmpVal != 0 ) { break; }
      tmpIndex[idim] = 0;
      }
    } // end iregion loop

  // Determine the borders next and intialize them

  // Allocate and intialize memory to the borders

  unsigned int numberOfBorders = 0;
  for ( unsigned int idim = 0; idim < InputImageDimension; idim++ )
    {
    unsigned int tmpVal = 1;
    for ( unsigned int jdim = 0; jdim < InputImageDimension; jdim++ )
      {
      tmpVal *= ( jdim == idim ?
                  numRegionsAlongDim[jdim] - 1 :
                  numRegionsAlongDim[jdim] );
      }
    numberOfBorders += tmpVal;
    }

  // Allow a single region to pass through; this memory would not be
  // used but the memory allocation and free routine will throw
  // exception otherwise.
  if ( numberOfBorders == 0 )
    {
    itkExceptionMacro(<< "Number of initial regions must be 2 or more: reduce granularity of the grid");
    }

  m_BordersPointer.resize(numberOfBorders);
  for ( unsigned int k = 0; k < m_BordersPointer.size(); k++ )
    {
    m_BordersPointer[k] = KLMSegmentationBorder::New();
    }

  /* the following initialization of the borders ensures that
     each border is assigned region1 and region2 such that,
     the label of region1 is less than the label of region2/
     and, that when a new border is added to a region,
     PushBack can be used for region1 and PushFront can be used
     for region2.  This will ensure that the borders are
     sorted by increased labels for region1 then region2 */

  m_TotalBorderLength = 0;
  unsigned int borderCounter   = 0;

  // Along each dimension, visit every border between two regions

  for ( unsigned int idim = 0; idim < InputImageDimension; idim++ )
    {
    // along the dimension there is one less border than there are regions
    InputImageSizeType numBordersAlongDim = numRegionsAlongDim;
    numBordersAlongDim[idim]--;

    // compute number of borders to be seen this dimension and area of
    // each border
    unsigned int numBorderThisDim = 1;
    double       borderLengthTmp  = 1;
    for ( unsigned int jdim = 0; jdim < InputImageDimension; jdim++ )
      {
      numBorderThisDim *= numBordersAlongDim[jdim];
      borderLengthTmp *= ( jdim == idim ? 1 : gridSize[jdim] * spacing[jdim] );
      }

    // index to atomic region1 and atomic region2
    InputImageIndexType indexRegion1;
    InputImageIndexType indexRegion2;
    indexRegion1.Fill(0);
    indexRegion2.Fill(0);
    indexRegion2[idim]++;

    for ( unsigned int iborder = 0; iborder < numBorderThisDim; iborder++ )
      {
      if ( borderCounter >= numberOfBorders )
        {
        itkExceptionMacro(<< "KLM initialization is incorrect");
        } // end if

      // Load the border of interest
      KLMSegmentationBorderPtr pcurrentBorder = m_BordersPointer[borderCounter];

      // Set the length of the border
      pcurrentBorder->SetBorderLength(borderLengthTmp);

      // m_TotalBorderLength is used as a sanity check
      m_TotalBorderLength += borderLengthTmp;

      // Find the two neighbor regions

      unsigned int intRegion1Index = 0;
      unsigned int intRegion2Index = 0;
      IndexValueType        tmpVal = 1;
      for ( unsigned int jdim = 0; jdim < InputImageDimension; jdim++ )
        {
        intRegion1Index += indexRegion1[jdim] * tmpVal;
        intRegion2Index += indexRegion2[jdim] * tmpVal;
        tmpVal *= numRegionsAlongDim[jdim];
        } // end jdim loop

      KLMSegmentationRegionPtr pRegion1 = m_RegionsPointer[intRegion1Index];
      KLMSegmentationRegionPtr pRegion2 = m_RegionsPointer[intRegion2Index];

      // Attach the region border off lesser label value to region1
      // Attach the region border of the greater label value to region2
      pcurrentBorder->SetRegion1(pRegion1);
      pcurrentBorder->SetRegion2(pRegion2);

      // The current border is linked to the region1 and region2
      // Initialize the border in the region objects
      pRegion1->PushBackRegionBorder(pcurrentBorder);
      pRegion2->PushFrontRegionBorder(pcurrentBorder);

      // Compute the scale parameter lambda
      pcurrentBorder->EvaluateLambda();

      // Increment the border counter to go to the next border
      borderCounter++;

      // Calculate next indices to atomic region1 and atomic region2
      tmpVal = 1;
      for ( unsigned int jdim = 0; jdim < InputImageDimension; jdim++ )
        {
        indexRegion1[jdim]++;
        tmpVal *= numBordersAlongDim[jdim];
        if ( ( iborder + 1 ) % tmpVal != 0 ) { break; }
        indexRegion1[jdim] = 0;
        }
      indexRegion2 = indexRegion1;
      indexRegion2[idim]++;
      }
    }

  // For DEBUG purposes
  if ( this->GetDebug() )
    {
    PrintAlgorithmRegionStats();
    PrintAlgorithmBorderStats();
    }

  // Verification of the initialization process

  double actualBorderLength = 0;
  for ( unsigned int idim = 0; idim < InputImageDimension; idim++ )
    {
    double tmpDblVal = 1;
    for ( unsigned int jdim = 0; jdim < InputImageDimension; jdim++ )
      {
      tmpDblVal *= ( jdim == idim ?
                     numRegionsAlongDim[jdim] - 1 : inputImageSize[jdim] * spacing[jdim] );
      }
    actualBorderLength += tmpDblVal;
    }

  if ( Math::NotAlmostEquals( m_TotalBorderLength, actualBorderLength ) )
    {
    itkExceptionMacro(<< "KLM initialization is incorrect");
    } // end if
  else
    {
    itkDebugMacro(<< "Passed initialization");
    } // end else

  // Allocate memory to store the array of pointers that point to the
  // static border objects

  m_BordersDynamicPointer.resize( m_BordersPointer.size() );
  for ( unsigned int k = 0; k < m_BordersDynamicPointer.size(); k++ )
    {
    m_BordersDynamicPointer[k].m_Pointer = m_BordersPointer[k];
    }

  // For DEBUG purposes
  if ( this->GetDebug() )
    {
    for ( unsigned int k = 0; k < m_BordersDynamicPointer.size(); k++ )
      {
      itkDebugMacro(<< m_BordersDynamicPointer[k].m_Pointer);
      }
    }

  std::stable_sort( m_BordersDynamicPointer.begin(),
                    ( m_BordersDynamicPointer.end() ),
                    std::greater< KLMDynamicBorderArray< BorderType > >() );

  m_BorderCandidate = &( m_BordersDynamicPointer[m_BordersDynamicPointer.size() - 1] );
  m_InternalLambda = m_BorderCandidate->m_Pointer->GetLambda();

  if ( m_InternalLambda < 0.0 )
    {
    itkExceptionMacro(<< "KLM initialization is incorrect");
    }
} // end InitializeKLM()

template< typename TInputImage, typename TOutputImage >
void
KLMRegionGrowImageFilter< TInputImage, TOutputImage >
::InitializeRegionParameters(InputRegionType region)
{
  // Get a pointer to the image
  InputImageConstPointer inputImage = this->GetInput();

  // Set the iterators and the pixel type definition for the input image
  InputImageConstIterator inputIt(inputImage, region);

  // Variable to store the input pixel vector value
  InputImageVectorType inputPixelVec;

  // Calculate V[0] for the constant model facet for the Region Growing
  // algorithm

  m_InitialRegionMean.fill(0);

  while ( ! inputIt.IsAtEnd() )
    {
    inputPixelVec = inputIt.Value();

    for ( unsigned int ivecdim = 0; ivecdim < InputImageVectorDimension; ivecdim++ )
      {
      m_InitialRegionMean[ivecdim] += inputPixelVec[ivecdim];
      }

    ++inputIt;
    }

  // Calculate the area and the mean associated with the region
  GridSizeType gridSize = this->GetGridSize();
  typename TInputImage::SpacingType spacing  = inputImage->GetSpacing();
  m_InitialRegionArea = 1;
  for ( unsigned int idim = 0; idim < InputImageDimension; idim++ )
    {
    m_InitialRegionArea *= gridSize[idim] * spacing[idim];
    }
  m_InitialRegionMean /= m_InitialRegionArea;
} // end InitializeRegionParameters

template< typename TInputImage, typename TOutputImage >
void
KLMRegionGrowImageFilter< TInputImage, TOutputImage >
::MergeRegions()
{
  itkDebugMacro(<< "--------------------");
  itkDebugMacro(<< "   Merging Regions  ");

  // Subtract border length before removing it
  m_TotalBorderLength -= m_BorderCandidate->m_Pointer->GetBorderLength();
  if ( m_TotalBorderLength <= 0 ) { itkExceptionMacro(<< "KLM algorithm error"); }

  // Two regions are associated with the candidate border
  KLMSegmentationRegion *pRegion1;
  KLMSegmentationRegion *pRegion2;

  pRegion1 = m_BorderCandidate->m_Pointer->GetRegion1();
  pRegion2 = m_BorderCandidate->m_Pointer->GetRegion2();

  // For consistency, always assign smaller label: this affects
  // GenerateOutputImage and GenerateLabelledImage
  if ( pRegion1->GetRegionLabel() >= pRegion2->GetRegionLabel() )
    {
    itkExceptionMacro(<< "Invalid region labelling");
    }

  // Add the new region's parameter data to the old.
  pRegion1->CombineRegionParameters(pRegion2);

  // Remove the common region border from region 1 and region 2
  pRegion1->DeleteRegionBorder(m_BorderCandidate->m_Pointer);
  pRegion2->DeleteRegionBorder(m_BorderCandidate->m_Pointer);

  // Assign new equivalence label to the old region and update
  // the region borders, this is needed for ResolveRegions()
  pRegion2->ResetRegionLabelAndUpdateBorders(pRegion1);

  // Merge the borders and region borders of two regions
  pRegion1->SpliceRegionBorders(pRegion2);

  // Do not need the old region borders anymore
  pRegion2->DeleteAllRegionBorders();

  // Recompute the lambda's for all the borders of region1
  pRegion1->UpdateRegionBorderLambda();

  // Remove the common region border from list of sorted borders.
  // The BorderCandidate is always the last element.
  // Set the iterator to very last value and then erase that location
  m_BordersDynamicPointer.erase(m_BordersDynamicPointer.end() - 1);

  // Decrement for the one deleted border and a deleted region
  m_NumberOfRegions--;
  if ( m_BordersDynamicPointer.empty() ) { itkExceptionMacro(<< "KLM algorithm error"); }

  // For DEBUG purposes
  if ( this->GetDebug() )
    {
    itkDebugMacro(<< "First Region ");
    pRegion1->PrintRegionInfo();
    itkDebugMacro(<< "Second Region ");
    pRegion2->PrintRegionInfo();
    }

  // If any duplicate borders are found during SpliceRegionBorders,
  // lambda is set to -1.0, and pRegion1 and pRegion2 are set ITK_NULLPTR
  // so that after this sort, the duplicate border will be the last
  // entry in m_BordersDynamicPointer

  // Resort the border list based on the lambda values
  std::stable_sort( m_BordersDynamicPointer.begin(),
                    ( m_BordersDynamicPointer.end() ),
                    std::greater< KLMDynamicBorderArray< BorderType > >() );

  // Assign new BorderCandidate (it is always the last element).
  // Set Pointer to BorderCandidate to the last element
  m_BorderCandidate = &( m_BordersDynamicPointer[m_BordersDynamicPointer.size() - 1] );
  m_InternalLambda = m_BorderCandidate->m_Pointer->GetLambda();

  // Remove any duplicate borders found during SpliceRegionBorders:
  // lambda = -1.0,  pRegion1 and pRegion2 = ITK_NULLPTR
  while ( m_BorderCandidate->m_Pointer->GetRegion1() == ITK_NULLPTR
          || m_BorderCandidate->m_Pointer->GetRegion2() == ITK_NULLPTR )
    {
    m_BordersDynamicPointer.erase(m_BordersDynamicPointer.end() - 1);

    // Decrement for the one deleted border
    if ( m_BordersDynamicPointer.empty() ) { itkExceptionMacro(<< "KLM algorithm error"); }

    m_BorderCandidate = &( m_BordersDynamicPointer[m_BordersDynamicPointer.size() - 1] );
    m_InternalLambda = m_BorderCandidate->m_Pointer->GetLambda();
    }
} // end MergeRegions

template< typename TInputImage, typename TOutputImage >
void
KLMRegionGrowImageFilter< TInputImage, TOutputImage >
::ResolveRegions()
{
  InputImageConstPointer inputImage = this->GetInput();

  // Scan through the region labels to establish the correspondence
  // between the final region (and label) and the initial regions.

  // Set up the unique label container class
  typedef std::vector< RegionLabelType > UnsignedIntVectorType;
  UnsignedIntVectorType uniqueLabelsVec;

  // Resolve region labels to contain only unique labels.
  // Go backward from largest to smallest region label
  std::vector< KLMSegmentationRegionPtr >::reverse_iterator regionsPointerIt =
    m_RegionsPointer.rbegin();
  std::vector< KLMSegmentationRegionPtr >::reverse_iterator regionsPointerItEnd =
    m_RegionsPointer.rend();
  RegionLabelType iregion = m_InitialNumberOfRegions;
  while ( regionsPointerIt != regionsPointerItEnd )
    {
    RegionLabelType origLabel = iregion;
    iregion--;
    RegionLabelType currLabel = m_RegionsPointer[iregion]->GetRegionLabel();

    // Unresolved chain
    if ( currLabel != origLabel )
      {
      // Resolve a chain of equivalences by first finding the end of the chain
      RegionLabelType uniqLabel = origLabel;
      RegionLabelType tmpLabel  = currLabel;
      while ( uniqLabel != tmpLabel )
        {
        // In memory, the regions go from 0 to label-1. Hence the -1 offset
        uniqLabel = tmpLabel;
        tmpLabel =  m_RegionsPointer[uniqLabel - 1]->GetRegionLabel();
        } // end of chain (while loop)

      // Then re-walk the chain to change the label of each chain
      // member to be the last one just retrieved (uniqLabel)
      while ( currLabel != origLabel )
        {
        m_RegionsPointer[origLabel - 1]->SetRegionLabel(uniqLabel);
        origLabel  = currLabel;
        currLabel = m_RegionsPointer[origLabel - 1]->GetRegionLabel();
        } // end of while ( currLabel != origLabel )
      }   // end of the if condition for detecting unresolved chain

    else // The original label is unique, record it
      {
      uniqueLabelsVec.push_back(origLabel);
      }

    regionsPointerIt++;
    } // end of all regions

  // Sort the unique labels
  std::sort( uniqueLabelsVec.begin(), uniqueLabelsVec.end() );

  // Remap sorted unique labels to consecutive values

  UnsignedIntVectorType remapLabelsVec(m_InitialNumberOfRegions, 0);

  UnsignedIntVectorType::iterator uniqueLabelsVecIterator;
  uniqueLabelsVecIterator = uniqueLabelsVec.begin();

  RegionLabelType newLabelValue = 1;

  while ( uniqueLabelsVecIterator != uniqueLabelsVec.end() )
    {
    remapLabelsVec[*uniqueLabelsVecIterator - 1] = newLabelValue;
    uniqueLabelsVecIterator++;
    newLabelValue++;
    }

  // Assign new consecutive labels
  for ( iregion = 0; iregion < m_InitialNumberOfRegions; iregion++ )
    {
    RegionLabelType labelValue = m_RegionsPointer[iregion]->GetRegionLabel();

    newLabelValue = remapLabelsVec[labelValue - 1];
    double                  newAreaValue = m_RegionsPointer[labelValue - 1]->GetRegionArea();
    MeanRegionIntensityType newMeanValue =
      m_RegionsPointer[labelValue - 1]->GetMeanRegionIntensity();

    m_RegionsPointer[iregion]->SetRegionParameters(newMeanValue,
                                                   newAreaValue,
                                                   newLabelValue);
    } // end looping through the regions
}     // end ResolveRegions()

template< typename TInputImage, typename TOutputImage >
void
KLMRegionGrowImageFilter< TInputImage, TOutputImage >
::PrintAlgorithmRegionStats()
{
  // Print the stats associated with all the regions
  for ( unsigned int k = 0; k < m_InitialNumberOfRegions; k++ )
    {
    int i = static_cast<int>( m_RegionsPointer[k]->GetRegionBorderSize() );
    if ( i > 0 )
      {
      std::cout << "Stats for Region No: "
                << m_RegionsPointer[k]->GetRegionLabel()
                << std::endl;
      m_RegionsPointer[k]->PrintRegionInfo();
      }
    } // end region printloop
}     // end PrintAlgorithmRegionStats

template< typename TInputImage, typename TOutputImage >
void
KLMRegionGrowImageFilter< TInputImage, TOutputImage >
::PrintAlgorithmBorderStats()
{
  // Print the stats associated with all the regions
  for ( unsigned int k = 0; k < m_BordersDynamicPointer.size(); k++ )
    {
    std::cout << "Stats for Border No: " << ( k + 1 ) << std::endl;
    m_BordersDynamicPointer[k].m_Pointer->PrintBorderInfo();
    } // end region printloop
}     // end PrintAlgorithmBorderStats
}     // namespace itk

#endif
