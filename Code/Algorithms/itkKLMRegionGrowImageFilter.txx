/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKLMRegionGrowImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkKLMRegionGrowImageFilter_txx
#define _itkKLMRegionGrowImageFilter_txx

namespace itk
{


template<class TInputImage, class TOutputImage>
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::KLMRegionGrowImageFilter(void):
  m_MaxLambda(1000),
  m_nBorders(0),
  m_NumRegions(0),
  m_InitRegionArea(0),
  m_pBordersCandidateDynPtr(NULL),
  m_pBorderCandidate(NULL)
{
  m_InitRegionMean = 0;
 this->SetMaxNumRegions( 2 );
} 

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::~KLMRegionGrowImageFilter()
{

}

//----------------------------------------------------------------------

/**
 * PrintSelf
 */
template <class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Region grow segmentation" << std::endl;

}// end PrintSelf

//----------------------------------------------------------------------

/**
 * GenerateInputRequestedRegion method.
 */
template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{

  // this filter requires the all of the input image to be in
  // the buffer
  InputImageType inputPtr = this->GetInput();
  inputPtr->SetRequestedRegionToLargestPossibleRegion();
}

//----------------------------------------------------------------------
/**
 * EnlargeOutputRequestedRegion method.
 */
template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::EnlargeOutputRequestedRegion(
DataObject *output )
{

  // this filter requires the all of the output image to be in
  // the buffer
  TOutputImage *imgData;
  imgData = dynamic_cast< TOutputImage* >( output );
  imgData->SetRequestedRegionToLargestPossibleRegion();

}

//----------------------------------------------------------------------
/**
 * GenerateOutputInformation method.
 */
template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::GenerateOutputInformation()
{

  typename TInputImage::Pointer  input  = this->GetInput();
  typename TOutputImage::Pointer output = this->GetOutput();
  output->SetLargestPossibleRegion( input->GetLargestPossibleRegion() );
}

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::GenerateData()
{

  //Run the KLM algorithm
  ApplyKLM();

  //Set the output labelled and allocate the memory
  OutputImageType outputPtr = this->GetOutput();

  //Allocate the output buffer memory 
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  //Generate the output approximation image
  GenerateOutputImage();
        
}// end GenerateData

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::GenerateOutputImage()
{
  //Get the pointer to the output image
  OutputImageType outputPtr = this->GetOutput(); 

  // Get the vector dimension from the TInputImage paramete
  unsigned int vecDim = OutputImagePixelType::GetVectorDimension();

  //--------------------------------------------------------------------
  // Set the iterators for the output image
  //--------------------------------------------------------------------
  OutputImageIterator  
    outputImageIt( outputPtr, outputPtr->GetBufferedRegion() );

  OutputImageIterator  outputIt    = outputImageIt.Begin();

  OutputImagePixelType *tempImgIt  = &(outputIt.Value()); // dangerous!
  OutputImagePixelType *outImgIt   = &(outputIt.Value()); // dangerous!
  
  //---------------------------------------------------------------------
  //Calculate the initial number of regions 
  //--------------------------------------------------------------------- 
  unsigned int nRowSquareBlocks = m_imgWidth/( this->GetRowGridSize() );
  unsigned int nColSquareBlocks = m_imgHeight/( this->GetColGridSize() ); 

  m_NumRegions           = nRowSquareBlocks * nColSquareBlocks;

  int rowGridSize        = this->GetRowGridSize();
  int colGridSize        = this->GetColGridSize();
  int labelValue         = 0;
  int newRegionLabel     = 0;

  VecDblType tmpMeanValue;
  unsigned int row_start, row_end, col_start, col_end, offset;

  //--------------------------------------------------------------------
  // walk through the entire region and get the mean approximations
  //
  // This is needed as the region labels associated each atomic
  // block (as it was for during the initialization stage), have changed.
  // The new region label points to the region that has the uptodate
  // region intensity. After the updated region intensity is found,
  // each pixel is updated with the mean approximation value.
  //--------------------------------------------------------------------
  for(unsigned int r = 0; r < nRowSquareBlocks; r++ )
  {
    for(unsigned int c = 0; c < nColSquareBlocks; c++, labelValue++ )
    {
      newRegionLabel = m_pRegions[labelValue]->GetRegionLabel();

      // Subtract 1 from the newRegionLabels as the regions are indexed
      // in the memory starting from zero.
      // Get the new mean value of the region.
      tmpMeanValue   = m_pRegions[newRegionLabel-1]->GetMeanRegionIntensity();

      OutputImageVectorType outMeanValue;

      typedef typename OutputImagePixelType::ValueType OutputValueType;

      //Get the mean value in the right format
      for (unsigned int j = 0; j < vecDim; j++ )
        outMeanValue[j]  = (OutputValueType) tmpMeanValue[j][0];

      row_start = r;
      row_end   = row_start + rowGridSize;
      col_start = c;
      col_end   = col_start + colGridSize;

      // Loop through the each atomic region to fill the 
      // mean approximated pixel value after the region growing operation.

      for (unsigned int nrow = row_start; nrow < row_end; nrow++ ) 
      {
        for (unsigned int ncol = col_start; ncol < col_end; ncol++ ) 
        {
          offset      = nrow * m_imgWidth + ncol;
          tempImgIt   = outImgIt + offset;
          *tempImgIt = outMeanValue;
         }
      }//end Loop through the region to fill approx image
                                                        
    }//end for stepping throug the col region blocks
  }//end for stepping throug the row region blocks  

}// end GenerateOutputImage()

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
KLMRegionGrowImageFilter<TInputImage,TOutputImage>::LabelImagePointer
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::GetLabelledImage()
{
  //--------------------------------------------------------------------
  // Allocate the memory for the labelled image
  //--------------------------------------------------------------------
  LabelImagePointer labelImagePtr = LabelImageType::New();

  LabelImageType::SizeType labelImageSize 
    = this->GetInput()->GetBufferedRegion().GetSize();
        
  LabelImageType::IndexType labelImageIndex = LabelImageType::IndexType::ZeroIndex;
  LabelImageType::RegionType labelImageRegion;

  labelImageRegion.SetSize( labelImageSize );
  labelImageRegion.SetIndex( labelImageIndex );

  labelImagePtr->SetLargestPossibleRegion( labelImageRegion );
  labelImagePtr->SetBufferedRegion( labelImageRegion );
  labelImagePtr->Allocate();

  //---------------------------------------------------------------------
  //Get the iterators for the label image
  //---------------------------------------------------------------------
  LabelImageIterator  
    labelImageIt(labelImagePtr, labelImagePtr->GetBufferedRegion());

  LabelImageIterator  labelIt    = labelImageIt.Begin();
  LabelImagePixelType *tempImgIt = &(labelIt.Value()); // dangerous!
  LabelImagePixelType *outImgIt  = &(labelIt.Value()); // dangerous!

  //---------------------------------------------------------------------
  // Loop through the regions and fill the regions with the labels
  //---------------------------------------------------------------------
  //---------------------------------------------------------------------
  //Calculate the initial number of regions 
  //--------------------------------------------------------------------- 
  unsigned int nRowSquareBlocks = m_imgWidth/( this->GetRowGridSize() );
  unsigned int nColSquareBlocks = m_imgHeight/( this->GetColGridSize() ); 

  m_NumRegions           = nRowSquareBlocks * nColSquareBlocks;
  int rowGridSize        = this->GetRowGridSize();
  int colGridSize        = this->GetColGridSize();
  int labelValue         = 0;
  int newRegionLabel     = 0;

  VecDblType tmpMeanValue;
  unsigned int row_start, row_end, col_start, col_end, offset;

  //--------------------------------------------------------------------
  // walk through the entire region and get the new unique labels
  // representing the final segmentation.
  //
  // This is needed as the region labels associated each atomic
  // block (as it was during the initialization stage), have changed.
  // The new region label points to the region that has the uptodate
  // region intensity. After the updated region intensity is found,
  // each pixel is updated with the mean approximation value.
  //--------------------------------------------------------------------
  //--------------------------------------------------------------------
  for(unsigned int r= 0; r < nRowSquareBlocks; r++)
  {
    for(unsigned int c=0; c<nColSquareBlocks;c++,labelValue++)
    {
      newRegionLabel = m_pRegions[labelValue]->GetRegionLabel();

      OutputImageVectorType outMeanValue;

      row_start = r*rowGridSize;
      row_end   = row_start + rowGridSize;
      col_start = c*colGridSize;
      col_end   = col_start + colGridSize;

      //Loop through the region to fill the new region label
      for (unsigned int nrow = row_start; nrow < row_end; nrow++ ) 
      {
        for (unsigned int ncol = col_start; ncol < col_end; ncol++ ) 
        {
          offset = nrow * m_imgWidth + ncol;
          tempImgIt = outImgIt + offset;
          ScalarTraits<LabelImagePixelType>::SetScalar( *tempImgIt,
                                                        newRegionLabel );
        }
      }
                                                        
    }//end col for loop
  }//end row for loop

  //Return the reference to the labelled image
  return labelImagePtr;

}// end GetLabelledImage()

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::ApplyRegionGrowImageFilter()
{
  ApplyKLM();
}// end ApplyRegionGrowImageFilter()

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::ApplyKLM()
{
  if( this->GetMaxNumRegions() <= 0 )
    {
    throw ExceptionObject();
    }
  initializeKLM();

  //-----------------------------------------------------------------
  // Region merging based on the minimum value of the current
  // lambdas associated with the borders. The border with the
  // smallest lambda value will be taken out for region merging.
  // This growing process is repeated until the number of current
  // different regions is not bigger than the desired and the
  // current minimum scale parameter is not less than the desired scale.
  //-----------------------------------------------------------------

  while(( m_NumRegions   > this->GetMaxNumRegions() ) &&
        ( m_RegionLambda < m_MaxLambda))
  {
    // subtract border length before removing it
    m_TotalBorderLength -= 
      (m_pBordersCandidateDynPtr->m_Pointer->GetBorderLength());

    MergeRegions();

    // since the number of borders decreases or increases, possibly
    // many times with each iteration, it is reasonable to check
    // for an invalid value 
    if ( m_nBorders <= 0 ) 
    {
      std::cout<<  "KLM Algorithm error" << std::endl;
    }// end if

  }// end while

  resolve_region_labels();

}// end ApplyKLM()

//----------------------------------------------------------------------
/**
 * Merge Regions
 */
template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::MergeRegions()
{

  merge_regions();
}
//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::initializeKLM()
{ 
  //---------------------------------------------------------------------
  //Get the image width/height
  //--------------------------------------------------------------------- 
  InputImageType inputImage     = this->GetInput();
  InputImageSize inputImageSize = inputImage->GetBufferedRegion().GetSize();

  m_imgWidth  = inputImageSize[0];
  m_imgHeight = inputImageSize[1];
  if( TInputImage::ImageDimension > 2 ) m_imgDepth = inputImageSize[2];

  // Sanity check for the parameters
  if ( ( m_imgWidth < this->GetRowGridSize() ) ||
     ( m_imgHeight < this->GetColGridSize() ) )
    throw ExceptionObject();
 
  //---------------------------------------------------------------------
  //Determine the regions first and intiaize them
  //--------------------------------------------------------------------- 

  //---------------------------------------------------------------------
  //Calculate the initial number of regions 
  //--------------------------------------------------------------------- 
  unsigned int nRow             = m_imgWidth;
  unsigned int nCol             = m_imgHeight;
  unsigned int nRowSquareBlocks = nRow/( this->GetRowGridSize() );
  unsigned int nColSquareBlocks = nCol/( this->GetColGridSize() ); 
  m_NumRegions                  = nRowSquareBlocks * nColSquareBlocks;

  if( m_NumRegions < this->GetMaxNumRegions() )
  {
    std::cout<< "Reduce granularity of the grid" << std::endl;
    std::cout<<
      "No. of image regions are less than max. no. requested regions" 
         <<std::endl;
  }

  //----------------------------------------------------------------------
  //Allocate and intialize memory to the regions in initial image block
  //----------------------------------------------------------------------
  m_pRegions.resize( m_NumRegions );
  for( unsigned int k = 0; k < m_NumRegions; k++ )
    {
    m_pRegions[k] = KLMSegmentationRegion<TInputImage,TOutputImage>::New();
    }

  //----------------------------------------------------------------------
  //Label the regions
  //----------------------------------------------------------------------

  unsigned int rowGridSize        = this->GetRowGridSize();
  unsigned int colGridSize        = this->GetColGridSize();
  int labelValue         = 0;

  for( unsigned int r = 0; r < nRowSquareBlocks; r++ )
    {
    for( unsigned int c = 0; c < nColSquareBlocks; c++, labelValue++ )
      {
      CalculateInitRegionStats(r * rowGridSize,
                               c * colGridSize,
                               rowGridSize,
                               colGridSize);

      m_pRegions[labelValue]->SetRegion( m_InitRegionMean,
                                        m_InitRegionArea,
                                        ( labelValue + 1 ) );
                                                        
    }//end col for loop
  }//end row for loop

  //---------------------------------------------------------------------
  //Determine the borders next and intiaize them
  //--------------------------------------------------------------------- 
  //----------------------------------------------------------------------
  //Allocate and intialize memory to the borders
  //----------------------------------------------------------------------
  m_nBorders = ( nColSquareBlocks - 1 ) * nRowSquareBlocks +
               ( nRowSquareBlocks - 1 ) * nColSquareBlocks;

  m_pBorders.resize( m_nBorders );
  for( unsigned int k = 0; k < m_nBorders; k++ )
    {
    m_pBorders[k] = KLMSegmentationBorder<TInputImage,TOutputImage>::New();
    }

  /* 
  the following initialization of the horizontal and vertical
  borders ensures that each borders is assigned region1 and
  region2 such that, for a given region, the labels of
  the neighboring regions are in ascending order.
  For a given horizontal border (-) , the top region is
  linked first (a), and the bottom region is linked second (b).
  For a given vertical border (|) , the left region is linked
  first (c), and the right region is linked second (d).
  The result, for a given region (X), is that the bottom border is
  assigned first, the top border is second, the right border
  is third, and the left border is last.  To have the labels
  of the neighboring regions be in ascending order, the
  region borders must be assigned to the region as [b,d,c,a].
  Therefore, when the region borders are assigned to a region,
  (a) is inserted at the head of the list, then (b)b is inserted
  at the head of the list, then (c) is inserted as the second list
  element, and finally (d) is inserted as the second list element.

  X c | d X c | d X
  a       a       a                X represents a region
  -       -       -                - represents a horizontal border
  b       b       b                | represents a vertical border
  X c | d X c | d X                a,b,c,d represent region borders
  a       a       a
  -       -       -
  b       b       b
  X c | d X c | d X

  Horizontal borders are assigned from the bottom of the image to
  the top.  This ensures that (a) is always the first border
  region assigned to the region.

  Region border (b) is the first border being assigned to the
  region only at last row of regions.  Otherwise it ends up
  at the front of the list, ahead of (a).
  */

  // horizontal border initialization 

  KLMSegmentationBorder<TInputImage,TOutputImage>::Pointer pcurrentBorder;

  m_TotalBorderLength = 0;
  unsigned int borderCounter   = 0;

  for ( unsigned int r = nRowSquareBlocks - 1; r >= 1; r-- )
  {
    for ( unsigned int c = 0; c < nColSquareBlocks; c++ ) 
    {
      // Load the border of interest
      pcurrentBorder = m_pBorders[borderCounter];

      //Length of the border 
      pcurrentBorder->SetBorderLength( colGridSize );

      // m_TotalBorderLength is used as a sanity check 
      m_TotalBorderLength += colGridSize;

      // Find the two neighbor regions (top and bottom) 
      int topRegionBlockOffset    = ( r - 1 ) * nColSquareBlocks + c;

      // Effectively calculate ( r * nColSquareBlocks + c );
      int bottomRegionBlockOffset = topRegionBlockOffset + nColSquareBlocks;
                  
      // Assign the 2 neighboring regions of a border
      KLMSegmentationRegionPtr
        pneighborRegion1 = m_pRegions[ topRegionBlockOffset ];
      
      KLMSegmentationRegionPtr
        pneighborRegion2 = m_pRegions[ bottomRegionBlockOffset ];

      // The current border is linked to the top region (pregion1),
      // and the bottom (region2) 
         
      pcurrentBorder->SetRegion1( pneighborRegion1 );
      pcurrentBorder->SetRegion2( pneighborRegion2 );

      // Initialize the border in the region objects
      // attach the (a) region border to the top region           
      pneighborRegion1->SetRegionBorder( pcurrentBorder );

      // Attach the (b) region border to the bottom region 
      pneighborRegion2->SetRegionBorder( pcurrentBorder );

      // Compute the scale parameter dlambda 
      pcurrentBorder->EvaluateLambda();

      // Increment the border counter to go to the next border
      borderCounter += 1;
       

    }// end col loop
  }//end row loop

  //End horizontal border processing

  // Vertical border initialization
    
  for (unsigned  int r = 0; r < nRowSquareBlocks; r++ ) 
  {
    for (unsigned  int c = nColSquareBlocks - 1; c >= 1; c-- ) 
    {
      // Point to next border
      pcurrentBorder = m_pBorders[borderCounter];

      //Length of the border 
      pcurrentBorder->SetBorderLength(rowGridSize);

      // m_TotalBorderLength is used as a sanity check 
      m_TotalBorderLength += rowGridSize;

      // Find the two neighbor regions (left and right) 
      int leftRegionBlockOffset    = r * nColSquareBlocks + ( c - 1 );

      // Effectively calculate (r * nColSquareBlocks + c)
      int rightRegionBlockOffset = leftRegionBlockOffset + 1;

      // Assign the right 
      KLMSegmentationRegionPtr
        pneighborRegion1 = m_pRegions[ leftRegionBlockOffset ];
      
      KLMSegmentationRegionPtr
        pneighborRegion2 = m_pRegions[ rightRegionBlockOffset ];

      // The current border is linked to the left region (pregion1),
      // and the right (region2)         
      pcurrentBorder->SetRegion1( pneighborRegion1 );
      pcurrentBorder->SetRegion2( pneighborRegion2 );

      // Initialize the border in the region objects
      // attach the (c) region border to the left region          
      pneighborRegion1->SetRegionBorder( pcurrentBorder );

      // Attach the (d) region border to the right region 
      pneighborRegion2->SetRegionBorder( pcurrentBorder );

      // Compute the scale parameter dlambda 
      pcurrentBorder->EvaluateLambda();

      // Increment the border counter
      borderCounter += 1;


    }// end col loop
  }//end row loop

  // End Vertical border processing

  // For DEBUG purposes
#if DEBUG
     PrintAlgorithmRegionStats();
     PrintAlgorithmBorderStats();
#endif

  // Verification of the initialization process
  if ( m_TotalBorderLength != 
       ( ( nRowSquareBlocks - 1 ) * nColSquareBlocks * colGridSize +
         ( nColSquareBlocks - 1 ) * nRowSquareBlocks * rowGridSize ) ) 
  {
    std::cout << "Initialization is incorrect" << std::endl;
    throw ExceptionObject();
  }// end if
  else
  {
    std::cout << "Passed initialization." << std::endl;
  }// end else

  // Allocate memory to store the array of pointers that point to the
  // static border objects

  m_pBordersDynPtrs.resize( m_nBorders );

  for( unsigned int k = 0; k < m_nBorders; k++ )
    m_pBordersDynPtrs[ k ].m_Pointer = m_pBorders[k];

  // For DEBUG purposes
#if DEBUG
  for( unsigned int k = 0; k < m_nBorders; k++ )
    std::cout << m_pBordersDynPtrs[ k ].m_Pointer << std::endl;
#endif

  std::sort(m_pBordersDynPtrs.begin(), 
           (m_pBordersDynPtrs.end()), 
           std::greater < DynamicBorderArrayKLM<BorderType> >());
   
  m_pBordersCandidateDynPtr = &(m_pBordersDynPtrs[ m_nBorders - 1 ]);
  m_RegionLambda = m_pBordersCandidateDynPtr->m_Pointer->GetLambda();


  //Sorted border counter
  //For DEBUG purposes
#if DEBUG
  std::cout <<"++++++++++++++++++++++++++++++++++++++++"<<std::endl;
  std::cout <<"     Rearranged Data Structure List      "<<std::endl;
  std::cout <<"++++++++++++++++++++++++++++++++++++++++"<<std::endl;

  //Rearranged list of the borders
  bool smartPointerUseFlag = true;
  PrintAlgorithmBorderStats(smartPointerUseFlag);

  std::cout <<"+++++++++++++++++++++++++++++++++"<<std::endl;
#endif

}// End localfn_initializeKLM()

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::CalculateInitRegionStats( int   regionRowIndex, 
                            int   regionColIndex, 
                            int   regionRowGridSize,
                            int   regionColGridSize)
{

  // Get the vector dimension from the TInputImage parameter
  unsigned int vecDim      = InputImagePixelType::GetVectorDimension();
  unsigned int row_start   = regionRowIndex;
  unsigned int row_end     = row_start + regionRowGridSize;
  unsigned int col_start   = regionColIndex;
  unsigned int col_end     = col_start + regionColGridSize;

  m_InitRegionArea         = 0;

  //--------------------------------------------------------------------
  // Set the iterators and the pixel type definition for the input image
  //-------------------------------------------------------------------
  InputImageType inputImage = this->GetInput();
  InputImageIterator  inputImageIt( inputImage, 
                                    inputImage->GetBufferedRegion() );

  InputImageIterator  inputIt    = inputImageIt.Begin();
  InputImageIterator  inputItEnd = inputImageIt.End();

  //Varible to store the input pixel vector value
  InputImageVectorType inputPixelVec;

  InputImagePixelType *tempImgIt   = &( inputIt.Value() ); // dangerous!
  InputImagePixelType *inImgIt     = &( inputIt.Value() ); // dangerous!


  //Calculate V[0] for the constant model facet for the Region Grow
  //algorithm

  m_InitRegionMean.resize( vecDim, 1 );
  m_InitRegionMean.fill(NULL);

  for ( unsigned int nrow = row_start; nrow < row_end; nrow++ ) 
  {
    for ( unsigned int ncol = col_start; ncol < col_end; ncol++ ) 
    {
      int offset    = nrow * m_imgWidth + ncol;
      tempImgIt     = inImgIt + offset;
      inputPixelVec = *tempImgIt;

      for ( unsigned int j = 0; j < vecDim; j++ )
        m_InitRegionMean[j][0] += inputPixelVec[j];
    }
  }

  //Calculate the area and the mean associated with the region
  m_InitRegionArea = regionRowGridSize * regionColGridSize;
  m_InitRegionMean /= m_InitRegionArea;

}//end Set Initial Region Stats

//-------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::merge_regions()
{
  // One region associated with the candidate border
  KLMSegmentationRegion<TInputImage,TOutputImage> *pRegion1;

  // Second region associated with the candidate border
  KLMSegmentationRegion<TInputImage,TOutputImage> *pRegion2;

  // For consistency with connected components always assign smaller
  // label: this affects localfn_construct_approx_image and
  // localfn_construct_label_image 
  if( (m_pBordersCandidateDynPtr->m_Pointer->GetRegion1()->GetRegionLabel() ) <
      (m_pBordersCandidateDynPtr->m_Pointer->GetRegion2()->GetRegionLabel() ))
  {
    pRegion1 = m_pBordersCandidateDynPtr->m_Pointer->GetRegion1();
    pRegion2 = m_pBordersCandidateDynPtr->m_Pointer->GetRegion2();
  }// end if
  else
  {
    std::cout << "Inappropriate region labelling. " << std::endl;
    throw ExceptionObject();
  }// end else
  
  //---------------------------------------------------------------
  // Calculate the new region's parameter data. 
  //---------------------------------------------------------------
  VecDblType region1Mean = pRegion1->GetMeanRegionIntensity();
  VecDblType region2Mean = pRegion2->GetMeanRegionIntensity();

  unsigned int region1Area = pRegion1->GetRegionArea();
  unsigned int region2Area = pRegion2->GetRegionArea();

  VecDblType mergedRegionMean = 
    ( (region1Mean * region1Area ) + ( region2Mean * region2Area ) )/
    ( region1Area + region2Area );

  unsigned int mergedRegionArea = region1Area + region2Area;

  //---------------------------------------------------------------
  // Set the new region's parameter data. 
  //---------------------------------------------------------------
  pRegion1->SetRegionArea( mergedRegionArea );
  pRegion1->SetMeanRegionIntensity( mergedRegionMean );

  //---------------------------------------------------------------
  // Remove the common region border from region 1
  //---------------------------------------------------------------
  pRegion1->DeleteRegionBorder( m_pBordersCandidateDynPtr->m_Pointer ); 

  //---------------------------------------------------------------
  // Remove the common region border from region 2
  //---------------------------------------------------------------
  pRegion2->DeleteRegionBorder( m_pBordersCandidateDynPtr->m_Pointer ); 

  // For DEBUG purposes
#if DEBUG

  std::cout << "First Region " << std::endl;
  pRegion1->PrintRegionInfo();
  std::cout << "Second Region " << std::endl;
  pRegion2->PrintRegionInfo();

#endif

  //---------------------------------------------------------------
  // Clear old pointers associated with the region
  // that relates to the border candidate
  //---------------------------------------------------------------
  m_pBordersCandidateDynPtr->m_Pointer->SetRegion1( NULL );
  m_pBordersCandidateDynPtr->m_Pointer->SetRegion2( NULL );

  //---------------------------------------------------------------
  // Remove the common region border from list of sorted borders
  // Set the last border to be NULL and reduce the number of 
  // borders (m_nBorders) by 1
  //---------------------------------------------------------------
  
  //  *(m_pBorders+m_nBorders-1) = NULL;
  // Decrement for the one deleted border and a deleted region
  m_nBorders--;
  m_NumRegions--;

  // Merge the borders and region borders of two regions 
  union_borders( pRegion1, pRegion2 );

  // Recompute the lambda's for all the borders of region1
  pRegion1->UpdateRegionBorderLambda();

  // Resort the border list based on the lambda values
  std::sort( &*(m_pBordersDynPtrs.begin()), 
             &(m_pBordersDynPtrs[m_nBorders]), 
             std::greater < DynamicBorderArrayKLM<BorderType> >() );

  // Sorted border counter
  // For DEBUG purposes
#if DEBUG
  std::cout <<"++++++++++++++++++++++++++++++++++++++++"<<std::endl;
  std::cout <<"     Rearranged Data Structure List      "<<std::endl;
  std::cout <<"++++++++++++++++++++++++++++++++++++++++"<<std::endl;

  for(unsigned int k=0; k < m_nBorders; k++)
  {
    //std::cout<<"Stats for Border No: " << (k+1) << std::endl;
    //m_pBordersDynPtrs[k].m_Pointer->PrintBorderInfo() ;                       
  }//end region printloop
#endif
  
  //One border has been deleted. So reduce the no. of smart border pointers
  //by 1.

   m_pBordersCandidateDynPtr = &(m_pBordersDynPtrs[ m_nBorders - 1 ]);
   m_RegionLambda = m_pBordersCandidateDynPtr->m_Pointer->GetLambda();

}//end localfn_merge_regions

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::union_borders(KLMSegmentationRegion<TInputImage,TOutputImage> *pnewRegion,
                        KLMSegmentationRegion<TInputImage,TOutputImage> *poldRegion)
{

  KLMSegmentationRegion<TInputImage,TOutputImage> *ptmpRegion;  

  typedef 
    std::vector< KLMSegmentationBorder<TInputImage,TOutputImage>* > RegionBorderVecT;
  typedef RegionBorderVecT::iterator RegionBorderVecIt;

  //Point the old region iterators to the appropriate region border to the 
  //head/tail of the vector containers.

  RegionBorderVecIt 
    oldRegionBordersIt = poldRegion->GetRegionBorderItBegin();
  RegionBorderVecIt 
    endOfOldRegionBorders = poldRegion->GetRegionBorderItEnd();

  //Assign smaller label to region 2
  poldRegion->SetRegionLabel( pnewRegion->GetRegionLabel() );

  // Ensure that all borders associated with the old region
  // are in the correct order 
  while( oldRegionBordersIt != endOfOldRegionBorders )
  {
    // make sure the neighbors now point to the new region, and
    // reorder the region borders of the neighbors since they are
    // longer in ascending order 

    //Ensure that region 1 label is less than  region 2 label 
    if( ( *oldRegionBordersIt )->GetRegion1()->GetRegionLabel() ==
        ( *oldRegionBordersIt )->GetRegion2()->GetRegionLabel() )
    {
      throw ExceptionObject();
    }

    // unused: int tmpReg1Label = ( *oldRegionBordersIt )->GetRegion1()->GetRegionLabel();
    // unused: int tmpReg2Label = ( *oldRegionBordersIt )->GetRegion2()->GetRegionLabel();

    //For DEBUG purposes
#ifdef DEBUG
    //std::cout<< "Border in question" << (*oldRegionBordersIt) << std::endl;
#endif

    // correct order 
    if ( ( *oldRegionBordersIt )->GetRegion1()->GetRegionLabel()  >
         ( *oldRegionBordersIt )->GetRegion2()->GetRegionLabel() ) 
    {

      // Swap the regions pointed to by the border 
      ptmpRegion = ( *oldRegionBordersIt )->GetRegion1();
      ( *oldRegionBordersIt )
        ->SetRegion1( ( *oldRegionBordersIt )->GetRegion2() );
      
      ( *oldRegionBordersIt )->SetRegion2( ptmpRegion );

    }// end if (correct order)

    // Point to the newly merged region, and since the region labels
    // have been switched (if the region 1 label of a border is found to be
    // greater than the correspnding region 2 label), the list of borders for 
    // the neighbor region has changed. 

    // The border's region1 is the neighbor 
    if ( ( *oldRegionBordersIt )->GetRegion1() != poldRegion &&
         ( *oldRegionBordersIt )->GetRegion1() != pnewRegion &&
         ( *oldRegionBordersIt )->GetRegion2() == poldRegion   ) 
    {

      // Make the border's region 2 point to the new region 
      ( *oldRegionBordersIt )->SetRegion2( pnewRegion );

      // Reorder the region borders of the neighbor 
      ( *oldRegionBordersIt )
        ->GetRegion1()
           ->ReorderRegionBorders( *oldRegionBordersIt );

    } //end if (the border's region1 is the neighbor)

    // The border's region2 is the neighbor 
    else if ( ( *oldRegionBordersIt )->GetRegion2() != poldRegion &&
              ( *oldRegionBordersIt )->GetRegion2() != pnewRegion &&
              ( *oldRegionBordersIt )->GetRegion1() == poldRegion )
    {
      // make the border's region 1 point to the new region 
      (*oldRegionBordersIt)->SetRegion1(pnewRegion);

      // reorder the region borders of the neighbor 
      ( *oldRegionBordersIt )
        ->GetRegion2()
           ->ReorderRegionBorders( *oldRegionBordersIt );
    }// end else if (the border's region2 is the neighbor)

    else
    {
      std::cout << "Invalid border" << std::endl;
    }// end else

    // Go to the next region border pointed by the iterator
    ++oldRegionBordersIt;

  }// end while

  // Do the actual union of the borders
  // Point the new region iterators to the appropriate region border to the 
  // head/tail of the vector containers.
  
  RegionBorderVecIt 
    newRegionBordersIt    = pnewRegion->GetRegionBorderItBegin();
  RegionBorderVecIt 
    endOfNewRegionBorders = pnewRegion->GetRegionBorderItEnd();

  // Refresh the old region iterators
  oldRegionBordersIt    = poldRegion->GetRegionBorderItBegin();
  endOfOldRegionBorders = poldRegion->GetRegionBorderItEnd();

  //Merge the two sets of region borders into the newly merged region
  //For DEBUG purposes
#ifdef DEBUG
  std::cout << "The actual merge goes on here"<<std::endl;
  std::cout << "+++++++++++++++++++++++++++++"<<std::endl;
#endif

  while( ( newRegionBordersIt != ( pnewRegion->GetRegionBorderItEnd() ) ) &&
         ( oldRegionBordersIt != endOfOldRegionBorders ) )
  {

    // Ensure that there are no common borders in the new region 
    if( ( *newRegionBordersIt )->GetRegion1() ==
        ( *newRegionBordersIt )->GetRegion2() )
    {
      throw ExceptionObject();
    }

    // Ensure that there are no common borders in the old region 
    if( ( *oldRegionBordersIt )->GetRegion1() ==
        ( *oldRegionBordersIt )->GetRegion2() )
    {
      throw ExceptionObject();
    }

    // Ensure that there are no common borders in the old region 
    if( ( *newRegionBordersIt ) == ( *oldRegionBordersIt ) )
    {
      throw ExceptionObject();
    }

    // The two borders point to the same regions, they must be merged
    // into one border 

    if ( ( ( *newRegionBordersIt )->GetRegion1() ==
           ( *oldRegionBordersIt )->GetRegion1() ) &&
         ( ( *newRegionBordersIt )->GetRegion2() ==
           ( *oldRegionBordersIt )->GetRegion2() ) ) 
    {

      // Add the lengths of the borders 
      unsigned int 
        tempOldRegionBorderLength = ( *oldRegionBordersIt )->GetBorderLength();
      unsigned int 
        tempNewRegionBorderLength = ( *newRegionBordersIt )->GetBorderLength();

      ( *newRegionBordersIt )->SetBorderLength(tempOldRegionBorderLength +
                                               tempNewRegionBorderLength);

      // The border's region1 is the neighbor 
      if ( ( *oldRegionBordersIt )->GetRegion1() != pnewRegion &&
           ( *oldRegionBordersIt )->GetRegion2() == pnewRegion ) 
      {
        ( *oldRegionBordersIt )
          ->GetRegion1()
             ->DeleteRegionBorder( ( *oldRegionBordersIt ) );

      }// end if (the border's region1 is the neighbor )

      // The border's region2 is the neighbor 
      else if ( ( *oldRegionBordersIt )->GetRegion2() != pnewRegion &&
                ( *oldRegionBordersIt )->GetRegion1() == pnewRegion ) 
      {

        ( *oldRegionBordersIt )
          ->GetRegion2()
              ->DeleteRegionBorder( ( *oldRegionBordersIt ) );

      }//end else if (the border's region2 is the neighbor )

      else 
      {
         std::cout << "Invalid border"  << std::endl;
         throw ExceptionObject();
      }// end else

      // remove the old region border by pointing to next region
      //   border
      ( *oldRegionBordersIt )->SetRegion1( NULL );
      ( *oldRegionBordersIt )->SetRegion2( NULL );
      ( *oldRegionBordersIt ) = NULL;
      oldRegionBordersIt++;

    }// end if loop for case when two borders point to same region

    // The new neighbor region label is less then that of
    // the old neighbor region label 
    else if ( (   ( *newRegionBordersIt )->GetRegion1()->GetRegionLabel() <
                  ( *oldRegionBordersIt )->GetRegion1()->GetRegionLabel() ) ||

              ( ( ( *newRegionBordersIt )->GetRegion1()->GetRegionLabel() ==
                  ( *oldRegionBordersIt )->GetRegion1()->GetRegionLabel() ) &&

                ( ( *newRegionBordersIt )->GetRegion2()->GetRegionLabel() <
                  ( *oldRegionBordersIt )->GetRegion2()->GetRegionLabel() ) ) ) 
    {

      // The region border is already in the correct stop in the list 
      // point to next region border 
      newRegionBordersIt++;

    }// end else if clause

    // The old neighbor region label is less then that of
    // the new neighbor region label 
    else if ( (   ( *oldRegionBordersIt )->GetRegion1()->GetRegionLabel() <
                  ( *newRegionBordersIt )->GetRegion1()->GetRegionLabel() ) ||

              ( ( ( *oldRegionBordersIt )->GetRegion1()->GetRegionLabel() ==
                  ( *newRegionBordersIt )->GetRegion1()->GetRegionLabel() ) &&

                ( ( *oldRegionBordersIt )->GetRegion2()->GetRegionLabel() <
                  ( *newRegionBordersIt )->GetRegion2()->GetRegionLabel() ) ) ) 
    {

      // Add the region border from the old region to the list of
      // region border of the new region right before the current
      // position 
      if( newRegionBordersIt == (pnewRegion->GetRegionBorderItBegin())) 
      {
        pnewRegion->InsertRegionBorder( newRegionBordersIt,
                                        *oldRegionBordersIt );
        oldRegionBordersIt++;
        newRegionBordersIt++;
  
       }//end if

      else 
      {

        pnewRegion->InsertRegionBorder( ( newRegionBordersIt ),
                                        *oldRegionBordersIt);           
        oldRegionBordersIt++;

        // Increment this pointer as 1 extra border has been added to the 
        // list of borders defining the new region
        newRegionBordersIt++;
      }//end else

    }// end else if
    else 
    {
      std::cout << "Invalid Region Border" << std::endl;
    }// end else

  }// end of while

  // If anything is remaining in pnew_region_borders, all of it
  // is in the correct part of the list. 
  // If anything is remaining in pold_region_borders add it to the
  // end of the pnew_region_borders list.

  endOfNewRegionBorders = pnewRegion->GetRegionBorderItEnd();

  while ( oldRegionBordersIt != endOfOldRegionBorders )
  {

    newRegionBordersIt = pnewRegion->GetRegionBorderItEnd();          
    pnewRegion->InsertRegionBorder( ( newRegionBordersIt + 1 ),
                                    *oldRegionBordersIt );
    oldRegionBordersIt++;

  }//end while

}// end localfn_union_borders

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::resolve_region_labels()
{
  //---------------------------------------------------------------------
  //Calculate the initial number of regions 
  //--------------------------------------------------------------------- 
  unsigned int nRowSquareBlocks = m_imgWidth/( this->GetRowGridSize() );
  unsigned int nColSquareBlocks = m_imgHeight/( this->GetColGridSize() ); 

  unsigned int numBlocks = nRowSquareBlocks * nColSquareBlocks;
  int rowGridSize        = this->GetRowGridSize();
  int colGridSize        = this->GetColGridSize();

  // Scan through the region labels to establish the correspondence
  // between the final region( and label ) and the initial regions.

  // Resolve region labels to contain only unique labels
  int endOfChain;
  for ( int i = numBlocks - 1; i >= 0; i-- ) // faster when going backward
  {
    unsigned int ncurrBlock = (unsigned int)(i + 1);
    unsigned int nequivBlock = m_pRegions[ ncurrBlock - 1 ]->GetRegionLabel(); 

    // Bounds checking 
    if( nequivBlock > numBlocks || nequivBlock == 0 )
    {
      throw ExceptionObject();
    }

    // unresolved chain 
    if ( nequivBlock != ncurrBlock )  
    {
      // resolve a chain of equivalences by first finding the end of the chain
      while ( nequivBlock != ncurrBlock ) 
      {
        ncurrBlock = nequivBlock;        
                
        //in memory the regions go from 0 to label-1. Hence the -1 offset
        nequivBlock = m_pRegions[ ncurrBlock - 1 ]->GetRegionLabel(); 
        
        // bounds checking 
        if( nequivBlock > numBlocks || nequivBlock == 0 )
        {
          throw ExceptionObject();
        }

      } // end of chain is ncurrBlock (while loop)

      endOfChain = ncurrBlock;

      // Then re-walk the chain to change the label of each chain
      // member to be the last one just retrieved (end_of_chain) 
      ncurrBlock = (unsigned int)(i + 1);
      nequivBlock = m_pRegions[ ncurrBlock - 1 ]->GetRegionLabel(); 

      while ( nequivBlock != ncurrBlock ) 
      {
         m_pRegions[ ncurrBlock - 1 ]->SetRegionLabel( endOfChain );        
         ncurrBlock  = nequivBlock;
         nequivBlock = m_pRegions[ ncurrBlock - 1 ]->GetRegionLabel(); 
      } // end of while ( nequivBlock != ncurrBlock ) 

    }//end of the if condition for detecting unresolved chain

  } // end of all blocks 

  //Reorder the labels for unique representation 
  //Set up the unique label container class

  typedef std::vector<unsigned short> ShortIntVectorType;
  ShortIntVectorType                  uniqueLabelsVec;
  ShortIntVectorType::iterator        uniqueLabelsVecIterator;

  numBlocks =  nRowSquareBlocks * nColSquareBlocks;

  // Scan through the region labels to identify the resolved region labels.
  // Resolve region labels to contain only unique labels
 
  int labelvalue = m_pRegions[ 0 ]->GetRegionLabel(); //Get the first region value
  uniqueLabelsVec.push_back( labelvalue );
  uniqueLabelsVecIterator = uniqueLabelsVec.begin();  


  for ( unsigned int i = 1; i < numBlocks; i++ )
  {
    int labelvalue = m_pRegions[ i ]->GetRegionLabel();
    bool uniqueLabelsFlag = false;
    uniqueLabelsVecIterator = uniqueLabelsVec.begin();
    
    while (uniqueLabelsVecIterator != uniqueLabelsVec.end())        
    {
      if( ( *uniqueLabelsVecIterator ) == labelvalue) 
      {
        uniqueLabelsFlag = false;
        break;
      }
      else
      {
        uniqueLabelsFlag = true;
        uniqueLabelsVecIterator++;
      }                                      
    }//end while

    if(uniqueLabelsFlag == true) uniqueLabelsVec.push_back( labelvalue );
  }

  //Unique labels have been identified now remap them
  for ( unsigned int i = 0; i < numBlocks; i++ )
  {
    int labelvalue = m_pRegions[ i ]->GetRegionLabel();
    int newLabelValue = 1;
    uniqueLabelsVecIterator = uniqueLabelsVec.begin();

    while ( uniqueLabelsVecIterator != uniqueLabelsVec.end() )        
    {
      if( ( *uniqueLabelsVecIterator ) != labelvalue ) 
      {
        uniqueLabelsVecIterator++;
        newLabelValue++;
      }
      else
      {
        break;
      }                                      
    }//end while

    m_pRegions[i]->SetRegionLabel(newLabelValue);

  }//end looping through the blocks

}//end resolve_region_labels()

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::PrintAlgorithmRegionStats()
{
  //Print the stats associated with all the regions
  for( unsigned int k = 0; k < m_NumRegions; k++ )
  {
    std::cout<<"Stats for Region No: " << ( k + 1 ) << std::endl;
    m_pRegions[ k ].PrintRegionInfo();                                                    
  }//end region printloop

}//end PrintAlgorithmRegionStats

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::PrintAlgorithmBorderStats()
{
  //Print the stats associated with all the regions
  for( unsigned int k = 0; k < m_nBorders; k++)
  {
    std::cout<<"Stats for Border No: " << ( k + 1 ) << std::endl;
    m_pBorders[ k ].PrintBorderInfo();                                                    
  }//end region printloop

}//end PrintAlgorithmBorderStats

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::PrintAlgorithmBorderStats(bool smartBorderPointerUseFlag)
{

  //Print the stats associated with all the regions
  for( unsigned int k = 0; k < m_nBorders; k++ )
  {
    std::cout<<"Stats for Border No: " << ( k + 1 ) << std::endl;
    m_pBordersDynamicPtrs[ k ].m_Pointer->PrintBorderInfo() ;                 
  }//end region printloop

}//end PrintAlgorithmBorderStats

//----------------------------------------------------------------------

} // namespace itk


#endif
