/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKLMRegionGrowImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkKLMRegionGrowImageFilter_txx
#define _itkKLMRegionGrowImageFilter_txx
#include "itkKLMRegionGrowImageFilter.h"

namespace itk
{


template<class TInputImage, class TOutputImage>
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::KLMRegionGrowImageFilter(void):
  m_MaxLambda(1000),
  m_NumberOfBorders(0),
  m_NumRegions(0),
  m_InitRegionArea(0),
  m_BordersCandidateDynamicPointer(NULL),
  m_BordersCandidatePointer(NULL),
  m_ImgWidth(0),
  m_ImgHeight(0),
  m_ImgDepth(0)
{
  m_InitRegionMean = 0;
  this->SetMaximumNumberOfRegions( 2 );
} 

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::~KLMRegionGrowImageFilter()
{
}

//----------------------------------------------------------------------

/*
 * PrintSelf
 */
template <class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "KLM Region grow segmentation object" << std::endl;
  os << indent << "KLM Region grow image filter object" << std::endl;
  os << indent << "Maximum value of lambda parameter: " << m_MaxLambda << std::endl;

}// end PrintSelf

//----------------------------------------------------------------------

/*
 * GenerateInputRequestedRegion method.
 */
template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  // this filter requires the all of the input image to be in
  // the buffer
  InputImagePointer inputPtr = 
    const_cast< InputImageType * >( this->GetInput() );
  inputPtr->SetRequestedRegionToLargestPossibleRegion();
}

//----------------------------------------------------------------------
/*
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
/*
 * GenerateOutputInformation method.
 */
template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::GenerateOutputInformation()
{
  InputImageConstPointer  input  = this->GetInput();
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
  this->ApplyKLM();

  //Set the output labelled and allocate the memory
  OutputImagePointer outputPtr = this->GetOutput();

  //Allocate the output buffer memory 
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  //---------------------------------------------------------------------
  //Get the image width/height/depth
  //--------------------------------------------------------------------- 
  InputImageConstPointer inputImage     = this->GetInput();
  InputImageSize         inputImageSize = inputImage->GetBufferedRegion().GetSize();

  enum { imageDimension = TInputImage::ImageDimension };

  m_ImgWidth  = inputImageSize[0];
  m_ImgHeight = inputImageSize[1];
  if( TInputImage::ImageDimension > 2 ) m_ImgDepth = inputImageSize[2];

  //Generate the output approximation image
  if( ( imageDimension == 2 ) || ( m_ImgDepth == 1 ) ) 
    {
    // 2D initialization routine for the region grwoing algorithm 
    GenerateOutputImage( m_ImgWidth, m_ImgHeight );
    }

  if( ( imageDimension > 2 ) && ( m_ImgDepth > 1 ) ) 
    {
    // 3D initialization routine for the region growing algorithm
    GenerateOutputImage( m_ImgWidth, m_ImgHeight, m_ImgDepth );
    }
        
}// end GenerateData

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::GenerateOutputImage(unsigned int, unsigned int)
{
  //Get the pointer to the output image
  OutputImagePointer outputImage = this->GetOutput(); 

  // Get the vector dimension from the TInputImage paramete
  unsigned int vecDim = OutputImagePixelType::GetVectorDimension();

  //--------------------------------------------------------------------
  // Set the iterators for the output image
  //--------------------------------------------------------------------
  OutputImageIterator  
    outputImageIt( outputImage, outputImage->GetBufferedRegion() );

  OutputImageIterator  outputIt    = outputImageIt.Begin();

  //Set the variable to store the offset
  OutputImageOffsetType offset2D;

  //Set the variable to store the index
  OutputImageIndexType index2D;
  
  //---------------------------------------------------------------------
  //Calculate the initial number of regions 
  //--------------------------------------------------------------------- 
  unsigned int nRowSquareBlocks = m_ImgWidth/( this->GetRowGridSize() );
  unsigned int nColSquareBlocks = m_ImgHeight/( this->GetColGridSize() ); 

  m_NumRegions           = nRowSquareBlocks * nColSquareBlocks;

  int rowGridSize        = this->GetRowGridSize();
  int colGridSize        = this->GetColGridSize();
  int labelValue         = 0;
  int newRegionLabel;

  VecDblType tmpMeanValue;
  unsigned int row_start, row_end, col_start, col_end;

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
      newRegionLabel = m_RegionsPointer[labelValue]->GetRegionLabel();

      // Subtract 1 from the newRegionLabels as the regions are indexed
      // in the memory starting from zero.
      // Get the new mean value of the region.
      tmpMeanValue   = m_RegionsPointer[newRegionLabel-1]->GetMeanRegionIntensity();

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

          //Set the offsets appropriately
          offset2D[0] = ncol;
          offset2D[1] = nrow;

          index2D = outputImageIt.GetIndex();
          index2D += offset2D;
      
          outputImage->SetPixel( index2D, outMeanValue );

          }
        }//end Loop through the region to fill approx image
                                                        
      }//end for stepping throug the col region blocks
    }//end for stepping throug the row region blocks  

}// end GenerateOutputImage()

//----------------------------------------------------------------------
template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::GenerateOutputImage(unsigned int imgWidth, 
                      unsigned int imgHeight,
                      unsigned int imgDepth)
{
  //Get the pointer to the output image
  OutputImagePointer outputImage = this->GetOutput(); 

  // Get the vector dimension from the TInputImage paramete
  unsigned int vecDim = OutputImagePixelType::GetVectorDimension();

  //--------------------------------------------------------------------
  // Set the iterators for the output image
  //--------------------------------------------------------------------
  OutputImageIterator  
    outputImageIt( outputImage, outputImage->GetBufferedRegion() );

  OutputImageIterator  outputIt    = outputImageIt.Begin();

  //Set the variable to store the offset
  OutputImageOffsetType offset3D;

  //Set the variable to store the index
  OutputImageIndexType index3D;
  
  //---------------------------------------------------------------------
  //Calculate the initial number of regions 
  //--------------------------------------------------------------------- 
  unsigned int nRowSquareBlocks = imgWidth/( this->GetRowGridSize() );
  unsigned int nColSquareBlocks = imgHeight/( this->GetColGridSize() ); 
  unsigned int nSliceSquareBlocks = imgDepth/( this->GetSliceGridSize() ); 


  m_NumRegions           = nRowSquareBlocks * 
    nColSquareBlocks *
    nSliceSquareBlocks;

  int rowGridSize        = this->GetRowGridSize();
  int colGridSize        = this->GetColGridSize();
  int sliceGridSize      = this->GetSliceGridSize();
  int labelValue         = 0;
  int newRegionLabel;

  VecDblType tmpMeanValue;
  unsigned int row_start, row_end;
  unsigned int col_start, col_end;
  unsigned int slice_start, slice_end;

  //--------------------------------------------------------------------
  // walk through the entire region and get the mean approximations
  //
  // This is needed as the region labels associated each atomic
  // block (as it was for during the initialization stage), have changed.
  // The new region label points to the region that has the uptodate
  // region intensity. After the updated region intensity is found,
  // each pixel is updated with the mean approximation value.
  //--------------------------------------------------------------------
  for(unsigned int s = 0; s < nSliceSquareBlocks; s++ )
    {
    for(unsigned int r = 0; r < nRowSquareBlocks; r++ )
      {
      for(unsigned int c = 0; c < nColSquareBlocks; c++, labelValue++ )
        {
        newRegionLabel = m_RegionsPointer[labelValue]->GetRegionLabel();

        // Subtract 1 from the newRegionLabels as the regions are indexed
        // in the memory starting from zero.
        // Get the new mean value of the region.
        tmpMeanValue   = m_RegionsPointer[newRegionLabel-1]->GetMeanRegionIntensity();

        OutputImageVectorType outMeanValue;

        typedef typename OutputImagePixelType::ValueType OutputValueType;

        //Get the mean value in the right format
        for (unsigned int j = 0; j < vecDim; j++ )
          outMeanValue[j]  = (OutputValueType) tmpMeanValue[j][0];

        row_start   = r;
        row_end     = row_start + rowGridSize;
        col_start   = c;
        col_end     = col_start + colGridSize;
        slice_start = s;
        slice_end   = slice_start + sliceGridSize;

        // Loop through the each atomic region to fill the 
        // mean approximated pixel value after the region growing operation.
        for (unsigned int nslice = slice_start; nslice < slice_end; nslice++ )
          {
          for (unsigned int nrow = row_start; nrow < row_end; nrow++ ) 
            {
            for (unsigned int ncol = col_start; ncol < col_end; ncol++ ) 
              {

              //Set the offsets appropriately
              offset3D[0] = ncol;
              offset3D[1] = nrow;
              offset3D[2] = nslice;

              index3D = outputImageIt.GetIndex();
              index3D += offset3D;
      
              outputImage->SetPixel( index3D, outMeanValue );

              }
            }
          }//end Loop through the region to fill approx image
                                                        
        }//end for stepping through the col region blocks
      }//end for stepping through the row region blocks  
    }//end for stepping through the slice region blocks

} //End GenerateOutputImage()
//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
typename KLMRegionGrowImageFilter<TInputImage,TOutputImage>::LabelImagePointer
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::GetLabelledImage()
{

  //---------------------------------------------------------------------
  //Get the image width/height/depth
  //--------------------------------------------------------------------- 

  enum { imageDimension = TInputImage::ImageDimension };

  //--------------------------------------------------------------------
  // Allocate the memory for the labelled image
  //--------------------------------------------------------------------
  LabelImagePointer labelImagePtr = LabelImageType::New();

  typename LabelImageType::SizeType labelImageSize 
    = this->GetInput()->GetBufferedRegion().GetSize();
        
  typename LabelImageType::IndexType labelImageIndex;
  labelImageIndex.Fill(0);

  typename LabelImageType::RegionType labelImageRegion;

  labelImageRegion.SetSize( labelImageSize );
  labelImageRegion.SetIndex( labelImageIndex );

  labelImagePtr->SetLargestPossibleRegion( labelImageRegion );
  labelImagePtr->SetBufferedRegion( labelImageRegion );
  labelImagePtr->Allocate();

  //Generate the output approximation image
  if( ( imageDimension == 2 ) || ( m_ImgDepth == 1 ) ) 
    {
    // 2D generate output label routine for the region grwoing algorithm 
    labelImagePtr = localfn_generate_labeled2Dimage( labelImagePtr );
    }

  if( ( imageDimension > 2 ) && ( m_ImgDepth > 1 ) ) 
    {
    // 3D initialization routine for the region growing algorithm
    labelImagePtr = localfn_generate_labeled3Dimage( labelImagePtr );
    }

  return labelImagePtr;

}// end GetLabelledImage()
//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
typename KLMRegionGrowImageFilter<TInputImage,TOutputImage>::LabelImagePointer
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::localfn_generate_labeled2Dimage( LabelImageType *labelImagePtr )
{

  //---------------------------------------------------------------------
  //Get the iterators for the label image
  //---------------------------------------------------------------------
  LabelImageIterator  
    labelImageIt(labelImagePtr, labelImagePtr->GetBufferedRegion());

  LabelImageIterator  labelIt    = labelImageIt.Begin();

  //Set the variable to store the offset
  LabelImageOffsetType offset2D;

  //Set the variable to store the index
  LabelImageIndexType index2D;

  //---------------------------------------------------------------------
  // Loop through the regions and fill the regions with the labels
  //---------------------------------------------------------------------
  //---------------------------------------------------------------------
  //Calculate the initial number of regions 
  //--------------------------------------------------------------------- 
  unsigned int nRowSquareBlocks = m_ImgWidth/( this->GetRowGridSize() );
  unsigned int nColSquareBlocks = m_ImgHeight/( this->GetColGridSize() ); 

  m_NumRegions           = nRowSquareBlocks * nColSquareBlocks;
  int rowGridSize        = this->GetRowGridSize();
  int colGridSize        = this->GetColGridSize();
  int labelValue         = 0;
  int newRegionLabel;

  VecDblType tmpMeanValue;
  unsigned int row_start, row_end, col_start, col_end;

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
      newRegionLabel = m_RegionsPointer[labelValue]->GetRegionLabel();

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

          //Set the offsets appropriately
          offset2D[0] = ncol;
          offset2D[1] = nrow;

          index2D = labelImageIt.GetIndex();
          index2D += offset2D;
      
          labelImagePtr->SetPixel( index2D, newRegionLabel );

          //offset = nrow * m_ImgWidth + ncol;
          //tempImgIt = outImgIt + offset;
          //*tempImgIt = newRegionLabel;
          }
        }
                                                        
      }//end col for loop
    }//end row for loop

  //Return the reference to the labelled image
  return labelImagePtr;
} // end localfn_generate_labeled2Dimage
//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
typename KLMRegionGrowImageFilter<TInputImage,TOutputImage>::LabelImagePointer
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::localfn_generate_labeled3Dimage( LabelImageType *labelImagePtr )
{
  //---------------------------------------------------------------------
  //Get the iterators for the label image
  //---------------------------------------------------------------------
  LabelImageIterator  
    labelImageIt(labelImagePtr, labelImagePtr->GetBufferedRegion());

  LabelImageIterator  labelIt    = labelImageIt.Begin();

  //Set the variable to store the offset
  LabelImageOffsetType offset3D;

  //Set the variable to store the index
  LabelImageIndexType index3D;

  //---------------------------------------------------------------------
  // Loop through the regions and fill the regions with the labels
  //---------------------------------------------------------------------
  //---------------------------------------------------------------------
  //Calculate the initial number of regions 
  //--------------------------------------------------------------------- 
  unsigned int nRowSquareBlocks = m_ImgWidth/( this->GetRowGridSize() );
  unsigned int nColSquareBlocks = m_ImgHeight/( this->GetColGridSize() ); 
  unsigned int nSliceSquareBlocks = m_ImgDepth/( this->GetSliceGridSize() ); 

  m_NumRegions           = nRowSquareBlocks * nColSquareBlocks;
  int rowGridSize        = this->GetRowGridSize();
  int colGridSize        = this->GetColGridSize();
  int sliceGridSize      = this->GetSliceGridSize();
  int labelValue         = 0;
  int newRegionLabel;

  VecDblType tmpMeanValue;
  unsigned int row_start, row_end;
  unsigned int col_start, col_end;
  unsigned int slice_start, slice_end;

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
  for(unsigned int s = 0; s < nSliceSquareBlocks; s++ )
    {
    for(unsigned int r= 0; r < nRowSquareBlocks; r++)
      {
      for(unsigned int c=0; c<nColSquareBlocks;c++,labelValue++)
        {
        newRegionLabel = m_RegionsPointer[labelValue]->GetRegionLabel();

        OutputImageVectorType outMeanValue;

        row_start   = r * rowGridSize;
        row_end     = row_start + rowGridSize;
        col_start   = c * colGridSize;
        col_end     = col_start + colGridSize;
        slice_start = s * sliceGridSize;
        slice_end   = slice_start + sliceGridSize;

        //Loop through the region to fill the new region label
        for (unsigned int nslice = slice_start; nslice < slice_end; nslice++ )
          {
          for (unsigned int nrow = row_start; nrow < row_end; nrow++ ) 
            {
            for (unsigned int ncol = col_start; ncol < col_end; ncol++ ) 
              {

              //Set the offsets appropriately
              offset3D[0] = ncol;
              offset3D[1] = nrow;
              offset3D[2] = nslice;

              index3D = labelImageIt.GetIndex();
              index3D += offset3D;


              labelImagePtr->SetPixel( index3D, newRegionLabel );

 
              //tempImgIt  = outImgIt + offset;
              //*tempImgIt = newRegionLabel;
              }
            }
          }
                                                        
        }//end col for loop
      }//end row for loop
    }//end slice for loop

  //Return the reference to the labelled image
  return labelImagePtr;

}// End localfn_generate_labeled3Dimage()

//----------------------------------------------------------------------
template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::ApplyRegionGrowImageFilter()
{
  this->ApplyKLM();
}// end ApplyRegionGrowImageFilter()

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::ApplyKLM()
{
  //Maximum number of regions requested must be greater than 0
  if( this->GetMaximumNumberOfRegions() <= 0 )
    {
    throw ExceptionObject(__FILE__, __LINE__);
    }


  enum { imageDimension = TInputImage::ImageDimension };

  //---------------------------------------------------------------------
  //Get the image width/height/depth
  //--------------------------------------------------------------------- 
  InputImageConstPointer inputImage     = this->GetInput();
  InputImageSize         inputImageSize = inputImage->GetBufferedRegion().GetSize();

  m_ImgWidth  = inputImageSize[0];
  m_ImgHeight = inputImageSize[1];
  if( TInputImage::ImageDimension > 2 ) m_ImgDepth = inputImageSize[2];

  //--------------------------------------------------------------------------
  // Check for dimensionality and Grid sizes in each dimension
  // The algorithm requires the image dimensions to be multiple of 
  // the user specified grid sizes.
  //--------------------------------------------------------------------------
  if ( imageDimension >= 2 ) 
    {
    if( m_ImgWidth % (this->GetRowGridSize()) != 0 )
      {
      throw ExceptionObject(__FILE__, __LINE__);
      }
    if( m_ImgHeight % (this->GetColGridSize()) != 0 )
      {
      throw ExceptionObject(__FILE__, __LINE__);
      }
    }

  if ( imageDimension == 3 ) 
    {
    if( m_ImgDepth % (this->GetSliceGridSize()) != 0 )
      {
      throw ExceptionObject(__FILE__, __LINE__);
      }
    }

  // Algorithm not supported for data greater than 3D
  if ( imageDimension > 3 )
    {
    throw ExceptionObject(__FILE__, __LINE__);
    }

  //--------------------------------------------------------------------------

  if( ( imageDimension == 2 ) || ( m_ImgDepth == 1 ) ) 
    {

    // 2D initialization routine for the region grwoing algorithm 
    this->InitializeKLM( m_ImgWidth, m_ImgHeight );

    }

  if( ( imageDimension > 2 ) && ( m_ImgDepth > 1 ) ) 
    {
    // 3D initialization routine for the region growing algorithm
    this->InitializeKLM( m_ImgWidth, m_ImgHeight, m_ImgDepth );

    }
  

  //-----------------------------------------------------------------
  // Region merging based on the minimum value of the current
  // lambdas associated with the borders. The border with the
  // smallest lambda value will be taken out for region merging.
  // This growing process is repeated until the number of current
  // different regions is not bigger than the desired and the
  // current minimum scale parameter is not less than the desired scale.
  //-----------------------------------------------------------------


  while(( m_NumRegions   > this->GetMaximumNumberOfRegions() ) &&
        ( m_RegionLambda < m_MaxLambda))
    {
    // subtract border length before removing it
    m_TotalBorderLength -= 
      (m_BordersCandidateDynamicPointer->m_Pointer->GetBorderLength());  

    itkDebugMacro( << "-------------------");
    itkDebugMacro( << "    Before merge   ");
    itkDebugMacro( << "-------------------");

/*
    std::cout << "-------------------" << std::endl;
    std::cout << "    Before merge   " << std::endl;
    std::cout << "-------------------" << std::endl;

    for( unsigned int k = 0; k < m_NumRegions; k++ )
      {
      m_RegionsPointer[k]->PrintRegionInfo(); 
      }
*/

    MergeRegions();

    /*
    itkDebugMacro( << "-------------------");
    itkDebugMacro( << "    After merge    ");
    itkDebugMacro( << "-------------------");
   
   
    for( unsigned int k = 0; k < m_NumRegions; k++ )
      {
      m_RegionsPointer[k]->PrintRegionInfo(); 
      }

    itkDebugMacro( << "-------------------");
    itkDebugMacro( << "    +++++++++++    ");
    itkDebugMacro( << "-------------------");  
    */

    /*
    unsigned int initialNumberOfRegions = m_NumRegions;
    std::cout << "-------------------" << std::endl;
    std::cout << "    After merge   " << std::endl;
    std::cout << "-------------------" << std::endl;

    for( unsigned int k = 0; k < initialNumberOfRegions; k++ )
      {
      m_RegionsPointer[k]->PrintRegionInfo(); 
      }
    */
    // since the number of borders decreases or increases, possibly
    // many times with each iteration, it is reasonable to check
    // for an invalid value 
    if ( m_NumberOfBorders <= 0 ) 
      {
      itkExceptionMacro(<<  "KLM Algorithm error");
      }// end if

    }// end while

  if( ( imageDimension == 2 ) || ( m_ImgDepth == 1 ) ) 
    {

    // 2D initialization routine for the region grwoing algorithm 
    this->ResolveRegionLabels( m_ImgWidth, m_ImgHeight );

    }

  if( ( imageDimension > 2 ) && ( m_ImgDepth > 1 ) ) 
    {
    // 3D initialization routine for the region growing algorithm
    this->ResolveRegionLabels( m_ImgWidth, m_ImgHeight, m_ImgDepth );

    }

}// end ApplyKLM()

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::InitializeKLM( unsigned int imgWidth,
                 unsigned int imgHeight )
{ 

  // Sanity check for the parameters
  if ( ( m_ImgWidth < this->GetRowGridSize() ) ||
       ( m_ImgHeight < this->GetColGridSize() ) )
    throw ExceptionObject(__FILE__, __LINE__);
 
  //---------------------------------------------------------------------
  //Determine the regions first and intiaize them
  //--------------------------------------------------------------------- 

  //---------------------------------------------------------------------
  //Calculate the initial number of regions 
  //--------------------------------------------------------------------- 
  unsigned int nRow             = imgWidth;
  unsigned int nCol             = imgHeight;
  unsigned int nRowSquareBlocks = nRow/( this->GetRowGridSize() );
  unsigned int nColSquareBlocks = nCol/( this->GetColGridSize() ); 
  m_NumRegions                  = nRowSquareBlocks * nColSquareBlocks;

  if( m_NumRegions < this->GetMaximumNumberOfRegions() )
    {
    itkWarningMacro(<< "Reduce granularity of the grid");
    itkWarningMacro(<<"No. of image regions are less than max. no. requested regions");
    }

  //----------------------------------------------------------------------
  //Allocate and intialize memory to the regions in initial image block
  //----------------------------------------------------------------------
  m_RegionsPointer.resize( m_NumRegions );
  for( unsigned int k = 0; k < m_NumRegions; k++ )
    {
    m_RegionsPointer[k] = KLMSegmentationRegion::New();
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

      m_RegionsPointer[labelValue]->SetRegion( m_InitRegionMean,
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
  m_NumberOfBorders = ( nColSquareBlocks - 1 ) * nRowSquareBlocks +
    ( nRowSquareBlocks - 1 ) * nColSquareBlocks;

  // Allow a singe region to pass through; this memory would not be
  // used but the memory allocation and free routine will throw 
  // exception otherwise.
  if( m_NumberOfBorders == 0 ) m_NumberOfBorders = 1;

  m_BordersPointer.resize( m_NumberOfBorders );
  for( unsigned int k = 0; k < m_NumberOfBorders; k++ )
    {
    m_BordersPointer[k] = KLMSegmentationBorder::New();
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

  KLMSegmentationBorder::Pointer pcurrentBorder;

  m_TotalBorderLength = 0;
  unsigned int borderCounter   = 0;

  for ( unsigned int r = nRowSquareBlocks - 1; r >= 1; r-- )
    {
    for ( unsigned int c = 0; c < nColSquareBlocks; c++ ) 
      {
      // Load the border of interest
      pcurrentBorder = m_BordersPointer[borderCounter];

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
        pneighborRegion1 = m_RegionsPointer[ topRegionBlockOffset ];
      
      KLMSegmentationRegionPtr
        pneighborRegion2 = m_RegionsPointer[ bottomRegionBlockOffset ];

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
      pcurrentBorder = m_BordersPointer[borderCounter];

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
        pneighborRegion1 = m_RegionsPointer[ leftRegionBlockOffset ];
      
      KLMSegmentationRegionPtr
        pneighborRegion2 = m_RegionsPointer[ rightRegionBlockOffset ];

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

  if ( this->GetDebug() )
    {
    PrintAlgorithmRegionStats();
    PrintAlgorithmBorderStats();
    }


  // Verification of the initialization process
  if ( m_TotalBorderLength != 
       ( ( nRowSquareBlocks - 1 ) * nColSquareBlocks * colGridSize +
         ( nColSquareBlocks - 1 ) * nRowSquareBlocks * rowGridSize ) ) 
    {
    itkExceptionMacro(<< "Initialization is incorrect");
    throw ExceptionObject(__FILE__, __LINE__);
    }// end if
  else
    {
    itkDebugMacro(<< "Passed initialization.");
    }// end else

  // Allocate memory to store the array of pointers that point to the
  // static border objects

  m_BordersDynamicPointer.resize( m_NumberOfBorders );

  for( unsigned int k = 0; k < m_NumberOfBorders; k++ )
    m_BordersDynamicPointer[ k ].m_Pointer = m_BordersPointer[k];

  // For DEBUG purposes
  if ( this->GetDebug() )
    {
    for( unsigned int k = 0; k < m_NumberOfBorders; k++ )
      {
      itkDebugMacro(<< m_BordersDynamicPointer[ k ].m_Pointer);
      }
    }

  //COMMENT off
  //bool smartPointerUseFlag = true;
  //PrintAlgorithmBorderStats(smartPointerUseFlag);

  std::sort(m_BordersDynamicPointer.begin(), 
            (m_BordersDynamicPointer.end()), 
            std::greater < KLMDynamicBorderArray<BorderType> >());
   
  m_BordersCandidateDynamicPointer = &(m_BordersDynamicPointer[ m_NumberOfBorders - 1 ]);
  m_RegionLambda = m_BordersCandidateDynamicPointer->m_Pointer->GetLambda();

  //COMMENT off
  //bool smartPointerUseFlag = true;
  //PrintAlgorithmBorderStats(smartPointerUseFlag);

  //Sorted border counter
  //For DEBUG purposes
  if ( this->GetDebug() )
    {
    itkDebugMacro( <<"++++++++++++++++++++++++++++++++++++++++");
    itkDebugMacro( <<"     Rearranged Data Structure List      ");
    itkDebugMacro( <<"++++++++++++++++++++++++++++++++++++++++");

    //Rearranged list of the borders
    bool smartPointerUseFlag = true;
    PrintAlgorithmBorderStats(smartPointerUseFlag);
    itkDebugMacro( <<"+++++++++++++++++++++++++++++++++");

    }

}// End localfn_initializeKLM()

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::InitializeKLM( unsigned int imgWidth,
                 unsigned int imgHeight,
                 unsigned int imgDepth )
{ 

  // Sanity check for the parameters
  if ( ( m_ImgWidth < this->GetRowGridSize() ) ||
       ( m_ImgHeight < this->GetColGridSize() ) || 
       ( m_ImgDepth < this->GetSliceGridSize() ) )
    throw ExceptionObject(__FILE__, __LINE__);

  //---------------------------------------------------------------------
  //Determine the regions first and intiaize them
  //--------------------------------------------------------------------- 

  //---------------------------------------------------------------------
  //Calculate the initial number of regions 
  //--------------------------------------------------------------------- 
  unsigned int nRow             = imgWidth;
  unsigned int nCol             = imgHeight;
  unsigned int nSlice           = imgDepth;
  unsigned int nRowSquareBlocks = nRow/( this->GetRowGridSize() );
  unsigned int nColSquareBlocks = nCol/( this->GetColGridSize() ); 
  unsigned int nSliceSquareBlocks = nSlice/( this->GetSliceGridSize() );

  m_NumRegions                  = nRowSquareBlocks * 
    nColSquareBlocks * 
    nSliceSquareBlocks;

  if( m_NumRegions < this->GetMaximumNumberOfRegions() )
    {
    itkWarningMacro(<< "Reduce granularity of the grid");
    itkWarningMacro(<<"No. of image regions are less than max. no. requested regions");
    }

  //----------------------------------------------------------------------
  //Allocate and intialize memory to the regions in initial image block
  //----------------------------------------------------------------------
  m_RegionsPointer.resize( m_NumRegions );
  for( unsigned int k = 0; k < m_NumRegions; k++ )
    {
    m_RegionsPointer[k] = KLMSegmentationRegion::New();
    }

  //----------------------------------------------------------------------
  //Label the regions
  //----------------------------------------------------------------------

  unsigned int rowGridSize        = this->GetRowGridSize();
  unsigned int colGridSize        = this->GetColGridSize();
  unsigned int sliceGridSize      = this->GetSliceGridSize();

  int labelValue                  = 0;

  for( unsigned int s = 0; s < nSliceSquareBlocks; s++)
    {
    for( unsigned int r = 0; r < nRowSquareBlocks; r++ )
      {
      for( unsigned int c = 0; c < nColSquareBlocks; c++, labelValue++ )
        {
        CalculateInitRegionStats( r * rowGridSize,
                                  c * colGridSize,
                                  s * sliceGridSize,
                                  rowGridSize,
                                  colGridSize,
                                  sliceGridSize );
 
        m_RegionsPointer[labelValue]->SetRegion( m_InitRegionMean,
                                                 m_InitRegionArea,
                                                 ( labelValue + 1 ) );
                                                        
        }//end col for loop
      }//end row for loop
    }// end slice for loop

  //---------------------------------------------------------------------
  //Determine the borders next and intiaize them
  //--------------------------------------------------------------------- 
  //----------------------------------------------------------------------
  //Allocate and intialize memory to the borders
  //----------------------------------------------------------------------
  m_NumberOfBorders = ( nColSquareBlocks - 1 ) * 
    nRowSquareBlocks * nSliceSquareBlocks +
    ( nRowSquareBlocks - 1 ) * 
    nColSquareBlocks * nSliceSquareBlocks +
    ( nSliceSquareBlocks - 1 ) * 
    nRowSquareBlocks * nColSquareBlocks;

  // Allow a singe region to pass through; this memory would not be
  // used but the memory allocation and free routine will throw 
  // exception otherwise.
  if( m_NumberOfBorders == 0 ) m_NumberOfBorders = 1;

  m_BordersPointer.resize( m_NumberOfBorders );
  for( unsigned int k = 0; k < m_NumberOfBorders; k++ )
    {
    m_BordersPointer[k] = KLMSegmentationBorder::New();
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


  // horizontal border initialization (for each slice) 

  KLMSegmentationBorder::Pointer pcurrentBorder;

  m_TotalBorderLength = 0;
  unsigned int borderCounter   = 0;
  unsigned int borderLengthTmp = colGridSize * sliceGridSize;
  unsigned int numBlocksInSlice = nColSquareBlocks * nRowSquareBlocks;

  for( unsigned int s = 0; s < nSliceSquareBlocks; s++ )
    {
    for ( unsigned int r = nRowSquareBlocks - 1; r >= 1; r-- )
      {
      for ( unsigned int c = 0; c < nColSquareBlocks; c++ ) 
        {
        // Load the border of interest
        pcurrentBorder = m_BordersPointer[borderCounter];

        //Length of the border 
        pcurrentBorder->SetBorderLength( borderLengthTmp );

        // m_TotalBorderLength is used as a sanity check 
        m_TotalBorderLength += borderLengthTmp;

        // Find the two neighbor regions (top and bottom) 
        int topRegionBlockOffset    = s * numBlocksInSlice + 
          ( r - 1 ) * nColSquareBlocks + c;

        // Effectively calculate ( r * nColSquareBlocks + c );
        int bottomRegionBlockOffset = topRegionBlockOffset + nColSquareBlocks;
                  
        // Assign the 2 neighboring regions of a border
        KLMSegmentationRegionPtr
          pneighborRegion1 = m_RegionsPointer[ topRegionBlockOffset ];
      
        KLMSegmentationRegionPtr
          pneighborRegion2 = m_RegionsPointer[ bottomRegionBlockOffset ];

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
    }// end slice loop

  //End horizontal border processing

  borderLengthTmp = rowGridSize * sliceGridSize;

  // Vertical border initialization
  for( unsigned int s = 0; s < nSliceSquareBlocks; s++ )
    {  
    for (unsigned  int r = 0; r < nRowSquareBlocks; r++ ) 
      {
      for (unsigned  int c = nColSquareBlocks - 1; c >= 1; c-- ) 
        {
        // Point to next border
        pcurrentBorder = m_BordersPointer[borderCounter];

        //Length of the border 
        pcurrentBorder->SetBorderLength(borderLengthTmp);

        // m_TotalBorderLength is used as a sanity check 
        m_TotalBorderLength += borderLengthTmp;

        // Find the two neighbor regions (left and right) 
        int leftRegionBlockOffset    = s * numBlocksInSlice + 
          r * nColSquareBlocks + ( c - 1 );

        // Effectively calculate (numBlocksInSlice + r * nColSquareBlocks + c)
        int rightRegionBlockOffset = leftRegionBlockOffset + 1;

        // Assign the right 
        KLMSegmentationRegionPtr
          pneighborRegion1 = m_RegionsPointer[ leftRegionBlockOffset ];
      
        KLMSegmentationRegionPtr
          pneighborRegion2 = m_RegionsPointer[ rightRegionBlockOffset ];

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
    }//end slice loop


  // End Vertical border processing

  //Slice border initialization
  borderLengthTmp = rowGridSize * colGridSize;

  for( unsigned int s = nSliceSquareBlocks -1; s >= 1; s-- )
    {
    for( unsigned int r = 0; r < nRowSquareBlocks; r++ )
      {
      for( unsigned int c = 0; c < nColSquareBlocks; c++ )
        {
        // Point to next border
        pcurrentBorder = m_BordersPointer[borderCounter];

        //Length of the border 
        pcurrentBorder->SetBorderLength(borderLengthTmp);

        // m_TotalBorderLength is used as a sanity check 
        m_TotalBorderLength += borderLengthTmp;

        // Find the two neighbor regions (left (front) and right (back)) 
        int frontRegionBlockOffset    = (s-1) * numBlocksInSlice + 
          r * nColSquareBlocks + c;

        // Effectively calculate (numBlocksInSlice + r * nColSquareBlocks + c)
        int backRegionBlockOffset = frontRegionBlockOffset + numBlocksInSlice;

        // Assign the right 
        KLMSegmentationRegionPtr
          pneighborRegion1 = m_RegionsPointer[ frontRegionBlockOffset ];
      
        KLMSegmentationRegionPtr
          pneighborRegion2 = m_RegionsPointer[ backRegionBlockOffset ];

        // The current border is linked to the left region (pregion1),
        // and the right (region2)         
        pcurrentBorder->SetRegion1( pneighborRegion1 );
        pcurrentBorder->SetRegion2( pneighborRegion2 );

        // Initialize the border in the region objects
        // attach the (c) region border to the left region          
        pneighborRegion1->SetRegionBorder3d( pcurrentBorder );

        // Attach the (d) region border to the right region 
        pneighborRegion2->SetRegionBorder3d( pcurrentBorder );

        // Compute the scale parameter dlambda 
        pcurrentBorder->EvaluateLambda();

        // Increment the border counter
        borderCounter += 1;
        }//end col loop
      }//end row loop
    }//end slice loop

  // End Slice border processing

  // For DEBUG purposes
  if ( this->GetDebug() )
    {
    PrintAlgorithmRegionStats();
    PrintAlgorithmBorderStats();
    }

  unsigned int actualBorderLength =
    ( nRowSquareBlocks - 1 ) * nColSquareBlocks * nSliceSquareBlocks * 
    colGridSize * sliceGridSize +
    ( nColSquareBlocks - 1 ) * nRowSquareBlocks * nSliceSquareBlocks * 
    rowGridSize * sliceGridSize +
    ( nSliceSquareBlocks - 1 ) * nRowSquareBlocks * nColSquareBlocks * 
    rowGridSize * colGridSize;

  // Verification of the initialization process
  if ( m_TotalBorderLength != actualBorderLength )
    {
    itkExceptionMacro( << "Initialization is incorrect");
    throw ExceptionObject(__FILE__, __LINE__);
    }// end if
  else
    {
    itkDebugMacro( << "Passed initialization.");
    }// end else

  // Allocate memory to store the array of pointers that point to the
  // static border objects

  m_BordersDynamicPointer.resize( m_NumberOfBorders );

  for( unsigned int k = 0; k < m_NumberOfBorders; k++ )
    m_BordersDynamicPointer[ k ].m_Pointer = m_BordersPointer[k];

  // For DEBUG purposes
  if ( this->GetDebug() )
    {
    for( unsigned int k = 0; k < m_NumberOfBorders; k++ )
      itkDebugMacro( << m_BordersDynamicPointer[ k ].m_Pointer);
    }

  std::sort(m_BordersDynamicPointer.begin(), 
            (m_BordersDynamicPointer.end()), 
            std::greater < KLMDynamicBorderArray<BorderType> >());
   
  m_BordersCandidateDynamicPointer = &(m_BordersDynamicPointer[ m_NumberOfBorders - 1 ]);
  m_RegionLambda = m_BordersCandidateDynamicPointer->m_Pointer->GetLambda();

  //COMMENT later
  //bool smartPointerUseFlag = true;
  //PrintAlgorithmBorderStats(smartPointerUseFlag);

  //Sorted border counter
  //For DEBUG purposes
  if ( this->GetDebug() )
    {
    itkDebugMacro( <<"++++++++++++++++++++++++++++++++++++++++");
    itkDebugMacro( <<"     Rearranged Data Structure List      ");
    itkDebugMacro( <<"++++++++++++++++++++++++++++++++++++++++");

    //Rearranged list of the borders
    bool smartPointerUseFlag = true;
    PrintAlgorithmBorderStats(smartPointerUseFlag);

    itkDebugMacro( <<"+++++++++++++++++++++++++++++++++");
    }


  //Rearranged list of the borders
  //
  //  bool smartPointerUseFlag = true;
  //PrintAlgorithmBorderStats(smartPointerUseFlag);

}
 
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
  InputImageConstPointer   inputImage = this->GetInput();
  InputImageConstIterator  inputImageIt( inputImage, 
                                         inputImage->GetBufferedRegion() );

  InputImageConstIterator  inputIt    = inputImageIt.Begin();
  InputImageConstIterator  inputItEnd = inputImageIt.End();

  //Varible to store the input pixel vector value
  InputImageVectorType inputPixelVec;

  //Set a variable to store the offset
  InputImageOffsetType offset2D;

  //Set a variable to store the index
  InputImageIndexType index2D;

  //Calculate V[0] for the constant model facet for the Region Grow
  //algorithm

  m_InitRegionMean.resize( vecDim, 1 );
  m_InitRegionMean.fill(0);

  for ( unsigned int nrow = row_start; nrow < row_end; nrow++ ) 
    {
    for ( unsigned int ncol = col_start; ncol < col_end; ncol++ ) 
      {
      offset2D[0] = ncol;
      offset2D[1] = nrow;
      index2D = inputImageIt.GetIndex();
      index2D += offset2D;
      
      inputPixelVec = inputImage->GetPixel( index2D );

      for ( unsigned int j = 0; j < vecDim; j++ )
        m_InitRegionMean[j][0] += inputPixelVec[j];
      }
    }

  //Calculate the area and the mean associated with the region
  m_InitRegionArea = regionRowGridSize * regionColGridSize;
  m_InitRegionMean /= m_InitRegionArea;

}//end Set Initial Region Stats

//-------------------------------------------------------------------
//----------------------------------------------------------------------
template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::CalculateInitRegionStats( int   regionRowIndex, 
                            int   regionColIndex,
                            int   regionSliceIndex, 
                            int   regionRowGridSize,
                            int   regionColGridSize,
                            int   regionSliceGridSize)
{

  // Get the vector dimension from the TInputImage parameter
  unsigned int vecDim      = InputImagePixelType::GetVectorDimension();
  unsigned int row_start   = regionRowIndex;
  unsigned int row_end     = row_start + regionRowGridSize;
  unsigned int col_start   = regionColIndex;
  unsigned int col_end     = col_start + regionColGridSize;
  unsigned int slice_start = regionSliceIndex;
  unsigned int slice_end   = slice_start + regionSliceGridSize;

  m_InitRegionArea         = 0;

  //--------------------------------------------------------------------
  // Set the iterators and the pixel type definition for the input image
  //-------------------------------------------------------------------
  InputImageConstPointer   inputImage = this->GetInput();
  InputImageConstIterator  inputImageIt( inputImage, 
                                         inputImage->GetBufferedRegion() );

  InputImageConstIterator  inputIt    = inputImageIt.Begin();
  InputImageConstIterator  inputItEnd = inputImageIt.End();

  //Varible to store the input pixel vector value
  InputImageVectorType inputPixelVec;

  //Set a variable to store the offset
  InputImageOffsetType offset3D;

  //Set a variable to store the index
  InputImageIndexType index3D;

  //Calculate V[0] for the constant model facet for the Region Grow
  //algorithm

  m_InitRegionMean.resize( vecDim, 1 );
  m_InitRegionMean.fill(0);

  for( unsigned int nslice = slice_start; nslice < slice_end; nslice++ )
    {
    for ( unsigned int nrow = row_start; nrow < row_end; nrow++ ) 
      {
      for ( unsigned int ncol = col_start; ncol < col_end; ncol++ ) 
        {
        offset3D[0] = ncol;
        offset3D[1] = nrow;
        offset3D[2] = nslice;
        index3D = inputImageIt.GetIndex();
        index3D += offset3D;
      
        inputPixelVec = inputImage->GetPixel( index3D );

        for ( unsigned int j = 0; j < vecDim; j++ )
          m_InitRegionMean[j][0] += inputPixelVec[j];
        }
      }
    }

  //Calculate the area and the mean associated with the region
  m_InitRegionArea = regionRowGridSize * 
    regionColGridSize * 
    regionSliceGridSize;
  m_InitRegionMean /= m_InitRegionArea;

}//end Set Initial Region Stats

//-------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::MergeRegions()
{
  // One region associated with the candidate border
  KLMSegmentationRegion *pRegion1;

  // Second region associated with the candidate border
  KLMSegmentationRegion *pRegion2;

  // For consistency with connected components always assign smaller
  // label: this affects localfn_construct_approx_image and
  // localfn_construct_label_image 
  if( (m_BordersCandidateDynamicPointer->m_Pointer->GetRegion1()->GetRegionLabel() ) <
      (m_BordersCandidateDynamicPointer->m_Pointer->GetRegion2()->GetRegionLabel() ))
    {
    pRegion1 = m_BordersCandidateDynamicPointer->m_Pointer->GetRegion1();
    pRegion2 = m_BordersCandidateDynamicPointer->m_Pointer->GetRegion2();
    }// end if
  else
    {
    itkExceptionMacro( << "Inappropriate region labelling. ");
    throw ExceptionObject(__FILE__, __LINE__);
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

  pRegion1->DeleteRegionBorder( m_BordersCandidateDynamicPointer->m_Pointer ); 

  //---------------------------------------------------------------
  // Remove the common region border from region 2
  //---------------------------------------------------------------
  pRegion2->DeleteRegionBorder( m_BordersCandidateDynamicPointer->m_Pointer ); 

  //---------------------------------------------------------------
  // Clear old pointers associated with the region
  // that relates to the border candidate
  //---------------------------------------------------------------
  m_BordersCandidateDynamicPointer->m_Pointer->SetRegion1( NULL );
  m_BordersCandidateDynamicPointer->m_Pointer->SetRegion2( NULL );

  //---------------------------------------------------------------
  // Remove the common region border from list of sorted borders
  // Set the last border to be NULL and reduce the number of 
  // borders (m_NumberOfBorders) by 1
  //---------------------------------------------------------------
  
  //  *(m_BordersPointer+m_NumberOfBorders-1) = NULL;
  // Decrement for the one deleted border and a deleted region
  m_NumberOfBorders--;
  m_NumRegions--;

  // Merge the borders and region borders of two regions 
  this->UnionBorders( pRegion1, pRegion2 );

  //See if the merging resulted in other pointers in the list
  //that got removed.
  this->UpdateBordersDynamicPointer();

  // For DEBUG purposes
  if ( this->GetDebug() )
    {
    //COME BACK
    itkDebugMacro( << "First Region ");
    pRegion1->PrintRegionInfo();
    itkDebugMacro( << "Second Region ");
    pRegion2->PrintRegionInfo();
    }

  // Recompute the lambda's for all the borders of region1
  pRegion1->UpdateRegionBorderLambda();

  
  //COMMENT OFF
  //bool smartPointerUseFlag = true;
  //PrintAlgorithmBorderStats(smartPointerUseFlag);

  // Resort the border list based on the lambda values
  std::sort( &*(m_BordersDynamicPointer.begin()), 
             &(m_BordersDynamicPointer[m_NumberOfBorders]), 
             std::greater < KLMDynamicBorderArray<BorderType> >() );
  
  //COMMENT OFF
  //bool smartPointerUseFlag = true;
  //PrintAlgorithmBorderStats(smartPointerUseFlag);

  // Sorted border counter
  // For DEBUG purposes
  if ( this->GetDebug() )
    {
    itkDebugMacro( <<"++++++++++++++++++++++++++++++++++++++++");
    itkDebugMacro( <<"     Rearranged Data Structure List      ");
    itkDebugMacro( <<"++++++++++++++++++++++++++++++++++++++++");

    for(unsigned int k=0; k < m_NumberOfBorders; k++)
      {
      itkDebugMacro(<<"Stats for Border No: " << (k+1));
      m_BordersDynamicPointer[k].m_Pointer->PrintBorderInfo() ;                       
      }//end region printloop
    }
  
  //One border has been deleted. So reduce the no. of smart border pointers
  //by 1.
  m_BordersCandidateDynamicPointer = &(m_BordersDynamicPointer[ m_NumberOfBorders - 1 ]);
  m_RegionLambda = m_BordersCandidateDynamicPointer->m_Pointer->GetLambda();

}//end localfn_merge_regions

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::UnionBorders(KLMSegmentationRegion *pnewRegion,
               KLMSegmentationRegion *poldRegion)
{
  KLMSegmentationRegion *ptmpRegion;  

  typedef 
    std::vector< KLMSegmentationBorder* > RegionBorderVecT;
  typedef RegionBorderVecT::iterator RegionBorderVecIt;

  //Point the old region iterators to the appropriate region border to the 
  //head/tail of the vector containers.

  RegionBorderVecIt 
    oldRegionBordersIt    = poldRegion->GetRegionBorderItBegin();
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
    // no longer in ascending order 

    //Ensure that region 1 label is less than  region 2 label 
    if( ( *oldRegionBordersIt )->GetRegion1()->GetRegionLabel() ==
        ( *oldRegionBordersIt )->GetRegion2()->GetRegionLabel() )
      {
      throw ExceptionObject(__FILE__, __LINE__);
      }

    //For DEBUG purposes
    if ( this->GetDebug() )
      {
      //itkDebugMacro(<< "Border in question" << (*oldRegionBordersIt));
      }

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
      itkExceptionMacro( << "Invalid border");
      }// end else

    // Go to the next region border pointed by the iterator
    ++oldRegionBordersIt;

    }// end while

  // For DEBUG purposes
  if ( this->GetDebug() )
    {
    itkDebugMacro( << "New Region ");
    pnewRegion->PrintRegionInfo();
    itkDebugMacro( << "Old Region ");
    poldRegion->PrintRegionInfo();
    }


  // Do the actual union of the borders
  // Point the new region iterators to the appropriate region border to the 
  // head/tail of the vector containers.
  
  RegionBorderVecIt 
    newRegionBordersIt    = pnewRegion->GetRegionBorderItBegin();

  // Refresh the old region iterators
  oldRegionBordersIt    = poldRegion->GetRegionBorderItBegin();
  endOfOldRegionBorders = poldRegion->GetRegionBorderItEnd();

  //Merge the two sets of region borders into the newly merged region

  //For DEBUG purposes
  if ( this->GetDebug() )
    {
    itkDebugMacro( << "The actual merge goes on here");
    itkDebugMacro( << "+++++++++++++++++++++++++++++");
    }

  while( ( newRegionBordersIt != ( pnewRegion->GetRegionBorderItEnd() ) ) &&
         ( oldRegionBordersIt != endOfOldRegionBorders ) )
    {
    // Ensure that there are no common borders in the new region 
    if( ( *newRegionBordersIt )->GetRegion1() ==
        ( *newRegionBordersIt )->GetRegion2() )
      {
      throw ExceptionObject(__FILE__, __LINE__);
      }

    // Ensure that there are no common borders in the old region 
    if( ( *oldRegionBordersIt )->GetRegion1() ==
        ( *oldRegionBordersIt )->GetRegion2() )
      {
      throw ExceptionObject(__FILE__, __LINE__);
      }

    // Ensure that there are no common borders in the old region 
    if( ( *newRegionBordersIt ) == ( *oldRegionBordersIt ) )
      {
      throw ExceptionObject(__FILE__, __LINE__);
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
        itkExceptionMacro( << "Invalid border");
        throw ExceptionObject(__FILE__, __LINE__);
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
      itkExceptionMacro( << "Invalid Region Border");
      }// end else

    }// end of while

  // If anything is remaining in pnew_region_borders, all of it
  // is in the correct part of the list. 
  // If anything is remaining in pold_region_borders add it to the
  // end of the pnew_region_borders list.

  while ( oldRegionBordersIt != endOfOldRegionBorders )
    {

    newRegionBordersIt = pnewRegion->GetRegionBorderItEnd();          
    pnewRegion->InsertRegionBorder( ( newRegionBordersIt + 1 ),
                                    *oldRegionBordersIt );
    oldRegionBordersIt++;

    }//end while

  //Do not need the old region borders anymore
  poldRegion->DeleteAllRegionBorders();

}// end localfn_union_borders

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::UpdateBordersDynamicPointer()
{
  BordersDynamicPointerIterator 
    bordersDynamicPointerIt = m_BordersDynamicPointer.begin();

  for( unsigned int k = 0; k < m_NumberOfBorders; k++)
    {

    if( ( m_BordersDynamicPointer[k].m_Pointer->GetRegion1() == NULL ) ||
        ( m_BordersDynamicPointer[k].m_Pointer->GetRegion2() == NULL ) )
      {  
      m_BordersDynamicPointer.erase( bordersDynamicPointerIt + k);
      k=0;
      m_NumberOfBorders -=  1;
      }
                                                 
    }//end looping through the borders

  
}
//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::ResolveRegionLabels(unsigned int, unsigned int)
{
  //---------------------------------------------------------------------
  //Calculate the initial number of regions 
  //--------------------------------------------------------------------- 
  unsigned int nRowSquareBlocks = m_ImgWidth/( this->GetRowGridSize() );
  unsigned int nColSquareBlocks = m_ImgHeight/( this->GetColGridSize() ); 

  unsigned int numBlocks = nRowSquareBlocks * nColSquareBlocks;

  // Scan through the region labels to establish the correspondence
  // between the final region( and label ) and the initial regions.

  // Resolve region labels to contain only unique labels
  int endOfChain;
  for ( int i = numBlocks - 1; i >= 0; i-- ) // faster when going backward
    {
    unsigned int ncurrBlock = (unsigned int)(i + 1);
    unsigned int nequivBlock = m_RegionsPointer[ ncurrBlock - 1 ]->GetRegionLabel(); 

    // Bounds checking 
    if( nequivBlock > numBlocks || nequivBlock == 0 )
      {
      throw ExceptionObject(__FILE__, __LINE__);
      }

    // unresolved chain 
    if ( nequivBlock != ncurrBlock )  
      {
      // resolve a chain of equivalences by first finding the end of the chain
      while ( nequivBlock != ncurrBlock ) 
        {
        ncurrBlock = nequivBlock;        
                
        //in memory the regions go from 0 to label-1. Hence the -1 offset
        nequivBlock = m_RegionsPointer[ ncurrBlock - 1 ]->GetRegionLabel(); 
        
        // bounds checking 
        if( nequivBlock > numBlocks || nequivBlock == 0 )
          {
          throw ExceptionObject(__FILE__, __LINE__);
          }

        } // end of chain is ncurrBlock (while loop)

      endOfChain = ncurrBlock;

      // Then re-walk the chain to change the label of each chain
      // member to be the last one just retrieved (end_of_chain) 
      ncurrBlock = (unsigned int)(i + 1);
      nequivBlock = m_RegionsPointer[ ncurrBlock - 1 ]->GetRegionLabel(); 

      while ( nequivBlock != ncurrBlock ) 
        {
        m_RegionsPointer[ ncurrBlock - 1 ]->SetRegionLabel( endOfChain );        
        ncurrBlock  = nequivBlock;
        nequivBlock = m_RegionsPointer[ ncurrBlock - 1 ]->GetRegionLabel(); 
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
 
  int labelvalue = m_RegionsPointer[ 0 ]->GetRegionLabel(); //Get the first region value
  uniqueLabelsVec.push_back( labelvalue );

  for ( unsigned int i = 1; i < numBlocks; i++ )
    {
    int labelvalue = m_RegionsPointer[ i ]->GetRegionLabel();
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
    int labelvalue = m_RegionsPointer[ i ]->GetRegionLabel();
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

    m_RegionsPointer[i]->SetRegionLabel(newLabelValue);

    }//end looping through the blocks

}//end resolve_region_labels()

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::ResolveRegionLabels(unsigned int, 
                      unsigned int,
                      unsigned int)
{
  //---------------------------------------------------------------------
  //Calculate the initial number of regions 
  //--------------------------------------------------------------------- 
  unsigned int nRowSquareBlocks = m_ImgWidth/( this->GetRowGridSize() );
  unsigned int nColSquareBlocks = m_ImgHeight/( this->GetColGridSize() ); 
  unsigned int nSliceSquareBlocks = m_ImgDepth/(this->GetSliceGridSize() );

  unsigned int numBlocks = nRowSquareBlocks * 
    nColSquareBlocks *
    nSliceSquareBlocks;

  // Scan through the region labels to establish the correspondence
  // between the final region( and label ) and the initial regions.

  // Resolve region labels to contain only unique labels
  int endOfChain;
  for ( int i = numBlocks - 1; i >= 0; i-- ) // faster when going backward
    {
    unsigned int ncurrBlock = (unsigned int)(i + 1);
    unsigned int nequivBlock = m_RegionsPointer[ ncurrBlock - 1 ]->GetRegionLabel(); 

    // Bounds checking 
    if( nequivBlock > numBlocks || nequivBlock == 0 )
      {
      throw ExceptionObject(__FILE__, __LINE__);
      }

    // unresolved chain 
    if ( nequivBlock != ncurrBlock )  
      {
      // resolve a chain of equivalences by first finding the end of the chain
      while ( nequivBlock != ncurrBlock ) 
        {
        ncurrBlock = nequivBlock;        
                
        //in memory the regions go from 0 to label-1. Hence the -1 offset
        nequivBlock = m_RegionsPointer[ ncurrBlock - 1 ]->GetRegionLabel(); 
        
        // bounds checking 
        if( nequivBlock > numBlocks || nequivBlock == 0 )
          {
          throw ExceptionObject(__FILE__, __LINE__);
          }

        } // end of chain is ncurrBlock (while loop)

      endOfChain = ncurrBlock;

      // Then re-walk the chain to change the label of each chain
      // member to be the last one just retrieved (end_of_chain) 
      ncurrBlock = (unsigned int)(i + 1);
      nequivBlock = m_RegionsPointer[ ncurrBlock - 1 ]->GetRegionLabel(); 

      while ( nequivBlock != ncurrBlock ) 
        {
        m_RegionsPointer[ ncurrBlock - 1 ]->SetRegionLabel( endOfChain );        
        ncurrBlock  = nequivBlock;
        nequivBlock = m_RegionsPointer[ ncurrBlock - 1 ]->GetRegionLabel(); 
        } // end of while ( nequivBlock != ncurrBlock ) 

      }//end of the if condition for detecting unresolved chain

    } // end of all blocks 

  //Reorder the labels for unique representation 
  //Set up the unique label container class

  typedef std::vector<unsigned short> ShortIntVectorType;
  ShortIntVectorType                  uniqueLabelsVec;
  ShortIntVectorType::iterator        uniqueLabelsVecIterator;

  numBlocks = nRowSquareBlocks * 
    nColSquareBlocks *
    nSliceSquareBlocks;
  // Scan through the region labels to identify the resolved region labels.
  // Resolve region labels to contain only unique labels
 
  int labelvalue = m_RegionsPointer[ 0 ]->GetRegionLabel(); //Get the first region value
  uniqueLabelsVec.push_back( labelvalue );

  for ( unsigned int i = 1; i < numBlocks; i++ )
    {
    int labelvalue = m_RegionsPointer[ i ]->GetRegionLabel();
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
    int labelvalue = m_RegionsPointer[ i ]->GetRegionLabel();
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

    m_RegionsPointer[i]->SetRegionLabel(newLabelValue);

    }//end looping through the blocks

}//end resolve_region_labels()

//--------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::PrintAlgorithmRegionStats()
{
  //Print the stats associated with all the regions
  for( unsigned int k = 0; k < m_NumRegions; k++ )
    {
    //itkDebugMacro(<<"Stats for Region No: " << ( k + 1 ));
    //COMMENT ME OFF
    std::cout << "Stats for Region No: " << ( k + 1 ) << std::endl;
    m_RegionsPointer[ k ]->PrintRegionInfo();
    }//end region printloop

}//end PrintAlgorithmRegionStats

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::PrintAlgorithmBorderStats()
{
  //Print the stats associated with all the regions
  for( unsigned int k = 0; k < m_NumberOfBorders; k++)
    {
    //itkDebugMacro(<<"Stats for Border No: " << ( k + 1 ));
    //COMMENT ME OFF
    std::cout << "Stats for Border No: " << ( k + 1 ) << std::endl;
    m_BordersPointer[ k ]->PrintBorderInfo();                                                    
    }//end region printloop

}//end PrintAlgorithmBorderStats

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMRegionGrowImageFilter<TInputImage,TOutputImage>
::PrintAlgorithmBorderStats(bool)
{

  //Print the stats associated with all the regions
  for( unsigned int k = 0; k < m_NumberOfBorders; k++ )
    {
    //itkDebugMacro(<<"Stats for Border No: " << ( k + 1 ));
    //COMMENT ME OFF
    std::cout << "Stats for Border No: " << ( k + 1 ) << std::endl;
    m_BordersDynamicPointer[ k ].m_Pointer->PrintBorderInfo() ;                 
    }//end region printloop

}//end PrintAlgorithmBorderStats

//----------------------------------------------------------------------

} // namespace itk


#endif
