/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImagePCAShapeModelEstimator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImagePCAShapeModelEstimator_txx
#define _itkImagePCAShapeModelEstimator_txx

#include "itkImagePCAShapeModelEstimator.h"

namespace itk
{

template<class TInputImage, class TOutputImage>
ImagePCAShapeModelEstimator<TInputImage,TOutputImage>
::ImagePCAShapeModelEstimator(void):m_NumberOfPixels(0),m_NumberOfTrainingImages(0)
{
  m_EigenVectors.resize(0,0);
  m_EigenValues.resize(0);

  m_NumberOfPrincipalComponentsRequired = 0;
  this->SetNumberOfPrincipalComponentsRequired( 1 );

}

template<class TInputImage, class TOutputImage>
ImagePCAShapeModelEstimator<TInputImage, TOutputImage>
::~ImagePCAShapeModelEstimator(void)
{

}

/**
 * PrintSelf
 */
template <class TInputImage, class TOutputImage>
void
ImagePCAShapeModelEstimator<TInputImage, TOutputImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{  

  os << indent << "                   " << std::endl;
  os << indent << "Shape Models " << std::endl;
  os << indent << "Results printed in the superclass " << std::endl;
  os << indent << "                   " << std::endl;

  Superclass::PrintSelf(os,indent);

  itkDebugMacro(<<"                                    ");
  itkDebugMacro(<<"Results of the shape model algorithms");
  itkDebugMacro(<<"====================================");

  itkDebugMacro(<< "The eigen values new method are: ");
    
  itkDebugMacro(<< m_EigenValues);
  itkDebugMacro(<< m_EigenVectorNormalizedEnergy);

  itkDebugMacro(<< " ");
  itkDebugMacro(<< "==================   ");

  itkDebugMacro(<< "The eigen vectors new method are: ");


  for(unsigned int i = 0; i < m_EigenValues.size(); i++) 
    {
    itkDebugMacro(<< m_EigenVectors.get_row(i));
    }  

  itkDebugMacro(<< " ");
  itkDebugMacro(<< "+++++++++++++++++++++++++");

  // Print out ivars
  os << indent << "NumberOfPrincipalComponentsRequired: ";
  os << m_NumberOfPrincipalComponentsRequired << std::endl;
  os << indent << "NumberOfTrainingImages: ";
  os << m_NumberOfTrainingImages << std::endl;


}// end PrintSelf

/**
 * Enlarge the output requested region to the largest possible region.
 */
template<class TInputImage, class TOutputImage>
void
ImagePCAShapeModelEstimator<TInputImage,TOutputImage>
::EnlargeOutputRequestedRegion( DataObject * output )
{

  // this filter requires the all of the output image to be in
  // the buffer
  TOutputImage *imgData;
  imgData = dynamic_cast<TOutputImage*>( output );
  if ( imgData )
    {
    imgData->SetRequestedRegionToLargestPossibleRegion();
    }
  else
    {
    // pointer could not be cast to TOutputImage *
    itkWarningMacro(<< "itk::ImagePCAShapeModelEstimator" <<
              "::EnlargeOutputRequestedRegion cannot cast "
              << typeid(output).name() << " to "
              << typeid(TOutputImage*).name() );

    }

}

/**
 * Requires all of the inputs to be in the buffer up to the
 * LargestPossibleRegion of the first input.
 */
template<class TInputImage, class TOutputImage>
void
ImagePCAShapeModelEstimator<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  if ( this->GetInput(0) )
    {

    // Set the requested region of the first input to largest possible region
    InputImagePointer input = const_cast< TInputImage *>( this->GetInput(0) );
    input->SetRequestedRegionToLargestPossibleRegion();

    // Set the requested region of the remaining input to the largest possible
    // region of the first input
    unsigned int idx;
    for (idx = 1; idx < this->GetNumberOfInputs(); ++idx)
      {
      if ( this->GetInput(idx) )
        {
        typename TInputImage::RegionType requestedRegion = 
          this->GetInput(0)->GetLargestPossibleRegion();

        typename TInputImage::RegionType largestRegion =
          this->GetInput(idx)->GetLargestPossibleRegion();

        if( !largestRegion.IsInside( requestedRegion ) )
          {
          itkExceptionMacro("LargestPossibleRegion of input " << idx <<
            " is not a superset of the LargestPossibleRegion of input 0" );
          }

        InputImagePointer ptr = const_cast<TInputImage *>( this->GetInput(idx) );
        ptr->SetRequestedRegion( requestedRegion );

        } // if ( this->GetOutput(idx))
      } // for idx
    } // if( this->GetOutput(0) )

}

/**
 * Generate data (start the model building process)
 */
template<class TInputImage, class TOutputImage>
void 
ImagePCAShapeModelEstimator<TInputImage, TOutputImage>
::GenerateData( )
{
  this->EstimateShapeModels();

  // Allocate memory for each output.
  unsigned int numberOfOutputs = 
    static_cast<unsigned int>( this->GetNumberOfOutputs() );

  InputImagePointer input = const_cast<TInputImage *>( this->GetInput(0) );
  for( unsigned int j = 0; j < numberOfOutputs; j++ )
    {

    OutputImagePointer output = this->GetOutput( j );
    output->SetBufferedRegion( output->GetRequestedRegion() );
    output->Allocate();

    }

  // Fill the output images.
  VectorOfDoubleType m_OneEigenVector;
  typedef ImageRegionIterator<OutputImageType>     OutputIterator;

  //Fill the mean image first

  typename OutputImageType::RegionType region = this->GetOutput( 0 )->GetRequestedRegion();
  OutputIterator outIter( this->GetOutput( 0 ), region ); 

  unsigned int i = 0;
  outIter.GoToBegin();
  while( !outIter.IsAtEnd() )
    {
    outIter.Set( static_cast<typename OutputImageType::PixelType>(m_Means[i]));
    ++outIter;
    ++i;
    } 

  //Now fill the m_NumberOfPrincipal components
  unsigned int kthLargestPrincipalComp = m_NumberOfTrainingImages;

  for( unsigned int j = 1; j < numberOfOutputs; j++ )
    {

    //Extract one column vector at a time
    m_OneEigenVector = m_EigenVectors.get_column(kthLargestPrincipalComp-1);


    typename OutputImageType::RegionType region = this->GetOutput( j )->GetRequestedRegion();
    OutputIterator outIter( this->GetOutput( j ), region );

    unsigned int idx = 0;
    outIter.GoToBegin();
    while( !outIter.IsAtEnd() )
      {
      outIter.Set( static_cast<typename OutputImageType::PixelType>( 
        m_OneEigenVector[ idx ] ) );
      ++outIter;
      ++idx;
      }

    //Decrement to get the next principal component
    --kthLargestPrincipalComp;
    }

}// end Generate data


/**
 * Set the number of training images.
 */
template<class TInputImage, class TOutputImage>
void
ImagePCAShapeModelEstimator<TInputImage,TOutputImage>
::SetNumberOfPrincipalComponentsRequired( unsigned int n )
{
  if ( m_NumberOfPrincipalComponentsRequired != n )
    {
    m_NumberOfPrincipalComponentsRequired = n;

    this->Modified();

    // Modify the required number of inputs and outputs ( 1 extra 
    // for the mean image ) 
    this->SetNumberOfRequiredInputs( m_NumberOfTrainingImages );
    this->SetNumberOfRequiredOutputs( m_NumberOfPrincipalComponentsRequired + 1 );

    unsigned int numberOfOutputs = static_cast<unsigned int>( this->GetNumberOfOutputs() );
    unsigned int idx;

    if( numberOfOutputs < m_NumberOfTrainingImages )
      {
      // Make and add extra outputs
      for( idx = numberOfOutputs; idx < m_NumberOfTrainingImages; idx++ )
        {
        typename DataObject::Pointer output = this->MakeOutput( idx );
        this->SetNthOutput( idx, output.GetPointer() );
        }
      }
    else if ( numberOfOutputs > m_NumberOfTrainingImages )
      {
      // Remove the extra outputs
      for ( idx = m_NumberOfTrainingImages; idx < numberOfOutputs; idx++ )
        {
        typename DataObject::Pointer output = this->GetOutputs()[idx];
        this->RemoveOutput( output );
        }
      }

    }

}

/**-----------------------------------------------------------------
 * Takes a set of training images and returns the means 
 * and variance of the various classes defined in the
 * training set.
 */
template<class TInputImage, class TOutputImage>
void 
ImagePCAShapeModelEstimator<TInputImage, TOutputImage>
::EstimateShapeModels()
{

  this->CalculateInnerProduct();

  this->EstimatePCAShapeModelPrameters(); 

}// end EstimateShapeModels 

/**
 * Calculate the inner product between the input training vector
 * where each image is treated as a vector of n-elements
 */
template<class TInputImage, class TOutputImage>
void 
ImagePCAShapeModelEstimator<TInputImage, TOutputImage>
::CalculateInnerProduct()
{
  // Get the pointers to the input images and initialize the iterators
  // We use dynamic_cast since inputs are stored as DataObjects.  The
  // ImageToImageFilter::GetInput(int) always returns a pointer to a
  // TInputImage1 so it cannot be used for the second input.

  InputImagePointerArray  inputImagePointerArray( m_NumberOfTrainingImages );
  m_InputImageIteratorArray.resize( m_NumberOfTrainingImages );


  for( unsigned int i=0; i< m_NumberOfTrainingImages; i++ )
    {

    InputImageConstPointer inputImagePtr
      = dynamic_cast<const TInputImage*>(ProcessObject::GetInput(i));
    
    inputImagePointerArray[i] = inputImagePtr;

    InputImageConstIterator inputImageIt( inputImagePtr, inputImagePtr->GetBufferedRegion() );

    m_InputImageIteratorArray[i] = inputImageIt;

    m_InputImageIteratorArray[i].GoToBegin();
    
    }

  //-------------------------------------------------------------------
  // Set up the matrix to hold the inner product and the means from the
  // training data
  //-------------------------------------------------------------------
  m_InputImageSize = (inputImagePointerArray[0])->GetBufferedRegion().GetSize();

  m_NumberOfPixels = 1;
  for( unsigned int i = 0; i < InputImageDimension; i++ )
    {
    m_NumberOfPixels *= m_InputImageSize[i]; 
    }

  //-------------------------------------------------------------------------
  //Calculate the Means
  //-------------------------------------------------------------------------
  m_Means.resize(m_NumberOfPixels);
  m_Means.fill(0);

  InputImageConstIterator tempImageItA;

  for(unsigned int img_number = 0; img_number < m_NumberOfTrainingImages; img_number++)
    {
    tempImageItA = m_InputImageIteratorArray[img_number];

    for(unsigned int band_x = 0; band_x < m_NumberOfPixels; band_x++)
      {
      m_Means[band_x] += tempImageItA.Get();
      ++tempImageItA;
      }
    } // end: looping through the image
  //-------------------------------------------------------------------------

  m_Means /= m_NumberOfTrainingImages;

  //-------------------------------------------------------------------------
  // Calculate the inner product
  //-------------------------------------------------------------------------
  m_InnerProduct.resize( m_NumberOfTrainingImages, m_NumberOfTrainingImages );
  m_InnerProduct.fill( 0 );

  InputImageConstIterator tempImageItB; 

  //------------------------------------------------------------------------- 
  for(unsigned int band_x = 0; band_x < m_NumberOfTrainingImages; band_x++)
    {
    for(unsigned int band_y = 0; band_y <= band_x; band_y++ )
      {
      //Pointer to the vector (in original matrix)
      tempImageItA = m_InputImageIteratorArray[band_x];
        
      //Pointer to the vector in the transposed matrix
      tempImageItB = m_InputImageIteratorArray[band_y];

      for(unsigned int pix_number = 0; pix_number < m_NumberOfPixels; pix_number++)
        {
        m_InnerProduct[band_x][band_y] += 
          (tempImageItA.Get() - m_Means[pix_number]) * 
          (tempImageItB.Get() - m_Means[pix_number]);

        ++tempImageItA; 
        ++tempImageItB;
        }// end: looping through the image
      } // end: band_y loop
    } // end: band_x loop
 
  //--------------------------------------------------------------------- 
  // Fill the rest of the inner product matrix and make it symmetric
  //---------------------------------------------------------------------

  for(unsigned int band_x = 0; band_x < (m_NumberOfTrainingImages - 1); band_x++)
    {
    for(unsigned int band_y = band_x+1; band_y < m_NumberOfTrainingImages; band_y++)
      {  
      m_InnerProduct[band_x][band_y] = m_InnerProduct[band_y][band_x];    
      }// end band_y loop
    }// end band_x loop

  if( ( m_NumberOfTrainingImages - 1 ) != 0 )
    {
    m_InnerProduct /= ( m_NumberOfTrainingImages - 1 );
    }
  else
    {
    m_InnerProduct.fill(0);
    }  
      
}// end CalculateInnerProduct


/*-----------------------------------------------------------------
 *Estimage shape models using PCA.
 *-----------------------------------------------------------------
 */
template<class TInputImage, class TOutputImage>
void 
ImagePCAShapeModelEstimator<TInputImage, TOutputImage>
::EstimatePCAShapeModelPrameters()
{

  MatrixOfDoubleType identityMatrix( m_NumberOfTrainingImages, m_NumberOfTrainingImages );
  identityMatrix.set_identity();

  vnl_generalized_eigensystem eigenVectors_eigenValues(m_InnerProduct, identityMatrix);

  MatrixOfDoubleType eigenVectorsOfInnerProductMatrix = eigenVectors_eigenValues.V;

  //--------------------------------------------------------------------
  //Calculate the pricipal shape variations
  //
  //m_EigenVectors capture the principal shape variantions
  //m_EigenValues capture the relative weight of each variation
  //Multiply original image vetors with the eigenVectorsOfInnerProductMatrix
  //to derive the pricipal shapes.
  //--------------------------------------------------------------------

  m_EigenVectors.resize(m_NumberOfPixels, m_NumberOfTrainingImages);    
  m_EigenVectors.fill(0);  

  double pix_value;
  InputImageConstIterator tempImageItA;

  for(unsigned int img_number =0; img_number < m_NumberOfTrainingImages; img_number++ )
    {
    tempImageItA = m_InputImageIteratorArray[img_number];
    for(unsigned int pix_number =0; pix_number < m_NumberOfPixels; pix_number++ )
      {
      pix_value = tempImageItA.Get();
      for( unsigned int vec_number = 0; vec_number < m_NumberOfTrainingImages; vec_number++ )
        {
        m_EigenVectors[pix_number][vec_number] +=
          (pix_value * eigenVectorsOfInnerProductMatrix[img_number][vec_number]);
        }
      ++tempImageItA;
      }
    }

  m_EigenVectors.normalize_columns();

  m_EigenValues.resize(m_NumberOfTrainingImages);    

  //Extract the diagonal elements into the Eigen value vector
  m_EigenValues = (eigenVectors_eigenValues.D).diagonal();
  
  //--------------------------------------------------------------------
  //Normalize the eigen values 
  //--------------------------------------------------------------------

  m_EigenVectorNormalizedEnergy = m_EigenValues;
  m_EigenVectorNormalizedEnergy.normalize();


}// end EstimatePCAShapeModelPrameters

//-----------------------------------------------------------------


} // namespace itk

#endif
