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
#ifndef itkMRFImageFilter_hxx
#define itkMRFImageFilter_hxx
#include "itkMRFImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TClassifiedImage >
MRFImageFilter< TInputImage, TClassifiedImage >
::MRFImageFilter(void):
  m_NumberOfClasses(0),
  m_MaximumNumberOfIterations(50),
  m_ErrorCounter(0),
  m_NeighborhoodSize(27),
  m_TotalNumberOfValidPixelsInOutputImage(1),
  m_TotalNumberOfPixelsInInputImage(1),
  m_ErrorTolerance(0.2),
  m_SmoothingFactor(1),
  m_ClassProbability(ITK_NULLPTR),
  m_NumberOfIterations(0),
  m_StopCondition(MaximumNumberOfIterations),
  m_ClassifierPtr(ITK_NULLPTR)
{
  if ( (int)InputImageDimension != (int)ClassifiedImageDimension )
    {
    std::ostringstream msg;
    msg << "Input image dimension: " << InputImageDimension << " != output image dimension: "
        << ClassifiedImageDimension;
    throw ExceptionObject(__FILE__, __LINE__, msg.str().c_str(), ITK_LOCATION);
    }
  m_InputImageNeighborhoodRadius.Fill(0);
  m_MRFNeighborhoodWeight.resize(0);
  m_NeighborInfluence.resize(0);
  m_DummyVector.resize(0);
  this->SetMRFNeighborhoodWeight(m_DummyVector);
  this->SetDefaultMRFNeighborhoodWeight();
}

template< typename TInputImage, typename TClassifiedImage >
MRFImageFilter< TInputImage, TClassifiedImage >
::~MRFImageFilter(void)
{}

template< typename TInputImage, typename TClassifiedImage >
void
MRFImageFilter< TInputImage, TClassifiedImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  unsigned int i;

  os << indent << " MRF Image filter object " << std::endl;

  os << indent << " Number of classes: " << m_NumberOfClasses << std::endl;

  os << indent << " Maximum number of iterations: "
     << m_MaximumNumberOfIterations << std::endl;

  os << indent << " Error tolerance for convergence: "
     << m_ErrorTolerance << std::endl;

  os << indent << " Size of the MRF neighborhood radius:"
     << m_InputImageNeighborhoodRadius << std::endl;

  os << indent << " Number of elements in MRF neighborhood :"
     << static_cast< SizeValueType >( m_MRFNeighborhoodWeight.size() ) << std::endl;

  os << indent << " Neighborhood weight : [";
  const unsigned int neighborhoodWeightSize =
    static_cast< unsigned int >( m_MRFNeighborhoodWeight.size() );
  for ( i = 0; i + 1 < neighborhoodWeightSize; i++ )
    {
    os << m_MRFNeighborhoodWeight[i] << ", ";
    }
  os << m_MRFNeighborhoodWeight[i] << "]" << std::endl;

  os << indent << " Smoothing factor for the MRF neighborhood:"
     << m_SmoothingFactor << std::endl;

  os << indent << "StopCondition: "
     << m_StopCondition << std::endl;

  os << indent << " Number of iterations: "
     << m_NumberOfIterations << std::endl;
} // end PrintSelf

/*
 * GenerateInputRequestedRegion method.
 */
template< typename TInputImage, typename TClassifiedImage >
void
MRFImageFilter< TInputImage, TClassifiedImage >
::GenerateInputRequestedRegion()
{
  // this filter requires that all of the input images
  // are the size of the output requested region
  InputImagePointer inputPtr =
    const_cast< InputImageType * >( this->GetInput() );
  OutputImagePointer outputPtr = this->GetOutput();

  if ( inputPtr && outputPtr )
    {
    inputPtr->SetRequestedRegion( outputPtr->GetRequestedRegion() );
    }
}

/**
 * EnlargeOutputRequestedRegion method.
 */
template< typename TInputImage, typename TClassifiedImage >
void
MRFImageFilter< TInputImage, TClassifiedImage >
::EnlargeOutputRequestedRegion(
  DataObject *output)
{
  // this filter requires that all of the output images
  // are in the buffer
  TClassifiedImage *imgData;

  imgData = dynamic_cast< TClassifiedImage * >( output );
  imgData->SetRequestedRegionToLargestPossibleRegion();
}

/**
 * GenerateOutputInformation method.
 */
template< typename TInputImage, typename TClassifiedImage >
void
MRFImageFilter< TInputImage, TClassifiedImage >
::GenerateOutputInformation()
{
  typename TInputImage::ConstPointer input = this->GetInput();
  typename TClassifiedImage::Pointer output = this->GetOutput();
  output->SetLargestPossibleRegion( input->GetLargestPossibleRegion() );
}

template< typename TInputImage, typename TClassifiedImage >
void
MRFImageFilter< TInputImage, TClassifiedImage >
::GenerateData()
{
  //First run the Gaussian classifier calculator and
  //generate the Gaussian model for the different classes
  //and then generate the initial labelled dataset.

  InputImageConstPointer inputImage = this->GetInput();

  //Give the input image and training image set to the
  // classifier
  m_ClassifierPtr->SetInputImage(inputImage);

  //Run the gaussian classifier algorithm
  m_ClassifierPtr->Update();

  //Allocate memory for the labelled images
  this->Allocate();

  this->ApplyMRFImageFilter();
  //Set the output labelled and allocate the memory
  LabelledImagePointer outputPtr = this->GetOutput();

  //Allocate the output buffer memory
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  //--------------------------------------------------------------------
  //Copy labelling result to the output buffer
  //--------------------------------------------------------------------
  // Set the iterators to the processed image
  //--------------------------------------------------------------------
  LabelledImageRegionIterator
  labelledImageIt( m_ClassifierPtr->GetClassifiedImage(),
                   outputPtr->GetRequestedRegion() );

  //--------------------------------------------------------------------
  // Set the iterators to the output image buffer
  //--------------------------------------------------------------------
  LabelledImageRegionIterator
  outImageIt( outputPtr, outputPtr->GetRequestedRegion() );

  //--------------------------------------------------------------------

  while ( !outImageIt.IsAtEnd() )
    {
    LabelledImagePixelType labelvalue =
      (LabelledImagePixelType)labelledImageIt.Get();

    outImageIt.Set(labelvalue);
    ++labelledImageIt;
    ++outImageIt;
    } // end while
}     // end GenerateData

template< typename TInputImage, typename TClassifiedImage >
void
MRFImageFilter< TInputImage, TClassifiedImage >
::SetClassifier(typename ClassifierType::Pointer ptrToClassifier)
{
  if ( ( ptrToClassifier.IsNull() ) || ( m_NumberOfClasses <= 0 ) )
    {
    throw ExceptionObject(__FILE__, __LINE__, "NumberOfClasses is <= 0", ITK_LOCATION);
    }
  m_ClassifierPtr = ptrToClassifier;
  m_ClassifierPtr->SetNumberOfClasses(m_NumberOfClasses);
} //end SetPtrToClassifier

//-------------------------------------------------------
//Set the neighborhood radius
//-------------------------------------------------------

template< typename TInputImage, typename TClassifiedImage >
void
MRFImageFilter< TInputImage, TClassifiedImage >
::SetNeighborhoodRadius(const SizeValueType radiusValue)
{
  //Set up the neighborhood
  NeighborhoodRadiusType radius;

  for ( unsigned int i = 0; i < InputImageDimension; ++i )
    {
    radius[i] = radiusValue;
    }
  this->SetNeighborhoodRadius(radius);
} // end SetNeighborhoodRadius

template< typename TInputImage, typename TClassifiedImage >
void
MRFImageFilter< TInputImage, TClassifiedImage >
::SetNeighborhoodRadius(const SizeValueType *radiusArray)
{
  NeighborhoodRadiusType radius;

  for ( unsigned int i = 0; i < InputImageDimension; ++i )
    {
    radius[i] = radiusArray[i];
    }
  //Set up the neighborhood
  this->SetNeighborhoodRadius(radius);
} // end SetNeighborhoodRadius

//Set the neighborhood radius
template< typename TInputImage, typename TClassifiedImage >
void
MRFImageFilter< TInputImage, TClassifiedImage >
::SetNeighborhoodRadius(const NeighborhoodRadiusType & radius)
{
  //Set up the neighbor hood
  for ( unsigned int i = 0; i < InputImageDimension; ++i )
    {
    m_InputImageNeighborhoodRadius[i] =
      radius[i];
    m_LabelledImageNeighborhoodRadius[i] =
      radius[i];
    m_LabelStatusImageNeighborhoodRadius[i] =
      radius[i];
    }
} // end SetNeighborhoodRadius

//-------------------------------------------------------

//-------------------------------------------------------
//Set the neighborhood weights
//-------------------------------------------------------

template< typename TInputImage, typename TClassifiedImage >
void
MRFImageFilter< TInputImage, TClassifiedImage >
::SetDefaultMRFNeighborhoodWeight()
{
  // Set the beta matrix of a 3x3x3 kernel.
  // The index starts from 0 going along the three dimensions
  // in the order of [coloumn], [row], [depth].

  //Allocate memory for the weights of the 3D MRF algorithm
  // and corresponding memory offsets.

  //-----------------------------------------------------
  //Determine the default neighborhood size
  //-----------------------------------------------------
  m_NeighborhoodSize = 1;
  int neighborhoodRadius = 1; //Default assumes a radius of 1
  for ( unsigned int i = 0; i < InputImageDimension; i++ )
    {
    m_NeighborhoodSize *= ( 2 * neighborhoodRadius + 1 );
    }
  if ( ( InputImageDimension == 3 ) )
    {
    //Assumes a default 3x3x3 window size
    m_MRFNeighborhoodWeight.resize(m_NeighborhoodSize);

    for ( int i = 0; i < 9; i++ )
      {
      m_MRFNeighborhoodWeight[i] = 1.3 * m_SmoothingFactor;
      }

    for ( int i = 9; i < 18; i++ )
      {
      m_MRFNeighborhoodWeight[i] = 1.7 * m_SmoothingFactor;
      }

    for ( int i = 18; i < 27; i++ )
      {
      m_MRFNeighborhoodWeight[i] = 1.3 * m_SmoothingFactor;
      }

    // Change the center weights
    m_MRFNeighborhoodWeight[4]  = 1.5 * m_SmoothingFactor;
    m_MRFNeighborhoodWeight[13] = 0.0;
    m_MRFNeighborhoodWeight[22] = 1.5 * m_SmoothingFactor;
    }
  if ( ( InputImageDimension == 2 ) )
    {
    //Assumes a default 3x3x3 window size
    m_MRFNeighborhoodWeight.resize(m_NeighborhoodSize);

    for ( int i = 0; i < m_NeighborhoodSize; i++ )
      {
      m_MRFNeighborhoodWeight[i] = 1.7 * m_SmoothingFactor;
      }

    // Change the center weights
    m_MRFNeighborhoodWeight[4]  = 0;
    }
  else
    {
    if ( ( InputImageDimension > 3 ) )
      {
      for ( int i = 0; i < m_NeighborhoodSize; i++ )
        {
        m_MRFNeighborhoodWeight[i] = 1;
        }
      }
    }
} // SetMRFNeighborhoodWeight

template< typename TInputImage, typename TClassifiedImage >
void
MRFImageFilter< TInputImage, TClassifiedImage >
::SetMRFNeighborhoodWeight(std::vector< double > betaMatrix)
{
  if ( betaMatrix.size() == 0 )
    {
    //Call a the function to set the default neighborhood
    this->SetDefaultMRFNeighborhoodWeight();
    }
  else
    {
    m_NeighborhoodSize = 1;
    for ( unsigned int i = 0; i < InputImageDimension; i++ )
      {
      m_NeighborhoodSize *= ( 2 * m_InputImageNeighborhoodRadius[i] + 1 );
      }

    if ( m_NeighborhoodSize != static_cast< int >( betaMatrix.size() ) )
      {
      throw ExceptionObject(__FILE__, __LINE__, "NeighborhoodSize != betaMatrix.szie()", ITK_LOCATION);
      }

    //Allocate memory for the weights of the 3D MRF algorithm
    // and corresponding memory offsets.
    m_MRFNeighborhoodWeight.resize(m_NeighborhoodSize);

    for ( unsigned int i = 0; i < betaMatrix.size(); i++ )
      {
      m_MRFNeighborhoodWeight[i] = ( betaMatrix[i] * m_SmoothingFactor );
      }
    }
} // end SetDefaultMRFNeighborhoodWeight

//-------------------------------------------------------
//-------------------------------------------------------
//Allocate algorithm specific resources
//-------------------------------------------------------
template< typename TInputImage, typename TClassifiedImage >
void
MRFImageFilter< TInputImage, TClassifiedImage >
::Allocate()
{
  if ( m_NumberOfClasses <= 0 )
    {
    throw ExceptionObject(__FILE__, __LINE__, "NumberOfClasses <= 0.", ITK_LOCATION);
    }

  InputImageSizeType inputImageSize = this->GetInput()->GetBufferedRegion().GetSize();

  //---------------------------------------------------------------------
  //Get the number of valid pixels in the output MRF image
  //---------------------------------------------------------------------
  int tmp;
  for ( unsigned int i = 0; i < InputImageDimension; i++ )
    {
    tmp = static_cast< int >( inputImageSize[i] );

    m_TotalNumberOfPixelsInInputImage *= tmp;

    m_TotalNumberOfValidPixelsInOutputImage *=
      ( tmp - 2 * m_InputImageNeighborhoodRadius[i] );
    }

  //Allocate the label image status
  LabelStatusIndexType index;
  index.Fill(0);

  LabelStatusRegionType region;
  region.SetSize(inputImageSize);
  region.SetIndex(index);

  m_LabelStatusImage = LabelStatusImageType::New();
  m_LabelStatusImage->SetLargestPossibleRegion(region);
  m_LabelStatusImage->SetBufferedRegion(region);
  m_LabelStatusImage->Allocate();

  LabelStatusImageIterator rIter( m_LabelStatusImage,
                                  m_LabelStatusImage->GetBufferedRegion() );

  //Initialize the label status image to 1
  while ( !rIter.IsAtEnd() )
    {
    rIter.Set(1);
    ++rIter;
    }
} // Allocate

//-------------------------------------------------------
//-------------------------------------------------------
//Apply the MRF image filter
//-------------------------------------------------------

template< typename TInputImage, typename TClassifiedImage >
void
MRFImageFilter< TInputImage, TClassifiedImage >
::ApplyMRFImageFilter()
{
  InputImageSizeType inputImageSize =
    this->GetInput()->GetBufferedRegion().GetSize();

  int totalNumberOfPixelsInInputImage = 1;

  for ( unsigned int i = 0; i < InputImageDimension; i++ )
    {
    totalNumberOfPixelsInInputImage *= static_cast< int >( inputImageSize[i] );
    }

  int maxNumPixelError = Math::Round< int >(m_ErrorTolerance
                                            * m_TotalNumberOfValidPixelsInOutputImage);

  m_NumberOfIterations = 0;
  do
    {
    itkDebugMacro(<< "Iteration No." << m_NumberOfIterations);

    MinimizeFunctional();
    m_NumberOfIterations += 1;
    m_ErrorCounter = m_TotalNumberOfValidPixelsInOutputImage
                     - totalNumberOfPixelsInInputImage;

    LabelStatusImageIterator rIter( m_LabelStatusImage,
                                    m_LabelStatusImage->GetBufferedRegion() );

    //Initialize the label status image to 1
    while ( !rIter.IsAtEnd() )
      {
      if ( rIter.Get() == 1 ) { m_ErrorCounter += 1; }
      ++rIter;
      }
    }
  while ( ( m_NumberOfIterations < m_MaximumNumberOfIterations )
          && ( m_ErrorCounter > maxNumPixelError ) );

  //Determine stop condition
  if ( m_NumberOfIterations >= m_MaximumNumberOfIterations )
    {
    m_StopCondition = MaximumNumberOfIterations;
    }
  else if ( m_ErrorCounter <= maxNumPixelError )
    {
    m_StopCondition = ErrorTolerance;
    }
} // ApplyMRFImageFilter

//-------------------------------------------------------
//-------------------------------------------------------
//Minimize the functional
//-------------------------------------------------------

template< typename TInputImage, typename TClassifiedImage >
void
MRFImageFilter< TInputImage, TClassifiedImage >
::MinimizeFunctional()
{
  //This implementation uses the ICM algorithm
  ApplyICMLabeller();
}

//-------------------------------------------------------
//-------------------------------------------------------
//Core of the ICM algorithm
//-------------------------------------------------------
template< typename TInputImage, typename TClassifiedImage >
void
MRFImageFilter< TInputImage, TClassifiedImage >
::ApplyICMLabeller()
{
  //---------------------------------------------------------------------
  // Loop through the data set and classify the data
  //---------------------------------------------------------------------
  m_NeighborInfluence.resize(m_NumberOfClasses);

  //Varible to store the input pixel vector value
  //InputImagePixelType inputPixel;

  m_MahalanobisDistance.resize(m_NumberOfClasses);

  //---------------------------------------------------------------------
  // Set up the neighborhood iterators and the valid neighborhoods
  // for iteration
  //---------------------------------------------------------------------

  //Set up the nighborhood iterators

  //Define the face list for the input/labelled image
  InputImageFacesCalculator       inputImageFacesCalculator;
  LabelledImageFacesCalculator    labelledImageFacesCalculator;
  LabelStatusImageFacesCalculator labelStatusImageFacesCalculator;

  InputImageFaceListType       inputImageFaceList;
  LabelledImageFaceListType    labelledImageFaceList;
  LabelStatusImageFaceListType labelStatusImageFaceList;

  //Compute the faces for the neighborhoods in the input/labelled image
  InputImageConstPointer inputImage = this->GetInput();
  inputImageFaceList =
    inputImageFacesCalculator(inputImage,
                              inputImage->GetBufferedRegion(),
                              m_InputImageNeighborhoodRadius);

  LabelledImagePointer labelledImage = m_ClassifierPtr->GetClassifiedImage();
  labelledImageFaceList =
    labelledImageFacesCalculator(labelledImage,
                                 labelledImage->GetBufferedRegion(),
                                 m_LabelledImageNeighborhoodRadius);

  labelStatusImageFaceList =
    labelStatusImageFacesCalculator(m_LabelStatusImage,
                                    m_LabelStatusImage->GetBufferedRegion(),
                                    m_LabelStatusImageNeighborhoodRadius);
  //Set up a face list iterator
  InputImageFaceListIterator inputImageFaceListIter =
    inputImageFaceList.begin();

  LabelledImageFaceListIterator labelledImageFaceListIter =
    labelledImageFaceList.begin();

  LabelStatusImageFaceListIterator labelStatusImageFaceListIter =
    labelStatusImageFaceList.begin();

  //Walk through the entire data set (not visiting the boundaries )
  InputImageNeighborhoodIterator
  nInputImageNeighborhoodIter(m_InputImageNeighborhoodRadius,
                              inputImage,
                              *inputImageFaceListIter);

  LabelledImageNeighborhoodIterator
  nLabelledImageNeighborhoodIter(m_LabelledImageNeighborhoodRadius,
                                 labelledImage,
                                 *labelledImageFaceListIter);

  LabelStatusImageNeighborhoodIterator
  nLabelStatusImageNeighborhoodIter(m_LabelStatusImageNeighborhoodRadius,
                                    m_LabelStatusImage,
                                    *labelStatusImageFaceListIter);

  //---------------------------------------------------------------------
  while ( !nInputImageNeighborhoodIter.IsAtEnd() )
    {
    //Process each neighborhood
    this->DoNeighborhoodOperation(nInputImageNeighborhoodIter,
                                  nLabelledImageNeighborhoodIter,
                                  nLabelStatusImageNeighborhoodIter);

    ++nInputImageNeighborhoodIter;
    ++nLabelledImageNeighborhoodIter;
    ++nLabelStatusImageNeighborhoodIter;
    }
} //ApplyICMlabeller

//-------------------------------------------------------
//-------------------------------------------------------
//Function that performs the MRF operation with each neighborhood
//-------------------------------------------------------

template< typename TInputImage, typename TClassifiedImage >
void
MRFImageFilter< TInputImage, TClassifiedImage >
::DoNeighborhoodOperation(const InputImageNeighborhoodIterator & imageIter,
                          LabelledImageNeighborhoodIterator & labelledIter,
                          LabelStatusImageNeighborhoodIterator & labelStatusIter)
{
  unsigned int index;

  //Read the pixel of interest and get its corresponding membership value
  InputImagePixelType *inputPixelVec = imageIter.GetCenterValue();

  const std::vector< double > & pixelMembershipValue =
    m_ClassifierPtr->GetPixelMembershipValue(*inputPixelVec);

  //Reinitialize the neighborhood influence at the beginning of the
  //neighborhood operation
  for ( index = 0; index < m_NeighborInfluence.size(); index++ )
    {
    m_NeighborInfluence[index] = 0;
    }

  LabelledImagePixelType labelledPixel;

  //Begin neighborhood processing. Calculate the prior for each label
  for ( int i = 0; i < m_NeighborhoodSize; ++i )
    {
    labelledPixel = labelledIter.GetPixel(i);
    index = (unsigned int)labelledPixel;
    m_NeighborInfluence[index] += m_MRFNeighborhoodWeight[i];
    } //End neighborhood processing

  //Add the prior probability to the pixel probability
  for ( index = 0; index < m_NumberOfClasses; index++ )
    {
    m_MahalanobisDistance[index] = m_NeighborInfluence[index]
                                   - pixelMembershipValue[index];
    }

  //Determine the maximum possible distance
  double maximumDistance = -1e+20;
  int    pixLabel = -1;
  double tmpPixDistance;
  for ( index = 0; index < m_NumberOfClasses; index++ )
    {
    tmpPixDistance = m_MahalanobisDistance[index];
    if ( tmpPixDistance > maximumDistance )
      {
      maximumDistance = tmpPixDistance;
      pixLabel = index;
      } // if
    }   // for

  //Read the current pixel label
  LabelledImagePixelType *previousLabel = labelledIter.GetCenterValue();

  //Check if the labelled pixel value in the previous iteration has changed
  //If the value has changed then update the m_LabelStatus set;

  if ( pixLabel != (int)( *previousLabel ) )
    {
    labelledIter.SetCenterPixel(pixLabel);
    for ( int i = 0; i < m_NeighborhoodSize; ++i )
      {
      labelStatusIter.SetPixel(i, 1);
      } //End neighborhood processing
    }   //end if
  else
    {
    labelStatusIter.SetCenterPixel(0);
    }
} // end DoNeighborhoodOperation
} // namespace itk

#endif
