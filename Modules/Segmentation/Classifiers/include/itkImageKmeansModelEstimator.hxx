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
#ifndef itkImageKmeansModelEstimator_hxx
#define itkImageKmeansModelEstimator_hxx

#include "itkImageKmeansModelEstimator.h"

namespace itk
{
template< typename TInputImage,
          typename TMembershipFunction >
ImageKmeansModelEstimator< TInputImage, TMembershipFunction >
::ImageKmeansModelEstimator(void)
{
  m_ValidInCodebook  = false;
  m_DoubleMaximum    = NumericTraits< double >::max();
  m_Threshold        = 0.01;
  m_OffsetAdd        = 0.01;
  m_OffsetMultiply   = 0.01;
  m_MaxSplitAttempts = 10;
  m_OutputDistortion = 0.0;
  m_OutputNumberOfEmptyCells = 0;
  m_VectorDimension   = 1;
  m_NumberOfCodewords = 1;
  m_CurrentNumberOfCodewords = 1;
}

template< typename TInputImage,
          typename TMembershipFunction >
ImageKmeansModelEstimator< TInputImage, TMembershipFunction >
::~ImageKmeansModelEstimator(void)
{}

/**
 * PrintSelf
 */
template< typename TInputImage,
          typename TMembershipFunction >
void
ImageKmeansModelEstimator< TInputImage, TMembershipFunction >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "                   " << std::endl;
  os << indent << "Kmeans Models " << std::endl;
  os << indent << "Results printed in the superclass " << std::endl;
  os << indent << "                   " << std::endl;

  Superclass::PrintSelf(os, indent);

  os << indent << "Unsupervised Classifier / Clusterer" << std::endl;
  os << indent << "Offset value for addition:" << m_OffsetAdd << std::endl;
  os << indent << "Offset value for multiplication:" << m_OffsetMultiply << std::endl;
  os << indent << "Maximum number of attempts to split a cluster: " << m_MaxSplitAttempts << std::endl;
  os << indent << "Codebook : " << m_Codebook << std::endl;
  os << indent << "Threshold value :" << m_Threshold << std::endl;
} // end PrintSelf

template< typename TInputImage,
          typename TMembershipFunction >
void
ImageKmeansModelEstimator< TInputImage, TMembershipFunction >
::PrintKmeansAlgorithmResults()
{
  itkDebugMacro(<< "                                    ");
  itkDebugMacro(<< "Results of the clustering algorithms");
  itkDebugMacro(<< "====================================");

  itkDebugMacro(<< "                                    ");
  itkDebugMacro(<< "Means of the clustered vector       ");
  itkDebugMacro(<< "++++++++++++++++++++++++++++++++++++");

  itkDebugMacro(<< m_Centroid);

  itkDebugMacro(<< "                                    ");
  itkDebugMacro(<< "Distortion measures                 ");
  itkDebugMacro(<< "+++++++++++++++++++++++++++++++++++ ");

  itkDebugMacro(<< m_CodewordDistortion);

  itkDebugMacro(<< "                                    ");
  itkDebugMacro(<< "Histogram of the vector             ");
  itkDebugMacro(<< "+++++++++++++++++++++++++++++++++++ ");

  itkDebugMacro(<< m_CodewordHistogram);
} // End PrintKmeansAlgorithmResults

/**
 * Generate data (start the model building process)
 */
template< typename TInputImage,
          typename TMembershipFunction >
void
ImageKmeansModelEstimator< TInputImage, TMembershipFunction >
::GenerateData()
{
  this->EstimateModels();
} // end Generate data

// Set the input codebook and allocate memory
// for the output codebook and other scratch memory
template< typename TInputImage,
          typename TMembershipFunction >
void
ImageKmeansModelEstimator< TInputImage, TMembershipFunction >
::SetCodebook(CodebookMatrixOfDoubleType inCodebook)
{
  m_Codebook        = inCodebook;

  //Check if the input codebook is a valid
  if ( InputImagePixelType::GetVectorDimension() == m_Codebook.cols() )
    {
    m_ValidInCodebook = true;
    this->Allocate();
    }
} //End SetInCodebook

// Allocate scratch memory

template< typename TInputImage,
          typename TMembershipFunction >
void
ImageKmeansModelEstimator< TInputImage, TMembershipFunction >
::Allocate()
{
  SizeValueType initCodebookSize, finalCodebookSize;

  m_VectorDimension = InputImagePixelType::GetVectorDimension();

  if ( m_ValidInCodebook )
    {
    m_NumberOfCodewords = m_Codebook.rows();
    m_VectorDimension     = m_Codebook.cols();
    // Set the initial and final codebook size
    finalCodebookSize = m_NumberOfCodewords;
    } // end(if valid codebook clause)
  else
    {
    m_ValidInCodebook = true;

    //Check the validity of the n
    if ( this->GetNumberOfModels() <= 0 )
      {
      itkExceptionMacro(<< "Number of models is less than 0.");
      }

    m_NumberOfCodewords      = this->GetNumberOfModels();
    m_VectorDimension        = InputImagePixelType::GetVectorDimension();

    // Set the initial and final codebook size

    initCodebookSize = (SizeValueType)1;
    finalCodebookSize = (SizeValueType)m_NumberOfCodewords;

    m_Codebook.set_size(initCodebookSize, m_VectorDimension);

    // initialize m_Codebook to 0 (it now has only one row)
    m_Codebook.fill(0);
    } // end (else not valid codebook clause)

  //----------------------------------------------------------
  //Allocate scratch memory for the centroid, codebook histogram
  //and the codebook distortion

  m_Centroid.set_size(finalCodebookSize, m_VectorDimension);
  m_Centroid.fill(0);

  m_CodewordHistogram.set_size(m_NumberOfCodewords, 1);
  m_CodewordHistogram.fill(0);

  m_CodewordDistortion.set_size(m_NumberOfCodewords, 1);
  m_CodewordDistortion.fill(0);
} // end Allocate function

//-----------------------------------------------------------------
//Reallocate various memories and then make a copy of the old data
//-----------------------------------------------------------------

template< typename TInputImage,
          typename TMembershipFunction >
void
ImageKmeansModelEstimator< TInputImage, TMembershipFunction >
::Reallocate(int oldSize, int newSize)
{
  //Set up a temporary codebook
  CodebookMatrixOfDoubleType tmpCodebook(oldSize, m_VectorDimension);

  //Save the contents of m_Codebook in the tmpCodebook
  tmpCodebook = m_Codebook;
  m_Codebook.set_size(newSize, m_VectorDimension);

  // Copy back the saved data into the codebook

  if ( oldSize < newSize )
    {
    for ( int r = 0; r < oldSize; r++ )
      {
      for ( unsigned int c = 0; c < m_VectorDimension; c++ )
        {
        m_Codebook[r][c] = tmpCodebook[r][c];
        }
      }

    for ( int r = oldSize; r < newSize; r++ )
      {
      for ( unsigned int c = 0; c < m_VectorDimension; c++ )
        {
        m_Codebook[r][c] = 0;
        }
      }
    } // if oldsize is smaller than the new size
  else
    {
    for ( int r = 0; r < newSize; r++ )
      {
      for ( unsigned int c = 0; c < m_VectorDimension; c++ )
        {
        m_Codebook[r][c] = tmpCodebook[r][c];
        }
      }
    } // else oldsize is greater than the new size
}     // end Reallocate

//-----------------------------------------------------------------
// Takes a set of training images and returns the means
// and variance of the various classes defined in the
// training set.
//-----------------------------------------------------------------
template< typename TInputImage,
          typename TMembershipFunction >
void
ImageKmeansModelEstimator< TInputImage, TMembershipFunction >
::EstimateModels()
{
  this->EstimateKmeansModelParameters();

  //-------------------------------------------------------------------
  // Set up the membership calculators
  //-------------------------------------------------------------------

  unsigned int numberOfModels = this->GetNumberOfModels();

  //-------------------------------------------------------------------
  // Call local function to estimate mean variances of the various
  // class labels in the training set
  // The statistics class functions have not been used since all the
  // class statistics are calculated simultaneously here.
  //-------------------------------------------------------------------

  //-------------------------------------------------------------------
  // Populate the membership functions for all the classes
  //-------------------------------------------------------------------
  MembershipFunctionPointer membershipFunction;

  if ( this->GetNumberOfMembershipFunctions() > 0 )
    {
    this->DeleteAllMembershipFunctions();
    }

  for ( unsigned int classIndex = 0; classIndex < numberOfModels; classIndex++ )
    {
    membershipFunction = TMembershipFunction::New();
    typename TMembershipFunction::CentroidType centroid;
    centroid = m_Centroid.get_row(classIndex);
    membershipFunction->SetCentroid(centroid);
    this->AddMembershipFunction(membershipFunction);
    }
} // end EstimateModels

//-----------------------------------------------------------------
//Estimate K-means models (private function) for the core function
//-----------------------------------------------------------------
template< typename TInputImage,
          typename TMembershipFunction >
void
ImageKmeansModelEstimator< TInputImage, TMembershipFunction >
::EstimateKmeansModelParameters()
{
  //If a codebook is provided by the user then call the
  //Kmenas algorithm directly that is based on the
  //Generalized Lloyd algorithm (GLA) if a valid codebook
  //is provided or m_NumberOfModels is set to 0, else
  //Linde-Buzo-Gray algorithm is used for clustering
  if ( m_ValidInCodebook )
    {
    WithCodebookUseGLA();
    }
  else
    {
    //Assign memory for the initial codebook
    //since no input codebook is provided for this
    //function
    Allocate();
    m_CurrentNumberOfCodewords = m_Codebook.rows();
    WithoutCodebookUseLBG();
    }

  m_ValidInCodebook = false;
} // end EstimateKmeansModelParameters

//-----------------------------------------------------------------
template< typename TInputImage,
          typename TMembershipFunction >
int
ImageKmeansModelEstimator< TInputImage, TMembershipFunction >
::WithCodebookUseGLA()
{
  // Do the Lloyd iteration.  Use the nearest neighbor condition to
  // find the cells.  Then find the centroid of each cell.

  // First pass requires very large distortion

  double olddistortion = m_DoubleMaximum;
  double distortion, tempdistortion;
  int    pass = 0; // no empty cells have been found yet
  int    emptycells;
  int    bestcodeword;

  m_CurrentNumberOfCodewords = m_Codebook.rows();

  do
    {
    // encode all of the input vectors using the given codebook
    NearestNeighborSearchBasic(&distortion);

    // check for lack of convergence
    if ( olddistortion < distortion )
      {
      itkExceptionMacro(<< "Distortion is increasing, not decreasing");
      }

    // find number of empty cells
    emptycells = 0;
    for ( unsigned int i = 0; i < m_CurrentNumberOfCodewords; i++ )
      {
      if ( m_CodewordHistogram[i][0] == 0 )
        {
        emptycells += 1;
        m_CodewordDistortion[i][0] = 0.0;
        }
      }

    // if distortion = 0.0, or
    // if change in distortion < threshold AND there aren't any empty cells,
    // and exit
    if ( ( distortion == 0.0 ) || ( ( emptycells == 0 )
                                    && ( olddistortion - distortion ) / distortion < m_Threshold ) )
      {
      m_OutputNumberOfEmptyCells   = emptycells;
      m_OutputDistortion = distortion;
      return GLA_CONVERGED;
      }

    // no empty cells, find new centroids and reinitialize for next pass
    if ( emptycells == 0 )
      {
      for ( unsigned int i = 0; i < m_CurrentNumberOfCodewords; i++ )
        {
        for ( unsigned int j = 0; j < m_VectorDimension; j++ )
          {
          m_Codebook[i][j] = m_Centroid[i][j];
          }
        }

      olddistortion = distortion;
      pass = 0;
      } // end if

    // there are empty cells, split the highest distortion codewords.
    // try again
    else
      {
      // If there have been too many attempts to fill cells, stop iterations
      if ( pass == m_MaxSplitAttempts )
        {
        itkWarningMacro(<< "Unable to fill all empty cells");
        m_OutputNumberOfEmptyCells = emptycells;
        m_OutputDistortion = distortion;
        return GLA_CONVERGED;
        }

      // try getting new codewords, send a warning to user
      itkDebugMacro(<< "Attempting to fill empty cells in the codebook");

      // consolidate the highest distortion codewords into the beginning
      // of the array.  Take care to protect zero distortion codewords
      // which have a positive m_CodewordHistogram.  note: there must be a
      // faster sort algorithm, but this event should be very unlikely
      for ( unsigned int n = 0; n < m_CurrentNumberOfCodewords - emptycells; n++ )
        {
        tempdistortion = 0.0;
        bestcodeword = 0;
        for ( unsigned int i = 0; i < m_NumberOfCodewords; i++ )
          {
          if ( ( m_CodewordDistortion[i][0] >= tempdistortion )
               && ( m_CodewordHistogram[i][0] > 0 ) )
            {
            tempdistortion = m_CodewordDistortion[i][0];
            bestcodeword = i;
            }
          }

        // put highest distortion centroid into nth codebook row,
        // and erase the set of hightest centroid stats to 0 so
        // it will not be used again.

        // find centroid, reinitialize

        for ( unsigned int j = 0; j < m_VectorDimension; j++ )
          {
          m_Codebook[n][j] = m_Centroid[bestcodeword][j];
          }

        m_CodewordHistogram[bestcodeword][0] = 0;
        m_CodewordDistortion[bestcodeword][0] = 0.0;
        }

      // split the required number of codewords
      SplitCodewords(m_CurrentNumberOfCodewords - emptycells,
                     emptycells, pass);

      olddistortion = distortion;
      pass++;
      } // end else
    }
  while ( pass <= m_MaxSplitAttempts );
  itkExceptionMacro(<< "Lack of convergence");
} // end WithCodebookUseGLA

//-----------------------------------------------------------------
template< typename TInputImage,
          typename TMembershipFunction >
void
ImageKmeansModelEstimator< TInputImage, TMembershipFunction >
::NearestNeighborSearchBasic(double *distortion)
{
  //itkDebugMacro(<<"Start nearest_neighbor_search_basic()");

  double  bestdistortion, tempdistortion, diff;
  int     bestcodeword;
  double *tempVec = (double *)new double[m_VectorDimension];

// unused: double *centroidVecTemp = ( double * ) new double[m_VectorDimension];

// initialize codeword histogram and distortion
  for ( unsigned int i = 0; i < m_CurrentNumberOfCodewords; i++ )
    {
    m_CodewordHistogram[i][0] = 0;
    m_CodewordDistortion[i][0] = 0.0;
    }

  // initialize centroid if it exists
  m_Centroid.fill(0);

  // perform encoding using partial distortion method
  *distortion = 0.0;

  //-----------------------------------------------------------------
  // Declare the iterators for the image and the codebook
  //-----------------------------------------------------------------
  InputImageConstPointer  inputImage = this->GetInputImage();
  InputImageConstIterator inputImageIt( inputImage, inputImage->GetBufferedRegion() );
  inputImageIt.GoToBegin();

  //-----------------------------------------------------------------
  // Calculate the number of vectors in the input data set
  //-----------------------------------------------------------------

  ImageSizeType size = inputImage->GetBufferedRegion().GetSize();

  unsigned int totalNumVecsInInput = 1;
  for ( unsigned int i = 0; i < TInputImage::ImageDimension; i++ )
    {
    totalNumVecsInInput *= (SizeValueType)size[i];
    }

  //-----------------------------------------------------------------
  //Loop through the input image vectors
  //-----------------------------------------------------------------

  InputPixelVectorType inputImagePixelVector;

  for ( unsigned int n = 0; n < totalNumVecsInInput; n++ )
    {
    // keep convention that ties go to lower index
    bestdistortion = m_DoubleMaximum;
    bestcodeword = 0;

    for ( unsigned int i = 0; i < m_CurrentNumberOfCodewords; i++ )
      {
      // find the best codeword
      tempdistortion = 0.0;
      inputImagePixelVector = inputImageIt.Get();

      for ( unsigned int j = 0; j < m_VectorDimension; j++ )
        {
        diff = (double)( inputImagePixelVector[j] - m_Codebook[i][j] );
        tempdistortion += diff * diff;

        if ( tempdistortion > bestdistortion ) { break; }
        }

      if ( tempdistortion < bestdistortion )
        {
        bestdistortion = tempdistortion;
        bestcodeword = i;
        }

      // if the bestdistortion is 0.0, the best codeword is found
      if ( bestdistortion == 0.0 ) { break; }
      }

    m_CodewordHistogram[bestcodeword][0] += 1;
    m_CodewordDistortion[bestcodeword][0] += bestdistortion;
    *distortion += bestdistortion;

    //inputImagePixelVector = *tempImgIt;
    inputImagePixelVector = inputImageIt.Get();

    for ( unsigned int j = 0; j < m_VectorDimension; j++ )
      {
      m_Centroid[bestcodeword][j] += inputImagePixelVector[j];
      }

    ++inputImageIt;
    } // all training vectors have been encoded

  // compute table frequency and distortion
  for ( unsigned int i = 0; i < m_CurrentNumberOfCodewords; i++ )
    {
    if ( m_CodewordHistogram[i][0] > 0 )
      {
      m_CodewordDistortion[i][0] /= (double)m_CodewordHistogram[i][0];
      }
    }

  // compute centroid
  for ( unsigned int i = 0; i < m_CurrentNumberOfCodewords; i++ )
    {
    if ( m_CodewordHistogram[i][0] > 0 )
      {
      for ( unsigned int j = 0; j < m_VectorDimension; j++ )
        {
        m_Centroid[i][j] /= (double)m_CodewordHistogram[i][0];
        }
      }
    }

  // normalize the distortions
  *distortion /= (double)totalNumVecsInInput;

  delete[] tempVec;

  // check for bizarre errors
  if ( *distortion < 0.0 )
    {
    itkExceptionMacro(<< "Computational overflow");
    }
} // End nearest_neighbor_search_basic

//-----------------------------------------------------------------
template< typename TInputImage,
          typename TMembershipFunction >
void
ImageKmeansModelEstimator< TInputImage, TMembershipFunction >
::SplitCodewords(int currentSize, int numDesired, int scale)
{
  double *newCodebookData = (double *)new double[m_VectorDimension];
  double *inCodebookData  = (double *)new double[m_VectorDimension];

  for ( int i = 0; i < numDesired; i++ )
    {
    for ( unsigned int j = 0; j < m_VectorDimension; j++ )
      {
      inCodebookData[j] = m_Codebook[i][j];
      }

    Perturb(inCodebookData, scale, newCodebookData);

    for ( unsigned int j = 0; j < m_VectorDimension; j++ )
      {
      m_Codebook[i + currentSize][j] = newCodebookData[j];
      }
    }

  delete[] inCodebookData;
  delete[] newCodebookData;
} // End splitcodewords

//-----------------------------------------------------------------
template< typename TInputImage,
          typename TMembershipFunction >
void
ImageKmeansModelEstimator< TInputImage, TMembershipFunction >
::Perturb(double *oldCodeword,
          int scale,
          double *newCodeword)
{
  unsigned int i;
  double       addoffset;
  double       muloffset;
  double       rand_num;

  addoffset = m_OffsetAdd / std::pow(2.0, (double)scale);
  muloffset = m_OffsetMultiply / std::pow(2.0, (double)scale);

  for ( i = 0; i < m_VectorDimension; i++ )
    {
    srand( (unsigned)time(ITK_NULLPTR) );
    rand_num = ( rand() ) / ( (double)RAND_MAX );

    if ( oldCodeword[i] == 0.0 )
      {
      newCodeword[i] = addoffset * rand_num;
      }

    else if ( std::fabs(oldCodeword[i]) < 0.9 * addoffset )
      {
      newCodeword[i] = oldCodeword[i];

      if ( oldCodeword[i] < 0 )
        {
        newCodeword[i] -= addoffset * rand_num;
        }
      else
        {
        newCodeword[i] += addoffset * rand_num;
        }
      }

    else
      {
      newCodeword[i] = oldCodeword[i] + muloffset * oldCodeword[i] * rand_num;
      }
    } // End looping through the vector
}     // End perturb

//-----------------------------------------------------------------
template< typename TInputImage,
          typename TMembershipFunction >
int
ImageKmeansModelEstimator< TInputImage, TMembershipFunction >
::WithoutCodebookUseLBG()
{
  //itkDebugMacro(<<"Start local function lbg design()");

  unsigned int tmp_ncodewords, j;

// do the LBG algorithm
// iterations begins here
// start with one word codebook

// set initial distortion
  m_OutputDistortion = m_DoubleMaximum;

  // Apply the generalized Lloyd algorithm on all codebook sizes
  for ( tmp_ncodewords = 1; tmp_ncodewords < m_NumberOfCodewords; )
    {
    // run the GLA for codebook of size i
    // run gla
    WithCodebookUseGLA();

    // if empty cells, do not continue
    // if distortion is zero, no need to continue.
    if ( m_OutputNumberOfEmptyCells > 0 || m_OutputDistortion == 0.0 ) { break; }

    // find the number of new codewords to be made (j-tmp_ncodewords)
    j = 2 * tmp_ncodewords;
    if ( j > m_NumberOfCodewords ) { j = m_NumberOfCodewords; }

    // split the codewords

    // increase size of codebook
    const SizeValueType oldSize = m_Codebook.rows();
    Reallocate(oldSize, j);

    // initialize the new codewords
    SplitCodewords(tmp_ncodewords, ( j - tmp_ncodewords ), (int)0);

    // if error, do not continue

    // increment the codebook size
    tmp_ncodewords = j;
    }

  // if there are no errors, no empty cells and the distortion is positive,
  // create the final codebook
  if ( m_OutputNumberOfEmptyCells == 0 && m_OutputDistortion > 0.0 )
    {
    // run gla
    WithCodebookUseGLA();
    }

  // done with all iterations

  const SizeValueType codebookSize = m_Codebook.rows();
  if ( m_NumberOfCodewords != codebookSize )
    {
    itkDebugMacro(<< "Returning fewer codewords than requested");
    } // end if

  //itkDebugMacro(<<"Done with local function LBG ()");

  return LBG_COMPLETED;
} // End WithoutCodebookUseLBG()
} // namespace itk

#endif
