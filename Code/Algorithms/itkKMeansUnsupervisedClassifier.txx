/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKMeansUnsupervisedClassifier.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkKMeansUnsupervisedClassifier_txx
#define _itkKMeansUnsupervisedClassifier_txx

#include "itkNumericTraits.h"

namespace itk
{

template<class TInputImage, class TClassifiedImage>
KMeansUnsupervisedClassifier<TInputImage,TClassifiedImage>
::KMeansUnsupervisedClassifier(void)
{
  m_ValidInCodebook  = false; 
  m_DoubleMax        = NumericTraits<double>::max();
  m_Threshold        = 0.01;
  m_OffsetAdd        = 0.01;
  m_OffsetMultiply        = 0.01;
  m_MaxSplitAttempts = 10;
  m_NumberOfClasses       = 0;
}

template<class TInputImage, class TClassifiedImage>
KMeansUnsupervisedClassifier<TInputImage, TClassifiedImage>
::~KMeansUnsupervisedClassifier(void)
{

}

// PrintSelf

template <class TInputImage, class TClassifiedImage>
void
KMeansUnsupervisedClassifier<TInputImage,TClassifiedImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os,indent );
  os << indent << "Unsupervised Classifier / Clusterer" << std::endl;
  os << indent << "Offset value for addition:" << m_OffsetAdd << std::endl;
  os << indent << "Offset value for multiplication:" << m_OffsetMultiply << std::endl;
  os << indent << "Maximum number of attempts to split a cluster: " << m_MaxSplitAttempts << std::endl; 
  os << indent << "Codebook : " << m_Codebook << std::endl;
  os << indent << "Threshold value :" << m_Threshold << std::endl;

}// end PrintSelf

// Set the input codebook and allocate memory 
// for the output codebook and other scratch memory
template<class TInputImage, class TClassifiedImage>
void
KMeansUnsupervisedClassifier<TInputImage,TClassifiedImage>
::SetCodebook( CodebookMatDblType inCodebook )
{
  m_Codebook        = inCodebook;

  //Check if the input codebook is a valid
  if( InputImagePixelType::GetVectorDimension() == m_Codebook.cols() )
    {
    m_ValidInCodebook = true;
    this->Allocate();
    }

}//End SetInCodebook


template<class TInputImage, class TClassifiedImage>
void
KMeansUnsupervisedClassifier<TInputImage,TClassifiedImage>
::Allocate()
{
  unsigned long initCodebookSize, finalCodebookSize;;

  m_VectorDimension = InputImagePixelType::GetVectorDimension();

  if( m_ValidInCodebook )
    {
    m_Ncodewords = m_Codebook.rows();
    m_VectorDimension     = m_Codebook.cols();
    // Set the initial and final codebook size
    initCodebookSize  = m_Ncodewords;
    finalCodebookSize = m_Ncodewords;

    }// end(if valid codebook clause)
  else
    {
    m_ValidInCodebook = true;

    //Check the validity of the n
    if( this->GetNumberOfClasses() <= 0)
      {
      throw ExceptionObject(__FILE__, __LINE__);
      }

    m_Ncodewords      = this->GetNumberOfClasses();
    m_VectorDimension          = InputImagePixelType::GetVectorDimension();

    // Set the initial and final codebook size

    initCodebookSize = (unsigned long) 1;
    finalCodebookSize= (unsigned long)m_Ncodewords;

    m_Codebook.resize( initCodebookSize, m_VectorDimension );

    // initialize m_Codebook to 0 (it now has only one row) 
    m_Codebook.fill(NULL);

    } // end (else not valid codebook clause)

  //----------------------------------------------------------
  //Allocate scratch memory for the centroid, codebook histogram
  //and the codebook distorsion

  m_Centroid.resize( finalCodebookSize, m_VectorDimension );
  m_Centroid.fill( NULL );

  m_CodewordHist.resize( m_Ncodewords, 1 ); 
  m_CodewordHist.fill( NULL );

  m_CodewordDist.resize( m_Ncodewords, 1 );  
  m_CodewordDist.fill( NULL );

} // end Allocate function


template<class TInputImage, class TClassifiedImage>
void
KMeansUnsupervisedClassifier<TInputImage,TClassifiedImage>
::Reallocate( int oldSize, int newSize )
{
  //Set up a temporary codebook
  CodebookMatDblType tmpCodebook( oldSize, m_VectorDimension ); 

  //Save the contents of m_Codebook in the tmpCodebook
  tmpCodebook = m_Codebook;
  m_Codebook.resize( newSize, m_VectorDimension );

  // Copy back the saved data into the codebook

  if( oldSize < newSize )
    {
    for( int r = 0; r < oldSize; r++ )
      for( unsigned int c = 0; c < m_VectorDimension; c++ )
        m_Codebook[r][c] = tmpCodebook[r][c]; 

    for( int r = oldSize; r < newSize; r++ )
      for(unsigned int c = 0; c < m_VectorDimension; c++ )
        m_Codebook[r][c] = 0; 

    } // if oldsize is smaller than the new size
  else
    {
    for( int r = 0; r < newSize; r++ )
      for( unsigned int c = 0; c < m_VectorDimension; c++ )
        m_Codebook[r][c] = tmpCodebook[r][c]; 

    }// else oldsize is greater than the new size

}// end Reallocate

template<class TInputImage, class TClassifiedImage>
void 
KMeansUnsupervisedClassifier<TInputImage,TClassifiedImage>
::PrintKmeansAlgorithmResults()
{

  itkDebugMacro(<<"                                    ");
  itkDebugMacro(<<"Results of the clustering algorithms");
  itkDebugMacro(<<"====================================");

  itkDebugMacro(<<"                                    ");
  itkDebugMacro(<<"Means of the clustered vector       ");
  itkDebugMacro(<<"++++++++++++++++++++++++++++++++++++");

  itkDebugMacro(<<m_Centroid);

  itkDebugMacro(<<"                                    ");
  itkDebugMacro(<<"Distortion measures                 ");
  itkDebugMacro(<<"+++++++++++++++++++++++++++++++++++ ");

  itkDebugMacro(<<m_CodewordDist);

  itkDebugMacro(<<"                                    ");
  itkDebugMacro(<<"Histogram of the vector             ");
  itkDebugMacro(<<"+++++++++++++++++++++++++++++++++++ ");

  itkDebugMacro(<<m_CodewordHist);

}// End PrintKmeansAlgorithmResults


// Takes a set of training images and returns the means 
// and variance of the various classes defined in the
// training set.

template<class TInputImage, class TClassifiedImage>
void 
KMeansUnsupervisedClassifier<TInputImage,TClassifiedImage>
::Cluster()
{

  //If a codebook is provided by the user then call the 
  //Kmenas algorithm directly that is based on the 
  //Generalized Lloyn algorithm (GLA) if a valid codebook
  //is provided or m_NumberOfClasses is set to 0, else 
  //Linde-Buzo-Gray algorithm is used for clustering
  if(m_ValidInCodebook)
    {
    GLA();
    }
  else
    {
    //Assign memory for the initial codebook
    //since no input codebook is provided for this
    //function
    Allocate();
    m_CurrentNcodewords = m_Codebook.rows();
    LBG();
    }

  m_ValidInCodebook = false;

}// end ApplyKMeans



template<class TInputImage, class TClassifiedImage>
int 
KMeansUnsupervisedClassifier<TInputImage,TClassifiedImage>
::GLA()
{
  // Do the Lloyd iteration.  Use the nearest neighbor condition to
  // find the cells.  Then find the centroid of each cell.

// First pass requires very large distortion

  double olddistortion = m_DoubleMax;
  double distortion, tempdistortion;
  int    pass = 0; // no empty cells have been found yet 
  int    emptycells;
  int    bestcodeword;

  m_CurrentNcodewords = m_Codebook.rows();

  do 
    {
    // encode all of the input vectors using the given codebook 
    nearest_neighbor_search_basic(&distortion);

    // check for lack of convergence 
    if ( olddistortion < distortion ) 
      {
      itkErrorMacro(<<"Distortion is increasing, not decreasing");
      throw ExceptionObject(__FILE__, __LINE__); // GLA_NOT_CONVERGED;
      }

    // find number of empty cells
    emptycells = 0;
    for ( unsigned int i = 0; i < m_CurrentNcodewords; i++ ) 
      {
      if ( m_CodewordHist[i][0] == 0 ) 
        {
        emptycells += 1;
        m_CodewordDist[i][0] = 0.0;
        }
      }

    // if distortion = 0.0, or
    // if change in distortion < threshold AND there aren't any empty cells,
    // and exit 
    if ( (distortion == 0.0) || ( (emptycells == 0) &&
                                  (olddistortion - distortion) / distortion < m_Threshold) ) 
      {
      m_OutNEmptyCells = emptycells;
      m_OutDist    = distortion;
      return GLA_CONVERGED;
      }

    // no empty cells, find new centroids and reinitialize for next pass 
    if ( emptycells == 0 ) 
      {
      for ( unsigned int i = 0; i < m_CurrentNcodewords; i++ ) 
        for( unsigned int j = 0; j < m_VectorDimension; j++ ) 
          m_Codebook[i][j] = m_Centroid[i][j];

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
        itkWarningMacro(<<"Unable to fill all empty cells");
        m_OutNEmptyCells = emptycells;
        m_OutDist = distortion;
        return GLA_CONVERGED;
        } 

      // try getting new codewords, send a warning to user 
      itkWarningMacro(<<"Attempting to fill empty cells in the codebook");

      // consolidate the highest distortion codewords into the beginning
      // of the array.  Take care to protect zero distortion codewords
      // which have a positive m_CodewordHist.  note: there must be a
      // faster sort algorithm, but this event should be very unlikely
      for ( unsigned int n = 0; n < m_CurrentNcodewords - emptycells; n++ ) 
        {
        tempdistortion = 0.0;
        bestcodeword = 0;
        for ( unsigned int i = 0; i < m_Ncodewords; i++ ) 
          {
          if ( ( m_CodewordDist[i][0] >= tempdistortion ) &&
               ( m_CodewordHist[i][0] > 0) ) 
            {
            tempdistortion = m_CodewordDist[i][0];
            bestcodeword = i;
            }
          }

        // put highest distortion centroid into nth codebook row,
        // and erase the set the hightest centroid stats to 0 so
        // it will not be used again.

        // find centroid, reinitialize 

        for(unsigned int j = 0; j < m_VectorDimension; j++ )
          m_Codebook[n][j] = m_Centroid[bestcodeword][j];

        m_CodewordHist[bestcodeword][0] = 0;
        m_CodewordDist[bestcodeword][0] = 0.0;
        }

      // split the required number of codewords
      splitcodewords( m_CurrentNcodewords - emptycells, 
                      emptycells, pass );

      olddistortion = distortion;
      pass++;
      } // end else
    } while ( pass <= m_MaxSplitAttempts );
  itkErrorMacro(<<"Fatal error");
  throw ExceptionObject(__FILE__, __LINE__); //return GLA_NOT_CONVERGED;

}// end localfn_GLA

template<class TInputImage, class TClassifiedImage>
void 
KMeansUnsupervisedClassifier<TInputImage,TClassifiedImage>
::nearest_neighbor_search_basic( double *distortion )
{
  //itkDebugMacro(<<"Start nearest_neighbor_search_basic()");

  double bestdistortion, tempdistortion, diff;
  int    bestcodeword;
  double *tempVec = ( double * ) new double[m_VectorDimension];

// unused: double *centroidVecTemp = ( double * ) new double[m_VectorDimension];

// initialize codeword histogram and distortion 
  for ( unsigned int i = 0; i < m_CurrentNcodewords; i++ ) 
    {
    m_CodewordHist[i][0] = 0;
    m_CodewordDist[i][0] = 0.0;
    }

  // initialize centroid if it exists 
  m_Centroid.fill( NULL );

  // perform encoding using partial distortion method 
  *distortion = 0.0;

  //-----------------------------------------------------------------
  // Declare the iterators for the image and the codebook
  //-----------------------------------------------------------------
  InputImageType  inputImage = this->GetInputImage();

  typedef typename TInputImage::PixelType    InputPixel;

  typedef
    ImageRegionIterator<TInputImage> InputIterator;

  InputIterator inIt( inputImage, inputImage->GetBufferedRegion() );

  InputIterator imgIt     =  inIt.Begin();
  InputPixel *tempImgIt   = &(imgIt.Value()); // dangerous!
  InputPixel *inImgIt     = &(imgIt.Value()); // dangerous!

//-----------------------------------------------------------------
// Calculate the number of vectors in the input data set
//-----------------------------------------------------------------

  ImageSizeType size = inputImage->GetBufferedRegion().GetSize();

  // unused:  unsigned long *temp =new unsigned long [TInputImage::ImageDimension];

  unsigned int totalNumVecsInInput = 1;
  for( unsigned int i = 0; i < TInputImage::ImageDimension; i++ ) 
    totalNumVecsInInput *= (unsigned long) size[i];

//-----------------------------------------------------------------
//Loop through the input image vectors
//-----------------------------------------------------------------

  InVectorType   inImgVec; 

  for (unsigned int n = 0; n < totalNumVecsInInput; n++) {

  // make tempvector point to one vector 
  tempImgIt = inImgIt + n;

  // keep convention that ties go to lower index 
  bestdistortion = m_DoubleMax;
  bestcodeword = 0;    

  for ( unsigned int i = 0; i < m_CurrentNcodewords; i++ ) 
    { 
    // find the best codeword 
    tempdistortion = 0.0;
    inImgVec = *tempImgIt;

    for ( unsigned int j = 0; j < m_VectorDimension; j++ ) 
      {
      diff = ( double ) ( inImgVec[j] - m_Codebook[i][j] ); 
      tempdistortion += diff*diff;

      if ( tempdistortion > bestdistortion ) break;
      }

    if ( tempdistortion < bestdistortion ) 
      {
      bestdistortion = tempdistortion;
      bestcodeword = i;
      }

    // if the bestdistortion is 0.0, the best codeword is found
    if ( bestdistortion == 0.0 ) break;
    }

  m_CodewordHist[bestcodeword][0] += 1;
  m_CodewordDist[bestcodeword][0] += bestdistortion;
  *distortion += bestdistortion;

  inImgVec = *tempImgIt;

  for (unsigned int j = 0; j < m_VectorDimension; j++ ) 
    m_Centroid[bestcodeword][j] += inImgVec[j];

  } // all training vectors have been encoded 

  // compute table frequency and distortion 
  for ( unsigned int i = 0; i < m_CurrentNcodewords; i++ ) 
    {
    if ( m_CodewordHist[i][0] > 0 ) {
    m_CodewordDist[i][0] /= (double) m_CodewordHist[i][0];
    }
    }

  // compute centroid 
  for ( unsigned int i = 0; i < m_CurrentNcodewords; i++ ) 
    {
    if ( m_CodewordHist[i][0] > 0 ) 
      {
      for ( unsigned int j = 0; j < m_VectorDimension; j++ ) 
        m_Centroid[i][j] /= (double) m_CodewordHist[i][0];
      }
    }

  // normalize the distortions 

  *distortion /= ( double ) totalNumVecsInInput;

  // check for bizarre errors 
  if ( *distortion < 0.0 ) 
    {
    itkErrorMacro(<<"Computational overflow");
    throw ExceptionObject(__FILE__, __LINE__);
    }

  //itkDebugMacro(<<"Done nearest_neighbor_search_basic()");
  delete [] tempVec;

}// End nearest_neighbor_search_basic

template<class TInputImage, class TClassifiedImage>
void 
KMeansUnsupervisedClassifier<TInputImage,TClassifiedImage>
::splitcodewords( int currentSize, int numDesired, int scale )
{
  double *newCodebookData = ( double * ) new double[m_VectorDimension];
  double *inCodebookData  = ( double * ) new double[m_VectorDimension];

  for ( int i = 0; i < numDesired; i++ ) 
    {
    for(unsigned int j = 0; j < m_VectorDimension; j++ )
      inCodebookData[j] = m_Codebook[i][j];

    perturb(inCodebookData, scale, newCodebookData);

    for(unsigned int j=0; j< m_VectorDimension; j++)
      m_Codebook[i+currentSize][j] = newCodebookData[j];
    }

  delete [] inCodebookData;
  delete [] newCodebookData;

}// End splitcodewords

template<class TInputImage, class TClassifiedImage>
void 
KMeansUnsupervisedClassifier<TInputImage,TClassifiedImage>
::perturb(double *oldCodeword, 
          int scale, 
          double *newCodeword)
{
  unsigned int  i;
  double        addoffset;
  double        muloffset;
  double        rand_num ;

  addoffset = m_OffsetAdd / pow( 2.0, ( double ) scale );
  muloffset = m_OffsetMultiply / pow( 2.0, ( double ) scale );

  for ( i = 0; i < m_VectorDimension; i++ ) 
    {
    srand( (unsigned)time( NULL ) );
    rand_num = (rand())/((double)RAND_MAX);

    if ( oldCodeword[i] == 0.0 ) 
      {
      newCodeword[i] = addoffset * rand_num;
      }

    else if ( fabs(oldCodeword[i]) < 0.9 * addoffset ) 
      {
      newCodeword[i] = oldCodeword[i];

      if ( oldCodeword[i] < 0 ) 
        {
        newCodeword[i] -=  addoffset * rand_num;
        }
      else 
        {
        newCodeword[i] +=  addoffset * rand_num;
        }
      }

    else 
      {
      newCodeword[i] = oldCodeword[i] + muloffset * oldCodeword[i] * rand_num;
      }

    } // End looping through the vector

} // End perturb

template<class TInputImage, class TClassifiedImage>
int 
KMeansUnsupervisedClassifier<TInputImage,TClassifiedImage>
::LBG()
{
  //itkDebugMacro(<<"Start local function lbg design()");

  unsigned int tmp_ncodewords, j;

// do the LBG algorithm 
// iterations begins here 
// start with one word codebook 

// set initial distortion 
  m_OutDist = m_DoubleMax;

  // Apply the generalize Lloyd algorithm on all codebook sizes 
  for ( tmp_ncodewords = 1; tmp_ncodewords < m_Ncodewords; ) 
    {
    // run the GLA for codebook of size i 
    // run gla 
    GLA();

    // if empty cells, do not continue 
    // if distortion is zero, no need to continue.
    if ( m_OutNEmptyCells > 0 || m_OutDist == 0.0 ) break;

  // find the number of new codewords to be made (j-tmp_ncodewords)
    j = 2 * tmp_ncodewords;
    if ( j > m_Ncodewords ) { j = m_Ncodewords; }

    // split the codewords

  // increase size of codebook 
    const unsigned long oldSize= m_Codebook.rows();
    Reallocate( oldSize, j);

  // initialize the new codewords 
    splitcodewords( tmp_ncodewords, ( j - tmp_ncodewords ), (int) 0 );

    // if error, do not continue 

  // increment the codebook size 
    tmp_ncodewords = j;
    }

  // if there are no errors, no empty cells and the distortion is positive,
  // create the final codebook 
  if ( m_OutNEmptyCells == 0 && m_OutDist > 0.0 ) 
    {
    // run gla 
    GLA();
    }

  // done with all iterations 

  const unsigned long codebookSize = m_Codebook.rows(); 
  if ( m_Ncodewords != codebookSize ) 
    {
    itkDebugMacro(<<"Returning fewer codewords than requested");
    }// end if

  //itkDebugMacro(<<"Done with local function LBG ()");

  return LBG_COMPLETED;
}// End LBG()

} // namespace itk








#endif
