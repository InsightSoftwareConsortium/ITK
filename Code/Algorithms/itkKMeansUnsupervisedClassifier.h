/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKMeansUnsupervisedClassifier.h
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
#ifndef _itkKMeansUnsupervisedClassifier_h
#define _itkKMeansUnsupervisedClassifier_h

#include "itkObject.h"
#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_math.h"
//#include "vnl/vnl_numeric_limits.h"

#include "itkUnsupervisedClassifier.h"

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <math.h>
#include <float.h>

#include "itkVector.h"
#include <vector>
#include "vnl/vnl_vector_ref.h"
#include "vnl/vnl_matrix.h"

#define  ONEBAND        1
#define  GLA_CONVERGED     1
#define  GLA_NOT_CONVERGED 2
#define  LBG_COMPLETED     3



namespace itk
{

/** \class KMeansUnsupervisedClassifier
 * \brief Implementation KMeansUnsupervisedClassifier object
 *
 * This object performs clusterring of data sets into different clusters
 * either using a user provided seed points as initial guess or generates
 * the clusters using a recursive approach when the user provides the
 * number of desired clusters. Each cluster is represented by its cluster
 * center. The two algorithms used are the generalized Lloyd
 * algorithm (GLA) and the Linde-Buzo-Gray algorithms. The cluster centers
 * are also referred to as codewords and a table of cluster centers is 
 * is referred as a codebook.
 *
 * As required by the GLA algorithm, the initial seed cluster should contain 
 * approximate centers of clusters.  The GLA algorithm genrates an updated
 * cluster centers that result in a lower distortion than the input seed 
 * clusterwhen the input vectors are mapped/classified/labelled using the 
 * given codebooks.
 *
 * If no codebook is provided, the Linde-Buzo-Gray algorithm is used.
 * This algorithm uses the GLA algorithm at its core to generate the
 * centroids of the input vectors (data). However, since there is no initial
 * codebook, LBG first creates a one word codebook (or centroid of one
 * cluster comprising of all the input training vectors). The LBG uses 
 * codeword/or centroid splitting to create increasing number of clusters.
 * Each new set of clusters are optimized using the GLA algorithm. 
 * The number of clusters increases as $2^{n}$ n= 0, 1, ... The codebook 
 * is expected to be in the form of a vnl matrix, where there are N rows.
 * each row representing the cluster mean of a given cluster. The number
 * of columns in a the codebook should be equal to the input image vector
 * dimension.
 *
 * The threshold parameter controls the ``optimality'' of the returned
 * codebook where optimality is related to the least possible
 * mean-squared error distortion that can be found by the algorithm.
 * For larger thresholds, the result will be less optimal.  For
 * smaller thresholds, the result will be more optimal.  If a more
 * optimal result is desired, then the algorithm will take longer to
 * complete. A reasonable threshold value is 0.01.
 *
 * If, during the operation of the algorithm, there are any unused
 * clusters or cells, the m_OffsetAdd and m_OffsetMul parameters is
 * used to split the cells with the highest distortion.  This
 * functions will attempt to fill empty cells up to 10 times (unless
 * the overall distortion is zero). Using 0.01 is a reasonable default  
 * values for the m_OffsetAdd and m_OffsetMul parameters.
 *
 * If the GLA is unable to resolve the data into the desired number of
 * clusters or cells, only the codewords which were used will be
 * returned. 
 *
 * In terms of clustering, codewords are cluster centers, and a codebook
 * is a table containing all cluster centers.  The GLA produces results
 * that are equivalent to the K-means clustering algorithm.
 *
 * For more information about the algorithms, see A. Gersho and R. M. Gray,
 * {\em Vector Quantization and Signal Compression},
 * Kluwer Academic Publishers, Boston, MA, 1992.
 *
 * This object supports data handling of multiband images. The object
 * accepts the input image in vector format only, where each pixel is a 
 * vector and each element of the vector corresponds to an entry from
 * 1 particular band of a multiband dataset. A single band image is treated 
 * as a vector image with a single element for every vector. The classified
 * image is treated as a single band scalar image. Currently there is no
 * support for generating a classified image. This class can be extended by
 * overriding the virtual GetPixelDistance() function.
 *
 * \ingroup UnSupervisedClassificationFilters 
 */

template <class TInputImage, class TClassifiedImage>
class ITK_EXPORT KMeansUnsupervisedClassifier 
: public UnsupervisedClassifier <TInputImage,TClassifiedImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef KMeansUnsupervisedClassifier   Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef UnsupervisedClassifier<TInputImage,TClassifiedImage> Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(KMeansUnsupervisedClassifier,UnsupervisedClassifier);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Type definition for the input image.
   */
  typedef typename TInputImage::Pointer   InputImageType;

  /**
   * Type definition for the vector associated with
   * input image pixel type.
   */     
  typedef typename TInputImage::PixelType::VectorType    
    InputImageVectorType;

  /**
   * Type definition for the input image pixel type.
   */
  typedef typename TInputImage::PixelType     InputImagePixelType;

  /**
   * Type definition for a double matrix.
   */
  typedef vnl_matrix<double> CodebookMatDblType; 

  /**
   * Type definition for an integer vector.
   */
  typedef vnl_matrix<int>    CodebookMatIntType;

  /**
   * Set the cluster centers
   */
  void SetCodebook(CodebookMatDblType InCodebook);

  /**
   * Get the cluster centers
   */
  itkGetMacro(Codebook,CodebookMatDblType);

  /**
   * Get the optimized codebook or the centroids of the clusters.
   */
  CodebookMatDblType GetOutCodebook()
  {
    return m_Codebook;
  }

  /**
   * Set the threshold parameter
   */
  itkSetMacro(Threshold,double);

  /**
   * Get the threshold parameter
   */
  itkGetMacro(Threshold,double);

  /**
   * Set the offset add parameter
   */
  itkSetMacro(OffsetAdd,double);

  /**
   * Get the offset add parameter
   */
  itkGetMacro(OffsetAdd,double);

  /**
   * Set the offset multiplication parameter
   */
  itkSetMacro(OffsetMul,double);

  /**
   * Get the offset multiplication parameter
   */
  itkGetMacro(OffsetMul,double);

  /**
   * Set the maximum number of attempts to split a codeword  
   */
  itkSetMacro(MaxSplitAttempts,int);

  /**
   * Get the manimum number of attempts to split a codeword  
   */
  itkGetMacro(MaxSplitAttempts,int);

  /**
   * Return the codebook/cluster centers
   */
  CodebookMatDblType GetKmeansResults()
  {
    return m_Centroid;
  }

  /**
   * Generate the cluster centers corresponding to the initial codebook.
   * If no codebook is provided, then use the number of classes to 
   * determine the number of cluster centers desired. This is the
   * the base function to call the K-means classifier.
   */
  void Cluster();

  /**
   * Given a pixel value return the probability  
   * of the pixel belonging to different classes
   * as a distance metric. The higher the distance 
   * the lower is the probability of a the pixel 
   * belonging to that class.
   * For the unsupervised classifier this function
   * has not been implemented and returns a null 
   * value. 
   */
  virtual double *GetPixelDistance(InputImageVectorType &inPixelVec){return 0;}

  /**
   * Print out the results on the screen for visual feedback
   */
  void PrintKmeansAlgorithmResults();

protected:
  /**
   * Constructor
   */
  KMeansUnsupervisedClassifier();

  /**
   * Destructor
   */
  ~KMeansUnsupervisedClassifier();

  /**
   * Copy constructor
   */
  KMeansUnsupervisedClassifier(const Self&) {}

  /**
   * Assignment operator
   */
  void operator=(const Self&) {}

  /**
   * Print self identity
   */      
  void PrintSelf(std::ostream& os, Indent indent) const;

  /**
   * Allocate memory for the Output Model
   */
  void Allocate();

private:
  typedef typename TInputImage::SizeType ImageSizeType;

  //Set up the vector to store the image  data
  typedef typename TInputImage::PixelType::VectorType InVectorType;

  void Reallocate(int oldSize, int newSize);

  //Local functions
  int  GLA();
  int  LBG();

  void nearest_neighbor_search_basic(double *distortion);
  
  void splitcodewords(int currentSize, 
                      int numDesired, 
                      int scale);
  
  void perturb(double *oldCodeword, 
                       int scale, 
                       double *newCodeword);

  CodebookMatDblType  m_Codebook;

  // Buffer for K-means calcualtions
  CodebookMatDblType  m_Centroid;

  double              m_Threshold;
  double              m_OffsetAdd;
  double              m_OffsetMul;
  int                 m_MaxSplitAttempts;

  unsigned long       m_NumClasses;
  bool                m_ValidInCodebook;
  double              m_DoubleMax;
  double              m_OutDist;
  int                 m_OutNEmptyCells;

  unsigned long       m_VecDim;
  unsigned long       m_Ncodewords;
  unsigned long       m_CurrentNcodewords;
  
  CodebookMatIntType  m_CodewordHist;
  CodebookMatDblType  m_CodewordDist;

}; // class KMeansUnsupervisedClassifier


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKMeansUnsupervisedClassifier.txx"
#endif



#endif










