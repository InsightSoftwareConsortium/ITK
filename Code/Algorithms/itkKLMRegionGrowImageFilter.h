/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKLMRegionGrowImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkKLMRegionGrowImageFilter_h
#define _itkKLMRegionGrowImageFilter_h

#include "itkImage.h"
#include "itkObject.h"
#include "itkImageToImageFilter.h"
#include "itkRegionGrowImageFilter.h"
#include "itkKLMSegmentationRegion.h"
#include "itkRGBPixelorderKLM.h"
#include <algorithm>
#include <functional>

namespace itk
{

/** \class KLMRegionGrowImageFilter
 * \brief Base class for a region growing object that performs energy-based 
 * region growing for multiband images.  
 * 
 * itkKLMRegionGrowImageFilter is the base class for the KLMRegionGrowImageFilter objects.
 * This object performes energy-based region growing for multiband images. 
 * Since this is based on G. Koepfler,C. Lopez and J. M. Morel's work 
 * described below, the acronym KLM is added at the end of the object name
 * .
 * The ApplyRegionGrowImageFilter() function implements the segmentation algorithm
 * that partitions the input image into non-overlapping regions
 * by minimizing an energy functional which trades off the similarity
 * of regions against the length of their shared boundary. The heart of the
 * prcess relies on the MergeRegion() method that calls a private function
 * to perform the merging of region based on the piecewise constance KLM
 * algorithm for region merging. For extensibiltiy purposes, the MergeRegion()
 * function is made virtual. Extensions can be made possible using
 * function overloading or overriding the virutual function in a derived 
 * class. It starts by breaking the image into many small regions and fitting 
 * the regions to a polynomial model.  The algorithm iteratively merges into
 * one region the two adjoining regions which are most alike in terms
 * of the specified polynomial model given the length of the border
 * between the two regions.  Internally, the energy functional is
 * evaluated using a Lagrangian parameter called lambda which is also
 * called the scale parameter as it controls the coarseness of the
 * segmentation where a small value of lambda corresponds to a finer
 * segmentation with more regions and a large value corresponds to a
 * coarse segmentation with fewer regions.  Since the algorithm grows
 * regions by merging like regions, the internal value of lambda
 * increases as the number of regions decreases.
 *
 * The user can stop the merging of regions using the SetMaxNumRegions() 
 * and SetLambda() functions.  The SetMaxNumRegions() fuction is publicly
 * inherited from its base class and internally sets the m_MaxNumRegions
 * parameter. The SetLambda() function sets the m_Lambda parameter. If the
 * number of regions in the image is equal to m_MaxNumRegions or if the
 * internal energy functional becomes greater than m_Lambda, then the 
 * merging iterations will stop.  Note that a larger value for m_Lambda
 * will result in fewer boundaries and fewer regions, while a smaller value 
 * for m_Lambda will result in more boundaries and more regions. To have 
 * m_MaxNumRegions control exactly the number of output regions, m_Lamda 
 * should be set to a very large number. To have m_Lambda control exactly 
 * the number of output regions, m_MaxNumRegions should be set to 2. As a 
 * default value the maximum lambda value is set to 1000 and m_MaxNumRegions
 * is set to 2 as default. 
 *
 * Currently implementation puts equal weight to the multichannel values.
 * In future improvements we plan to allow the user to control the weights
 * associated with each individual channels. 
 *
 * It is templated over the type of input and output image. This object 
 * supports data handling of multiband images. The object accepts images 
 * in vector format, where each pixel is a vector and each element of the
 * vector corresponds to an entry from 1 particular band of a multiband 
 * dataset. We expect the user to provide the input to the routine in vector 
 * format. A single band image is treated as a vector image with a single  
 * element for every vector.
 * 
 * This algorithm implementation takes a multiband image stored in vector
 * format as input and produces two outputs. Using the ImageToImageFilter, 
 * the piecewise constant approximation image is the output calculated
 * using the process update mechanism. The second output, i.e., the 
 * image with the region labels (segmentation image) is returned at 
 * users request by calling GetLabelledImage() function. This function
 * returns a reference to the labelled image determined using the KLM 
 * algorithm. 
 *
 * For more information about the algorithm, see G. Koepfler, C. Lopez
 * and J. M. Morel, ``A Multiscale Algorithm for Image Segmentation by
 * Variational Method,'' {\em SIAM Journal of Numerical Analysis},
 * vol. 31, pp. 282-299, 1994.
 *
 * Algorithm details:
 *
 * This function segments a two-dimensional input image into
 * non-overlapping regions $O_i$, i=1,2,...,N, where N is the total
 * number of region, by minimizing the following energy functional
 * (also known as the simplified Mumford and Shah functional):
 * $E(u,K)=\int_{\Omega-K}||u(r,c)-g(r,c)||^2{d{\Omega}}+\lambda\cdot{L(K)}$,
 * where $\Omega$ denotes the domain of an image, g(r,c) is the input
 * image, and u(r,c) is an approximation of g(r,c).  Furthermore,
 * u(r,c) is defined to be piecewise constant in regions $O_i$.  If
 * $\partial O_i$ represents the boundary of the region,
 * $K=\bigcup_{i=1}^N\partial{O_i}$ denotes the set of all region
 * boundaries and L(K) is the total length of the boundaries.  The
 * parameter $\lambda$ controls the coarseness of the segmentation
 * (i.e. a larger $\lambda$ will result in fewer boundaries).  
 *
 * Starting with small, piecewise-constant initial regions the algorithm
 * iteratively merges the two adjacent regions $O_i$ and $O_j$ which  most 
 * decrease the energy functional.  In other words, the merging criterion 
 * is based on the difference between the current energy E(u,K) and the 
 * energy that would result after a merge, $E(\hat{u},K-\partial(O_i,O_j))$,
 * where $\hat{u}$ is the piecewise constant approximation of the
 * input image g, and $\partial(O_i,O_j)$ is the common boundary
 * between region $O_i$ and $O_j$.  It can be shown that
 * $E(u,K)-E(\hat{u},K-\partial(O_i,O_j))=
 * \lambda\cdot{L(\partial(O_i,O_j))}- 
 * {(|O_i| \cdot |O_j|)\over (|O_i|+|O_j|)} ||c_i-c_j||^2$.
 *
 * Once two regions are merged the following update equations are used
 * to calculated the  constant approximation of the new region:
 * 
 * $c_{i,j} = (c_i |O_i| + c_j |O_j|) \over (|O_i| + |O_j|)$.
 *
 * Again, the merging of regions continues until the desired number of
 * regions has been reached or until the desired coarseness (specified
 * by the scale parameter $\lambda$) has been reached.
 *
 * The two outputs are possible to derive from the object: 
 * (1) u, the piecewise constant approximation (mean of the regions)
 *     to the input image set; This is currently generated by the 
 *     process object pipeline and the 
 * (2) the labelled regions in the input image set is generated by the
 *     GetLabelledImage() function.
 */

template <class TInputImage, class TOutputImage>
class ITK_EXPORT KLMRegionGrowImageFilter : public RegionGrowImageFilter<TInputImage,TOutputImage>
{

public:
  /**
   * Standard "Self" typedef.
   */
  typedef KLMRegionGrowImageFilter   Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef RegionGrowImageFilter<TInputImage,TOutputImage> Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(KLMRegionGrowImageFilter,RegionGrowImageFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Type definition for the input image.
   */
  typedef typename TInputImage::Pointer   InputImageType;

  /**
   * Type definition for the input image pixel type.
   */
  typedef typename TInputImage::PixelType InputImagePixelType;

  /**
   * Type definition for the input image pixel vector type.
   */
  typedef typename TInputImage::PixelType::VectorType InputImageVectorType;

  /**
   * Type definition for the image iterators to be used.
   */
  typedef
    ImageRegionIterator< InputImagePixelType, TInputImage::ImageDimension>  
      InputImageIterator;

  /**
   * Type definition for the output image.
   */
  typedef typename TOutputImage::Pointer   OutputImageType;

  /**
   * Type definition for the output image pixel type.
   */
  typedef typename TOutputImage::PixelType OutputImagePixelType;

  /**
   * Type definition for the output image iterators. 
   */
  typedef
    ImageRegionIterator< OutputImagePixelType,TOutputImage::ImageDimension>  
      OutputImageIterator;

  /**
   * Type definition for the output image pixel vector type.
   */
  typedef typename TOutputImage::PixelType::VectorType 
    OutputImageVectorType;

  /**
   * Type definition for the labelled image pixel type.
   */
  enum { LabelImageDimension = TInputImage::ImageDimension };
  typedef Image<unsigned short,LabelImageDimension> LabelImageType;

  /**
   * Type definition for the labelled image pointer 
   */
  typedef typename LabelImageType::Pointer LabelImagePointer;

   /**
   * Type definition for the labelled image pixel type.
   */
  typedef typename LabelImageType::PixelType    LabelImagePixelType;

  /**
   * Type definition for the labelled image iterators. 
   */
  typedef
    ImageRegionIterator< LabelImagePixelType,LabelImageDimension>  
      LabelImageIterator;

  /**
   * Storage type for the mean region intensity.
   */
  typedef vnl_matrix<double> VecDblType;

  /**
   * Type definition for the smart border type.
   */
  typedef RGBPixelorderKLM<TInputImage,TOutputImage>    BorderType;

  /**
   * Type definition for the smart border pointers object.
   */
  typedef DynamicBorderArrayKLM<BorderType>  RGBPixelorderKLMPtr;

  /**
   * Set the desired threshold parameter for lambda. See itkRGBPixelorderKLM 
   * documentation for details regarding this parameter.
   */
  itkSetMacro(MaxLambda, unsigned int);

  /**
   * Get the maximum lambda value set by the user. 
   */
  itkGetMacro(MaxLambda, unsigned int);

  /**
   * This is the interface function that calls the specific algorithm
   * implementation of region growing.
   */
  void ApplyRegionGrowImageFilter();

  /**
   * Merge two regions
   */
  virtual void MergeRegions();

  /**
   * Generate output approximated image
   */
  void GenerateOutputImage();

  /**
   * Generate labelled image
   */
  LabelImagePointer GetLabelledImage();

  /**
   * Function that prints all the region information 
   */
  void PrintAlgorithmRegionStats();

  /**
   * Function that prints all the border information 
   */
  void PrintAlgorithmBorderStats();

  /**
   * Function that prints all the border information 
   */
  void PrintAlgorithmBorderStats(bool smartBorderPointerUseFlag);

  /**
   * Calculate the statistics representing the regions. In this
   * case we comput the mean region intensity and the area of the
   * initial rectangular area. This is the function that can be
   * overriden in order to enable a different statistical 
   * representation for region initialization.
   */
  virtual void CalculateInitRegionStats( int   regionRowIndex, 
                                         int   regionColIndex, 
                                         int   regionRowGridSize,
                                         int   regionColGridSize );

protected:
  /**
   * Constructor
   */
  KLMRegionGrowImageFilter();

  /**
   * Destructor
   */
  ~KLMRegionGrowImageFilter();

  /**
   * Copy constructor
   */
  KLMRegionGrowImageFilter(const Self&) {}

  /**
   * Assignment operator
   */
  void operator=(const Self&) {}

  /**
   * Print self identity
   */      
  void PrintSelf(std::ostream& os, Indent indent);

  virtual void GenerateData();
  virtual void GenerateInputRequestedRegion();
  virtual void EnlargeOutputRequestedRegion( DataObject * );
  virtual void GenerateOutputInformation();

  /**
   * Function that calls the KLM region growing algorithm.
   */
  void ApplyKLM();

  /**
   * Initialize the RegionGrowImageFilter algorithm
   */
  void initializeKLM();

private:
  typedef typename TInputImage::SizeType InputImageSize;

  unsigned int     m_MaxLambda;
  unsigned int     m_imgWidth;
  unsigned int     m_imgHeight;
  unsigned int     m_imgDepth;

  unsigned int     m_nBorders;
  unsigned int     m_TotalBorderLength;
  double           m_RegionLambda;
  unsigned int     m_NumRegions;

  VecDblType       m_InitRegionMean;
  unsigned int     m_InitRegionArea;

  KLMSegmentationRegion<TInputImage,TOutputImage> *m_pRegions;   
  RGBPixelorderKLM<TInputImage,TOutputImage> *m_pBorders;

  RGBPixelorderKLMPtr                        *m_pBordersDynPtrs;
  RGBPixelorderKLMPtr                        *m_pBordersCandidateDynPtr;
  RGBPixelorderKLM<TInputImage,TOutputImage> *m_pBorderCandidate;

  /**
   * Function responsible for merging two regions using energy-based 
   * regions growing criteria until the desired number of regions has been
   * reached. When merging two regions, the smaller label is always 
   * assigned to the new region.  This is consistent with the connected 
   * components algorithm.
   */
  void merge_regions();

  /**
   * Function to unite borders and region borders of region1 and region2
   * into region1.  Called from \Ref{localfn_merge_regions}. This is
   * basically a merge sort of the two regions.
   */
  void union_borders(KLMSegmentationRegion<TInputImage,
                                 TOutputImage> *pRegion1,
                     KLMSegmentationRegion<TInputImage,
                                 TOutputImage> *pRegion2);

  /** Function to resolve the region labels.  Once region labels are
   * resolved, the unique labels are sorted and
   * recorded. 
   */
  void resolve_region_labels();
                                    
}; // class KLMRegionGrowImageFilter

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKLMRegionGrowImageFilter.txx"
#endif



#endif
