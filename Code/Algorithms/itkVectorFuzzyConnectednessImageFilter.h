/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorFuzzyConnectednessImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkVectorFuzzyConnectednessImageFilter_h
#define __itkVectorFuzzyConnectednessImageFilter_h

#include "itkImage.h"
#include "itkImageToImageFilter.h"
#include "itkScalar.h"
#include "itkVector.h"
#include "itkMatrix.h"


#include <vector>
#include <list>


namespace itk{

/** \class VectorFuzzyConnectednessImageFilter
 *
 * The basic concepts in our program are fuzzy affinity, fuzzy connectedness, 
 * scale and relative fuzzy connectedness.
 *
 * Fuzzy affinity is a local fuzzy relation which is defined on the whole image domain. 
 * It assigns to every pair of nearby voxels a strength of local hanging togetherness. 
 * A global fuzzy relation called fuzzy connectedness is defined on the whole image domain 
 * which assigns to every pair of voxels a strength of global hanging togetherness. 
 * The strength of hanging togetherness (connectedness) between two voxels is the largest 
 * of the strengths of all paths between them. Scale-based fuzzy affinity and fuzzy connectedness
 * can make our algorithms more robust and less sensitive to noise. "Scale" is a number assigned
 * to every voxel in the image. It represents the radius of the largest ball centered at the 
 * voxel, within which the intensity is homogeneous. In determining affinity between two voxels,
 * all voxels within the ball associated with both voxels are considered.
 *
 * In our program, fuzzy affinity, fuzzy connectedness and scale computation are done in vector
 * space comprised of R-, G-, B- components of a color image.
 *
 * The other important concept is relative fuzzy connectedness. An object gets defined in an 
 * image because of the presence of other co-objects. All co-objects of importance that are 
 * present in an image are let to compete among themselves in having voxels as their members. 
 * In this competition, every pair of voxels in the image will have a strength of connectedness
 * in each object. The object in which this strength is highest will claim membership of the 
 * voxels. 
 * 
 * Usage:
 *
 *1.	Use SetInput to import the input image.
 *2.	Use SetObjects to set the number of objects of importance in the image.
 *3.	Use SetSelectedObject to specify the particular object to be segmented.
 *4.	Use Initialization to allocate memory in terms of the number of objects.
 *5.	Use SetObjectsSeed to specify seed points of objects.
 *6.	Use SetObjectsMean to specify the mean value of objects.
 *7.	Use SetObjectsMatrix to specify the covariance matrix of objects.
 *      Note: parameters of function 6 and 7 can be computed via specified training region.
 *8.	Run DoFuzzySegment to perform the segmentation.
 *9.	Use GetOutput to obtain the resulting binary image.
 *
 */

template <class TInputImage, class TOutputImage>
class VectorFuzzyConnectednessImageFilter:
  public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef VectorFuzzyConnectednessImageFilter       Self;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer <Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageToImageFilter<TInputImage,TOutputImage>   Superclass;

  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(VectorFuzzyConnectednessImageFilter,ImageToImageFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  enum {ImageDimension = TInputImage::ImageDimension };

  
  typedef   itk::Matrix<double,3,3>               MatrixType;
  typedef   itk::Vector<int,3>                    IntVector;


  typedef   TInputImage	                          InputImageType;
  typedef   TOutputImage                          OutputImageType;
  typedef   Image <unsigned short,ImageDimension> UShortImage;
  typedef   typename TInputImage::IndexType       IndexType;
  typedef   typename TInputImage::SizeType        SizeType;
  typedef   typename TOutputImage::RegionType     RegionType;

  typedef   std::list<IndexType>                  ListType;
  typedef   std::vector<IntVector>                OffsetType;
  typedef   std::vector<float>                    FloatType;
  
  /**
   * Set the object number be segmented in the input image.
   */
  itkSetMacro(Objects, int);
  /**
   * Get the object number be segmented in the input image.
   */
  itkGetMacro(Objects, int);

  /**
   * Set the selected object number to be segmented in the input image.
   */
  itkSetMacro(SelectedObject, int);
  /**
   * Get the selected object number to be segmented in the input image.
   */
  itkGetMacro(SelectedObject, int);  

  /**
   * Setting the covariance matrix for specified object:
   */
  void SetObjectsMatrix(const MatrixType object_max,const int object_num);
	
  /**
   * Setting the seed points for specified object.
   */
  void SetObjectsSeed( const IndexType &seed, const int object_num);

  /**
   * Setting the seed points for specified object.
   */
  void SetObjectsMean(const IntVector, const int object_num);

  /**
   * Allocate the variate in terms of the number of Objects
   */
  void Initialization();
 
  /**
   * Perform the segmentation.
   */
  void DoFuzzySegmentation();	

protected:
  VectorFuzzyConnectednessImageFilter();
  ~VectorFuzzyConnectednessImageFilter();

private:


  SizeType                       m_Size;
  OffsetType                     *m_SpherePointsLoc;
  int                            *m_SpherePointsNum;

  double                         m_Mask[3][3][3];
  double                         m_MaskTotal;
  IntVector                      m_HomoMaxDiff;
  IntVector                      m_FeaturesThreshold;
  IntVector                      m_PowerValue;
  
  int                            m_Objects;
  int                            m_SelectedObject;

  MatrixType                     *m_ObjectsCovMatrix;
  IntVector                      *m_ObjectsMean;

  IntVector                      *m_ObjectsMaxDiff;
  FloatType                      *m_ObjectsMap;
  ListType                       *m_ObjectsSeed;


  std::vector<float>             m_HomogeneityMap;
  std::vector<float>             m_ScaleMap;
  std::vector<char>              m_ScaleArray;
  std::vector<double>            m_Material;


  typename InputImageType::Pointer   m_InputImage;
  typename UShortImage::Pointer      m_ObjectFuzzyScene;
  typename UShortImage::Pointer      m_BackgroundFuzzyScene;
  typename OutputImageType::Pointer  m_SegmentObject; 


  std::vector<unsigned short>  m_Xaffinity;
  std::vector<unsigned short>  m_Yaffinity;
  std::vector<unsigned short>  m_Zaffinity;


  void ScalePrepare();
  void Compute_LookupTable();
  void Compute_Scale();
  void Compute_Affinity();
  void FastTracking(int);
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorFuzzyConnectednessImageFilter.txx"
#endif

#endif
