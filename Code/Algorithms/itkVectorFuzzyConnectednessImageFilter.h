/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorFuzzyConnectednessImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVectorFuzzyConnectednessImageFilter_h
#define __itkVectorFuzzyConnectednessImageFilter_h

#include "itkImage.h"
#include "itkImageToImageFilter.h"
#include "itkVector.h"
#include "itkMatrix.h"
#include <vector>
#include <list>

#define MAX_SCALE 8


namespace itk{

/** \class VectorFuzzyConnectednessImageFilter
 *    
 * The purpose of our program is to segment an object of interest in
 * vectorial images using fuzzy connectedness principles. Here is a very
 * brief description of these principles.
 *
 * There are four basic concepts in fuzzy connectedness: fuzzy affinity
 * [1,2], fuzzy connectedness [1], scale [2], and relative fuzzy
 * connectedness [3]. These references give a complete theoretical basis and
 * description of the underlying algorithms. For algorithm implementation and
 * efficiency, see [4].
 *
 * Fuzzy affinity is a local fuzzy relation which is defined on the whole
 * image domain.  It assigns to every pair of nearby voxels a strength of
 * local hanging togetherness.  Two components of fuzzy affinity named
 * homogeneity-feature-based affinity and object-feature-based affinity are
 * devised in a fully vectorial manner. The strength of affinity between any
 * two nearby voxels v1 and v2 is large when v1 and v2 are spatially close,
 * the intensity vectors of voxels in the "vicinity" of v1 and v2 have a high
 * degree of homogeneity, and the intensity vectors are close to an intensity
 * vector expected for the object of interest. (If v1 and v2 are far apart,
 * their affinity is considered to be 0) The extent of "vicinity" is
 * determined by the "scale" value assigned to v1 and v2.
 *
 * "Scale" is a number assigned to every voxel in the image. It represents
 * the radius of the largest ball centered at the voxel within which the
 * vectorial values of all voxels are homogeneous. In determining affinity
 * between two voxels, all voxels within the ball associated with both voxels
 * are considered. Scale-based fuzzy affinity and fuzzy connectedness can
 * make our algorithms more robust and less sensitive to noise.
 * 
 * A global fuzzy relation called fuzzy connectedness is defined on the whole
 * image domain which assigns to every pair of voxels a strength of global
 * hanging togetherness. The strength of hanging togetherness (connectedness)
 * between two voxels is the largest of the strengths of all paths between
 * them. The strength of any path is simply the smallest affinity of
 * successive pair of voxels along the path.
 * 
 * The other important concept is relative fuzzy connectedness. An object
 * gets defined in an image because of the presence of other co-objects. All
 * co-objects of importance that are present in an image are let to compete
 * among themselves in having voxels as their members. In this competition,
 * every pair of voxels in the image will have a strength of connectedness in
 * each object. The object in which this strength is highest will claim
 * membership of the voxels. Suppose that there are n objects (note that
 * image background is also an object) in the image and that we specify a
 * seed voxel si in each object.  Then any voxel v in the image has a
 * strength of connectedness with respect to each si.  Voxel v is considered
 * to belong to that object with respect to whose seed v has the highest
 * strength of connectedness [3]. The method requires some modification when
 * a set of seed voxels is specified for each object. The basic algorithmic
 * tool required in all cases is dynamic programming.
 * 
 * The fuzzy connectedness methods have been utilized on 1000s of images in
 * several applications [6-10].
 *
 * Our program takes a vectorial image as input and produces one binary
 * output. The procedure includes six main steps.
 * 1. Use function Compute_LookupTable () to setup three look-up tables
 *    m_ScaleMap, m_HomogeneityMap and m_ObjectMap in order to facilitate the
 *    computation of scale, homogeneity-based affinity and
 *    object-feature-based * affinity.
 * 2. Use function Compute_Scale() to compute scale value at every voxel in
 *    and keep them in m_ScaleArray. In our program we set the maximum scale
 *    value as 8, this value could be changed in terms of the input image.
 * 3. Use function Compute_Filter() to compute scale-based filtered vectorial
 *    value at every voxel and keep the value in m_FilterImage, this step 
 *    is to speed up the computation of next step.
 * 4. Use function Compute_Affinity() to compute fuzzy affinity value of each
 *    pair of neighboring voxels and keep them in m_Xaffinity, m_Yaffinity, and
 *    m_Zaffinity in terms of the coordinate direction. At first, we compute
 *    homogeneity-based affinity, them compute object-feature-based affinity,
 *    finally we combine them together.
 * 5. Use function FastTracking() to compute global fuzzy connectedness
 *    between every voxel and seed points of foreground (selected object) and
 *    compute global fuzzy connectedness between every voxel and seed points of
 *    background ( other objects).  And keep their values in m_ObjectFuzzyScene
 *    and m_BackgroundFuzzyScene respectively.
 * 6. In function GenerateDate(), we implement iterative relative fuzzy
 *    connectedness strategy. We let foreground and background to compete
 *    between them for having voxels as their members in an iterative fashion
 *    [5].
 *
 * Before running our program, user needs to input some parameters.
 * 
 * 1. Use function SetObjects to set the number of objects of importance in
 *    the image.
 * 2. Use function SetSelectedObject to specify the particular object to be
 *    segmented.
 * 3. Use function SetObjectsSeed to specify seed points for objects.
 * 4. Use function SetObjectsMean to specify the mean value of the intensity
 *    vector for objects.
 * 5. Use function SetObjectsMatrix to specify the covariance matrix of the
 *    intensity vector for objects.
 *
 * Reference:
 *
 * 1. J. K. Udupa and S. Samarasekera, Fuzzy connectedness and object
 *    definition: Theory, algorithms, and applications in image segmentation,
 *    Graphical Models Image Processing 58, 1996, 246-261.
 * 2. P. K. Saha, J. K. Udupa, and D. Odhner, Scale-based fuzzy connected
 *    image segmentation: theory, algorithms, and validation, Computer Vision
 *    and Image Understanding 77, 2000, 145-174.
 * 3. P. K.Saha and J. K. Udupa, Relative Fuzzy connectedness among multiple
 *    objects: Theory, algorithms, and applications in image segmentation,
 *    Computer Vision and Image Understanding 82, 2001, 42-56.
 * 4. L. G. Nyul and J. K. Udupa, Fuzzy-connected 3D image segmentation at
 *    interactive speeds, Proc. SPIE: Medical Imaging 3979, 2000, 212-223.
 * 5. P. K.Saha and J. K. Udupa, Iterative relative fuzzy connectedness and
 *    object definition: Theory, algorithms, and applications in image
 *    segmentation, in Proc. Of IEEE Workshop on Mathematical Methods in
 *    Biomedical Image Analysis, 2000, 28-35.
 * 6. J. K. Udupa, L. Wei, S. Samarasekera, Y. Miki, M. A. van Buchen, and
 *    R. I. Grossman, Multiple sclerosis lesion quantification using fuzzy
 *    connected principles, IEEE Trans. Med. Imag. 16(5), 1997, 598-609.
 * 7. B. L. Rice and J. K. Udupa, Fuzzy connected clutter-free volume
 *    rendering for MR angiography, Int. J. Imaging System. Tech. 11, 2000,
 *    62-70.
 * 8. T. Lei and J. K. Udupa, Artery-Vein Separation via MRA-An Image
 *    Processing Approch, IEEE Trans. Med. Imag. 20(8), 2001, 689-703.
 * 9. P. K. Saha, J. K. Udupa, E. F. conant, D. P. Chakraborty, and
 *    D. Sullivan, Breast Tissue density Quantification via Digitized
 *    Mammograms, IEEE Trans. Med. Imag.  20(8), 2001, 792-803.
 * 10. J. Liu, J. K. Udupa, D. Hackney, and G. Moonis, Brain tumor
 *     segmentation in MRI by using the fuzzy connectedness method, Proc. SPIE:
 *     Medical Imaging 4322, 2001, 1455-1465.  
 *
 * \ingroup FuzzyConnectednessSegmentation */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT VectorFuzzyConnectednessImageFilter:
  public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef VectorFuzzyConnectednessImageFilter       Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>   Superclass;
  typedef SmartPointer <Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorFuzzyConnectednessImageFilter,ImageToImageFilter);

  /** Extract the image and vector types from the template parameters. */
  typedef typename TInputImage::PixelType InputPixelType;
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename InputPixelType::VectorType InputVectorType;

  /** Extract the image and vector dimension from the template parameters. */
  itkStaticConstMacro(VectorDimension, unsigned int,
                      InputPixelType::Dimension);
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

  /** Double matrix type */
  typedef   Matrix<double, itkGetStaticConstMacro(VectorDimension), 
                     itkGetStaticConstMacro(VectorDimension)> DoubleMatrixType;

  /** Array of double matrics */
  typedef std::vector<DoubleMatrixType>            DoubleMatrixArrayType;

  /** Vector of unsigned sStrong affinity type*/
  typedef   Vector<unsigned short,itkGetStaticConstMacro(ImageDimension)>  AffinityVector;

  /** Vector of double type */
  typedef   Vector<double, itkGetStaticConstMacro(VectorDimension)> DoubleVectorType;

  /** Array of double vectors */
  typedef std::vector<DoubleVectorType>            DoubleVectorArrayType;

  typedef   Vector<int,2>                          TDVector;

  /** Convenient typedefs. */
  typedef   TInputImage                            InputImageType;
  typedef   TOutputImage                           OutputImageType;
  typedef   Image <unsigned short,itkGetStaticConstMacro(ImageDimension)>  UShortImageType;
  typedef   Image <AffinityVector, itkGetStaticConstMacro(ImageDimension)> AffinityImageType;

  typedef   typename TInputImage::IndexType        IndexType;
  typedef   typename TInputImage::SizeType         SizeType;
  typedef   typename TOutputImage::RegionType      OutRegionType;

  /** List of Seeds type*/
  typedef   std::list<IndexType>                   ListSeedType;

  /** Array of Lists */
  typedef   std::vector<ListSeedType>              ListSeedArrayType;

  typedef   std::vector<TDVector>                  OffsetType;
  
  /** Set/Get the object number be segmented in the input image. */
  itkSetMacro(NumberOfObjects, int);
  itkGetMacro(NumberOfObjects, int);

  /** Setting the covariance matrix for specified object: */
  void SetHomogeneityMatrix(const DoubleMatrixType homo_max);

  /** Setting the covariance matrix for specified object: */
  void SetObjectsMatrix(const DoubleMatrixType object_max,const int object_num);

  /** Setting the seed points for specified object. */
  void SetObjectsSeed( const IndexType &seed, const int object_num);

  /** Setting the seed points for specified object. */
  void SetObjectsMean(const DoubleVectorType, const int object_num);

  /** Setting the flag to suppress background. */
  void SetSuppressBckgFlag(const int flag)
  {
    m_SuppressBckgFlag = flag;
  }

  /** Setting the threshold of strength of fuzzy connectedness 
  *   if user selects absolute FC algorighm
  */
  void SetThreshold(const float threshold)
  {
    m_Threshold = threshold;
  }

  /** Allocate the variate in terms of the number of Objects */
  void Initialization();

protected:
  VectorFuzzyConnectednessImageFilter();
  ~VectorFuzzyConnectednessImageFilter();

  virtual void PrintSelf(std::ostream& os, Indent indent) const;

  /** Standard pipeline method. */
  void GenerateData();

private:
  SizeType                       m_Size;
  std::vector<OffsetType>        m_CirclePointsLoc;
  std::vector<int>               m_CirclePointsNum;

  InputPixelType                 m_Mean;
  int                            m_SuppressBckgFlag;
  float                          m_Threshold;

  DoubleMatrixType               m_HomoCovariance;

  int                            m_NumberOfObjects;
  DoubleMatrixArrayType          m_ObjectCovariances;
  DoubleVectorArrayType          m_ObjectMeans;
  ListSeedArrayType              m_ObjectSeeds;

  std::vector<char>              m_ScaleArray;

  typename InputImageType::ConstPointer   m_InputImage;
  typename InputImageType::Pointer        m_FilterImage;
  typename OutputImageType::Pointer       m_SegmentObject;
  typename AffinityImageType::Pointer     m_AffinityImage;
  typename UShortImageType::Pointer       m_FuzzyConnImage;

  void ScalePrepare();
  void Compute_Scale();
  void Compute_Filter();
  void Compute_Affinity(const int);
  double FuzzyAffinity(const InputVectorType , const InputVectorType, const int);
  void Fast_Tracking(const int);
  
private:
  VectorFuzzyConnectednessImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented  
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorFuzzyConnectednessImageFilter.txx"
#endif

#endif
