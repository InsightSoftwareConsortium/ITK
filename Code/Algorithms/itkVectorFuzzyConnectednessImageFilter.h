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
#include "itkVector.h"
#include "itkMatrix.h"


#include <vector>
#include <list>


namespace itk{

/** \class VectorFuzzyConnectednessImageFilter
 * 
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
  enum {VectorDimension = TInputImage::PixelType::VectorType::VectorDimension};

  
  typedef   itk::Matrix<double,VectorDimension,VectorDimension>               MatrixType;
  typedef   itk::Vector<int,VectorDimension>                    VDVector;
  typedef   itk::Vector<int,ImageDimension>                     IDVector;


  typedef   TInputImage                           InputImageType;
  typedef   TOutputImage                          OutputImageType;
  typedef   Image <unsigned short,ImageDimension> UShortImage;
  typedef   typename TInputImage::IndexType       IndexType;
  typedef   typename TInputImage::SizeType        SizeType;
  typedef   typename TOutputImage::RegionType     RegionType;

  typedef   std::list<IndexType>                  ListType;
  typedef   std::vector<IDVector>                 OffsetType;
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
  void SetObjectsMean(const VDVector, const int object_num);

  /**
   * Allocate the variate in terms of the number of Objects
   */
  void Initialization();
 

protected:
  VectorFuzzyConnectednessImageFilter();
  ~VectorFuzzyConnectednessImageFilter();

  /**
   * Standard pipeline method.
   */
  void GenerateData();

private:

  SizeType                       m_Size;
  OffsetType                     *m_SpherePointsLoc;
  int                            *m_SpherePointsNum;

  double                         m_Mask[3][3];
  double                         m_MaskTotal;
  VDVector                       m_HomoMaxDiff;
  VDVector                       m_FeaturesThreshold;
  VDVector                       m_PowerValue;
  
  int                            m_Objects;
  int                            m_SelectedObject;

  MatrixType                     *m_ObjectsCovMatrix;
  VDVector                       *m_ObjectsMean;

  VDVector                       *m_ObjectsMaxDiff;
  FloatType                      *m_ObjectsMap;
  ListType                       *m_ObjectsSeed;


  std::vector<float>             m_HomogeneityMap;
  std::vector<float>             m_ScaleMap;
  std::vector<char>              m_ScaleArray;
  std::vector<double>            m_Material;


  typename InputImageType::Pointer   m_InputImage;
  typename InputImageType::Pointer   m_FilterImage;
  typename UShortImage::Pointer      m_ObjectFuzzyScene;
  typename UShortImage::Pointer      m_BackgroundFuzzyScene;
  typename OutputImageType::Pointer  m_SegmentObject; 


  std::vector<unsigned short>  m_Xaffinity;
  std::vector<unsigned short>  m_Yaffinity;
  std::vector<unsigned short>  m_Zaffinity;


  void ScalePrepare();
  void Compute_LookupTable();
  void Compute_Scale();
  void Compute_Filter();
  void Compute_Affinity();
  void FastTracking(int);
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorFuzzyConnectednessImageFilter.txx"
#endif

#endif
