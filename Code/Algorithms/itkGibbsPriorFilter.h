/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGibbsPriorFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkGibbsPriorFilter_h
#define __itkGibbsPriorFilter_h

//#include "itkObject.h"
#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"

#include "itkImageToImageFilter.h"
//#include "itkSupervisedClassifier.h"
#include "itkMRFImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"


namespace itk
{

/** \class GibbsPriorFilter
 * \brief 
 *
 * GibbsPriorFilter is a class that apply Gibbs Prior model into 
 * segmentation of MRF images. The core of the method is to minimize
 * a Gibbsian form energy function.
 * The function can be divided into three part: f = f_1 + f_2 + f_3;
 * f_1 for the object homogeneity, f_2 for the boundary smoothness,
 * f_3 is the noise model. f_1 and f_3 is minimized in the function
 * GradientEnergy and f_2 is minized in the function GibbsTotalEnergy
 *
 * \ingroup MRFFilters 
 */
template <class TInputImage, class TClassifiedImage>
class ITK_EXPORT GibbsPriorFilter : public MRFImageFilter<TInputImage, 
  TClassifiedImage>
{
public:
  /** Standard class typedefs. */
  typedef GibbsPriorFilter  Self;
  typedef MRFImageFilter<TInputImage, TClassifiedImage> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(GibbsPriorFilter,MRFImageFilter);

  /** The input image type. */
  typedef typename TInputImage::Pointer              InputImageType;  

  /** Type definition for the input image pixel type. */
  typedef typename TInputImage::PixelType            InputPixelType;

  /** Type definitions for the training image. */
  typedef typename TClassifiedImage::Pointer         TrainingImageType;

  /** Type definitions for the training image pixel type. */
  typedef typename TClassifiedImage::PixelType       TrainingPixelType;

  /** Type definitions for the labelled image.
   * It is derived from the training image. */
  typedef typename TClassifiedImage::Pointer         LabelledImageType;
      
  /** Type definitions for the classified image pixel type.
   * It has to be the same type as the training image. */
  typedef typename TClassifiedImage::PixelType       LabelledPixelType;

  /** Type definition for the classified image index type. */
  typedef typename TClassifiedImage::IndexType       LabelledImageIndexType;

  /** Type definitions for classifier to be used for the MRF lavbelling. */
  typedef typename Superclass::ClassifierType ClassifierType;
//  typedef Classifier<TInputImage,TClassifiedImage> ClassifierType;

  /** Pointer to the classifier to be used for the MRF lavbelling. */
  typename ClassifierType::Pointer m_ClassifierPtr;
  typedef typename TInputImage::PixelType      InputImagePixelType;
  typedef typename TClassifiedImage::PixelType TrainingImagePixelType;
  typedef typename TClassifiedImage::PixelType LabelledImagePixelType;

  /** Convenient typedefs. */
  typedef ImageRegionIteratorWithIndex< TInputImage > InputImageIterator;
  typedef ImageRegionIteratorWithIndex<TClassifiedImage> LabelledImageIterator;
  typedef typename TInputImage::PixelType    InputImageVectorType;

  /** Set the image required for training type classifiers */
  void SetTrainingImage(TrainingImageType image);

  /** Set the labelled image.  */
  void SetLabelledImage(LabelledImageType LabelledImage);

  /** Get the labelled image.  */
  LabelledImageType GetLabelledImage()
    { return m_LabelledImage; }

  /** Set the pointer to the classifer being used. */
  void SetClassifier( typename ClassifierType::Pointer ptrToClassifier );

  /** Set/Get the number of classes. */
  itkSetMacro(NumberOfClasses, unsigned int);
  itkGetMacro(NumberOfClasses, unsigned int);

  /** Set/Get the maximum number of iterations of the Iterated Conditional
   * Mode (ICM) algorithm. A default value is set at 50 iterations. */
  itkSetMacro(MaximumNumberOfIterations, unsigned int);
  itkGetMacro(MaximumNumberOfIterations, unsigned int);

  /** Set/Get the error tolerance level which is used as a threshold
   * to quit the iterations. */
  itkSetMacro(ErrorTolerance, double);
  itkGetMacro(ErrorTolerance, double);

  /** Threshold of the object size. */
  itkSetMacro(ClusterSize, unsigned int);

  /** The label of object region. */  
  itkSetMacro(ObjectLabel, unsigned int);

  /** The image dimension. */
  enum {ImageDimension = TInputImage::ImageDimension };

  /** Methods to control the algorithm. */
  void SetStartPoint (int x, int y, int z); 
  void SetBoundaryGradient(int a);
  void Advance();

protected:
  GibbsPriorFilter();
  ~GibbsPriorFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  virtual void MinimizeFunctional();
  virtual void GenerateData();
  void ApplyGibbsLabeller();
  virtual void ApplyMRFImageFilter();
  void Allocate();

  InputImageType         m_InputImage;
  TrainingImageType      m_TrainingImage;
  LabelledImageType      m_LabelledImage;

  float m_BoundaryWeight; 
  float m_GibbsPriorWeight; 
  int m_StartRadius;

  int m_StartPoint[3]; 
  int m_StartModelSize; 
  int m_GibbsNeighborsThreshold; 
  int m_BoundaryGradient;
  int m_RecursiveNum;

  unsigned int          m_NumberOfClasses;
  unsigned int          m_MaximumNumberOfIterations;
  unsigned int          *m_LabelStatus;
  
  double                m_ErrorTolerance;
  double                *m_ClassProbability; //Class liklihood

  int                   m_ErrorCounter;
  int                   m_ImageWidth;
  int                   m_ImageHeight;
  int                   m_ImageDepth;
  int                   m_ClusterSize;
  int                   m_ObjectLabel;

  unsigned short    *m_Region;
  unsigned short    *m_RegionCount;

  /** function defintions */
  void  GenerateInputRequestedRegion();
  void  EnlargeOutputRequestedRegion( DataObject * );
  void  GenerateOutputInformation();

  void  GibbsTotalEnergy(int i);
  int   GreyScalarBoundary(LabelledImageIndexType Index3D); 
  float GibbsEnergy(int i, int k, int k1);
  int Sim(int a, int b);
  int LabelRegion(int i, int l, int change);
  void  RegionEraser();

private:
  GibbsPriorFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  typedef typename TInputImage::SizeType InputImageSizeType;
};

} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGibbsPriorFilter.txx"
#endif
#endif
