#ifndef __itkRGBGibbsPriorFilter_h
#define __itkRGBGibbsPriorFilter_h

//#include "itkObject.h"
#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"

#include "itkImageToImageFilter.h"
#include "itkSupervisedClassifier.h"
#include "itkMRFImageFilter.h"


namespace itk
{

/** \class RGBGibbsPriorFilter
 * \brief 
 *
 * RGBGibbsPriorFilter is a class that apply Gibbs Prior model into 
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
class ITK_EXPORT RGBGibbsPriorFilter : public MRFImageFilter<TInputImage, 
  TClassifiedImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RGBGibbsPriorFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef MRFImageFilter<TInputImage, TClassifiedImage> Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RGBGibbsPriorFilter,MRFImageFilter);

  typedef typename TInputImage::Pointer              InputImageType;  

  /**
   * Type definition for the input image pixel type.
   */
  typedef typename TInputImage::PixelType            InputPixelType;

  /**
   * Type definitions for the training image.
   */
  typedef typename TClassifiedImage::Pointer         TrainingImageType;

  /**
   * Type definitions for the training image pixel type.
   */
//  typedef typename TClassifiedImage::PixelType       TrainingPixelType;

  /**
   * Type definitions for the labelled image.
   * It is derived from the training image.
   */
  typedef typename TClassifiedImage::Pointer         LabelledImageType;
      
  /**
   * Type definitions for the classified image pixel type.
   * It has to be the same type as the training image.
   */
//  typedef typename TClassifiedImage::PixelType       LabelledPixelType;

  /**
   * Type definition for the classified image index type.
   */
  typedef typename TClassifiedImage::IndexType       LabelledImageIndexType;

  /**
   * Type definitions for classifier to be used for the MRF lavbelling.
   */
  typedef Classifier<TInputImage,TClassifiedImage> ClassifierType;

  /**
   * Pointer to the classifier to be used for the MRF lavbelling.
   */
//  typename ClassifierType::Pointer m_ClassifierPtr;

//  typedef typename TInputImage::PixelType   InputImagePixelType;
//  typedef typename TClassifiedImage::PixelType  TrainingImagePixelType;
//  typedef typename TClassifiedImage::PixelType  LabelledImagePixelType;

//  typedef
//    ImageRegionIteratorWithIndex< TInputImage > InputImageIterator;

//  typedef
//    ImageRegionIteratorWithIndex< TClassifiedImage > LabelledImageIterator;


  typedef typename TInputImage::PixelType    InputImageVecType;

  /**
   * Set the image required for training type classifiers
   */
  void SetTrainingImage(TrainingImageType image);

  /** 
   * Set the labelled image. 
   */
  void SetLabelledImage(LabelledImageType LabelledImage);

  /** 
   * Get the labelled image. 
   */

  LabelledImageType GetLabelledImage()
  {
    return m_LabelledImage;
  }

  /**
   * Set the pointer to the classifer being used.
   */
  void SetClassifier( typename ClassifierType::Pointer ptrToClassifier );

  /**
   * Set the Number of class macro
   */
  itkSetMacro(NumberOfClasses, unsigned int);

  /**
   * Get the Number of class macro
   */
  itkGetMacro(NumberOfClasses, unsigned int);

  /**
   * Set the number of iteration of the Iterated Conditional Mode
   * (ICM) algorithm. A default value is set at 50 iterations.
   */
  itkSetMacro(MaximumNumberOfIterations, unsigned int);

  /**
   * Set the number of iteration of the Iterated Conditional Mode
   * (ICM) algorithm.
   */
  itkGetMacro(MaximumNumberOfIterations, unsigned int);

  /**
   * Set the error tollerance level which is used as a threshold
   * to quit the iterations
   */
//  itkSetMacro(ErrorTollerance, double);

  /**
   * Get the error tollerance level which is used as a threshold
   * to quit the iterations
   */
//  itkGetMacro(ErrorTollerance, double);

/**
 *  Threshold of the object size
 */
  itkSetMacro(ClusterSize, unsigned int);

/**
 *  The label of object region
 */  
  itkSetMacro(ObjectLabel, unsigned int);

  enum {ImageDimension = TInputImage::ImageDimension };

  void SetStartPoint (int x, int y, int z); 

  itkSetMacro(BoundaryGradient, unsigned int);

  void Advance();

  typedef vnl_matrix<double> MatrixType; 

protected:
  RGBGibbsPriorFilter();
  ~RGBGibbsPriorFilter() {};
  RGBGibbsPriorFilter(const Self&) {};
  void operator=(const Self&) {};

  void Allocate();

  virtual void MinimizeFunctional();
  virtual void GenerateData();
  virtual void ApplyGibbsLabeller();
  virtual void ApplyGPImageFilter();

//  void Allocate();

private:
  typedef typename TInputImage::SizeType InputImageSizeType;
  typename ClassifierType::Pointer m_ClassifierPtr;

  InputImageType    m_InputImage;
  InputImageType    m_MediumImage;
  TrainingImageType   m_TrainingImage;
  LabelledImageType   m_LabelledImage;

  float m_BoundaryWt; 
  float m_GibbsPriorWt; 
  int m_StartRadius;
  float m_NewRegionThreshold;
  int m_Temp;

//  Parameter definitions
   
  int m_StartPoint[3]; 
  int m_StartModelSize; 
  int m_GibbsNeighborsThreshold; 
  int m_BoundaryGradient;
  int m_RecursiveNum;
  unsigned int      *m_LabelStatus;

  int                   m_imgWidth;
  int                   m_imgHeight;
  int                   m_imgDepth;
  int         m_ClusterSize;
  int         m_ObjectLabel;
  int         m_VecDim;
  int         m_NumberOfClasses;
  unsigned int          m_MaximumNumberOfIterations;
  InputPixelType    m_LowPoint;

  unsigned short    *m_Region;
  unsigned short    *m_RegionCount;

  void  GibbsTotalEnergy(int i);
  float GibbsEnergy(int i, int k, int k1);
  int Sim(int a, int b);
  int LabelRegion(int i, int l, int change);
  void  RegionEraser();
  void  GenerateMediumImage();

  void  GreyScalarBoundary(LabelledImageIndexType Index3D); 

};

} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRGBGibbsPriorFilter.txx"
#endif
#endif
