#ifndef __itkGibbsPriorFilter_h
#define __itkGibbsPriorFilter_h

//#include "itkObject.h"
#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"

#include "itkImageToImageFilter.h"
#include "itkSupervisedClassifier.h"
#include "itkMRFImageFilter.h"


namespace itk
{

/** \class MRFImageFilter
 * \brief 
 *
 * MRFImageFilter is the base class for all process objects that output
 * mesh data, and require mesh data as input. Specifically, this class
 * defines the SetInput() method for defining the input to a filter.
 */
template <class TInputImage, class TClassifiedImage>
class ITK_EXPORT GibbsPriorFilter : public MRFImageFilter<TInputImage, TClassifiedImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef GibbsPriorFilter  Self;

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
  itkTypeMacro(GibbsPriorFilter,MRFImageFilter);

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
  typedef typename TClassifiedImage::PixelType       TrainingPixelType;

  /**
   * Type definitions for the labelled image.
   * It is derived from the training image.
   */
  typedef typename TClassifiedImage::Pointer         LabelledImageType;
      
  /**
   * Type definitions for the classified image pixel type.
   * It has to be the same type as the training image.
   */
  typedef typename TClassifiedImage::PixelType       LabelledPixelType;

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
  typename ClassifierType::Pointer m_ClassifierPtr;

  typedef typename TInputImage::PixelType      InputImagePixelType;
  typedef typename TClassifiedImage::PixelType TrainingImagePixelType;
  typedef typename TClassifiedImage::PixelType LabelledImagePixelType;

  typedef
    SimpleImageRegionIterator< TInputImage > InputImageIterator;

  typedef
    SimpleImageRegionIterator< TClassifiedImage > LabelledImageIterator;


  typedef typename TInputImage::PixelType    InputImageVectorType;

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
  itkSetMacro(NumClasses, unsigned int);

  /**
   * Get the Number of class macro
   */
  itkGetMacro(NumClasses, unsigned int);

  /**
   * Set the number of iteration of the Iterated Conditional Mode
   * (ICM) algorithm. A default value is set at 50 iterations.
   */
  itkSetMacro(MaxNumIter, unsigned int);

  /**
   * Set the number of iteration of the Iterated Conditional Mode
   * (ICM) algorithm.
   */
  itkGetMacro(MaxNumIter, unsigned int);

  /**
   * Set the error tollerance level which is used as a threshold
   * to quit the iterations
   */
  itkSetMacro(ErrorTollerance, double);

  /**
   * Get the error tollerance level which is used as a threshold
   * to quit the iterations
   */
  itkGetMacro(ErrorTollerance, double);


  /**
   *  Pointer type for output image
   */
//  typedef typename TClassifiedImage::Pointer OutputImage;
  itkSetMacro(ClusterSize, unsigned int);
  /**
   *  Region
   */
//  typedef typename TClassifiedImage::RegionType RegionType;
   enum {ImageDimension = TInputImage::ImageDimension };

  void SetStartPoint (int x, int y, int z); 
  void SetBoundaryGradient(int a);

protected:
  GibbsPriorFilter();
  ~GibbsPriorFilter() {};
//  GibbsPriorFilter(const GibbsPriorFilter&) {};
//  void operator=(const GibbsPriorFilter&) {};

  virtual void MinimizeFunctional();
  virtual void GenerateData();
  void ApplyGibbsLabeller();
  virtual void ApplyMRFImageFilter();
  void Allocate();

  InputImageType         m_InputImage;
  TrainingImageType      m_TrainingImage;
  LabelledImageType      m_LabelledImage;

  float m_BoundaryWt; 
  float m_GibbsPriorWt; 
  int m_StartRadius;

//  OutputImage m_OutputImage;
	 
  int m_StartPoint[3]; 
  int m_StartModelSize; 
  int m_GibbsNeighborsThreshold; 
  int m_BoundaryGradient;
  int m_RecursiveNum;

  unsigned int           m_NumClasses;
  unsigned int           m_MaxNumIter;
  unsigned int           m_KernelSize;
  unsigned int           *m_LabelStatus;
  
  double                 m_ErrorTollerance;
  double                 *m_ClassProbability; //Class liklihood
  double                 *m_Beta3x3x3;


  int                    m_ErrorCounter;
  int                    *m_Offset;
  int                    m_kWidth;
  int                    m_kHeight;
  int                    m_kDepth;
  int                    m_imgWidth;
  int                    m_imgHeight;
  int                    m_imgDepth;
  int					 m_ClusterSize;

  int                    *m_WidthOffset;
  int                    *m_HeightOffset;
  int                    *m_DepthOffset;

  unsigned short		 *m_Region;
  unsigned short		 *m_RegionCount;

  void  GenerateInputRequestedRegion();
  void  EnlargeOutputRequestedRegion( DataObject * );
  void  GenerateOutputInformation();
  float GradientEnergy (InputImageVectorType x, int n); 
  void  GibbsTotalEnergy(int i);
  int   GreyScalarBoundary(LabelledImageIndexType Index3D); 
  float GibbsEnergy(int i, int k, int k1);
  int	Mini(int i); 
  int	Maxi(int i); 
  int	Sim(int a, int b);
  int	LabelRegion(int i, int l, int change);
  void	RegionEraser();
//  void InitialStat(int dims[3]);

//  void Execute();
private:
  typedef typename TInputImage::SizeType InputImageSizeType;
};

} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGibbsPriorFilter.txx"
#endif
#endif
