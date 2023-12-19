/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkRGBGibbsPriorFilter_h
#define itkRGBGibbsPriorFilter_h

#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"

#include "itkMRFImageFilter.h"

#include <memory> // For unique_ptr.

namespace itk
{
/**
 * \class RGBGibbsPriorFilter
 * \brief The RGBGibbsPriorFilter applies Gibbs Prior model for the segmentation
 * of MRF images.
 *
 * The core of the method is based on the minimization of a Gibbsian energy function.
 * This energy function f can be divided into three part:
 *
 *  \f[
 *    f = f_1 + f_2 + f_3
 *  \f]
 *
 * where
 * \f$f_1\f$ is related to the object homogeneity,
 * \f$f_2\f$ is related to the boundary smoothness,
 * \f$f_3\f$ is related to the constraint of the observation (or the noise model).
 *
 * The two force components \f$f_1\f$ and \f$f_3\f$ are minimized by the GradientEnergy
 * method while \f$f_2\f$ is minimized by the GibbsTotalEnergy method.
 *
 * This filter only works with 3D images.
 *
 * \ingroup MRFFilters
 * \ingroup ITKMarkovRandomFieldsClassifiers
 */
template <typename TInputImage, typename TClassifiedImage>
class ITK_TEMPLATE_EXPORT RGBGibbsPriorFilter : public MRFImageFilter<TInputImage, TClassifiedImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(RGBGibbsPriorFilter);

  /** Standard "Self" type alias. */
  using Self = RGBGibbsPriorFilter;
  using Superclass = MRFImageFilter<TInputImage, TClassifiedImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkOverrideGetNameOfClassMacro(RGBGibbsPriorFilter);

  /** Types from superclass.  */
  using typename Superclass::InputImagePixelType;
  using typename Superclass::InputImageRegionConstIterator;
  using typename Superclass::InputImageRegionIterator;
  using typename Superclass::LabelledImageRegionIterator;
  using typename Superclass::LabelledImagePixelType;
  using typename Superclass::IndexValueType;

  /** A smart pointer to the input image type. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename TInputImage::Pointer;
  using InputImageConstPointer = typename TInputImage::ConstPointer;

  /** Type definition for the input image pixel type. */
  using InputPixelType = typename TInputImage::PixelType;

  /** Type definitions for the training image. */
  using ClassifiedImageType = TClassifiedImage;
  using TrainingImageType = typename TClassifiedImage::Pointer;

  /** Type definitions for the labelled image.
   * Derived from the training image. */
  using LabelledImageType = typename TClassifiedImage::Pointer;

  /** Type definition for the classified image index type. */
  using LabelledImageIndexType = typename TClassifiedImage::IndexType;

  /** Type used as identifier for the labels.
   * \warning -1 cannot be used as the identifier for unlabeled pixels
   * the NumericTraits<>::max() value is used for indicating unlabeled pixels */
  using LabelType = unsigned int;

  /** Type definitions for classifier to be used for the MRF labeling. */
  using ClassifierType = ImageClassifierBase<TInputImage, TClassifiedImage>;

  /** Input pixel type. */
  using InputImageVecType = typename TInputImage::PixelType;
  using IndexType = typename TInputImage::IndexType;

  /** Set/Get the image required for training type classifiers. */
  itkSetMacro(TrainingImage, TrainingImageType);
  itkGetConstMacro(TrainingImage, TrainingImageType);

  /** Set the labelled image. */
  void
  SetLabelledImage(LabelledImageType image);

  /** Get the labelled image. */
  LabelledImageType
  GetLabelledImage()
  {
    return m_LabelledImage;
  }

  /** Set the classifier. */
  void
  SetClassifier(typename ClassifierType::Pointer ptrToClassifier);

  /** Set the number of classes. */
  void
  SetNumberOfClasses(const unsigned int numberOfClasses) override
  {
    itkDebugMacro("setting NumberOfClasses to " << numberOfClasses);
    if (this->m_NumberOfClasses != numberOfClasses)
    {
      this->m_NumberOfClasses = numberOfClasses;
      this->Modified();
    }
  }

  /** Get the number of classes. */
  unsigned int
  GetNumberOfClasses() const override
  {
    return this->m_NumberOfClasses;
  }

  /** Set/Get the number of iteration of the Iterated Conditional Mode
   * (ICM) algorithm. A default value is set at 50 iterations. */
  void
  SetMaximumNumberOfIterations(const unsigned int numberOfIterations) override
  {
    itkDebugMacro("setting MaximumNumberOfIterations to " << numberOfIterations);
    if (this->m_MaximumNumberOfIterations != numberOfIterations)
    {
      this->m_MaximumNumberOfIterations = numberOfIterations;
      this->Modified();
    }
  }

  /** Get the number of iterations of the Iterated Conditional Mode
   * (ICM) algorithm. */
  unsigned int
  GetMaximumNumberOfIterations() const override
  {
    return this->m_MaximumNumberOfIterations;
  }

  /** Set/Get the threshold for the object size. */
  itkSetMacro(ClusterSize, unsigned int);
  itkGetConstMacro(ClusterSize, unsigned int);

  /** Set/Get the label for the object region. */
  itkSetMacro(ObjectLabel, LabelType);
  itkGetConstMacro(ObjectLabel, LabelType);

  /** Extract the input image dimension. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  itkSetMacro(StartPoint, IndexType);
  itkGetConstMacro(StartPoint, IndexType);

  itkSetMacro(BoundaryGradient, unsigned int);
  itkGetConstMacro(BoundaryGradient, unsigned int);

  itkSetMacro(ObjectThreshold, double);
  itkGetConstMacro(ObjectThreshold, double);

  /** Set/Get the value for clique weights. */
  itkSetMacro(CliqueWeight_1, double);
  itkGetConstMacro(CliqueWeight_1, double);
  itkSetMacro(CliqueWeight_2, double);
  itkGetConstMacro(CliqueWeight_2, double);
  itkSetMacro(CliqueWeight_3, double);
  itkGetConstMacro(CliqueWeight_3, double);
  itkSetMacro(CliqueWeight_4, double);
  itkGetConstMacro(CliqueWeight_4, double);
  itkSetMacro(CliqueWeight_5, double);
  itkGetConstMacro(CliqueWeight_5, double);
  itkSetMacro(CliqueWeight_6, double);
  itkGetConstMacro(CliqueWeight_6, double);

  /** Type of matrix to use. */
  using MatrixType = vnl_matrix<double>;

protected:
  RGBGibbsPriorFilter();
  ~RGBGibbsPriorFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Allocate memory for classified image pixel status. */
  void
  Allocate();

  void
  MinimizeFunctional() override;

  void
  GenerateData() override;

  virtual void
  ApplyGibbsLabeller();

  virtual void
  ApplyGPImageFilter();

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(
    SameDimension,
    (Concept::SameDimension<Self::InputImageType::ImageDimension, Self::ClassifiedImageType::ImageDimension>));
  itkConceptMacro(DimensionShouldBe3, (Concept::SameDimension<Self::InputImageType::ImageDimension, 3>));
  // End concept checking
#endif

private:
  using InputImageSizeType = typename TInputImage::SizeType;

  /** Input. */
  InputImageConstPointer m_InputImage{};
  /** Image to train the filter. */
  TrainingImageType m_TrainingImage{};
  /** Output. */
  LabelledImageType m_LabelledImage{};
  /** Number of classes that need to be classified. */
  unsigned int m_NumberOfClasses{ 0 };
  /** Maximum number of iterations. */
  unsigned int m_MaximumNumberOfIterations{ 10 };

  typename ClassifierType::Pointer m_ClassifierPtr{};

  /** Threshold for the existence of a boundary. */
  unsigned int m_BoundaryGradient{ 7 };
  /** Weight for \f$H_1\f$. */
  double m_BoundaryWeight{ 1 };
  /** Weight for \f$H_2\f$. */
  double m_GibbsPriorWeight{ 1 };
  /** Start region of the object. */
  int m_StartRadius{ 10 };
  /** Number of SA iterations. */
  int m_RecursiveNumber{ 0 };
  /** Array to store the state of each pixel. */
  std::unique_ptr<LabelType[]> m_LabelStatus{ nullptr };

  /** Intermediate result image. */
  InputImagePointer m_MediumImage{};

  /** Used by the SA algorithm. */
  unsigned int m_Temp{ 0 };
  /** Seed. */
  IndexType m_StartPoint{};

  /** Image width. */
  unsigned int m_ImageWidth{ 0 };
  /** Image height. */
  unsigned int m_ImageHeight{ 0 };
  /** Image depth. */
  unsigned int m_ImageDepth{ 0 };
  /** Region sizes smaller than this threshold value will be erased. */
  unsigned int m_ClusterSize{ 10 };
  /** Label for object region. */
  LabelType m_ObjectLabel{ 1 };
  /** Number of channels in the image. */
  unsigned int m_VecDim{ 0 };
  /** Point giving lowest value of \f$H_1\f$ in neighborhood. */
  InputPixelType m_LowPoint{};

  /** Used for erasing regions. */
  std::unique_ptr<unsigned short[]> m_Region{ nullptr };
  /** Used for erasing regions. */
  std::unique_ptr<unsigned short[]> m_RegionCount{ nullptr };

  // Weights for different clique configurations.

  /** Weight for cliques that v/h smooth boundary. */
  double m_CliqueWeight_1{ 0.0 };
  /** Weight for clique that has an intermediate smooth boundary. */
  double m_CliqueWeight_2{ 0.0 };
  /** Weight for clique that has a diagonal smooth boundary. */
  double m_CliqueWeight_3{ 0.0 };
  /** Weight for clique consists only object pixels. */
  double m_CliqueWeight_4{ 0.0 };
  /** Weight for clique consists only background pixels. */
  double m_CliqueWeight_5{ 0.0 };
  /** Weight for other cliques. */
  double m_CliqueWeight_6{ 0.0 };

  /** Minimize the local characteristic \f$f_2\f$ term in the energy function.
   * Calculates \f$H_2\f$. */
  void
  GibbsTotalEnergy(int i);

  /** Calculate the energy in each cluster. */
  double
  GibbsEnergy(unsigned int i, unsigned int k, unsigned int k1);

  /** Check if the values are identical. */
  int
  Sim(int a, int b);

  /** Label a region. */
  unsigned int
  LabelRegion(int i, int l, int change);

  /** Erase small regions.
   * Removes the tiny bias inside the object region. */
  void
  RegionEraser();

  /** Create the intermediate image. */
  void
  GenerateMediumImage();

  /** Smooth the image in piecewise fashion.
   * Calculates \f$H_1\f$. */
  void
  GreyScalarBoundary(LabelledImageIndexType Index3D);

  double m_ObjectThreshold{ 5.0 };
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRGBGibbsPriorFilter.hxx"
#endif
#endif
