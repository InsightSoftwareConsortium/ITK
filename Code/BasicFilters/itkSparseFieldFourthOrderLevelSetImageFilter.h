#ifndef _itkSparseFieldFourthOrderLevelSetImageFilter_h_
#define _itkSparseFieldFourthOrderLevelSetImageFilter_h_

#include "itkSparseImage.h"
#include "itkNormalVectorDiffusionFunction.h"
#include "itkImplicitManifoldNormalVectorFilter.h"
#include "itkLevelSetFunctionWithRefitTerm.h"
#include "itkSparseFieldLevelSetImageFilter.h"
#include <math.h>

namespace itk {

/**
 * \class NormalBandNode
 *
 * \brief This is a data storage class that can is used as the node type for
 * the SparseImage class.
 *
 */
template <class TImageType>
class NormalBandNode
{
public:
  /** The scalar image type. */
  typedef TImageType LevelSetImageType;

  /** The pixel type of the scalar image. Expected to be float or double. */
  typedef typename LevelSetImageType::PixelType  NodeValueType;

  /** The index type for the scalar image. */
  typedef typename LevelSetImageType::IndexType  IndexType;

  /** The definition for the normal vector type of the scalar image. */
  typedef Vector <NodeValueType,
                  ::itk::GetImageDimension<TImageType>::ImageDimension>
  NodeDataType;

  /** Container for output data (normal vectors). */
  NodeDataType m_Data;

  /** Container for a copy of normal vectors before processing. */
  NodeDataType m_InputData;

  /** Container for storing update vectors. */
  NodeDataType m_Update;

  /** Container for the manifold normal vector. These are computed once at
      initialization and later used for computing intrinsic derivatives. */
  NodeDataType
  m_ManifoldNormal[::itk::GetImageDimension<TImageType>::ImageDimension];

  /** Intermediate flux computations used in computing the update. */
  NodeDataType m_Flux [::itk::GetImageDimension<TImageType>::ImageDimension];

  /** Curvature computed from the output normal vectors. Used by
      LevelSetFunctionWithRefitTerm for its propagation term. */
  NodeValueType m_Curvature;

  /** This flag is true if the curvature value at this node is valid, false
      otherwise. */
  bool          m_CurvatureFlag;

  /** The position of this node in the sparse image. */
  IndexType m_Index;  

  /** Pointers to previous and next nodes in the list. */
  NormalBandNode *Next;  
  NormalBandNode *Previous;
};

/**
 * \class SparseFieldFourthOrderLevelSetImageFilter
 *
 * \brief This class implements the fourth order level set PDE framework.
 *
 * \par
 * Halt function needs to be defined by subclass.
 * This class defines a ProcessNormals method which uses the
 * ImplicitManifoldNormalDiffusionFilter class to generate a SparseImage of
 * filtered normal vectors. We make a copy of the current state of the output
 * image (also referred to as level set image) for this class and pass it to
 * ImplicitManifoldNormalDiffusionFilter. That class computes the normal
 * vectors to the level set image and filters them. The output is in the form
 * of a sparse image templated with the NormalBandNode type.
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT SparseFieldFourthOrderLevelSetImageFilter
  : public SparseFieldLevelSetImageFilter <TInputImage, TOutputImage>
{
  public:
  /** Standard class typedefs */
  typedef SparseFieldFourthOrderLevelSetImageFilter Self;
  typedef SparseFieldLevelSetImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Standard image dimension macro. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);

  /** Typedefs derived from the superclass. */
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename Superclass::ValueType ValueType;
  typedef typename Superclass::IndexType IndexType;
  typedef typename Superclass::LayerType LayerType;

  /** The storage class used as the node type for the sparse normal vector
      image. */
  typedef NormalBandNode <OutputImageType> NodeType;
  
  /** The sparse image type used for processing the normal vectors. */
  typedef SparseImage <NodeType,
                       itkGetStaticConstMacro(ImageDimension)> SparseImageType;

  /** The normal vector type. */
  typedef typename NodeType::NodeDataType NormalVectorType;

  /** The iterator type for the sparse image. */
  typedef NeighborhoodIterator <SparseImageType> SparseImageIteratorType;

  /** The filter type for processing the normal vectors of the level set. */
  typedef ImplicitManifoldNormalVectorFilter <OutputImageType, SparseImageType>
  NormalVectorFilterType;

  /** The function type for processing the normal vector neigborhood. */
  typedef NormalVectorDiffusionFunction <SparseImageType>
  NormalVectorFunctionType;

  /** The radius type derived from the normal vector function. */
  //typedef typename NormalVectorFunctionType::RadiusType RadiusType;

  /** The level set function with refitting term type. */
  typedef LevelSetFunctionWithRefitTerm <OutputImageType,
                                         SparseImageType> LevelSetFunctionType; 
  
  itkGetMacro (MaxRefitIteration,int);
  itkSetMacro (MaxRefitIteration,int);
  itkGetMacro (MaxNormalIteration,int);
  itkSetMacro (MaxNormalIteration,int);
  itkGetMacro (CurvatureBandWidth,ValueType);
  itkSetMacro (CurvatureBandWidth,ValueType);
  itkGetMacro (RMSChangeNormalProcessTrigger, ValueType);
  itkSetMacro (RMSChangeNormalProcessTrigger, ValueType);
  itkGetMacro (NormalProcessType, int);
  itkSetMacro (NormalProcessType, int);
  itkGetMacro (NormalProcessConductance, ValueType);
  itkSetMacro (NormalProcessConductance, ValueType);
  itkSetMacro (NormalProcessUnsharpFlag, bool);
  itkGetMacro (NormalProcessUnsharpFlag, bool);
  itkSetMacro (NormalProcessUnsharpWeight, ValueType);
  itkGetMacro (NormalProcessUnsharpWeight, ValueType);

  /** Set the level set function. Must LevelSetFunctionWithRefitTerm or a
      subclass. */
  void SetLevelSetFunction (LevelSetFunctionType *lsf);

  /** Compute the number of layers that must be used in
      SparseFieldLevelSetImageFilter to accomodate the desired normal
      processing band. */
  unsigned int GetMinimumNumberOfLayers () const
  {
    return (int)ceil(m_CurvatureBandWidth+
                     itkGetStaticConstMacro(ImageDimension));
  }

  void SetNumberOfLayers (unsigned int n)
  {
    unsigned int nm = vnl_math_max (this->GetMinimumNumberOfLayers (), n );
    if (nm != this->GetNumberOfLayers())
      {
      Superclass::SetNumberOfLayers (nm);
      this->Modified();
      }
  }
  
private:
  /** This is a iteration counter that gets reset to 0 every time
      ProcessNormals method is called. */
  int m_RefitIteration;

  /** This parameter determines the maximum number of
      SparseFieldLevelSetImageFilter iterations that will be executed between
      calls to ProcessNormals. */
  int m_MaxRefitIteration;

  /** This parameter is used to set the corresponding parameter in
      ImplicitManifoldNormalDiffusionfFilter. */
  int m_MaxNormalIteration;

  /** This is used to trigger a call to the ProcessNormals method
      before m_RefitIteration reaches m_MaxRefitIteration if the RMSChange falls
      below this parameter. */
  ValueType m_RMSChangeNormalProcessTrigger;

  /** This flag is set to true to signal final convergence. It can be used by
      subclasses that define a Halt method. */
  bool m_ConvergenceFlag;

  /** The level set function with the term for refitting the level set to the
      processed normal vectors. */
  LevelSetFunctionType *m_LevelSetFunction;
  
public:

  /** This method first calls the Superclass InitializeIteration method. Then
      it determines whether ProcessNormals should be called. */
  virtual void InitializeIteration()
  {
    Superclass::InitializeIteration();
    ValueType rmschange = this->GetRMSChange();

    // debugging line -- will remove later
    //std::cout<<" , rmschange = "<<rmschange<<" ("<<(this->GetElapsedIterations())<<")\n";
    
    if ( ( this->GetElapsedIterations()==0 ) ||
         ( m_RefitIteration == m_MaxRefitIteration ) ||
         ( rmschange <= m_RMSChangeNormalProcessTrigger ) ||
         ( this->ActiveLayerCheckBand() ) )
      {
      if ( ( this->GetElapsedIterations() != 0 ) &&
           ( rmschange <= m_RMSChangeNormalProcessTrigger ) &&
           ( m_RefitIteration <= 1) )
        {
        m_ConvergenceFlag = true;
        }

      m_RefitIteration = 0;
      ProcessNormals();
      }
    
    m_RefitIteration++;
  }

protected:
  SparseFieldFourthOrderLevelSetImageFilter ();
  ~SparseFieldFourthOrderLevelSetImageFilter () {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** This method computes curvature from normal vectors stored in a sparse
      image neighborhood. */
  ValueType ComputeCurvatureFromSparseImageNeighborhood
  (SparseImageIteratorType &neighborhood) const;

  /** This method computes curvature from the processed normal vectors over
   *  the region specified by the CurvatureBandWidth parameter. The
   *  curvatures are stored in the sparse image. */
  void ComputeCurvatureTarget (const OutputImageType *distanceImage,
                               SparseImageType *sparseImage) const;

  /** The method for processing the normal vectors. */
  void ProcessNormals();

  /** This method checks whether the level set front is touching the edges of
   * the band where curvature from the processed normal vectors has been
   * computed. This is one of the conditions for triggering the ProcessNormals
   * method. */
  bool ActiveLayerCheckBand() const;
  
private:
  /** This parameter determines the width of the band where we compute
   * curvature from the processed normals. The wider the band, the more level set
   * iterations that can be performed between calls to ProcessNormals. It is
   * qsuggested that this value is left at its default. */
  ValueType m_CurvatureBandWidth;

  /** The parameter that chooses between isotropic/anisotropic filtering of the
      normal vectors. */
  int m_NormalProcessType;

  /** The conductance parameter used if anisotropic filtering of the normal
      vectors is chosen. */
  ValueType m_NormalProcessConductance;

  /** The parameter that turns on/off the unsharp mask enhancement of the
      normals. */
  bool m_NormalProcessUnsharpFlag;

  /** The weight that controls the extent of enhancement if the
      NormalProcessUnsharpFlag is ON. */
  ValueType m_NormalProcessUnsharpWeight;
  
  /** Constants used in the computations. */
  static const unsigned long m_NumVertex;
  static const ValueType m_DimConst;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSparseFieldFourthOrderLevelSetImageFilter.txx"
#endif

#endif
