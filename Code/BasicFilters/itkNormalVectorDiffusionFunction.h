#ifndef __itkNormalVectorDiffusionFunction_h_
#define __itkNormalVectorDiffusionFunction_h_

#include "itkNormalVectorFunctionBase.h"
#include "itkNumericTraits.h"
#include <math.h>

namespace itk {

template <class TSparseImageType>
class ITK_EXPORT NormalVectorDiffusionFunction 
  :public NormalVectorFunctionBase <TSparseImageType>
{
public:
  /** Standard class typedef. */
  typedef NormalVectorDiffusionFunction Self;
  typedef NormalVectorFunctionBase <TSparseImageType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro( NormalVectorDiffusionFunction, NormalVectorFunctionBase);
   
  /** Image dimension derived from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** Standard New macro. */
  itkNewMacro(Self);

  /** Typedefs from the superclass. */
  typedef typename Superclass::TimeStepType     TimeStepType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::FloatOffsetType  FloatOffsetType;
  typedef typename Superclass::IndexType        IndexType;
  typedef typename Superclass::SparseImageType  SparseImageType;
  typedef typename Superclass::NodeType         NodeType;
  typedef typename Superclass::NodeValueType    NodeValueType;
  typedef typename Superclass::NormalVectorType NormalVectorType;

  /** This method is used to choose between isotropic/anisotropic filtering. A
      parameter value of 0 indicates isotropic diffusion and is the
      default. Parameter value 1 is anisotropic diffusion. When using
      anisotropic diffusion the conductance parameter should also be set. */
  void SetNormalProcessType (int npt)
  { m_NormalProcessType = npt; }

  /** This method returns the isotropic/anisotropic filtering parameter. */
  int GetNormalProcessType () const 
  { return m_NormalProcessType; }

  /** This method sets the conductance parameter used in anisotropic
   * filtering. Useful values for processing 2D and 3D shapes are between
   *  0.1 and 0.25. Lower values preserve more shape features, higher values
   *  smooth more. As the conductance parameter large, the processing becomes
   *  isotropic. Default is 0. */
  void SetConductanceParameter (NodeValueType cp)
  {
    m_ConductanceParameter = cp;
    m_FluxStopConstant = static_cast<NodeValueType> (-1.0/(cp*cp));
  }

  /** This method returns the conductance parameter. */
  NodeValueType GetConductanceParameter () const
  { return m_ConductanceParameter; }

  /** This method returns the internal variable FluxStopConstant. */
  NodeValueType GetFluxStopConstant () const
  { return m_FluxStopConstant; }
  
  /** This function is called from LevelSetNormalImageFilter for all of the
   *  nodes to compute and store the flux vectors (first derivatives of the
   *  normal vectors. ComputeUpdateNormal then takes derivatives of the flux
   *  vectors. This way we avoid repeating the same flux computations. */
  virtual void PrecomputeSparseUpdate (NeighborhoodType &it) const;

  /** The actual update rule for the normal vectors. */
  virtual NormalVectorType ComputeSparseUpdate (NeighborhoodType &neighborhood,
                                                void *globalData,
                                                const FloatOffsetType &offset) const;
  
private:
  /** The conductance parameter used for anisotropic diffusion. */
  NodeValueType m_ConductanceParameter;

  /** The internal variable used in the FluxStopFunction. It is computed from
   * ConductanceParameter. */
  NodeValueType m_FluxStopConstant;

protected:
  NormalVectorDiffusionFunction();
  ~NormalVectorDiffusionFunction() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** The method called in anisotropic diffusion to inhibit diffusion across
      areas with large curvature. */
  NodeValueType FluxStopFunction (const NodeValueType v) const
  {
    // the slow ::exp function could be replaced with a lookup table
    if (v<=0.0) return NumericTraits<NodeValueType>::One;
    else return static_cast<NodeValueType>(::exp(m_FluxStopConstant*v));
  }
  
private:
  /** The isotropic/anisotropic filtering choice parameter. */
  int m_NormalProcessType; 

  NormalVectorDiffusionFunction(const Self&); //purposely not implemented
  void operator=(const Self&);                //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNormalVectorDiffusionFunction.txx"
#endif

#endif
