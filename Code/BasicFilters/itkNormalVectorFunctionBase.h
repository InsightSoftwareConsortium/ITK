#ifndef __itkNormalVectorFunctionBase_h_
#define __itkNormalVectorFunctionBase_h_

#include "itkFiniteDifferenceSparseImageFunction.h"

namespace itk {

/*
 * \class NormalVectorFunctionBase
 *
 * \brief This class defines the common functionality for Sparse Image
 * neighborhoods of unit vectors.
 *
 * \par
 * This class is derived from the FiniteDifferenceSparseImageFunction class and
 * adds the functionality needed to process unit normal vector
 * neighborhoods.
 *
 * \par
 * This class is the parent class of the NormalVectorDiffusionFunction
 * class which defines all the necessary functionality for performing diffusion
 * operations on unit vectors stored in sparse images. Other (non-diffusion)
 * filters (such as median filtering) on unit vectors can also be derived from
 * this base class.
 *
 * \par PARAMETERS
 * This function class has a time step parameter which is returned by the
 * ComputeGloablTimeStep method. Unlike other finite difference function
 * classes, this class does not use the maximum change magnitude to compute the
 * time step, it returns this predetermined time step.
 */

template <class TSparseImageType>
class ITK_EXPORT NormalVectorFunctionBase
  :public FiniteDifferenceSparseImageFunction<TSparseImageType>
{
public:
  /** Standard class typedef. */
  typedef NormalVectorFunctionBase Self;
  typedef FiniteDifferenceSparseImageFunction<TSparseImageType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro( NormalVectorFunctionBase, FiniteDifferenceSparseImageFunction );
   
  /** Image dimension derived from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** Typedefs from the superclass. */
  typedef typename Superclass::TimeStepType     TimeStepType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::FloatOffsetType  FloatOffsetType;
  typedef typename Superclass::IndexType        IndexType;
  typedef typename Superclass::SparseImageType  SparseImageType;

  /** The node type for the sparse image. */
  typedef typename SparseImageType::NodeType NodeType;
  
  /** The basic floating point type for the variables. */
  typedef typename NodeType::NodeValueType NodeValueType;

  /** The vector type for the normals. */
  typedef typename NodeType::NodeDataType NormalVectorType;
  
  /** Globaldata methods are not needed in this class. */
  virtual void *GetGlobalDataPointer() const {return 0;}
  virtual void ReleaseGlobalDataPointer(void*) const {};
  
  /** For the global time step, we return the time step parameter. */
  virtual TimeStepType ComputeGlobalTimeStep (void*) const
  { return m_TimeStep; }

  /** Sets the time step. */
  void SetTimeStep (const TimeStepType &ts)
  { m_TimeStep = ts; }

  /** Returns the time step. */
  TimeStepType GetTimeStep () const
  { return m_TimeStep; } 

protected:
  NormalVectorFunctionBase();
  ~NormalVectorFunctionBase() {}
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

private:
  /** The time step for normal vector finite difference computations. */
  TimeStepType m_TimeStep;
  
  NormalVectorFunctionBase(const Self&); //purposely not implemented
  void operator=(const Self&);           //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNormalVectorFunctionBase.txx"
#endif

#endif
