/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNarrowBandImageFilterBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNarrowBandImageFilterBase_h_
#define __itkNarrowBandImageFilterBase_h_

#include "itkFiniteDifferenceImageFilter.h"
#include "itkMultiThreader.h"
#include "itkNarrowBand.h"
#include "itkObjectStore.h"

namespace itk {

/**
 * \class NarrowBandImageFilterBase 
 *
 * \brief This class implements a multi-threaded finite difference
 *  image to image solver that can be applied to an arbitrary list of 
 *  pixels. 
 *
 * \par
 *  This class is intended as a common base class for classical narrowband
 *  solvers and manifold solvers. This base class implements a common 
 *  memory management and multi-threaded architecture for applying a 
 *  finite difference function to a list of pixels in an image. The specifics
 *  of narrowband solvers such as re-initialization and the use of land-mines
 *  are not implemented. 
 *
 * \par INPUTS
 *  This filter takes an itk::Image as input.  The appropriate type of input
 *  image is entirely determined by the application.  As a rule, however, the
 *  input type is immediately converted to the output type before processing.
 *  This is because the input is not assumed to be a real value type and must be
 *  converted to signed, real values for the calculations.
 *
 * \par OUTPUTS
 *  The output is an itk::Image and is the solution of the pde.  The embedding of
 *  the interface may vary with the application, but the usual ITK convention is
 *  that it is the zero level set in the output image.
 *
 * \par IMPORTANT!
 *  Read the documentation for FiniteDifferenceImageFilter before attempting to
 *  use this filter.  The solver requires that you specify a
 *  FiniteDifferenceFunction to use for calculations.  This is set using the
 *  method SetDifferenceFunction in the parent class.
 *
 * \par REFERENCES
 * Sethian, J.A. Level Set Methods. Cambridge University Press. 1996.
 *
 */
template <class TInputImage, class TOutputImage>
class NarrowBandImageFilterBase  
  : public FiniteDifferenceImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs */
  typedef NarrowBandImageFilterBase Self;
  typedef FiniteDifferenceImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(NarrowBandImageFilterBase, ImageToImageFilter );
  
  /**Typedefs from the superclass */
  typedef typename Superclass::InputImageType  InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename Superclass::FiniteDifferenceFunctionType FiniteDifferenceFunctionType;

  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);

  /** The pixel type of the output image will be used in computations.
   * Inherited from the superclass. */
  typedef typename Superclass::PixelType PixelType;

  /** The value type of a time step.  Inherited from the superclass. */
  typedef typename Superclass::TimeStepType TimeStepType;

  /** The index type for the output image*/
  typedef typename OutputImageType::IndexType IndexType;
  
  /** The data type used in numerical computations.  Derived from the output
   *  image type. */
  typedef typename OutputImageType::ValueType ValueType;

  /*
  
  /** This is the storage type for the nodes on the narrow band */
  typedef BandNode<IndexType,PixelType> BandNodeType;

  /** The list type for storing the narrow band. */
  typedef NarrowBand<BandNodeType> NarrowBandType;
  typedef NarrowBandType::Pointer NarrowBandPointer;
  typedef NarrowBandType::RegionType RegionType;

  /** Set/Get IsoSurfaceValue to use in the input image */
  itkSetMacro( IsoSurfaceValue, ValueType);
  itkGetMacro( IsoSurfaceValue, ValueType);

  /** Root Mean Square Change between successive iterations */
  //  itkGetMacro( RMSChange, ValueType);


  /** This function is used to insert a pixel index into the narrow band  The
   *   entire narrow band can be constructed using this method.  Usually,
   *   however, the narrow band is initialized and reinitialized automatically
   *   by the subclass.*/
  void InsertNarrowBandNode (BandNodeType &node) 
  {
    m_NarrowBand->PushBack(node); // add new node 
    this->Modified();
  };
  
  void InsertNarrowBandNode (IndexType &index)
  {
    BandNodeType tmpnode;
    tmpnode.m_Index = index;
    m_NarrowBand->PushBack(tmpnode);
    this->Modified();
  };
  
  void InsertNarrowBandNode (IndexType &index, PixelType &value, signed char &nodestate)
  {
    BandNodeType tmpnode;
    tmpnode.m_Data = value;
    tmpnode.m_Index = index;
    tmpnode.m_NodeState = nodestate;
    
    m_NarrowBand->PushBack(tmpnode);
    this->Modified();
  }; 

  /** Set the narrow band total radius. The narrow band width will be twice
   this value (positive and negative distance to the zero level set).
   The default value is 3.
  */
  void SetNarrowBandTotalRadius (float val)
    {
     if (m_NarrowBand->GetTotalRadius() != val)
       {
       m_NarrowBand->SetTotalRadius(val);    
       this->Modified();
       }
    }
    
  /** Get the narrow band total radius. */
  float GetNarrowBandTotalRadius()
    {
    return m_NarrowBand->GetTotalRadius();
    }
  
  /** Set the narrow band inner radius. The inner radius is the safe are
  where the level set can be computed.
  The default value is 1.
  */
  void SetNarrowBandInnerRadius (float val)
    {
    if (m_NarrowBand->GetInnerRadius() != val)
      {
      m_NarrowBand->SetInnerRadius(val);
      this->Modified();
      }
    }
  
  /** Get the narrow band inner radius. */
  float GetNarrowBandInnerRadius()
    {
    return m_NarrowBand->GetInnerRadius();
    }
             
  /** This is the virtual method called by Initialize to set the band of operation.
   *  It is left to the subclasses to define this functionality. 
   *  This function can make use of above InsertNarrowBandNode function to create a
   *  band.
   */
  virtual void CreateNarrowBand (){};
  
  
  virtual void SetNarrowBand(NarrowBandType * ptr)
  {
  
  if ( m_NarrowBand != ptr )
    {
     m_NarrowBand = ptr;
     this->Modified();
    }   
  };

  virtual void CopyInputToOutput ();

protected: 
  typename NarrowBandType::Pointer m_NarrowBand;
  NarrowBandImageFilterBase() 
  {
    m_NarrowBand = NarrowBandType::New();
    m_NarrowBand->SetTotalRadius(3);
    m_NarrowBand->SetInnerRadius(1);
    m_ReinitializationFrequency = 6;
    m_Step    = 0;
    m_Touched = false;
  }

  virtual ~NarrowBandImageFilterBase() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** The type of region used in multithreading.  Defines a subregion of the
      narrowband. */
  struct ThreadRegionType 
  {
    typename NarrowBandType::Iterator first; // this is the actual first element
    typename NarrowBandType::Iterator last;  // this is one past the actual last //element
  };

  /** A list of subregions of the narrowband which are passed to each thread
   * for parallel processing. */
  //typename ListType::RegionListType m_RegionList;
  std::vector<RegionType> m_RegionList;
  /** This function returns a single region (of the narrow band list) for use
      in multi-threading */
  int GetSplitRegion (int i, int num, ThreadRegionType &splitRegion);
  
  /** This function clears the existing narrow band, calls CreateNarrowBand to create
   *  a band, and calls the SplitRegions function of NarrowBand to pre-partition
   *  the band for multi-threading.
   */
  virtual void Initialize();

  /** This method check the narrow band state each iteration and reinitialize the narrow band if it is appropiate calling CreateNarrowBand and SplitRegions to pre-partion the band for multi-threading.
  */
  virtual void InitializeIteration();

  /* This function clears all pixels from the narrow band */
  void ClearNarrowBand ();

  unsigned int m_ReinitializationFrequency;
  unsigned int m_CheckFrequency;
  unsigned int m_Step;
  ValueType m_IsoSurfaceValue;
  bool m_Touched;

private:
  NarrowBandImageFilterBase(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Structure for passing information into static callback methods.  Used in
   * the subclasses' threading mechanisms. */
  struct NarrowBandFDThreadStruct
  {
    NarrowBandImageFilterBase *Filter;
    TimeStepType TimeStep;
    TimeStepType *TimeStepList;
    bool *ValidTimeStepList;
  };

  /* This class does not use AllocateUpdateBuffer to allocate memory for its 
   * narrow band. This is taken care of in SetNarrowBand, and InsertNarrowBandNode
   * functions. This function is here for compatability with the 
   * FiniteDifferenceSolver framework.
   */
  virtual void AllocateUpdateBuffer() {};
  
  /** This method applies changes from the m_NarrowBand to the output using
   * the ThreadedAPplyUpdate() method and a multithreading mechanism.  "dt" is
   * the time step to use for the update of each pixel. */
  virtual void ApplyUpdate(TimeStepType dt);

  static ITK_THREAD_RETURN_TYPE ApplyUpdateThreaderCallback( void *arg );
  
  /** This method populates m_NarrowBand with changes for each pixel in the
   * output using the ThreadedCalculateChange() method and a multithreading
   * mechanism. Returns value is a time step to be used for the update. */
  virtual TimeStepType CalculateChange();

  static ITK_THREAD_RETURN_TYPE CalculateChangeThreaderCallback( void *arg );
  
  virtual void ThreadedApplyUpdate(TimeStepType dt,
                                   const ThreadRegionType &regionToProcess,
                                   int threadId);
  virtual TimeStepType ThreadedCalculateChange(const ThreadRegionType &regionToProcess,
                                               int threadId);

};

}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNarrowBandImageFilterBase.txx"
#endif

#endif
