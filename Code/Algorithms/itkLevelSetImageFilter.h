/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkLevelSetImageFilter_h
#define _itkLevelSetImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkLevelSet.h"

namespace itk
{

/** \class LevelSetImageFilter
  * \brief
  *
  * LevelSetImageFilter is the abstract base class for all process objects 
  * which evolves an input level set to an output level set according to some 
  * PDE update equations. This class is templated over the level set type. 
  * Both the input and output level sets are of the same type
  * represented as Images.
  *
  * Depending on the application a level set function may correspond to 
  * iso-intensity contours of an image (e.g. for image denoising) or
  * some signed distance function from a shape boundary or interface. 
  *
  * Typically, level set evolution is a iterative process.
  * This class provides infrastructure for internal iterations. Specifically,
  * this class maintains two buffers to keep intermediate results between
  * iterations.
  *
  * This class also provides a mechanism for narrowbanding, where only
  * data within a narrow band of interest is processed. 
  *
  * This class is templated over the image type which represents the
  * level set.
  *
  * Note: All level set algorithms will eventually be reimplemented
  * to in the Finite Difference Solver framework.
  *
  * \ingroup LevelSetSegmentation 
  * \ingroup ImageEnhancement 
  */
template <class TLevelSet>
class ITK_EXPORT LevelSetImageFilter : 
  public ImageToImageFilter<TLevelSet,TLevelSet>
{
public:
  /** Standard class typedefs. */
  typedef LevelSetImageFilter Self;
  typedef ImageToImageFilter<TLevelSet,TLevelSet> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(LevelSetImageFilter, ImageToImageFilter);

  /** LevelSetType typedef support. */
  typedef LevelSetTypeDefault<TLevelSet>  LevelSetType;
  typedef typename LevelSetType::LevelSetImageType  LevelSetImageType;
  typedef typename LevelSetType::LevelSetPointer  LevelSetPointer;
  typedef typename LevelSetType::LevelSetConstPointer  LevelSetConstPointer;
  typedef typename LevelSetType::PixelType  PixelType;
  typedef typename LevelSetType::NodeType NodeType;
  typedef typename LevelSetType::NodeContainer NodeContainer;
  typedef typename LevelSetType::NodeContainerPointer NodeContainerPointer;

  /** SetDimension enumeration. */
  enum { SetDimension = LevelSetType::SetDimension};

  /** Set the evolution time step size. Default is 0.5. The timestep is
   * typically algorithm and application dependent. It should be chosen
   * to meet the CFL condition which requires that no pixels move
   * more than one grid position at each time step. */
  itkSetClampMacro( TimeStepSize, double, 0.0, 
    NumericTraits<double>::max());

  /** Get the evolution time step size. */
  itkGetMacro( TimeStepSize, double );

  /** Set the narrowbanding flag. If NarrowBanding is turned on, only the
   * pixels in the InputNarrowBand are processed. A NarrowBand is represented
   * as a VectorContainer of LevelSetNodes. NarrowBanding is set to false
   * by default.
   * \sa LevelSetNode */
  itkSetMacro( NarrowBanding, bool );
  itkBooleanMacro( NarrowBanding );

  /** Get the narrowbanding flag. */
  itkGetMacro( NarrowBanding, bool );

  /** Set the narrow bandwidth. Default is 12. */
  itkSetClampMacro( NarrowBandwidth, double, 0.0, 
    NumericTraits<double>::max());

  /** Get the narrow bandwidth. */
  itkGetMacro( NarrowBandwidth, double );

  /** Get the number of pixels in the narrowband. */
  int GetNarrowBandSize()
    { 
    if( !m_NarrowBanding ) return 0;
    return m_InputNarrowBand->Size();
    }

  /** Set the input narrowband. */
  void SetInputNarrowBand( NodeContainer *ptr );

  /** Get the input narrowband. */
  NodeContainerPointer GetInputNarrowBand( )
    { return m_InputNarrowBand; }

  /** Set the number of iterations. Default is 10. */
  itkSetMacro( NumberOfIterations, unsigned int );

  /** Get the number of iterations. */
  itkGetMacro( NumberOfIterations, unsigned int );

protected:
  LevelSetImageFilter();
  ~LevelSetImageFilter(){};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Allocate the internal buffers for internal interations. */
  virtual void AllocateBuffers(bool outputOnly = false);


  /* */
  virtual void InitializeIteration()
  {
    // Estimate the progress of the filter
    //    this->SetProgress( (float)this->GetElapsedIterations()
    //                       / (float)this->GetMaximumIterations() );
  }
  /** Swap the pointers to the two internal pointers. Useful
   * when the output of one iteration is the input to the next
   * iteration. */
  virtual void SwapBuffers();

  /** Copy data from the input level set to the internal input
   * buffer. */
  virtual void CopyInputToInputBuffer();

  /** Copy data from the internal output buffer to the process
   * buffer. */
  virtual void CopyOutputBufferToOutput();

  /** Get a pointer to the internal input buffer. */
  LevelSetPointer GetInputBuffer(void)
    { return m_InputBuffer; }

  /** Get a pointer to the internal output buffer. */
  LevelSetPointer GetOutputBuffer(void)
    { return m_OutputBuffer; }

  /** Specify the required input region to satisfiy the output
   * request. The default is to request for the largest
   * possible region for any output. Subclasses should override
   * this method if a different input size is desired.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion();

  /** Specify a larger than requested output region. The default
   * is to enlarge the requested region to the largest possible.
   * Subclasses should override this method if a different
   * output region is desired.
   * \sa ProcessObject::EnlargeOutputRequestedRegion(); */
  virtual void EnlargeOutputRequestedRegion(DataObject * output);

private:
  LevelSetImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  NodeContainerPointer   m_InputNarrowBand;
  double                 m_TimeStepSize;
  bool                   m_NarrowBanding;
  double                 m_NarrowBandwidth;

  unsigned int           m_NumberOfIterations;
  LevelSetPointer        m_InputBuffer;
  LevelSetPointer        m_OutputBuffer;

};


} //namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetImageFilter.txx"
#endif

#endif
