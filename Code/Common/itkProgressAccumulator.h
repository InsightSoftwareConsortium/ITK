/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkProgressAccumulator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkProgressAccumulator_h_
#define __itkProgressAccumulator_h_

#include "itkCommand.h"
#include "itkObject.h"
#include "itkProcessObject.h"
#include <vector>

namespace itk {

/**
 * \class ProgressAccumulator
 * \brief Facilitates progress reporting for filters that wrap around
 *        multiple other filters.
 *        
 * This object allows a mini-pipeline filters to easily keep track of the 
 * progress performed by the internal filters.  
 * See DiscreteGaussianImageFilter.txx for an implementation example.
 *
 * \sa DiscreteGaussianImageFilter
 *
 * <pre>
 * 
 */ 
class ITKCommon_EXPORT ProgressAccumulator : public Object
{
public:
  /** Standard class typedefs. */
  typedef ProgressAccumulator        Self;
  typedef Object                     Superclass;
  typedef SmartPointer<Self>         Pointer;
  typedef SmartPointer<const Self>   ConstPointer;

  /** Typedef for inputting filters */
  typedef ProcessObject              GenericFilterType;
  typedef GenericFilterType::Pointer GenericFilterPointer;

  /** Standard New method. */
  itkNewMacro(Self);  

  /** Runtime information support. */
  itkTypeMacro(ProgressAccumulator,Object);
  
  /** Get the total progress accumulated by this object */
  itkGetMacro(AccumulatedProgress,float);

  /** Set the mini-pipeline filter */
  itkSetObjectMacro(MiniPipelineFilter,ProcessObject);

  /** Set the mini-pipeline filter */
  itkGetConstObjectMacro(MiniPipelineFilter,ProcessObject);

  /** 
   * Register a filter with the progress accumulator and specify the
   * fraction of the overall progress associated with this filter
   */
  void RegisterInternalFilter(GenericFilterType *filter, float weight);

  /**
   * Unregister all filters that have been registered with this object
   */
  void UnregisterAllFilters();

  /** 
   * Reset the progress accumulator.  This method should be called in
   * the beginning of the GenerateData() method in the mini-pipeline
   * filter.
   */
  void ResetProgress();

protected:
  ProgressAccumulator();
  virtual ~ProgressAccumulator();
  void PrintSelf(std::ostream &s, Indent indent) const;

private:
  /**  Command for observing progress of pipeline filters */
  typedef MemberCommand< Self >      CommandType;  
  typedef CommandType::Pointer       CommandPointer;  

  /** Structure associated with each filter in the pipeline */
  struct FilterRecord 
    {
    // Pointer to the filter
    GenericFilterPointer Filter;

    // The weight of the filter in total progress of the mini-pipeline
    float                Weight;

    // The tags for adding/removing observers to mini-pipeline filter
    unsigned long        ProgressObserverTag;
    unsigned long        IterationObserverTag;

    // The progress accumulated by the filter since last Reset()
    float                Progress;
    };

  /** A callback function that is called by the progressing filters */
  void ReportProgress(Object * object, const EventObject & event);
    
  /** The client mini-pipeline filter */
  GenericFilterPointer m_MiniPipelineFilter;
  
  /** An array of record structures */
  typedef std::vector<struct FilterRecord> FilterRecordVector;

  /** The total accumulated progress */
  float m_AccumulatedProgress;

  /** 
   * A list of progress proportions of the different filters in the 
   * pipeline 
   */
  FilterRecordVector m_FilterRecord;

  /** The callback command */
  CommandPointer m_CallbackCommand; 
};

} // End namespace itk

#endif // __itkProgressAccumulator_h_

