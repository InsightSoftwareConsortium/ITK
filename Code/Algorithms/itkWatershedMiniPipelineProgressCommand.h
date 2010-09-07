/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedMiniPipelineProgressCommand.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWatershedMiniPipelineProgressCommand_h
#define __itkWatershedMiniPipelineProgressCommand_h

#include "itkProcessObject.h"
#include "itkCommand.h"

namespace itk
{
/** \class WatershedMiniPipelineProgressCommand
 * A specialized Command object for updating the progress of a
 *  MiniPipeline.  Follows the progress of a series of filters
 *  and calls UpdateProgress on another filter (i.e. the filter
 * implementing the mini-pipeline). */
class ITK_EXPORT WatershedMiniPipelineProgressCommand:public Command
{
public:
  /** Smart pointer declaration methods */
  typedef WatershedMiniPipelineProgressCommand Self;
  typedef Command                              Superclass;
  typedef itk::SmartPointer< Self >            Pointer;
  typedef itk::SmartPointer< const Self >      ConstPointer;
  itkTypeMacro(WatershedMiniPipelineProgressCommand, Command);
  itkNewMacro(Self);

  /** Standard Command virtual methods */
  void Execute(Object *caller, const EventObject & event);

  void Execute(const Object *caller, const EventObject & event);

  /** Set/Get the filter whose UpdateProgress will be set by this
   * command object */
  void SetFilter(ProcessObject *p)
  { m_Filter = p; }
  const ProcessObject * GetFilter()
  { return m_Filter; }

  /** Set/Get the base count for stepping through filter progress values */
  itkSetMacro(Count, double);
  itkGetConstMacro(Count, double);

  /** Set/Get the number of filters that this command will expect to be
   * observing */
  itkSetMacro(NumberOfFilters, double);
  itkGetConstMacro(NumberOfFilters, double);
protected:
  WatershedMiniPipelineProgressCommand():m_Count(0.0), m_Filter(NULL),
    m_NumberOfFilters(1.0) {}
  virtual ~WatershedMiniPipelineProgressCommand() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  double         m_Count;
  ProcessObject *m_Filter;
  double         m_NumberOfFilters;
};
} // end namespace itk

#endif
