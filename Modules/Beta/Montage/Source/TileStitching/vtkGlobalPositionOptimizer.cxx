/*=========================================================================
 *
 *  Copyright Kitware Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "vtkObjectFactory.h"
#include "vtkGlobalPositionOptimizer.h"

#include <itkLevenbergMarquardtOptimizer.h>
#include <itkMultipleValuedCostFunction.h>

vtkCxxRevisionMacro(vtkGlobalPositionOptimizer, "$Revision: 0.01 $");
vtkStandardNewMacro(vtkGlobalPositionOptimizer);

//------------------------------------------------------------------------------
// Description:
// Used to output iteration information
class CommandIterationUpdateLevenbergMarquardt : public itk::Command
{
public:
  typedef  CommandIterationUpdateLevenbergMarquardt   Self;
  typedef  itk::Command                               Superclass;
  typedef  itk::SmartPointer<Self>                    Pointer;

  itkNewMacro( Self );

protected:
  CommandIterationUpdateLevenbergMarquardt()
    {
    this->IterationNumber=0;
    }

public:
  typedef itk::LevenbergMarquardtOptimizer   OptimizerType;
  typedef const OptimizerType   *            OptimizerPointer;

  void Execute(itk::Object *caller, const itk::EventObject & event)
    {
    Execute( (const itk::Object *)caller, event);
    }

  void Execute(const itk::Object * object, const itk::EventObject & event)
    {
    std::cout << "Observer::Execute() " << std::endl;
    OptimizerPointer optimizer =
      dynamic_cast< OptimizerPointer >( object );
    if (this->FunctionEvent.CheckEvent( &event ))
      {
      std::cout << this->IterationNumber++ << " "
                << optimizer->GetCachedCurrentPosition() << std::endl;
      }
    }
private:
  unsigned long                             IterationNumber;
  itk::FunctionEvaluationIterationEvent     FunctionEvent;
};

class GlobalPositionCostFunction : public itk::MultipleValuedCostFunction
{
public:
  typedef GlobalPositionCostFunction                Self;
  typedef itk::MultipleValuedCostFunction   Superclass;
  typedef itk::SmartPointer<Self>           Pointer;
  typedef itk::SmartPointer<const Self>     ConstPointer;
  itkNewMacro(Self)

  typedef Superclass::ParametersType        ParametersType;
  typedef Superclass::DerivativeType        DerivativeType;
  typedef Superclass::MeasureType           MeasureType;

  unsigned int GetNumberOfParameters() const
    {
    return this->m_NumberOfParameters;
    }

  void SetNumberOfParameters(unsigned int num)
    {
    this->m_NumberOfParameters = num;
    this->AllocateMemoryForMeasureAndParameters();
    }

  unsigned int GetNumberOfValues() const
    {
    return this->m_NumberOfValues;
    }

  void SetNumberOfValues(unsigned int num)
    {
    this->m_NumberOfValues = num;
    this->AllocateMemoryForMeasureAndParameters();
    }

  void SetOffsets(const MeasureType& offsets)
    {
    this->m_Offset = offsets;
    }

  void SetMeasureIdToParameterIdsMap(std::map<int, std::vector<int> >& idMap)
    {
    this->m_MeasureIdToParameterIdsMap = idMap;
    }

  // For the i-th pair of overlapping tile i_1 and i_2. Define their positions
  // to the common origin as p_i_1 and p_i_2, translation offset as t_i.
  // The i-th measure element is d_i = |t_i + p_i_1 - p_i_2|.
  MeasureType GetValue(const ParametersType& parameters) const
    {
    std::map<int, std::vector<int> >::const_iterator it;
    for (unsigned int i = 0; i < this->GetNumberOfValues(); i++)
      {
      it = this->m_MeasureIdToParameterIdsMap.find(static_cast<int>(i));
      if (it != this->m_MeasureIdToParameterIdsMap.end())
        {
        this->m_Measure[i] = parameters[(it->second)[0]]
                           - parameters[(it->second)[1]] + this->m_Offset[i];
        }
      else
        {
        std::cout << "cannot find corresponding tile pair for id " << i << std::endl;
        }
      }
    return this->m_Measure;
    }

  // get the derivatives for each measure
  void GetDerivative(const ParametersType& parameters,
                     DerivativeType& derivative) const
    {
    for (unsigned int i = 0; i < this->GetNumberOfParameters(); i++)
      {
      for (unsigned int j = 0; j < this->GetNumberOfValues(); j++)
        {
        this->m_Derivative[i][j] = 0;
        }
      }

    std::map<int, std::vector<int> >::const_iterator it;
    for (unsigned int i = 0; i < this->GetNumberOfValues(); i++)
      {
      it = this->m_MeasureIdToParameterIdsMap.find(i);
      if (it != this->m_MeasureIdToParameterIdsMap.end())
        {
        this->m_Derivative[(it->second)[0]][i] = 1;
        this->m_Derivative[(it->second)[1]][i] = -1;
        }
      else
        {
        std::cout << "cannot find corresponding tile pair for id " << i << std::endl;
        }
      }

    // note: we will fix the position of the first grid as the positions of all
    // the other grids are defined with respect to it.
    for (unsigned int i = 0; i < this->GetNumberOfValues(); i++)
      {
      this->m_Derivative[0][i] = 0;
      }

    derivative = this->m_Derivative;
    }

protected:
  GlobalPositionCostFunction()
    {
    this->m_NumberOfParameters = 0;
    this->m_NumberOfValues = 0;
    this->m_Measure.SetSize(0);
    this->m_Derivative.SetSize(0,0);
    }

  ~GlobalPositionCostFunction()
    {
    }

  void AllocateMemoryForMeasureAndParameters()
    {
    this->m_Measure.SetSize(this->GetNumberOfValues());
    this->m_Derivative.SetSize(this->GetNumberOfParameters(), this->GetNumberOfValues());
    }

private:
  mutable MeasureType               m_Measure;
  mutable DerivativeType            m_Derivative;
  unsigned int                      m_NumberOfValues;
  unsigned int                      m_NumberOfParameters;
  MeasureType                       m_Offset;
  std::map<int, std::vector<int> >  m_MeasureIdToParameterIdsMap;
};

//------------------------------------------------------------------------------
vtkGlobalPositionOptimizer::vtkGlobalPositionOptimizer()
{
  for (int i = 0; i < 3; i++)
    {
    this->GridSize[i] = 0;
    }
  this->DebugFlag = false;
}

//------------------------------------------------------------------------------
vtkGlobalPositionOptimizer::~vtkGlobalPositionOptimizer()
{
}


//------------------------------------------------------------------------------
void vtkGlobalPositionOptimizer::PrintSelf(ostream& os,
                                                     vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);
}

//------------------------------------------------------------------------------
void vtkGlobalPositionOptimizer::Update()
{
  // sanity check
  if ( (this->PositionParameters.size() % this->GetNumberOfPositions())
    || (this->OffsetParameters.size() % this->GetNumberOfOffsets()) )
    {
    vtkErrorMacro("Position and offset vector size do not match the dimension and "
      "size of the tile grid. Global optimization exit without running.");
    return;
    }

  typedef itk::LevenbergMarquardtOptimizer      OptimizerType;
  typedef OptimizerType::InternalOptimizerType  VnlOptimizerType;

  // Apply Levenberg-Marquardt optimization scheme one each of the three dimensions.
  for (int dimensionId = 0; dimensionId < 3; dimensionId++)
    {
    if (this->GridSize[dimensionId] <= 1)
      {
      continue;
      }

    // set up cost function
    GlobalPositionCostFunction::Pointer costFunction = GlobalPositionCostFunction::New();
    costFunction->SetNumberOfParameters(this->GetNumberOfPositions());
    costFunction->SetNumberOfValues(this->GetNumberOfOffsets());
    costFunction->SetMeasureIdToParameterIdsMap(this->OffsetIdToPositionIdsMap);

    // set up optimizer
    OptimizerType::Pointer optimizer = OptimizerType::New();
    optimizer->SetCostFunction(costFunction);
    optimizer->UseCostFunctionGradientOn();

    // set up convergence
    VnlOptimizerType* vnlOptimizer = optimizer->GetOptimizer();
    vnlOptimizer->set_f_tolerance(1e-2);
    vnlOptimizer->set_g_tolerance(1e-2);
    vnlOptimizer->set_x_tolerance(1e-5);
    vnlOptimizer->set_epsilon_function(1e-9);
    vnlOptimizer->set_max_function_evals(200);

    // set initial position
    typedef GlobalPositionCostFunction::ParametersType ParametersType;
    ParametersType initialParameters(this->GetNumberOfPositions());
    for (unsigned int i = 0; i < this->GetNumberOfPositions(); i++)
      {
      initialParameters[i] = this->PositionParameters[3*i + dimensionId];
      }
    optimizer->SetInitialPosition(initialParameters);

    // set offsets computed from registration
    typedef GlobalPositionCostFunction::MeasureType MeasureType;
    MeasureType offsets(this->GetNumberOfOffsets());
    for (unsigned int i = 0; i < this->GetNumberOfOffsets(); i++)
      {
      offsets[i] = this->OffsetParameters[3*i + dimensionId];
      }
    costFunction->SetOffsets(offsets);

    if (this->DebugFlag)
      {
      CommandIterationUpdateLevenbergMarquardt::Pointer observer =
        CommandIterationUpdateLevenbergMarquardt::New();
      optimizer->AddObserver(itk::IterationEvent(), observer);
      optimizer->AddObserver(itk::FunctionEvaluationIterationEvent(), observer);
      }

    try
      {
      optimizer->StartOptimization();
      }
    catch( itk::ExceptionObject & e )
      {
      std::cerr << "Exception thrown ! " << std::endl;
      std::cerr << "Error in optimization of dimension " << dimensionId << std::endl;
      std::cerr << "Location    = " << e.GetLocation()    << std::endl;
      std::cerr << "Description = " << e.GetDescription() << std::endl;
      return;
      }

    if (this->DebugFlag)
      {
      std::cerr << "Optimizer end condition for dimension " << dimensionId << ": "
                << vtkGlobalPositionOptimizer::
        GetOptimizerEndCondition(vnlOptimizer->get_failure_code())
                << std::endl;
      }

    OptimizerType::ParametersType finalPosition =
      optimizer->GetCurrentPosition();
    for (unsigned int i = 0; i < this->GetNumberOfPositions(); i++)
      {
      this->PositionParameters[3*i + dimensionId] = finalPosition[i];
      }
    }
}

//------------------------------------------------------------------------------
std::string vtkGlobalPositionOptimizer::GetOptimizerEndCondition(int failureCode)
{
  // Error codes taken from vxl/vnl/vnl_nonlinear_minimizer.h
  switch(failureCode)
    {
    case vnl_nonlinear_minimizer::ERROR_FAILURE:
      return " Error Failure"; break;
    case vnl_nonlinear_minimizer::ERROR_DODGY_INPUT:
      return " Error Dogy Input"; break;
    case  vnl_nonlinear_minimizer::CONVERGED_FTOL:
      return " Converged F  Tolerance"; break;
    case  vnl_nonlinear_minimizer::CONVERGED_XTOL:
      return " Converged X  Tolerance"; break;
    case  vnl_nonlinear_minimizer::CONVERGED_XFTOL:
      return " Converged XF Tolerance"; break;
    case  vnl_nonlinear_minimizer::CONVERGED_GTOL:
      return " Converged G  Tolerance"; break;
    case  vnl_nonlinear_minimizer::FAILED_TOO_MANY_ITERATIONS:
      return " Too many iterations   "; break;
    case  vnl_nonlinear_minimizer::FAILED_FTOL_TOO_SMALL:
      return " Failed F Tolerance too small "; break;
    case  vnl_nonlinear_minimizer::FAILED_XTOL_TOO_SMALL:
      return " Failed X Tolerance too small "; break;
    case  vnl_nonlinear_minimizer::FAILED_GTOL_TOO_SMALL:
      return " Failed G Tolerance too small "; break;
    case  vnl_nonlinear_minimizer::FAILED_USER_REQUEST:
      return " Failed user request "; break;
    default:
      return " Invalid failure code"; break;
    }
}

//------------------------------------------------------------------------------
const std::vector<double> &
vtkGlobalPositionOptimizer::GetOptimizedPositionParameters()
{
  return this->PositionParameters;
}

//------------------------------------------------------------------------------
unsigned int vtkGlobalPositionOptimizer::GetNumberOfValidDimensions()
{
  unsigned int dimension = 0;
  for (int i = 0; i < 3; i++)
    {
    if (this->GridSize[i] > 1)
      {
      dimension++;
      }
    }
  return dimension;
}

//------------------------------------------------------------------------------
unsigned int vtkGlobalPositionOptimizer::GetNumberOfOffsets()
{
  return static_cast<unsigned int>(this->OffsetParameters.size() / 3);
}

//------------------------------------------------------------------------------
unsigned int vtkGlobalPositionOptimizer::GetNumberOfPositions()
{
  return static_cast<unsigned int>(this->PositionParameters.size() / 3);
}

//------------------------------------------------------------------------------
void vtkGlobalPositionOptimizer
::SetInitialOffsetParameters(const std::vector<double>& parameters)
{
  this->OffsetParameters = parameters;
}

//------------------------------------------------------------------------------
void vtkGlobalPositionOptimizer
::SetInitialPositionParameters(const std::vector<double>& parameters)
{
  this->PositionParameters = parameters;
}

//------------------------------------------------------------------------------
void vtkGlobalPositionOptimizer::SetOffsetIdToPositionIdsMap(
         const std::map<int, std::vector<int> >& idMap)
{
  this->OffsetIdToPositionIdsMap = idMap;
}
