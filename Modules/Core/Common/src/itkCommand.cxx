/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#include "itkCommand.h"

namespace itk
{
  Command::Command()
    {}

  Command::~Command()
    {}

  CStyleCommand::CStyleCommand() :
    m_ClientData( ITK_NULLPTR ),
    m_Callback( ITK_NULLPTR ),
    m_ConstCallback( ITK_NULLPTR ),
    m_ClientDataDeleteCallback( ITK_NULLPTR )
  {}

  CStyleCommand::~CStyleCommand()
    {
    if ( m_ClientDataDeleteCallback )
      {
      m_ClientDataDeleteCallback(m_ClientData);
      }
    }

  void CStyleCommand::SetClientData(void *cd)
    {
    m_ClientData = cd;
    }
  void CStyleCommand::SetCallback(FunctionPointer f)
    {
    m_Callback = f;
    }
  void CStyleCommand::SetConstCallback(ConstFunctionPointer f)
    {
    m_ConstCallback = f;
    }
  void CStyleCommand::SetClientDataDeleteCallback(DeleteDataFunctionPointer f)
    {
    m_ClientDataDeleteCallback = f;
    }

  void CStyleCommand::Execute(Object *caller, const EventObject & event)
    {
    if ( m_Callback )
      {
      m_Callback(caller, event, m_ClientData);
      }
    }

  void CStyleCommand::Execute(const Object *caller, const EventObject & event)
    {
    if ( m_ConstCallback )
      {
      m_ConstCallback(caller, event, m_ClientData);
      }
    }
}
