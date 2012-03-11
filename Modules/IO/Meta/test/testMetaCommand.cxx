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

#include <fstream>


#include <metaCommand.h>

int testMetaCommand(int argc, char * argv[])
{
  MetaCommand command;
  command.SetOption("ExpectedFailStatus","ExpectedFailStatus",true,"Use to check that proper failures are listed.");
  command.AddOptionField("ExpectedFailStatus","ExpectedFailStatus", MetaCommand::INT,true,"1","Expected fail status is true");

  command.SetOption("SumOfValues","sum",true,"Sum of values passed into command line, Default values are 100");
  command.AddOptionField("SumOfValues","SumOfValues", MetaCommand::INT,true);

  command.SetOption("Required1Complete_rs","r1c_rs",true, "RequiredOption1");
  command.AddOptionField("Required1Complete_rs","Required1Complete_rs", MetaCommand::STRING,true);
  command.SetOptionComplete("Required1Complete_rs",true);

  command.SetOption("Required2_ri","r2_ri",true,"Required2");
  command.AddOptionField("Required2_ri","Required2_ri", MetaCommand::INT,true);

  command.SetOption("OptionalField1_ri","o1_ri",false, "Optional Field 1");
  command.AddOptionField("OptionalField1_ri","OptionalField1_ri", MetaCommand::INT,true,"100","A value of 100");

  command.SetOption("OptionalField2_ri","o2_ri",false, "Optional Field 1");
  command.AddOptionField("OptionalField2_ri","OptionalField2_ri", MetaCommand::INT,true,"100","A value of 100");

  command.SetOption("OptionalField1_oi","o1_oi",false, "Optional Field 2");
  command.AddOptionField("OptionalField1_oi","OptionalField1_oi", MetaCommand::INT,false,"100","A value of 100");

  command.SetOption("OptionalField2_oi","o2_oi",false, "Optional Field 2");
  command.AddOptionField("OptionalField2_oi","OptionalField2_oi", MetaCommand::INT,false,"100","A value of 100");

  command.SetOption("OptionalField3_ri_oi","o3_ri_oi",false, "Optional Field 3, one required, one optional flag");
  command.AddOptionField("OptionalField3_ri_oi","OptionalField3_ri", MetaCommand::INT,true);
  command.AddOptionField("OptionalField3_ri_oi","OptionalField3_oi", MetaCommand::INT,false,"100","A value of 100");

  if(command.Parse(argc,argv) == true )
    {
    const int SumValue=
      +( command.GetOptionWasSet("Required2_ri")         ? command.GetValueAsInt("Required2_ri","Required2_ri") : 0 )
      +( command.GetOptionWasSet("OptionalField1_ri")    ? command.GetValueAsInt("OptionalField1_ri","OptionalField1_ri"): 0 )
      +( command.GetOptionWasSet("OptionalField2_ri")    ? command.GetValueAsInt("OptionalField2_ri","OptionalField2_ri"): 0 )
      +( command.GetOptionWasSet("OptionalField1_oi")    ? command.GetValueAsInt("OptionalField1_oi","OptionalField1_oi"): 0 )
      +( command.GetOptionWasSet("OptionalField2_oi")    ? command.GetValueAsInt("OptionalField2_oi","OptionalField2_oi"): 0 )
      +( command.GetOptionWasSet("OptionalField3_ri_oi") ? command.GetValueAsInt("OptionalField3_ri_oi","OptionalField3_ri"): 0 )
      +( command.GetOptionWasSet("OptionalField3_ri_oi") ? command.GetValueAsInt("OptionalField3_ri_oi","OptionalField3_oi"): 0 );
    std::cout << "Computed " <<  SumValue << " expected " << command.GetValueAsInt("SumOfValues") << std::endl;

    if(command.GetValueAsInt("ExpectedFailStatus","ExpectedFailStatus") == 1)
      {
      std::cout << "Expected parse failure that did not occur, so test failed" << std::endl;
      return 1;
      }
    else
      {
      return ( command.GetValueAsInt("SumOfValues") - SumValue );
      }
    }
    else
    {
    if(command.GetValueAsInt("ExpectedFailStatus","ExpectedFailStatus") == 1)
      {
      std::cout << "Expected parse failure, so test succeeded" << std::endl;
      return 0;
      }
    }
  std::cout << "Unexpected parse failure, so test failed" << std::endl;
  return 1;
}
