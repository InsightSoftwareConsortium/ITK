#==========================================================================
#
#   Copyright Insight Software Consortium
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          http://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/

from __future__ import print_function

import itk

md = itk.MetaDataDictionary()
# one way of setting and retrieving double value in the dictionary
dv = itk.MetaDataObject.D.New()
dv.SetMetaDataObjectValue(10.0)
md.Set("double", dv)
print(md.Get("double"))
# other way of setting and retrieving double value (leverages Python's
# weak type system)
md['double'] = 10.0
print(md['double'])
