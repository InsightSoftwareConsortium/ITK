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
/**
 *  Example on the use of the MetaDataDictionary in Java
 *
 */
import org.itk.itkcommon.*;
public class MetaDataDictionary
{
  public static void main( String argv[] )
  {
    System.out.println("MetaDataDictionary Example");
    itkMetaDataDictionary md = new itkMetaDataDictionary();
    itkMetaDataObjectD d = new itkMetaDataObjectD();
    d.SetMetaDataObjectValue(10.0);
    md.Set("double", d);
    itkMetaDataObjectBase base = md.Get("double");
    itkMetaDataObjectD der = new itkMetaDataObjectD(itkMetaDataObjectBase.getCPtr(base),false);
    System.out.printf("%f\n", der.GetMetaDataObjectValue());
 }
}
