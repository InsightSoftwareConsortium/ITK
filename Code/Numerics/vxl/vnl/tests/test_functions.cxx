#include <vcl_cstdlib.h>
#include <vcl_iostream.h>
#include <vcl_iomanip.h>
#include <vcl_cmath.h>

#include <vnl/vnl_test.h>
#include <vnl/vnl_matops.h>
#include <vnl/algo/vnl_chi_squared.h>

// Test function results for chi-squared cumulative density function.
// The values are those calculated from this function on solaris, and
// agree (to 3sf) with those from a mathematical table.

// Each row is for a different dof, from 1 through 96 in increments of
// 5.

// Each column is for a different chi-squared, from 0 though 180 in
// increments of 20.

float cdf_baseline[] =
{
  0.0F,        0.9999922513961792F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,

  0.0F,         0.997230589389801F,        0.9999995231628418F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,

  0.0F,        0.9546593427658081F,        0.9999642372131348F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,
  
  0.0F,        0.7797793745994568F,        0.9992213845252991F,         0.999999463558197F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,
  
  0.0F,        0.4787387549877167F,        0.9925632476806641F,        0.9999872446060181F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,
  
  0.0F,        0.2084435224533081F,        0.9609879851341248F,        0.9998323321342468F,        0.9999997615814209F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,
  
  0.0F,          0.06419637799263F,        0.8709596395492554F,        0.9986501932144165F,        0.9999966621398926F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,
  
  0.0F,       0.01427761372178793F,        0.7029715776443481F,        0.9927297830581665F,        0.9999653100967407F,        0.9999999403953552F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,
  
  0.0F,      0.002355103613808751F,          0.48504838347435F,        0.9720590710639954F,         0.999743640422821F,        0.9999992251396179F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,
  
  0.0F,     0.0002957367978524417F,        0.2793886661529541F,        0.9194309711456299F,        0.9986010789871216F,         0.999992847442627F,                         1.0F,                         1.0F,                         1.0F,                         1.0F,
  
  0.0F,     2.893863529607188e-05F,        0.1331225633621216F,        0.8182400465011597F,        0.9941486716270447F,        0.9999499917030334F,        0.9999998211860657F,                         1.0F,                         1.0F,                         1.0F,
  
  0.0F,     2.253534148621839e-06F,       0.05248071253299713F,        0.6671308875083923F,        0.9806610345840454F,        0.9997251629829407F,        0.9999985098838806F,                         1.0F,                         1.0F,                         1.0F,
  
  0.0F,       1.3772501006315e-07F,       0.01720699854195118F,        0.4878147542476654F,        0.9481646418571472F,        0.9987862706184387F,        0.9999901056289673F,        0.9999999403953552F,                         1.0F,                         1.0F,
  
  0.0F,     7.372207733169489e-09F,      0.004727425519376993F,         0.315458744764328F,        0.8846964240074158F,        0.9956070780754089F,        0.9999452233314514F,        0.9999997019767761F,                         1.0F,                         1.0F,
  
  0.0F,    -4.068493097975079e-09F,      0.001097434549592435F,        0.1790828108787537F,        0.7825800180435181F,        0.9867346882820129F,        0.9997487664222717F,        0.9999980330467224F,                         1.0F,                         1.0F,
  
  0.0F,     1.164024432398492e-11F,     0.0002170786901842803F,        0.0890129953622818F,        0.6453492641448975F,         0.966045081615448F,        0.9990298748016357F,        0.9999889731407166F,        0.9999999403953552F,                         1.0F,

  0.0F,    -4.387028074148702e-09F,     3.688604556373321e-05F,       0.03875835239887238F,        0.4894571304321289F,        0.9252463579177856F,        0.9968048334121704F,        0.9999479651451111F,        0.9999995827674866F,                         1.0F,

  0.0F,     9.658940314238862e-15F,     5.425228664535098e-06F,       0.01481951586902142F,        0.3381826877593994F,        0.8564977645874023F,        0.9909185767173767F,        0.9997891187667847F,        0.9999977350234985F,                         1.0F,

  0.0F,     -4.38739045094394e-09F,     6.958833296266675e-07F,      0.004993025213479996F,        0.2116235643625259F,        0.7565873265266418F,        0.9774854183197021F,        0.9992581009864807F,        0.9999892115592957F,        0.9999999403953552F,

  0.0F,     2.220446049250313e-16F,     7.826930215060202e-08F,      0.001488302601501346F,        0.1195827126502991F,         0.630331814289093F,        0.9508233070373535F,        0.9977140426635742F,        0.9999545812606812F,        0.9999995231628418F
};

extern "C"
int testFunctions()
{
  int n;
  float chisq;
  int idx = 0;
  for (n=1; n<100; n+=5)
    {
      for (chisq = 0; chisq < 200; chisq+=20)
	{
	  float cdf = vnl_chi_squared_cumulative(chisq,n);
	  Assert("vnl_chi_squared CDF", fabs(cdf - cdf_baseline[idx]) < 1e-15);
	  if (! (fabs(cdf - cdf_baseline[idx]) < 1e-15))
	    vcl_cout << "Error = "
	      /// win32		 <<  setprecision(16)
                 <<  fabs(cdf - cdf_baseline[idx]) << vcl_endl;
	  ++idx;
	}
    }
  
  vcl_cout << "cdf(7.88,1) = " << vnl_chi_squared_cumulative(7.88F,1) 
       << " should be about 0.995 " << vcl_endl;
  vcl_cout << "cdf(14.8,12) = " << vnl_chi_squared_cumulative(14.8F,12)
       << " should be about 0.75 " << vcl_endl;
  vcl_cout << "cdf(10.1,19) = " << vnl_chi_squared_cumulative(10.1F,19)
       << " should be about 0.05 " << vcl_endl;
  vcl_cout << "cdf(39.3,40) = " << vnl_chi_squared_cumulative(39.3F,40)
       << " should be about 0.50 " << vcl_endl;
  vcl_cout << "cdf(109.1,100) = " << vnl_chi_squared_cumulative(109.1F,100)
       << " should be about 0.75 " << vcl_endl;

  float hist1[20];
  float hist2[20];
  int i;
  for (i=0; i<20; i++)
    {
      hist1[i] = (float)(10+20.0*((((float)rand())/RAND_MAX)-0.5));
      hist2[i] = 10;
    }
  chisq = 0;
  for (i=0; i<20; i++)
    {
      vcl_cout << i << " " << hist1[i] << " " << hist2[i] << vcl_endl;
      float delta = hist1[i] - hist2[i];
      chisq += delta*delta/(hist1[i] + hist2[i]);
    }
  vcl_cout << "cdf(" << chisq << ",20) = " 
       << vnl_chi_squared_cumulative(chisq,20)
       << " so P(same dist) = " << (1.0 - vnl_chi_squared_cumulative(chisq,20))
       << vcl_endl;
  return 0;
}

TESTMAIN(testFunctions);
