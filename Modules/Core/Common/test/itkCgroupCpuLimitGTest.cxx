/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "ITKCommonExport.h"
#include "itksys/SystemTools.hxx"
#include "itkGTest.h"

#include <fstream>
#include <limits>
#include <string>

namespace itk::detail
{
// Defined in itkCgroupCpuLimit.cxx, which has no header.
ITKCommon_EXPORT unsigned int
ComputeCgroupCpuLimit(const std::string & procSelfCgroup, const std::string & cgroupRoot);
} // namespace itk::detail

namespace
{
// A cgroup file system fixture under the GoogleTest temporary directory.
class CgroupTree
{
public:
  explicit CgroupTree(const std::string & name)
    : m_Root(::testing::TempDir() + "itkCgroupCpuLimitGTest_" + name)
  {
    itksys::SystemTools::RemoveADirectory(m_Root);
    itksys::SystemTools::MakeDirectory(m_Root);
  }
  ~CgroupTree() { itksys::SystemTools::RemoveADirectory(m_Root); }

  void
  Write(const std::string & relativePath, const std::string & content) const
  {
    const std::string path = m_Root + relativePath;
    itksys::SystemTools::MakeDirectory(itksys::SystemTools::GetFilenamePath(path));
    std::ofstream file(path);
    file << content;
    ASSERT_TRUE(file.good()) << path;
  }

  const std::string &
  Root() const
  {
    return m_Root;
  }

private:
  std::string m_Root;
};
} // namespace

TEST(CgroupCpuLimit, Version2NoQuota)
{
  const CgroupTree tree("v2max");
  tree.Write("/a/b/cpu.max", "max 100000\n");
  EXPECT_EQ(itk::detail::ComputeCgroupCpuLimit("0::/a/b\n", tree.Root()), 0U);
}

TEST(CgroupCpuLimit, Version2QuotaRoundsUp)
{
  const CgroupTree tree("v2quota");
  tree.Write("/a/b/cpu.max", "250000 100000\n");
  EXPECT_EQ(itk::detail::ComputeCgroupCpuLimit("0::/a/b\n", tree.Root()), 3U);
  tree.Write("/a/b/cpu.max", "400000 100000\n");
  EXPECT_EQ(itk::detail::ComputeCgroupCpuLimit("0::/a/b\n", tree.Root()), 4U);
  tree.Write("/a/b/cpu.max", "9223372036854775807 100000\n");
  EXPECT_EQ(itk::detail::ComputeCgroupCpuLimit("0::/a/b\n", tree.Root()), std::numeric_limits<unsigned int>::max());
}

TEST(CgroupCpuLimit, Version2ContainerRootCgroup)
{
  // With a private cgroup namespace the process cgroup is "/", and the mount point holds its files.
  const CgroupTree tree("v2root");
  tree.Write("/cpu.max", "200000 100000\n");
  EXPECT_EQ(itk::detail::ComputeCgroupCpuLimit("0::/\n", tree.Root()), 2U);
}

TEST(CgroupCpuLimit, QuotaOnParentOnly)
{
  const CgroupTree tree("v2parent");
  tree.Write("/a/cpu.max", "150000 100000\n");
  tree.Write("/a/b/cpu.max", "max 100000\n");
  EXPECT_EQ(itk::detail::ComputeCgroupCpuLimit("0::/a/b\n", tree.Root()), 2U);
}

TEST(CgroupCpuLimit, SmallestQuotaOfAncestorsApplies)
{
  const CgroupTree tree("v2nested");
  tree.Write("/a/cpu.max", "800000 100000\n");
  tree.Write("/a/b/cpu.max", "100000 100000\n");
  EXPECT_EQ(itk::detail::ComputeCgroupCpuLimit("0::/a/b\n", tree.Root()), 1U);
}

TEST(CgroupCpuLimit, Version1)
{
  const CgroupTree tree("v1");
  tree.Write("/cpu,cpuacct/job/cpu.cfs_quota_us", "-1\n");
  tree.Write("/cpu,cpuacct/job/cpu.cfs_period_us", "100000\n");
  const std::string cgroup = "12:memory:/job\n4:cpu,cpuacct:/job\n";
  EXPECT_EQ(itk::detail::ComputeCgroupCpuLimit(cgroup, tree.Root()), 0U);
  tree.Write("/cpu,cpuacct/job/cpu.cfs_quota_us", "350000\n");
  EXPECT_EQ(itk::detail::ComputeCgroupCpuLimit(cgroup, tree.Root()), 4U);
}

TEST(CgroupCpuLimit, Version1MountedUnderAnotherName)
{
  // The controller list need not match the mount directory, which systemd and Docker also link as "cpu".
  const CgroupTree tree("v1cpu");
  tree.Write("/cpu/job/cpu.cfs_quota_us", "200000\n");
  tree.Write("/cpu/job/cpu.cfs_period_us", "100000\n");
  EXPECT_EQ(itk::detail::ComputeCgroupCpuLimit("4:cpuacct,cpu:/job\n", tree.Root()), 2U);
}

TEST(CgroupCpuLimit, MalformedInputIsIgnored)
{
  const CgroupTree tree("malformed");
  tree.Write("/a/cpu.max", "300000 100000\n");
  tree.Write("/b/cpu.max", "lots 100000\n");
  tree.Write("/c/cpu.max", "1.5e5 100000\n");
  tree.Write("/d/cpu.max", "-\n100000\n");
  tree.Write("/e/cpu.max", "99999999999999999999 100000\n");
  EXPECT_EQ(itk::detail::ComputeCgroupCpuLimit("garbage\n0::relative\n0::/a\n", tree.Root()), 3U);
  EXPECT_EQ(itk::detail::ComputeCgroupCpuLimit("0::/b\n", tree.Root()), 0U);
  EXPECT_EQ(itk::detail::ComputeCgroupCpuLimit("0::/c\n", tree.Root()), 0U);
  EXPECT_EQ(itk::detail::ComputeCgroupCpuLimit("0::/d\n", tree.Root()), 0U);
  EXPECT_EQ(itk::detail::ComputeCgroupCpuLimit("0::/e\n", tree.Root()), 0U);
}

TEST(CgroupCpuLimit, MissingFilesMeanNoQuota)
{
  const CgroupTree tree("missing");
  EXPECT_EQ(itk::detail::ComputeCgroupCpuLimit("0::/not/there\n", tree.Root()), 0U);
  EXPECT_EQ(itk::detail::ComputeCgroupCpuLimit("4:cpu:/not/there\n", tree.Root()), 0U);
  EXPECT_EQ(itk::detail::ComputeCgroupCpuLimit("", tree.Root()), 0U);
}
