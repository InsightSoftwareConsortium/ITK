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

#include <algorithm>
#include <cstdint>
#include <fstream>
#include <limits>
#include <sstream>
#include <string>
#include <vector>

namespace itk
{
namespace
{
// Parses a whole token as a decimal 64-bit integer, without locale dependence.
bool
ParseInt64(const std::string & token, std::int64_t & value)
{
  const bool  negative = !token.empty() && token[0] == '-';
  std::size_t position = negative ? 1 : 0;
  if (position == token.size())
  {
    return false;
  }
  std::int64_t magnitude = 0;
  for (; position < token.size(); ++position)
  {
    const char c = token[position];
    if (c < '0' || c > '9' || magnitude > (std::numeric_limits<std::int64_t>::max() - (c - '0')) / 10)
    {
      return false;
    }
    magnitude = magnitude * 10 + (c - '0');
  }
  value = negative ? -magnitude : magnitude;
  return true;
}

// Reads the first whitespace-separated tokens of a small file; an unreadable file gives no tokens.
std::vector<std::string>
ReadTokens(const std::string & path)
{
  std::ifstream            file(path);
  std::vector<std::string> tokens;
  std::string              token;
  while (tokens.size() < 2 && file >> token)
  {
    tokens.push_back(token);
  }
  return tokens;
}

// Applies the quota of the cgroup at path under root and of each of its ancestors, since those also apply.
template <typename TApplyQuota>
void
ApplyQuotasOfAncestors(const std::string & root, std::string path, bool isVersion2, TApplyQuota & applyQuota)
{
  while (true)
  {
    const std::string directory = root + (path == "/" ? "" : path);
    std::int64_t      quota = 0;
    std::int64_t      period = 0;
    if (isVersion2)
    {
      const auto tokens = ReadTokens(directory + "/cpu.max");
      if (tokens.size() == 2 && tokens[0] != "max" && ParseInt64(tokens[0], quota) && ParseInt64(tokens[1], period))
      {
        applyQuota(quota, period);
      }
    }
    else
    {
      const auto quotaTokens = ReadTokens(directory + "/cpu.cfs_quota_us");
      const auto periodTokens = ReadTokens(directory + "/cpu.cfs_period_us");
      if (!quotaTokens.empty() && !periodTokens.empty() && ParseInt64(quotaTokens[0], quota) &&
          ParseInt64(periodTokens[0], period))
      {
        applyQuota(quota, period);
      }
    }
    if (path == "/")
    {
      break;
    }
    const auto slash = path.rfind('/');
    path = slash == 0 ? "/" : path.substr(0, slash);
  }
}

} // namespace

namespace detail
{
// Number of CPUs allowed by the cgroup CPU quotas in procSelfCgroup (the content of /proc/self/cgroup) under
// cgroupRoot, rounded up, or 0 if there is no quota. For the v2 entry, cpu.max of the cgroup and of each ancestor is
// read; for a v1 entry with the cpu controller, cpu.cfs_quota_us and cpu.cfs_period_us. The smallest quota applies.
// Exported only for itkCgroupCpuLimitGTest; no header declares it.
ITKCommon_EXPORT unsigned int
ComputeCgroupCpuLimit(const std::string & procSelfCgroup, const std::string & cgroupRoot)
{
  std::uint64_t limit = 0;
  auto          applyQuota = [&limit](std::int64_t quota, std::int64_t period) {
    if (quota > 0 && period > 0)
    {
      const auto cpus = static_cast<std::uint64_t>((quota + period - 1) / period);
      if (limit == 0 || cpus < limit)
      {
        limit = cpus;
      }
    }
  };

  std::istringstream lines(procSelfCgroup);
  std::string        line;
  while (std::getline(lines, line))
  {
    // Lines are "hierarchy-ID:controller-list:cgroup-path".
    const auto first = line.find(':');
    const auto second = first == std::string::npos ? std::string::npos : line.find(':', first + 1);
    if (second == std::string::npos || second + 1 >= line.size() || line[second + 1] != '/')
    {
      continue;
    }
    const std::string controllers = line.substr(first + 1, second - first - 1);
    const std::string path = line.substr(second + 1);

    const bool isVersion2 = controllers.empty();
    if (!isVersion2 && (',' + controllers + ',').find(",cpu,") == std::string::npos)
    {
      continue;
    }
    // A version 1 hierarchy is mounted under its controller list, for example "cpu,cpuacct", and systemd and
    // Docker also link it as "cpu"; the list order can differ from the mount name, so look under both.
    std::vector<std::string> roots{ isVersion2 ? cgroupRoot : cgroupRoot + '/' + controllers };
    if (!isVersion2 && controllers != "cpu")
    {
      roots.push_back(cgroupRoot + "/cpu");
    }
    for (const std::string & root : roots)
    {
      ApplyQuotasOfAncestors(root, path, isVersion2, applyQuota);
    }
  }
  return static_cast<unsigned int>(std::min<std::uint64_t>(limit, std::numeric_limits<unsigned int>::max()));
}

// ComputeCgroupCpuLimit for this process; declared in itkMultiThreaderBase.cxx.
unsigned int
GetProcessCgroupCpuLimit()
{
  std::ifstream file("/proc/self/cgroup");
  if (!file)
  {
    return 0;
  }
  std::stringstream content;
  content << file.rdbuf();
  return ComputeCgroupCpuLimit(content.str(), "/sys/fs/cgroup");
}
} // namespace detail
} // namespace itk
