#!/bin/bash
#==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/

#% NAME
#%     CodeSwarmWithGource.sh - render a git history video with gource
#%
#% SYNOPSIS
#%     CodeSwarmWithGource.sh SOURCE_TREE OUTPUT_NAME
#%     CodeSwarmWithGource.sh -h | --help
#%
#% DESCRIPTION
#%     Render the commit history of the git repository SOURCE_TREE as an
#%     animation with gource, and encode it with ffmpeg into the MPEG-4
#%     video OUTPUT_NAME.mp4. An existing file of that name is overwritten.
#%     The arguments are not checked.
#%
#% OPTIONS
#%     SOURCE_TREE
#%         The git repository to visualize.
#%     OUTPUT_NAME
#%         Output file name without the .mp4 extension.
#%     -h, --help
#%         Print this help and exit with status 1.
#%
#% IMPLEMENTATION
#%     Written for Linux; needs gource and ffmpeg. gource opens a 1024x768
#%     window while it renders. The video is written relative to the
#%     current directory.
#%
#% EXAMPLES
#%     CodeSwarmWithGource.sh ~/src/ITK ITKSwarm
#%         Write the history of ~/src/ITK to ITKSwarm.mp4.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.
#%     Utilities/Maintenance/computeGitStatistics.sh:
#%         Compute statistics on the git history.

usage() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
}

if test "$1" == "-h" -o "$1" == "--help"; then
  usage
  exit 1
fi


gource \
--bloom-intensity 1.0 \
--bloom-multiplier 0.5 \
--hide filenames  \
--max-user-speed 100 \
--user-scale 0.5 \
--output-framerate 60 \
--camera-mode overview \
--date-format "%B %Y" -s 0.01 \
-1024x768 \
--stop-at-end   \
--output-ppm-stream - \
$1 \
| ffmpeg -y -b 3000K -r 24 -f image2pipe \
-vcodec ppm -i - -vcodec mpeg4 $2.mp4
