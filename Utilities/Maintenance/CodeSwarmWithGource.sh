#!/bin/bash
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

usage() {

cat << EOF
  How to use this script:

Use Linux, install

     - gource

Invoke the script as:

    CodeSwarmWithGource.sh  sourcetree  outputvideofile


This will generate as output a file called

                outputvideofile.mp4


For example:

  CodeSwarmWithGource.sh  ~/src/ITK  ITKSwarm


This will generate the video file

                 ITKSwarm.mp4


EOF

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
