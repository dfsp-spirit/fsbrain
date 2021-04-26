#!/bin/bash
# This script illustrates howto save images with resolution exceeding your screen resolution in fsbrain.

# Install dependencies on Debian-based distros (tested under Ubuntu 20.04 LTS):
#sudo apt install x11proto-xf86vidmode-dev xvfb libxxf86vm-dev

xvfb-run --server-args="-screen 0, 1920x1080x24" ./fsbrain_do_plot.R
