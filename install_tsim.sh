#!/bin/bash

# Install dependencies
sudo apt-get install libxaw7-dev libxt-dev libxmu-dev libx11-dev libxext-dev

# Extract source tsim source code
cd tsim
tar -xzvf tsim-0.84.tgz
cd tsim-0.84

# Configure and install
./configure
make
sudo make install
