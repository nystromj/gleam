# Grinnell Livescripting Environment for Art and Music

The Grinnell Livescripting Environment for Art and Music, or GLEAM, is part of GLIMMER 2012. It is a .scm library designed for use with Andrew Sorensen's live-coding environment, [Impromptu](http://impromptu.moso.com.au/ "Impromptu"). The library adds gesture tracking capabilities to Impromptu via the Microsoft Kinect and Sensebloom's [OSCeleton](https://github.com/Sensebloom/OSCeleton "OSCeleton"). 

## Project Architecture

The kinectlib/ subdirectory contains kinectlib.scm, the Scheme library that can be loaded into Impromptu to enable gesture-tracking. The osc_tools/ subdirectory contains a patch for OSCeleton that adds oscfilter.c, a filter for the OSC messages sent by OSCeleton. The patch must be added to OSCeleton for skeleton tracking to work correctly in Impromptu.

For more information on setting up GLEAM and using the gesture libray, please see our github wiki.

## License

Copyright (c) 2012, Jennelle Nystrom

 This program is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.