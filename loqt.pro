#--------------------------------------------------
# loqt.pro: Logic / Qt interface
#--------------------------------------------------
# Collection of Qt components to efficiently
# interface logic languages
#--------------------------------------------------
# Author        : Carlo Capelli
# Copyright (C) : 2013,2014,2015,2016

#    This library is free software; you can redistribute it and/or
#    modify it under the terms of the GNU Lesser General Public
#    License as published by the Free Software Foundation; either
#    version 2.1 of the License, or (at your option) any later version.
#
#    This library is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    Lesser General Public License for more details.
#
#    You should have received a copy of the GNU Lesser General Public
#    License along with this library; if not, write to the Free Software
#    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

TEMPLATE = subdirs

SUBDIRS += \
    lqUty \
    lqXDot \
    pqConsole \
    pqGraphviz \
    pqXml \
    pqSource \
    lq3d \
    lqShapes \
    lqXDot_test \
    lq3d_test \
    pqSource_test \
    fdqueens \
    qPrologPad \
    spqr \
    lqGraphix \
    lqShapes_test

OTHER_FILES += \
    loqt.pri \
    README.md \
    img/cluster.png \
    img/cluster-dot.png
