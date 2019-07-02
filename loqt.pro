#--------------------------------------------------
# loqt.pro: Logic / Qt interface
#--------------------------------------------------
# Collection of Qt components to efficiently
# interface logic languages
#--------------------------------------------------
# Author       : Carlo Capelli
# Copyright (C): 2013,2014,2015,2016,2017,2018,2019

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

SUBDIRS = \
    lqUty \
    lqXDot \
    lqXDot_test \
    pqConsole \
    pqGraphviz \
    pqXml \
    pqSource \
    pqSource_test

lqUty.subdir = lqUty

lqXDot.subdir = lqXDot
lqXDot.depends = lqUty

lqXDot_test.subdir = lqXDot_test
lqXDot_test.depends = lqUty lqXDot

pqConsole.subdir = pqConsole
pqConsole.depends = lqUty

pqGraphviz.subdir = pqGraphviz
pqGraphviz.depends = lqUty lqXDot pqConsole

pqXml.subdir = pqXml
pqXml.depends = lqUty pqConsole

pqSource.subdir = pqSource
pqSource.depends = lqUty pqConsole pqGraphviz lqXDot pqXml

pqSource_test.subdir = pqSource_test
pqSource_test

OTHER_FILES += \
    loqt.pri \
    README.md \
    img/cluster.png \
    img/cluster-dot.png
