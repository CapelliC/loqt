#--------------------------------------------------
# lqXDot_test.pro: Logic / Qt interface
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

include(../loqt.pri)

TARGET = lqXDot_test
TEMPLATE = app

# uses lqUty, lqXDot
CONFIG += link_prl

INCLUDEPATH += $$PWD/../lqUty
DEPENDPATH += $$PWD/../lqUty/debug
LIBS += -L$$OUT_PWD/../lqUty/debug -llqUty
message(A $CONFIG)

INCLUDEPATH += $$PWD/../lqXDot
DEPENDPATH += $$PWD/../lqXDot/debug
LIBS += -L$$OUT_PWD/../lqXDot/debug -llqXDot

DEFINES += WITH_CGRAPH GVDLL QT_NO_OPENGL

unix {
    CONFIG += link_pkgconfig
    PKGCONFIG += libcgraph libgvc
}

windows {
    GRAPHVIZ = "C:/Program Files/Graphviz"
    INCLUDEPATH += $$GRAPHVIZ/include
    LIBS += -L$$GRAPHVIZ/lib -lgvc -lcgraph -lcdt
}

SOURCES += \
    main.cpp \
    mainwindow.cpp

HEADERS += \
    mainwindow.h

OTHER_FILES += \
    gallery/images/rosemary.jpg \
    gallery/images/Rose_kennedy.JPG \
    gallery/images/n48862003257_1275276_1366.jpg \
    gallery/images/kennedyface.jpg \
    gallery/images/jacqueline-kennedy-onassis.jpg \
    gallery/images/BE037819.jpg \
    gallery/images/1025901671.jpg \
    gallery/images/180px-JFKJr2.jpg \
    gallery/images/165px-Caroline_Kennedy.jpg \
    gallery/crazy.gv \
    gallery/cluster.gv \
    gallery/angles.gv \
    gallery/wos.gv \
    gallery/world.gv \
    gallery/unix.gv \
    gallery/twopi2.gv \
    gallery/tree test.gv \
    gallery/tree_foldable.gv \
    gallery/transparency.gv \
    gallery/traffic_lights.gv \
    gallery/test_colors.gv \
    gallery/table.xdot \
    gallery/table.gv \
    gallery/switch.gv \
    gallery/softmaint.gv \
    gallery/siblings.gv \
    gallery/sdh.gv \
    gallery/root.gv \
    gallery/radial_angle.gv \
    gallery/psg.gv \
    gallery/profile.gv \
    gallery/philo.gv \
    gallery/networkmap_twopi.gv \
    gallery/lion_share.gv \
    gallery/linear_angle.gv \
    gallery/kennedyanc.gv \
    gallery/html3.gv \
    gallery/html2.gv \
    gallery/html1.gv \
    gallery/Genetic_Programming.gv \
    gallery/gd_1994_2007.gv \
    gallery/g_c_n.gv \
    gallery/fsm.gv \
    gallery/fdpclust.gv \
    gallery/ER.gv \
    gallery/debugger.gv \
    gallery/datastruct_gradient.gv \
    gallery/datastruct.gv \
    gallery/colors.gv \
    gallery/cluster_gradient.gv \
    gallery/tree test.gv

RESOURCES += \
    lqxdot_test.qrc

DISTFILES += \
    app.png
