#--------------------------------------------------
# loqt.pro: Logic / Qt interface
#--------------------------------------------------
# Collection of Qt components to efficiently
# interface logic languages
#--------------------------------------------------
# Author        : Carlo Capelli
# Copyright @ 2023

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

# please, an up-to-date compiler
CONFIG += C++20

QT += core gui widgets statemachine

windows {
    SwiPl = "C:\Program Files\swipl"
    INCLUDEPATH += $$SwiPl\include
    LIBS += -L$$SwiPl\bin -llibswipl
}

windows {
    GRAPHVIZ = "C:/Program Files/Graphviz"
    INCLUDEPATH += $$GRAPHVIZ/include
    LIBS += -L$$GRAPHVIZ/lib -lgvc -lcgraph -lcdt -lxdot
}

DEFINES += WITH_CGRAPH
DEFINES += GVDLL
DEFINES += QT_NO_OPENGL
